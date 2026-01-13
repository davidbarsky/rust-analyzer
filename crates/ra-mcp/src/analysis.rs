//! Workspace loading, file watching, and snapshot access for MCP requests.

use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Condvar, Mutex, mpsc as std_mpsc};
use std::time::Duration;

use crossbeam_channel::{Receiver, RecvTimeoutError};
use hir::ChangeWithProcMacros;
use ide::{AnalysisHost, FilePosition};
use ide_db::base_db::salsa::Cancelled;
use ide_db::base_db::{SourceDatabase, all_crates};
use ide_db::{FxHashMap, FxHashSet};
use load_cargo::{LoadCargoConfig, ProcMacroServerChoice, SourceRootConfig};
use paths::{AbsPath, AbsPathBuf};
use project_model::{CargoConfig, ProjectJson, ProjectManifest, ProjectWorkspace, RustLibSource};
use tokio::sync::mpsc;
use vfs::VfsPath;
use vfs::loader::Handle as _;

use crate::discover::{self, DiscoverArgument, DiscoverConfig};
use crate::error::{AnalysisError, EnvironmentError, McpError, RequestError, cancelled_in};
use crate::params::{Column, Line};

/// A request to rebuild the project structure (crate graph, source roots, proc
/// macros). `Workspaces` re-runs cargo/rust-project discovery; `Discover` runs
/// the configured `discoverProject` command for a non-cargo build system.
enum ReloadRequest {
    Workspaces,
    Discover(DiscoverArgument),
}

struct ReloadCommand {
    request: ReloadRequest,
    reload: ActiveReload,
}

#[derive(Clone)]
struct WorkspaceLifecycle {
    sender: mpsc::UnboundedSender<ReloadCommand>,
    state: Arc<WorkspaceLifecycleState>,
}

struct WorkspaceLifecycleState {
    jobs: Mutex<Vec<Arc<ReloadJob>>>,
    ready: Condvar,
}

struct ReloadTicket {
    finished: std_mpsc::Receiver<()>,
}

struct ReloadJob;

struct ActiveReload {
    job: Arc<ReloadJob>,
    finished: Option<std_mpsc::Sender<()>>,
    state: Arc<WorkspaceLifecycleState>,
}

impl WorkspaceLifecycle {
    fn new(sender: mpsc::UnboundedSender<ReloadCommand>) -> Self {
        Self {
            sender,
            state: Arc::new(WorkspaceLifecycleState {
                jobs: Mutex::new(Vec::new()),
                ready: Condvar::new(),
            }),
        }
    }

    fn request_reload(&self, request: ReloadRequest) -> Option<ReloadTicket> {
        let (reload, ticket) = self.begin_reload_with_ticket();
        match self.sender.send(ReloadCommand { request, reload }) {
            Ok(()) => Some(ticket),
            Err(err) => {
                let mpsc::error::SendError(command) = err;
                let ReloadCommand { request, reload } = command;
                drop(request);
                drop(reload);
                None
            }
        }
    }

    fn begin_reload(&self) -> ActiveReload {
        let (reload, ticket) = self.begin_reload_with_ticket();
        drop(ticket);
        reload
    }

    fn begin_reload_with_ticket(&self) -> (ActiveReload, ReloadTicket) {
        let (finished, ticket) = std_mpsc::channel();
        let job = Arc::new(ReloadJob);
        let mut jobs = self.state.jobs.lock().unwrap();
        jobs.push(job.clone());
        drop(jobs);
        (
            ActiveReload { job, finished: Some(finished), state: self.state.clone() },
            ReloadTicket { finished: ticket },
        )
    }

    fn is_ready(&self) -> bool {
        let jobs = self.state.jobs.lock().unwrap();
        jobs.is_empty()
    }

    fn wait_until_ready(&self) {
        let mut jobs = self.state.jobs.lock().unwrap();
        while !jobs.is_empty() {
            jobs = self.state.ready.wait(jobs).unwrap();
        }
    }
}

impl ReloadTicket {
    fn wait(&self) {
        match self.finished.recv() {
            Ok(()) => (),
            Err(std_mpsc::RecvError) => (),
        }
    }
}

impl Drop for ActiveReload {
    fn drop(&mut self) {
        let mut jobs = self.state.jobs.lock().unwrap();
        match jobs.iter().position(|job| Arc::ptr_eq(job, &self.job)) {
            Some(index) => {
                jobs.swap_remove(index);
                if jobs.is_empty() {
                    self.state.ready.notify_all();
                }
            }
            None => panic!("active reload completed without a pending reload"),
        }
        drop(jobs);
        if let Some(finished) = self.finished.take() {
            match finished.send(()) {
                Ok(()) => (),
                Err(std_mpsc::SendError(())) => (),
            }
        }
    }
}

pub struct ServerState {
    pub host: AnalysisHost,
    /// Root paths the workspaces were discovered from; reused on reload.
    roots: Vec<AbsPathBuf>,
    source_root_config: SourceRootConfig,
    /// Files the crate graph was last derived from; a create/delete of one of
    /// these warrants a reload. Empty until the first reload populates it.
    crate_graph_file_dependencies: FxHashSet<VfsPath>,
    /// Present when the project is described by a `discoverProject` command
    /// rather than cargo; drives how the watcher classifies changes.
    discover_config: Option<DiscoverConfig>,
    reloads: WorkspaceLifecycle,
    loader: vfs_notify::NotifyHandle,
    /// Bumped each time the watcher is reconfigured; pairs with `scan_sync`
    /// so a reload can wait for its rescan to be fully applied.
    watch_config_version: u32,
    scan_sync: Arc<ScanSync>,
}

/// Watcher-to-reload rendezvous. The watcher thread publishes the highest
/// scan `config_version` it has fully applied; `reload` waits on it so a
/// workspace rebuild returns only once the rescan it triggered — which loads
/// any newly added crates' files — is in the database.
struct ScanSync {
    applied_version: Mutex<u32>,
    finished: Condvar,
}

impl ScanSync {
    fn wait_applied(&self, version: u32) {
        // The rescan normally finishes in milliseconds; the timeout only
        // keeps a wedged notify thread from hanging the reload forever.
        const SCAN_TIMEOUT: Duration = Duration::from_secs(60);

        let applied = self.applied_version.lock().unwrap();
        let (_applied, timeout) = self
            .finished
            .wait_timeout_while(applied, SCAN_TIMEOUT, |applied| *applied < version)
            .unwrap();
        if timeout.timed_out() {
            tracing::warn!(version, "rescan after reload did not finish; continuing");
        }
    }
}

impl ServerState {
    fn apply_loader_changes(&mut self, files: Vec<(vfs::AbsPathBuf, Option<Vec<u8>>)>) {
        let db = self.host.raw_database_mut();
        let mut change = ChangeWithProcMacros::default();
        let mut changed = false;
        let mut has_structure_changes = false;
        let mut source_root_files = db.file_paths().into_iter().collect::<FxHashMap<_, _>>();

        for (path, contents) in files {
            let vfs_path = VfsPath::from(path);
            match (db.file_id_for_path(&vfs_path), contents) {
                (Some(file_id), Some(bytes)) => {
                    source_root_files.insert(file_id, vfs_path);
                    if let Ok(text) = String::from_utf8(bytes) {
                        change.change_file(file_id, Some(text));
                    }
                    changed = true;
                }
                (None, Some(bytes)) => {
                    let file_id = db.intern_file_path(vfs_path.clone());
                    source_root_files.insert(file_id, vfs_path);
                    if let Ok(text) = String::from_utf8(bytes) {
                        change.change_file(file_id, Some(text));
                    }
                    changed = true;
                    has_structure_changes = true;
                }
                (Some(file_id), None) => {
                    source_root_files.remove(&file_id);
                    changed = true;
                    has_structure_changes = true;
                }
                (None, None) => (),
            }
        }

        if has_structure_changes {
            let roots = self.source_root_config.partition(source_root_files);
            change.set_roots(roots);
        }

        if changed {
            self.host.apply_change(change);
            tracing::debug!("Applied file changes to analysis database");
        }
    }
}

struct SnapshotDiscover {
    reloads: WorkspaceLifecycle,
}

/// Holds an analysis view from a single applied change.
pub struct Snapshot {
    analysis: ide::Analysis,
    /// Workspace roots, used to resolve relative request paths. Results are
    /// rendered workspace-relative, so requests must accept the same form back.
    roots: Vec<AbsPathBuf>,
    discover: Option<SnapshotDiscover>,
    discovery_attempts: Arc<Mutex<Vec<AbsPathBuf>>>,
}

impl Snapshot {
    fn new(state: &ServerState, discovery_attempts: Arc<Mutex<Vec<AbsPathBuf>>>) -> Self {
        let discover = state
            .discover_config
            .as_ref()
            .map(|_| SnapshotDiscover { reloads: state.reloads.clone() });
        Self {
            analysis: state.host.analysis(),
            roots: state.roots.clone(),
            discover,
            discovery_attempts,
        }
    }

    /// Test-only constructor. Builds a `Snapshot` directly from a pre-built
    /// `Analysis`, skipping the workspace-loading machinery. Used by fixture-based
    /// unit tests to exercise analysis helpers without spinning up a Cargo workspace.
    #[doc(hidden)]
    pub fn for_test(analysis: ide::Analysis) -> Self {
        Self {
            analysis,
            roots: Vec::new(),
            discover: None,
            discovery_attempts: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn raw_database(&self) -> &ide_db::RootDatabase {
        self.analysis.raw_database()
    }

    pub fn analysis(&self) -> &ide::Analysis {
        &self.analysis
    }

    pub fn resolve_file_id(&self, path: &Path) -> Result<ide_db::FileId, McpError> {
        let path_str = path.to_string_lossy();
        let candidates: Vec<AbsPathBuf> = match paths::AbsPathBuf::try_from(path_str.as_ref()) {
            Ok(abs_path) => vec![abs_path],
            Err(_) => {
                let mut joined = Vec::with_capacity(self.roots.len());
                for root in &self.roots {
                    joined.push(root.join(path_str.as_ref()));
                }
                joined
            }
        };
        if candidates.is_empty() {
            return Err(RequestError::InvalidPath { path: path.display().to_string() }.into());
        }

        let db = self.raw_database();
        for abs_path in &candidates {
            let vfs_path = VfsPath::from(abs_path.clone());
            if let Some(file_id) = db.file_id_for_path(&vfs_path) {
                return Ok(file_id);
            }
        }
        if self.roots.is_empty() {
            let vfs_path = VfsPath::new_virtual_path(path_str.to_string());
            if let Some(file_id) = db.file_id_for_path(&vfs_path) {
                return Ok(file_id);
            }
        }

        let Some(discover) = &self.discover else {
            return Err(RequestError::FileNotFound { path: path.display().to_string() }.into());
        };
        let candidate = candidates[0].clone();
        let mut discovery_attempts = self.discovery_attempts.lock().unwrap();
        if discovery_attempts.iter().any(|attempt| attempt == &candidate) {
            return Err(RequestError::FileNotFound { path: path.display().to_string() }.into());
        }
        discovery_attempts.push(candidate.clone());
        drop(discovery_attempts);

        match discover
            .reloads
            .request_reload(ReloadRequest::Discover(DiscoverArgument::Path(candidate)))
        {
            Some(ticket) => {
                ticket.wait();
                Err(AnalysisError::StaleSnapshot.into())
            }
            None => Err(RequestError::FileNotFound { path: path.display().to_string() }.into()),
        }
    }

    pub fn resolve_position(
        &self,
        path: &Path,
        line: Line,
        column: Column,
    ) -> Result<FilePosition, McpError> {
        let file_id = self.resolve_file_id(path)?;

        let line_index =
            self.analysis.file_line_index(file_id).map_err(cancelled_in("get line index"))?;

        // Requests are 1-based (matching grep-shaped tool output); LineIndex
        // is 0-based. saturating_sub keeps a stray 0 from underflowing.
        let offset = line_index
            .offset(ide_db::line_index::LineCol {
                line: line.get().saturating_sub(1),
                col: column.get().saturating_sub(1),
            })
            .ok_or_else(|| RequestError::InvalidPosition {
                line: line.get(),
                column: column.get(),
                reason: "position outside file bounds (line/column are 1-based)".into(),
            })?;

        Ok(FilePosition { file_id, offset })
    }

    pub fn resolve_range(
        &self,
        path: &Path,
        start_line: Line,
        start_col: Column,
        end_line: Line,
        end_col: Column,
    ) -> Result<(ide_db::FileId, syntax::TextRange), McpError> {
        let file_id = self.resolve_file_id(path)?;

        let line_index =
            self.analysis.file_line_index(file_id).map_err(cancelled_in("get line index"))?;

        let db = self.raw_database();
        let text = db.file_text(file_id).text(db);
        let file_end = syntax::TextSize::of(&**text);
        let offset = |line: Line, column: Column| {
            let line = line.get().saturating_sub(1);
            let column = column.get().saturating_sub(1);
            if let Some(offset) =
                line_index.offset(ide_db::line_index::LineCol { line, col: column })
            {
                return offset;
            }

            let Some(_) = line_index.offset(ide_db::line_index::LineCol { line, col: 0 }) else {
                return file_end;
            };
            line_index
                .offset(ide_db::line_index::LineCol { line: line.saturating_add(1), col: 0 })
                .unwrap_or(file_end)
        };

        let start = offset(start_line, start_col);
        let end = offset(end_line, end_col);

        if start > end {
            return Err(RequestError::InvalidPosition {
                line: end_line.get(),
                column: end_col.get(),
                reason: "end position is before start position".into(),
            }
            .into());
        }

        Ok((file_id, syntax::TextRange::new(start, end)))
    }
}

#[derive(Clone, Default)]
pub struct Analysis {
    workspaces: Arc<Mutex<FxHashMap<WorkspaceKey, Workspace>>>,
    initializing: Arc<Mutex<()>>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct WorkspaceKey {
    roots: Vec<AbsPathBuf>,
}

#[derive(Clone)]
pub struct Workspace {
    state: Arc<Mutex<ServerState>>,
}

impl Analysis {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn workspace(&self, paths: Vec<PathBuf>) -> Result<Workspace, McpError> {
        let key = normalize_roots(paths)?;
        {
            let guard = self.workspaces.lock().unwrap();
            if let Some(workspace) = guard.get(&key) {
                return Ok(workspace.clone());
            }
        }

        let _initializing = self.initializing.lock().unwrap();
        {
            let guard = self.workspaces.lock().unwrap();
            if let Some(workspace) = guard.get(&key) {
                return Ok(workspace.clone());
            }
        }

        let abs_paths = key.roots.clone();
        let (mut server_state, vfs_receiver, abs_paths, reload_rx) = load_workspaces(abs_paths)?;
        drain_initial_scan(&mut server_state, &vfs_receiver);

        let scan_sync = server_state.scan_sync.clone();
        let state = Arc::new(Mutex::new(server_state));
        let workspace = Workspace { state: state.clone() };

        spawn_file_watcher(state.clone(), vfs_receiver, scan_sync);
        spawn_reload_actor(state, abs_paths, reload_rx);
        let mut guard = self.workspaces.lock().unwrap();
        guard.insert(key, workspace.clone());
        Ok(workspace)
    }
}

impl Workspace {
    pub fn apply_file_changes(&self, files: Vec<(PathBuf, String)>) {
        let mut state = self.state.lock().unwrap();
        let mut vfs_files = Vec::with_capacity(files.len());
        for (path, content) in files {
            let abs = paths::AbsPathBuf::assert_utf8(path);
            vfs_files.push((abs, Some(content.into_bytes())));
        }
        state.apply_loader_changes(vfs_files);
    }

    /// Runs one workspace reload synchronously, bypassing the async actor, and
    /// returns once the crate graph has been rebuilt. Intended for tests that
    /// need a deterministic reload without waiting on the file watcher.
    #[doc(hidden)]
    pub fn reload_blocking(&self) {
        let (roots, reloads) = {
            let guard = self.state.lock().unwrap();
            (guard.roots.clone(), guard.reloads.clone())
        };
        let active_reload = reloads.begin_reload();
        let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            reload(&self.state, &roots, ReloadRequest::Workspaces)
        }));
        drop(active_reload);
        if let Err(payload) = result {
            std::panic::resume_unwind(payload);
        }
    }

    fn snapshot(
        &self,
        discovery_attempts: Arc<Mutex<Vec<AbsPathBuf>>>,
    ) -> Result<Snapshot, McpError> {
        loop {
            let guard = self.state.lock().unwrap();
            if guard.reloads.is_ready() {
                return Ok(Snapshot::new(&guard, discovery_attempts));
            }
            let reloads = guard.reloads.clone();
            drop(guard);
            reloads.wait_until_ready();
        }
    }

    pub async fn run_snapshot<F, T, E>(&self, f: F) -> Result<T, McpError>
    where
        F: Fn(&Snapshot) -> Result<T, E> + Send + Sync + 'static,
        T: Send + 'static,
        E: Into<McpError> + Send + 'static,
    {
        const MAX_RETRIES: usize = 20;

        let workspace = self.clone();
        tokio::task::spawn_blocking(move || {
            let discovery_attempts = Arc::new(Mutex::new(Vec::new()));
            for attempt in 0..MAX_RETRIES {
                let snapshot = workspace.snapshot(discovery_attempts.clone())?;
                let result =
                    Cancelled::catch(AssertUnwindSafe(|| f(&snapshot).map_err(Into::into)));
                drop(snapshot);

                match result {
                    Ok(Ok(value)) => return Ok(value),
                    Ok(Err(err)) => {
                        let retry_reason = match &err {
                            McpError::Analysis(AnalysisError::StaleSnapshot) => {
                                Some("snapshot became stale")
                            }
                            McpError::Analysis(AnalysisError::Cancelled {
                                operation: _,
                                source,
                            }) => match source {
                                Cancelled::PendingWrite => Some("pending write"),
                                Cancelled::Local | Cancelled::PropagatedPanic => None,
                                _ => None,
                            },
                            McpError::Analysis(AnalysisError::Failed {
                                operation: _,
                                details: _,
                            }) => None,
                            McpError::Analysis(AnalysisError::NotFound { what: _ }) => None,
                            McpError::Request(_) | McpError::Environment(_) => None,
                        };
                        if let Some(retry_reason) = retry_reason {
                            let backoff_ms = (attempt + 1) as u64 * 10;
                            tracing::debug!(
                                attempt = attempt + 1,
                                max_retries = MAX_RETRIES,
                                backoff_ms,
                                retry_reason,
                                "Query needs a fresh analysis snapshot, retrying",
                            );
                            std::thread::sleep(Duration::from_millis(backoff_ms));
                            continue;
                        }
                        return Err(err);
                    }
                    Err(cancelled) => {
                        let backoff_ms = (attempt + 1) as u64 * 10;
                        tracing::debug!(
                            attempt = attempt + 1,
                            max_retries = MAX_RETRIES,
                            backoff_ms,
                            source = %cancelled,
                            "Query panicked with cancellation, retrying",
                        );
                        std::thread::sleep(Duration::from_millis(backoff_ms));
                        continue;
                    }
                }
            }
            Err(AnalysisError::Failed {
                operation: "query",
                details: "all retries exhausted due to cancellation".into(),
            }
            .into())
        })
        .await
        .map_err(|e| EnvironmentError::TaskFailed { message: e.to_string() })?
    }
}

fn normalize_roots(paths: Vec<PathBuf>) -> Result<WorkspaceKey, McpError> {
    let cwd = std::env::current_dir()
        .map_err(|e| EnvironmentError::Io { path: ".".into(), message: e.to_string() })?;

    let mut roots = Vec::with_capacity(paths.len());
    for path in paths {
        roots.push(AbsPathBuf::assert_utf8(cwd.join(path)).normalize());
    }
    roots.sort();
    roots.dedup();
    Ok(WorkspaceKey { roots })
}

fn cargo_config() -> CargoConfig {
    CargoConfig {
        sysroot: Some(RustLibSource::Discover),
        all_targets: true,
        set_test: true,
        ..Default::default()
    }
}

fn load_config() -> LoadCargoConfig {
    LoadCargoConfig {
        load_out_dirs_from_check: true,
        with_proc_macro_server: ProcMacroServerChoice::Sysroot,
        prefill_caches: false,
        num_worker_threads: 1,
        proc_macro_processes: 1,
    }
}

/// Discovers manifests under `abs_paths` and loads each into a `ProjectWorkspace`,
/// running build scripts. Shared by the initial load and by reloads.
fn discover_and_load_workspaces(
    abs_paths: &[AbsPathBuf],
    cargo_config: &CargoConfig,
    load_config: &LoadCargoConfig,
) -> Result<Vec<ProjectWorkspace>, McpError> {
    let manifests = ProjectManifest::discover_all(abs_paths);
    if manifests.is_empty() {
        return Err(EnvironmentError::WorkspaceLoadFailed {
            message: format!("no Cargo.toml or rust-project.json found in roots: {abs_paths:?}"),
        }
        .into());
    }

    tracing::info!(count = manifests.len(), ?manifests, "Discovered manifests");

    let progress = |msg: String| {
        tracing::debug!(msg, "Loading");
    };

    let mut workspaces = Vec::with_capacity(manifests.len());
    for manifest in &manifests {
        tracing::info!(%manifest, "Loading workspace from manifest");
        let mut ws = match ProjectWorkspace::load(manifest.clone(), cargo_config, &progress) {
            Ok(ws) => ws,
            Err(e) => {
                tracing::warn!(%manifest, error = %e, "Failed to load workspace");
                continue;
            }
        };
        if load_config.load_out_dirs_from_check {
            match ws.run_build_scripts(cargo_config, &progress) {
                Ok(build_scripts) => {
                    if let Some(error) = build_scripts.error() {
                        tracing::debug!(%manifest, %error, "Build script errors");
                    }
                    ws.set_build_scripts(build_scripts);
                }
                Err(e) => {
                    tracing::warn!(%manifest, error = %e, "Failed to run build scripts");
                }
            }
        }
        workspaces.push(ws);
    }

    if workspaces.is_empty() {
        return Err(EnvironmentError::WorkspaceLoadFailed {
            message: "all discovered workspaces failed to load".into(),
        }
        .into());
    }

    Ok(workspaces)
}

/// Runs the `discoverProject` command for `arg` and loads the resulting
/// `rust-project.json` into a single workspace.
fn discover_workspaces(
    config: &DiscoverConfig,
    arg: &DiscoverArgument,
    base: &AbsPath,
    cargo_config: &CargoConfig,
) -> Result<Vec<ProjectWorkspace>, McpError> {
    let (data, _buildfile) = discover::run_discover(config, arg, base)
        .map_err(|e| EnvironmentError::WorkspaceLoadFailed { message: e.to_string() })?;
    let project = ProjectJson::new(None, base, data);
    let progress = |msg: String| tracing::debug!(msg, "Discovering");
    Ok(vec![ProjectWorkspace::load_inline(project, cargo_config, &progress)])
}

/// Discovers and loads workspaces from the given root paths, building the initial
/// analysis database, file watching, and the reload channel.
fn load_workspaces(
    abs_paths: Vec<AbsPathBuf>,
) -> Result<
    (
        ServerState,
        Receiver<vfs::loader::Message>,
        Vec<AbsPathBuf>,
        mpsc::UnboundedReceiver<ReloadCommand>,
    ),
    McpError,
> {
    let cargo_config = cargo_config();
    let load_config = load_config();

    tracing::info!(?abs_paths, "Discovering workspaces");

    let discover_config = abs_paths.first().and_then(|root| discover::read_discover_config(root));
    let workspaces = match (&discover_config, abs_paths.first()) {
        (Some(config), Some(root)) => {
            discover_workspaces(config, &DiscoverArgument::Path(root.clone()), root, &cargo_config)?
        }
        _ => discover_and_load_workspaces(&abs_paths, &cargo_config, &load_config)?,
    };

    let lru_cap = std::env::var("RA_LRU_CAP").ok().and_then(|it| it.parse::<u16>().ok());
    let mut db = ide_db::RootDatabase::new(lru_cap);

    let (_proc_macro, project_folders) = load_cargo::load_workspaces_into_db(
        &workspaces,
        &cargo_config.extra_env,
        &load_config,
        &mut db,
    );

    let crate_count = all_crates(&db).len();
    let host = AnalysisHost::with_database(db);

    let (sender, receiver) = crossbeam_channel::unbounded();
    let mut loader = vfs_notify::NotifyHandle::spawn(sender);
    loader.set_config(vfs::loader::Config {
        load: project_folders.load,
        watch: project_folders.watch,
        version: 0,
    });

    let (reload_tx, reload_rx) = mpsc::unbounded_channel();
    let reloads = WorkspaceLifecycle::new(reload_tx);

    let server_state = ServerState {
        host,
        roots: abs_paths.clone(),
        source_root_config: project_folders.source_root_config,
        crate_graph_file_dependencies: FxHashSet::default(),
        discover_config,
        reloads,
        loader,
        watch_config_version: 0,
        scan_sync: Arc::new(ScanSync { applied_version: Mutex::new(0), finished: Condvar::new() }),
    };

    tracing::info!(
        crate_count,
        workspace_count = workspaces.len(),
        "Workspaces loaded, file watching enabled"
    );

    Ok((server_state, receiver, abs_paths, reload_rx))
}

/// Applies the loader's initial scan synchronously, so `initialize` returns
/// with a quiesced file scan. The scan re-reads every workspace file from disk;
/// applying it here (a no-op, since the database was loaded from the same
/// disk state) instead of on the watcher thread means it can never race an
/// in-memory edit made after startup and clobber it with stale disk content.
fn drain_initial_scan(state: &mut ServerState, receiver: &Receiver<vfs::loader::Message>) {
    // The scan normally finishes in milliseconds; the timeout only keeps a
    // wedged notify thread from hanging startup forever.
    const SCAN_TIMEOUT: Duration = Duration::from_secs(60);

    loop {
        let msg = match receiver.recv_timeout(SCAN_TIMEOUT) {
            Ok(msg) => msg,
            Err(RecvTimeoutError::Timeout | RecvTimeoutError::Disconnected) => {
                tracing::warn!("initial file scan did not finish; falling back to async apply");
                return;
            }
        };
        match msg {
            vfs::loader::Message::Loaded { files } => {
                state.apply_loader_changes(files);
            }
            vfs::loader::Message::Changed { files } => {
                let reload_request = classify_reload(&files, state);
                state.apply_loader_changes(files);
                if let Some(request) = reload_request {
                    let _ = state.reloads.request_reload(request);
                }
            }
            vfs::loader::Message::Progress { n_done, .. } => {
                if n_done == vfs::loader::LoadingProgress::Finished {
                    tracing::debug!("initial file scan applied synchronously");
                    return;
                }
            }
        }
    }
}

/// Whether an external change to `path` warrants a full project reload (as
/// opposed to the per-file reindex the watcher already does). Covers the common
/// metadata files; the crate-graph's own file dependencies are checked
/// separately by the caller once a reload has populated them.
fn is_reload_trigger(path: &VfsPath) -> bool {
    let Some((stem, ext)) = path.name_and_extension() else {
        return false;
    };
    match (stem, ext) {
        ("Cargo", Some("toml" | "lock")) => true,
        ("build", Some("rs")) => true,
        ("rust-project", Some("json")) => true,
        ("Cargo" | "build" | "rust-project", None | Some(_)) => false,
        (_, None | Some(_)) => false,
    }
}

/// Rebuilds project structure in trigger order.
///
/// Snapshots wait for accepted reload jobs before cloning analysis state.
fn spawn_reload_actor(
    state: Arc<Mutex<ServerState>>,
    abs_paths: Vec<AbsPathBuf>,
    mut reload_rx: mpsc::UnboundedReceiver<ReloadCommand>,
) {
    tokio::spawn(async move {
        while let Some(command) = reload_rx.recv().await {
            let ReloadCommand { request, reload: active_reload } = command;
            let state = state.clone();
            let abs_paths = abs_paths.clone();
            let joined =
                tokio::task::spawn_blocking(move || reload(&state, &abs_paths, request)).await;
            drop(active_reload);
            if let Err(e) = joined {
                tracing::error!(error = %e, "reload task panicked");
            }
        }
        tracing::debug!("reload actor exited");
    });
}

/// Rediscover and reload the workspaces, applying the rebuilt crate graph to the
/// live database.
fn reload(state: &Arc<Mutex<ServerState>>, abs_paths: &[AbsPathBuf], request: ReloadRequest) {
    let cargo_config = cargo_config();
    let load_config = load_config();

    let discover_config = {
        let guard = match state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        guard.discover_config.clone()
    };

    let workspaces = match request {
        ReloadRequest::Workspaces => {
            match discover_and_load_workspaces(abs_paths, &cargo_config, &load_config) {
                Ok(workspaces) => workspaces,
                Err(e) => {
                    tracing::warn!(error = %e, "reload: discovery failed, keeping current state");
                    return;
                }
            }
        }
        ReloadRequest::Discover(arg) => {
            let (Some(config), Some(base)) = (&discover_config, abs_paths.first()) else {
                tracing::warn!("reload: discover requested but no discover config or root");
                return;
            };
            match discover_workspaces(config, &arg, base, &cargo_config) {
                Ok(workspaces) => workspaces,
                Err(e) => {
                    tracing::warn!(error = %e, "reload: discover failed, keeping current state");
                    return;
                }
            }
        }
    };

    let mut guard = match state.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    };
    let (deps, project_folders) = load_cargo::reload_workspaces_into_db(
        &workspaces,
        &cargo_config.extra_env,
        &load_config,
        guard.host.raw_database_mut(),
    );
    guard.crate_graph_file_dependencies = deps;
    guard.source_root_config = project_folders.source_root_config;
    let crate_count = all_crates(guard.host.raw_database()).len();

    // The crate-graph rebuild only load_syncs crate roots. Re-point the
    // watcher at the new project layout — the rescan this triggers is what
    // loads the remaining files of any newly added crates and watches their
    // directories — then wait for it, so callers see a quiesced database.
    guard.watch_config_version += 1;
    let scan_version = guard.watch_config_version;
    guard.loader.set_config(vfs::loader::Config {
        load: project_folders.load,
        watch: project_folders.watch,
        version: scan_version,
    });
    let scan_sync = guard.scan_sync.clone();
    drop(guard);

    scan_sync.wait_applied(scan_version);
    tracing::info!(crate_count, "Reloaded");
}

/// Decides whether a batch of changed files warrants a reload, and which kind.
/// For a `discoverProject` project, a change to a watched buildfile triggers
/// rediscovery; otherwise a change to a crate-graph input triggers a cargo reload.
fn classify_reload(
    files: &[(vfs::AbsPathBuf, Option<Vec<u8>>)],
    server_state: &ServerState,
) -> Option<ReloadRequest> {
    files.iter().find_map(|(path, _)| match &server_state.discover_config {
        Some(config) => {
            let name = path.file_name();
            let watched = config.files_to_watch.iter().any(|it| Some(it.as_str()) == name);
            watched.then(|| ReloadRequest::Discover(DiscoverArgument::Buildfile(path.clone())))
        }
        None => {
            let vfs_path = VfsPath::from(path.clone());
            let trigger = is_reload_trigger(&vfs_path)
                || server_state.crate_graph_file_dependencies.contains(&vfs_path);
            trigger.then_some(ReloadRequest::Workspaces)
        }
    })
}

fn spawn_file_watcher(
    state: Arc<Mutex<ServerState>>,
    receiver: Receiver<vfs::loader::Message>,
    scan_sync: Arc<ScanSync>,
) {
    const DEBOUNCE: Duration = Duration::from_millis(50);

    std::thread::Builder::new()
        .name("ra-mcp-file-watcher".into())
        .spawn(move || {
            let apply = |files: Vec<(vfs::AbsPathBuf, Option<Vec<u8>>)>, classify: bool| {
                let mut guard = match state.lock() {
                    Ok(g) => g,
                    Err(poisoned) => {
                        tracing::error!("file watcher: state mutex poisoned, recovering");
                        poisoned.into_inner()
                    }
                };
                let reload_request = classify.then(|| classify_reload(&files, &guard)).flatten();
                if let Err(e) = std::panic::catch_unwind(AssertUnwindSafe(|| {
                    guard.apply_loader_changes(files);
                })) {
                    tracing::error!(?e, "file watcher: apply_loader_changes panicked");
                }
                if let Some(request) = reload_request {
                    let _ = guard.reloads.request_reload(request);
                }
            };

            let mut pending: Option<vfs::loader::Message> = None;
            loop {
                let msg = match pending.take() {
                    Some(msg) => msg,
                    None => match receiver.recv() {
                        Ok(msg) => msg,
                        Err(_) => break,
                    },
                };
                match msg {
                    vfs::loader::Message::Changed { mut files } => {
                        loop {
                            match receiver.recv_timeout(DEBOUNCE) {
                                Ok(vfs::loader::Message::Changed { files: more }) => {
                                    files.extend(more);
                                }
                                Ok(other) => {
                                    pending = Some(other);
                                    break;
                                }
                                Err(RecvTimeoutError::Timeout | RecvTimeoutError::Disconnected) => {
                                    break;
                                }
                            }
                        }
                        tracing::debug!(count = files.len(), "files changed");
                        apply(files, true);
                    }
                    vfs::loader::Message::Loaded { files } => {
                        tracing::debug!(count = files.len(), "files loaded");
                        apply(files, false);
                    }
                    vfs::loader::Message::Progress { n_done, config_version, .. } => {
                        if n_done == vfs::loader::LoadingProgress::Finished {
                            tracing::debug!(config_version, "file scan finished");
                            let mut applied = scan_sync.applied_version.lock().unwrap();
                            if *applied < config_version {
                                *applied = config_version;
                            }
                            scan_sync.finished.notify_all();
                        }
                    }
                }
            }
            tracing::debug!("file watcher thread exited");
        })
        .expect("failed to spawn file watcher thread");
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    fn triggers(path: &str) -> bool {
        is_reload_trigger(&VfsPath::from(AbsPathBuf::assert_utf8(path.into())))
    }

    #[test]
    fn metadata_files_trigger_reload() {
        assert!(triggers("/ws/Cargo.toml"));
        assert!(triggers("/ws/Cargo.lock"));
        assert!(triggers("/ws/crate/build.rs"));
        assert!(triggers("/ws/rust-project.json"));
    }

    #[test]
    fn source_files_do_not_trigger_reload() {
        assert!(!triggers("/ws/src/lib.rs"));
        assert!(!triggers("/ws/src/main.rs"));
        assert!(!triggers("/ws/src/module.rs"));
        assert!(!triggers("/ws/README.md"));
    }

    #[test]
    fn reload_ticket_waits_for_command_completion() {
        let (sender, mut receiver) = mpsc::unbounded_channel();
        let reloads = WorkspaceLifecycle::new(sender);

        let ticket =
            reloads.request_reload(ReloadRequest::Workspaces).expect("reload request queued");
        assert!(!reloads.is_ready());

        let command = receiver.try_recv().expect("reload command queued");
        let ReloadCommand { request, reload: active_reload } = command;
        match request {
            ReloadRequest::Workspaces => (),
            ReloadRequest::Discover(arg) => drop(arg),
        }
        drop(active_reload);

        ticket.wait();
        assert!(reloads.is_ready());
    }

    #[test]
    fn workspace_lifecycle_waits_for_all_reload_jobs() {
        let (sender, mut receiver) = mpsc::unbounded_channel();
        let reloads = WorkspaceLifecycle::new(sender);

        let first_ticket =
            reloads.request_reload(ReloadRequest::Workspaces).expect("reload request queued");
        let second_ticket =
            reloads.request_reload(ReloadRequest::Workspaces).expect("reload request queued");
        assert!(!reloads.is_ready());

        let first_command = receiver.try_recv().expect("first reload command queued");
        let ReloadCommand { request: first_request, reload: first_reload } = first_command;
        match first_request {
            ReloadRequest::Workspaces => (),
            ReloadRequest::Discover(arg) => drop(arg),
        }

        let second_command = receiver.try_recv().expect("second reload command queued");
        let ReloadCommand { request: second_request, reload: second_reload } = second_command;
        match second_request {
            ReloadRequest::Workspaces => (),
            ReloadRequest::Discover(arg) => drop(arg),
        }

        drop(first_reload);
        first_ticket.wait();
        assert!(!reloads.is_ready());

        drop(second_reload);
        second_ticket.wait();
        assert!(reloads.is_ready());
    }
}
