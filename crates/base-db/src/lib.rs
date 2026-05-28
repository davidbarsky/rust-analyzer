//! base_db defines basic database traits. The concrete DB is defined by ide.

#![cfg_attr(feature = "in-rust-tree", feature(rustc_private))]

#[cfg(feature = "in-rust-tree")]
extern crate rustc_driver as _;

pub use salsa;
pub use salsa_macros;
use span::TextSize;

// FIXME: Rename this crate, base db is non descriptive
mod change;
mod editioned_file_id;
mod input;
pub mod target;

use std::{
    cell::RefCell,
    panic,
    sync::{Once, atomic::AtomicUsize},
};

pub use crate::{
    change::FileChange,
    editioned_file_id::EditionedFileId,
    input::{
        BuiltCrateData, BuiltDependency, Crate, CrateBuilder, CrateBuilderId, CrateDataBuilder,
        CrateDisplayName, CrateGraphBuilder, CrateName, CrateOrigin, CratesIdMap, CratesMap,
        DependencyBuilder, Env, ExtraCrateData, LangCrateOrigin, ProcMacroLoadingError,
        ProcMacroPaths, ReleaseChannel, SourceRoot, SourceRootId, SourceRootKind, UniqueCrateData,
    },
};
pub use query_group;
use rustc_hash::FxHashMap;
use salsa::{Durability, Setter, plumbing::AsId as _};
pub use semver::{BuildMetadata, Prerelease, Version, VersionReq};
use triomphe::Arc;
pub use vfs::{AbsPathBuf, AnchoredPath, AnchoredPathBuf, FileId, VfsPath, file_set::FileSet};

pub type FxIndexSet<T> = indexmap::IndexSet<T, rustc_hash::FxBuildHasher>;
pub type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[macro_export]
macro_rules! impl_intern_key {
    ($id:ident, $loc:ident) => {
        #[salsa_macros::interned(no_lifetime, revisions = usize::MAX)]
        #[derive(PartialOrd, Ord)]
        pub struct $id {
            #[returns(ref)]
            pub loc: $loc,
        }

        // If we derive this salsa prints the values recursively, and this causes us to blow.
        impl ::std::fmt::Debug for $id {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_tuple(stringify!($id))
                    .field(&format_args!("{:04x}", self.0.index()))
                    .finish()
            }
        }
    };
}

/// # SAFETY
///
/// `old_pointer` must be valid for unique writes
pub unsafe fn unsafe_update_eq<T>(old_pointer: *mut T, new_value: T) -> bool
where
    T: PartialEq,
{
    // SAFETY: Caller obligation
    let old_ref: &mut T = unsafe { &mut *old_pointer };

    if *old_ref != new_value {
        *old_ref = new_value;
        true
    } else {
        // Subtle but important: Eq impls can be buggy or define equality
        // in surprising ways. If it says that the value has not changed,
        // we do not modify the existing value, and thus do not have to
        // update the revision, as downstream code will not see the new value.
        false
    }
}

pub const DEFAULT_FILE_TEXT_LRU_CAP: u16 = 16;
pub const DEFAULT_PARSE_LRU_CAP: u16 = 128;
pub const DEFAULT_BORROWCK_LRU_CAP: u16 = 2024;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LineEndings {
    Unix,
    Dos,
}

impl LineEndings {
    pub fn normalize(src: String) -> (String, LineEndings) {
        let mut buf = src.into_bytes();
        let mut gap_len = 0;
        let mut tail = buf.as_mut_slice();
        let mut crlf_seen = false;

        let finder = memchr::memmem::Finder::new(b"\r\n");

        loop {
            let idx = match finder.find(&tail[gap_len..]) {
                None if crlf_seen => tail.len(),
                None => {
                    return (
                        String::from_utf8(buf).expect("input was valid UTF-8"),
                        LineEndings::Unix,
                    );
                }
                Some(idx) => {
                    crlf_seen = true;
                    idx + gap_len
                }
            };
            tail.copy_within(gap_len..idx, 0);
            tail = &mut tail[idx - gap_len..];
            if tail.len() == gap_len {
                break;
            }
            gap_len += 1;
        }

        let new_len = buf.len() - gap_len;
        // SAFETY: removing `\r` from UTF-8 `\r\n` pairs preserves UTF-8 validity.
        let src = unsafe {
            buf.set_len(new_len);
            String::from_utf8_unchecked(buf)
        };
        (src, LineEndings::Dos)
    }
}

#[salsa_macros::input(debug)]
pub struct FileText {
    #[returns(ref)]
    pub text: Arc<str>,
    pub line_endings: LineEndings,
}

#[salsa_macros::input(singleton, debug)]
pub struct FileTextTable {
    #[returns(ref)]
    pub texts: FxHashMap<FileId, FileText>,
}

#[salsa_macros::input(singleton, debug)]
pub struct SourceRootTable {
    #[returns(ref)]
    pub roots: FxHashMap<SourceRootId, SourceRoot>,
}

#[doc(hidden)]
#[salsa::interned]
pub struct InternedFileId {
    pub id: FileId,
}

#[doc(hidden)]
#[salsa_macros::interned(no_lifetime, debug, revisions = usize::MAX)]
pub struct InternedFilePath {
    #[returns(ref)]
    pub path: VfsPath,
}

#[doc(hidden)]
#[salsa::interned]
pub struct InternedAnchoredPath {
    #[returns(ref)]
    pub path: AnchoredPathBuf,
}

#[salsa_macros::db]
pub trait SourceDatabase: salsa::Database {
    /// Text of the file.
    fn file_text(&self, file_id: vfs::FileId) -> FileText {
        let db = self.as_dyn_database();
        lookup_file_text(db, InternedFileId::new(db, file_id))
    }

    fn set_file_text(&mut self, file_id: vfs::FileId, text: &str) {
        ensure_file_text(self, file_id).set_text(self).to(Arc::from(text));
    }

    fn set_file_text_with_line_endings_and_durability(
        &mut self,
        file_id: vfs::FileId,
        text: &str,
        line_endings: LineEndings,
        durability: Durability,
    ) {
        let file_text = ensure_file_text(self, file_id);
        file_text.set_text(self).with_durability(durability).to(Arc::from(text));
        file_text.set_line_endings(self).with_durability(durability).to(line_endings);
    }

    /// Contents of the source root.
    fn source_root(&self, id: SourceRootId) -> SourceRoot {
        let db = self.as_dyn_database();
        lookup_source_root(db, InternedSourceRootId::new(db, id))
    }

    /// Source root the file belongs to.
    fn file_source_root(&self, file_id: vfs::FileId) -> SourceRootId {
        let db = self.as_dyn_database();
        match lookup_file_source_root(db, InternedFileId::new(db, file_id)) {
            Some(source_root) => source_root,
            None => {
                panic!("Unable to fetch source root for `vfs::FileId`: {file_id:?}; this is a bug")
            }
        }
    }

    fn resolve_path(&self, anchor: FileId, paths: &[&str]) -> Option<(FileId, usize)> {
        let db = self.as_dyn_database();

        for (index, path) in paths.iter().enumerate() {
            let target_id = lookup_resolve_path(
                db,
                InternedAnchoredPath::new(db, AnchoredPathBuf { anchor, path: path.to_string() }),
            );
            match target_id {
                Some(target_id) => return Some((target_id, index)),
                None => (),
            }
        }

        None
    }

    fn file_path(&self, file_id: FileId) -> Option<VfsPath> {
        let db = self.as_dyn_database();
        lookup_file_path(db, InternedFileId::new(db, file_id))
    }

    fn file_id_for_path(&self, path: &VfsPath) -> Option<FileId> {
        let db = self.as_dyn_database();
        SourceRootTable::get(db)
            .roots(db)
            .values()
            .find_map(|source_root| source_root.file_for_path(db, path))
    }

    fn intern_file_path(&self, path: VfsPath) -> FileId {
        let db = self.as_dyn_database();
        let interned_path = InternedFilePath::new(db, path);
        FileId::from_raw(interned_path.as_id().index())
    }

    fn file_paths(&self) -> Vec<(FileId, VfsPath)> {
        let db = self.as_dyn_database();
        lookup_file_paths(db)
    }

    #[doc(hidden)]
    fn crates_map(&self) -> Arc<CratesMap>;

    fn nonce_and_revision(&self) -> (Nonce, salsa::Revision);

    fn line_column(&self, file: FileId, offset: TextSize) -> Result<(u32, u32), ()>;
}

#[salsa::tracked]
fn lookup_file_text(db: &dyn salsa::Database, file_id: InternedFileId<'_>) -> FileText {
    let file_id = file_id.id(db);
    match FileTextTable::get(db).texts(db).get(&file_id) {
        Some(text) => *text,
        None => panic!("Unable to fetch file text for `vfs::FileId`: {file_id:?}; this is a bug"),
    }
}

#[salsa::tracked]
fn lookup_file_path(db: &dyn salsa::Database, file_id: InternedFileId<'_>) -> Option<VfsPath> {
    let file_id = file_id.id(db);
    SourceRootTable::get(db)
        .roots(db)
        .values()
        .find_map(|source_root| source_root.path_for_file(db, &file_id).cloned())
}

#[salsa::tracked]
fn lookup_file_source_root(
    db: &dyn salsa::Database,
    file_id: InternedFileId<'_>,
) -> Option<SourceRootId> {
    let file_id = file_id.id(db);
    SourceRootTable::get(db).roots(db).iter().find_map(|(&source_root_id, source_root)| {
        source_root.path_for_file(db, &file_id).map(|_| source_root_id)
    })
}

#[salsa::tracked]
fn lookup_file_paths(db: &dyn salsa::Database) -> Vec<(FileId, VfsPath)> {
    SourceRootTable::get(db)
        .roots(db)
        .values()
        .flat_map(|source_root| {
            source_root
                .iter(db)
                .map(|file_id| {
                    let path = source_root
                        .path_for_file(db, &file_id)
                        .expect("source root file has no path")
                        .clone();
                    (file_id, path)
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

#[salsa::tracked]
fn lookup_resolve_path(db: &dyn salsa::Database, path: InternedAnchoredPath<'_>) -> Option<FileId> {
    let path = path.path(db);
    let source_root_id = lookup_file_source_root(db, InternedFileId::new(db, path.anchor))?;
    let source_root = SourceRootTable::get(db).roots(db).get(&source_root_id)?;
    source_root.resolve_path(db, AnchoredPath { anchor: path.anchor, path: &path.path })
}

fn ensure_file_text<DB: SourceDatabase + ?Sized>(db: &mut DB, file_id: FileId) -> FileText {
    match FileTextTable::get(db).texts(db).get(&file_id).copied() {
        Some(file_text) => file_text,
        None => {
            panic!("Unable to fetch file text for `vfs::FileId`: {file_id:?}; this is a bug")
        }
    }
}

pub(crate) struct FileTextRegistration {
    pub(crate) file_id: FileId,
    pub(crate) durability: Durability,
}

pub(crate) fn register_file_texts(
    db: &mut dyn SourceDatabase,
    registrations: impl IntoIterator<Item = FileTextRegistration>,
) {
    let table = FileTextTable::get(db);
    let mut texts = table.texts(db).clone();
    let mut changed = false;
    for FileTextRegistration { file_id, durability } in registrations {
        match texts.get(&file_id).copied() {
            Some(_) => (),
            None => {
                let file_text = FileText::builder(Arc::from(""), LineEndings::Unix)
                    .durability(durability)
                    .new(db);
                texts.insert(file_id, file_text);
                changed = true;
            }
        }
    }
    if changed {
        table.set_texts(db).to(texts);
    }
}

#[salsa::tracked]
fn lookup_source_root(
    db: &dyn salsa::Database,
    source_root_id: InternedSourceRootId<'_>,
) -> SourceRoot {
    let source_root_id = source_root_id.id(db);
    match SourceRootTable::get(db).roots(db).get(&source_root_id) {
        Some(source_root) => *source_root,
        None => panic!(
            "Unable to fetch `SourceRoot` with `SourceRootId` ({source_root_id:?}); this is a bug"
        ),
    }
}

static NEXT_NONCE: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Nonce(usize);

impl Default for Nonce {
    #[inline]
    fn default() -> Self {
        Nonce::new()
    }
}

impl Nonce {
    #[inline]
    pub fn new() -> Nonce {
        Nonce(NEXT_NONCE.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

/// Crate related data shared by the whole workspace.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CrateWorkspaceData {
    pub target: Result<target::TargetData, target::TargetLoadError>,
    /// Toolchain version used to compile the crate.
    pub toolchain: Option<Version>,
}

impl CrateWorkspaceData {
    pub fn is_atleast_187(&self) -> bool {
        const VERSION_187: Version = Version {
            major: 1,
            minor: 87,
            patch: 0,
            pre: Prerelease::EMPTY,
            build: BuildMetadata::EMPTY,
        };
        self.toolchain.as_ref().map_or(false, |v| *v >= VERSION_187)
    }
}

pub fn toolchain_channel(db: &dyn salsa::Database, krate: Crate) -> Option<ReleaseChannel> {
    krate.workspace_data(db).toolchain.as_ref().and_then(|v| ReleaseChannel::from_str(&v.pre))
}

#[salsa::input(singleton, debug)]
struct AllCrates {
    crates: std::sync::Arc<[Crate]>,
}

pub fn set_all_crates_with_durability(
    db: &mut dyn salsa::Database,
    crates: impl IntoIterator<Item = Crate>,
    durability: Durability,
) {
    AllCrates::try_get(db)
        .unwrap_or_else(|| AllCrates::new(db, std::sync::Arc::default()))
        .set_crates(db)
        .with_durability(durability)
        .to(crates.into_iter().collect());
}

/// Returns the crates in topological order.
///
/// **Warning**: do not use this query in `hir-*` crates! It kills incrementality across crate metadata modifications.
pub fn all_crates(db: &dyn salsa::Database) -> std::sync::Arc<[Crate]> {
    AllCrates::try_get(db).map_or(std::sync::Arc::default(), |all_crates| all_crates.crates(db))
}

#[doc(hidden)]
#[salsa::interned]
pub struct InternedSourceRootId {
    pub id: SourceRootId,
}

#[salsa::tracked(returns(deref))]
pub fn local_roots(db: &dyn SourceDatabase) -> Box<[SourceRootId]> {
    let mut roots = SourceRootTable::get(db)
        .roots(db)
        .iter()
        .filter_map(|(&source_root_id, source_root)| {
            (source_root.kind(db) == SourceRootKind::Local).then_some(source_root_id)
        })
        .collect::<Vec<_>>();
    roots.sort();
    roots.into_boxed_slice()
}

#[salsa::tracked(returns(deref))]
pub fn library_roots(db: &dyn SourceDatabase) -> Box<[SourceRootId]> {
    let mut roots = SourceRootTable::get(db)
        .roots(db)
        .iter()
        .filter_map(|(&source_root_id, source_root)| {
            (source_root.kind(db) == SourceRootKind::Library).then_some(source_root_id)
        })
        .collect::<Vec<_>>();
    roots.sort();
    roots.into_boxed_slice()
}

/// Crates whose root file is in `id`.
pub fn source_root_crates(db: &dyn SourceDatabase, id: SourceRootId) -> &[Crate] {
    #[salsa::tracked(returns(deref))]
    pub fn source_root_crates<'db>(
        db: &'db dyn SourceDatabase,
        id: InternedSourceRootId<'db>,
    ) -> Box<[Crate]> {
        let crates = AllCrates::get(db).crates(db);
        let id = id.id(db);
        crates
            .iter()
            .copied()
            .filter(|&krate| {
                let root_file = krate.data(db).root_file_id;
                db.file_source_root(root_file) == id
            })
            .collect()
    }
    source_root_crates(db, InternedSourceRootId::new(db, id))
}

pub fn relevant_crates(db: &dyn SourceDatabase, file_id: FileId) -> &[Crate] {
    let _p = tracing::info_span!("relevant_crates").entered();

    let source_root = db.file_source_root(file_id);
    source_root_crates(db, source_root)
}

#[must_use]
#[non_exhaustive]
pub struct DbPanicContext;

impl Drop for DbPanicContext {
    fn drop(&mut self) {
        Self::with_ctx(|ctx| assert!(ctx.pop().is_some()));
    }
}

impl DbPanicContext {
    pub fn enter(frame: String) -> DbPanicContext {
        #[expect(clippy::print_stderr, reason = "already panicking anyway")]
        fn set_hook() {
            let default_hook = panic::take_hook();
            panic::set_hook(Box::new(move |panic_info| {
                default_hook(panic_info);
                if let Some(backtrace) = salsa::Backtrace::capture() {
                    eprintln!("{backtrace:#}");
                }
                DbPanicContext::with_ctx(|ctx| {
                    if !ctx.is_empty() {
                        eprintln!("additional context:");
                        for (idx, frame) in ctx.iter().enumerate() {
                            eprintln!("{idx:>4}: {frame}\n");
                        }
                    }
                });
            }));
        }

        static SET_HOOK: Once = Once::new();
        SET_HOOK.call_once(set_hook);

        Self::with_ctx(|ctx| ctx.push(frame));
        DbPanicContext
    }

    fn with_ctx(f: impl FnOnce(&mut Vec<String>)) {
        thread_local! {
            static CTX: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
        }
        CTX.with(|ctx| f(&mut ctx.borrow_mut()));
    }
}
