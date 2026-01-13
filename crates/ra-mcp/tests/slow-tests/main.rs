//! Slow integration tests for `ra-mcp`.
//!
//! These tests are a separate target and run directly without `RUN_SLOW_TESTS`.
//! Run them with:
//!
//! `cargo test -p ra-mcp --test slow-tests`
//!
//! Slow-test checklist (protocol-level):
//! [x] tools_work_without_explicit_initialize
//! [x] tool_schema_surface_is_stable
//! [x] repeated_roots_list_changed_preserves_crate_graph
//! [x] database_reflects_apply_immediately
//! [x] reindex_links_new_module_after_apply (direct `Workspace`, not protocol)
//! [x] reload_picks_up_new_path_dependency (direct `Workspace`, not protocol)

use std::{
    collections::VecDeque,
    fs, io,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll, Waker},
};

use expect_test::expect;
use paths::{Utf8Path, Utf8PathBuf};
use ra_mcp_handlers::error::cancelled_in;
use ra_mcp_handlers::{Analysis, McpError, RaMcpServer, Workspace};
use rmcp::model::{CallToolRequestParams, ListRootsResult, Root};
use rmcp::{ClientHandler, ServiceExt};
use serde_json::json;
use temp_dir::TempDir;
use test_utils::skip_slow_tests;
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};

#[derive(Default)]
struct ByteChannelState {
    buffer: VecDeque<u8>,
    closed: bool,
    reader_waker: Option<Waker>,
}

#[derive(Clone)]
struct ChannelReader {
    state: Arc<Mutex<ByteChannelState>>,
}

#[derive(Clone)]
struct ChannelWriter {
    state: Arc<Mutex<ByteChannelState>>,
}

impl AsyncRead for ChannelReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let mut state = self.state.lock().expect("channel reader lock");
        if !state.buffer.is_empty() {
            let to_read = state.buffer.len().min(buf.remaining());
            let mut tmp = Vec::with_capacity(to_read);
            for _ in 0..to_read {
                tmp.push(state.buffer.pop_front().expect("buffer length was checked"));
            }
            buf.put_slice(&tmp);
            return Poll::Ready(Ok(()));
        }

        if state.closed {
            return Poll::Ready(Ok(()));
        }

        state.reader_waker = Some(cx.waker().clone());
        Poll::Pending
    }
}

impl AsyncWrite for ChannelWriter {
    fn poll_write(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        bytes: &[u8],
    ) -> Poll<io::Result<usize>> {
        let mut state = self.state.lock().expect("channel writer lock");
        if state.closed {
            return Poll::Ready(Err(io::Error::new(io::ErrorKind::BrokenPipe, "channel closed")));
        }

        state.buffer.extend(bytes.iter().copied());
        if let Some(waker) = state.reader_waker.take() {
            waker.wake();
        }
        Poll::Ready(Ok(bytes.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        let mut state = self.state.lock().expect("channel writer lock");
        state.closed = true;
        if let Some(waker) = state.reader_waker.take() {
            waker.wake();
        }
        Poll::Ready(Ok(()))
    }
}

impl Drop for ChannelWriter {
    fn drop(&mut self) {
        let mut state = self.state.lock().expect("channel writer lock");
        state.closed = true;
        if let Some(waker) = state.reader_waker.take() {
            waker.wake();
        }
    }
}

fn create_channel_pair() -> (ChannelWriter, ChannelReader, ChannelWriter, ChannelReader) {
    let client_to_server = Arc::new(Mutex::new(ByteChannelState::default()));
    let server_to_client = Arc::new(Mutex::new(ByteChannelState::default()));

    let client_writer = ChannelWriter { state: client_to_server.clone() };
    let server_reader = ChannelReader { state: client_to_server };

    let server_writer = ChannelWriter { state: server_to_client.clone() };
    let client_reader = ChannelReader { state: server_to_client };

    (client_writer, client_reader, server_writer, server_reader)
}

fn write_fixture_workspace(root: &Utf8Path) {
    let src = root.join("src");
    fs::create_dir_all(&src).expect("create fixture src dir");

    fs::write(
        root.join("Cargo.toml"),
        r#"[package]
name = "ra_mcp_slow_fixture"
version = "0.0.0"
edition = "2024"
"#,
    )
    .expect("write fixture Cargo.toml");

    fs::write(
        src.join("lib.rs"),
        r#"pub mod left;
pub mod right;
pub mod other;

#[derive(Clone)]
pub struct One;
pub struct Two;

pub fn compute(x: i32) -> i32 {
    x + 1
}

pub fn call_compute() -> i32 {
    compute(1)
}

pub fn uses_one(input: One) -> One {
    input
}
"#,
    )
    .expect("write fixture lib.rs");

    fs::write(src.join("left.rs"), "pub struct Dup;\n").expect("write fixture left.rs");
    fs::write(src.join("right.rs"), "pub struct Dup;\n").expect("write fixture right.rs");
    fs::write(src.join("other.rs"), "pub const SCOPE_ONLY: i32 = 1;\n")
        .expect("write fixture other.rs");
}

/// Grep-shaped result lines, i.e. everything except the trailing `[handle N]`
/// / `[K more; …]` line and notes.
fn result_lines(text: &str) -> Vec<&str> {
    let mut lines = Vec::new();
    for line in text.lines() {
        if line.starts_with('[') || line.starts_with("note:") {
            continue;
        }
        lines.push(line);
    }
    lines
}

/// Parses `path:line:col` from the first result line containing `needle`.
fn first_loc(result: &str, needle: &str) -> (String, u32, u32) {
    let line = result
        .lines()
        .find(|line| line.contains(needle))
        .unwrap_or_else(|| panic!("no result line containing '{needle}' in:\n{result}"));
    let mut parts = line.splitn(4, ':');
    let path = parts.next().expect("result line should start with a path").to_owned();
    let line_no =
        parts.next().and_then(|s| s.parse().ok()).expect("result line should have a line number");
    let col_no =
        parts.next().and_then(|s| s.parse().ok()).expect("result line should have a column number");
    (path, line_no, col_no)
}

struct McpSlowHarness {
    client: rmcp::service::RunningService<rmcp::RoleClient, TestClientHandler>,
    _server_handle: tokio::task::JoinHandle<anyhow::Result<()>>,
    _fixture_dir: TempDir,
    _workspace_root: Utf8PathBuf,
}

struct TestClientHandler {
    root_uri: String,
}

impl ClientHandler for TestClientHandler {
    async fn list_roots(
        &self,
        _context: rmcp::service::RequestContext<rmcp::RoleClient>,
    ) -> Result<ListRootsResult, rmcp::ErrorData> {
        Ok(ListRootsResult {
            roots: vec![Root { uri: self.root_uri.clone(), name: Some("fixture".to_owned()) }],
        })
    }
}

impl McpSlowHarness {
    async fn new() -> Self {
        let fixture_dir =
            TempDir::with_prefix("ra-mcp-slow-tests").expect("create fixture temp dir");
        let workspace_root = Utf8PathBuf::from_path_buf(fixture_dir.path().to_path_buf())
            .unwrap_or_else(|path| panic!("fixture path should be utf8, got {}", path.display()));
        write_fixture_workspace(&workspace_root);
        let root_uri = format!("file://{}", workspace_root.as_str());

        let (client_write, client_read, server_write, server_read) = create_channel_pair();
        let server_handle = tokio::spawn(async move {
            let server = RaMcpServer::new();
            let service = server.serve((server_read, server_write)).await?;
            service.waiting().await?;
            anyhow::Ok(())
        });
        let client = TestClientHandler { root_uri }
            .serve((client_read, client_write))
            .await
            .expect("client should connect");

        Self {
            client,
            _server_handle: server_handle,
            _fixture_dir: fixture_dir,
            _workspace_root: workspace_root,
        }
    }

    async fn find_types(&self, query: &str) -> String {
        self.call_tool_ok(
            "find_symbol",
            Some(json!({ "query": query, "mode": "exact", "onlyTypes": true, "limit": 20 })),
        )
        .await
    }

    async fn call_tool_ok(&self, name: &str, arguments: Option<serde_json::Value>) -> String {
        let arguments = arguments.and_then(|v| v.as_object().cloned());
        let name = name.to_owned();
        let result = self
            .client
            .call_tool(CallToolRequestParams {
                meta: None,
                name: name.into(),
                arguments,
                task: None,
            })
            .await
            .expect("tool call should succeed");
        assert!(!result.is_error.unwrap_or(false), "tool call unexpectedly returned error");
        let structured =
            result.structured_content.as_ref().expect("tool result should have structuredContent");
        assert!(structured.is_object(), "structuredContent should be a JSON object");
        result.content[0].as_text().expect("result should be text").text.clone()
    }
}

#[tokio::test]
async fn tools_work_without_explicit_initialize() {
    if skip_slow_tests() {
        return;
    }
    let harness = McpSlowHarness::new().await;
    let symbols = harness
        .call_tool_ok(
            "find_symbol",
            Some(json!({
                "query": "One",
                "mode": "exact",
                "onlyTypes": true,
                "limit": 20
            })),
        )
        .await;
    expect!["src/lib.rs:6:12: struct One: pub struct One"].assert_eq(&symbols);
}

#[tokio::test]
async fn find_symbol_include_libs_keeps_workspace_symbols() {
    if skip_slow_tests() {
        return;
    }
    let harness = McpSlowHarness::new().await;
    let symbols = harness
        .call_tool_ok(
            "find_symbol",
            Some(json!({
                "query": "One",
                "mode": "exact",
                "onlyTypes": true,
                "includeLibs": true,
                "limit": 20
            })),
        )
        .await;
    expect!["src/lib.rs:6:12: struct One: pub struct One"].assert_eq(&symbols);
}

#[tokio::test]
async fn repeated_roots_list_changed_preserves_crate_graph() {
    if skip_slow_tests() {
        return;
    }
    let harness = McpSlowHarness::new().await;

    let first_symbols = harness
        .call_tool_ok(
            "find_symbol",
            Some(json!({
                "query": "One",
                "mode": "exact",
                "onlyTypes": true,
                "limit": 20
            })),
        )
        .await;
    expect!["src/lib.rs:6:12: struct One: pub struct One"].assert_eq(&first_symbols);

    harness.client.notify_roots_list_changed().await.expect("roots/list_changed should succeed");

    let second_symbols = harness
        .call_tool_ok(
            "find_symbol",
            Some(json!({
                "query": "One",
                "mode": "exact",
                "onlyTypes": true,
                "limit": 20
            })),
        )
        .await;
    assert_eq!(
        result_lines(&first_symbols),
        result_lines(&second_symbols),
        "symbol results should remain stable across roots/list_changed"
    );
}

#[tokio::test]
async fn tool_schema_surface_is_stable() {
    if skip_slow_tests() {
        return;
    }
    let harness = McpSlowHarness::new().await;
    let tools =
        harness.client.list_tools(Default::default()).await.expect("list_tools should succeed");

    let mut tools = tools.tools;
    tools.sort_by(|a, b| a.name.cmp(&b.name));

    let mut lines = Vec::new();
    for tool in &tools {
        let mut props = Vec::new();
        if let Some(properties) = tool.input_schema.get("properties").and_then(|it| it.as_object())
        {
            for key in properties.keys() {
                props.push(key.as_str());
            }
        }
        props.sort();
        let input = if props.is_empty() { "-".to_owned() } else { props.join(", ") };

        let output_schema = tool
            .output_schema
            .as_ref()
            .unwrap_or_else(|| panic!("tool {} should advertise an outputSchema", tool.name));
        assert_eq!(
            output_schema.get("type").and_then(|it| it.as_str()),
            Some("object"),
            "tool {} outputSchema should have root type object",
            tool.name
        );
        let output = output_schema_summary(output_schema);
        lines.push(format!("{}: input [{}]; output [{}]", tool.name, input, output));
    }

    fn output_schema_summary(schema: &serde_json::Map<String, serde_json::Value>) -> String {
        let defs = schema.get("$defs").and_then(|it| it.as_object());
        output_schema_value_summary(&serde_json::Value::Object(schema.clone()), defs)
    }

    fn output_schema_value_summary(
        schema: &serde_json::Value,
        defs: Option<&serde_json::Map<String, serde_json::Value>>,
    ) -> String {
        if let Some(name) = schema_ref_name(schema) {
            let Some(resolved) = defs.and_then(|defs| defs.get(name)) else {
                return name.to_owned();
            };
            let summary = output_schema_value_summary(resolved, defs);
            return if name == "ToolRejection" { format!("rejection: {summary}") } else { summary };
        }
        if let Some(any_of) = schema.get("anyOf").and_then(|it| it.as_array()) {
            let variants = any_of
                .iter()
                .map(|it| output_schema_value_summary(it, defs))
                .collect::<Vec<_>>()
                .join(" | ");
            return format!("anyOf {variants}");
        }
        if let Some(one_of) = schema.get("oneOf").and_then(|it| it.as_array()) {
            let variants = one_of
                .iter()
                .map(|it| output_variant_summary(it, defs))
                .collect::<Vec<_>>()
                .join(" | ");
            return format!("oneOf {variants}");
        }

        property_summary(schema)
    }

    fn output_variant_summary(
        schema: &serde_json::Value,
        defs: Option<&serde_json::Map<String, serde_json::Value>>,
    ) -> String {
        if let Some(name) = schema_ref_name(schema) {
            let Some(resolved) = defs.and_then(|defs| defs.get(name)) else {
                return name.to_owned();
            };
            return output_variant_summary(resolved, defs);
        }
        let Some(properties) = schema.get("properties").and_then(|it| it.as_object()) else {
            return "?".to_owned();
        };
        let tag = properties.get("kind").or_else(|| properties.get("status"));
        let kind = tag.and_then(schema_tag_value).unwrap_or("?");
        let mut props = Vec::new();
        for key in properties.keys() {
            if key != "kind" && key != "status" {
                props.push(key.as_str());
            }
        }
        if let Some(all_of) = schema.get("allOf").and_then(|it| it.as_array()) {
            for item in all_of {
                if let Some(name) = item
                    .get("$ref")
                    .and_then(|it| it.as_str())
                    .and_then(|it| it.strip_prefix("#/$defs/"))
                {
                    props.push(name);
                }
            }
        }
        props.sort();
        if props.is_empty() { kind.to_owned() } else { format!("{kind}: {}", props.join(", ")) }
    }

    fn property_summary(schema: &serde_json::Value) -> String {
        let mut props = Vec::new();
        if let Some(properties) = schema.get("properties").and_then(|it| it.as_object()) {
            for key in properties.keys() {
                props.push(key.as_str());
            }
        }
        props.sort();
        props.join(", ")
    }

    fn schema_ref_name(schema: &serde_json::Value) -> Option<&str> {
        schema.get("$ref").and_then(|it| it.as_str()).and_then(|it| it.strip_prefix("#/$defs/"))
    }

    fn schema_tag_value(schema: &serde_json::Value) -> Option<&str> {
        schema
            .get("const")
            .and_then(|it| it.as_str())
            .or_else(|| schema.get("enum")?.as_array()?.first()?.as_str())
    }
    let surface = lines.join("\n");
    expect![[r#"
        add_argument: input [apply, argumentIndex, column, file, line, placeholder]; output [anyOf oneOf preview: edits, skipped, total_edits | applied: edits, edits_applied, files_changed, skipped | rejection: code, guidance, message]
        expand_macro: input [column, file, line]; output [anyOf expansion, name | rejection: code, guidance, message]
        find_symbol: input [assocMode, caseSensitive, excludeImports, includeLibs, limit, mode, offset, onlyTypes, path, query]; output [anyOf more, offset, symbols | rejection: code, guidance, message]
        inlay_hints: input [file, range]; output [anyOf hints | rejection: code, guidance, message]
        inspect: input [column, file, line]; output [anyOf definitions, metadata | rejection: code, guidance, message]
        reachable: input [column, depth, direction, dispatch, edgeKinds, file, line, nodeKinds, path, range, referenceCategories, scope, target, usageKinds]; output [anyOf depth, depths, direction, edge_kinds, edges, nodes, note, roots, scope | rejection: code, guidance, message]
        read: input [file, range]; output [anyOf file_path, range, text | rejection: code, guidance, message]
        rename: input [column, file, line, newName]; output [anyOf oneOf ok: edits, edits_applied, file_system_edits, files_changed | rejected: reason | rejection: code, guidance, message]
        search: input [countBy, filePattern, files, inAsync, inFunction, inImpl, inTest, inUnsafe, limit, offset, pattern, usageKind]; output [anyOf oneOf matches: matches, offset, total_matches | counts: group_by, groups, total_matches | rejection: code, guidance, message]
        ssr: input [apply, files, rule]; output [anyOf oneOf preview: edits | applied: edits, edits_applied, files_changed | rejection: code, guidance, message]
        workspace_crates: input [-]; output [anyOf crates | rejection: code, guidance, message]"#]].assert_eq(&surface);
}

#[tokio::test]
async fn database_reflects_apply_immediately() {
    if skip_slow_tests() {
        return;
    }
    let harness = McpSlowHarness::new().await;

    let symbols = harness.find_types("Two").await;
    expect!["src/lib.rs:7:12: struct Two: pub struct Two"].assert_eq(&symbols);
    expect!["no matching symbols; try mode=fuzzy, a shorter query, or includeLibs=true"]
        .assert_eq(&harness.find_types("TwoDatabaseImmediate").await);

    let (file_path, line, column) = first_loc(&symbols, "struct Two");

    let result = harness
        .call_tool_ok(
            "rename",
            Some(json!({
                "file": file_path,
                "line": line,
                "column": column,
                "newName": "TwoDatabaseImmediate",
            })),
        )
        .await;
    expect![[r#"
        applied 1 edits in 1 files
        src/lib.rs:7:12: => TwoDatabaseImmediate"#]]
    .assert_eq(&result);

    // The database must reflect the rename immediately: the new name resolves,
    // and the old one is gone.
    expect!["src/lib.rs:7:12: struct TwoDatabaseImmediate: pub struct TwoDatabaseImmediate"]
        .assert_eq(&harness.find_types("TwoDatabaseImmediate").await);
    expect!["no matching symbols; try mode=fuzzy, a shorter query, or includeLibs=true"]
        .assert_eq(&harness.find_types("Two").await);
}

/// Sorted display names of every crate in the graph, sysroot included —
/// assertions state concretely what the workspace resolves against.
async fn crate_names(workspace: &Workspace) -> String {
    workspace
        .run_snapshot(|snapshot| {
            let db = snapshot.raw_database();
            let mut names: Vec<String> = ide_db::base_db::all_crates(db)
                .iter()
                .filter_map(|&krate| {
                    krate
                        .extra_data(db)
                        .display_name
                        .as_ref()
                        .map(|it| it.canonical_name().as_str().to_owned())
                })
                .collect();
            names.sort();
            names.dedup();
            Ok::<_, McpError>(names.join("\n"))
        })
        .await
        .expect("crate name query should succeed")
}

fn write_reindex_workspace(root: &Utf8Path) {
    let src = root.join("src");
    fs::create_dir_all(&src).expect("create fixture src dir");
    fs::write(
        root.join("Cargo.toml"),
        "[package]\nname = \"ra_mcp_reindex_fixture\"\nversion = \"0.0.0\"\nedition = \"2021\"\n",
    )
    .expect("write fixture Cargo.toml");
    fs::write(src.join("lib.rs"), "pub fn root_fn() {}\n").expect("write fixture lib.rs");
}

/// Exercises the structural reindex directly through `Workspace` (no MCP
/// transport): after `apply_file_changes` adds a new module file and its `mod`
/// declaration, the file must be linked into the crate — which only happens if
/// the reindex rebuilt the source roots, not just the file text.
#[tokio::test]
async fn reindex_links_new_module_after_apply() {
    if skip_slow_tests() {
        return;
    }

    let fixture_dir = TempDir::with_prefix("ra-mcp-reindex").expect("create fixture temp dir");
    let root = Utf8PathBuf::from_path_buf(fixture_dir.path().to_path_buf())
        .unwrap_or_else(|path| panic!("fixture path should be utf8, got {}", path.display()));
    write_reindex_workspace(&root);

    let analysis = Analysis::new();
    let root_path = root.clone().into_std_path_buf();
    let workspace = tokio::task::spawn_blocking(move || analysis.workspace(vec![root_path]))
        .await
        .expect("workspace load task should join")
        .expect("workspace should load");

    let foo_path = root.join("src/foo.rs").into_std_path_buf();
    let lib_path = root.join("src/lib.rs").into_std_path_buf();
    workspace.apply_file_changes(vec![
        (foo_path.clone(), "pub fn hello() {}\n".to_owned()),
        (lib_path, "pub fn root_fn() {}\npub mod foo;\n".to_owned()),
    ]);

    let owners = workspace
        .run_snapshot(move |snapshot| {
            let file_id = snapshot.resolve_file_id(&foo_path)?;
            let crates =
                snapshot.analysis().crates_for(file_id).map_err(cancelled_in("crates_for"))?;
            let db = snapshot.raw_database();
            let mut names: Vec<String> = crates
                .iter()
                .filter_map(|&krate| {
                    krate
                        .extra_data(db)
                        .display_name
                        .as_ref()
                        .map(|it| it.canonical_name().as_str().to_owned())
                })
                .collect();
            names.sort();
            Ok::<_, McpError>(names)
        })
        .await
        .expect("snapshot query should succeed");

    assert_eq!(owners, ["ra_mcp_reindex_fixture"], "foo.rs should be linked after the reindex");
}

/// Exercises a full crate-graph reload: after a path dependency is added to
/// `Cargo.toml`, a reload must re-run metadata and pull the new crate into the
/// graph. Uses a synchronous reload (not the async actor / file watcher) so the
/// assertion is deterministic; a local path dep keeps it hermetic (no network).
#[tokio::test]
async fn reload_picks_up_new_path_dependency() {
    if skip_slow_tests() {
        return;
    }

    let fixture_dir = TempDir::with_prefix("ra-mcp-reload").expect("create fixture temp dir");
    let root = Utf8PathBuf::from_path_buf(fixture_dir.path().to_path_buf())
        .unwrap_or_else(|path| panic!("fixture path should be utf8, got {}", path.display()));
    write_reindex_workspace(&root);

    let analysis = Analysis::new();
    let root_path = root.clone().into_std_path_buf();
    let workspace = tokio::task::spawn_blocking(move || analysis.workspace(vec![root_path]))
        .await
        .expect("workspace load task should join")
        .expect("workspace should load");

    expect![[r#"
        alloc
        cfg_if
        compiler_builtins
        core
        foldhash
        getopts
        hashbrown
        libc
        panic_abort
        panic_unwind
        proc_macro
        ra_mcp_reindex_fixture
        rand
        rand_core
        rand_xorshift
        rustc_demangle
        rustc_literal_escaper
        std
        std_detect
        test
        unwind"#]]
    .assert_eq(&crate_names(&workspace).await);

    // Add a local `bar` crate on disk and depend on it from the root package.
    // `bar` deliberately has a non-root module: the crate-graph rebuild only
    // load_syncs crate roots, so `extra.rs` reaches the database solely through
    // the post-reload watcher rescan.
    let bar_src = root.join("bar/src");
    fs::create_dir_all(&bar_src).expect("create bar src dir");
    fs::write(
        root.join("bar/Cargo.toml"),
        "[package]\nname = \"bar\"\nversion = \"0.0.0\"\nedition = \"2021\"\n",
    )
    .expect("write bar Cargo.toml");
    fs::write(bar_src.join("lib.rs"), "pub mod extra;\npub fn bar_fn() {}\n")
        .expect("write bar lib.rs");
    fs::write(bar_src.join("extra.rs"), "pub fn extra_fn() {}\n").expect("write bar extra.rs");
    fs::write(
        root.join("Cargo.toml"),
        "[package]\nname = \"ra_mcp_reindex_fixture\"\nversion = \"0.0.0\"\nedition = \"2021\"\n\n[dependencies]\nbar = { path = \"bar\" }\n",
    )
    .expect("rewrite root Cargo.toml with dependency");

    let reload = workspace.clone();
    tokio::task::spawn_blocking(move || reload.reload_blocking())
        .await
        .expect("reload task should join");

    expect![[r#"
        alloc
        bar
        cfg_if
        compiler_builtins
        core
        foldhash
        getopts
        hashbrown
        libc
        panic_abort
        panic_unwind
        proc_macro
        ra_mcp_reindex_fixture
        rand
        rand_core
        rand_xorshift
        rustc_demangle
        rustc_literal_escaper
        std
        std_detect
        test
        unwind"#]]
    .assert_eq(&crate_names(&workspace).await);

    // The non-root module must be loaded and linked once the reload returns —
    // this only holds if the reload reconfigured the watcher and waited for
    // the resulting rescan.
    let extra_path = root.join("bar/src/extra.rs").into_std_path_buf();
    let owners = workspace
        .run_snapshot(move |snapshot| {
            let file_id = snapshot.resolve_file_id(&extra_path)?;
            let crates =
                snapshot.analysis().crates_for(file_id).map_err(cancelled_in("crates_for"))?;
            let db = snapshot.raw_database();
            let mut names: Vec<String> = crates
                .iter()
                .filter_map(|&krate| {
                    krate
                        .extra_data(db)
                        .display_name
                        .as_ref()
                        .map(|it| it.canonical_name().as_str().to_owned())
                })
                .collect();
            names.sort();
            Ok::<_, McpError>(names)
        })
        .await
        .expect("bar/src/extra.rs should be in the database after reload");
    assert_eq!(owners, ["bar"], "bar/src/extra.rs should belong to bar after reload");
}
