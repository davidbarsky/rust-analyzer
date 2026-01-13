use std::fs;
use std::sync::{Arc, Mutex};

use expect_test::expect;

use paths::{Utf8Path, Utf8PathBuf};
use ra_mcp_handlers::RaMcpServer;
use rmcp::model::{CallToolRequestParams, CallToolResult, ListRootsResult, Root};
use rmcp::{ClientHandler, ServiceExt};
use serde_json::json;
use temp_dir::TempDir;
use test_utils::skip_slow_tests;

const FIXTURE_PACKAGE: &str = "ra_mcp_test_fixture";

fn write_fixture_workspace(root: &Utf8Path, package_name: &str) {
    let src = root.join("src");
    fs::create_dir_all(&src).expect("create fixture src dir");

    fs::write(
        root.join("Cargo.toml"),
        format!(
            r#"[package]
name = "{package_name}"
version = "0.0.0"
edition = "2021"
"#,
        ),
    )
    .expect("write fixture Cargo.toml");

    fs::write(
        src.join("lib.rs"),
        r#"pub mod types;

pub trait ServerHandler {
    fn run(&self) -> u32;
}

#[derive(Clone, Debug)]
pub struct Analysis {
    pub value: u32,
}

impl Analysis {
    pub fn new(value: u32) -> Self {
        Self { value }
    }
}

impl ServerHandler for Analysis {
    fn run(&self) -> u32 {
        let local = self.value;
        local
    }
}

#[derive(Debug, Clone)]
pub enum McpError {
    NotFound,
    InvalidRequest,
}

pub fn uses_analysis(a: Analysis) -> u32 {
    let inferred = Analysis::new(1);
    inferred.value + a.run()
}

pub enum Payload {
    StructLike {
        code: u32,
        label: String,
    },
    TupleLike(u32, String),
}
"#,
    )
    .expect("write fixture lib.rs");

    fs::write(
        src.join("types.rs"),
        r#"use crate::{Analysis, McpError};

#[derive(Debug, Clone)]
pub struct MacroCarrier {
    pub error: McpError,
}

pub fn make_error() -> McpError {
    McpError::NotFound
}

pub fn make_analysis() -> Analysis {
    Analysis::new(1)
}

macro_rules! carrier {
    ($name:ident) => {
        pub fn $name() -> u32 {
            1
        }
    };
}

carrier!(generated_carrier);
"#,
    )
    .expect("write fixture types.rs");
}

/// Parses `path:line:col` from the first grep-shaped result line containing
/// `needle`. Line/column are 1-based, path is workspace-relative — exactly the
/// form other tools accept back.
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

/// A helper struct that holds both server and client services for testing.
struct McpTestHarness {
    client: rmcp::service::RunningService<rmcp::RoleClient, TestClientHandler>,
    _server_handle: tokio::task::JoinHandle<anyhow::Result<()>>,
    _fixture_dirs: Vec<TempDir>,
    workspace_root: Utf8PathBuf,
    root_uri: Arc<Mutex<String>>,
}

struct TestClientHandler {
    root_uri: Arc<Mutex<String>>,
}

impl ClientHandler for TestClientHandler {
    async fn list_roots(
        &self,
        _context: rmcp::service::RequestContext<rmcp::RoleClient>,
    ) -> Result<ListRootsResult, rmcp::ErrorData> {
        let root_uri = self.root_uri.lock().unwrap().clone();
        Ok(ListRootsResult {
            roots: vec![Root { uri: root_uri, name: Some("fixture".to_owned()) }],
        })
    }
}

impl McpTestHarness {
    async fn new() -> Self {
        let fixture_dir =
            TempDir::with_prefix("ra-mcp-integration").expect("create fixture temp dir");
        let workspace_root = Utf8PathBuf::from_path_buf(fixture_dir.path().to_path_buf())
            .unwrap_or_else(|path| panic!("fixture path should be utf8, got {}", path.display()));
        write_fixture_workspace(&workspace_root, FIXTURE_PACKAGE);
        let root_uri = Arc::new(Mutex::new(format!("file://{}", workspace_root.as_str())));

        let (server_transport, client_transport) = tokio::io::duplex(65536);

        let server_handle = tokio::spawn(async move {
            let server = RaMcpServer::new();
            let service = server.serve(server_transport).await?;
            service.waiting().await?;
            anyhow::Ok(())
        });

        let client = TestClientHandler { root_uri: root_uri.clone() }
            .serve(client_transport)
            .await
            .expect("client should connect");

        Self {
            client,
            _server_handle: server_handle,
            _fixture_dirs: vec![fixture_dir],
            workspace_root,
            root_uri,
        }
    }

    fn test_file_path(&self) -> Utf8PathBuf {
        self.workspace_root.join("src/lib.rs")
    }

    fn select_workspace_root(&mut self, workspace_root: Utf8PathBuf) {
        *self.root_uri.lock().unwrap() = format!("file://{}", workspace_root.as_str());
        self.workspace_root = workspace_root;
    }

    fn add_workspace_root(&mut self, fixture_dir: TempDir, workspace_root: Utf8PathBuf) {
        self.select_workspace_root(workspace_root);
        self._fixture_dirs.push(fixture_dir);
    }

    async fn call_tool_result(
        &self,
        name: &str,
        arguments: Option<serde_json::Value>,
    ) -> CallToolResult {
        let arguments = arguments.and_then(|v| v.as_object().cloned());
        let name = name.to_owned();
        self.client
            .call_tool(CallToolRequestParams {
                meta: None,
                name: name.into(),
                arguments,
                task: None,
            })
            .await
            .expect("tool call should succeed")
    }

    async fn call_tool(&self, name: &str, arguments: Option<serde_json::Value>) -> String {
        let result = self.call_tool_result(name, arguments).await;

        assert!(!result.is_error.unwrap_or(false), "tool call should not return error");
        assert!(!result.content.is_empty(), "tool result should have content");
        let structured =
            result.structured_content.as_ref().expect("tool result should have structuredContent");
        assert!(structured.is_object(), "structuredContent should be a JSON object");

        result.content[0].as_text().expect("content should be text").text.clone()
    }
}

async fn get_harness() -> McpTestHarness {
    McpTestHarness::new().await
}

#[tokio::test]
async fn test_workspace_crates() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness.call_tool("workspace_crates", None).await;
    expect!["ra_mcp_test_fixture 0.0.0 src/lib.rs"].assert_eq(&result);
}

#[tokio::test]
async fn test_client_root_change_uses_cached_workspaces() {
    if skip_slow_tests() {
        return;
    }
    let mut harness = get_harness().await;
    let first_root = harness.workspace_root.clone();
    let result = harness.call_tool("workspace_crates", None).await;
    expect!["ra_mcp_test_fixture 0.0.0 src/lib.rs"].assert_eq(&result);

    let fixture_dir =
        TempDir::with_prefix("ra-mcp-integration-second").expect("create fixture temp dir");
    let workspace_root = Utf8PathBuf::from_path_buf(fixture_dir.path().to_path_buf())
        .unwrap_or_else(|path| panic!("fixture path should be utf8, got {}", path.display()));
    write_fixture_workspace(&workspace_root, "ra_mcp_second_fixture");
    harness.add_workspace_root(fixture_dir, workspace_root);

    let result = harness.call_tool("workspace_crates", None).await;
    expect!["ra_mcp_second_fixture 0.0.0 src/lib.rs"].assert_eq(&result);

    harness.select_workspace_root(first_root);
    let result = harness.call_tool("workspace_crates", None).await;
    expect!["ra_mcp_test_fixture 0.0.0 src/lib.rs"].assert_eq(&result);
}

#[tokio::test]
async fn test_find_symbol_fuzzy() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "find_symbol",
            Some(json!({
                "query": "Analysis",
                "mode": "fuzzy",
                "limit": 50
            })),
        )
        .await;

    expect!["src/lib.rs:8:12: struct Analysis: pub struct Analysis"].assert_eq(&result);
}

#[tokio::test]
async fn test_invalid_search_pattern_returns_rejection_result() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool_result(
            "search",
            Some(json!({
                "pattern": "foo($($args))",
            })),
        )
        .await;

    assert!(
        !result.is_error.unwrap_or(false),
        "invalid request should be returned as a retryable rejection result"
    );
    let text = result.content[0].as_text().expect("content should be text").text.clone();
    expect![[r#"
        ra-mcp rejected this request
        reason: invalid_params
        message: Invalid SSR pattern: Parse error: Placeholders should be $name, $'lifetime, or ${name:constraints}
        next: Adjust the arguments and rerun. For symbol navigation, prefer find_symbol, inspect, or reachable; use search only for valid rust-analyzer structural-search patterns."#]]
    .assert_eq(&text);

    let structured = result.structured_content.expect("rejection should have structuredContent");
    assert_eq!(structured["code"], "invalid_params");
    assert_eq!(
        structured["message"],
        "Invalid SSR pattern: Parse error: Placeholders should be $name, $'lifetime, or ${name:constraints}",
    );
    assert!(structured["guidance"].as_str().expect("guidance should be string").contains("search"));
}

#[tokio::test]
async fn test_find_symbol_exact() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "find_symbol",
            Some(json!({
                "query": "McpError",
                "mode": "exact",
                "limit": 50
            })),
        )
        .await;

    expect!["src/lib.rs:26:10: enum McpError: pub enum McpError"].assert_eq(&result);
}

#[tokio::test]
async fn test_find_symbol_prefix() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "find_symbol",
            Some(json!({
                "query": "Mcp",
                "mode": "prefix",
                "limit": 50
            })),
        )
        .await;

    expect!["src/lib.rs:26:10: enum McpError: pub enum McpError"].assert_eq(&result);
}

#[tokio::test]
async fn test_find_symbol_only_types() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "find_symbol",
            Some(json!({
                "query": "Analysis",
                "mode": "exact",
                "onlyTypes": true,
                "limit": 50
            })),
        )
        .await;

    expect!["src/lib.rs:8:12: struct Analysis: pub struct Analysis"].assert_eq(&result);
}

#[tokio::test]
async fn test_inspect() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    // `pub mod types;` at 1:9 resolves to the types module. Inspect keeps the
    // location grep-shaped and renders rich metadata as labeled Markdown sections.
    let result = harness
        .call_tool(
            "inspect",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 1,
                "column": 9
            })),
        )
        .await;

    expect![[r#"
        src/types.rs:1:1: definition mod types

        qualified_path:
        ```rust
        ra_mcp_test_fixture
        ```

        declaration:
        ```rust
        pub mod types
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_inspect_from_derive_attribute_uses_following_item() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "inspect",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 7,
                "column": 12
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:8:12: definition struct Analysis

        qualified_path:
        ```rust
        ra_mcp_test_fixture
        ```

        declaration:
        ```rust
        #[derive(Clone, Debug)]
        pub struct Analysis {
            pub value: u32,
        }
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_inspect_function_declaration_includes_body() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "inspect",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 13,
                "column": 12
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:13:12: definition fn new in Analysis

        qualified_path:
        ```rust
        ra_mcp_test_fixture::Analysis
        ```

        declaration:
        ```rust
        pub fn new(value: u32) -> Self {
                Self { value }
            }
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_inspect_variant_declaration_marks_focused_variant() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "inspect",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 27,
                "column": 5
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:27:5: definition variant NotFound

        qualified_path:
        ```rust
        ra_mcp_test_fixture::McpError
        ```

        declaration:
        ```rust
        #[derive(Debug, Clone)]
        pub enum McpError {
            /* focus:start */NotFound/* focus:end */,
            InvalidRequest,
        }
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_inspect_field_declaration_marks_focused_field() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "inspect",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 9,
                "column": 9
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:9:9: definition field value

        qualified_path:
        ```rust
        ra_mcp_test_fixture::Analysis
        ```

        declaration:
        ```rust
        #[derive(Clone, Debug)]
        pub struct Analysis {
            /* focus:start */pub value: u32/* focus:end */,
        }
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_inspect_multiline_variant_declaration_marks_full_variant() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "inspect",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 37,
                "column": 5
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:37:5: definition variant StructLike

        qualified_path:
        ```rust
        ra_mcp_test_fixture::Payload
        ```

        declaration:
        ```rust
        pub enum Payload {
            /* focus:start */StructLike {
                code: u32,
                label: String,
            }/* focus:end */,
            TupleLike(u32, String),
        }
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_read_range_returns_raw_source_text() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool_result(
            "read",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "range": {
                    "startLine": 13,
                    "startCol": 1,
                    "endLine": 16,
                    "endCol": 2,
                },
            })),
        )
        .await;

    assert!(!result.is_error.unwrap_or(false), "tool call should not return error");
    let text = result.content[0].as_text().expect("content should be text").text.clone();
    expect![[r#"
        src/lib.rs:13:1: read

            pub fn new(value: u32) -> Self {
                Self { value }
            }"#]]
    .assert_eq(&text);

    let structured = result.structured_content.expect("read should have structuredContent");
    assert_eq!(structured["file_path"], harness.test_file_path().as_str());
    assert_eq!(structured["range"]["start_line"], 13);
    assert_eq!(structured["range"]["start_col"], 1);
    assert_eq!(structured["range"]["end_line"], 16);
    assert_eq!(structured["range"]["end_col"], 2);
    assert_eq!(
        structured["text"],
        "    pub fn new(value: u32) -> Self {\n        Self { value }\n    }",
    );
}

#[tokio::test]
async fn test_read_range_clamps_end_outside_file() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "read",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "range": {
                    "startLine": 37,
                    "startCol": 1,
                    "endLine": 999,
                    "endCol": 1,
                },
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:37:1: read

        pub enum Payload {
            StructLike {
                code: u32,
                label: String,
            },
            TupleLike(u32, String),
        }"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_reachable_incoming_calls() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "reachable",
            Some(json!({
                "file": harness.test_file_path().as_str(),
                "line": 13,
                "column": 12
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:13:12: reachable incoming call depth=1 root #0 fn new
        src/lib.rs:32:30: d1 call #1 fn uses_analysis -> #0 fn new: let inferred = Analysis::new(1);
        src/types.rs:13:15: d1 call #2 fn make_analysis -> #0 fn new: Analysis::new(1)"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_reachable_outgoing_calls() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let symbol_result = harness
        .call_tool(
            "find_symbol",
            Some(json!({
                "query": "uses_analysis",
                "mode": "exact",
                "limit": 1
            })),
        )
        .await;
    let (file_path, line, col) = first_loc(&symbol_result, "fn uses_analysis");

    let result = harness
        .call_tool(
            "reachable",
            Some(json!({
                "file": file_path,
                "line": line,
                "column": col,
                "direction": "outgoing"
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:31:8: reachable outgoing call depth=1 root #0 fn uses_analysis
        src/lib.rs:32:30: d1 call #0 fn uses_analysis -> #1 fn new: let inferred = Analysis::new(1);
        src/lib.rs:33:24: d1 call #0 fn uses_analysis -> #2 fn run: inferred.value + a.run()"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_inlay_hints() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "inlay_hints",
            Some(json!({
                "file": harness.test_file_path().as_str()
            })),
        )
        .await;

    expect![[r#"
        20:13: type: u32
        27:5: discriminant = 0
        28:5: discriminant = 1
        32:9: type: Analysis"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_search_ssr() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "search",
            Some(json!({
                "pattern": "McpError::$variant",
                "files": ["src/types.rs"]
            })),
        )
        .await;

    expect!["src/types.rs:9:5: McpError::NotFound [other, in make_error]"].assert_eq(&result);

    // `files` restricts, not just resolves: the same pattern scoped to a file
    // with no occurrences returns nothing.
    let restricted = harness
        .call_tool(
            "search",
            Some(json!({
                "pattern": "McpError::$variant",
                "files": ["src/lib.rs"]
            })),
        )
        .await;
    expect!["no matches; loosen the pattern or drop a filter"].assert_eq(&restricted);
}

#[tokio::test]
async fn test_ssr_preview() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "ssr",
            Some(json!({
                "rule": "McpError::$v ==>> McpError::$v",
                "files": ["src/types.rs"]
            })),
        )
        .await;

    expect![[r#"
        src/types.rs:9:5: => McpError::NotFound
        [rerun with apply=true to write these edits]"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_ssr_apply() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "ssr",
            Some(json!({
                "rule": "Analysis::new(1) ==>> Analysis::new(7)",
                "apply": true
            })),
        )
        .await;

    expect![[r#"
        applied 2 edits in 2 files
        src/lib.rs:32:20: => Analysis::new(7)
        src/types.rs:13:5: => Analysis::new(7)"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_search_count_by() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    let result = harness
        .call_tool(
            "search",
            Some(json!({
                "pattern": "Analysis::new($x)",
                "countBy": "file"
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs: 1
        src/types.rs: 1
        [2 total]"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_expand_macro() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    // `carrier!(generated_carrier);` at the bottom of types.rs.
    let types_file = harness.workspace_root.join("src/types.rs");

    let result = harness
        .call_tool(
            "expand_macro",
            Some(json!({
                "file": types_file.as_str(),
                "line": 24,
                "column": 1
            })),
        )
        .await;

    expect![[r#"
        carrier! expands to:
        ```rust
        pub fn generated_carrier() -> u32 {
            1
        }
        ```"#]]
    .assert_eq(&result);
}

#[tokio::test]
async fn test_reachable_implementation_edges() {
    if skip_slow_tests() {
        return;
    }
    let harness = get_harness().await;

    // Find the ServerHandler trait
    let symbol_result = harness
        .call_tool(
            "find_symbol",
            Some(json!({
                "query": "ServerHandler",
                "mode": "exact",
                "onlyTypes": true,
                "limit": 10
            })),
        )
        .await;

    let (file_path, line, col) = first_loc(&symbol_result, "trait ServerHandler");
    assert_eq!(file_path, "src/lib.rs", "result paths should be workspace-relative");

    let result = harness
        .call_tool(
            "reachable",
            Some(json!({
                "file": file_path,
                "line": line,
                "column": col,
                "edgeKinds": ["implementation"]
            })),
        )
        .await;

    expect![[r#"
        src/lib.rs:3:10: reachable incoming implementation depth=1 root #0 trait ServerHandler
        src/lib.rs:18:24: d1 implementation #1 impl impl -> #0 trait ServerHandler [dispatch=goto_implementation]: impl ServerHandler for Analysis {"#]]
    .assert_eq(&result);
}
