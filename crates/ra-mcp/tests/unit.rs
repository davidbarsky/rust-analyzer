//! Fast unit tests that drive ra-mcp's analysis helpers without spinning up a
//! Cargo workspace. Each test builds an in-memory fixture, gets a `Snapshot`,
//! and calls the helper directly.
//!
//! These tests run by default (no `RUN_SLOW_TESTS` gate). They typically
//! finish in single-digit milliseconds — cheap enough to add many of them,
//! unlike the protocol-level integration tests that load a full workspace.
//!
//! Most tests use `expect-test` to snapshot the produced edits / reachable graphs /
//! symbols. To update an expected value after a deliberate change, run
//! `UPDATE_EXPECT=1 cargo test -p ra-mcp --test unit`.

mod common;

use common::{format_edits, format_symbols, snapshot_from_fixture};
use expect_test::{Expect, expect};
use ra_mcp_handlers::analysis::Snapshot;
use ra_mcp_handlers::params::{
    ArgumentIndex, Column, DispatchFilterText, Line, PathPatternText, PatternText, PlaceholderText,
    PreviewOffset, ReachabilityDepth, RenameTargetText, SsrRuleText, SymbolLimit, SymbolQueryText,
};
use ra_mcp_handlers::requests::{
    AssocMode, CaseSensitivity, LineColRangeRequest, ReachableRequest, ReachableSeed, SearchMode,
    SearchScope, SymbolKindFilter, SymbolSearchParams,
};
use ra_mcp_handlers::server::{
    add_argument_source_change, reachable, rename_source_change, search_matches,
    source_change_to_serialized_edits, ssr_compute_edits, symbol_search,
};
use ra_mcp_handlers::types::{
    ConversionContext, ReachableDirection, ReachableEdgeKind, ReachableScope, ReachableUsageKind,
    ReferenceCategoryTag, SerializableSymbolKind,
};

/// Returns 1-based `(line, column)` for a `FilePosition` by consulting the
/// analysis's line index. Lets tests express positions as `$0` markers rather
/// than hardcoding coordinates. Request positions are 1-based; `LineIndex` is
/// 0-based, hence the +1.
fn line_col_for(snapshot: &Snapshot, position: ide::FilePosition) -> (Line, Column) {
    let line_index = snapshot.analysis().file_line_index(position.file_id).unwrap();
    let line_col = line_index.line_col(position.offset);
    (Line::new(line_col.line + 1), Column::new(line_col.col + 1))
}

fn check(actual: impl AsRef<str>, expect: Expect) {
    expect.assert_eq(actual.as_ref());
}

// ---------- reachable ----------

#[test]
fn reachable_incoming_calls_returns_call_sites() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target$0() {}

fn caller_one() { target(); }
fn caller_two() {
    target();
    target();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert_eq!(result.nodes[result.roots[0]].name, "target");
    check(
        result.render(None),
        expect![[r#"
            /lib.rs:1:4: reachable incoming call depth=1 root #0 fn target
            /lib.rs:3:19: d1 call #1 fn caller_one -> #0 fn target [dispatch=call_hierarchy]: fn caller_one() { target(); }
            /lib.rs:5:5: d1 call #2 fn caller_two -> #0 fn target [dispatch=call_hierarchy]: target();
            /lib.rs:6:5: d1 call #2 fn caller_two -> #0 fn target [dispatch=call_hierarchy]: target();"#]],
    );
}

#[test]
fn reachable_incoming_calls_traverses_to_depth() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target$0() {}

fn caller() { target(); }

fn outer() { caller(); }
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(2),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:1:4: reachable incoming call depth=2 root #0 fn target
            /lib.rs:3:15: d1 call #1 fn caller -> #0 fn target [dispatch=call_hierarchy]: fn caller() { target(); }
            /lib.rs:5:14: d2 call #2 fn outer -> #1 fn caller [dispatch=call_hierarchy]: fn outer() { caller(); }"#]],
    );
}

#[test]
fn reachable_incoming_calls_across_files() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub mod helpers;
pub fn entry$0() {}

fn local_caller() { entry(); }

//- /helpers.rs
use crate::entry;

pub fn helper_caller() {
    entry();
    crate::entry();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:2:8: reachable incoming call depth=1 root #0 fn entry
            /helpers.rs:4:5: d1 call #1 fn helper_caller -> #0 fn entry [dispatch=call_hierarchy]: entry();
            /helpers.rs:5:12: d1 call #1 fn helper_caller -> #0 fn entry [dispatch=call_hierarchy]: crate::entry();
            /lib.rs:4:21: d1 call #2 fn local_caller -> #0 fn entry [dispatch=call_hierarchy]: fn local_caller() { entry(); }"#]],
    );
}

#[test]
fn reachable_incoming_usage_returns_enclosing_symbols() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
struct Data$0;

fn takes(_: Data) {}

fn constructs() {
    let _ = Data;
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:1:8: reachable incoming usage depth=1 root #0 struct Data
            /lib.rs:3:13: d1 usage #1 fn takes -> #0 struct Data [type, dispatch=path]: fn takes(_: Data) {}
            /lib.rs:6:13: d1 usage #2 fn constructs -> #0 struct Data [type, dispatch=path]: let _ = Data;"#]],
    );
}

#[test]
fn reachable_outgoing_usage_returns_referenced_symbols() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
struct Data;

fn helper() {}

fn caller$0(value: Data) {
    helper();
    let _ = Data;
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Outgoing,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:5:4: reachable outgoing usage depth=1 root #0 fn caller
            /lib.rs:5:18: d1 usage #0 fn caller -> #1 struct Data [type, dispatch=path]: fn caller(value: Data) {
            /lib.rs:6:5: d1 usage #0 fn caller -> #2 fn helper [callable, dispatch=path]: helper();
            /lib.rs:7:13: d1 usage #0 fn caller -> #1 struct Data [type, dispatch=path]: let _ = Data;"#]],
    );
}

#[test]
fn reachable_outgoing_can_return_call_and_usage_edges() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
struct Data;

fn helper() {}

fn caller$0(value: Data) {
    helper();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Outgoing,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Call, ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:5:4: reachable outgoing call,usage depth=1 root #0 fn caller
            /lib.rs:6:5: d1 call #0 fn caller -> #1 fn helper [dispatch=call_hierarchy]: helper();
            /lib.rs:5:18: d1 usage #0 fn caller -> #2 struct Data [type, dispatch=path]: fn caller(value: Data) {
            /lib.rs:6:5: d1 usage #0 fn caller -> #1 fn helper [callable, dispatch=path]: helper();"#]],
    );
}

#[test]
fn reachable_incoming_usage_keeps_reference_categories_and_node_metadata() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub struct Data {
    pub value$0: u32,
}

fn read(data: &Data) -> u32 {
    data.value
}

fn write(data: &mut Data) {
    data.value = 92;
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    let root = &result.nodes[result.roots[0]];
    assert_eq!(root.name, "value");
    assert_eq!(root.visibility.as_deref(), Some("pub"));
    assert!(root.declaration.as_deref().unwrap().contains("pub value: u32"));

    assert!(result.edges.iter().any(|edge| {
        matches!(edge.usage_kind, Some(ReachableUsageKind::Field))
            && edge
                .reference_categories
                .iter()
                .any(|category| matches!(category, ReferenceCategoryTag::Read))
    }));
    assert!(result.edges.iter().any(|edge| {
        matches!(edge.usage_kind, Some(ReachableUsageKind::Field))
            && edge
                .reference_categories
                .iter()
                .any(|category| matches!(category, ReferenceCategoryTag::Write))
    }));
}

#[test]
fn reachable_outgoing_usage_records_receiver_and_substitution() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
struct Boxed<T> {
    value: T,
}

impl<T> Boxed<T> {
    fn get(&self) -> &T {
        &self.value
    }
}

fn caller$0(input: Boxed<u32>) {
    let _ = input.get();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Outgoing,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    let method_edge = result
        .edges
        .iter()
        .find(|edge| result.nodes[edge.target_node].name == "get")
        .expect("method use should be reachable");
    assert!(matches!(method_edge.usage_kind, Some(ReachableUsageKind::Callable)));
    assert_eq!(method_edge.dispatch.as_deref(), Some("method_call"));
    assert!(
        method_edge
            .receiver_type
            .as_deref()
            .is_some_and(|receiver| receiver.contains("Boxed<u32>"))
    );
    assert!(
        method_edge
            .generic_substitution
            .iter()
            .any(|arg| arg.parameter == "T" && arg.value == "u32")
    );
}

#[test]
fn reachable_incoming_implementation_edges_use_the_same_graph() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
trait Service$0 {
    fn run(&self) -> u32;
}

struct Worker;

impl Service for Worker {
    fn run(&self) -> u32 {
        1
    }
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Implementation],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:1:7: reachable incoming implementation depth=1 root #0 trait Service
            /lib.rs:7:18: d1 implementation #1 impl impl -> #0 trait Service [dispatch=goto_implementation, trait=trait Service {…]: impl Service for Worker {"#]],
    );
}

#[test]
fn reachable_trait_method_implementation_edge_points_to_impl_method() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
trait Service {
    fn run$0(&self) -> u32;
}

struct Worker;

impl Service for Worker {
    fn run(&self) -> u32 {
        1
    }
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Implementation],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    check(
        result.render(None),
        expect![[r#"
            /lib.rs:2:8: reachable incoming implementation depth=1 root #0 fn run
            /lib.rs:8:8: d1 implementation #1 fn run -> #0 fn run [dispatch=goto_implementation, trait=trait Service {…]: fn run(&self) -> u32 {"#]],
    );
}

#[test]
fn reachable_accepts_range_seed() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target$0() {}

fn caller() { target(); }
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: None,
            column: None,
            range: Some(LineColRangeRequest {
                start_line: line,
                start_col: column,
                end_line: line,
                end_col: Column::new(column.get() + "target".len() as u32),
            }),
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert_eq!(result.nodes[result.roots[0]].name, "target");
    assert_eq!(result.edges.len(), 1);
    assert_eq!(result.nodes[result.edges[0].source_node].name, "caller");
}

#[test]
fn reachable_falls_back_from_salsa_attribute_to_enclosing_item() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
#[salsa::$0tracked]
fn target() {}

fn caller() { target(); }
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert_eq!(result.nodes[result.roots[0]].name, "target");
    assert!(
        result.note.as_deref().is_some_and(|note| note.contains("using enclosing item `target`"))
    );
    let mut has_caller_edge = false;
    for edge in &result.edges {
        if result.nodes[edge.source_node].name == "caller" {
            has_caller_edge = true;
        }
    }
    assert!(has_caller_edge);
}

#[test]
fn reachable_both_direction_returns_call_neighborhood() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn leaf() {}
fn middle$0() { leaf(); }
fn caller() { middle(); }
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Both,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    let mut has_incoming = false;
    let mut has_outgoing = false;
    for edge in &result.edges {
        let source = &result.nodes[edge.source_node].name;
        let target = &result.nodes[edge.target_node].name;
        if source == "caller" && target == "middle" {
            has_incoming = true;
        }
        if source == "middle" && target == "leaf" {
            has_outgoing = true;
        }
    }
    assert!(has_incoming);
    assert!(has_outgoing);
}

#[test]
fn reachable_target_returns_path() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn root$0() { middle(); }
fn middle() { leaf(); }
fn leaf() {}
fn unrelated() {}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: Some(ReachableSeed {
                file: fx.files[0].clone(),
                line: Some(Line::new(3)),
                column: Some(Column::new(4)),
                range: None,
            }),
            direction: ReachableDirection::Outgoing,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(2),
        },
    )
    .unwrap();

    assert_eq!(result.edges.len(), 2);
    let names = result.nodes.iter().map(|node| node.name.as_str()).collect::<Vec<_>>();
    assert_eq!(names, ["root", "middle", "leaf"]);
}

#[test]
fn reachable_path_filter_keeps_matching_edge_locations() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub mod a;
pub mod b;
pub fn target$0() {}

//- /a.rs
use crate::target;
pub fn a_call() { target(); }

//- /b.rs
use crate::target;
pub fn b_call() { target(); }
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: Some(PathPatternText::parse("a.rs").unwrap()),
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert_eq!(result.edges.len(), 1);
    assert_eq!(result.nodes[result.edges[0].source_node].name, "a_call");
    assert!(result.edges[0].location.file_path.ends_with("a.rs"));
}

#[test]
fn reachable_node_kind_filter_keeps_matching_neighbors() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
struct Data;

fn helper() {}

fn caller$0(value: Data) {
    helper();
    let _ = Data;
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Outgoing,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: vec![SerializableSymbolKind::Struct],
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert!(!result.edges.is_empty());
    assert!(result.edges.iter().all(|edge| result.nodes[edge.target_node].name == "Data"));
}

#[test]
fn reachable_dispatch_and_usage_kind_filters_keep_matching_edges() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
struct Boxed<T> {
    value: T,
}

impl<T> Boxed<T> {
    fn get(&self) -> &T {
        &self.value
    }
}

fn caller$0(input: Boxed<u32>) {
    let _ = input.get();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Outgoing,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: vec![DispatchFilterText::parse("method_call").unwrap()],
            reference_categories: Vec::new(),
            usage_kinds: vec![ReachableUsageKind::Callable],
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert_eq!(result.edges.len(), 1);
    let edge = &result.edges[0];
    assert_eq!(edge.dispatch.as_deref(), Some("method_call"));
    assert!(matches!(edge.usage_kind, Some(ReachableUsageKind::Callable)));
    assert_eq!(result.nodes[edge.target_node].name, "get");
}

#[test]
fn reachable_reference_category_filter_keeps_matching_edges() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub struct Data {
    pub value$0: u32,
}

fn read(data: &Data) -> u32 {
    data.value
}

fn write(data: &mut Data) {
    data.value = 92;
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Usage],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: vec![ReferenceCategoryTag::Write],
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    assert_eq!(result.edges.len(), 1);
    assert_eq!(result.nodes[result.edges[0].source_node].name, "write");
    assert!(
        result.edges[0]
            .reference_categories
            .iter()
            .any(|category| matches!(category, ReferenceCategoryTag::Write))
    );
}

#[test]
fn reachable_edges_include_enclosing_item_and_branch_excerpt() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target$0() {}

fn caller(flag: bool) {
    match flag {
        true => {
            target();
        }
        false => {}
    }
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: Vec::new(),
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    let edge = result.edges.iter().find(|edge| result.nodes[edge.source_node].name == "caller");
    let edge = edge.expect("caller should call target");
    let enclosing = edge.location.enclosing_item.as_ref().expect("enclosing item");
    assert_eq!(enclosing.name, "caller");
    assert!(matches!(enclosing.kind, Some(SerializableSymbolKind::Function)));
    let excerpt = edge.location.source_excerpt.as_ref().expect("branch excerpt");
    assert_eq!(excerpt.kind, "match_arm");
    assert!(excerpt.text.contains("true =>"));
    assert!(excerpt.text.contains("target();"));
}

// ---------- search_matches (SSR) ----------

#[test]
fn search_matches_finds_pattern_occurrences() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target(x: i32) -> i32 { x }

fn call_once() { target(1); }
fn call_twice() {
    target(2);
    target(3);
}
"#,
    );

    let pattern = PatternText::parse("target($x)").unwrap();
    let matches = search_matches(&fx.snapshot, &pattern, &None).unwrap();

    let texts: Vec<&str> = matches.iter().map(|m| m.matched_text.as_str()).collect();
    assert_eq!(texts, ["target(1)", "target(2)", "target(3)"]);
}

#[test]
fn search_matches_returns_empty_when_pattern_does_not_match() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target(x: i32) -> i32 { x }
fn never_called() {}
"#,
    );

    let pattern = PatternText::parse("target($x)").unwrap();
    let matches = search_matches(&fx.snapshot, &pattern, &None).unwrap();
    assert!(matches.is_empty(), "got {:?}", matches);
}

// ---------- ssr_compute_edits (the diff produced for ssr_apply) ----------

#[test]
fn ssr_compute_edits_diff_for_single_call_site() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target(x: i32) -> i32 { x }
fn renamed(x: i32) -> i32 { x }
fn caller() { target(42); }
"#,
    );

    let rule = SsrRuleText::parse("target($x) ==>> renamed($x)").unwrap();
    let computed = ssr_compute_edits(&fx.snapshot, &rule, &None).unwrap();
    check(format_edits(&computed.serialized), expect![[r#"lib.rs 3:15-3:25 -> "renamed(42)""#]]);
}

#[test]
fn ssr_compute_edits_diff_for_multiple_call_sites() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target(x: i32) -> i32 { x }
fn renamed(x: i32) -> i32 { x }
fn a() { target(1); }
fn b() {
    target(2);
    target(3);
}
"#,
    );

    let rule = SsrRuleText::parse("target($x) ==>> renamed($x)").unwrap();
    let computed = ssr_compute_edits(&fx.snapshot, &rule, &None).unwrap();
    check(
        format_edits(&computed.serialized),
        expect![[r#"
            lib.rs 3:10-3:19 -> "renamed(1)"
            lib.rs 5:5-5:14 -> "renamed(2)"
            lib.rs 6:5-6:14 -> "renamed(3)""#]],
    );
}

// ---------- rename_source_change (the diff produced for `rename`) ----------

#[test]
fn rename_source_change_renames_function_at_definition() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target$0() {}

fn caller() {
    target();
    target();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let new_name = RenameTargetText::parse("renamed").unwrap();
    let source_change =
        rename_source_change(&fx.snapshot, &fx.files[0], line, column, &new_name).unwrap();

    let db = fx.snapshot.raw_database();
    let ctx = ConversionContext::new(db);
    let edits = source_change_to_serialized_edits(&ctx, &source_change);
    check(
        format_edits(&edits),
        expect![[r#"
            lib.rs 1:4-1:10 -> "renamed"
            lib.rs 4:5-4:11 -> "renamed"
            lib.rs 5:5-5:11 -> "renamed""#]],
    );
}

#[test]
fn rename_source_change_renames_across_files() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub mod helpers;
pub fn target$0() {}

//- /helpers.rs
use crate::target;

pub fn helper() {
    target();
    crate::target();
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let new_name = RenameTargetText::parse("renamed").unwrap();
    let source_change =
        rename_source_change(&fx.snapshot, &fx.files[0], line, column, &new_name).unwrap();

    let db = fx.snapshot.raw_database();
    let ctx = ConversionContext::new(db);
    let edits = source_change_to_serialized_edits(&ctx, &source_change);
    check(
        format_edits(&edits),
        expect![[r#"
            helpers.rs 1:12-1:18 -> "renamed"
            helpers.rs 4:5-4:11 -> "renamed"
            helpers.rs 5:12-5:18 -> "renamed"
            lib.rs 2:8-2:14 -> "renamed""#]],
    );
}

// ---------- add_argument_source_change (the diff produced for `add_argument`) ----------

#[test]
fn add_argument_inserts_placeholder_at_every_call_site() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
fn target$0(x: i32) -> i32 { x }

fn caller() {
    target(1);
    target(2);
}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let placeholder = PlaceholderText::new("todo!()");
    let ide::AddArgumentResult { source_change, skipped } = add_argument_source_change(
        &fx.snapshot,
        &fx.files[0],
        line,
        column,
        ArgumentIndex::new(1),
        &placeholder,
    )
    .unwrap();
    assert_eq!(skipped, 0);

    let db = fx.snapshot.raw_database();
    let ctx = ConversionContext::new(db);
    let edits = source_change_to_serialized_edits(&ctx, &source_change);
    check(
        format_edits(&edits),
        expect![[r#"
            lib.rs 4:13-4:13 -> ", todo!()"
            lib.rs 5:13-5:13 -> ", todo!()""#]],
    );
}

// ---------- symbol_search ----------

#[test]
fn symbol_search_exact_returns_matching_type() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub struct Target;
pub struct Other;
pub fn target_fn() {}
"#,
    );

    let params = SymbolSearchParams {
        query: SymbolQueryText::parse("Target").unwrap(),
        mode: SearchMode::Exact,
        kind_filter: SymbolKindFilter::TypesOnly,
        scope: SearchScope::WorkspaceOnly,
        case_sensitivity: CaseSensitivity::Insensitive,
        limit: SymbolLimit::new(10),
        offset: PreviewOffset::new(0),
        path: None,
        exclude_imports: false,
        assoc_mode: AssocMode::Include,
    };
    let (symbols, more) = symbol_search(&fx.snapshot, &params).unwrap();
    assert!(!more);
    check(format_symbols(&symbols), expect![[r#"lib.rs Target Some(Struct) 1:1"#]]);
}

#[test]
fn symbol_search_include_libs_keeps_workspace_symbols() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub struct Target;
"#,
    );

    let params = SymbolSearchParams {
        query: SymbolQueryText::parse("Target").unwrap(),
        mode: SearchMode::Exact,
        kind_filter: SymbolKindFilter::TypesOnly,
        scope: SearchScope::IncludeLibs,
        case_sensitivity: CaseSensitivity::Insensitive,
        limit: SymbolLimit::new(10),
        offset: PreviewOffset::new(0),
        path: None,
        exclude_imports: false,
        assoc_mode: AssocMode::Include,
    };
    let (symbols, more) = symbol_search(&fx.snapshot, &params).unwrap();
    assert!(!more);
    check(format_symbols(&symbols), expect![[r#"lib.rs Target Some(Struct) 1:1"#]]);
}

#[test]
fn symbol_search_fuzzy_returns_partial_matches() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub struct TargetA;
pub struct TargetB;
pub struct Unrelated;
"#,
    );

    let params = SymbolSearchParams {
        query: SymbolQueryText::parse("Target").unwrap(),
        mode: SearchMode::Fuzzy,
        kind_filter: SymbolKindFilter::TypesOnly,
        scope: SearchScope::WorkspaceOnly,
        case_sensitivity: CaseSensitivity::Insensitive,
        limit: SymbolLimit::new(10),
        offset: PreviewOffset::new(0),
        path: None,
        exclude_imports: false,
        assoc_mode: AssocMode::Include,
    };
    let (symbols, more) = symbol_search(&fx.snapshot, &params).unwrap();
    assert!(!more);
    check(
        format_symbols(&symbols),
        expect![[r#"
            lib.rs TargetA Some(Struct) 1:1
            lib.rs TargetB Some(Struct) 2:1"#]],
    );
}

#[test]
fn reachable_implementation_edges_return_all_impls() {
    let fx = snapshot_from_fixture(
        r#"
//- /lib.rs
pub trait Target$0 {}

pub struct A;
impl Target for A {}

pub struct B;
impl Target for B {}
"#,
    );

    let (line, column) = line_col_for(&fx.snapshot, fx.position.unwrap());
    let result = reachable(
        &fx.snapshot,
        &ReachableRequest {
            file: fx.files[0].clone(),
            line: Some(line),
            column: Some(column),
            range: None,
            target: None,
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Implementation],
            path: None,
            node_kinds: Vec::new(),
            dispatch: Vec::new(),
            reference_categories: Vec::new(),
            usage_kinds: Vec::new(),
            depth: ReachabilityDepth::new(1),
        },
    )
    .unwrap();

    let mut rows: Vec<String> = result
        .edges
        .iter()
        .map(|edge| {
            let node = &result.nodes[edge.source_node];
            format!(
                "{}:{} {}",
                node.range.start_line,
                node.range.start_col,
                node.kind.unwrap().as_str(),
            )
        })
        .collect();
    rows.sort();
    check(
        rows.join("\n"),
        expect![[r#"
            4:17 impl
            7:17 impl"#]],
    );
}
