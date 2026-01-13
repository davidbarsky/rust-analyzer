#![allow(unreachable_pub, dead_code)]

//! Shared test helpers for the ra-mcp integration test crates.
//!
//! The `snapshot_from_fixture` function builds a `Snapshot` from an in-memory
//! fixture string, skipping the Cargo workspace loading and proc-macro server
//! setup that the production path goes through. Tests using this helper run
//! in milliseconds instead of seconds, which lets us cover the analysis-using
//! helpers (search, reachable, symbol_search, ...) without the workspace
//! tax.
//!
//! The fixture format is the standard rust-analyzer fixture syntax (see
//! `test_utils::fixture`):
//!
//! ```text
//! //- /lib.rs
//! fn foo() { bar(1$0); }
//! fn bar(x: i32) {}
//! ```
//!
//! `$0` marks a cursor position. File paths must start with `/`.

use std::path::PathBuf;

use ide::{AnalysisHost, FilePosition};
use ra_mcp_handlers::analysis::Snapshot;
use test_fixture::ChangeFixture;
use test_utils::FixtureWithProjectMeta;

pub struct FixtureSnapshot {
    pub snapshot: Snapshot,
    /// Files in fixture order. The `i`-th entry corresponds to `FileId(i)`.
    pub files: Vec<PathBuf>,
    /// Position of the `$0` marker, if present.
    pub position: Option<FilePosition>,
}

/// Builds a `Snapshot` from a fixture string.
pub fn snapshot_from_fixture(ra_fixture: &str) -> FixtureSnapshot {
    let change_fixture = ChangeFixture::parse(ra_fixture);
    let mut host = AnalysisHost::default();
    host.raw_database_mut().enable_proc_attr_macros();
    host.apply_change(change_fixture.change);

    // Re-parse to recover per-file paths for test assertions.
    let project_meta = FixtureWithProjectMeta::parse(ra_fixture);

    let mut files = Vec::with_capacity(project_meta.fixture.len());
    for entry in &project_meta.fixture {
        files.push(PathBuf::from(&entry.path));
    }

    let position = change_fixture.file_position.map(|(efid, range_or_offset)| FilePosition {
        file_id: efid.file_id(),
        offset: range_or_offset.expect_offset(),
    });

    FixtureSnapshot { snapshot: Snapshot::for_test(host.analysis()), files, position }
}

/// Formats a list of edits as one line per edit, in a stable order, with
/// the file basename only. Designed for `expect_test` snapshot assertions —
/// catches changes to edit positions and replacement text without churn from
/// absolute paths or temp-dir prefixes.
pub fn format_edits(edits: &[ra_mcp_handlers::types::SerializableEdit]) -> String {
    let mut lines: Vec<String> = edits
        .iter()
        .map(|edit| {
            let name = edit
                .file_path
                .file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_else(|| edit.file_path.display().to_string());
            format!(
                "{} {}:{}-{}:{} -> {:?}",
                name,
                edit.range.start_line,
                edit.range.start_col,
                edit.range.end_line,
                edit.range.end_col,
                edit.new_text,
            )
        })
        .collect();
    lines.sort();
    lines.join("\n")
}

/// Formats a list of symbols for snapshot assertions.
pub fn format_symbols(symbols: &[ra_mcp_handlers::types::SerializableSymbol]) -> String {
    let mut lines: Vec<String> =
        symbols
            .iter()
            .map(|s| {
                let name = s
                    .file_path
                    .file_name()
                    .map(|p| p.to_string_lossy().into_owned())
                    .unwrap_or_else(|| s.file_path.display().to_string());
                format!(
                    "{} {} {:?} {}:{}",
                    name, s.name, s.kind, s.range.start_line, s.range.start_col,
                )
            })
            .collect();
    lines.sort();
    lines.join("\n")
}
