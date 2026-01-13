//! JSON request shapes accepted by MCP tools.

use std::path::{Path, PathBuf};

use globset::{Glob, GlobMatcher};
use schemars::JsonSchema;
use serde::Deserialize;

use crate::params::{
    ArgumentIndex, Column, DispatchFilterText, FunctionFilterText, ImplFilterText, Line,
    PathPatternText, PatternText, PlaceholderText, PreviewLimit, PreviewOffset, ReachabilityDepth,
    RenameTargetText, SsrRuleText, SymbolLimit, SymbolQueryText, default_placeholder,
    default_preview_limit, default_preview_offset, default_reachability_depth,
    default_symbol_limit,
};
use crate::types::{
    BoolFilter, GroupByField, ReachableDirection, ReachableEdgeKind, ReachableScope,
    ReachableUsageKind, ReferenceCategoryTag, SerializableSymbolKind, SerializableUsageKind,
};

#[derive(Debug, Default, Clone, Copy, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum AssocMode {
    #[default]
    Include,
    Exclude,
    AssocItemsOnly,
}

#[derive(Debug, Default, Clone, Copy, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub enum SearchMode {
    #[default]
    Fuzzy,
    Exact,
    Prefix,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKindFilter {
    AllKinds,
    TypesOnly,
}

#[derive(Debug, Clone, Copy)]
pub enum SearchScope {
    WorkspaceOnly,
    IncludeLibs,
}

#[derive(Debug, Clone, Copy)]
pub enum CaseSensitivity {
    Insensitive,
    Sensitive,
}

#[derive(Debug, Clone)]
pub struct SymbolSearchParams {
    pub query: SymbolQueryText,
    pub mode: SearchMode,
    pub kind_filter: SymbolKindFilter,
    pub scope: SearchScope,
    pub case_sensitivity: CaseSensitivity,
    pub limit: SymbolLimit,
    pub offset: PreviewOffset,
    pub path: Option<PathPatternText>,
    pub exclude_imports: bool,
    pub assoc_mode: AssocMode,
}

#[derive(Debug, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct SearchRequest {
    #[schemars(description = "SSR pattern to search for (e.g., 'foo($x)' or 'Vec::new()')")]
    pub pattern: PatternText,
    #[schemars(
        description = "Restrict the search to these files (workspace-relative or absolute); also sets the path-resolution context (paths in the pattern resolve from files[0])"
    )]
    #[serde(default)]
    pub files: Option<Vec<PathBuf>>,
    #[schemars(description = "Glob pattern to filter results by file path (e.g., 'src/**/*.rs')")]
    #[serde(default)]
    pub file_pattern: Option<PathPatternText>,
    #[schemars(description = "Filter to matches in test code: 'yes', 'no', or 'any' (default)")]
    #[serde(default)]
    pub in_test: BoolFilter,
    #[schemars(
        description = "Filter to matches in unsafe blocks/functions: 'yes', 'no', or 'any' (default)"
    )]
    #[serde(default)]
    pub in_unsafe: BoolFilter,
    #[schemars(
        description = "Filter to matches in async functions: 'yes', 'no', or 'any' (default)"
    )]
    #[serde(default)]
    pub in_async: BoolFilter,
    #[schemars(description = "Filter to matches inside the named function")]
    #[serde(default)]
    pub in_function: Option<FunctionFilterText>,
    #[schemars(description = "Filter to matches inside the named impl block")]
    #[serde(default)]
    pub in_impl: Option<ImplFilterText>,
    #[schemars(description = "Filter to matches with a specific usage kind")]
    #[serde(default)]
    pub usage_kind: Option<SerializableUsageKind>,
    #[schemars(description = "Number of matches to skip, for paging. Default: 0")]
    #[serde(default = "default_preview_offset")]
    pub offset: PreviewOffset,
    #[schemars(description = "Maximum number of matches to return. Default: 20")]
    #[serde(default = "default_preview_limit")]
    pub limit: PreviewLimit,
    #[schemars(description = "Return grouped counts instead of match lines (like grep -c): \
                       'file', 'usage_kind', 'function', or 'impl'. Counts the filtered set; offset/limit are ignored")]
    #[serde(default)]
    pub count_by: Option<GroupByField>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SsrRequest {
    #[schemars(
        description = "SSR rule in format 'pattern ==>> replacement' (e.g., 'foo($x) ==>> bar($x)')"
    )]
    pub rule: SsrRuleText,
    #[schemars(
        description = "Restrict the rewrite to these files (workspace-relative or absolute); also sets the path-resolution context (paths in the rule resolve from files[0])"
    )]
    #[serde(default)]
    pub files: Option<Vec<PathBuf>>,
    #[schemars(description = "Write the edits to disk. Default: false (preview only)")]
    #[serde(default)]
    pub apply: bool,
}

#[derive(Debug, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct FindSymbolRequest {
    #[schemars(
        description = "Symbol name or path query. Examples: 'Vec', 'HashMap', 'std::vec::Vec', '::std::' (list std crate contents)"
    )]
    pub query: SymbolQueryText,
    #[schemars(description = "Search mode: 'fuzzy' (default), 'exact', or 'prefix'")]
    #[serde(default)]
    pub mode: SearchMode,
    #[schemars(
        description = "Only return types (structs, enums, traits, type aliases). Default: false"
    )]
    #[serde(default)]
    pub only_types: bool,
    #[schemars(
        description = "Include library/dependency symbols. Default: false (workspace only)"
    )]
    #[serde(default)]
    pub include_libs: bool,
    #[schemars(description = "Case-sensitive matching. Default: false")]
    #[serde(default)]
    pub case_sensitive: bool,
    #[schemars(description = "Maximum number of results. Default: 50")]
    #[serde(default = "default_symbol_limit")]
    pub limit: SymbolLimit,
    #[schemars(description = "Number of results to skip, for paging. Default: 0")]
    #[serde(default = "default_preview_offset")]
    pub offset: PreviewOffset,
    #[schemars(
        description = "Filter results to files matching this path. Supports exact paths (e.g., 'src/lib.rs'), \
                       folder prefixes (e.g., 'src/'), and glob patterns (e.g., '**/table.rs', 'crates/*/src/**/*.rs'). \
                       Paths are matched against the full file path."
    )]
    #[serde(default)]
    pub path: Option<PathPatternText>,
    #[schemars(
        description = "Exclude re-exported symbols, showing only original definitions. Default: false"
    )]
    #[serde(default)]
    pub exclude_imports: bool,
    #[schemars(description = "How to handle associated items (trait methods, constants): \
                       'include' (default) returns both, 'exclude' omits them, \
                       'assoc_items_only' returns only associated items.")]
    #[serde(default)]
    pub assoc_mode: AssocMode,
}

pub enum PathFilter {
    Glob(GlobMatcher),
    Substring(String),
}

impl PathFilter {
    pub fn new(pattern: &str) -> Result<Self, String> {
        let pattern = pattern.trim();
        if pattern.is_empty() {
            return Err("pattern cannot be empty".into());
        }

        if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
            let glob_pattern = if pattern.starts_with("**/") || pattern.starts_with('/') {
                pattern.to_owned()
            } else {
                format!("**/{pattern}")
            };

            Glob::new(&glob_pattern)
                .map(|glob| PathFilter::Glob(glob.compile_matcher()))
                .map_err(|error| error.to_string())
        } else {
            Ok(PathFilter::Substring(pattern.to_owned()))
        }
    }

    pub fn matches(&self, file_path: &Path) -> bool {
        match self {
            PathFilter::Glob(matcher) => matcher.is_match(file_path),
            PathFilter::Substring(pattern) => {
                let path = file_path.to_string_lossy();
                path.contains(pattern)
            }
        }
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ReachableSeed {
    #[schemars(
        description = "Workspace-relative or absolute path to the file containing the seed symbol"
    )]
    pub file: PathBuf,
    #[schemars(description = "Line number (1-based, matching tool result lines)")]
    #[serde(default)]
    pub line: Option<Line>,
    #[schemars(
        description = "Column number (1-based, UTF-8 byte offset, matching tool result lines)"
    )]
    #[serde(default)]
    pub column: Option<Column>,
    #[schemars(
        description = "Exact 1-based half-open range from another semantic tool; the start is used as the graph seed"
    )]
    #[serde(default)]
    pub range: Option<LineColRangeRequest>,
}

#[derive(Debug, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ReachableRequest {
    #[schemars(
        description = "Workspace-relative or absolute path to the file containing the seed symbol"
    )]
    pub file: PathBuf,
    #[schemars(description = "Line number (1-based, matching tool result lines)")]
    #[serde(default)]
    pub line: Option<Line>,
    #[schemars(
        description = "Column number (1-based, UTF-8 byte offset, matching tool result lines)"
    )]
    #[serde(default)]
    pub column: Option<Column>,
    #[schemars(
        description = "Exact 1-based half-open range from another semantic tool; the start is used as the graph seed"
    )]
    #[serde(default)]
    pub range: Option<LineColRangeRequest>,
    #[schemars(
        description = "Optional target seed. When set, reachable returns the discovered path from the root seed to this target when one exists."
    )]
    #[serde(default)]
    pub target: Option<ReachableSeed>,
    #[schemars(
        description = "Traversal direction. Supports 'incoming' dependents/callers, 'outgoing' dependencies/callees, or 'both' for a one-tool neighborhood. Default: incoming"
    )]
    #[serde(default)]
    pub direction: ReachableDirection,
    #[schemars(
        description = "Traversal scope. Default: workspace. Use workspace_and_dependencies to include library/dependency nodes."
    )]
    #[serde(default)]
    pub scope: ReachableScope,
    #[schemars(
        description = "Graph edge kinds to traverse. Supports 'call', 'usage', and 'implementation'. Omit or pass [] for the default ['call']"
    )]
    #[serde(default)]
    pub edge_kinds: Vec<ReachableEdgeKind>,
    #[schemars(
        description = "Filter returned edges/nodes to files matching this path. Supports exact paths, folder prefixes, and glob patterns like '**/collector.rs'. The edge is kept when the edge location or discovered neighbor node matches."
    )]
    #[serde(default)]
    pub path: Option<PathPatternText>,
    #[schemars(
        description = "Filter discovered neighbor nodes by symbol kind, using the same kind strings returned in structuredContent nodes."
    )]
    #[serde(default)]
    pub node_kinds: Vec<SerializableSymbolKind>,
    #[schemars(
        description = "Filter edges by dispatch tag, such as 'call_hierarchy', 'path', 'method_call', 'record_expr_field', or 'goto_implementation'."
    )]
    #[serde(default)]
    pub dispatch: Vec<DispatchFilterText>,
    #[schemars(
        description = "Filter usage edges by reference category: read, write, import, or test."
    )]
    #[serde(default)]
    pub reference_categories: Vec<ReferenceCategoryTag>,
    #[schemars(
        description = "Filter usage edges by graph usage kind, such as callable, type, field, import, macro, value, or local."
    )]
    #[serde(default)]
    pub usage_kinds: Vec<ReachableUsageKind>,
    #[schemars(description = "Number of graph hops to traverse. Default: 1")]
    #[serde(default = "default_reachability_depth")]
    pub depth: ReachabilityDepth,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct InspectRequest {
    #[schemars(description = "Workspace-relative or absolute path to the file")]
    pub file: PathBuf,
    #[schemars(description = "Line number (1-based, matching tool result lines)")]
    pub line: Line,
    #[schemars(
        description = "Column number (1-based, UTF-8 byte offset, matching tool result lines)"
    )]
    pub column: Column,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ExpandMacroRequest {
    #[schemars(description = "Workspace-relative or absolute path to the file")]
    pub file: PathBuf,
    #[schemars(description = "Line number (1-based, matching tool result lines)")]
    pub line: Line,
    #[schemars(
        description = "Column number (1-based, UTF-8 byte offset, matching tool result lines)"
    )]
    pub column: Column,
}

#[derive(Debug, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct RenameRequest {
    #[schemars(description = "Workspace-relative or absolute path to the file")]
    pub file: PathBuf,
    #[schemars(description = "Line number (1-based, matching tool result lines)")]
    pub line: Line,
    #[schemars(
        description = "Column number (1-based, UTF-8 byte offset, matching tool result lines)"
    )]
    pub column: Column,
    #[schemars(description = "The new name for the symbol")]
    pub new_name: RenameTargetText,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct InlayHintsRequest {
    #[schemars(description = "Workspace-relative or absolute path to the file")]
    pub file: PathBuf,
    #[schemars(
        description = "Optional range to get hints for. If not specified, returns hints for the entire file."
    )]
    pub range: Option<LineColRangeRequest>,
}

#[derive(Debug, Clone, Copy, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct LineColRangeRequest {
    #[schemars(description = "Start line (1-based)")]
    pub start_line: Line,
    #[schemars(description = "Start column (1-based, UTF-8 byte offset)")]
    pub start_col: Column,
    #[schemars(description = "End line (1-based)")]
    pub end_line: Line,
    #[schemars(description = "End column (1-based, UTF-8 byte offset)")]
    pub end_col: Column,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ReadRequest {
    #[schemars(description = "Workspace-relative or absolute path to the file")]
    pub file: PathBuf,
    #[schemars(
        description = "Optional half-open 1-based range to read. If omitted, returns the whole file."
    )]
    #[serde(default)]
    pub range: Option<LineColRangeRequest>,
}

#[derive(Debug, Clone, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct AddArgumentRequest {
    #[schemars(description = "Workspace-relative or absolute path to the file")]
    pub file: PathBuf,
    #[schemars(description = "Line number (1-based, matching tool result lines)")]
    pub line: Line,
    #[schemars(
        description = "Column number (1-based, UTF-8 byte offset, matching tool result lines)"
    )]
    pub column: Column,
    #[schemars(
        description = "0-based index in the param list (not counting self) where the new argument goes"
    )]
    pub argument_index: ArgumentIndex,
    #[schemars(description = "Text to insert at each call site. Default: 'todo!()'")]
    #[serde(default = "default_placeholder")]
    pub placeholder: PlaceholderText,
    #[schemars(description = "Write the edits to disk. Default: false (preview only)")]
    #[serde(default)]
    pub apply: bool,
}
