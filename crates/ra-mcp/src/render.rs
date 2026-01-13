//! Renders tool results as grep-shaped text lines: `path:line:col: kind name: context`.
//!
//! MCP responses also carry structured JSON from [`crate::types`]. The text
//! render remains the transcript-friendly channel: paths are workspace-relative
//! when they fall under `root`, and lines/columns are 1-based.

use std::path::Path;

use crate::types::{
    AddArgumentApplyOutcome, AddArgumentOutput, AddArgumentPreviewResult, ContextProperty,
    DefinitionLocation, ExpandMacroResult, InlayHintsResult, InspectResult, LineColRange,
    MatchGroup, MetadataSection, MetadataSectionKind, ReachableEdge, ReachableNode,
    ReachableResult, ReachableScope, ReadResult, RenameResult, SearchCountResult, SearchOutput,
    SearchResults, SerializableEdit, SerializableFileSystemEdit, SerializableMatch,
    SerializableSymbol, SsrApplyOutcome, SsrOutput, SsrPreviewResult, SymbolsResults,
    ToolRejection, WorkspaceCratesResult,
};

fn rel(path: &Path, root: Option<&Path>) -> String {
    match root.and_then(|root| path.strip_prefix(root).ok()) {
        Some(stripped) => stripped.display().to_string(),
        None => path.display().to_string(),
    }
}

fn loc(path: &Path, range: &LineColRange, root: Option<&Path>) -> String {
    format!("{}:{}:{}", rel(path, root), range.start_line, range.start_col)
}

fn one_line(text: &str) -> String {
    match text.split_once('\n') {
        Some((first, _)) => format!("{}…", first.trim_end()),
        None => text.to_owned(),
    }
}

fn rust_fence(text: &str) -> String {
    let mut longest_run = 0;
    let mut current_run = 0;
    for ch in text.chars() {
        if ch == '`' {
            current_run += 1;
            longest_run = longest_run.max(current_run);
        } else {
            current_run = 0;
        }
    }

    let fence = "`".repeat((longest_run + 1).max(3));
    format!("{fence}rust\n{text}\n{fence}")
}

/// The truncation marker paged results end with. Paging is stateless: the
/// tools re-run the query, so the marker names the offset that fetches the
/// next page — but refining the query is usually the better move.
fn more_line(offset: u32, shown: usize, total: usize) -> Option<String> {
    let end = offset as usize + shown;
    (end < total)
        .then(|| format!("[{} more; refine the query or rerun with offset={end}]", total - end))
}

impl ToolRejection {
    pub fn render(&self) -> String {
        format!(
            "ra-mcp rejected this request\nreason: {}\nmessage: {}\nnext: {}",
            self.code.as_str(),
            self.message,
            self.guidance,
        )
    }
}

impl SerializableSymbol {
    fn render_line(&self, root: Option<&Path>) -> String {
        let range = self.focus_range.as_ref().unwrap_or(&self.range);
        let kind = match self.kind {
            Some(kind) => kind.as_str(),
            None => "symbol",
        };
        let mut line = format!("{}: {kind} {}", loc(&self.file_path, range, root), self.name);
        if let Some(container) = &self.container_name {
            line.push_str(&format!(" in {container}"));
        }
        if let Some(description) = &self.description {
            line.push_str(&format!(": {}", one_line(description)));
        }
        line
    }
}

impl ReachableNode {
    fn render_label(&self) -> String {
        let kind = match self.kind {
            Some(kind) => kind.as_str(),
            None => "symbol",
        };
        format!("#{} {kind} {}", self.id, self.name)
    }
}

impl ReachableEdge {
    fn render_tags(&self) -> Vec<String> {
        let mut tags = Vec::new();
        if let Some(usage_kind) = self.usage_kind {
            tags.push(usage_kind.as_str().to_owned());
        }
        for category in &self.reference_categories {
            tags.push(category.as_str().to_owned());
        }
        if !self.generic_substitution.is_empty() {
            let substitutions = self
                .generic_substitution
                .iter()
                .map(|arg| format!("{}={}", arg.parameter, arg.value))
                .collect::<Vec<_>>()
                .join(", ");
            tags.push(format!("subst({substitutions})"));
        }
        if let Some(receiver_type) = &self.receiver_type {
            tags.push(format!("receiver={receiver_type}"));
        }
        if let Some(dispatch) = &self.dispatch {
            tags.push(format!("dispatch={dispatch}"));
        }
        if let Some(trait_context) = &self.trait_context {
            tags.push(format!("trait={}", one_line(trait_context)));
        }
        if let Some(macro_expansion) = &self.macro_expansion {
            tags.push(format!("macro={macro_expansion}"));
        }
        tags
    }

    fn render_line(
        &self,
        nodes: &[ReachableNode],
        root: Option<&Path>,
        include_source_text: bool,
    ) -> String {
        let source = nodes
            .get(self.source_node)
            .map(ReachableNode::render_label)
            .unwrap_or_else(|| format!("#{}", self.source_node));
        let target = nodes
            .get(self.target_node)
            .map(ReachableNode::render_label)
            .unwrap_or_else(|| format!("#{}", self.target_node));
        let mut line = format!(
            "{}: d{} {} {} -> {}",
            loc(&self.location.file_path, &self.location.range, root),
            self.depth,
            self.kind.as_str(),
            source,
            target,
        );
        let tags = self.render_tags();
        if !tags.is_empty() {
            line.push_str(&format!(" [{}]", tags.join(", ")));
        }
        if include_source_text && !self.location.line_text.is_empty() {
            line.push_str(&format!(": {}", one_line(&self.location.line_text)));
        }
        line
    }
}

const MAX_REACHABLE_EDGE_LINES: usize = 80;
const MAX_REACHABLE_EDGE_LINES_WITH_SOURCE: usize = 40;

impl SerializableMatch {
    fn render_line(&self, root: Option<&Path>) -> String {
        let mut tags = vec![self.usage_kind.as_str().to_owned()];
        if let Some(function) = &self.context.enclosing_function {
            tags.push(format!("in {function}"));
        }
        for property in &self.context.properties {
            tags.push(
                match property {
                    ContextProperty::Test => "test",
                    ContextProperty::Unsafe => "unsafe",
                    ContextProperty::Async => "async",
                }
                .to_owned(),
            );
        }
        format!(
            "{}: {} [{}]",
            loc(&self.file_path, &self.range, root),
            one_line(&self.matched_text),
            tags.join(", "),
        )
    }
}

impl SerializableEdit {
    fn render_line(&self, root: Option<&Path>) -> String {
        format!(
            "{}: => {}",
            loc(&self.file_path, &self.range, root),
            self.new_text.replace('\n', "\\n"),
        )
    }
}

/// Cap on per-edit lines in apply/preview output. A workspace-wide rename can
/// touch hundreds of call sites; the summary line and the files on disk carry
/// the full story, so the edit lines are a sample, not the record.
const MAX_EDIT_LINES: usize = 10;

fn push_edit_lines(lines: &mut Vec<String>, edits: &[SerializableEdit], root: Option<&Path>) {
    for edit in edits.iter().take(MAX_EDIT_LINES) {
        lines.push(edit.render_line(root));
    }
    if edits.len() > MAX_EDIT_LINES {
        lines.push(format!("[{} more edits]", edits.len() - MAX_EDIT_LINES));
    }
}

impl SymbolsResults {
    pub fn render(&self, root: Option<&Path>) -> String {
        if self.symbols.is_empty() && !self.more {
            return "no matching symbols; try mode=fuzzy, a shorter query, or includeLibs=true"
                .to_owned();
        }
        let mut lines = Vec::new();
        for symbol in &self.symbols {
            lines.push(symbol.render_line(root));
        }
        if self.more {
            let end = self.offset as usize + self.symbols.len();
            lines.push(format!("[more; refine the query or rerun with offset={end}]"));
        }
        lines.join("\n")
    }
}

impl ReachableResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        let mut lines = Vec::new();
        for root_id in &self.roots {
            let Some(root_node) = self.nodes.get(*root_id) else {
                continue;
            };
            let mut edge_kinds = Vec::new();
            for edge_kind in &self.edge_kinds {
                edge_kinds.push(edge_kind.as_str());
            }
            lines.push(format!(
                "{}: reachable {} {} depth={}{} root {}",
                loc(&root_node.file_path, &root_node.range, root),
                self.direction.as_str(),
                edge_kinds.join(","),
                self.depth,
                match self.scope {
                    ReachableScope::Workspace => String::new(),
                    ReachableScope::WorkspaceAndDependencies => {
                        format!(" scope={}", self.scope.as_str())
                    }
                },
                root_node.render_label(),
            ));
        }
        let include_source_text = self.edges.len() <= MAX_REACHABLE_EDGE_LINES_WITH_SOURCE;
        for edge in self.edges.iter().take(MAX_REACHABLE_EDGE_LINES) {
            lines.push(edge.render_line(&self.nodes, root, include_source_text));
        }
        if self.edges.len() > MAX_REACHABLE_EDGE_LINES {
            lines.push(format!(
                "[{} more edges; structuredContent contains the full graph]",
                self.edges.len() - MAX_REACHABLE_EDGE_LINES
            ));
        }
        if self.edges.is_empty() {
            if !self.roots.is_empty() {
                lines.push("no reachable graph edges found".to_owned());
            } else {
                lines.push("no reachable nodes found".to_owned());
            }
        }
        if let Some(note) = &self.note {
            lines.push(format!("note: {note}"));
        }
        lines.join("\n")
    }
}

impl SearchResults {
    pub fn render(&self, root: Option<&Path>) -> String {
        render_search_results(self.total_matches, self.offset, &self.matches, root)
    }
}

impl SearchCountResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        render_search_counts(self.total_matches, &self.groups, root)
    }
}

impl SearchOutput {
    pub fn render(&self, root: Option<&Path>) -> String {
        match self {
            SearchOutput::Matches { total_matches, offset, matches } => {
                render_search_results(*total_matches, *offset, matches, root)
            }
            SearchOutput::Counts { total_matches, group_by: _, groups } => {
                render_search_counts(*total_matches, groups, root)
            }
        }
    }
}

fn render_search_results(
    total_matches: usize,
    offset: u32,
    matches: &[SerializableMatch],
    root: Option<&Path>,
) -> String {
    if total_matches == 0 {
        return "no matches; loosen the pattern or drop a filter".to_owned();
    }
    let mut lines = Vec::new();
    for m in matches {
        lines.push(m.render_line(root));
    }
    if let Some(more) = more_line(offset, matches.len(), total_matches) {
        lines.push(more);
    }
    lines.join("\n")
}

fn render_search_counts(
    total_matches: usize,
    groups: &[MatchGroup],
    root: Option<&Path>,
) -> String {
    let root_prefix = root.map(|root| format!("{}/", root.display()));
    let mut lines = Vec::new();
    for group in groups {
        let key = match &root_prefix {
            Some(prefix) => group.key.strip_prefix(prefix.as_str()).unwrap_or(&group.key),
            None => &group.key,
        };
        lines.push(format!("{key}: {}", group.count));
    }
    lines.push(format!("[{total_matches} total]"));
    lines.join("\n")
}

impl SsrPreviewResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        render_ssr_preview(&self.edits, root)
    }
}

impl SsrApplyOutcome {
    pub fn render(&self, root: Option<&Path>) -> String {
        render_ssr_applied(self.files_changed, self.edits_applied, &self.edits, root)
    }
}

impl SsrOutput {
    pub fn render(&self, root: Option<&Path>) -> String {
        match self {
            SsrOutput::Preview { edits } => render_ssr_preview(edits, root),
            SsrOutput::Applied { files_changed, edits_applied, edits } => {
                render_ssr_applied(*files_changed, *edits_applied, edits, root)
            }
        }
    }
}

fn render_ssr_preview(edits: &[SerializableEdit], root: Option<&Path>) -> String {
    if edits.is_empty() {
        return "no matches for rule".to_owned();
    }
    let mut lines = Vec::new();
    push_edit_lines(&mut lines, edits, root);
    lines.push("[rerun with apply=true to write these edits]".to_owned());
    lines.join("\n")
}

fn render_ssr_applied(
    files_changed: usize,
    edits_applied: usize,
    edits: &[SerializableEdit],
    root: Option<&Path>,
) -> String {
    let mut lines = vec![format!("applied {edits_applied} edits in {files_changed} files")];
    push_edit_lines(&mut lines, edits, root);
    lines.join("\n")
}

impl AddArgumentPreviewResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        render_add_argument_preview(self.total_edits, self.skipped, &self.edits, root)
    }
}

impl AddArgumentApplyOutcome {
    pub fn render(&self, root: Option<&Path>) -> String {
        render_add_argument_applied(
            self.files_changed,
            self.edits_applied,
            self.skipped,
            &self.edits,
            root,
        )
    }
}

impl AddArgumentOutput {
    pub fn render(&self, root: Option<&Path>) -> String {
        match self {
            AddArgumentOutput::Preview { total_edits, skipped, edits } => {
                render_add_argument_preview(*total_edits, *skipped, edits, root)
            }
            AddArgumentOutput::Applied { files_changed, edits_applied, skipped, edits } => {
                render_add_argument_applied(*files_changed, *edits_applied, *skipped, edits, root)
            }
        }
    }
}

fn render_add_argument_preview(
    total_edits: usize,
    skipped: usize,
    edits: &[SerializableEdit],
    root: Option<&Path>,
) -> String {
    if total_edits == 0 && skipped == 0 {
        return "no call sites found".to_owned();
    }
    let mut lines = Vec::new();
    push_edit_lines(&mut lines, edits, root);
    if total_edits > 0 {
        lines.push("[rerun with apply=true to write these edits]".to_owned());
    }
    push_skipped_line(&mut lines, skipped);
    lines.join("\n")
}

fn render_add_argument_applied(
    files_changed: usize,
    edits_applied: usize,
    skipped: usize,
    edits: &[SerializableEdit],
    root: Option<&Path>,
) -> String {
    let mut lines = vec![format!("applied {edits_applied} edits in {files_changed} files")];
    push_edit_lines(&mut lines, edits, root);
    push_skipped_line(&mut lines, skipped);
    lines.join("\n")
}

fn push_skipped_line(lines: &mut Vec<String>, skipped: usize) {
    if skipped > 0 {
        lines.push(format!(
            "[{skipped} call sites (in macros or unrecognized forms) were not modified]"
        ));
    }
}

impl RenameResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        match self {
            RenameResult::Ok { files_changed, edits_applied, edits, file_system_edits } => {
                let mut lines =
                    vec![format!("applied {edits_applied} edits in {files_changed} files")];
                push_edit_lines(&mut lines, edits, root);
                for fs_edit in file_system_edits {
                    lines.push(match fs_edit {
                        SerializableFileSystemEdit::CreateFile { dst, .. } => {
                            format!("create {dst} (not applied; create it manually)")
                        }
                        SerializableFileSystemEdit::MoveFile { src, dst } => {
                            format!("move {src} => {dst} (not applied; move it manually)")
                        }
                        SerializableFileSystemEdit::MoveDir { src, dst } => {
                            format!("move dir {src} => {dst} (not applied; move it manually)")
                        }
                    });
                }
                lines.join("\n")
            }
            RenameResult::Rejected { reason } => format!("rename rejected: {reason}"),
        }
    }
}

impl InspectResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        if self.definitions.is_empty() && self.metadata.is_empty() {
            return "nothing known at this position; point at an identifier".to_owned();
        }
        let mut lines = Vec::new();
        let include_description = self.metadata.is_empty();
        for DefinitionLocation { file_path, range, name, kind, container_name, description } in
            &self.definitions
        {
            let kind = match kind {
                Some(kind) => kind.as_str(),
                None => "symbol",
            };
            let mut line = format!("{}: definition {kind} {name}", loc(file_path, range, root));
            if let Some(container) = container_name {
                line.push_str(&format!(" in {container}"));
            }
            if include_description && let Some(description) = description {
                line.push_str(&format!(": {}", one_line(description)));
            }
            lines.push(line);
        }
        for MetadataSection { kind, text } in &self.metadata.sections {
            match kind {
                MetadataSectionKind::QualifiedPath => {
                    lines.push(format!("qualified_path:\n{}", rust_fence(text)));
                }
                MetadataSectionKind::Signature => {
                    lines.push(format!("signature:\n{}", rust_fence(text)));
                }
                MetadataSectionKind::Declaration => {
                    lines.push(format!("declaration:\n{}", rust_fence(text)));
                }
                MetadataSectionKind::Type => {
                    lines.push(format!("type:\n{}", rust_fence(text)));
                }
                MetadataSectionKind::Code => {
                    lines.push(format!("code:\n{}", rust_fence(text)));
                }
                MetadataSectionKind::Docs => lines.push(format!("docs:\n{text}")),
            }
        }
        lines.join("\n\n")
    }
}

impl ReadResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        format!("{}: read\n\n{}", loc(&self.file_path, &self.range, root), self.text)
    }
}

impl ExpandMacroResult {
    pub fn render(&self) -> String {
        match (&self.name, &self.expansion) {
            (Some(name), Some(expansion)) => {
                format!("{name} expands to:\n{}", rust_fence(expansion))
            }
            (Some(name), None) => format!("{name} has no expansion"),
            (None, Some(expansion)) => format!("expands to:\n{}", rust_fence(expansion)),
            (None, None) => "no macro invocation at this position".to_owned(),
        }
    }
}

impl InlayHintsResult {
    pub fn render(&self) -> String {
        if self.hints.is_empty() {
            return "no inlay hints".to_owned();
        }
        let mut lines = Vec::new();
        for hint in &self.hints {
            let sep = if hint.label.starts_with([':', '=', ' ']) { "" } else { " " };
            lines.push(format!(
                "{}:{}: {}{sep}{}",
                hint.range.start_line,
                hint.range.start_col,
                hint.kind.as_str(),
                hint.label,
            ));
        }
        lines.join("\n")
    }
}

impl WorkspaceCratesResult {
    pub fn render(&self, root: Option<&Path>) -> String {
        let mut lines = Vec::new();
        for c in &self.crates {
            let name = c.name.as_deref().unwrap_or("<unnamed>");
            let mut line = name.to_owned();
            if let Some(version) = &c.version {
                line.push_str(&format!(" {version}"));
            }
            line.push_str(&format!(" {}", rel(&c.root_file, root)));
            lines.push(line);
        }
        lines.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::types::{
        PositionMetadata, ReachableDirection, ReachableEdgeKind, ReachableLocation,
        SerializableContext, SerializableSymbolKind, SerializableUsageKind,
    };

    fn range(line: u32, col: u32) -> LineColRange {
        LineColRange { start_line: line, start_col: col, end_line: line, end_col: col + 1 }
    }

    #[test]
    fn rust_fence_uses_longer_fence_when_code_contains_backticks() {
        assert_eq!(rust_fence("let docs = \"```\";"), "````rust\nlet docs = \"```\";\n````");
    }

    #[test]
    fn symbol_line_is_grep_shaped_one_based_and_relative() {
        let result = SymbolsResults {
            offset: 0,
            more: false,
            symbols: vec![SerializableSymbol {
                name: "process_batch".into(),
                kind: Some(SerializableSymbolKind::Function),
                file_path: PathBuf::from("/ws/src/batch.rs"),
                range: range(142, 1),
                focus_range: Some(range(142, 8)),
                container_name: None,
                description: Some("pub fn process_batch(items: &[Item]) -> Result<()>".into()),
            }],
        };
        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            "src/batch.rs:142:8: fn process_batch: pub fn process_batch(items: &[Item]) -> Result<()>",
        );
    }

    #[test]
    fn reachable_edge_line_carries_graph_endpoints_and_source_text() {
        let result = ReachableResult {
            roots: vec![0],
            direction: ReachableDirection::Incoming,
            scope: ReachableScope::Workspace,
            edge_kinds: vec![ReachableEdgeKind::Call],
            depth: 1,
            depths: vec![],
            nodes: vec![
                ReachableNode {
                    id: 0,
                    depth: 0,
                    file_path: PathBuf::from("/ws/src/batch.rs"),
                    range: range(142, 8),
                    full_range: range(142, 1),
                    name: "process_batch".into(),
                    kind: Some(SerializableSymbolKind::Function),
                    container_name: None,
                    description: None,
                    docs: None,
                    signature: None,
                    declaration: None,
                    body: None,
                    visibility: None,
                    trait_context: None,
                    impl_context: None,
                    monikers: Vec::new(),
                },
                ReachableNode {
                    id: 1,
                    depth: 1,
                    file_path: PathBuf::from("/ws/src/server.rs"),
                    range: range(81, 4),
                    full_range: range(81, 1),
                    name: "handle".into(),
                    kind: Some(SerializableSymbolKind::Function),
                    container_name: None,
                    description: None,
                    docs: None,
                    signature: None,
                    declaration: None,
                    body: None,
                    visibility: None,
                    trait_context: None,
                    impl_context: None,
                    monikers: Vec::new(),
                },
            ],
            edges: vec![ReachableEdge {
                depth: 1,
                kind: ReachableEdgeKind::Call,
                location: ReachableLocation {
                    file_path: PathBuf::from("/ws/src/server.rs"),
                    range: range(87, 13),
                    line_text: "let total = process_batch(&items)?;".into(),
                    enclosing_item: None,
                    source_excerpt: None,
                },
                source_node: 1,
                target_node: 0,
                reference_categories: Vec::new(),
                usage_kind: None,
                generic_substitution: Vec::new(),
                receiver_type: None,
                dispatch: None,
                trait_context: None,
                macro_expansion: None,
                monikers: Vec::new(),
            }],
            note: None,
        };
        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            "src/batch.rs:142:8: reachable incoming call depth=1 root #0 fn process_batch\n\
             src/server.rs:87:13: d1 call #1 fn handle -> #0 fn process_batch: let total = process_batch(&items)?;",
        );
    }

    #[test]
    fn match_line_tags_context_and_truncates_multiline_text() {
        let m = SerializableMatch {
            file_path: PathBuf::from("/ws/src/lib.rs"),
            range: range(10, 5),
            matched_text: "target(\n    1,\n)".into(),
            context: SerializableContext {
                enclosing_function: Some("caller".into()),
                enclosing_impl: None,
                properties: vec![ContextProperty::Test],
            },
            usage_kind: SerializableUsageKind::FunctionArg,
        };
        assert_eq!(
            m.render_line(Some(Path::new("/ws"))),
            "src/lib.rs:10:5: target(… [function_arg, in caller, test]",
        );
    }

    #[test]
    fn edit_lines_are_capped_with_a_more_marker() {
        let edits: Vec<SerializableEdit> = (0..MAX_EDIT_LINES + 3)
            .map(|i| SerializableEdit {
                file_path: PathBuf::from("/ws/src/lib.rs"),
                range: range(i as u32 + 1, 1),
                new_text: "renamed".into(),
            })
            .collect();
        let outcome = SsrApplyOutcome { files_changed: 1, edits_applied: edits.len(), edits };
        let rendered = outcome.render(Some(Path::new("/ws")));
        assert_eq!(
            rendered,
            "applied 13 edits in 1 files\n\
             src/lib.rs:1:1: => renamed\n\
             src/lib.rs:2:1: => renamed\n\
             src/lib.rs:3:1: => renamed\n\
             src/lib.rs:4:1: => renamed\n\
             src/lib.rs:5:1: => renamed\n\
             src/lib.rs:6:1: => renamed\n\
             src/lib.rs:7:1: => renamed\n\
             src/lib.rs:8:1: => renamed\n\
             src/lib.rs:9:1: => renamed\n\
             src/lib.rs:10:1: => renamed\n\
             [3 more edits]",
        );
    }

    #[test]
    fn add_argument_reports_skipped_macro_call_sites() {
        let edits = vec![SerializableEdit {
            file_path: PathBuf::from("/ws/src/lib.rs"),
            range: range(4, 5),
            new_text: "todo!(), ".into(),
        }];
        let preview =
            AddArgumentPreviewResult { total_edits: edits.len(), skipped: 2, edits: edits.clone() };
        assert_eq!(
            preview.render(Some(Path::new("/ws"))),
            "src/lib.rs:4:5: => todo!(), \n\
             [rerun with apply=true to write these edits]\n\
             [2 call sites (in macros or unrecognized forms) were not modified]",
        );

        let outcome = AddArgumentApplyOutcome {
            files_changed: 1,
            edits_applied: edits.len(),
            skipped: 2,
            edits,
        };
        assert_eq!(
            outcome.render(Some(Path::new("/ws"))),
            "applied 1 edits in 1 files\n\
             src/lib.rs:4:5: => todo!(), \n\
             [2 call sites (in macros or unrecognized forms) were not modified]",
        );
    }

    #[test]
    fn paths_outside_the_root_stay_absolute() {
        let definition = InspectResult {
            metadata: PositionMetadata::default(),
            definitions: vec![DefinitionLocation {
                file_path: PathBuf::from("/other/lib.rs"),
                range: range(1, 1),
                name: "foo".into(),
                kind: Some(SerializableSymbolKind::Function),
                container_name: None,
                description: None,
            }],
        };
        assert_eq!(
            definition.render(Some(Path::new("/ws"))),
            "/other/lib.rs:1:1: definition fn foo",
        );
    }

    #[test]
    fn read_renders_grep_header_and_raw_text() {
        let result = ReadResult {
            file_path: PathBuf::from("/ws/crates/ide-db/src/search.rs"),
            range: LineColRange { start_line: 1401, start_col: 1, end_line: 1405, end_col: 2 },
            text: "impl ReferenceCategory {\n    fn new() -> ReferenceCategory {\n        ReferenceCategory::READ\n    }\n}".into(),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"crates/ide-db/src/search.rs:1401:1: read

impl ReferenceCategory {
    fn new() -> ReferenceCategory {
        ReferenceCategory::READ
    }
}"#,
        );
    }

    #[test]
    fn inspect_renders_grep_definition_and_labeled_metadata() {
        let result = InspectResult {
            definitions: vec![DefinitionLocation {
                file_path: PathBuf::from("/ws/src/server.rs"),
                range: range(523, 14),
                name: "workspace_for_request".into(),
                kind: Some(SerializableSymbolKind::Function),
                container_name: Some("RaMcpServer".into()),
                description: Some(
                    "async fn workspace_for_request(&self, context: &RequestContext<rmcp::RoleServer>) -> Result<RequestWorkspace, McpError>".into(),
                ),
            }],
            metadata: PositionMetadata::from_markup(
                r#"```rust
ra_mcp_handlers::server
```

```rust
async fn workspace_for_request(
    &self,
    context: &RequestContext<rmcp::RoleServer>,
) -> Result<RequestWorkspace, McpError>
```

---

Loads the request workspace."#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"src/server.rs:523:14: definition fn workspace_for_request in RaMcpServer

qualified_path:
```rust
ra_mcp_handlers::server
```

signature:
```rust
async fn workspace_for_request(
    &self,
    context: &RequestContext<rmcp::RoleServer>,
) -> Result<RequestWorkspace, McpError>
```

docs:
Loads the request workspace."#,
        );
    }

    #[test]
    fn inspect_omits_absent_docs_section() {
        let result = InspectResult {
            definitions: Vec::new(),
            metadata: PositionMetadata::from_markup(
                r#"```rust
ra_mcp_handlers::types
```

```rust
pub struct InspectResult {
    pub definitions: Vec<DefinitionLocation>,
    pub metadata: PositionMetadata,
}
```"#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"qualified_path:
```rust
ra_mcp_handlers::types
```

declaration:
```rust
pub struct InspectResult {
    pub definitions: Vec<DefinitionLocation>,
    pub metadata: PositionMetadata,
}
```"#,
        );
    }

    #[test]
    fn inspect_classifies_single_path_like_code_block_as_type() {
        let result = InspectResult {
            definitions: Vec::new(),
            metadata: PositionMetadata::from_markup(
                r#"```rust
String
```"#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"type:
```rust
String
```"#,
        );
    }

    #[test]
    fn inspect_classifies_field_code_block_as_declaration() {
        let result = InspectResult {
            definitions: Vec::new(),
            metadata: PositionMetadata::from_markup(
                r#"```rust
pub kind: SerializableInlayKind
```"#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"declaration:
```rust
pub kind: SerializableInlayKind
```"#,
        );
    }

    #[test]
    fn inspect_classifies_variant_code_block_as_declaration() {
        let result = InspectResult {
            definitions: Vec::new(),
            metadata: PositionMetadata::from_markup(
                r#"```rust
Code = 4
```"#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"declaration:
```rust
Code = 4
```"#,
        );
    }

    #[test]
    fn inspect_preserves_docs_that_say_no_docs() {
        let result = InspectResult {
            definitions: Vec::new(),
            metadata: PositionMetadata::from_markup(
                r#"```rust
fn confusing()
```

---

No docs."#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"signature:
```rust
fn confusing()
```

docs:
No docs."#,
        );
    }

    #[test]
    fn inspect_keeps_docs_markdown_inside_docs_section() {
        let result = InspectResult {
            definitions: Vec::new(),
            metadata: PositionMetadata::from_markup(
                r#"```rust
let
```

---

Bind a value.

```rust
let value = 1;
```"#,
            ),
        };

        assert_eq!(
            result.render(Some(Path::new("/ws"))),
            r#"code:
```rust
let
```

docs:
Bind a value.

```rust
let value = 1;
```"#,
        );
    }
}
