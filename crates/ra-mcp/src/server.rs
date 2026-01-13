//! MCP tool routing and request handlers.

mod edits;
mod grouping;
mod ssr;

#[doc(hidden)] // exposed for tests; not a stable API
pub use self::edits::{ComputedEdits, source_change_to_serialized_edits};
#[doc(hidden)] // exposed for tests; not a stable API
pub use self::ssr::{search_matches, ssr_compute_edits};

use std::collections::VecDeque;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hir::HirDisplay;
use ide::{
    CallHierarchyConfig, FilePosition, FileStructureConfig, FindAllRefsConfig,
    GotoDefinitionConfig, HoverConfig, HoverDocFormat, InlayHintsConfig, MonikerResult,
    NavigationTarget, RaFixtureConfig, RenameConfig, StructureNode, StructureNodeKind, TryToNav,
};
use ide_db::base_db::{CrateOrigin, SourceDatabase, SourceRootId, local_roots};
use ide_db::defs::{Definition, NameRefClass};
use ide_db::symbol_index::Query;
use ide_db::{FileId, FileRange, FxHashMap, FxHashSet, RootDatabase, SymbolKind};
use rmcp::handler::server::router::tool::ToolRouter;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{
    CallToolRequestParams, CallToolResult, Content, ErrorCode, ErrorData, Implementation,
    JsonObject, ListToolsResult, PaginatedRequestParams, ProtocolVersion, ServerCapabilities,
    ServerInfo, Tool,
};
use rmcp::service::RequestContext;
use rmcp::{ServerHandler, tool, tool_router};
use schemars::JsonSchema;
use serde::Serialize;
use syntax::ast::HasVisibility;
use syntax::{AstNode, Edition, SourceFile, SyntaxKind, SyntaxNode, TextRange, TextSize, ast};
use url::Url;

use crate::analysis::{Analysis, Snapshot, Workspace};
use crate::error::{AnalysisError, EnvironmentError, McpError, RequestError, cancelled_in};
use crate::params::{
    ArgumentIndex, Column, DispatchFilterText, Line, PlaceholderText, RenameTargetText,
};
use crate::requests::{
    AddArgumentRequest, AssocMode, CaseSensitivity, ExpandMacroRequest, FindSymbolRequest,
    InlayHintsRequest, InspectRequest, LineColRangeRequest, PathFilter, ReachableRequest,
    ReadRequest, RenameRequest, SearchMode, SearchRequest, SearchScope, SsrRequest,
    SymbolKindFilter, SymbolSearchParams,
};
use crate::server::edits::{
    EditCounts, apply_text_edits_to_disk, source_change_to_computed_edits,
    source_change_to_raw_edits,
};
use crate::server::grouping::{
    FilterCriteria, count_matches_by, filter_matches, group_key_file, group_key_function,
    group_key_impl, group_key_usage_kind,
};
use crate::server::ssr::ssr_preview_inner;
use crate::types::{
    AddArgumentOutput, ConversionContext, CrateInfo, DefinitionLocation, ExpandMacroResult,
    GroupByField, InlayHintsResult, InspectResult, PositionMetadata, ReachabilityDepthCount,
    ReachableDirection, ReachableEdge, ReachableEdgeKind, ReachableEnclosingItem,
    ReachableGenericArgument, ReachableLocation, ReachableNode, ReachableResult, ReachableScope,
    ReachableSourceExcerpt, ReachableUsageKind, ReadResult, ReferenceCategoryTag, RenameResult,
    SearchOutput, SerializableFileSystemEdit, SerializableInlayHint, SerializableInlayHintPosition,
    SerializableInlayKind, SerializableSymbol, SerializableSymbolKind, SsrOutput, SymbolsResults,
    ToolOutput, ToolRejection, ToolRejectionCode, WorkspaceCratesResult,
};

/// Caps on how much detail hover renders. These ceilings keep responses small
/// enough for LLM context windows without losing the leading content of a
/// type. Adjust here rather than per call site.
const HOVER_MAX_FIELDS: usize = 10;
const HOVER_MAX_ENUM_VARIANTS: usize = 10;

/// Inlay hint label cap. Same trade-off as the hover caps: long hint labels
/// (full type signatures with many generic arguments) bloat responses for
/// little marginal value.
const INLAY_HINT_MAX_LENGTH: usize = 25;

/// Hover policy: markdown output, no links (the MCP client renders its own),
/// docs and keyword hovers included, no memory layout (rust-analyzer-internal
/// debugging info), no drop-glue annotations. Field/variant lists capped to
/// keep responses bounded; full substitution types preserved.
fn hover_config() -> HoverConfig<'static> {
    HoverConfig {
        links_in_hover: false,
        memory_layout: None,
        documentation: true,
        keywords: true,
        format: HoverDocFormat::Markdown,
        max_fields_count: Some(HOVER_MAX_FIELDS),
        max_enum_variants_count: Some(HOVER_MAX_ENUM_VARIANTS),
        max_subst_ty_len: ide::SubstTyLen::Unlimited,
        max_trait_assoc_items_count: None,
        show_drop_glue: false,
        ra_fixture: RaFixtureConfig::default(),
    }
}

/// Inlay hints policy: type, parameter, chaining, and discriminant hints are
/// enabled because they convey information that is not visible in the source.
/// Lifetime, binding-mode, closure-capture, and adjustment hints are off
/// because they pollute output and an LLM caller can derive them from context.
fn inlay_hints_config() -> InlayHintsConfig<'static> {
    InlayHintsConfig {
        render_colons: true,
        type_hints: true,
        type_hints_placement: ide::TypeHintsPlacement::Inline,
        sized_bound: false,
        parameter_hints: true,
        parameter_hints_for_missing_arguments: false,
        chaining_hints: true,
        discriminant_hints: ide::DiscriminantHints::Always,
        closure_return_type_hints: ide::ClosureReturnTypeHints::WithBlock,
        closure_capture_hints: false,
        binding_mode_hints: false,
        lifetime_elision_hints: ide::LifetimeElisionHints::Never,
        param_names_for_lifetime_elision_hints: false,
        hide_named_constructor_hints: false,
        hide_closure_initialization_hints: false,
        hide_closure_parameter_hints: false,
        hide_inferred_type_hints: false,
        implied_dyn_trait_hints: false,
        closure_style: hir::ClosureStyle::ImplFn,
        max_length: Some(INLAY_HINT_MAX_LENGTH),
        closing_brace_hints_min_lines: None,
        implicit_drop_hints: false,
        range_exclusive_hints: false,
        fields_to_resolve: ide::InlayFieldsToResolve::empty(),
        adjustment_hints: ide::AdjustmentHints::Never,
        adjustment_hints_mode: ide::AdjustmentHintsMode::Prefix,
        adjustment_hints_hide_outside_unsafe: false,
        adjustment_hints_disable_reborrows: false,
        generic_parameter_hints: ide::GenericParameterHints {
            type_hints: false,
            lifetime_hints: false,
            const_hints: false,
        },
        ra_fixture: RaFixtureConfig::default(),
    }
}

fn source_declaration(
    db: &RootDatabase,
    file_id: FileId,
    full_range: TextRange,
    focus_range: TextRange,
    kind: Option<SymbolKind>,
) -> Option<String> {
    let text = db.file_text(file_id).text(db);
    match kind? {
        SymbolKind::Field => {
            let source = SourceFile::parse(&text, Edition::CURRENT).tree();
            if let Some(field) =
                syntax::algo::find_node_at_range::<ast::RecordField>(source.syntax(), focus_range)
            {
                let container = declaration_container_range(field.syntax())?;
                return focus_source_range(&text, container, field.syntax().text_range());
            }
            let field =
                syntax::algo::find_node_at_range::<ast::TupleField>(source.syntax(), focus_range)?;
            let container = declaration_container_range(field.syntax())?;
            focus_source_range(&text, container, field.syntax().text_range())
        }
        SymbolKind::Variant => {
            let source = SourceFile::parse(&text, Edition::CURRENT).tree();
            let variant =
                syntax::algo::find_node_at_range::<ast::Variant>(source.syntax(), focus_range)?;
            let container = declaration_container_range(variant.syntax())?;
            focus_source_range(&text, container, variant.syntax().text_range())
        }
        SymbolKind::Const
        | SymbolKind::Enum
        | SymbolKind::Function
        | SymbolKind::Method
        | SymbolKind::Static
        | SymbolKind::Struct
        | SymbolKind::Trait
        | SymbolKind::TypeAlias
        | SymbolKind::Union => {
            let start = usize::from(full_range.start());
            let end = usize::from(full_range.end());
            Some(text[start..end].to_owned())
        }
        SymbolKind::Attribute
        | SymbolKind::BuiltinAttr
        | SymbolKind::ConstParam
        | SymbolKind::CrateRoot
        | SymbolKind::Derive
        | SymbolKind::DeriveHelper
        | SymbolKind::Impl
        | SymbolKind::InlineAsmRegOrRegClass
        | SymbolKind::Label
        | SymbolKind::LifetimeParam
        | SymbolKind::Local
        | SymbolKind::Macro
        | SymbolKind::Module
        | SymbolKind::ProcMacro
        | SymbolKind::SelfParam
        | SymbolKind::SelfType
        | SymbolKind::ToolModule
        | SymbolKind::TypeParam
        | SymbolKind::ValueParam => None,
    }
}

fn declaration_container_range(node: &SyntaxNode) -> Option<TextRange> {
    node.ancestors().find_map(|node| {
        ast::Struct::cast(node.clone())
            .map(|node| node.syntax().text_range())
            .or_else(|| ast::Union::cast(node.clone()).map(|node| node.syntax().text_range()))
            .or_else(|| ast::Enum::cast(node).map(|node| node.syntax().text_range()))
    })
}

fn focus_source_range(text: &str, container: TextRange, focus: TextRange) -> Option<String> {
    if !container.contains_range(focus) {
        return None;
    }
    let container_start = usize::from(container.start());
    let container_end = usize::from(container.end());
    let focus_start = usize::from(focus.start());
    let focus_end = usize::from(focus.end());
    Some(format!(
        "{}/* focus:start */{}/* focus:end */{}",
        &text[container_start..focus_start],
        &text[focus_start..focus_end],
        &text[focus_end..container_end],
    ))
}

#[derive(Default)]
struct ReachableSourceMetadata {
    declaration: Option<String>,
    body: Option<String>,
    visibility: Option<String>,
    trait_context: Option<String>,
    impl_context: Option<String>,
}

struct ReachableUsageMetadata {
    definition: Definition,
    usage_kind: ReachableUsageKind,
    generic_substitution: Vec<ReachableGenericArgument>,
    receiver_type: Option<String>,
    dispatch: Option<String>,
    trait_context: Option<String>,
}

fn source_range_text(text: &str, range: TextRange) -> Option<String> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    if end > text.len() || start > end {
        return None;
    }
    Some(text[start..end].to_owned())
}

fn header_text(text: &str, node: &SyntaxNode) -> Option<String> {
    let start = node.text_range().start();
    let end = node
        .children_with_tokens()
        .find(|child| child.kind() == SyntaxKind::L_CURLY)
        .map(|child| child.text_range().start())
        .unwrap_or_else(|| node.text_range().end());
    source_range_text(text, TextRange::new(start, end)).map(|text| text.trim().to_owned())
}

fn body_text(node: &SyntaxNode) -> Option<String> {
    for ancestor in node.ancestors() {
        if let Some(func) = ast::Fn::cast(ancestor.clone()) {
            return func.body().map(|body| body.syntax().text().to_string());
        }
        if let Some(const_) = ast::Const::cast(ancestor.clone()) {
            return const_.body().map(|body| body.syntax().text().to_string());
        }
        if let Some(static_) = ast::Static::cast(ancestor) {
            return static_.body().map(|body| body.syntax().text().to_string());
        }
    }
    None
}

fn visibility_text(node: &SyntaxNode) -> Option<String> {
    for ancestor in node.ancestors() {
        if let Some(field) = ast::RecordField::cast(ancestor.clone()) {
            return Some(
                field
                    .visibility()
                    .map(|visibility| visibility.syntax().text().to_string())
                    .unwrap_or_else(|| "private".to_owned()),
            );
        }
        if let Some(field) = ast::TupleField::cast(ancestor.clone()) {
            return Some(
                field
                    .visibility()
                    .map(|visibility| visibility.syntax().text().to_string())
                    .unwrap_or_else(|| "private".to_owned()),
            );
        }
        let Some(item) = ast::Item::cast(ancestor) else {
            continue;
        };
        let visibility = match item {
            ast::Item::Const(item) => item.visibility(),
            ast::Item::Enum(item) => item.visibility(),
            ast::Item::ExternCrate(item) => item.visibility(),
            ast::Item::Fn(item) => item.visibility(),
            ast::Item::Module(item) => item.visibility(),
            ast::Item::Static(item) => item.visibility(),
            ast::Item::Struct(item) => item.visibility(),
            ast::Item::Trait(item) => item.visibility(),
            ast::Item::TypeAlias(item) => item.visibility(),
            ast::Item::Union(item) => item.visibility(),
            ast::Item::Use(item) => item.visibility(),
            ast::Item::AsmExpr(_)
            | ast::Item::ExternBlock(_)
            | ast::Item::Impl(_)
            | ast::Item::MacroCall(_)
            | ast::Item::MacroDef(_)
            | ast::Item::MacroRules(_) => return None,
        };
        return Some(
            visibility
                .map(|visibility| visibility.syntax().text().to_string())
                .unwrap_or_else(|| "private".to_owned()),
        );
    }
    None
}

fn item_contexts(text: &str, node: &SyntaxNode) -> (Option<String>, Option<String>) {
    let mut trait_context = None;
    let mut impl_context = None;
    for ancestor in node.ancestors() {
        if trait_context.is_none()
            && let Some(trait_) = ast::Trait::cast(ancestor.clone())
        {
            trait_context = header_text(text, trait_.syntax());
        }
        if impl_context.is_none()
            && let Some(impl_) = ast::Impl::cast(ancestor)
        {
            impl_context = header_text(text, impl_.syntax());
        }
        if trait_context.is_some() && impl_context.is_some() {
            break;
        }
    }
    (trait_context, impl_context)
}

fn reachable_source_metadata(
    db: &RootDatabase,
    file_id: FileId,
    full_range: TextRange,
    focus_range: TextRange,
    kind: Option<SymbolKind>,
) -> ReachableSourceMetadata {
    if full_range.is_empty() {
        return ReachableSourceMetadata::default();
    }

    let text = db.file_text(file_id).text(db);
    let declaration = if matches!(kind, Some(SymbolKind::CrateRoot)) {
        None
    } else {
        source_declaration(db, file_id, full_range, focus_range, kind)
            .or_else(|| source_range_text(&text, full_range))
    };

    let source = SourceFile::parse(&text, Edition::CURRENT).tree();
    let token = source
        .syntax()
        .token_at_offset(focus_range.start())
        .right_biased()
        .or_else(|| source.syntax().token_at_offset(focus_range.start()).left_biased());
    let Some(node) = token.and_then(|token| token.parent()) else {
        return ReachableSourceMetadata {
            declaration,
            body: None,
            visibility: None,
            trait_context: None,
            impl_context: None,
        };
    };

    let body = body_text(&node);
    let visibility = visibility_text(&node);
    let (trait_context, impl_context) = item_contexts(&text, &node);
    ReachableSourceMetadata { declaration, body, visibility, trait_context, impl_context }
}

fn reference_category_tags(category: ide::ReferenceCategory) -> Vec<ReferenceCategoryTag> {
    let mut tags = Vec::new();
    if category.contains(ide::ReferenceCategory::WRITE) {
        tags.push(ReferenceCategoryTag::Write);
    }
    if category.contains(ide::ReferenceCategory::READ) {
        tags.push(ReferenceCategoryTag::Read);
    }
    if category.contains(ide::ReferenceCategory::IMPORT) {
        tags.push(ReferenceCategoryTag::Import);
    }
    if category.contains(ide::ReferenceCategory::TEST) {
        tags.push(ReferenceCategoryTag::Test);
    }
    tags
}

fn moniker_strings(analysis: &ide::Analysis, position: FilePosition) -> Vec<String> {
    let Some(monikers) = analysis.moniker(position).ok().flatten() else {
        return Vec::new();
    };

    let mut result = Vec::new();
    for moniker in monikers.info {
        match moniker {
            MonikerResult::Moniker(moniker) => result.push(moniker.identifier.to_string()),
            MonikerResult::Local { enclosing_moniker: Some(moniker) } => {
                result.push(format!("local in {}", moniker.identifier))
            }
            MonikerResult::Local { enclosing_moniker: None } => result.push("local".to_owned()),
        }
    }
    result.sort();
    result.dedup();
    result
}

fn macro_expansion_name(
    analysis: &ide::Analysis,
    db: &RootDatabase,
    position: FilePosition,
) -> Option<String> {
    let text = db.file_text(position.file_id).text(db);
    let source = SourceFile::parse(&text, Edition::CURRENT).tree();
    let token = source
        .syntax()
        .token_at_offset(position.offset)
        .right_biased()
        .or_else(|| source.syntax().token_at_offset(position.offset).left_biased())?;
    if !token.parent_ancestors().any(|node| ast::MacroCall::cast(node).is_some()) {
        return None;
    }
    analysis.expand_macro(position).ok().flatten().map(|expanded| expanded.name)
}

fn generic_substitution_arguments(
    db: &RootDatabase,
    definition: Definition,
    substitution: Option<&hir::GenericSubstitution<'_>>,
) -> Vec<ReachableGenericArgument> {
    let Some(substitution) = substitution else {
        return Vec::new();
    };
    let Some(display_target) = definition.krate(db).map(|krate| krate.to_display_target(db)) else {
        return Vec::new();
    };

    substitution
        .types(db)
        .into_iter()
        .map(|(parameter, value)| ReachableGenericArgument {
            parameter: parameter.as_str().to_owned(),
            value: value.display(db, display_target).to_string(),
        })
        .collect()
}

fn usage_kind_from_definition(definition: Definition) -> ReachableUsageKind {
    match definition {
        Definition::Macro(_) | Definition::DeriveHelper(_) | Definition::BuiltinAttr(_) => {
            ReachableUsageKind::Macro
        }
        Definition::Function(_) => ReachableUsageKind::Callable,
        Definition::Adt(_)
        | Definition::BuiltinType(_)
        | Definition::SelfType(_)
        | Definition::Trait(_)
        | Definition::TypeAlias(_) => ReachableUsageKind::Type,
        Definition::Field(_) | Definition::TupleField(_) => ReachableUsageKind::Field,
        Definition::Module(_) | Definition::Crate(_) | Definition::ToolModule(_) => {
            ReachableUsageKind::Module
        }
        Definition::Const(_) | Definition::EnumVariant(_) | Definition::Static(_) => {
            ReachableUsageKind::Value
        }
        Definition::GenericParam(_) => ReachableUsageKind::GenericParam,
        Definition::Local(_) => ReachableUsageKind::Local,
        Definition::Label(_) => ReachableUsageKind::Label,
        Definition::BuiltinLifetime(_)
        | Definition::InlineAsmOperand(_)
        | Definition::InlineAsmRegOrRegClass(_) => ReachableUsageKind::Builtin,
        Definition::ExternCrateDecl(_) => ReachableUsageKind::ExternCrateShorthand,
    }
}

fn dispatch_from_name_ref(name_ref: &ast::NameRef, fallback: ReachableUsageKind) -> String {
    let Some(parent) = name_ref.syntax().parent() else {
        return fallback.as_str().to_owned();
    };
    if ast::MethodCallExpr::cast(parent.clone()).is_some() {
        return "method_call".to_owned();
    }
    if ast::FieldExpr::cast(parent.clone()).is_some() {
        return "field_access".to_owned();
    }
    if ast::RecordExprField::cast(parent.clone()).is_some() {
        return "record_expr_field".to_owned();
    }
    if ast::RecordPatField::cast(parent.clone()).is_some() {
        return "record_pat_field".to_owned();
    }
    if ast::AssocTypeArg::cast(parent.clone()).is_some() {
        return "associated_type_argument".to_owned();
    }
    if ast::PathSegment::cast(parent).is_some() {
        return "path".to_owned();
    }
    fallback.as_str().to_owned()
}

fn receiver_type(
    db: &RootDatabase,
    sema: &hir::Semantics<'_, RootDatabase>,
    name_ref: &ast::NameRef,
    definition: Definition,
) -> Option<String> {
    let parent = name_ref.syntax().parent()?;
    let method_call = ast::MethodCallExpr::cast(parent)?;
    let receiver = method_call.receiver()?;
    let display_target = definition.krate(db)?.to_display_target(db);
    let ty = sema.type_of_expr(&receiver)?.adjusted();
    Some(ty.display(db, display_target).to_string())
}

fn trait_context_from_definition(db: &RootDatabase, definition: Definition) -> Option<String> {
    let enclosing = definition.enclosing_definition(db)?;
    match enclosing {
        Definition::Trait(_) => {
            let display_target = enclosing.krate(db)?.to_display_target(db);
            Some(enclosing.label(db, display_target))
        }
        Definition::Macro(_)
        | Definition::Field(_)
        | Definition::TupleField(_)
        | Definition::Module(_)
        | Definition::Crate(_)
        | Definition::Function(_)
        | Definition::Adt(_)
        | Definition::EnumVariant(_)
        | Definition::Const(_)
        | Definition::Static(_)
        | Definition::TypeAlias(_)
        | Definition::SelfType(_)
        | Definition::GenericParam(_)
        | Definition::Local(_)
        | Definition::Label(_)
        | Definition::DeriveHelper(_)
        | Definition::BuiltinType(_)
        | Definition::BuiltinLifetime(_)
        | Definition::BuiltinAttr(_)
        | Definition::ToolModule(_)
        | Definition::ExternCrateDecl(_)
        | Definition::InlineAsmRegOrRegClass(_)
        | Definition::InlineAsmOperand(_) => None,
    }
}

fn usage_metadata_from_name_ref_class(
    db: &RootDatabase,
    sema: &hir::Semantics<'_, RootDatabase>,
    name_ref: &ast::NameRef,
    class: NameRefClass<'_>,
    category: ide::ReferenceCategory,
) -> ReachableUsageMetadata {
    let is_import = category.contains(ide::ReferenceCategory::IMPORT);
    let (definition, usage_kind, generic_substitution) = match class {
        NameRefClass::Definition(definition, substitution) => {
            let usage_kind = if is_import {
                ReachableUsageKind::Import
            } else {
                usage_kind_from_definition(definition)
            };
            (
                definition,
                usage_kind,
                generic_substitution_arguments(db, definition, substitution.as_ref()),
            )
        }
        NameRefClass::FieldShorthand { field_ref, adt_subst, .. } => (
            Definition::Field(field_ref),
            ReachableUsageKind::FieldShorthand,
            generic_substitution_arguments(db, Definition::Field(field_ref), Some(&adt_subst)),
        ),
        NameRefClass::ExternCrateShorthand { krate, .. } => {
            (Definition::Crate(krate), ReachableUsageKind::ExternCrateShorthand, Vec::new())
        }
    };
    ReachableUsageMetadata {
        definition,
        usage_kind,
        generic_substitution,
        receiver_type: receiver_type(db, sema, name_ref, definition),
        dispatch: Some(dispatch_from_name_ref(name_ref, usage_kind)),
        trait_context: trait_context_from_definition(db, definition),
    }
}

fn usage_metadata_at_reference(
    db: &RootDatabase,
    sema: &hir::Semantics<'_, RootDatabase>,
    file_id: FileId,
    range: TextRange,
    category: ide::ReferenceCategory,
) -> Option<ReachableUsageMetadata> {
    let source_file = sema.parse_guess_edition(file_id);
    let name_ref = syntax::algo::find_node_at_range::<ast::NameRef>(source_file.syntax(), range)?;
    let class = NameRefClass::classify(sema, &name_ref)?;
    Some(usage_metadata_from_name_ref_class(db, sema, &name_ref, class, category))
}

/// All successful tool output goes through here as transcript text plus the
/// same typed result in `structuredContent`.
fn to_tool_result<T>(result: &T, text: String) -> Result<CallToolResult, ErrorData>
where
    T: Serialize,
{
    to_call_tool_result(result, text)
}

fn to_tool_rejection_result(error: ErrorData) -> Result<CallToolResult, ErrorData> {
    let code = tool_rejection_code(error.code);
    let guidance = tool_rejection_guidance(code);
    let result =
        ToolRejection { code, message: error.message.into_owned(), guidance: guidance.to_owned() };
    let text = result.render();
    to_call_tool_result(&result, text)
}

fn tool_rejection_code(code: ErrorCode) -> ToolRejectionCode {
    if code == ErrorCode::INVALID_REQUEST {
        ToolRejectionCode::InvalidRequest
    } else if code == ErrorCode::INVALID_PARAMS {
        ToolRejectionCode::InvalidParams
    } else if code == ErrorCode::INTERNAL_ERROR {
        ToolRejectionCode::InternalError
    } else if code == ErrorCode::METHOD_NOT_FOUND {
        ToolRejectionCode::MethodNotFound
    } else if code == ErrorCode::PARSE_ERROR {
        ToolRejectionCode::ParseError
    } else if code == ErrorCode::RESOURCE_NOT_FOUND {
        ToolRejectionCode::ResourceNotFound
    } else {
        ToolRejectionCode::Unknown
    }
}

fn tool_rejection_guidance(code: ToolRejectionCode) -> &'static str {
    match code {
        ToolRejectionCode::InvalidParams => {
            "Adjust the arguments and rerun. For symbol navigation, prefer find_symbol, inspect, or reachable; use search only for valid rust-analyzer structural-search patterns."
        }
        ToolRejectionCode::InvalidRequest
        | ToolRejectionCode::MethodNotFound
        | ToolRejectionCode::ParseError => {
            "Check the request shape and rerun. This is a rejected request, not a ra-mcp process failure."
        }
        ToolRejectionCode::InternalError
        | ToolRejectionCode::ResourceNotFound
        | ToolRejectionCode::Unknown => {
            "Retry after the workspace finishes loading. If this repeats, reduce the request and inspect the ra-mcp logs."
        }
    }
}

fn to_call_tool_result<T>(result: &T, text: String) -> Result<CallToolResult, ErrorData>
where
    T: Serialize,
{
    let structured_content = serde_json::to_value(result).map_err(|e| {
        ErrorData::internal_error(format!("failed to serialize structured tool result: {e}"), None)
    })?;
    if !structured_content.is_object() {
        return Err(ErrorData::internal_error(
            "structured tool result must serialize to a JSON object",
            None,
        ));
    }

    Ok(CallToolResult {
        content: vec![Content::text(text)],
        structured_content: Some(structured_content),
        is_error: Some(false),
        meta: None,
    })
}

fn schema_for<T: JsonSchema + 'static>() -> Result<Arc<JsonObject>, ErrorData> {
    rmcp::handler::server::tool::schema_for_output::<ToolOutput<T>>()
        .map_err(|e| ErrorData::internal_error(format!("invalid ra-mcp output schema: {e}"), None))
}

fn set_output_schema(tool: &mut Tool) -> Result<(), ErrorData> {
    let schema = match tool.name.as_ref() {
        "add_argument" => schema_for::<AddArgumentOutput>(),
        "expand_macro" => schema_for::<ExpandMacroResult>(),
        "find_symbol" => schema_for::<SymbolsResults>(),
        "inlay_hints" => schema_for::<InlayHintsResult>(),
        "inspect" => schema_for::<InspectResult>(),
        "read" => schema_for::<ReadResult>(),
        "rename" => schema_for::<RenameResult>(),
        "search" => schema_for::<SearchOutput>(),
        "ssr" => schema_for::<SsrOutput>(),
        "reachable" => schema_for::<ReachableResult>(),
        "workspace_crates" => schema_for::<WorkspaceCratesResult>(),
        _ => return Ok(()),
    }?;
    tool.output_schema = Some(schema);
    Ok(())
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn add_argument_source_change(
    snapshot: &Snapshot,
    file: &Path,
    line: Line,
    column: Column,
    argument_index: ArgumentIndex,
    placeholder: &PlaceholderText,
) -> Result<ide::AddArgumentResult, McpError> {
    let position = snapshot.resolve_position(file, line, column)?;
    let analysis = snapshot.analysis();

    let result = analysis
        .add_argument_to_call_sites(position, argument_index.get(), placeholder.as_str())
        .map_err(cancelled_in("add argument"))?
        .ok_or(AnalysisError::NotFound { what: "function at position" })?;

    Ok(result)
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn rename_source_change(
    snapshot: &Snapshot,
    file: &Path,
    line: Line,
    column: Column,
    new_name: &RenameTargetText,
) -> Result<ide::SourceChange, McpError> {
    let position = snapshot.resolve_position(file, line, column)?;
    let analysis = snapshot.analysis();

    let config = RenameConfig { show_conflicts: true, ..RenameConfig::default() };

    let source_change = analysis
        .rename(position, new_name.as_str(), &config)
        .map_err(cancelled_in("rename"))?
        .map_err(|e| RequestError::RenameRejected { reason: e.to_string() })?;

    Ok(source_change)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ReachableNodeKey {
    file_id: FileId,
    range: TextRange,
}

#[derive(Debug, Clone, Copy)]
struct ReachablePredecessor {
    parent_node: usize,
    edge: usize,
}

struct ReachabilityBuilder {
    nodes: Vec<ReachableNode>,
    positions: Vec<FilePosition>,
    scan_ranges: Vec<FileRange>,
    node_keys: Vec<ReachableNodeKey>,
    keys: FxHashMap<ReachableNodeKey, usize>,
    depths: Vec<ReachabilityDepthCount>,
    edges: Vec<ReachableEdge>,
    predecessors: Vec<Option<ReachablePredecessor>>,
    frontier: VecDeque<(usize, u32)>,
}

impl ReachabilityBuilder {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            positions: Vec::new(),
            scan_ranges: Vec::new(),
            node_keys: Vec::new(),
            keys: FxHashMap::default(),
            depths: Vec::new(),
            edges: Vec::new(),
            predecessors: Vec::new(),
            frontier: VecDeque::new(),
        }
    }

    fn depth_count(&mut self, depth: u32) -> &mut ReachabilityDepthCount {
        while self.depths.len() <= depth as usize {
            let depth = self.depths.len() as u32;
            self.depths.push(ReachabilityDepthCount { depth, nodes: 0, edges: 0 });
        }
        &mut self.depths[depth as usize]
    }

    fn insert_nav(
        &mut self,
        analysis: &ide::Analysis,
        db: &RootDatabase,
        ctx: &ConversionContext<'_>,
        nav: &NavigationTarget,
        depth: u32,
    ) -> Option<(usize, bool)> {
        let key = reachable_nav_key(nav);
        let range = key.range;
        if let Some(id) = self.keys.get(&key) {
            return Some((*id, false));
        }

        let file_path = ctx.file_path(nav.file_id)?;
        let id = self.nodes.len();
        self.keys.insert(key, id);
        self.node_keys.push(key);
        self.positions.push(FilePosition { file_id: nav.file_id, offset: range.start() });
        self.scan_ranges.push(FileRange { file_id: nav.file_id, range: nav.full_range });
        self.predecessors.push(None);
        let source = reachable_source_metadata(db, nav.file_id, nav.full_range, range, nav.kind);
        let position = FilePosition { file_id: nav.file_id, offset: range.start() };
        self.nodes.push(ReachableNode {
            id,
            depth,
            file_path,
            range: ctx.text_range_to_line_col(nav.file_id, range),
            full_range: ctx.text_range_to_line_col(nav.file_id, nav.full_range),
            name: nav.name.as_str().to_owned(),
            kind: nav.kind.map(SerializableSymbolKind::from),
            container_name: nav.container_name.as_ref().map(|name| name.as_str().to_owned()),
            description: nav.description.clone(),
            docs: None,
            signature: nav.description.clone(),
            declaration: source.declaration,
            body: source.body,
            visibility: source.visibility,
            trait_context: source.trait_context,
            impl_context: source.impl_context,
            monikers: moniker_strings(analysis, position),
        });
        self.depth_count(depth).nodes += 1;
        Some((id, true))
    }

    fn insert_structure(
        &mut self,
        analysis: &ide::Analysis,
        db: &RootDatabase,
        ctx: &ConversionContext<'_>,
        file_id: FileId,
        structure: &StructureNode,
        container_name: Option<String>,
        depth: u32,
    ) -> Option<(usize, bool)> {
        let key_range = match structure.kind {
            StructureNodeKind::SymbolKind(SymbolKind::Impl) => structure.node_range,
            StructureNodeKind::SymbolKind(_)
            | StructureNodeKind::ExternBlock
            | StructureNodeKind::Region => structure.navigation_range,
        };
        let key = ReachableNodeKey { file_id, range: key_range };
        if let Some(id) = self.keys.get(&key) {
            return Some((*id, false));
        }

        let file_path = ctx.file_path(file_id)?;
        let kind = match structure.kind {
            StructureNodeKind::SymbolKind(kind) => Some(SerializableSymbolKind::from(kind)),
            StructureNodeKind::ExternBlock | StructureNodeKind::Region => None,
        };
        let symbol_kind = match structure.kind {
            StructureNodeKind::SymbolKind(kind) => Some(kind),
            StructureNodeKind::ExternBlock | StructureNodeKind::Region => None,
        };
        let id = self.nodes.len();
        self.keys.insert(key, id);
        self.node_keys.push(key);
        self.positions.push(FilePosition { file_id, offset: structure.navigation_range.start() });
        self.scan_ranges.push(FileRange { file_id, range: structure.node_range });
        self.predecessors.push(None);
        let source = reachable_source_metadata(
            db,
            file_id,
            structure.node_range,
            structure.navigation_range,
            symbol_kind,
        );
        let position = FilePosition { file_id, offset: structure.navigation_range.start() };
        self.nodes.push(ReachableNode {
            id,
            depth,
            file_path,
            range: ctx.text_range_to_line_col(file_id, structure.navigation_range),
            full_range: ctx.text_range_to_line_col(file_id, structure.node_range),
            name: structure.label.clone(),
            kind,
            container_name,
            description: structure.detail.clone(),
            docs: None,
            signature: structure.detail.clone(),
            declaration: source.declaration,
            body: source.body,
            visibility: source.visibility,
            trait_context: source.trait_context,
            impl_context: source.impl_context,
            monikers: moniker_strings(analysis, position),
        });
        self.depth_count(depth).nodes += 1;
        Some((id, true))
    }

    fn insert_file_root(
        &mut self,
        analysis: &ide::Analysis,
        db: &RootDatabase,
        ctx: &ConversionContext<'_>,
        file_id: FileId,
        depth: u32,
    ) -> Option<(usize, bool)> {
        let key = ReachableNodeKey { file_id, range: TextRange::empty(TextSize::from(0)) };
        if let Some(id) = self.keys.get(&key) {
            return Some((*id, false));
        }

        let text = db.file_text(file_id).text(db);
        let file_path = ctx.file_path(file_id)?;
        let name = file_path
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_else(|| file_path.display().to_string());
        let id = self.nodes.len();
        self.keys.insert(key, id);
        self.node_keys.push(key);
        self.positions.push(FilePosition { file_id, offset: TextSize::from(0) });
        self.scan_ranges
            .push(FileRange { file_id, range: TextRange::up_to(TextSize::of(&**text)) });
        self.predecessors.push(None);
        let full_range = TextRange::up_to(TextSize::of(&**text));
        self.nodes.push(ReachableNode {
            id,
            depth,
            file_path,
            range: ctx.text_range_to_line_col(file_id, TextRange::empty(TextSize::from(0))),
            full_range: ctx.text_range_to_line_col(file_id, full_range),
            name,
            kind: Some(SerializableSymbolKind::Module),
            container_name: None,
            description: None,
            docs: None,
            signature: None,
            declaration: None,
            body: None,
            visibility: None,
            trait_context: None,
            impl_context: None,
            monikers: moniker_strings(
                analysis,
                FilePosition { file_id, offset: TextSize::from(0) },
            ),
        });
        self.depth_count(depth).nodes += 1;
        Some((id, true))
    }

    fn push_edge(&mut self, edge: ReachableEdge) -> usize {
        self.depth_count(edge.depth).edges += 1;
        let index = self.edges.len();
        self.edges.push(edge);
        index
    }

    fn record_predecessor(&mut self, child_node: usize, parent_node: usize, edge: usize) {
        if child_node >= self.predecessors.len() {
            return;
        }
        if self.predecessors[child_node].is_none() {
            self.predecessors[child_node] = Some(ReachablePredecessor { parent_node, edge });
        }
    }

    fn into_path(
        self,
        root_ids: &[usize],
        target_id: usize,
    ) -> (Vec<usize>, Vec<ReachabilityDepthCount>, Vec<ReachableNode>, Vec<ReachableEdge>) {
        let mut path_nodes = vec![target_id];
        let mut path_edges = Vec::new();
        let mut current = target_id;
        while !root_ids.contains(&current) {
            let Some(predecessor) = self.predecessors[current] else {
                break;
            };
            path_edges.push(predecessor.edge);
            current = predecessor.parent_node;
            path_nodes.push(current);
        }
        path_nodes.reverse();
        path_edges.reverse();

        let mut remapped_nodes = FxHashMap::default();
        let mut nodes = Vec::new();
        for old_id in path_nodes {
            let mut node = self.nodes[old_id].clone();
            let new_id = nodes.len();
            node.id = new_id;
            remapped_nodes.insert(old_id, new_id);
            nodes.push(node);
        }

        let mut edges = Vec::new();
        for edge_id in path_edges {
            let mut edge = self.edges[edge_id].clone();
            let Some(source_node) = remapped_nodes.get(&edge.source_node) else {
                continue;
            };
            let Some(target_node) = remapped_nodes.get(&edge.target_node) else {
                continue;
            };
            edge.source_node = *source_node;
            edge.target_node = *target_node;
            edges.push(edge);
        }

        let mut depths = Vec::new();
        for node in &nodes {
            while depths.len() <= node.depth as usize {
                let depth = depths.len() as u32;
                depths.push(ReachabilityDepthCount { depth, nodes: 0, edges: 0 });
            }
            depths[node.depth as usize].nodes += 1;
        }
        for edge in &edges {
            while depths.len() <= edge.depth as usize {
                let depth = depths.len() as u32;
                depths.push(ReachabilityDepthCount { depth, nodes: 0, edges: 0 });
            }
            depths[edge.depth as usize].edges += 1;
        }

        let roots = root_ids
            .iter()
            .filter_map(|root| remapped_nodes.get(root).copied())
            .collect::<Vec<_>>();
        (roots, depths, nodes, edges)
    }
}

fn reachable_nav_key(nav: &NavigationTarget) -> ReachableNodeKey {
    ReachableNodeKey { file_id: nav.file_id, range: nav.focus_or_full_range() }
}

fn structure_contains_range(structure: &StructureNode, range: TextRange) -> bool {
    if range.is_empty() {
        return structure.node_range.contains(range.start());
    }
    structure.node_range.contains_range(range)
}

fn enclosing_structure(
    structures: &[StructureNode],
    range: TextRange,
) -> Option<(&StructureNode, Option<String>)> {
    let mut enclosing: Option<&StructureNode> = None;
    for structure in structures {
        if !structure_contains_range(structure, range) {
            continue;
        }
        match enclosing {
            Some(current) if current.node_range.len() <= structure.node_range.len() => (),
            Some(_) | None => enclosing = Some(structure),
        }
    }

    let structure = enclosing?;
    let container_name =
        structure.parent.and_then(|idx| structures.get(idx)).map(|parent| parent.label.clone());
    Some((structure, container_name))
}

fn cached_file_structures<'a>(
    analysis: &ide::Analysis,
    structure_cache: &'a mut FxHashMap<FileId, Vec<StructureNode>>,
    file_id: FileId,
) -> Result<&'a [StructureNode], McpError> {
    if !structure_cache.contains_key(&file_id) {
        let structures = analysis
            .file_structure(&FileStructureConfig { exclude_locals: true }, file_id)
            .map_err(cancelled_in("file structure"))?;
        structure_cache.insert(file_id, structures);
    }

    Ok(structure_cache.get(&file_id).map_or(&[], Vec::as_slice))
}

fn reachable_enclosing_item(
    analysis: &ide::Analysis,
    ctx: &ConversionContext<'_>,
    structure_cache: &mut FxHashMap<FileId, Vec<StructureNode>>,
    file_id: FileId,
    range: TextRange,
) -> Result<Option<ReachableEnclosingItem>, McpError> {
    let structures = cached_file_structures(analysis, structure_cache, file_id)?;
    let Some((structure, container_name)) = enclosing_structure(structures, range) else {
        return Ok(None);
    };
    let kind = match structure.kind {
        StructureNodeKind::SymbolKind(kind) => Some(SerializableSymbolKind::from(kind)),
        StructureNodeKind::ExternBlock | StructureNodeKind::Region => None,
    };
    Ok(Some(ReachableEnclosingItem {
        name: structure.label.clone(),
        kind,
        range: ctx.text_range_to_line_col(file_id, structure.node_range),
        container_name,
    }))
}

fn syntax_context_node(source_file: &SourceFile, range: TextRange) -> Option<SyntaxNode> {
    let token = source_file
        .syntax()
        .token_at_offset(range.start())
        .right_biased()
        .or_else(|| source_file.syntax().token_at_offset(range.start()).left_biased())?;

    let mut best: Option<(u8, SyntaxNode)> = None;
    for node in token.parent_ancestors() {
        if !node.text_range().contains_range(range) {
            continue;
        }
        let priority = match node.kind() {
            SyntaxKind::MATCH_ARM => 0,
            SyntaxKind::LET_STMT | SyntaxKind::EXPR_STMT => 1,
            SyntaxKind::PARAM
            | SyntaxKind::SELF_PARAM
            | SyntaxKind::RECORD_EXPR_FIELD
            | SyntaxKind::RECORD_PAT_FIELD
            | SyntaxKind::USE
            | SyntaxKind::USE_TREE => 2,
            _ => continue,
        };
        if best.as_ref().is_none_or(|(best_priority, _)| priority < *best_priority) {
            best = Some((priority, node));
        }
        if priority == 0 {
            break;
        }
    }

    best.map(|(_, node)| node)
}

fn reachable_source_excerpt(
    analysis: &ide::Analysis,
    ctx: &ConversionContext<'_>,
    file_id: FileId,
    range: TextRange,
) -> Result<Option<ReachableSourceExcerpt>, McpError> {
    let source_file = analysis.parse(file_id).map_err(cancelled_in("parse file"))?;
    let Some(node) = syntax_context_node(&source_file, range) else {
        return Ok(None);
    };
    let line_range = ctx.text_range_to_line_col(file_id, node.text_range());
    if line_range.start_line == line_range.end_line {
        return Ok(None);
    }

    const MAX_EXCERPT_CHARS: usize = 1200;
    let mut text = node.text().to_string();
    text = text.trim().to_owned();
    if text.chars().count() > MAX_EXCERPT_CHARS {
        text = text.chars().take(MAX_EXCERPT_CHARS).collect::<String>();
        text.push_str("...");
    }

    Ok(Some(ReachableSourceExcerpt {
        kind: format!("{:?}", node.kind()).to_ascii_lowercase(),
        range: line_range,
        text,
    }))
}

fn reachable_location(
    analysis: &ide::Analysis,
    ctx: &ConversionContext<'_>,
    structure_cache: &mut FxHashMap<FileId, Vec<StructureNode>>,
    file_id: FileId,
    range: TextRange,
) -> Result<ReachableLocation, McpError> {
    let location_range = ctx.text_range_to_line_col(file_id, range);
    Ok(ReachableLocation {
        file_path: ctx.file_path(file_id).unwrap_or_default(),
        line_text: ctx.line_text(file_id, location_range.start_line.saturating_sub(1)),
        range: location_range,
        enclosing_item: reachable_enclosing_item(analysis, ctx, structure_cache, file_id, range)?,
        source_excerpt: reachable_source_excerpt(analysis, ctx, file_id, range)?,
    })
}

fn insert_usage_source_node(
    analysis: &ide::Analysis,
    db: &RootDatabase,
    ctx: &ConversionContext<'_>,
    builder: &mut ReachabilityBuilder,
    structure_cache: &mut FxHashMap<FileId, Vec<StructureNode>>,
    file_id: FileId,
    range: TextRange,
    depth: u32,
    filters: &ReachableFilters<'_>,
) -> Result<Option<(usize, bool)>, McpError> {
    let (structure, container_name) = {
        let structures = cached_file_structures(analysis, structure_cache, file_id)?;
        match enclosing_structure(structures, range) {
            Some((structure, container_name)) => (Some(structure.clone()), container_name),
            None => (None, None),
        }
    };

    let kind = match structure.as_ref().map(|structure| structure.kind) {
        Some(StructureNodeKind::SymbolKind(kind)) => Some(SerializableSymbolKind::from(kind)),
        Some(StructureNodeKind::ExternBlock | StructureNodeKind::Region) => None,
        None => Some(SerializableSymbolKind::Module),
    };
    if !filters.matches_node_kind(kind) {
        return Ok(None);
    }

    let node = match structure {
        Some(structure) => {
            builder.insert_structure(analysis, db, ctx, file_id, &structure, container_name, depth)
        }
        None => builder.insert_file_root(analysis, db, ctx, file_id, depth),
    };
    Ok(node)
}

fn diagnose_unresolved_position(
    analysis: &ide::Analysis,
    position: FilePosition,
) -> Option<String> {
    let crates = analysis.crates_for(position.file_id).ok()?;
    if crates.is_empty() {
        return Some(
            "file is not part of any crate in the workspace (semantic analysis unavailable)".into(),
        );
    }

    let source_file = analysis.parse(position.file_id).ok()?;
    let token = source_file.syntax().token_at_offset(position.offset).right_biased();
    let Some(token) = token else {
        return Some(format!("no token found at offset {:?}", position.offset));
    };

    let kind = token.kind();
    let text = token.text();
    let is_ident = kind == syntax::SyntaxKind::IDENT;
    if !is_ident {
        return Some(format!(
            "token at offset {:?} is {kind:?} (`{text}`), not an identifier",
            position.offset,
        ));
    }

    Some(format!(
        "identifier `{text}` at offset {:?} could not be resolved (the crate may have errors)",
        position.offset,
    ))
}

fn resolve_reachable_seed(
    snapshot: &Snapshot,
    file: &Path,
    line: Option<Line>,
    column: Option<Column>,
    range: Option<LineColRangeRequest>,
) -> Result<FilePosition, McpError> {
    match range {
        Some(range) => {
            let (file_id, range) = snapshot.resolve_range(
                file,
                range.start_line,
                range.start_col,
                range.end_line,
                range.end_col,
            )?;
            Ok(FilePosition { file_id, offset: range.start() })
        }
        None => match (line, column) {
            (Some(line), Some(column)) => snapshot.resolve_position(file, line, column),
            (Some(line), None) => Err(RequestError::InvalidPosition {
                line: line.get(),
                column: 0,
                reason: "reachable requires either range or both line and column".into(),
            }
            .into()),
            (None, Some(column)) => Err(RequestError::InvalidPosition {
                line: 0,
                column: column.get(),
                reason: "reachable requires either range or both line and column".into(),
            }
            .into()),
            (None, None) => Err(RequestError::InvalidPosition {
                line: 0,
                column: 0,
                reason: "reachable requires either range or both line and column".into(),
            }
            .into()),
        },
    }
}

fn symbol_seed_position(
    analysis: &ide::Analysis,
    ctx: &ConversionContext<'_>,
    position: FilePosition,
) -> Result<(FilePosition, Option<String>), McpError> {
    let source_file = analysis.parse(position.file_id).map_err(cancelled_in("parse file"))?;
    let tokens = source_file.syntax().token_at_offset(position.offset);
    let right_token = tokens.clone().right_biased();
    let left_token = tokens.left_biased();
    let accepts_token = |token: &syntax::SyntaxToken| {
        let kind = token.kind();
        let is_name_token = kind == SyntaxKind::IDENT
            || kind == SyntaxKind::SELF_KW
            || kind == SyntaxKind::SELF_TYPE_KW;
        let in_attribute = token.parent_ancestors().any(|node| ast::Attr::cast(node).is_some());
        is_name_token && !in_attribute
    };

    if right_token.as_ref().is_some_and(&accepts_token) {
        return Ok((position, None));
    }
    if left_token.as_ref().is_some_and(&accepts_token) {
        return Ok((position, None));
    }

    if right_token.is_none() && left_token.is_none() {
        return Ok((position, None));
    }

    let structures = analysis
        .file_structure(&FileStructureConfig { exclude_locals: true }, position.file_id)
        .map_err(cancelled_in("file structure"))?;
    let range = TextRange::empty(position.offset);
    let Some((structure, _container_name)) = enclosing_structure(&structures, range) else {
        return Ok((position, None));
    };

    let position =
        FilePosition { file_id: position.file_id, offset: structure.navigation_range.start() };
    let location = ctx.text_range_to_line_col(position.file_id, structure.navigation_range);
    Ok((
        position,
        Some(format!(
            "seed was inside non-symbol syntax; using enclosing item `{}` at {}:{}",
            structure.label, location.start_line, location.start_col,
        )),
    ))
}

fn reachable_directions(direction: ReachableDirection) -> &'static [ReachableDirection] {
    match direction {
        ReachableDirection::Incoming => &[ReachableDirection::Incoming],
        ReachableDirection::Outgoing => &[ReachableDirection::Outgoing],
        ReachableDirection::Both => &[ReachableDirection::Incoming, ReachableDirection::Outgoing],
    }
}

fn reachable_scope_includes_file(
    db: &RootDatabase,
    local_roots: &[SourceRootId],
    scope: ReachableScope,
    file_id: FileId,
) -> bool {
    match scope {
        ReachableScope::Workspace => {
            let source_root = db.file_source_root(file_id);
            local_roots.iter().any(|root| *root == source_root)
        }
        ReachableScope::WorkspaceAndDependencies => true,
    }
}

struct ReachableFilters<'a> {
    path: Option<PathFilter>,
    node_kinds: &'a [SerializableSymbolKind],
    dispatch: &'a [DispatchFilterText],
    reference_categories: &'a [ReferenceCategoryTag],
    usage_kinds: &'a [ReachableUsageKind],
}

impl ReachableFilters<'_> {
    fn matches_path(&self, ctx: &ConversionContext<'_>, file_ids: &[FileId]) -> bool {
        let Some(filter) = &self.path else {
            return true;
        };
        file_ids.iter().any(|file_id| {
            ctx.file_path(*file_id).is_some_and(|file_path| filter.matches(&file_path))
        })
    }

    fn matches_node_kind(&self, kind: Option<SerializableSymbolKind>) -> bool {
        self.node_kinds.is_empty()
            || kind.is_some_and(|kind| self.node_kinds.iter().any(|filter| *filter == kind))
    }

    fn matches_dispatch(&self, dispatch: Option<&str>) -> bool {
        self.dispatch.is_empty()
            || dispatch.is_some_and(|dispatch| {
                self.dispatch.iter().any(|filter| filter.as_str() == dispatch)
            })
    }

    fn matches_reference_categories(&self, categories: &[ReferenceCategoryTag]) -> bool {
        self.reference_categories.is_empty()
            || categories
                .iter()
                .any(|category| self.reference_categories.iter().any(|filter| filter == category))
    }

    fn matches_usage_kind(&self, usage_kind: Option<ReachableUsageKind>) -> bool {
        self.usage_kinds.is_empty()
            || usage_kind.is_some_and(|usage_kind| {
                self.usage_kinds.iter().any(|filter| *filter == usage_kind)
            })
    }
}

fn sorted_hierarchy_roots(
    analysis: &ide::Analysis,
    ctx: &ConversionContext<'_>,
    position: FilePosition,
    config: &CallHierarchyConfig<'_>,
) -> Result<Option<Vec<NavigationTarget>>, McpError> {
    let Some(hierarchy) =
        analysis.call_hierarchy(position, config).map_err(cancelled_in("call hierarchy"))?
    else {
        return Ok(None);
    };

    let mut roots = hierarchy.info;
    roots.sort_by(|a, b| {
        let a_path = ctx.file_path(a.file_id).unwrap_or_default();
        let b_path = ctx.file_path(b.file_id).unwrap_or_default();
        (a_path, a.focus_or_full_range().start(), a.name.as_str()).cmp(&(
            b_path,
            b.focus_or_full_range().start(),
            b.name.as_str(),
        ))
    });
    Ok(Some(roots))
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn reachable(
    snapshot: &Snapshot,
    request: &ReachableRequest,
) -> Result<ReachableResult, McpError> {
    let db = snapshot.raw_database();
    let analysis = snapshot.analysis();
    let ctx = ConversionContext::new(db);
    let position = resolve_reachable_seed(
        snapshot,
        &request.file,
        request.line,
        request.column,
        request.range,
    )?;
    let (position, seed_note) = symbol_seed_position(analysis, &ctx, position)?;
    let mut notes = Vec::new();
    if let Some(note) = seed_note {
        notes.push(note);
    }

    let direction = request.direction;
    let scope = request.scope;
    let edge_kinds = if request.edge_kinds.is_empty() {
        vec![ReachableEdgeKind::Call]
    } else {
        request.edge_kinds.clone()
    };
    let path_filter: Option<PathFilter> = match request.path.as_ref() {
        Some(pattern) => {
            Some(PathFilter::new(pattern.as_str()).map_err(|e| RequestError::InvalidPattern {
                pattern: pattern.as_str().to_owned(),
                message: e.to_string(),
            })?)
        }
        None => None,
    };
    let filters = ReachableFilters {
        path: path_filter,
        node_kinds: &request.node_kinds,
        dispatch: &request.dispatch,
        reference_categories: &request.reference_categories,
        usage_kinds: &request.usage_kinds,
    };
    let depth = request.depth.get();
    let config =
        CallHierarchyConfig { exclude_tests: false, ra_fixture: RaFixtureConfig::default() };
    let refs_config = FindAllRefsConfig {
        search_scope: None,
        ra_fixture: RaFixtureConfig::default(),
        exclude_imports: false,
        exclude_tests: false,
    };
    let implementation_config =
        ide::GotoImplementationConfig { filter_adjacent_derive_implementations: false };
    let sema = hir::Semantics::new(db);
    let mut structure_cache = FxHashMap::default();
    let local_root_ids = local_roots(db);

    let Some(roots) = sorted_hierarchy_roots(analysis, &ctx, position, &config)? else {
        if let Some(note) = diagnose_unresolved_position(analysis, position) {
            notes.push(note);
        }
        return Ok(ReachableResult {
            roots: Vec::new(),
            direction,
            scope,
            edge_kinds,
            depth,
            depths: Vec::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            note: (!notes.is_empty()).then(|| notes.join("; ")),
        });
    };

    let mut target_keys = FxHashSet::default();
    let target_requested = request.target.is_some();
    if let Some(target) = &request.target {
        let target_position = resolve_reachable_seed(
            snapshot,
            &target.file,
            target.line,
            target.column,
            target.range,
        )?;
        let (target_position, target_note) = symbol_seed_position(analysis, &ctx, target_position)?;
        if let Some(note) = target_note {
            notes.push(format!("target {note}"));
        }
        match sorted_hierarchy_roots(analysis, &ctx, target_position, &config)? {
            Some(target_roots) => {
                for target_root in &target_roots {
                    target_keys.insert(reachable_nav_key(target_root));
                }
            }
            None => {
                notes.push("target seed did not resolve to a reachable symbol".to_owned());
            }
        }
    }

    let mut builder = ReachabilityBuilder::new();
    let mut root_ids = Vec::new();
    for root_nav in &roots {
        let Some((root, inserted)) = builder.insert_nav(analysis, db, &ctx, root_nav, 0) else {
            continue;
        };
        if !root_ids.contains(&root) {
            root_ids.push(root);
        }
        if inserted {
            builder.frontier.push_back((root, 0));
        }
    }
    if root_ids.is_empty() {
        if let Some(note) = diagnose_unresolved_position(analysis, position) {
            notes.push(note);
        }
        return Ok(ReachableResult {
            roots: Vec::new(),
            direction,
            scope,
            edge_kinds,
            depth,
            depths: Vec::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            note: (!notes.is_empty()).then(|| notes.join("; ")),
        });
    }

    let mut found_target = None;
    for root_id in &root_ids {
        if target_keys.contains(&builder.node_keys[*root_id]) {
            found_target = Some(*root_id);
            break;
        }
    }

    while let Some((target_id, target_depth)) = builder.frontier.pop_front() {
        if target_depth >= depth {
            continue;
        }

        let position = builder.positions[target_id];
        let next_depth = target_depth + 1;
        for traversal_direction in reachable_directions(direction) {
            if edge_kinds.contains(&ReachableEdgeKind::Call) {
                let mut calls = match traversal_direction {
                    ReachableDirection::Incoming => analysis
                        .incoming_calls(&config, position)
                        .map_err(cancelled_in("incoming calls"))?
                        .unwrap_or_default(),
                    ReachableDirection::Outgoing => analysis
                        .outgoing_calls(&config, position)
                        .map_err(cancelled_in("outgoing calls"))?
                        .unwrap_or_default(),
                    ReachableDirection::Both => Vec::new(),
                };
                calls.sort_by(|a, b| {
                    let a_path = ctx.file_path(a.target.file_id).unwrap_or_default();
                    let b_path = ctx.file_path(b.target.file_id).unwrap_or_default();
                    (a_path, a.target.focus_or_full_range().start(), a.target.name.as_str()).cmp(&(
                        b_path,
                        b.target.focus_or_full_range().start(),
                        b.target.name.as_str(),
                    ))
                });

                for mut call in calls {
                    if !filters.matches_dispatch(Some("call_hierarchy")) {
                        continue;
                    }
                    if !filters
                        .matches_node_kind(call.target.kind.map(SerializableSymbolKind::from))
                    {
                        continue;
                    }
                    if !reachable_scope_includes_file(
                        db,
                        &local_root_ids,
                        scope,
                        call.target.file_id,
                    ) {
                        continue;
                    }

                    call.ranges.sort_by(|a, b| {
                        let a_path = ctx.file_path(a.file_id).unwrap_or_default();
                        let b_path = ctx.file_path(b.file_id).unwrap_or_default();
                        (a_path, a.range.start()).cmp(&(b_path, b.range.start()))
                    });
                    call.ranges.retain(|range| {
                        filters.matches_path(&ctx, &[call.target.file_id, range.file_id])
                    });
                    if call.ranges.is_empty() {
                        continue;
                    }

                    let Some((other_id, inserted)) =
                        builder.insert_nav(analysis, db, &ctx, &call.target, next_depth)
                    else {
                        continue;
                    };
                    if inserted && next_depth < depth {
                        builder.frontier.push_back((other_id, next_depth));
                    }

                    for FileRange { file_id, range } in call.ranges {
                        let (source_node, target_node) = match traversal_direction {
                            ReachableDirection::Incoming => (other_id, target_id),
                            ReachableDirection::Outgoing => (target_id, other_id),
                            ReachableDirection::Both => continue,
                        };

                        let position = FilePosition { file_id, offset: range.start() };
                        let edge = builder.push_edge(ReachableEdge {
                            depth: next_depth,
                            kind: ReachableEdgeKind::Call,
                            location: reachable_location(
                                analysis,
                                &ctx,
                                &mut structure_cache,
                                file_id,
                                range,
                            )?,
                            source_node,
                            target_node,
                            reference_categories: Vec::new(),
                            usage_kind: None,
                            generic_substitution: Vec::new(),
                            receiver_type: None,
                            dispatch: Some("call_hierarchy".to_owned()),
                            trait_context: None,
                            macro_expansion: macro_expansion_name(analysis, db, position),
                            monikers: moniker_strings(analysis, position),
                        });
                        builder.record_predecessor(other_id, target_id, edge);
                        if found_target.is_none()
                            && target_keys.contains(&builder.node_keys[other_id])
                        {
                            found_target = Some(other_id);
                        }
                    }
                }
            }

            if edge_kinds.contains(&ReachableEdgeKind::Implementation) {
                match traversal_direction {
                    ReachableDirection::Incoming => {
                        let mut implementations = analysis
                            .goto_implementation(&implementation_config, position)
                            .map_err(cancelled_in("goto implementation"))?
                            .map(|implementations| implementations.info)
                            .unwrap_or_default();
                        implementations.sort_by(|a, b| {
                            let a_path = ctx.file_path(a.file_id).unwrap_or_default();
                            let b_path = ctx.file_path(b.file_id).unwrap_or_default();
                            (a_path, a.focus_or_full_range().start(), a.name.as_str()).cmp(&(
                                b_path,
                                b.focus_or_full_range().start(),
                                b.name.as_str(),
                            ))
                        });

                        for implementation in implementations {
                            if !filters.matches_dispatch(Some("goto_implementation")) {
                                continue;
                            }
                            if !filters.matches_node_kind(
                                implementation.kind.map(SerializableSymbolKind::from),
                            ) {
                                continue;
                            }
                            if !filters.matches_path(&ctx, &[implementation.file_id]) {
                                continue;
                            }
                            if !reachable_scope_includes_file(
                                db,
                                &local_root_ids,
                                scope,
                                implementation.file_id,
                            ) {
                                continue;
                            }
                            let Some((source_node, inserted)) =
                                builder.insert_nav(analysis, db, &ctx, &implementation, next_depth)
                            else {
                                continue;
                            };
                            if inserted && next_depth < depth {
                                builder.frontier.push_back((source_node, next_depth));
                            }

                            let range = implementation.focus_or_full_range();
                            let position = FilePosition {
                                file_id: implementation.file_id,
                                offset: range.start(),
                            };
                            let edge = builder.push_edge(ReachableEdge {
                                depth: next_depth,
                                kind: ReachableEdgeKind::Implementation,
                                location: reachable_location(
                                    analysis,
                                    &ctx,
                                    &mut structure_cache,
                                    implementation.file_id,
                                    range,
                                )?,
                                source_node,
                                target_node: target_id,
                                reference_categories: Vec::new(),
                                usage_kind: None,
                                generic_substitution: Vec::new(),
                                receiver_type: None,
                                dispatch: Some("goto_implementation".to_owned()),
                                trait_context: builder.nodes[target_id].trait_context.clone(),
                                macro_expansion: macro_expansion_name(analysis, db, position),
                                monikers: moniker_strings(analysis, position),
                            });
                            builder.record_predecessor(source_node, target_id, edge);
                            if found_target.is_none()
                                && target_keys.contains(&builder.node_keys[source_node])
                            {
                                found_target = Some(source_node);
                            }
                        }
                    }
                    ReachableDirection::Outgoing | ReachableDirection::Both => (),
                }
            }

            if edge_kinds.contains(&ReachableEdgeKind::Usage) {
                match traversal_direction {
                    ReachableDirection::Incoming => {
                        let Some(refs) = analysis
                            .find_all_refs(position, &refs_config)
                            .map_err(cancelled_in("find usages"))?
                        else {
                            continue;
                        };
                        let mut usages = Vec::new();
                        for refs in refs {
                            for (file_id, ranges) in refs.references {
                                for (range, category) in ranges {
                                    usages.push((file_id, range, category));
                                }
                            }
                        }
                        usages.sort_by(|(a_file, a_range, _), (b_file, b_range, _)| {
                            let a_path = ctx.file_path(*a_file).unwrap_or_default();
                            let b_path = ctx.file_path(*b_file).unwrap_or_default();
                            (a_path, a_range.start()).cmp(&(b_path, b_range.start()))
                        });

                        for (file_id, range, category) in usages {
                            if !reachable_scope_includes_file(db, &local_root_ids, scope, file_id) {
                                continue;
                            }
                            if !filters.matches_path(&ctx, &[file_id]) {
                                continue;
                            }
                            let reference_categories = reference_category_tags(category);
                            if !filters.matches_reference_categories(&reference_categories) {
                                continue;
                            }

                            let metadata = hir::attach_db(db, || {
                                usage_metadata_at_reference(db, &sema, file_id, range, category)
                            });
                            let (
                                usage_kind,
                                generic_substitution,
                                receiver_type,
                                dispatch,
                                trait_context,
                            ) = match metadata {
                                Some(metadata) => (
                                    Some(metadata.usage_kind),
                                    metadata.generic_substitution,
                                    metadata.receiver_type,
                                    metadata.dispatch,
                                    metadata.trait_context,
                                ),
                                None => (None, Vec::new(), None, None, None),
                            };
                            if !filters.matches_usage_kind(usage_kind) {
                                continue;
                            }
                            if !filters.matches_dispatch(dispatch.as_deref()) {
                                continue;
                            }
                            let Some((source_node, inserted)) = insert_usage_source_node(
                                analysis,
                                db,
                                &ctx,
                                &mut builder,
                                &mut structure_cache,
                                file_id,
                                range,
                                next_depth,
                                &filters,
                            )?
                            else {
                                continue;
                            };
                            if inserted && next_depth < depth {
                                builder.frontier.push_back((source_node, next_depth));
                            }
                            let position = FilePosition { file_id, offset: range.start() };
                            let edge = builder.push_edge(ReachableEdge {
                                depth: next_depth,
                                kind: ReachableEdgeKind::Usage,
                                location: reachable_location(
                                    analysis,
                                    &ctx,
                                    &mut structure_cache,
                                    file_id,
                                    range,
                                )?,
                                source_node,
                                target_node: target_id,
                                reference_categories,
                                usage_kind,
                                generic_substitution,
                                receiver_type,
                                dispatch,
                                trait_context,
                                macro_expansion: macro_expansion_name(analysis, db, position),
                                monikers: moniker_strings(analysis, position),
                            });
                            builder.record_predecessor(source_node, target_id, edge);
                            if found_target.is_none()
                                && target_keys.contains(&builder.node_keys[source_node])
                            {
                                found_target = Some(source_node);
                            }
                        }
                    }
                    ReachableDirection::Outgoing => {
                        let scan_range = builder.scan_ranges[target_id];
                        let mut usages = hir::attach_db(db, || {
                            let source_file = sema.parse_guess_edition(scan_range.file_id);
                            let mut usages = Vec::new();
                            for node in source_file.syntax().descendants() {
                                let Some(name_ref) = ast::NameRef::cast(node) else {
                                    continue;
                                };
                                let range = name_ref.syntax().text_range();
                                if !scan_range.range.contains_range(range) {
                                    continue;
                                }
                                let Some(class) = NameRefClass::classify(&sema, &name_ref) else {
                                    continue;
                                };
                                let metadata = usage_metadata_from_name_ref_class(
                                    db,
                                    &sema,
                                    &name_ref,
                                    class,
                                    ide::ReferenceCategory::empty(),
                                );
                                let Some(nav) = metadata
                                    .definition
                                    .try_to_nav(&sema)
                                    .map(|nav| nav.call_site())
                                else {
                                    continue;
                                };
                                usages.push((range, nav, metadata));
                            }
                            usages
                        });
                        usages.sort_by(|(a_range, a_nav, _), (b_range, b_nav, _)| {
                            let a_path = ctx.file_path(a_nav.file_id).unwrap_or_default();
                            let b_path = ctx.file_path(b_nav.file_id).unwrap_or_default();
                            (a_path, a_range.start(), a_nav.name.as_str()).cmp(&(
                                b_path,
                                b_range.start(),
                                b_nav.name.as_str(),
                            ))
                        });

                        for (range, nav, metadata) in usages {
                            if !reachable_scope_includes_file(
                                db,
                                &local_root_ids,
                                scope,
                                nav.file_id,
                            ) {
                                continue;
                            }
                            if !filters.matches_path(&ctx, &[scan_range.file_id, nav.file_id]) {
                                continue;
                            }
                            if !filters
                                .matches_node_kind(nav.kind.map(SerializableSymbolKind::from))
                            {
                                continue;
                            }
                            if !filters.matches_reference_categories(&[]) {
                                continue;
                            }
                            if !filters.matches_usage_kind(Some(metadata.usage_kind)) {
                                continue;
                            }
                            if !filters.matches_dispatch(metadata.dispatch.as_deref()) {
                                continue;
                            }
                            let Some((target_node, inserted)) =
                                builder.insert_nav(analysis, db, &ctx, &nav, next_depth)
                            else {
                                continue;
                            };
                            if inserted && next_depth < depth {
                                builder.frontier.push_back((target_node, next_depth));
                            }

                            let position =
                                FilePosition { file_id: scan_range.file_id, offset: range.start() };
                            let edge = builder.push_edge(ReachableEdge {
                                depth: next_depth,
                                kind: ReachableEdgeKind::Usage,
                                location: reachable_location(
                                    analysis,
                                    &ctx,
                                    &mut structure_cache,
                                    scan_range.file_id,
                                    range,
                                )?,
                                source_node: target_id,
                                target_node,
                                reference_categories: Vec::new(),
                                usage_kind: Some(metadata.usage_kind),
                                generic_substitution: metadata.generic_substitution,
                                receiver_type: metadata.receiver_type,
                                dispatch: metadata.dispatch,
                                trait_context: metadata.trait_context,
                                macro_expansion: macro_expansion_name(analysis, db, position),
                                monikers: moniker_strings(analysis, position),
                            });
                            builder.record_predecessor(target_node, target_id, edge);
                            if found_target.is_none()
                                && target_keys.contains(&builder.node_keys[target_node])
                            {
                                found_target = Some(target_node);
                            }
                        }
                    }
                    ReachableDirection::Both => (),
                }
            }
        }
    }

    if target_requested && found_target.is_none() {
        notes.push(format!("target was not reachable within depth {depth}"));
    }

    let (roots, depths, nodes, edges) = match (target_requested, found_target) {
        (true, Some(target_id)) => builder.into_path(&root_ids, target_id),
        (true, None) | (false, Some(_)) | (false, None) => {
            (root_ids, builder.depths, builder.nodes, builder.edges)
        }
    };

    Ok(ReachableResult {
        roots,
        direction,
        scope,
        edge_kinds,
        depth,
        depths,
        nodes,
        edges,
        note: (!notes.is_empty()).then(|| notes.join("; ")),
    })
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn symbol_search(
    snapshot: &Snapshot,
    params: &SymbolSearchParams,
) -> Result<(Vec<SerializableSymbol>, bool), McpError> {
    let SymbolSearchParams {
        ref query,
        mode,
        kind_filter,
        scope,
        case_sensitivity,
        limit,
        offset,
        ref path,
        exclude_imports,
        assoc_mode,
    } = *params;

    let path_filter: Option<PathFilter> = match path.as_ref() {
        Some(pattern) => {
            Some(PathFilter::new(pattern.as_str()).map_err(|e| RequestError::InvalidPattern {
                pattern: pattern.as_str().to_owned(),
                message: e.to_string(),
            })?)
        }
        None => None,
    };

    let db = snapshot.raw_database();
    let analysis = snapshot.analysis();

    let mut q = Query::new(query.as_str().to_owned());
    match mode {
        SearchMode::Fuzzy => {
            q.fuzzy();
        }
        SearchMode::Exact => {
            q.exact();
        }
        SearchMode::Prefix => {
            q.prefix();
        }
    }
    match kind_filter {
        SymbolKindFilter::AllKinds => {}
        SymbolKindFilter::TypesOnly => {
            q.only_types();
        }
    }
    match case_sensitivity {
        CaseSensitivity::Insensitive => {}
        CaseSensitivity::Sensitive => {
            q.case_sensitive();
        }
    }
    if exclude_imports {
        q.exclude_imports();
    }
    match assoc_mode {
        AssocMode::Include => {}
        AssocMode::Exclude => {
            q.assoc_search_mode(hir::import_map::AssocSearchMode::Exclude);
        }
        AssocMode::AssocItemsOnly => {
            q.assoc_search_mode(hir::import_map::AssocSearchMode::AssocItemsOnly);
        }
    }
    q.include_hidden();
    q.strict_only_types();
    let limit = match usize::try_from(limit.get()) {
        Ok(limit) => limit,
        Err(_) => usize::MAX,
    };
    let wanted = offset.as_usize().saturating_add(limit).saturating_add(1);
    let search_limit: usize =
        if path_filter.is_some() { usize::MAX } else { wanted.saturating_mul(2).max(200) };

    let mut targets =
        analysis.symbol_search(q.clone(), search_limit).map_err(cancelled_in("symbol search"))?;
    match scope {
        SearchScope::WorkspaceOnly => {}
        SearchScope::IncludeLibs => {
            q.libs();
            targets.extend(
                analysis.symbol_search(q, search_limit).map_err(cancelled_in("symbol search"))?,
            );
        }
    }
    let ctx = ConversionContext::new(db);
    let local_root_ids = match scope {
        SearchScope::WorkspaceOnly => Some(local_roots(db)),
        SearchScope::IncludeLibs => None,
    };

    let mut symbols = Vec::new();
    let mut seen = FxHashSet::default();
    for nav in targets {
        if symbols.len() >= wanted {
            break;
        }
        if !seen.insert((nav.file_id, nav.full_range)) {
            continue;
        }

        if let Some(local_root_ids) = local_root_ids.as_ref() {
            let source_root = db.file_source_root(nav.file_id);
            if !local_root_ids.iter().any(|root| *root == source_root) {
                continue;
            }
        }

        let Some(file_path) = ctx.file_path(nav.file_id) else {
            continue;
        };

        match path_filter.as_ref() {
            Some(filter) if !filter.matches(&file_path) => continue,
            Some(_) | None => (),
        }

        let range = ctx.text_range_to_line_col(nav.file_id, nav.full_range);
        let focus_range = nav.focus_range.map(|r| ctx.text_range_to_line_col(nav.file_id, r));

        symbols.push(SerializableSymbol {
            name: nav.name.as_str().to_owned(),
            kind: nav.kind.map(SerializableSymbolKind::from),
            file_path,
            range,
            focus_range,
            container_name: nav.container_name.map(|s| s.as_str().to_owned()),
            description: nav.description,
        });
    }

    let more = symbols.len() > offset.as_usize().saturating_add(limit);
    let page: Vec<_> = symbols.into_iter().skip(offset.as_usize()).take(limit).collect();
    Ok((page, more))
}

fn parse_file_root_uri(uri: &str) -> Result<PathBuf, McpError> {
    let url = Url::parse(uri).map_err(|e| RequestError::InvalidRequest {
        message: format!("invalid root URI '{uri}': {e}"),
    })?;
    if url.scheme() != "file" {
        return Err(RequestError::InvalidRequest {
            message: format!("unsupported root URI scheme '{}' (expected file://)", url.scheme()),
        }
        .into());
    }
    url.to_file_path().map_err(|_| {
        McpError::from(RequestError::InvalidRequest {
            message: format!("root URI is not a local file path: {uri}"),
        })
    })
}

pub struct RaMcpServer {
    analysis: Analysis,
    tool_router: ToolRouter<Self>,
}

#[derive(Clone)]
struct RequestWorkspace {
    workspace: Workspace,
    root: PathBuf,
}

tokio::task_local! {
    static REQUEST_WORKSPACE: RequestWorkspace;
}

fn current_workspace() -> Result<RequestWorkspace, McpError> {
    REQUEST_WORKSPACE.try_with(Clone::clone).map_err(|_| RequestError::NotInitialized.into())
}

impl Clone for RaMcpServer {
    fn clone(&self) -> Self {
        Self { analysis: self.analysis.clone(), tool_router: self.tool_router.clone() }
    }
}

#[tool_router]
impl RaMcpServer {
    pub fn new() -> Self {
        Self { analysis: Analysis::new(), tool_router: Self::tool_router() }
    }

    async fn workspace_for_request(
        &self,
        context: &RequestContext<rmcp::RoleServer>,
    ) -> Result<RequestWorkspace, McpError> {
        let roots_result =
            context.peer.list_roots().await.map_err(|err| RequestError::InvalidRequest {
                message: format!("failed to request client roots: {err}"),
            })?;
        let mut paths = Vec::new();
        for root in &roots_result.roots {
            if let Ok(path) = parse_file_root_uri(&root.uri) {
                paths.push(path);
            }
        }
        if paths.is_empty() {
            return Err(RequestError::InvalidRequest {
                message: "client did not provide any usable file:// roots".into(),
            }
            .into());
        }
        let analysis = self.analysis.clone();
        let first_path = paths[0].clone();
        let all_paths = paths;
        let workspace = tokio::task::spawn_blocking(move || analysis.workspace(all_paths))
            .await
            .map_err(|e| EnvironmentError::TaskFailed { message: e.to_string() })??;
        Ok(RequestWorkspace { workspace, root: first_path })
    }

    #[tool(
        description = "ALWAYS use this for structural code search. NEVER approximate code structure with bash grep/rg regexes: \
                       instead of `rg 'unwrap()'` to find unwrap calls, call search with pattern '$x.unwrap()'. \
                       Patterns match the syntax tree with semantic path resolution, so formatting, line breaks, and import aliases \
                       never cause misses, and matches carry context (enclosing function, impl, test/unsafe/async) that grep cannot produce. \
                       Narrow with the inTest/inFunction/usageKind/filePattern filters — semantic criteria grep cannot express — \
                       and page with 'offset'. Pass countBy to get grouped counts instead of match lines (like grep -c), \
                       which sizes up a broad pattern before paging through it. \
                       The optional 'files' parameter restricts results to those files and sets the path-resolution context. \
                       Results are one line per match (workspace-relative path, 1-based line:col), e.g. \
                       'src/batch.rs:142:9: items.unwrap() [let_binding, in process_batch]'; \
                       a trailing '[K more; …]' line gives the next offset when truncated."
    )]
    async fn search(
        &self,
        Parameters(request): Parameters<SearchRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let SearchRequest {
            pattern,
            files,
            file_pattern,
            in_test,
            in_unsafe,
            in_async,
            in_function,
            in_impl,
            usage_kind,
            offset,
            limit,
            count_by,
        } = request;
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let matches = workspace
            .run_snapshot(move |snapshot: &Snapshot| search_matches(snapshot, &pattern, &files))
            .await?;

        let path_filter = match file_pattern.as_ref() {
            Some(pattern) => Some(PathFilter::new(pattern.as_str()).map_err(|e| {
                RequestError::InvalidPattern {
                    pattern: pattern.as_str().to_owned(),
                    message: e.to_string(),
                }
            })?),
            None => None,
        };
        let criteria = FilterCriteria {
            path_filter,
            in_test,
            in_unsafe,
            in_async,
            in_function,
            in_impl,
            usage_kind,
        };
        let matches = filter_matches(&matches, &criteria);
        let total_matches = matches.len();

        if let Some(group_by) = count_by {
            let groups = match group_by {
                GroupByField::File => count_matches_by(&matches, group_key_file),
                GroupByField::UsageKind => count_matches_by(&matches, group_key_usage_kind),
                GroupByField::Function => count_matches_by(&matches, group_key_function),
                GroupByField::Impl => count_matches_by(&matches, group_key_impl),
            };
            let result = SearchOutput::Counts { total_matches, group_by, groups };
            let text = result.render(Some(root.as_path()));
            return to_tool_result(&result, text);
        }

        let page: Vec<_> =
            matches.into_iter().skip(offset.as_usize()).take(limit.as_usize()).collect();
        let result = SearchOutput::Matches { total_matches, offset: offset.get(), matches: page };
        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "Structural search-and-replace. ALWAYS use this for mechanical rewrites across call sites. \
                       NEVER use sed/awk or a chain of manual edits: instead of a sed substitution over `old_call(...)`, \
                       use rule 'old_call($a) ==>> new_call($a)'. Edits are computed on the syntax tree, so nesting, \
                       comments, and formatting that break regexes are handled correctly. \
                       Previews by default, one line per proposed edit, e.g. 'src/batch.rs:142:9: => renamed(42)'; \
                       rerun with apply=true to write ('applied N edits in M files' plus up to 10 edit lines). \
                       The optional 'files' parameter restricts the rewrite to those files and sets the path-resolution context."
    )]
    async fn ssr(
        &self,
        Parameters(request): Parameters<SsrRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let SsrRequest { rule, files, apply } = request;
        let RequestWorkspace { workspace, root } = current_workspace()?;

        if !apply {
            let (_matches, edits) = workspace
                .run_snapshot(move |snapshot: &Snapshot| ssr_preview_inner(snapshot, &rule, &files))
                .await?;
            let result = SsrOutput::Preview { edits };
            let text = result.render(Some(root.as_path()));
            return to_tool_result(&result, text);
        }

        let ComputedEdits { serialized, file_edits } = workspace
            .run_snapshot(move |snapshot: &Snapshot| ssr_compute_edits(snapshot, &rule, &files))
            .await?;

        let EditCounts { files_changed, edits_applied } =
            apply_text_edits_to_disk(&workspace, &file_edits)?;
        let result = SsrOutput::Applied { files_changed, edits_applied, edits: serialized };
        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "Add a placeholder argument at every call site of a function — instead of grepping for calls \
                       and editing each one by hand. Call sites are found semantically, including method calls, UFCS calls, and \
                       calls through re-exports that textual search misses. \
                       Previews by default, one line per proposed edit; rerun with apply=true to write \
                       ('applied N edits in M files' plus up to 10 edit lines)."
    )]
    async fn add_argument(
        &self,
        Parameters(request): Parameters<AddArgumentRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let AddArgumentRequest { file, line, column, argument_index, placeholder, apply } = request;
        let RequestWorkspace { workspace, root } = current_workspace()?;

        if !apply {
            let (edits, skipped) = workspace
                .run_snapshot(move |snapshot: &Snapshot| {
                    let ide::AddArgumentResult { source_change, skipped } =
                        add_argument_source_change(
                            snapshot,
                            &file,
                            line,
                            column,
                            argument_index,
                            &placeholder,
                        )?;
                    let db = snapshot.raw_database();
                    let ctx = ConversionContext::new(db);
                    let edits = source_change_to_serialized_edits(&ctx, &source_change);
                    Ok::<_, McpError>((edits, skipped))
                })
                .await?;

            let total_edits = edits.len();
            let result = AddArgumentOutput::Preview { total_edits, skipped, edits };
            let text = result.render(Some(root.as_path()));
            return to_tool_result(&result, text);
        }

        let (ComputedEdits { serialized, file_edits }, skipped) = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let ide::AddArgumentResult { source_change, skipped } = add_argument_source_change(
                    snapshot,
                    &file,
                    line,
                    column,
                    argument_index,
                    &placeholder,
                )?;
                let db = snapshot.raw_database();
                let ctx = ConversionContext::new(db);
                let computed = source_change_to_computed_edits(&ctx, &source_change);
                Ok::<_, McpError>((computed, skipped))
            })
            .await?;

        let EditCounts { files_changed, edits_applied } =
            apply_text_edits_to_disk(&workspace, &file_edits)?;
        let result =
            AddArgumentOutput::Applied { files_changed, edits_applied, skipped, edits: serialized };
        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "ALWAYS use this to traverse the Rust symbol graph. NEVER compose caller/callee or usage traversals by hand with grep/rg: \
                       call reachable at a symbol definition/use with file plus either line/column or range from another semantic tool. \
                       Choose direction=incoming for dependents/callers, outgoing for dependencies/callees, or both for a local neighborhood; \
                       set edgeKinds to call, usage, and/or implementation, scope to workspace or workspace_and_dependencies, and depth for transitive reachability. \
                       Use implementation edges from trait and trait-method declarations to reach impl blocks and implementing method bodies; use usage edges for references. \
                       Use path, nodeKinds, dispatch, referenceCategories, and usageKinds to filter high-fanout graphs before reading files. \
                       Pass target to return a path to another seed when one is reachable. \
                       Results are grep-shaped transcript lines, while structuredContent carries nodes, edges, per-depth counts, and edge location context."
    )]
    async fn reachable(
        &self,
        Parameters(request): Parameters<ReachableRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let result = workspace
            .run_snapshot(move |snapshot: &Snapshot| reachable(snapshot, &request))
            .await?;
        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "ALWAYS use this to locate a definition by name. NEVER use bash grep/rg for definition lookups: \
                       instead of `rg 'fn process_batch'` or `rg 'struct Config'`, call find_symbol with query 'process_batch' or 'Config'. \
                       Matches declarations only — no hits from call sites, comments, or strings — and fuzzy matching (the default) \
                       tolerates imprecise or partially remembered names. Supports exact and prefix modes, type-only filtering, \
                       and path globs. Results are grep-shaped lines (workspace-relative path, 1-based line:col), e.g. \
                       'src/batch.rs:142:8: fn process_batch: pub fn process_batch(items: &[Item]) -> Result<()>'; \
                       a trailing '[more; …]' line gives the next offset when truncated."
    )]
    async fn find_symbol(
        &self,
        Parameters(request): Parameters<FindSymbolRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let kind_filter = if request.only_types {
            SymbolKindFilter::TypesOnly
        } else {
            SymbolKindFilter::AllKinds
        };
        let scope = if request.include_libs {
            SearchScope::IncludeLibs
        } else {
            SearchScope::WorkspaceOnly
        };
        let case_sensitivity = if request.case_sensitive {
            CaseSensitivity::Sensitive
        } else {
            CaseSensitivity::Insensitive
        };
        let offset = request.offset;
        let params = SymbolSearchParams {
            query: request.query,
            mode: request.mode,
            kind_filter,
            scope,
            case_sensitivity,
            limit: request.limit,
            offset,
            path: request.path,
            exclude_imports: request.exclude_imports,
            assoc_mode: request.assoc_mode,
        };
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let (symbols, more) = workspace
            .run_snapshot(move |snapshot: &Snapshot| symbol_search(snapshot, &params))
            .await?;

        let result = SymbolsResults { offset: offset.get(), more, symbols };
        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "ALWAYS use this to identify the symbol at a position: where it is defined, its type and signature, and \
                       its documentation, in one call. NEVER grep for a declaration: instead of `rg 'fn helper'` after seeing a \
                       call to helper(), call inspect at the call site (file, line, column). Resolves method calls through \
                       auto-deref and trait dispatch, macro-generated items, and re-exports, and shows inferred types that \
                       appear nowhere in the source — textual search can do neither. Returns grep-shaped definition lines \
                       (workspace-relative path, 1-based line:col), followed by labeled Markdown sections such as \
                       qualified_path:, signature:, declaration:, type:, code:, and docs:. Rust metadata is fenced as rust \
                       code; function, method, const, and static declarations include the source body when available; \
                       field and variant declarations include the containing item and mark the inspected member with \
                       /* focus:start */ and /* focus:end */ comments."
    )]
    async fn inspect(
        &self,
        Parameters(request): Parameters<InspectRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let result = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let position =
                    snapshot.resolve_position(&request.file, request.line, request.column)?;
                let db = snapshot.raw_database();
                let analysis = snapshot.analysis();
                let ctx = ConversionContext::new(db);
                let (position, _) = symbol_seed_position(analysis, &ctx, position)?;

                let config = GotoDefinitionConfig { ra_fixture: RaFixtureConfig::default() };

                let definitions = analysis
                    .goto_definition(position, &config)
                    .map_err(cancelled_in("goto definition"))?;

                let mut locations = Vec::new();
                let mut declaration_source = None;
                if let Some(range_info) = definitions {
                    for nav in &range_info.info {
                        let focus_range = nav.focus_range.unwrap_or(nav.full_range);
                        let range = ctx.text_range_to_line_col(nav.file_id, focus_range);
                        if declaration_source.is_none() {
                            declaration_source = source_declaration(
                                db,
                                nav.file_id,
                                nav.full_range,
                                focus_range,
                                nav.kind,
                            );
                        }
                        locations.push(DefinitionLocation {
                            file_path: ctx.file_path(nav.file_id).unwrap_or_default(),
                            range,
                            name: nav.name.as_str().to_owned(),
                            kind: nav.kind.map(SerializableSymbolKind::from),
                            container_name: nav
                                .container_name
                                .as_ref()
                                .map(|s| s.as_str().to_owned()),
                            description: nav.description.clone(),
                        });
                    }
                }
                if locations.is_empty() {
                    let structures = analysis
                        .file_structure(
                            &FileStructureConfig { exclude_locals: true },
                            position.file_id,
                        )
                        .map_err(cancelled_in("file structure"))?;
                    if let Some(structure) = structures
                        .iter()
                        .find(|structure| structure.navigation_range.contains(position.offset))
                    {
                        let kind = match structure.kind {
                            StructureNodeKind::SymbolKind(kind) => Some(kind),
                            StructureNodeKind::ExternBlock | StructureNodeKind::Region => None,
                        };
                        if let Some(kind) = kind {
                            declaration_source = source_declaration(
                                db,
                                position.file_id,
                                structure.node_range,
                                structure.navigation_range,
                                Some(kind),
                            );
                        }
                        let container_name = structure
                            .parent
                            .and_then(|idx| structures.get(idx))
                            .map(|parent| parent.label.clone());
                        locations.push(DefinitionLocation {
                            file_path: ctx.file_path(position.file_id).unwrap_or_default(),
                            range: ctx.text_range_to_line_col(
                                position.file_id,
                                structure.navigation_range,
                            ),
                            name: structure.label.clone(),
                            kind: kind.map(SerializableSymbolKind::from),
                            container_name,
                            description: structure.detail.clone(),
                        });
                    }
                }

                let file_range = ide_db::FileRange {
                    file_id: position.file_id,
                    range: TextRange::empty(position.offset),
                };
                let metadata = analysis
                    .hover(&hover_config(), file_range)
                    .map_err(cancelled_in("hover"))?
                    .map(|hover| PositionMetadata::from_markup(hover.info.markup.as_str()))
                    .unwrap_or_default();
                let mut metadata = metadata;
                if let Some(declaration) = declaration_source {
                    metadata.replace_declaration(declaration);
                }

                Ok::<_, McpError>(InspectResult { definitions: locations, metadata })
            })
            .await?;

        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "Read raw source text from a workspace file or exact 1-based half-open range. \
                       Use this after a semantic tool returns a file/range and you need surrounding syntax or a full body; \
                       prefer inspect, reachable, find_symbol, and search for semantic lookup. \
                       Returns 'path:line:col: read' followed by the raw source text."
    )]
    async fn read(
        &self,
        Parameters(request): Parameters<ReadRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let ReadRequest { file, range } = request;
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let result = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let (file_id, range) = match range {
                    Some(range) => snapshot.resolve_range(
                        &file,
                        range.start_line,
                        range.start_col,
                        range.end_line,
                        range.end_col,
                    )?,
                    None => {
                        let file_id = snapshot.resolve_file_id(&file)?;
                        let db = snapshot.raw_database();
                        let text = db.file_text(file_id).text(db);
                        (file_id, TextRange::up_to(TextSize::of(&**text)))
                    }
                };

                let db = snapshot.raw_database();
                let ctx = ConversionContext::new(db);
                let text = db.file_text(file_id).text(db);
                let start = usize::from(range.start());
                let end = usize::from(range.end());

                Ok::<_, McpError>(ReadResult {
                    file_path: ctx.file_path(file_id).unwrap_or_default(),
                    range: ctx.text_range_to_line_col(file_id, range),
                    text: text[start..end].to_owned(),
                })
            })
            .await?;

        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "ALWAYS use this to rename a symbol. NEVER rename with grep-and-edit or sed: textual renames hit comments, \
                       strings, and unrelated same-named symbols, and miss uses through re-exports, macros, and trait impls. \
                       This renames exactly the semantic symbol across the entire workspace and writes the edits to disk. \
                       File system edits (file/dir renames) are reported but not yet applied. \
                       Returns 'applied N edits in M files' plus up to 10 edit lines, or 'rename rejected: <reason>'."
    )]
    async fn rename(
        &self,
        Parameters(request): Parameters<RenameRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let snapshot_data = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let source_change = rename_source_change(
                    snapshot,
                    &request.file,
                    request.line,
                    request.column,
                    &request.new_name,
                )?;

                let db = snapshot.raw_database();
                let ctx = ConversionContext::new(db);

                let edits = source_change_to_serialized_edits(&ctx, &source_change);
                let file_edits = source_change_to_raw_edits(&ctx, &source_change);

                let mut file_system_edits = Vec::new();
                for fs_edit in &source_change.file_system_edits {
                    file_system_edits.push(match fs_edit {
                        ide_db::source_change::FileSystemEdit::CreateFile {
                            dst,
                            initial_contents,
                        } => SerializableFileSystemEdit::CreateFile {
                            dst: ctx.resolve_anchored_path(dst).display().to_string(),
                            initial_contents: initial_contents.clone(),
                        },
                        ide_db::source_change::FileSystemEdit::MoveFile { src, dst } => {
                            SerializableFileSystemEdit::MoveFile {
                                src: ctx.file_path(*src).unwrap_or_default().display().to_string(),
                                dst: ctx.resolve_anchored_path(dst).display().to_string(),
                            }
                        }
                        ide_db::source_change::FileSystemEdit::MoveDir { src, src_id: _, dst } => {
                            SerializableFileSystemEdit::MoveDir {
                                src: ctx.resolve_anchored_path(src).display().to_string(),
                                dst: ctx.resolve_anchored_path(dst).display().to_string(),
                            }
                        }
                    });
                }

                Ok::<_, McpError>((edits, file_edits, file_system_edits))
            })
            .await;

        let result = match snapshot_data {
            Err(McpError::Request(RequestError::RenameRejected { reason })) => {
                RenameResult::Rejected { reason }
            }
            Err(e) => return Err(e.into()),
            Ok((edits, file_edits, file_system_edits)) => {
                let EditCounts { files_changed, edits_applied } =
                    apply_text_edits_to_disk(&workspace, &file_edits)?;
                RenameResult::Ok { files_changed, edits_applied, edits, file_system_edits }
            }
        };

        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }

    #[tool(
        description = "Expand the macro invocation at a position and show the generated code. This is the only way to see \
                       what a macro actually produces — the expansion exists nowhere on disk, so no bash tool can show it. \
                       Text content returns '<name> expands to:' followed by the generated code in a fenced rust block; \
                       structuredContent is the authoritative result."
    )]
    async fn expand_macro(
        &self,
        Parameters(request): Parameters<ExpandMacroRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let workspace = current_workspace()?.workspace;

        let result = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let position =
                    snapshot.resolve_position(&request.file, request.line, request.column)?;
                let analysis = snapshot.analysis();

                let expanded =
                    analysis.expand_macro(position).map_err(cancelled_in("expand macro"))?;

                let result = match expanded {
                    Some(exp) => {
                        ExpandMacroResult { name: Some(exp.name), expansion: Some(exp.expansion) }
                    }
                    None => ExpandMacroResult { name: None, expansion: None },
                };

                Ok::<_, McpError>(result)
            })
            .await?;

        let text = result.render();
        to_tool_result(&result, text)
    }

    #[tool(
        description = "Get inlay hints (inferred types, parameter names, enum discriminants) for a file or range. \
                       This information exists nowhere in the source text, so no textual search can produce it. \
                       Returns one 'line:col: kind label' line per hint (1-based)."
    )]
    async fn inlay_hints(
        &self,
        Parameters(request): Parameters<InlayHintsRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let workspace = current_workspace()?.workspace;

        let result = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let (file_id, range) = match request.range.as_ref() {
                    Some(r) => {
                        let (file_id, range) = snapshot.resolve_range(
                            &request.file,
                            r.start_line,
                            r.start_col,
                            r.end_line,
                            r.end_col,
                        )?;
                        (file_id, Some(range))
                    }
                    None => (snapshot.resolve_file_id(&request.file)?, None),
                };

                let db = snapshot.raw_database();
                let analysis = snapshot.analysis();

                let config = inlay_hints_config();

                let hints = analysis
                    .inlay_hints(&config, file_id, range)
                    .map_err(cancelled_in("inlay hints"))?;
                let ctx = ConversionContext::new(db);

                let mut serialized = Vec::new();
                for hint in hints {
                    serialized.push(SerializableInlayHint {
                        range: ctx.text_range_to_line_col(file_id, hint.range),
                        kind: SerializableInlayKind::from(hint.kind),
                        label: hint.label.to_string(),
                        position: SerializableInlayHintPosition::from(hint.position),
                    });
                }

                Ok::<_, McpError>(InlayHintsResult { hints: serialized })
            })
            .await?;

        let text = result.render();
        to_tool_result(&result, text)
    }

    #[tool(
        description = "List the workspace's member crates (not dependencies) with name, version, and root file — instead of \
                       parsing Cargo.toml files or `cargo metadata` output. Returns one 'name version root-file' line per crate."
    )]
    async fn workspace_crates(&self) -> Result<CallToolResult, ErrorData> {
        let RequestWorkspace { workspace, root } = current_workspace()?;

        let result = workspace
            .run_snapshot(move |snapshot: &Snapshot| {
                let db = snapshot.raw_database();
                let ctx = ConversionContext::new(db);

                // Not `Analysis::fetch_crates`: that powers the editor
                // dependency tree and deliberately excludes local crates —
                // the opposite of what this tool is for.
                let mut crate_infos = Vec::new();
                for &krate in ide_db::base_db::all_crates(db).iter() {
                    let data = krate.data(db);
                    match data.origin {
                        CrateOrigin::Local { .. } => {}
                        CrateOrigin::Library { .. }
                        | CrateOrigin::Rustc { .. }
                        | CrateOrigin::Lang(_) => continue,
                    }
                    let extra = krate.extra_data(db);
                    crate_infos.push(CrateInfo {
                        name: extra
                            .display_name
                            .as_ref()
                            .map(|it| it.canonical_name().as_str().to_owned()),
                        version: extra.version.clone(),
                        root_file: ctx.file_path(data.root_file_id).unwrap_or_default(),
                    });
                }

                Ok::<_, McpError>(WorkspaceCratesResult { crates: crate_infos })
            })
            .await?;

        let text = result.render(Some(root.as_path()));
        to_tool_result(&result, text)
    }
}

impl Default for RaMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

impl ServerHandler for RaMcpServer {
    async fn call_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<rmcp::RoleServer>,
    ) -> Result<CallToolResult, ErrorData> {
        let request_workspace = match self.workspace_for_request(&context).await {
            Ok(request_workspace) => request_workspace,
            Err(error) => return to_tool_rejection_result(ErrorData::from(error)),
        };
        let tcc = rmcp::handler::server::tool::ToolCallContext::new(self, request, context);
        match REQUEST_WORKSPACE.scope(request_workspace, self.tool_router.call(tcc)).await {
            Ok(result) => Ok(result),
            Err(error) => to_tool_rejection_result(error),
        }
    }

    async fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<rmcp::RoleServer>,
    ) -> Result<ListToolsResult, ErrorData> {
        let mut tools = self.tool_router.list_all();
        for tool in &mut tools {
            set_output_schema(tool)?;
        }
        Ok(ListToolsResult { tools, meta: None, next_cursor: None })
    }

    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            server_info: Implementation::from_build_env(),
            instructions: Some(
                 "rust-analyzer MCP server: semantic code search, navigation, and refactoring for Rust. \
                 ALWAYS prefer these tools over bash grep/rg for Rust symbol work: definitions via find_symbol, \
                 call/usage/implementation graph reachability via reachable, structural patterns via search, \
                 and what-is-this at a position (definition + type + docs) via inspect. \
                 They resolve through traits, macros, and re-exports and never match comments or strings — grep cannot. \
                 Results are grep-shaped 'path:line:col: …' lines with workspace-relative paths and 1-based positions; \
                 pass those paths and positions straight back into other tools and into Read/Edit. \
                 Workspace is initialized from client roots; the first call may take a while as the index warms up. \
                 This is expected — wait for the result, do not fall back to grep."
                    .into(),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ide_db::line_index::LineIndex;
    use rmcp::model::ErrorCode;

    use crate::params::{FunctionFilterText, ImplFilterText};
    use crate::requests::PathFilter;
    use crate::server::grouping::{
        FilterCriteria, context_has_property, count_matches_by, filter_matches, group_key_function,
        group_key_usage_kind,
    };
    use crate::types::{LineColRange, SerializableMatch};

    #[test]
    fn test_line_index_single_line() {
        let text = "hello world";
        let line_index = LineIndex::new(text);

        let start = line_index.line_col(syntax::TextSize::from(6));
        let end = line_index.line_col(syntax::TextSize::from(11));

        assert_eq!(start.line, 0);
        assert_eq!(start.col, 6);
        assert_eq!(end.line, 0);
        assert_eq!(end.col, 11);
    }

    #[test]
    fn test_line_index_multiline() {
        let text = "line one\nline two\nline three";
        let line_index = LineIndex::new(text);

        let start = line_index.line_col(syntax::TextSize::from(14));
        let end = line_index.line_col(syntax::TextSize::from(17));

        assert_eq!(start.line, 1);
        assert_eq!(start.col, 5);
        assert_eq!(end.line, 1);
        assert_eq!(end.col, 8);
    }

    #[test]
    fn test_line_index_at_newline() {
        let text = "first\nsecond";
        let line_index = LineIndex::new(text);

        let pos = line_index.line_col(syntax::TextSize::from(5));
        assert_eq!(pos.line, 0);
        assert_eq!(pos.col, 5);

        let pos = line_index.line_col(syntax::TextSize::from(6));
        assert_eq!(pos.line, 1);
        assert_eq!(pos.col, 0);
    }

    #[test]
    fn test_line_index_empty_string() {
        let text = "";
        let line_index = LineIndex::new(text);

        let pos = line_index.line_col(syntax::TextSize::from(0));
        assert_eq!(pos.line, 0);
        assert_eq!(pos.col, 0);
    }

    #[test]
    fn test_error_codes() {
        assert_eq!(
            McpError::from(RequestError::NotInitialized).into_error_data().code,
            ErrorCode::INVALID_REQUEST
        );
        assert_eq!(
            McpError::from(RequestError::InvalidPath { path: "test".into() })
                .into_error_data()
                .code,
            ErrorCode::INVALID_PARAMS
        );
        assert_eq!(
            McpError::from(RequestError::FileNotFound { path: "test".into() })
                .into_error_data()
                .code,
            ErrorCode::INVALID_PARAMS
        );
        assert_eq!(
            McpError::from(RequestError::InvalidPosition {
                line: 1,
                column: 1,
                reason: "test".into(),
            })
            .into_error_data()
            .code,
            ErrorCode::INVALID_PARAMS
        );
        assert_eq!(
            McpError::from(RequestError::InvalidSyntax { kind: "test", message: "msg".into() })
                .into_error_data()
                .code,
            ErrorCode::INVALID_PARAMS
        );

        assert_eq!(
            McpError::from(AnalysisError::Failed { operation: "test", details: "details".into() })
                .into_error_data()
                .code,
            ErrorCode::INTERNAL_ERROR
        );
        assert_eq!(
            McpError::from(EnvironmentError::TaskFailed { message: "test".into() })
                .into_error_data()
                .code,
            ErrorCode::INTERNAL_ERROR
        );
    }

    #[test]
    fn test_error_messages() {
        let err = McpError::from(RequestError::NotInitialized);
        let data = err.into_error_data();
        assert_eq!(data.code, ErrorCode::INVALID_REQUEST);
        assert_eq!(
            data.message,
            "Server workspace not initialized. Client must provide workspace roots during MCP initialization."
        );

        let err = McpError::from(RequestError::InvalidPosition {
            line: 10,
            column: 5,
            reason: "out of bounds".into(),
        });
        let msg = err.into_error_data().message;
        assert_eq!(msg, "Invalid position (line 10, column 5): out of bounds");
    }

    #[test]
    fn test_path_filter_glob() {
        use std::path::Path;

        let filter = PathFilter::new("**/*.rs").expect("valid pattern");
        assert!(filter.matches(Path::new("src/main.rs")));
        assert!(filter.matches(Path::new("/home/user/project/lib.rs")));
        assert!(!filter.matches(Path::new("src/main.txt")));
    }

    #[test]
    fn test_path_filter_prefix() {
        use std::path::Path;

        let filter = PathFilter::new("src/").expect("valid pattern");
        assert!(filter.matches(Path::new("src/main.rs")));
        assert!(filter.matches(Path::new("/home/user/project/src/lib.rs")));
        assert!(!filter.matches(Path::new("tests/main.rs")));
    }

    #[test]
    fn test_path_filter_empty_error() {
        let result = PathFilter::new("");
        match result {
            Err(msg) => assert_eq!(msg, "pattern cannot be empty"),
            Ok(_) => panic!("expected error for empty pattern"),
        }
    }

    #[test]
    fn test_invalid_request_error() {
        let err =
            McpError::Request(RequestError::InvalidRequest { message: "missing rule".into() });
        let data = err.into_error_data();
        assert_eq!(data.code, ErrorCode::INVALID_PARAMS);
        assert_eq!(data.message, "Invalid request: missing rule");
    }

    #[test]
    fn test_group_by_usage_kind() {
        use crate::types::{SerializableContext, SerializableUsageKind};

        let make_match = |kind: SerializableUsageKind| SerializableMatch {
            file_path: PathBuf::from("test.rs"),
            range: LineColRange { start_line: 1, start_col: 1, end_line: 1, end_col: 1 },
            matched_text: "x".into(),
            context: SerializableContext::default(),
            usage_kind: kind,
        };

        let matches = vec![
            make_match(SerializableUsageKind::ForLoop),
            make_match(SerializableUsageKind::ForLoop),
            make_match(SerializableUsageKind::LetBinding),
            make_match(SerializableUsageKind::Other),
        ];

        let groups = count_matches_by(&matches, group_key_usage_kind);
        let groups: Vec<(&str, usize)> = groups.iter().map(|g| (g.key.as_str(), g.count)).collect();
        assert_eq!(groups, vec![("for_loop", 2), ("let_binding", 1), ("other", 1)]);
    }

    #[test]
    fn test_group_by_function() {
        use crate::types::{SerializableContext, SerializableUsageKind};

        let make_match = |fn_name: Option<&str>| SerializableMatch {
            file_path: PathBuf::from("test.rs"),
            range: LineColRange { start_line: 1, start_col: 1, end_line: 1, end_col: 1 },
            matched_text: "x".into(),
            context: SerializableContext {
                enclosing_function: fn_name.map(|s| s.to_owned()),
                ..Default::default()
            },
            usage_kind: SerializableUsageKind::Other,
        };

        let matches = vec![
            make_match(Some("main")),
            make_match(Some("main")),
            make_match(Some("helper")),
            make_match(None),
        ];

        let groups = count_matches_by(&matches, group_key_function);
        let groups: Vec<(&str, usize)> = groups.iter().map(|g| (g.key.as_str(), g.count)).collect();
        assert_eq!(groups, vec![("main", 2), ("<none>", 1), ("helper", 1)]);
    }

    fn make_test_matches(n: usize) -> Vec<SerializableMatch> {
        use crate::types::{SerializableContext, SerializableUsageKind};

        let mut matches = Vec::new();
        for i in 0..n {
            matches.push(SerializableMatch {
                file_path: PathBuf::from(format!("file_{i}.rs")),
                range: LineColRange {
                    start_line: i as u32 + 1,
                    start_col: 1,
                    end_line: i as u32 + 1,
                    end_col: 2,
                },
                matched_text: format!("match_{i}"),
                context: SerializableContext::default(),
                usage_kind: SerializableUsageKind::Other,
            });
        }
        matches
    }

    fn paginate(
        matches: &[SerializableMatch],
        offset: usize,
        limit: usize,
    ) -> Vec<SerializableMatch> {
        matches.iter().skip(offset).take(limit).cloned().collect()
    }

    #[test]
    fn pagination_properties() {
        use arbtest::arbtest;

        arbtest(|u| {
            let total: usize = u.arbitrary::<usize>()? % 256;
            let offset: usize = u.arbitrary::<usize>()? % 512;
            let limit: usize = u.arbitrary::<usize>()? % 512;

            let matches = make_test_matches(total);
            let page = paginate(&matches, offset, limit);

            let expected_len = if offset >= total { 0 } else { limit.min(total - offset) };
            assert_eq!(page.len(), expected_len);

            for (i, m) in page.iter().enumerate() {
                assert_eq!(m.matched_text, format!("match_{}", offset + i));
            }

            Ok(())
        });
    }

    fn make_rich_match(
        path: &str,
        is_test: bool,
        is_unsafe: bool,
        is_async: bool,
        fn_name: Option<&str>,
        impl_name: Option<&str>,
        usage_kind: crate::types::SerializableUsageKind,
    ) -> SerializableMatch {
        use crate::types::{ContextProperty, SerializableContext};

        let mut properties = Vec::new();
        if is_test {
            properties.push(ContextProperty::Test);
        }
        if is_unsafe {
            properties.push(ContextProperty::Unsafe);
        }
        if is_async {
            properties.push(ContextProperty::Async);
        }

        SerializableMatch {
            file_path: PathBuf::from(path),
            range: LineColRange { start_line: 1, start_col: 1, end_line: 1, end_col: 2 },
            matched_text: "x".into(),
            context: SerializableContext {
                enclosing_function: fn_name.map(|s| s.to_owned()),
                enclosing_impl: impl_name.map(|s| s.to_owned()),
                properties,
            },
            usage_kind,
        }
    }

    #[test]
    fn filter_matches_multiple_criteria() {
        use crate::types::{BoolFilter, ContextProperty, SerializableUsageKind};
        use arbtest::arbtest;

        const FUNCTIONS: &[&str] = &["main", "helper", "process"];
        const IMPLS: &[&str] = &["Foo", "Bar", "Baz"];
        const PATHS: &[&str] = &["src/lib.rs", "src/main.rs", "tests/test.rs"];
        const USAGE_KINDS: &[SerializableUsageKind] = &[
            SerializableUsageKind::ForLoop,
            SerializableUsageKind::LetBinding,
            SerializableUsageKind::Return,
            SerializableUsageKind::Other,
        ];

        fn arbitrary_bool_filter(
            u: &mut arbtest::arbitrary::Unstructured<'_>,
        ) -> Result<BoolFilter, arbtest::arbitrary::Error> {
            let tag: u8 = u.arbitrary::<u8>()? % 3;
            match tag {
                0 => Ok(BoolFilter::Any),
                1 => Ok(BoolFilter::Yes),
                2 => Ok(BoolFilter::No),
                _ => unreachable!(),
            }
        }

        arbtest(|u| {
            let num_matches: usize = u.arbitrary::<usize>()? % 64;

            let mut matches = Vec::new();
            for _ in 0..num_matches {
                let path_idx = u.arbitrary::<usize>()? % PATHS.len();
                let is_test: bool = u.arbitrary()?;
                let is_unsafe: bool = u.arbitrary()?;
                let is_async: bool = u.arbitrary()?;
                let has_fn: bool = u.arbitrary()?;
                let fn_name = if has_fn {
                    Some(FUNCTIONS[u.arbitrary::<usize>()? % FUNCTIONS.len()])
                } else {
                    None
                };
                let has_impl: bool = u.arbitrary()?;
                let impl_name = if has_impl {
                    Some(IMPLS[u.arbitrary::<usize>()? % IMPLS.len()])
                } else {
                    None
                };
                let usage_idx = u.arbitrary::<usize>()? % USAGE_KINDS.len();

                matches.push(make_rich_match(
                    PATHS[path_idx],
                    is_test,
                    is_unsafe,
                    is_async,
                    fn_name,
                    impl_name,
                    USAGE_KINDS[usage_idx],
                ));
            }

            let filter_test = arbitrary_bool_filter(u)?;
            let filter_unsafe = arbitrary_bool_filter(u)?;
            let filter_async = arbitrary_bool_filter(u)?;
            let filter_fn: Option<FunctionFilterText> = if u.arbitrary()? {
                Some(
                    FunctionFilterText::parse(FUNCTIONS[u.arbitrary::<usize>()? % FUNCTIONS.len()])
                        .unwrap(),
                )
            } else {
                None
            };
            let filter_impl: Option<ImplFilterText> = if u.arbitrary()? {
                Some(ImplFilterText::parse(IMPLS[u.arbitrary::<usize>()? % IMPLS.len()]).unwrap())
            } else {
                None
            };
            let filter_usage: Option<SerializableUsageKind> = if u.arbitrary()? {
                Some(USAGE_KINDS[u.arbitrary::<usize>()? % USAGE_KINDS.len()])
            } else {
                None
            };

            let criteria = FilterCriteria {
                path_filter: None,
                in_test: filter_test,
                in_unsafe: filter_unsafe,
                in_async: filter_async,
                in_function: filter_fn.clone(),
                in_impl: filter_impl.clone(),
                usage_kind: filter_usage,
            };

            let filtered = filter_matches(&matches, &criteria);

            assert!(filtered.len() <= matches.len());

            for m in &filtered {
                assert!(
                    filter_test.matches(context_has_property(&m.context, ContextProperty::Test))
                );
                assert!(
                    filter_unsafe
                        .matches(context_has_property(&m.context, ContextProperty::Unsafe))
                );
                assert!(
                    filter_async.matches(context_has_property(&m.context, ContextProperty::Async))
                );
                if let Some(ref fn_name) = filter_fn {
                    assert_eq!(m.context.enclosing_function.as_deref(), Some(fn_name.as_str()));
                }
                if let Some(ref impl_name) = filter_impl {
                    assert_eq!(m.context.enclosing_impl.as_deref(), Some(impl_name.as_str()));
                }
                if let Some(usage) = filter_usage {
                    assert_eq!(m.usage_kind.as_str(), usage.as_str());
                }
            }

            let mut expected_count = 0;
            for m in &matches {
                let pass_test =
                    filter_test.matches(context_has_property(&m.context, ContextProperty::Test));
                let pass_unsafe = filter_unsafe
                    .matches(context_has_property(&m.context, ContextProperty::Unsafe));
                let pass_async =
                    filter_async.matches(context_has_property(&m.context, ContextProperty::Async));
                let pass_fn = filter_fn.as_ref().is_none_or(|name| {
                    m.context.enclosing_function.as_deref().is_some_and(|n| n == name.as_str())
                });
                let pass_impl = filter_impl.as_ref().is_none_or(|name| {
                    m.context.enclosing_impl.as_deref().is_some_and(|n| n == name.as_str())
                });
                let pass_usage = match filter_usage {
                    Some(kind) => m.usage_kind.as_str() == kind.as_str(),
                    None => true,
                };
                if pass_test && pass_unsafe && pass_async && pass_fn && pass_impl && pass_usage {
                    expected_count += 1;
                }
            }
            assert_eq!(filtered.len(), expected_count);

            Ok(())
        });
    }
}
