//! Serializable result types returned by MCP tools.

use std::path::PathBuf;

use ide_db::base_db::SourceDatabase;
use ide_db::{FileId, RootDatabase, line_index};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use syntax::ast::{HasModuleItem, HasName};
use syntax::{AstNode, Edition, SourceFile, TextRange, ast};
use vfs::AnchoredPathBuf;

/// One-based, half-open text range in line/column form.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct LineColRange {
    /// One-based start line.
    pub start_line: u32,
    /// One-based start column.
    pub start_col: u32,
    /// One-based end line.
    pub end_line: u32,
    /// One-based end column.
    pub end_col: u32,
}

#[derive(Debug, Clone, Copy, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ToolRejectionCode {
    InvalidRequest,
    InvalidParams,
    InternalError,
    MethodNotFound,
    ParseError,
    ResourceNotFound,
    Unknown,
}

impl ToolRejectionCode {
    pub fn as_str(self) -> &'static str {
        match self {
            ToolRejectionCode::InvalidRequest => "invalid_request",
            ToolRejectionCode::InvalidParams => "invalid_params",
            ToolRejectionCode::InternalError => "internal_error",
            ToolRejectionCode::MethodNotFound => "method_not_found",
            ToolRejectionCode::ParseError => "parse_error",
            ToolRejectionCode::ResourceNotFound => "resource_not_found",
            ToolRejectionCode::Unknown => "unknown",
        }
    }
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ToolRejection {
    pub code: ToolRejectionCode,
    pub message: String,
    pub guidance: String,
}

#[derive(Debug, Clone, JsonSchema)]
#[serde(untagged)]
#[schemars(extend("type" = "object"))]
pub enum ToolOutput<T> {
    Ok(T),
    Rejected(ToolRejection),
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SsrPreviewResult {
    pub edits: Vec<SerializableEdit>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SsrApplyOutcome {
    pub files_changed: usize,
    pub edits_applied: usize,
    pub edits: Vec<SerializableEdit>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[schemars(extend("type" = "object"))]
pub enum SsrOutput {
    Preview { edits: Vec<SerializableEdit> },
    Applied { files_changed: usize, edits_applied: usize, edits: Vec<SerializableEdit> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SerializableSymbolKind {
    Attribute,
    BuiltinAttr,
    Const,
    ConstParam,
    CrateRoot,
    Derive,
    DeriveHelper,
    Enum,
    Field,
    Function,
    Method,
    Impl,
    InlineAsmRegOrRegClass,
    Label,
    LifetimeParam,
    Local,
    Macro,
    ProcMacro,
    Module,
    SelfParam,
    SelfType,
    Static,
    Struct,
    ToolModule,
    Trait,
    TypeAlias,
    TypeParam,
    Union,
    ValueParam,
    Variant,
}

impl SerializableSymbolKind {
    /// Short kind tag used in rendered result lines (`fn`, `struct`, …).
    /// Deliberately matches Rust surface syntax where one exists so lines read
    /// like source code.
    pub fn as_str(self) -> &'static str {
        match self {
            SerializableSymbolKind::Attribute => "attribute",
            SerializableSymbolKind::BuiltinAttr => "builtin_attr",
            SerializableSymbolKind::Const => "const",
            SerializableSymbolKind::ConstParam => "const_param",
            SerializableSymbolKind::CrateRoot => "crate",
            SerializableSymbolKind::Derive => "derive",
            SerializableSymbolKind::DeriveHelper => "derive_helper",
            SerializableSymbolKind::Enum => "enum",
            SerializableSymbolKind::Field => "field",
            SerializableSymbolKind::Function => "fn",
            SerializableSymbolKind::Method => "fn",
            SerializableSymbolKind::Impl => "impl",
            SerializableSymbolKind::InlineAsmRegOrRegClass => "asm_reg",
            SerializableSymbolKind::Label => "label",
            SerializableSymbolKind::LifetimeParam => "lifetime",
            SerializableSymbolKind::Local => "local",
            SerializableSymbolKind::Macro => "macro",
            SerializableSymbolKind::ProcMacro => "proc_macro",
            SerializableSymbolKind::Module => "mod",
            SerializableSymbolKind::SelfParam => "self",
            SerializableSymbolKind::SelfType => "Self",
            SerializableSymbolKind::Static => "static",
            SerializableSymbolKind::Struct => "struct",
            SerializableSymbolKind::ToolModule => "tool_module",
            SerializableSymbolKind::Trait => "trait",
            SerializableSymbolKind::TypeAlias => "type",
            SerializableSymbolKind::TypeParam => "type_param",
            SerializableSymbolKind::Union => "union",
            SerializableSymbolKind::ValueParam => "param",
            SerializableSymbolKind::Variant => "variant",
        }
    }
}

impl From<ide_db::SymbolKind> for SerializableSymbolKind {
    fn from(kind: ide_db::SymbolKind) -> Self {
        match kind {
            ide_db::SymbolKind::Attribute => SerializableSymbolKind::Attribute,
            ide_db::SymbolKind::BuiltinAttr => SerializableSymbolKind::BuiltinAttr,
            ide_db::SymbolKind::Const => SerializableSymbolKind::Const,
            ide_db::SymbolKind::ConstParam => SerializableSymbolKind::ConstParam,
            ide_db::SymbolKind::CrateRoot => SerializableSymbolKind::CrateRoot,
            ide_db::SymbolKind::Derive => SerializableSymbolKind::Derive,
            ide_db::SymbolKind::DeriveHelper => SerializableSymbolKind::DeriveHelper,
            ide_db::SymbolKind::Enum => SerializableSymbolKind::Enum,
            ide_db::SymbolKind::Field => SerializableSymbolKind::Field,
            ide_db::SymbolKind::Function => SerializableSymbolKind::Function,
            ide_db::SymbolKind::Method => SerializableSymbolKind::Method,
            ide_db::SymbolKind::Impl => SerializableSymbolKind::Impl,
            ide_db::SymbolKind::InlineAsmRegOrRegClass => {
                SerializableSymbolKind::InlineAsmRegOrRegClass
            }
            ide_db::SymbolKind::Label => SerializableSymbolKind::Label,
            ide_db::SymbolKind::LifetimeParam => SerializableSymbolKind::LifetimeParam,
            ide_db::SymbolKind::Local => SerializableSymbolKind::Local,
            ide_db::SymbolKind::Macro => SerializableSymbolKind::Macro,
            ide_db::SymbolKind::ProcMacro => SerializableSymbolKind::ProcMacro,
            ide_db::SymbolKind::Module => SerializableSymbolKind::Module,
            ide_db::SymbolKind::SelfParam => SerializableSymbolKind::SelfParam,
            ide_db::SymbolKind::SelfType => SerializableSymbolKind::SelfType,
            ide_db::SymbolKind::Static => SerializableSymbolKind::Static,
            ide_db::SymbolKind::Struct => SerializableSymbolKind::Struct,
            ide_db::SymbolKind::ToolModule => SerializableSymbolKind::ToolModule,
            ide_db::SymbolKind::Trait => SerializableSymbolKind::Trait,
            ide_db::SymbolKind::TypeAlias => SerializableSymbolKind::TypeAlias,
            ide_db::SymbolKind::TypeParam => SerializableSymbolKind::TypeParam,
            ide_db::SymbolKind::Union => SerializableSymbolKind::Union,
            ide_db::SymbolKind::ValueParam => SerializableSymbolKind::ValueParam,
            ide_db::SymbolKind::Variant => SerializableSymbolKind::Variant,
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SerializableInlayKind {
    Adjustment,
    BindingMode,
    Chaining,
    ClosingBrace,
    ClosureCapture,
    Discriminant,
    GenericParamList,
    Lifetime,
    Parameter,
    GenericParameter,
    Type,
    Dyn,
    Drop,
    RangeExclusive,
    ExternUnsafety,
}

impl SerializableInlayKind {
    pub fn as_str(self) -> &'static str {
        match self {
            SerializableInlayKind::Adjustment => "adjustment",
            SerializableInlayKind::BindingMode => "binding_mode",
            SerializableInlayKind::Chaining => "chaining",
            SerializableInlayKind::ClosingBrace => "closing_brace",
            SerializableInlayKind::ClosureCapture => "closure_capture",
            SerializableInlayKind::Discriminant => "discriminant",
            SerializableInlayKind::GenericParamList => "generic_params",
            SerializableInlayKind::Lifetime => "lifetime",
            SerializableInlayKind::Parameter => "param",
            SerializableInlayKind::GenericParameter => "generic_param",
            SerializableInlayKind::Type => "type",
            SerializableInlayKind::Dyn => "dyn",
            SerializableInlayKind::Drop => "drop",
            SerializableInlayKind::RangeExclusive => "range_exclusive",
            SerializableInlayKind::ExternUnsafety => "extern_unsafety",
        }
    }
}

impl From<ide::InlayKind> for SerializableInlayKind {
    fn from(kind: ide::InlayKind) -> Self {
        match kind {
            ide::InlayKind::Adjustment => SerializableInlayKind::Adjustment,
            ide::InlayKind::BindingMode => SerializableInlayKind::BindingMode,
            ide::InlayKind::Chaining => SerializableInlayKind::Chaining,
            ide::InlayKind::ClosingBrace => SerializableInlayKind::ClosingBrace,
            ide::InlayKind::ClosureCapture => SerializableInlayKind::ClosureCapture,
            ide::InlayKind::Discriminant => SerializableInlayKind::Discriminant,
            ide::InlayKind::GenericParamList => SerializableInlayKind::GenericParamList,
            ide::InlayKind::Lifetime => SerializableInlayKind::Lifetime,
            ide::InlayKind::Parameter => SerializableInlayKind::Parameter,
            ide::InlayKind::GenericParameter => SerializableInlayKind::GenericParameter,
            ide::InlayKind::Type => SerializableInlayKind::Type,
            ide::InlayKind::Dyn => SerializableInlayKind::Dyn,
            ide::InlayKind::Drop => SerializableInlayKind::Drop,
            ide::InlayKind::RangeExclusive => SerializableInlayKind::RangeExclusive,
            ide::InlayKind::ExternUnsafety => SerializableInlayKind::ExternUnsafety,
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SerializableInlayHintPosition {
    Before,
    After,
}

impl From<ide::InlayHintPosition> for SerializableInlayHintPosition {
    fn from(pos: ide::InlayHintPosition) -> Self {
        match pos {
            ide::InlayHintPosition::Before => SerializableInlayHintPosition::Before,
            ide::InlayHintPosition::After => SerializableInlayHintPosition::After,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum BoolFilter {
    #[default]
    Any,
    Yes,
    No,
}

impl BoolFilter {
    pub fn matches(self, value: bool) -> bool {
        match self {
            BoolFilter::Any => true,
            BoolFilter::Yes => value,
            BoolFilter::No => !value,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ReachableDirection {
    #[default]
    Incoming,
    Outgoing,
    Both,
}

impl ReachableDirection {
    pub fn as_str(self) -> &'static str {
        match self {
            ReachableDirection::Incoming => "incoming",
            ReachableDirection::Outgoing => "outgoing",
            ReachableDirection::Both => "both",
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ReachableScope {
    #[default]
    Workspace,
    WorkspaceAndDependencies,
}

impl ReachableScope {
    pub fn as_str(self) -> &'static str {
        match self {
            ReachableScope::Workspace => "workspace",
            ReachableScope::WorkspaceAndDependencies => "workspace_and_dependencies",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ReachableEdgeKind {
    Call,
    Implementation,
    Usage,
}

impl ReachableEdgeKind {
    pub fn as_str(self) -> &'static str {
        match self {
            ReachableEdgeKind::Call => "call",
            ReachableEdgeKind::Implementation => "implementation",
            ReachableEdgeKind::Usage => "usage",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ReferenceCategoryTag {
    Write,
    Read,
    Import,
    Test,
}

impl ReferenceCategoryTag {
    pub fn as_str(self) -> &'static str {
        match self {
            ReferenceCategoryTag::Write => "write",
            ReferenceCategoryTag::Read => "read",
            ReferenceCategoryTag::Import => "import",
            ReferenceCategoryTag::Test => "test",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ReachableUsageKind {
    Builtin,
    Callable,
    ExternCrateShorthand,
    Field,
    FieldShorthand,
    GenericParam,
    Import,
    Label,
    Local,
    Macro,
    Module,
    Type,
    Unknown,
    Value,
}

impl ReachableUsageKind {
    pub fn as_str(self) -> &'static str {
        match self {
            ReachableUsageKind::Builtin => "builtin",
            ReachableUsageKind::Callable => "callable",
            ReachableUsageKind::ExternCrateShorthand => "extern_crate_shorthand",
            ReachableUsageKind::Field => "field",
            ReachableUsageKind::FieldShorthand => "field_shorthand",
            ReachableUsageKind::GenericParam => "generic_param",
            ReachableUsageKind::Import => "import",
            ReachableUsageKind::Label => "label",
            ReachableUsageKind::Local => "local",
            ReachableUsageKind::Macro => "macro",
            ReachableUsageKind::Module => "module",
            ReachableUsageKind::Type => "type",
            ReachableUsageKind::Unknown => "unknown",
            ReachableUsageKind::Value => "value",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ContextProperty {
    Test,
    Unsafe,
    Async,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum GroupByField {
    File,
    UsageKind,
    Function,
    Impl,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct DefinitionLocation {
    pub file_path: PathBuf,
    pub range: LineColRange,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<SerializableSymbolKind>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, JsonSchema)]
pub struct PositionMetadata {
    pub sections: Vec<MetadataSection>,
}

impl PositionMetadata {
    pub fn from_markup(markup: &str) -> Self {
        let mut segments = Vec::new();
        let mut segment = String::new();
        for line in markup.trim_start().lines() {
            if is_metadata_separator(line) {
                segments.push(trim_metadata_block(&segment));
                segment.clear();
            } else {
                segment.push_str(line);
                segment.push('\n');
            }
        }
        segments.push(trim_metadata_block(&segment));

        let mut sections = Vec::new();
        if let Some(first) = segments.first() {
            let code_blocks = leading_code_blocks(first);
            let code_block_count = code_blocks.len();
            for (idx, text) in code_blocks.into_iter().enumerate() {
                let kind = metadata_code_kind(idx, code_block_count, &text);
                sections.push(MetadataSection { kind, text });
            }
        }

        for (idx, segment) in segments.into_iter().enumerate() {
            if idx == 0 || segment.is_empty() {
                continue;
            }
            sections.push(MetadataSection { kind: MetadataSectionKind::Docs, text: segment });
        }

        Self { sections }
    }

    pub fn is_empty(&self) -> bool {
        self.sections.is_empty()
    }

    pub fn replace_declaration(&mut self, text: String) {
        for MetadataSection { kind, text: section_text } in &mut self.sections {
            match kind {
                MetadataSectionKind::Declaration
                | MetadataSectionKind::Signature
                | MetadataSectionKind::Type
                | MetadataSectionKind::Code => {
                    *kind = MetadataSectionKind::Declaration;
                    *section_text = text;
                    return;
                }
                MetadataSectionKind::QualifiedPath | MetadataSectionKind::Docs => (),
            }
        }
        self.sections.push(MetadataSection { kind: MetadataSectionKind::Declaration, text });
    }
}

fn is_metadata_separator(line: &str) -> bool {
    let line = line.trim();
    line == "---" || line == "___"
}

fn trim_metadata_block(text: &str) -> String {
    text.trim_matches(|ch| ch == '\n' || ch == '\r').trim().to_owned()
}

fn leading_code_blocks(text: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut lines = text.lines();
    while let Some(line) = lines.next() {
        if !line.trim_start().starts_with("```") {
            continue;
        }
        let mut block = String::new();
        for line in lines.by_ref() {
            if line.trim_start().starts_with("```") {
                break;
            }
            block.push_str(line);
            block.push('\n');
        }
        let block = trim_metadata_block(&block);
        if !block.is_empty() {
            blocks.push(block);
        }
    }
    blocks
}

fn metadata_code_kind(idx: usize, code_block_count: usize, text: &str) -> MetadataSectionKind {
    if idx == 0 && code_block_count > 1 && parses_as_path_type(text) {
        return MetadataSectionKind::QualifiedPath;
    }
    if parses_as_function_signature(text) {
        return MetadataSectionKind::Signature;
    }
    if parses_as_item(text) || parses_as_record_field(text) {
        return MetadataSectionKind::Declaration;
    }
    if parses_as_type(text) {
        return MetadataSectionKind::Type;
    }
    if parses_as_variant(text) {
        return MetadataSectionKind::Declaration;
    }
    MetadataSectionKind::Code
}

fn parse_source_file(text: &str) -> Option<ast::SourceFile> {
    let parse = SourceFile::parse(text, Edition::CURRENT);
    if !parse.errors().is_empty() {
        return None;
    }
    Some(parse.tree())
}

fn single_item(text: &str) -> Option<ast::Item> {
    let file = parse_source_file(text)?;
    let mut items = file.items();
    let item = items.next()?;
    if items.next().is_some() {
        return None;
    }
    Some(item)
}

fn parses_as_function_signature(text: &str) -> bool {
    let wrapped = format!("{text} {{}}");
    let Some(item) = single_item(&wrapped) else {
        return false;
    };
    ast::Fn::cast(item.syntax().clone()).is_some()
}

fn parses_as_item(text: &str) -> bool {
    if let Some(item) = single_item(text)
        && is_declaration_item(&item)
    {
        return true;
    }
    let text = format!("{text};");
    let Some(item) = single_item(&text) else {
        return false;
    };
    is_declaration_item(&item)
}

fn is_declaration_item(item: &ast::Item) -> bool {
    match item {
        ast::Item::AsmExpr(_) => false,
        ast::Item::Const(_) => true,
        ast::Item::Enum(_) => true,
        ast::Item::ExternBlock(_) => true,
        ast::Item::ExternCrate(_) => true,
        ast::Item::Fn(_) => true,
        ast::Item::Impl(_) => true,
        ast::Item::MacroCall(_) => false,
        ast::Item::MacroDef(_) => true,
        ast::Item::MacroRules(_) => true,
        ast::Item::Module(_) => true,
        ast::Item::Static(_) => true,
        ast::Item::Struct(_) => true,
        ast::Item::Trait(_) => true,
        ast::Item::TypeAlias(_) => true,
        ast::Item::Union(_) => true,
        ast::Item::Use(_) => true,
    }
}

fn parses_as_record_field(text: &str) -> bool {
    let wrapped = format!("struct __RaMcp {{ {text}, }}");
    let Some(file) = parse_source_file(&wrapped) else {
        return false;
    };
    let Some(field_list) = file.syntax().descendants().find_map(ast::RecordFieldList::cast) else {
        return false;
    };
    let mut fields = field_list.fields();
    let Some(field) = fields.next() else {
        return false;
    };
    fields.next().is_none() && field.name().is_some() && field.ty().is_some()
}

fn parses_as_variant(text: &str) -> bool {
    let wrapped = format!("enum __RaMcp {{ {text}, }}");
    let Some(file) = parse_source_file(&wrapped) else {
        return false;
    };
    let Some(variant_list) = file.syntax().descendants().find_map(ast::VariantList::cast) else {
        return false;
    };
    let mut variants = variant_list.variants();
    let Some(variant) = variants.next() else {
        return false;
    };
    variants.next().is_none() && variant.name().is_some()
}

fn type_alias_type(text: &str) -> Option<ast::Type> {
    let wrapped = format!("type __RaMcp = {text};");
    let Some(ast::Item::TypeAlias(alias)) = single_item(&wrapped) else {
        return None;
    };
    alias.ty()
}

fn parses_as_type(text: &str) -> bool {
    type_alias_type(text).is_some()
}

fn parses_as_path_type(text: &str) -> bool {
    let Some(ty) = type_alias_type(text) else {
        return false;
    };
    ast::PathType::cast(ty.syntax().clone()).is_some()
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct MetadataSection {
    pub kind: MetadataSectionKind,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MetadataSectionKind {
    QualifiedPath,
    Signature,
    Declaration,
    Type,
    Code,
    Docs,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct InspectResult {
    pub definitions: Vec<DefinitionLocation>,
    pub metadata: PositionMetadata,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReadResult {
    pub file_path: PathBuf,
    pub range: LineColRange,
    pub text: String,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
#[serde(tag = "status", rename_all = "snake_case")]
#[schemars(extend("type" = "object"))]
pub enum RenameResult {
    Ok {
        files_changed: usize,
        edits_applied: usize,
        edits: Vec<SerializableEdit>,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        file_system_edits: Vec<SerializableFileSystemEdit>,
    },
    Rejected {
        reason: String,
    },
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SerializableFileSystemEdit {
    CreateFile { dst: String, initial_contents: String },
    MoveFile { src: String, dst: String },
    MoveDir { src: String, dst: String },
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ExpandMacroResult {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expansion: Option<String>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SerializableInlayHint {
    pub range: LineColRange,
    pub kind: SerializableInlayKind,
    pub label: String,
    pub position: SerializableInlayHintPosition,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct InlayHintsResult {
    pub hints: Vec<SerializableInlayHint>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct CrateInfo {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    pub root_file: PathBuf,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct WorkspaceCratesResult {
    pub crates: Vec<CrateInfo>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct MatchGroup {
    pub key: String,
    pub count: usize,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SearchResults {
    pub total_matches: usize,
    pub offset: u32,
    pub matches: Vec<SerializableMatch>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SearchCountResult {
    pub total_matches: usize,
    pub group_by: GroupByField,
    pub groups: Vec<MatchGroup>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[schemars(extend("type" = "object"))]
pub enum SearchOutput {
    Matches { total_matches: usize, offset: u32, matches: Vec<SerializableMatch> },
    Counts { total_matches: usize, group_by: GroupByField, groups: Vec<MatchGroup> },
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct AddArgumentPreviewResult {
    pub total_edits: usize,
    pub skipped: usize,
    pub edits: Vec<SerializableEdit>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct AddArgumentApplyOutcome {
    pub files_changed: usize,
    pub edits_applied: usize,
    pub skipped: usize,
    pub edits: Vec<SerializableEdit>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[schemars(extend("type" = "object"))]
pub enum AddArgumentOutput {
    Preview {
        total_edits: usize,
        skipped: usize,
        edits: Vec<SerializableEdit>,
    },
    Applied {
        files_changed: usize,
        edits_applied: usize,
        skipped: usize,
        edits: Vec<SerializableEdit>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SerializableUsageKind {
    ForLoop,
    WhileLet,
    IteratorMap,
    IteratorFilter,
    IteratorForEach,
    IteratorChain,
    FunctionArg,
    MethodArg,
    Return,
    LetBinding,
    Assignment,
    FieldInit,
    MatchArm,
    IfLet,
    MethodReceiver,
    Other,
}

impl SerializableUsageKind {
    pub fn as_str(self) -> &'static str {
        match self {
            SerializableUsageKind::ForLoop => "for_loop",
            SerializableUsageKind::WhileLet => "while_let",
            SerializableUsageKind::IteratorMap => "iterator_map",
            SerializableUsageKind::IteratorFilter => "iterator_filter",
            SerializableUsageKind::IteratorForEach => "iterator_for_each",
            SerializableUsageKind::IteratorChain => "iterator_chain",
            SerializableUsageKind::FunctionArg => "function_arg",
            SerializableUsageKind::MethodArg => "method_arg",
            SerializableUsageKind::Return => "return",
            SerializableUsageKind::LetBinding => "let_binding",
            SerializableUsageKind::Assignment => "assignment",
            SerializableUsageKind::FieldInit => "field_init",
            SerializableUsageKind::MatchArm => "match_arm",
            SerializableUsageKind::IfLet => "if_let",
            SerializableUsageKind::MethodReceiver => "method_receiver",
            SerializableUsageKind::Other => "other",
        }
    }
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SerializableMatch {
    pub file_path: PathBuf,
    pub range: LineColRange,
    pub matched_text: String,
    pub context: SerializableContext,
    pub usage_kind: SerializableUsageKind,
}

#[derive(Debug, Clone, Default, Serialize, JsonSchema)]
pub struct SerializableContext {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enclosing_function: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enclosing_impl: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub properties: Vec<ContextProperty>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableLocation {
    pub file_path: PathBuf,
    pub range: LineColRange,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub line_text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enclosing_item: Option<ReachableEnclosingItem>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_excerpt: Option<ReachableSourceExcerpt>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableEnclosingItem {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<SerializableSymbolKind>,
    pub range: LineColRange,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableSourceExcerpt {
    pub kind: String,
    pub range: LineColRange,
    pub text: String,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableGenericArgument {
    pub parameter: String,
    pub value: String,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableNode {
    pub id: usize,
    pub depth: u32,
    pub file_path: PathBuf,
    pub range: LineColRange,
    pub full_range: LineColRange,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<SerializableSymbolKind>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub declaration: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visibility: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trait_context: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub impl_context: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub monikers: Vec<String>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableEdge {
    pub depth: u32,
    pub kind: ReachableEdgeKind,
    pub location: ReachableLocation,
    pub source_node: usize,
    pub target_node: usize,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub reference_categories: Vec<ReferenceCategoryTag>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub usage_kind: Option<ReachableUsageKind>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub generic_substitution: Vec<ReachableGenericArgument>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub receiver_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dispatch: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trait_context: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub macro_expansion: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub monikers: Vec<String>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachabilityDepthCount {
    pub depth: u32,
    pub nodes: usize,
    pub edges: usize,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct ReachableResult {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub roots: Vec<usize>,
    pub direction: ReachableDirection,
    pub scope: ReachableScope,
    pub edge_kinds: Vec<ReachableEdgeKind>,
    pub depth: u32,
    pub depths: Vec<ReachabilityDepthCount>,
    pub nodes: Vec<ReachableNode>,
    pub edges: Vec<ReachableEdge>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SerializableEdit {
    pub file_path: PathBuf,
    pub range: LineColRange,
    pub new_text: String,
}

#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SerializableSymbol {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<SerializableSymbolKind>,
    pub file_path: PathBuf,
    pub range: LineColRange,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub focus_range: Option<LineColRange>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// `symbol_search` caps the underlying index query, so the true total is
/// unknown; `more` reports whether at least one further result exists past
/// this page.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct SymbolsResults {
    pub offset: u32,
    pub more: bool,
    pub symbols: Vec<SerializableSymbol>,
}

#[derive(Clone, Copy)]
pub struct ConversionContext<'a> {
    db: &'a RootDatabase,
}

impl<'a> ConversionContext<'a> {
    pub fn new(db: &'a RootDatabase) -> Self {
        Self { db }
    }

    pub fn file_path(&self, file_id: FileId) -> Option<PathBuf> {
        let path = self.db.file_path(file_id)?;
        match path.as_path() {
            Some(path) => Some(path.to_owned().into()),
            None => Some(PathBuf::from(path.to_string())),
        }
    }

    pub fn resolve_anchored_path(&self, anchored: &AnchoredPathBuf) -> PathBuf {
        let Some(mut base) = self.db.file_path(anchored.anchor) else {
            return PathBuf::default();
        };
        base.pop();
        let Some(joined) = base.join(&anchored.path) else {
            return PathBuf::default();
        };
        match joined.as_path() {
            Some(path) => path.to_owned().into(),
            None => PathBuf::from(joined.to_string()),
        }
    }

    /// The trimmed text of source line `line` (0-based), for the `— context`
    /// part of rendered result lines. Empty string when the line is out of
    /// bounds rather than an error: context is decoration, not data.
    pub fn line_text(&self, file_id: FileId, line: u32) -> String {
        use ide_db::base_db::SourceDatabase;

        let text = self.db.file_text(file_id).text(self.db);
        let index = line_index(self.db, file_id);
        let Some(start) = index.offset(line_index::LineCol { line, col: 0 }) else {
            return String::new();
        };
        let end = index
            .offset(line_index::LineCol { line: line + 1, col: 0 })
            .map(usize::from)
            .unwrap_or(text.len());
        text[usize::from(start)..end].trim().to_owned()
    }

    pub fn text_range_to_line_col(&self, file_id: FileId, range: TextRange) -> LineColRange {
        let line_index = line_index(self.db, file_id);
        let start = line_index.line_col(range.start());
        let end = line_index.line_col(range.end());
        LineColRange {
            start_line: start.line + 1,
            start_col: start.col + 1,
            end_line: end.line + 1,
            end_col: end.col + 1,
        }
    }

    pub fn convert_match(&self, m: &ide_ssr::StructuredMatch) -> SerializableMatch {
        let ide_ssr::StructuredMatch { file_id, range, ref matched_text, ref context, usage_kind } =
            *m;

        SerializableMatch {
            file_path: self.file_path(file_id).unwrap_or_default(),
            range: self.text_range_to_line_col(file_id, range),
            matched_text: matched_text.clone(),
            context: self.convert_context(context),
            usage_kind: convert_usage_kind(usage_kind),
        }
    }

    fn convert_context(&self, c: &ide_ssr::MatchContext) -> SerializableContext {
        let ide_ssr::MatchContext {
            ref enclosing_function,
            ref enclosing_impl,
            is_test,
            is_unsafe,
            is_async,
        } = *c;

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

        SerializableContext {
            enclosing_function: enclosing_function.clone(),
            enclosing_impl: enclosing_impl.clone(),
            properties,
        }
    }
}

fn convert_usage_kind(kind: ide_ssr::UsageKind) -> SerializableUsageKind {
    match kind {
        ide_ssr::UsageKind::ForLoop => SerializableUsageKind::ForLoop,
        ide_ssr::UsageKind::WhileLet => SerializableUsageKind::WhileLet,
        ide_ssr::UsageKind::IteratorMap => SerializableUsageKind::IteratorMap,
        ide_ssr::UsageKind::IteratorFilter => SerializableUsageKind::IteratorFilter,
        ide_ssr::UsageKind::IteratorForEach => SerializableUsageKind::IteratorForEach,
        ide_ssr::UsageKind::IteratorChain => SerializableUsageKind::IteratorChain,
        ide_ssr::UsageKind::FunctionArg => SerializableUsageKind::FunctionArg,
        ide_ssr::UsageKind::MethodArg => SerializableUsageKind::MethodArg,
        ide_ssr::UsageKind::Return => SerializableUsageKind::Return,
        ide_ssr::UsageKind::LetBinding => SerializableUsageKind::LetBinding,
        ide_ssr::UsageKind::Assignment => SerializableUsageKind::Assignment,
        ide_ssr::UsageKind::FieldInit => SerializableUsageKind::FieldInit,
        ide_ssr::UsageKind::MatchArm => SerializableUsageKind::MatchArm,
        ide_ssr::UsageKind::IfLet => SerializableUsageKind::IfLet,
        ide_ssr::UsageKind::MethodReceiver => SerializableUsageKind::MethodReceiver,
        ide_ssr::UsageKind::Other => SerializableUsageKind::Other,
    }
}
