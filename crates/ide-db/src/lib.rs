//! This crate defines the core data structure representing IDE state -- `RootDatabase`.
//!
//! It is mainly a `HirDatabase` for semantic analysis, plus a `SymbolsDatabase`, for fuzzy search.

mod apply_change;

pub mod active_parameter;
pub mod assists;
pub mod defs;
pub mod documentation;
pub mod famous_defs;
pub mod helpers;
pub mod items_locator;
pub mod label;
pub mod path_transform;
pub mod prime_caches;
pub mod rename;
pub mod rust_doc;
pub mod search;
pub mod source_change;
pub mod symbol_index;
pub mod text_edit;
pub mod traits;
pub mod ty_filter;
pub mod use_trivial_constructor;

pub mod imports {
    pub mod import_assets;
    pub mod insert_use;
    pub mod merge_imports;
}

pub mod generated {
    pub mod lints;
}

pub mod syntax_helpers {
    pub mod format_string;
    pub mod format_string_exprs;
    pub mod tree_diff;
    pub use hir::prettify_macro_expansion;
    pub mod node_ext;
    pub mod suggest_name;

    pub use parser::LexedStr;
}

use dashmap::DashMap;
pub use hir::ChangeWithProcMacros;
use salsa::Durability;
use vfs::AnchoredPath;

use std::{fmt, hash::BuildHasherDefault, mem::ManuallyDrop};

use base_db::{
    db_ext_macro::{self},
    CrateId, FileText, RootQueryDb, SourceDatabase, SourceRoot, SourceRootId, SourceRootInput,
    Upcast, DEFAULT_FILE_TEXT_LRU_CAP,
};
use hir::{
    db::{DefDatabase, ExpandDatabase, HirDatabase},
    FilePositionWrapper, FileRangeWrapper,
};
use triomphe::Arc;

use crate::{line_index::LineIndex, symbol_index::SymbolsDatabase};
pub use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

pub use ::line_index;

/// `base_db` is normally also needed in places where `ide_db` is used, so this re-export is for convenience.
pub use base_db;
pub use span::{EditionedFileId, FileId};

pub type FxIndexSet<T> = indexmap::IndexSet<T, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;
pub type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

pub type FilePosition = FilePositionWrapper<FileId>;
pub type FileRange = FileRangeWrapper<FileId>;

#[salsa::db]
pub struct RootDatabase {
    // We use `ManuallyDrop` here because every codegen unit that contains a
    // `&RootDatabase -> &dyn OtherDatabase` cast will instantiate its drop glue in the vtable,
    // which duplicates `Weak::drop` and `Arc::drop` tens of thousands of times, which makes
    // compile times of all `ide_*` and downstream crates suffer greatly.
    storage: ManuallyDrop<salsa::Storage<Self>>,
    files: Arc<DashMap<vfs::FileId, FileText, BuildHasherDefault<FxHasher>>>,
    source_roots: Arc<DashMap<vfs::FileId, SourceRootInput, BuildHasherDefault<FxHasher>>>,
}

impl std::panic::RefUnwindSafe for RootDatabase {}

#[salsa::db]
impl salsa::Database for RootDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

impl Drop for RootDatabase {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.storage) };
    }
}

impl Clone for RootDatabase {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            files: self.files.clone(),
            source_roots: self.source_roots.clone(),
        }
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish()
    }
}

impl Upcast<dyn ExpandDatabase> for RootDatabase {
    #[inline]
    fn upcast(&self) -> &(dyn ExpandDatabase + 'static) {
        self
    }
}

impl Upcast<dyn DefDatabase> for RootDatabase {
    #[inline]
    fn upcast(&self) -> &(dyn DefDatabase + 'static) {
        self
    }
}

impl Upcast<dyn HirDatabase> for RootDatabase {
    #[inline]
    fn upcast(&self) -> &(dyn HirDatabase + 'static) {
        self
    }
}

impl Upcast<dyn RootQueryDb> for RootDatabase {
    fn upcast(&self) -> &(dyn RootQueryDb + 'static) {
        self
    }
}

impl Upcast<dyn SourceDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn SourceDatabase + 'static) {
        self
    }
}

#[salsa::db]
impl SourceDatabase for RootDatabase {
    fn file_text(&self, file_id: vfs::FileId) -> FileText {
        *self.files.get(&file_id).expect("Unable to fetch file; this is a bug")
    }

    fn set_file_text(&self, file_id: vfs::FileId, text: &str) {
        self.files.insert(file_id, FileText::new(self, file_id, Arc::from(text)));
    }

    fn set_file_text_with_durability(
        &self,
        file_id: vfs::FileId,
        text: &str,
        durability: Durability,
    ) {
        self.files.insert(
            file_id,
            FileText::builder(file_id, Arc::from(text)).durability(durability).new(self),
        );
    }

    /// Source root of the file.
    fn source_root(&self, file_id: vfs::FileId) -> SourceRootInput {
        let source_root =
            self.source_roots.get(&file_id).expect("Unable to fetch source root id; this is a bug");

        *source_root
    }

    fn set_source_root_with_durability(
        &self,
        file_id: vfs::FileId,
        source_root_id: SourceRootId,
        source_root: Arc<SourceRoot>,
        durability: Durability,
    ) {
        let input =
            SourceRootInput::builder(source_root_id, source_root).durability(durability).new(self);
        self.source_roots.insert(file_id, input);
    }

    fn resolve_path(&self, path: AnchoredPath<'_>) -> Option<FileId> {
        // FIXME: this *somehow* should be platform agnostic...
        let source_root = self.source_root(path.anchor);
        source_root.source_root(self).resolve_path(path)
    }

    fn relevant_crates(&self, file_id: FileId) -> Arc<[CrateId]> {
        let _p = tracing::info_span!("relevant_crates").entered();

        let file_id = self.file_text(file_id).file_id(self);
        let source_root = self.source_root(file_id);
        self.source_root_crates(source_root.source_root_id(self))
    }
}

impl Default for RootDatabase {
    fn default() -> RootDatabase {
        RootDatabase::new(None)
    }
}

impl RootDatabase {
    pub fn new(lru_capacity: Option<u16>) -> RootDatabase {
        let mut db = RootDatabase {
            storage: ManuallyDrop::new(salsa::Storage::default()),
            files: Default::default(),
            source_roots: Default::default(),
        };
        db.set_crate_graph_with_durability(Default::default(), Durability::HIGH);
        db.set_proc_macros_with_durability(Default::default(), Durability::HIGH);
        db.set_local_roots_with_durability(Default::default(), Durability::HIGH);
        db.set_library_roots_with_durability(Default::default(), Durability::HIGH);
        db.set_expand_proc_attr_macros_with_durability(false, Durability::HIGH);
        db.update_base_query_lru_capacities(lru_capacity);
        // hir::setup_syntax_context_root(db.as_dyn_database());
        db
    }

    pub fn enable_proc_attr_macros(&mut self) {
        self.set_expand_proc_attr_macros_with_durability(true, Durability::HIGH);
    }

    pub fn update_base_query_lru_capacities(&mut self, _lru_capacity: Option<u16>) {
        // let lru_capacity = lru_capacity.unwrap_or(base_db::DEFAULT_PARSE_LRU_CAP);
        // base_db::FileTextQuery.in_db_mut(self).set_lru_capacity(DEFAULT_FILE_TEXT_LRU_CAP);
        // base_db::ParseQuery.in_db_mut(self).set_lru_capacity(lru_capacity);
        // // macro expansions are usually rather small, so we can afford to keep more of them alive
        // hir::db::ParseMacroExpansionQuery.in_db_mut(self).set_lru_capacity(4 * lru_capacity);
        // hir::db::BorrowckQuery.in_db_mut(self).set_lru_capacity(base_db::DEFAULT_BORROWCK_LRU_CAP);
        // hir::db::BodyWithSourceMapQuery.in_db_mut(self).set_lru_capacity(2048);
    }

    pub fn update_lru_capacities(&mut self, _lru_capacities: &FxHashMap<Box<str>, u16>) {
        // use hir::db as hir_db;

        // base_db::FileTextQuery.in_db_mut(self).set_lru_capacity(DEFAULT_FILE_TEXT_LRU_CAP);
        // base_db::ParseQuery.in_db_mut(self).set_lru_capacity(
        //     lru_capacities
        //         .get(stringify!(ParseQuery))
        //         .copied()
        //         .unwrap_or(base_db::DEFAULT_PARSE_LRU_CAP),
        // );
        // hir_db::ParseMacroExpansionQuery.in_db_mut(self).set_lru_capacity(
        //     lru_capacities
        //         .get(stringify!(ParseMacroExpansionQuery))
        //         .copied()
        //         .unwrap_or(4 * base_db::DEFAULT_PARSE_LRU_CAP),
        // );
        // hir_db::BorrowckQuery.in_db_mut(self).set_lru_capacity(
        //     lru_capacities
        //         .get(stringify!(BorrowckQuery))
        //         .copied()
        //         .unwrap_or(base_db::DEFAULT_BORROWCK_LRU_CAP),
        // );
        // hir::db::BodyWithSourceMapQuery.in_db_mut(self).set_lru_capacity(2048);
    }

    pub fn snapshot(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            files: self.files.clone(),
            source_roots: self.source_roots.clone(),
        }
    }
}

#[db_ext_macro::query_group]
pub trait LineIndexDatabase: base_db::RootQueryDb {
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id).text(db);
    Arc::new(LineIndex::new(&text))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolKind {
    Attribute,
    BuiltinAttr,
    Const,
    ConstParam,
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
    TraitAlias,
    TypeAlias,
    TypeParam,
    Union,
    ValueParam,
    Variant,
}

impl From<hir::MacroKind> for SymbolKind {
    fn from(it: hir::MacroKind) -> Self {
        match it {
            hir::MacroKind::Declarative | hir::MacroKind::BuiltIn => SymbolKind::Macro,
            hir::MacroKind::ProcMacro => SymbolKind::ProcMacro,
            hir::MacroKind::Derive => SymbolKind::Derive,
            hir::MacroKind::Attr => SymbolKind::Attribute,
        }
    }
}

impl From<hir::ModuleDefId> for SymbolKind {
    fn from(it: hir::ModuleDefId) -> Self {
        match it {
            hir::ModuleDefId::ConstId(..) => SymbolKind::Const,
            hir::ModuleDefId::EnumVariantId(..) => SymbolKind::Variant,
            hir::ModuleDefId::FunctionId(..) => SymbolKind::Function,
            hir::ModuleDefId::MacroId(hir::MacroId::ProcMacroId(..)) => SymbolKind::ProcMacro,
            hir::ModuleDefId::MacroId(..) => SymbolKind::Macro,
            hir::ModuleDefId::ModuleId(..) => SymbolKind::Module,
            hir::ModuleDefId::StaticId(..) => SymbolKind::Static,
            hir::ModuleDefId::AdtId(hir::AdtId::StructId(..)) => SymbolKind::Struct,
            hir::ModuleDefId::AdtId(hir::AdtId::EnumId(..)) => SymbolKind::Enum,
            hir::ModuleDefId::AdtId(hir::AdtId::UnionId(..)) => SymbolKind::Union,
            hir::ModuleDefId::TraitId(..) => SymbolKind::Trait,
            hir::ModuleDefId::TraitAliasId(..) => SymbolKind::TraitAlias,
            hir::ModuleDefId::TypeAliasId(..) => SymbolKind::TypeAlias,
            hir::ModuleDefId::BuiltinType(..) => SymbolKind::TypeAlias,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SnippetCap {
    _private: (),
}

impl SnippetCap {
    pub const fn new(allow_snippets: bool) -> Option<SnippetCap> {
        if allow_snippets {
            Some(SnippetCap { _private: () })
        } else {
            None
        }
    }
}

pub struct Ranker<'a> {
    pub kind: parser::SyntaxKind,
    pub text: &'a str,
    pub ident_kind: bool,
}

impl<'a> Ranker<'a> {
    pub const MAX_RANK: usize = 0b1110;

    pub fn from_token(token: &'a syntax::SyntaxToken) -> Self {
        let kind = token.kind();
        Ranker { kind, text: token.text(), ident_kind: kind.is_any_identifier() }
    }

    /// A utility function that ranks a token again a given kind and text, returning a number that
    /// represents how close the token is to the given kind and text.
    pub fn rank_token(&self, tok: &syntax::SyntaxToken) -> usize {
        let tok_kind = tok.kind();

        let exact_same_kind = tok_kind == self.kind;
        let both_idents = exact_same_kind || (tok_kind.is_any_identifier() && self.ident_kind);
        let same_text = tok.text() == self.text;
        // anything that mapped into a token tree has likely no semantic information
        let no_tt_parent =
            tok.parent().is_some_and(|it| it.kind() != parser::SyntaxKind::TOKEN_TREE);
        (both_idents as usize)
            | ((exact_same_kind as usize) << 1)
            | ((same_text as usize) << 2)
            | ((no_tt_parent as usize) << 3)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
    WeakWarning,
    Allow,
}
