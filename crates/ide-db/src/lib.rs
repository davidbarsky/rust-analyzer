//! This crate defines the core data structure representing IDE state -- `RootDatabase`.
//!
//! It is mainly a `HirDatabase` for semantic analysis, plus a `SymbolsDatabase`, for fuzzy search.

#![cfg_attr(feature = "in-rust-tree", feature(rustc_private))]

#[cfg(feature = "in-rust-tree")]
extern crate rustc_driver as _;

extern crate self as ide_db;

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
pub mod ra_fixture;
pub mod range_mapper;
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

pub use hir::{ChangeWithProcMacros, EditionedFileId};
use salsa::Durability;

use std::{fmt, mem::ManuallyDrop};

use base_db::{
    CrateGraphBuilder, CratesMap, Nonce, SourceDatabase, set_all_crates_with_durability,
};
use hir::{FilePositionWrapper, FileRangeWrapper, db::HirDatabase};
use triomphe::Arc;

use crate::line_index::LineIndex;
pub use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

pub use ::line_index;

/// `base_db` is normally also needed in places where `ide_db` is used, so this re-export is for convenience.
pub use base_db::{self, FxIndexMap, FxIndexSet};
pub use span::{self, FileId};

pub type FilePosition = FilePositionWrapper<FileId>;
pub type FileRange = FileRangeWrapper<FileId>;

#[salsa_macros::db]
pub struct RootDatabase {
    // FIXME: Revisit this commit now that we migrated to the new salsa, given we store arcs in this
    // db directly now
    // We use `ManuallyDrop` here because every codegen unit that contains a
    // `&RootDatabase -> &dyn OtherDatabase` cast will instantiate its drop glue in the vtable,
    // which duplicates `Weak::drop` and `Arc::drop` tens of thousands of times, which makes
    // compile times of all `ide_*` and downstream crates suffer greatly.
    storage: ManuallyDrop<salsa::Storage<Self>>,
    crates_map: Arc<CratesMap>,
    nonce: Nonce,
}

impl std::panic::RefUnwindSafe for RootDatabase {}

#[salsa_macros::db]
impl salsa::Database for RootDatabase {}

impl Drop for RootDatabase {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.storage) };
    }
}

impl Clone for RootDatabase {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            crates_map: self.crates_map.clone(),
            nonce: self.nonce,
        }
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish()
    }
}

#[salsa_macros::db]
impl SourceDatabase for RootDatabase {
    fn crates_map(&self) -> Arc<CratesMap> {
        self.crates_map.clone()
    }

    fn nonce_and_revision(&self) -> (Nonce, salsa::Revision) {
        (self.nonce, salsa::plumbing::ZalsaDatabase::zalsa(self).current_revision())
    }

    fn line_column(&self, file: FileId, offset: syntax::TextSize) -> Result<(u32, u32), ()> {
        line_index(self, file).try_line_col(offset).map(|lc| (lc.line, lc.col)).ok_or(())
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
            crates_map: Default::default(),
            nonce: Nonce::new(),
        };
        // This needs to be here otherwise `CrateGraphBuilder` will panic.
        set_all_crates_with_durability(&mut db, std::iter::empty(), Durability::HIGH);
        CrateGraphBuilder::default().set_in_db(&mut db);
        hir::ProcMacros::init_default(&db, Durability::MEDIUM);
        _ = base_db::FileTextTable::builder(Default::default())
            .durability(Durability::LOW)
            .new(&db);
        _ = base_db::SourceRootTable::builder(Default::default())
            .durability(Durability::LOW)
            .new(&db);
        hir::db::set_expand_proc_attr_macros(&mut db, false);
        db.update_base_query_lru_capacities(lru_capacity);
        db
    }

    pub fn enable_proc_attr_macros(&mut self) {
        hir::db::set_expand_proc_attr_macros(self, true);
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
        // FIXME(salsa-transition): bring this back; allow changing LRU settings at runtime.
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
}

pub fn line_index(db: &dyn SourceDatabase, file_id: FileId) -> &Arc<LineIndex> {
    #[salsa::interned]
    pub struct InternedFileId {
        id: FileId,
    }
    #[salsa::tracked(returns(ref))]
    fn line_index<'db>(
        db: &'db dyn SourceDatabase,
        file_id: InternedFileId<'db>,
    ) -> Arc<LineIndex> {
        let text = db.file_text(file_id.id(db)).text(db);
        Arc::new(LineIndex::new(text))
    }
    line_index(db, InternedFileId::new(db, file_id))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolKind {
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

impl From<hir::MacroKind> for SymbolKind {
    fn from(it: hir::MacroKind) -> Self {
        match it {
            hir::MacroKind::Declarative | hir::MacroKind::DeclarativeBuiltIn => SymbolKind::Macro,
            hir::MacroKind::ProcMacro => SymbolKind::ProcMacro,
            hir::MacroKind::Derive | hir::MacroKind::DeriveBuiltIn => SymbolKind::Derive,
            hir::MacroKind::Attr | hir::MacroKind::AttrBuiltIn => SymbolKind::Attribute,
        }
    }
}

impl SymbolKind {
    pub fn from_module_def(db: &dyn HirDatabase, it: hir::ModuleDef) -> Self {
        match it {
            hir::ModuleDef::Const(..) => SymbolKind::Const,
            hir::ModuleDef::EnumVariant(..) => SymbolKind::Variant,
            hir::ModuleDef::Function(..) => SymbolKind::Function,
            hir::ModuleDef::Macro(mac) if mac.is_proc_macro() => SymbolKind::ProcMacro,
            hir::ModuleDef::Macro(..) => SymbolKind::Macro,
            hir::ModuleDef::Module(m) if m.is_crate_root(db) => SymbolKind::CrateRoot,
            hir::ModuleDef::Module(..) => SymbolKind::Module,
            hir::ModuleDef::Static(..) => SymbolKind::Static,
            hir::ModuleDef::Adt(hir::Adt::Struct(..)) => SymbolKind::Struct,
            hir::ModuleDef::Adt(hir::Adt::Enum(..)) => SymbolKind::Enum,
            hir::ModuleDef::Adt(hir::Adt::Union(..)) => SymbolKind::Union,
            hir::ModuleDef::Trait(..) => SymbolKind::Trait,
            hir::ModuleDef::TypeAlias(..) => SymbolKind::TypeAlias,
            hir::ModuleDef::BuiltinType(..) => SymbolKind::TypeAlias,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SnippetCap {
    _private: (),
}

impl SnippetCap {
    pub const fn new(allow_snippets: bool) -> Option<SnippetCap> {
        if allow_snippets { Some(SnippetCap { _private: () }) } else { None }
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

#[derive(Clone, Copy)]
pub struct MiniCore<'a>(&'a str);

impl<'a> MiniCore<'a> {
    #[inline]
    pub fn new(minicore: &'a str) -> Self {
        Self(minicore)
    }

    #[inline]
    pub const fn default() -> Self {
        Self(test_utils::MiniCore::RAW_SOURCE)
    }
}

impl std::fmt::Debug for MiniCore<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("MiniCore");
        if self.0 == test_utils::MiniCore::RAW_SOURCE {
            // Don't print the whole contents if they correspond to the default.
            // The `format_args!` makes it so that the output is
            // `MiniCore(<default>)` and not `MiniCore("<default>").
            d.field(&format_args!("<default>"));
        } else {
            d.field(&self.0);
        };
        d.finish()
    }
}

impl<'a> Default for MiniCore<'a> {
    #[inline]
    fn default() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {
    use base_db::{FileChange, FileId, FileSet, SourceDatabase, SourceRootKind, VfsPath};

    use crate::RootDatabase;

    #[test]
    fn source_roots_provide_file_id_and_path() {
        let mut db = RootDatabase::new(None);
        let file_id = FileId::from_raw(0);
        let path = VfsPath::new_virtual_path("/foo.rs".to_owned());
        let mut file_set = FileSet::default();
        file_set.insert(file_id, path.clone());

        let mut change = FileChange::default();
        change.set_roots(vec![(SourceRootKind::Local, file_set)]);
        change.apply(&mut db);

        assert_eq!(db.file_path(file_id), Some(path.clone()));
        assert_eq!(db.file_id_for_path(&path), Some(file_id));
        assert_eq!(db.file_path(FileId::from_raw(1)), None);
        assert_eq!(db.file_id_for_path(&VfsPath::new_virtual_path("/bar.rs".to_owned())), None);
    }

    #[test]
    fn duplicate_paths_are_rejected() {
        let mut db = RootDatabase::new(None);
        let first_lib = FileId::from_raw(0);
        let first_foo = FileId::from_raw(1);
        let second_lib = FileId::from_raw(2);
        let second_foo = FileId::from_raw(3);
        let lib_path = VfsPath::new_virtual_path("/lib.rs".to_owned());
        let foo_path = VfsPath::new_virtual_path("/foo.rs".to_owned());

        let mut first_file_set = FileSet::default();
        first_file_set.insert(first_lib, lib_path.clone());
        first_file_set.insert(first_foo, foo_path.clone());
        let mut second_file_set = FileSet::default();
        second_file_set.insert(second_lib, lib_path.clone());
        second_file_set.insert(second_foo, foo_path.clone());

        let mut change = FileChange::default();
        change.set_roots(vec![
            (SourceRootKind::Local, first_file_set),
            (SourceRootKind::Local, second_file_set),
        ]);
        let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            change.apply(&mut db);
        }))
        .expect_err("duplicate file path should panic");
        let message = match panic.downcast_ref::<String>() {
            Some(message) => message.as_str(),
            None => match panic.downcast_ref::<&'static str>() {
                Some(message) => *message,
                None => panic!("unexpected panic payload for duplicate file path"),
            },
        };
        assert!(message.contains("duplicate file path"), "{message}");
    }

    #[test]
    fn removed_source_roots_do_not_leave_paths_behind() {
        let mut db = RootDatabase::new(None);
        let file_id = FileId::from_raw(0);
        let path = VfsPath::new_virtual_path("/foo.rs".to_owned());
        let mut file_set = FileSet::default();
        file_set.insert(file_id, path.clone());

        let mut change = FileChange::default();
        change.set_roots(vec![(SourceRootKind::Local, file_set)]);
        change.apply(&mut db);

        assert_eq!(db.file_path(file_id), Some(path.clone()));
        assert_eq!(db.file_id_for_path(&path), Some(file_id));

        let mut change = FileChange::default();
        change.set_roots(Vec::new());
        change.apply(&mut db);

        assert_eq!(db.file_path(file_id), None);
        assert_eq!(db.file_id_for_path(&path), None);
    }
}
