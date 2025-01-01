//! base_db defines basic database traits. The concrete DB is defined by ide.
// FIXME: Rename this crate, base db is non descriptive
mod change;
mod input;

pub use crate::{
    change::FileChange,
    input::{
        Crate, CrateBuilder, CrateBuilderId, CrateData, CrateDataBuilder, CrateDisplayName,
        CrateGraphBuilder, CrateName, CrateOrigin, CratesIdMap, CratesMap, Dependency,
        DependencyBuilder, Env, ExtraCrateData, LangCrateOrigin, ProcMacroPaths, ReleaseChannel,
        SourceRoot, SourceRootId, TargetLayoutLoadResult, UniqueCrateData,
    },
};
pub use db_ext_macro::{self};
use rustc_hash::FxHashSet;
use salsa::Durability;
pub use salsa::{self};
pub use semver::{BuildMetadata, Prerelease, Version, VersionReq};
use span::EditionedFileId;
use syntax::{ast, Parse, SyntaxError};
use triomphe::Arc;
pub use vfs::{file_set::FileSet, AnchoredPath, AnchoredPathBuf, VfsPath};
use vfs::{AbsPathBuf, FileId};

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl $crate::salsa::plumbing::AsId for $name {
            fn as_id(&self) -> $crate::salsa::Id {
                self.0
            }
        }

        impl $crate::salsa::plumbing::FromId for $name {
            fn from_id(id: salsa::Id) -> Self {
                $name(id)
            }
        }
    };
}

#[macro_export]
macro_rules! impl_wrapper {
    ($id:ident, $loc:ident, $intern:ident) => {
        #[salsa::interned_sans_lifetime(id = $id)]
        pub struct $intern {
            pub loc: $loc,
        }
    };
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

pub const DEFAULT_FILE_TEXT_LRU_CAP: u16 = 16;
pub const DEFAULT_PARSE_LRU_CAP: u16 = 128;
pub const DEFAULT_BORROWCK_LRU_CAP: u16 = 2024;

/// Crate related data shared by the whole workspace.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CrateWorkspaceData {
    /// The working directory to run proc-macros in. This is usually the workspace root of cargo workspaces.
    pub proc_macro_cwd: Option<AbsPathBuf>,
    // FIXME: Consider removing this, making HirDatabase::target_data_layout an input query
    pub data_layout: TargetLayoutLoadResult,
    /// Toolchain version used to compile the crate.
    pub toolchain: Option<Version>,
}

#[salsa::input]
pub struct FileText {
    pub text: Arc<str>,
}

#[salsa::input]
pub struct FileSourceRootInput {
    pub source_root_id: SourceRootId,
}

#[salsa::input]
pub struct SourceRootInput {
    pub source_root: Arc<SourceRoot>,
}

/// Database which stores all significant input facts: source code and project
/// model. Everything else in rust-analyzer is derived from these queries.
#[db_ext_macro::query_group]
pub trait RootQueryDb: SourceDatabase + salsa::Database {
    /// Parses the file into the syntax tree.
    #[db_ext_macro::lru]
    fn parse(&self, file_id: EditionedFileId) -> Parse<ast::SourceFile>;

    /// Returns the set of errors obtained from parsing the file including validation errors.
    fn parse_errors(&self, file_id: EditionedFileId) -> Option<Arc<[SyntaxError]>>;

    #[db_ext_macro::transparent]
    fn toolchain_channel(&self, krate: Crate) -> Option<ReleaseChannel>;

    /// Crates whose root file is in `id`.
    fn source_root_crates(&self, id: SourceRootId) -> Arc<[Crate]>;

    #[db_ext_macro::transparent]
    fn relevant_crates(&self, file_id: FileId) -> Arc<[Crate]>;

    /// Do not use this query in analysis! It kills incrementality across crate metadata modifications.
    ///
    /// Returns the crates in topological order.
    #[db_ext_macro::input]
    fn all_crates(&self) -> Arc<Box<[Crate]>>;

    /// Do not use this query in analysis! It kills incrementality across crate metadata modifications.
    ///
    /// Returns an iterator over all transitive dependencies of the given crate,
    /// including the crate itself.
    #[db_ext_macro::transparent]
    fn transitive_deps(&self, crate_id: Crate) -> FxHashSet<Crate>;

    /// Do not use this query in analysis! It kills incrementality across crate metadata modifications.
    ///
    /// Returns all transitive reverse dependencies of the given crate,
    /// including the crate itself.
    #[db_ext_macro::invoke(input::transitive_rev_deps)]
    #[db_ext_macro::transparent]
    fn transitive_rev_deps(&self, of: Crate) -> FxHashSet<Crate>;
}

pub fn transitive_deps(db: &dyn SourceDatabase, crate_id: Crate) -> FxHashSet<Crate> {
    // There is a bit of duplication here and in `CrateGraphBuilder` in the same method, but it's not terrible
    // and removing that is a bit difficult.
    let mut worklist = vec![crate_id];
    let mut deps = FxHashSet::default();

    while let Some(krate) = worklist.pop() {
        if !deps.insert(krate) {
            continue;
        }

        worklist.extend(krate.data(db).dependencies.iter().map(|dep| dep.crate_id));
    }

    deps
}

#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    /// Text of the file.
    fn file_text(&self, file_id: vfs::FileId) -> FileText;

    fn set_file_text(&mut self, file_id: vfs::FileId, text: &str);

    fn set_file_text_with_durability(
        &mut self,
        file_id: vfs::FileId,
        text: &str,
        durability: Durability,
    );

    /// Contents of the source root.
    fn source_root(&self, id: SourceRootId) -> SourceRootInput;

    fn file_source_root(&self, id: vfs::FileId) -> FileSourceRootInput;

    fn set_file_source_root_with_durability(
        &mut self,
        id: vfs::FileId,
        source_root_id: SourceRootId,
        durability: Durability,
    );

    /// Source root of the file.
    fn set_source_root_with_durability(
        &mut self,
        source_root_id: SourceRootId,
        source_root: Arc<SourceRoot>,
        durability: Durability,
    );

    fn resolve_path(&self, path: AnchoredPath<'_>) -> Option<FileId> {
        // FIXME: this *somehow* should be platform agnostic...
        let source_root = self.file_source_root(path.anchor);
        let source_root = self.source_root(source_root.source_root_id(self));
        source_root.source_root(self).resolve_path(path)
    }

    #[doc(hidden)]
    fn crates_map(&self) -> Arc<CratesMap>;
}

fn toolchain_channel(db: &dyn RootQueryDb, krate: Crate) -> Option<ReleaseChannel> {
    krate.workspace_data(db).toolchain.as_ref().and_then(|v| ReleaseChannel::from_str(&v.pre))
}

fn parse(db: &dyn RootQueryDb, file_id: EditionedFileId) -> Parse<ast::SourceFile> {
    let _p = tracing::info_span!("parse", ?file_id).entered();
    let (file_id, edition) = file_id.unpack();
    let text = db.file_text(file_id);
    ast::SourceFile::parse(&text.text(db), edition)
}

fn parse_errors(db: &dyn RootQueryDb, file_id: EditionedFileId) -> Option<Arc<[SyntaxError]>> {
    let errors = db.parse(file_id).errors();
    match &*errors {
        [] => None,
        [..] => Some(errors.into()),
    }
}

fn source_root_crates(db: &dyn RootQueryDb, id: SourceRootId) -> Arc<[Crate]> {
    let crates = db.all_crates();
    crates
        .iter()
        .copied()
        .filter(|&krate| {
            let root_file = krate.data(db).root_file_id;
            db.file_source_root(root_file).source_root_id(db) == id
        })
        .collect()
}

fn relevant_crates(db: &dyn RootQueryDb, file_id: FileId) -> Arc<[Crate]> {
    let _p = tracing::info_span!("relevant_crates").entered();

    let source_root = db.file_source_root(file_id);
    db.source_root_crates(source_root.source_root_id(db))
}
