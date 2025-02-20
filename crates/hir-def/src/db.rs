//! Defines database & queries for name resolution.
use base_db::{impl_wrapper, CrateId, RootQueryDb, SourceDatabase, Upcast};
use either::Either;
use hir_expand::{db::ExpandDatabase, HirFileId, MacroDefId};
use intern::sym;
use la_arena::ArenaMap;
use span::{EditionedFileId, MacroCallId};
use syntax::{ast, AstPtr};
use triomphe::Arc;

use crate::{
    attr::{Attrs, AttrsWithOwner},
    data::{
        adt::{EnumData, EnumVariantData, StructData, VariantData},
        ConstData, ExternCrateDeclData, FunctionData, ImplData, Macro2Data, MacroRulesData,
        ProcMacroData, StaticData, TraitAliasData, TraitData, TypeAliasData,
    },
    expr_store::{scope::ExprScopes, Body, BodySourceMap},
    generics::GenericParams,
    import_map::ImportMap,
    item_tree::{AttrOwner, ItemTree, ItemTreeSourceMaps},
    lang_item::{self, LangItem, LangItemTarget, LangItems},
    nameres::{diagnostics::DefDiagnostics, DefMap},
    tt,
    type_ref::TypesSourceMap,
    visibility::{self, Visibility},
    AttrDefId, BlockId, BlockLoc, ConstBlockId, ConstBlockLoc, ConstId, ConstLoc, DefWithBodyId,
    EnumId, EnumLoc, EnumVariantId, EnumVariantLoc, ExternBlockId, ExternBlockLoc, ExternCrateId,
    ExternCrateLoc, FunctionId, FunctionLoc, GenericDefId, ImplId, ImplLoc, InTypeConstId,
    InTypeConstLoc, LocalFieldId, Macro2Id, Macro2Loc, MacroId, MacroRulesId, MacroRulesLoc,
    MacroRulesLocFlags, ProcMacroId, ProcMacroLoc, StaticId, StaticLoc, StructId, StructLoc,
    TraitAliasId, TraitAliasLoc, TraitId, TraitLoc, TypeAliasId, TypeAliasLoc, UnionId, UnionLoc,
    UseId, UseLoc, VariantId,
};

use salsa::plumbing::AsId;

impl_wrapper!(UseId, UseLoc, UseLocWrapper);
impl_wrapper!(ExternCrateId, ExternCrateLoc, ExternCrateLocWrapper);
impl_wrapper!(FunctionId, FunctionLoc, FunctionLocWrapper);
impl_wrapper!(StructId, StructLoc, StructLocWrapper);
impl_wrapper!(UnionId, UnionLoc, UnionLocWrapper);
impl_wrapper!(EnumId, EnumLoc, EnumLocWrapper);
impl_wrapper!(EnumVariantId, EnumVariantLoc, EnumVariantWrapper);
impl_wrapper!(ConstId, ConstLoc, ConstWrapper);
impl_wrapper!(StaticId, StaticLoc, StaticWrapper);
impl_wrapper!(TraitId, TraitLoc, TraitWrapper);
impl_wrapper!(TraitAliasId, TraitAliasLoc, TraitAliasWrapper);
impl_wrapper!(TypeAliasId, TypeAliasLoc, TypeAliasWrapper);
impl_wrapper!(ImplId, ImplLoc, ImplWrapper);
impl_wrapper!(ExternBlockId, ExternBlockLoc, ExternBlockWrapper);
impl_wrapper!(Macro2Id, Macro2Loc, Macro2Wrapper);
impl_wrapper!(ProcMacroId, ProcMacroLoc, ProcMacroWrapper);
impl_wrapper!(MacroRulesId, MacroRulesLoc, MacroRulesWrapper);
impl_wrapper!(BlockId, BlockLoc, BlockWrapper);
impl_wrapper!(ConstBlockId, ConstBlockLoc, AnonymusConstWrapper);
impl_wrapper!(InTypeConstId, InTypeConstLoc, InTypeConstWrapper);

#[db_ext_macro::query_group(InternDatabaseStorage)]
pub trait InternDatabase: RootQueryDb {
    // region: items
    #[db_ext_macro::interned(UseLocWrapper)]
    fn intern_use(&self, loc: UseLoc) -> UseId;

    #[db_ext_macro::interned(ExternCrateLocWrapper)]
    fn intern_extern_crate(&self, loc: ExternCrateLoc) -> ExternCrateId;

    #[db_ext_macro::interned(FunctionLocWrapper)]
    fn intern_function(&self, loc: FunctionLoc) -> FunctionId;

    #[db_ext_macro::interned(StructLocWrapper)]
    fn intern_struct(&self, loc: StructLoc) -> StructId;

    #[db_ext_macro::interned(UnionLocWrapper)]
    fn intern_union(&self, loc: UnionLoc) -> UnionId;

    #[db_ext_macro::interned(EnumLocWrapper)]
    fn intern_enum(&self, loc: EnumLoc) -> EnumId;

    #[db_ext_macro::interned(EnumVariantWrapper)]
    fn intern_enum_variant(&self, loc: EnumVariantLoc) -> EnumVariantId;

    #[db_ext_macro::interned(ConstWrapper)]
    fn intern_const(&self, loc: ConstLoc) -> ConstId;

    #[db_ext_macro::interned(StaticWrapper)]
    fn intern_static(&self, loc: StaticLoc) -> StaticId;

    #[db_ext_macro::interned(TraitWrapper)]
    fn intern_trait(&self, loc: TraitLoc) -> TraitId;

    #[db_ext_macro::interned(TraitAliasWrapper)]
    fn intern_trait_alias(&self, loc: TraitAliasLoc) -> TraitAliasId;

    #[db_ext_macro::interned(TypeAliasWrapper)]
    fn intern_type_alias(&self, loc: TypeAliasLoc) -> TypeAliasId;

    #[db_ext_macro::interned(ImplWrapper)]
    fn intern_impl(&self, loc: ImplLoc) -> ImplId;

    #[db_ext_macro::interned(ExternBlockWrapper)]
    fn intern_extern_block(&self, loc: ExternBlockLoc) -> ExternBlockId;

    #[db_ext_macro::interned(Macro2Wrapper)]
    fn intern_macro2(&self, loc: Macro2Loc) -> Macro2Id;

    #[db_ext_macro::interned(ProcMacroWrapper)]
    fn intern_proc_macro(&self, loc: ProcMacroLoc) -> ProcMacroId;

    #[db_ext_macro::interned(MacroRulesWrapper)]
    fn intern_macro_rules(&self, loc: MacroRulesLoc) -> MacroRulesId;
    // // endregion: items

    #[db_ext_macro::interned(BlockWrapper)]
    fn intern_block(&self, loc: BlockLoc) -> BlockId;
    #[db_ext_macro::interned(AnonymusConstWrapper)]
    fn intern_anonymous_const(&self, id: ConstBlockLoc) -> ConstBlockId;

    #[db_ext_macro::interned(InTypeConstWrapper)]
    fn intern_in_type_const(&self, id: InTypeConstLoc) -> InTypeConstId;
}

#[db_ext_macro::query_group]
pub trait DefDatabase:
    InternDatabase
    + ExpandDatabase
    + SourceDatabase
    + Upcast<dyn ExpandDatabase>
    + Upcast<dyn RootQueryDb>
{
    /// Whether to expand procedural macros during name resolution.
    #[db_ext_macro::input]
    fn expand_proc_attr_macros(&self) -> bool;

    /// Computes an [`ItemTree`] for the given file or macro expansion.
    #[db_ext_macro::invoke(ItemTree::file_item_tree_query)]
    fn file_item_tree(&self, file_id: HirFileId) -> Arc<ItemTree>;

    #[db_ext_macro::invoke(ItemTree::block_item_tree_query)]
    fn block_item_tree(&self, block_id: BlockId) -> Arc<ItemTree>;

    #[db_ext_macro::invoke(ItemTree::file_item_tree_with_source_map_query)]
    fn file_item_tree_with_source_map(
        &self,
        file_id: HirFileId,
    ) -> (Arc<ItemTree>, Arc<ItemTreeSourceMaps>);

    #[db_ext_macro::invoke(ItemTree::block_item_tree_with_source_map_query)]
    fn block_item_tree_with_source_map(
        &self,
        block_id: BlockId,
    ) -> (Arc<ItemTree>, Arc<ItemTreeSourceMaps>);

    #[db_ext_macro::invoke(DefMap::crate_def_map_query)]
    fn crate_def_map(&self, krate: CrateId) -> Arc<DefMap>;

    /// Computes the block-level `DefMap`.
    #[db_ext_macro::invoke(DefMap::block_def_map_query)]
    fn block_def_map(&self, block: BlockId) -> Arc<DefMap>;

    /// Turns a MacroId into a MacroDefId, describing the macro's definition post name resolution.
    fn macro_def(&self, m: MacroId) -> MacroDefId;

    // region:data

    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(StructData::struct_data_query)]
    fn struct_data(&self, id: StructId) -> Arc<StructData>;

    #[db_ext_macro::invoke(StructData::struct_data_with_diagnostics_query)]
    fn struct_data_with_diagnostics(&self, id: StructId) -> (Arc<StructData>, DefDiagnostics);

    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(StructData::union_data_query)]
    fn union_data(&self, id: UnionId) -> Arc<StructData>;

    #[db_ext_macro::invoke(StructData::union_data_with_diagnostics_query)]
    fn union_data_with_diagnostics(&self, id: UnionId) -> (Arc<StructData>, DefDiagnostics);

    #[db_ext_macro::invoke(EnumData::enum_data_query)]
    fn enum_data(&self, e: EnumId) -> Arc<EnumData>;

    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(EnumVariantData::enum_variant_data_query)]
    fn enum_variant_data(&self, id: EnumVariantId) -> Arc<EnumVariantData>;

    #[db_ext_macro::invoke(EnumVariantData::enum_variant_data_with_diagnostics_query)]
    fn enum_variant_data_with_diagnostics(
        &self,
        id: EnumVariantId,
    ) -> (Arc<EnumVariantData>, DefDiagnostics);

    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(VariantData::variant_data)]
    fn variant_data(&self, id: VariantId) -> Arc<VariantData>;
    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(ImplData::impl_data_query)]
    fn impl_data(&self, e: ImplId) -> Arc<ImplData>;

    #[db_ext_macro::invoke(ImplData::impl_data_with_diagnostics_query)]
    fn impl_data_with_diagnostics(&self, e: ImplId) -> (Arc<ImplData>, DefDiagnostics);

    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(TraitData::trait_data_query)]
    fn trait_data(&self, e: TraitId) -> Arc<TraitData>;

    #[db_ext_macro::invoke(TraitData::trait_data_with_diagnostics_query)]
    fn trait_data_with_diagnostics(&self, tr: TraitId) -> (Arc<TraitData>, DefDiagnostics);

    #[db_ext_macro::invoke(TraitAliasData::trait_alias_query)]
    fn trait_alias_data(&self, e: TraitAliasId) -> Arc<TraitAliasData>;

    #[db_ext_macro::invoke(TypeAliasData::type_alias_data_query)]
    fn type_alias_data(&self, e: TypeAliasId) -> Arc<TypeAliasData>;

    #[db_ext_macro::invoke(FunctionData::fn_data_query)]
    fn function_data(&self, func: FunctionId) -> Arc<FunctionData>;

    #[db_ext_macro::invoke(ConstData::const_data_query)]
    fn const_data(&self, konst: ConstId) -> Arc<ConstData>;

    #[db_ext_macro::invoke(StaticData::static_data_query)]
    fn static_data(&self, statik: StaticId) -> Arc<StaticData>;

    #[db_ext_macro::invoke(Macro2Data::macro2_data_query)]
    fn macro2_data(&self, makro: Macro2Id) -> Arc<Macro2Data>;

    #[db_ext_macro::invoke(MacroRulesData::macro_rules_data_query)]
    fn macro_rules_data(&self, makro: MacroRulesId) -> Arc<MacroRulesData>;

    #[db_ext_macro::invoke(ProcMacroData::proc_macro_data_query)]
    fn proc_macro_data(&self, makro: ProcMacroId) -> Arc<ProcMacroData>;

    #[db_ext_macro::invoke(ExternCrateDeclData::extern_crate_decl_data_query)]
    fn extern_crate_decl_data(&self, extern_crate: ExternCrateId) -> Arc<ExternCrateDeclData>;

    // endregion:data

    #[db_ext_macro::invoke(Body::body_with_source_map_query)]
    #[db_ext_macro::lru(2048)]
    fn body_with_source_map(&self, def: DefWithBodyId) -> (Arc<Body>, Arc<BodySourceMap>);
    #[db_ext_macro::invoke(Body::body_query)]
    fn body(&self, def: DefWithBodyId) -> Arc<Body>;

    #[db_ext_macro::invoke(ExprScopes::expr_scopes_query)]
    fn expr_scopes(&self, def: DefWithBodyId) -> Arc<ExprScopes>;

    #[db_ext_macro::invoke(GenericParams::generic_params_query)]
    fn generic_params(&self, def: GenericDefId) -> Arc<GenericParams>;

    /// If this returns `None` for the source map, that means it is the same as with the item tree.
    #[db_ext_macro::invoke(GenericParams::generic_params_with_source_map_query)]
    fn generic_params_with_source_map(
        &self,
        def: GenericDefId,
    ) -> (Arc<GenericParams>, Option<Arc<TypesSourceMap>>);

    // region:attrs

    #[db_ext_macro::invoke(Attrs::fields_attrs_query)]
    fn fields_attrs(&self, def: VariantId) -> Arc<ArenaMap<LocalFieldId, Attrs>>;

    // should this really be a query?
    #[db_ext_macro::invoke(crate::attr::fields_attrs_source_map)]
    fn fields_attrs_source_map(
        &self,
        def: VariantId,
    ) -> Arc<ArenaMap<LocalFieldId, AstPtr<Either<ast::TupleField, ast::RecordField>>>>;

    #[db_ext_macro::invoke(AttrsWithOwner::attrs_query)]
    fn attrs(&self, def: AttrDefId) -> Attrs;

    #[db_ext_macro::transparent]
    #[db_ext_macro::invoke(lang_item::lang_attr)]
    fn lang_attr(&self, def: AttrDefId) -> Option<LangItem>;

    // endregion:attrs

    #[db_ext_macro::invoke(LangItems::lang_item_query)]
    fn lang_item(&self, start_crate: CrateId, item: LangItem) -> Option<LangItemTarget>;

    #[db_ext_macro::invoke(ImportMap::import_map_query)]
    fn import_map(&self, krate: CrateId) -> Arc<ImportMap>;

    // region:visibilities

    #[db_ext_macro::invoke(visibility::field_visibilities_query)]
    fn field_visibilities(&self, var: VariantId) -> Arc<ArenaMap<LocalFieldId, Visibility>>;

    // FIXME: unify function_visibility and const_visibility?
    #[db_ext_macro::invoke(visibility::function_visibility_query)]
    fn function_visibility(&self, def: FunctionId) -> Visibility;

    #[db_ext_macro::invoke(visibility::const_visibility_query)]
    fn const_visibility(&self, def: ConstId) -> Visibility;

    // endregion:visibilities

    #[db_ext_macro::invoke(LangItems::crate_lang_items_query)]
    fn crate_lang_items(&self, krate: CrateId) -> Option<Arc<LangItems>>;

    #[db_ext_macro::invoke(crate::lang_item::notable_traits_in_deps)]
    fn notable_traits_in_deps(&self, krate: CrateId) -> Arc<[Arc<[TraitId]>]>;
    #[db_ext_macro::invoke(crate::lang_item::crate_notable_traits)]
    fn crate_notable_traits(&self, krate: CrateId) -> Option<Arc<[TraitId]>>;

    fn crate_supports_no_std(&self, crate_id: CrateId) -> bool;

    fn include_macro_invoc(&self, crate_id: CrateId) -> Arc<[(MacroCallId, EditionedFileId)]>;
}

// return: macro call id and include file id
fn include_macro_invoc(
    db: &dyn DefDatabase,
    krate: CrateId,
) -> Arc<[(MacroCallId, EditionedFileId)]> {
    db.crate_def_map(krate)
        .modules
        .values()
        .flat_map(|m| m.scope.iter_macro_invoc())
        .filter_map(|invoc| {
            db.lookup_intern_macro_call(*invoc.1)
                .include_file_id(db.upcast(), *invoc.1)
                .map(|x| (*invoc.1, x))
        })
        .collect()
}

fn crate_supports_no_std(db: &dyn DefDatabase, crate_id: CrateId) -> bool {
    let file = db.crate_graph()[crate_id].root_file_id();
    let item_tree = db.file_item_tree(file.into());
    let attrs = item_tree.raw_attrs(AttrOwner::TopLevel);
    for attr in &**attrs {
        match attr.path().as_ident() {
            Some(ident) if *ident == sym::no_std.clone() => return true,
            Some(ident) if *ident == sym::cfg_attr.clone() => {}
            _ => continue,
        }

        // This is a `cfg_attr`; check if it could possibly expand to `no_std`.
        // Syntax is: `#[cfg_attr(condition(cfg, style), attr0, attr1, <...>)]`
        let tt = match attr.token_tree_value() {
            Some(tt) => tt.token_trees(),
            None => continue,
        };

        let segments =
            tt.split(|tt| matches!(tt, tt::TtElement::Leaf(tt::Leaf::Punct(p)) if p.char == ','));
        for output in segments.skip(1) {
            match output.flat_tokens() {
                [tt::TokenTree::Leaf(tt::Leaf::Ident(ident))] if ident.sym == sym::no_std => {
                    return true
                }
                _ => {}
            }
        }
    }

    false
}

fn macro_def(db: &dyn DefDatabase, id: MacroId) -> MacroDefId {
    use hir_expand::InFile;

    use crate::{Lookup, MacroDefKind, MacroExpander};

    let kind = |expander, file_id, m| {
        let in_file = InFile::new(file_id, m);
        match expander {
            MacroExpander::Declarative => MacroDefKind::Declarative(in_file),
            MacroExpander::BuiltIn(it) => MacroDefKind::BuiltIn(in_file, it),
            MacroExpander::BuiltInAttr(it) => MacroDefKind::BuiltInAttr(in_file, it),
            MacroExpander::BuiltInDerive(it) => MacroDefKind::BuiltInDerive(in_file, it),
            MacroExpander::BuiltInEager(it) => MacroDefKind::BuiltInEager(in_file, it),
        }
    };

    match id {
        MacroId::Macro2Id(it) => {
            let loc: Macro2Loc = it.lookup(db);

            let item_tree = loc.id.item_tree(db);
            let makro = &item_tree[loc.id.value];
            MacroDefId {
                krate: loc.container.krate,
                kind: kind(loc.expander, loc.id.file_id(), makro.ast_id.upcast()),
                local_inner: false,
                allow_internal_unsafe: loc.allow_internal_unsafe,
                edition: loc.edition,
            }
        }
        MacroId::MacroRulesId(it) => {
            let loc: MacroRulesLoc = it.lookup(db);

            let item_tree = loc.id.item_tree(db);
            let makro = &item_tree[loc.id.value];
            MacroDefId {
                krate: loc.container.krate,
                kind: kind(loc.expander, loc.id.file_id(), makro.ast_id.upcast()),
                local_inner: loc.flags.contains(MacroRulesLocFlags::LOCAL_INNER),
                allow_internal_unsafe: loc
                    .flags
                    .contains(MacroRulesLocFlags::ALLOW_INTERNAL_UNSAFE),
                edition: loc.edition,
            }
        }
        MacroId::ProcMacroId(it) => {
            let loc = it.lookup(db);

            let item_tree = loc.id.item_tree(db);
            let makro = &item_tree[loc.id.value];
            MacroDefId {
                krate: loc.container.krate,
                kind: MacroDefKind::ProcMacro(
                    InFile::new(loc.id.file_id(), makro.ast_id),
                    loc.expander,
                    loc.kind,
                ),
                local_inner: false,
                allow_internal_unsafe: false,
                edition: loc.edition,
            }
        }
    }
}
