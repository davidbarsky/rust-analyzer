//! Database used for testing `hir`.

use std::{fmt, panic, sync::Mutex};

use base_db::{
    CrateGraphBuilder, CratesMap, Nonce, SourceDatabase, all_crates, relevant_crates,
    set_all_crates_with_durability,
};

use hir_def::{ModuleId, nameres::crate_def_map};
use hir_expand::EditionedFileId;
use rustc_hash::FxHashMap;
use salsa::Durability;
use span::FileId;
use syntax::TextRange;
use test_utils::extract_annotations;
use triomphe::Arc;

#[salsa_macros::db]
pub(crate) struct TestDB {
    storage: salsa::Storage<Self>,
    crates_map: Arc<CratesMap>,
    events: Arc<Mutex<Option<Vec<salsa::Event>>>>,
    nonce: Nonce,
}

impl Default for TestDB {
    fn default() -> Self {
        let events = <Arc<Mutex<Option<Vec<salsa::Event>>>>>::default();
        let mut this = Self {
            storage: salsa::Storage::new(Some(Box::new({
                let events = events.clone();
                move |event| {
                    let mut events = events.lock().unwrap();
                    if let Some(events) = &mut *events {
                        events.push(event);
                    }
                }
            }))),
            events,
            crates_map: Default::default(),
            nonce: Nonce::new(),
        };
        hir_def::set_expand_proc_attr_macros(&mut this, true);
        // This needs to be here otherwise `CrateGraphBuilder` panics.
        set_all_crates_with_durability(&mut this, std::iter::empty(), Durability::HIGH);
        _ = base_db::FileTextTable::builder(Default::default())
            .durability(Durability::LOW)
            .new(&this);
        _ = base_db::SourceRootTable::builder(Default::default())
            .durability(Durability::LOW)
            .new(&this);
        CrateGraphBuilder::default().set_in_db(&mut this);
        this
    }
}

impl Clone for TestDB {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            crates_map: self.crates_map.clone(),
            events: self.events.clone(),
            nonce: Nonce::new(),
        }
    }
}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}

#[salsa_macros::db]
impl SourceDatabase for TestDB {
    fn crates_map(&self) -> Arc<CratesMap> {
        self.crates_map.clone()
    }

    fn nonce_and_revision(&self) -> (Nonce, salsa::Revision) {
        (self.nonce, salsa::plumbing::ZalsaDatabase::zalsa(self).current_revision())
    }

    fn line_column(&self, _file: FileId, _offset: syntax::TextSize) -> Result<(u32, u32), ()> {
        Err(())
    }
}

#[salsa_macros::db]
impl salsa::Database for TestDB {}

impl panic::RefUnwindSafe for TestDB {}

impl TestDB {
    pub(crate) fn module_for_file_opt(&self, file_id: impl Into<FileId>) -> Option<ModuleId> {
        let file_id = file_id.into();
        for &krate in relevant_crates(self, file_id).iter() {
            let crate_def_map = crate_def_map(self, krate);
            for (module_id, data) in crate_def_map.modules() {
                if data.origin.file_id().map(|file_id| file_id.file_id(self)) == Some(file_id) {
                    return Some(module_id);
                }
            }
        }
        None
    }

    pub(crate) fn module_for_file(&self, file_id: impl Into<FileId>) -> ModuleId {
        self.module_for_file_opt(file_id.into()).unwrap()
    }

    pub(crate) fn extract_annotations(
        &self,
    ) -> FxHashMap<EditionedFileId, Vec<(TextRange, String)>> {
        let mut files = Vec::new();
        for &krate in all_crates(self).iter() {
            let crate_def_map = crate_def_map(self, krate);
            for (module_id, _) in crate_def_map.modules() {
                let file_id = crate_def_map[module_id].origin.file_id();
                files.extend(file_id)
            }
        }
        files
            .into_iter()
            .filter_map(|file_id| {
                let text = self.file_text(file_id.file_id(self));
                let annotations = extract_annotations(text.text(self));
                if annotations.is_empty() {
                    return None;
                }
                Some((file_id, annotations))
            })
            .collect()
    }
}

impl TestDB {
    pub(crate) fn log(&self, f: impl FnOnce()) -> Vec<salsa::Event> {
        *self.events.lock().unwrap() = Some(Vec::new());
        f();
        self.events.lock().unwrap().take().unwrap()
    }

    pub(crate) fn log_executed(&self, f: impl FnOnce()) -> (Vec<String>, Vec<salsa::Event>) {
        let events = self.log(f);
        let executed = events
            .iter()
            .filter_map(|e| match e.kind {
                // This is pretty horrible, but `Debug` is the only way to inspect
                // QueryDescriptor at the moment.
                salsa::EventKind::WillExecute { database_key } => {
                    let ingredient = (self as &dyn salsa::Database)
                        .ingredient_debug_name(database_key.ingredient_index());
                    Some(ingredient.to_string())
                }
                _ => None,
            })
            .collect();
        (executed, events)
    }
}
