//! Defines a unit of change that can applied to the database to get the next
//! state. Changes are transactional.

use std::fmt;

use rustc_hash::FxHashMap;
use salsa::{Durability, Setter as _};
use triomphe::Arc;
use vfs::{FileId, file_set::FileSet};

use crate::{
    CrateGraphBuilder, CratesIdMap, FileTextRegistration, LineEndings, SourceDatabase, SourceRoot,
    SourceRootId, SourceRootKind, SourceRootTable, register_file_texts,
};

/// Encapsulate a bunch of raw `.set` calls on the database.
#[derive(Default)]
pub struct FileChange {
    pub roots: Option<Vec<(SourceRootKind, FileSet)>>,
    pub files_changed: Vec<(FileId, Option<String>)>,
    pub crate_graph: Option<CrateGraphBuilder>,
}

impl fmt::Debug for FileChange {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = fmt.debug_struct("Change");
        if let Some(roots) = &self.roots {
            d.field("roots", roots);
        }
        if !self.files_changed.is_empty() {
            d.field("files_changed", &self.files_changed.len());
        }
        if self.crate_graph.is_some() {
            d.field("crate_graph", &self.crate_graph);
        }
        d.finish()
    }
}

impl FileChange {
    pub fn set_roots(&mut self, roots: Vec<(SourceRootKind, FileSet)>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file_id: FileId, new_text: Option<String>) {
        self.files_changed.push((file_id, new_text))
    }

    pub fn set_crate_graph(&mut self, graph: CrateGraphBuilder) {
        self.crate_graph = Some(graph);
    }

    pub fn apply(self, db: &mut dyn SourceDatabase) -> Option<CratesIdMap> {
        let _p = tracing::info_span!("FileChange::apply").entered();
        let FileChange { roots, files_changed, crate_graph } = self;
        let mut file_durability = FxHashMap::default();
        if let Some(roots) = roots {
            let mut file_texts = Vec::new();
            let mut paths = FxHashMap::default();
            let mut files = FxHashMap::default();
            let source_root_table = SourceRootTable::get(db);
            let roots_by_id = source_root_table.roots(db).clone();
            let mut active_source_roots = FxHashMap::default();
            for (idx, (kind, file_set)) in roots.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);
                let durability = source_root_durability(kind);
                let file_set = Arc::new(file_set);
                for file_id in file_set.iter() {
                    file_texts.push(FileTextRegistration { file_id, durability });
                    file_durability.insert(file_id, file_text_durability_for_kind(kind));
                    let path =
                        file_set.path_for_file(&file_id).expect("source root file has no path");
                    match files.insert(path.clone(), file_id) {
                        None => (),
                        Some(previous) if previous == file_id => (),
                        Some(previous) => {
                            panic!(
                                "duplicate file path `{path}` for file ids {previous:?} and {file_id:?}"
                            )
                        }
                    }
                    match paths.insert(file_id, path.clone()) {
                        None => (),
                        Some(previous) if previous == *path => (),
                        Some(previous) => {
                            panic!(
                                "file id {file_id:?} has multiple file paths `{previous}` and `{path}`"
                            )
                        }
                    }
                }

                match roots_by_id.get(&root_id).copied() {
                    Some(source_root) => {
                        source_root.set_kind(db).with_durability(durability).to(kind);
                        source_root.set_file_set(db).with_durability(durability).to(file_set);
                        active_source_roots.insert(root_id, source_root);
                    }
                    None => {
                        let source_root =
                            SourceRoot::builder(kind, file_set).durability(durability).new(db);
                        active_source_roots.insert(root_id, source_root);
                    }
                }
            }
            source_root_table.set_roots(db).to(active_source_roots);
            register_file_texts(db, file_texts);
        }

        for (file_id, text) in files_changed {
            let durability = file_durability.get(&file_id).copied().unwrap_or_else(|| {
                let source_root_id = db.file_source_root(file_id);
                let source_root = db.source_root(source_root_id);
                file_text_durability_for_kind(source_root.kind(db))
            });
            let (text, line_endings) = match text {
                Some(text) => LineEndings::normalize(text),
                None => (String::new(), LineEndings::Unix),
            };
            db.set_file_text_with_line_endings_and_durability(
                file_id,
                &text,
                line_endings,
                durability,
            )
        }

        if let Some(crate_graph) = crate_graph {
            return Some(crate_graph.set_in_db(db));
        }
        None
    }
}

fn source_root_durability(kind: SourceRootKind) -> Durability {
    match kind {
        SourceRootKind::Local => Durability::LOW,
        SourceRootKind::Library => Durability::MEDIUM,
    }
}

fn file_text_durability_for_kind(kind: SourceRootKind) -> Durability {
    match kind {
        SourceRootKind::Local => Durability::LOW,
        SourceRootKind::Library => Durability::HIGH,
    }
}
