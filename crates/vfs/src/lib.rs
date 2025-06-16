//! # Virtual File System
//!
//! VFS records all file changes pushed to it via [`set_file_contents`].
//! As such it only ever stores changes, not the actual content of a file at any given moment.
//! All file changes are logged, and can be retrieved via
//! [`take_changes`] method. The pack of changes is then pushed to `salsa` and
//! triggers incremental recomputation.
//!
//! Files in VFS are identified with [`FileId`]s -- interned paths. The notion of
//! the path, [`VfsPath`] is somewhat abstract: at the moment, it is represented
//! as an [`std::path::PathBuf`] internally, but this is an implementation detail.
//!
//! VFS doesn't do IO or file watching itself. For that, see the [`loader`]
//! module. [`loader::Handle`] is an object-safe trait which abstracts both file
//! loading and file watching. [`Handle`] is dynamically configured with a set of
//! directory entries which should be scanned and watched. [`Handle`] then
//! asynchronously pushes file changes. Directory entries are configured in
//! free-form via list of globs, it's up to the [`Handle`] to interpret the globs
//! in any specific way.
//!
//! VFS stores a flat list of files. [`file_set::FileSet`] can partition this list
//! of files into disjoint sets of files. Traversal-like operations (including
//! getting the neighbor file by the relative path) are handled by the [`FileSet`].
//! [`FileSet`]s are also pushed to salsa and cause it to re-check `mod foo;`
//! declarations when files are created or deleted.
//!
//! [`FileSet`] and [`loader::Entry`] play similar, but different roles.
//! Both specify the "set of paths/files", one is geared towards file watching,
//! the other towards salsa changes. In particular, single [`FileSet`]
//! may correspond to several [`loader::Entry`]. For example, a crate from
//! crates.io which uses code generation would have two [`Entries`] -- for sources
//! in `~/.cargo`, and for generated code in `./target/debug/build`. It will
//! have a single [`FileSet`] which unions the two sources.
//!
//! [`set_file_contents`]: Vfs::set_file_contents
//! [`take_changes`]: Vfs::take_changes
//! [`FileSet`]: file_set::FileSet
//! [`Handle`]: loader::Handle
//! [`Entries`]: loader::Entry

mod anchored_path;
pub mod file_set;
pub mod loader;
mod vfs_path;

use std::{fmt, hash::BuildHasherDefault, mem};

pub use crate::{
    anchored_path::{AnchoredPath, AnchoredPathBuf},
    vfs_path::VfsPath,
};
use indexmap::{IndexMap, map::Entry};
pub use paths::{AbsPath, AbsPathBuf};

use rustc_hash::FxHasher;
use stdx::hash_once;
use tracing::{Level, span};

/// A file path input for the Salsa database system.
#[salsa_macros::input(debug)]
pub struct File {
    pub path: VfsPath,
}

impl File {
    pub const fn index(&self) -> u32 {
        self.0.as_u32()
    }
}

/// Storage for all file changes and the file id to path mapping.
///
/// For more information see the [crate-level](crate) documentation.
#[derive(Default)]
pub struct Vfs {
    data: IndexMap<File, FileState, BuildHasherDefault<FxHasher>>,
    changes: IndexMap<File, ChangedFile, BuildHasherDefault<FxHasher>>,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum FileState {
    /// The file exists with the given content hash.
    Exists(u64),
    /// The file is deleted.
    Deleted,
    /// The file was specifically excluded by the user. We still include excluded files
    /// when they're opened (without their contents).
    Excluded,
}

/// Changed file in the [`Vfs`].
#[derive(Debug)]
pub struct ChangedFile {
    /// Path of the changed file
    pub file: File,
    /// Kind of change
    pub change: Change,
}

impl ChangedFile {
    /// Returns `true` if the change is not [`Delete`](ChangeKind::Delete).
    pub fn exists(&self) -> bool {
        !matches!(self.change, Change::Delete)
    }

    /// Returns `true` if the change is [`Create`](ChangeKind::Create) or
    /// [`Delete`](Change::Delete).
    pub fn is_created_or_deleted(&self) -> bool {
        matches!(self.change, Change::Create(_, _) | Change::Delete)
    }

    /// Returns `true` if the change is [`Create`](ChangeKind::Create).
    pub fn is_created(&self) -> bool {
        matches!(self.change, Change::Create(_, _))
    }

    /// Returns `true` if the change is [`Modify`](ChangeKind::Modify).
    pub fn is_modified(&self) -> bool {
        matches!(self.change, Change::Modify(_, _))
    }

    pub fn kind(&self) -> ChangeKind {
        match self.change {
            Change::Create(_, _) => ChangeKind::Create,
            Change::Modify(_, _) => ChangeKind::Modify,
            Change::Delete => ChangeKind::Delete,
        }
    }
}

/// Kind of [file change](ChangedFile).
#[derive(Eq, PartialEq, Debug)]
pub enum Change {
    /// The file was (re-)created
    Create(Vec<u8>, u64),
    /// The file was modified
    Modify(Vec<u8>, u64),
    /// The file was deleted
    Delete,
}

/// Kind of [file change](ChangedFile).
#[derive(Eq, PartialEq, Debug)]
pub enum ChangeKind {
    /// The file was (re-)created
    Create,
    /// The file was modified
    Modify,
    /// The file was deleted
    Delete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileExcluded {
    Yes,
    No,
}

impl Vfs {
    /// Check if the given path exists in the `Vfs` and is not deleted.
    pub fn file_exists(&self, file_path: &File) -> Option<FileExcluded> {
        let file_state = self.get(file_path)?;
        match file_state {
            FileState::Exists(_) => Some(FileExcluded::No),
            FileState::Deleted => None,
            FileState::Excluded => Some(FileExcluded::Yes),
        }
    }

    /// Returns an iterator over the stored file paths.
    ///
    /// This will skip deleted files.
    pub fn iter(&self) -> impl Iterator<Item = &File> + '_ {
        self.data
            .iter()
            .filter(|&(_, file_state)| matches!(file_state, FileState::Exists(_)))
            .map(|(file_path, _)| file_path)
    }

    /// Update the file with the given `contents`. `None` means the file was deleted.
    ///
    /// Returns `true` if the file was modified, and saves the [change](ChangedFile).
    pub fn set_file_contents(&mut self, file_path: File, contents: Option<Vec<u8>>) -> bool {
        let _p = span!(Level::INFO, "Vfs::set_file_contents").entered();
        let state: FileState = self.get(&file_path).unwrap_or(FileState::Deleted);
        let change = match (state, contents) {
            (FileState::Deleted, None) => return false,
            (FileState::Deleted, Some(v)) => {
                let hash = hash_once::<FxHasher>(&*v);
                Change::Create(v, hash)
            }
            (FileState::Exists(_), None) => Change::Delete,
            (FileState::Exists(hash), Some(v)) => {
                let new_hash = hash_once::<FxHasher>(&*v);
                if new_hash == hash {
                    return false;
                }
                Change::Modify(v, new_hash)
            }
            (FileState::Excluded, _) => return false,
        };

        let mut set_data = |change_kind| {
            self.data.insert(file_path, match change_kind {
                &Change::Create(_, hash) | &Change::Modify(_, hash) => FileState::Exists(hash),
                Change::Delete => FileState::Deleted,
            });
        };

        let changed_file = ChangedFile { file: file_path, change };
        match self.changes.entry(file_path) {
            // two changes to the same file in one cycle, merge them appropriately
            Entry::Occupied(mut o) => {
                use Change::*;

                o.get_mut().change = match (&o.get().change, changed_file.change) {
                    // newer `Delete` wins
                    (_, Delete) => Delete,
                    // merge `Create` with `Create` or `Modify`
                    (Create(_, _), Create(new, new_hash) | Modify(new, new_hash)) => {
                        Create(new, new_hash)
                    }
                    // collapse identical `Modify`es
                    (Modify(_, _), Modify(new, new_hash)) => {
                        Modify(new, new_hash)
                    }
                    // equivalent to `Modify`
                    (Delete, Create(new, new_hash)) => {
                        Modify(new, new_hash)
                    }
                    // shouldn't occur, but collapse into `Create`
                    (Delete, Modify(new, new_hash)) => {
                        stdx::never!();
                        Create(new, new_hash)
                    }
                    // shouldn't occur, but keep the Create
                    (Modify(_, _), new @ Create(_, _)) => new,
                };
                set_data(&o.get().change);
            }
            Entry::Vacant(v) => set_data(&v.insert(changed_file).change),
        };

        true
    }

    /// Drain and returns all the changes in the `Vfs`.
    pub fn take_changes(&mut self) -> IndexMap<File, ChangedFile, BuildHasherDefault<FxHasher>> {
        mem::take(&mut self.changes)
    }

    /// Provides a panic-less way to verify file existence.
    pub fn exists(&self, file_path: &File) -> bool {
        matches!(self.get(file_path), Some(FileState::Exists(_)))
    }

    /// Returns the status of the file associated with the given `file_path`.
    fn get(&self, file_path: &File) -> Option<FileState> {
        self.data.get(file_path).copied()
    }

    /// We cannot ignore excluded files, because this will lead to errors when the client
    /// requests semantic information for them, so we instead mark them specially.
    pub fn insert_excluded_file(&mut self, file_path: File) {
        self.data.insert(file_path, FileState::Excluded);
    }
}

impl fmt::Debug for Vfs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Vfs").field("n_files", &self.data.len()).finish()
    }
}
