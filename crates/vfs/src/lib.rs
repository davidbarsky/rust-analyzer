//! # Virtual file paths and loading
//!
//! Analysis file IDs and source roots are owned by `salsa`. This crate keeps the
//! path types, source-root partitioning helpers, and loader abstraction used by
//! rust-analyzer's outer IO shell.
//!
//! [`FileSet`] and [`loader::Entry`] play similar, but different roles.
//! Both specify the "set of paths/files", one is geared towards file watching,
//! the other towards salsa changes. In particular, single [`FileSet`]
//! may correspond to several [`loader::Entry`]. For example, a crate from
//! crates.io which uses code generation would have two [`Entries`] -- for sources
//! in `~/.cargo`, and for generated code in `./target/debug/build`. It will
//! have a single [`FileSet`] which unions the two sources.
//!
//! [`FileSet`]: file_set::FileSet
//! [`Handle`]: loader::Handle
//! [`Entries`]: loader::Entry

mod anchored_path;
pub mod file_set;
pub mod loader;
mod vfs_path;

pub use crate::{
    anchored_path::{AnchoredPath, AnchoredPathBuf},
    vfs_path::VfsPath,
};
pub use paths::{AbsPath, AbsPathBuf};

/// Handle to a file in the salsa database.
///
/// Most functions in rust-analyzer use this when they need to refer to a file.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FileId(u32);

impl FileId {
    const MAX: u32 = 0x7fff_ffff;

    #[inline]
    pub const fn from_raw(raw: u32) -> FileId {
        assert!(raw <= Self::MAX);
        FileId(raw)
    }

    #[inline]
    pub const fn index(self) -> u32 {
        self.0
    }
}

/// safe because `FileId` is a newtype of `u32`
impl nohash_hasher::IsEnabled for FileId {}

/// Kind of file change received from the loader or pending in the server shell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChangeKind {
    /// The file was (re-)created
    Create,
    /// The file was modified
    Modify,
    /// The file was deleted
    Delete,
}
