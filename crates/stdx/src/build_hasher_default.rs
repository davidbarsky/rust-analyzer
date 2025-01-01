use std::fmt;
use std::hash::{BuildHasher, Hasher};
use std::marker::PhantomData;

/// A copy of [`std::hash::BuildHasherDefault`] but with `const` new.
// FIXME: Remove this once `build_hasher_default_const_new` stabilizes.
pub struct BuildHasherDefault<H>(PhantomData<fn() -> H>);

impl<H> BuildHasherDefault<H> {
    /// Creates a new BuildHasherDefault for Hasher `H`.
    pub const fn new() -> Self {
        BuildHasherDefault(PhantomData)
    }
}

impl<H> fmt::Debug for BuildHasherDefault<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BuildHasherDefault").finish()
    }
}

impl<H: Default + Hasher> BuildHasher for BuildHasherDefault<H> {
    type Hasher = H;

    fn build_hasher(&self) -> H {
        H::default()
    }
}

impl<H> Clone for BuildHasherDefault<H> {
    fn clone(&self) -> BuildHasherDefault<H> {
        BuildHasherDefault(PhantomData)
    }
}

impl<H> Default for BuildHasherDefault<H> {
    fn default() -> BuildHasherDefault<H> {
        Self::new()
    }
}

impl<H> PartialEq for BuildHasherDefault<H> {
    fn eq(&self, _other: &BuildHasherDefault<H>) -> bool {
        true
    }
}

impl<H> Eq for BuildHasherDefault<H> {}
