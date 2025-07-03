use std::{fmt, panic};

use base_db::{
    Crate, CrateGraphBuilder, CratesMap, FileSourceRootInput, FileText, RootQueryDb,
    SourceDatabase, SourceRoot, SourceRootId, SourceRootInput,
};
use criterion::{Criterion, black_box, criterion_group, criterion_main};
use hir_def::{db::DefDatabase, nameres::crate_def_map};
use salsa::Durability;
use test_fixture::WithFixture;
use triomphe::Arc;

#[salsa_macros::db]
#[derive(Clone)]
struct TestDB {
    storage: salsa::Storage<Self>,
    files: Arc<base_db::Files>,
    crates_map: Arc<CratesMap>,
}

impl Default for TestDB {
    fn default() -> Self {
        let mut this = Self {
            storage: salsa::Storage::new(None),
            files: Default::default(),
            crates_map: Default::default(),
        };
        this.set_expand_proc_attr_macros_with_durability(true, Durability::HIGH);
        // This needs to be here otherwise `CrateGraphBuilder` panics.
        this.set_all_crates(Arc::new(Box::new([])));
        CrateGraphBuilder::default().set_in_db(&mut this);
        this
    }
}

#[salsa_macros::db]
impl salsa::Database for TestDB {}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}

impl panic::RefUnwindSafe for TestDB {}

#[salsa_macros::db]
impl SourceDatabase for TestDB {
    fn file_text(&self, file_id: base_db::FileId) -> FileText {
        self.files.file_text(file_id)
    }

    fn set_file_text(&mut self, file_id: base_db::FileId, text: &str) {
        let files = Arc::clone(&self.files);
        files.set_file_text(self, file_id, text);
    }

    fn set_file_text_with_durability(
        &mut self,
        file_id: base_db::FileId,
        text: &str,
        durability: Durability,
    ) {
        let files = Arc::clone(&self.files);
        files.set_file_text_with_durability(self, file_id, text, durability);
    }

    /// Source root of the file.
    fn source_root(&self, source_root_id: SourceRootId) -> SourceRootInput {
        self.files.source_root(source_root_id)
    }

    fn set_source_root_with_durability(
        &mut self,
        source_root_id: SourceRootId,
        source_root: Arc<SourceRoot>,
        durability: Durability,
    ) {
        let files = Arc::clone(&self.files);
        files.set_source_root_with_durability(self, source_root_id, source_root, durability);
    }

    fn file_source_root(&self, id: base_db::FileId) -> FileSourceRootInput {
        self.files.file_source_root(id)
    }

    fn set_file_source_root_with_durability(
        &mut self,
        id: base_db::FileId,
        source_root_id: SourceRootId,
        durability: Durability,
    ) {
        let files = Arc::clone(&self.files);
        files.set_file_source_root_with_durability(self, id, source_root_id, durability);
    }

    fn crates_map(&self) -> Arc<CratesMap> {
        self.crates_map.clone()
    }
}

impl TestDB {
    fn fetch_test_crate(&self) -> Crate {
        let all_crates = self.all_crates();
        all_crates
            .iter()
            .copied()
            .find(|&krate| {
                krate.extra_data(self).display_name.as_ref().map(|it| it.canonical_name().as_str())
                    == Some("ra_test_fixture")
            })
            .unwrap_or(*all_crates.last().unwrap())
    }
}

fn render_crate_def_map(ra_fixture: &str) -> String {
    let db = TestDB::with_files(ra_fixture);
    let krate = db.fetch_test_crate();
    crate_def_map(&db, krate).dump(&db)
}

fn bench_simple_glob(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod foo;
use foo::*;

//- /foo/mod.rs
pub mod bar;
pub use self::bar::Baz;
pub struct Foo;

//- /foo/bar.rs
pub struct Baz;
"#;

    c.bench_function("simple_glob", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_nested_glob_reexports(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod foo;
use foo::*;

//- /foo/mod.rs
pub mod bar;
pub use self::bar::*;
pub struct Foo;

//- /foo/bar.rs
pub struct Baz;
pub use super::*;
"#;

    c.bench_function("nested_glob_reexports", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_glob_across_crates(c: &mut Criterion) {
    let fixture = r#"
//- /main.rs crate:main deps:test_crate
use test_crate::*;

//- /lib.rs crate:test_crate
pub struct Foo;
pub struct Bar;
pub struct Baz;
pub mod nested {
    pub struct Inner;
}
"#;

    c.bench_function("glob_across_crates", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_glob_enum_variants(c: &mut Criterion) {
    let fixture = r#"
enum Foo {
    Bar, Baz, Qux, Quux, Corge, Grault, Garply, Waldo, Fred, Plugh, Xyzzy, Thud
}
use self::Foo::*;
"#;

    c.bench_function("glob_enum_variants", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_complex_glob_hierarchy(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod a;
mod b;
mod c;
use a::*;
use b::*;
use c::*;

//- /a.rs
pub mod inner_a {
    pub struct A1;
    pub struct A2;
    pub struct A3;
}
pub use inner_a::*;

//- /b.rs
pub mod inner_b {
    pub struct B1;
    pub struct B2;
    pub struct B3;
}
pub use inner_b::*;

//- /c.rs
pub mod inner_c {
    pub struct C1;
    pub struct C2;
    pub struct C3;
}
pub use inner_c::*;
"#;

    c.bench_function("complex_glob_hierarchy", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_glob_with_shadowing(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod foo;
mod bar;
use foo::*;
use bar::baz;
use baz::Bar;

//- /foo.rs
pub mod baz { pub struct Foo; }

//- /bar.rs
pub mod baz { pub struct Bar; }
"#;

    c.bench_function("glob_with_shadowing", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_large_glob_import(c: &mut Criterion) {
    let mut fixture = String::from(
        r#"
//- /lib.rs
mod items;
use items::*;

//- /items.rs
"#,
    );

    // Generate a large number of items to glob import
    for i in 0..100 {
        fixture.push_str(&format!("pub struct Item{};\n", i));
        fixture.push_str(&format!("pub fn func{}() {{}}\n", i));
        fixture.push_str(&format!("pub const CONST{}: u32 = {};\n", i, i));
    }

    c.bench_function("large_glob_import", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(&fixture));
            black_box(result);
        })
    });
}

fn bench_recursive_glob_reexports(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod a;
use a::*;

//- /a.rs
mod b;
pub use b::*;

//- /a/b.rs
mod c;
pub use c::*;

//- /a/b/c.rs
mod d;
pub use d::*;

//- /a/b/c/d.rs
pub struct DeepStruct;
pub fn deep_function() {}
pub const DEEP_CONST: u32 = 42;
"#;

    c.bench_function("recursive_glob_reexports", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_recursive_reexports(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod a;
use a::DeepStruct;

//- /a.rs
mod b;
pub use b::DeepStruct;

//- /a/b.rs
mod c;
pub use c::DeepStruct;

//- /a/b/c.rs
mod d;
pub use d::DeepStruct;

//- /a/b/c/d.rs
pub struct DeepStruct;
pub fn deep_function() {}
pub const DEEP_CONST: u32 = 42;
"#;

    c.bench_function("recursive_reexports", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_glob_with_visibility_filters(c: &mut Criterion) {
    let fixture = r#"
//- /lib.rs
mod foo;
use foo::*;
use foo::bar::*;

//- /foo/mod.rs
pub mod bar;
fn private_foo() {};
pub struct PublicFoo {};

//- /foo/bar.rs
pub(super) struct SuperVisible;
struct Private;
pub(crate) struct CrateVisible;
pub struct Public;
"#;

    c.bench_function("glob_with_visibility_filters", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

fn bench_std_prelude_like(c: &mut Criterion) {
    let fixture = r#"
//- /main.rs crate:main deps:std
#[prelude_import]
use ::std::prelude::*;

use Option::*;
use Result::*;

//- /lib.rs crate:std
pub mod prelude;

//- /prelude.rs
pub enum Option<T> { Some(T), None }
pub enum Result<T, E> { Ok(T), Err(E) }
pub struct Vec<T>;
pub struct String;
pub trait Clone {}
pub trait Copy {}
pub trait Send {}
pub trait Sync {}
"#;

    c.bench_function("std_prelude_like", |b| {
        b.iter(|| {
            let result = render_crate_def_map(black_box(fixture));
            black_box(result);
        })
    });
}

criterion_group!(
    benches,
    // bench_simple_glob,
    // bench_nested_glob_reexports,
    // bench_glob_across_crates,
    // bench_glob_enum_variants,
    // bench_complex_glob_hierarchy,
    // bench_large_glob_import,
    bench_recursive_glob_reexports,
    bench_recursive_reexports,
    // bench_glob_with_shadowing,
    // bench_glob_with_visibility_filters,
    // bench_std_prelude_like
);

criterion_main!(benches);
