//! Adds an argument expression to every resolved call site of a function.

use hir::Semantics;
use ide_db::{
    RootDatabase,
    defs::{Definition, NameClass, NameRefClass},
    search::FileReference,
    source_change::SourceChange,
    text_edit::TextEdit,
};
use syntax::{
    AstNode, SourceFile, SyntaxToken,
    algo::find_node_at_range,
    ast::{self, HasArgList},
};

use crate::FilePosition;

/// The result of [`add_argument`]: the edit, plus how many references it
/// could not place an argument at (macro call sites and other forms
/// `process_reference` doesn't recognize).
pub struct AddArgumentResult {
    pub source_change: SourceChange,
    pub skipped: usize,
}

pub(crate) fn add_argument(
    db: &RootDatabase,
    position: FilePosition,
    argument_index: usize,
    placeholder: &str,
) -> Option<AddArgumentResult> {
    let sema = Semantics::new(db);
    let source_file = sema.parse_guess_edition(position.file_id);
    let syntax = source_file.syntax();

    let token = syntax.token_at_offset(position.offset).right_biased()?;
    let hir_func = target_function(&sema, &token)
        .or_else(|| sema.to_def(&token.parent_ancestors().find_map(ast::Fn::cast)?))?;
    let func_node = sema.source(hir_func)?.value;

    let is_self_present = func_node.param_list().and_then(|pl| pl.self_param()).is_some();

    let fn_def = Definition::Function(hir_func);

    let mut source_change = SourceChange::default();
    let mut skipped = 0;

    for (editioned_file_id, references) in fn_def.usages(&sema).all().iter() {
        let file_id = editioned_file_id.file_id(db);
        let parse = sema.parse(editioned_file_id);

        let mut edits = TextEdit::builder();
        for reference in references {
            let edited = process_reference(
                &parse,
                reference,
                argument_index,
                is_self_present,
                placeholder,
                &mut edits,
            );
            if !edited {
                skipped += 1;
            }
        }
        let edit = edits.finish();
        if !edit.is_empty() {
            source_change.insert_source_edit(file_id, edit);
        }
    }

    Some(AddArgumentResult { source_change, skipped })
}

/// Resolves `token` as a reference to a function: the cursor is on a call's
/// callee name, a method call's name, or a function's own name. Returns
/// `None` when the token isn't on any name, so callers fall back to the
/// enclosing function.
fn target_function(
    sema: &Semantics<'_, RootDatabase>,
    token: &SyntaxToken,
) -> Option<hir::Function> {
    token.parent_ancestors().find_map(|node| {
        if let Some(name_ref) = ast::NameRef::cast(node.clone()) {
            let NameRefClass::Definition(Definition::Function(f), _) =
                NameRefClass::classify(sema, &name_ref)?
            else {
                return None;
            };
            return Some(f);
        }
        let name = ast::Name::cast(node)?;
        let NameClass::Definition(Definition::Function(f)) = NameClass::classify(sema, &name)?
        else {
            return None;
        };
        Some(f)
    })
}

/// Returns whether the reference was rewritten. `false` (a skip) happens for
/// references inside macro invocations (no `CallExpr`/`MethodCallExpr` covers
/// the range) or missing syntax (no arg list, no l_paren).
fn process_reference(
    source_file: &SourceFile,
    FileReference { range, name: _, category: _ }: &FileReference,
    argument_index: usize,
    is_self_present: bool,
    placeholder: &str,
    edits: &mut ide_db::text_edit::TextEditBuilder,
) -> bool {
    let range = *range;

    if let Some(call_expr) = find_node_at_range::<ast::CallExpr>(source_file.syntax(), range) {
        let Some(callee) = call_expr.expr() else { return false };
        if !callee.syntax().text_range().contains_range(range) {
            return false;
        }

        let effective_index = if is_self_present { argument_index + 1 } else { argument_index };

        let Some(arg_list) = call_expr.arg_list() else { return false };
        return insert_arg(&arg_list, effective_index, placeholder, edits);
    }

    if let Some(method_call) =
        find_node_at_range::<ast::MethodCallExpr>(source_file.syntax(), range)
    {
        let Some(name_ref) = method_call.name_ref() else { return false };
        if !name_ref.syntax().text_range().contains_range(range) {
            return false;
        }

        let Some(arg_list) = method_call.arg_list() else { return false };
        return insert_arg(&arg_list, argument_index, placeholder, edits);
    }

    false
}

fn insert_arg(
    arg_list: &ast::ArgList,
    index: usize,
    placeholder: &str,
    edits: &mut ide_db::text_edit::TextEditBuilder,
) -> bool {
    let args: Vec<ast::Expr> = arg_list.args().collect();

    if args.is_empty() {
        let Some(l_paren) = arg_list.l_paren_token() else { return false };
        let insert_offset = l_paren.text_range().end();
        edits.insert(insert_offset, placeholder.to_owned());
    } else if index >= args.len() {
        let last_arg = &args[args.len() - 1];
        let insert_offset = last_arg.syntax().text_range().end();
        edits.insert(insert_offset, format!(", {placeholder}"));
    } else {
        let arg_at_index = &args[index];
        let insert_offset = arg_at_index.syntax().text_range().start();
        edits.insert(insert_offset, format!("{placeholder}, "));
    }
    true
}

#[cfg(test)]
mod tests {
    use ide_db::FxHashMap;
    use stdx::trim_indent;
    use test_utils::assert_eq_text;

    use crate::fixture;

    fn check(
        argument_index: usize,
        placeholder: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_after: &str,
    ) {
        let ra_fixture_after = &trim_indent(ra_fixture_after);
        let (analysis, position) = fixture::position(ra_fixture_before);
        let super::AddArgumentResult { source_change, skipped } = analysis
            .add_argument_to_call_sites(position, argument_index, placeholder)
            .unwrap()
            .expect("add_argument returned None");
        assert_eq!(skipped, 0, "unexpected skipped call sites");

        let mut texts_by_file = FxHashMap::default();
        for (&file_id, (edit, _snippet)) in &source_change.source_file_edits {
            let mut text = analysis.file_text(file_id).unwrap().to_string();
            edit.apply(&mut text);
            texts_by_file.insert(file_id, text);
        }

        if source_change.source_file_edits.len() == 1 {
            let (_, text) = texts_by_file.into_iter().next().unwrap();
            assert_eq_text!(ra_fixture_after, &text);
        } else {
            let text = texts_by_file.get(&position.file_id).expect("expected edit in main file");
            assert_eq_text!(ra_fixture_after, text);
        }
    }

    fn check_multi_file(
        argument_index: usize,
        placeholder: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
        expected_files: &[&str],
    ) {
        let (analysis, position) = fixture::position(ra_fixture_before);
        // Multi-file fixtures can legitimately skip non-call references (e.g.
        // `use super::foo;`), so `skipped` isn't asserted here.
        let super::AddArgumentResult { source_change, skipped: _ } = analysis
            .add_argument_to_call_sites(position, argument_index, placeholder)
            .unwrap()
            .expect("add_argument returned None");

        let mut results = Vec::new();
        for (&file_id, (edit, _snippet)) in &source_change.source_file_edits {
            let mut text = analysis.file_text(file_id).unwrap().to_string();
            edit.apply(&mut text);
            results.push(text);
        }

        for expected in expected_files {
            let expected = &trim_indent(expected);
            assert!(
                results.iter().any(|r| r.trim() == expected.trim()),
                "Expected to find:\n{expected}\n\nGot:\n{results:#?}"
            );
        }
    }

    #[test]
    fn free_function_insert_middle() {
        check(
            1,
            "todo!()",
            r#"
fn foo$0(a: i32, b: i32) {}

fn main() {
    foo(1, 2);
}
"#,
            r#"
fn foo(a: i32, b: i32) {}

fn main() {
    foo(1, todo!(), 2);
}
"#,
        );
    }

    #[test]
    fn free_function_insert_beginning() {
        check(
            0,
            "todo!()",
            r#"
fn foo$0(a: i32, b: i32) {}

fn main() {
    foo(1, 2);
}
"#,
            r#"
fn foo(a: i32, b: i32) {}

fn main() {
    foo(todo!(), 1, 2);
}
"#,
        );
    }

    #[test]
    fn free_function_append() {
        check(
            1,
            "todo!()",
            r#"
fn foo$0(a: i32) {}

fn main() {
    foo(1);
}
"#,
            r#"
fn foo(a: i32) {}

fn main() {
    foo(1, todo!());
}
"#,
        );
    }

    #[test]
    fn empty_arg_list() {
        check(
            0,
            "todo!()",
            r#"
fn foo$0() {}

fn main() {
    foo();
}
"#,
            r#"
fn foo() {}

fn main() {
    foo(todo!());
}
"#,
        );
    }

    #[test]
    fn method_call_insert_beginning() {
        check(
            0,
            "todo!()",
            r#"
struct S;
impl S {
    fn f$0(&self, a: i32) {}
}

fn main() {
    S.f(1);
}
"#,
            r#"
struct S;
impl S {
    fn f(&self, a: i32) {}
}

fn main() {
    S.f(todo!(), 1);
}
"#,
        );
    }

    #[test]
    fn method_call_append() {
        check(
            1,
            "todo!()",
            r#"
struct S;
impl S {
    fn f$0(&self, a: i32) {}
}

fn main() {
    S.f(1);
}
"#,
            r#"
struct S;
impl S {
    fn f(&self, a: i32) {}
}

fn main() {
    S.f(1, todo!());
}
"#,
        );
    }

    #[test]
    fn ufcs_call() {
        check(
            0,
            "todo!()",
            r#"
struct S;
impl S {
    fn f$0(&self, a: i32) {}
}

fn main() {
    S::f(&S, 1);
}
"#,
            r#"
struct S;
impl S {
    fn f(&self, a: i32) {}
}

fn main() {
    S::f(&S, todo!(), 1);
}
"#,
        );
    }

    #[test]
    fn mixed_method_and_ufcs() {
        check(
            0,
            "42",
            r#"
struct S;
impl S {
    fn f$0(&self, a: i32) {}
}

fn main() {
    let s = S;
    s.f(1);
    S::f(&s, 1);
}
"#,
            r#"
struct S;
impl S {
    fn f(&self, a: i32) {}
}

fn main() {
    let s = S;
    s.f(42, 1);
    S::f(&s, 42, 1);
}
"#,
        );
    }

    #[test]
    fn custom_placeholder() {
        check(
            0,
            "Default::default()",
            r#"
fn foo$0(a: i32) {}

fn main() {
    foo(1);
}
"#,
            r#"
fn foo(a: i32) {}

fn main() {
    foo(Default::default(), 1);
}
"#,
        );
    }

    #[test]
    fn cross_file() {
        check_multi_file(
            1,
            "todo!()",
            r#"
//- /main.rs
fn foo$0(a: i32) {}

mod other;

//- /other.rs
use super::foo;

fn bar() {
    foo(1);
}
"#,
            &[r#"
use super::foo;

fn bar() {
    foo(1, todo!());
}
"#],
        );
    }

    #[test]
    fn call_site_targets_callee_not_enclosing_fn() {
        check(
            0,
            "todo!()",
            r#"
fn foo(a: i32) {}

fn main() {
    $0foo(1);
}
"#,
            r#"
fn foo(a: i32) {}

fn main() {
    foo(todo!(), 1);
}
"#,
        );
    }

    #[test]
    fn method_call_name_targets_method_not_enclosing_fn() {
        check(
            0,
            "todo!()",
            r#"
struct S;
impl S {
    fn f(&self, a: i32) {}
}

fn main() {
    S.$0f(1);
}
"#,
            r#"
struct S;
impl S {
    fn f(&self, a: i32) {}
}

fn main() {
    S.f(todo!(), 1);
}
"#,
        );
    }

    #[test]
    fn cursor_in_body_without_name_targets_enclosing_fn() {
        check(
            1,
            "todo!()",
            r#"
fn foo(a: i32, b: i32) {
    a +$0 b;
}

fn main() {
    foo(1, 2);
}
"#,
            r#"
fn foo(a: i32, b: i32) {
    a + b;
}

fn main() {
    foo(1, todo!(), 2);
}
"#,
        );
    }

    #[test]
    fn macro_call_site_is_skipped_and_counted() {
        let (analysis, position) = fixture::position(
            r#"
macro_rules! m {
    ($x:expr) => { $x };
}

fn foo$0(a: i32) {}

fn main() {
    foo(1);
    m!(foo(2));
}
"#,
        );
        let super::AddArgumentResult { source_change, skipped } = analysis
            .add_argument_to_call_sites(position, 0, "todo!()")
            .unwrap()
            .expect("add_argument returned None");
        assert_eq!(skipped, 1);

        let (&file_id, (edit, _snippet)) =
            source_change.source_file_edits.iter().next().expect("expected one edited file");
        let mut text = analysis.file_text(file_id).unwrap().to_string();
        edit.apply(&mut text);
        assert_eq_text!(
            &trim_indent(
                r#"
                macro_rules! m {
                    ($x:expr) => { $x };
                }

                fn foo(a: i32) {}

                fn main() {
                    foo(todo!(), 1);
                    m!(foo(2));
                }
                "#
            ),
            &text
        );
    }
}
