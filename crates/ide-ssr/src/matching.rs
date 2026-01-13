//! This module is responsible for matching a search pattern against a node in the AST. In the
//! process of matching, placeholder values are recorded.

use crate::{
    SsrMatches, fragments,
    parsing::{Constraint, ContextKind, NodeKind, Placeholder, Var, WhereClause, WhereCondition},
    resolving::{ResolvedPattern, ResolvedRule, UfcsCallInfo},
};
use hir::{
    Access, CallableKind, FileRange, FindPathConfig, HirDisplay, ModuleDef, PathResolution,
    Semantics, Trait,
};
use ide_db::{
    FxHashMap,
    base_db::{SourceDatabase, all_crates},
};
use std::{cell::Cell, iter::Peekable};
use syntax::{
    SmolStr, SyntaxElement, SyntaxElementChildren, SyntaxKind, SyntaxNode, SyntaxToken,
    ast::{self, AstNode, AstToken, HasGenericArgs},
};

// Creates a match error. If we're currently attempting to match some code that we thought we were
// going to match, as indicated by the --debug-snippet flag, then populate the reason field.
macro_rules! match_error {
    ($e:expr) => {{
            MatchFailed {
                reason: if recording_match_fail_reasons() {
                    Some(format!("{}", $e))
                } else {
                    None
                }
            }
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        MatchFailed {
            reason: if recording_match_fail_reasons() {
                Some(format!($fmt, $($arg)+))
            } else {
                None
            }
        }
    }};
}

// Fails the current match attempt, recording the supplied reason if we're recording match fail reasons.
macro_rules! fail_match {
    ($($args:tt)*) => {return Err(match_error!($($args)*))};
}

/// Information about a match that was found.
#[derive(Debug)]
pub struct Match {
    pub(crate) range: FileRange,
    pub(crate) matched_node: SyntaxNode,
    pub(crate) placeholder_values: FxHashMap<Var, PlaceholderMatch>,
    pub(crate) ignored_comments: Vec<ast::Comment>,
    pub(crate) rule_index: usize,
    /// The depth of matched_node.
    pub(crate) depth: usize,
    // Each path in the template rendered for the module in which the match was found.
    pub(crate) rendered_template_paths: FxHashMap<SyntaxNode, hir::ModPath>,
}

/// Information about a placeholder bound in a match.
#[derive(Debug)]
pub(crate) struct PlaceholderMatch {
    pub(crate) range: FileRange,
    /// More matches, found within `node`.
    pub(crate) inner_matches: SsrMatches,
    /// How many times the code that the placeholder matched needed to be dereferenced. Will only be
    /// non-zero if the placeholder matched to the receiver of a method call.
    pub(crate) autoderef_count: usize,
    pub(crate) autoref_kind: ast::SelfParamKind,
}

#[derive(Debug)]
pub(crate) struct MatchFailureReason {
    pub(crate) reason: String,
}

/// An "error" indicating that matching failed. Use the fail_match! macro to create and return this.
#[derive(Clone)]
pub(crate) struct MatchFailed {
    /// The reason why we failed to match. Only present when debug_active true in call to
    /// `get_match`.
    pub(crate) reason: Option<String>,
}

/// Checks if `code` matches the search pattern found in `search_scope`, returning information about
/// the match, if it does. Since we only do matching in this module and searching is done by the
/// parent module, we don't populate nested matches.
pub(crate) fn get_match<'db>(
    debug_active: bool,
    rule: &ResolvedRule<'db>,
    code: &SyntaxNode,
    restrict_range: &Option<FileRange>,
    sema: &Semantics<'db, ide_db::RootDatabase>,
) -> Result<Match, MatchFailed> {
    record_match_fails_reasons_scope(debug_active, || {
        Matcher::try_match(rule, code, restrict_range, sema)
    })
}

/// Checks if our search pattern matches a particular node of the AST.
struct Matcher<'db, 'sema> {
    sema: &'sema Semantics<'db, ide_db::RootDatabase>,
    /// If any placeholders come from anywhere outside of this range, then the match will be
    /// rejected.
    restrict_range: Option<FileRange>,
    rule: &'sema ResolvedRule<'db>,
}

/// Which phase of matching we're currently performing. We do two phases because most attempted
/// matches will fail and it means we can defer more expensive checks to the second phase.
enum Phase<'a> {
    /// On the first phase, we perform cheap checks. No state is mutated and nothing is recorded.
    First,
    /// On the second phase, we construct the `Match`. Things like what placeholders bind to is
    /// recorded.
    Second(&'a mut Match),
}

enum ResolvedConstraintType<'db> {
    Pattern(TypePattern<'db>),
    Trait(Trait),
}

struct TraitBoundPattern<'db> {
    trait_: Trait,
    args: Vec<hir::Type<'db>>,
    fn_sig: Option<FnTraitSignature<'db>>,
}

struct FnTraitSignature<'db> {
    params: Vec<TypePattern<'db>>,
    ret: Option<Box<TypePattern<'db>>>,
}

#[derive(Clone, Default, PartialEq, Eq)]
struct FnPtrQualifiers {
    is_const: bool,
    is_async: bool,
    is_unsafe: bool,
    abi: Option<SmolStr>,
}

enum TypePattern<'db> {
    Resolved(hir::Type<'db>),
    Adt {
        adt: hir::Adt,
        args: Vec<TypePattern<'db>>,
    },
    ImplTrait {
        bounds: Vec<TraitBoundPattern<'db>>,
    },
    DynTrait {
        bounds: Vec<TraitBoundPattern<'db>>,
    },
    FnPtr {
        params: Vec<TypePattern<'db>>,
        ret: Option<Box<TypePattern<'db>>>,
        qualifiers: FnPtrQualifiers,
    },
    Ref {
        mutable: bool,
        inner: Box<TypePattern<'db>>,
    },
    RawPtr {
        inner: Box<TypePattern<'db>>,
    },
    Slice {
        inner: Box<TypePattern<'db>>,
    },
    Array {
        inner: Box<TypePattern<'db>>,
        len: Option<usize>,
    },
    Tuple {
        items: Vec<TypePattern<'db>>,
    },
}

impl<'db, 'sema> Matcher<'db, 'sema> {
    fn try_match(
        rule: &ResolvedRule<'db>,
        code: &SyntaxNode,
        restrict_range: &Option<FileRange>,
        sema: &'sema Semantics<'db, ide_db::RootDatabase>,
    ) -> Result<Match, MatchFailed> {
        let match_state = Matcher { sema, restrict_range: *restrict_range, rule };
        // First pass at matching, where we check that node types and idents match.
        match_state.attempt_match_node(&mut Phase::First, &rule.pattern.node, code)?;
        let file_range = sema
            .original_range_opt(code)
            .ok_or(MatchFailed { reason: Some("def site definition".to_owned()) })?;
        match_state.validate_range(&file_range)?;
        let mut the_match = Match {
            range: file_range,
            matched_node: code.clone(),
            placeholder_values: FxHashMap::default(),
            ignored_comments: Vec::new(),
            rule_index: rule.index,
            depth: 0,
            rendered_template_paths: FxHashMap::default(),
        };
        // Second matching pass, where we record placeholder matches, ignored comments and maybe do
        // any other more expensive checks that we didn't want to do on the first pass.
        match_state.attempt_match_node(
            &mut Phase::Second(&mut the_match),
            &rule.pattern.node,
            code,
        )?;
        match_state.check_where_clause(&rule.where_clause, code, &the_match)?;
        the_match.depth = sema.ancestors_with_macros(the_match.matched_node.clone()).count();
        if let Some(template) = &rule.template {
            the_match.render_template_paths(template, sema)?;
        }
        Ok(the_match)
    }

    /// Checks that `range` is within the permitted range if any. This is applicable when we're
    /// processing a macro expansion and we want to fail the match if we're working with a node that
    /// didn't originate from the token tree of the macro call.
    fn validate_range(&self, range: &FileRange) -> Result<(), MatchFailed> {
        if let Some(restrict_range) = &self.restrict_range
            && (restrict_range.file_id != range.file_id
                || !restrict_range.range.contains_range(range.range))
        {
            fail_match!("Node originated from a macro");
        }
        Ok(())
    }

    fn attempt_match_node(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SyntaxNode,
        code: &SyntaxNode,
    ) -> Result<(), MatchFailed> {
        // Handle placeholders.
        if let Some(placeholder) = self.get_placeholder_for_node(pattern) {
            // Type constraints require inference, so they're deferred to the second pass along
            // with the other expensive checks.
            let check_types = match phase {
                Phase::First => false,
                Phase::Second(_) => true,
            };
            for constraint in &placeholder.constraints {
                if check_types || !constraint.needs_type_inference() {
                    self.check_constraint(constraint, code)?;
                }
            }
            if let Phase::Second(matches_out) = phase {
                let original_range = self
                    .sema
                    .original_range_opt(code)
                    .ok_or(MatchFailed { reason: Some("def site definition".to_owned()) })?;
                // We validated the range for the node when we started the match, so the placeholder
                // probably can't fail range validation, but just to be safe...
                self.validate_range(&original_range)?;
                matches_out.placeholder_values.insert(
                    placeholder.ident.clone(),
                    PlaceholderMatch::from_range(original_range),
                );
            }
            return Ok(());
        }
        // We allow a UFCS call to match a method call, provided they resolve to the same function.
        if let Some(pattern_ufcs) = self.rule.pattern.ufcs_function_calls.get(pattern) {
            if let Some(code) = ast::MethodCallExpr::cast(code.clone()) {
                return self.attempt_match_ufcs_to_method_call(phase, pattern_ufcs, &code);
            }
            if let Some(code) = ast::CallExpr::cast(code.clone()) {
                return self.attempt_match_ufcs_to_ufcs(phase, pattern_ufcs, &code);
            }
        }
        if pattern.kind() != code.kind() {
            fail_match!(
                "Pattern had `{}` ({:?}), code had `{}` ({:?})",
                pattern.text(),
                pattern.kind(),
                code.text(),
                code.kind()
            );
        }
        // Some kinds of nodes have special handling. For everything else, we fall back to default
        // matching.
        match code.kind() {
            SyntaxKind::RECORD_EXPR_FIELD_LIST => {
                self.attempt_match_record_field_list(phase, pattern, code)
            }
            SyntaxKind::TOKEN_TREE => self.attempt_match_token_tree(phase, pattern, code),
            SyntaxKind::PATH => self.attempt_match_path(phase, pattern, code),
            _ => self.attempt_match_node_children(phase, pattern, code),
        }
    }

    fn attempt_match_node_children(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SyntaxNode,
        code: &SyntaxNode,
    ) -> Result<(), MatchFailed> {
        self.attempt_match_sequences(
            phase,
            PatternIterator::new(pattern),
            code.children_with_tokens(),
        )
    }

    fn attempt_match_sequences(
        &self,
        phase: &mut Phase<'_>,
        pattern_it: PatternIterator,
        mut code_it: SyntaxElementChildren,
    ) -> Result<(), MatchFailed> {
        let mut pattern_it = pattern_it.peekable();
        loop {
            match phase.next_non_trivial(&mut code_it) {
                None => {
                    if let Some(p) = pattern_it.next() {
                        fail_match!("Part of the pattern was unmatched: {:?}", p);
                    }
                    return Ok(());
                }
                Some(SyntaxElement::Token(c)) => {
                    self.attempt_match_token(phase, &mut pattern_it, &c)?;
                }
                Some(SyntaxElement::Node(c)) => match pattern_it.next() {
                    Some(SyntaxElement::Node(p)) => {
                        self.attempt_match_node(phase, &p, &c)?;
                    }
                    Some(p) => fail_match!("Pattern wanted '{}', code has {}", p, c.text()),
                    None => fail_match!("Pattern reached end, code has {}", c.text()),
                },
            }
        }
    }

    fn attempt_match_token(
        &self,
        phase: &mut Phase<'_>,
        pattern: &mut Peekable<PatternIterator>,
        code: &syntax::SyntaxToken,
    ) -> Result<(), MatchFailed> {
        phase.record_ignored_comments(code);
        // Ignore whitespace and comments.
        if code.kind().is_trivia() {
            return Ok(());
        }
        if let Some(SyntaxElement::Token(p)) = pattern.peek() {
            // If the code has a comma and the pattern is about to close something, then accept the
            // comma without advancing the pattern. i.e. ignore trailing commas.
            if code.kind() == SyntaxKind::COMMA && is_closing_token(p.kind()) {
                return Ok(());
            }
            // Conversely, if the pattern has a comma and the code doesn't, skip that part of the
            // pattern and continue to match the code.
            if p.kind() == SyntaxKind::COMMA && is_closing_token(code.kind()) {
                pattern.next();
            }
        }
        // Consume an element from the pattern and make sure it matches.
        match pattern.next() {
            Some(SyntaxElement::Token(p)) => {
                if p.kind() != code.kind() || p.text() != code.text() {
                    fail_match!(
                        "Pattern wanted token '{}' ({:?}), but code had token '{}' ({:?})",
                        p.text(),
                        p.kind(),
                        code.text(),
                        code.kind()
                    )
                }
            }
            Some(SyntaxElement::Node(p)) => {
                // Not sure if this is actually reachable.
                fail_match!(
                    "Pattern wanted {:?}, but code had token '{}' ({:?})",
                    p,
                    code.text(),
                    code.kind()
                );
            }
            None => {
                fail_match!("Pattern exhausted, while code remains: `{}`", code.text());
            }
        }
        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_constraint(
        &self,
        constraint: &Constraint,
        code: &SyntaxNode,
    ) -> Result<(), MatchFailed> {
        match constraint {
            Constraint::Kind(kind) => {
                kind.matches(code)?;
            }
            Constraint::Type(type_text) => {
                self.check_type_constraint(code, type_text)?;
            }
            Constraint::Not(sub) => {
                if self.check_constraint(sub, code).is_ok() {
                    fail_match!("Constraint {:?} failed for '{}'", constraint, code.text());
                }
            }
            Constraint::Context(context_kind) => {
                context_kind.matches(code)?;
            }
        }
        Ok(())
    }

    fn check_where_clause(
        &self,
        where_clause: &WhereClause,
        code: &SyntaxNode,
        the_match: &Match,
    ) -> Result<(), MatchFailed> {
        for condition in &where_clause.conditions {
            self.check_where_condition(condition, code, the_match)?;
        }
        Ok(())
    }

    fn check_where_condition(
        &self,
        condition: &WhereCondition,
        code: &SyntaxNode,
        the_match: &Match,
    ) -> Result<(), MatchFailed> {
        match condition {
            WhereCondition::Kind(kind) => {
                kind.matches(code)?;
            }
            WhereCondition::Type(type_text) => {
                self.check_type_constraint(code, type_text)?;
            }
            WhereCondition::Context(context_kind) => {
                context_kind.matches(code)?;
            }
            WhereCondition::Not(inner) => {
                if self.check_where_condition(inner, code, the_match).is_ok() {
                    fail_match!("not() condition was satisfied but shouldn't be");
                }
            }
            WhereCondition::Or(alternatives) => {
                let mut any_matched = false;
                for alt in alternatives {
                    if self.check_where_condition(alt, code, the_match).is_ok() {
                        any_matched = true;
                        break;
                    }
                }
                if !any_matched {
                    fail_match!("None of the or() alternatives matched");
                }
            }
            WhereCondition::PlaceholderOneOf { placeholder, values } => {
                let placeholder_match =
                    the_match.placeholder_values.get(placeholder).ok_or_else(|| {
                        match_error!("Placeholder {} not found in match", placeholder)
                    })?;
                let file_id = placeholder_match.range.file_id.file_id(self.sema.db);
                let file_text = self.sema.db.file_text(file_id).text(self.sema.db);
                let matched_text = &file_text[placeholder_match.range.range];
                if !values.iter().any(|v| v.as_str() == matched_text) {
                    fail_match!(
                        "Placeholder {} matched '{}', expected one of {:?}",
                        placeholder,
                        matched_text,
                        values
                    );
                }
            }
            WhereCondition::PlaceholderEq { placeholder, value } => {
                let placeholder_match =
                    the_match.placeholder_values.get(placeholder).ok_or_else(|| {
                        match_error!("Placeholder {} not found in match", placeholder)
                    })?;
                let file_id = placeholder_match.range.file_id.file_id(self.sema.db);
                let file_text = self.sema.db.file_text(file_id).text(self.sema.db);
                let matched_text = &file_text[placeholder_match.range.range];
                if matched_text != value.as_str() {
                    fail_match!(
                        "Placeholder {} matched '{}', expected '{}'",
                        placeholder,
                        matched_text,
                        value
                    );
                }
            }
            WhereCondition::PlaceholderMutSelf { placeholder } => {
                self.check_method_self_param(code, the_match, placeholder, Access::Exclusive)?;
            }
            WhereCondition::PlaceholderRefSelf { placeholder } => {
                self.check_method_self_param(code, the_match, placeholder, Access::Shared)?;
            }
            WhereCondition::PlaceholderOwnedSelf { placeholder } => {
                self.check_method_self_param(code, the_match, placeholder, Access::Owned)?;
            }
        }
        Ok(())
    }

    /// Checks that the method call in `code` has the expected self parameter access kind.
    /// The `placeholder` is used to verify which method we're checking (for error messages).
    fn check_method_self_param(
        &self,
        code: &SyntaxNode,
        the_match: &Match,
        placeholder: &Var,
        expected_access: Access,
    ) -> Result<(), MatchFailed> {
        let placeholder_match = the_match
            .placeholder_values
            .get(placeholder)
            .ok_or_else(|| match_error!("Placeholder {} not found in match", placeholder))?;

        let file_id = placeholder_match.range.file_id.file_id(self.sema.db);
        let file_text = self.sema.db.file_text(file_id).text(self.sema.db);
        let method_name = &file_text[placeholder_match.range.range];

        let method_call = ast::MethodCallExpr::cast(code.clone()).ok_or_else(|| {
            match_error!("Expected method call for self parameter check, found {:?}", code.kind())
        })?;

        let function = self
            .sema
            .resolve_method_call(&method_call)
            .ok_or_else(|| match_error!("Failed to resolve method call `{}`", method_name))?;

        let self_param = function
            .self_param(self.sema.db)
            .ok_or_else(|| match_error!("Method `{}` has no self parameter", method_name))?;

        let actual_access = self_param.access(self.sema.db);
        if actual_access != expected_access {
            let expected_str = match expected_access {
                Access::Exclusive => "&mut self",
                Access::Shared => "&self",
                Access::Owned => "self",
            };
            let actual_str = match actual_access {
                Access::Exclusive => "&mut self",
                Access::Shared => "&self",
                Access::Owned => "self",
            };
            fail_match!(
                "Method `{}` takes `{}`, expected `{}`",
                method_name,
                actual_str,
                expected_str
            );
        }

        Ok(())
    }

    fn check_type_constraint(
        &self,
        code: &SyntaxNode,
        type_text: &SmolStr,
    ) -> Result<(), MatchFailed> {
        let resolved = self.resolve_constraint_type(code, type_text)?;
        let node_type = if let Some(expr) = ast::Expr::cast(code.clone()) {
            self.sema
                .type_of_expr(&expr)
                .ok_or_else(|| {
                    match_error!("Failed to get type for expression `{}`", expr.syntax().text())
                })?
                .original
        } else if let Some(pat) = ast::Pat::cast(code.clone()) {
            self.sema
                .type_of_pat(&pat)
                .ok_or_else(|| {
                    match_error!("Failed to get type for pattern `{}`", pat.syntax().text())
                })?
                .original
        } else {
            fail_match!("type() constraints require expression or pattern, got {:?}", code.kind());
        };
        let krate = self.sema.scope(code).map(|it| it.krate()).unwrap_or_else(|| {
            hir::Crate::from(*all_crates(self.sema.db).last().expect("no crate graph present"))
        });
        let display_target = krate.to_display_target(self.sema.db);
        match resolved {
            ResolvedConstraintType::Pattern(pattern) => {
                if !self.matches_type_pattern(&node_type, &pattern) {
                    fail_match!(
                        "Type `{}` did not match `{}`",
                        node_type.display(self.sema.db, display_target),
                        type_text
                    );
                }
            }
            ResolvedConstraintType::Trait(trait_) => {
                if !node_type.impls_trait(self.sema.db, trait_, &[]) {
                    let trait_name = trait_.name(self.sema.db);
                    let trait_name = trait_name.display(self.sema.db, krate.edition(self.sema.db));
                    fail_match!(
                        "Type `{}` does not implement `{}`",
                        node_type.display(self.sema.db, display_target),
                        trait_name
                    );
                }
            }
        }
        Ok(())
    }

    fn resolve_constraint_type(
        &self,
        code: &SyntaxNode,
        type_text: &str,
    ) -> Result<ResolvedConstraintType<'db>, MatchFailed> {
        let type_node =
            fragments::ty(type_text).map_err(|_| match_error!("Invalid type `{type_text}`"))?;
        let ty =
            ast::Type::cast(type_node).ok_or_else(|| match_error!("Invalid type `{type_text}`"))?;
        let scope = self.sema.scope(code).ok_or_else(|| match_error!("No scope for node"))?;

        if let ast::Type::PathType(path_ty) = &ty
            && let Some(path) = path_ty.path()
            && let Some(resolution) = scope.speculative_resolve(&path)
        {
            return match resolution {
                PathResolution::Def(ModuleDef::Trait(trait_)) => {
                    Ok(ResolvedConstraintType::Trait(trait_))
                }
                _ => Ok(ResolvedConstraintType::Pattern(self.type_pattern_from_ast(&scope, &ty)?)),
            };
        }

        Ok(ResolvedConstraintType::Pattern(self.type_pattern_from_ast(&scope, &ty)?))
    }

    fn type_pattern_from_ast(
        &self,
        scope: &hir::SemanticsScope<'db>,
        ty: &ast::Type,
    ) -> Result<TypePattern<'db>, MatchFailed> {
        match ty {
            ast::Type::ParenType(paren) => {
                let inner = paren.ty().ok_or_else(|| match_error!("Invalid type"))?;
                self.type_pattern_from_ast(scope, &inner)
            }
            ast::Type::PathType(path_ty) => {
                let path = path_ty.path().ok_or_else(|| match_error!("Invalid type"))?;
                let arg_types = collect_type_args(&path)?;
                let mut args = Vec::new();
                for arg_ty in arg_types {
                    args.push(self.type_pattern_from_ast(scope, &arg_ty)?);
                }
                let resolution = scope
                    .speculative_resolve(&path)
                    .ok_or_else(|| match_error!("Failed to resolve type"))?;
                match resolution {
                    PathResolution::Def(ModuleDef::Adt(adt)) => Ok(TypePattern::Adt { adt, args }),
                    PathResolution::Def(ModuleDef::TypeAlias(alias)) => {
                        if !args.is_empty() {
                            fail_match!("Unsupported type arguments");
                        }
                        // `instantiate_with_errors` moves the type out of the alias's generic
                        // context; types owned by a definition cannot be unified with types
                        // from the matched code's inference context.
                        Ok(TypePattern::Resolved(alias.ty(self.sema.db).instantiate_with_errors()))
                    }
                    PathResolution::Def(ModuleDef::BuiltinType(builtin)) => {
                        if !args.is_empty() {
                            fail_match!("Unsupported type arguments");
                        }
                        Ok(TypePattern::Resolved(builtin.ty(self.sema.db)))
                    }
                    PathResolution::SelfType(impl_def) => {
                        if !args.is_empty() {
                            fail_match!("Unsupported type arguments");
                        }
                        Ok(TypePattern::Resolved(
                            impl_def.self_ty(self.sema.db).instantiate_with_errors(),
                        ))
                    }
                    PathResolution::Def(ModuleDef::Trait(_)) => {
                        fail_match!("Trait types are not supported here");
                    }
                    _ => fail_match!("Unsupported type"),
                }
            }
            ast::Type::ImplTraitType(impl_ty) => {
                let bounds = impl_ty
                    .type_bound_list()
                    .ok_or_else(|| match_error!("Invalid impl trait bounds"))?;
                Ok(TypePattern::ImplTrait { bounds: self.trait_bounds_from_list(scope, &bounds)? })
            }
            ast::Type::DynTraitType(dyn_ty) => {
                let bounds = dyn_ty
                    .type_bound_list()
                    .ok_or_else(|| match_error!("Invalid dyn trait bounds"))?;
                Ok(TypePattern::DynTrait { bounds: self.trait_bounds_from_list(scope, &bounds)? })
            }
            ast::Type::FnPtrType(fn_ptr) => {
                let qualifiers = FnPtrQualifiers {
                    is_const: fn_ptr.const_token().is_some(),
                    is_async: fn_ptr.async_token().is_some(),
                    is_unsafe: fn_ptr.unsafe_token().is_some(),
                    abi: fn_ptr.abi().and_then(abi_string),
                };
                let param_list = fn_ptr.param_list().ok_or_else(|| match_error!("Invalid fn"))?;
                if param_list.self_param().is_some() {
                    fail_match!("Unsupported fn pointer self param");
                }
                let mut params = Vec::new();
                for param in param_list.params() {
                    if param.dotdotdot_token().is_some() {
                        fail_match!("Unsupported variadic fn pointer");
                    }
                    let param_ty = param.ty().ok_or_else(|| match_error!("Invalid fn param"))?;
                    params.push(self.type_pattern_from_ast(scope, &param_ty)?);
                }
                let ret = match fn_ptr.ret_type().and_then(|ret| ret.ty()) {
                    Some(ret_ty) => Some(Box::new(self.type_pattern_from_ast(scope, &ret_ty)?)),
                    None => None,
                };
                Ok(TypePattern::FnPtr { params, ret, qualifiers })
            }
            ast::Type::RefType(ref_ty) => {
                let inner = ref_ty.ty().ok_or_else(|| match_error!("Invalid type"))?;
                Ok(TypePattern::Ref {
                    mutable: ref_ty.mut_token().is_some(),
                    inner: Box::new(self.type_pattern_from_ast(scope, &inner)?),
                })
            }
            ast::Type::PtrType(ptr_ty) => {
                let inner = ptr_ty.ty().ok_or_else(|| match_error!("Invalid type"))?;
                Ok(TypePattern::RawPtr {
                    inner: Box::new(self.type_pattern_from_ast(scope, &inner)?),
                })
            }
            ast::Type::SliceType(slice_ty) => {
                let inner = slice_ty.ty().ok_or_else(|| match_error!("Invalid type"))?;
                Ok(TypePattern::Slice {
                    inner: Box::new(self.type_pattern_from_ast(scope, &inner)?),
                })
            }
            ast::Type::ArrayType(array_ty) => {
                let inner = array_ty.ty().ok_or_else(|| match_error!("Invalid type"))?;
                let len = array_ty
                    .const_arg()
                    .and_then(|arg| arg.expr())
                    .and_then(|expr| parse_usize_literal(&expr));
                Ok(TypePattern::Array {
                    inner: Box::new(self.type_pattern_from_ast(scope, &inner)?),
                    len,
                })
            }
            ast::Type::TupleType(tuple_ty) => {
                let mut items = Vec::new();
                for item in tuple_ty.fields() {
                    items.push(self.type_pattern_from_ast(scope, &item)?);
                }
                Ok(TypePattern::Tuple { items })
            }
            _ => fail_match!("Unsupported type"),
        }
    }

    fn type_from_ast(
        &self,
        scope: &hir::SemanticsScope<'db>,
        ty: &ast::Type,
    ) -> Result<hir::Type<'db>, MatchFailed> {
        match ty {
            ast::Type::ParenType(paren) => {
                let inner = paren.ty().ok_or_else(|| match_error!("Invalid type"))?;
                self.type_from_ast(scope, &inner)
            }
            ast::Type::PathType(path_ty) => {
                let path = path_ty.path().ok_or_else(|| match_error!("Invalid type"))?;
                let arg_types = collect_type_args(&path)?;
                let mut args = Vec::new();
                for arg_ty in arg_types {
                    args.push(self.type_from_ast(scope, &arg_ty)?);
                }
                let resolution = scope
                    .speculative_resolve(&path)
                    .ok_or_else(|| match_error!("Failed to resolve type"))?;
                match resolution {
                    PathResolution::Def(ModuleDef::Adt(adt)) => {
                        Ok(adt.ty(self.sema.db).instantiate(args).instantiate_with_errors())
                    }
                    PathResolution::Def(ModuleDef::TypeAlias(alias)) => {
                        if !args.is_empty() {
                            fail_match!("Unsupported type arguments");
                        }
                        Ok(alias.ty(self.sema.db).instantiate_with_errors())
                    }
                    PathResolution::Def(ModuleDef::BuiltinType(builtin)) => {
                        if !args.is_empty() {
                            fail_match!("Unsupported type arguments");
                        }
                        Ok(builtin.ty(self.sema.db))
                    }
                    PathResolution::SelfType(impl_def) => {
                        if !args.is_empty() {
                            fail_match!("Unsupported type arguments");
                        }
                        Ok(impl_def.self_ty(self.sema.db).instantiate_with_errors())
                    }
                    _ => fail_match!("Unsupported type"),
                }
            }
            ast::Type::TupleType(tuple_ty) => {
                let mut items = Vec::new();
                for item in tuple_ty.fields() {
                    items.push(self.type_from_ast(scope, &item)?);
                }
                Ok(hir::Type::new_tuple(self.sema.db, &items))
            }
            ast::Type::SliceType(slice_ty) => {
                let inner = slice_ty.ty().ok_or_else(|| match_error!("Invalid type"))?;
                Ok(hir::Type::new_slice(self.sema.db, self.type_from_ast(scope, &inner)?))
            }
            _ => fail_match!("Unsupported type"),
        }
    }

    fn trait_bounds_from_list(
        &self,
        scope: &hir::SemanticsScope<'db>,
        bounds: &ast::TypeBoundList,
    ) -> Result<Vec<TraitBoundPattern<'db>>, MatchFailed> {
        let mut patterns = Vec::new();
        for bound in bounds.bounds() {
            let (for_binder, path_ty) = match bound.kind() {
                Some(ast::TypeBoundKind::PathType(for_binder, path_ty)) => (for_binder, path_ty),
                Some(ast::TypeBoundKind::Lifetime(_)) => {
                    fail_match!("Unsupported lifetime bound");
                }
                Some(ast::TypeBoundKind::Use(_)) => {
                    fail_match!("Unsupported use bound");
                }
                None => fail_match!("Invalid trait bound"),
            };
            if for_binder.is_some() {
                fail_match!("Unsupported for<...> bound");
            }
            let path = path_ty.path().ok_or_else(|| match_error!("Invalid trait bound"))?;
            let segment = path.segment().ok_or_else(|| match_error!("Invalid trait bound"))?;
            if segment.generic_arg_list().is_some() && segment.parenthesized_arg_list().is_some() {
                fail_match!("Unsupported trait bound arguments");
            }
            let arg_types = if segment.parenthesized_arg_list().is_some() {
                Vec::new()
            } else {
                collect_type_args(&path)?
            };
            let mut args = Vec::new();
            for arg_ty in arg_types {
                args.push(self.type_from_ast(scope, &arg_ty)?);
            }
            let fn_sig = if let Some(parenthesized) = segment.parenthesized_arg_list() {
                let mut params = Vec::new();
                for arg in parenthesized.type_args() {
                    let param_ty = arg.ty().ok_or_else(|| match_error!("Invalid fn trait arg"))?;
                    params.push(self.type_pattern_from_ast(scope, &param_ty)?);
                }
                let ret = segment
                    .ret_type()
                    .and_then(|ret| ret.ty())
                    .map(|ret| self.type_pattern_from_ast(scope, &ret))
                    .transpose()?
                    .map(Box::new);
                Some(FnTraitSignature { params, ret })
            } else {
                None
            };
            let trait_ = match scope.speculative_resolve(&path) {
                Some(PathResolution::Def(ModuleDef::Trait(trait_))) => trait_,
                _ => {
                    let name = segment
                        .name_ref()
                        .map(|name| name.text().to_string())
                        .ok_or_else(|| match_error!("Failed to resolve trait"))?;
                    fn_trait_from_name(self.sema.db, scope.krate(), &name)
                        .ok_or_else(|| match_error!("Failed to resolve trait"))?
                }
            };
            patterns.push(TraitBoundPattern { trait_, args, fn_sig });
        }
        Ok(patterns)
    }

    fn matches_type_pattern(&self, expr_type: &hir::Type<'db>, pattern: &TypePattern<'db>) -> bool {
        match pattern {
            TypePattern::Resolved(pattern_type) => {
                expr_type.could_unify_with(self.sema.db, pattern_type)
            }
            TypePattern::Adt { adt, args } => {
                let Some(expr_adt) = expr_type.as_adt() else {
                    return false;
                };
                if &expr_adt != adt {
                    return false;
                }
                if args.is_empty() {
                    return true;
                }
                let expr_args: Vec<_> = expr_type.type_arguments().collect();
                if expr_args.len() != args.len() {
                    return false;
                }
                expr_args
                    .iter()
                    .zip(args)
                    .all(|(arg, pattern)| self.matches_type_pattern(arg, pattern))
            }
            TypePattern::ImplTrait { bounds } => {
                let Some(impl_traits) = expr_type.as_impl_traits(self.sema.db) else {
                    return false;
                };
                let impl_traits: Vec<_> = impl_traits.collect();
                self.matches_trait_bounds(expr_type, bounds, Some(&impl_traits))
            }
            TypePattern::DynTrait { bounds } => {
                let Some(dyn_trait) = expr_type.as_dyn_trait() else {
                    return false;
                };
                if !bounds.is_empty() && !bounds.iter().any(|bound| bound.trait_ == dyn_trait) {
                    return false;
                }
                self.matches_trait_bounds(expr_type, bounds, None)
            }
            TypePattern::FnPtr { params, ret, qualifiers } => {
                let Some(callable) = expr_type.as_callable(self.sema.db) else {
                    return false;
                };
                let mut actual_ret = callable.return_type();
                match callable.kind() {
                    CallableKind::Function(function) => {
                        if qualifiers.is_async != function.is_async(self.sema.db) {
                            return false;
                        }
                        if qualifiers.is_const != function.is_const(self.sema.db) {
                            return false;
                        }
                        if qualifiers.is_unsafe != function.is_unsafe(self.sema.db) {
                            return false;
                        }
                        match qualifiers.is_async {
                            true => match function.async_ret_type(self.sema.db) {
                                Some(async_ret) => actual_ret = async_ret,
                                None => (),
                            },
                            false => (),
                        }
                        let expected_abi = qualifiers.abi.as_deref();
                        let actual_abi = function.abi(self.sema.db);
                        match (expected_abi, actual_abi) {
                            (None, None) => {}
                            (Some(expected), Some(actual)) if actual.as_str() == expected => {}
                            (Some(_), None) | (None, Some(_)) | (Some(_), Some(_)) => {
                                return false;
                            }
                        }
                    }
                    CallableKind::FnPtr => {
                        if qualifiers != &FnPtrQualifiers::default() {
                            return false;
                        }
                    }
                    _ => return false,
                }
                let actual_params = callable.params();
                if actual_params.len() != params.len() {
                    return false;
                }
                if !actual_params
                    .iter()
                    .zip(params)
                    .all(|(param, pattern)| self.matches_type_pattern(param.ty(), pattern))
                {
                    return false;
                }
                match ret {
                    Some(ret) => self.matches_type_pattern(&actual_ret, ret),
                    None => actual_ret.is_unit(),
                }
            }
            TypePattern::Ref { mutable, inner } => {
                let Some((inner_ty, mutability)) = expr_type.as_reference() else {
                    return false;
                };
                if *mutable != matches!(mutability, hir::Mutability::Mut) {
                    return false;
                }
                self.matches_type_pattern(&inner_ty, inner)
            }
            TypePattern::RawPtr { inner } => {
                if !expr_type.is_raw_ptr() {
                    return false;
                }
                let Some(inner_ty) = expr_type.remove_raw_ptr() else {
                    return false;
                };
                self.matches_type_pattern(&inner_ty, inner)
            }
            TypePattern::Slice { inner } => {
                let Some(inner_ty) = expr_type.as_slice() else {
                    return false;
                };
                self.matches_type_pattern(&inner_ty, inner)
            }
            TypePattern::Array { inner, len } => {
                let Some((inner_ty, actual_len)) = expr_type.as_array(self.sema.db) else {
                    return false;
                };
                if let Some(len) = len
                    && *len != actual_len
                {
                    return false;
                }
                self.matches_type_pattern(&inner_ty, inner)
            }
            TypePattern::Tuple { items } => {
                if !expr_type.is_tuple() {
                    return false;
                }
                let fields = expr_type.tuple_fields(self.sema.db);
                if fields.len() != items.len() {
                    return false;
                }
                fields.iter().zip(items).all(|(field, item)| self.matches_type_pattern(field, item))
            }
        }
    }

    fn matches_trait_bounds(
        &self,
        expr_type: &hir::Type<'db>,
        bounds: &[TraitBoundPattern<'db>],
        expected: Option<&[Trait]>,
    ) -> bool {
        bounds.iter().all(|bound| {
            if let Some(expected) = expected {
                let expected_has_exact = expected.iter().any(|trait_| trait_ == &bound.trait_);
                if bound.fn_sig.is_some() {
                    if !expected_has_exact {
                        let expected_has_related = if is_fn_trait(self.sema.db, bound.trait_) {
                            expected.iter().any(|trait_| is_fn_trait(self.sema.db, *trait_))
                        } else if is_async_fn_trait(self.sema.db, bound.trait_) {
                            expected.iter().any(|trait_| is_async_fn_trait(self.sema.db, *trait_))
                        } else {
                            false
                        };
                        if !expected_has_related {
                            return false;
                        }
                    }
                } else if !expected_has_exact {
                    return false;
                }
            }
            if bound.fn_sig.is_none()
                && !expr_type.impls_trait(self.sema.db, bound.trait_, &bound.args)
            {
                return false;
            }
            if let Some(fn_sig) = &bound.fn_sig {
                if !is_fn_trait(self.sema.db, bound.trait_) {
                    return false;
                }
                let Some(callable) = expr_type.as_callable(self.sema.db) else {
                    return false;
                };
                let mut actual_ret = callable.return_type();
                if is_async_fn_trait(self.sema.db, bound.trait_)
                    && let CallableKind::Function(function) = callable.kind()
                    && let Some(async_ret) = function.async_ret_type(self.sema.db)
                {
                    actual_ret = async_ret;
                }
                let actual_params = callable.params();
                if actual_params.len() != fn_sig.params.len() {
                    return false;
                }
                if !actual_params
                    .iter()
                    .zip(&fn_sig.params)
                    .all(|(param, pattern)| self.matches_type_pattern(param.ty(), pattern))
                {
                    return false;
                }
                match &fn_sig.ret {
                    Some(ret) => self.matches_type_pattern(&actual_ret, ret),
                    None => actual_ret.is_unit(),
                }
            } else {
                true
            }
        })
    }

    /// Paths are matched based on whether they refer to the same thing, even if they're written
    /// differently.
    fn attempt_match_path(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SyntaxNode,
        code: &SyntaxNode,
    ) -> Result<(), MatchFailed> {
        if let Some(pattern_resolved) = self.rule.pattern.resolved_paths.get(pattern) {
            let pattern_path = ast::Path::cast(pattern.clone()).unwrap();
            let code_path = ast::Path::cast(code.clone()).unwrap();
            if let (Some(pattern_segment), Some(code_segment)) =
                (pattern_path.segment(), code_path.segment())
            {
                // Match everything within the segment except for the name-ref, which is handled
                // separately via comparing what the path resolves to below.
                self.attempt_match_opt(
                    phase,
                    pattern_segment.generic_arg_list(),
                    code_segment.generic_arg_list(),
                )?;
                self.attempt_match_opt(
                    phase,
                    pattern_segment.parenthesized_arg_list(),
                    code_segment.parenthesized_arg_list(),
                )?;
            }
            if matches!(phase, Phase::Second(_)) {
                let resolution = self
                    .sema
                    .resolve_path(&code_path)
                    .ok_or_else(|| match_error!("Failed to resolve path `{}`", code.text()))?;
                if pattern_resolved.resolution != resolution {
                    fail_match!("Pattern had path `{}` code had `{}`", pattern.text(), code.text());
                }
            }
        } else {
            return self.attempt_match_node_children(phase, pattern, code);
        }
        Ok(())
    }

    fn attempt_match_opt<T: AstNode>(
        &self,
        phase: &mut Phase<'_>,
        pattern: Option<T>,
        code: Option<T>,
    ) -> Result<(), MatchFailed> {
        match (pattern, code) {
            (Some(p), Some(c)) => self.attempt_match_node(phase, p.syntax(), c.syntax()),
            (None, None) => Ok(()),
            (Some(p), None) => fail_match!("Pattern `{}` had nothing to match", p.syntax().text()),
            (None, Some(c)) => {
                fail_match!("Nothing in pattern to match code `{}`", c.syntax().text())
            }
        }
    }

    /// We want to allow the records to match in any order, so we have special matching logic for
    /// them.
    fn attempt_match_record_field_list(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SyntaxNode,
        code: &SyntaxNode,
    ) -> Result<(), MatchFailed> {
        // Build a map keyed by field name.
        let mut fields_by_name: FxHashMap<SmolStr, SyntaxNode> = FxHashMap::default();
        for child in code.children() {
            if let Some(record) = ast::RecordExprField::cast(child.clone())
                && let Some(name) = record.field_name()
            {
                fields_by_name.insert(name.text().into(), child.clone());
            }
        }
        for p in pattern.children_with_tokens() {
            if let SyntaxElement::Node(p) = p
                && let Some(name_element) = p.first_child_or_token()
            {
                if self.get_placeholder(&name_element).is_some() {
                    // If the pattern is using placeholders for field names then order
                    // independence doesn't make sense. Fall back to regular ordered
                    // matching.
                    return self.attempt_match_node_children(phase, pattern, code);
                }
                if let Some(ident) = only_ident(name_element) {
                    let code_record = fields_by_name.remove(ident.text()).ok_or_else(|| {
                        match_error!("Placeholder has record field '{}', but code doesn't", ident)
                    })?;
                    self.attempt_match_node(phase, &p, &code_record)?;
                }
            }
        }
        if let Some(unmatched_fields) = fields_by_name.keys().next() {
            fail_match!(
                "{} field(s) of a record literal failed to match, starting with {}",
                fields_by_name.len(),
                unmatched_fields
            );
        }
        Ok(())
    }

    /// Outside of token trees, a placeholder can only match a single AST node, whereas in a token
    /// tree it can match a sequence of tokens. Note, that this code will only be used when the
    /// pattern matches the macro invocation. For matches within the macro call, we'll already have
    /// expanded the macro.
    fn attempt_match_token_tree(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SyntaxNode,
        code: &syntax::SyntaxNode,
    ) -> Result<(), MatchFailed> {
        let mut pattern = PatternIterator::new(pattern).peekable();
        let mut children = code.children_with_tokens();
        while let Some(child) = children.next() {
            if let Some(placeholder) = pattern.peek().and_then(|p| self.get_placeholder(p)) {
                pattern.next();
                let next_pattern_token = pattern
                    .peek()
                    .and_then(|p| match p {
                        SyntaxElement::Token(t) => Some(t.clone()),
                        SyntaxElement::Node(n) => n.first_token(),
                    })
                    .map(|p| p.text().to_owned());
                let first_matched_token = child.clone();
                let mut last_matched_token = child;
                // Read code tokens util we reach one equal to the next token from our pattern
                // or we reach the end of the token tree.
                for next in &mut children {
                    match &next {
                        SyntaxElement::Token(t) => {
                            if Some(t.to_string()) == next_pattern_token {
                                pattern.next();
                                break;
                            }
                        }
                        SyntaxElement::Node(n) => {
                            if let Some(first_token) = n.first_token()
                                && Some(first_token.text()) == next_pattern_token.as_deref()
                                && let Some(SyntaxElement::Node(p)) = pattern.next()
                            {
                                // We have a subtree that starts with the next token in our pattern.
                                self.attempt_match_token_tree(phase, &p, n)?;
                                break;
                            }
                        }
                    };
                    last_matched_token = next;
                }
                if let Phase::Second(match_out) = phase {
                    match_out.placeholder_values.insert(
                        placeholder.ident.clone(),
                        PlaceholderMatch::from_range(FileRange {
                            file_id: self
                                .sema
                                .original_range_opt(code)
                                .ok_or(MatchFailed {
                                    reason: Some("def site definition".to_owned()),
                                })?
                                .file_id,
                            range: first_matched_token
                                .text_range()
                                .cover(last_matched_token.text_range()),
                        }),
                    );
                }
                continue;
            }
            // Match literal (non-placeholder) tokens.
            match child {
                SyntaxElement::Token(token) => {
                    self.attempt_match_token(phase, &mut pattern, &token)?;
                }
                SyntaxElement::Node(node) => match pattern.next() {
                    Some(SyntaxElement::Node(p)) => {
                        self.attempt_match_token_tree(phase, &p, &node)?;
                    }
                    Some(SyntaxElement::Token(p)) => fail_match!(
                        "Pattern has token '{}', code has subtree '{}'",
                        p.text(),
                        node.text()
                    ),
                    None => fail_match!("Pattern has nothing, code has '{}'", node.text()),
                },
            }
        }
        if let Some(p) = pattern.next() {
            fail_match!("Reached end of token tree in code, but pattern still has {:?}", p);
        }
        Ok(())
    }

    fn attempt_match_ufcs_to_method_call(
        &self,
        phase: &mut Phase<'_>,
        pattern_ufcs: &UfcsCallInfo<'db>,
        code: &ast::MethodCallExpr,
    ) -> Result<(), MatchFailed> {
        use ast::HasArgList;
        let code_resolved_function = self
            .sema
            .resolve_method_call(code)
            .ok_or_else(|| match_error!("Failed to resolve method call"))?;
        if pattern_ufcs.function != code_resolved_function {
            fail_match!("Method call resolved to a different function");
        }
        // Check arguments.
        let mut pattern_args = pattern_ufcs
            .call_expr
            .arg_list()
            .ok_or_else(|| match_error!("Pattern function call has no args"))?
            .args();
        // If the function we're calling takes a self parameter, then we store additional
        // information on the placeholder match about autoderef and autoref. This allows us to use
        // the placeholder in a context where autoderef and autoref don't apply.
        if code_resolved_function.self_param(self.sema.db).is_some() {
            if let (Some(pattern_type), Some(expr)) =
                (&pattern_ufcs.qualifier_type, &code.receiver())
            {
                let deref_count = self.check_expr_type(pattern_type, expr)?;
                let pattern_receiver = pattern_args.next();
                self.attempt_match_opt(phase, pattern_receiver.clone(), code.receiver())?;
                if let Phase::Second(match_out) = phase
                    && let Some(placeholder_value) = pattern_receiver
                        .and_then(|n| self.get_placeholder_for_node(n.syntax()))
                        .and_then(|placeholder| {
                            match_out.placeholder_values.get_mut(&placeholder.ident)
                        })
                {
                    placeholder_value.autoderef_count = deref_count;
                    placeholder_value.autoref_kind = self
                        .sema
                        .resolve_method_call_as_callable(code)
                        .and_then(|callable| {
                            let (self_param, _) = callable.receiver_param(self.sema.db)?;
                            Some(self.sema.source(self_param)?.value.kind())
                        })
                        .unwrap_or(ast::SelfParamKind::Owned);
                }
            }
        } else {
            self.attempt_match_opt(phase, pattern_args.next(), code.receiver())?;
        }
        let mut code_args =
            code.arg_list().ok_or_else(|| match_error!("Code method call has no args"))?.args();
        loop {
            match (pattern_args.next(), code_args.next()) {
                (None, None) => return Ok(()),
                (p, c) => self.attempt_match_opt(phase, p, c)?,
            }
        }
    }

    fn attempt_match_ufcs_to_ufcs(
        &self,
        phase: &mut Phase<'_>,
        pattern_ufcs: &UfcsCallInfo<'db>,
        code: &ast::CallExpr,
    ) -> Result<(), MatchFailed> {
        use ast::HasArgList;
        // Check that the first argument is the expected type.
        if let (Some(pattern_type), Some(expr)) = (
            &pattern_ufcs.qualifier_type,
            &code.arg_list().and_then(|code_args| code_args.args().next()),
        ) {
            self.check_expr_type(pattern_type, expr)?;
        }
        self.attempt_match_node_children(phase, pattern_ufcs.call_expr.syntax(), code.syntax())
    }

    /// Verifies that `expr` matches `pattern_type`, possibly after dereferencing some number of
    /// times. Returns the number of times it needed to be dereferenced.
    fn check_expr_type(
        &self,
        pattern_type: &hir::Type<'db>,
        expr: &ast::Expr,
    ) -> Result<usize, MatchFailed> {
        use hir::HirDisplay;
        let code_type = self
            .sema
            .type_of_expr(expr)
            .ok_or_else(|| {
                match_error!("Failed to get receiver type for `{}`", expr.syntax().text())
            })?
            .original;
        let krate = self.sema.scope(expr.syntax()).map(|it| it.krate()).unwrap_or_else(|| {
            hir::Crate::from(*all_crates(self.sema.db).last().expect("no crate graph present"))
        });

        code_type
            .autoderef(self.sema.db)
            .enumerate()
            .find(|(_, deref_code_type)| pattern_type == deref_code_type)
            .map(|(count, _)| count)
            .ok_or_else(|| {
                let display_target = krate.to_display_target(self.sema.db);
                // Temporary needed to make the borrow checker happy.
                match_error!(
                    "Pattern type `{}` didn't match code type `{}`",
                    pattern_type.display(self.sema.db, display_target),
                    code_type.display(self.sema.db, display_target)
                )
            })
    }

    fn get_placeholder_for_node(&self, node: &SyntaxNode) -> Option<&Placeholder> {
        self.get_placeholder(&SyntaxElement::Node(node.clone()))
    }

    fn get_placeholder(&self, element: &SyntaxElement) -> Option<&Placeholder> {
        // Try as normal placeholder (IDENT)
        if let Some(placeholder) =
            only_ident(element.clone()).and_then(|ident| self.rule.get_placeholder(&ident))
        {
            return Some(placeholder);
        }
        // Try as lifetime placeholder (LIFETIME)
        if let Some(placeholder) = only_lifetime(element.clone())
            .and_then(|lifetime| self.rule.get_lifetime_placeholder(&lifetime))
        {
            return Some(placeholder);
        }
        None
    }
}

fn parse_usize_literal(expr: &ast::Expr) -> Option<usize> {
    let literal = ast::Literal::cast(expr.syntax().clone())?;
    let token = literal.token();
    let text = token.text();
    if text.starts_with('-') {
        return None;
    }
    text.parse().ok()
}

fn abi_string(abi: ast::Abi) -> Option<SmolStr> {
    if let Some(token) = abi.string_token() {
        let text = token.text();
        Some(text.trim_matches('"').into())
    } else {
        Some("C".into())
    }
}

fn is_async_fn_trait(db: &dyn hir::db::HirDatabase, trait_: Trait) -> bool {
    matches!(trait_.name(db).as_str(), "AsyncFn" | "AsyncFnMut" | "AsyncFnOnce")
}

fn is_fn_trait(db: &dyn hir::db::HirDatabase, trait_: Trait) -> bool {
    matches!(
        trait_.name(db).as_str(),
        "Fn" | "FnMut" | "FnOnce" | "AsyncFn" | "AsyncFnMut" | "AsyncFnOnce"
    )
}

fn fn_trait_from_name(
    db: &dyn hir::db::HirDatabase,
    krate: hir::Crate,
    name: &str,
) -> Option<Trait> {
    let fn_trait = match name {
        "FnOnce" => hir::FnTrait::FnOnce,
        "FnMut" => hir::FnTrait::FnMut,
        "Fn" => hir::FnTrait::Fn,
        "AsyncFnOnce" => hir::FnTrait::AsyncFnOnce,
        "AsyncFnMut" => hir::FnTrait::AsyncFnMut,
        "AsyncFn" => hir::FnTrait::AsyncFn,
        _ => return None,
    };
    fn_trait.get_id(db, krate)
}

fn collect_type_args(path: &ast::Path) -> Result<Vec<ast::Type>, MatchFailed> {
    let mut segments: Vec<_> = path.segments().collect();
    let Some(last) = segments.pop() else {
        return Ok(Vec::new());
    };
    for segment in segments {
        if segment.generic_arg_list().is_some() || segment.parenthesized_arg_list().is_some() {
            fail_match!("Unsupported type arguments");
        }
    }
    if last.parenthesized_arg_list().is_some() {
        fail_match!("Unsupported type arguments");
    }
    let Some(arg_list) = last.generic_arg_list() else {
        return Ok(Vec::new());
    };
    let mut out = Vec::new();
    for arg in arg_list.generic_args() {
        match arg {
            ast::GenericArg::TypeArg(arg) => {
                let ty = arg.ty().ok_or_else(|| match_error!("Invalid type argument"))?;
                out.push(ty);
            }
            _ => fail_match!("Unsupported type arguments"),
        }
    }
    Ok(out)
}

impl Match {
    fn render_template_paths<'db>(
        &mut self,
        template: &ResolvedPattern<'db>,
        sema: &Semantics<'db, ide_db::RootDatabase>,
    ) -> Result<(), MatchFailed> {
        let module = sema
            .scope(&self.matched_node)
            .ok_or_else(|| match_error!("Matched node isn't in a module"))?
            .module();
        for (path, resolved_path) in &template.resolved_paths {
            if let hir::PathResolution::Def(module_def) = resolved_path.resolution {
                let cfg = FindPathConfig {
                    prefer_no_std: false,
                    prefer_prelude: true,
                    prefer_absolute: false,
                    allow_unstable: true,
                };
                let mod_path = module.find_path(sema.db, module_def, cfg).ok_or_else(|| {
                    match_error!("Failed to render template path `{}` at match location")
                })?;
                self.rendered_template_paths.insert(path.clone(), mod_path);
            }
        }
        Ok(())
    }
}

impl Phase<'_> {
    fn next_non_trivial(&mut self, code_it: &mut SyntaxElementChildren) -> Option<SyntaxElement> {
        loop {
            let c = code_it.next();
            if let Some(SyntaxElement::Token(t)) = &c {
                self.record_ignored_comments(t);
                if t.kind().is_trivia() {
                    continue;
                }
            }
            return c;
        }
    }

    fn record_ignored_comments(&mut self, token: &SyntaxToken) {
        if token.kind() == SyntaxKind::COMMENT
            && let Phase::Second(match_out) = self
            && let Some(comment) = ast::Comment::cast(token.clone())
        {
            match_out.ignored_comments.push(comment);
        }
    }
}

fn is_closing_token(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::R_PAREN || kind == SyntaxKind::R_CURLY || kind == SyntaxKind::R_BRACK
}

pub(crate) fn record_match_fails_reasons_scope<F, T>(debug_active: bool, f: F) -> T
where
    F: Fn() -> T,
{
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.set(debug_active));
    let res = f();
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.set(false));
    res
}

// For performance reasons, we don't want to record the reason why every match fails, only the bit
// of code that the user indicated they thought would match. We use a thread local to indicate when
// we are trying to match that bit of code. This saves us having to pass a boolean into all the bits
// of code that can make the decision to not match.
thread_local! {
    pub static RECORDING_MATCH_FAIL_REASONS: Cell<bool> = const { Cell::new(false) };
}

fn recording_match_fail_reasons() -> bool {
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.get())
}

impl PlaceholderMatch {
    fn from_range(range: FileRange) -> Self {
        Self {
            range,
            inner_matches: SsrMatches::default(),
            autoderef_count: 0,
            autoref_kind: ast::SelfParamKind::Owned,
        }
    }
}

impl Constraint {
    fn needs_type_inference(&self) -> bool {
        match self {
            Constraint::Kind(_) => false,
            Constraint::Context(_) => false,
            Constraint::Type(_) => true,
            Constraint::Not(inner) => inner.needs_type_inference(),
        }
    }
}

impl NodeKind {
    fn matches(&self, node: &SyntaxNode) -> Result<(), MatchFailed> {
        let ok = match self {
            Self::Literal => {
                cov_mark::hit!(literal_constraint);
                ast::Literal::can_cast(node.kind())
            }
            Self::FieldExpr => ast::FieldExpr::can_cast(node.kind()),
            Self::MethodCall => ast::MethodCallExpr::can_cast(node.kind()),
            Self::CallExpr => ast::CallExpr::can_cast(node.kind()),
        };
        if !ok {
            fail_match!("Code '{}' isn't of kind {:?}", node.text(), self);
        }
        Ok(())
    }
}

impl ContextKind {
    fn matches(&self, node: &SyntaxNode) -> Result<(), MatchFailed> {
        let Some(parent) = node.parent() else {
            fail_match!("Code '{}' has no parent for context check {:?}", node.text(), self);
        };
        let ok = match self {
            Self::Receiver => {
                if let Some(method_call) = ast::MethodCallExpr::cast(parent) {
                    method_call.receiver().is_some_and(|receiver| receiver.syntax() == node)
                } else {
                    false
                }
            }
            Self::Argument => {
                if let Some(arg_list) = ast::ArgList::cast(parent) {
                    arg_list.args().any(|arg| arg.syntax() == node)
                } else {
                    false
                }
            }
            Self::Lhs => {
                if let Some(bin_expr) = ast::BinExpr::cast(parent) {
                    bin_expr.op_kind() == Some(ast::BinaryOp::Assignment { op: None })
                        && bin_expr.lhs().is_some_and(|lhs| lhs.syntax() == node)
                } else {
                    false
                }
            }
        };
        if !ok {
            fail_match!("Code '{}' is not in context {:?}", node.text(), self);
        }
        Ok(())
    }
}

fn only_ident(element: SyntaxElement) -> Option<SyntaxToken> {
    match element {
        SyntaxElement::Token(t) => {
            if t.kind() == SyntaxKind::IDENT {
                return Some(t);
            }
        }
        SyntaxElement::Node(n) => {
            let mut children = n.children_with_tokens();
            if let (Some(only_child), None) = (children.next(), children.next()) {
                return only_ident(only_child);
            }
        }
    }
    None
}

fn only_lifetime(element: SyntaxElement) -> Option<SyntaxToken> {
    match element {
        SyntaxElement::Token(t) => {
            if t.kind() == SyntaxKind::LIFETIME {
                return Some(t);
            }
        }
        SyntaxElement::Node(n) => {
            let mut children = n.children_with_tokens();
            if let (Some(only_child), None) = (children.next(), children.next()) {
                return only_lifetime(only_child);
            }
        }
    }
    None
}

struct PatternIterator {
    iter: SyntaxElementChildren,
}

impl Iterator for PatternIterator {
    type Item = SyntaxElement;

    fn next(&mut self) -> Option<SyntaxElement> {
        self.iter.find(|element| !element.kind().is_trivia())
    }
}

impl PatternIterator {
    fn new(parent: &SyntaxNode) -> Self {
        Self { iter: parent.children_with_tokens() }
    }
}

#[cfg(test)]
mod tests {
    use crate::{MatchFinder, SsrRule};

    #[test]
    fn parse_match_replace() {
        let rule: SsrRule = "foo($x) ==>> bar($x)".parse().unwrap();
        let input = "fn foo() {} fn bar() {} fn main() { foo(1+2); }";

        let (db, position, selections) = crate::tests::single_file(input);
        hir::attach_db(&db, || {
            let position = ide_db::FilePosition {
                file_id: position.file_id.file_id(&db),
                offset: position.offset,
            };
            let mut match_finder = MatchFinder::in_context(
                &db,
                position,
                selections
                    .into_iter()
                    .map(|frange| ide_db::FileRange {
                        file_id: frange.file_id.file_id(&db),
                        range: frange.range,
                    })
                    .collect(),
            )
            .unwrap();
            match_finder.add_rule(rule).unwrap();
            let matches = match_finder.matches();
            assert_eq!(matches.matches.len(), 1);
            assert_eq!(matches.matches[0].matched_node.text(), "foo(1+2)");
            assert_eq!(matches.matches[0].placeholder_values.len(), 1);

            let edits = match_finder.edits();
            assert_eq!(edits.len(), 1);
            let edit = &edits[&position.file_id];
            let mut after = input.to_owned();
            edit.apply(&mut after);
            assert_eq!(after, "fn foo() {} fn bar() {} fn main() { bar(1+2); }");
        });
    }
}
