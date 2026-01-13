//! Structured metadata extracted from SSR matches for filtering and rendering.

use crate::{Match, SsrMatches};
use hir::Semantics;
use ide_db::{FileId, RootDatabase};
use syntax::ast::HasName;
use syntax::{AstNode, SyntaxKind, SyntaxNode, TextRange, ast};

#[derive(Debug, Clone)]
pub struct StructuredMatch {
    pub file_id: FileId,
    pub range: TextRange,
    pub matched_text: String,
    pub context: MatchContext,
    pub usage_kind: UsageKind,
}

#[derive(Debug, Clone, Default)]
pub struct MatchContext {
    pub enclosing_function: Option<String>,
    pub enclosing_impl: Option<String>,
    pub is_test: bool,
    pub is_unsafe: bool,
    pub is_async: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageKind {
    ForLoop,
    WhileLet,
    IteratorMap,
    IteratorFilter,
    IteratorForEach,
    IteratorChain,
    FunctionArg,
    MethodArg,
    Return,
    LetBinding,
    Assignment,
    FieldInit,
    MatchArm,
    IfLet,
    MethodReceiver,
    Other,
}

impl StructuredMatch {
    pub fn from_match(m: &Match, sema: &Semantics<'_, RootDatabase>) -> Self {
        StructuredMatch {
            file_id: m.range.file_id.file_id(sema.db),
            range: m.range.range,
            matched_text: m.matched_node.text().to_string(),
            context: match_context(sema, &m.matched_node),
            usage_kind: classify_usage(&m.matched_node),
        }
    }
}

impl SsrMatches {
    pub fn structured(&self, sema: &Semantics<'_, RootDatabase>) -> Vec<StructuredMatch> {
        self.matches.iter().map(|m| StructuredMatch::from_match(m, sema)).collect()
    }
}

fn match_context(sema: &Semantics<'_, RootDatabase>, node: &SyntaxNode) -> MatchContext {
    let mut context = MatchContext::default();

    for ancestor in sema.ancestors_with_macros(node.clone()) {
        if context.enclosing_function.is_none()
            && let Some(func) = ast::Fn::cast(ancestor.clone())
        {
            context.enclosing_function = func.name().map(|name| name.text().to_string());
            context.is_unsafe = func.unsafe_token().is_some();
            match sema.to_def(&func) {
                Some(def) => {
                    context.is_test = def.is_test(sema.db);
                    context.is_async = def.is_async(sema.db);
                }
                None => context.is_async = func.async_token().is_some(),
            }
        }

        if context.enclosing_impl.is_none()
            && let Some(impl_def) = ast::Impl::cast(ancestor.clone())
        {
            context.enclosing_impl = impl_def.self_ty().map(|ty| ty.syntax().text().to_string());
            if let Some(def) = sema.to_def(&impl_def) {
                context.is_unsafe |= def.is_unsafe(sema.db);
            } else {
                context.is_unsafe |= impl_def.unsafe_token().is_some();
            }
        }

        if context.enclosing_function.is_some() && context.enclosing_impl.is_some() {
            break;
        }
    }

    context
}

fn classify_usage(node: &SyntaxNode) -> UsageKind {
    let Some(parent) = node.parent() else {
        return UsageKind::Other;
    };

    match parent.kind() {
        SyntaxKind::FOR_EXPR => return UsageKind::ForLoop,
        SyntaxKind::WHILE_EXPR => {
            // The only `let` child of a while is a `while let` condition; plain conditions
            // and loop bodies fall through.
            if ast::LetExpr::can_cast(node.kind()) {
                return UsageKind::WhileLet;
            }
        }
        SyntaxKind::RETURN_EXPR => return UsageKind::Return,
        SyntaxKind::LET_STMT => return UsageKind::LetBinding,
        SyntaxKind::MATCH_ARM => return UsageKind::MatchArm,
        SyntaxKind::IF_EXPR => {
            if ast::LetExpr::can_cast(node.kind()) {
                return UsageKind::IfLet;
            }
        }
        SyntaxKind::RECORD_EXPR_FIELD => return UsageKind::FieldInit,
        SyntaxKind::BIN_EXPR => match ast::BinExpr::cast(parent.clone()) {
            Some(bin_expr)
                if bin_expr.op_kind() == Some(ast::BinaryOp::Assignment { op: None }) =>
            {
                return UsageKind::Assignment;
            }
            Some(_) | None => (),
        },
        SyntaxKind::METHOD_CALL_EXPR => {
            if let Some(method_call) = ast::MethodCallExpr::cast(parent.clone()) {
                let is_receiver = method_call.receiver().is_some_and(|r| r.syntax() == node);
                if is_receiver {
                    return UsageKind::MethodReceiver;
                }
                return UsageKind::MethodArg;
            }
        }
        SyntaxKind::ARG_LIST => {
            let Some(grandparent) = parent.parent() else {
                return UsageKind::Other;
            };
            if ast::MethodCallExpr::cast(grandparent.clone()).is_some() {
                return UsageKind::MethodArg;
            }
            if ast::CallExpr::cast(grandparent).is_some() {
                return UsageKind::FunctionArg;
            }
        }
        _ => {}
    }

    match parent.parent().and_then(ast::MethodCallExpr::cast) {
        Some(method_call) => {
            let method_name = method_call.name_ref().map(|n| n.text().to_string());
            match method_name.as_deref() {
                Some("map") => return UsageKind::IteratorMap,
                Some("filter") => return UsageKind::IteratorFilter,
                Some("for_each") => return UsageKind::IteratorForEach,
                Some("chain") => return UsageKind::IteratorChain,
                Some(_) | None => (),
            }
        }
        None => (),
    }

    UsageKind::Other
}
