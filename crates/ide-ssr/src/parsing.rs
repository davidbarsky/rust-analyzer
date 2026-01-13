//! This file contains code for parsing SSR rules, which look something like `foo($a) ==>> bar($b)`.
//! We first split everything before and after the separator `==>>`. Next, both the search pattern
//! and the replacement template get tokenized by the Rust tokenizer. Tokens are then searched for
//! placeholders, which start with `$`. For replacement templates, this is the final form. For
//! search patterns, we go further and parse the pattern as each kind of thing that we can match.
//! e.g. expressions, type references etc.
use ide_db::{FxHashMap, FxHashSet};
use std::{fmt::Display, str::FromStr};
use syntax::{SmolStr, SyntaxKind, SyntaxNode, T};

use crate::errors::bail;
use crate::{SsrError, SsrPattern, SsrRule, fragments};

#[derive(Debug)]
pub(crate) struct ParsedRule {
    pub(crate) placeholders_by_stand_in: FxHashMap<SmolStr, Placeholder>,
    pub(crate) pattern: SyntaxNode,
    pub(crate) template: Option<SyntaxNode>,
    pub(crate) where_clause: WhereClause,
}

#[derive(Debug)]
pub(crate) struct RawPattern {
    tokens: Vec<PatternElement>,
}

// Part of a search or replace pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum PatternElement {
    Token(Token),
    Placeholder(Placeholder),
}

/// What kind of syntax element a placeholder matches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PlaceholderKind {
    /// Matches expressions, types, patterns, items (the default).
    Normal,
    /// Matches lifetimes ('a, 'static, etc.). Use `$'name` syntax.
    Lifetime,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Placeholder {
    /// The name of this placeholder. e.g. for "$a", this would be "a"
    pub(crate) ident: Var,
    /// A unique name used in place of this placeholder when we parse the pattern as Rust code.
    stand_in_name: String,
    pub(crate) constraints: Vec<Constraint>,
}

/// Represents a `$var` in an SSR query.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Var(pub(crate) String);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Constraint {
    Kind(NodeKind),
    Type(SmolStr),
    Not(Box<Constraint>),
    Context(ContextKind),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ContextKind {
    Receiver,
    Argument,
    Lhs,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum NodeKind {
    Literal,
    FieldExpr,
    MethodCall,
    CallExpr,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct WhereClause {
    pub(crate) conditions: Vec<WhereCondition>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum WhereCondition {
    Kind(NodeKind),
    Type(SmolStr),
    Context(ContextKind),
    Not(Box<WhereCondition>),
    Or(Vec<WhereCondition>),
    PlaceholderOneOf { placeholder: Var, values: Vec<SmolStr> },
    PlaceholderEq { placeholder: Var, value: SmolStr },
    PlaceholderMutSelf { placeholder: Var },
    PlaceholderRefSelf { placeholder: Var },
    PlaceholderOwnedSelf { placeholder: Var },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token {
    kind: SyntaxKind,
    pub(crate) text: SmolStr,
}

impl ParsedRule {
    fn new(
        pattern: &RawPattern,
        template: Option<&RawPattern>,
        where_clause: WhereClause,
    ) -> Result<Vec<ParsedRule>, SsrError> {
        let raw_pattern = pattern.as_rust_code();
        let raw_template = template.map(|t| t.as_rust_code());
        let raw_template = raw_template.as_deref();
        let mut builder = RuleBuilder {
            placeholders_by_stand_in: pattern.placeholders_by_stand_in(),
            rules: Vec::new(),
            where_clause,
        };

        let raw_template_stmt = raw_template.map(fragments::stmt);
        if let raw_template_expr @ Some(Ok(_)) = raw_template.map(fragments::expr) {
            builder.try_add(fragments::expr(&raw_pattern), raw_template_expr);
        } else {
            builder.try_add(fragments::expr(&raw_pattern), raw_template_stmt.clone());
        }
        builder.try_add(fragments::ty(&raw_pattern), raw_template.map(fragments::ty));
        builder.try_add(fragments::item(&raw_pattern), raw_template.map(fragments::item));
        builder.try_add(fragments::pat(&raw_pattern), raw_template.map(fragments::pat));
        builder.try_add(fragments::stmt(&raw_pattern), raw_template_stmt);
        builder.build()
    }
}

struct RuleBuilder {
    placeholders_by_stand_in: FxHashMap<SmolStr, Placeholder>,
    rules: Vec<ParsedRule>,
    where_clause: WhereClause,
}

impl RuleBuilder {
    fn try_add(
        &mut self,
        pattern: Result<SyntaxNode, ()>,
        template: Option<Result<SyntaxNode, ()>>,
    ) {
        match (pattern, template) {
            (Ok(pattern), Some(Ok(template))) => self.rules.push(ParsedRule {
                placeholders_by_stand_in: self.placeholders_by_stand_in.clone(),
                pattern,
                template: Some(template),
                where_clause: self.where_clause.clone(),
            }),
            (Ok(pattern), None) => self.rules.push(ParsedRule {
                placeholders_by_stand_in: self.placeholders_by_stand_in.clone(),
                pattern,
                template: None,
                where_clause: self.where_clause.clone(),
            }),
            _ => {}
        }
    }

    fn build(mut self) -> Result<Vec<ParsedRule>, SsrError> {
        if self.rules.is_empty() {
            bail!("Not a valid Rust expression, type, item, path or pattern");
        }
        // If any rules contain paths, then we reject any rules that don't contain paths. Allowing a
        // mix leads to strange semantics, since the path-based rules only match things where the
        // path refers to semantically the same thing, whereas the non-path-based rules could match
        // anything. Specifically, if we have a rule like `foo ==>> bar` we only want to match the
        // `foo` that is in the current scope, not any `foo`. However "foo" can be parsed as a
        // pattern (IDENT_PAT -> NAME -> IDENT). Allowing such a rule through would result in
        // renaming everything called `foo` to `bar`. It'd also be slow, since without a path, we'd
        // have to use the slow-scan search mechanism.
        if self.rules.iter().any(|rule| contains_path(&rule.pattern)) {
            let old_len = self.rules.len();
            self.rules.retain(|rule| contains_path(&rule.pattern));
            if self.rules.len() < old_len {
                cov_mark::hit!(pattern_is_a_single_segment_path);
            }
        }
        Ok(self.rules)
    }
}

/// Returns whether there are any paths in `node`.
fn contains_path(node: &SyntaxNode) -> bool {
    node.kind() == SyntaxKind::PATH
        || node.descendants().any(|node| node.kind() == SyntaxKind::PATH)
}

impl FromStr for SsrRule {
    type Err = SsrError;

    fn from_str(query: &str) -> Result<SsrRule, SsrError> {
        let mut it = query.split("==>>");
        let pattern_with_where = it.next().expect("at least empty string").trim();
        let template = it
            .next()
            .ok_or_else(|| SsrError("Cannot find delimiter `==>>`".into()))?
            .trim()
            .to_owned();
        if it.next().is_some() {
            return Err(SsrError("More than one delimiter found".into()));
        }
        let (pattern, where_clause) = extract_where_clause(pattern_with_where)?;
        let raw_pattern = pattern.parse()?;
        let raw_template = template.parse()?;
        let parsed_rules = ParsedRule::new(&raw_pattern, Some(&raw_template), where_clause)?;
        let rule = SsrRule { pattern: raw_pattern, template: raw_template, parsed_rules };
        validate_rule(&rule)?;
        Ok(rule)
    }
}

impl FromStr for RawPattern {
    type Err = SsrError;

    fn from_str(pattern_str: &str) -> Result<RawPattern, SsrError> {
        Ok(RawPattern { tokens: parse_pattern(pattern_str)? })
    }
}

impl RawPattern {
    /// Returns this search pattern as Rust source code that we can feed to the Rust parser.
    fn as_rust_code(&self) -> String {
        let mut res = String::new();
        for t in &self.tokens {
            res.push_str(match t {
                PatternElement::Token(token) => token.text.as_str(),
                PatternElement::Placeholder(placeholder) => placeholder.stand_in_name.as_str(),
            });
        }
        res
    }

    pub(crate) fn placeholders_by_stand_in(&self) -> FxHashMap<SmolStr, Placeholder> {
        let mut res = FxHashMap::default();
        for t in &self.tokens {
            if let PatternElement::Placeholder(placeholder) = t {
                res.insert(SmolStr::new(&placeholder.stand_in_name), placeholder.clone());
            }
        }
        res
    }
}

impl FromStr for SsrPattern {
    type Err = SsrError;

    fn from_str(pattern_str: &str) -> Result<SsrPattern, SsrError> {
        let (pattern, where_clause) = extract_where_clause(pattern_str)?;
        let raw_pattern = pattern.parse()?;
        let parsed_rules = ParsedRule::new(&raw_pattern, None, where_clause)?;
        Ok(SsrPattern { parsed_rules })
    }
}

/// Returns `pattern_str`, parsed as a search or replace pattern. If `remove_whitespace` is true,
/// then any whitespace tokens will be removed, which we do for the search pattern, but not for the
/// replace pattern.
fn parse_pattern(pattern_str: &str) -> Result<Vec<PatternElement>, SsrError> {
    let mut res = Vec::new();
    let mut placeholder_names = FxHashSet::default();
    let mut tokens = tokenize(pattern_str)?.into_iter();
    while let Some(token) = tokens.next() {
        if token.kind == T![$] {
            let placeholder = parse_placeholder(&mut tokens)?;
            if !placeholder_names.insert(placeholder.ident.clone()) {
                bail!("Placeholder `{}` repeats more than once", placeholder.ident);
            }
            res.push(PatternElement::Placeholder(placeholder));
        } else {
            res.push(PatternElement::Token(token));
        }
    }
    Ok(res)
}

/// Checks for errors in a rule. e.g. the replace pattern referencing placeholders that the search
/// pattern didn't define.
fn validate_rule(rule: &SsrRule) -> Result<(), SsrError> {
    let mut defined_placeholders = FxHashSet::default();
    for p in &rule.pattern.tokens {
        if let PatternElement::Placeholder(placeholder) = p {
            defined_placeholders.insert(&placeholder.ident);
        }
    }
    let mut undefined = Vec::new();
    for p in &rule.template.tokens {
        if let PatternElement::Placeholder(placeholder) = p {
            if !defined_placeholders.contains(&placeholder.ident) {
                undefined.push(placeholder.ident.to_string());
            }
            if !placeholder.constraints.is_empty() {
                bail!("Replacement placeholders cannot have constraints");
            }
        }
    }
    if !undefined.is_empty() {
        bail!("Replacement contains undefined placeholders: {}", undefined.join(", "));
    }
    Ok(())
}

fn tokenize(source: &str) -> Result<Vec<Token>, SsrError> {
    let lexed = parser::LexedStr::new(parser::Edition::CURRENT, source);
    if let Some((_, first_error)) = lexed.errors().next() {
        bail!("Failed to parse pattern: {}", first_error);
    }
    let mut tokens: Vec<Token> = Vec::new();
    for i in 0..lexed.len() {
        tokens.push(Token { kind: lexed.kind(i), text: lexed.text(i).into() });
    }
    Ok(tokens)
}

fn parse_placeholder(tokens: &mut std::vec::IntoIter<Token>) -> Result<Placeholder, SsrError> {
    let mut name = None;
    let mut constraints = Vec::new();
    let mut kind = PlaceholderKind::Normal;
    if let Some(token) = tokens.next() {
        match token.kind {
            SyntaxKind::IDENT => {
                name = Some(token.text);
            }
            // Handle lifetime placeholder syntax: $'name
            SyntaxKind::LIFETIME => {
                // token.text is "'name", strip the leading quote
                let lifetime_name = token
                    .text
                    .strip_prefix('\'')
                    .ok_or_else(|| SsrError::new("Invalid lifetime placeholder"))?;
                name = Some(SmolStr::new(lifetime_name));
                kind = PlaceholderKind::Lifetime;
            }
            T!['{'] => {
                let token =
                    tokens.next().ok_or_else(|| SsrError::new("Unexpected end of placeholder"))?;
                match token.kind {
                    SyntaxKind::IDENT => {
                        name = Some(token.text);
                    }
                    SyntaxKind::LIFETIME => {
                        let lifetime_name = token
                            .text
                            .strip_prefix('\'')
                            .ok_or_else(|| SsrError::new("Invalid lifetime placeholder"))?;
                        name = Some(SmolStr::new(lifetime_name));
                        kind = PlaceholderKind::Lifetime;
                    }
                    _ => {}
                }
                loop {
                    let token = tokens
                        .next()
                        .ok_or_else(|| SsrError::new("Placeholder is missing closing brace '}'"))?;
                    match token.kind {
                        T![:] => {
                            constraints.push(parse_constraint(tokens)?);
                        }
                        T!['}'] => break,
                        _ => bail!("Unexpected token while parsing placeholder: '{}'", token.text),
                    }
                }
            }
            _ => {
                bail!("Placeholders should be $name, $'lifetime, or ${{name:constraints}}");
            }
        }
    }
    let name = name.ok_or_else(|| SsrError::new("Placeholder ($) with no name"))?;
    Ok(Placeholder::new(name, constraints, kind))
}

fn parse_constraint(tokens: &mut std::vec::IntoIter<Token>) -> Result<Constraint, SsrError> {
    let constraint_type = tokens
        .next()
        .ok_or_else(|| SsrError::new("Found end of placeholder while looking for a constraint"))?
        .text
        .to_string();
    match constraint_type.as_str() {
        "kind" => {
            expect_token(tokens, "(")?;
            let t = tokens.next().ok_or_else(|| {
                SsrError::new("Unexpected end of constraint while looking for kind")
            })?;
            if t.kind != SyntaxKind::IDENT {
                bail!("Expected ident, found {:?} while parsing kind constraint", t.kind);
            }
            expect_token(tokens, ")")?;
            Ok(Constraint::Kind(NodeKind::from(&t.text)?))
        }
        "type" => {
            expect_token(tokens, "(")?;
            let type_tokens = collect_parenthesized_tokens(tokens)?;
            let type_text = tokens_to_text(&type_tokens);
            if fragments::ty(&type_text).is_err() && fragments::ty_in_return(&type_text).is_err() {
                bail!("Invalid type in type(...) constraint");
            }
            Ok(Constraint::Type(type_text.into()))
        }
        "not" => {
            expect_token(tokens, "(")?;
            let sub = parse_constraint(tokens)?;
            expect_token(tokens, ")")?;
            Ok(Constraint::Not(Box::new(sub)))
        }
        "receiver" => Ok(Constraint::Context(ContextKind::Receiver)),
        "argument" => Ok(Constraint::Context(ContextKind::Argument)),
        "lhs" => Ok(Constraint::Context(ContextKind::Lhs)),
        x => bail!("Unsupported constraint type '{}'", x),
    }
}

fn expect_token(tokens: &mut std::vec::IntoIter<Token>, expected: &str) -> Result<(), SsrError> {
    if let Some(t) = tokens.next() {
        if t.text == expected {
            return Ok(());
        }
        bail!("Expected {} found {}", expected, t.text);
    }
    bail!("Expected {} found end of stream", expected);
}

/// Collects tokens up to the `)` matching an already-consumed `(`.
fn collect_parenthesized_tokens(
    tokens: &mut impl Iterator<Item = Token>,
) -> Result<Vec<Token>, SsrError> {
    let mut depth = 1usize;
    let mut collected = Vec::new();
    for token in tokens.by_ref() {
        match token.text.as_str() {
            "(" => {
                depth += 1;
                collected.push(token);
            }
            ")" => {
                depth -= 1;
                if depth == 0 {
                    return Ok(collected);
                }
                collected.push(token);
            }
            _ => collected.push(token),
        }
    }
    bail!("Unexpected end of constraint while looking for closing ')'");
}

fn tokens_to_text(tokens: &[Token]) -> String {
    tokens.iter().map(|token| token.text.as_str()).collect()
}

impl NodeKind {
    fn from(name: &SmolStr) -> Result<NodeKind, SsrError> {
        Ok(match name.as_str() {
            "literal" => NodeKind::Literal,
            "field_expr" => NodeKind::FieldExpr,
            "method_call" => NodeKind::MethodCall,
            "call_expr" => NodeKind::CallExpr,
            _ => bail!("Unknown node kind '{}'", name),
        })
    }
}

fn extract_where_clause(pattern_str: &str) -> Result<(&str, WhereClause), SsrError> {
    if let Some(bracket_start) = pattern_str.rfind("[where") {
        let after_bracket = &pattern_str[bracket_start..];
        if !after_bracket.trim_end().ends_with(']') {
            bail!("Where clause missing closing ']'");
        }
        let bracket_end = pattern_str.rfind(']').unwrap();
        let pattern = pattern_str[..bracket_start].trim();
        let where_content = &pattern_str[bracket_start + "[where".len()..bracket_end].trim();
        let where_clause = parse_where_clause(where_content)?;
        Ok((pattern, where_clause))
    } else {
        Ok((pattern_str, WhereClause::default()))
    }
}

fn skip_whitespace(tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>) {
    while let Some(token) = tokens.peek() {
        if token.kind == SyntaxKind::WHITESPACE {
            tokens.next();
        } else {
            break;
        }
    }
}

fn parse_where_clause(content: &str) -> Result<WhereClause, SsrError> {
    if content.is_empty() {
        return Ok(WhereClause::default());
    }
    let tokens = tokenize(content)?;
    let mut token_iter = tokens.into_iter().peekable();
    let conditions = parse_where_conditions(&mut token_iter)?;
    Ok(WhereClause { conditions })
}

fn parse_where_conditions(
    tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>,
) -> Result<Vec<WhereCondition>, SsrError> {
    let mut conditions = Vec::new();

    conditions.push(parse_or_condition(tokens)?);

    loop {
        skip_whitespace(tokens);
        match tokens.peek() {
            Some(token) if token.kind == T![,] => {
                tokens.next();
                skip_whitespace(tokens);
                if tokens.peek().is_none() {
                    break;
                }
                conditions.push(parse_or_condition(tokens)?);
            }
            _ => break,
        }
    }

    Ok(conditions)
}

fn parse_or_condition(
    tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>,
) -> Result<WhereCondition, SsrError> {
    let mut parts = vec![parse_single_condition(tokens)?];

    loop {
        skip_whitespace(tokens);
        match tokens.peek() {
            Some(token) if token.kind == T![|] => {
                tokens.next();
                parts.push(parse_single_condition(tokens)?);
            }
            _ => break,
        }
    }

    if parts.len() == 1 { Ok(parts.pop().unwrap()) } else { Ok(WhereCondition::Or(parts)) }
}

/// Parses a single condition
fn parse_single_condition(
    tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>,
) -> Result<WhereCondition, SsrError> {
    skip_whitespace(tokens);
    let token = tokens.next().ok_or_else(|| SsrError::new("Expected condition"))?;

    match token.kind {
        T!['('] => {
            let inner = parse_where_conditions(tokens)?;
            expect_where_token(tokens, ")")?;
            if inner.len() == 1 {
                Ok(inner.into_iter().next().unwrap())
            } else {
                bail!(
                    "Multiple conditions in parentheses not yet supported, use comma at top level"
                );
            }
        }
        T![$] => {
            let name_token =
                tokens.next().ok_or_else(|| SsrError::new("Expected placeholder name after $"))?;
            if name_token.kind != SyntaxKind::IDENT {
                bail!("Expected identifier after $, found {:?}", name_token.kind);
            }
            let placeholder = Var(name_token.text.to_string());

            expect_where_token(tokens, ".")?;

            let method_token =
                tokens.next().ok_or_else(|| SsrError::new("Expected method name"))?;
            if method_token.kind != SyntaxKind::IDENT {
                bail!("Expected method name, found {:?}", method_token.kind);
            }

            match method_token.text.as_str() {
                "one_of" => {
                    expect_where_token(tokens, "(")?;
                    let values = parse_ident_list(tokens)?;
                    expect_where_token(tokens, ")")?;
                    Ok(WhereCondition::PlaceholderOneOf { placeholder, values })
                }
                "eq" => {
                    expect_where_token(tokens, "(")?;
                    let value_token =
                        tokens.next().ok_or_else(|| SsrError::new("Expected value in eq()"))?;
                    if value_token.kind != SyntaxKind::IDENT {
                        bail!("Expected identifier in eq(), found {:?}", value_token.kind);
                    }
                    expect_where_token(tokens, ")")?;
                    Ok(WhereCondition::PlaceholderEq { placeholder, value: value_token.text })
                }
                "mut_self" => Ok(WhereCondition::PlaceholderMutSelf { placeholder }),
                "ref_self" => Ok(WhereCondition::PlaceholderRefSelf { placeholder }),
                "owned_self" => Ok(WhereCondition::PlaceholderOwnedSelf { placeholder }),
                other => bail!("Unknown placeholder method '{}'", other),
            }
        }
        T![type] => {
            expect_where_token(tokens, "(")?;
            let type_tokens = collect_parenthesized_tokens(tokens)?;
            let type_text = tokens_to_text(&type_tokens);
            if fragments::ty(&type_text).is_err() && fragments::ty_in_return(&type_text).is_err() {
                bail!("Invalid type in type() constraint: '{}'", type_text);
            }
            Ok(WhereCondition::Type(type_text.into()))
        }
        SyntaxKind::IDENT => match token.text.as_str() {
            "not" => {
                expect_where_token(tokens, "(")?;
                let inner = parse_single_condition(tokens)?;
                expect_where_token(tokens, ")")?;
                Ok(WhereCondition::Not(Box::new(inner)))
            }
            "kind" => {
                expect_where_token(tokens, "(")?;
                let kind_token =
                    tokens.next().ok_or_else(|| SsrError::new("Expected node kind"))?;
                if kind_token.kind != SyntaxKind::IDENT {
                    bail!("Expected identifier in kind(), found {:?}", kind_token.kind);
                }
                expect_where_token(tokens, ")")?;
                Ok(WhereCondition::Kind(NodeKind::from(&kind_token.text)?))
            }
            "type" => {
                expect_where_token(tokens, "(")?;
                let type_tokens = collect_parenthesized_tokens(tokens)?;
                let type_text = tokens_to_text(&type_tokens);
                if fragments::ty(&type_text).is_err()
                    && fragments::ty_in_return(&type_text).is_err()
                {
                    bail!("Invalid type in type() constraint: '{}'", type_text);
                }
                Ok(WhereCondition::Type(type_text.into()))
            }
            "receiver" => Ok(WhereCondition::Context(ContextKind::Receiver)),
            "argument" => Ok(WhereCondition::Context(ContextKind::Argument)),
            "lhs" => Ok(WhereCondition::Context(ContextKind::Lhs)),
            other => bail!("Unknown condition '{}'", other),
        },
        _ => bail!("Unexpected token in where clause: '{}'", token.text),
    }
}

fn expect_where_token(
    tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>,
    expected: &str,
) -> Result<(), SsrError> {
    skip_whitespace(tokens);
    match tokens.next() {
        Some(t) if t.text == expected => Ok(()),
        Some(t) => bail!("Expected '{}', found '{}'", expected, t.text),
        None => bail!("Expected '{}', found end of where clause", expected),
    }
}

fn parse_ident_list(
    tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>,
) -> Result<Vec<SmolStr>, SsrError> {
    let mut values = Vec::new();

    skip_whitespace(tokens);
    let first = tokens.next().ok_or_else(|| SsrError::new("Expected identifier in list"))?;
    if first.kind != SyntaxKind::IDENT {
        bail!("Expected identifier, found {:?}", first.kind);
    }
    values.push(first.text);

    loop {
        skip_whitespace(tokens);
        match tokens.peek() {
            Some(token) if token.kind == T![,] => {
                tokens.next();
                skip_whitespace(tokens);
                match tokens.peek() {
                    Some(next) if next.kind == T![')'] => {
                        break;
                    }
                    Some(_) | None => (),
                }
                let ident = tokens
                    .next()
                    .ok_or_else(|| SsrError::new("Expected identifier after comma"))?;
                if ident.kind != SyntaxKind::IDENT {
                    bail!("Expected identifier, found {:?}", ident.kind);
                }
                values.push(ident.text);
            }
            _ => break,
        }
    }

    Ok(values)
}

impl Placeholder {
    fn new(name: SmolStr, constraints: Vec<Constraint>, kind: PlaceholderKind) -> Self {
        let stand_in_name = match kind {
            PlaceholderKind::Normal => format!("__placeholder_{name}"),
            PlaceholderKind::Lifetime => format!("'__placeholder_{name}"),
        };
        Self { stand_in_name, constraints, ident: Var(name.to_string()) }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_happy_case() {
        fn token(kind: SyntaxKind, text: &str) -> PatternElement {
            PatternElement::Token(Token { kind, text: SmolStr::new(text) })
        }
        fn placeholder(name: &str) -> PatternElement {
            PatternElement::Placeholder(Placeholder::new(
                SmolStr::new(name),
                Vec::new(),
                PlaceholderKind::Normal,
            ))
        }
        let result: SsrRule = "foo($a, $b) ==>> bar($b, $a)".parse().unwrap();
        assert_eq!(
            result.pattern.tokens,
            vec![
                token(SyntaxKind::IDENT, "foo"),
                token(T!['('], "("),
                placeholder("a"),
                token(T![,], ","),
                token(SyntaxKind::WHITESPACE, " "),
                placeholder("b"),
                token(T![')'], ")"),
            ]
        );
        assert_eq!(
            result.template.tokens,
            vec![
                token(SyntaxKind::IDENT, "bar"),
                token(T!['('], "("),
                placeholder("b"),
                token(T![,], ","),
                token(SyntaxKind::WHITESPACE, " "),
                placeholder("a"),
                token(T![')'], ")"),
            ]
        );
    }
}
