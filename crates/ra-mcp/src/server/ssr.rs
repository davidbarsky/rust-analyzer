//! SSR finder setup and match-execution helpers.

use std::path::PathBuf;

use ide_db::base_db::SourceDatabase;
use ide_ssr::{MatchFinder, SsrPattern, SsrRule};

use crate::analysis::Snapshot;
use crate::error::{McpError, RequestError};
use crate::params::{PatternText, SsrRuleText};
use crate::server::edits::ComputedEdits;
use crate::types::{ConversionContext, SerializableEdit, SerializableMatch};

pub(super) fn collect_crate_root_file_ids(db: &ide_db::RootDatabase) -> Vec<ide_db::FileId> {
    use ide_db::base_db::{local_roots, source_root_crates};

    let mut root_file_ids = Vec::new();
    for &root in local_roots(db).iter() {
        for &krate in source_root_crates(db, root).iter() {
            let krate = hir::Crate::from(krate);
            root_file_ids.push(krate.root_file(db));
        }
    }
    root_file_ids
}

fn add_pattern_or_rule_to_finder(
    finder: &mut MatchFinder<'_>,
    text: &str,
    is_rule: bool,
) -> Result<(), McpError> {
    if is_rule {
        let rule: SsrRule = text.parse().map_err(|e: ide_ssr::SsrError| {
            RequestError::InvalidSyntax { kind: "SSR rule", message: e.to_string() }
        })?;
        finder.add_rule(rule).map_err(|e| {
            McpError::from(RequestError::InvalidSyntax { kind: "rule", message: e.to_string() })
        })
    } else {
        let pattern: SsrPattern = text.parse().map_err(|e: ide_ssr::SsrError| {
            RequestError::InvalidSyntax { kind: "SSR pattern", message: e.to_string() }
        })?;
        finder.add_search_pattern(pattern).map_err(|e| {
            McpError::from(RequestError::InvalidSyntax { kind: "pattern", message: e.to_string() })
        })
    }
}

pub(super) fn create_finder_with_resolved_pattern<'db>(
    snapshot: &Snapshot,
    db: &'db ide_db::RootDatabase,
    files: &Option<Vec<PathBuf>>,
    text: &str,
    is_rule: bool,
) -> Result<MatchFinder<'db>, McpError> {
    if let Some(files) = files.as_ref().filter(|f| !f.is_empty()) {
        let file_id = snapshot.resolve_file_id(&files[0])?;
        let lookup_context = ide_db::FilePosition { file_id, offset: syntax::TextSize::from(0) };
        let mut finder = MatchFinder::in_context(db, lookup_context, vec![]).map_err(|e| {
            McpError::from(RequestError::InvalidSyntax { kind: "matcher", message: e.to_string() })
        })?;
        add_pattern_or_rule_to_finder(&mut finder, text, is_rule)?;
        return Ok(finder);
    }

    let root_file_ids = collect_crate_root_file_ids(db);
    let mut first_valid_context = None;

    for file_id in &root_file_ids {
        let context = ide_db::FilePosition { file_id: *file_id, offset: 0.into() };
        let mut finder = match MatchFinder::in_context(db, context, vec![]) {
            Ok(f) => f,
            Err(_) => continue,
        };

        if first_valid_context.is_none() {
            first_valid_context = Some(context);
        }

        if add_pattern_or_rule_to_finder(&mut finder, text, is_rule).is_ok() {
            return Ok(finder);
        }
    }

    if let Some(context) = first_valid_context {
        let mut finder = MatchFinder::in_context(db, context, vec![]).map_err(|e| {
            McpError::from(RequestError::InvalidSyntax { kind: "matcher", message: e.to_string() })
        })?;
        add_pattern_or_rule_to_finder(&mut finder, text, is_rule)?;
        return Ok(finder);
    }

    let mut finder = MatchFinder::at_first_file(db).map_err(|e| {
        McpError::from(RequestError::InvalidSyntax { kind: "matcher", message: e.to_string() })
    })?;
    add_pattern_or_rule_to_finder(&mut finder, text, is_rule)?;
    Ok(finder)
}

pub(super) fn ssr_setup_finder<'db>(
    snapshot: &Snapshot,
    db: &'db ide_db::RootDatabase,
    rule_str: &SsrRuleText,
    files: &Option<Vec<PathBuf>>,
) -> Result<MatchFinder<'db>, McpError> {
    create_finder_with_resolved_pattern(snapshot, db, files, rule_str.as_str(), true)
}

/// Resolves the request's `files` list into the set of absolute paths results
/// may come from. `files` both restricts output and provides the pattern's
/// path-resolution context; this is the restriction half of the contract.
fn allowed_paths(
    snapshot: &Snapshot,
    files: &Option<Vec<PathBuf>>,
) -> Result<Option<ide_db::FxHashSet<PathBuf>>, McpError> {
    let Some(files) = files.as_ref().filter(|files| !files.is_empty()) else {
        return Ok(None);
    };
    let mut file_ids = Vec::with_capacity(files.len());
    for file in files {
        file_ids.push(snapshot.resolve_file_id(file)?);
    }
    let db = snapshot.raw_database();
    let mut allowed = ide_db::FxHashSet::default();
    for file_id in file_ids {
        if let Some(path) =
            db.file_path(file_id).and_then(|path| path.as_path().map(|path| path.to_owned().into()))
        {
            allowed.insert(path);
        }
    }
    Ok(Some(allowed))
}

pub(super) fn ssr_preview_inner(
    snapshot: &Snapshot,
    rule_str: &SsrRuleText,
    files: &Option<Vec<PathBuf>>,
) -> Result<(Vec<SerializableMatch>, Vec<SerializableEdit>), McpError> {
    let db = snapshot.raw_database();

    hir::attach_db(db, || {
        let finder = ssr_setup_finder(snapshot, db, rule_str, files)?;
        let ctx = ConversionContext::new(db);

        let mut matches = Vec::new();
        for m in &finder.structured_matches() {
            matches.push(ctx.convert_match(m));
        }

        let mut edits = Vec::new();
        for (file_id, edit) in finder.edits() {
            let file_path = ctx.file_path(file_id).unwrap_or_default();
            for indel in edit {
                edits.push(SerializableEdit {
                    file_path: file_path.clone(),
                    range: ctx.text_range_to_line_col(file_id, indel.delete),
                    new_text: indel.insert,
                });
            }
        }
        sort_edits(&mut edits);

        if let Some(allowed) = allowed_paths(snapshot, files)? {
            matches.retain(|m| allowed.contains(&m.file_path));
            edits.retain(|edit| allowed.contains(&edit.file_path));
        }

        Ok::<_, McpError>((matches, edits))
    })
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn ssr_compute_edits(
    snapshot: &Snapshot,
    rule_str: &SsrRuleText,
    files: &Option<Vec<PathBuf>>,
) -> Result<ComputedEdits, McpError> {
    let db = snapshot.raw_database();

    hir::attach_db(db, || {
        let finder = ssr_setup_finder(snapshot, db, rule_str, files)?;

        let edits = finder.edits();
        let ctx = ConversionContext::new(db);

        let mut serialized = Vec::new();
        let mut file_edits = Vec::new();
        for (fid, edit) in &edits {
            let Some(file_path) = ctx.file_path(*fid) else {
                continue;
            };
            for indel in edit.iter() {
                serialized.push(SerializableEdit {
                    file_path: file_path.clone(),
                    range: ctx.text_range_to_line_col(*fid, indel.delete),
                    new_text: indel.insert.clone(),
                });
            }
            file_edits.push((file_path, edit.clone()));
        }
        sort_edits(&mut serialized);

        if let Some(allowed) = allowed_paths(snapshot, files)? {
            serialized.retain(|edit| allowed.contains(&edit.file_path));
            file_edits.retain(|(path, _)| allowed.contains(path));
        }

        Ok::<_, McpError>(ComputedEdits { serialized, file_edits })
    })
}

/// Edit order follows FileId assignment, which is not part of the output
/// contract — sort so rendered edit lines are deterministic across instances.
fn sort_edits(edits: &mut [SerializableEdit]) {
    edits.sort_by(|a, b| {
        (&a.file_path, a.range.start_line, a.range.start_col).cmp(&(
            &b.file_path,
            b.range.start_line,
            b.range.start_col,
        ))
    });
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn search_matches(
    snapshot: &Snapshot,
    pattern: &PatternText,
    files: &Option<Vec<PathBuf>>,
) -> Result<Vec<SerializableMatch>, McpError> {
    let db = snapshot.raw_database();

    hir::attach_db(db, || {
        let finder =
            create_finder_with_resolved_pattern(snapshot, db, files, pattern.as_str(), false)?;
        let ctx = ConversionContext::new(db);
        let mut matches = Vec::new();
        for m in &finder.structured_matches() {
            matches.push(ctx.convert_match(m));
        }

        // Match order follows FileId assignment, which is not part of the
        // output contract — sort so output and offset paging are deterministic.
        matches.sort_by(|a, b| {
            (&a.file_path, a.range.start_line, a.range.start_col).cmp(&(
                &b.file_path,
                b.range.start_line,
                b.range.start_col,
            ))
        });

        if let Some(allowed) = allowed_paths(snapshot, files)? {
            matches.retain(|m| allowed.contains(&m.file_path));
        }

        Ok::<_, McpError>(matches)
    })
}
