//! Edit conversion and disk-application helpers.

use std::fs;
use std::path::PathBuf;

use crate::analysis::Workspace;
use crate::error::{EnvironmentError, McpError};
use crate::types::{ConversionContext, SerializableEdit};

#[doc(hidden)] // exposed for tests; not a stable API
pub struct ComputedEdits {
    pub serialized: Vec<SerializableEdit>,
    pub file_edits: Vec<(PathBuf, ide_db::text_edit::TextEdit)>,
}

pub(super) struct EditCounts {
    pub(super) files_changed: usize,
    pub(super) edits_applied: usize,
}

#[doc(hidden)] // exposed for tests; not a stable API
pub fn source_change_to_serialized_edits(
    ctx: &ConversionContext<'_>,
    source_change: &ide::SourceChange,
) -> Vec<SerializableEdit> {
    let mut edits = Vec::new();
    for (&file_id, (text_edit, _snippet)) in &source_change.source_file_edits {
        let file_path = ctx.file_path(file_id).unwrap_or_default();
        for indel in text_edit.iter() {
            edits.push(SerializableEdit {
                file_path: file_path.clone(),
                range: ctx.text_range_to_line_col(file_id, indel.delete),
                new_text: indel.insert.clone(),
            });
        }
    }
    edits
}

pub(super) fn source_change_to_raw_edits(
    ctx: &ConversionContext<'_>,
    source_change: &ide::SourceChange,
) -> Vec<(PathBuf, ide_db::text_edit::TextEdit)> {
    let mut file_edits = Vec::new();
    for (&fid, (text_edit, _snippet)) in &source_change.source_file_edits {
        let Some(file_path) = ctx.file_path(fid) else {
            continue;
        };
        file_edits.push((file_path, text_edit.clone()));
    }
    file_edits
}

pub(super) fn source_change_to_computed_edits(
    ctx: &ConversionContext<'_>,
    source_change: &ide::SourceChange,
) -> ComputedEdits {
    ComputedEdits {
        serialized: source_change_to_serialized_edits(ctx, source_change),
        file_edits: source_change_to_raw_edits(ctx, source_change),
    }
}

pub(super) fn apply_text_edits_to_disk(
    workspace: &Workspace,
    file_edits: &[(PathBuf, ide_db::text_edit::TextEdit)],
) -> Result<EditCounts, McpError> {
    let files_changed = file_edits.len();
    let mut edits_applied = 0;
    let mut written_files = Vec::with_capacity(files_changed);

    for (path, edit) in file_edits {
        if path.as_os_str().is_empty() {
            continue;
        }
        let mut content = fs::read_to_string(path).map_err(|e| EnvironmentError::Io {
            path: path.display().to_string(),
            message: e.to_string(),
        })?;

        edits_applied += edit.len();
        edit.apply(&mut content);

        fs::write(path, &content).map_err(|e| EnvironmentError::Io {
            path: path.display().to_string(),
            message: e.to_string(),
        })?;
        written_files.push((path.clone(), content));
    }

    workspace.apply_file_changes(written_files);
    Ok(EditCounts { files_changed, edits_applied })
}
