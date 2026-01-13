//! Pure helpers for counting, grouping, and filtering matches.
//!
//! These functions never touch the analysis database — they operate on already
//! serialized data — so they're cheap to call and trivial to unit-test.

use ide_db::FxHashMap;

use crate::params::{FunctionFilterText, ImplFilterText};
use crate::requests::PathFilter;
use crate::types::{
    BoolFilter, ContextProperty, MatchGroup, SerializableContext, SerializableMatch,
    SerializableUsageKind,
};
pub(super) fn group_key_file(m: &SerializableMatch) -> String {
    m.file_path.to_string_lossy().into_owned()
}

pub(super) fn group_key_usage_kind(m: &SerializableMatch) -> String {
    m.usage_kind.as_str().to_owned()
}

pub(super) fn group_key_function(m: &SerializableMatch) -> String {
    m.context.enclosing_function.clone().unwrap_or_else(|| "<none>".to_owned())
}

pub(super) fn group_key_impl(m: &SerializableMatch) -> String {
    m.context.enclosing_impl.clone().unwrap_or_else(|| "<none>".to_owned())
}

pub(super) fn count_matches_by(
    matches: &[SerializableMatch],
    key_fn: fn(&SerializableMatch) -> String,
) -> Vec<MatchGroup> {
    let mut counts: FxHashMap<String, usize> = FxHashMap::default();
    for m in matches {
        *counts.entry(key_fn(m)).or_insert(0) += 1;
    }
    let mut result = Vec::new();
    for (key, count) in counts {
        result.push(MatchGroup { key, count });
    }
    // Tie-break by key: hash-map iteration order varies across instances
    // (keys embed absolute paths), and rendered output must be deterministic.
    result.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.key.cmp(&b.key)));
    result
}

pub(super) struct FilterCriteria {
    pub(super) path_filter: Option<PathFilter>,
    pub(super) in_test: BoolFilter,
    pub(super) in_unsafe: BoolFilter,
    pub(super) in_async: BoolFilter,
    pub(super) in_function: Option<FunctionFilterText>,
    pub(super) in_impl: Option<ImplFilterText>,
    pub(super) usage_kind: Option<SerializableUsageKind>,
}

pub(super) fn context_has_property(ctx: &SerializableContext, prop: ContextProperty) -> bool {
    ctx.properties.contains(&prop)
}

pub(super) fn filter_matches(
    matches: &[SerializableMatch],
    criteria: &FilterCriteria,
) -> Vec<SerializableMatch> {
    let mut filtered = Vec::new();
    for m in matches {
        match criteria.path_filter.as_ref() {
            Some(filter) if !filter.matches(&m.file_path) => continue,
            Some(_) | None => (),
        }
        if !criteria.in_test.matches(context_has_property(&m.context, ContextProperty::Test)) {
            continue;
        }
        if !criteria.in_unsafe.matches(context_has_property(&m.context, ContextProperty::Unsafe)) {
            continue;
        }
        if !criteria.in_async.matches(context_has_property(&m.context, ContextProperty::Async)) {
            continue;
        }
        if let Some(ref fn_name) = criteria.in_function {
            let matches_fn = m
                .context
                .enclosing_function
                .as_deref()
                .is_some_and(|name| name == fn_name.as_str());
            if !matches_fn {
                continue;
            }
        }
        if let Some(ref impl_name) = criteria.in_impl {
            let matches_impl =
                m.context.enclosing_impl.as_deref().is_some_and(|name| name == impl_name.as_str());
            if !matches_impl {
                continue;
            }
        }
        match criteria.usage_kind {
            Some(usage) if m.usage_kind != usage => continue,
            Some(_) | None => (),
        }
        filtered.push(m.clone());
    }
    filtered
}
