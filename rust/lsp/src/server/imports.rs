use std::collections::HashSet;

use tower_lsp_server::ls_types::*;

use super::{BbnfLanguageServer, GlobalRule};
use crate::state::DocumentState;

impl BbnfLanguageServer {
    /// Update the global rule index for a document.
    pub(crate) async fn update_global_rules(&self, uri: &Uri) {
        let docs = self.documents.read().await;
        let mut global = self.global_rules.write().await;

        // Remove old entries for this URI.
        for entries in global.values_mut() {
            entries.retain(|e| &e.uri != uri);
        }
        // Remove empty entries.
        global.retain(|_, v| !v.is_empty());

        // Add new entries.
        if let Some(state) = docs.get(uri) {
            for (idx, rule) in state.info.rules.iter().enumerate() {
                global.entry(rule.name.clone()).or_default().push(GlobalRule {
                    uri: uri.clone(),
                    rule_index: idx,
                });
            }
        }
    }

    /// Update the import graph for a document.
    pub(crate) async fn update_import_graph(&self, uri: &Uri) {
        let docs = self.documents.read().await;
        let Some(state) = docs.get(uri) else { return };

        let new_imports: Vec<Uri> = state.info.imports.iter()
            .filter_map(|imp| Self::resolve_import_uri(uri, &imp.path))
            .collect();

        drop(docs);

        let mut graph = self.import_graph.write().await;
        let mut reverse = self.importers.write().await;

        // Remove old reverse entries.
        if let Some(old) = graph.get(uri) {
            for old_uri in old {
                if let Some(set) = reverse.get_mut(old_uri) {
                    set.remove(uri);
                }
            }
        }

        // Add new reverse entries.
        for new_uri in &new_imports {
            reverse.entry(new_uri.clone()).or_default().insert(uri.clone());
        }

        graph.insert(uri.clone(), new_imports);
    }

    /// Filter diagnostics: suppress "Undefined rule" for rules that are available via imports.
    pub(crate) async fn filter_diagnostics_with_imports(
        &self,
        uri: &Uri,
        diagnostics: Vec<Diagnostic>,
    ) -> Vec<Diagnostic> {
        let graph = self.import_graph.read().await;
        let Some(_imported_uris) = graph.get(uri) else {
            return diagnostics;
        };

        // Collect all rule names available via imports.
        let docs = self.documents.read().await;
        let mut available_rules: HashSet<String> = HashSet::new();

        // For each imported URI, check the document's import info.
        let doc_imports = docs.get(uri).map(|s| s.info.imports.clone()).unwrap_or_default();
        drop(docs);

        for import_info in &doc_imports {
            if let Some(import_uri) = Self::resolve_import_uri(uri, &import_info.path) {
                let docs = self.documents.read().await;
                if let Some(target_state) = docs.get(&import_uri) {
                    if let Some(ref items) = import_info.items {
                        // Selective import.
                        for name in items {
                            if target_state.info.rule_index.contains_key(name.as_str()) {
                                available_rules.insert(name.clone());
                            }
                        }
                    } else {
                        // Glob import: all rules.
                        for rule in &target_state.info.rules {
                            available_rules.insert(rule.name.clone());
                        }
                    }
                }
            }
        }

        if available_rules.is_empty() {
            return diagnostics;
        }

        diagnostics
            .into_iter()
            .filter(|d| {
                // Suppress "Undefined rule: `name`" if `name` is imported.
                if d.message.starts_with("Undefined rule: `") {
                    if let Some(name) = d.message.strip_prefix("Undefined rule: `").and_then(|s| s.strip_suffix('`')) {
                        return !available_rules.contains(name);
                    }
                }
                true
            })
            .collect()
    }

    /// Apply incremental text edits to the stored document text.
    pub(crate) fn apply_incremental_changes(text: &mut String, changes: Vec<TextDocumentContentChangeEvent>) {
        for change in changes {
            if let Some(range) = change.range {
                let start = crate::analysis::position_to_offset(text, range.start);
                let end = crate::analysis::position_to_offset(text, range.end);
                text.replace_range(start..end, &change.text);
            } else {
                // Full content replacement.
                *text = change.text;
            }
        }
    }
}

/// Check if a byte offset falls on an import directive.
/// Returns (target_uri, path_string_byte_range) for constructing a LocationLink.
pub(crate) fn resolve_import_at_offset(
    state: &DocumentState,
    uri: &Uri,
    offset: usize,
) -> Option<(Uri, (usize, usize))> {
    for imp in &state.info.imports {
        // Check if cursor is within this import directive's span.
        if offset < imp.span.0 || offset > imp.span.1 {
            continue;
        }

        // Find the path string span within the directive text.
        let directive_text = &state.text[imp.span.0..imp.span.1];
        let path_in_text = format!("\"{}\"", imp.path);
        let path_local_offset = directive_text.find(&path_in_text)?;
        let path_start = imp.span.0 + path_local_offset;
        let path_end = path_start + path_in_text.len();

        // Resolve the import path relative to the current file.
        let file_path = uri.path().to_string();
        let dir = std::path::Path::new(&file_path).parent()?;
        let target = dir.join(&imp.path);
        let canonical = target.canonicalize().ok().unwrap_or(target);
        let target_uri: Uri = format!("file://{}", canonical.display()).parse().ok()?;
        return Some((target_uri, (path_start, path_end)));
    }
    None
}
