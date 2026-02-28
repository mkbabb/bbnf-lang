mod imports;
mod protocol;

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::RwLock;

use tower_lsp_server::ls_types::*;
use tower_lsp_server::Client;

use crate::state::DocumentState;

/// Global rule index entry: a rule name defined in a specific document.
#[derive(Debug, Clone)]
pub struct GlobalRule {
    pub uri: Uri,
    /// Index into the document's `rules` vec.
    pub rule_index: usize,
}

pub struct BbnfLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<Uri, DocumentState>>>,
    /// Forward import graph: URI → set of URIs it imports.
    import_graph: Arc<RwLock<HashMap<Uri, Vec<Uri>>>>,
    /// Reverse import graph: URI → set of URIs that import it.
    importers: Arc<RwLock<HashMap<Uri, HashSet<Uri>>>>,
    /// Global rule index: rule name → list of (uri, rule_index) where it's defined.
    global_rules: Arc<RwLock<HashMap<String, Vec<GlobalRule>>>>,
}

impl BbnfLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            import_graph: Arc::new(RwLock::new(HashMap::new())),
            importers: Arc::new(RwLock::new(HashMap::new())),
            global_rules: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Resolve an import path relative to a document URI.
    fn resolve_import_uri(base_uri: &Uri, import_path: &str) -> Option<Uri> {
        let base_path = base_uri.to_file_path()?;
        let dir = base_path.parent()?;
        let mut resolved = dir.join(import_path);
        if resolved.extension().is_none() {
            resolved.set_extension("bbnf");
        }
        // Canonicalize if possible (file exists).
        let resolved = resolved.canonicalize().unwrap_or(resolved);
        Uri::from_file_path(&resolved)
    }

    async fn on_change(&self, uri: Uri, text: String) {
        let diagnostics;
        {
            let mut docs = self.documents.write().await;
            let state = docs
                .entry(uri.clone())
                .or_insert_with(|| DocumentState::new(String::new()));
            state.update(text);
            diagnostics = state.info.diagnostics.clone();
        }

        // Update global rule index for this document.
        self.update_global_rules(&uri).await;

        // Update import graph.
        self.update_import_graph(&uri).await;

        // Suppress "Undefined rule" diagnostics for imported rules.
        let filtered_diagnostics = self.filter_diagnostics_with_imports(&uri, diagnostics).await;

        self.client
            .publish_diagnostics(uri.clone(), filtered_diagnostics, None)
            .await;

        // Re-publish diagnostics for any documents that import this one
        // (their "undefined rule" warnings may be stale).
        let reverse_deps = {
            let importers = self.importers.read().await;
            importers.get(&uri).cloned().unwrap_or_default()
        };
        for importer_uri in reverse_deps {
            let docs = self.documents.read().await;
            if let Some(state) = docs.get(&importer_uri) {
                let diags = state.info.diagnostics.clone();
                drop(docs);
                let filtered = self.filter_diagnostics_with_imports(&importer_uri, diags).await;
                self.client
                    .publish_diagnostics(importer_uri, filtered, None)
                    .await;
            }
        }
    }
}

/// Semantic token legend shared between server and client.
pub fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::new("ruleDefinition"),  // 0
            SemanticTokenType::new("ruleReference"),    // 1
            SemanticTokenType::STRING,                  // 2
            SemanticTokenType::REGEXP,                  // 3
            SemanticTokenType::OPERATOR,                // 4
            SemanticTokenType::KEYWORD,                 // 5
            SemanticTokenType::COMMENT,                 // 6
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
        ],
    }
}
