use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::RwLock;

use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer};

use crate::features;
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

    /// Update the global rule index for a document.
    async fn update_global_rules(&self, uri: &Uri) {
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
    async fn update_import_graph(&self, uri: &Uri) {
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
    async fn filter_diagnostics_with_imports(
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
    fn apply_incremental_changes(text: &mut String, changes: Vec<TextDocumentContentChangeEvent>) {
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

/// Semantic token legend shared between server and client.
/// Check if a byte offset falls on an import directive.
/// Returns (target_uri, path_string_byte_range) for constructing a LocationLink.
fn resolve_import_at_offset(
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

impl LanguageServer for BbnfLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                })),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        "=".into(),
                        "|".into(),
                        ",".into(),
                        "(".into(),
                        "[".into(),
                        "{".into(),
                    ]),
                    ..Default::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                    first_trigger_character: ";".into(),
                    more_trigger_character: None,
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_token_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "BBNF language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        // Apply incremental edits to the stored text, then re-analyze.
        let new_text;
        {
            let mut docs = self.documents.write().await;
            if let Some(state) = docs.get_mut(&uri) {
                Self::apply_incremental_changes(&mut state.text, changes);
                new_text = state.text.clone();
            } else {
                // Document not tracked yet — take the last change as full text.
                new_text = changes
                    .into_iter()
                    .last()
                    .map(|c| c.text)
                    .unwrap_or_default();
            }
        }

        self.on_change(uri, new_text).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents
            .write()
            .await
            .remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(features::hover::hover(state, pos))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri.clone();
        let pos = params.text_document_position_params.position;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        // Try local first.
        if let Some(result) = features::goto_definition::goto_definition(state, &uri, pos) {
            return Ok(Some(result));
        }
        let offset = crate::analysis::position_to_offset(&state.text, pos);

        // Check if cursor is on an import directive — jump to the imported file.
        if let Some((target_uri, path_span)) = resolve_import_at_offset(state, &uri, offset) {
            let origin_range = crate::analysis::span_to_range(&state.text, path_span.0, path_span.1);
            let target_range = Range::new(Position::new(0, 0), Position::new(0, 0));
            return Ok(Some(GotoDefinitionResponse::Link(vec![LocationLink {
                origin_selection_range: Some(origin_range),
                target_uri,
                target_range,
                target_selection_range: target_range,
            }])));
        }

        // Cross-file: look up in global rules.
        let symbol = crate::analysis::symbol_at_offset(&state.info, offset);
        if let Some(crate::analysis::SymbolAtOffset::RuleReference { name, .. }) = symbol {
            let global = self.global_rules.read().await;
            if let Some(entries) = global.get(&name) {
                for entry in entries {
                    if entry.uri != uri {
                        if let Some(target_state) = docs.get(&entry.uri) {
                            if let Some(rule) = target_state.info.rules.get(entry.rule_index) {
                                let range = crate::analysis::span_to_range(
                                    &target_state.text, rule.name_span.0, rule.name_span.1,
                                );
                                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                    uri: entry.uri.clone(),
                                    range,
                                })));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let pos = params.text_document_position.position;
        let include_decl = params.context.include_declaration;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        // Start with local references.
        let mut locations = features::references::references(state, &uri, pos, include_decl)
            .unwrap_or_default();

        // Determine the symbol name for cross-file search.
        let offset = crate::analysis::position_to_offset(&state.text, pos);
        let symbol = crate::analysis::symbol_at_offset(&state.info, offset);
        let name = match &symbol {
            Some(crate::analysis::SymbolAtOffset::RuleDefinition(rule)) => Some(rule.name.clone()),
            Some(crate::analysis::SymbolAtOffset::RuleReference { name, .. }) => Some(name.clone()),
            None => None,
        };

        // Cross-file: search all other open documents for references.
        if let Some(name) = name {
            for (doc_uri, doc_state) in docs.iter() {
                if doc_uri == &uri {
                    continue; // Already searched.
                }
                for rule in &doc_state.info.rules {
                    for refinfo in &rule.references {
                        if refinfo.name == name {
                            locations.push(Location {
                                uri: doc_uri.clone(),
                                range: crate::analysis::span_to_range(
                                    &doc_state.text, refinfo.span.0, refinfo.span.1,
                                ),
                            });
                        }
                    }
                }
            }
        }

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let pos = params.text_document_position.position;
        let new_name = params.new_name;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(features::rename::rename(state, &uri, pos, &new_name))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri.clone();
        let pos = params.position;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(features::rename::prepare_rename(state, pos))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };

        let mut response = features::completion::completion(state);

        // Add imported rules to completion.
        for import_info in &state.info.imports {
            if let Some(import_uri) = Self::resolve_import_uri(&uri, &import_info.path) {
                if let Some(target_state) = docs.get(&import_uri) {
                    let source_file = import_info.path.rsplit('/').next().unwrap_or(&import_info.path);
                    if let Some(ref items) = import_info.items {
                        // Selective: only listed names.
                        for name in items {
                            if let Some(&idx) = target_state.info.rule_index.get(name.as_str()) {
                                let rule = &target_state.info.rules[idx];
                                if let CompletionResponse::Array(ref mut arr) = response {
                                    arr.push(CompletionItem {
                                        label: rule.name.clone(),
                                        kind: Some(CompletionItemKind::FUNCTION),
                                        detail: Some(format!("{} (from {})", rule.rhs_text, source_file)),
                                        ..Default::default()
                                    });
                                }
                            }
                        }
                    } else {
                        // Glob: all rules.
                        for rule in &target_state.info.rules {
                            if let CompletionResponse::Array(ref mut arr) = response {
                                arr.push(CompletionItem {
                                    label: rule.name.clone(),
                                    kind: Some(CompletionItemKind::FUNCTION),
                                    detail: Some(format!("{} (from {})", rule.rhs_text, source_file)),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                }
            }
        }

        Ok(Some(response))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::document_symbols::document_symbols(state)))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::code_lens::code_lens(state)))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::folding::folding_ranges(state)))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.clone();
        let range = params.range;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::code_actions::code_actions(
            state, &uri, range,
        )))
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(features::formatting::format_document(state))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::semantic_tokens::semantic_tokens_full(state)))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::inlay_hints::inlay_hints(state, params.range)))
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(features::selection_range::selection_ranges(
            state,
            params.positions,
        )))
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(features::formatting::format_range(state, params.range))
    }

    async fn on_type_formatting(
        &self,
        params: DocumentOnTypeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document_position.text_document.uri;
        let docs = self.documents.read().await;
        let Some(state) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(features::formatting::format_on_type(
            state,
            params.text_document_position.position,
        ))
    }
}
