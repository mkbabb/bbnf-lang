use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::LanguageServer;

use crate::features;

use super::BbnfLanguageServer;
use super::imports::resolve_import_at_offset;
use super::semantic_token_legend;

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
                    .next_back()
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
