use tower_lsp_server::ls_types::*;

use crate::state::DocumentState;

#[expect(
    deprecated,
    reason = "ls-types 0.0.3 still requires the deprecated `deprecated` field in struct literals"
)]
pub fn document_symbols(state: &DocumentState) -> DocumentSymbolResponse {
    let symbols: Vec<DocumentSymbol> = state
        .info
        .rules
        .iter()
        .map(|rule| {
            DocumentSymbol {
                name: rule.name.clone(),
                detail: Some(rule.rhs_text.clone()),
                kind: SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                range: state.line_index.span_to_range(rule.full_span.0, rule.full_span.1),
                selection_range: state.line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                children: None,
            }
        })
        .collect();

    DocumentSymbolResponse::Nested(symbols)
}
