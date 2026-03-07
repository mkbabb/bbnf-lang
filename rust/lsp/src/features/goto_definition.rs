use tower_lsp_server::ls_types::*;

use crate::analysis::{symbol_at_offset, SymbolAtOffset};
use crate::state::DocumentState;

pub fn goto_definition(
    state: &DocumentState,
    uri: &Uri,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let offset = state.line_index.position_to_offset(position);
    let symbol = symbol_at_offset(&state.info, offset)?;

    let name = match &symbol {
        SymbolAtOffset::RuleDefinition(rule) => &rule.name,
        SymbolAtOffset::RuleReference { name, .. } => name,
    };

    let &idx = state.info.rule_index.get(name.as_str())?;
    let rule = &state.info.rules[idx];
    let range = state.line_index.span_to_range(rule.name_span.0, rule.name_span.1);

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range,
    }))
}
