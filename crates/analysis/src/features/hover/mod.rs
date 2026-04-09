use ls_types::*;

use crate::analysis::{SymbolAtOffset, symbol_at_offset};
use crate::state::DocumentState;

mod directive;
mod import;
mod pretty;
mod rule;

use directive::{hover_debug, hover_recover, hover_ws};
use import::hover_import;
use pretty::hover_pretty;
use rule::{build_rule_definition_hover, build_rule_reference_hover};

pub fn hover(state: &DocumentState, position: Position) -> Option<Hover> {
    let offset = state.line_index.position_to_offset(position);

    // Check import hovers first.
    if let Some(hover) = hover_import(state, offset) {
        return Some(hover);
    }

    // Check directive keyword hovers first.
    if let Some(hover) = hover_recover(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_pretty(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_debug(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_ws(state, offset) {
        return Some(hover);
    }

    let symbol = symbol_at_offset(&state.info, offset)?;

    match symbol {
        SymbolAtOffset::RuleDefinition(rule) => Some(build_rule_definition_hover(state, rule)),
        SymbolAtOffset::RuleReference { name, .. } => {
            Some(build_rule_reference_hover(state, &name))
        }
    }
}

/// Lowercase the first character of a string (for inline descriptions).
pub(super) fn lowercase_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().to_string() + chars.as_str(),
    }
}
