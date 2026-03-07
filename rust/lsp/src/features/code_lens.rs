use tower_lsp_server::ls_types::*;

use crate::state::DocumentState;

pub fn code_lens(state: &DocumentState) -> Vec<CodeLens> {
    state
        .info
        .rules
        .iter()
        .map(|rule| {
            let ref_count: usize = state
                .info
                .rules
                .iter()
                .flat_map(|r| &r.references)
                .filter(|r| r.name == rule.name)
                .count();

            let range = state.line_index.span_to_range(rule.name_span.0, rule.name_span.1);

            CodeLens {
                range,
                command: Some(Command {
                    title: format!(
                        "{} reference{}",
                        ref_count,
                        if ref_count == 1 { "" } else { "s" }
                    ),
                    command: "editor.action.findReferences".into(),
                    arguments: None,
                }),
                data: None,
            }
        })
        .collect()
}
