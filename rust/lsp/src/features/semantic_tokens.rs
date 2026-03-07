use tower_lsp_server::ls_types::*;

use crate::state::DocumentState;

pub fn semantic_tokens_full(state: &DocumentState) -> SemanticTokensResult {
    let mut data = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for token in &state.info.semantic_tokens {
        if token.span.0 >= token.span.1 {
            continue;
        }

        let pos = state.line_index.offset_to_position(token.span.0);
        let length = (token.span.1 - token.span.0) as u32;

        let delta_line = pos.line - prev_line;
        let delta_start = if delta_line == 0 {
            pos.character - prev_start
        } else {
            pos.character
        };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token.token_type,
            token_modifiers_bitset: 0,
        });

        prev_line = pos.line;
        prev_start = pos.character;
    }

    SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    })
}
