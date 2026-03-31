//! `@ws` directive extraction and semantic token generation.

use crate::state::types::{SemanticTokenInfo, WsPatternInfo, token_types};

/// Extract `WsPatternInfo` from a parsed grammar's ws pattern.
///
/// Finds byte span by searching source text for `@ws` pattern.
pub fn extract_ws_pattern(ws_pattern: Option<&str>, src: &str) -> Option<WsPatternInfo> {
    ws_pattern.and_then(|pat| {
        let dir_start = src.find("@ws")?;
        let after_kw = dir_start + "@ws".len();
        let dir_end = src[after_kw..]
            .find(';')
            .map(|off| after_kw + off + 1)
            .unwrap_or(after_kw);
        Some(WsPatternInfo {
            pattern: pat.to_string(),
            span: (dir_start, dir_end),
        })
    })
}

/// Generate semantic tokens for the `@ws` directive.
pub fn ws_semantic_tokens(ws_pattern: Option<&WsPatternInfo>) -> Vec<SemanticTokenInfo> {
    let mut tokens = Vec::new();
    if let Some(ws) = ws_pattern {
        // Semantic token: KEYWORD for "@ws" (3 chars).
        tokens.push(SemanticTokenInfo {
            span: (ws.span.0, ws.span.0 + 3),
            token_type: token_types::KEYWORD,
        });
    }
    tokens
}
