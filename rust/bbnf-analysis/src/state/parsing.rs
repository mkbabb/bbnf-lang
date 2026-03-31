use bbnf::grammar::BBNFGrammar;
use bbnf::types::AST;

use self_cell::self_cell;

use super::pretty::{self, PrettyInfo};
use super::types::{
    DebugInfo, ImportInfo, ImportedItem, ParseDiagnostics, RecoverInfo, TokenInfo, WsPatternInfo,
};

// Self-referential struct: owns the source text and the parsed AST that borrows from it.
self_cell! {
    pub struct OwnedAst {
        owner: String,
        #[covariant]
        dependent: CachedAst,
    }
}

pub type CachedAst<'a> = Option<CachedParseResult<'a>>;

/// Holds both the parsed grammar and import/recover/pretty directives (borrows from OwnedAst's owner).
pub struct CachedParseResult<'a> {
    pub ast: AST<'a>,
    pub imports: Vec<ImportInfo>,
    pub recovers: Vec<RecoverInfo>,
    pub pretties: Vec<PrettyInfo>,
    pub debugs: Vec<DebugInfo>,
    pub tokens: Vec<TokenInfo>,
    pub ws_pattern: Option<WsPatternInfo>,
}

/// Parse the source text once, returning the cached AST data and diagnostic info.
/// Both are extracted from a single parse call.
pub fn parse_once(src: &str) -> (Option<CachedParseResult<'_>>, ParseDiagnostics) {
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let parser = BBNFGrammar::grammar_with_imports();
        parser.parse_return_state(src)
    }));

    match parse_result {
        Ok((result, parser_state)) => {
            let diag = ParseDiagnostics {
                offset: parser_state.offset,
                furthest_offset: parser_state.furthest_offset,
                panic_message: None,
            };
            let cached = result.map(|pg| {
                let imports = pg.imports.iter().map(|imp| ImportInfo {
                    path: imp.path.to_string(),
                    span: (imp.span.start, imp.span.end),
                    items: imp.items.as_ref().map(|items| {
                        items.iter().map(|i| ImportedItem {
                            name: i.name.to_string(),
                            span: (i.span.start, i.span.end),
                        }).collect()
                    }),
                }).collect();
                let recovers = pg.recovers.iter().map(|rec| {
                    let name_str = rec.rule_name.as_ref();
                    let dir_src = rec.span.as_str();
                    let name_start = dir_src.find(name_str).map(|off| rec.span.start + off).unwrap_or_else(|| {
                        panic!(
                            "could not resolve @recover rule-name span for `{}` within directive `{}`",
                            name_str, dir_src
                        )
                    });
                    // Extract sync expression text: everything between end of rule name and end of directive (minus trailing `;`).
                    let after_name = &dir_src[name_start - rec.span.start + name_str.len()..];
                    let sync_text = after_name.trim().trim_end_matches(';').trim().to_string();
                    RecoverInfo {
                        rule_name: name_str.to_string(),
                        span: (rec.span.start, rec.span.end),
                        rule_name_span: (name_start, name_start + name_str.len()),
                        sync_expr_text: sync_text,
                    }
                }).collect();
                let pretties = pretty::extract_pretties(&pg.pretties, src);

                // Extract @debug directives — find byte spans by searching source text.
                let debugs = pg.debug_rules.iter().filter_map(|name| {
                    let name_str = name.as_ref();
                    let needle = format!("@debug {}", name_str);
                    let dir_start = src.find(&needle)?;
                    let kw_end = dir_start + needle.len();
                    let dir_end = src[kw_end..].find(';').map(|off| kw_end + off + 1).unwrap_or(kw_end);
                    let name_start = dir_start + "@debug ".len();
                    Some(DebugInfo {
                        rule_name: name_str.to_string(),
                        span: (dir_start, dir_end),
                        rule_name_span: (name_start, name_start + name_str.len()),
                    })
                }).collect();

                // Extract @token directives — find byte spans by searching source text.
                let tokens = pg.token_rules.iter().filter_map(|name| {
                    let name_str = name.as_ref();
                    let needle = format!("@token {}", name_str);
                    let dir_start = src.find(&needle)?;
                    let kw_end = dir_start + needle.len();
                    let dir_end = src[kw_end..].find(';').map(|off| kw_end + off + 1).unwrap_or(kw_end);
                    let name_start = dir_start + "@token ".len();
                    Some(TokenInfo {
                        rule_name: name_str.to_string(),
                        span: (dir_start, dir_end),
                        rule_name_span: (name_start, name_start + name_str.len()),
                    })
                }).collect();

                // Extract @ws directive — find byte span by searching source text.
                let ws_pattern = pg.ws_pattern.as_ref().and_then(|pat| {
                    let dir_start = src.find("@ws")?;
                    let after_kw = dir_start + "@ws".len();
                    let dir_end = src[after_kw..].find(';').map(|off| after_kw + off + 1).unwrap_or(after_kw);
                    Some(WsPatternInfo {
                        pattern: pat.to_string(),
                        span: (dir_start, dir_end),
                    })
                });

                CachedParseResult {
                    ast: pg.rules,
                    imports,
                    recovers,
                    pretties,
                    debugs,
                    tokens,
                    ws_pattern,
                }
            });
            (cached, diag)
        }
        Err(panic_info) => {
            let msg = if let Some(s) = panic_info.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = panic_info.downcast_ref::<&str>() {
                s.to_string()
            } else {
                panic!("parser panicked with non-string payload")
            };
            let diag = ParseDiagnostics {
                offset: 0,
                furthest_offset: 0,
                panic_message: Some(msg),
            };
            (None, diag)
        }
    }
}
