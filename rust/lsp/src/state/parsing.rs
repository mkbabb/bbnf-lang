use bbnf::grammar::BBNFGrammar;
use bbnf::types::AST;

use self_cell::self_cell;

use super::types::{ImportInfo, RecoverInfo, ParseDiagnostics};

// Self-referential struct: owns the source text and the parsed AST that borrows from it.
self_cell! {
    pub(crate) struct OwnedAst {
        owner: String,
        #[covariant]
        dependent: CachedAst,
    }
}

pub(crate) type CachedAst<'a> = Option<CachedParseResult<'a>>;

/// Holds both the parsed grammar and import/recover directives (borrows from OwnedAst's owner).
pub(crate) struct CachedParseResult<'a> {
    pub(crate) ast: AST<'a>,
    pub(crate) imports: Vec<ImportInfo>,
    pub(crate) recovers: Vec<RecoverInfo>,
}

/// Parse the source text once, returning the cached AST data and diagnostic info.
/// Both are extracted from a single parse call.
pub(crate) fn parse_once(src: &str) -> (Option<CachedParseResult<'_>>, ParseDiagnostics) {
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
                        items.iter().map(|i| i.to_string()).collect()
                    }),
                }).collect();
                let recovers = pg.recovers.iter().map(|rec| RecoverInfo {
                    rule_name: rec.rule_name.to_string(),
                    span: (rec.span.start, rec.span.end),
                    rule_name_span: {
                        // The rule name starts after "@recover " — approximate from directive span.
                        // We'll refine in diagnostics where we have the source text.
                        let name_str = rec.rule_name.as_ref();
                        let dir_src = rec.span.as_str();
                        let name_start = dir_src.find(name_str)
                            .map(|off| rec.span.start + off)
                            .unwrap_or(rec.span.start);
                        (name_start, name_start + name_str.len())
                    },
                }).collect();
                CachedParseResult {
                    ast: pg.rules,
                    imports,
                    recovers,
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
                "Internal parser error".to_string()
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
