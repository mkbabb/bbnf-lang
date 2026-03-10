use bbnf::grammar::BBNFGrammar;
use bbnf::types::AST;

use self_cell::self_cell;

use super::pretty::{self, PrettyInfo};
use super::types::{ImportInfo, NoCollapseInfo, RecoverInfo, ParseDiagnostics};

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
    pub no_collapses: Vec<NoCollapseInfo>,
    pub pretties: Vec<PrettyInfo>,
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
                        let name_start = dir_src.find(name_str).map(|off| rec.span.start + off).unwrap_or_else(|| {
                            panic!(
                                "could not resolve @recover rule-name span for `{}` within directive `{}`",
                                name_str, dir_src
                            )
                        });
                        (name_start, name_start + name_str.len())
                    },
                }).collect();
                let no_collapses = pg.no_collapses.iter().map(|nc| {
                    let name_str = nc.rule_name.as_ref();
                    let dir_src = nc.span.as_str();
                    let name_start = dir_src.find(name_str).map(|off| nc.span.start + off).unwrap_or_else(|| {
                        panic!(
                            "could not resolve @no_collapse rule-name span for `{}` within directive `{}`",
                            name_str, dir_src
                        )
                    });
                    NoCollapseInfo {
                        rule_name: name_str.to_string(),
                        span: (nc.span.start, nc.span.end),
                        rule_name_span: (name_start, name_start + name_str.len()),
                    }
                }).collect();
                let pretties = pretty::extract_pretties(&pg.pretties, src);
                CachedParseResult {
                    ast: pg.rules,
                    imports,
                    recovers,
                    no_collapses,
                    pretties,
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
