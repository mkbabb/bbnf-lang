use bbnf::grammar::BBNFGrammar;
use bbnf::types::AST;

use self_cell::self_cell;

use crate::directives::debug;
use crate::directives::import;
use crate::directives::recover;
use crate::directives::token;
use crate::directives::ws;
use super::pretty::{self, PrettyInfo};
use super::types::{
    DebugInfo, ImportInfo, ParseDiagnostics, RecoverInfo, TokenInfo, WsPatternInfo,
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
                let imports = import::extract_imports(&pg.imports);
                let recovers = recover::extract_recovers(&pg.recovers);
                let pretties = pretty::extract_pretties(&pg.pretties, src);
                let debugs = debug::extract_debugs(&pg.debug_rules, src);
                let tokens = token::extract_tokens(&pg.token_rules, src);
                let ws_pattern = ws::extract_ws_pattern(pg.ws_pattern.as_deref(), src);

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
