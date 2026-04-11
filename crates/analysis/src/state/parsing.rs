use bbnf::grammar;
use bbnf::types::AST;

use self_cell::self_cell;

use super::pretty::{self, PrettyInfo};
use super::types::{
    DebugInfo, ImportInfo, ParseDiagnostics, RecoverInfo, TokenInfo, WsPatternInfo,
};
use crate::directives::debug;
use crate::directives::import;
use crate::directives::recover;
use crate::directives::token;
use crate::directives::ws;

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
///
/// Post-Tranche AC.2, `grammar::parse_with_state` returns an owning
/// `Option<ParsedGrammar<'_>>` — the tape-first bootstrap parser
/// manages its own `ParserState` internally and no longer surfaces
/// `furthest_offset` to callers. Offsets default to `0` in the
/// resulting `ParseDiagnostics`; the richer error information that
/// used to live in `parser_state` is now surfaced via the
/// tape-first error path (`ParseErr::Syntax { offset, rule }`) and
/// will be threaded back through diagnostics in a follow-up tranche.
pub fn parse_once(src: &str) -> (Option<CachedParseResult<'_>>, ParseDiagnostics) {
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        grammar::parse_with_state(src)
    }));

    match parse_result {
        Ok(result) => {
            let diag = ParseDiagnostics {
                offset: 0,
                furthest_offset: 0,
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
