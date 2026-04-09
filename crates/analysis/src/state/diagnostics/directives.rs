use std::collections::{HashMap, HashSet};

use ls_types::*;

use crate::analysis::LineIndex;

use super::super::pretty;
use super::super::types::{
    DebugInfo, ImportInfo, RecoverInfo, SemanticTokenInfo, TokenInfo, WsPatternInfo,
};
use super::super::pretty::PrettyInfo;

/// Run all directive validation + semantic-token emission in the same order as
/// the original monolithic `analyze_from_cache` block. Mirrors the block
/// verbatim, including the post-validation re-inserts into `referenced_names`.
#[allow(clippy::too_many_arguments)]
pub(super) fn validate_directives<'a>(
    import_infos: &'a [ImportInfo],
    recover_infos: &'a [RecoverInfo],
    pretty_infos: &'a [PrettyInfo],
    debug_infos: &'a [DebugInfo],
    token_infos: &'a [TokenInfo],
    ws_pattern_info: Option<&WsPatternInfo>,
    defined: &HashMap<&str, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
    semantic_tokens: &mut Vec<SemanticTokenInfo>,
    referenced_names: &mut HashSet<&'a str>,
) {
    // @import directive semantic tokens.
    semantic_tokens.extend(crate::directives::import::import_semantic_tokens(
        import_infos,
    ));

    // @recover directive validation and semantic tokens.
    {
        let (rec_diags, rec_tokens) = crate::directives::recover::validate_recovers(
            recover_infos,
            defined,
            imported_names,
            line_index,
        );
        diagnostics.extend(rec_diags);
        semantic_tokens.extend(rec_tokens);

        // Mark recover directive rule names as referenced (for unused rule detection).
        for rec in recover_infos {
            referenced_names.insert(&rec.rule_name);
        }
    }

    // @pretty directive validation and semantic tokens.
    {
        let (pretty_diags, pretty_tokens) =
            pretty::validate_pretties(pretty_infos, defined, imported_names, line_index);
        diagnostics.extend(pretty_diags);
        semantic_tokens.extend(pretty_tokens);

        // Mark pretty directive rule names as referenced (for unused rule detection).
        for p in pretty_infos {
            referenced_names.insert(&p.rule_name);
        }
    }

    // @debug directive validation and semantic tokens.
    {
        let (dbg_diags, dbg_tokens) = crate::directives::debug::validate_debugs(
            debug_infos,
            defined,
            imported_names,
            line_index,
        );
        diagnostics.extend(dbg_diags);
        semantic_tokens.extend(dbg_tokens);

        // Mark debug directive rule names as referenced (for unused rule detection).
        for dbg in debug_infos {
            if dbg.rule_name != "*" {
                referenced_names.insert(&dbg.rule_name);
            }
        }
    }

    // @token directive validation and semantic tokens.
    {
        let (tok_diags, tok_tokens) = crate::directives::token::validate_tokens(
            token_infos,
            defined,
            imported_names,
            line_index,
        );
        diagnostics.extend(tok_diags);
        semantic_tokens.extend(tok_tokens);

        // Mark token directive rule names as referenced (for unused rule detection).
        for tok in token_infos {
            referenced_names.insert(&tok.rule_name);
        }
    }

    // @ws directive semantic tokens.
    semantic_tokens.extend(crate::directives::ws::ws_semantic_tokens(ws_pattern_info));
}
