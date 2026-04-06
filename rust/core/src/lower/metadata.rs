//! Metadata lowering: recover directives, pretty hints.
//!
//! Alias detection, transparent alternation detection, and span eligibility are
//! NOT computed here -- they are handled by IR passes (`compute_aliases`,
//! `compute_transparent`, `refine_span_eligibility`) that run post-lowering.

use bbnf_ir::{MemoStrategy, PrettyHints, RuleDirectives, RuleMeta};

use crate::types::Expression;

use super::LowerCtx;
use super::charset_to_128;
use super::expression::lower_expression;

/// Build rule metadata from analysis results.
///
/// Sets FIRST set, nullability, SCC info, memo strategy, pretty hints, recovery
/// expression, and token flag. Alias detection (`is_alias`), transparent
/// alternation (`is_transparent`), and span eligibility (`span_eligible`) are
/// left at their defaults -- they are computed by IR passes post-lowering.
pub(crate) fn build_rule_meta<'a>(
    lhs: &'a Expression<'a>,
    name: &str,
    ctx: &mut LowerCtx<'a>,
) -> RuleMeta {
    // FIRST set.
    let first_set = ctx
        .first_sets
        .first
        .get(lhs)
        .map(charset_to_128)
        .unwrap_or_default();

    // Nullability.
    let nullable = ctx.first_sets.nullable.contains(lhs);

    // SCC info.
    let scc_id = ctx.scc_result.scc_index.get(lhs).map(|&id| id as u32);
    let is_cyclic = ctx.cyclic_rules.contains(lhs);

    // Memoization strategy.
    let memo = if is_cyclic {
        MemoStrategy::Full
    } else {
        MemoStrategy::None
    };

    // Pretty hints.
    let pretty = ctx
        .pretties
        .and_then(|p| p.get(name))
        .map(|hint_strs| lower_pretty_hints(hint_strs));

    // Recovery expression (lowered with recovery_mode = true).
    let recover = ctx.recovers.and_then(|r| r.get(name)).map(|sync_expr| {
        debug_assert!(!ctx.recovery_mode, "nested recovery_mode is a bug");
        ctx.recovery_mode = true;
        let node = lower_expression(sync_expr, ctx);
        ctx.recovery_mode = false;
        node
    });

    // Token rule flag (span eligibility is refined by IR pass, but is_token
    // is a directive that we record here).
    let is_token = ctx.token_rules.is_some_and(|set| set.contains(name));

    RuleMeta {
        first_set,
        nullable,
        scc_id,
        is_cyclic,
        memo,
        dispatch: None,        // Populated by generate_dispatch_tables IR pass.
        span_eligible: false,  // Populated by refine_span_eligibility IR pass.
        is_alias: None,        // Populated by compute_aliases IR pass.
        is_transparent: false, // Populated by compute_transparent IR pass.
        directives: RuleDirectives {
            pretty,
            recover,
            token: is_token,
            debug: false, // Set by caller from @debug directives.
        },
        has_sp_method: false, // Computed by compute_sp_method_rules pass.
        sub_variants: Vec::new(),
    }
}

/// Lower `@pretty` hint strings into structured `PrettyHints`.
fn lower_pretty_hints(hint_strs: &[String]) -> PrettyHints {
    let mut ph = PrettyHints::default();

    for hint in hint_strs {
        let h = hint.as_str();
        match h {
            "group" => ph.group = true,
            "indent" => ph.indent = true,
            "dedent" => ph.dedent = true,
            "block" => ph.block = true,
            "blankline" => ph.blankline = true,
            "nobreak" => ph.nobreak = true,
            "softbreak" => ph.softbreak = true,
            "hardbreak" => ph.hardbreak = true,
            "compact" => ph.compact = true,
            "fast" => ph.fast = true,
            "off" => ph.off = true,
            _ => {
                // Check for parameterized hints.
                if let Some(sep) = bbnf_ir::parse_sep_hint(h) {
                    ph.sep = Some(sep.to_string());
                } else if let Some(split) = bbnf_ir::parse_split_hint(h) {
                    ph.split = Some(split.to_string());
                }
                // Validation owns unknown-hint errors. `split("...")` is preserved
                // so prettify codegen can surface the explicit unsupported-path error.
            }
        }
    }

    ph
}
