//! Metadata lowering: recover directives, pretty hints.

use bbnf_ir::{CharSet128, MemoStrategy, PrettyHints, RuleMeta};

use crate::types::{Expression, Token};

use super::LowerCtx;
use super::expression::lower_expression;

/// Convert a `CharSet` ([u32; 4]) to a `CharSet128` ([u64; 2]).
fn charset_to_128(cs: &crate::analysis::CharSet) -> CharSet128 {
    CharSet128::from_u32x4(&cs.bits)
}

/// Build rule metadata from analysis results.
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

    // Dispatch hint — populated later by the IR `generate_dispatch_tables` pass.
    let dispatch = None;

    // Span eligibility.
    let span_eligible = ctx.span_eligible_rules.contains(name);

    // Alias detection.
    let is_alias = ctx.aliases.get(lhs).and_then(|target| {
        if let Expression::Nonterminal(Token { value, .. }) = target {
            ctx.name_to_rule_id.get(value.as_ref()).copied()
        } else {
            None
        }
    });

    // Transparent alternation.
    let is_transparent = ctx.transparent_rules.contains(name);

    // Pretty hints.
    let pretty = ctx
        .pretties
        .and_then(|p| p.get(name))
        .map(|hint_strs| lower_pretty_hints(hint_strs));

    // Recovery expression (lowered with recovery_mode = true).
    let recover = ctx.recovers.and_then(|r| r.get(name)).map(|sync_expr| {
        ctx.recovery_mode = true;
        let node = lower_expression(sync_expr, ctx);
        ctx.recovery_mode = false;
        node
    });

    // Token rule: implies span_eligible (returns Span even in arena context).
    let is_token = ctx
        .token_rules
        .is_some_and(|set| set.contains(name));
    let span_eligible = span_eligible || is_token;

    RuleMeta {
        first_set,
        nullable,
        scc_id,
        is_cyclic,
        memo,
        dispatch,
        span_eligible,
        is_alias,
        is_transparent,
        pretty,
        recover,
        is_token,
        debug: false, // Set by caller from @debug directives.
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
                if h.starts_with("sep(\"") && h.ends_with("\")") {
                    ph.sep = Some(h[5..h.len() - 2].to_string());
                } else if h.starts_with("split(\"") && h.ends_with("\")") {
                    ph.split = Some(h[7..h.len() - 2].to_string());
                }
                // Unknown hints are silently ignored (validation happens earlier).
            }
        }
    }

    ph
}
