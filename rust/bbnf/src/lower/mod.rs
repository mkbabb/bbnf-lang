//! Lowering pass: Expression AST -> GrammarIR.
//!
//! This module converts the parsed BBNF `Expression` AST (with all analysis results)
//! into the canonical `GrammarIR` consumed by all backends.

mod expression;
mod fn_table;
mod metadata;
mod string_interner;

use std::collections::{HashMap, HashSet};

use bbnf_ir::{GrammarIR, IrRule, RuleId};

use crate::analysis::{DispatchTable, FirstSets, SccResult};
use crate::types::{Expression, Token, AST};

use expression::{lower_expression, unwrap_rule};
use fn_table::FnTable;
use metadata::build_rule_meta;
use string_interner::StringInterner;

/// Context for the lowering pass.
pub(crate) struct LowerCtx<'a> {
    pub(crate) strings: StringInterner,
    pub(crate) fns: FnTable,

    /// Map from nonterminal name -> RuleId.
    pub(crate) name_to_rule_id: HashMap<&'a str, RuleId>,

    /// Analysis results.
    pub(crate) first_sets: &'a FirstSets<'a>,
    pub(crate) scc_result: &'a SccResult<'a>,

    /// Metadata maps.
    pub(crate) aliases: &'a HashMap<&'a Expression<'a>, &'a Expression<'a>>,
    pub(crate) transparent_rules: &'a HashSet<String>,
    pub(crate) span_eligible_rules: &'a HashSet<String>,
    pub(crate) cyclic_rules: &'a HashSet<Expression<'a>>,

    /// Directives.
    pub(crate) recovers: Option<&'a HashMap<String, Expression<'a>>>,
    pub(crate) pretties: Option<&'a HashMap<String, Vec<String>>>,
    pub(crate) no_collapse_rules: Option<&'a HashSet<String>>,
    pub(crate) inline_rules: Option<&'a HashSet<String>>,
    pub(crate) debug_rules: Option<&'a HashSet<String>>,
    pub(crate) debug_all: bool,

    /// The current LHS expression being lowered (for branch_firsts lookup).
    pub(crate) current_lhs: Option<&'a Expression<'a>>,

    /// When true, unknown nonterminals emit Epsilon instead of Literal fallback,
    /// and unsupported expressions emit Epsilon. Used for recovery sync expressions.
    pub(crate) recovery_mode: bool,
}

/// Lower a full BBNF grammar (AST + analysis results) to the canonical `GrammarIR`.
///
/// # Arguments
///
/// * `ast` -- Topologically sorted AST.
/// * `first_sets` -- Pre-computed FIRST sets.
/// * `scc_result` -- SCC analysis results.
/// * `aliases` -- Alias detection results.
/// * `transparent_rules` -- Transparent alternation rule names.
/// * `span_eligible_rules` -- Span-eligible rule names.
/// * `recovers` -- `@recover` directive map (rule_name -> sync expression).
/// * `pretties` -- `@pretty` directive map (rule_name -> hint strings).
/// * `no_collapse_rules` -- `@no_collapse` rule name set.
/// * `dispatch_tables` -- Pre-built dispatch tables for alternation rules.
#[allow(clippy::too_many_arguments)]
pub fn lower_to_ir<'a>(
    ast: &'a AST<'a>,
    first_sets: &'a FirstSets<'a>,
    scc_result: &'a SccResult<'a>,
    aliases: &'a HashMap<&'a Expression<'a>, &'a Expression<'a>>,
    transparent_rules: &'a HashSet<String>,
    span_eligible_rules: &'a HashSet<String>,
    recovers: Option<&'a HashMap<String, Expression<'a>>>,
    pretties: Option<&'a HashMap<String, Vec<String>>>,
    no_collapse_rules: Option<&'a HashSet<String>>,
    dispatch_tables: &'a HashMap<String, DispatchTable>,
    ws_pattern: Option<&str>,
    inline_rules: Option<&'a HashSet<String>>,
    debug_rules: Option<&'a HashSet<String>>,
    debug_all: bool,
) -> GrammarIR {
    let mut ctx = LowerCtx {
        strings: StringInterner::new(),
        fns: FnTable::new(),
        name_to_rule_id: HashMap::new(),
        first_sets,
        scc_result,
        aliases,
        transparent_rules,
        span_eligible_rules,
        cyclic_rules: &scc_result.cyclic_rules,
        recovers,
        pretties,
        no_collapse_rules,
        inline_rules,
        debug_rules,
        debug_all,
        current_lhs: None,
        recovery_mode: false,
    };

    // Phase 1: Assign RuleIds to all nonterminal names.
    let mut rule_names: Vec<&str> = Vec::new();
    for (lhs, _) in ast.iter() {
        if let Expression::Nonterminal(Token { value, .. }) = lhs {
            let id = rule_names.len() as RuleId;
            ctx.name_to_rule_id.insert(value.as_ref(), id);
            rule_names.push(value.as_ref());
        }
    }

    // Phase 2: Lower rule bodies (interns all body strings).
    let mut rule_bodies: Vec<(
        RuleId,
        bbnf_ir::StringId,
        bbnf_ir::IrNode,
        &str,
        &Expression,
        Option<bbnf_ir::GrammarSpan>,
    )> = Vec::with_capacity(rule_names.len());

    for (lhs, rhs) in ast.iter() {
        let (name, source_span) = match lhs {
            Expression::Nonterminal(Token { value, span, .. }) => {
                let gs = bbnf_ir::GrammarSpan {
                    start: span.start as u32,
                    end: span.end as u32,
                };
                (value.as_ref(), Some(gs))
            }
            _ => continue,
        };

        let rule_id = ctx.name_to_rule_id[name];
        let name_id = ctx.strings.intern(name);

        // Unwrap Rule wrapper to get the body expression.
        let body_expr = unwrap_rule(rhs);

        // Set current LHS for branch_firsts lookup during alternation lowering.
        ctx.current_lhs = Some(lhs);

        // Lower the body expression.
        let body = lower_expression(body_expr, &mut ctx);

        rule_bodies.push((rule_id, name_id, body, name, lhs, source_span));
    }

    // Phase 3: Build metadata (recovery expressions can now intern strings too).
    let mut rules = Vec::with_capacity(rule_bodies.len());
    for (rule_id, name_id, body, name, lhs, source_span) in rule_bodies {
        let mut meta = build_rule_meta(lhs, name, &mut ctx, dispatch_tables);

        // Set debug flag from @debug directive.
        meta.debug = ctx.debug_all
            || ctx
                .debug_rules
                .map_or(false, |set| set.contains(name));

        rules.push(IrRule {
            id: rule_id,
            name: name_id,
            body,
            meta,
            source_span,
        });
    }

    // Default entry: last rule in the (topologically sorted) IR.
    // Callers should override this with the actual entry rule from the
    // original source order (e.g., the last rule in the grammar file).
    let entry = rules.last().map(|r| r.id).unwrap_or(0);

    let ws_pattern_id = ws_pattern.map(|pat| ctx.strings.intern(pat));

    GrammarIR {
        rules,
        entry,
        strings: ctx.strings.strings,
        fns: ctx.fns.fns,
        types: Vec::new(), // Type info populated by a later pass or backend.
        follow_sets: HashMap::new(), // Populated by compute_follow_sets pass.
        ws_pattern: ws_pattern_id,
        b1_span_collapse: false, // Set by generate_all based on prettify flag.
        debug_all: ctx.debug_all,
        debug_labels: Vec::new(),
    }
}
