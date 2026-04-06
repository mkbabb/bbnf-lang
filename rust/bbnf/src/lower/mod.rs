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

use crate::graph::first_sets::unwrap_rule;
use crate::graph::{FirstSets, SccResult};
use crate::types::{AST, Expression, Token};

use expression::lower_expression;
use fn_table::FnTable;
use metadata::build_rule_meta;
use string_interner::StringInterner;

/// Convert a `CharSet` ([u32; 4]) to a `CharSet128` ([u64; 2]).
pub(crate) fn charset_to_128(cs: &crate::graph::CharSet) -> bbnf_ir::CharSet128 {
    bbnf_ir::CharSet128::from_u32x4(&cs.bits)
}

/// All directive data extracted from a parsed grammar.
///
/// Encapsulates the 6 directive parameters that were previously passed individually
/// to `lower_to_ir` and `compile_ast`.
pub struct DirectiveSet<'a> {
    pub recovers: Option<&'a HashMap<String, Expression<'a>>>,
    pub pretties: Option<&'a HashMap<String, Vec<String>>>,
    pub ws_pattern: Option<&'a str>,
    pub token_rules: Option<&'a HashSet<String>>,
    pub debug_rules: Option<&'a HashSet<String>>,
    pub debug_all: bool,
    pub host_fns: Option<&'a HashSet<String>>,
}

impl<'a> DirectiveSet<'a> {
    /// Create a `DirectiveSet` with all directives empty/disabled.
    pub fn empty() -> Self {
        Self {
            recovers: None,
            pretties: None,
            ws_pattern: None,
            token_rules: None,
            debug_rules: None,
            debug_all: false,
            host_fns: None,
        }
    }
}

/// Context for the lowering pass.
pub(crate) struct LowerCtx<'a> {
    pub(crate) strings: StringInterner,
    pub(crate) fns: FnTable,

    /// Map from nonterminal name -> RuleId.
    pub(crate) name_to_rule_id: HashMap<&'a str, RuleId>,

    /// Analysis results.
    pub(crate) first_sets: &'a FirstSets<'a>,
    pub(crate) scc_result: &'a SccResult<'a>,
    pub(crate) cyclic_rules: &'a HashSet<Expression<'a>>,

    /// Directives.
    pub(crate) recovers: Option<&'a HashMap<String, Expression<'a>>>,
    pub(crate) pretties: Option<&'a HashMap<String, Vec<String>>>,
    pub(crate) token_rules: Option<&'a HashSet<String>>,
    pub(crate) debug_rules: Option<&'a HashSet<String>>,
    pub(crate) debug_all: bool,
    pub(crate) host_fns: Option<&'a HashSet<String>>,

    /// The current LHS expression being lowered (for branch_firsts lookup).
    pub(crate) current_lhs: Option<&'a Expression<'a>>,

    /// When true, unknown nonterminals emit Epsilon instead of Literal fallback,
    /// and unsupported expressions emit Epsilon. Used for recovery sync expressions.
    pub(crate) recovery_mode: bool,
}

/// Lower a full BBNF grammar (AST + analysis results) to the canonical `GrammarIR`.
///
/// Alias detection, transparent alternation detection, and span eligibility are
/// computed by IR passes (`compute_aliases`, `compute_transparent`,
/// `refine_span_eligibility`) that run post-lowering. This function only needs
/// FIRST sets and SCC results (which are required during expression lowering for
/// per-branch FIRST sets and cycle/memo metadata).
///
/// # Arguments
///
/// * `ast` -- Topologically sorted AST.
/// * `first_sets` -- Pre-computed FIRST sets.
/// * `scc_result` -- SCC analysis results.
/// * `directives` -- All directive data from the parsed grammar.
pub fn lower_to_ir<'a>(
    ast: &'a AST<'a>,
    first_sets: &'a FirstSets<'a>,
    scc_result: &'a SccResult<'a>,
    directives: &'a DirectiveSet<'a>,
) -> GrammarIR {
    let mut ctx = LowerCtx {
        strings: StringInterner::new(),
        fns: FnTable::new(),
        name_to_rule_id: HashMap::new(),
        first_sets,
        scc_result,
        cyclic_rules: &scc_result.cyclic_rules,
        recovers: directives.recovers,
        pretties: directives.pretties,
        token_rules: directives.token_rules,
        debug_rules: directives.debug_rules,
        debug_all: directives.debug_all,
        host_fns: directives.host_fns,
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
    type RuleBody<'b> = (
        RuleId,
        bbnf_ir::StringId,
        bbnf_ir::IrNode,
        &'b str,
        &'b Expression<'b>,
        Option<bbnf_ir::GrammarSpan>,
    );
    let mut rule_bodies: Vec<RuleBody<'_>> = Vec::with_capacity(rule_names.len());

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
        let mut meta = build_rule_meta(lhs, name, &mut ctx);

        // Set debug flag from @debug directive.
        meta.directives.debug =
            ctx.debug_all || ctx.debug_rules.is_some_and(|set| set.contains(name));

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

    let ws_pattern_id = directives.ws_pattern.map(|pat| ctx.strings.intern(pat));

    GrammarIR {
        rules,
        entry,
        strings: ctx.strings.strings,
        fns: ctx.fns.fns,
        types: Vec::new(), // Type info populated by a later pass or backend.
        follow_sets: HashMap::new(), // Populated by compute_follow_sets pass.
        ws_pattern: ws_pattern_id,
        collapse_simple_spans: false, // Set by generate_all based on prettify flag.
        debug_all: ctx.debug_all,
        debug_labels: Vec::new(),
        type_map: None,
    }
}
