//! Lowering pass: BbnfBootstrapEnum → GrammarIR.
//!
//! Converts the bootstrap parse tree directly into the canonical `GrammarIR`
//! consumed by all backends. No intermediate Expression AST.

mod expression;
mod fn_table;
mod metadata;
mod string_interner;
pub(crate) mod value_expr;

use std::collections::{HashMap, HashSet};

use bbnf_ir::{GrammarIR, IrRule, RuleId};

use crate::grammar::generated::BbnfBootstrapEnum;
use crate::graph::SccResult;
use crate::types::AST;

use expression::lower_rhs;
use fn_table::FnTable;
use metadata::build_rule_meta;
use string_interner::StringInterner;

/// A grammar function definition (first-class closure).
///
/// Grammar functions are the universal abstraction — `|params| body` binds
/// parameters to grammar expressions. Application (`name(args)`) performs
/// beta-reduction at compile time: substitute params with args, then lower.
/// Closures never reach the IR.
pub(crate) struct ClosureDef<'a> {
    pub(crate) params: Vec<&'a str>,
    pub(crate) body: &'a BbnfBootstrapEnum<'a>,
}

/// Context for the lowering pass.
pub(crate) struct LowerCtx<'a> {
    pub(crate) strings: StringInterner,
    pub(crate) fns: FnTable,

    /// Map from nonterminal name -> RuleId.
    pub(crate) name_to_rule_id: HashMap<&'a str, RuleId>,

    /// Registered grammar closures (expanded at call sites, never reach IR).
    pub(crate) closures: HashMap<&'a str, ClosureDef<'a>>,

    /// Beta-reduction environment stack. Each frame maps a closure parameter
    /// name to the CST node it's bound to. Identifier resolution checks the
    /// stack from top to bottom *before* falling back to the rule table —
    /// this is what makes grammar function applications work without a
    /// parallel substitution walker. Pushed by `lower_term_dispatch` when it
    /// expands a grammar call, popped after the body is lowered.
    pub(crate) env: Vec<HashMap<&'a str, &'a BbnfBootstrapEnum<'a>>>,

    /// Value-expression beta-reduction environment stack. Mirrors `env` but
    /// for the value-expression sub-language: each frame maps a value-closure
    /// parameter name to the already-lowered `MapExpr` it's bound to. Pushed
    /// by `lower_value_expr_with_bindings`, popped after the body is lowered.
    /// `lower_value_expr` consults this stack at `value_ident` before
    /// constructing a `FnCall` — eliminates the `lower_value_expr_substituted`
    /// parallel walker.
    pub(crate) value_env: Vec<HashMap<&'a str, bbnf_ir::MapExpr>>,

    /// Analysis results.
    pub(crate) scc_result: &'a SccResult<'a>,
    pub(crate) cyclic_rules: &'a HashSet<&'a str>,

    /// Directives.
    pub(crate) recovers: Option<&'a HashMap<String, &'a BbnfBootstrapEnum<'a>>>,
    pub(crate) pretties: Option<&'a HashMap<String, Vec<String>>>,
    pub(crate) token_rules: Option<&'a HashSet<String>>,
    pub(crate) debug_rules: Option<&'a HashSet<String>>,
    pub(crate) debug_all: bool,
    pub(crate) host_fns: Option<&'a HashMap<String, Option<String>>>,

    /// When true, unknown nonterminals emit Epsilon instead of panicking.
    /// Used for recovery sync expressions.
    pub(crate) recovery_mode: bool,
}

/// All directive data extracted from a parsed grammar.
pub struct DirectiveSet<'a> {
    pub recovers: Option<&'a HashMap<String, &'a BbnfBootstrapEnum<'a>>>,
    pub pretties: Option<&'a HashMap<String, Vec<String>>>,
    pub ws_pattern: Option<&'a str>,
    pub token_rules: Option<&'a HashSet<String>>,
    pub debug_rules: Option<&'a HashSet<String>>,
    pub debug_all: bool,
    pub host_fns: Option<&'a HashMap<String, Option<String>>>,
}

impl<'a> DirectiveSet<'a> {
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

/// Lower a full BBNF grammar (AST + analysis results) to the canonical `GrammarIR`.
pub fn lower_to_ir<'a>(
    ast: &'a AST<'a>,
    scc_result: &'a SccResult<'a>,
    directives: &'a DirectiveSet<'a>,
    closure_defs: &'a [(&'a str, &'a BbnfBootstrapEnum<'a>)],
) -> GrammarIR {
    // Pre-register closure definitions.
    let mut closures = HashMap::new();
    for &(name, body) in closure_defs {
        if let Some(def) = extract_closure_def(body) {
            closures.insert(name, def);
        }
    }

    let mut ctx = LowerCtx {
        strings: StringInterner::new(),
        fns: FnTable::new(),
        name_to_rule_id: HashMap::new(),
        closures,
        env: Vec::new(),
        value_env: Vec::new(),
        scc_result,
        cyclic_rules: &scc_result.cyclic_rules,
        recovers: directives.recovers,
        pretties: directives.pretties,
        token_rules: directives.token_rules,
        debug_rules: directives.debug_rules,
        debug_all: directives.debug_all,
        host_fns: directives.host_fns,
        recovery_mode: false,
    };

    // Phase 1: Assign RuleIds to all rules.
    let mut rule_names: Vec<&str> = Vec::new();
    for (&name, _) in ast.iter() {
        if ctx.closures.contains_key(name) {
            continue;
        }
        let id = rule_names.len() as RuleId;
        ctx.name_to_rule_id.insert(name, id);
        rule_names.push(name);
    }

    // Phase 2: Lower non-closure rule bodies.
    type RuleBody<'b> = (
        RuleId,
        bbnf_ir::StringId,
        bbnf_ir::IrNode,
        &'b str,
        Option<bbnf_ir::GrammarSpan>,
    );
    let mut rule_bodies: Vec<RuleBody<'_>> = Vec::with_capacity(rule_names.len());

    for (&name, entry) in ast.iter() {
        if ctx.closures.contains_key(name) {
            continue;
        }

        let rule_id = ctx.name_to_rule_id[name];
        let name_id = ctx.strings.intern(name);

        let source_span = Some(bbnf_ir::GrammarSpan {
            start: entry.name_span.start as u32,
            end: entry.name_span.end as u32,
        });

        let body = lower_rhs(entry.rhs, &mut ctx);

        rule_bodies.push((rule_id, name_id, body, name, source_span));
    }

    // Phase 3: Build metadata.
    let mut rules = Vec::with_capacity(rule_bodies.len());
    for (rule_id, name_id, body, name, source_span) in rule_bodies {
        let mut meta = build_rule_meta(name, &mut ctx);

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

    let entry = rules.last().map(|r| r.id).unwrap_or(0);
    let ws_pattern_id = directives.ws_pattern.map(|pat| ctx.strings.intern(pat));

    GrammarIR {
        rules,
        entry,
        strings: ctx.strings.strings,
        fns: ctx.fns.fns,
        types: Vec::new(),
        follow_sets: HashMap::new(),
        ws_pattern: ws_pattern_id,
        collapse_simple_spans: false,
        debug_all: ctx.debug_all,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: HashMap::new(),
        regex_info: HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: HashMap::new(),
        key_dispatch_configs: HashMap::new(),
        context_facts: HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::from_env(),
    }
}

/// Extract a ClosureDef from a bootstrap node if it's a closure.
fn extract_closure_def<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Option<ClosureDef<'a>> {
    match node {
        BbnfBootstrapEnum::closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            let mut param_names: Vec<&'a str> = Vec::new();
            let first_name = crate::grammar::generated::BbnfBootstrapEnum::span_text(first_param);
            if !first_name.is_empty() {
                param_names.push(first_name);
            }
            for (_comma, name) in *rest_params {
                let n = match name {
                    BbnfBootstrapEnum::identifier(s) => s.as_str(),
                    other => crate::grammar::generated::BbnfBootstrapEnum::span_text(other),
                };
                if !n.is_empty() {
                    param_names.push(n);
                }
            }
            Some(ClosureDef {
                params: param_names,
                body,
            })
        }
        // Unwrap alternation/concatenation wrappers that might wrap a closure.
        BbnfBootstrapEnum::alternation(branches) if branches.len() == 1 => {
            extract_closure_def(branches[0].0)
        }
        BbnfBootstrapEnum::concatenation(parts) if parts.len() == 1 => {
            extract_closure_def(parts[0].0)
        }
        BbnfBootstrapEnum::binary_factor((first, rest)) if rest.is_empty() => {
            extract_closure_def(first)
        }
        BbnfBootstrapEnum::mapped_factor((inner, None)) => extract_closure_def(inner),
        BbnfBootstrapEnum::factor((None, inner, None, None)) => extract_closure_def(inner),
        _ => None,
    }
}
