//! Lowering pass: BbnfView → GrammarIR.
//!
//! Converts the struct-direct BBNF parse tree (rooted at a
//! [`BbnfDocument`]) directly into the canonical `GrammarIR`
//! consumed by all backends. No intermediate Expression AST.
//!
//! AZ-II.cutover.D1 — every CST descent goes through
//! [`BbnfView::compound_kind`] / [`BbnfView::span_text`] /
//! [`BbnfView::children`] / [`BbnfView::child`]. Lowering is now
//! rooted in the struct-direct accessor surface.

mod expression;
mod fn_table;
mod metadata;
mod string_interner;
pub(crate) mod view_walk;
pub(crate) mod value_expr;

use std::collections::{HashMap, HashSet};

use bbnf_ir::{GrammarIR, IrRule, RuleId};

use crate::graph::SccResult;
use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};
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
    pub(crate) body: BbnfView<'a, 'a>,
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
    /// name to the CST view it's bound to. Identifier resolution checks the
    /// stack from top to bottom *before* falling back to the rule table —
    /// this is what makes grammar function applications work without a
    /// parallel substitution walker. Pushed by `lower_term_dispatch` when it
    /// expands a grammar call, popped after the body is lowered.
    pub(crate) env: Vec<HashMap<&'a str, BbnfView<'a, 'a>>>,

    /// Value-expression beta-reduction environment stack. Mirrors `env` but
    /// for the value-expression sub-language: each frame maps a value-closure
    /// parameter name to the already-lowered `MapExpr` it's bound to. Pushed
    /// by `lower_value_closure` before lowering the body, popped afterwards.
    /// `lower_value_expr` consults this stack at every bare-identifier site
    /// before constructing a `FnCall` — eliminates the parallel substitution
    /// walker.
    pub(crate) value_env: Vec<HashMap<&'a str, bbnf_ir::MapExpr>>,

    /// Analysis results.
    pub(crate) scc_result: &'a SccResult<'a>,
    pub(crate) cyclic_rules: &'a HashSet<&'a str>,

    /// Directives.
    pub(crate) recovers: Option<&'a HashMap<String, BbnfView<'a, 'a>>>,
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
    pub recovers: Option<&'a HashMap<String, BbnfView<'a, 'a>>>,
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
    closure_defs: &'a [(&'a str, BbnfView<'a, 'a>)],
) -> GrammarIR {
    // Pre-register closure definitions.
    let mut closures = HashMap::new();
    for (name, body) in closure_defs.iter().copied() {
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
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: HashMap::new(),
        eclass_facts: HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        payload_layouts: HashMap::new(),
        string_index: HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),
        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        struct_registry: bbnf_ir::StructRegistry::default(),
    }
}

/// Whether a view's byte-span has zero length — the sentinel for an
/// absent-optional slot in a positional compound layout.
#[inline]
fn is_empty_span(view: BbnfView<'_, '_>) -> bool {
    match view.byte_span() {
        Some((lo, hi)) => lo == hi,
        // Numeric / boolean / unit leaves carry no source span; they
        // are always semantically present (the parse value IS the
        // signal), so they're never "empty placeholders".
        None => false,
    }
}

/// Extract a ClosureDef from a node if it's a closure.
fn extract_closure_def<'a>(node: BbnfView<'a, 'a>) -> Option<ClosureDef<'a>> {
    match node.compound_kind()? {
        BbnfCompoundKind::Closure => {
            // closure = "|", first_param, (",", param)*, "|", body
            // child layout: (0: "|", 1: first_param, 2: rest_list, 3: "|", 4: body)
            let first_param = node.child(1)?;
            let rest_params = node.child(2);
            let body = node.child(4)?;
            let mut param_names: Vec<&'a str> = Vec::new();
            let first_name = first_param.span_text();
            if !first_name.is_empty() {
                param_names.push(first_name);
            }
            if let Some(rest) = rest_params {
                for pair in rest.children() {
                    // pair = (",", identifier)
                    if let Some(name_node) = pair.child(1) {
                        let n = name_node.span_text();
                        if !n.is_empty() {
                            param_names.push(n);
                        }
                    }
                }
            }
            Some(ClosureDef {
                params: param_names,
                body,
            })
        }
        // Unwrap alternation/concatenation wrappers that might wrap a closure.
        BbnfCompoundKind::Alternation | BbnfCompoundKind::CallArg => {
            let mut iter = node.children();
            let first = iter.next()?;
            if iter.next().is_none() {
                // Single-branch alternation — the first child is a
                // `(branch, pipe)` pair whose child(0) is the branch.
                let branch = first.child(0).unwrap_or(first);
                extract_closure_def(branch)
            } else {
                None
            }
        }
        BbnfCompoundKind::Concatenation => {
            let mut iter = node.children();
            let first = iter.next()?;
            if iter.next().is_none() {
                let part = first.child(0).unwrap_or(first);
                extract_closure_def(part)
            } else {
                None
            }
        }
        BbnfCompoundKind::BinaryFactor => {
            // binary_factor = (first, (op, operand)*) — unwrap when
            // the rest list is empty.
            let first = node.child(0)?;
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                extract_closure_def(first)
            } else {
                None
            }
        }
        BbnfCompoundKind::MappedFactor => {
            // mapped_factor = (inner, mapping?) — unwrap when the
            // mapping slot is absent.
            let inner = node.child(0)?;
            let mapping = node.child(1);
            if mapping.is_none() || mapping.map(|m| is_empty_span(m)).unwrap_or(false) {
                extract_closure_def(inner)
            } else {
                None
            }
        }
        BbnfCompoundKind::Factor => {
            // factor = (comment_before?, term, modifier?, comment_after?)
            // — unwrap when all three optional slots are absent.
            let term = node.child(1)?;
            let modifier = node.child(2);
            let comment_before = node.child(0);
            let comment_after = node.child(3);
            let all_bare = comment_before.map(|c| is_empty_span(c)).unwrap_or(true)
                && modifier.map(|c| is_empty_span(c)).unwrap_or(true)
                && comment_after.map(|c| is_empty_span(c)).unwrap_or(true);
            if all_bare {
                extract_closure_def(term)
            } else {
                None
            }
        }
        _ => None,
    }
}
