//! Full analysis + IR lowering pipeline.
//!
//! Orchestrates: parse → analysis → lower → IR passes, producing a `GrammarIR`
//! ready for consumption by any backend (Rust codegen, bytecode VM, TS interpreter).

use std::collections::{HashMap, HashSet};

use bbnf_ir::GrammarIR;

use crate::analysis::{
    compute_first_sets, find_aliases, find_span_eligible_rules, find_transparent_alternations,
    tarjan_scc, topological_sort_scc,
};
use crate::grammar::BBNFGrammar;
use crate::lower::lower_to_ir;
use crate::optimize::{remove_direct_left_recursion, remove_indirect_left_recursion};
use crate::types::{AST, Expression};

/// Options for the compilation pipeline.
#[derive(Default)]
pub struct PipelineOptions {
    /// Whether to apply left-recursion elimination.
    pub remove_left_recursion: bool,
    /// Override the entry rule name. If `None`, defaults to the last rule in source order.
    pub entry_rule: Option<String>,
}

/// Compile a BBNF grammar source string to a `GrammarIR`.
///
/// This is the main entry point for the WASM bytecode VM path.
/// Parses the grammar, runs all analysis passes, lowers to IR, and runs IR passes.
pub fn compile_grammar(source: &str, options: &PipelineOptions) -> Result<GrammarIR, String> {
    // Leak to get 'static lifetime — the AST borrows from the source string.
    // For WASM usage this is fine: each grammar compilation is a one-shot operation.
    let source_static: &'static str = Box::leak(source.to_string().into_boxed_str());

    let parser = BBNFGrammar::grammar_with_imports();
    let (parsed, _state) = parser.parse_return_state(source_static);

    let parsed = parsed.ok_or_else(|| "Failed to parse grammar".to_string())?;

    // Extract directives.
    let mut recover_map: HashMap<String, Expression<'static>> = HashMap::new();
    let mut pretty_map: HashMap<String, Vec<String>> = HashMap::new();
    let mut no_collapse_set: HashSet<String> = HashSet::new();

    for rec in &parsed.recovers {
        recover_map.insert(rec.rule_name.to_string(), rec.sync_expr.clone());
    }
    for p in &parsed.pretties {
        pretty_map.insert(
            p.rule_name.to_string(),
            p.hints.iter().map(|h| h.to_string()).collect(),
        );
    }
    for nc in &parsed.no_collapses {
        no_collapse_set.insert(nc.rule_name.to_string());
    }

    let ws_pat = parsed.ws_pattern;
    let mut inline_set: HashSet<String> = HashSet::new();
    for name in &parsed.inline_rules {
        inline_set.insert(name.to_string());
    }
    let inline_ref = if inline_set.is_empty() { None } else { Some(&inline_set) };
    let ast = parsed.rules;
    compile_ast(
        ast,
        &recover_map,
        &pretty_map,
        &no_collapse_set,
        options,
        ws_pat.as_deref(),
        inline_ref,
    )
}

/// Compile an already-parsed AST to `GrammarIR`.
///
/// Useful when the AST is already available (e.g., from `DocumentState`).
pub fn compile_ast<'a>(
    ast: AST<'a>,
    recover_map: &HashMap<String, Expression<'a>>,
    pretty_map: &HashMap<String, Vec<String>>,
    no_collapse_set: &HashSet<String>,
    options: &PipelineOptions,
    ws_pattern: Option<&str>,
    inline_rules: Option<&HashSet<String>>,
) -> Result<GrammarIR, String> {
    // Determine the entry rule name: use override if provided, otherwise last rule in source order.
    let entry_rule_name: Option<String> = options.entry_rule.clone().or_else(|| {
        ast.keys().last().and_then(|lhs| {
            if let Expression::Nonterminal(tok) = lhs {
                Some(tok.value.to_string())
            } else {
                None
            }
        })
    });

    // Optional: remove left-recursion (indirect first via Paull's, then direct).
    let ast = if options.remove_left_recursion {
        // Extract multi-member SCC names (owned) so deps/scc_result can be dropped.
        let indirect_sccs = {
            let deps = crate::calculate_ast_deps(&ast);
            let scc_result = tarjan_scc(&deps);
            scc_result
                .sccs
                .iter()
                .filter(|scc| scc.len() > 1)
                .map(|scc| {
                    scc.iter()
                        .filter_map(|expr| match expr {
                            Expression::Nonterminal(tok) => Some(tok.value.to_string()),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        };
        let ast = remove_indirect_left_recursion(&ast, &indirect_sccs);
        remove_direct_left_recursion(&ast)
    } else {
        ast
    };

    // Dependency analysis (recomputed after potential LR transformation).
    let deps = crate::calculate_ast_deps(&ast);

    // SCC detection + topological ordering.
    let scc_result = tarjan_scc(&deps);
    let ast = topological_sort_scc(&ast, &scc_result, &deps);

    // FIRST set computation.
    let first_sets = compute_first_sets(&ast, &deps, &scc_result);

    // Alias, transparent, and span-eligible detection.
    let aliases = find_aliases(&ast, &scc_result.cyclic_rules);
    let transparent_rules = find_transparent_alternations(&ast, &scc_result.cyclic_rules);
    let span_eligible_rules = find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

    // Directive refs.
    let recovers_ref = if recover_map.is_empty() {
        None
    } else {
        Some(recover_map)
    };
    let pretties_ref = if pretty_map.is_empty() {
        None
    } else {
        Some(pretty_map)
    };
    let no_collapse_ref = if no_collapse_set.is_empty() {
        None
    } else {
        Some(no_collapse_set)
    };

    // Dispatch tables (empty for now — IR passes generate these).
    let dispatch_tables = HashMap::new();

    // Lower to IR.
    let mut ir = lower_to_ir(
        &ast,
        &first_sets,
        &scc_result,
        &aliases,
        &transparent_rules,
        &span_eligible_rules,
        recovers_ref,
        pretties_ref,
        no_collapse_ref,
        &dispatch_tables,
        ws_pattern,
        inline_rules,
    );

    // Set the correct entry rule (last rule in original source order).
    if let Some(ref name) = entry_rule_name {
        if let Some(rule) = ir.find_rule(name) {
            ir.entry = rule.id;
        }
    }

    // Run IR optimization passes.
    bbnf_ir::passes::canonicalize_aliases(&mut ir);
    bbnf_ir::passes::prune_unreachable(&mut ir);
    bbnf_ir::passes::inline_acyclic(&mut ir);
    // Force-inline @inline rules at all call sites.
    bbnf_ir::passes::force_inline(&mut ir);
    // Second prune: inlined rules may now be unreachable.
    bbnf_ir::passes::prune_unreachable(&mut ir);
    // Fuse single-use rules into their call sites for better dispatch coverage.
    bbnf_ir::passes::fuse_single_use(&mut ir);
    // Third prune: fused rules may now be unreachable.
    bbnf_ir::passes::prune_unreachable(&mut ir);
    bbnf_ir::passes::eliminate_epsilon(&mut ir);
    bbnf_ir::passes::merge_literals(&mut ir);
    bbnf_ir::passes::merge_regex_alts(&mut ir);
    bbnf_ir::passes::factor_common_prefixes(&mut ir);
    bbnf_ir::passes::refine_span_eligibility(&mut ir);

    // Compute FOLLOW sets before dispatch and memo passes that consume them.
    ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&ir);

    // Factor regex prefixes with lookahead dispatch — restructures Alts where
    // branches share a leading regex but have disjoint continuation FIRST sets.
    bbnf_ir::passes::factor_regex_with_lookahead(&mut ir);

    // Dispatch tables use FOLLOW sets for nullable branch optimization.
    bbnf_ir::passes::generate_dispatch_tables(&mut ir);

    // Memo strategies use FOLLOW set cardinality as a signal.
    bbnf_ir::passes::refine_memo_strategies(&mut ir);

    // Type inference (populates GrammarIR::types for codegen backends).
    bbnf_ir::passes::infer_types(&mut ir);

    Ok(ir)
}