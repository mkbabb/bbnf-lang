use std::path::PathBuf;

use bbnf_ir::GrammarIR;

use crate::grammar;
use crate::grammar::generated::BbnfBootstrapEnum;
use crate::graph::{tarjan_scc, topological_sort_scc};
use crate::backend::prepare_grammar;
use crate::lower::{DirectiveSet, lower_to_ir};
use crate::pipeline::directives::{DirectiveMaps, load_merged_paths};
use crate::pipeline::validate::{validate_ast, validate_pretty_directives};
use crate::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use crate::types::AST;

/// Compile a BBNF grammar source string to a VM-ready `GrammarIR`.
///
/// This keeps the existing VM-facing API intact while routing through the shared
/// request pipeline.
pub fn compile_grammar(source: &str, options: &PipelineOptions) -> Result<GrammarIR, String> {
    let request = CompileRequest {
        options: options.clone(),
        target: CompileTarget::Vm,
    };
    match compile_grammar_request(source, &request) {
        Ok(CompileOutput::Vm(ir)) => Ok(ir),
        Ok(_) => unreachable!("VM wrapper received non-VM output"),
        Err(err) => Err(err.to_string()),
    }
}

/// Compile a grammar source string for the requested backend.
pub fn compile_grammar_request(
    source: &str,
    request: &CompileRequest,
) -> Result<CompileOutput, CompileError> {
    let parsed = grammar::parse(source)
        .ok_or_else(|| CompileError::Parse("failed to parse grammar".to_string()))?;

    if !parsed.imports.is_empty() {
        return Err(CompileError::Import(
            "compile_grammar(source) does not resolve @import; use compile_paths_request"
                .to_string(),
        ));
    }

    let (ast, directive_maps) = DirectiveMaps::from_parsed(parsed);
    let directives = directive_maps.as_directive_set();
    compile_ast_request(ast, &directives, request)
}

/// Compile explicit grammar paths for the requested backend.
pub fn compile_paths_request(
    paths: &[PathBuf],
    request: &CompileRequest,
) -> Result<CompileOutput, CompileError> {
    let merged = load_merged_paths(paths)?;
    let directives = merged.directives.as_directive_set();
    compile_ast_request_internal(merged.ast, &directives, request)
}

/// Compile an already-parsed AST to a VM-ready `GrammarIR`.
///
/// Useful when the AST is already available (e.g., from `DocumentState`).
pub fn compile_ast<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    options: &PipelineOptions,
) -> Result<GrammarIR, String> {
    let request = CompileRequest {
        options: options.clone(),
        target: CompileTarget::Vm,
    };
    match compile_ast_request(ast, directives, &request) {
        Ok(CompileOutput::Vm(ir)) => Ok(ir),
        Ok(_) => unreachable!("VM wrapper received non-VM output"),
        Err(err) => Err(err.to_string()),
    }
}

/// Compile an already-parsed AST for the requested backend.
pub fn compile_ast_request<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    request: &CompileRequest,
) -> Result<CompileOutput, CompileError> {
    compile_ast_request_internal(ast, directives, request)
}

fn compile_ast_request_internal<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    request: &CompileRequest,
) -> Result<CompileOutput, CompileError> {
    validate_pretty_directives(&ast, directives.pretties)?;
    let ir = compile_ast_common(ast, directives, &request.options)?;
    finalize_compile(ir, &request.target)
}

fn finalize_compile(
    mut ir: GrammarIR,
    target: &CompileTarget,
) -> Result<CompileOutput, CompileError> {
    match target {
        CompileTarget::Rust { requested_prettify } => Ok(CompileOutput::Rust(prepare_grammar(
            ir,
            *requested_prettify,
        ))),
        CompileTarget::Vm => {
            bbnf_ir::passes::project_types(&mut ir);
            Ok(CompileOutput::Vm(ir))
        }
        CompileTarget::Ts => {
            bbnf_ir::passes::compute_sp_method_rules(&mut ir);
            bbnf_ir::passes::project_types(&mut ir);

            let entry_name = ir.get_string(ir.rules[ir.entry as usize].name).to_string();
            let enum_name = format!("{entry_name}Value");

            let analysis = crate::backend::driver::analysis::BackendAnalysis::default();
            let call_strategies = compute_call_strategies(&ir);
            let mut dstate = crate::backend::driver::DriverState::new(call_strategies);
            install_pattern_caches(&mut dstate, &ir);
            let mut emitter = crate::backend::ts::TsEmitter { enum_name };
            let mut ctx = crate::backend::ts::emitter::TsEmitCtx::default();

            let code =
                crate::backend::driver::compile_grammar(&ir, &analysis, &mut dstate, &mut emitter, &mut ctx);
            let output = if code.stmts.is_empty() { code.expr } else { format!("{}\n{}", code.stmts, code.expr) };
            Ok(CompileOutput::Ts(output))
        }
        CompileTarget::Wasm => {
            bbnf_ir::passes::compute_sp_method_rules(&mut ir);
            bbnf_ir::passes::project_types(&mut ir);

            let entry_name = ir.get_string(ir.rules[ir.entry as usize].name).to_string();
            let module_name = format!("{entry_name}_parser");

            let analysis = crate::backend::driver::analysis::BackendAnalysis::default();
            let call_strategies = compute_call_strategies(&ir);
            let mut dstate = crate::backend::driver::DriverState::new(call_strategies);
            install_pattern_caches(&mut dstate, &ir);
            // Pre-register ws pattern so the emitter knows its ID.
            let ws_regex_id = ir.ws_pattern.map(|ws_sid| {
                dstate.register_regex(ir.get_string(ws_sid))
            });
            let mut emitter = crate::backend::wasm::WasmEmitter {
                module_name,
                ws_regex_id,
            };
            let mut ctx = crate::backend::wasm::emitter::WasmEmitCtx::default();

            let wat_source =
                crate::backend::driver::compile_grammar(&ir, &analysis, &mut dstate, &mut emitter, &mut ctx);
            Ok(CompileOutput::Wasm(wat_source.into_bytes()))
        }
    }
}

/// Populate the pattern detection caches on a fresh `DriverState`
/// (Tranche F): `solve_alt_strategies`, `solve_delim_scan_configs`,
/// `solve_key_dispatch_configs` run once per grammar; the driver
/// then looks up the pre-solved results at compile time.
fn install_pattern_caches(dstate: &mut crate::backend::driver::DriverState, ir: &GrammarIR) {
    dstate.alt_strategies =
        crate::backend::strategy::alt_strategy::solve_alt_strategies(ir);
    dstate.delim_scan_configs =
        crate::backend::patterns::cache::solve_delim_scan_configs(ir);
    dstate.key_dispatch_configs =
        crate::backend::patterns::cache::solve_key_dispatch_configs(ir);
}

/// Compute call strategies using the shared inline analysis.
///
/// Converts `CallMode::DirectCall` → `CallStrategy::DirectCall`,
/// `CallMode::InlineBody` → `CallStrategy::InlineBody`.
pub fn compute_call_strategies(ir: &GrammarIR) -> Vec<crate::backend::CallStrategy> {
    use crate::backend::CallStrategy;
    use std::collections::HashSet;

    // Read operator chain rules from NodeFacts (computed by
    // `recognize_patterns` pass; keyed by stable `NodeId` via
    // `ir.dag`).
    let operator_chain_rules: HashSet<bbnf_ir::RuleId> = ir
        .rules
        .iter()
        .filter(|rule| {
            ir.dag
                .as_ref()
                .and_then(|dag| dag.node_for(&rule.body))
                .and_then(|id| ir.node_facts.get(&id))
                .is_some_and(|f| f.operator_chain)
        })
        .map(|r| r.id)
        .collect();

    let plan =
        crate::backend::rust::analysis::inline::analyze_parse_inline_plan(ir, &operator_chain_rules);

    plan.parse_call_modes
        .iter()
        .map(|mode| match mode {
            crate::backend::rust::analysis::inline::CallMode::DirectCall => {
                CallStrategy::DirectCall
            }
            crate::backend::rust::analysis::inline::CallMode::InlineBody => {
                CallStrategy::InlineBody
            }
        })
        .collect()
}

/// Separate closure rules from the AST. Returns (closures, non-closure rules).
fn partition_closures<'a>(
    ast: AST<'a>,
) -> (Vec<(&'a str, &'a BbnfBootstrapEnum<'a>)>, AST<'a>) {
    let mut closures: Vec<(&'a str, &'a BbnfBootstrapEnum<'a>)> = Vec::new();
    let mut rules: AST<'a> = indexmap::IndexMap::new();

    for (&name, entry) in &ast {
        if is_closure_rhs(entry.rhs) {
            closures.push((name, entry.rhs));
        } else {
            rules.insert(name, entry.clone());
        }
    }

    (closures, rules)
}

/// Check if a bootstrap RHS node is a closure, unwrapping structural wrappers.
fn is_closure_rhs(node: &BbnfBootstrapEnum<'_>) -> bool {
    match node {
        BbnfBootstrapEnum::closure(_) => true,
        // Unwrap single-element alternation/concatenation wrappers.
        BbnfBootstrapEnum::alternation(branches) if branches.len() == 1 => {
            is_closure_rhs(branches[0].0)
        }
        BbnfBootstrapEnum::concatenation(parts) if parts.len() == 1 => {
            is_closure_rhs(parts[0].0)
        }
        BbnfBootstrapEnum::binary_factor((first, rest)) if rest.is_empty() => {
            is_closure_rhs(first)
        }
        BbnfBootstrapEnum::mapped_factor((inner, None)) => is_closure_rhs(inner),
        BbnfBootstrapEnum::factor((None, inner, None, None)) => is_closure_rhs(inner),
        _ => false,
    }
}

fn compile_ast_common<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    options: &PipelineOptions,
) -> Result<GrammarIR, CompileError> {
    // Determine the entry rule name: use override if provided, otherwise last rule in source order.
    let entry_rule_name: Option<String> = options
        .entry_rule
        .clone()
        .or_else(|| ast.keys().last().map(|name| name.to_string()));

    // Partition AST: extract closure rules, keep the rest for graph analysis.
    // Closures are first-class grammar functions expanded inline during lowering.
    let (closure_rules, ast) = partition_closures(ast);

    // Collect closure parameter names — these are valid identifiers that should
    // not be flagged as unknown nonterminals during validation.
    let mut closure_params: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for &(name, _) in &closure_rules {
        closure_params.insert(name);
    }
    // Also collect the parameter names from inside the closures themselves.
    for &(_, body) in &closure_rules {
        collect_closure_param_names(body, &mut closure_params);
    }

    // Validate after partitioning — closure params are now known.
    validate_ast(&ast, true, &closure_params)?;

    // Dependency analysis.
    let deps = crate::calculate_ast_deps(&ast);

    // SCC detection + topological ordering.
    let scc_result = tarjan_scc(&deps);
    let ast = topological_sort_scc(&ast, &scc_result, &deps);

    // Lower to IR.
    let mut ir = lower_to_ir(&ast, &scc_result, directives, &closure_rules);

    // Compute FIRST sets via IR CSP pass (replaces AST-level computation).
    bbnf_ir::passes::compute_first_sets(&mut ir);

    // Set the correct entry rule (last rule in original source order).
    if let Some(ref name) = entry_rule_name {
        if let Some(rule) = ir.find_rule(name) {
            ir.entry = rule.id;
        }
    }

    // Optional: eliminate left-recursion at IR level (indirect via Paull's, then direct).
    if options.remove_left_recursion {
        bbnf_ir::passes::eliminate_indirect_lr(&mut ir);
        bbnf_ir::passes::eliminate_direct_lr(&mut ir);
    }

    // In structural mode, mark all rules as preserve_identity so no rule is
    // pruned, aliased, or made transparent — but bodies are still fully optimized.
    if options.structural {
        for rule in &mut ir.rules {
            rule.meta.preserve_identity = true;
        }
    }

    // Run IR metadata passes (alias + transparent detection from IR structure).
    // preserve_identity rules are automatically skipped by these passes.
    bbnf_ir::passes::compute_aliases(&mut ir);
    bbnf_ir::passes::compute_transparent(&mut ir);

    if !options.structural {
        // Layer 1 — structural normalization. Primary cross-rule
        // optimizer. Destructive tree rewrites iterated to fixed
        // point: handles the inline→merge→factor→inline cascading
        // feedback that equality saturation cannot express in a
        // single pass.
        const MAX_OPT_ITER: usize = 64;
        for iteration in 0..MAX_OPT_ITER {
            let fingerprint = ir.structural_fingerprint();

            bbnf_ir::passes::canonicalize_aliases(&mut ir);
            bbnf_ir::passes::prune_unreachable(&mut ir);
            bbnf_ir::passes::inline_acyclic(&mut ir);
            bbnf_ir::passes::prune_unreachable(&mut ir);
            bbnf_ir::passes::fuse_single_use(&mut ir);
            bbnf_ir::passes::prune_unreachable(&mut ir);
            bbnf_ir::passes::eliminate_epsilon(&mut ir);
            bbnf_ir::passes::merge_literals(&mut ir);
            // simplify_regex_algebra and merge_regex_alts deleted in
            // Tranche H-7 after Gate B parity proof: the retained
            // grammar-tier e-graph rules (DeduplicateAltBranches,
            // SupersetAbsorbAlt, UnionMergeAlt, FuseAltRegexBranches
            // in crates/ir/src/egraph/rules/regex.rs) cover every
            // rewrite these destructive passes performed, using the
            // same bbnf_regex::algebra helpers and the same
            // pattern_has_top_level_pipe grouping logic. The HIR
            // e-graph landed in H-3..H-6 additionally canonicalizes
            // each individual pattern's HIR before these grammar-tier
            // rules compare them, sharpening the retained rules
            // without replacing them.
            bbnf_ir::passes::factor_common_prefixes(&mut ir);

            if ir.structural_fingerprint() == fingerprint {
                break;
            }
            assert!(
                iteration < MAX_OPT_ITER - 1,
                "structural normalizer loop did not converge after {MAX_OPT_ITER} iterations",
            );
        }

        // Layer 1b — equivalence discovery. Single e-graph saturation
        // on the normalized IR. Retained rules target ordering-
        // independent equivalences and regex-algebra rewrites the
        // normalizer's fixed pass order can miss; cost-guided
        // extraction picks the cheapest canonical form per rule via
        // `GrammarCostModel` (shared with bbnf-regex HIR e-graph in
        // Tranche H).
        let (egraph, pool, rule_body_ids) =
            bbnf_ir::egraph::build_and_saturate(&ir);
        let cost = bbnf_ir::egraph::GrammarCostModel::default();
        bbnf_ir::egraph::write_back_optimized(
            &egraph,
            &mut ir,
            &rule_body_ids,
            &cost,
        );
        pool.write_back(&mut ir);
        drop(egraph);

        #[cfg(debug_assertions)]
        for rule in &ir.rules {
            if let bbnf_ir::IrNode::Ref(target) = &rule.body {
                assert_ne!(
                    *target, rule.id,
                    "e-graph write-back produced self-cycle: rule {} = Ref({})",
                    rule.id, target
                );
            }
        }

        bbnf_ir::passes::sort_alt_branches(&mut ir);
        bbnf_ir::passes::refine_span_eligibility(&mut ir);

        // Refresh SCC metadata: the normalizer + e-graph write-back
        // may have restructured the rule reference graph (alias
        // canonicalization, inlining, fusing), so `is_cyclic` and
        // `scc_id` computed during initial lowering can be stale.
        // Downstream inline planning relies on these flags to break
        // mutual-recursion cycles; re-running Tarjan here ensures
        // they reflect the final optimized graph.
        bbnf_ir::passes::compute_scc(&mut ir);

        // Layer 2a — body-mutating facts/restructuring passes.
        // `factor_regex_with_lookahead` and `fuse_token_dispatch`
        // rewrite rule bodies in place (lookahead dispatch factoring,
        // @token fusion). Running them before the durable DAG is
        // built keeps the reverse pointer map valid for every
        // NodeId-keyed consumer downstream.
        ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&ir);
        bbnf_ir::passes::factor_regex_with_lookahead(&mut ir);
        bbnf_ir::passes::fuse_token_dispatch(&mut ir);

        // Layer 2b — non-mutating facts on the stable DAG. The DAG
        // is built below (outside the `!structural` branch) because
        // it is a stable substrate for every downstream NodeId-keyed
        // pass — including `project_types` in `finalize_compile` —
        // and must exist whether or not the structural optimizer ran.
        //
        // These fact passes only run when optimization is enabled
        // because they depend on the cleaned-up alternation shapes
        // produced by the structural normalizer + e-graph.
    }

    // Durable post-extraction canonical DAG. Mandatory — the
    // reverse pointer map inside is the identity substrate for
    // every downstream NodeId-keyed pass (Tranches D, F, G) and
    // for `project_types` in `finalize_compile`. Built after the
    // body-mutating facts passes converge (when they run) so the
    // pointer index remains valid.
    ir.dag = Some(bbnf_ir::dag::GrammarDag::from_ir(&ir));
    debug_assert!(
        ir.dag.is_some(),
        "DAG must be built before facts/strategy phases",
    );

    if !options.structural {
        // Non-mutating facts on the stable DAG. Gated on !structural
        // because they depend on optimizer output.
        bbnf_ir::passes::generate_dispatch_tables(&mut ir);
        bbnf_ir::passes::compute_regex_info(&mut ir);
        bbnf_ir::passes::mine_recognizers(&mut ir);
        // Tranche V.6 — derive per-NodeId recognizer decisions from
        // the upstream facts. Stored on `ir.recognizer_decisions` for
        // V.7 (kernel registry) and V.8 (driver dispatchers).
        ir.recognizer_decisions = bbnf_ir::passes::solve_recognizer_decisions(&ir);
    }

    Ok(ir)
}

/// Collect closure parameter names from a bootstrap AST node.
fn collect_closure_param_names<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    params: &mut std::collections::HashSet<&'a str>,
) {
    match node {
        BbnfBootstrapEnum::closure((_pipe, first_param, rest_params, _pipe2, _body)) => {
            let first = crate::grammar::generated::BbnfBootstrapEnum::span_text(first_param);
            if !first.is_empty() {
                params.insert(first);
            }
            for (_comma, p) in *rest_params {
                let name = match p {
                    BbnfBootstrapEnum::identifier(s) => s.as_str(),
                    other => crate::grammar::generated::BbnfBootstrapEnum::span_text(other),
                };
                if !name.is_empty() {
                    params.insert(name);
                }
            }
        }
        // Unwrap structural wrappers.
        BbnfBootstrapEnum::alternation(b) if b.len() == 1 => {
            collect_closure_param_names(b[0].0, params)
        }
        BbnfBootstrapEnum::concatenation(p) if p.len() == 1 => {
            collect_closure_param_names(p[0].0, params)
        }
        BbnfBootstrapEnum::binary_factor((f, r)) if r.is_empty() => {
            collect_closure_param_names(f, params)
        }
        BbnfBootstrapEnum::mapped_factor((inner, None)) => {
            collect_closure_param_names(inner, params)
        }
        BbnfBootstrapEnum::factor((None, inner, None, None)) => {
            collect_closure_param_names(inner, params)
        }
        _ => {}
    }
}
