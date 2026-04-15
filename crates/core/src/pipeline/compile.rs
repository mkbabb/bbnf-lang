use std::path::PathBuf;
use std::time::{Duration, Instant};

use bbnf_ir::GrammarIR;

use crate::grammar;
use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::graph::{tarjan_scc, topological_sort_scc};
use crate::backend::prepare_grammar;
use crate::lower::{DirectiveSet, lower_to_ir};
use crate::pipeline::directives::{DirectiveMaps, load_merged_paths};
use crate::pipeline::validate::{validate_ast, validate_pretty_directives};
use crate::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use crate::types::AST;

/// Tranche AA.0 — per-pass timing accumulator.
///
/// When `BBNF_PIPELINE_REPORT=1` is set in the environment,
/// `compile_ast_common` wraps every pipeline pass with [`PipelineTimer::span`]
/// and, on exit, prints a CSV row to stderr: `pass,elapsed_us` per pass,
/// plus a final `__total__,<us>` line. This is pure observability — the
/// `Instant::now()` calls are elided when the env var is unset so the hot
/// LSP path pays zero cost.
///
/// Consumer documentation: every "+X%" claim in `post-AA.json` for a
/// compile-time phase must cite a `BBNF_PIPELINE_REPORT=1` CSV diff on
/// `compile_css_l4` or `compile_bbnf`.
struct PipelineTimer {
    enabled: bool,
    total: Instant,
    rows: Vec<(&'static str, Duration)>,
}

impl PipelineTimer {
    fn new() -> Self {
        Self {
            enabled: std::env::var("BBNF_PIPELINE_REPORT").is_ok(),
            total: Instant::now(),
            rows: Vec::new(),
        }
    }

    /// Wrap a pass body, accumulating its wall-clock time when enabled.
    /// The `pass` argument is a `&'static str` pass name (matches the
    /// pipeline operation documented in `crates/ir/CLAUDE.md`).
    #[inline]
    fn span<R>(&mut self, pass: &'static str, body: impl FnOnce() -> R) -> R {
        if !self.enabled {
            return body();
        }
        let start = Instant::now();
        let result = body();
        self.rows.push((pass, start.elapsed()));
        result
    }

    /// Emit the CSV report to stderr and consume the timer. Called at
    /// the end of `compile_ast_common`.
    fn finish(self, grammar_label: &str) {
        if !self.enabled {
            return;
        }
        let total = self.total.elapsed();
        eprintln!("pipeline_report: grammar={}", grammar_label);
        eprintln!("  pass,elapsed_us");
        for (pass, dur) in &self.rows {
            eprintln!("  {},{}", pass, dur.as_micros());
        }
        eprintln!("  __total__,{}", total.as_micros());
    }
}

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
            // Tranche AQ.6.B — plan aggregate payload layouts so any
            // VM consumer that reads `ir.payload_layouts` sees the
            // same map the Rust backend does.
            ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts(&ir);
            Ok(CompileOutput::Vm(ir))
        }
        CompileTarget::Ts => {
            bbnf_ir::passes::compute_sp_method_rules(&mut ir);
            bbnf_ir::passes::project_types(&mut ir);
            ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts(&ir);

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
            ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts(&ir);

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

/// Populate the driver state's pattern caches from the authoritative
/// IR sidecars.
///
/// Tranche X.8b: `ir.delim_scan_configs` and `ir.key_dispatch_configs`
/// are populated upstream during `mine_recognizers` (see
/// `bbnf_ir::passes::recognizers::{delim_scan,key_dispatch}`). The
/// driver state just clones them.
fn install_pattern_caches(dstate: &mut crate::backend::driver::DriverState, ir: &GrammarIR) {
    dstate.alt_strategies =
        crate::backend::strategy::alt_strategy::solve_alt_strategies(ir);
    dstate.delim_scan_configs = ir.delim_scan_configs.clone();
    dstate.key_dispatch_configs = ir.key_dispatch_configs.clone();
    // Tranche AB.2 — clone the AB.0/AB.1 per-NodeId materialization
    // map into driver state. Read by the per-kind emitters to decide
    // rule prelude/epilogue shape and `TransparentElide` inlining at
    // call sites.
    dstate.materialization = ir.materialization.clone();
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

    let mut strategies: Vec<CallStrategy> = plan.parse_call_modes
        .iter()
        .map(|mode| match mode {
            crate::backend::rust::analysis::inline::CallMode::DirectCall => {
                CallStrategy::DirectCall
            }
            crate::backend::rust::analysis::inline::CallMode::InlineBody => {
                CallStrategy::InlineBody
            }
        })
        .collect();

    // AN: Two fixups on the raw CSP-derived strategies:
    //
    // 1. Force InlineBody for TransparentElide rules. These rules
    //    don't emit a function; DirectCall would reference nothing.
    // 2. Force DirectCall for the entry rule AND for the first
    //    non-transparent rule (the root function fallback). The
    //    parse() entry always calls the root function by name.
    // The entry rule always gets DirectCall — parse() calls it by name.
    let root_rule_id = Some(ir.entry);

    for rule in &ir.rules {
        use bbnf_ir::passes::MaterializationClass;

        // Force DirectCall for the root rule.
        if Some(rule.id) == root_rule_id {
            if let Some(strat) = strategies.get_mut(rule.id as usize) {
                *strat = CallStrategy::DirectCall;
            }
            continue;
        }

        // Force InlineBody for TransparentElide.
        let class = if let Some(dag) = ir.dag.as_ref() {
            dag.node_for(&rule.body)
                .and_then(|nid| ir.materialization.get(&nid).copied())
                .unwrap_or(MaterializationClass::MustTape)
        } else {
            MaterializationClass::MustTape
        };
        if class == MaterializationClass::TransparentElide {
            if let Some(strat) = strategies.get_mut(rule.id as usize) {
                *strat = CallStrategy::InlineBody;
            }
        }
    }

    strategies
}

/// Separate closure rules from the AST. Returns (closures, non-closure rules).
fn partition_closures<'a>(
    ast: AST<'a>,
) -> (Vec<(&'a str, BbnfBootstrapNodeView<'a>)>, AST<'a>) {
    let mut closures: Vec<(&'a str, BbnfBootstrapNodeView<'a>)> = Vec::new();
    let mut rules: AST<'a> = indexmap::IndexMap::new();

    for (&name, entry) in &ast {
        if is_closure_rhs(entry.rhs) {
            closures.push((name, entry.rhs));
        } else {
            rules.insert(name, *entry);
        }
    }

    (closures, rules)
}

/// Check if a bootstrap RHS view is a closure, unwrapping structural wrappers.
fn is_closure_rhs(node: BbnfBootstrapNodeView<'_>) -> bool {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::closure => true,
        // Unwrap single-branch alternation/concatenation wrappers.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            let mut iter = node.children();
            let Some(first) = iter.next() else {
                return false;
            };
            if iter.next().is_some() {
                return false;
            }
            let branch = first.child(0).unwrap_or(first);
            is_closure_rhs(branch)
        }
        BbnfBootstrapRuleKind::concatenation => {
            let mut iter = node.children();
            let Some(first) = iter.next() else {
                return false;
            };
            if iter.next().is_some() {
                return false;
            }
            let part = first.child(0).unwrap_or(first);
            is_closure_rhs(part)
        }
        BbnfBootstrapRuleKind::binary_factor => {
            let Some(first) = node.child(0) else {
                return false;
            };
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                is_closure_rhs(first)
            } else {
                false
            }
        }
        BbnfBootstrapRuleKind::mapped_factor => {
            let Some(inner) = node.child(0) else {
                return false;
            };
            let mapping = node.child(1);
            let no_mapping = mapping
                .map(|m| m.span().1 == m.span().0)
                .unwrap_or(true);
            if no_mapping {
                is_closure_rhs(inner)
            } else {
                false
            }
        }
        BbnfBootstrapRuleKind::factor => {
            let Some(inner) = node.child(1) else {
                return false;
            };
            let comment_before = node.child(0);
            let modifier = node.child(2);
            let comment_after = node.child(3);
            let all_bare = comment_before
                .map(|c| c.span().1 == c.span().0)
                .unwrap_or(true)
                && modifier.map(|m| m.span().1 == m.span().0).unwrap_or(true)
                && comment_after
                    .map(|c| c.span().1 == c.span().0)
                    .unwrap_or(true);
            if all_bare {
                is_closure_rhs(inner)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn compile_ast_common<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    options: &PipelineOptions,
) -> Result<GrammarIR, CompileError> {
    let mut timer = PipelineTimer::new();

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
    timer.span("validate_ast", || {
        validate_ast(&ast, true, &closure_params)
    })?;

    // Dependency analysis.
    let deps = timer.span("calculate_ast_deps", || crate::calculate_ast_deps(&ast));

    // SCC detection + topological ordering.
    let scc_result = timer.span("tarjan_scc", || tarjan_scc(&deps));
    let ast = timer.span("topological_sort_scc", || {
        topological_sort_scc(&ast, &scc_result, &deps)
    });

    // Lower to IR.
    let mut ir = timer.span("lower_to_ir", || {
        lower_to_ir(&ast, &scc_result, directives, &closure_rules)
    });

    // Compute FIRST sets via IR CSP pass (replaces AST-level computation).
    timer.span("compute_first_sets", || {
        bbnf_ir::passes::compute_first_sets(&mut ir);
    });

    // Set the correct entry rule (last rule in original source order).
    if let Some(ref name) = entry_rule_name {
        if let Some(rule) = ir.find_rule(name) {
            ir.entry = rule.id;
        }
    }

    // Optional: eliminate left-recursion at IR level (indirect via Paull's, then direct).
    if options.remove_left_recursion {
        timer.span("eliminate_indirect_lr", || {
            bbnf_ir::passes::eliminate_indirect_lr(&mut ir);
        });
        timer.span("eliminate_direct_lr", || {
            bbnf_ir::passes::eliminate_direct_lr(&mut ir);
        });
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
    timer.span("compute_aliases", || {
        bbnf_ir::passes::compute_aliases(&mut ir);
    });
    timer.span("compute_transparent", || {
        bbnf_ir::passes::compute_transparent(&mut ir);
    });

    if !options.structural {
        // Layer 1 — structural normalization. Primary cross-rule
        // optimizer. Destructive tree rewrites iterated to fixed
        // point: handles the inline→merge→factor→inline cascading
        // feedback that equality saturation cannot express in a
        // single pass.
        timer.span("structural_normalizer_loop", || {
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
                bbnf_ir::passes::factor_common_prefixes(&mut ir);

                if ir.structural_fingerprint() == fingerprint {
                    break;
                }
                assert!(
                    iteration < MAX_OPT_ITER - 1,
                    "structural normalizer loop did not converge after {MAX_OPT_ITER} iterations",
                );
            }
        });

        // Layer 1b — equivalence discovery. Single e-graph saturation
        // on the normalized IR. Retained rules target ordering-
        // independent equivalences and regex-algebra rewrites the
        // normalizer's fixed pass order can miss; cost-guided
        // extraction picks the cheapest canonical form per rule via
        // `GrammarCostModel` (shared with bbnf-regex HIR e-graph in
        // Tranche H).
        timer.span("egraph_build_saturate_writeback", || {
            let (egraph, pool, rule_body_ids) =
                bbnf_ir::egraph::build_and_saturate(&ir);
            let cost = bbnf_ir::egraph::GrammarCostModel::from_config(&ir.cost_config);
            bbnf_ir::egraph::write_back_optimized(
                &egraph,
                &mut ir,
                &rule_body_ids,
                &cost,
            );
            pool.write_back(&mut ir);
            drop(egraph);
        });

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

        timer.span("sort_alt_branches", || {
            bbnf_ir::passes::sort_alt_branches(&mut ir);
        });
        timer.span("refine_span_eligibility", || {
            bbnf_ir::passes::refine_span_eligibility(&mut ir);
        });

        // Refresh SCC metadata: the normalizer + e-graph write-back
        // may have restructured the rule reference graph (alias
        // canonicalization, inlining, fusing), so `is_cyclic` and
        // `scc_id` computed during initial lowering can be stale.
        // Downstream inline planning relies on these flags to break
        // mutual-recursion cycles; re-running Tarjan here ensures
        // they reflect the final optimized graph.
        timer.span("compute_scc", || {
            bbnf_ir::passes::compute_scc(&mut ir);
        });

        // Layer 2a — body-mutating facts/restructuring passes.
        timer.span("compute_follow_sets", || {
            ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&ir);
        });
        timer.span("factor_regex_with_lookahead", || {
            bbnf_ir::passes::factor_regex_with_lookahead(&mut ir);
        });
        timer.span("fuse_token_dispatch", || {
            bbnf_ir::passes::fuse_token_dispatch(&mut ir);
        });
    }

    // Durable post-extraction canonical DAG. Mandatory — the
    // reverse pointer map inside is the identity substrate for
    // every downstream NodeId-keyed pass (Tranches D, F, G) and
    // for `project_types` in `finalize_compile`. Built after the
    // body-mutating facts passes converge (when they run) so the
    // pointer index remains valid.
    timer.span("build_durable_dag", || {
        ir.dag = Some(bbnf_ir::dag::GrammarDag::from_ir(&ir));
    });
    debug_assert!(
        ir.dag.is_some(),
        "DAG must be built before facts/strategy phases",
    );

    // Build the reverse string index for O(1) `&str → StringId` lookups
    // used by downstream codegen (regex engine decisions, regex info cache).
    ir.build_string_index();

    if !options.structural {
        // Non-mutating facts on the stable DAG. Gated on !structural
        // because they depend on optimizer output.
        timer.span("generate_dispatch_tables", || {
            bbnf_ir::passes::generate_dispatch_tables(&mut ir);
        });
        // Tranche AU.2.7 — grammar-parameterised structural alphabet.
        // Runs immediately after `generate_dispatch_tables` because
        // the dispatch tables are the canonical source of Alt-branch
        // first-byte data. The alphabet is read at codegen time by
        // the scanner-kernel emitters in
        // `crates/core/src/generate/regex/emit/simd.rs`.
        timer.span("compute_structural_alphabet", || {
            bbnf_ir::passes::sets::compute_structural_alphabet(&mut ir);
        });
        timer.span("compute_regex_info", || {
            bbnf_ir::passes::compute_regex_info(&mut ir);
        });
        timer.span("mine_recognizers", || {
            bbnf_ir::passes::mine_recognizers(&mut ir);
        });
        // Tranche AB.0 — bottom-up materialization classification.
        // Runs before the strategy CSP so the joint solve can consult
        // the initial per-NodeId class as a domain prefilter. The
        // classification only consumes `ir.dag`, `ir.fns`, and
        // per-rule `RuleMeta` directives — no dependency on
        // `project_types`, so it's safe at this pipeline position.
        timer.span("classify_materialization", || {
            bbnf_ir::passes::classify_materialization(&mut ir);
        });
        // Tranche W phase 3b / AB.1 — derive per-NodeId strategy
        // decisions from the upstream facts via a real
        // `csp_solver::Csp` running `OptimizationMode::MinimizeCost`.
        // Tranche AB.1 joins the materialization refinement into
        // the same solver, overriding the bottom-up classification
        // with the cost-optimal per-rule-root class where the CSP
        // can prove it legal under pin constraints.
        timer.span("solve_grammar_components", || {
            let (decisions, mat_refined) =
                bbnf_ir::passes::solve_grammar_components(&ir);
            ir.recognizer_decisions = decisions;
            for (node_id, class) in mat_refined {
                ir.materialization.insert(node_id, class);
            }
        });
        // Tranche X.8d — project the per-NodeId RegexEngine decisions
        // into a per-StringId map so `scanner_plan::plan_regex_scanner`
        // can look up the authoritative engine choice by pattern
        // string instead of re-classifying on every emit.
        timer.span("extract_regex_engine_decisions", || {
            ir.regex_engine_decisions =
                bbnf_ir::passes::extract_regex_engine_decisions(&ir, &ir.recognizer_decisions);
        });

    }

    // Emit the per-pass CSV report when BBNF_PIPELINE_REPORT=1.
    let label = entry_rule_name.as_deref().unwrap_or("<unknown>");
    timer.finish(label);

    Ok(ir)
}

/// Collect closure parameter names from a bootstrap view node.
fn collect_closure_param_names<'a>(
    node: BbnfBootstrapNodeView<'a>,
    params: &mut std::collections::HashSet<&'a str>,
) {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::closure => {
            // closure = "|", first_param, rest_params, "|", body
            if let Some(first_param) = node.child(1) {
                let first = first_param.span_text();
                if !first.is_empty() {
                    params.insert(first);
                }
            }
            if let Some(rest) = node.child(2) {
                for pair in rest.children() {
                    if let Some(p) = pair.child(1) {
                        let name = p.span_text();
                        if !name.is_empty() {
                            params.insert(name);
                        }
                    }
                }
            }
        }
        // Unwrap structural wrappers.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            let mut iter = node.children();
            if let Some(first) = iter.next() {
                if iter.next().is_none() {
                    let branch = first.child(0).unwrap_or(first);
                    collect_closure_param_names(branch, params);
                }
            }
        }
        BbnfBootstrapRuleKind::concatenation => {
            let mut iter = node.children();
            if let Some(first) = iter.next() {
                if iter.next().is_none() {
                    let part = first.child(0).unwrap_or(first);
                    collect_closure_param_names(part, params);
                }
            }
        }
        BbnfBootstrapRuleKind::binary_factor => {
            let Some(first) = node.child(0) else {
                return;
            };
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                collect_closure_param_names(first, params);
            }
        }
        BbnfBootstrapRuleKind::mapped_factor => {
            let Some(inner) = node.child(0) else {
                return;
            };
            let mapping = node.child(1);
            let no_mapping = mapping
                .map(|m| m.span().1 == m.span().0)
                .unwrap_or(true);
            if no_mapping {
                collect_closure_param_names(inner, params);
            }
        }
        BbnfBootstrapRuleKind::factor => {
            let Some(inner) = node.child(1) else {
                return;
            };
            let comment_before = node.child(0);
            let modifier = node.child(2);
            let comment_after = node.child(3);
            let all_bare = comment_before
                .map(|c| c.span().1 == c.span().0)
                .unwrap_or(true)
                && modifier.map(|m| m.span().1 == m.span().0).unwrap_or(true)
                && comment_after
                    .map(|c| c.span().1 == c.span().0)
                    .unwrap_or(true);
            if all_bare {
                collect_closure_param_names(inner, params);
            }
        }
        _ => {}
    }
}
