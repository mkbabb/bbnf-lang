//! `compile_ast_common` — the canonical pass-list orchestrator.
//!
//! This module owns the IR-pipeline pass list itself and the backend
//! driver-state plumbing helpers (`compute_call_strategies`,
//! `install_pattern_caches`) that read the freshly-completed IR.
//!
//! The pass list is intentionally explicit and source-side; per
//! `feedback_pluggable-components` AZ-IV.AUDIT-B has flagged the
//! pass list for conversion to a typed `&[Pass]` plan in W4. Until
//! then, the imperative shape stays here as the canonical
//! recipe for pipeline composition.

use bbnf_ir::GrammarIR;

use crate::graph::{tarjan_scc, topological_sort_scc};
use crate::lower::{DirectiveSet, lower_to_ir};
use crate::pipeline::compile::closure_partition::{
    collect_closure_param_names, partition_closures,
};
use crate::pipeline::compile::timer::PipelineTimer;
use crate::pipeline::validate::validate_ast;
use crate::pipeline::{CompileError, PipelineOptions};
use crate::types::AST;

/// The canonical IR-pipeline orchestrator.
///
/// Validates, partitions closures, runs SCC + topological order,
/// lowers to IR, computes FIRST sets, optionally eliminates
/// left-recursion, runs the structural normalizer + e-graph
/// saturation, and runs the body-mutating facts/restructuring passes.
/// Profile-populating mining passes (regex_info, structural_alphabet,
/// recognizers, shape_dict_selection) run unconditionally; the
/// codegen-decision passes (dispatch_tables, classify_materialization,
/// solve_grammar_components, extract_regex_engine_decisions) run only
/// when `options.structural` is `false`.
pub fn compile_ast_common<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    options: &PipelineOptions,
) -> Result<GrammarIR, CompileError> {
    let mut timer = PipelineTimer::new();

    // Tranche BB.scaffold.C — per-grammar rewrite-rule wiring. The
    // xtask regen entry passes loaded rules through
    // `PipelineOptions.rewrites`; the cost-config substrate that
    // consumes them lands in BB.scaffold.B. Today the wiring is a
    // diagnostic eprint when `BBNF_PIPELINE_REPORT=1` so the rule
    // count is visible at compile time without forcing a noisy
    // default. Empty rulesets are treated identically to `None`.
    if std::env::var("BBNF_PIPELINE_REPORT").is_ok() {
        match &options.rewrites {
            Some(rs) if !rs.is_empty() => {
                eprintln!(
                    "[pipeline] rewrites: {} rule(s) loaded for cost-config",
                    rs.len()
                );
            }
            _ => {}
        }
    }

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
    timer.span("validate_ast", || validate_ast(&ast, true, &closure_params))?;

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
                // AW-I.W2.3 — SCC recompute plumbing between passes.
                //
                // `inline_acyclic` and `fuse_single_use` guard their
                // candidate sets with `!is_cyclic && scc_id.is_none()`,
                // which reads the SCC metadata populated by
                // `compute_scc`. Without an in-loop recompute the
                // metadata drifts relative to the mid-iteration rule
                // graph (alias canonicalization / prior inlining
                // restructure refs; freshly-emergent SCCs retain the
                // stale ids the initial lowering stamped). The two
                // calls below keep the metadata fresh:
                //
                // 1. top-of-iteration, after `canonicalize_aliases` —
                //    so `inline_acyclic` reads SCC ids that reflect
                //    the canonicalized reference graph.
                // 2. inter-pass, between `inline_acyclic` and
                //    `fuse_single_use` — so fuse sees metadata that
                //    reflects any SCCs inlining just collapsed.
                //
                // Convention collision. `lower::metadata::build_rule_meta`
                // stamps `scc_id = Some(id)` for every rule (singleton
                // SCCs included) while `compute_scc` stamps `None` for
                // acyclic rules. Pre-W2.3 the guards read lowering-time
                // values and were therefore always false — the passes
                // were dormant by accident. Calling `compute_scc` inside
                // the loop realigns the field to the authoritative post-
                // loop convention and the guards begin to fire on
                // acyclic rules. Tranche-plan invariant: this is the
                // exact state W4.5 needs as its starting point before
                // dropping the guards themselves (inline.rs:42 /
                // fuse.rs:55). The resulting test regressions (sheets
                // parity, payload layouts, grammar roundtrips, tape
                // parity) are the ~45-test delta PROGRESS.md §"W1b
                // continuation" forecast; W4.5's snapshot-migration
                // consumes the coordinated update.
                bbnf_ir::passes::compute_scc(&mut ir);
                bbnf_ir::passes::prune_unreachable(&mut ir);
                // AZ-IV.W2.2 — route the inline passes through the
                // recording wrappers so the inline trace captures every
                // `Ref(source) → body` substitution. The trace is the
                // sidecar `path_check` consumes after `project_types`
                // to re-resolve `path!` literals that name source
                // rules whose layouts the registry no longer holds.
                let mut trace = std::mem::take(&mut ir.inline_trace);
                bbnf_ir::passes::inline_acyclic_with_trace(&mut ir, &mut trace);
                bbnf_ir::passes::prune_unreachable(&mut ir);
                bbnf_ir::passes::compute_scc(&mut ir);
                bbnf_ir::passes::fuse_single_use_with_trace(&mut ir, &mut trace);
                ir.inline_trace = trace;
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

        // AW-IV.W4.3.c — grammar-level pattern dedup (AP.4.2 chronic).
        // Hoists recurring Seq/Alt sub-patterns (e.g. `ws + ':' + ws`
        // across CSS L4 declarations; `!important` across every
        // pretty-aware rule) into synthesised non-terminals, rewriting
        // each occurrence to `Ref(__pattern_<hash>)`. Pre-egraph
        // because the egraph saturation then operates on a simpler
        // IR (already-factored commonality lets
        // DeduplicateAltBranches + UnionMergeAlt converge faster).
        timer.span("hoist_recurring_patterns", || {
            bbnf_ir::passes::transform::hoist_recurring_patterns(&mut ir);
        });

        // Layer 1b — equivalence discovery. Single e-graph saturation
        // on the normalized IR. Retained rules target ordering-
        // independent equivalences and regex-algebra rewrites the
        // normalizer's fixed pass order can miss; cost-guided
        // extraction picks the cheapest canonical form per rule via
        // `GrammarCostModel` (shared with bbnf-regex HIR e-graph in
        // Tranche H).
        timer.span("egraph_build_saturate_writeback", || {
            let (egraph, pool, rule_body_ids) = bbnf_ir::egraph::build_and_saturate(&ir);
            let cost = bbnf_ir::egraph::GrammarCostModel::from_config(&ir.cost_config);
            bbnf_ir::egraph::write_back_optimized(&egraph, &mut ir, &rule_body_ids, &cost);
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

    // AW-V.W5.1 — profile-populating mining passes.
    //
    // The four passes in this block (`compute_regex_info`,
    // `compute_structural_alphabet`, `mine_recognizers`,
    // `solve_shape_dict_selection`) are purely non-mutating fact
    // collectors: they read the stable DAG and populate sidecar
    // slots on `GrammarIR` (`regex_info`, `structural_alphabet`,
    // `keyword_branches`, `shape_dict_templates`,
    // `shape_dict_selection`, etc.). They touch neither rule
    // bodies nor rule identities, so they compose with
    // `structural` mode verbatim — the `preserve_identity` flag
    // set upstream still holds.
    //
    // Pre-W5.1 the entire block below (profile-populating + codegen-
    // decision) was gated on `!options.structural`, which silently
    // dropped every mined `GrammarProfile` slot for the BBNF
    // bootstrap (`#[parser(path=..., structural)]`). The emitter
    // consumed `GrammarIR::profile()` and lowered `&[]` for
    // `structural_alphabet`, `structural_digraphs`, `quote_classes`,
    // `keyword_tables`, and `shape_dict` — the wire-contract P4
    // samply observation from AW-IV. W5.1 splits the gate: the
    // non-mutating fact passes run unconditionally, the codegen-
    // decision passes remain gated on optimizer output.
    //
    // AW-IV.W1.δ — `compute_regex_info` precedes
    // `compute_structural_alphabet` so the latter's quote-class
    // mining can read the `RegexClass::QuotedString` classification
    // off `ir.regex_info`. Pre-AW-IV the order was inverted, which
    // left `structural_quote_classes` empty for every non-bootstrap
    // grammar — the wire-contract pipeline carried `&[]` through
    // for that slot. Reordering activates it without touching
    // either pass body.
    timer.span("compute_regex_info", || {
        bbnf_ir::passes::compute_regex_info(&mut ir);
    });
    // Tranche AU.2.7 — grammar-parameterised structural alphabet.
    // The alphabet is read at codegen time by the scanner-kernel
    // emitters in `crates/core/src/generate/regex/emit/simd.rs`,
    // and by the runtime `GrammarProfile` consumer in
    // `simd-scan::StructuralAlphabet::from_profile`.
    timer.span("compute_structural_alphabet", || {
        bbnf_ir::passes::sets::compute_structural_alphabet(&mut ir);
    });
    timer.span("mine_recognizers", || {
        bbnf_ir::passes::mine_recognizers(&mut ir);
    });
    // Tranche AV.5.3 — shape-dictionary admission solve. Selects
    // up to MAX_SHAPE_DICT_ENTRIES candidates from the
    // `mine_recognizers`-emitted pool by greedy maximisation of
    // `freq × savings - static_entry_cost`. The result indexes
    // into `ir.shape_dict_templates`; the codegen emitter walks
    // the selection to bake `GrammarProfile::shape_dict`.
    timer.span("solve_shape_dict_selection", || {
        ir.shape_dict_selection =
            bbnf_ir::passes::csp_strategy::constraints::shape_dict::solve_shape_dict_selection(&ir);
    });

    if !options.structural {
        // Codegen-decision passes. Gated on `!structural` because
        // they produce per-NodeId dispatch tables, materialization
        // classes, engine choices, and recognizer decisions that
        // the downstream codegen consumes — those decisions
        // presuppose the optimized rule graph (inlined wrappers,
        // fused token dispatch, normalized body shapes). The
        // bootstrap and analysis callers that set `structural =
        // true` preserve the raw rule graph for self-hosting /
        // diagnostics, so the codegen-decision passes are
        // inapplicable to their downstream uses.
        timer.span("generate_dispatch_tables", || {
            bbnf_ir::passes::generate_dispatch_tables(&mut ir);
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
            let (decisions, mat_refined) = bbnf_ir::passes::solve_grammar_components(&ir);
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

/// Populate the driver state's pattern caches from the authoritative
/// IR sidecars.
///
/// Tranche X.8b: `ir.delim_scan_configs` and `ir.key_dispatch_configs`
/// are populated upstream during `mine_recognizers` (see
/// `bbnf_ir::passes::recognizers::{delim_scan,key_dispatch}`). The
/// driver state just clones them.
pub fn install_pattern_caches(dstate: &mut crate::backend::driver::DriverState, ir: &GrammarIR) {
    dstate.alt_strategies = crate::backend::strategy::alt_strategy::solve_alt_strategies(ir);
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

    let plan = crate::backend::rust::analysis::inline::analyze_parse_inline_plan(
        ir,
        &operator_chain_rules,
    );

    let mut strategies: Vec<CallStrategy> = plan
        .parse_call_modes
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
