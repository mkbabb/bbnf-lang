use std::path::PathBuf;
use std::time::{Duration, Instant};

use bbnf_ir::GrammarIR;

use crate::graph::{tarjan_scc, topological_sort_scc};
use crate::backend::prepare_grammar;
use crate::lower::{DirectiveSet, lower_to_ir};
use crate::pipeline::directives::{load_merged_paths, parse_to_pipeline_inputs};
use crate::pipeline::validate::{validate_ast, validate_pretty_directives};
use crate::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};
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
    // Tape-direct ingress: walk the bootstrap tape straight into
    // pipeline-shaped containers — AST, DirectiveMaps, imports — in
    // one pass. No `GrammarExtract` / `ParsedGrammar` middle step.
    let (ast, directive_maps, imports) = parse_to_pipeline_inputs(source)
        .ok_or_else(|| CompileError::Parse("failed to parse grammar".to_string()))?;

    if !imports.is_empty() {
        return Err(CompileError::Import(
            "compile_grammar(source) does not resolve @import; use compile_paths_request"
                .to_string(),
        ));
    }

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

/// AZ-I.W2.RA — Pipeline-level dispatch hook for the codegen
/// substrate selector.
///
/// Delegates to [`bbnf_ir::registry::EmitStrategy::for_grammar`] —
/// the single source of truth for "which substrate does this grammar
/// emit?" The pipeline does not branch on the result; the strategy
/// is consumed at the per-grammar emit site (`emit_grammar_impl`)
/// and at the per-shape dispatcher entry (`emit_shapes_for_grammar`).
///
/// `grammar_ident` is the parser-struct ident the bootstrap regen
/// emits (e.g. `"JsonParser"`, `"BbnfBootstrap"`). The pipeline
/// derives this from the grammar's entry-rule name capitalised plus
/// `"Parser"`; downstream consumers (the Rust emitter, test
/// fixtures) call this helper before codegen to record the resolved
/// substrate alongside the prepared IR.
///
/// AZ-I.W2-act.A — `EmitStrategy` lives in `bbnf-ir` per
/// `audit/AUDIT-6-ARCHITECTURE.md` §4 + §8.1; the resolver is
/// backend-shared. Per `feedback_pluggable-components` the resolver
/// is the boundary; this fn is a thin pipeline-side adapter so test
/// harnesses can drive the resolver without reaching into the IR
/// module path directly.
pub fn resolve_emit_strategy(
    grammar_ident: &str,
    ir: &GrammarIR,
) -> bbnf_ir::registry::EmitStrategy {
    bbnf_ir::registry::EmitStrategy::for_grammar(grammar_ident, &ir.struct_registry)
}

/// AZ-I.W2-act.A — `audit_payload_coverage` artefact emission.
///
/// Per `audit/AUDIT-2-SUBSTRATE-CONSUMER.md` §6.B (path a) the
/// W0 audit pass becomes load-bearing: every pipeline-compile run
/// emits a per-grammar coverage JSON to `target/audit/<entry>.json`.
/// Future tranches (AZ-I.W4 close ceremony) consume the artefact as
/// the substrate-coverage gate input.
///
/// Failures to write are non-fatal — the pipeline continues so a
/// permission-bound CI environment without writable target/ does
/// not break compile. The artefact's absence simply blocks the
/// downstream coverage gate; it does not block codegen.
fn write_audit_coverage_artefact(ir: &GrammarIR) {
    use bbnf_ir::passes::{audit_payload_coverage, write_coverage_report, GrammarAuditTag};
    // Empty grammar — bootstrap path or fixture; no artefact to write.
    if ir.rules.is_empty() {
        return;
    }
    // Resolve the grammar's entry-rule name as the row key. The
    // `Custom(&'static str)` variant is the safe default for
    // arbitrary grammars; JSON / CSS L4 / Sheets exercise the typed
    // variants. `Box::leak` produces the required `&'static str`;
    // each pipeline compile leaks one ident-sized string — a
    // bounded, one-shot allocation matching cargo's per-build
    // process lifetime.
    let entry_name: &'static str = Box::leak(
        ir.get_string(ir.rules[ir.entry as usize].name)
            .to_string()
            .into_boxed_str(),
    );
    let tag = match entry_name {
        "value" | "json" => GrammarAuditTag::Json,
        "stylesheet" | "css_l4" | "cssL4" => GrammarAuditTag::CssL4,
        "spreadsheet" | "sheets" | "google_sheets" => GrammarAuditTag::Sheets,
        other => GrammarAuditTag::Custom(other),
    };
    let coverage = audit_payload_coverage(ir, tag.clone(), &&ir.struct_registry);

    // AZ-I.W2-act.close A.fix — wire-or-delete decay item. The
    // audit pass becomes load-bearing as a development invariant:
    // every `Missing` marker is the red signal that a registered
    // `StructLayout` exists but does not cover the typed `->` site
    // its enclosing rule projects. Gated behind `cfg(debug_assertions)`
    // so release builds skip the assertion (the artefact write
    // continues so downstream tooling can still consume the JSON).
    #[cfg(debug_assertions)]
    {
        if !coverage.is_clean() {
            panic!(
                "audit_payload_coverage: grammar {:?} reports {} `Missing` typed-`->` marker(s):\n  {}",
                tag.key(),
                coverage.missing_markers,
                coverage
                    .missing
                    .iter()
                    .map(|m| format!(
                        "rule={} fn_id={} typed_leaf={:?} fn_kind={}",
                        m.rule_name, m.fn_id, m.typed_leaf, m.fn_kind,
                    ))
                    .collect::<Vec<_>>()
                    .join("\n  "),
            );
        }
    }

    let mut report = bbnf_ir::passes::AuditCoverageReport::new();
    report.push(coverage);

    // `target/audit/<entry>.json` — the workspace target dir is the
    // canonical resolved location since `cargo` always invokes from
    // the workspace root or a sub-crate. The default path resolves
    // relative to the working directory; failures are silent so
    // permission-bound CI does not break compile.
    let path = std::path::PathBuf::from("target/audit").join(format!("{}.json", tag.key()));
    let _ = write_coverage_report(&report, &path);
}

fn finalize_compile(
    mut ir: GrammarIR,
    target: &CompileTarget,
) -> Result<CompileOutput, CompileError> {
    match target {
        CompileTarget::Rust { requested_prettify } => {
            // AZ-I.W2.RA — preflight invariant: prepare_grammar runs
            // project_types via analyze_grammar, populating
            // ir.struct_registry; the per-grammar emit_grammar_impl
            // then resolves EmitStrategy::for_grammar against the
            // populated registry. The pipeline's role is to ensure
            // the substrate (registry) is populated before the
            // emitter runs — this is the same path that has been
            // active since W1 close.
            let prepared = prepare_grammar(ir, *requested_prettify);
            // AZ-I.W2-act.A — emit the audit-coverage artefact post-
            // analyze_grammar. `prepared.ir` carries the populated
            // struct_registry the audit pass reads.
            write_audit_coverage_artefact(&prepared.ir);
            Ok(CompileOutput::Rust(prepared))
        }
        CompileTarget::Vm => {
            bbnf_ir::passes::project_types(&mut ir);
            // Tranche AQ.6.B — plan aggregate payload layouts so any
            // VM consumer that reads `ir.payload_layouts` sees the
            // same map the Rust backend does.
            ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts(&ir);
            // AZ-I.W2-act.A — emit the audit-coverage artefact.
            write_audit_coverage_artefact(&ir);
            Ok(CompileOutput::Vm(ir))
        }
        CompileTarget::Ts => {
            bbnf_ir::passes::compute_sp_method_rules(&mut ir);
            bbnf_ir::passes::project_types(&mut ir);
            ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts(&ir);
            write_audit_coverage_artefact(&ir);

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
            write_audit_coverage_artefact(&ir);

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
) -> (Vec<(&'a str, BbnfView<'a, 'a>)>, AST<'a>) {
    let mut closures: Vec<(&'a str, BbnfView<'a, 'a>)> = Vec::new();
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

/// Check if a struct-direct RHS view is a closure, unwrapping
/// single-branch structural wrappers.
fn is_closure_rhs(view: BbnfView<'_, '_>) -> bool {
    match view.compound_kind() {
        Some(BbnfCompoundKind::Closure) => true,
        // Unwrap single-branch alternation / call_arg wrappers.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::CallArg) => {
            let mut iter = view.children_iter();
            let Some(first) = iter.next() else {
                return false;
            };
            if iter.next().is_some() {
                return false;
            }
            // The struct-direct alternation child is the chosen branch
            // value directly (no extra Seq wrapper); recurse.
            is_closure_rhs(first)
        }
        Some(BbnfCompoundKind::Concatenation) => {
            let mut iter = view.children_iter();
            let Some(first) = iter.next() else {
                return false;
            };
            if iter.next().is_some() {
                return false;
            }
            is_closure_rhs(first)
        }
        Some(BbnfCompoundKind::BinaryFactor) => {
            // `binary_factor = mapped_factor , ( binary_operators ?w
            //   mapped_factor ?w ) *`. A bare binary_factor with no
            // operators is a single-operand wrapper — recurse into
            // child(0). When operator children are present (compound
            // count > 1) the chain is operator-typed, not a closure.
            if view.num_children() != 1 {
                return false;
            }
            view.child(0).map(is_closure_rhs).unwrap_or(false)
        }
        Some(BbnfCompoundKind::MappedFactor) => {
            // `mapped_factor = factor , ( "->" ?w , value_expr ,
            //   type_annotation ? ) ?`. The mapping slot manifests in
            // the source span as a `->` substring; absent when the
            // compound's text doesn't contain `->`.
            if view.span_text().contains("->") {
                return false;
            }
            view.child(0).map(is_closure_rhs).unwrap_or(false)
        }
        Some(BbnfCompoundKind::Factor) => {
            // `factor = big_comment ? , term ?w , modifier ? ,
            //   big_comment ?`. The modifier slot manifests as a
            // trailing `?` / `*` / `+` byte on the compound's text.
            // Comments embed `/*` markers; recurse only when neither
            // is present.
            let text = view.span_text().trim();
            if matches!(text.as_bytes().last(), Some(b'?') | Some(b'*') | Some(b'+')) {
                return false;
            }
            if text.contains("/*") {
                return false;
            }
            // Locate the inner `term` and recurse on it; if absent,
            // dispatch on whatever the factor's first child is.
            let inner = view
                .find_descendant_by_kind(BbnfCompoundKind::Term)
                .or_else(|| view.child(0));
            inner.map(is_closure_rhs).unwrap_or(false)
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
                bbnf_ir::passes::inline_acyclic(&mut ir);
                bbnf_ir::passes::prune_unreachable(&mut ir);
                bbnf_ir::passes::compute_scc(&mut ir);
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
            bbnf_ir::passes::csp_strategy::constraints::shape_dict::solve_shape_dict_selection(
                &ir,
            );
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

/// Collect closure parameter names from a struct-direct view node.
///
/// `closure = "|" , identifier , ( "," ?w , identifier ) * , "|" ?w , rhs`
/// — the leading Span children of a `Closure` compound are the
/// parameter names; the trailing compound child is the body. Walking
/// the closure's children and grabbing every Span-typed leaf yields
/// every param without depending on positional indices.
fn collect_closure_param_names<'a>(
    view: BbnfView<'a, 'a>,
    params: &mut std::collections::HashSet<&'a str>,
) {
    match view.compound_kind() {
        Some(BbnfCompoundKind::Closure) => {
            for child in view.children_iter() {
                if !child.is_compound() {
                    if child.is_span() {
                        // The Span text comes from the document's input
                        // slice; re-borrow against `view.input()` so
                        // the lifetime matches the param-set's `'a`.
                        let text = child.span_text();
                        if !text.is_empty() {
                            params.insert(slice_in_input(view.input(), text));
                        }
                    }
                }
                // Compound children (the rhs body) don't contribute
                // closure params for the OUTER closure's signature —
                // nested closures inside the body are handled via
                // their own collect_closure_param_names call when the
                // outer rule list is iterated.
            }
        }
        // Unwrap single-branch structural wrappers.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::CallArg) => {
            let mut iter = view.children_iter();
            if let Some(first) = iter.next() {
                if iter.next().is_none() {
                    collect_closure_param_names(first, params);
                }
            }
        }
        Some(BbnfCompoundKind::Concatenation) => {
            let mut iter = view.children_iter();
            if let Some(first) = iter.next() {
                if iter.next().is_none() {
                    collect_closure_param_names(first, params);
                }
            }
        }
        Some(BbnfCompoundKind::BinaryFactor) => {
            if view.num_children() != 1 {
                return;
            }
            if let Some(inner) = view.child(0) {
                collect_closure_param_names(inner, params);
            }
        }
        Some(BbnfCompoundKind::MappedFactor) => {
            if view.span_text().contains("->") {
                return;
            }
            if let Some(inner) = view.child(0) {
                collect_closure_param_names(inner, params);
            }
        }
        Some(BbnfCompoundKind::Factor) => {
            let text = view.span_text().trim();
            if matches!(text.as_bytes().last(), Some(b'?') | Some(b'*') | Some(b'+')) {
                return;
            }
            if text.contains("/*") {
                return;
            }
            let inner = view
                .find_descendant_by_kind(BbnfCompoundKind::Term)
                .or_else(|| view.child(0));
            if let Some(inner) = inner {
                collect_closure_param_names(inner, params);
            }
        }
        _ => {}
    }
}

/// Re-borrow `text` against `input` so the returned slice has the
/// `'a` lifetime of the input. The struct-direct `BbnfValue::Span`
/// payload is already a sub-slice of the document's input; the
/// pointer-arithmetic recover keeps the lifetime tight without
/// allocation.
fn slice_in_input<'a>(input: &'a str, text: &str) -> &'a str {
    let input_start = input.as_ptr() as usize;
    let input_end = input_start + input.len();
    let s_start = text.as_ptr() as usize;
    let s_end = s_start + text.len();
    if s_start < input_start || s_end > input_end {
        if let Some(pos) = input.find(text) {
            return &input[pos..pos + text.len()];
        }
        return &input[..0];
    }
    let lo = s_start - input_start;
    let hi = lo + text.len();
    &input[lo..hi]
}
