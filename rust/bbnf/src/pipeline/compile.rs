use std::path::PathBuf;

use bbnf_ir::GrammarIR;

use crate::analysis::{compute_first_sets, tarjan_scc, topological_sort_scc};
use crate::backend::prepare_grammar;
use crate::grammar::BBNFGrammar;
use crate::lower::{DirectiveSet, lower_to_ir};
use crate::pipeline::loader::{DirectiveMaps, load_merged_paths};
use crate::pipeline::validate::{validate_ast, validate_pretty_directives};
use crate::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use crate::types::{AST, Expression};

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
    let parser = BBNFGrammar::grammar_with_imports();
    let (parsed, _state) = parser.parse_return_state(source);
    let parsed =
        parsed.ok_or_else(|| CompileError::Parse("failed to parse grammar".to_string()))?;

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
    validate_ast(&ast, true)?;
    let ir = compile_ast_common(ast, directives, &request.options);
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

            let analysis = crate::backend::analysis::BackendAnalysis::default();
            let call_strategies = compute_call_strategies(&ir);
            let mut dstate = crate::backend::driver::DriverState::new(call_strategies);
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

            let analysis = crate::backend::analysis::BackendAnalysis::default();
            let call_strategies = compute_call_strategies(&ir);
            let mut dstate = crate::backend::driver::DriverState::new(call_strategies);
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

/// Compute call strategies using the shared inline analysis.
///
/// Converts `CallMode::DirectCall` → `CallStrategy::DirectCall`,
/// `CallMode::InlineBody` → `CallStrategy::InlineBody`.
pub fn compute_call_strategies(ir: &GrammarIR) -> Vec<crate::backend::CallStrategy> {
    use crate::backend::CallStrategy;
    use std::collections::HashSet;

    // Detect operator chain rules for the inline plan.
    let operator_chain_rules: HashSet<bbnf_ir::RuleId> = ir
        .rules
        .iter()
        .filter(|rule| {
            if let bbnf_ir::IrNode::Seq(children) = &rule.body {
                if children.len() == 2 {
                    if let bbnf_ir::IrNode::Repeat {
                        inner,
                        lo: 0,
                        hi: u32::MAX,
                    } = &children[1]
                    {
                        if let bbnf_ir::IrNode::Seq(link) = inner.as_ref() {
                            return link.len() == 2;
                        }
                    }
                }
            }
            false
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

fn compile_ast_common<'a>(
    ast: AST<'a>,
    directives: &'a DirectiveSet<'a>,
    options: &PipelineOptions,
) -> GrammarIR {
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

    // Dependency analysis.
    let deps = crate::calculate_ast_deps(&ast);

    // SCC detection + topological ordering.
    let scc_result = tarjan_scc(&deps);
    let ast = topological_sort_scc(&ast, &scc_result, &deps);

    // FIRST set computation.
    let first_sets = compute_first_sets(&ast, &deps, &scc_result);

    // Lower to IR.
    let mut ir = lower_to_ir(&ast, &first_sets, &scc_result, directives);

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

    // Run IR metadata passes (alias + transparent detection from IR structure).
    bbnf_ir::passes::compute_aliases(&mut ir);
    bbnf_ir::passes::compute_transparent(&mut ir);

    // Fixed-point optimization loop.
    // Passes are monotonically shrinking; convergence happens in 1-3 iterations.
    // The bound guards against buggy passes that oscillate.
    const MAX_OPT_ITERATIONS: usize = 64;
    for iteration in 0..MAX_OPT_ITERATIONS {
        let fingerprint = ir.structural_fingerprint();

        bbnf_ir::passes::canonicalize_aliases(&mut ir);
        bbnf_ir::passes::prune_unreachable(&mut ir);
        bbnf_ir::passes::inline_acyclic(&mut ir);
        bbnf_ir::passes::prune_unreachable(&mut ir);
        bbnf_ir::passes::fuse_single_use(&mut ir);
        bbnf_ir::passes::prune_unreachable(&mut ir);
        bbnf_ir::passes::eliminate_epsilon(&mut ir);
        bbnf_ir::passes::merge_literals(&mut ir);
        bbnf_ir::passes::simplify_regex_algebra(&mut ir);
        bbnf_ir::passes::merge_regex_alts(&mut ir);
        bbnf_ir::passes::factor_common_prefixes(&mut ir);

        if ir.structural_fingerprint() == fingerprint {
            break;
        }
        assert!(
            iteration < MAX_OPT_ITERATIONS - 1,
            "IR optimization did not converge after {MAX_OPT_ITERATIONS} iterations \
             (fingerprint: {:?})",
            ir.structural_fingerprint(),
        );
    }
    bbnf_ir::passes::sort_alt_branches(&mut ir);
    bbnf_ir::passes::refine_span_eligibility(&mut ir);

    // Compute FOLLOW sets before dispatch and memo passes that consume them.
    ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&ir);

    // Factor regex prefixes with lookahead dispatch.
    bbnf_ir::passes::factor_regex_with_lookahead(&mut ir);

    // @token-guided prefix factoring.
    bbnf_ir::passes::fuse_token_dispatch(&mut ir);

    // Dispatch tables use FOLLOW sets for nullable branch optimization.
    bbnf_ir::passes::generate_dispatch_tables(&mut ir);

    ir
}
