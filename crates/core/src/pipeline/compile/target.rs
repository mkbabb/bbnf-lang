//! Per-`CompileTarget` dispatch.
//!
//! [`finalize_compile`] runs the post-`compile_ast_common` work:
//! per-target IR completion (project_types / path_check /
//! payload_layouts / audit-coverage), then the per-backend driver
//! drive when the target is TS or WASM, or the prepared-grammar
//! handoff when the target is Rust, or the bare-IR return when the
//! target is VM.

use bbnf_ir::GrammarIR;

use crate::backend::prepare_grammar;
use crate::pipeline::compile::audit::{run_path_check_pass, write_audit_coverage_artefact};
use crate::pipeline::compile::pipeline::{compute_call_strategies, install_pattern_caches};
use crate::pipeline::{CompileError, CompileOutput, CompileTarget};

/// Dispatch on [`CompileTarget`] and produce a [`CompileOutput`].
///
/// The Rust target hands off to [`prepare_grammar`] which runs the
/// remaining analysis passes and bundles the result into a
/// [`crate::backend::PreparedGrammar`]. The VM/TS/WASM targets each
/// finish the IR by running the post-pipeline passes
/// (`project_types`, `path_check`, `payload_layouts`,
/// `audit_coverage`) and — for TS/WASM — drive the backend emitter.
pub fn finalize_compile(
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
            // AZ-IV.W2.2 — path_check resolver after project_types.
            run_path_check_pass(&mut ir);
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
            // AZ-IV.W2.2 — path_check resolver after project_types.
            run_path_check_pass(&mut ir);
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

            let code = crate::backend::driver::compile_grammar(
                &ir,
                &analysis,
                &mut dstate,
                &mut emitter,
                &mut ctx,
            );
            let output = if code.stmts.is_empty() {
                code.expr
            } else {
                format!("{}\n{}", code.stmts, code.expr)
            };
            Ok(CompileOutput::Ts(output))
        }
        CompileTarget::Wasm => {
            bbnf_ir::passes::compute_sp_method_rules(&mut ir);
            bbnf_ir::passes::project_types(&mut ir);
            // AZ-IV.W2.2 — path_check resolver after project_types.
            run_path_check_pass(&mut ir);
            ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts(&ir);
            write_audit_coverage_artefact(&ir);

            let entry_name = ir.get_string(ir.rules[ir.entry as usize].name).to_string();
            let module_name = format!("{entry_name}_parser");

            let analysis = crate::backend::driver::analysis::BackendAnalysis::default();
            let call_strategies = compute_call_strategies(&ir);
            let mut dstate = crate::backend::driver::DriverState::new(call_strategies);
            install_pattern_caches(&mut dstate, &ir);
            // Pre-register ws pattern so the emitter knows its ID.
            let ws_regex_id = ir
                .ws_pattern
                .map(|ws_sid| dstate.register_regex(ir.get_string(ws_sid)));
            let mut emitter = crate::backend::wasm::WasmEmitter {
                module_name,
                ws_regex_id,
            };
            let mut ctx = crate::backend::wasm::emitter::WasmEmitCtx::default();

            let wat_source = crate::backend::driver::compile_grammar(
                &ir,
                &analysis,
                &mut dstate,
                &mut emitter,
                &mut ctx,
            );
            Ok(CompileOutput::Wasm(wat_source.into_bytes()))
        }
    }
}
