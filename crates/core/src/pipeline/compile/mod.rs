//! Pipeline compile entrypoints.
//!
//! AZ-IV.AUDIT-B — split from the prior monolithic `compile.rs`
//! (1049 LOC) into a directory module with one sub-file per concern:
//!
//! - [`mod@target`] — per-`CompileTarget` dispatch (`finalize_compile`).
//! - [`mod@timer`] — per-pass timing accumulator (`PipelineTimer`).
//! - [`mod@audit`] — audit-coverage artefact emission, path-check pass
//!   wiring, and the pipeline-level [`bbnf_ir::registry::EmitStrategy`]
//!   adapter.
//! - [`mod@closure_partition`] — closure structural-detection helpers.
//! - [`mod@pipeline`] — `compile_ast_common` (the canonical pass-list
//!   orchestrator) plus the backend driver-state plumbing
//!   (`compute_call_strategies`, `install_pattern_caches`).
//!
//! The public API is preserved verbatim by re-exporting every
//! previously-public item below.

pub mod audit;
pub mod closure_partition;
pub mod pipeline;
pub mod target;
pub mod timer;

use std::path::PathBuf;

use bbnf_ir::GrammarIR;

use crate::lower::DirectiveSet;
use crate::pipeline::directives::{load_merged_paths, parse_to_pipeline_inputs};
use crate::pipeline::validate::validate_pretty_directives;
use crate::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use crate::types::AST;

pub use self::audit::resolve_emit_strategy;
pub use self::pipeline::{compile_ast_common, compute_call_strategies, install_pattern_caches};
pub use self::target::finalize_compile;

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
