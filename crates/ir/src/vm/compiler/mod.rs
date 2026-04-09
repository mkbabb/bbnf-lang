//! IR → Bytecode compiler.
//!
//! Compiles a `GrammarIR` into a `BytecodeProgram` for the VM interpreter.
//!
//! Sub-modules group the inherent methods on `Compiler`:
//!
//! - [`emit`] — `emit`, `current_offset`, `patch`, and `patch_fail_jumps` —
//!   the low-level instruction-buffer primitives.
//! - [`rule`] — `compile_rule`: rule entry, source map, debug breaks, memo
//!   wrapping.
//! - [`node`] — `compile_node`: the per-`IrNode` dispatch.
//! - [`compound`] — compound-node compilers: `compile_seq`, `compile_alt`,
//!   `compile_dispatch`, `compile_token_dispatch`, `compile_repeat`, the
//!   shared `compile_binary_backtrack`, `compile_minus`, `compile_negate`,
//!   plus the `DispatchPatchPlan` scratch struct and the
//!   `finalize_dispatch_backtracking` helper.
//!
//! `mod.rs` itself owns the `Compiler` struct, the public entry points
//! `compile` / `compile_with_debug`, and the small predicates
//! `grammar_needs_memo` / `node_has_direct_left_recursion`.

mod compound;
mod emit;
mod node;
mod rule;

use super::bytecode::{
    BytecodeProgram, DispatchData, Op, SourceMapEntry, TokenDispatchData,
};
use crate::{GrammarIR, IrNode, MemoStrategy};

/// Compile a GrammarIR to bytecode.
pub fn compile(ir: &GrammarIR) -> BytecodeProgram {
    compile_with_debug(ir, false)
}

/// Compile a GrammarIR to bytecode, optionally generating debug info.
///
/// When `debug` is true:
/// - Source map entries are emitted for each rule
/// - `DebugBreak` opcodes are emitted for `@debug`-annotated rules
pub fn compile_with_debug(ir: &GrammarIR, debug: bool) -> BytecodeProgram {
    let memo_enabled = grammar_needs_memo(ir);
    let mut compiler = Compiler {
        code: Vec::new(),
        entries: vec![0; ir.rules.len()],
        dispatch_tables: Vec::new(),
        token_dispatch_tables: Vec::new(),
        source_map: Vec::new(),
        debug,
        debug_all: ir.debug_all,
        memo_enabled,
    };

    for rule in &ir.rules {
        compiler.compile_rule(rule, ir);
    }

    compiler.emit(Op::Halt);

    // Collect rule name StringIds for error messages.
    let rule_names: Vec<u32> = ir.rules.iter().map(|r| r.name).collect();

    let mut program = BytecodeProgram {
        code: compiler.code,
        entries: compiler.entries,
        strings: ir.strings.clone(),
        entry: ir.entry,
        memo_enabled,
        dispatch_tables: compiler.dispatch_tables,
        token_dispatch_tables: compiler.token_dispatch_tables,
        compiled_regexes: Vec::new(),
        follow_sets: ir.follow_sets.clone(),
        rule_names,
        source_map: compiler.source_map,
    };

    // Pre-compile all regex patterns referenced by MatchRegex opcodes.
    program.prepare_regexes();

    program
}

pub(super) struct Compiler {
    pub(super) code: Vec<Op>,
    pub(super) entries: Vec<u32>,
    /// Dispatch tables stored by index (referenced via `Op::Dispatch(idx)`).
    pub(super) dispatch_tables: Vec<DispatchData>,
    /// Token-dispatch tables stored by index (referenced via `Op::DispatchToken(idx)`).
    pub(super) token_dispatch_tables: Vec<TokenDispatchData>,
    /// Source map entries (only populated when `debug` is true).
    pub(super) source_map: Vec<SourceMapEntry>,
    /// Whether to generate debug info.
    pub(super) debug: bool,
    /// Whether all rules are instrumented (`@debug *`).
    pub(super) debug_all: bool,
    /// Whether the program should emit VM memo ops at all.
    pub(super) memo_enabled: bool,
}

fn grammar_needs_memo(ir: &GrammarIR) -> bool {
    ir.rules.iter().any(|rule| {
        rule.meta.memo != MemoStrategy::None && node_has_direct_left_recursion(&rule.body, rule.id)
    })
}

fn node_has_direct_left_recursion(node: &IrNode, rule_id: u32) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => children
            .first()
            .is_some_and(|first| node_has_direct_left_recursion(first, rule_id)),
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|branch| node_has_direct_left_recursion(&branch.node, rule_id)),
        _ => false,
    }
}
