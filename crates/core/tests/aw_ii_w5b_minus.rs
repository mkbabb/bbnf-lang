//! AW-II.W5b — focused test for `IrNode::Minus` → `DtaState::Minus`
//! lift. Verifies the EBNF grammar's `character - "'"` sites reach
//! the DTA as `Minus` states (not silently discarded as in the
//! pre-W5b lifter).

use std::path::PathBuf;

use bbnf::pipeline::{compile_paths_request, CompileOutput, CompileRequest, CompileTarget};
use bbnf_ir::passes::{lift_dta, DtaState};
use bbnf_ir::IrNode;

/// Recursively count `IrNode::Minus` sites across every rule body.
fn count_ir_minus(ir: &bbnf_ir::GrammarIR) -> usize {
    fn walk(node: &IrNode) -> usize {
        match node {
            IrNode::Minus(_, _) => 1,
            IrNode::Seq(children) => children.iter().map(walk).sum(),
            IrNode::Alt(branches, _) => branches.iter().map(|b| walk(&b.node)).sum(),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => walk(inner),
            IrNode::Skip(a, b) | IrNode::Next(a, b) => walk(a) + walk(b),
            _ => 0,
        }
    }
    ir.rules.iter().map(|r| walk(&r.body)).sum()
}

fn count_dta_minus(table: &bbnf_ir::passes::DtaTable) -> usize {
    table
        .states
        .iter()
        .filter(|s| matches!(s, DtaState::Minus { .. }))
        .count()
}

fn locate_ebnf() -> PathBuf {
    for c in [
        "../../grammar/ebnf/ebnf.bbnf",
        "../grammar/ebnf/ebnf.bbnf",
        "grammar/ebnf/ebnf.bbnf",
    ] {
        let p = PathBuf::from(c);
        if p.exists() {
            return p;
        }
    }
    panic!("ebnf.bbnf not found");
}

#[test]
fn ebnf_minus_sites_lift_to_dta_minus() {
    let path = locate_ebnf();
    let request = CompileRequest {
        options: Default::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(&[path], &request).expect("compile ebnf.bbnf");
    let ir = match out {
        CompileOutput::Vm(ir) => ir,
        _ => panic!("expected CompileOutput::Vm"),
    };
    let ir_minus = count_ir_minus(&ir);
    assert!(
        ir_minus >= 4,
        "expected ≥4 IR Minus sites in ebnf.bbnf (character - \"'\", etc.); got {}",
        ir_minus
    );
    let table = lift_dta(&ir);
    let dta_minus = count_dta_minus(&table);
    assert_eq!(
        ir_minus, dta_minus,
        "every IrNode::Minus must lift to DtaState::Minus — IR {}, DTA {}",
        ir_minus, dta_minus
    );
}
