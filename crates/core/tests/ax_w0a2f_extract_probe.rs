//! AX.W0a.2.f — element-extraction probe.
//!
//! For BBNF / EBNF / BNF / CSS / Sheets, dump the entry rule's body
//! shape and report whether `extract_element_ref` would return Some
//! or None. Diagnoses why the admitted shape-dispatch emission for
//! BBNF's `grammar` hits the `__value` fallback and infinite-loops at
//! runtime.

use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::{GrammarIR, IrNode};
use std::path::PathBuf;

fn compile(rel: &str, structural: bool) -> GrammarIR {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let p = PathBuf::from(manifest).join(rel);
    let options = PipelineOptions {
        structural,
        ..PipelineOptions::default()
    };
    let request = CompileRequest {
        options,
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&p), &request)
        .unwrap_or_else(|e| panic!("grammar {rel} compile: {e:?}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm for {rel}, got {other:?}"),
    }
}

fn describe(node: &IrNode, depth: usize, ir: &GrammarIR) {
    let pad = "  ".repeat(depth);
    match node {
        IrNode::Ref(rid) => {
            let name = ir
                .rules
                .iter()
                .find(|r| r.id == *rid)
                .map(|r| ir.get_string(r.name).to_string())
                .unwrap_or_else(|| format!("<rid={rid}>"));
            println!("{pad}Ref({name})");
        }
        IrNode::Literal(sid) => {
            println!("{pad}Literal({:?})", ir.get_string(*sid));
        }
        IrNode::Regex(sid) => {
            println!("{pad}Regex({:?})", ir.get_string(*sid));
        }
        IrNode::Alt(branches, _) => {
            println!("{pad}Alt[");
            for b in branches {
                describe(&b.node, depth + 1, ir);
            }
            println!("{pad}]");
        }
        IrNode::Seq(children) => {
            println!("{pad}Seq[");
            for c in children {
                describe(c, depth + 1, ir);
            }
            println!("{pad}]");
        }
        IrNode::Next(lhs, rhs) => {
            println!("{pad}Next(");
            describe(lhs, depth + 1, ir);
            describe(rhs, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::Skip(lhs, rhs) => {
            println!("{pad}Skip(");
            describe(lhs, depth + 1, ir);
            describe(rhs, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::OptionalWhitespace(inner) => {
            println!("{pad}OW(");
            describe(inner, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::Map { inner, .. } => {
            println!("{pad}Map(");
            describe(inner, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::Repeat { inner, lo, hi } => {
            println!("{pad}Repeat[{lo}..={hi:?}](");
            describe(inner, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::Negate(inner) => {
            println!("{pad}Negate(");
            describe(inner, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::Minus(lhs, rhs) => {
            println!("{pad}Minus(");
            describe(lhs, depth + 1, ir);
            describe(rhs, depth + 1, ir);
            println!("{pad})");
        }
        IrNode::TokenDispatch { .. } => {
            println!("{pad}TokenDispatch{{...}}");
        }
        IrNode::Epsilon => {
            println!("{pad}Epsilon");
        }
    }
}

fn describe_entry(label: &str, rel: &str, structural: bool) {
    let ir = compile(rel, structural);
    let entry_rule = ir.rules.iter().find(|r| r.id == ir.entry).unwrap();
    let entry_name = ir.get_string(entry_rule.name);
    println!("## {label}");
    println!();
    println!("Entry rule `{entry_name}` body:");
    println!();
    describe(&entry_rule.body, 0, &ir);
    println!();
}

#[test]
fn describe_entries() {
    describe_entry("CSS L4", "../../grammar/css/l4/stylesheet.bbnf", false);
    describe_entry("Sheets", "../../grammar/google-sheets/google-sheets.bbnf", false);
    describe_entry("BBNF", "../../grammar/bbnf/bbnf.bbnf", false);
    describe_entry("EBNF", "../../grammar/ebnf/ebnf.bbnf", false);
    describe_entry("BNF", "../../grammar/bnf/bnf.bbnf", false);
    describe_entry("BbnfBootstrap", "../../grammar/bbnf/bbnf.bbnf", true);
}
