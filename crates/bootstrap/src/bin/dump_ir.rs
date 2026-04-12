//! Tranche AG.4 diagnostic harness: load a grammar file through the
//! regular pipeline, lower it to IR, and dump the body of a named rule
//! (or every rule) so the resulting `IrNode` tree can be inspected
//! without running the full codegen. Intended to localise the clean-
//! regen regression where `grammar = ( grammar_item ?w ) *` emits as
//! three empty `'alt_blk:` branches instead of a `'rpt_blk:` loop.
//!
//! Usage:
//!   cargo run -p bbnf-bootstrap --bin dump_ir -- <file.bbnf> [<rule>] [--structural]

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::{GrammarIR, IrNode, RuleId};

fn indent(n: usize) -> String {
    "  ".repeat(n)
}

fn dump_node(ir: &GrammarIR, node: &IrNode, depth: usize) {
    let ind = indent(depth);
    match node {
        IrNode::Literal(sid) => println!("{ind}Literal({:?})", ir.get_string(*sid)),
        IrNode::Regex(sid) => println!("{ind}Regex({:?})", ir.get_string(*sid)),
        IrNode::Epsilon => println!("{ind}Epsilon"),
        IrNode::OptionalWhitespace(inner) => {
            println!("{ind}OptionalWhitespace");
            dump_node(ir, inner, depth + 1);
        }
        IrNode::Ref(rid) => {
            let name = ir.get_string(ir.get_rule(*rid).name);
            println!("{ind}Ref(#{rid} {name})");
        }
        IrNode::Seq(children) => {
            println!("{ind}Seq[{}]", children.len());
            for c in children {
                dump_node(ir, c, depth + 1);
            }
        }
        IrNode::Alt(branches, dispatch) => {
            let d = if dispatch.is_some() { " +dispatch" } else { "" };
            println!("{ind}Alt[{}]{}", branches.len(), d);
            for b in branches {
                dump_node(ir, &b.node, depth + 1);
            }
        }
        IrNode::Repeat { inner, lo, hi } => {
            println!("{ind}Repeat({lo}..={hi})");
            dump_node(ir, inner, depth + 1);
        }
        IrNode::Skip(a, b) => {
            println!("{ind}Skip");
            dump_node(ir, a, depth + 1);
            dump_node(ir, b, depth + 1);
        }
        IrNode::Next(a, b) => {
            println!("{ind}Next");
            dump_node(ir, a, depth + 1);
            dump_node(ir, b, depth + 1);
        }
        IrNode::Minus(a, b) => {
            println!("{ind}Minus");
            dump_node(ir, a, depth + 1);
            dump_node(ir, b, depth + 1);
        }
        IrNode::Negate(inner) => {
            println!("{ind}Negate");
            dump_node(ir, inner, depth + 1);
        }
        IrNode::Map { inner, fn_id } => {
            println!("{ind}Map(fn_id={fn_id})");
            dump_node(ir, inner, depth + 1);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            println!("{ind}TokenDispatch[{} arms]", arms.len());
            println!("{ind}  token:");
            dump_node(ir, token, depth + 2);
            for (i, arm) in arms.iter().enumerate() {
                println!("{ind}  arm[{i}] patterns={}:", arm.patterns.len());
                dump_node(ir, &arm.continuation, depth + 2);
            }
            println!("{ind}  fallback:");
            dump_node(ir, fallback, depth + 2);
        }
    }
}

fn dump_rule(ir: &GrammarIR, rid: RuleId) {
    let rule = ir.get_rule(rid);
    let name = ir.get_string(rule.name);
    println!(
        "=== rule #{rid} {name} (entry={}) ===",
        ir.entry == rid,
    );
    dump_node(ir, &rule.body, 0);
    println!();
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("usage: dump_ir <grammar.bbnf> [<rule>] [--structural]");
        std::process::exit(2);
    }

    let structural = args.iter().any(|a| a == "--structural");
    let positional: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    let path = PathBuf::from(positional[0]);
    let rule_filter = positional.get(1).map(|s| s.to_string());

    let request = CompileRequest {
        target: CompileTarget::Vm,
        options: PipelineOptions {
            structural,
            ..PipelineOptions::default()
        },
    };

    let output = match compile_paths_request(&[path.clone()], &request) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("pipeline error: {e}");
            std::process::exit(1);
        }
    };

    let ir = match output {
        CompileOutput::Vm(ir) => ir,
        _ => {
            eprintln!("expected Vm target");
            std::process::exit(1);
        }
    };

    println!("--- IR dump for {} ({}) ---", path.display(),
             if structural { "structural mode" } else { "standard mode" });
    println!("entry rule id = {}", ir.entry);
    println!("total rules = {}", ir.rules.len());
    println!();

    match rule_filter.as_deref() {
        None => {
            for rid in 0..ir.rules.len() as RuleId {
                dump_rule(&ir, rid);
            }
        }
        Some(name) => {
            let mut found = false;
            for rid in 0..ir.rules.len() as RuleId {
                let r = ir.get_rule(rid);
                if ir.get_string(r.name) == name {
                    dump_rule(&ir, rid);
                    found = true;
                }
            }
            if !found {
                eprintln!("rule not found: {name}");
                std::process::exit(1);
            }
        }
    }
}
