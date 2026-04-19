//! AW-III.W1 — DTA walker trace for Sheets `add_op` branches.

use bbnf::runtime::tape::TapeKind;
use bbnf_derive::Parser;

#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", skip_recover)]
struct SheetsParser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4ParserDebug;

#[test]
fn css_percentage_dump() {
    let input = "a { width: 50%; }";
    let parsed = CssL4ParserDebug::parse(input).expect("parse");
    let tape = parsed.tape();
    eprintln!("tape.len = {}, arena.len = {}", tape.len(), tape.arena().len());
    eprintln!("arena bytes: {:02X?}", tape.arena());
    for (i, rec) in tape.iter().enumerate() {
        let u8 = tape.payload_u8(rec);
        let bytes_1 = tape.payload_bytes(rec, 1);
        eprintln!(
            "[{i:3}] kind={:?} variant_idx={} has_payload={} child_off={:?} u8={:?} bytes_1={:?}",
            rec.kind(), rec.variant_idx(), rec.has_payload(), rec.child_off, u8, bytes_1
        );
    }
}

#[test]
fn css_white_color_dump() {
    use std::path::PathBuf;
    let input = "a { color: white; }";
    let parsed = CssL4ParserDebug::parse(input).expect("parse");
    let tape = parsed.tape();
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .unwrap()
        .to_path_buf();
    let bbnf = workspace.join("grammar/css/l4/stylesheet.bbnf");
    let req = bbnf::pipeline::CompileRequest {
        target: bbnf::pipeline::CompileTarget::Vm,
        options: bbnf::pipeline::PipelineOptions::default(),
    };
    let output = bbnf::pipeline::compile_paths_request(&[bbnf], &req).expect("compile");
    let ir = match output {
        bbnf::pipeline::CompileOutput::Vm(ir) => ir,
        _ => panic!(),
    };
    let nc = ir.rules.iter().find(|r| ir.get_string(r.name) == "namedColor").unwrap();
    eprintln!("namedColor rule id = {} (& 0x3F) = {} (& 0xFF) = {}", nc.id, nc.id & 0x3F, nc.id & 0xFF);
    eprintln!("tape.len = {}, arena.len = {}", tape.len(), tape.arena().len());
    eprintln!("arena bytes: {:02X?}", tape.arena());
    for (i, rec) in tape.iter().enumerate() {
        eprintln!(
            "[{i:3}] kind={:?} variant_idx={} has_payload={} has_children={} child_off={:?}",
            rec.kind(), rec.variant_idx(), rec.has_payload(), rec.has_children(), rec.child_off
        );
    }
}

#[test]
fn add_op_minus_branch_dump() {
    let input = "=1-2";
    let parsed = SheetsParser::parse(input).expect("parse");
    let tape = parsed.tape();
    eprintln!("tape len = {}", tape.len());
    for (i, rec) in tape.iter().enumerate() {
        let bytes_1 = tape.payload_bytes(rec, 1);
        let bytes_8 = tape.payload_bytes(rec, 8);
        eprintln!(
            "[{i:3}] kind={:8?} variant_idx={:3} has_payload={:5} child_off={:?} bytes_1={:?} bytes_8.is_some={}",
            rec.kind(),
            rec.variant_idx(),
            rec.has_payload(),
            rec.child_off,
            bytes_1,
            bytes_8.is_some()
        );
        if i > 100 {
            break;
        }
    }
}

#[test]
fn _ignore_keep_assertion_off() {
    // Placeholder so the trace test fires under default cargo test —
    // the trace dumps are eprintln so cargo test's stderr capture
    // shows them on the failure path.
    let input = "=1-2";
    let parsed = SheetsParser::parse(input).expect("parse");
    let tape = parsed.tape();
    let kvs: Vec<_> = tape
        .iter()
        .enumerate()
        .filter(|(_, r)| r.kind() == TapeKind::KvPair || r.kind() == TapeKind::Span)
        .filter(|(_, r)| r.has_payload())
        .map(|(i, r)| (i, r.kind(), r.variant_idx(), tape.payload_bytes(r, 1).map(|b| b[0])))
        .collect();
    eprintln!("payload-bearing: {:?}", kvs);
}
