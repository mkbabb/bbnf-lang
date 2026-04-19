//! AX.W0a.2.o probe — characterize JSON / CSS / Sheets shape regressions
//! + IR-level inspection.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf")]
struct SheetsParser;

#[test]
fn probe_json_scalars_vs_compounds() {
    for input in &["null", "true", "false", "42", "\"hello\""] {
        let r = JsonParser::parse(input);
        eprintln!("JSON scalar {:?}: {:?}", input, r.map(|_| "ok"));
    }
    for input in &["[]", "[1, 2, 3]", "{}", r#"{"a": 1}"#] {
        let r = JsonParser::parse(input);
        eprintln!("JSON compound {:?}: {:?}", input, r.map(|_| "ok"));
    }
}

#[test]
fn probe_css_minimal() {
    for input in &["a { width: 50%; }", "a { }", "a{}"] {
        let r = CssL4Parser::parse(input);
        eprintln!("CSS {:?}: {:?}", input, r.map(|_| "ok"));
    }
}

#[test]
fn probe_sheets_minimal() {
    for input in &["1", "1 + 2", "#REF!", "A1", "true"] {
        let r = SheetsParser::parse(input);
        eprintln!("Sheets {:?}: {:?}", input, r.map(|_| "ok"));
    }
}

#[test]
fn dump_css_ruleblock_ir() {
    use bbnf::pipeline::{CompileRequest, CompileOutput, CompileTarget, PipelineOptions};
    use bbnf::pipeline::compile::compile_paths_request;

    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    for path in &[
        "../../grammar/css/l4/stylesheet.bbnf",
        "../../grammar/json/json.bbnf",
    ] {
        eprintln!("=== {}", path);
        match compile_paths_request(&[path.into()], &request) {
            Ok(CompileOutput::Vm(ir)) => {
                for rule in &ir.rules {
                    let name = ir.get_string(rule.name);
                    if matches!(
                        name,
                        "ruleBlock" | "blockContent" | "stylesheet" | "ruleList" |
                        "array" | "object" | "pair" | "value"
                    ) {
                        eprintln!("RULE {} = {:#?}", name, rule.body);
                        eprintln!("  shape = {:?}", ir.shape_assignments.get(rule.id));
                    }
                }
            }
            Ok(_) => eprintln!("non-VM output"),
            Err(e) => eprintln!("compile failed: {:?}", e),
        }
    }
}

#[test]
fn dump_bbnf_binary_factor_parse_of_json_snippet() {
    // Use BbnfBootstrap to parse the json.bbnf snippet `"[" >> "X" << "]"`
    // and inspect the binary_factor Pratt output.
    use bbnf::grammar::generated::BbnfBootstrap;

    let input = r#"array = "[" >> "X" << "]" ;
"#;
    let parsed = BbnfBootstrap::parse(input).expect("parse");
    let tape = parsed.tape();
    eprintln!("tape len = {}", tape.iter().count());
    for (i, rec) in tape.iter().enumerate() {
        eprintln!(
            "  rec {} kind={:?} span=({},{}) vi={} meta={} ch={:?}",
            i,
            rec.kind(),
            rec.span_lo,
            rec.span_hi,
            rec.variant_idx(),
            rec.meta_idx(),
            rec.child_off,
        );
    }

    // Walk rec 30's + rec 29's children to verify sib_skip.
    use bbnf::runtime::tape::{TapeCursor};
    for &parent_idx in &[30u32, 29u32, 19u32, 38u32, 34u32, 33u32, 32u32, 36u32, 37u32] {
        let cur = TapeCursor::new(tape, bbnf::runtime::tape::TapeOffset(parent_idx));
        eprintln!("\n--- children of rec {} (kind={:?}, vi={}) ---", parent_idx, cur.kind(), cur.record().variant_idx());
        for child in cur.children_zero_alloc() {
            let rec = child.record();
            eprintln!(
                "  child kind={:?} span=({},{}) vi={}",
                rec.kind(),
                rec.span_lo,
                rec.span_hi,
                rec.variant_idx(),
            );
        }
    }

    // Now use host.rs to extract AST + lower to IR.
    eprintln!("\n=== Lower to IR ===");
    use bbnf::pipeline::{CompileRequest, CompileOutput, CompileTarget, PipelineOptions};
    use bbnf::pipeline::compile::compile_grammar_request;
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    match compile_grammar_request(input, &request) {
        Ok(CompileOutput::Vm(ir)) => {
            for rule in &ir.rules {
                let name = ir.get_string(rule.name);
                if name == "array" {
                    eprintln!("RULE {} = {:#?}", name, rule.body);
                    eprintln!("  shape = {:?}", ir.shape_assignments.get(rule.id));
                }
            }
        }
        Ok(_) => eprintln!("non-VM output"),
        Err(e) => eprintln!("compile failed: {:?}", e),
    }
}

