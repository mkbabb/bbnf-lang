use bbnf::runtime::tape::TapeKind;
use bbnf_derive::Parser;

#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_: &str) -> u32 {
        0
    }
}

#[test]
fn probe_error_literal_ir() {
    use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions};
    use bbnf::pipeline::compile::compile_paths_request;

    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    match compile_paths_request(
        &["../../grammar/google-sheets/google-sheets.bbnf".into()],
        &request,
    ) {
        Ok(CompileOutput::Vm(ir)) => {
            for rule in &ir.rules {
                let name = ir.get_string(rule.name);
                if matches!(name, "error_literal") {
                    eprintln!("RULE {name} body={:#?}", rule.body);
                }
            }
        }
        Ok(_) => eprintln!("non-VM"),
        Err(e) => eprintln!("compile failed: {e:?}"),
    }
}

#[test]
fn probe_dir_pseudo_type() {
    use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions};
    use bbnf::pipeline::compile::compile_paths_request;

    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    for path in &[
        "../../grammar/css/l4/stylesheet.bbnf",
        "../../grammar/json/json.bbnf",
        "../../grammar/google-sheets/google-sheets.bbnf",
    ] {
        eprintln!("=== {path}");
        match compile_paths_request(&[path.to_string().into()], &request) {
            Ok(CompileOutput::Vm(ir)) => {
                for rule in &ir.rules {
                    let name = ir.get_string(rule.name);
                    if matches!(
                        name,
                        "dirPseudo"
                            | "dirKeyword"
                            | "percentageUnit"
                            | "percentage"
                            | "hex"
                            | "namedColor"
                            | "keyframeStop"
                            | "null"
                            | "bool"
                            | "boolean"
                            | "error_literal"
                            | "add_op"
                            | "mul_op"
                            | "compare_op"
                            | "number"
                            | "range_ref"
                            | "range_end"
                    ) {
                        let ty = ir.types.iter().find_map(|(rid, t)| {
                            if *rid == rule.id {
                                Some(t.clone())
                            } else {
                                None
                            }
                        });
                        let shape = ir.shape_assignments.get(rule.id);
                        eprintln!("  RULE {name} shape={shape:?} type={ty:?}");
                    }
                }
            }
            Ok(_) => eprintln!("  non-VM"),
            Err(e) => eprintln!("  compile failed: {e:?}"),
        }
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf")]
struct SheetsParser;

#[test]
fn probe() {
    for input in &[
        "a { opacity: 1; }",
        "a { opacity: .5; }",
        "a { opacity: 0.5; }",
        "a { x: .5; }",
        "@keyframes pulse { 50% { opacity: .5; } }",
    ] {
        let r = CssL4Parser::parse(input);
        eprintln!("CSS {:?}: {:?}", input, r.map(|_| "ok"));
    }
}

#[test]
fn probe_dir_tape() {
    let input = "a:dir(rtl) { color: red; }";
    let parsed = CssL4Parser::parse(input).expect("parse");
    let tape = parsed.tape();
    for (i, rec) in tape.iter().enumerate() {
        let kind = rec.kind();
        let has_p = rec.has_payload();
        let payload_u8 = tape.payload_u8(rec);
        let payload_bytes_1 = tape.payload_bytes(rec, 1).map(|b| b[0]);
        eprintln!(
            "rec {i} kind={:?} span=({},{}) vi={} has_payload={} payload_u8={:?} bytes1={:?}",
            kind,
            rec.span_lo,
            rec.span_hi,
            rec.variant_idx(),
            has_p,
            payload_u8,
            payload_bytes_1
        );
    }
    let _ = TapeKind::KvPair;
}

#[test]
fn probe_sheets_range() {
    for input in &["=A1:B2", "=A:A", "=Sheet1!A1:B2", "='Sheet 1'!A1:B2", "=42"] {
        let r = SheetsParser::parse(input);
        eprintln!("Sheets {:?}: {:?}", input, r.map(|_| "ok"));
    }
}

#[test]
fn probe_error_literal_tape() {
    for input in &["=#NULL!", "=#VALUE!", "=#REF!", "=#N/A", "=TRUE"] {
        eprintln!("\n=== Input: {input}");
        let parsed = SheetsParser::parse(input).expect("parse");
        let tape = parsed.tape();
        for (i, rec) in tape.iter().enumerate() {
            let kind = rec.kind();
            let has_p = rec.has_payload();
            let payload_u8 = tape.payload_u8(rec);
            let payload_bytes_1 = tape.payload_bytes(rec, 1).map(|b| b[0]);
            eprintln!(
                "rec {i} kind={:?} span=({},{}) vi={} has_payload={} payload_u8={:?} bytes1={:?}",
                kind,
                rec.span_lo,
                rec.span_hi,
                rec.variant_idx(),
                has_p,
                payload_u8,
                payload_bytes_1
            );
        }
    }
}
