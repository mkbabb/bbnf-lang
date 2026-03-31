use std::collections::HashMap;

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf::{calculate_ast_deps, Expression};
use bbnf::analysis::{
    compute_first_sets, find_aliases, find_span_eligible_rules, find_transparent_alternations,
    tarjan_scc, topological_sort_scc,
};
use bbnf::grammar::BBNFGrammar;
use bbnf::lower::lower_to_ir;
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use bbnf_ir::GrammarIR;

const JSON_GRAMMAR: &str = r#"
null = "null" ;
bool = "true" | "false" ;
number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;
comma = "," ?w ;
colon = ":" ?w ;
string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
array = "[" >> (( value << comma ? ) *)?w << "]" ;
pair = string, colon >> value ;
object = "{" >> (( pair << comma ? ) *)?w << "}" ;
value = object | array | string | number | bool | null ;
    "#;

fn parse_json(input: &str) -> bbnf_ir::interpreter::ParseResult {
    let ir = compile_grammar(JSON_GRAMMAR, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    let mut interp = Interpreter::new(&program, input);
    interp.run()
}

#[test]
fn pipeline_compiles_json_grammar() {
    let ir = compile_grammar(JSON_GRAMMAR, &PipelineOptions::default()).unwrap();
    assert!(ir.rules.len() >= 8);
    assert!(ir.find_rule("value").is_some());

    // Verify entry is "value".
    assert_eq!(ir.get_string(ir.get_rule(ir.entry).name), "value");

    // Verify type inference populated types for all rules.
    assert!(
        !ir.types.is_empty(),
        "type inference should populate ir.types"
    );
    assert_eq!(ir.types.len(), ir.rules.len());
}

#[test]
fn pipeline_parse_null() {
    let result = parse_json("null");
    assert!(result.success, "failed to parse 'null'");
    assert_eq!(result.offset, 4);
}

#[test]
fn pipeline_parse_bool() {
    let result = parse_json("true");
    assert!(result.success, "failed to parse 'true'");
    assert_eq!(result.offset, 4);
}

#[test]
fn pipeline_parse_number() {
    let result = parse_json("42");
    assert!(result.success, "failed to parse '42'");
    assert_eq!(result.offset, 2);
}

#[test]
fn pipeline_parse_string() {
    let result = parse_json(r#""hello""#);
    assert!(result.success, "failed to parse string");
    assert_eq!(result.offset, 7);
}

#[test]
fn pipeline_parse_array() {
    let result = parse_json("[1, 2, 3]");
    assert!(result.success, "failed to parse array: {:?}", result);
    assert_eq!(result.offset, 9);
}

#[test]
fn pipeline_parse_object() {
    // Test simple object without spaces
    let result = parse_json(r#"{"a":"b"}"#);
    assert!(result.success, "failed to parse simple object: {:?}", result);
    assert_eq!(result.offset, 9);

    // Test object with space after colon
    let result = parse_json(r#"{"key": "value"}"#);
    assert!(result.success, "failed to parse object with space: {:?}", result);
    assert_eq!(result.offset, 16);
}

#[test]
fn pipeline_parse_nested() {
    let input = r#"{"a": [1, true, null], "b": {"c": "d"}}"#;
    let result = parse_json(input);
    assert!(result.success, "failed to parse nested JSON: {:?}", result);
    assert_eq!(result.offset, input.len() as u32);
}

#[test]
fn pipeline_left_recursion_elimination() {
    // expr = expr "+" term | term ;
    // term = /[0-9]+/ ;
    // This grammar has direct left recursion on `expr`.
    let lr_grammar = r#"
term = /[0-9]+/ ;
expr = expr, "+" >> term | term ;
        "#;

    let opts = PipelineOptions {
        remove_left_recursion: true,
        ..PipelineOptions::default()
    };
    let ir = compile_grammar(lr_grammar, &opts).unwrap();

    // Should compile without panic.
    let program = compile_bytecode(&ir);
    let mut interp = Interpreter::new(&program, "1+2+3");
    let result = interp.run();
    assert!(result.success, "failed to parse '1+2+3': {:?}", result);
    assert_eq!(result.offset, 5);
}

#[test]
fn pipeline_msgpack_roundtrip() {
    let ir = compile_grammar(JSON_GRAMMAR, &PipelineOptions::default()).unwrap();
    let bytes = ir.to_msgpack().unwrap();
    let ir2 = GrammarIR::from_msgpack(&bytes).unwrap();
    assert_eq!(ir.rules.len(), ir2.rules.len());
    assert_eq!(ir.strings.len(), ir2.strings.len());
}

/// Regression test: two regex branches in an alternation (no `?w`).
/// The 11-pass IR pipeline must not break dispatch for this pattern.
#[test]
fn pipeline_two_regex_alternation() {
    let grammar = r#"
value = string | number ;
string = /"[^"]*"/ ;
number = /-?\d+/ ;
        "#;

    let opts = PipelineOptions {
        entry_rule: Some("value".to_string()),
        ..PipelineOptions::default()
    };
    let ir = compile_grammar(grammar, &opts).unwrap();

    // At this point all 11 passes ran. Test first.
    let program = compile_bytecode(&ir);
    let mut interp = Interpreter::new(&program, r#""hello""#);
    let result = interp.run();
    assert!(result.success, "failed to parse string: {:?}", result);
    assert_eq!(result.offset, 7);

    let mut interp = Interpreter::new(&program, "42");
    let result = interp.run();
    assert!(result.success, "failed to parse number: {:?}", result);
    assert_eq!(result.offset, 2);
}

/// Bisect: run passes one-by-one and test after each.
#[test]
fn pipeline_bisect_passes() {
    let grammar = r#"
value = string | number ;
string = /"[^"]*"/ ;
number = /-?\d+/ ;
        "#;

    let source_static: &'static str = Box::leak(grammar.to_string().into_boxed_str());
    let parser = BBNFGrammar::grammar_with_imports();
    let (parsed, _) = parser.parse_return_state(source_static);
    let parsed = parsed.unwrap();
    let ast = parsed.rules;

    // Lower to IR without running passes.
    let deps = calculate_ast_deps(&ast);
    let scc_result = tarjan_scc(&deps);
    let ast = topological_sort_scc(&ast, &scc_result, &deps);
    let first_sets = compute_first_sets(&ast, &deps, &scc_result);
    let aliases = find_aliases(&ast, &scc_result.cyclic_rules);
    let transparent = find_transparent_alternations(&ast, &scc_result.cyclic_rules);
    let span_eligible = find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

    let entry_rule_name: Option<String> = ast.keys().last().and_then(|lhs| {
        if let Expression::Nonterminal(tok) = lhs {
            Some(tok.value.to_string())
        } else {
            None
        }
    });

    let mut ir = lower_to_ir(
        &ast,
        &first_sets,
        &scc_result,
        &aliases,
        &transparent,
        &span_eligible,
        None, None,
        &HashMap::new(),
        None,
        None,
        None,
        false,
    );

    if let Some(ref name) = entry_rule_name {
        if let Some(rule) = ir.find_rule(name) {
            ir.entry = rule.id;
        }
    }

    fn test_parse(ir: &GrammarIR, label: &str) -> bool {
        let program = compile_bytecode(ir);
        let mut interp = Interpreter::new(&program, r#""hello""#);
        let result = interp.run();
        eprintln!("{}: string={} num={}", label,
            result.success,
            {
                let mut i2 = Interpreter::new(&program, "42");
                i2.run().success
            }
        );
        result.success
    }

    assert!(test_parse(&ir, "0-base"), "base IR fails");

    bbnf_ir::passes::canonicalize_aliases(&mut ir);
    assert!(test_parse(&ir, "1-canonicalize_aliases"), "canonicalize_aliases broke it");

    bbnf_ir::passes::prune_unreachable(&mut ir);
    assert!(test_parse(&ir, "2-prune_unreachable"), "prune_unreachable broke it");

    bbnf_ir::passes::inline_acyclic(&mut ir);
    assert!(test_parse(&ir, "3-inline_acyclic"), "inline_acyclic broke it");

    bbnf_ir::passes::eliminate_epsilon(&mut ir);
    assert!(test_parse(&ir, "4-eliminate_epsilon"), "eliminate_epsilon broke it");

    bbnf_ir::passes::merge_literals(&mut ir);
    assert!(test_parse(&ir, "5-merge_literals"), "merge_literals broke it");

    bbnf_ir::passes::merge_regex_alts(&mut ir);
    assert!(test_parse(&ir, "6-merge_regex_alts"), "merge_regex_alts broke it");

    bbnf_ir::passes::factor_common_prefixes(&mut ir);
    assert!(test_parse(&ir, "7-factor_common_prefixes"), "factor_common_prefixes broke it");

    bbnf_ir::passes::refine_span_eligibility(&mut ir);
    assert!(test_parse(&ir, "8-refine_span_eligibility"), "refine_span_eligibility broke it");

    ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&ir);
    assert!(test_parse(&ir, "9-compute_follow_sets"), "compute_follow_sets broke it");

    bbnf_ir::passes::generate_dispatch_tables(&mut ir);
    assert!(test_parse(&ir, "10-generate_dispatch_tables"), "generate_dispatch_tables broke it");

    bbnf_ir::passes::infer_types(&mut ir);
    assert!(test_parse(&ir, "11-infer_types"), "infer_types broke it");
}

#[test]
fn pipeline_next_operator() {
    // `>>` (Next) discards left's value, keeps right's.
    // `<<` (Skip) keeps left's value, discards right's.
    // `>>` does NOT implicitly trim whitespace — use `?w` for that.
    let grammar = r#"a = "x", ":" >> "y" ?w ;"#;
    let ir = compile_grammar(grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);

    // Input "x:y" — ":" >> "y" keeps "y".
    let mut interp = Interpreter::new(&program, "x:y");
    let r = interp.run();
    assert!(r.success, "failed to parse 'x:y': {:?}", r);
    assert_eq!(r.offset, 3);

    // Input "x: y" — whitespace trimmed by ?w on "y".
    let mut interp = Interpreter::new(&program, "x: y");
    let r = interp.run();
    assert!(r.success, "failed to parse 'x: y': {:?}", r);
    assert_eq!(r.offset, 4);
}

#[test]
fn pipeline_type_inference_json() {
    use bbnf_ir::TypeDesc;

    let ir = compile_grammar(JSON_GRAMMAR, &PipelineOptions::default()).unwrap();

    // Every rule should have a type.
    assert_eq!(ir.types.len(), ir.rules.len());

    // Helper: look up type by rule name.
    let type_of = |name: &str| -> &TypeDesc {
        let rule = ir.find_rule(name).unwrap();
        ir.types
            .iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, t)| t)
            .unwrap()
    };

    // Leaf rules (literal/regex) should produce Span.
    assert_eq!(*type_of("null"), TypeDesc::Span);
    assert_eq!(*type_of("bool"), TypeDesc::Span);
    assert_eq!(*type_of("number"), TypeDesc::Span);
    assert_eq!(*type_of("string"), TypeDesc::Span);
    assert_eq!(*type_of("comma"), TypeDesc::Span);
    assert_eq!(*type_of("colon"), TypeDesc::Span);
}

#[test]
fn pipeline_google_sheets_formula() {
    let grammar = std::fs::read_to_string("../../grammar/google-sheets/google-sheets.bbnf")
        .expect("failed to read google-sheets.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);

    // Test individual rules directly to isolate issues
    let test_rule = |ir: &bbnf_ir::GrammarIR, name: &str, input: &str, trace: bool| -> (bool, u32) {
        let rule = ir.find_rule(name).unwrap_or_else(|| panic!("{} not found", name));
        let mut test_ir = ir.clone();
        test_ir.entry = rule.id;
        let prog = compile_bytecode(&test_ir);
        let mut interp = Interpreter::new(&prog, input);
        interp.trace = trace;
        let result = interp.run();
        eprintln!("{} '{}': success={} offset={}", name, input, result.success, result.offset);
        (result.success, result.offset)
    };

    test_rule(&ir, "identifier", "SUM", false);
    test_rule(&ir, "number", "42", false);
    let (ok, off) = test_rule(&ir, "func_call", "SUM(1)", true);
    assert!(ok, "func_call should parse 'SUM(1)'");
    assert_eq!(off, 6, "func_call should consume all of 'SUM(1)'");

    // Full formula parse
    let mut interp = Interpreter::new(&program, "=SUM(1)");
    let result = interp.run();
    eprintln!("formula '=SUM(1)': success={} offset={}", result.success, result.offset);
    assert_eq!(result.offset as usize, "=SUM(1)".len(), "should consume all input");

    // Test IF (3 args = 1 pair + 1 solo)
    let mut interp = Interpreter::new(&program, "=IF(1,2,3)");
    let result = interp.run();
    eprintln!("formula '=IF(1,2,3)': success={} offset={}", result.success, result.offset);
    assert!(result.success, "IF(1,2,3) failed");

    // Test LET (pairs)
    let mut interp = Interpreter::new(&program, "=LET(x,1,y,2,x)");
    let result = interp.run();
    eprintln!("formula '=LET(x,1,y,2,x)': success={} offset={}", result.success, result.offset);
    assert!(result.success, "LET(x,1,y,2,x) failed at offset={}", result.offset);

    // Test nested functions
    let mut interp = Interpreter::new(&program, "=LET(x,SUM(A1:A10),x)");
    let result = interp.run();
    eprintln!("nested LET: success={} offset={}", result.success, result.offset);
    assert!(result.success, "nested LET failed at offset={}", result.offset);

    // Test empty args
    let mut interp = Interpreter::new(&program, "=INDEX(A1,,3)");
    let result = interp.run();
    eprintln!("empty args: success={} offset={}", result.success, result.offset);

    // Progressively more complex
    let tests = [
        r#"=LET(data, A1:Z100, data)"#,
        r#"=LET(data, A1:Z100, filtered, FILTER(data, A1>0), data)"#,
        r#"=LET(data, A1:Z100, filtered, FILTER(data, INDEX(data,,1)>0), data)"#,
        r#"=IF(count>0, MAKEARRAY(count, 3, LAMBDA(r, c, INDEX(filtered, r, c))), "No data")"#,
    ];
    for t in tests {
        let mut interp = Interpreter::new(&program, t);
        let result = interp.run();
        let ok = result.success && result.offset as usize == t.len();
        eprintln!("  {} [{}] offset={}/{}", if ok { "OK" } else { "FAIL" }, &t[..t.len().min(60)], result.offset, t.len());
    }

    // LET with nested functions
    let input = r#"=LET(data, A1:Z100, filtered, FILTER(data, INDEX(data,,1)>0), count, ROWS(filtered), IF(count>0, MAKEARRAY(count, 3, LAMBDA(r, c, INDEX(filtered, r, c))), "No data"))"#;
    let mut interp = Interpreter::new(&program, input);
    let result = interp.run();
    eprintln!("LET formula: success={} offset={} len={} remaining='{}'",
        result.success, result.offset, input.len(),
        &input[result.offset as usize..]);
    assert!(result.success, "failed to parse LET formula: offset={}", result.offset);
    assert_eq!(result.offset as usize, input.len(), "should consume all input");

    // Test func_call rule directly
    let (fc_ok, fc_off) = test_rule(&ir, "func_call", "IF(1,2)", false);
    assert!(fc_ok, "func_call should parse 'IF(1,2)' at offset {}", fc_off);

    // Formatting via gorgeous VM
    let value = result.value.as_ref().unwrap();
    let printer = pprint::Printer::new(80, 2, false);
    let formatted = gorgeous::vm::format_value(&ir, value, input, printer);
    assert!(formatted.is_some(), "formatting should produce output");
    let formatted = formatted.unwrap();
    eprintln!("Formatted output:\n{}", formatted);
    assert!(formatted.contains("LET"), "formatted output should contain LET");
    assert!(formatted.contains('\n'), "formatted output should contain line breaks");

    // Pathological formula: deeply nested LET + IF + LAMBDA
    let pathological = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))"#;
    let mut interp = Interpreter::new(&program, pathological);
    let result = interp.run();
    assert!(result.success, "pathological formula failed at offset={}", result.offset);
    assert_eq!(result.offset as usize, pathological.len(), "should consume all input");
    let value = result.value.as_ref().unwrap();
    let formatted = gorgeous::vm::format_value(&ir, value, pathological, pprint::Printer::new(80, 2, false));
    let formatted = formatted.unwrap();
    eprintln!("Pathological:\n{}", formatted);
    assert!(formatted.contains('\n'), "pathological should have line breaks");
    assert!(formatted.lines().count() >= 5, "pathological should have 5+ lines");

    // Test with trailing space before final paren
    let with_space = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results") )"#;
    let mut interp = Interpreter::new(&program, with_space);
    let result = interp.run();
    eprintln!("with_space: success={} offset={} len={} remaining='{}'",
        result.success, result.offset, with_space.len(),
        &with_space[result.offset as usize..]);
    assert!(result.success, "with_space formula failed at offset={}", result.offset);
    assert_eq!(result.offset as usize, with_space.len(), "should consume all input");
    let value = result.value.as_ref().unwrap();
    let formatted_space = gorgeous::vm::format_value(&ir, value, with_space, pprint::Printer::new(80, 2, false));
    let formatted_space = formatted_space.unwrap();
    eprintln!("With space formatted:\n{}", formatted_space);
    // Both should produce identical formatted output (whitespace is insignificant)
    eprintln!("Without space formatted:\n{}", formatted);
    assert_eq!(formatted_space, formatted, "trailing space should not change formatting");
}

#[test]
fn pipeline_google_sheets_multiline_let() {
    let grammar = std::fs::read_to_string("../../grammar/google-sheets/google-sheets.bbnf")
        .expect("failed to read google-sheets.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);

    let test_rule = |name: &str, input: &str| -> (bool, u32) {
        let rule = ir.find_rule(name).unwrap_or_else(|| panic!("{} not found", name));
        let mut test_ir = ir.clone();
        test_ir.entry = rule.id;
        let prog = compile_bytecode(&test_ir);
        let mut interp = Interpreter::new(&prog, input);
        let result = interp.run();
        eprintln!("  {} '{}': success={} offset={}/{}", name,
            &input[..input.len().min(60)], result.success, result.offset, input.len());
        (result.success, result.offset)
    };

    // Sub-expression diagnostics
    let subs: &[(&str, &str)] = &[
        ("cell_or_range", "B3:B"),
        ("cell_or_range", "Sheet1!C2:C"),
        ("cell_or_range", "Sheet1!AD2:AD"),
        ("cell_or_range", "H2:O2"),
        ("cell_or_range", "H3:O"),
        ("cell_or_range", "A:A"),
        ("expression", r#"B3:B <> """#),
        ("func_call", r#"FILTER(B3:B, B3:B <> "")"#),
        ("lambda_call", "LAMBDA(x, LOWER(TRIM(TO_TEXT(x))))"),
        ("expression", "(sheet1Psus = psu) * (sheet1Providers = provider)"),
        ("func_call", "MATCH(1, (sheet1Psus = psu) * (sheet1Providers = provider), 0)"),
        ("func_call", "N(INDEX(recurring, r, c))"),
        ("func_call", "IFERROR(INDEX(oneTimeCosts, MATCH(1, (sheet1Psus = psu) * (sheet1Providers = provider), 0)), 0)"),
        ("expression", "monthlyValue * scale + N(oneTimeCost)"),
        ("func_call", "IF(monthlyValue > 0, monthlyValue * scale + N(oneTimeCost), 0)"),
        ("func_call", "VSTACK(providers, values)"),
    ];
    for (rule_name, sub) in subs {
        let (ok, off) = test_rule(rule_name, sub);
        assert!(ok && off as usize == sub.len(),
            "{} should fully parse '{}' (offset={}/{})", rule_name, sub, off, sub.len());
    }

    // Full multiline formula
    let input = r#"=LET(
  scale, DURATION,

  psus, FILTER(B3:B, B3:B <> ""),
  providers, H2:O2,
  recurring, FILTER(H3:O, B3:B <> ""),

  normalize, LAMBDA(x, LOWER(TRIM(TO_TEXT(x)))),

  sheet1Psus, ARRAYFORMULA(Sheet1!C2:C),
  sheet1Providers, ARRAYFORMULA(Sheet1!B2:B),
  oneTimeCosts, Sheet1!AD2:AD,

  values,
    MAKEARRAY(
      ROWS(psus),
      COLUMNS(providers),
      LAMBDA(r, c,
        LET(
          psu, INDEX(psus, r),
          provider, INDEX(providers, c),
          monthlyValue, N(INDEX(recurring, r, c)),
          oneTimeCost,
            IFERROR(
              INDEX(
                oneTimeCosts,
                MATCH(
                  1,
                  (sheet1Psus = psu) * (sheet1Providers = provider),
                  0
                )
              ),
              0
            ),
          IF(monthlyValue > 0, monthlyValue * scale + N(oneTimeCost), 0)
        )
      )
    ),

  VSTACK(providers, values)
)"#;

    let mut interp = Interpreter::new(&program, input);
    let result = interp.run();
    eprintln!("multiline LET: success={} offset={} len={} remaining='{}'",
        result.success, result.offset, input.len(),
        &input[result.offset as usize..]);
    assert!(result.success, "multiline LET failed at offset={}", result.offset);
    assert_eq!(result.offset as usize, input.len(), "should consume all input");

    // Format via VM
    let value = result.value.as_ref().unwrap();
    let formatted = gorgeous::vm::format_value(&ir, value, input, pprint::Printer::new(80, 2, false));
    let formatted = formatted.unwrap();
    eprintln!("VM formatted:\n{}", formatted);

    // Each let_binding (name, value) should stay on one line when it fits
    assert!(formatted.contains("scale, DURATION"), "name-value pair should stay on one line");

    // Same formula without leading =
    let no_eq = &input[1..];
    let mut interp = Interpreter::new(&program, no_eq);
    let result = interp.run();
    eprintln!("no-eq LET: success={} offset={} len={}", result.success, result.offset, no_eq.len());
    assert!(result.success, "formula without = failed at offset={}", result.offset);
    assert_eq!(result.offset as usize, no_eq.len(), "should consume all input without =");
}

#[test]
fn pipeline_span_capture_type_inference() {
    // @{expr} should always infer TypeDesc::Span, regardless of inner expression type.
    let grammar = r#"
        number = /[0-9]+/ ;
        comma  = "," ;
        pair   = number , comma , number ;
        captured = @{ pair } ;
        value  = captured | number ;
    "#;

    let ir = compile_grammar(grammar, &PipelineOptions::default()).unwrap();

    // Find the "captured" rule and verify its type is Span.
    let captured_rule = ir.find_rule("captured").expect("rule 'captured' should exist");
    let captured_id = captured_rule.id;
    let captured_type = ir
        .types
        .iter()
        .find(|(id, _)| *id == captured_id)
        .map(|(_, td)| td);
    assert_eq!(
        captured_type,
        Some(&bbnf_ir::TypeDesc::Span),
        "span capture @{{...}} should infer TypeDesc::Span"
    );

    // Verify it parses correctly via the interpreter.
    let program = bbnf_ir::compiler::compile(&ir);
    let mut interp = Interpreter::new(&program, "123,456");
    let result = interp.run();
    assert!(result.success, "span capture should parse '123,456'");
    assert_eq!(result.offset as usize, 7);
}
