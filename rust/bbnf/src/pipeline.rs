//! Full analysis + IR lowering pipeline.
//!
//! Orchestrates: parse → analysis → lower → IR passes, producing a `GrammarIR`
//! ready for consumption by any backend (Rust codegen, bytecode VM, TS interpreter).

use std::collections::{HashMap, HashSet};

use bbnf_ir::GrammarIR;

use crate::analysis::{
    compute_first_sets, find_aliases, find_span_eligible_rules, find_transparent_alternations,
    tarjan_scc, topological_sort_scc,
};
use crate::grammar::BBNFGrammar;
use crate::lower::lower_to_ir;
use crate::optimize::{remove_direct_left_recursion, remove_indirect_left_recursion};
use crate::types::{AST, Expression};

/// Options for the compilation pipeline.
#[derive(Default)]
pub struct PipelineOptions {
    /// Whether to apply left-recursion elimination.
    pub remove_left_recursion: bool,
    /// Override the entry rule name. If `None`, defaults to the last rule in source order.
    pub entry_rule: Option<String>,
}

/// Compile a BBNF grammar source string to a `GrammarIR`.
///
/// This is the main entry point for the WASM bytecode VM path.
/// Parses the grammar, runs all analysis passes, lowers to IR, and runs IR passes.
pub fn compile_grammar(source: &str, options: &PipelineOptions) -> Result<GrammarIR, String> {
    // Leak to get 'static lifetime — the AST borrows from the source string.
    // For WASM usage this is fine: each grammar compilation is a one-shot operation.
    let source_static: &'static str = Box::leak(source.to_string().into_boxed_str());

    let parser = BBNFGrammar::grammar_with_imports();
    let (parsed, _state) = parser.parse_return_state(source_static);

    let parsed = parsed.ok_or_else(|| "Failed to parse grammar".to_string())?;

    // Extract directives.
    let mut recover_map: HashMap<String, Expression<'static>> = HashMap::new();
    let mut pretty_map: HashMap<String, Vec<String>> = HashMap::new();
    let mut no_collapse_set: HashSet<String> = HashSet::new();

    for rec in &parsed.recovers {
        recover_map.insert(rec.rule_name.to_string(), rec.sync_expr.clone());
    }
    for p in &parsed.pretties {
        pretty_map.insert(
            p.rule_name.to_string(),
            p.hints.iter().map(|h| h.to_string()).collect(),
        );
    }
    for nc in &parsed.no_collapses {
        no_collapse_set.insert(nc.rule_name.to_string());
    }

    let ast = parsed.rules;
    compile_ast(
        ast,
        &recover_map,
        &pretty_map,
        &no_collapse_set,
        options,
    )
}

/// Compile an already-parsed AST to `GrammarIR`.
///
/// Useful when the AST is already available (e.g., from `DocumentState`).
pub fn compile_ast<'a>(
    ast: AST<'a>,
    recover_map: &HashMap<String, Expression<'a>>,
    pretty_map: &HashMap<String, Vec<String>>,
    no_collapse_set: &HashSet<String>,
    options: &PipelineOptions,
) -> Result<GrammarIR, String> {
    // Determine the entry rule name: use override if provided, otherwise last rule in source order.
    let entry_rule_name: Option<String> = options.entry_rule.clone().or_else(|| {
        ast.keys().last().and_then(|lhs| {
            if let Expression::Nonterminal(tok) = lhs {
                Some(tok.value.to_string())
            } else {
                None
            }
        })
    });

    // Optional: remove left-recursion (indirect first via Paull's, then direct).
    let ast = if options.remove_left_recursion {
        // Extract multi-member SCC names (owned) so deps/scc_result can be dropped.
        let indirect_sccs = {
            let deps = crate::calculate_ast_deps(&ast);
            let scc_result = tarjan_scc(&deps);
            scc_result
                .sccs
                .iter()
                .filter(|scc| scc.len() > 1)
                .map(|scc| {
                    scc.iter()
                        .filter_map(|expr| match expr {
                            Expression::Nonterminal(tok) => Some(tok.value.to_string()),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        };
        let ast = remove_indirect_left_recursion(&ast, &indirect_sccs);
        remove_direct_left_recursion(&ast)
    } else {
        ast
    };

    // Dependency analysis (recomputed after potential LR transformation).
    let deps = crate::calculate_ast_deps(&ast);

    // SCC detection + topological ordering.
    let scc_result = tarjan_scc(&deps);
    let ast = topological_sort_scc(&ast, &scc_result, &deps);

    // FIRST set computation.
    let first_sets = compute_first_sets(&ast, &deps, &scc_result);

    // Alias, transparent, and span-eligible detection.
    let aliases = find_aliases(&ast, &scc_result.cyclic_rules);
    let transparent_rules = find_transparent_alternations(&ast, &scc_result.cyclic_rules);
    let span_eligible_rules = find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

    // Directive refs.
    let recovers_ref = if recover_map.is_empty() {
        None
    } else {
        Some(recover_map)
    };
    let pretties_ref = if pretty_map.is_empty() {
        None
    } else {
        Some(pretty_map)
    };
    let no_collapse_ref = if no_collapse_set.is_empty() {
        None
    } else {
        Some(no_collapse_set)
    };

    // Dispatch tables (empty for now — IR passes generate these).
    let dispatch_tables = HashMap::new();

    // Lower to IR.
    let mut ir = lower_to_ir(
        &ast,
        &first_sets,
        &scc_result,
        &aliases,
        &transparent_rules,
        &span_eligible_rules,
        recovers_ref,
        pretties_ref,
        no_collapse_ref,
        &dispatch_tables,
    );

    // Set the correct entry rule (last rule in original source order).
    if let Some(ref name) = entry_rule_name {
        if let Some(rule) = ir.find_rule(name) {
            ir.entry = rule.id;
        }
    }

    // Run IR optimization passes.
    bbnf_ir::passes::canonicalize_aliases(&mut ir);
    bbnf_ir::passes::prune_unreachable(&mut ir);
    bbnf_ir::passes::inline_acyclic(&mut ir);
    bbnf_ir::passes::eliminate_epsilon(&mut ir);
    bbnf_ir::passes::merge_literals(&mut ir);
    bbnf_ir::passes::merge_regex_alts(&mut ir);
    bbnf_ir::passes::factor_common_prefixes(&mut ir);
    bbnf_ir::passes::refine_span_eligibility(&mut ir);

    // Compute FOLLOW sets before dispatch and memo passes that consume them.
    ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&ir);

    // Dispatch tables use FOLLOW sets for nullable branch optimization.
    bbnf_ir::passes::generate_dispatch_tables(&mut ir);

    // Memo strategies use FOLLOW set cardinality as a signal.
    bbnf_ir::passes::refine_memo_strategies(&mut ir);

    // Type inference (populates GrammarIR::types for codegen backends).
    bbnf_ir::passes::infer_types(&mut ir);

    Ok(ir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use bbnf_ir::compiler::compile as compile_bytecode;
    use bbnf_ir::interpreter::Interpreter;

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
        let parser = crate::grammar::BBNFGrammar::grammar_with_imports();
        let (parsed, _) = parser.parse_return_state(source_static);
        let parsed = parsed.unwrap();
        let ast = parsed.rules;

        // Lower to IR without running passes.
        let deps = crate::calculate_ast_deps(&ast);
        let scc_result = crate::analysis::tarjan_scc(&deps);
        let ast = crate::analysis::topological_sort_scc(&ast, &scc_result, &deps);
        let first_sets = crate::analysis::compute_first_sets(&ast, &deps, &scc_result);
        let aliases = crate::analysis::find_aliases(&ast, &scc_result.cyclic_rules);
        let transparent = crate::analysis::find_transparent_alternations(&ast, &scc_result.cyclic_rules);
        let span_eligible = crate::analysis::find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

        let entry_rule_name: Option<String> = ast.keys().last().and_then(|lhs| {
            if let Expression::Nonterminal(tok) = lhs {
                Some(tok.value.to_string())
            } else {
                None
            }
        });

        let mut ir = crate::lower::lower_to_ir(
            &ast,
            &first_sets,
            &scc_result,
            &aliases,
            &transparent,
            &span_eligible,
            None, None, None,
            &HashMap::new(),
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

        bbnf_ir::passes::refine_memo_strategies(&mut ir);
        assert!(test_parse(&ir, "11-refine_memo_strategies"), "refine_memo_strategies broke it");

        bbnf_ir::passes::infer_types(&mut ir);
        assert!(test_parse(&ir, "12-infer_types"), "infer_types broke it");
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
        let grammar = std::fs::read_to_string("../../grammar/lang/google-sheets.bbnf")
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
    }
}
