use bbnf_analysis::analysis::LineIndex;
use bbnf_analysis::state::diagnostics::analyze;

#[test]
fn test_analyze_parse_error() {
    let text = "value = number";
    let result = bbnf::grammar::parse_with_state(text);
    eprintln!("Result is_some: {}", result.is_some());
    if let Some(ref pg) = result {
        eprintln!("AST len: {}", pg.rules.len());
    }
    let line_index = LineIndex::new(text);
    let info = analyze(text, &line_index);
    eprintln!("Diagnostics: {:?}", info.diagnostics);
    eprintln!(
        "Rules: {:?}",
        info.rules.iter().map(|r| &r.name).collect::<Vec<_>>()
    );
    // Should produce a diagnostic
    assert!(
        !info.diagnostics.is_empty(),
        "Expected diagnostics for missing semicolon"
    );
}

#[test]
fn test_analyze_valid() {
    let text = "value = /[0-9]+/;";
    let line_index = LineIndex::new(text);
    let info = analyze(text, &line_index);
    eprintln!("Diagnostics: {:?}", info.diagnostics);
    eprintln!(
        "Rules: {:?}",
        info.rules.iter().map(|r| &r.name).collect::<Vec<_>>()
    );
    assert!(
        info.diagnostics.is_empty(),
        "Expected no diagnostics for valid grammar"
    );
    assert_eq!(info.rules.len(), 1);
}

#[test]
fn test_analyze_json_grammar() {
    let grammar = r#"null = "null";
bool = "true" | "false";
number = /\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?/;
char = /[^"\\]/ | /\\["\\/bfnrt]/ | /\\u[0-9a-fA-F]{4}/;
string = "\"" , { char } , "\"";
array = "[" , [ value , { "," , value } ] , "]";
pair = string , ":" , value;
object = "{" , [ pair , { "," , pair } ] , "}";
value = string | number | object | array | bool | null;"#;
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics: {:?}", info.diagnostics);
    eprintln!(
        "Rules: {:?}",
        info.rules.iter().map(|r| &r.name).collect::<Vec<_>>()
    );
}

#[test]
fn test_analyze_recover_directive() {
    let grammar = r#"@recover stmt /[^;]*;/ ;

stmt = /[a-z]+/ , "=" , /[a-z]+/ , ";" ;
program = stmt * ;"#;
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics: {:?}", info.diagnostics);
    eprintln!("Recovers: {:?}", info.recovers);
    assert_eq!(info.recovers.len(), 1, "Should have 1 @recover directive");
    assert_eq!(info.recovers[0].rule_name, "stmt");
    let undefined_warnings: Vec<_> = info
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("undefined") || d.message.contains("Undefined"))
        .collect();
    assert!(
        undefined_warnings.is_empty(),
        "Should not warn about @recover target: {:?}",
        undefined_warnings
    );
}

#[test]
fn test_analyze_recover_undefined_target() {
    let grammar = r#"@recover nonexistent /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;"#;
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics: {:?}", info.diagnostics);
    let recover_warnings: Vec<_> = info
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("@recover"))
        .collect();
    assert!(
        !recover_warnings.is_empty(),
        "Should warn about @recover targeting undefined rule"
    );
}

#[test]
fn test_analyze_simple_json_grammar() {
    let grammar = r#"null = "null";
bool = "true" | "false";
number = /[0-9]+/;
string = /[a-z]+/;
value = string | number | bool | null;"#;
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics: {:?}", info.diagnostics);
    eprintln!(
        "Rules: {:?}",
        info.rules.iter().map(|r| &r.name).collect::<Vec<_>>()
    );
    assert_eq!(info.rules.len(), 5, "Should have 5 rules");
}

#[test]
fn test_analyze_undefined_rule() {
    let grammar = "value = number | missing_rule;\nnumber = /[0-9]+/;";
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    let undefined = info.diagnostics.iter().any(|d| d.message.contains("Undefined rule"));
    assert!(undefined, "Expected undefined rule diagnostic for missing_rule");
}

#[test]
fn test_rules_populated() {
    let grammar = "number = /[0-9]+/;\nstring = /\"[^\"]*\"/;\nvalue = number ;";
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    assert_eq!(info.rules.len(), 3, "Expected 3 rules");
    assert!(info.rules.iter().any(|r| r.name == "number"), "Expected 'number' rule");
    assert!(info.rules.iter().any(|r| r.name == "string"), "Expected 'string' rule");
    assert!(info.rules.iter().any(|r| r.name == "value"), "Expected 'value' rule");
}

#[test]
fn test_first_set_conflict_detection() {
    let grammar = "value = integer | decimal;\ninteger = /[0-9]+/;\ndecimal = /[0-9]+/, \".\", /[0-9]+/;";
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics count: {}", info.diagnostics.len());
    for d in &info.diagnostics {
        eprintln!("  severity={:?}, message={}", d.severity, d.message);
    }
    eprintln!("First set labels: {:?}", info.first_set_labels);
    eprintln!("IR meta keys: {:?}", info.ir_meta.keys().collect::<Vec<_>>());
    eprintln!("Rules: {:?}", info.rules.iter().map(|r| &r.name).collect::<Vec<_>>());
    // Verify FIRST set conflict detection
    let has_conflict = info.diagnostics.iter().any(|d| d.message.contains("ambiguous FIRST sets") || d.message.contains("overlap"));
    eprintln!("Has FIRST conflict: {}", has_conflict);
}

#[test]
fn test_cycle_detection() {
    let grammar = "expr = expr, \"+\", term | term;\nterm = /[0-9]+/;";
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics count: {}", info.diagnostics.len());
    for d in &info.diagnostics {
        eprintln!("  severity={:?}, message={}", d.severity, d.message);
    }
    eprintln!("Cyclic paths: {:?}", info.cyclic_rule_paths);
}

#[test]
fn test_alias_detection() {
    let grammar = "value = number;\nnumber = /[0-9]+/;\nint = number;";
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Diagnostics count: {}", info.diagnostics.len());
    for d in &info.diagnostics {
        eprintln!("  severity={:?}, message={}", d.severity, d.message);
    }
}

#[test]
fn test_nullable_detection() {
    let grammar = "value = [\"hello\"];";
    let line_index = LineIndex::new(grammar);
    let info = analyze(grammar, &line_index);
    eprintln!("Nullable rules: {:?}", info.nullable_rules);
    eprintln!("First set labels: {:?}", info.first_set_labels);
    eprintln!("IR meta keys: {:?}", info.ir_meta.keys().collect::<Vec<_>>());
    if let Some(meta) = info.ir_meta.get("value") {
        eprintln!("value meta: {:?}", meta);
    }

    // Also check the IR directly
    let pg = bbnf::grammar::parse(grammar).expect("parse");
    let directives = bbnf::lower::DirectiveSet::empty();
    let options = bbnf::pipeline::PipelineOptions { structural: true, ..Default::default() };
    match bbnf::pipeline::compile_ast(pg.rules, &directives, &options) {
        Ok(ir) => {
            for rule in &ir.rules {
                let name = ir.get_string(rule.name);
                eprintln!("IR rule '{}': nullable={}, body={:?}", name, rule.meta.nullable, rule.body);
            }
        },
        Err(e) => eprintln!("compile_ast FAILED: {}", e),
    }
}
