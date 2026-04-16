use gorgeous::PrinterConfig;
use gorgeous::bbnf::prettify_bbnf;

#[test]
fn test_prettify_simple_rule() {
    let config = PrinterConfig::default();
    let input = "foo = \"bar\" ;\n";
    let result = prettify_bbnf(input, &config);
    assert!(result.is_some(), "should parse simple BBNF rule");
    let output = result.unwrap();
    assert!(output.contains("foo"), "should contain rule name");
    assert!(output.contains("bar"), "should contain rhs");
}

#[test]
fn test_prettify_bbnf_alternation() {
    let config = PrinterConfig::default();
    let input = "value = \"true\" | \"false\" ;\n";
    let result = prettify_bbnf(input, &config);
    assert!(result.is_some(), "should parse BBNF alternation");
    let output = result.unwrap();
    assert!(output.contains("true"), "should contain first alt");
    assert!(output.contains("false"), "should contain second alt");
}

#[test]
fn test_prettify_bbnf_regex() {
    let config = PrinterConfig::default();
    let input = "number = /[0-9]+/ ;\n";
    let result = prettify_bbnf(input, &config);
    assert!(result.is_some(), "should parse BBNF with regex");
    let output = result.unwrap();
    assert!(output.contains("number"), "should contain rule name");
}

#[test]
fn test_prettify_bbnf_multi_rule() {
    let config = PrinterConfig::default();
    let input = "foo = \"bar\" ;\nbaz = \"qux\" ;\n";
    let result = prettify_bbnf(input, &config);
    assert!(result.is_some(), "should parse multiple BBNF rules");
    let output = result.unwrap();
    assert!(output.contains("foo"), "should contain first rule");
    assert!(output.contains("qux"), "should contain second rule rhs");
}

#[test]
fn test_prettify_bbnf_idempotent() {
    let config = PrinterConfig::default();
    let input = "foo = \"bar\" | \"baz\" ;\n";
    let first = prettify_bbnf(input, &config).unwrap();
    let second = prettify_bbnf(&first, &config).unwrap();
    assert_eq!(first, second, "prettify should be idempotent");
}
