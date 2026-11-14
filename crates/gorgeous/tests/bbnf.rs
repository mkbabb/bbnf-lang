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
fn test_prettify_parse_that_regex() {
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

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-13e S3CPP-B): a line comment inside a
/// rule is admitted where a block comment is (`factor`'s comment slots), the
/// recognizer and the prettifier both read it, the comment is kept, and
/// formatting is idempotent.
#[test]
fn bbnf_line_comment_inside_a_rule_is_read_and_kept() {
    let config = PrinterConfig::default();
    let input = "value = \"true\" // the first alternative\n      | \"false\" ;\n";
    assert!(
        gorgeous::bbnf::BbnfParser::parse(input).is_ok(),
        "the recognizer admits a line comment inside a rule"
    );
    let first = prettify_bbnf(input, &config).expect("a line comment inside a rule formats");
    assert!(first.contains("// the first alternative"), "comment kept: {first:?}");
    assert!(first.contains("\"false\""), "the second alternative kept: {first:?}");
    let second = prettify_bbnf(&first, &config).expect("the formatted output formats");
    assert_eq!(first, second, "prettify is idempotent");
}

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-13e S3CPP-B): input the grammar does not
/// consume is refused, not formatted as its parsed prefix.
#[test]
fn bbnf_prettify_refuses_unconsumed_input() {
    let config = PrinterConfig::default();
    assert_eq!(prettify_bbnf("foo = \"bar\" ;\n) \"rest\"\n", &config), None);
}
