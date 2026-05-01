use gorgeous::PrinterConfig;
use gorgeous::bnf::prettify_bnf;

#[test]
fn test_prettify_simple_bnf() {
    let config = PrinterConfig::default();
    let input = "<expr> ::= <term>\n";
    let result = prettify_bnf(input, &config);
    assert!(result.is_some(), "should parse simple BNF rule");
    let output = result.unwrap();
    assert!(
        output.contains("expr"),
        "should contain rule name: got '{}'",
        output
    );
    assert!(
        output.contains("::="),
        "should contain definition: got '{}'",
        output
    );
    assert!(
        output.contains("term"),
        "should contain rhs: got '{}'",
        output
    );
}

#[test]
fn test_prettify_bnf_alternation() {
    let config = PrinterConfig::default();
    let input = "<expr> ::= <term> | <factor>\n";
    let result = prettify_bnf(input, &config);
    assert!(result.is_some(), "should parse BNF alternation");
    let output = result.unwrap();
    assert!(output.contains("term"), "should contain first alt");
    assert!(output.contains("factor"), "should contain second alt");
}

#[test]
fn test_prettify_bnf_multi_rule() {
    let config = PrinterConfig::default();
    let input = "<expr> ::= <term>\n<term> ::= <factor>\n";
    let result = prettify_bnf(input, &config);
    assert!(result.is_some(), "should parse multiple BNF rules");
    let output = result.unwrap();
    assert!(output.contains("expr"), "should contain first rule");
    assert!(output.contains("factor"), "should contain second rule rhs");
}

#[test]
fn test_prettify_bnf_terminal() {
    let config = PrinterConfig::default();
    let input = "<digit> ::= \"0\" | \"1\" | \"2\"\n";
    let result = prettify_bnf(input, &config);
    assert!(result.is_some(), "should parse BNF with terminals");
    let output = result.unwrap();
    assert!(output.contains("digit"), "should contain rule name");
}

#[test]
fn test_prettify_bnf_idempotent() {
    let config = PrinterConfig::default();
    let input = "<expr> ::= <term> | <factor>\n";
    let first = prettify_bnf(input, &config).unwrap();
    let second = prettify_bnf(&first, &config).unwrap();
    assert_eq!(first, second, "prettify should be idempotent");
}
