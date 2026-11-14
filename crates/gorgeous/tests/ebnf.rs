use gorgeous::PrinterConfig;
use gorgeous::ebnf::prettify_ebnf;

#[test]
fn test_prettify_simple_ebnf() {
    let config = PrinterConfig::default();
    let input = r#"letter = "A" | "B" | "C" ;"#;
    let result = prettify_ebnf(input, &config);
    assert!(result.is_some(), "should parse simple EBNF rule");
    let output = result.unwrap();
    assert!(
        output.contains("letter"),
        "should contain rule name: got '{}'",
        output
    );
    assert!(
        output.contains("="),
        "should contain assignment: got '{}'",
        output
    );
}

#[test]
fn test_prettify_repetition() {
    let config = PrinterConfig::default();
    let input = r#"identifier = letter , { letter | digit | "_" } ;"#;
    let result = prettify_ebnf(input, &config);
    assert!(result.is_some(), "should parse repetition rule");
}

#[test]
fn test_prettify_multi_rule() {
    let config = PrinterConfig::default();
    let input = r#"digit = "0" | "1" | "2" ;
number = digit , { digit } ;"#;
    let result = prettify_ebnf(input, &config);
    assert!(result.is_some(), "should parse multiple rules");
    let output = result.unwrap();
    assert!(output.contains("digit"), "should contain first rule");
    assert!(output.contains("number"), "should contain second rule");
}

#[test]
fn test_prettify_idempotent() {
    let config = PrinterConfig::default();
    let input = r#"letter = "A" | "B" | "C" ;"#;
    let first = prettify_ebnf(input, &config).unwrap();
    let second = prettify_ebnf(&first, &config).unwrap();
    assert_eq!(first, second, "prettify should be idempotent");
}

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-12c PP2-G3, QX-1 clause 4): the ebnf
/// prettifier keeps every rule and every terminal (`r = "a" ;` formatted to
/// `r =`), and formatting is idempotent on a multi-rule grammar.
#[test]
fn ebnf_prettify_keeps_every_rule_and_terminal() {
    let config = PrinterConfig::default();
    let input = "r = \"a\" ;\ndigit = \"0\" | \"1\" ;\nnumber = digit , { digit } ;\n";
    let once = prettify_ebnf(input, &config).expect("the input formats");
    for t in ["r", "\"a\"", "digit", "\"0\"", "\"1\"", "number", "{", "}"] {
        assert!(once.contains(t), "{t:?} lost: got {once:?}");
    }
    assert_eq!(once.matches(';').count(), 3, "a terminator lost: got {once:?}");
    let twice = prettify_ebnf(&once, &config).expect("its own output formats");
    assert_eq!(once, twice, "fmt . fmt != fmt");
}
