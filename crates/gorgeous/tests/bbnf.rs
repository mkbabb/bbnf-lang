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

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-14a S3CPP-L): a top-level line comment
/// prints as it did before the layout rule: the grammar's block layout places
/// its line, and no blank line is added after it. A second pass is a fixed point.
#[test]
fn bbnf_top_level_line_comment_output_is_unchanged() {
    let config = PrinterConfig::default();
    let input = "// A grammar with comments between rules.\nfoo = \"bar\" ;\n\n// The second rule.\nbaz = \"qux\" | \"quux\" ;\n";
    let first = prettify_bbnf(input, &config).expect("top-level comments format");
    assert_eq!(
        first,
        "// A grammar with comments between rules.\nfoo=  \"bar\" ;\n// The second rule.\nbaz=  \"qux\" | \"quux\" ;"
    );
    assert_eq!(prettify_bbnf(&first, &config).as_deref(), Some(first.as_str()));
}

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-14a S3CPP-L): a line comment that
/// begins its line inside a rule (the second of two) is still followed by a
/// line: the token after it is kept, and a second pass is a fixed point.
#[test]
fn bbnf_line_comment_beginning_a_line_inside_a_rule_keeps_the_next_token() {
    let config = PrinterConfig::default();
    let input = "value = \"a\" // one\n      // two\n      \"b\" ;\n";
    let first = prettify_bbnf(input, &config).expect("two line comments inside a rule format");
    assert!(first.contains("// two"), "comment kept: {first:?}");
    let second = prettify_bbnf(&first, &config).expect("the formatted output formats");
    assert_eq!(first, second, "prettify is idempotent");
    assert!(
        second.lines().any(|line| line.trim_start().starts_with("\"b\"")),
        "\"b\" begins a line, not read into a comment: {second:?}"
    );
}

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-14b S3CPP-L2): three line comments in a
/// row inside a rule; each is followed by a line, every comment and the token
/// after them are kept, and a second pass is a fixed point.
#[test]
fn bbnf_three_line_comments_in_a_row_inside_a_rule_keep_the_next_token() {
    let config = PrinterConfig::default();
    let input = "value = \"a\" // one\n      // two\n      // three\n      \"b\" ;\n";
    let first = prettify_bbnf(input, &config).expect("three line comments inside a rule format");
    for comment in ["// one", "// two", "// three"] {
        assert!(first.contains(comment), "{comment} kept: {first:?}");
    }
    let second = prettify_bbnf(&first, &config).expect("the formatted output formats");
    assert_eq!(first, second, "prettify is idempotent");
    assert!(
        second.lines().any(|line| line.trim_start().starts_with("\"b\"")),
        "\"b\" begins a line, not read into a comment: {second:?}"
    );
}

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-14b S3CPP-L2): a line comment as the
/// last thing before the rule's terminator; the `;` is printed on a line of
/// its own, not read into the comment, and a second pass is a fixed point.
#[test]
fn bbnf_line_comment_before_the_rule_terminator_keeps_the_terminator() {
    let config = PrinterConfig::default();
    let input = "value = \"a\" | \"b\" // last\n      ;\nnext = \"c\" ;\n";
    let first = prettify_bbnf(input, &config).expect("a line comment before `;` formats");
    assert!(first.contains("// last"), "comment kept: {first:?}");
    assert!(first.contains("next"), "the following rule kept: {first:?}");
    let second = prettify_bbnf(&first, &config).expect("the formatted output formats");
    assert_eq!(first, second, "prettify is idempotent");
    assert!(
        second
            .lines()
            .any(|line| line.trim_start().starts_with(';')),
        "`;` begins a line, not read into the comment: {second:?}"
    );
}

/// sk-v27 LW3.S3c-PP (RULINGS 2026-11-14c S3CPP-L3): five comments in a row
/// inside a rule, block and line comments mixed; `factor`'s comment slots read
/// `( big_comment | comment ) *`, so every one is read and kept, the next
/// token is not read into a line comment, and a second pass is a fixed point.
#[test]
fn bbnf_five_mixed_comments_in_a_row_inside_a_rule_keep_the_next_token() {
    let config = PrinterConfig::default();
    let input = "value = \"a\" /* one */ // two\n      /* three */\n      // four\n      /* five */ \"b\" ;\n";
    let first = prettify_bbnf(input, &config).expect("five mixed comments inside a rule format");
    for comment in ["/* one */", "// two", "/* three */", "// four", "/* five */"] {
        assert!(first.contains(comment), "{comment} kept: {first:?}");
    }
    let second = prettify_bbnf(&first, &config).expect("the formatted output formats");
    assert_eq!(first, second, "prettify is idempotent");
    assert!(
        second.contains("\"b\"") && !second.lines().any(|line| {
            line.find("//").is_some_and(|at| line[at..].contains("\"b\""))
        }),
        "\"b\" kept, not read into a line comment: {second:?}"
    );
}
