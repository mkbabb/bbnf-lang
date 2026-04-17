use gorgeous::PrinterConfig;
use gorgeous::google_sheets::{
    GoogleSheetsParser, GoogleSheetsParserNodeView, GoogleSheetsParserRuleKind,
    parse_formula, prettify_formula,
};

#[test]
fn test_parse_simple() {
    assert!(parse_formula("=SUM(A1:A10)").is_some());
}

#[test]
fn test_parse_if() {
    assert!(parse_formula("=IF(A1>0, 1, 0)").is_some());
}

#[test]
fn test_parse_let() {
    let input = r#"=LET(data, A1:Z100, filtered, FILTER(data, INDEX(data,,1)>0), count, ROWS(filtered), IF(count>0, MAKEARRAY(count, 3, LAMBDA(r, c, INDEX(filtered, r, c))), "No data"))"#;
    assert!(parse_formula(input).is_some(), "LET formula should parse");
}

#[test]
fn test_parse_pathological() {
    let input = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))"#;
    assert!(parse_formula(input).is_some(), "pathological should parse");
}

#[test]
fn test_let_parses_as_let_call() {
    // AW-III.W6.5: the test's original `format!("{:?}", parsed)`
    // shape carried rule names in the pre-AC.2 AST-based parser;
    // post-AC.2 `Parsed<Grammar>` wraps a `Tape` whose Debug output
    // contains only numeric variant_idx values. W6.5 un-ignores the
    // test and rewrites the assertion against the view-layer
    // introspection substrate: walk the tape via `NodeView` /
    // `rule_kind()` and verify at least one record surfaces with
    // `GoogleSheetsParserRuleKind::let_call`. If the dispatch
    // regressed to `func_call`, no `let_call` record ever appears.
    let input = "=LET(a, 1, b)";
    let parsed = GoogleSheetsParser::parse(input).expect("parse failed");
    let node =
        GoogleSheetsParserNodeView::new(parsed.tape(), input, parsed.root_offset());

    // Walk the tape and collect every distinct rule_kind value.
    let mut found_let_call = false;
    let mut found_func_call = false;
    fn walk<'p>(
        v: GoogleSheetsParserNodeView<'p>,
        let_call: &mut bool,
        func_call: &mut bool,
    ) {
        match v.rule_kind() {
            GoogleSheetsParserRuleKind::let_call => *let_call = true,
            GoogleSheetsParserRuleKind::func_call => *func_call = true,
            _ => {}
        }
        for child in v.children() {
            walk(child, let_call, func_call);
        }
    }
    walk(node, &mut found_let_call, &mut found_func_call);

    assert!(
        found_let_call,
        "=LET(a,1,b) must dispatch to let_call; tape walk did not \
         surface a let_call record (func_call seen: {})",
        found_func_call,
    );
}

#[test]
fn test_let_binding_pair_formatting() {
    let config = PrinterConfig::new(80, 2);
    let tests_short = [
        ("=LET(a, 1, a)", "short"),
        ("=LET(scale, DURATION, scale)", "medium"),
        ("=LET(x, SUM(A1:A10), x)", "func-value"),
    ];
    for (input, label) in tests_short {
        let formatted = prettify_formula(input, &config).unwrap();
        eprintln!("{label}: {input:40} → {formatted:?}");
    }
}

#[test]
fn test_trailing_space_formatting() {
    let config = PrinterConfig::new(80, 2);
    let without_space = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))"#;
    let with_space = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results") )"#;
    // Both inputs parse and format successfully.
    let fmt_without = prettify_formula(without_space, &config).unwrap();
    let fmt_with = prettify_formula(with_space, &config).unwrap();
    // The trailing space between `) )` is preserved in formatting (faithful span
    // reproduction). Both produce valid output; they differ only in the final space.
    assert!(fmt_without.ends_with("))"), "without-space should end with ))");
    assert!(fmt_with.ends_with(") )"), "with-space should end with ) )");
}
