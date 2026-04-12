use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Debug, Parser)]
#[parser(
    path = "grammar/google-sheets/google-sheets.bbnf",
    prettify
)]
pub struct GoogleSheetsParser;

/// Parse a Google Sheets formula. Returns true if the input is valid.
pub fn parse_formula(input: &str) -> Option<()> {
    GoogleSheetsParser::parse(input).ok().map(|_| ())
}

/// Parse and pretty-print a Google Sheets formula.
pub fn prettify_formula(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = GoogleSheetsParser::formula_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let input = "=LET(a, 1, b)";
        let parsed = GoogleSheetsParser::parse(input).expect("parse failed");
        let ast_debug = format!("{:?}", parsed);
        assert!(
            ast_debug.contains("let_call"),
            "=LET(a,1,b) should parse as let_call, not func_call"
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
}
