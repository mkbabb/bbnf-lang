use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Parser)]
#[parser(
    path = "grammar/ebnf/ebnf.bbnf",
    prettify
)]
pub struct EbnfParser;

/// Pretty-print an EBNF grammar string.
pub fn prettify_ebnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = EbnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prettify_simple_ebnf() {
        let config = PrinterConfig::default();
        let input = r#"letter = "A" | "B" | "C" ;"#;
        let result = prettify_ebnf(input, &config);
        assert!(result.is_some(), "should parse simple EBNF rule");
        let output = result.unwrap();
        assert!(output.contains("letter"), "should contain rule name: got '{}'", output);
        assert!(output.contains("="), "should contain assignment: got '{}'", output);
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
}
