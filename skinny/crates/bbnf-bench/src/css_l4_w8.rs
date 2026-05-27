use crate::css_l4_corpus::{load_all, total_bytes, CSS_L4_SK_V14_MIN_BYTES};
use cssparser::{
    AtRuleParser, CowRcStr, DeclarationParser, Parser, ParserInput, ParserState,
    QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, StyleSheetParser, Token,
};
use lightningcss::stylesheet::{ParserOptions, StyleSheet};
use runtime::{
    generated_css_l4_at_rules_and_media, generated_css_l4_declaration_values,
    generated_css_l4_declaration_values_extended, generated_css_l4_nested_layout,
    generated_css_l4_stylesheet_selectors, generated_css_l4_vendor_and_custom_atrules,
    generated_css_l4_visual_functions,
};

pub const W8_WAVE_ID: &str = "SK-V14-W8";
pub const W8_SELECTED_CSS_ROWS: usize = 24;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum W8Disposition {
    Rejected,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CssL4W8AttemptReport {
    pub wave_id: &'static str,
    pub selected_rows: usize,
    pub corpus_files: usize,
    pub corpus_bytes: usize,
    pub lightningcss_full_parse_passes: usize,
    pub cssparser_full_parse_passes: usize,
    pub track1_profile_runs: usize,
    pub track1_fact_stream_runs: usize,
    pub track1_errors: usize,
    pub admitted_rows: usize,
    pub disposition: W8Disposition,
    pub block_reason: &'static str,
}

type Track1Parse = fn(&str) -> Result<String, String>;

#[derive(Clone, Copy)]
struct Track1Profile {
    output_plane: &'static str,
    parse: Track1Parse,
}

const TRACK1_PROFILES: &[Track1Profile] = &[
    Track1Profile {
        output_plane: "css_l4_declaration_value_fact_stream",
        parse: parse_declaration_values,
    },
    Track1Profile {
        output_plane: "css_l4_declaration_value_extended_fact_stream",
        parse: parse_declaration_values_extended,
    },
    Track1Profile {
        output_plane: "css_l4_stylesheet_selector_fact_stream",
        parse: parse_stylesheet_selectors,
    },
    Track1Profile {
        output_plane: "css_l4_visual_function_fact_stream",
        parse: parse_visual_functions,
    },
    Track1Profile {
        output_plane: "css_l4_at_rules_media_fact_stream",
        parse: parse_at_rules_and_media,
    },
    Track1Profile {
        output_plane: "css_l4_vendor_custom_fact_stream",
        parse: parse_vendor_and_custom_atrules,
    },
    Track1Profile {
        output_plane: "css_l4_nested_layout_fact_stream",
        parse: parse_nested_layout,
    },
];

pub fn run_production_attempt() -> Result<CssL4W8AttemptReport, String> {
    let corpora =
        load_all().map_err(|error| format!("failed to load CSS L4 W8 corpora: {error}"))?;
    let corpus_bytes = total_bytes(&corpora);
    if corpus_bytes < CSS_L4_SK_V14_MIN_BYTES {
        return Err(format!(
            "CSS L4 W8 corpus is too small: {corpus_bytes} < {CSS_L4_SK_V14_MIN_BYTES}"
        ));
    }

    let mut lightningcss_full_parse_passes = 0usize;
    let mut cssparser_full_parse_passes = 0usize;
    let mut track1_profile_runs = 0usize;
    let mut track1_fact_stream_runs = 0usize;
    let mut track1_errors = 0usize;

    for corpus in &corpora {
        let source = std::str::from_utf8(&corpus.bytes)
            .map_err(|error| format!("{} is not UTF-8: {error}", corpus.spec.id))?;

        StyleSheet::parse(source, ParserOptions::default())
            .map_err(|error| format!("lightningcss rejected {}: {error}", corpus.spec.id))?;
        lightningcss_full_parse_passes += 1;

        cssparser_full_parse(source)
            .map_err(|error| format!("cssparser rejected {}: {error}", corpus.spec.id))?;
        cssparser_full_parse_passes += 1;

        for profile in TRACK1_PROFILES {
            track1_profile_runs += 1;
            match (profile.parse)(source) {
                Ok(output) if generated_fact_stream_marker(&output, profile) => {
                    track1_fact_stream_runs += 1;
                }
                Ok(_) | Err(_) => {
                    track1_errors += 1;
                }
            }
        }
    }

    Ok(CssL4W8AttemptReport {
        wave_id: W8_WAVE_ID,
        selected_rows: W8_SELECTED_CSS_ROWS,
        corpus_files: corpora.len(),
        corpus_bytes,
        lightningcss_full_parse_passes,
        cssparser_full_parse_passes,
        track1_profile_runs,
        track1_fact_stream_runs,
        track1_errors,
        admitted_rows: 0,
        disposition: W8Disposition::Rejected,
        block_reason: "post_w7_track1_is_generated_fact_stream_not_css_full_parse",
    })
}

fn generated_fact_stream_marker(output: &str, profile: &Track1Profile) -> bool {
    output.lines().any(|line| {
        line.starts_with("row\tid=") && line.ends_with(&format!("\tplane={}", profile.output_plane))
    }) && output.contains("\npolicy\tbackend_shape=admitted_fact_output")
        && output.contains("\nfrontend\tsource_hash=")
        && output.contains("_fact_stream")
}

fn cssparser_full_parse(source: &str) -> Result<(), String> {
    let mut parser_input = ParserInput::new(source);
    let mut parser = Parser::new(&mut parser_input);
    let mut probe = CssparserFullParseProbe;
    for item in StyleSheetParser::new(&mut parser, &mut probe) {
        item.map_err(|(error, fragment)| {
            format!("cssparser full-parse error at `{fragment}`: {error:?}")
        })?;
    }
    Ok(())
}

struct CssparserFullParseProbe;

impl CssparserFullParseProbe {
    fn parse_nested_rules<'i, 't>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        for item in RuleBodyParser::new(input, self) {
            item.map_err(|(error, _fragment)| error)?;
        }
        Ok(())
    }

    fn consume_component_values<'i, 't>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        loop {
            let token = match input.next_including_whitespace_and_comments().cloned() {
                Ok(token) => token,
                Err(_) => break,
            };
            match token {
                Token::Function(_)
                | Token::ParenthesisBlock
                | Token::SquareBracketBlock
                | Token::CurlyBracketBlock => {
                    input.parse_nested_block(|input| self.consume_component_values(input))?;
                }
                Token::BadUrl(_) | Token::BadString(_) => {
                    return Err(input.new_unexpected_token_error(token));
                }
                _ => {}
            }
        }
        Ok(())
    }
}

impl<'i> DeclarationParser<'i> for CssparserFullParseProbe {
    type Declaration = ();
    type Error = String;

    fn parse_value<'t>(
        &mut self,
        _name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.consume_component_values(input)
    }
}

impl<'i> AtRuleParser<'i> for CssparserFullParseProbe {
    type Prelude = ();
    type AtRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        _name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.consume_component_values(input)
    }

    fn rule_without_block(&mut self, _prelude: (), _start: &ParserState) -> Result<(), ()> {
        Ok(())
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.parse_nested_rules(input)
    }
}

impl<'i> QualifiedRuleParser<'i> for CssparserFullParseProbe {
    type Prelude = ();
    type QualifiedRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.consume_component_values(input)
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.parse_nested_rules(input)
    }
}

impl<'i> RuleBodyItemParser<'i, (), String> for CssparserFullParseProbe {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        true
    }
}

fn parse_declaration_values(input: &str) -> Result<String, String> {
    generated_css_l4_declaration_values::parser::parse(input).map_err(|error| error.to_string())
}

fn parse_declaration_values_extended(input: &str) -> Result<String, String> {
    generated_css_l4_declaration_values_extended::parser::parse(input)
        .map_err(|error| error.to_string())
}

fn parse_stylesheet_selectors(input: &str) -> Result<String, String> {
    generated_css_l4_stylesheet_selectors::parser::parse(input).map_err(|error| error.to_string())
}

fn parse_visual_functions(input: &str) -> Result<String, String> {
    generated_css_l4_visual_functions::parser::parse(input).map_err(|error| error.to_string())
}

fn parse_at_rules_and_media(input: &str) -> Result<String, String> {
    generated_css_l4_at_rules_and_media::parser::parse(input).map_err(|error| error.to_string())
}

fn parse_vendor_and_custom_atrules(input: &str) -> Result<String, String> {
    generated_css_l4_vendor_and_custom_atrules::parser::parse(input)
        .map_err(|error| error.to_string())
}

fn parse_nested_layout(input: &str) -> Result<String, String> {
    generated_css_l4_nested_layout::parser::parse(input).map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn css_l4_w8_production_attempt_rejects_fact_stream_track1() {
        let report = run_production_attempt().expect("run W8 CSS production attempt");

        assert_eq!(report.wave_id, W8_WAVE_ID);
        assert_eq!(report.selected_rows, W8_SELECTED_CSS_ROWS);
        assert!(report.corpus_bytes >= CSS_L4_SK_V14_MIN_BYTES);
        assert_eq!(report.corpus_files, 4);
        assert_eq!(report.lightningcss_full_parse_passes, report.corpus_files);
        assert_eq!(report.cssparser_full_parse_passes, report.corpus_files);
        assert_eq!(
            report.track1_profile_runs,
            report.corpus_files * TRACK1_PROFILES.len()
        );
        assert_eq!(report.track1_fact_stream_runs, report.track1_profile_runs);
        assert_eq!(report.track1_errors, 0);
        assert_eq!(report.admitted_rows, 0);
        assert_eq!(report.disposition, W8Disposition::Rejected);
        assert_eq!(
            report.block_reason,
            "post_w7_track1_is_generated_fact_stream_not_css_full_parse"
        );
    }
}
