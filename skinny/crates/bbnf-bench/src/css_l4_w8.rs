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
use std::hint::black_box;
use std::time::{Duration, Instant};

pub const W8_WAVE_ID: &str = "SK-V14-W8";
pub const W8_SELECTED_CSS_ROWS: usize = 24;
pub const W8_PROFILE_ITERS: usize = 8;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum W8Disposition {
    Diagnostic,
    Rejected,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CssL4W8AttemptReport {
    pub wave_id: &'static str,
    pub selected_rows: usize,
    pub corpus_files: usize,
    pub corpus_bytes: usize,
    pub lightningcss_full_parse_passes: usize,
    pub cssparser_full_parse_passes: usize,
    pub track1_profile_runs: usize,
    pub track1_full_parse_runs: usize,
    pub track1_wrong_plane_outputs: usize,
    pub track1_errors: usize,
    pub profile_iters: usize,
    pub profiled_bytes: usize,
    pub track1_full_parse_mbps: f64,
    pub lightningcss_full_parse_mbps: f64,
    pub cssparser_full_parse_mbps: f64,
    pub track1_lightningcss_margin_mbps: f64,
    pub admitted_rows: usize,
    pub disposition: W8Disposition,
    pub disposition_reason: &'static str,
}

type Track1Parse = fn(&str) -> Result<String, String>;

#[derive(Clone, Copy)]
struct Track1Profile {
    row_id: &'static str,
    parse: Track1Parse,
}

const TRACK1_FULL_PARSE_OUTPUT_PLANE: &str = "css_l4_full_parse";
const TRACK1_FULL_PARSE_SCHEMA: &str = "css-l4-full-parse-v1";

const TRACK1_PROFILES: &[Track1Profile] = &[
    Track1Profile {
        row_id: "css_l4/declaration_values/direct_to_struct/main",
        parse: parse_declaration_values,
    },
    Track1Profile {
        row_id: "css_l4/declaration_values_extended/direct_to_struct/main",
        parse: parse_declaration_values_extended,
    },
    Track1Profile {
        row_id: "css_l4/stylesheet_and_selectors/direct_to_struct/main",
        parse: parse_stylesheet_selectors,
    },
    Track1Profile {
        row_id: "css_l4/visual_functions/direct_to_struct/main",
        parse: parse_visual_functions,
    },
    Track1Profile {
        row_id: "css_l4/at_rules_and_media/direct_to_struct/main",
        parse: parse_at_rules_and_media,
    },
    Track1Profile {
        row_id: "css_l4/vendor_and_custom_atrules/direct_to_struct/main",
        parse: parse_vendor_and_custom_atrules,
    },
    Track1Profile {
        row_id: "css_l4/nested_layout/direct_to_struct/main",
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
    let mut track1_full_parse_runs = 0usize;
    let mut track1_wrong_plane_outputs = 0usize;
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
                Ok(output) if generated_full_parse_marker(&output, profile) => {
                    track1_full_parse_runs += 1;
                }
                Ok(output) if generated_fact_stream_marker(&output) => {
                    track1_wrong_plane_outputs += 1;
                }
                Ok(_) | Err(_) => {
                    track1_errors += 1;
                }
            }
        }
    }

    let all_track1_runs_same_plane = track1_full_parse_runs == track1_profile_runs
        && track1_wrong_plane_outputs == 0
        && track1_errors == 0;
    let profile = measure_full_parse_profiles(&corpora)?;
    let track1_lightningcss_margin_mbps =
        profile.track1_full_parse_mbps - (profile.lightningcss_full_parse_mbps + 1.0);
    let track1_beats_lightningcss = track1_lightningcss_margin_mbps > 0.0;
    let admits = all_track1_runs_same_plane && track1_beats_lightningcss;
    let admitted_rows = 0;
    let disposition = if admits {
        W8Disposition::Diagnostic
    } else {
        W8Disposition::Rejected
    };
    let disposition_reason = if admits {
        "SK-V15-W1-diagnostic-broadcast-demotion"
    } else if !track1_beats_lightningcss {
        "track1_css_full_parse_below_lightningcss"
    } else {
        "track1_css_full_parse_plane_mismatch"
    };

    Ok(CssL4W8AttemptReport {
        wave_id: W8_WAVE_ID,
        selected_rows: W8_SELECTED_CSS_ROWS,
        corpus_files: corpora.len(),
        corpus_bytes,
        lightningcss_full_parse_passes,
        cssparser_full_parse_passes,
        track1_profile_runs,
        track1_full_parse_runs,
        track1_wrong_plane_outputs,
        track1_errors,
        profile_iters: W8_PROFILE_ITERS,
        profiled_bytes: profile.profiled_bytes,
        track1_full_parse_mbps: profile.track1_full_parse_mbps,
        lightningcss_full_parse_mbps: profile.lightningcss_full_parse_mbps,
        cssparser_full_parse_mbps: profile.cssparser_full_parse_mbps,
        track1_lightningcss_margin_mbps,
        admitted_rows,
        disposition,
        disposition_reason,
    })
}

fn generated_full_parse_marker(output: &str, profile: &Track1Profile) -> bool {
    output.lines().next() == Some(TRACK1_FULL_PARSE_SCHEMA)
        && output.lines().any(|line| {
            line.starts_with("row\tid=")
                && line.contains(profile.row_id)
                && line.ends_with(&format!("\tplane={TRACK1_FULL_PARSE_OUTPUT_PLANE}"))
        })
        && output.contains("\nfull_parse\tstatus=accepted")
        && !generated_fact_stream_marker(output)
}

fn generated_fact_stream_marker(output: &str) -> bool {
    output
        .lines()
        .any(|line| line.starts_with("row\tid=") && line.contains("_fact_stream"))
        || output.contains("\npolicy\tbackend_shape=admitted_fact_output")
}

struct W8ProfileMeasurement {
    profiled_bytes: usize,
    track1_full_parse_mbps: f64,
    lightningcss_full_parse_mbps: f64,
    cssparser_full_parse_mbps: f64,
}

fn measure_full_parse_profiles(
    corpora: &[crate::css_l4_corpus::CssL4Corpus],
) -> Result<W8ProfileMeasurement, String> {
    let mut sources = Vec::with_capacity(corpora.len());
    for corpus in corpora {
        sources.push(
            std::str::from_utf8(&corpus.bytes)
                .map_err(|error| format!("{} is not UTF-8: {error}", corpus.spec.id))?,
        );
    }

    let profiled_bytes = total_bytes(corpora) * TRACK1_PROFILES.len() * W8_PROFILE_ITERS;
    let track1_elapsed = time_loop(|| {
        for _ in 0..W8_PROFILE_ITERS {
            for profile in TRACK1_PROFILES {
                for source in &sources {
                    let output = (profile.parse)(source)?;
                    black_box(output.len());
                }
            }
        }
        Ok(())
    })?;
    let lightningcss_elapsed = time_loop(|| {
        for _ in 0..W8_PROFILE_ITERS {
            for _profile in TRACK1_PROFILES {
                for source in &sources {
                    let stylesheet =
                        StyleSheet::parse(source, ParserOptions::default()).map_err(|error| {
                            format!("lightningcss rejected production corpus: {error}")
                        })?;
                    black_box(stylesheet.rules.0.len());
                }
            }
        }
        Ok(())
    })?;
    let cssparser_elapsed = time_loop(|| {
        for _ in 0..W8_PROFILE_ITERS {
            for _profile in TRACK1_PROFILES {
                for source in &sources {
                    cssparser_full_parse(source)?;
                }
            }
        }
        Ok(())
    })?;

    Ok(W8ProfileMeasurement {
        profiled_bytes,
        track1_full_parse_mbps: throughput_mbps(profiled_bytes, track1_elapsed),
        lightningcss_full_parse_mbps: throughput_mbps(profiled_bytes, lightningcss_elapsed),
        cssparser_full_parse_mbps: throughput_mbps(profiled_bytes, cssparser_elapsed),
    })
}

fn time_loop(mut run: impl FnMut() -> Result<(), String>) -> Result<Duration, String> {
    let start = Instant::now();
    run()?;
    Ok(start.elapsed())
}

fn throughput_mbps(bytes: usize, elapsed: Duration) -> f64 {
    if elapsed.is_zero() {
        return f64::INFINITY;
    }
    ((bytes as f64) * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0)
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
    generated_css_l4_declaration_values::parser::parse_full(input)
        .map_err(|error| error.to_string())
}

fn parse_declaration_values_extended(input: &str) -> Result<String, String> {
    generated_css_l4_declaration_values_extended::parser::parse_full(input)
        .map_err(|error| error.to_string())
}

fn parse_stylesheet_selectors(input: &str) -> Result<String, String> {
    generated_css_l4_stylesheet_selectors::parser::parse_full(input)
        .map_err(|error| error.to_string())
}

fn parse_visual_functions(input: &str) -> Result<String, String> {
    generated_css_l4_visual_functions::parser::parse_full(input).map_err(|error| error.to_string())
}

fn parse_at_rules_and_media(input: &str) -> Result<String, String> {
    generated_css_l4_at_rules_and_media::parser::parse_full(input)
        .map_err(|error| error.to_string())
}

fn parse_vendor_and_custom_atrules(input: &str) -> Result<String, String> {
    generated_css_l4_vendor_and_custom_atrules::parser::parse_full(input)
        .map_err(|error| error.to_string())
}

fn parse_nested_layout(input: &str) -> Result<String, String> {
    generated_css_l4_nested_layout::parser::parse_full(input).map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn css_l4_w8_production_attempt_is_diagnostic_full_parse_track1() {
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
        assert_eq!(report.track1_full_parse_runs, report.track1_profile_runs);
        assert_eq!(report.track1_wrong_plane_outputs, 0);
        assert_eq!(report.track1_errors, 0);
        assert_eq!(report.profile_iters, W8_PROFILE_ITERS);
        assert_eq!(
            report.profiled_bytes,
            report.corpus_bytes * TRACK1_PROFILES.len() * W8_PROFILE_ITERS
        );
        assert!(
            report.track1_lightningcss_margin_mbps > 0.0,
            "track1={} lightningcss={} margin={} cssparser={}",
            report.track1_full_parse_mbps,
            report.lightningcss_full_parse_mbps,
            report.track1_lightningcss_margin_mbps,
            report.cssparser_full_parse_mbps,
        );
        eprintln!(
            "W8_CSS_FULL_PARSE profile_iters={} profiled_bytes={} track1_mbps={:.3} lightningcss_mbps={:.3} cssparser_mbps={:.3} margin_mbps={:.3}",
            report.profile_iters,
            report.profiled_bytes,
            report.track1_full_parse_mbps,
            report.lightningcss_full_parse_mbps,
            report.cssparser_full_parse_mbps,
            report.track1_lightningcss_margin_mbps,
        );
        assert_eq!(report.admitted_rows, 0);
        assert_eq!(report.disposition, W8Disposition::Diagnostic);
        assert_eq!(
            report.disposition_reason,
            "SK-V15-W1-diagnostic-broadcast-demotion"
        );
    }
}
