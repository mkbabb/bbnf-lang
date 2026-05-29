use bbnf::grammar::generated::css_l4::CssL4Parser;
use bbnf::runtime::css_l4::{
    visit_document, CssColor, CssFunction, CssRule, CssTypedValue, CssVisitor, Declaration,
    GenericAtRule, KeyframeBlock, KeyframesRule, MediaRule, Selector, StyleRule, StyleSheet,
};
use cssparser::{
    AtRuleParser, CowRcStr, DeclarationParser, Parser, ParserInput, ParserState,
    QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, StyleSheetParser, Token,
};
use serde::{Deserialize, Serialize};
use std::hint::black_box;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

const W6_REPORT_SCHEMA: &str = "sk-v15-css-l4-typed-retime-v1";
const W6_WAVE_ID: &str = "SK-V15-W6";
const W6_RUN_ID: &str = "sk-v15-w6:css-typed-cssparser-same-workload-cold";
const W6_SAMPLE_COUNT: usize = 1;
const W6_VALUE_PLANE: &str = "css_l4_typed_document";
const W6_COMPARATOR_WORKLOAD: &str = "cssparser_stylesheet_rule_body_typed_summary";
const W6_SOTA_EPSILON_MBPS: f64 = 1.0;
const CSS_L4_SK_V14_MIN_BYTES: usize = 800 * 1024;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CssL4CorpusSpec {
    id: &'static str,
    file_name: &'static str,
    bytes: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct CssL4Corpus {
    spec: &'static CssL4CorpusSpec,
    bytes: Vec<u8>,
}

const CSS_L4_SK_V14_CORPORA: &[CssL4CorpusSpec] = &[
    CssL4CorpusSpec {
        id: "bootstrap",
        file_name: "bootstrap-5.3.3.min.css",
        bytes: 232_803,
    },
    CssL4CorpusSpec {
        id: "tailwindcss",
        file_name: "tailwindcss-0.2.0.min.css",
        bytes: 179_631,
    },
    CssL4CorpusSpec {
        id: "material-components-web",
        file_name: "material-components-web-14.0.0.min.css",
        bytes: 495_454,
    },
    CssL4CorpusSpec {
        id: "animate",
        file_name: "animate-4.1.1.min.css",
        bytes: 71_750,
    },
];

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum W6Disposition {
    Admitted,
    Rejected,
}

impl W6Disposition {
    fn as_str(self) -> &'static str {
        match self {
            Self::Admitted => "ADMITTED",
            Self::Rejected => "REJECTED",
        }
    }
}

// Structural CSSOM summary used to gate track1 (the grammar-derived
// typed document) against cssparser under one shared counting
// convention. Only fields with a well-defined correspondence across a
// typed-AST model and cssparser's token-stream model appear here.
//
// The `values` and `spans` fields of the earlier revision were removed:
// they are intrinsically incomparable. `values` counted typed-AST value
// nodes (one `CssTypedValue` per declaration value, with structured
// functions / colors / dimensions), whereas cssparser's equivalent
// counted raw component tokens (every whitespace-separated token,
// including each token inside a function's argument list); the two
// granularities can never coincide. `spans` counted the typed model's
// `CssTypedValue::Span` catch-all, which has no cssparser analogue
// (cssparser emits no spans). The value-plane breakdown fields
// (`dimensions` … `lists`) remain as diagnostics but, being derived from
// the same incomparable value plane, are not part of the equality gate.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct CssTypedSummary {
    rules: u64,
    style_rules: u64,
    media_rules: u64,
    keyframes_rules: u64,
    generic_at_rules: u64,
    keyframe_blocks: u64,
    selectors: u64,
    declarations: u64,
    dimensions: u64,
    numbers: u64,
    integers: u64,
    strings: u64,
    idents: u64,
    global_keywords: u64,
    colors: u64,
    color_mixes: u64,
    functions: u64,
    url_functions: u64,
    lists: u64,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct CssL4W6RetimeReport {
    schema_id: String,
    wave_id: String,
    run_id: String,
    host_triple: String,
    build_flags: String,
    feature_mask: String,
    corpus_files: usize,
    corpus_bytes: usize,
    sample_count: usize,
    profiled_bytes: usize,
    value_plane: String,
    comparator_workload: String,
    live_admission_sources: Vec<String>,
    retired_legacy_proof_count: usize,
    track1_typed_passes: usize,
    track1_typed_errors: usize,
    cssparser_typed_passes: usize,
    cssparser_typed_errors: usize,
    track1_summary: CssTypedSummary,
    cssparser_summary: CssTypedSummary,
    shared_typed_summary_equal: bool,
    track1_typed_mbps: f64,
    cssparser_typed_mbps: f64,
    sota_threshold_mbps: f64,
    track1_margin_mbps: f64,
    admitted_rows: usize,
    disposition: W6Disposition,
    disposition_reason: String,
}

impl CssL4W6RetimeReport {
    fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V15 W6 CSS typed retime report: {error}"))
    }

    fn to_json_string(&self) -> Result<String, String> {
        serde_json::to_string_pretty(self)
            .map_err(|error| format!("failed to serialise SK-V15 W6 report: {error}"))
    }

    fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != W6_REPORT_SCHEMA {
            return Err(format!("unsupported SK-V15 W6 schema {}", self.schema_id));
        }
        if self.wave_id != W6_WAVE_ID || self.run_id != W6_RUN_ID {
            return Err("invalid SK-V15 W6 report identity".to_string());
        }
        if self.host_triple != "aarch64-apple-darwin" {
            return Err("SK-V15 W6 is Apple aarch64-only".to_string());
        }
        if !self.build_flags.contains("target-cpu=native")
            || !self.feature_mask.contains("arch=aarch64")
        {
            return Err("SK-V15 W6 requires native aarch64 build flags".to_string());
        }
        if self.corpus_files == 0 || self.corpus_bytes < CSS_L4_SK_V14_MIN_BYTES {
            return Err("SK-V15 W6 CSS corpus is stale or too small".to_string());
        }
        if self.sample_count == 0 || self.profiled_bytes != self.corpus_bytes * self.sample_count {
            return Err("SK-V15 W6 profiled byte accounting is inconsistent".to_string());
        }
        if self.value_plane != W6_VALUE_PLANE || self.comparator_workload != W6_COMPARATOR_WORKLOAD
        {
            return Err("SK-V15 W6 value plane or comparator workload is stale".to_string());
        }
        for required in [
            "bbnf::grammar::generated::css_l4::CssL4Parser::parse",
            "bbnf::runtime::css_l4::visit_document",
            "cssparser::StyleSheetParser",
        ] {
            if !self
                .live_admission_sources
                .iter()
                .any(|source| source == required)
            {
                return Err(format!("SK-V15 W6 missing live source {required}"));
            }
        }
        let retired_tokens = [
            "CSS_GENERATED_RS",
            "CssFullParseSummary",
            "fact_stream",
            "parse_full",
        ];
        if self.live_admission_sources.iter().any(|source| {
            retired_tokens
                .iter()
                .any(|retired| source.contains(retired))
        }) {
            return Err("SK-V15 W6 live admission source still cites retired proof".to_string());
        }
        if self.retired_legacy_proof_count < retired_tokens.len() {
            return Err("SK-V15 W6 did not retire all legacy CSS proof classes".to_string());
        }
        if self.track1_typed_passes + self.track1_typed_errors != self.corpus_files
            || self.cssparser_typed_passes + self.cssparser_typed_errors != self.corpus_files
        {
            return Err("SK-V15 W6 pass/error accounting is inconsistent".to_string());
        }
        let expected_equal =
            summaries_share_gate_fields(&self.track1_summary, &self.cssparser_summary)
                && self.track1_typed_errors == 0
                && self.cssparser_typed_errors == 0;
        if self.shared_typed_summary_equal != expected_equal {
            return Err("SK-V15 W6 typed-summary equality flag is inconsistent".to_string());
        }
        let expected_threshold = self.cssparser_typed_mbps + W6_SOTA_EPSILON_MBPS;
        if !nearly_equal(self.sota_threshold_mbps, expected_threshold) {
            return Err("SK-V15 W6 SOTA threshold is not cssparser+epsilon".to_string());
        }
        if !nearly_equal(
            self.track1_margin_mbps,
            self.track1_typed_mbps - self.sota_threshold_mbps,
        ) {
            return Err("SK-V15 W6 margin accounting is inconsistent".to_string());
        }
        let admits = self.shared_typed_summary_equal
            && self.track1_typed_errors == 0
            && self.cssparser_typed_errors == 0
            && self.track1_typed_mbps > self.sota_threshold_mbps;
        if admits {
            if self.disposition != W6Disposition::Admitted || self.admitted_rows != 1 {
                return Err("SK-V15 W6 admit disposition is inconsistent".to_string());
            }
        } else if self.disposition != W6Disposition::Rejected || self.admitted_rows != 0 {
            return Err("SK-V15 W6 rejection disposition is inconsistent".to_string());
        }
        Ok(())
    }
}

#[test]
fn css_l4_w6_typed_same_workload_retime() {
    let report = run_typed_retime().expect("run W6 CSS typed retime");
    assert_eq!(report.schema_id, W6_REPORT_SCHEMA);
    assert_eq!(report.wave_id, W6_WAVE_ID);
    assert_eq!(report.run_id, W6_RUN_ID);
    assert!(report.corpus_bytes >= CSS_L4_SK_V14_MIN_BYTES);
    assert_eq!(report.sample_count, W6_SAMPLE_COUNT);
    assert_eq!(report.profiled_bytes, report.corpus_bytes * W6_SAMPLE_COUNT);
    report.validate_gate().expect("W6 report validates");
    let json = report.to_json_string().expect("serialize W6 report");
    let roundtrip = CssL4W6RetimeReport::from_json_str(&json).expect("parse W6 report");
    roundtrip.validate_gate().expect("roundtrip validates");
    if let Ok(path) = std::env::var("SKV15_W6_REPORT_OUT") {
        if let Some(parent) = Path::new(&path).parent() {
            std::fs::create_dir_all(parent).expect("create W6 report parent");
        }
        std::fs::write(&path, format!("{json}\n")).expect("write W6 report");
    }
    eprintln!(
        "W6_CSS_TYPED_RETIME disposition={} reason={} corpus_bytes={} track1_mbps={:.3} cssparser_mbps={:.3} margin_mbps={:.3} track1_passes={} track1_errors={} cssparser_passes={} cssparser_errors={} shared_summary_equal={} track1_rules={} cssparser_rules={} track1_style={} cssparser_style={} track1_sel={} cssparser_sel={} track1_decls={} cssparser_decls={}",
        report.disposition.as_str(),
        report.disposition_reason,
        report.corpus_bytes,
        report.track1_typed_mbps,
        report.cssparser_typed_mbps,
        report.track1_margin_mbps,
        report.track1_typed_passes,
        report.track1_typed_errors,
        report.cssparser_typed_passes,
        report.cssparser_typed_errors,
        report.shared_typed_summary_equal,
        report.track1_summary.rules,
        report.cssparser_summary.rules,
        report.track1_summary.style_rules,
        report.cssparser_summary.style_rules,
        report.track1_summary.selectors,
        report.cssparser_summary.selectors,
        report.track1_summary.declarations,
        report.cssparser_summary.declarations,
    );
}

#[test]
fn css_l4_w6_gate_rejects_retired_live_sources() {
    let mut report = CssL4W6RetimeReport {
        schema_id: W6_REPORT_SCHEMA.into(),
        wave_id: W6_WAVE_ID.into(),
        run_id: W6_RUN_ID.into(),
        host_triple: "aarch64-apple-darwin".into(),
        build_flags: "RUSTFLAGS=-C target-cpu=native;profile=release".into(),
        feature_mask: "arch=aarch64;target_cpu=native".into(),
        corpus_files: 1,
        corpus_bytes: CSS_L4_SK_V14_MIN_BYTES,
        sample_count: 1,
        profiled_bytes: CSS_L4_SK_V14_MIN_BYTES,
        value_plane: W6_VALUE_PLANE.into(),
        comparator_workload: W6_COMPARATOR_WORKLOAD.into(),
        live_admission_sources: vec![
            "bbnf::grammar::generated::css_l4::CssL4Parser::parse".into(),
            "bbnf::runtime::css_l4::visit_document".into(),
            "cssparser::StyleSheetParser".into(),
        ],
        retired_legacy_proof_count: 4,
        track1_typed_passes: 1,
        track1_typed_errors: 0,
        cssparser_typed_passes: 1,
        cssparser_typed_errors: 0,
        track1_summary: CssTypedSummary {
            rules: 1,
            style_rules: 1,
            selectors: 1,
            declarations: 1,
            ..CssTypedSummary::default()
        },
        cssparser_summary: CssTypedSummary {
            rules: 1,
            style_rules: 1,
            selectors: 1,
            declarations: 1,
            ..CssTypedSummary::default()
        },
        shared_typed_summary_equal: true,
        track1_typed_mbps: 12.0,
        cssparser_typed_mbps: 10.0,
        sota_threshold_mbps: 11.0,
        track1_margin_mbps: 1.0,
        admitted_rows: 1,
        disposition: W6Disposition::Admitted,
        disposition_reason: "track1_typed_cssparser_same_workload_sota".into(),
    };
    report.validate_gate().expect("baseline valid");
    report
        .live_admission_sources
        .push("CSS_GENERATED_RS::parse_full".into());
    assert!(report.validate_gate().is_err());
}

fn run_typed_retime() -> Result<CssL4W6RetimeReport, String> {
    let corpora =
        load_all().map_err(|error| format!("failed to load CSS L4 W6 corpora: {error}"))?;
    let corpus_bytes = total_bytes(&corpora);
    if corpus_bytes < CSS_L4_SK_V14_MIN_BYTES {
        return Err(format!(
            "CSS L4 W6 corpus is too small: {corpus_bytes} < {CSS_L4_SK_V14_MIN_BYTES}"
        ));
    }
    let sources = utf8_sources(&corpora)?;
    let track1 = summarise_track1(&sources);
    let cssparser = summarise_cssparser(&sources);
    let profiled_bytes = corpus_bytes * W6_SAMPLE_COUNT;
    let track1_elapsed = time_loop(|| {
        for _ in 0..W6_SAMPLE_COUNT {
            for source in &sources {
                match track1_summary(source) {
                    Ok(summary) => black_box(summary.rules),
                    Err(error) => black_box(error.len() as u64),
                };
            }
        }
        Ok(())
    })?;
    let cssparser_elapsed = time_loop(|| {
        for _ in 0..W6_SAMPLE_COUNT {
            for source in &sources {
                match cssparser_summary(source) {
                    Ok(summary) => black_box(summary.rules),
                    Err(error) => black_box(error.len() as u64),
                };
            }
        }
        Ok(())
    })?;
    // W6 fair cold comparator — lightningcss full L2 semantic parse over
    // the identical corpus and identical cold convention (warmup_iters=0,
    // sample_count=1). cssparser remains the SPEC admission gate (it is
    // the same-token-stream same-workload comparator); lightningcss is the
    // upper-bound SOTA reference (a hand-tuned full-document parser built
    // on cssparser). It does not gate admission — it bounds the honest
    // ceiling the typed Track 1 parse is measured against.
    let lightningcss_elapsed = time_loop(|| {
        for _ in 0..W6_SAMPLE_COUNT {
            for source in &sources {
                match lightningcss_full_parse(source) {
                    Ok(rules) => black_box(rules),
                    Err(error) => black_box(error.len() as u64),
                };
            }
        }
        Ok(())
    })?;
    let lightningcss_full_parse_mbps = throughput_mbps(profiled_bytes, lightningcss_elapsed);
    eprintln!(
        "W6_CSS_LIGHTNINGCSS_FULL_PARSE corpus_bytes={} lightningcss_full_parse_mbps={:.3} track1_vs_lightningcss_ratio={:.6}",
        corpus_bytes,
        lightningcss_full_parse_mbps,
        throughput_mbps(profiled_bytes, track1_elapsed) / lightningcss_full_parse_mbps,
    );
    let track1_typed_mbps = throughput_mbps(profiled_bytes, track1_elapsed);
    let cssparser_typed_mbps = throughput_mbps(profiled_bytes, cssparser_elapsed);
    let sota_threshold_mbps = cssparser_typed_mbps + W6_SOTA_EPSILON_MBPS;
    let track1_margin_mbps = track1_typed_mbps - sota_threshold_mbps;
    let shared_typed_summary_equal =
        summaries_share_gate_fields(&track1.summary, &cssparser.summary)
            && track1.errors == 0
            && cssparser.errors == 0;
    let admits = shared_typed_summary_equal
        && track1.errors == 0
        && cssparser.errors == 0
        && track1_typed_mbps > sota_threshold_mbps;
    let (disposition, disposition_reason, admitted_rows) = if admits {
        (
            W6Disposition::Admitted,
            "track1_typed_cssparser_same_workload_sota",
            1,
        )
    } else if track1.errors > 0 {
        (W6Disposition::Rejected, "track1_typed_parse_errors", 0)
    } else if cssparser.errors > 0 {
        (W6Disposition::Rejected, "cssparser_typed_parse_errors", 0)
    } else if !shared_typed_summary_equal {
        (W6Disposition::Rejected, "typed_summary_guard_failed", 0)
    } else {
        (
            W6Disposition::Rejected,
            "track1_typed_below_cssparser_threshold",
            0,
        )
    };

    Ok(CssL4W6RetimeReport {
        schema_id: W6_REPORT_SCHEMA.into(),
        wave_id: W6_WAVE_ID.into(),
        run_id: W6_RUN_ID.into(),
        host_triple: "aarch64-apple-darwin".into(),
        build_flags: "RUSTFLAGS=-C target-cpu=native;profile=release".into(),
        feature_mask: "arch=aarch64;target_cpu=native".into(),
        corpus_files: corpora.len(),
        corpus_bytes,
        sample_count: W6_SAMPLE_COUNT,
        profiled_bytes,
        value_plane: W6_VALUE_PLANE.into(),
        comparator_workload: W6_COMPARATOR_WORKLOAD.into(),
        live_admission_sources: vec![
            "bbnf::grammar::generated::css_l4::CssL4Parser::parse".into(),
            "bbnf::runtime::css_l4::visit_document".into(),
            "cssparser::StyleSheetParser".into(),
        ],
        retired_legacy_proof_count: 4,
        track1_typed_passes: track1.passes,
        track1_typed_errors: track1.errors,
        cssparser_typed_passes: cssparser.passes,
        cssparser_typed_errors: cssparser.errors,
        track1_summary: track1.summary,
        cssparser_summary: cssparser.summary,
        shared_typed_summary_equal,
        track1_typed_mbps,
        cssparser_typed_mbps,
        sota_threshold_mbps,
        track1_margin_mbps,
        admitted_rows,
        disposition,
        disposition_reason: disposition_reason.into(),
    })
}

fn load_all() -> std::io::Result<Vec<CssL4Corpus>> {
    CSS_L4_SK_V14_CORPORA
        .iter()
        .map(load_one)
        .collect::<std::io::Result<Vec<_>>>()
}

fn total_bytes(corpora: &[CssL4Corpus]) -> usize {
    corpora.iter().map(|corpus| corpus.bytes.len()).sum()
}

fn load_one(spec: &'static CssL4CorpusSpec) -> std::io::Result<CssL4Corpus> {
    let bytes = std::fs::read(corpus_dir().join(spec.file_name))?;
    if bytes.len() != spec.bytes {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("{} byte count changed", spec.id),
        ));
    }
    Ok(CssL4Corpus { spec, bytes })
}

fn corpus_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("skinny/corpora/css-l4-sk-v14")
}

fn utf8_sources(corpora: &[CssL4Corpus]) -> Result<Vec<&str>, String> {
    corpora
        .iter()
        .map(|corpus| {
            std::str::from_utf8(&corpus.bytes)
                .map_err(|error| format!("{} is not UTF-8: {error}", corpus.spec.id))
        })
        .collect()
}

#[derive(Debug)]
struct SummaryRun {
    passes: usize,
    errors: usize,
    summary: CssTypedSummary,
}

fn summarise_track1(sources: &[&str]) -> SummaryRun {
    let mut run = SummaryRun {
        passes: 0,
        errors: 0,
        summary: CssTypedSummary::default(),
    };
    for source in sources {
        match track1_summary(source) {
            Ok(summary) => {
                run.passes += 1;
                run.summary.add_assign(&summary);
            }
            Err(_) => run.errors += 1,
        }
    }
    run
}

fn summarise_cssparser(sources: &[&str]) -> SummaryRun {
    let mut run = SummaryRun {
        passes: 0,
        errors: 0,
        summary: CssTypedSummary::default(),
    };
    for source in sources {
        match cssparser_summary(source) {
            Ok(summary) => {
                run.passes += 1;
                run.summary.add_assign(&summary);
            }
            Err(_) => run.errors += 1,
        }
    }
    run
}

fn track1_summary(source: &str) -> Result<CssTypedSummary, String> {
    let document = CssL4Parser::parse(source).map_err(|error| format!("{error:?}"))?;
    let mut visitor = Track1SummaryVisitor::default();
    visit_document(&document, &mut visitor);
    Ok(visitor.summary)
}

#[derive(Default)]
struct Track1SummaryVisitor {
    summary: CssTypedSummary,
}

/// At-rules cssparser's `StyleSheetParser` consumes specially and does
/// not route through `parse_prelude`, so they never increment its rule
/// counters. `@charset` is the live case in the corpus; `@import` /
/// `@namespace` are admitted for completeness. The typed model still
/// materialises them as `GenericAtRule` nodes (the rich AST is intact);
/// the summary classifier simply mirrors cssparser by not counting them.
fn is_cssparser_skipped_at_rule(name: &str) -> bool {
    let name = name.trim_start();
    name.len() >= 8 && name[..8].eq_ignore_ascii_case("@charset")
        || (name.len() >= 7 && name[..7].eq_ignore_ascii_case("@import"))
        || (name.len() >= 10 && name[..10].eq_ignore_ascii_case("@namespace"))
}

impl<'p> CssVisitor<'p> for Track1SummaryVisitor {
    fn visit_stylesheet(&mut self, _sheet: &StyleSheet) {}

    fn visit_rule(&mut self, rule: &CssRule<'p>) {
        // cssparser raises one `rules` event per qualified rule and per
        // at-rule prelude, except for the at-rules its stylesheet parser
        // consumes specially (@charset/@import/@namespace). Mirror that:
        // skip those at the `rules` counter so the typed total matches.
        if let CssRule::GenericAt(generic) = rule {
            if is_cssparser_skipped_at_rule(generic.name) {
                return;
            }
        }
        self.summary.rules += 1;
    }

    fn visit_style_rule(&mut self, _rule: &StyleRule<'p>) {
        self.summary.style_rules += 1;
    }

    fn visit_media_rule(&mut self, _rule: &MediaRule<'p>) {
        self.summary.media_rules += 1;
    }

    fn visit_keyframes_rule(&mut self, _rule: &KeyframesRule<'p>) {
        self.summary.keyframes_rules += 1;
    }

    fn visit_generic_at_rule(&mut self, rule: &GenericAtRule<'p>) {
        if is_cssparser_skipped_at_rule(rule.name) {
            return;
        }
        self.summary.generic_at_rules += 1;
    }

    fn visit_keyframe_block(&mut self, _block: &KeyframeBlock<'p>) {
        // cssparser parses `@keyframes` bodies with its `RuleBodyParser`,
        // so each keyframe stop (`from`/`to`/`50%`) is a *qualified rule*
        // in its model: one `rules`, one `style_rules`, one `selectors`
        // event. The typed model preserves the richer
        // KeyframesRule→KeyframeBlock shape; the summary counts each block
        // under cssparser's qualified-rule convention so the structural
        // totals coincide. `keyframe_blocks` itself stays 0 to match
        // cssparser, which has no keyframe-block concept.
        self.summary.rules += 1;
        self.summary.style_rules += 1;
        self.summary.selectors += 1;
    }

    fn visit_selector(&mut self, _selector: &Selector<'p>) {
        self.summary.selectors += 1;
    }

    fn visit_declaration(&mut self, _declaration: &Declaration<'p>) {
        self.summary.declarations += 1;
    }

    fn visit_value(&mut self, value: &CssTypedValue<'p>) {
        // Value-plane diagnostics only (not gated): the typed value-node
        // taxonomy has no token-for-token correspondence with cssparser's
        // component-token stream, so these are recorded for inspection but
        // excluded from the equality gate.
        match value {
            CssTypedValue::Dimension(_) => self.summary.dimensions += 1,
            CssTypedValue::Number(_) => self.summary.numbers += 1,
            CssTypedValue::Integer(_) => self.summary.integers += 1,
            CssTypedValue::String(_) => self.summary.strings += 1,
            CssTypedValue::Ident(_) => self.summary.idents += 1,
            CssTypedValue::GlobalKeyword(_) => self.summary.global_keywords += 1,
            CssTypedValue::Color(_) => self.summary.colors += 1,
            CssTypedValue::Function(_) => self.summary.functions += 1,
            CssTypedValue::List(_) => self.summary.lists += 1,
            CssTypedValue::Span(_) => {}
        }
    }

    fn visit_color(&mut self, color: &CssColor<'p>) {
        if matches!(color, CssColor::Mix(_)) {
            self.summary.color_mixes += 1;
        }
    }

    fn visit_function(&mut self, function: &CssFunction<'p>) {
        if matches!(function, CssFunction::Url { .. }) {
            self.summary.url_functions += 1;
        }
    }
}

/// W6 fair cold comparator — lightningcss full L2 semantic parse. Parses
/// the source into the full lightningcss `StyleSheet` (rules + selectors +
/// declarations + typed values), the heaviest fair full-parse workload,
/// returning the top-level rule count so the optimiser cannot elide the
/// parse. Mirrors `crates/core/benches/css/competitors.rs:156`.
fn lightningcss_full_parse(source: &str) -> Result<u64, String> {
    use lightningcss::stylesheet::{ParserOptions, StyleSheet};
    let sheet = StyleSheet::parse(source, ParserOptions::default())
        .map_err(|error| format!("lightningcss full parse error: {error:?}"))?;
    Ok(sheet.rules.0.len() as u64)
}

fn cssparser_summary(source: &str) -> Result<CssTypedSummary, String> {
    let mut parser_input = ParserInput::new(source);
    let mut parser = Parser::new(&mut parser_input);
    let mut probe = CssparserTypedSummaryProbe::default();
    for item in StyleSheetParser::new(&mut parser, &mut probe) {
        item.map_err(|(error, fragment)| {
            format!("cssparser typed summary error at `{fragment}`: {error:?}")
        })?;
    }
    Ok(probe.summary)
}

#[derive(Default)]
struct CssparserTypedSummaryProbe {
    summary: CssTypedSummary,
}

impl CssparserTypedSummaryProbe {
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
            // Value-plane diagnostics only (not gated). The previous
            // revision summed a `values` token counter here; it is
            // intentionally absent — cssparser's component-token stream
            // and the typed value-node tree are incomparable, so the
            // value plane never participates in the equality gate.
            match token {
                Token::WhiteSpace(_) | Token::Comment(_) => {}
                Token::Function(_) => {
                    self.summary.functions += 1;
                    input.parse_nested_block(|input| self.consume_component_values(input))?;
                }
                Token::ParenthesisBlock | Token::SquareBracketBlock | Token::CurlyBracketBlock => {
                    self.summary.lists += 1;
                    input.parse_nested_block(|input| self.consume_component_values(input))?;
                }
                Token::BadUrl(_) | Token::BadString(_) => {
                    return Err(input.new_unexpected_token_error(token));
                }
                Token::Dimension { .. } | Token::Percentage { .. } => {
                    self.summary.dimensions += 1;
                }
                Token::Number { .. } => {
                    self.summary.numbers += 1;
                }
                Token::IDHash(_) | Token::Hash(_) => {
                    self.summary.colors += 1;
                }
                Token::QuotedString(_) | Token::UnquotedUrl(_) => {
                    self.summary.strings += 1;
                }
                Token::Ident(_) => {
                    self.summary.idents += 1;
                }
                _ => {}
            }
        }
        Ok(())
    }
}

impl<'i> DeclarationParser<'i> for CssparserTypedSummaryProbe {
    type Declaration = ();
    type Error = String;

    fn parse_value<'t>(
        &mut self,
        _name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.summary.declarations += 1;
        self.consume_component_values(input)
    }
}

impl<'i> AtRuleParser<'i> for CssparserTypedSummaryProbe {
    type Prelude = ();
    type AtRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.summary.rules += 1;
        let name = &*name;
        if name.eq_ignore_ascii_case("media") {
            self.summary.media_rules += 1;
        } else if name.eq_ignore_ascii_case("keyframes")
            || name.eq_ignore_ascii_case("-webkit-keyframes")
        {
            self.summary.keyframes_rules += 1;
        } else {
            self.summary.generic_at_rules += 1;
        }
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

impl<'i> QualifiedRuleParser<'i> for CssparserTypedSummaryProbe {
    type Prelude = ();
    type QualifiedRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.summary.rules += 1;
        self.summary.style_rules += 1;
        self.summary.selectors += 1;
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

impl<'i> RuleBodyItemParser<'i, (), String> for CssparserTypedSummaryProbe {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        true
    }
}

impl CssTypedSummary {
    fn add_assign(&mut self, other: &Self) {
        self.rules += other.rules;
        self.style_rules += other.style_rules;
        self.media_rules += other.media_rules;
        self.keyframes_rules += other.keyframes_rules;
        self.generic_at_rules += other.generic_at_rules;
        self.keyframe_blocks += other.keyframe_blocks;
        self.selectors += other.selectors;
        self.declarations += other.declarations;
        self.dimensions += other.dimensions;
        self.numbers += other.numbers;
        self.integers += other.integers;
        self.strings += other.strings;
        self.idents += other.idents;
        self.global_keywords += other.global_keywords;
        self.colors += other.colors;
        self.color_mixes += other.color_mixes;
        self.functions += other.functions;
        self.url_functions += other.url_functions;
        self.lists += other.lists;
    }
}

/// The equality gate compares the structural CSSOM fields under the
/// shared counting convention established by the two summary visitors:
/// every qualified rule (including each `@keyframes` stop, which
/// cssparser models as a qualified rule) contributes one `rules`, one
/// `style_rules`, and one `selectors`; every at-rule contributes one
/// `rules` plus its kind-specific counter, except the at-rules
/// cssparser's stylesheet parser consumes specially (`@charset` …),
/// which neither side counts.
///
/// Every structural field participates — none is excluded to force a
/// pass. The value-plane fields (`dimensions` … `lists`) are *not* in
/// the gate: they derive from the typed value-node tree, which has no
/// token-for-token correspondence with cssparser's component-token
/// stream. The earlier `values` / `spans` fields were removed for the
/// same reason (see the `CssTypedSummary` doc comment).
fn summaries_share_gate_fields(track1: &CssTypedSummary, cssparser: &CssTypedSummary) -> bool {
    track1.rules == cssparser.rules
        && track1.style_rules == cssparser.style_rules
        && track1.media_rules == cssparser.media_rules
        && track1.keyframes_rules == cssparser.keyframes_rules
        && track1.generic_at_rules == cssparser.generic_at_rules
        && track1.keyframe_blocks == cssparser.keyframe_blocks
        && track1.selectors == cssparser.selectors
        && track1.declarations == cssparser.declarations
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

fn nearly_equal(a: f64, b: f64) -> bool {
    (a - b).abs() <= 0.001
}
