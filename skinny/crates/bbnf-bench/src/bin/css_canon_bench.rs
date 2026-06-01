//! SK-V17 canonical cold N>=50 per-corpus CSS benchmark harness.
//!
//! Formalizes the statistically adequate replacement for the W6/W8
//! single-sample + broadcast harness. Per benched CSS corpus
//! (bootstrap / tailwindcss / material-components-web / animate, the set fixed
//! in `css_l4_corpus.rs:22-54`), per workload, it takes N cold per-parse
//! samples and reports median / min / max / stddev in Mbps.
//!
//! Cold discipline: each sample is ONE parse of ONE corpus. There is no warm
//! cache and no amortised allocation across samples — the timed region is a
//! single `parse(...)` call, black-boxed, with the result dropped each sample.
//! This is the `no-warm-benches` cold-per-parse contract.
//!
//! Workloads measured per corpus:
//!   - track1_full_parse : `parse_full` (the benched real-corpus Track 1
//!                          structural full-parse, CssFullParser summary).
//!   - track1_rich       : `rich_summary` (the tape-routed RICH typed CSSOM
//!                          projection — selectors + typed value-node counts
//!                          materialized atop the offset tape; the W1-PRUNE
//!                          successor to the deleted fact-stream String, and the
//!                          eager-vs-eager >SOTA product vs lightningcss CSSOM).
//!   - lightningcss      : full-CSSOM `StyleSheet::parse` (the fair >SOTA bar).
//!   - cssparser         : token-scan StyleSheetParser (flaw probe; materializes
//!                          nothing).
//!
//! Invocation:
//!     cargo build --release -p bbnf-bench --bin css_canon_bench
//!     ./target/release/css_canon_bench [N]        (default N=200)
//!     CSS_CANON_PROFILE=<workload> ./target/release/css_canon_bench <iters>
//!         runs a tight profiling loop of one workload over all corpora for
//!         samply attribution (no statistics; flame-profile driver).

use bbnf_bench::css_l4_corpus::{load_all, CssL4Corpus};
use cssparser::{
    AtRuleParser, CowRcStr, DeclarationParser, Parser, ParserInput, ParserState,
    QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, StyleSheetParser, Token,
};
use lightningcss::stylesheet::{ParserOptions, StyleSheet};
use runtime::generated_css_l4_declaration_values as css_decl;
use std::hint::black_box;
use std::time::Instant;

// --- PMU via proc_pid_rusage V5 (cycles/instructions), mirrors profile_direct.rs ---
#[repr(C)]
#[derive(Default, Clone, Copy)]
struct RusageInfoV5 {
    ri_uuid: [u8; 16],
    ri_user_time: u64,
    ri_system_time: u64,
    ri_pkg_idle_wkups: u64,
    ri_interrupt_wkups: u64,
    ri_pageins: u64,
    ri_wired_size: u64,
    ri_resident_size: u64,
    ri_phys_footprint: u64,
    ri_proc_start_abstime: u64,
    ri_proc_exit_abstime: u64,
    ri_child_user_time: u64,
    ri_child_system_time: u64,
    ri_child_pkg_idle_wkups: u64,
    ri_child_interrupt_wkups: u64,
    ri_child_pageins: u64,
    ri_child_elapsed_abstime: u64,
    ri_diskio_bytesread: u64,
    ri_diskio_byteswritten: u64,
    ri_cpu_time_qos_default: u64,
    ri_cpu_time_qos_maintenance: u64,
    ri_cpu_time_qos_background: u64,
    ri_cpu_time_qos_utility: u64,
    ri_cpu_time_qos_legacy: u64,
    ri_cpu_time_qos_user_initiated: u64,
    ri_cpu_time_qos_user_interactive: u64,
    ri_billed_system_time: u64,
    ri_serviced_system_time: u64,
    ri_logical_writes: u64,
    ri_lifetime_max_phys_footprint: u64,
    ri_instructions: u64,
    ri_cycles: u64,
    ri_billed_energy: u64,
    ri_serviced_energy: u64,
    ri_interval_max_phys_footprint: u64,
    ri_runnable_time: u64,
    ri_flags: u64,
}
const RUSAGE_INFO_V5: i32 = 5;
extern "C" {
    fn proc_pid_rusage(pid: libc::pid_t, flavor: i32, buffer: *mut u8) -> i32;
}
fn read_rusage_v5() -> RusageInfoV5 {
    let mut ri = RusageInfoV5::default();
    let rc = unsafe {
        proc_pid_rusage(
            libc::getpid(),
            RUSAGE_INFO_V5,
            (&mut ri) as *mut RusageInfoV5 as *mut u8,
        )
    };
    if rc != 0 {
        panic!("proc_pid_rusage failed rc={rc}");
    }
    ri
}

type ParseFn = fn(&str) -> u64;

fn track1_full_parse(input: &str) -> u64 {
    let out = css_decl::parser::parse_full(input).expect("track1 full_parse");
    black_box(out.len() as u64)
}

fn track1_rich(input: &str) -> u64 {
    let s = css_decl::parser::rich_summary(input).expect("track1 rich");
    black_box(
        (s.rules
            + s.at_rules
            + s.qualified_rules
            + s.declarations
            + s.selectors
            + s.dimensions
            + s.numbers
            + s.colors
            + s.functions) as u64,
    )
}

fn lightningcss_full_cssom(input: &str) -> u64 {
    let sheet = StyleSheet::parse(input, ParserOptions::default()).expect("lightningcss CSSOM");
    black_box(sheet.rules.0.len() as u64)
}

fn cssparser_token_scan(input: &str) -> u64 {
    cssparser_full_parse(input).expect("cssparser token scan");
    black_box(input.len() as u64)
}

const WORKLOADS: &[(&str, ParseFn)] = &[
    ("track1_full_parse", track1_full_parse),
    ("track1_rich", track1_rich),
    ("lightningcss", lightningcss_full_cssom),
    ("cssparser", cssparser_token_scan),
];

struct Stats {
    median: f64,
    min: f64,
    max: f64,
    stddev: f64,
    n: usize,
}

fn mbps(bytes: usize, secs: f64) -> f64 {
    if secs <= 0.0 {
        return f64::INFINITY;
    }
    (bytes as f64 * 8.0) / (secs * 1_000_000.0)
}

/// N cold per-parse samples. Each sample times exactly one parse of `input`.
fn sample(parse: ParseFn, input: &str, n: usize) -> Stats {
    let bytes = input.len();
    let mut mbps_samples: Vec<f64> = Vec::with_capacity(n);
    // Touch each corpus once outside the timed window so the measured samples
    // are not dominated by first-touch page faults of the source buffer; the
    // PARSE itself is cold per sample (no parser state reused, output dropped).
    black_box(parse(black_box(input)));
    for _ in 0..n {
        let start = Instant::now();
        let r = parse(black_box(input));
        let secs = start.elapsed().as_secs_f64();
        black_box(r);
        mbps_samples.push(mbps(bytes, secs));
    }
    mbps_samples.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let median = if n % 2 == 0 {
        (mbps_samples[n / 2 - 1] + mbps_samples[n / 2]) / 2.0
    } else {
        mbps_samples[n / 2]
    };
    let min = mbps_samples[0];
    let max = mbps_samples[n - 1];
    let mean = mbps_samples.iter().sum::<f64>() / n as f64;
    let var = mbps_samples.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n as f64;
    Stats {
        median,
        min,
        max,
        stddev: var.sqrt(),
        n,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Profiling driver mode: tight loop of one workload for samply attribution.
    if let Ok(workload) = std::env::var("CSS_CANON_PROFILE") {
        let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(5_000);
        let corpora = load_all().expect("load CSS corpora");
        let parse = WORKLOADS
            .iter()
            .find(|(name, _)| *name == workload)
            .map(|(_, f)| *f)
            .unwrap_or_else(|| panic!("unknown CSS_CANON_PROFILE workload {workload}"));
        eprintln!("css_canon_bench: PROFILE workload={workload} iters={iters}");
        let sources: Vec<&str> = corpora
            .iter()
            .map(|c| std::str::from_utf8(&c.bytes).unwrap())
            .collect();
        let mut cksum = 0u64;
        let start = Instant::now();
        for _ in 0..iters {
            for src in &sources {
                cksum ^= parse(black_box(src));
            }
        }
        eprintln!(
            "css_canon_bench: PROFILE done {:.2}s cksum={cksum}",
            start.elapsed().as_secs_f64()
        );
        return;
    }

    // PMU mode: per-corpus per-workload cycles/byte over a fixed iter count.
    if std::env::var("CSS_CANON_PMU").is_ok() {
        let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(2_000);
        let corpora = load_all().expect("load CSS corpora");
        println!("# SK-V17 PMU cycles/byte  iters={iters}");
        println!("# schema: corpus workload bytes iters cycles instructions cycles_per_byte cpi mbps");
        for corpus in &corpora {
            let input = std::str::from_utf8(&corpus.bytes).unwrap();
            for (name, parse) in WORKLOADS {
                black_box(parse(black_box(input))); // touch
                let ri0 = read_rusage_v5();
                let t0 = Instant::now();
                let mut ck = 0u64;
                for _ in 0..iters {
                    ck ^= parse(black_box(input));
                }
                let secs = t0.elapsed().as_secs_f64();
                let ri1 = read_rusage_v5();
                black_box(ck);
                let total = (input.len() as u128) * (iters as u128);
                let cyc = ri1.ri_cycles - ri0.ri_cycles;
                let ins = ri1.ri_instructions - ri0.ri_instructions;
                println!(
                    "PMU corpus={} workload={} bytes={} iters={} cycles={} instructions={} cycles_per_byte={:.4} cpi={:.4} mbps={:.3}",
                    corpus.spec.id,
                    name,
                    input.len(),
                    iters,
                    cyc,
                    ins,
                    cyc as f64 / total as f64,
                    if ins == 0 { 0.0 } else { cyc as f64 / ins as f64 },
                    mbps(total as usize, secs)
                );
            }
        }
        return;
    }

    let n: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(200);
    assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)");

    let corpora: Vec<CssL4Corpus> = load_all().expect("load CSS corpora");
    let host = std::process::Command::new("uname")
        .arg("-m")
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default();

    println!("# SK-V17 canonical cold CSS bench  N={n}  host={host}");
    println!("# schema: corpus workload bytes n median_mbps min_mbps max_mbps stddev_mbps");
    for corpus in &corpora {
        let input = std::str::from_utf8(&corpus.bytes).expect("utf8");
        for (name, parse) in WORKLOADS {
            let s = sample(*parse, input, n);
            println!(
                "ROW corpus={} workload={} bytes={} n={} median_mbps={:.3} min_mbps={:.3} max_mbps={:.3} stddev_mbps={:.3}",
                corpus.spec.id,
                name,
                input.len(),
                s.n,
                s.median,
                s.min,
                s.max,
                s.stddev
            );
        }
    }
}

// --- cssparser token-scan probe (mirrors css_l4_w8.rs CssparserFullParseProbe) ---

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
