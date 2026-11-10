//! SK-V22 graduation-baseline cold JSON harness — rebuilt from report 33 +
//! PROTO-A1 recipes. Byte-faithful mirror of `css_canon_bench.rs` cold
//! discipline: one out-of-band touch to page in the source, then N=200 timed
//! single parses with fresh parser state and dropped output.
//!
//! Modes:
//!   default N            -> cold table: median/min ns/byte + MB/s per lane/corpus
//!   pmu    ITERS         -> per-lane cyc/byte + instructions + cpi (proc_pid_rusage V5)
//!   paired N             -> interleaved paired per-sample ratios (load-robust):
//!                           each sample times parse_tape, fused, sonic, serde
//!                           back-to-back; reports paired ratio mean + 95% CI so
//!                           load hits both sides of every ratio equally.
//!
//! Lanes:
//!   parse_only  runtime::generated_json::parse_only  (structural validation, no tape)
//!   parse_tape  runtime::generated_json::parse        (the live lazy-view product; tape)
//!   fused       direct_struct::track1_digest          (parse_direct sink, eager digest)
//!   sonic       sonic-rs 0.5.8 full-value
//!   serde       serde_json full-value

use std::hint::black_box;
use std::time::Instant;

// --- PMU via proc_pid_rusage V5 (cycles/instructions), mirrors css_canon_bench.rs ---
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

type ParseFn = fn(&str, &[u8]) -> u64;

fn parse_only_lane(input: &str, bytes: &[u8]) -> u64 {
    runtime::generated_json::parse_only(input).expect("generated parse_only failed");
    black_box(bytes.len() as u64)
}
fn parse_tape_lane(input: &str, bytes: &[u8]) -> u64 {
    let root = runtime::generated_json::parse(input).expect("generated parse (tape) failed");
    black_box(&root);
    black_box(bytes.len() as u64)
}
fn fused_lane(input: &str, _bytes: &[u8]) -> u64 {
    let d = bbnf_bench::direct_struct::track1_digest(input).expect("fused track1 digest failed");
    black_box(d.fingerprint ^ d.objects ^ d.arrays ^ d.strings ^ d.numbers ^ d.string_bytes)
}
fn sonic_lane(_input: &str, bytes: &[u8]) -> u64 {
    let d = bbnf_bench::direct_struct::sonic_digest(bytes).expect("sonic digest failed");
    black_box(d.fingerprint ^ d.objects ^ d.arrays ^ d.strings ^ d.numbers ^ d.string_bytes)
}
fn serde_lane(_input: &str, bytes: &[u8]) -> u64 {
    let d = bbnf_bench::direct_struct::serde_digest(bytes).expect("serde digest failed");
    black_box(d.fingerprint ^ d.objects ^ d.arrays ^ d.strings ^ d.numbers ^ d.string_bytes)
}

const WORKLOADS: &[(&str, ParseFn)] = &[
    ("parse_only", parse_only_lane),
    ("parse_tape", parse_tape_lane),
    ("fused", fused_lane),
    ("sonic", sonic_lane),
    ("serde", serde_lane),
];
const CORPORA: &[&str] = &["twitter", "canada", "citm_catalog"];

struct Stats {
    median_nspb: f64,
    min_nspb: f64,
    median_mb_s: f64,
    max_mb_s: f64,
    stddev_mb_s: f64,
    n: usize,
}

fn mb_s(bytes: usize, secs: f64) -> f64 {
    if secs <= 0.0 {
        return f64::INFINITY;
    }
    bytes as f64 / (secs * 1_000_000.0)
}

fn load_corpus(name: &str) -> Vec<u8> {
    let path = bbnf_bench::real_typed_struct::locate_fixture(name);
    std::fs::read(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"))
}

/// N cold per-parse samples. Each sample times exactly one parse of `input`.
fn sample(parse: ParseFn, input: &str, bytes: &[u8], n: usize) -> Stats {
    let nbytes = bytes.len();
    let mut ns_samples: Vec<f64> = Vec::with_capacity(n);
    black_box(parse(black_box(input), black_box(bytes)));
    for _ in 0..n {
        let start = Instant::now();
        let r = parse(black_box(input), black_box(bytes));
        let secs = start.elapsed().as_secs_f64();
        black_box(r);
        ns_samples.push(secs * 1e9 / nbytes as f64);
    }
    ns_samples.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let median_ns = if n % 2 == 0 {
        (ns_samples[n / 2 - 1] + ns_samples[n / 2]) / 2.0
    } else {
        ns_samples[n / 2]
    };
    let min_ns = ns_samples[0];
    // MB/s = bytes/(secs*1e6); secs/byte = ns_per_byte/1e9 -> MB/s = 1000/ns_per_byte.
    let median_mb_s = 1000.0 / median_ns;
    let max_mb_s = 1000.0 / min_ns;
    let mean_mb_s = ns_samples.iter().map(|ns| 1000.0 / ns).sum::<f64>() / n as f64;
    let var = ns_samples
        .iter()
        .map(|ns| (1000.0 / ns - mean_mb_s).powi(2))
        .sum::<f64>()
        / n as f64;
    Stats {
        median_nspb: median_ns,
        min_nspb: min_ns,
        median_mb_s,
        max_mb_s,
        stddev_mb_s: var.sqrt(),
        n,
    }
}

fn loadavg() -> String {
    std::process::Command::new("sysctl")
        .args(["-n", "vm.loadavg"])
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mode = args.get(1).map(|s| s.as_str()).unwrap_or("");

    // PMU mode: per-corpus per-lane cyc/byte over a fixed iter count.
    if mode == "pmu" || std::env::var("JSON_CANON_PMU").is_ok() {
        let iters: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(800);
        println!("# SK-V22 JSON PMU cycles/byte  iters={iters}  loadavg={}", loadavg());
        println!("# schema: PMU corpus lane bytes iters cycles instructions cyc_per_byte cpi mb_s");
        for name in CORPORA {
            let bytes = load_corpus(name);
            let input = std::str::from_utf8(&bytes).unwrap();
            for (lane, parse) in WORKLOADS {
                black_box(parse(black_box(input), black_box(&bytes)));
                let ri0 = read_rusage_v5();
                let t0 = Instant::now();
                let mut ck = 0u64;
                for _ in 0..iters {
                    ck ^= parse(black_box(input), black_box(&bytes));
                }
                let secs = t0.elapsed().as_secs_f64();
                let ri1 = read_rusage_v5();
                black_box(ck);
                let total = (bytes.len() as u128) * (iters as u128);
                let cyc = ri1.ri_cycles - ri0.ri_cycles;
                let ins = ri1.ri_instructions - ri0.ri_instructions;
                println!(
                    "PMU corpus={} lane={} bytes={} iters={} cycles={} instructions={} cyc_per_byte={:.4} cpi={:.4} mb_s={:.3}",
                    name, lane, bytes.len(), iters, cyc, ins,
                    cyc as f64 / total as f64,
                    if ins == 0 { 0.0 } else { cyc as f64 / ins as f64 },
                    mb_s(total as usize, secs)
                );
            }
        }
        return;
    }

    // Paired interleaved mode: per corpus, per sample, time parse_tape, fused,
    // sonic, serde back-to-back; report paired ratio mean + 95% CI. Load hits
    // both sides of every ratio inside one sample, nulling inter-invocation drift.
    if mode == "paired" || std::env::var("JSON_CANON_PAIRED").is_ok() {
        let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(200);
        println!("# SK-V22 JSON paired-interleaved  N={n}  loadavg={}", loadavg());
        println!("# schema: PAIR corpus ratio mean lo95 hi95  (ratio = A_ns_per_byte / B_ns_per_byte)");
        for name in CORPORA {
            let bytes = load_corpus(name);
            let input = std::str::from_utf8(&bytes).unwrap();
            let nb = bytes.len() as f64;
            // touch
            black_box(parse_tape_lane(input, &bytes));
            black_box(fused_lane(input, &bytes));
            black_box(sonic_lane(input, &bytes));
            black_box(serde_lane(input, &bytes));
            // per-sample ns/byte for each lane, interleaved inside one iteration
            let (mut tape, mut fused, mut sonic, mut serde) =
                (Vec::with_capacity(n), Vec::with_capacity(n), Vec::with_capacity(n), Vec::with_capacity(n));
            for _ in 0..n {
                let s = Instant::now();
                black_box(parse_tape_lane(black_box(input), black_box(&bytes)));
                tape.push(s.elapsed().as_secs_f64() * 1e9 / nb);
                let s = Instant::now();
                black_box(fused_lane(black_box(input), black_box(&bytes)));
                fused.push(s.elapsed().as_secs_f64() * 1e9 / nb);
                let s = Instant::now();
                black_box(sonic_lane(black_box(input), black_box(&bytes)));
                sonic.push(s.elapsed().as_secs_f64() * 1e9 / nb);
                let s = Instant::now();
                black_box(serde_lane(black_box(input), black_box(&bytes)));
                serde.push(s.elapsed().as_secs_f64() * 1e9 / nb);
            }
            // paired ratios: skinny/SOTA per sample (lower = skinny faster)
            emit_paired(name, "tape/sonic", &tape, &sonic);
            emit_paired(name, "fused/sonic", &fused, &sonic);
            emit_paired(name, "tape/serde", &tape, &serde);
            emit_paired(name, "fused/serde", &fused, &serde);
        }
        return;
    }

    // Default cold table.
    let n: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(200);
    assert!(n >= 50, "N must be >= 50 (telemetry-honesty gate)");
    let host = std::process::Command::new("uname")
        .arg("-m")
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default();
    println!("# SK-V22 canonical cold JSON bench  N={n}  host={host}  loadavg={}", loadavg());
    println!("# schema: ROW corpus lane bytes n median_nspb min_nspb median_mb_s max_mb_s stddev_mb_s");
    for name in CORPORA {
        let bytes = load_corpus(name);
        let input = std::str::from_utf8(&bytes).expect("utf8");
        for (lane, parse) in WORKLOADS {
            let s = sample(*parse, input, &bytes, n);
            println!(
                "ROW corpus={} lane={} bytes={} n={} median_nspb={:.4} min_nspb={:.4} median_mb_s={:.3} max_mb_s={:.3} stddev_mb_s={:.3}",
                name, lane, bytes.len(), s.n, s.median_nspb, s.min_nspb, s.median_mb_s, s.max_mb_s, s.stddev_mb_s
            );
        }
    }
}

/// Emit a paired ratio (per-sample a/b) with mean and 95% CI (normal approx).
fn emit_paired(corpus: &str, label: &str, a: &[f64], b: &[f64]) {
    let n = a.len();
    let ratios: Vec<f64> = a.iter().zip(b).map(|(x, y)| x / y).collect();
    let mean = ratios.iter().sum::<f64>() / n as f64;
    let var = ratios.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / (n as f64 - 1.0);
    let se = (var / n as f64).sqrt();
    let lo = mean - 1.96 * se;
    let hi = mean + 1.96 * se;
    println!("PAIR corpus={corpus} ratio={label} mean={mean:.4} lo95={lo:.4} hi95={hi:.4}");
}
