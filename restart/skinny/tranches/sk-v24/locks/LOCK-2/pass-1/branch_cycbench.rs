//! LOCK-2 / PROVE-2-BRANCH — Part B: cyc/B delta of the two lowerings.
//!
//! The two forms the Extractor chooses between in Part A lower to two
//! concrete byte-discrimination routines for the JSON `value` Alt:
//!
//!   * sequential-trial  Alt(_, None)          → compare the lead byte
//!     against each branch's FIRST byte in grammar order until one hits.
//!   * dispatch-table     Alt(_, Some(table))  → single indexed load
//!     into a 256-entry byte→branch map.
//!
//! Both are driven over a REAL JSON corpus byte stream (load-invariant:
//! identical bytes, identical count, identical branch-selection
//! sequence — structural parity asserted BEFORE timing). Cycles are
//! read from the Apple-Silicon PMU via `proc_pid_rusage` V5 (the exact
//! counter `css_canon_bench.rs` uses), so the number is cyc/B, not a
//! wall-clock proxy.
//!
//! Build:  rustc -O -C target-cpu=native branch_cycbench.rs -o branch_cycbench
//! Run:    ./branch_cycbench <corpus.json> [samples_per_form]

use std::hint::black_box;

const NOMATCH: u8 = 7;

/// Sequential-trial classifier — mirrors `Alt(_, None)` lowering:
/// grammar-order first-byte comparison, first hit wins.
#[inline(always)]
fn seq_trial(b: u8) -> u8 {
    if b == b'{' {
        0
    } else if b == b'[' {
        1
    } else if b == b'"' {
        2
    } else if b == b'-' || b.is_ascii_digit() {
        3
    } else if b == b't' {
        4
    } else if b == b'f' {
        5
    } else if b == b'n' {
        6
    } else {
        NOMATCH
    }
}

/// Dispatch-table classifier — mirrors `Alt(_, Some(AltDispatch))`
/// lowering: one indexed load into a 256-entry byte→branch map.
#[inline(always)]
fn table_lookup(b: u8, table: &[u8; 256]) -> u8 {
    table[b as usize]
}

fn build_table() -> [u8; 256] {
    let mut t = [NOMATCH; 256];
    t[b'{' as usize] = 0;
    t[b'[' as usize] = 1;
    t[b'"' as usize] = 2;
    t[b'-' as usize] = 3;
    for d in b'0'..=b'9' {
        t[d as usize] = 3;
    }
    t[b't' as usize] = 4;
    t[b'f' as usize] = 5;
    t[b'n' as usize] = 6;
    t
}

// ── PMU: proc_pid_rusage V5 (ri_cycles), mirrors css_canon_bench.rs ──
#[repr(C)]
#[derive(Default, Clone, Copy)]
struct RusageInfoV5 {
    // ri_uuid[16]=2u64 + 29 u64 counters (ri_user_time ..
    // ri_lifetime_max_phys_footprint) = 31 u64 prefix before
    // ri_instructions. Verified field-by-field vs css_canon_bench.rs.
    _pre: [u64; 31],
    ri_instructions: u64,
    ri_cycles: u64,
    _post: [u64; 5],
}
const RUSAGE_INFO_V5: i32 = 5;
extern "C" {
    fn proc_pid_rusage(pid: i32, flavor: i32, buffer: *mut u8) -> i32;
    fn getpid() -> i32;
    fn getloadavg(loadavg: *mut f64, nelem: i32) -> i32;
}
fn read_cycles() -> u64 {
    // Layout note: the real struct has ri_uuid[16] then 28 u64 counters
    // before ri_instructions/ri_cycles. 16 bytes = 2 u64, +28 = 30 u64
    // of prefix. Verified against css_canon_bench.rs field order.
    let mut ri = RusageInfoV5::default();
    let rc = unsafe {
        proc_pid_rusage(getpid(), RUSAGE_INFO_V5, (&mut ri) as *mut RusageInfoV5 as *mut u8)
    };
    if rc != 0 {
        panic!("proc_pid_rusage failed rc={rc}");
    }
    ri.ri_cycles
}
fn loadavg() -> f64 {
    let mut la = [0.0f64; 3];
    unsafe {
        getloadavg(la.as_mut_ptr(), 3);
    }
    la[0]
}

fn median(v: &mut [f64]) -> f64 {
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = v.len();
    if n % 2 == 0 {
        (v[n / 2 - 1] + v[n / 2]) / 2.0
    } else {
        v[n / 2]
    }
}
fn mean_sd(v: &[f64]) -> (f64, f64) {
    let n = v.len() as f64;
    let m = v.iter().sum::<f64>() / n;
    let var = v.iter().map(|x| (x - m).powi(2)).sum::<f64>() / n;
    (m, var.sqrt())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = args.get(1).cloned().unwrap_or_else(|| "data/json/twitter.json".into());
    let samples: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(240);
    let inner: usize = 4; // corpus passes per sample (raises Δcycles above PMU noise)

    let data = std::fs::read(&path).expect("read corpus");
    let bytes = data.len();
    let table = build_table();

    // ── Structural parity: both forms MUST pick identically on every byte.
    let mut mism = 0usize;
    for &b in &data {
        if seq_trial(b) != table_lookup(b, &table) {
            mism += 1;
        }
    }
    // Branch-selection histogram (evidence the Alt actually discriminates).
    let mut hist = [0u64; 8];
    for &b in &data {
        hist[seq_trial(b) as usize] += 1;
    }
    println!("# corpus={path} bytes={bytes} samples/form={samples} inner={inner}");
    println!("# structural parity mismatches = {mism}  (MUST be 0 before timing)");
    println!("# branch histogram [obj arr str num t f n NOMATCH] = {hist:?}");
    assert_eq!(mism, 0, "structural parity FAILED — forms are not equivalent");
    let load0 = loadavg();

    // Warm/touch both paths once (page-in corpus, prime i-cache).
    let mut warm = 0u64;
    for &b in &data {
        warm = warm.wrapping_add(seq_trial(b) as u64);
        warm = warm.wrapping_add(table_lookup(b, &table) as u64);
    }
    black_box(warm);

    // ── Interleaved paired sampling (A,B,A,B…) to cancel frequency drift.
    let mut cyc_seq: Vec<f64> = Vec::with_capacity(samples);
    let mut cyc_tab: Vec<f64> = Vec::with_capacity(samples);
    let denom = (inner * bytes) as f64;
    let mut sink = 0u64;

    for _ in 0..samples {
        // Form A: sequential-trial
        let c0 = read_cycles();
        let mut ck = 0u64;
        for _ in 0..inner {
            for &b in black_box(&data) {
                ck = ck.wrapping_add(seq_trial(black_box(b)) as u64);
            }
        }
        let c1 = read_cycles();
        black_box(ck);
        sink ^= ck;
        cyc_seq.push((c1 - c0) as f64 / denom);

        // Form B: dispatch-table
        let d0 = read_cycles();
        let mut ck2 = 0u64;
        for _ in 0..inner {
            for &b in black_box(&data) {
                ck2 = ck2.wrapping_add(table_lookup(black_box(b), &table) as u64);
            }
        }
        let d1 = read_cycles();
        black_box(ck2);
        sink ^= ck2;
        cyc_tab.push((d1 - d0) as f64 / denom);
    }
    let load1 = loadavg();
    black_box(sink);

    // ── Windows: split samples into 3 windows, report per-window medians.
    let w = samples / 3;
    println!("\n# per-window median cyc/B (window size {w})");
    println!("# window   seq_trial   dispatch_table   ratio(seq/tab)");
    for k in 0..3 {
        let lo = k * w;
        let hi = if k == 2 { samples } else { (k + 1) * w };
        let mut s: Vec<f64> = cyc_seq[lo..hi].to_vec();
        let mut t: Vec<f64> = cyc_tab[lo..hi].to_vec();
        let ms = median(&mut s);
        let mt = median(&mut t);
        println!("  W{}       {:.4}      {:.4}          {:.3}x", k + 1, ms, mt, ms / mt);
    }

    // ── Aggregate stats + 95% CI on the ratio (delta method).
    let (mseq, sdseq) = mean_sd(&cyc_seq);
    let (mtab, sdtab) = mean_sd(&cyc_tab);
    let mut sc = cyc_seq.clone();
    let mut tc = cyc_tab.clone();
    let med_seq = median(&mut sc);
    let med_tab = median(&mut tc);
    let n = samples as f64;
    let ratio = mseq / mtab;
    // Var(ratio)/ratio^2 ≈ (sd_seq/mean_seq)^2/n + (sd_tab/mean_tab)^2/n
    let rel_var = (sdseq / mseq).powi(2) / n + (sdtab / mtab).powi(2) / n;
    let ci_half = 1.96 * ratio * rel_var.sqrt();
    let lo = ratio - ci_half;
    let hi = ratio + ci_half;

    println!("\n── AGGREGATE (n={samples} per form) ──");
    println!("seq_trial      mean={mseq:.4} cyc/B  median={med_seq:.4}  sd={sdseq:.4}");
    println!("dispatch_table mean={mtab:.4} cyc/B  median={med_tab:.4}  sd={sdtab:.4}");
    println!("ratio seq/tab (mean-based) = {ratio:.4}x");
    println!("95% CI on ratio            = [{lo:.4}x, {hi:.4}x]");
    println!("loadavg(1m) start={load0:.2} end={load1:.2}");
    let excludes_one = lo > 1.0 || hi < 1.0;
    println!(
        "\n── VERDICT (Part B) ── 95% CI excludes 1.00x same-direction: {excludes_one}  (ratio {}1.0)",
        if ratio > 1.0 { ">" } else { "<" }
    );
}
