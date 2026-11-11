//! LOCK-5 pass-3 PROTO-1 (over-approx) — pt-scratch probe.
//!
//! (a-crossval) The DFA start-state class set (over-approx first(C)) for every
//!   corpus leaf, read straight off the parse-that anchored `Dfa` start row
//!   (state 0). This is the brief's literal over-approx source; it is compared
//!   in the report to the skinny test's fixed-up structural analyzer.
//! (c) Cold-paired direct membership scan vs `Dfa::find_at` on a rare-class leaf
//!   (`[0-9a-fA-F]{6}`) and a dense-class leaf (`[a-z]+`): N reps, paired same
//!   binary, correctness (tape/span identity) asserted at setup, then timed.

use std::hint::black_box;
use std::time::{Duration, Instant};

use bbnf_regex::{Dfa, DEAD};

/// Over-approx first(C): every byte on which the anchored DFA start state (0)
/// has a live (non-DEAD) transition. Definitionally the DFA start-state class
/// set. `None` if the pattern does not compile (⇒ skinny test over-approxes to
/// the full alphabet, which is strictly sound).
fn dfa_start_first(pattern: &str) -> Option<Vec<u8>> {
    let dfa = Dfa::compile(pattern)?;
    let num_cls = dfa.num_classes as usize;
    let mut out = Vec::new();
    for b in 0u8..=255 {
        let class = dfa.byte_classes[b as usize] as usize;
        let next = dfa.flat_transitions[class]; // state 0 row: 0*num_cls + class
        let _ = num_cls;
        if next != DEAD {
            out.push(b);
        }
    }
    Some(out)
}

fn show(bytes: &[u8]) -> String {
    let mut parts = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let start = bytes[i];
        let mut end = start;
        while i + 1 < bytes.len() && bytes[i + 1] == end + 1 {
            end = bytes[i + 1];
            i += 1;
        }
        let r = |b: u8| match b {
            b' ' => "SP".to_string(),
            b'\t' => "\\t".to_string(),
            b'\n' => "\\n".to_string(),
            b'\r' => "\\r".to_string(),
            0x21..=0x7e => (b as char).to_string(),
            o => format!("0x{o:02x}"),
        };
        if start == end {
            parts.push(r(start));
        } else {
            parts.push(format!("{}-{}", r(start), r(end)));
        }
        i += 1;
    }
    format!("[{}] (|{}|)", parts.join(","), bytes.len())
}

// ── direct membership scans (what 5D lowers a ByteClass leaf to) ──────────────
#[inline]
fn scan_lower(b: &[u8]) -> Option<usize> {
    let mut i = 0;
    while i < b.len() && b[i].is_ascii_lowercase() {
        i += 1;
    }
    if i > 0 {
        Some(i)
    } else {
        None
    }
}

#[inline]
fn scan_hex6(b: &[u8]) -> Option<usize> {
    if b.len() < 6 {
        return None;
    }
    let mut i = 0;
    while i < 6 {
        if !b[i].is_ascii_hexdigit() {
            return None;
        }
        i += 1;
    }
    Some(6)
}

/// deterministic LCG — reproducible inputs, no rand dep.
struct Lcg(u64);
impl Lcg {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        self.0
    }
    fn byte(&mut self) -> u8 {
        (self.next() >> 33) as u8
    }
}

fn median(mut v: Vec<f64>) -> f64 {
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = v.len();
    if n == 0 {
        0.0
    } else if n % 2 == 1 {
        v[n / 2]
    } else {
        0.5 * (v[n / 2 - 1] + v[n / 2])
    }
}

fn cold_pair<F>(name: &str, inputs: &[Vec<u8>], direct: F, dfa: &Dfa)
where
    F: Fn(&[u8]) -> Option<usize>,
{
    // ── correctness / tape identity at setup ──
    let mut identical = 0usize;
    for inp in inputs {
        let a = direct(inp);
        let b = dfa.find_at(inp, 0);
        assert_eq!(
            a, b,
            "TAPE MISMATCH on {name}: direct={a:?} dfa={b:?} input={:?}",
            String::from_utf8_lossy(inp)
        );
        identical += 1;
    }
    println!("  [{name}] tape byte-identical (matched span): {identical}/{}", inputs.len());

    // ── timed reps, paired ──
    let reps = 240usize; // ≥200
    let mut ratios = Vec::with_capacity(reps);
    let mut td_tot = Duration::ZERO;
    let mut tf_tot = Duration::ZERO;
    let mut sink = 0u64;
    for _ in 0..reps {
        let s = Instant::now();
        for inp in inputs {
            if let Some(e) = direct(black_box(inp)) {
                sink = sink.wrapping_add(e as u64);
            }
        }
        let td = s.elapsed();

        let s = Instant::now();
        for inp in inputs {
            if let Some(e) = dfa.find_at(black_box(inp), 0) {
                sink = sink.wrapping_add(e as u64);
            }
        }
        let tf = s.elapsed();

        td_tot += td;
        tf_tot += tf;
        // ratio = direct speed / dfa speed = tf / td  (>1 ⇒ direct faster)
        ratios.push(tf.as_secs_f64() / td.as_secs_f64());
    }
    black_box(sink);
    let med = median(ratios);
    let d_ns = td_tot.as_nanos() as f64 / (reps * inputs.len()) as f64;
    let f_ns = tf_tot.as_nanos() as f64 / (reps * inputs.len()) as f64;
    println!(
        "  [{name}] direct={d_ns:.2} ns/scan  Dfa::find={f_ns:.2} ns/scan  \
         median(direct_speed/dfa_speed)={med:.3}x  (reps={reps}, inputs={})",
        inputs.len()
    );
}

fn main() {
    println!("=== (a-crossval) DFA start-state class set — over-approx first(C) ===");
    let leaves: &[(&str, &str)] = &[
        ("json::ws", r"[ \t\n\r]*"),
        ("json::number", r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"),
        ("json::string", r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#),
        ("sheets::number", r"(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?"),
        ("sheets::string", r#""([^"]|"")*""#),
        ("sheets::boolean_true", r"[tT][rR][uU][eE]"),
        ("sheets::boolean_false", r"[fF][aA][lL][sS][eE]"),
        ("sheets::sheet_prefix", r"'(?:[^']|'')*'!|[A-Za-z_]\w*!"),
        ("sheets::cell_ref", r"\$?[A-Za-z]{1,3}\$?\d+"),
        ("sheets::range_end_num", r"\$?\d+"),
        ("sheets::range_end_alpha", r"\$?[A-Za-z]{1,3}"),
        ("sheets::identifier", r"[A-Za-z_][A-Za-z0-9_.]*"),
        ("sheets::let_call", r"[lL][eE][tT]\("),
        ("sheets::lambda_call", r"[lL][aA][mM][bB][dD][aA]\("),
        ("sheets::formula", r"=?"),
        ("counterexample::word", r"[a-z]+"),
    ];
    for (name, pat) in leaves {
        match dfa_start_first(pat) {
            Some(bytes) => println!("  {name:<26} /{pat}/  first={}", show(&bytes)),
            None => println!("  {name:<26} /{pat}/  DID NOT COMPILE ⇒ over-approx = ALL 256 (sound)"),
        }
    }

    println!("\n=== (c) cold-paired direct-scan vs Dfa::find ===");
    println!("loadavg: {}", std::fs::read_to_string("/proc/loadavg").unwrap_or_else(|_| "n/a (darwin)".into()));

    // rare-class leaf: [0-9a-fA-F]{6}
    let hex = Dfa::compile(r"[0-9a-fA-F]{6}").expect("hex6 compiles");
    let mut rng = Lcg(0x1234_5678_9abc_def0);
    let hexset = b"0123456789abcdefABCDEF";
    let mut hex_inputs: Vec<Vec<u8>> = Vec::new();
    for k in 0..256 {
        // mostly-valid hex strings; sprinkle non-hex tails and short ones
        let len = 6 + (rng.byte() % 4) as usize;
        let mut s = Vec::with_capacity(len);
        for j in 0..len {
            if k % 7 == 0 && j == 3 {
                s.push(b'z'); // force a non-match sometimes (rare-class reject)
            } else {
                s.push(hexset[(rng.byte() as usize) % hexset.len()]);
            }
        }
        hex_inputs.push(s);
    }
    cold_pair("hex6[rare]", &hex_inputs, scan_hex6, &hex);

    // dense-class leaf: [a-z]+
    let lower = Dfa::compile(r"[a-z]+").expect("lower compiles");
    let mut low_inputs: Vec<Vec<u8>> = Vec::new();
    for _ in 0..256 {
        let len = 1 + (rng.byte() % 24) as usize;
        let mut s = Vec::with_capacity(len + 1);
        for _ in 0..len {
            s.push(b'a' + (rng.byte() % 26));
        }
        s.push(b'0'); // terminator outside class (leftmost-longest boundary)
        low_inputs.push(s);
    }
    cold_pair("lower[dense]", &low_inputs, scan_lower, &lower);
}
