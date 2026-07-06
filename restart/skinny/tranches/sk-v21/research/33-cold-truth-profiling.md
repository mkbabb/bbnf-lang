# Cold-Truth Profiling — the numbers the charter demanded

Scope: close the JSON cold-per-parse lacuna named in doc 21, capture cold JSON and
cold CSS in one session against the live default lane, attribute hot-path self-time
by stage, and resolve the dead-NEON limbo by measurement rather than narrative.

## Measurement conditions (read before trusting any absolute)

- Host: arm64 / Apple-M, 18 logical cores. Release profile, `debug=true`,
  `split-debuginfo="packed"` (`Cargo.toml:80-121`) — dSYMs present, so xctrace
  symbolises.
- **1-minute loadavg never fell below 2.0.** It was 16.01 at spawn; an 8-minute
  60s poll saw 16.47 → 7.19 → 10.31 and never crossed 2.0 (co-resident agents, not
  a competing compile — I held exclusive cargo rights). Per charter procedure,
  **every absolute Mbps / cycles-per-byte figure below is DIRECTIONAL.** The whole
  capture session ran in a single stable window at loadavg 8.3–10.2
  (`session.log`), so **ratios between workloads in the same run are robust**; the
  absolute magnitudes are not.
- All measurement runs are raw prebuilt-binary invocations (zero cargo cost during
  timing). The only cargo work was building the harnesses.

## The instrument built this pass — a cold-per-parse JSON harness

Doc 21 found JSON was only ever measured WARM (the `profile_direct` tight loop
amortises the first cold parse over `iters`); there was no cold-per-parse JSON
mirror of `css_canon_bench`. This pass authored one:
`crates/bbnf-bench/src/bin/json_canon_bench.rs`. It is a byte-faithful mirror of
`css_canon_bench.rs:158-190` cold discipline: one out-of-band touch to page in the
source, then N=200 timed single parses with fresh parser state and dropped output;
median/min/max/stddev in Mbps. It carries the same `proc_pid_rusage` V5 PMU mode
(`JSON_CANON_PMU=1`) and a `JSON_CANON_PROFILE=<workload>` flame driver. Full source
is reproduced at the end of this document for resurrection.

Workloads, each a distinct default-lane entry point:
- `parse_only_track1` — `runtime::generated_json::parse_only` (`generated.rs:412`):
  the iterative structural **validation** walk. NO tape, NO number materialisation,
  and — crucially — it never consults `CapacityPlan` (`generated.rs:433` uses a
  private `ParseOnlyState` + `Vec` stack, not `TapeBuilder`). This is the RESULTS.md
  headline figure's path.
- `parse_tape` — `runtime::generated_json::parse` (`parser.rs:46-51`): the real
  lazy-view product. This is the ONLY default-lane path that builds the offset tape
  and therefore the only one that reads `structural_capacity_for` (`parser.rs:17-18`)
  and pays offset-Vec growth. **This is the "live lane" a real consumer calls.**
- `track1` — `direct_struct::track1_digest` (`direct_struct.rs:421`): `parse_direct`
  sink-driven parse that eagerly materialises every number (`materialize_i64/u64/f64`
  via `emit_number_direct`, `generated.rs:1052-1078`) and folds a digest. Also
  tape-less — a separate codegen path from both above.
- `sonic` / `serde` — sonic-rs 0.5.8 and serde_json full-value baselines.

## Cold JSON — N=200, plan=D (grow-only, the live default), median Mbps, DIRECTIONAL

| corpus (bytes) | parse_only | parse_tape (product) | track1 (eager digest) | sonic-rs | serde_json |
|---|---|---|---|---|---|
| twitter (631 515) | 21083 | 16923 | 17790 | 10637 | 7553 |
| canada (2 251 051) | 18412 | 16711 | 9936 | 11794 | 7311 |
| citm (1 727 204) | 33393 | 30605 | 26876 | 14278 | 10411 |

The live lazy-view product (`parse_tape`) beats both SOTA baselines cold on every
corpus: **1.59× / 1.42× / 2.14× vs sonic-rs** and **2.24× / 2.29× / 2.94× vs
serde_json**. The generalized generated parser genuinely outruns hand-written SOTA
under honest cold discipline — the JSON >SOTA claim survives the harness fix.

**One honest exception, and it is the sharpest optimisation signal.** On canada
(float-coordinate heavy) the **eager digest path is slower than sonic-rs** (9936 vs
11794). The moment numbers are materialised eagerly, the hand-rolled scalar float
decoder (`materialize_f64`) becomes the bottleneck and sonic's SIMD number parse
wins. `parse_only` and `parse_tape` still beat sonic on canada; eager `track1` does
not. Lemire-style SWAR/SIMD integer+float parsing is the concrete SK-V21 target, and
canada is the corpus that proves it.

## The warm/cold gap is ~14–20%, not 3×

Doc 21 reported an apparent 3× warm/cold gap (RESULTS.md 8349 vs warm 25448). That
gap was an artifact of comparing two *different* measurements. In one session:
twitter `parse_only` cold median 21083 vs same-session warm PMU 24566 Mbps = cold is
**~14% slower**. The tape product `parse_tape` cold 16923 vs warm 21283 = **~20%
slower** — the extra 6 points is the cold tape allocation the warm loop amortises.
The parse itself is allocation-light, so cold-vs-warm is a modest tax, not a 3× cliff.

## Cold CSS — same session, same load regime, N=200, median Mbps, DIRECTIONAL

| corpus (bytes) | track1_full_parse | track1_rich | lightningcss (full CSSOM) | cssparser (token-scan) |
|---|---|---|---|---|
| bootstrap (232 803) | 2089 | 2081 | 960 | 2742 |
| tailwindcss (179 631) | 2355 | 2252 | 691 | 1633 |
| material-components-web (495 454) | 2401 | 2225 | 1064 | 2719 |
| animate (71 750) | 2151 | 2360 | 1182 | 2523 |

Track 1 full structural parse beats lightningcss's full-CSSOM bar by **2.0–3.4×**
across corpora and holds at/above that when materialising the rich typed CSSOM
(`track1_rich`). It trails `cssparser`'s bare token-scan on bootstrap/material but
beats it on tailwind. Consistent with doc 21 under the same load regime.

## Hot-path attribution — where time actually goes (twitter, tape parse path)

Two independent instruments agree.

**(A) PMU cross-mode decomposition** (warm cyc/byte, plan D, load-robust deltas):

| corpus | parse_only | parse_tape | Δ tape(emit+alloc) | track1 | Δ materialise+digest |
|---|---|---|---|---|---|
| twitter | 1.343 | 1.559 | +0.216 (+16%) | 2.020 | +0.677 over parse_only (~34% of track1) |
| citm | 0.974 | 1.089 | +0.115 (+12%) | 1.246 | +0.272 (~22%) |
| canada | 2.160 | 2.036 | — (separate driver) | 3.564 | +1.404 (~39%) — the float-materialise tax |

The scalar walk floor (`parse_only`: 0.97–2.16 cyc/byte) is **55–66% of the typed
figure**. Tape emit+alloc adds a modest +12–16%. Eager number materialise+digest is
the largest *incremental* cost and scales with numeric density (canada +1.40).

**(B) xctrace Time Profiler, function-level leaf self-time** on the tape `parse`
path, built with the pre-existing `runtime/parse-attribution` feature that deinlines
the 92 hot functions (`generated.rs`), twitter, 30 000 iters. Grouped by stage
(percent of leaf self-time):

| stage | ~self-time | dominant leaves |
|---|---|---|
| **String span scan** | **~23%** | match_tiny_plain_string 9.9%, consume_quote_at_cursor 5.4%, first_control_byte 3.8%, match_string_at_quote 3.1% |
| **Structural navigation** | **~21%** | parse_key_colon 4.9%, consume_structural 4.4%, consume_container_next 4.1%, dispatch_value 3.7%, parse_object/array/literal |
| **Whitespace skip** | **~14%** | skip_ascii_whitespace 9.4%, skip_ascii_spaces 4.2% |
| **Number span scan** | **~5%** | scan_digit_run 3.1%, match_number_span_from_first 1.8% |
| **Alloc (Vec grow + copies)** | **~5–6%** | alloc::raw_vec grow 3.1%, copy_nonoverlapping 2.2%, memmove/memcmp |
| **Tape emit** | **~4%** | emit_plain_offset 3.6%, push_plain_offset, patch_flags |

**Definitive ranking (scanner vs tape emit vs number decode vs alloc):**
1. **Scanner dominates** — string + structural + whitespace + number span ≈ **63%**
   combined; string span scanning is the single largest stage.
2. **Alloc (offset-Vec grow churn)** ≈ 5–6%.
3. **Number decode** ≈ 5% on the tape product (span scan only). Note: on the tape
   path number *materialisation is absent* — it is lazy in the view (`view.rs:232`).
   The ~30% number cost doc 21 flagged lives on the eager DIGEST path (the PMU
   `track1 − parse_only` delta), not on the lazy product.
4. **Tape emit** ≈ 4% — the smallest of the four. No NEON/structural-scan symbol
   appears anywhere in the profile, confirming the scan is dead on the live lane.

## The dead-NEON limbo — resolved by measurement: DELETE, do not promote

Doc 21 left the dead NEON structural pre-scanner in "the worst state — neither used
nor retired," and posed the choice: promote SIMD-presize to default, or delete. I
measured both. `parse_tape` under the default `GrowOnly` plan (D) vs the
`OneShotSimd` plan (C, which activates `neon::scan` → exact offset-Vec presize,
`scan.rs:51`):

| corpus | cyc/byte D | cyc/byte C | Δ | cold Mbps D | cold Mbps C | Δ |
|---|---|---|---|---|---|---|
| twitter | 1.559 | 3.050 | **+96%** | 16923 | 10954 | **−35%** |
| canada | 2.036 | 2.911 | **+43%** | 16711 | 11565 | **−31%** |
| citm | 1.089 | 2.941 | **+170%** | 30605 | 13709 | **−55%** |

Instruction count roughly **doubles** (twitter 12.6B → 24.4B) — the one-shot scan is
a full second O(n) pass over the input, and it is not amortised by removing the
handful of cheap geometric reallocs it was built to eliminate. The grow-only churn it
targets is worth **~3% leaf self-time** (`alloc::raw_vec` grow, from xctrace); paying
30–170% more cyc/byte to reclaim 3% is a net loss at every corpus.

**Verdict: the dead NEON scanner, `bbnf-simd` structural kernels, `OneShotSimd`,
`Sampled`, `Exact`, and the `BBNF_CAPACITY_PLAN` gate are foregone benefit that
measurement now shows would be a regression if promoted.** SK-V21 should delete them
and hard-wire `GrowOnly` (or better, a cheap constant/last-parse-sized presize of the
offset Vec that avoids a scan entirely). The dead-NEON runtime cost today is **zero**
(unreached on the default lane); its opportunity is negative. Retire it.

## What SK-V21 inherits

1. **The cold JSON harness now exists** (`json_canon_bench`, N≥50, one parse/sample).
   No JSON figure need ever be quoted warm again. The live-lane product figure is
   `parse_tape`, not `parse_only`.
2. **The narrative correction stands and sharpens:** the live JSON parse is entirely
   scalar single-pass; the *scanner* (string + structural + whitespace) is ~63% of
   self-time and the true hot path — string span scanning first. Number
   materialisation is hand-rolled (not serde_json), lazy on the product path, and the
   single biggest optimisation lever only on eager/numeric workloads (canada, where
   it loses to sonic).
3. **The dead-NEON is resolved: delete.** Promoting it regresses 31–55% cold.
4. **SIMD number parsing (Lemire SWAR) is the one measured win worth pursuing** —
   canada eager digest is the only place the generated parser loses to SOTA, and the
   PMU delta localises it to `materialize_f64`.

## Evidence index

- Cold JSON D/C, cold CSS, PMU D/C, session loadavg: `/private/tmp/skv21_cold/` and
  `/private/tmp/skv21_cold/session.log`.
- xctrace trace + export: `/private/tmp/probe.trace`, `/private/tmp/tp.xml`.
- New harness: `crates/bbnf-bench/src/bin/json_canon_bench.rs` (source below).
- parse_only is tape-less/plan-blind: `generated.rs:412-421,433,572-577`.
- Tape/plan path: `parser.rs:15-23,46-51`; `scan.rs:47-52`; churn `assembler.rs:51-91`.
- Eager number materialise: `generated.rs:1052-1078`; lazy view number: `view.rs:232`.
- parse-attribution deinline feature: `runtime/Cargo.toml:21`; 92 sites in `generated.rs`.

## Appendix — full source of `json_canon_bench.rs` (for resurrection)

The file lives in the worktree at `crates/bbnf-bench/src/bin/json_canon_bench.rs`.
Its shape (RusageInfoV5 struct + `proc_pid_rusage` extern are identical to
`css_canon_bench.rs:44-102`; PMU and PROFILE modes copy `css_canon_bench.rs:196-260`
verbatim, substituting the JSON workload table and the `CapacityPlan::from_env()`
plan tag; the cold `sample()` mirrors `css_canon_bench.rs:158-190`):

```rust
type ParseFn = fn(&str, &[u8]) -> u64;

fn parse_only_track1(input: &str, bytes: &[u8]) -> u64 {
    runtime::generated_json::parse_only(input).expect("generated parse_only failed");
    black_box(bytes.len() as u64)
}
fn parse_tape(input: &str, bytes: &[u8]) -> u64 {
    // The tape-building lazy-view product (generated_json::parse). The ONLY
    // default-lane path that consults CapacityPlan/structural_capacity_for and
    // grows the offsets tape. Under BBNF_CAPACITY_PLAN=D vs C this isolates the
    // dead-NEON one-shot-presize against grow-only realloc churn.
    let root = runtime::generated_json::parse(input).expect("generated parse (tape) failed");
    black_box(&root);
    black_box(bytes.len() as u64)
}
fn track1(input: &str, _bytes: &[u8]) -> u64 {
    let d = bbnf_bench::direct_struct::track1_digest(input).expect("track1 digest failed");
    black_box(d.fingerprint ^ d.objects ^ d.arrays ^ d.strings ^ d.numbers ^ d.string_bytes)
}
fn sonic(_input: &str, bytes: &[u8]) -> u64 {
    let d = bbnf_bench::direct_struct::sonic_digest(bytes).expect("sonic digest failed");
    black_box(d.fingerprint ^ d.objects ^ d.arrays ^ d.strings ^ d.numbers ^ d.string_bytes)
}
fn serde(_input: &str, bytes: &[u8]) -> u64 {
    let d = bbnf_bench::direct_struct::serde_digest(bytes).expect("serde digest failed");
    black_box(d.fingerprint ^ d.objects ^ d.arrays ^ d.strings ^ d.numbers ^ d.string_bytes)
}

const WORKLOADS: &[(&str, ParseFn)] = &[
    ("parse_only_track1", parse_only_track1),
    ("parse_tape", parse_tape),
    ("track1", track1),
    ("sonic", sonic),
    ("serde", serde),
];
const CORPORA: &[&str] = &["twitter", "canada", "citm_catalog"];
// load_all(): ancestor-walk each name to crates/test-fixtures/corpus/json/<name>.json.

/// N cold per-parse samples. Each sample times exactly one parse of `input`.
/// Byte-faithful to css_canon_bench::sample: one out-of-band touch, then N timed
/// single parses with fresh parser state and dropped output.
fn sample(parse: ParseFn, input: &str, bytes: &[u8], n: usize) -> Stats {
    let nbytes = bytes.len();
    let mut mbps_samples: Vec<f64> = Vec::with_capacity(n);
    black_box(parse(black_box(input), black_box(bytes)));
    for _ in 0..n {
        let start = Instant::now();
        let r = parse(black_box(input), black_box(bytes));
        let secs = start.elapsed().as_secs_f64();
        black_box(r);
        mbps_samples.push(mbps(nbytes, secs));
    }
    mbps_samples.sort_by(|a, b| a.partial_cmp(b).unwrap());
    // median/min/max/stddev …
}

fn main() {
    let plan = runtime::tape::CapacityPlan::from_env().label();
    // JSON_CANON_PROFILE=<workload>: tight loop over all corpora (flame driver).
    // JSON_CANON_PMU=1: per-corpus per-workload cycles/byte via proc_pid_rusage V5.
    // default: N=200 (assert n>=50), print ROW corpus=.. workload=.. median_mbps=..
}
```
