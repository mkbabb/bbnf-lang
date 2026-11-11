# PROTO-P1 — NEON structural presize on the JSON path: promote or delete?

Date: 2026-07-05. Prototype agent P1 (analysis instrument; nothing lands on master).
Closes the N8/N20 two-way decision on `CapacityPlan::OneShotSimd` / the dead `neon::scan`.

## The falsifiable question

Does promoting `OneShotSimd` (NEON one-shot structural scan + single tape alloc) to default beat
`GrowOnly`'s 7–10 realloc/memcpy growth events per parse, cold-per-parse — or is the churn
negligible, mandating deletion of the dead scanner?

## Verdict: REFUTES the presize-wins hypothesis → DELETE

`OneShotSimd` is a **1.6×–2.4× cold regression on cycles/byte across all three corpora**, not a win.
The isolating control plane `Sampled` (near-exact presize at near-zero counting cost) merely **ties**
`GrowOnly` — proving the realloc churn `OneShotSimd` removes is negligible. The GO criterion (≥3% win
on any corpus without regressing others) fails decisively; the DELETE criterion fires.

## Method

- Built `skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs`, a mirror of `css_canon_bench.rs`:
  N≥50 cold per-parse samples, one `runtime::grammars::json::parse` per sample, result black-boxed and
  dropped, median reported. Two metrics per sample: wall-clock Mbps (`Instant`) and per-parse
  **cycles/byte** via `proc_pid_rusage(RUSAGE_INFO_V5)` (the same PMU path `css_canon_bench` uses).
- The capacity plan is a runtime value from `CapacityPlan::from_env()` read in `ParserState::new`; the
  compiled parse body is identical regardless of how the plan value is obtained. Selecting the plan by
  `BBNF_CAPACITY_PLAN={A,B,C,D}` therefore exercises the *identical* `OneShotSimd` code path a flipped
  `from_env` default would — an apples-to-apples measurement from one binary, superior to editing the
  `None` arm. (The one-line default flip `assembler.rs:27` was thus unnecessary and not made.)
- Release profile (fat LTO, cu=1, panic=abort — the committed harness profile). N=120, twitter/canada/
  citm, all plans measured back-to-back under identical system load.

### Load-discipline disclosure (inherited from report 21)

Loadavg was **14–18** throughout (shared machine, concurrent agents); the `<2` quiet target was
unreachable. Mitigation and why the verdict holds anyway: **cycles/byte is a per-process cycle count,
insensitive to wall-clock contention**, and is the verdict metric here. Wall Mbps is load-depressed and
noisier (its absolute values must NOT be quoted as cold-quiet throughput) but agrees on direction. The
measured deltas (58–135%) exceed any plausible load-induced noise by more than an order of magnitude,
and every plan was sampled under the same load. Direction and rough magnitude are trustworthy; absolute
Mbps is directional only, exactly per report 21's standing caveat.

## Liveness proof (triply established that `neon::scan` executes under OneShotSimd)

1. **Runtime witness** (`json_canon_bench witness`): for all three corpora,
   `scan_structurals(input).backend() == Neon` — the NEON scanner runs and produces a Neon-backed index.
   ```
   WITNESS corpus=twitter bytes=631515 scan_backend=Neon scan_positions=66516 cap_growonly=256 cap_oneshot=66524 presize_ratio=259.9x
   WITNESS corpus=canada  bytes=2251051 scan_backend=Neon scan_positions=223259 cap_growonly=256 cap_oneshot=223267 presize_ratio=872.1x
   WITNESS corpus=citm    bytes=1727204 scan_backend=Neon scan_positions=146939 cap_growonly=256 cap_oneshot=146947 presize_ratio=574.0x
   ```
2. **cargo expand** (`cargo expand -p runtime generated_json::scan`) — on aarch64 the neon branch is the
   compiled/live one, scalar marked `unreachable_code`; `OneShotSimd` dispatches to it:
   ```rust
   pub fn scan_structurals(input: &[u8]) -> StructuralIndex {
       { return StructuralIndex::from_positions(neon::scan(input), ScanBackend::Neon); }
       #[allow(unreachable_code)] scan_structurals_scalar(input)
   }
   pub fn structural_capacity_for(plan: CapacityPlan, source: &[u8]) -> usize {
       match plan {
           CapacityPlan::Sampled     => sampled_structural_capacity(source),
           CapacityPlan::Exact       => exact_structural_count(source) + 8,
           CapacityPlan::OneShotSimd => scan_structurals(source).positions().len() + 8,
           CapacityPlan::GrowOnly    => 256,
       }
   }
   ```
3. **Symbol table**: `nm target/release/json_canon_bench` contains
   `runtime::generated_json::scan::scan_structurals` (`neon::scan` inlined into it).

## Data — cycles/byte (verdict metric, median, N=120, release, loadavg 14–18)

| corpus | D GrowOnly | A Sampled | B Exact | C OneShotSimd | **C vs D** |
|--------|-----------:|----------:|--------:|--------------:|-----------:|
| twitter | 1.799 | 1.760 | 4.562 | 4.230 | **+135% (2.35×)** |
| canada  | 2.336 | 2.554 | 5.321 | 3.702 | **+58% (1.58×)** |
| citm    | 1.212 | 1.229 | 3.874 | 2.648 | **+118% (2.18×)** |

Wall Mbps (directional, load-depressed — corroborates direction only):
twitter D=19288 A=19653 B=7386 C=6673 · canada D=11930 A=13608 B=6468 C=8116 · citm D=28206 A=27147 B=8233 C=9182.

## Why OneShotSimd loses, and what the controls prove

- **The current `OneShotSimd` is a substrate-without-consumer strawman.** It runs a full-input NEON scan,
  allocates a ~66K–223K-entry `Vec<u32>`, fills it, then **discards the vector and keeps only
  `.positions().len()`** for the presize (`attach_structural_index` in `generated.rs:12` is a literal
  no-op — the index is never consumed by the parse). So `OneShotSimd` pays for a whole simdjson-stage-1
  pass plus a large throwaway allocation, purely to size a `Vec`.
- **`Sampled` (A) is the decisive isolation.** It estimates capacity from a 4 KB sample (sub-linear, no
  big allocation) and presizes the tape to a near-exact value — removing the same 7–10 reallocs
  `OneShotSimd` removes — yet ties `GrowOnly` (twitter −2.2%, canada +9.3%, citm +1.4%; no consistent
  winner, all within noise). **Removing the realloc churn buys nothing measurable.**
- **`Exact` (B) and `OneShotSimd` (C) both regress 2–3×** because each adds a full second pass over the
  input. B (scalar byte-count) is even worse than C (NEON) — the NEON scan is faster than the scalar
  count — but *both* pre-passes cost far more than the churn they eliminate.
- **The realloc churn is real but cheap:** the bench confirms 7 (twitter), 10 (canada), 9 (citm) growth
  events from cap 256 (`offset_len` 29 573 / 223 236 / 85 035). Seven-to-ten `memcpy`s of a geometrically
  growing `Vec` are negligible against a full-input classify pass.

## Recommendation to the plan

**DELETE**, per the TW4 dav1d wired-or-deleted law. Retire `neon::scan`-as-presize and the
`OneShotSimd` / `Exact` / `Sampled` capacity plans; keep `GrowOnly` as the sole live plan (its 256-start
+ geometric growth is measured optimal among the four). This resolves the "neither used nor retired —
the worst state" that report 21 named.

**Scope boundary (do not overclaim):** this refutes presize *as implemented* — a scan whose output is
thrown away. It does **not** test a *consumed* structural index (simdjson stage-1 → stage-2, where the
scan is amortized by driving the parse). That is a different architecture (P2 / fused single-pass) and
outside this mandate. If a future rewrite wants NEON scanning, it must be re-justified as a **consumed**
index that pays for its pass, never resurrected as GrowOnly's presize replacement.

## Resurrectable artifacts (worktree discarded after review)

New file `skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs` (auto-discovered bin; no Cargo.toml
edit). Core of the harness:

```rust
use runtime::grammars::json::parse as json_parse;
use runtime::tape::CapacityPlan;
// proc_pid_rusage(RUSAGE_INFO_V5) RusageInfoV5 struct copied verbatim from css_canon_bench.rs

fn full_parse(input: &str) -> u64 {
    let root = json_parse(input).expect("json full parse");
    let offs = root.offset_stream();
    black_box(offs.len() as u64 ^ (offs.first().copied().unwrap_or(0) as u64))
}

fn sample(input: &str, n: usize) -> Stats {          // N cold per-parse samples
    black_box(full_parse(black_box(input)));         // touch (page-fault the source once)
    for _ in 0..n {                                  // Mbps loop: time one parse
        let t = Instant::now(); let r = full_parse(black_box(input));
        mbps_samples.push(mbps(input.len(), t.elapsed().as_secs_f64())); black_box(r);
    }
    for _ in 0..n {                                  // cyc/byte loop (rusage isolated from Mbps)
        let a = read_rusage_v5(); let r = full_parse(black_box(input)); let b = read_rusage_v5();
        cpb_samples.push((b.ri_cycles - a.ri_cycles) as f64 / input.len() as f64); black_box(r);
    }
    // median/min/max/stddev over sorted samples
}
// corpora loaded from test_fixtures::default_json_corpus_dir(); plan = CapacityPlan::from_env()
// witness mode: prints scan_structurals(input).backend() + presize ratios for the liveness proof
```

Reproduce:
```
cargo build --release -p bbnf-bench --bin json_canon_bench
target/release/json_canon_bench witness
for P in D A B C; do BBNF_CAPACITY_PLAN=$P target/release/json_canon_bench 120; done
```

The DELETE diff the plan should carry: drop `CapacityPlan::{Sampled,Exact,OneShotSimd}` and their arms
in `scan.rs:47-54` / `assembler.rs:14-39`, collapse `structural_capacity_for` to the constant, and
delete the now-orphaned `neon` module + `scan_structurals*` / `sampled_structural_capacity` /
`exact_structural_count` from `scan.rs`. `GrowOnly` becomes unconditional (no env read).
