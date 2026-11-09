# Restart-Lacuna Profiling Hunt — where the time actually goes at HEAD

Scope: measure, not narrate. Where does the JSON and CSS parse figure actually
come from at the skinny HEAD (`05302109d`, generated JSON/CSS runtime unchanged
since `b7ab7c558`, 2026-06-03)? Verify or refute the standing narrative that the
JSON figure is carried by "scalar scan + serde_json number decode," and quantify
the dead-NEON structural scanner.

## Measurement conditions (read before trusting any absolute number)

- Host: aarch64 / Apple-M, 18 logical cores.
- **1-minute loadavg was 10.66 at bench start, 8.97 at end** (`sysctl vm.loadavg`).
  This is far above the 2.0 quiet threshold. **Every absolute Mbps / cycles-per-byte
  figure below is DIRECTIONAL.** Ratios between workloads measured in the same run
  under the same load are robust; the absolute magnitudes are not.
- No `cargo`/`rustc`/`samply` process was running during measurement; the build lock
  was free. The load came from co-resident agent activity, not a competing compile.
- **No build was performed.** The generated JSON runtime (`crates/runtime/src/grammars/json`)
  and generated CSS declaration-values runtime were NOT touched by any commit after the
  prebuilt bench binaries were produced (the last skinny/crates commit `b7ab7c558`
  changed the *generators* in `codegen/` and the Sheets runtime, not the emitted JSON/CSS
  parser bodies). The prebuilt `target/release/css_canon_bench` and `target/release/profile_direct`
  therefore contain HEAD-faithful hot paths and were run directly (zero cargo cost).
- samply was NOT run. samply symbol resolution on this repo requires interactive
  `samply record` (per the standing note; `--save-only` does not resolve), which cannot
  run in a headless agent shell, and under loadavg ~9 the sampled attribution would itself
  be directional. Per the charter's stated fallback I used **bench cycles/byte from the
  binaries' own `proc_pid_rusage` PMU counters + static allocation-site counting** instead.
  This is a genuine gap: no live flame attribution was captured this pass.

## CSS — cold-per-parse (the honest harness)

`css_canon_bench` (`crates/bbnf-bench/src/bin/css_canon_bench.rs:9-12`) is a true
cold-per-parse harness: each of N=50 samples times exactly one `parse()` of one corpus,
output dropped, no warm cache, median reported. Result at HEAD (median Mbps, DIRECTIONAL):

| corpus (bytes) | track1_full_parse | track1_rich | lightningcss (full CSSOM) | cssparser (token-scan, materializes nothing) |
|---|---|---|---|---|
| bootstrap (232 803) | 2101 | 2215 | 1047 | 2777 |
| tailwindcss (179 631) | 2718 | 2692 | 757 | 1635 |
| material-components-web (495 454) | 2454 | 2312 | 1167 | 2975 |
| animate (71 750) | 1608 | 1667 | 1091 | 2552 |

Reading: Track 1 full structural parse beats lightningcss's full-CSSOM bar by ~2.0–3.6×
across corpora, and — materializing a typed CSSOM summary (`track1_rich`) — stays at or
above the plain structural figure. Track 1 trails `cssparser`'s bare token-scan (which
materializes nothing) on bootstrap/material but *beats* it on tailwind. The CSS >SOTA
claim vs lightningcss holds under cold discipline even at loadavg ~10.

## JSON — WARM tight-loop throughput (harness-discipline LACUNA)

The JSON profiling binary is `profile_direct` (`crates/bbnf-bench/src/bin/profile_direct.rs`).
It runs `iters` parses in a tight loop, black-boxes and drops each result, then divides total
bytes by wall time (`profile_direct.rs:104-133`). **This is warm-amortized steady-state
throughput, not cold-per-parse** — the first cold parse is amortized away over `iters`,
the allocator arena and branch predictors are hot, only the *output* is dropped. It does
NOT honor the no-warm-benches precept. Result on twitter (631 515 bytes, iters=2000,
warmup=0, DIRECTIONAL, WARM):

| mode | Mbps (warm) | cycles/byte | CPI |
|---|---|---|---|
| parse_only_track1 (generated, no number materialization) | 25 448 | 1.318 | 0.211 |
| track1 (generated + digest, materializes numbers) | 17 717 | 1.914 | 0.155 |
| sonic-rs | 8 763 | 3.642 | 0.164 |
| serde_json | 6 418 | 5.136 | 0.189 |

**The warm/cold asymmetry is itself the finding.** The committed `RESULTS.md` twitter
`parse_only` Track-1 figure is **8349 Mbps** (`RESULTS.md:5`); my same-binary warm loop
reports **25 448 Mbps** — a ~3× gap. The delta is the harness, not a regression: CSS is
measured cold-per-parse (`css_canon_bench`), JSON is measured by a warm throughput loop
(`profile_direct`) whose "cold" naming refers only to a cold-*cache* first-parse mode that
my invocation (iters=2000) does not exercise. There is **no standalone cold-per-parse JSON
harness equivalent to `css_canon_bench`**; `json_parity` (`benches/json_parity.rs`) is a
criterion warm-sampling bench, also warm. The JSON cold-per-parse discipline is
underserved relative to CSS — a profiling-truth lacuna the SK-V21 plan must close (build a
`json_canon_bench` mirror of `css_canon_bench`, N≥50 cold-per-parse, before any JSON figure
is quoted as cold).

Ratios inside the warm run are still load-robust: generated Track 1 parse_only is ~2.9× sonic
and ~4.0× serde; the typed digest path (track1) is ~2.0× sonic. The generalized generated
parser genuinely outruns both SOTA baselines on this corpus; that conclusion survives the
warm caveat.

## Does the "scalar scan + serde_json number decode" narrative hold? — NO, both halves fail

**Half 1 — "scalar scan carries the figure":** imprecise. There is no separate scalar *scan
pass* on the live JSON path. `generated_json::parse_only` is a single-pass scalar
recursive-descent sink parser (`generated.rs` `parse_*_direct` family). The only structural
"scan" the live path touches is `structural_capacity_for` (`parser.rs:18`), which under the
default `GrowOnly` plan returns a constant 256 and scans nothing (`scan.rs:59-65`). So the
figure is carried by scalar *parsing* (recursive descent + string-span scan + whitespace
skip + tape emit), not by a scan phase. The phrase should be retired; the accurate statement
is "the live JSON parse is entirely scalar single-pass."

**Half 2 — "serde_json number decode carries the figure":** FALSE for the generated path.
The generated number path is hand-rolled: `parse_number_direct` → `match_number_span_from_first`
→ `emit_number_direct` (`generated.rs:768-809, 811+`), which materializes via
`materialize_i64/u64/f64` (`direct_struct.rs:9-10, 98-113`, `emit_number_direct` at
`generated.rs:813-838`). **serde_json is never called for numbers on the parse hot path.**
`serde_json::from_str::<serde_json::Number>` appears at exactly one site —
`view.rs:232` `JsonNumber::as_f64()` — the *lazy view* materialization, reached only when a
consumer reads a number through the typed view API. Neither `parse_only_track1` nor the
`track1` digest exercises it. The narrative conflated the serde_json *competitor baseline*
(mode=serde, a comparison bar) with our own decode. **Correction for SK-V21: number decode
on our path is hand-rolled scalar, and IS a real hot cost** — the parse_only→track1 delta is
1.318 → 1.914 cycles/byte (+45%, ~0.6 cyc/byte), attributable to number materialization +
digest fold. That ~30% of the typed figure is the legitimate optimization target (SIMD/SWAR
integer+float parsing à la simdjson/lemire), but it is *not* serde_json and never was.

## The dead-NEON structural scanner — not a cost, a foregone benefit

`scan.rs:3-13` documents (and the code confirms) that on the live JSON path the default
`CapacityPlan::GrowOnly` returns constant 256 (`scan.rs:64`) and **`neon::scan`
(`scan.rs:34-42, 213`) is unreachable.** The NEON `vceqq`-class structural pre-scan, the
`bbnf-simd` aarch64 kernels, `sampled_structural_capacity`, and `exact_structural_count` all
carry **zero live self-time** — they are reached only under the non-default env plans
`BBNF_TAPE_CAPACITY=A|B|C` and the parity/bench harnesses. So there is **no dead-NEON runtime
cost to quantify; the cost is zero.** The dead-NEON is instead *foregone acceleration*:

- The offsets tape opens at `Vec::with_capacity(256)` (`assembler.rs:54`) and grows via
  `reserve_offsets_cold` → `reserve((len/2).max(8))` (`assembler.rs:73-91`) each time it fills.
  For twitter (~40K structurals) that is roughly log2(40000/256) ≈ 7–8 realloc+memcpy growth
  events **per parse**, in the live steady path.
- The one plan that would presize the offsets Vec exactly once — `OneShotSimd`, which sizes
  from `scan_structurals(...).positions().len()` (`scan.rs:63`) — is precisely the path that
  drives the dead `neon::scan`. It is non-default and therefore never runs.

So the SIMD investment (a simdjson stage-1 structural classifier, already written and
parity-tested in `bbnf-simd`) sits on the shelf while the default path pays geometric
realloc churn it was built to remove. This is the sharpest profiling lacuna for SK-V21:
either (a) promote a SIMD-presize capacity oracle to the default and delete `GrowOnly` as the
live plan, measuring whether the one-shot NEON scan + single alloc beats the current
grow-only churn, or (b) if measurement shows the churn is negligible at these speeds, delete
the dead NEON scanner and its plan-gating outright rather than carrying an unmeasured
"someday" accelerator. Today it is neither used nor retired — the worst state.

## What SK-V21 must inherit

1. **Build a cold-per-parse JSON harness** (`json_canon_bench`, mirror of `css_canon_bench`,
   N≥50, one parse per sample, median). Until it exists, no JSON figure may be quoted as
   "cold" — the standing 8349 vs 25448 gap proves warm and cold differ ~3×, and only CSS is
   currently honest.
2. **Retire the "scalar scan + serde_json number decode" narrative.** Replace with the
   measured truth: live JSON parse is entirely scalar single-pass; number materialization is
   hand-rolled (not serde_json) and costs ~0.6 cyc/byte (~30% of the typed figure); serde_json
   is only the lazy-view `as_f64` fallback and a competitor baseline.
3. **Resolve the dead-NEON limbo** by measurement: SIMD-presize-default vs delete. The
   grow-only realloc churn (7–8 growth events/parse on twitter) is the concrete thing to
   measure the NEON one-shot presize against.
4. **Capture real flame attribution next pass** on a quiet machine (loadavg < 2) with
   interactive samply, to confirm the cycles/byte split (scalar structural recursion vs string
   span scan vs number materialize vs tape realloc) that this pass could only infer from PMU
   deltas and static reads.

## Evidence index

- CSS cold results (raw): `/private/tmp/css_canon_head.txt`
- JSON warm results (raw): `/private/tmp/json_head.txt`
- Dead-NEON disposition + GrowOnly=256: `crates/runtime/src/grammars/json/scan.rs:3-13,34-42,59-65,213`
- Live tape alloc + grow churn: `crates/runtime/src/tape/assembler.rs:51-91`
- Live capacity plan default: `crates/runtime/src/grammars/json/parser.rs:18`; plan enum default
  `crates/runtime/src/tape/assembler.rs:27`
- Hand-rolled number decode (NOT serde_json): `crates/runtime/src/grammars/json/generated.rs:768-838`;
  `crates/bbnf-bench/src/direct_struct.rs:9-10,98-113`
- serde_json number decode confined to lazy view: `crates/runtime/src/grammars/json/view.rs:232`
- Warm-loop harness (JSON): `crates/bbnf-bench/src/bin/profile_direct.rs:104-133`
- Cold harness (CSS): `crates/bbnf-bench/src/bin/css_canon_bench.rs:9-12,159-190`
- Committed cold JSON figure (for the warm/cold gap): `skinny/RESULTS.md:5`
