# SK-V3 Wave 2 Prototype — Eventcursor Dispatch Falsifiability Report

Date: 2026-05-12
Branch: `master` (uncommitted V9.4 baseline)
Worktree path proposed in spec: `/Users/mkbabb/Programming/bbnf-lang/skinny-eventcursor/` — *not used*, because the V9.4 capacity-plan refactor is uncommitted on `master` and the worktree branch checkout (8f2ee399) does not include it. The prototype lives in the live worktree behind the new `runtime/eventcursor` feature flag so the comparison is honest against the live V9.4 baseline.
Hardware: M5 Max (mimalloc global allocator, release profile, `lto=fat`, `codegen-units=1`).

## Verdict

**The 0.18–0.22 cycles/byte projection (18–22 GiB/s on `random.json`) is REFUTED at this implementation level.**  Across all six measured corpora the eventcursor prototype is *slower* than the legacy `generated` dispatch path:

| Corpus | Baseline (Mbps) | Prototype (Mbps) | Ratio | Verdict |
|---|---:|---:|---:|---|
| random | 1641 | 1280 | 0.78× | regression |
| unicode_escapes | 1950 | 1744 | 0.89× | regression |
| update-center | 2481 | 1559 | 0.63× | regression |
| twitter | 3152 | 2194 | 0.70× | regression |
| citm_catalog | 3686 | 2588 | 0.70× | regression |
| canada | 2145 | 1392 | 0.65× | regression |

Median-throughput, 200–5700 measured iterations per corpus, mimalloc, identical bench harness (`crates/runtime/examples/wave2_bench.rs`).  Falsifiability gate row (random.json): **1280 Mbps prototype vs. 15 000 Mbps required to pass the lower threshold → refuted by an order of magnitude.**

Throughput targets cited in the task brief (current 9 370 Mbps, projected 18 000–22 000 Mbps) do not match this baseline.  This baseline measures the *V9.4 tape-first* `runtime::generated_json::parse` Track 1 (the same path criterion measures as `track1_generated`).  Cached criterion estimates on the same path agree to within 1 % (random 1654 vs. 1641 Mbps).  The 9 370 Mbps figure cited in the brief is either lazy-mode-specific or measured under a different harness; it should be reconciled before any further Wave 2 work.

## Scope

The task spec carried two scopes (full prototype 60 min, mask + LUT only 35 min).  This prototype landed at the **scoped-down** level *and somewhat past it*:

| Step | Spec | Done? |
|---|---|---|
| 1. Materialise structural mask in `attach_structural_index` | full | partial — whitespace mask only; structural/value-byte mask deferred |
| 2. Replace `parse_value_at` dispatch with `VALUE_CLASS_LUT[byte]` jump table | full | yes |
| 3. Bounds-check elision via ptr/end sentinel | full | no — bounds-checks remain; cursor still `usize` |
| 4. Inline strategy revision (`#[inline(never)]` cold paths) | full | no |
| 5. Measurement | required | yes |
| 6. Falsifiability gate | required | refuted |

The implementation lives in two places:

* `crates/runtime/src/grammars/json/generated_eventcursor.rs` (new, 360 lines): mask-driven `skip_ws`, NEON `vshrn` movemask via `vceqq_u8` for `' '|'\t'|'\n'|'\r'`, dense u4 `VALUE_CLASS_LUT[256]` jump table.  Forwards to legacy `parse_string`/`parse_number` semantics.
* `crates/runtime/src/grammars/json/parser.rs` — adds `ws_bitmap: Option<Box<[u64]>>` field to `ParserState`; `parse()` entry-point routes to either path via `cfg(feature = "eventcursor")`.
* `crates/runtime/Cargo.toml` — new `eventcursor` cargo feature (off by default).
* `crates/runtime/examples/wave2_bench.rs` — standalone median-min throughput probe shared by both modes.

## Correctness

`xtask::parity-oracle` run with `runtime/eventcursor` enabled passes all sixteen corpora against `serde_json` structural-match (random, unicode_escapes, unicode_basic, update-center, twitter, citm_catalog, canada, mesh, apache_builds, github_events, instruments, numbers, distinct_values, gsoc-2018, marine_ik, unicode_mixed).  All five runtime lib-tests pass under `--features eventcursor`.

## Per-corpus measurement detail

CSV outputs at `profile/wave2-prototype/baseline.csv` and `profile/wave2-prototype/prototype.csv`.  Both used identical bench harness, mimalloc, `--release`, ax-iter scale of iterations (auto-targeting ~1.5 s per corpus).

```
mode,corpus,bytes,iters,median_ns,mean_ns,mbps_median,mbps_mean
baseline,random,510476,4498,311125,324746,1641,1572
baseline,unicode_escapes,1050797,2497,538750,559912,1950,1877
baseline,update-center,533178,5303,214875,220550,2481,2417
baseline,twitter,631515,5759,200375,205062,3152,3080
baseline,citm_catalog,1727204,2995,468625,482274,3686,3581
baseline,canada,2251051,1372,1049250,1076531,2145,2091
eventcursor,random,510476,3494,398834,413191,1280,1235
eventcursor,unicode_escapes,1050797,2008,602541,625725,1744,1679
eventcursor,update-center,533178,3675,341958,352270,1559,1514
eventcursor,twitter,631515,3821,287875,296043,2194,2133
eventcursor,citm_catalog,1727204,1720,667375,685726,2588,2519
eventcursor,canada,2251051,899,1616750,1657788,1392,1358
```

## Self-time attribution (`samply --rate 19999 --unstable-presymbolicate`)

Baseline `random.json` (34 077 samples):

| Symbol | Self-time |
|---|---:|
| `runtime::generated_json::generated::parse_value_at` | **78.82 %** |
| `core::str::converts::from_utf8` (bench-harness re-validation overhead) | 20.13 % |
| `wave2_bench::run_once` | 0.97 % |

Prototype `random.json` (38 392 samples):

| Symbol | Self-time |
|---|---:|
| `runtime::generated_json::generated_eventcursor::parse_value_at` | **71.83 %** |
| `core::str::converts::from_utf8` | 16.96 % |
| `wave2_bench::run_once` | 11.03 % |
| `__bzero` + `_mi_malloc_generic` + `mi_theap_malloc_zero_aligned_at_overalloc` | 0.18 % |

The dispatch hub continues to dominate self-time in both builds (cyclic refusal to lift below 70 %), but the prototype additionally pays **~10 % in `run_once`** — this is `attach_structural_index → build_ws_bitmap → vec![0u64; total_chunks]` plus the NEON scan that builds the per-chunk mask.  On compact JSON (random.json has ~0.2 % whitespace) the prototype amortises an allocation and a full source-sized scan *before* the parser ever runs, against a baseline whose `skip_json_whitespace` falls through on the very first byte.

Raw profiles at `profile/wave2-prototype/{baseline,prototype}-random.json.gz` (+ `.syms.json` sidecars).

## Instruction-count + branch density

`otool -tV` on the linked example binaries:

| Metric | Baseline `parse_value_at` | Prototype `parse_value_at` | Δ |
|---|---:|---:|---:|
| Instructions (mega-symbol body) | 1827 | 2564 | +40 % |
| Branch instructions (`b.`, `cb`, `tb`, `b`) | 406 | 547 | +35 % |

The prototype's hub is *larger*, not smaller.  The reason is the inlined `skip_ws_mask_at` wrapping: every call site that previously dispatched to the scalar `skip_json_whitespace` now inlines a chunk-index lookup (`cursor / 64`, `cursor % 64`, bounds vs. `bitmap.len()`, bit-shift, `trailing_zeros`, `min(n)`) under `#[inline(always)]`.  Where the scalar path was a single conditional load + branch, the mask path is a six-operation arithmetic-bit sequence — and it executes whether or not there is whitespace to skip.

## Residual bottleneck — why the projection is refuted at this implementation level

1. **No reduction in source-byte loads.**  The original 7-arm `match byte` *did* compile to a binary cmp-cascade, but each cmp is single-cycle and 5–6 of them in dependency-broken chains hide perfectly behind the load latency.  Replacing it with `VALUE_CLASS_LUT[byte]` saves ~2 cmps but adds one dependent load (LUT lookup) and a dependent jump-table indirect branch — net wash.  LLVM, in fact, already emits a jump table for the baseline match on `aarch64` once the dense-range optimization kicks in for the `b'0'..=b'9'` arm (visible in the disassembly).

2. **Whitespace-mask amortisation is negative on compact JSON.**  random.json, unicode_escapes, twitter, citm_catalog, canada — none of these have meaningful inter-token whitespace; mask materialisation is a fixed cost paid per parse with no per-call payoff.  The legacy `skip_json_whitespace` is already u64-vectorised inside `parse-that-regex` and exits within one iteration of its inner loop on the average call.

3. **Bounds-check density was not the dominant bottleneck.**  The Wave 1 audit's "67 b.hs per `parse_value_at`" count is *not* what the profiler attributes time to.  The branches are on already-cached cursor positions; mispredictions are rare because the trace-driven dispatch hub has very regular structure.  Eliminating those branches via ptr/end sentinel (Step 3 — not implemented here) would shrink the function by maybe 100 instructions but not change the dependency graph that limits IPC.

4. **The 0.18–0.22 c/B projection rests on assumptions not borne out by this measurement.**  Hitting 18+ GiB/s on M5 Max requires *not* having `parse_value_at` dominate at 70+ % self-time at all — it requires the dispatch to overlap with classifier work, the way simdjson's two-stage pipeline does.  That demands a *separate* classifier pass producing a structural-index event stream + a vectorised string-body scan, not a mask-driven dispatch hub that still reads one byte per call.

## What the data implies for V3 integration

1. **Do not commit the eventcursor path as-is.**  Every corpus regresses by 11–37 %.

2. **Re-evaluate the SK-V3 Wave 2 hypothesis.**  The bottleneck attribution in the Wave 1 packet — "dispatch byte-load is 98 % self-time, so a mask + LUT will recover 2.3-2.8×" — does not survive contact with measurement.  The byte-load cannot be the bottleneck if the binary cmp-cascade after the load is already well-predicted; the *function* dominates self-time because *all the work* is in the function, not because the dispatch byte is the slow operation.  The next-step prescription should rest on a two-stage pipeline (event stream first, dispatch second) rather than on optimising the dispatch hub in isolation.

3. **Reconcile the baseline number.**  9 370 Mbps cited in the brief is 5.7× faster than the V9.4 Track 1 measurement here.  If that figure is real on this machine, the bench harness used to obtain it must be identified before any further Wave-2 hypothesis testing — otherwise we are projecting deltas against an unknown reference.

4. **If a future Wave 2 prototype is attempted, the falsifiable claim should be measured at the smallest possible unit first**:
   * micro-bench just the dispatch (`parse_value_at` over a synthetic byte stream)
   * micro-bench just the whitespace skip (sparse vs dense ws streams)
   * before integrating both into a parser-level A/B.

## Artifacts

* `profile/wave2-prototype/baseline.csv` — baseline CSV
* `profile/wave2-prototype/prototype.csv` — prototype CSV
* `profile/wave2-prototype/baseline-random.json.gz` + `.syms.json` — baseline samply profile
* `profile/wave2-prototype/prototype-random.json.gz` + `.syms.json` — prototype samply profile
* `crates/runtime/src/grammars/json/generated_eventcursor.rs` — prototype implementation (feature-gated)
* `crates/runtime/examples/wave2_bench.rs` — shared median-min bench harness
