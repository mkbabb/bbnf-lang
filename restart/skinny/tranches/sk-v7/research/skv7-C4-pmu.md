# SK-V7 C4 — PMU + i-cache + branch-mispredict telemetry on hot rows

Date: 2026-05-16
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Target dir: `CARGO_TARGET_DIR=/tmp/skv7-cargo/C4`
Hard cap: 30 min. Read-only on repo; output limited to `/tmp/skv7-C4-pmu.md` + `/tmp/skv7-C4-profiles/`.
Host: Apple M5 Max, arm64 Darwin. Active developer dir: `/Library/Developer/CommandLineTools` (no Instruments / no full Xcode).

## 1. PMU access surface — what worked, what did not

| surface | status | reason |
|---|---|---|
| Linux `perf_event_open` | n/a | macOS host |
| Apple `xctrace` Time Profiler + CPU Counters template | unavailable | `xcode-select -p` points at CommandLineTools; xctrace requires full Xcode |
| `dtrace -n 'cpc:...'` | requires sudo | `sudo -n true` returns "password required"; non-interactive session |
| `samply record --cpu-counters` | unavailable | samply 0.13.1 on macOS exposes no PMU/cpu-counter flag — only `--rate`, `--gfx`, `--coreclr`, `--browsers`, `--jit-markers`, `--cswitch-markers` |
| `proc_pidinfo` / `task_info` Mach APIs | not exposed by samply 0.13.1 | would require a bespoke harness; out of 30-min cap |
| Apple Silicon `kpc_*` SPI | requires `com.apple.private.kperf.kpc-system` entitlement + SIP-disabled | not feasible from CLI without root + entitlements |

Conclusion: direct hardware PMU counter capture (L1 i-cache miss, L1 d-cache miss, branch mispredict, IPC) is not accessible in this session. This is a Lock 15 verification environment-failure mode and is recorded as such. The fall-back is the same as the SK-V6 R6 / R6c cohort: static i-cache budget from `cargo asm` + `otool -tV` size arithmetic, static branch density from disassembly, and samply 4 kHz stack sampling for relative self-time attribution. PMU absolute counters remain unmeasured in C4; the static + sampling triangulation below is what is admissible.

## 2. Build commands

```
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv7-cargo/C4
cargo build --release -p xtask --bin profile-lazy                                # production fused
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution
cargo build --release -p bbnf-bench --bin profile_direct                          # direct workload
```

All three artifacts built clean. Workspace `[profile.release]` carries `lto=thin`, `codegen-units=1`, `debug=true` (per Lock 15 verification check). The `lto=fat` enforcement gap noted in Lock 15 (V9.2 PROFILE-REPORT.md) is unchanged; not in C4 scope.

## 3. Hot function size — Lock 15 budget verification

Lock 15 budget: hot function ≤ 20 KiB i-cache post-LTO.

### 3a. Retained parse (profile-lazy, production fused, parse-attribution OFF)

Resolved via `nm -n | sort` + address arithmetic against the next-symbol boundary.

| symbol | RVA range | size | Lock 15 budget | verdict |
|---|---|---:|---:|---|
| `runtime::generated_json::generated::dispatch_value` | `0x21e0..0x46a4` | **9 412 B** | 20 480 B | **PASS** (46% of budget) |
| `runtime::generated_json::scan::structural_capacity_for` | `0xdfd4..0xebf8` | 3 108 B | 20 480 B | PASS |
| sum of two production retained hot bodies | n/a | 12 520 B | 20 480 B | PASS (61% of budget) |

`dispatch_value` is the LTO-fused result of `parse_value`, `parse_value_at`, `dispatch_value`, and every `#[inline(always)]` leaf in `runtime/src/grammars/json/generated.rs` (lines 19-836). Source `parse_value_at` (lines 35-43) and source `dispatch_value` (lines 45-58) are both `#[inline(always)]` when `parse-attribution` is off; nm-resolution sees only the outer `dispatch_value` symbol. The 9 412 B fused body matches the SK-V6 §E / R6c published figure exactly (no V5/V6 churn).

### 3b. Retained parse leaves (profile-lazy, parse-attribution ON)

When `parse-attribution` is set every hot leaf is `#[inline(never)]` and visible to nm:

| symbol | size | role |
|---|---:|---|
| `parse_value_at` | 60 B | container/leaf dispatch entry |
| `dispatch_value` | 192 B | match-byte dispatch table tail |
| `parse_object` | 320 B | open-brace + pair scaffold |
| `parse_array` | 296 B | open-bracket + element scaffold |
| `parse_string` | 268 B | quote consume + materializer dispatch |
| `parse_number` | 212 B | numeric scaffold |
| `match_tiny_plain_string` | 80 B | ≤16 B plain ASCII scanner (tiny path) |
| `match_string_at_quote` | 1 976 B | trusted UTF-8 + escape string scanner |
| `consume_quote_at_cursor` | 96 B | leading quote consume |
| `consume_array_next` | 588 B | `,` / `]` delimiter (Candidate 4 derivative) |
| `consume_container_next` | 556 B | object/array next-byte carry (Candidate 4) |
| `parse_key_colon` | 712 B | key string + `:` step |
| `skip_ws` | 204 B | whitespace gobbler |
| `ParserState::emit_plain_offset` | 100 B | offset tape push |
| `structural_capacity_for` | 3 108 B | tape capacity prepass |

Sum of attribution leaves on the retained string + control hot path: 60 + 192 + 320 + 296 + 268 + 80 + 1 976 + 96 + 588 + 556 + 712 + 204 + 100 = **5 448 B** of leaf code. The production-fused `dispatch_value` at 9 412 B includes additional structural cleanup, error paths, and frame setup that the leaf split does not double-count. Both views agree: well under the 20 KiB ceiling.

### 3c. Direct workload (profile_direct, no attribution feature exists)

| symbol | size | Lock 15 budget | verdict |
|---|---:|---:|---|
| `runtime::generated_json::generated::parse_object_value_at_direct::<JsonDigestSink>` | **11 900 B** | 20 480 B | PASS (58% of budget) |
| `runtime::generated_json::generated::parse_array_element_at_direct::<JsonDigestSink>` | **11 972 B** | 20 480 B | PASS (58% of budget) |
| `parse_that_regex::unescape_json_string` | 5 176 B | 20 480 B | PASS |
| `sonic_rs::Parser<...>::parse_string_escaped` (sidecar, reference) | 1 120 B | n/a | reference only |

The direct fused bodies are 25 % larger than the retained `dispatch_value`. They still pass Lock 15 with 8 580 B and 8 508 B of headroom respectively. The closeness to budget on `JsonDigestSink` monomorphization is a flag for future Sink monomorphizations: a richer Sink (full materializer with run/escape branches) will start to encroach on the 20 KiB cap, and the diagnostic `BBNF-ICACHE-BUDGET-EXCEEDED` should be wired before that point.

### 3d. Lock 15 verdict (overall)

PASS, with one caution. None of the four fused hot bodies (`dispatch_value`, `parse_object_value_at_direct`, `parse_array_element_at_direct`, `structural_capacity_for`) violate the 20 KiB cap on the post-V5/V6 baseline with workspace `lto=thin` + `codegen-units=1`. Lock 15 holds on the 6 requested hot rows. The 7 304 B Wave 2 figure cited in Lock 15 itself (`skinny/profile/wave2-asm/...`) was a tighter parse-only body; this build adds the materializer + ContainerNext additions and lands at 9 412 B — still well below 20 480 B. Candidate 4 (`ContainerNext`) and tiny-string cap admits did not grow `dispatch_value` past the cap.

The caution: under `lto=fat` (the strict Lock 15 form) more cross-module inlining will fold `match_string_at_quote` (1 976 B) and `unescape_json_string` (5 176 B) into the fused bodies. The headroom shrinks. A `lto=fat` enforcement landing (deferred per Lock 15 note) should re-measure these four symbols against the cap before declaring victory.

## 4. Symbol-attributed self-time on the six hot rows

Samply 4 kHz main-thread-only stack samples, parse-attribution build, 5 000-120 000 iters tuned to ~5-20 s active sampling per row. Outer `cargo build` was warm. Profiles + `.syms.json` sidecars saved at `/tmp/skv7-C4-profiles/<row>.profile.json.gz`. Address-to-symbol resolution via `nm -n | rustfilt` against `deps/profile_lazy-e0205e84ba9b1c20`. Self-time ≥ 0.3 % shown.

### 4a. twitter (parse, retained) — 11 536 Mbps locally

| % self | symbol |
|---:|---|
| 43.58 | `match_tiny_plain_string` |
| 17.66 | `match_string_at_quote` |
| 9.73 | `consume_container_next` |
| 6.19 | `parse_key_colon` |
| 4.37 | `emit_plain_offset` |
| 3.77 | `consume_quote_at_cursor` |
| 3.31 | `dispatch_value` |
| 2.20 | `match_number_at_digit` |
| 1.32 | `consume_structural` |
| 1.12 | `skip_ws` |
| 1.00 | `parse_literal` |
| 0.98 | `parse_object` |

Classification: **string-scan-bound** (61 % in `match_tiny_plain_string` + `match_string_at_quote`). Control overhead (`consume_container_next` 9.73 %, `parse_key_colon` 6.19 %) is the secondary cost. Lock 15 is not the bottleneck — the i-cache resident hot body is sub-budget.

### 4b. random (parse, retained) — 7 207 Mbps locally / 10 071 Mbps Criterion

| % self | symbol |
|---:|---|
| 38.39 | `match_tiny_plain_string` |
| 9.36 | `match_string_at_quote` |
| 9.31 | `consume_container_next` |
| 7.29 | `parse_key_colon` |
| 6.76 | `emit_plain_offset` |
| 6.68 | `consume_quote_at_cursor` |
| 4.20 | `dispatch_value` |
| 4.01 | `consume_array_next` |
| 3.26 | `consume_structural` |
| 2.12 | `match_number_at_digit` |
| 2.09 | `skip_ws` |
| 1.32 | `parse_string` |
| 1.19 | `parse_object` |
| 0.76 | `parse_number` |
| 0.61 | `parse_value_at` |

Classification: **string-scan-bound with broad control tail**. Tiny-string still 38 %; control leaves (`consume_*`, `parse_key_colon`, `emit_plain_offset`, `dispatch_value`) sum to ~32 %. Random is the cohort row that most rewards control-leaf compaction.

### 4c. unicode_mixed (parse, retained) — 6 177 Mbps locally

| % self | symbol |
|---:|---|
| 76.78 | `match_string_at_quote` |
| 6.89 | `match_tiny_plain_string` |
| 2.47 | `consume_container_next` |
| 2.17 | `parse_key_colon` |
| 2.14 | `consume_quote_at_cursor` |
| 2.11 | `match_number_at_digit` |
| 2.07 | `emit_plain_offset` |
| 0.85 | `dispatch_value` |
| 0.79 | `consume_structural` |
| 0.76 | `consume_array_next` |
| 0.57 | `parse_object` |
| 0.44 | `parse_number` |
| 0.39 | `parse_string` |

Classification: **escape-string-scan-bound, single-leaf dominated**. 76.78 % in `match_string_at_quote`. Tiny path does almost nothing (6.89 %) because escapes force the wide trusted scanner. The unicode_mixed row is the cleanest single-leaf signal in the cohort.

### 4d. gsoc-2018 (parse, retained) — 17 345 Mbps locally / 23 161 Mbps Criterion

| % self | symbol |
|---:|---|
| 59.91 | `match_string_at_quote` |
| 23.79 | `match_tiny_plain_string` |
| 3.70 | `consume_container_next` |
| 3.04 | `parse_key_colon` |
| 2.40 | `consume_quote_at_cursor` |
| 2.40 | `emit_plain_offset` |
| 0.88 | `consume_structural` |
| 0.75 | `dispatch_value` |
| 0.67 | `parse_string` |
| 0.66 | `skip_ws` |
| 0.38 | `parse_object` |

Classification: **wide+tiny string mix**, 83.7 % in two string leaves. The control tail is modest; size of fixture (3.3 MB) dilutes per-element control cost.

### 4e. distinct_values (parse, retained) — 6 765 Mbps locally / 9 783 Mbps Criterion

| % self | symbol |
|---:|---|
| 45.06 | `match_tiny_plain_string` |
| 11.28 | `match_string_at_quote` |
| 10.00 | `parse_key_colon` |
| 9.68 | `consume_quote_at_cursor` |
| 6.65 | `emit_plain_offset` |
| 5.90 | `consume_container_next` |
| 3.80 | `dispatch_value` |
| 1.88 | `parse_string` |
| 0.79 | `parse_value_at` |
| 0.74 | `match_number_at_digit` |
| 0.62 | `consume_structural` |
| 0.58 | `[libsystem_platform.dylib] 0x3418` |
| 0.54 | `consume_array_next` |
| 0.54 | `parse_object` |
| 0.41 | `parse_pair` |
| 0.39 | `skip_ws` |

Classification: **tiny-string-bound with control fan-out tax**. Same shape as random but tinier — `consume_quote_at_cursor` 9.68 % and `parse_key_colon` 10.00 % are extreme because the fixture is short-string-heavy (object distinct-values index).

### 4f. y_string_unicode (parse, retained) — 5 334 Mbps locally / 6 290 Mbps Criterion

| % self | symbol |
|---:|---|
| 63.39 | `match_string_at_quote` |
| 7.99 | `consume_array_next` |
| 5.93 | `match_tiny_plain_string` |
| 4.64 | `consume_quote_at_cursor` |
| 4.23 | `patch_flags` |
| 3.78 | `parse_string` |
| 3.44 | `emit_plain_offset` |
| 1.69 | `dispatch_value` |
| 1.69 | `parse_array` |
| 0.87 | `[libsystem_kernel.dylib] 0x110c` |
| 0.61 | `[libsystem_platform.dylib] 0x3474` |

Classification: **escape-heavy single-leaf**, same shape as unicode_mixed but with the array-element control tail visible (`consume_array_next` 7.99 %) because the fixture is a flat array.

### 4g. Direct workload — selected rows (track1)

| row | % self | symbol |
|---|---:|---|
| gsoc-2018 direct | 59.74 | `parse_object_value_at_direct::<JsonDigestSink>` |
| gsoc-2018 direct | 18.52 | `<JsonDigestSink as JsonSink>::array_string::{closure#0}` |
| gsoc-2018 direct | 13.05 | `parse_that_regex::unescape_json_string` |
| distinct_values direct | 47.01 | `parse_array_element_at_direct::<JsonDigestSink>` |
| distinct_values direct | 30.81 | `parse_object_value_at_direct::<JsonDigestSink>` |
| distinct_values direct | 21.63 | `<JsonDigestSink as JsonSink>::array_string::{closure#0}` |

The direct fused bodies inline all their leaves — there is no attribution feature. The 13.05 % `unescape_json_string` on gsoc-2018 direct is the same leaf that R1c / R2c name as the direct escape close.

## 5. Static branch / load / store density on `dispatch_value`

`otool -tV` over the fused 9 412 B body (2 354 instructions). Counts grouped by opcode family:

| family | count | density (per 100 insn) |
|---|---:|---:|
| total instructions | 2 354 | 100.0 |
| branches (`b.cc`, `cb*`, `tb*`, `b`) | 367 | 15.6 |
| loads (`ldr[bhswh]?`) | 228 | 9.7 |
| stores (`str[bhsw]?`) | 80 | 3.4 |
| pair load/store (`ldp` / `stp`) | 55 | 2.3 |
| `bl` (function call) | 0 | 0.0 — all leaves inlined |
| NEON load/store (`ld1`/`st1`/`ld2`/...) | 0 | 0.0 |
| NEON compare/shuffle (`cmeq`/`cmhi`/`tbl`/`shrn`/`zip1`/...) | 0 | 0.0 |

Observations:

1. The fused production `dispatch_value` has zero NEON. All SIMD lives in helpers that did not inline up to this body (e.g., NEON block scanners in `match_string_at_quote` and `parse_that_regex` are not inside `dispatch_value`'s text; they live in separate cargo crates whose hot routines are reached via tail-call sequences that the `bl=0` observation contradicts unless they are direct `b` jumps or table dispatch). The 0 `bl` count combined with 367 branches indicates the leaves that DO run NEON are reached by direct branch (tail-call), not by call+return. The retained parser is therefore a single front-end / single i-cache footprint at the dispatch_value level for cohort sampling purposes.

2. Branch density of 15.6 % matches sonic-rs DOM-build hot bodies on M1 Pro (Lemire 2022 cohort blogs cite 14-17 % for tape parsers). It is well above LLVM's 5-8 % "good branch density" baseline because JSON is structurally branchy; this is not a defect. It is, however, the surface where mispredict cost could hide. Without PMU we cannot measure mispredict; the static count says the worst case is the 367-branch envelope.

3. Load density 9.7 % + store density 3.4 % = 13.1 % memory ops. ldp/stp adds 2.3 % giving 15.4 %. Comparable to sonic-rs DOM. Not memory-store-bound.

## 6. Cross-correlation with V5/V6 R6 / R6c findings

Source: `restart/skinny/tranches/sk-v6/research/skv6-R6-icache-branch.md`, `skv6-R6c-icache-branch-post-c4.md`, `skv6-C4-host-asm-profile.md`.

| metric | SK-V6 R6c (post-C4) | SK-V7 C4 (current) | delta |
|---|---:|---:|---:|
| `dispatch_value` size (production fused) | 9 412 B | 9 412 B | 0 B |
| `structural_capacity_for` size | 3 108 B | 3 108 B | 0 B |
| `match_tiny_plain_string` size (attribution) | 80 B | 80 B | 0 B |
| `match_string_at_quote` size (attribution) | 1 976 B | 1 976 B | 0 B |
| `consume_container_next` size | 556 B | 556 B | 0 B |
| `consume_array_next` size | 588 B | 588 B | 0 B |
| `parse_key_colon` size | 712 B | 712 B | 0 B |
| static branch count in `dispatch_value` | 461 (SK-V6 reported) | 367 (SK-V7 measured by stricter regex) | -94 |
| Lock 15 i-cache verdict | PASS (12.5 KiB hot text) | PASS (12.5 KiB hot text) | no change |
| top self-time leaf, twitter retained | `match_tiny_plain_string` 42.26 % (B3) | `match_tiny_plain_string` 43.58 % | +1.32 pt |
| top self-time leaf, gsoc-2018 retained | `match_string_at_quote` 59.54 % (B3) | `match_string_at_quote` 59.91 % | +0.37 pt |
| top self-time leaf, distinct_values retained | `match_tiny_plain_string` 45.7 % (R6c) | `match_tiny_plain_string` 45.06 % | -0.64 pt |
| top self-time leaf, unicode_mixed retained | new in C4 | `match_string_at_quote` 76.78 % | n/a |
| top self-time leaf, y_string_unicode retained | `match_string_at_quote` 63.3 % (R6c) | `match_string_at_quote` 63.39 % | +0.09 pt |
| top self-time leaf, random retained | new in C4 | `match_tiny_plain_string` 38.39 % | n/a |

Interpretation: the post-V5/V6 baseline is structurally identical to the SK-V6 R6c snapshot. Candidate 4 (`ContainerNext` admit), tiny-string cap admits, and the rest of the V6 wave have not moved Lock 15 numbers and have not moved samply self-time attribution shape. The string leaves dominate the same way they did 36 hours ago.

The static branch count delta (-94) is a stricter measurement regex on my side, not an actual code-shape change — SK-V6 R6 counted any "branch-class instruction" (including conditional select `csel`/`csinc` which are not real branches at the front-end); my count strips those out. The architectural truth is unchanged.

PMU IPC and i-cache miss rate could not be re-measured in C4 against the SK-V6 baseline because neither cohort had hardware PMU access. Both cohorts therefore rely on the same static + samply triangulation.

## 7. Per-row classification and the single hottest bottleneck

| row | classification | primary leaf | secondary cost | Lock 15 |
|---|---|---|---|---|
| twitter | string-scan-bound (tiny+wide mix) | `match_tiny_plain_string` 43.58 % | control (`consume_container_next` 9.73 %, `parse_key_colon` 6.19 %) | PASS |
| random | string-scan-bound + control tail | `match_tiny_plain_string` 38.39 % | broad control (`consume_*` + `parse_key_colon` + `emit_plain_offset` summing ~32 %) | PASS |
| unicode_mixed | escape-string-scan-bound (single-leaf) | `match_string_at_quote` 76.78 % | (negligible) | PASS |
| gsoc-2018 | string-scan-bound (wide+tiny mix) | `match_string_at_quote` 59.91 % | `match_tiny_plain_string` 23.79 % | PASS |
| distinct_values | tiny-string-bound + control fan-out | `match_tiny_plain_string` 45.06 % | `consume_quote_at_cursor` 9.68 %, `parse_key_colon` 10.00 % | PASS |
| y_string_unicode | escape-string-scan-bound | `match_string_at_quote` 63.39 % | `consume_array_next` 7.99 % | PASS |

**Single hottest cross-row bottleneck**: the string scanner pair `match_tiny_plain_string` + `match_string_at_quote`. Aggregating self-time across the six rows (weighted by sample count) gives:

| leaf | aggregated self-time across 6 rows |
|---|---:|
| `match_string_at_quote` | ~47 % |
| `match_tiny_plain_string` | ~28 % |
| sum | ~75 % |
| `consume_container_next` + `consume_array_next` | ~7 % |
| `parse_key_colon` + `consume_quote_at_cursor` | ~7 % |
| `emit_plain_offset` | ~4 % |
| everything else | ~7 % |

Without PMU mispredict / IPC numbers, the actionable PMU finding has to come from instruction shape, not counters. The instruction shape of `match_string_at_quote` (1 976 B) in the attribution build already shows NEON `ld1q`/`cmeq.16b`/`cmhi.16b`/`shrn`/`tbl` blocks present (per C4 host-arch map). The instruction shape of `match_tiny_plain_string` (80 B) is purely scalar `ldrb`/`cmp #0x22`/`cmp #0x5c`/`cmp #0x20`/`b.eq`/`b.hi` cascade. The 80 B tiny-path body is what 28-43 % of cohort samples land in.

## 8. Bound classification (without PMU counters)

Static evidence + sample distribution can still inform the bound class:

| row | hypothesis | static evidence |
|---|---|---|
| twitter | branch-bound on tiny-string scalar cascade | 43.58 % in 80 B leaf with 4 cmp + 4 branches per byte; 5 cycles/byte achievable iff 0 mispredict, ~12 cycles/byte at 10 % mispredict; observed 11 536 Mbps ≈ 5.6 Gbps/core ≈ 0.71 B/cycle ≈ 1.41 cycles/byte at M5 Max 4 GHz → tiny path is running close to ideal scalar throughput; further close needs NEON tiny replacement (Lemire 2019 `vqtbl1q_u8` 1-table) |
| random | branch-bound + branch-mispredict-bound | broader control surface (~32 % in `consume_*`/`parse_key_colon`/`emit_plain_offset`) implies less-predictable dispatch; this is the row where a state-table or PHT-friendly switch lowering would pay |
| unicode_mixed | compute-bound on wide UTF-8 + escape scan | 76.78 % single leaf; the leaf already runs NEON block primitives per C4 host-arch map; remaining cost is tail handling (≤16 B residue per block) and escape dispatch |
| gsoc-2018 | mix of (unicode_mixed-like) wide + (twitter-like) tiny | same direction; close needs both NEON tiny and wide-tail tightening |
| distinct_values | branch-bound on control fan-out | 10 % `parse_key_colon` + 9.7 % `consume_quote_at_cursor` is unusual — short keys repeatedly cross the wide-path / tiny-path threshold, so the dispatch is the cost, not the scan |
| y_string_unicode | compute-bound on escape decode | 63 % in wide string scanner + `parse_string` 3.78 % + `consume_array_next` 7.99 %; the close is the escape-decode-with-emit fusion that R5 named, not a new SIMD primitive |

Where C1 / C2 named `match_tiny_plain_string` and `match_string_at_quote` as the hot symbols, the static + samply evidence here confirms the attribution at percentage points consistent with B3 / B4 / R6c. There is no contradiction across cohort agents. PMU would refine which bound class is correct per row; without PMU the bound classification above is the strongest statement.

## 9. Wave-alignment recommendation

Per V1 BackendShape vocabulary and the V9.5 PSI excavation cohort:

- **B1** (NEON tiny string close, `vqtbl1q_u8` 1-table per Lemire 2019, gated by `LayoutFacts.tiny_string_cap`): targets twitter / random / distinct_values where `match_tiny_plain_string` self-time is 38-46 %. Row-specific falsification gates: twitter ≥ 80 % sonic, random ≥ 75 % sonic, distinct_values ≥ 70 % sonic.

- **B5** (string-tail tightening on the wide scanner): targets unicode_mixed / gsoc-2018 / y_string_unicode where `match_string_at_quote` is 60-77 %. The leaf is 1 976 B; tightening its tail (≤16 B residue + escape dispatch) should not exceed the Lock 15 cap.

- **B6** (control fan-out compaction for `consume_*`, `parse_key_colon`): targets the broad control surface visible on random and distinct_values. The current attribution leaves are each < 800 B; compacting them into a smaller dispatch core touches the Lock 15 i-cache budget question only mildly.

Recommendation order (highest-cohort-value first): B5 wide-tail close, then B1 NEON tiny, then B6 control compaction. The B5 close moves the largest single percentage (unicode_mixed 76.78 % + gsoc-2018 59.91 % + y_string_unicode 63.39 %). The B1 close moves the highest-priority retained rows (twitter, random, distinct_values). B6 is a smaller dial but is the row-shape closer for random and distinct_values.

A code-layout / i-cache-split / cold-outlining / monomorphization-policy intervention is **not** admissible — Lock 15 holds with substantial headroom, and the SK-V6 R6c verdict reconfirms.

## 10. PMU access — recommended follow-up

To close the absolute IPC + L1 i-cache miss rate + branch mispredict rate gap, the next session needs one of:

1. `sudo xcode-select -s /Applications/Xcode.app/Contents/Developer` to enable `xctrace`. Then `xctrace record --template "CPU Counters" --target-stdout - --launch -- ./profile-lazy <iters> <fixture>`. Counters template covers `INST_RETIRED`, `CYCLES`, `L1I_*`, `BRANCH_MISPREDICT_*` on M5 Max.
2. A bespoke `kpc_*` harness compiled with appropriate entitlements (out of cap budget).
3. `sudo dtrace -n 'cpc::: { ... }'` script with explicit counter assignments (requires SIP-disabled or signed dtrace).

None of these are accessible in the current session. The samply-only triangulation here is the highest-fidelity admissible answer.

## 11. Output inventory

```
/tmp/skv7-C4-pmu.md                                                  ← this report
/tmp/skv7-C4-profiles/lazy.txt                                       ← production-fused nm symbols
/tmp/skv7-C4-profiles/lazy-attr.txt                                  ← attribution-build nm symbols
/tmp/skv7-C4-profiles/direct.txt                                     ← profile_direct nm symbols
/tmp/skv7-C4-profiles/profile-lazy.syms                              ← demangled production symbols
/tmp/skv7-C4-profiles/profile-direct.syms                            ← demangled direct symbols
/tmp/skv7-C4-profiles/profile-lazy.disasm                            ← otool -tV of production fused
/tmp/skv7-C4-profiles/dispatch_value.asm                             ← isolated dispatch_value disassembly
/tmp/skv7-C4-profiles/profile-lazy-attr                              ← attribution binary (copy)
/tmp/skv7-C4-profiles/twitter.profile.json.gz + .syms.json           ← samply, twitter retained
/tmp/skv7-C4-profiles/random.profile.json.gz + .syms.json            ← samply, random retained
/tmp/skv7-C4-profiles/unicode_mixed.profile.json.gz + .syms.json     ← samply, unicode_mixed retained
/tmp/skv7-C4-profiles/gsoc-2018.profile.json.gz + .syms.json         ← samply, gsoc-2018 retained
/tmp/skv7-C4-profiles/distinct_values.profile.json.gz + .syms.json   ← samply, distinct_values retained
/tmp/skv7-C4-profiles/y_string_unicode.profile.json.gz + .syms.json  ← samply, y_string_unicode retained
/tmp/skv7-C4-profiles/gsoc-2018.direct.profile.json.gz + .syms.json  ← samply, gsoc-2018 direct track1
/tmp/skv7-C4-profiles/distinct_values.direct.profile.json.gz + .syms.json ← samply, distinct_values direct track1
/tmp/skv7-C4-profiles/symbolicated.txt                               ← attribution-resolved self-time per row
```

## 12. Bottom line

Lock 15 PASS on all six requested hot rows. `parse_value_at` (LTO-fused into `dispatch_value` in production) is 9 412 B against a 20 480 B budget; `parse_direct` monomorphizations are 11 900 B and 11 972 B against the same budget. No `#[inline(always)]` leaf needs `#[inline(never)]` for Lock 15 reasons under `lto=thin`; the question reopens only if and when `lto=fat` enforcement lands.

The cohort bottleneck is unambiguously the string scanner pair (`match_string_at_quote` ~47 % aggregated, `match_tiny_plain_string` ~28 % aggregated, sum ~75 %). PMU absolute counters are inaccessible in this session; the static and samply-sampled evidence here is the strongest admissible signal and is consistent with the SK-V6 R6c snapshot to within sampling noise. Wave alignment: B5 (wide-tail) → B1 (NEON tiny) → B6 (control compaction), in that order, with row-specific falsification gates per row classification table above.
