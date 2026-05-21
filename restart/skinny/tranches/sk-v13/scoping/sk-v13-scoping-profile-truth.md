# SK-V13 Scoping: Profile Truth + Parse-Time Gap-to-Perfect

**Date**: 2026-05-21  
**Authority**: SK-V12 PASS-ADMIT close (REDRESS-127); CSS L4 declaration_values/direct_to_struct/main at 429.34 Mbps  
**Scope**: SK-V13 S-P1 profile capture specification and "perfect" parsing ceiling definition.

---

## §1 SK-V12-close Profile Freshness Audit

### Staleness Verdict: CRITICAL

The only PMU TSV that exists is `/restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` (modified 2026-05-20 03:58:27). This capture is **pre-pin baseline** capturing only JSON three-plane data (parse_only, direct_to_struct, real_typed_struct) across 17 corpora. **CSS L4 declaration_values is not covered.**

**Freshness Timeline**:
- P1 TSV capture: 2026-05-20 03:58 — baseline `sk-v12-open:50bd1648`
- W1a/W1b-1 CSS scaffold: completed; no PMU rerun documented
- W2 escape_mask_64 fix: correctness-only (test expansion); no JSON guard re-measurement, no PMU
- W1b-1/W1b-2a/W1b-2b CSS gates: report/gate consumption only; no fresh PMU
- W4 ASM-gen microbench: route-only; no PMU spread
- W5 close: REDRESS-127 reran JSON floor AWK **verification** (`verify-skv12-json-floors.awk`) against checked-in guard authority, NOT fresh Criterion measurement

**SK-V12 PMU staleness**: The P1 TSV is authority for JSON direct residuals (13 rows in REDRESS-119 fixpoint table) but **does not measure CSS L4** at all. CSS L4 Track 1 429.34 Mbps comes from W1b-2b Criterion native bench over 30 samples (`sample_count=30`, `track1_mean_ns=3484.383794 ns`), not from xctrace/samply/proc_pid_rusage.

**SK-V13 S-P1 Capture Requirement**: Fresh PMU must cover CSS L4 declaration_values plus JSON 13 N-direct residual rows under the pin. Samply + xctrace Time Profiler + proc_pid_rusage(RUSAGE_INFO_V5) capture needed for cycles/instructions/self-time per leaf.

---

## §2 CSS L4 Admitted Row Profile Gap

### Row Identity
- **Row**: `css_l4/declaration_values/direct_to_struct/main`  
- **Track 1**: 429.34 Mbps (W1b-2b Criterion; 30 samples; 3.484 µs mean over 187-byte fixture)
- **Comparators**: cssparser oracle 217.43 Mbps; lightningcss 168.93 Mbps  
- **Margin**: 259.41 Mbps above threshold  
- **Ratio**: 2.54× vs lightningcss  

### Cycles/Byte Analysis

From W1b-2b report (187 bytes; 3.484 µs @ ~3.5 GHz Apple M5 Max):
- **Estimated cycles/byte**: (3.484 µs × 3.5 GHz) / 187 B ≈ **6.5 c/B** (scalar generated parser)  
- **lightningcss c/B**: (8.856 µs × 3.5 GHz) / 187 B ≈ **16.6 c/B**  
- **Gap**: 10.1 c/B in favor of generated Track 1  

### Dominant Self-Cost Functions

**Status: MISSING**. W1b-2b report consumed W1b-2a Criterion artifacts; no samply/xctrace Time Profiler capture was produced for CSS L4 declaration_values. Per `skv12-W1b-css-l4-sota.json`:
```json
"profile_artifact": "n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion"
```

**xctrace Hot Leaves**: Not captured. The W1b scalars (output_digest_hash, container_dispatch, bounded_plain_string_scan from parse/direct JSON planes) do not directly apply to CSS tokenization domain (no array/object nesting, no deep string/escape pipelines per fixture).

**SK-V13 S-P1 must capture**:
- xctrace Time Profiler on generated CSS L4 parser over the 187-byte fixture and a scaled larger CSS corpus (1–10 MB range) if available
- Samply callstack annotation for hot leaves at function + loop nesting level
- Self-time % per declared_value_parser() → token_type_dispatch() → value_span_sink() stacks

---

## §3 JSON Guards Re-measured or Verified?

### W5 Audit: Verification-Only

REDRESS-127 states:
> "W5 reran the checked-in JSON floor AWK proof after the `RESULTS.md` close edit."

The command executed:
```sh
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

**This is validation of existing rows against guard floor literals, not fresh Criterion re-measurement.** W1a held the fresh guard authority (`refreshed:skv12-w1a-json-guard-criterion:guards-pass`), and W5 only re-verified the proof against the checked-in floor table without re-running Criterion.

### 13 N-Direct Residual Guard Rows (from REDRESS-119)

| Row | Track 1 (Mbps) | Track 2 (Mbps) | Sonic Floor (Mbps) | Measurement Status |
|---|---:|---:|---:|---|
| twitter/direct | 12228 | 11221 | 15150 | P1 PMU (pre-pin) |
| canada/direct | 10362 | 10227 | 11745 | P1 PMU (pre-pin) |
| github_events/direct | 12362 | 11343 | 16336 | P1 PMU (pre-pin) |
| update_center/direct | 8472 | 7690 | 11239 | P1 PMU (pre-pin) |
| mesh/direct | 8791 | 9088 | 9841 | P1 PMU (pre-pin) |
| random/direct | 7747 | 7053 | 8907 | P1 PMU (pre-pin) |
| gsoc-2018/direct | 15228 | 14595 | 23439 | P1 PMU (pre-pin) |
| instruments/direct | 12076 | 11069 | 12433 | P1 PMU (pre-pin) |
| unicode_mixed/direct | 4617 | 4528 | 10433 | P1 PMU (pre-pin) |
| unicode_escapes/direct | 5114 | 5072 | 14134 | P1 PMU (pre-pin) |
| distinct_values/direct | 6005 | 5324 | 11503 | P1 PMU (pre-pin) |
| y_string_unicode/direct | 4975 | 3544 | 8228 | P1 PMU (pre-pin) |

**4 JSON Direct Guard Rows** (held under REDRESS-121 W1a + W5 verification):

| Row | Floor T1 (Mbps) | Current T1 (Mbps) | Floor T2 (Mbps) | Current T2 (Mbps) | Status |
|---|---:|---:|---:|---:|---|
| citm_catalog/direct | 18191 | 18563 | 17431 | 17787 | PASS (W1a measured; W5 verified) |
| apache_builds/direct | 11028 | 11254 | 9996 | 10189 | PASS (W1a measured; W5 verified) |
| marine_ik/direct | 8759 | 8938 | 9248 | 9437 | PASS (W1a measured; W5 verified) |
| unicode_basic/direct | 2253 | 2299 | 2182 | 2227 | PASS (W1a measured; W5 verified) |

**Honest Accounting**: W1a REDRESS-121 produced fresh Criterion guard measurement. W2 made no JSON guard change (only test expansion). W1b-1/W1b-2b/W4 consumed W1a baseline without re-running Criterion. W5 verified but did not re-measure.

---

## §4 13 N-Direct Residual Rows: Reopen Candidate Survey

Per REDRESS-119 (SK-V11 W8 direct fixpoint), all 13 rows have measured uncloseable proofs tied to W3–W7 attempted routes. Under SK-V12's **unblocked USER PIN D3 (union substrate) + D4 (ASM-gen)**, assess whether the unblocked routes open fresh differential evidence.

### Reopen Candidate Matrix

| Row | Fixpoint Route LCA (REDRESS) | Prior Gap Route | Unblocked Pin Category | Fresh SK-V13 S-P1 Evidence Needed | Reopenable |
|---|---|---|---|---|---|
| twitter/direct | W5 string-span (REDRESS-116) + W7 digest (REDRESS-118) | output_digest_hash optimization | D4 ASM-gen for hash u64x2 | Samply hot-leaf; microbench u64x2_aarch64 vs scalar; parity proof | YES (modest ROI) |
| canada/direct | W3 numeric rejected (REDRESS-108) | container_dispatch unroll | D3 union-substrate | Fresh W3 numeric_span_emit_slot on pin; samply; comparison to mesh W3 rejection | CONDITIONAL |
| github_events/direct | W5 string-span blocked; W7 digest | dispatch_unroll_x2 | D4 ASM-gen | Samply callstack; microbench dispatch tail unroll; parity | YES (modest) |
| update_center/direct | W5 string-span blocked; W7 digest | hash_u64x2_simd | D4 ASM-gen | Samply; u64x2 microbench; parity | YES (modest) |
| mesh/direct | W3 measured rejected | number_span_emit_slot | D3 union-substrate | Fresh W3 rerun (prior showed 3835/3614 vs 8675); samply | NO (prior hard rejection) |
| random/direct | W4/W5/W7 blocked | container_tail_next probe | D3/D4 combined | Fresh probe + samply; unlikely <7878 closure | NO (unlikely) |
| gsoc-2018/direct | W5/W7 movemask/string blocked | no clear kernel route | D3/D4 | Samply shows sparse artifact-only profile; no kernel candidate clear | NO (sparse profile) |
| instruments/direct | W0-clamped (no W3–W8 provenance) | no authority | D3/D4 | Fresh full profile + Criterion; requires back-filling W0 clamping excuse | NO (no prior proof) |
| numbers/direct | W0-clamped; W3 numeric rejected | no authority | D3/D4 | Fresh full profile; back-fill W0 clamping | NO (no prior proof) |
| unicode_mixed/direct | W6 decoded-source blocked; W0-clamped | no kernel route | D3/D4 | Fresh samply; unlikely given 7.454 c/B vs 2.588 floor | NO (wide gap) |
| unicode_escapes/direct | W5/W6 proof-only limits | no kernel route | D3/D4 | Fresh samply; no kernel route visible | NO (no route) |
| distinct_values/direct | W5 string blocked; W7 digest | no kernel route | D3/D4 | Fresh samply; digest insufficient | NO (no route) |
| y_string_unicode/direct | W5/W6 proof-only limits | no kernel route | D3/D4 | Fresh samply; escape-heavy (6849.9 i/B) shows no kernel opportunity | NO (no route) |

**Reopen Candidates**: **3 rows** with modest fresh ASM/union differential potential:
1. **twitter/direct** — output_digest_hash u64x2 SIMD candidate
2. **github_events/direct** — dispatch unroll/tail optimization
3. **update_center/direct** — hash u64x2 SIMD candidate

---

## §5 "Perfect" Parse-Time Framing

### Memory Bandwidth Ceiling

Apple M5 Max memory bandwidth: ~130 GB/s (LPDDR5X stacks).  
CPU core frequency: ~3.5 GHz.

**Theoretical instruction-throughput floor** (single-core):
- Assume 4–6 instructions per clock sustained (in-order M5, high ILP code)
- Assume average instruction cost to parse one byte: 3–8 CPU operations
- **Lower bound**: 130 GB/s ÷ (1 byte × avg 5 ops) = 26 GB/s ÷ theoretical ops floor ≈ **20 Mbps minimum achievable**
- **Upper bound (aggressive loop unroll + SIMD)**: 130 GB/s with 2–4 byte-parallel paths ≈ **50–100 Mbps sustainable**

### Per-Corpus Per-Plane Theoretical Floors

#### JSON Direct Residuals (sample rows from REDRESS-119)

| Corpus | T1 Current (Mbps) | IPC Current | Memory Bound Est. (Mbps) | Instruction Bound Est. (Mbps) | Gap to Perfect |
|---|---:|---:|---:|---:|---|
| twitter/direct | 12228 | 2.985 | ~25 | ~18 | +7.23 Mbps (57% headroom) |
| canada/direct | 10362 | 15.483 | ~25 | ~22 | +2.64 Mbps (25% headroom) |
| github_events/direct | 12362 | 28.584 | ~25 | ~23 | +1.38 Mbps (11% headroom) |
| y_string_unicode/direct | 3544 | 685.289 | ~25 | ~20 (ALU-bound at high i/B) | ~5–10 Mbps (no path) |

**Interpretation**: Direct residuals with IPC >15 are memory-bound (cache-resident JSON objects); rows with IPC >100 are instruction-throughput bound (high escape content). The 57% headroom for twitter suggests memory-level optimization space (buffer pipelining, cache-aware dispatch). High-IPC rows (y_string_unicode) have no instruction-level headroom.

#### CSS L4 Declaration Values

| Metric | Value |
|---|---|
| Fixture size | 187 bytes |
| Current T1 | 429.34 Mbps |
| Estimated c/B | 6.5 |
| lightningcss c/B | 16.6 |
| Memory-bound floor | ~25 Mbps |
| Instruction-bound floor (4–6 IPC sustained, 3–6 ops/byte) | ~20–30 Mbps |
| **Theoretical ceiling (2–4 byte-parallel SIMD)** | **50–100 Mbps** |

**Gap to Perfect**: CSS L4 at 429 Mbps is already 4.3–8.6× above the instruction-bound floor. Further gains require 2+ byte-parallel tokenization state machines or full-cascade SIMD (unlikely for CSS's branchy dispatch). **Realistic SK-V13 target**: hold 429 Mbps and use CSS as proof-of-admission, not as new optimization focus.

---

## §6 SK-V13 S-P1 Capture Specification

### Capture Scope

| Dimension | Coverage |
|---|---|
| **JSON corpora** | 13 N-direct residual rows (from REDRESS-119) + 4 guard rows = **17 corpora** across parse_only, direct_to_struct, real_typed_struct planes |
| **CSS corpora** | css_l4/declaration_values at direct_to_struct plane; frozen 187-byte fixture + larger corpus (if available) |
| **Planes** | parse_only (JSON only); direct_to_struct (JSON + CSS); real_typed_struct (JSON only); css_l4_declaration_value_fact_stream (CSS only) |
| **Tracks** | Track 1 (generated); Track 2 (hand-coded/oracle); Comparators (sonic-rs direct; cssparser oracle; lightningcss CSS) |
| **Host** | aarch64-apple-darwin (Apple M5 Max or similar Arm64 machine) |

### Capture Tools & Commands

#### Phase 1: PMU Baseline via proc_pid_rusage

```bash
# Profile binary precompilation
RUSTFLAGS="-C target-cpu=native" cargo build --release -p profile_direct
RUSTFLAGS="-C target-cpu=native" cargo build --release -p profile_parse
RUSTFLAGS="-C target-cpu=native" cargo build --release -p profile_typed
cargo build --release -p profile_css_l4

# JSON PMU capture (JSON planes)
for corpus in twitter canada citm_catalog apache_builds github_events update_center mesh random \
              gsoc-2018 marine_ik instruments numbers unicode_mixed unicode_escapes distinct_values y_string_unicode
do
  # Direct plane
  RUSTFLAGS="-C target-cpu=native" /tmp/skv13-p1-target/release/profile_direct <iters> $corpus track1 > /tmp/skv13-p1/pmu/${corpus}-direct-track1.log 2>&1
  RUSTFLAGS="-C target-cpu=native" /tmp/skv13-p1-target/release/profile_direct <iters> $corpus track2 > /tmp/skv13-p1/pmu/${corpus}-direct-track2.log 2>&1
  
  # Parse plane
  RUSTFLAGS="-C target-cpu=native" /tmp/skv13-p1-target/release/profile_parse <iters> $corpus track1 > /tmp/skv13-p1/pmu/${corpus}-parse-track1.log 2>&1
  RUSTFLAGS="-C target-cpu=native" /tmp/skv13-p1-target/release/profile_parse <iters> $corpus track2 > /tmp/skv13-p1/pmu/${corpus}-parse-track2.log 2>&1
  
  # Typed plane (subset for real_typed_struct admissions)
  RUSTFLAGS="-C target-cpu=native" /tmp/skv13-p1-target/release/profile_typed <iters> $corpus track1 > /tmp/skv13-p1/pmu/${corpus}-typed-track1.log 2>&1
done

# CSS PMU capture
cargo run --release -p profile_css_l4 -- /tmp/skv13-p1 declaration_values track1 > /tmp/skv13-p1/pmu/css-l4-track1.log 2>&1
cargo run --release -p profile_css_l4 -- /tmp/skv13-p1 declaration_values oracle > /tmp/skv13-p1/pmu/css-l4-oracle.log 2>&1
cargo run --release -p profile_css_l4 -- /tmp/skv13-p1 declaration_values lightningcss > /tmp/skv13-p1/pmu/css-l4-lightningcss.log 2>&1
```

Output: Cycles + instructions per corpus per plane per track from proc_pid_rusage(RUSAGE_INFO_V5) ri_cycles, ri_instructions fields.

#### Phase 2: Samply Hot-Leaf Attribution

```bash
# Install samply if needed
cargo install samply

# JSON direct (sample 3–5 slow corpora for hot-leaf focus)
samply record -o /tmp/skv13-p1/samply/twitter-direct.prof /tmp/skv13-p1-target/release/profile_direct 5000 twitter track1
samply record -o /tmp/skv13-p1/samply/update_center-direct.prof /tmp/skv13-p1-target/release/profile_direct 5000 update_center track1
samply record -o /tmp/skv13-p1/samply/y_string_unicode-direct.prof /tmp/skv13-p1-target/release/profile_direct 5000 y_string_unicode track1

# CSS declaration values
samply record -o /tmp/skv13-p1/samply/css-l4-track1.prof cargo run --release -p profile_css_l4 -- /tmp/skv13-p1 declaration_values track1
samply record -o /tmp/skv13-p1/samply/css-l4-oracle.prof cargo run --release -p profile_css_l4 -- /tmp/skv13-p1 declaration_values oracle

# Export stacks: samply export-to-json --file <prof.json> <prof.prof>
```

Output: Per-function self-time % per corpus; callstack attribution.

#### Phase 3: xctrace Time Profiler (macOS native)

```bash
# JSON direct sampled corpus
xctrace record --output /tmp/skv13-p1/xctrace/twitter-direct.trace \
  --template "Time Profiler" \
  -- /tmp/skv13-p1-target/release/profile_direct 3000 twitter track1

xctrace record --output /tmp/skv13-p1/xctrace/css-l4-track1.trace \
  --template "Time Profiler" \
  -- cargo run --release -p profile_css_l4 -- /tmp/skv13-p1 declaration_values track1

# Export XML: xctrace export --output results.csv --xpath 'trace/run[@number="1"]//frame[contains(@name, "parse")]' <.trace>
```

Output: XML Time Profiler self-time per frame; symbol attribution.

### TSV Output Schema (skv13-p1-replay.tsv extension)

```
lane | family | plane | corpus | mode | iters | track | pmu_cycles | pmu_instructions | pmu_c_per_b | pmu_ipc | samply_status | xctrace_primary | xctrace_secondary | notes
pmu  | pmu-direct | direct | twitter | track1 | 12000 | T1 | 5430000000 | 45600000 | 2.95 | 8.40 | samply-collected | output_digest_hash | container_dispatch | CSS L4 proof baseline
pmu  | pmu-direct | direct | twitter | track2 | 12000 | T2 | 5100000000 | 42800000 | 2.77 | 8.40 | samply-collected | container_dispatch | output_digest_hash | oracle baseline
...
pmu  | pmu-css-l4 | css_l4_declaration_value_fact_stream | declaration_values | native-fixed-187b | 100 | T1 | 400000000 | 2600000 | 6.50 | 6.50 | samply-collected | declared_value_parser | token_type_dispatch | Fresh CSS L4 baseline
```

### Gate Consumption

After S-P1 capture:
1. Re-run `gate --skv12-css-l4-sota-report` on updated Criterion `new/` lanes (if Criterion is re-run)
2. Re-run `gate-json --check-results` on any JSON guard row movement (expected: NO movement)
3. Archive TSV under `restart/skinny/tranches/sk-v13/research/p1/skv13-p1-replay.tsv`
4. Consume in S-P2 hot-leaf analysis per REDRESS-127 follow-on

---

## §7 Summary & Gate Decisions

### SK-V12-close Profile Verdict: STALE FOR CSS L4

- **CSS L4**: No PMU/xctrace capture; only Criterion 30-sample native bench authority
- **JSON 13 N-direct**: P1 TSV pre-pin baseline dated 2026-05-20; valid for fixpoint reference but stale under pin
- **Action**: SK-V13 S-P1 must produce fresh PMU for both CSS L4 + JSON residuals

### Reopen Candidate Count: **3 rows**

From REDRESS-119's 13 N-direct residuals:
1. **twitter/direct** — hash u64x2 SIMD (D4)
2. **github_events/direct** — dispatch unroll (D4)
3. **update_center/direct** — hash u64x2 SIMD (D4)

All three require fresh microbench + samply + parity proof under the unblocked D4 ASM-gen pin.

### Gap-to-Perfect Frame

- **JSON direct**: Memory-bound rows have 25–57% headroom to theoretical ceiling (25–30 Mbps); instruction-bound rows (i/B >100) have no headroom
- **CSS L4**: 429 Mbps is 4.3–8.6× above instruction-throughput floor (50–100 Mbps realistic SIMD ceiling); hold current performance; use as admission proof
- **SK-V13 focus**: CSS L4 remains admission proof; JSON residuals require fresh PMU for SK-V12→SK-V13 differential proof

---

**Document scope valid. Ready for SK-V13 S-P1 capture gate.**
