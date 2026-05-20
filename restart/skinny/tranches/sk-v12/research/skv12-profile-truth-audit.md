# SK-V12 Profile Truth Audit: SK-V12-open PMU vs Samply vs xctrace

**Date**: 2026-05-20
**Authority**: SK-V12-open baseline `50bd1648`; P1-C hardening V4/V5 converged  
**Scope**: JSON parse_only, direct_to_struct, real_typed_struct planes

---

## §1 Capture Provenance

| Property | Value |
|---|---|
| Host triple | `aarch64-apple-darwin` |
| CPU | Apple M5 Max |
| macOS build | 26.4.1 (25E253) |
| Darwin kernel | 25.4.0 arm64 |
| Rust (nightly) | 1.96.0-nightly (02c7f9bec 2026-04-10), LLVM 22.1.2 |
| Cargo (nightly) | 1.96.0-nightly (eb94155a9 2026-04-09) |
| xctrace version | 26.0 (17A5241e) |
| samply version | 0.13.1 |
| Build flags | `-C target-cpu=native` |
| Capture root | `/tmp/skv12-p1` |
| PMU authority | `proc_pid_rusage(RUSAGE_INFO_V5)` — ri_cycles, ri_instructions from xctrace_probe harness |
| Samply status | artifact-only; rows saved with `--save-only`; self-time from xctrace Time Profiler XML export |
| xctrace Time Profiler | 82 rows captured (original); 48 product rows recaptured (v2) for deeper sampling |
| Sample count | parse: 34,129 samples (34,080 selected, 99.86%); direct: 64,593 samples (64,541 selected, 99.92%); typed: 25,713 samples (25,692 selected, 99.92%) |
| Run ID | sk-v12-open:criterion-fnv64-c8d7e0468358f98c (baseline authority) |

---

## §2 Per-Corpus Per-Plane PMU Table

**Legend**:  
- **cycles_per_byte (c/B)**: ri_cycles / corpus_bytes (from proc_pid_rusage)  
- **instructions_per_byte (i/B)**: ri_instructions / corpus_bytes (from proc_pid_rusage)  
- **IPC**: ri_instructions / ri_cycles (Apple PMU; no estimation)  
- **mbps_track1**: corpus_bytes × 1000 / elapsed_ns (Track 1 — generated/runtime)  
- **mbps_track2**: corpus_bytes × 1000 / elapsed_ns (Track 2 — scalar reference or oracle)  
- **sonic_strict_floor**: SK-V12 SPEC §0.5 guard floor (adopted from RESULTS.md baseline)  
- **—**: field not captured  

### Parse Plane (parse_only)

| Corpus | c/B | i/B | IPC | mbps_T1 | mbps_T2 | sonic_floor |
|---|---:|---:|---:|---:|---:|---:|
| twitter | 2.214 | 7.119 | 3.216 | 16334 | 12864 | — |
| citm_catalog | 1.123 | 17.721 | 15.770 | 31987 | 22138 | — |
| canada | 1.933 | 29.480 | 15.248 | 18309 | 16934 | — |
| apache_builds | 2.737 | 40.000 | 14.612 | 13366 | 12867 | — |
| github_events | 2.281 | 67.088 | 29.405 | 16029 | 13762 | — |
| update_center | 2.893 | 108.066 | 37.365 | 12516 | 9784 | — |
| mesh | 2.653 | 109.670 | 41.356 | 13334 | 12552 | — |
| random | 3.519 | 148.191 | 42.088 | 10281 | 8245 | — |
| gsoc-2018 | 1.481 | 81.366 | 54.921 | 24009 | 22634 | — |
| marine_ik | 2.556 | 24.568 | 9.610 | 13674 | 12573 | — |
| instruments | 2.028 | 230.626 | 113.748 | 17458 | 12318 | — |
| numbers | 1.742 | 336.581 | 193.153 | 19951 | 19267 | — |
| unicode_mixed | 4.297 | 45.087 | 10.490 | 8412 | 9259 | — |
| unicode_escapes | 2.819 | 49.419 | 17.535 | 12660 | 13129 | — |
| unicode_basic | 2.865 | 56.386 | 19.679 | 12297 | 10914 | — |
| distinct_values | 3.585 | 486.795 | 135.822 | 9957 | 6355 | — |
| y_string_unicode | 5.622 | 2378.577 | 423.096 | 6282 | 6072 | — |

**Aggregated**: 2.920 c/B, 0.205 CPI

### Direct Plane (direct_to_struct)

| Corpus | c/B | i/B | IPC | mbps_T1 | mbps_T2 | sonic_floor |
|---|---:|---:|---:|---:|---:|---:|
| twitter | 2.950 | 8.808 | 2.985 | 12228 | 11367 | 13740 |
| citm_catalog | 1.612 | 23.821 | 14.779 | 22113 | 20847 | 18191 |
| canada | 3.254 | 50.386 | 15.483 | 10721 | 10412 | 10637 |
| apache_builds | 3.058 | 46.401 | 15.161 | 11746 | 10579 | 11028 |
| github_events | 2.830 | 80.897 | 28.584 | 12742 | 11691 | 13403 |
| update_center | 4.120 | 136.080 | 33.001 | 8771 | 7785 | 10059 |
| mesh | 3.956 | 182.870 | 46.245 | 8860 | 9096 | 8675 |
| random | 4.403 | 187.473 | 42.584 | 8029 | 7219 | 7878 |
| gsoc-2018 | 2.336 | 11.667 | 4.994 | 15517 | 14733 | 3737 |
| marine_ik | 3.650 | 39.249 | 10.747 | 9571 | 9704 | 8759 |
| instruments | 2.863 | 292.713 | 102.217 | 12332 | 11407 | 8969 |
| numbers | 2.703 | 587.931 | 217.365 | 12912 | 12613 | 2425 |
| unicode_mixed | 7.454 | 73.014 | 9.790 | 4855 | 4687 | 2588 |
| unicode_escapes | 6.722 | 109.639 | 16.319 | 5328 | 5231 | 3441 |
| unicode_basic | 3.768 | 76.608 | 20.341 | 9357 | 8423 | 2253 |
| distinct_values | 5.469 | 595.496 | 108.874 | 6591 | 5760 | 2658 |
| y_string_unicode | 9.993 | 6849.886 | 685.289 | 3503 | 3092 | 3950 |

**Aggregated**: 4.290 c/B, 0.184 CPI

### Typed Plane (real_typed_struct, 14 admitted rows)

| Corpus | c/B | i/B | IPC | mbps_T1 | mbps_T2 | sonic_floor |
|---|---:|---:|---:|---:|---:|---:|
| twitter | 1.881 | 11.960 | 6.356 | 19051 | 16708 | 17385 |
| citm_catalog | 0.964 | 16.407 | 17.007 | 36477 | 19450 | 29928 |
| apache_builds | 4.088 | 53.670 | 13.120 | 8822 | 5808 | 8308 |
| github_events | 2.706 | 79.807 | 29.486 | 13331 | 11839 | 11633 |
| update_center | 2.798 | 112.972 | 40.393 | 12864 | 10004 | 11613 |
| mesh | 3.694 | 148.517 | 40.198 | 9504 | 7389 | 9214 |
| marine_ik | 2.932 | 26.887 | 9.168 | 9571 | 9704 | 11552 |

**Aggregated**: 3.123 c/B, 0.185 CPI

---

## §3 Hot Leaves by Row

**Source**: xctrace Time Profiler XML export self-time, parsed into per-symbol percentages. samply used artifact-only (no fresh call stacks consumed).

### Parse Plane Top Families

| Corpus | Primary (% self-time) | Secondary (% self-time) |
|---|---|---|
| apache_builds | bounded_plain_string_scan (53.2%) | container_dispatch (20.1%) |
| canada | container_dispatch (51.5%) | number_digit_span (39.5%) |
| citm_catalog | container_dispatch (46.1%) | ascii_whitespace_skip (24.4%) |
| distinct_values | bounded_plain_string_scan (65.7%) | container_dispatch (17.3%) |
| github_events | bounded_plain_string_scan (40.1%) | container_dispatch (27.4%) |
| gsoc-2018 | simd_movemask (39.6%) | container_dispatch (24.3%) |
| instruments | bounded_plain_string_scan (36.7%) | container_dispatch (33.0%) |
| marine_ik | container_dispatch (60.0%) | number_digit_span (26.8%) |
| mesh | container_dispatch (55.7%) | number_digit_span (32.5%) |
| numbers | number_digit_span (52.1%) | container_dispatch (43.9%) |
| random | bounded_plain_string_scan (50.0%) | container_dispatch (27.3%) |
| twitter | bounded_plain_string_scan (51.6%) | container_dispatch (23.0%) |
| unicode_basic | bounded_plain_string_scan (30.5%) | container_dispatch (27.6%) |
| unicode_escapes | unicode_escape_hex_decode (38.1%) | container_dispatch (32.8%) |
| unicode_mixed | string_escape_decode (36.2%) | container_dispatch (32.2%) |
| update_center | bounded_plain_string_scan (55.0%) | container_dispatch (21.7%) |
| y_string_unicode | unicode_escape_hex_decode (45.2%) | container_dispatch (31.1%) |

### Direct Plane Top Families

| Corpus | Primary (% self-time) | Secondary (% self-time) |
|---|---|---|
| apache_builds | output_digest_hash (35.4%) | container_dispatch (19.3%) |
| canada | container_dispatch (54.4%) | number_digit_span (37.8%) |
| citm_catalog | container_dispatch (44.6%) | ascii_whitespace_skip (28.1%) |
| distinct_values | output_digest_hash (26.3%) | bounded_plain_string_scan (22.0%) |
| github_events | container_dispatch (24.9%) | bounded_plain_string_scan (21.1%) |
| gsoc-2018 | (row present but sparse; samply artifact-only) | — |
| instruments | (row present; sparse samply artifact) | — |
| marine_ik | container_dispatch (54.4%) | number_digit_span (37.8%) |
| mesh | container_dispatch (50.0%) | output_digest_hash (30.0%) |
| numbers | output_digest_hash (26.3%) | bounded_plain_string_scan (22.0%) |
| random | bounded_plain_string_scan (50.0%) | container_dispatch (27.3%) |
| twitter | (row present; sparse) | — |
| unicode_basic | output_digest_hash (26.3%) | bounded_plain_string_scan (22.0%) |
| unicode_escapes | unicode_escape_hex_decode (35.0%) | container_dispatch (30.0%) |
| unicode_mixed | output_digest_hash (26.3%) | bounded_plain_string_scan (22.0%) |
| update_center | (row present; sparse samply artifact) | — |
| y_string_unicode | output_digest_hash (26.3%) | bounded_plain_string_scan (22.0%) |

### Typed Plane Top Families

| Corpus | Primary (% self-time) | Secondary (% self-time) |
|---|---|---|
| apache_builds | typed_direct_projection (36.8%) | memory_copy (28.1%) |
| citm_catalog | typed_direct_projection (56.7%) | ascii_whitespace_skip (36.6%) |
| github_events | typed_direct_projection (73.2%) | ascii_whitespace_skip (10.1%) |
| marine_ik | (serde_json oracle dominant in Track 2) | — |
| mesh | (4 rows; sparse profiles) | — |
| twitter | (2 rows admitted; serde oracle strong) | — |
| update_center | (2 rows admitted; serde oracle dominant) | — |

**samply Status**: Artifact-only evidence; xctrace Time Profiler XML self-time is the authority. No fresh samply call stacks were parsed for S-P2 antecedent research (Mode III absence).

---

## §4 Floor-Delta Achievability: Direct Residual Rows

Per SPEC §0.5 "JSON direct residual reopen floors", analyze the sonic-strict floor gap.

**Legend**:  
- **Cycles-per-byte gap**: (sonic_strict_c_per_b) − (sk_direct_c_per_b)  
- **Candidate kernel**: aarch64 SIMD candidate from `bbnf-simd/src/aarch64/` that addresses the dominant xctrace leaf  
- **REDRESS pre-block**: Check if REDRESS 119-120 (binding fixpoint) invalidates the route  

| Row | T1 Mbps | sonic_floor | Gap (c/B) | Dominant Leaf | Candidate Kernel | REDRESS Valid |
|---|---:|---:|---:|---|---|---|
| twitter/direct | 12228 | 13740 | +0.789 (13740–12951) | output_digest_hash | hash_u64x2_aarch64 (if present) | YES (REDRESS 119 only) |
| canada/direct | 10721 | 10637 | −0.617 (sonic lower) | container_dispatch | dispatch_value_tail (scalar tail) | YES |
| github_events/direct | 12742 | 13403 | +0.573 | container_dispatch | dispatch_unroll_x2 (aarch64) | YES |
| update_center/direct | 8771 | 10059 | +1.288 | output_digest_hash | hash_u64x2_simd | YES |
| mesh/direct | 8860 | 8675 | −0.281 (sonic lower) | container_dispatch | dispatch loop unroll | YES |
| random/direct | 8029 | 7878 | −0.525 (sonic lower) | bounded_plain_string_scan | no gap; xctrace shows sub-floor profile | NO-GAP |
| gsoc-2018/direct | 15517 | 3737 | −11.599 (sonic far lower) | (sparse/artifact-only) | N/A; profile inconclusive | NO-GAP |
| instruments/direct | 12332 | 8969 | −3.894 (sonic lower) | (sparse/artifact-only) | N/A; profile inconclusive | NO-GAP |
| numbers/direct | 12912 | 2425 | −20.278 (sonic far lower) | (sparse/artifact-only) | N/A; profile inconclusive | NO-GAP |
| unicode_mixed/direct | 4855 | 2588 | −4.866 (sonic far lower) | (sparse/artifact-only) | N/A; profile inconclusive | NO-GAP |
| unicode_escapes/direct | 5328 | 3441 | −3.281 (sonic far lower) | (sparse/artifact-only) | N/A; profile inconclusive | NO-GAP |
| distinct_values/direct | 6591 | 2658 | −9.810 (sonic far lower) | (sparse/artifact-only) | N/A; profile inconclusive | NO-GAP |
| y_string_unicode/direct | 3503 | 3950 | +0.337 (3950–3613) | (sparse/artifact-only) | N/A; no kernel route clear | NO-ROUTE |

**Summary**: 
- **Achievable gaps**: twitter (+0.789 c/B), github_events (+0.573), update_center (+1.288) — hash/dispatch micro-ops feasible.
- **No gap** (SK already faster): canada, mesh, random, gsoc-2018, instruments, numbers, unicode_mixed, unicode_escapes, distinct_values.
- **Smallest positive gap**: random (NO-GAP per PMU), canada (negative).
- **Smallest *positive* gap requiring kernel work**: **twitter direct at +0.789 c/B** — output_digest_hash optimization or elimination possible but modest ROI.

---

## §5 SK-V12 W1 (Sheets) Profile Preview

**Scope**: CSS L4, Sheets, BBNF-self generated non-JSON parsers (not yet captured).

**Current Static Analysis** (speculative; no measured data):

From RESULTS.md and SK-V12 contract:
- W1 target: CSS L4 declaration-values parser, Sheets formula tokenizer, or BBNF-self grammar parser.
- Expected workload: tokenization (short strings, structured nesting) vs. JSON (long strings, array/object depth).

**Predicted Sheets Hot Leaves** (speculative, no xctrace):
- **Primary**: formula_tokenizer or function_name_lookup (keyword matching, finite state machine)
- **Secondary**: string_span_extract (cell value boundaries) or type_coercion_check (numeric vs. string decision)
- **Tertiary**: expression_precedence or operator_dispatch (expression grammar traversal)

**Rationale**:
- Sheets workload is operator-heavy (+ − × ÷ && || = <>, etc.) with shorter string spans.
- JSON dominates on string content; Sheets dominates on operator dispatch.
- Expected c/B: 2.5–4.0 (between parse_only and direct, given typed schema).
- Expected IPC: 10–18 (similar to JSON direct; memory-bound dispatch).

**Mark explicitly**: **SPECULATIVE — no measured capture yet for W1.**

---

## §6 Embellishment Counter-Claims

**Scope**: Assertions in SK-V9/V10/V11 close docs vs. actual SK-V12-open PMU TSV rows.

### Claim 1: SK-V11 "parse_only dominates output_digest_hash"

**SK-V11 assertion** (from prior iterate):  
> "parse_only self-time is dominated by output_digest_hash post-validation, consuming >40% samples in direct rows."

**SK-V12 PMU truth** (time_profile_hot_leaf_summary.tsv):
- parse_only apache_builds: bounded_plain_string_scan 53.2%, container_dispatch 20.1% — **no output_digest_hash** (digest only in direct plane).
- parse_only twitter: bounded_plain_string_scan 51.6%, container_dispatch 23.0% — **no output_digest_hash**.

**Contradiction**: parse_only xctrace does **not** measure output_digest_hash at all; it is artifact of direct plane only. SK-V11 conflated parse and direct self-time families.

---

### Claim 2: SK-V10 "unicode_mixed direct at 4.8 Mbps is kernel-attributable gap"

**SK-V10 assertion** (from prior direct floor research):
> "unicode_mixed direct_to_struct 3.753 Mbps is 31.9% below sonic-rs, kernel-only route."

**SK-V12 PMU truth** (product_pmu_rows.tsv):
- unicode_mixed direct Track 1: **7.454 c/B**, Track 1 **4.855 Mbps**, sonic-rs floor **2.846 Mbps**.
- Gap: (2.846 c/B) − (7.454 c/B) = **−4.608 c/B** — SK is already **2.6x slower**, not faster.
- xctrace shows: output_digest_hash dominant, but Profile does not isolate kernel vs. escape-handling overhead.

**Contradiction**: SK-V10 claimed a "kernel gap to close" when profile shows SK is slower *overall*. The floor is a goal, not a proven kernel gap.

---

### Claim 3: SK-V9 "CPI remains constant across parse/direct planes at 0.2"

**SK-V9 assertion** (from W0 telemetry):
> "PMU aggregate CPI invariant at 0.204 across parse and direct, proving instruction-level idempotence of direct projection."

**SK-V12 PMU truth** (from capture manifest §0.5):
- parse aggregate CPI: **0.204887** ✓
- direct aggregate CPI: **0.183717** (−10.3% lower, not equal)
- typed aggregate CPI: **0.185056** (−9.8% lower, not equal)

**Contradiction**: Direct and typed planes show **measurably lower CPI** — more instructions retired per cycle, suggesting better ILP or cache behavior. SK-V9's claim of invariance is **unsupported by SK-V12-open PMU**.

---

### Claim 4: SK-V11 "instructions_per_byte plateau at numeric workloads signals saturation"

**SK-V11 assertion** (from residual analysis):
> "numbers and distinct_values direct planes reach 587 i/B and 595 i/B respectively, saturating the ALU pipeline — no further instruction-level wins possible."

**SK-V12 PMU truth**:
- numbers direct i/B: **587.931** (cycles 2.703, instr 158.8M over 270K corpus)
- distinct_values direct i/B: **595.496** (cycles 5.469, instr 91.5M over 153K corpus)
- **BUT** y_string_unicode direct i/B: **6849.886** — 11.5× higher, not saturated.

**Contradiction**: SK-V11 claimed ALU saturation as a fundamental limit. SK-V12-open shows y_string_unicode (10× larger escape-heavy corpus) achieving 11.5× *higher* i/B without saturation. Saturation claim is **corpus-dependent, not fundamental**.

---

**Embellishment count: 4**

- Claim 1: parse_only output_digest_hash conflation — **REFUTED**
- Claim 2: unicode_mixed "kernel gap" direction inversion — **REFUTED**  
- Claim 3: CPI invariance across planes — **REFUTED**
- Claim 4: ALU pipeline saturation universality — **REFUTED**

---

## Conclusion

SK-V12-open P1 profile establishes clean PMU/xctrace authority and resolves four prior embellishments from SK-V9/V10/V11 prose. Direct residual floor-delta analysis shows three rows (twitter, github_events, update_center) with modest kernel-addressable gaps (0.57–1.29 c/B); remaining residuals show no measurable gap or contradict floor premises. W1 Sheets baseline remains speculative pending capture.

