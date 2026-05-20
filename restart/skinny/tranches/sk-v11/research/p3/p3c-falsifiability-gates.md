# SK-V11 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-20.
Scope: measurable wave gates for SK-V11 direct closure, guard preservation, non-JSON proof, and SIMD/ASM micro-prove-first.
Output: this file.
Pass Alpha goalset: close the 13 SK-V11-open direct residual rows or record per-row uncloseable proofs; preserve the 7 typed and 4 direct A/GO guards; land at least one admitted benchmarked non-JSON generated direct or typed parser intervention; keep parse_only diagnostic and W3 union/event substrate pre-blocked.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

1. SK-V11-open is the only JSON floor authority for S-P3. W0 froze run
   `sk-v9-open:criterion-fnv64-c8d7e0468358f98c` with
   `gate-json --with-cost-facts --check-results` green
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:11-20`).
   The open result surface is 16 `parse_only S / NO-GO`, 1 `parse_only
   L / NO-GO`, 13 `direct_to_struct N-direct / NO-GO`, 4
   `direct_to_struct A / GO`, and 7 `real_typed_struct A / GO`
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:24-32`).

2. Direct closure is per-row and strict. Each residual direct row must clear
   `ceil(sonic-rs direct / 1.10)` on generated Track 1 and independent Track 2,
   or record a per-row measured REDRESS uncloseable proof
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:41-44`,
   `restart/skinny/tranches/sk-v11/SYNTHESIS.md:79-82`). W0-clamped rows
   `instruments`, `numbers`, and `unicode_mixed` are not retroactive admits;
   they need behavior-wave provenance before row movement
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:54-57`).

3. Existing wins are binding maintain surfaces. The guard rows are the seven
   typed rows and four direct rows in W0
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:59-80`).
   This artifact sets exact SK-V11 maintain floors from the W0 numbers:
   direct guards use `max(floor(SK-V11-open track Mbps * 0.98),
   ceil(sonic_direct / 1.10))` on both tracks; typed guards use
   `max(floor(SK-V11-open Track 1 Mbps * 0.98), ceil(sonic_typed / 1.10))`
   for generated Track 1 and `floor(SK-V11-open Track 2/oracle Mbps * 0.98)`
   for the independent oracle. The typed Track 2/oracle is an independence and
   parity guard, not a typed SOTA floor.

4. S-P2 converged the candidate pool. C1-C7 are parser primitives; C8 is
   benchmark/oracle or product-host sink only; C9 is Lock-1/output-plane
   accounting only; proof/support rows are not standalone row movers
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:23-32`).
   P2-F binds C1-C7 to byte masks, bounded string scans, escape/hex segments,
   digit spans, layout skip, generated FIRST/prefix/lookahead dispatch, and
   movemask support with same-wave consumers only
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:39-68`).

5. Non-JSON generality is a gate, not prose. SK-V11 must benchmark at least
   one generated non-JSON direct or typed parser intervention, preferred CSS L4
   declaration values, then Sheets, then BBNF-self
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:152-167`;
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86-91`).
   If a wave cannot name a generated non-JSON row, an independent oracle on the
   same output plane, and before/after Mbps, its non-JSON claim is unmeasurable
   and rejects before redress.

6. Every SIMD/ASM route is micro-prove-first. AArch64 bodies need scalar
   reference, strict differential/checkasm where applicable, feature/fallback,
   same-host caller microbench, same-wave consumer, and a row gate
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:62-67`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:80-83`). Passing a primitive
   checkasm alone cannot admit a behavior wave; REDRESS 106-108 prove that
   parity-green or proof-only string/escape work can still miss caller or row
   gates (`skinny/REDRESS.md:3150-3222`).

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

The gates below are written so P3-B can split or merge target subsets while
preserving one invariant: a behavior wave may admit only rows named in its plan,
and every named admitted row must meet the corresponding floor in §3. A plan
that chooses a subset inherits the same guard block and revert protocol.

| Gate | Likely wave | Candidate families | Exit condition | Revert protocol |
|---|---|---|---|---|
| `G-W0-SK-V11-OPEN-LOCK` | W0 baseline / telemetry | telemetry only | W0 run id and gate command in §3.1 remain coherent; no parser/runtime/SIMD/codegen/generated behavior changes; all current JSON rows render required telemetry; `gate-json --with-cost-facts --check-results` passes. | Revert telemetry/report/gate edits; restore prior `RESULTS.md`; record REDRESS if the lock cannot be reproduced. |
| `G-W1a-NONJSON-GATE` | W1a non-JSON gate/report schema lane | C9 accounting + Lock 14 telemetry surface | Gate/report code rejects missing grammar domain, comparator/oracle, output plane, Track 2/oracle, run id, feature mask, same-wave consumer class, and producer-only non-JSON telemetry. No generated baseline authority and no behavior row admission are allowed. | Revert gate/report/schema/fixture changes if the gate accepts missing fields, producer-only telemetry, JSON-only generality, or any row movement. |
| `G-W1b-NONJSON-BASELINE` | W1b generated non-JSON baseline and oracle lane | C9 accounting + generated parser baseline surface | Exactly one generated non-JSON direct or typed parser baseline row exists for the selected grammar/workload, preferably CSS L4 declaration values, with generated Track 1 Mbps, independent Track 2/oracle Mbps, strict output equality, run id, host, flags, sample count, grammar id, output plane, and gate consumption. No intervention and no behavior row admission are allowed. | Revert harness/codegen/oracle/report changes if the row is absent, Track 2/oracle is coupled to Track 1, strict equality fails, or the gate cannot consume the baseline evidence. |
| `G-W2-CSS-GENERATED-INTERVENTION` | W2 CSS L4 generated direct/typed intervention proof | C1/C2/C4/C5/C6 with C7 support; C8 oracle only; C9 accounting | At least one generated CSS L4 declaration-values direct or typed parser row is benchmarked and admitted with generated Track 1 before/after Mbps, independent Track 2/oracle Mbps, strict output equality, primitive self-time, no generic-crate JSON policy, and at least `ceil(W1b_css_baseline_mbps * 1.01)` on the selected non-JSON row. JSON companion rows `github_events >= 13403`, `update_center >= 10059`, `random >= 7878`, or `instruments >= 8969` may admit only if named and measured. W2 may not create the first non-JSON baseline. | Revert generic/codegen/runtime changes if the CSS row is absent, unbenchmarked, oracle-coupled to Track 1, below the 1% improvement floor, leaks JSON policy into generic crates, or misses a named guard floor. |
| `G-W3-NUMERIC-SEQUENCE-DIRECT` | W3 numeric direct closure slice | C4 digit span/accumulate; D4 number-slot emit; optional C1/C5 support | Selected numeric/container rows must clear both-track floors: `canada` 10637, `mesh` 8675, `numbers` 2425, and `instruments` 8969 if selected. Redress selects one or two rows unless existing same-host microbench data justifies all four. `marine_ik/direct_to_struct` is a direct guard, not a target, and must hold its guard floors. Generated numeric output must match Track 2/oracle exactly. | Revert on numeric semantic mismatch, f64/fallback policy drift, no selected row admits, direct guard miss, or non-JSON numeric proof failure when generic code is touched. |
| `G-W4-DISPATCH-BYTESET-DIRECT` | W4 generated dispatch and byte-set control slice | C1, C5, C6 with C7 support; D1/D2 | Selected control-heavy rows must clear both-track floors: `twitter` 13740, `github_events` 13403, `update_center` 10059, `random` 7878, plus any W3 residual only if the plan names one same-wave consumer. Masks are transient and consumed in generated direct/typed or non-JSON same-loop code. | Revert on sidecar/class-column/retained-position creation, missing same-wave consumer, zero selected row admits, guard-floor miss, or generic JSON policy leakage. |
| `G-W5-STRING-SPAN-DIRECT` | W5 bounded string span and special-byte scan | C2; D3; C7 support | Selected string-heavy rows must clear both-track direct floors: `twitter` 13740, `github_events` 13403, `update_center` 10059, `random` 7878, `distinct_values` 2658, `gsoc-2018` 3737, `y_string_unicode` 3950. Redress selects one string/key consumer and at most two target rows. Same-wave consumer must be generated direct or typed string/key path; any SIMD body must pass strict parity and caller microbench. | Revert production/generator/SIMD patch on parity failure, missing same-wave consumer, retained wide-string fact, zero selected row admits, or guard-floor miss. Save `/tmp/skv11-wave{W}-rejected.patch`. |
| `G-W6-ESCAPE-SEGMENT-DIRECT` | W6 escaped segment and hex decode slice | C3; C2/D3 support; `HEX_QUARTET_X4_PROOF` proof-only unless a new source delta exists | Selected unicode rows must clear both-track direct floors: `unicode_escapes` 3441, `unicode_mixed` 2588, `y_string_unicode` 3950. Same-wave consumer must be new product work, not the already-consuming JSON `unescape_string` path alone. `unicode_basic/direct_to_struct` must hold its guard floors. | Revert on JSON surrogate policy entering generic code, x4 proof-only promotion, strict checkasm failure, no new product consumer, zero selected row admits, or `unicode_basic` guard miss. |
| `G-W7-DIGEST-SINK` | W7 output digest/hash host sink | C8 output sink/oracle only; C9 accounting | A fresh post-W6 profile still names `output_digest_hash` as limiting on selected residual rows; redress is capped to that profiled subset and selected rows must clear their §3.2 direct floors with strict Track 1/Track 2-or-oracle parity. C8 cannot enter generic parser crates or close a parser row without a product output sink consumer. | Revert on digest mismatch, hidden semantic string/hash side table, cache-hint-only route, no selected row admits, or guard-floor miss. |
| `G-W8-DIRECT-FIXPOINT` | W8 direct residual fixpoint and row reclamation | remaining measured C1-C8 routes; docs/gate by default | Every direct residual row is `A / GO` by its §3.2 floor or has a REDRESS uncloseable proof naming the attempted intervention, Track 1, Track 2, sonic direct Mbps, floor, guard result, and exhausted route. At least one non-JSON generated parser intervention has admitted. All guard floors hold. | If any row lacks admission or proof, W8 rejects and routes the unresolved row/gate to REDRESS or a scoped CHALLENGE-approved residual source patch. |
| `G-W9-CLOSE-SK-V11` | W9 close and Alpha feedback | documentation and gate reconciliation | Close documents agree with `RESULTS.md`, `REDRESS.md`, `SPEC.md`, and `DISPATCH-PROMPT.md`; no new row movement; no guard demotion; G-Alpha can be presented only after W1a-W8 dispositions are recorded. | If any row lacks admission/proof or the non-JSON axis lacks an admitted intervention, close is BLOCKED. No behavior source is edited in close. |

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

### §3.1 W0 Gate

`G-W0-SK-V11-OPEN-LOCK` is already satisfied by the W0 artifact if and only if
the following facts remain true when P3-F materializes the SPEC:

- Criterion root: `/tmp/skv11-open-criterion-3ce75df`.
- Target root: `/tmp/skv11-open-target-3ce75df`.
- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Command:
  `CARGO_TARGET_DIR=/tmp/skv11-open-target-3ce75df CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory`.
- Verification:
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`.
- Source behavior freeze: no parser/runtime/SIMD/codegen/generated JSON behavior
  source change is attributed to W0.

### §3.2 Direct Residual Floors

Every row-moving direct wave uses this table. A direct row admits only when
generated Track 1 and independent Track 2 are both greater than or equal to the
floor under one same-run strict direct comparator.

| Row | Track 1 open | Track 2 open | sonic direct | Binding floor |
|---|---:|---:|---:|---:|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 |

### §3.3 Direct Guard Maintain Floors

Any wave that touches direct output, direct report/gate logic, generated direct
code, `parse-that-regex`, `bbnf-simd`, or direct row dispositions must preserve
these floors on both tracks.

Formula: `max(floor(SK-V11-open track Mbps * 0.98), ceil(sonic_direct / 1.10))`.

| Row | Track 1 maintain floor | Track 2 maintain floor | sonic direct floor |
|---|---:|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 | 14119 |
| `apache_builds/direct_to_struct` | 11028 | 9996 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 | 7703 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 | 2140 |

### §3.4 Typed Guard Maintain Floors

Any wave that touches typed output, typed report/gate logic, `parse-that-regex`,
`bbnf-simd`, generated typed code, or generic codegen/runtime surfaces must
preserve these floors. Track 1 is the generated typed product floor. Track 2 is
the independent oracle maintain floor and must also preserve checksum/output
parity, but it is not compared to sonic typed for SOTA admission.

| Row | Track 1 maintain floor | Track 2/oracle maintain floor | sonic typed floor |
|---|---:|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 | 13646 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 | 18842 |
| `apache_builds/real_typed_struct` | 8308 | 6754 | 7370 |
| `github_events/real_typed_struct` | 11633 | 12029 | 11113 |
| `update_center/real_typed_struct` | 11613 | 10150 | 11334 |
| `mesh/real_typed_struct` | 9214 | 7739 | 8112 |
| `marine_ik/real_typed_struct` | 11552 | 9894 | 8191 |

### §3.5 Track, Oracle, And Micro-Proof Requirements

- Direct admission requires generated Track 1, independent Track 2, strict
  same-run `sonic_rs_direct_to_struct`, digest output-plane equality, and
  `gate-json` consumption in the same wave. Track 2 must not call Track 1 or
  read a hidden sidecar.
- Typed guard or new typed admission requires generated typed Track 1, an
  independent Track 2/oracle on the same output plane, serde/sonic typed
  comparators when a typed row moves, checksum/output parity, and same-wave
  gate consumption.
- Non-JSON admission requires a generated direct or typed parser row, an
  independent Track 2/oracle, strict semantic equality, before/after Mbps, and
  a gate-consumed grammar id. Acceptable rows are named in
  `G-W2-CSS-GENERATED-INTERVENTION`, with W1a responsible for the gate/report
  lane and W1b responsible for the generated baseline plus oracle lane.
- SIMD/ASM production requires a scalar reference, strict checkasm/differential
  test where applicable, feature-gated AArch64 dispatch with scalar fallback,
  same-host caller microbench, and same-wave product consumer. For SIMD/ASM
  body routes, the caller microbench floor is median `>= 1.08x` on the selected
  representative slices and no selected slice below `0.99x`, unless CHALLENGE
  raises the threshold. The production row gate still decides admission.
- Scalar-only parser refactors need at least one selected target row to improve
  by `>= 1.0%` median throughput or cycles/byte and no guard row below its
  maintain floor. A scalar refactor that improves no selected row is a measured
  reject, not an admit.

### §3.6 Unmeasurable-Gate Reject Rule

A wave plan returns REJECT before redress if any of these are missing:

- named corpus row or generated non-JSON row;
- concrete Mbps threshold or W1b non-JSON baseline threshold;
- generated Track 1 path;
- independent Track 2/oracle path;
- strict comparator or same-output oracle on the same output plane;
- scalar reference for the primitive;
- strict checkasm/differential plan for SIMD/ASM bodies;
- same-wave hot-path consumer;
- guard maintain block;
- `gate-json` or sibling gate consumer for every emitted telemetry field;
- revert protocol.

An unmeasurable gate cannot be repaired by wording such as "wired",
"integrated", "should improve", "visible in profile", PMU-only evidence,
parse-only improvement, or checkasm-only parity.

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

- No parse-only SOTA movement. `parse_only` rows are diagnostic concession rows
  and cannot satisfy any SK-V11 close gate
  (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:51-53`).
- No W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
  repair. REDRESS 96 and 97 measured faithful W3 implementations as uniform
  regressions; REDRESS 98 retires the gate (`skinny/REDRESS.md:2797-2949`).
- No sidecar, parser-owned projection, structural-position vector, whitespace
  cursor, retained class lane, aux density table, or second scanner. This blocks
  REDRESS 50, 51, 53, and 102 families.
- No string/Unicode proof-to-production shortcut. REDRESS 54, 60-64, 67-69,
  72, 82, 83, and 106-108 block decoded stats, retained wide scans, JSON-only
  StringBlock wrappers, parser-owned decoded scratch, x4 proof-only promotion,
  and reuse of the already-consuming `unescape_string` caller.
- No numeric fallback widening or generic JSON number policy in generic crates.
  REDRESS 80 remains closed.
- No object/key/value-byte carry, generic JSON container policy, new directive,
  BIR variant, `BackendShape`, public substrate API, or benchmark-private parser.
  REDRESS 63, 65, 70, 71, and 84 remain guardrails.
- PMULL/CTZ/EOR3/BCAX/cache hints are inventory or support until a later wave
  names source delta, scalar oracle, strict parity, feature/fallback,
  same-wave product consumer, and row gate. REDRESS 88-90 remain binding.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
