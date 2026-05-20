# SK-V12 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-20.
Scope: per-wave measurable gates for the SK-V12 generated non-JSON baseline, intervention, guard, and conditional JSON companion surface.
Output: this file.
Pass Alpha goalset: admit exactly one generated non-JSON direct or typed parser baseline, admit one measured grammar-generalized intervention on that same row at `ceil(baseline_mbps * 1.01)` or higher, preserve the 4 direct and 7 typed JSON guard rows, keep `parse_only` diagnostic, and keep JSON direct residuals pre-blocked unless the REDRESS 114-119 reopen bar is met.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis

SK-V12 has one material close axis: create a generated non-JSON direct or typed
baseline first, then consume that same row with one measured intervention. The
opening contract forbids a JSON-direct retry before that axis succeeds or
blocks, and it requires gate consumption for every new row or field
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:50`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:60`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:66`).

The current JSON surface is a guard surface, not the primary target:
`direct_to_struct` has 4 `A / GO` guards and 13 `N-direct / NO-GO` residuals;
`real_typed_struct` has 7 `A / GO` guards; `parse_only` is 16 `S / NO-GO`
plus `canada` as `L / NO-GO` and remains diagnostic only
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:92`,
`skinny/RESULTS.md:143`). REDRESS 119/120 close the JSON direct residual rows
as a measured fixpoint and route SK-V12 to the generated non-JSON baseline
(`skinny/REDRESS.md:3497`, `skinny/REDRESS.md:3531`).

The S-P2 pool supports that direction. P2-F names six conditional
parser/support families that can generalize only through generated metadata and
same-wave consumers: byte-set/classifier/movemask, bounded string span,
escape/hex decode, digit span, layout skip, and FIRST/prefix dispatch
(`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:31`).
P2-D contributes no selectable substrate candidate and keeps class-lane/union
substrates rejected
(`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:78`).
P2-B/P2-C make scalar reference, strict parity/checkasm, microbench, feature
fallback, and same-wave consumer mandatory for any SIMD/ASM body
(`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:20`,
`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:48`).

## §2 — Deliverable

### 2.1 Gate Names And Wave Set

P3-B may rename waves, but the falsifiability surface must preserve these gate
facts.

| Likely wave | Gate | Measurable target | Required threshold |
|---|---|---|---|
| W0 telemetry lock | `G-W0-SK-V12-OPEN` | JSON table, schema, run freshness, guard rows | full gate pass plus guard floors below |
| W1 generated non-JSON baseline | `G-W1-GENERATED-NONJSON-BASELINE` | exactly one selected non-JSON direct or typed baseline row | Track 1 >= 1 Mbps, independent Track 2/oracle >= 1 Mbps, sample count >= 30 |
| W2 selected-baseline intervention | `G-W2-SELECTED-NONJSON-INTERVENTION` | same grammar/workload/output plane as W1 | Track 1 >= `ceil(baseline_mbps * 1.01)` |
| W3 JSON direct companion, conditional | `G-W3-CONDITIONAL-JSON-COMPANION` | behavior dispatch against one named residual row, or routed block with no source/RESULTS movement | behavior clears selected residual floor on both tracks; routed block records material-reopen failure |
| W4 close | `G-W4-CLOSE` | close packet agreement | W1+W2 admitted, W1 admitted + W2 measured reject, or generated-baseline BLOCKED with measurement; guards preserved |

No W1 split is authorized by the V3 packet. If the selected W1 baseline cannot
fit in one redress, W1 records measured BLOCKED/REJECTED evidence or S-P3 must
revise the manifest before dispatching any split wave.

### 2.2 Full-Table Maintain Budget

Every behavior wave must either rerun and maintain the JSON guard table or
prove it did not touch JSON-producing paths and that `skinny/RESULTS.md` did
not change. A wave that refreshes JSON reports must measure these 11 guard
rows on the same host/run family as the wave. Any single miss is a wave FAIL
unless the wave records an explicit measured demotion in REDRESS.

Direct guard floors:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard floors:

| Row | Track 1 maintain | Track 2 oracle guard |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Full-table surface budget:

- The existing JSON surface remains 16 `parse_only S / NO-GO`, one
  `parse_only L / NO-GO`, 4 direct `A / GO`, 13 direct
  `N-direct / NO-GO`, and 7 typed `A / GO` unless a same-wave measured
  disposition changes it.
- `parse_only` rows cannot contribute to SK-V12 close or SOTA admission.
- W0-clamped rows (`instruments`, `numbers`, `unicode_mixed`) cannot admit by
  documentation-only accounting.
- New non-JSON rows must be consumed by the non-JSON companion gate or by
  `gate-json`/`gate` in the same wave; producer-only rows fail closed.

### 2.3 W0 Gate: `G-W0-SK-V12-OPEN`

Entry: S-P1 and S-P2 are converged, and the source baseline is the SK-V12
opening baseline `50bd1648`.

Exit gate:

- `gate-json --with-cost-facts --check-results` passes.
- The 10-outcome enum remains exactly `A C G I J K L M N-direct S`.
- The schema-v3 required identifiers remain validator-consumed; no emitted
  field is producer-only.
- The JSON main table matches the full-table surface budget above.
- The direct and typed guard floors in §2.2 hold if W0 rerenders JSON rows.
- If W0 does not rerender JSON rows, it records the inherited SK-V11/SK-V12
  freshness rebinding and no behavior row movement; W1+ behavior gates may not
  cite the inherited `SK-V9-open` row id as fresh wave evidence.
- Stale-run rejection: any new or moved row with a stale run id, mixed run ids
  across Track 1/Track 2/oracle/comparator, missing host triple, missing
  `RUSTFLAGS="-C target-cpu=native"`, or missing sample count fails closed.

Revert protocol: revert report/gate/result edits as one slice; preserve the
failed gate output in REDRESS and save `/tmp/skv12-waveW0-rejected.patch` if
source/report code changed.

### 2.4 W1 Gate: `G-W1-GENERATED-NONJSON-BASELINE`

Entry: W0 closed. CHALLENGE selects exactly one baseline target in this order:

1. `css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main`
2. `sheets/formula/{direct_to_struct|real_typed_struct}/main`
3. `bbnf_self/grammar/{direct_to_struct|real_typed_struct}/main`

Exit gate:

- Exactly one selected generated non-JSON baseline row is emitted.
- Generated Track 1 source path and generated runtime path are named.
- Independent Track 2 or oracle source path is named and marked
  `independent_verified`; it must not call generated Track 1, generated
  SinkOnly helpers, generated typed helpers, or hidden shared parser code.
- Strict output equality passes between generated Track 1 and the
  independent Track 2/oracle on the selected fixture corpus.
- Concrete throughput thresholds: generated Track 1 >= 1 Mbps, independent
  Track 2/oracle >= 1 Mbps, sample count >= 30, and benchmark artifact path
  present.
- The selected row records grammar id, domain, corpus/workload, row id, output
  plane, workload class, run id, host triple, feature mask, build flags,
  sample cost, sample count, baseline/provenance, same-wave consumer class,
  and fail-closed gate status.
- The accepted REDRESS 111 non-JSON gate/report lane consumes the row, or a
  same-wave replacement gate consumes an equivalent field set.
- No JSON policy appears in generic crates or runtime outside generated
  per-grammar modules.
- No JSON direct or typed row moves in this wave except guard maintain evidence.

Revert protocol: revert the selected codegen/runtime/bench/report/gate/RESULTS
slice, preserve the failed proof in REDRESS, save
`/tmp/skv12-waveW1-rejected.patch`, and block W2 until a baseline row admits.

### 2.5 W2 Gate: `G-W2-SELECTED-NONJSON-INTERVENTION`

Entry: W1 admitted one baseline row and recorded
`baseline_mbps` (the W1 generated Track 1 Mbps), output plane, fixture corpus,
oracle path, and row id. If any of those values is missing, W2 is unmeasurable
and returns REVISE before source work.

Exit gate:

- The intervention consumes the same grammar, workload, fixture corpus, and
  output plane as W1. It may not create the first measurable non-JSON row.
- The intervention target is
  `ceil(baseline_mbps * 1.01)`. Track 1 must meet or exceed that
  integer Mbps threshold.
- Independent Track 2/oracle remains >= 1 Mbps, remains independent, and
  strict output equality passes.
- Any SIMD/ASM body has an executable scalar reference, strict
  differential/checkasm parity, scalar/no-op fallback, and same-host
  microbench evidence. The microbench must show candidate throughput at least
  `ceil(scalar_reference_mbps * 1.01)` on the named representative slice.
- The same-wave consumer is the generated non-JSON parser/product path, not a
  telemetry-only primitive. Primitive-only speed fails closed.
- All generic/codegen/runtime edits pass Lock 14: grammar facts are generated
  metadata, no generic crate branches on JSON/CSS/Sheets/BBNF-self names, and
  no new directive or BIR variant is added.
- JSON guard evidence satisfies §2.2: either all guard floors hold in a
  refreshed JSON run, or the wave proves it did not touch JSON-producing paths
  and `skinny/RESULTS.md` remained unchanged for JSON rows.

Revert protocol: revert intervention, parse-that/SIMD, codegen/runtime,
bench/report/gate/RESULTS edits as one slice on target miss, oracle coupling,
strict equality failure, guard regression, checkasm failure, stale run id, or
Lock 14 leak; preserve W1 baseline evidence; save
`/tmp/skv12-waveW2-rejected.patch`.

### 2.6 Conditional W3 Gate: `G-W3-CONDITIONAL-JSON-COMPANION`

Entry: W1 and W2 have admitted, W1 admitted and W2 recorded a measured reject,
or the generated non-JSON baseline priority is recorded as measured `BLOCKED`.
For behavior dispatch, CHALLENGE must also name fresh material evidence beyond
REDRESS 114-119: new hot-leaf evidence, a source delta materially different
from W3-W7, scalar/oracle proof, same-host microbench, independent Track 2,
strict same-run sonic-rs direct floor, and same-wave gate consumption. Without
that evidence, W3 is a routed block with no source/RESULTS movement.

Residual row floors:

| Row | Track 1 | Track 2 | sonic direct | floor |
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

Exit gate:

- Behavior form: exactly one residual direct row is selected and named; Track 1
  and Track 2 both meet or exceed that row's floor; strict output equality
  passes; Track 2 is independent; sonic-rs direct is same-run strict evidence;
  the selected source delta is not a replay of numeric slot, container-tail,
  bounded string span, escaped segment, output digest host-sink, W3 union, or
  W0-clamped docs-only admission; and the full guard floors in §2.2 hold.
- Routed-block form: W3 records why no current candidate passes the material
  reopen burden, moves no source or RESULTS row, preserves guard state, and
  records REDRESS evidence for the routed block.

Revert protocol: revert the JSON direct source/generated/bench/report/gate
slice on row-floor miss, Track 2 miss, strict comparator miss, REDRESS route
reopen, guard regression, stale run id, or oracle coupling; save
`/tmp/skv12-waveW3-rejected.patch`.

### 2.7 Close Gate: `G-W4-CLOSE`

Exit gate succeeds only in one of three forms:

- Admit form: W1 admitted one generated non-JSON baseline, W2 admitted one
  measured intervention on that same row at
  `ceil(baseline_mbps * 1.01)` or higher, guard floors hold, and all
  close documents agree.
- Reject form: W1 admitted one generated non-JSON baseline, W2 recorded a
  measured reject on that same row, W3 is adjudicated or routed, guard floors
  are preserved or any demotion is explicitly measured in REDRESS, and all close
  documents agree.
- Block form: W1 records a measured generated-baseline
  `BLOCKED` verdict with executable preflight evidence, failed gate output,
  no JSON row movement, guard floors preserved, and a REDRESS route explaining
  why the accepted owner surface cannot create the baseline.

Close fails on a prose-only Lock 14 claim, stale witness module, hand-only
non-JSON parser, producer-only telemetry, stale run id, oracle coupling,
generic JSON policy leak, parse-only SOTA claim, W3 substrate reopen, or any
unconsumed result field.

## §3 — Falsifiability binding

Concrete SK-V12 thresholds:

- Non-JSON baseline: selected generated Track 1 >= 1 Mbps and independent
  Track 2/oracle >= 1 Mbps, same row, same output plane, strict equality PASS.
- Non-JSON intervention: selected Track 1 >=
  `ceil(baseline_mbps * 1.01)`, with the same independent
  Track 2/oracle/equality proof still present.
- SIMD/ASM micro-proof: candidate primitive throughput >=
  `ceil(scalar_reference_mbps * 1.01)` on the named same-host representative
  slice, plus strict parity/checkasm and same-wave consumer.
- Direct guard floors: `citm_catalog` 18191/17431,
  `apache_builds` 11028/9996, `marine_ik` 8759/9248,
  `unicode_basic` 2253/2182.
- Typed guard floors: `twitter` 17385/15593, `citm_catalog` 29928/17321,
  `apache_builds` 8308/6754, `github_events` 11633/12029,
  `update_center` 11613/10150, `mesh` 9214/7739,
  `marine_ik` 11552/9894.
- Conditional JSON direct floors are the 13-row table in §2.6. Both Track 1 and
  Track 2 must meet the selected row floor.

Fail-closed rejection rules:

- Unmeasurable gate: missing baseline Mbps, missing selected row id, missing
  output plane, missing strict equality result, or missing independent
  Track 2/oracle source returns REVISE before redress.
- Stale-run gate: new or moved rows must not reuse inherited `SK-V9-open`
  telemetry as behavior evidence; mixed run ids across Track 1/Track 2/oracle,
  missing host/build/sample fields, or non-native build flags fail closed.
- Oracle-coupling gate: Track 2/oracle cannot call generated Track 1,
  generated direct/typed helpers, hidden shared parser code, or direct digest
  as typed proof.
- Producer-only gate: every emitted field, non-JSON report, companion table,
  cost fact, or telemetry cell must be consumed by `gate-json` or the same-wave
  non-JSON gate. Emit-now-consume-later fails.
- Full-table gate: guard rows cannot silently demote; parse-only cannot close
  SK-V12; W0-clamped residuals cannot admit by accounting.

## §4 — Pre-blocked routes

The gates above reject these route families before behavior redress unless the
SPEC records a material differential and CHALLENGE accepts it:

- REDRESS 111-113: non-JSON report lane is not a generated baseline; CSS L4
  baseline/intervention remain blocked until W1 creates a generated runtime
  row.
- REDRESS 114-119: JSON direct residual numeric, container-tail, bounded
  string, escaped segment, output digest, and fixpoint routes are pre-blocked.
- REDRESS 96-98, 102: W3 union/event/class-column/streaming-cursor/class-lane,
  retained structural vectors, sidecars, and `UnionTape` remain closed.
- REDRESS 50, 51, 53, 60-72, 82-84: parser-owned sidecars, retained metadata,
  decoded-byte/string materialization shortcuts, StringBlock16 retreads, and
  object-pair/control compaction remain blocked.
- REDRESS 80, 88-90: numeric fallback/mantissa routes, PMULL default
  prefix-XOR, and CTZ/bulk consumer rewires remain blocked unless narrowed to a
  new scalar-equivalent same-wave consumer.
- REDRESS 118: output digest/host-sink work is an oracle/report surface, not a
  parser primitive and not typed proof.
- Global: parse-only SOTA claims, PMU/cycles/structural-scan/masking as
  behavior producers, new directives, new BIR variants, public substrate APIs,
  second retained substrates, generic-crate JSON policy, and x86 targets are
  rejected.

## §5 — Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
