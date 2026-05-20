# SK-V11 SPEC

Pass: S-P3 Synthesis-Plan.
Cycle: V3 draft.
Date: 2026-05-20.
Status: DRAFT. S-P3 CHALLENGE must converge before this file becomes wave
dispatch authority.

This SPEC is the wave-sequenced implementation contract for SK-V11. It consumes
the SK-V11 Pass Alpha goalset, the SK-V11-open profile, and the S-P2 V3
accepted candidate pool. It authorizes no behavior source change until S-P3
converges and a specific wave's entry gate passes.

## 0. Close Condition And Goalset

SK-V11 closes only when all of these are true:

1. S-P3 converges under `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` and
   `ORCHESTRATOR.md` §3Z.
2. W0 remains a coherent SK-V11-open baseline:
   `/tmp/skv11-open-criterion-3ce75df`, run id
   `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, host
   `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`, with
   `RUSTFLAGS="-C target-cpu=native"` and `gate-json --with-cost-facts
   --check-results` green.
3. Direct plane closure is the JSON target. Each SK-V11-open residual
   `direct_to_struct N-direct / NO-GO` row either becomes strict same-run
   `A / GO` on generated Track 1 and independent Track 2/oracle, or receives a
   per-row uncloseable REDRESS proof with measurement.
4. `instruments`, `numbers`, and `unicode_mixed` remain W0-clamped
   non-admissions until a behavior or gate wave records measured provenance.
   Their positive W0 deltas are planning evidence only.
5. Existing admitted rows are guarded: 7 `real_typed_struct A / GO` rows and
   4 `direct_to_struct A / GO` rows cannot silently demote or regress outside
   their maintain gates.
6. Parse-only is not a SK-V11 SOTA target. `parse_only` rows may serve as
   guard, compatibility, or diagnostic evidence only.
7. REDRESS 96/97/98 and REDRESS 102 remain binding: no W3 union/event
   substrate, class column, structural-position vector, streaming cursor,
   class lane, sidecar producer, parse-plane substrate repair, or cascade-lock
   through W3 may dispatch.
8. At least one non-JSON grammar carries an admitted, benchmarked SK-V11
   intervention through a generated direct or typed parser. Preferred order:
   CSS L4 declaration values, then Sheets formulas, then BBNF-self.
9. AArch64 Apple Silicon is the only SK-V11 SIMD/ASM implementation target.
   x86 is comparator context only.
10. Micro-prove-first holds for every kernel, substrate-adjacent, SIMD/ASM, or
    generic parser intervention: scalar reference, differential/checkasm or
    product parity, same-host microbench, feature/fallback plan, same-wave
    consumer, and row gate.
11. Strict-vs-strict comparator discipline holds. Direct admission uses
    same-run sonic-rs strict direct comparator on the digest plane; typed
    admission uses same-run sonic-rs strict typed comparator on the typed plane.
12. No new directive, BIR variant, public substrate API, parser-owned sidecar,
    generic JSON policy, or second retained substrate is allowed.
13. Telemetry is consumed by the relevant gate in the same wave. No producer-only
    field, report, or proof artifact can close a wave.
14. Close docs agree: `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SYNTHESIS.md`,
    `HANDOFF.md`, this `SPEC.md`, and `DISPATCH-PROMPT.md`.

### 0.1 Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict direct, sonic-rs strict typed, serde_json only where the output plane matches | May support strict admission when row output plane, strictness, validation path, run id, and comparator freshness match. |
| Same-run flaw probe | sonic-rs lossy/permissive, view-boundary validation, absent sidecars | Planning only. Never strict admission. |
| Historical/sidecar planning signal | simdjson, yyjson, RapidJSON, asmjson, older sidecars | Planning only unless refreshed into a structured same-run strict manifest and consumed by the gate. |
| Non-JSON oracle | CSS/Sheets/BBNF-self independent oracle or Track 2 on the same generated output plane | May close the grammar-generalization axis only when the same wave benchmarks generated Track 1 and consumes the oracle proof. |

### 0.2 Outcome Enum

The inherited 10-identifier outcome enum is binding:

```text
A C G I J K L M N-direct S
```

No SK-V11 wave may add an outcome variant. `S`, `L`, and `N-direct` are
non-admission outcomes. `S` is diagnostic/substrate-guard, not a SOTA win.

### 0.3 Required Telemetry

SK-V11 inherits the schema-v3 required identifier set from P3-D:

```text
row_id grammar_id domain corpus workload outcome_id verdict strictness
parse_utf8 escape_complete flaw_probe output_plane track1_mbps track2_mbps
comparator_id comparator_plane comparator_strictness comparator_freshness
sidecar_freshness comparator_value_mbps comparator_source_artifact
measured_validation_path profile_artifact sample_cost sample_count build_flags
host_triple feature_mask costfacts_rule_id costfacts_chosen_shape
costfacts_rejected_alternative_ids redress_entry wave_id run_id
sk_v9_open_delta substrate_surface structural_projection_status
substrate_cardinality same_wave_consumer_class track2_independence_status
diagnostic_nonproducer_status
```

The gate may render fewer physical table columns than identifiers only when the
folded cell is reconstructable and validator-consumed. Comparator value/source
and UTF-8/escape/flaw-probe validation are evidence identifiers, not optional
display text.

`gate-json` must fail closed on missing required fields, duplicate or unknown
row ids, unsupported outcomes, non-uniform or invalid run ids, stale strict
anchors, strict plane mismatch, deferred validation admission, wrong strict
comparator, parse-only SOTA claims, direct digest as typed proof, Track 2
coupling, W3 reopen claims, and producer-only telemetry.

Non-JSON rows may enter `skinny/RESULTS.md` only if the same wave updates every
consumer. Otherwise the wave must write a companion gate-consumed report under
`restart/skinny/tranches/sk-v11/research/wave{W}/` that carries the same
semantic fields: grammar, workload, generated Track 1, independent oracle,
strict output equality, run id, host, flags, sample count, primitive self-time
when applicable, and no-sidecar proof.

### 0.4 Direct Residual Goalset

Direct floor = `ceil(sonic-rs strict direct Mbps / 1.10)`.

| Row | Current outcome | Track 1 | Track 2 | sonic direct | Floor |
|---|---|---:|---:|---:|---:|
| `twitter/direct_to_struct` | `N-direct / NO-GO` | 11613 | 10816 | 15113 | 13740 |
| `canada/direct_to_struct` | `N-direct / NO-GO` | 10316 | 9819 | 11700 | 10637 |
| `github_events/direct_to_struct` | `N-direct / NO-GO` | 11918 | 10596 | 14743 | 13403 |
| `update_center/direct_to_struct` | `N-direct / NO-GO` | 8187 | 7474 | 11064 | 10059 |
| `mesh/direct_to_struct` | `N-direct / NO-GO` | 8561 | 8652 | 9542 | 8675 |
| `random/direct_to_struct` | `N-direct / NO-GO` | 7693 | 6949 | 8665 | 7878 |
| `gsoc-2018/direct_to_struct` | `N-direct / NO-GO` | 2665 | 2578 | 4110 | 3737 |
| `instruments/direct_to_struct` | `N-direct / NO-GO` | 11569 | 10736 | 9865 | 8969 |
| `numbers/direct_to_struct` | `N-direct / NO-GO` | 4479 | 2366 | 2667 | 2425 |
| `unicode_mixed/direct_to_struct` | `N-direct / NO-GO` | 3753 | 2427 | 2846 | 2588 |
| `unicode_escapes/direct_to_struct` | `N-direct / NO-GO` | 1345 | 1341 | 3785 | 3441 |
| `distinct_values/direct_to_struct` | `N-direct / NO-GO` | 1750 | 1625 | 2923 | 2658 |
| `y_string_unicode/direct_to_struct` | `N-direct / NO-GO` | 1983 | 1029 | 4344 | 3950 |

### 0.5 Guard Rows

Direct guard floors are `max(ceil(sonic direct / 1.10), floor(SK-V11-open track
Mbps * 0.98))` per track.

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard Track 1 floors are
`max(ceil(sonic typed / 1.10), floor(SK-V11-open Track 1 * 0.98))`; Track 2 is
an independent oracle guard at `floor(SK-V11-open Track 2 * 0.98)` when the
row is measured.

| Row | Track 1 maintain | Track 2 oracle guard |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

## 1. Non-Negotiables

- No parse-only SOTA target or parse-only row admission.
- No W3 union/event/class-column/streaming-cursor/class-lane/substrate repair.
- No new BBNF directive, BIR variant, `BackendShape`, public substrate API, or
  parser-owned sidecar/fact slot.
- No JSON policy in generic crates or runtime outside generated per-grammar
  modules.
- No x86 implementation work in SK-V11.
- No primitive, SIMD kernel, generated path, codegen shape, or host sink ships
  without scalar reference or oracle, parity/checkasm where applicable,
  same-host microbench when required, same-wave consumer, and measured gate.
- No orphan kernel and no producer-only telemetry.
- Every generic/codegen/runtime-outside-JSON edit needs a same-wave CSS L4,
  Sheets, or BBNF-self proof.
- Generated output may be committed only as regenerated output from named
  generator/schema input.
- Track 2/oracle may not call generated Track 1, generated SinkOnly helpers,
  generated typed helpers, or hidden benchmark-private parser code.
- Every wave has a revert protocol. A miss becomes REDRESS evidence, not a
  silent scope cut.

## 2. Wave Manifest, Caps, And Reruns

| Wave | Section | Title | Candidate surface | Dispatch status | LOC budget | Redress cap |
|---|---|---|---|---|---:|---:|
| W0 | 3 | SK-V11-open Telemetry Lock | W0 profile authority | Closed by S-P1/W0 | 0 behavior LOC | n/a |
| W1a | 4 | Non-JSON Gate/Report Schema Lane | C9 accounting + Lock 14 gate | Conditional on S-P3 convergence + CHALLENGE | <=260 handwritten source/test/gate LOC; 0 generated LOC unless fixtures are named | <=90 min |
| W1b | 5 | Generated Non-JSON Baseline And Oracle Lane | C9 accounting + generated baseline harness | Conditional on W1a close + CHALLENGE | <=360 handwritten source/test/gate LOC; regenerated output capped to selected generated parser inputs | <=90 min |
| W2 | 6 | CSS L4 Generated Direct/Typed Intervention Proof | C1/C2/C4/C5/C6 with C7 support | Conditional on W1b close + CHALLENGE | <=430 handwritten source/test/gate LOC; regenerated output capped to named inputs | <=90 min |
| W3 | 7 | Numeric Direct Closure Slice | C4 + D4, optional UDOT | Conditional on W2 disposition + CHALLENGE | <=360 handwritten source/test/gate LOC; regenerated output capped to named JSON callers | <=90 min |
| W4 | 8 | Generated Dispatch And Byte-Set Control Slice | C1/C5/C6 with C7 support + D1/D2 | Conditional on W3 disposition + CHALLENGE | <=430 handwritten source/test/gate LOC; regenerated output capped to named inputs | <=90 min |
| W5 | 9 | Bounded String Span And Special-Byte Scan | C2 + D3 | Conditional on W4 disposition + CHALLENGE | <=360 handwritten source/test/gate LOC; regenerated output capped to named string/key callers | <=90 min |
| W6 | 10 | Escaped Segment And Hex Decode Slice | C3, x4 proof only unless new source delta | Conditional on W5 disposition + CHALLENGE | <=360 handwritten source/test/gate LOC; regenerated output capped to named escaped-string callers | <=90 min |
| W7 | 11 | Output Digest/Hash Host Sink | C8 only | Conditional on W3-W6 dispositions + CHALLENGE | <=350 handwritten source/test/gate LOC | <=90 min |
| W8 | 12 | Direct Residual Fixpoint And Row Reclamation | remaining C1-C8 measured routes | Conditional on W3-W7 dispositions | <=250 docs/gate/result LOC; source work requires W8a split and remaining bracket slot | <=90 min |
| W9 | 13 | Close And Alpha Feedback | docs/gate | Conditional on W8 close | 80-180 docs/gate LOC | <=90 min |

Phase caps follow `SKINNY-TRIUMVIRATE.md`: research 30 min per agent, plan 30
min, CHALLENGE 60-90 min when required, redress 75 min target and 90 min hard
cap unless CHALLENGE grants a SPEC-recorded split or extension. If a plan
cannot fit the budget, it returns REVISE before source work.

The bracket is W0, W1a, W1b, W2-W9: 11 waves, leaving one spare split before
the skinny `> 12` escalation rule. W1a-W7 are first-of-class or
generic/source-touching and therefore require CHALLENGE before redress. W8
requires CHALLENGE only if it adds a final source route; that source route
becomes W8a and consumes the only spare split. Gate/report-only fixpoint
accounting may skip W8 CHALLENGE.

### 2.1 Micro-Prove-First Gate

No W2/W3/W4/W5/W6/W7 plan reaches redress until it records:

- scalar reference or exact product oracle;
- strict differential/checkasm command when any SIMD/ASM body is used;
- same-host microbench on representative target slices;
- observed value, threshold, run id, host triple, build flags, sample count,
  and feature gate;
- same-wave consumer path and row gate;
- scalar/no-op fallback;
- reject boundary tied to REDRESS.

Primitive-only speed is not production evidence. The caller and row gate are
binding.

### 2.2 Generality And Lock 14 Gate

Every wave has this exit gate; generic/codegen/runtime-outside-JSON edits add
the stricter checks:

- no generic branch selects JSON, corpus, object/array role, field name, or
  JSON layout role;
- byte-set, delimiter, terminator, escape, numeric, and dispatch facts are
  grammar-generated metadata, not generic constants;
- CSS L4 / Sheets / BBNF-self generated parser proof is run and consumed in
  the same wave when generic behavior changes;
- the live `json_provider` path is replaced, bypassed with a grammar-neutral
  template proof, or explicitly left untouched before a non-JSON generality
  claim can pass;
- generated output is regenerated from named inputs, never hand patched;
- on proof failure, revert generic/codegen/runtime edits as one slice.

### 2.3 Track 2 And Oracle Independence Gate

Direct/typed/non-JSON row movement requires an independent Track 2 or oracle on
the same output plane. The gate must name the Track 2/oracle source path and
forbid calls into generated Track 1, generated SinkOnly helpers, generated
typed helpers, or hidden shared parser code.

## 3. W0 - SK-V11-open Telemetry Lock

Disposition: closed by S-P1/W0 authority before this SPEC draft.

Owner paths if rerun is forced:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: S-P1 has converged and `HARDENING-S-P1-CONVERGED.md` names the
SK-V11-open run id.

Exit gate `G-W0-SK-V11-OPEN-LOCK`:

- `skinny/RESULTS.md` and `gate-json` agree on the schema-v3 required identifier set and
  10-outcome enum.
- Overall surface remains 16 `parse_only S / NO-GO`, one `parse_only L /
  NO-GO`, 4 direct `A / GO`, 13 direct `N-direct / NO-GO`, and 7 typed `A /
  GO`.
- No row moves and no behavior source changes.

Revert protocol: revert report/gate/result changes and record REDRESS naming
the missing telemetry consumer.

Downstream effect: behavior waves dispatch only against this baseline.

## 4. W1a - Non-JSON Gate/Report Schema Lane

Candidate surface: C9 accounting and Lock 14 gate/report infrastructure. No
parser row moves in this wave.

Owner paths:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/benches/`
- `restart/skinny/tranches/sk-v11/research/w1a/`

Entry gate: W0 closed; CHALLENGE accepts the non-JSON identifier/gate
extension without behavior row movement.

Tasks:

- Teach the gate/report path to consume non-JSON benchmark evidence without
  weakening JSON `gate-json`.
- Add failing and passing gate fixtures for grammar id, domain, output plane,
  comparator/oracle, Track 2/oracle independence, run id, host, feature mask,
  same-wave consumer class, and producer-only telemetry rejection.
- Confirm JSON rows still pass with the schema-v3 required identifier set.

Exit gate `G-W1a-NONJSON-GATE`:

- Missing required non-JSON fields are rejected by the gate.
- Producer-only non-JSON telemetry is rejected.
- JSON `gate-json --with-cost-facts --check-results` remains green.
- No JSON `RESULTS.md` row moves.
- No generated non-JSON baseline authority is claimed.

Revert protocol: revert gate/report/fixture changes as one slice and record
REDRESS if the gate cannot consume the schema without weakening JSON.

Downstream effect: W1b may create the first generated non-JSON baseline row
against this gate/report lane.

Pre-blocked routes: JSON-provider emission as a generic proof, documentation-
only Lock 14 claims, behavior row movement, hidden directives/BIR variants, and
producer-only telemetry.

## 5. W1b - Generated Non-JSON Baseline And Oracle Lane

Candidate surface: C9 accounting, generated non-JSON baseline harness, and an
independent Track 2/oracle. No intervention admits in this wave.

Owner paths:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/runtime/src/grammars/`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/benches/`
- `grammar/css/l4/`
- `grammar/google-sheets/google-sheets.bbnf`
- `grammar/bbnf/`
- `restart/skinny/tranches/sk-v11/research/w1b/`

Entry gate: W1a closed; CHALLENGE selects exactly one non-JSON target,
preferring CSS L4 declaration values, then Sheets, then BBNF-self, and names
the independent oracle/Track 2 path.

Tasks:

- Stand up exactly one generated non-JSON direct or typed parser baseline row.
- Add or name an independent Track 2/oracle for the selected output plane.
- Prove strict output equality and gate consumption for the baseline.
- Prove the live `json_provider` path does not leak JSON policy into the
  selected generated parser.

Exit gate `G-W1b-NONJSON-BASELINE`:

- Generated Track 1 non-JSON parser baseline exists for the selected
  grammar/workload.
- Independent oracle or Track 2 exists and does not call generated Track 1.
- Strict output equality passes on the selected corpus.
- Baseline throughput is rendered with run id, host, flags, sample count,
  output plane, and oracle status.
- No JSON policy appears in generic crates or runtime outside generated
  per-grammar modules.
- No behavior row admits and no JSON `RESULTS.md` row moves.

Revert protocol: revert codegen/bench/gate/report generated baseline changes
as one slice; preserve failed proof in REDRESS.

Downstream effect: W2 and any generic-codegen wave may claim Lock 14 only by
using or extending this generated baseline/oracle lane.

Pre-blocked routes: old non-JSON struct-direct modules that sever the tape
substrate; JSON-provider emission as a generic proof; documentation-only Lock
14 claims; W2-style intervention in the baseline wave.

## 6. W2 - CSS L4 Generated Direct/Typed Intervention Proof

Candidates: C1-C6 as selected by plan; C7 support only; C8 oracle only; C9
accounting.

Owner paths:

- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/grammars/`
- `skinny/crates/parse-that-regex/src/`
- `skinny/crates/bbnf-simd/src/` only if the selected intervention uses SIMD
- `skinny/crates/bbnf-simd/tests/`
- `skinny/crates/bbnf-bench/src/`
- `skinny/crates/bbnf-bench/benches/`
- `grammar/css/l4/`
- `skinny/RESULTS.md` only if the wave extends it with gate consumption
- `skinny/REDRESS.md`

Entry gate: W1b closed; CHALLENGE selects exactly one generated non-JSON direct
or typed intervention, preferring CSS L4 declaration values, and names the
scalar oracle, independent Track 2/oracle, baseline Mbps, target threshold, and
generic-codegen Lock 14 proof.

Tasks:

- Generate and benchmark the selected CSS L4 direct/typed parser intervention.
- Wire exactly one SK-V11 primitive family into the generated non-JSON
  consumer.
- Keep JSON policy out of generic crates and generated runtime templates.
- Consume the W1b baseline; W2 may not create the first measurable non-JSON
  row.

Exit gate `G-W2-CSS-GENERATED-INTERVENTION`:

- Generated non-JSON Track 1 and independent Track 2/oracle exist for the
  selected workload.
- Strict output equality passes.
- Track 1 is at least `ceil(W1b_css_baseline_mbps * 1.01)` on the selected
  non-JSON row.
- Any SIMD body passes strict scalar differential/checkasm.
- JSON direct and typed guard floors in §0.5 hold if JSON reports are refreshed.
- No JSON policy appears in generic crates or runtime outside generated
  per-grammar code.

Revert protocol: revert codegen/runtime/parse-that/simd/bench/report changes
as one slice on non-JSON miss, oracle coupling, guard regression, parity
failure, or Lock 14 leak.

Downstream effect: satisfies the SK-V11 non-JSON admitted-intervention axis
when admitted and unlocks later generic primitive waves.

Pre-blocked routes: prose-only Lock 14 proof, JSON-provider emission as a
generic proof, old hand non-JSON runtimes, generic JSON policy, source sidecars.

## 7. W3 - Numeric Direct Closure Slice

Candidates: C4 digit span/accumulation, P2-D D4 `number_span_emit_slot`, P2-E
`pt_digit_run_span_accumulate`, optional P2-C UDOT support.

Owner paths:

- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`
- `skinny/crates/bbnf-simd/tests/`
- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W2 admits or records a measured non-JSON route; CHALLENGE accepts
scalar `DigitRun` semantics, optional DotProd feature gate, and one or two
numeric target rows unless existing same-host microbench data justifies all
four.

Tasks:

- Add scalar digit-run/span/accumulation oracle without changing number grammar
  policy.
- Add strict AArch64 parity if DotProd/UDOT is used.
- Wire the helper into generated direct or typed numeric consumers.
- Preserve CSS/Sheets numeric compatibility if generic parse-that/codegen
  changes are made.

Exit gate `G-W3-NUMERIC-SEQUENCE-DIRECT`:

- Selected direct rows from `canada`, `mesh`, `numbers`, and `instruments`
  meet their §0.4 floors where applicable.
- The redress plan selects one or two rows by default; selecting three or four
  rows requires pre-redress microbench evidence and CHALLENGE acceptance.
- `marine_ik/direct_to_struct` remains admitted and satisfies §0.5 if measured.
- Typed numeric guard rows named by plan meet §0.5 floors if measured.
- No f64 fallback, mantissa table, leading-zero/sign/exponent, suffix, or
  conversion policy changes move into the primitive.
- Guard floors hold.

Revert protocol: revert parse-that/simd/generated/bench/gate/RESULTS changes
as one slice on parity failure, row-floor miss, non-JSON proof miss, or numeric
semantic drift.

Downstream effect: closes or rejects number-heavy and W0-clamped direct
residuals with measured provenance.

Pre-blocked routes: REDRESS 80 numeric fallback/mantissa widening; generic
number policy; parse-only numeric evidence; direct admission from W0 clamp
without measured wave provenance.

## 8. W4 - Generated Dispatch And Byte-Set Control Slice

Candidates: C6 generated FIRST/prefix/lookahead dispatch; P2-D D1
`container_tail_next`; P2-D D2 `direct_slot_dispatch`.

Owner paths:

- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W3 admits or rejects with REDRESS; W2's non-JSON proof remains
valid for generic edits; CHALLENGE selects exactly one scalar generated
dispatch shape, at most three target rows, and proves no directive/BIR/
substrate change.

Tasks:

- Factor one scalar generated direct dispatch or container-tail helper.
- Differential generated Track 1 against current output, independent Track 2,
  and serde/sonic digest oracles.
- Measure selected direct rows and direct/typed guards.

Exit gate `G-W4-DISPATCH-BYTESET-DIRECT`:

- Every selected direct row meets its §0.4 floor on Track 1 and Track 2.
- Candidate target set is selected from `canada`, `mesh`, `random`,
  `update_center`, `github_events`, and `twitter`; maximum three rows per
  redress plan unless CHALLENGE splits the wave.
- Track 2 independence and same-output proof pass.
- No object/key/value-byte carry, retained cursor, class lane, sidecar, or
  JSON policy in generic crates.
- Guard floors in §0.5 hold.

Revert protocol: revert source/generated/gate/report/RESULTS as one slice on
row-floor miss, output mismatch, guard regression, or Lock 1/14 violation.

Downstream effect: closes or measures the container/dispatch subset of direct
residuals.

Pre-blocked routes: REDRESS 63/65/84 object carry; W3 substrate; sidecar
dispatch facts; Track 1/Track 2 coupling.

## 9. W5 - Bounded String Span And Special-Byte Scan

Candidates: C2 bounded special-byte scan, P2-D D3 `borrowed_string_span`, P2-E
`pt_bounded_plain_string_end`, optional string-block support only after
micro-proof.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/tests/`
- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W4 admits or rejects; CHALLENGE selects scalar span shape, one
string/key caller, cap, and at most two target rows; any SIMD body has strict
parity plan and REDRESS 106 material differential.

Tasks:

- Add scalar span oracle returning offsets and decode-needed status.
- Wire one generated direct/typed string/key consumer.
- Add strict parity and caller microbench if a block SIMD body is used.
- Monitor Unicode residual rows when the target is plain-string; they remain
  floor-bearing residuals for W6/W8 unless selected in W5.

Exit gate `G-W5-STRING-SPAN-DIRECT`:

- Selected direct rows from `twitter`, `github_events`, `update_center`,
  `random`, `distinct_values`, `gsoc-2018`, and `y_string_unicode` meet their
  §0.4 floors.
- The redress plan selects one string/key consumer and at most two target rows.
- Unicode residual rows `unicode_escapes`, `unicode_mixed`, and
  `y_string_unicode` are not admitted guards. If not selected, they remain
  residuals for W6/W8 and may not regress outside the full guard block.
- No decoded scratch, retained string side table, `StringBlock16` retained
  wrapper, primitive-parity-only production, or 64-byte retained scan ships.
- Non-JSON string/literal proof passes when generic code changes.
- Guard floors hold.

Revert protocol: revert parse-that/simd/generated/bench/gate/RESULTS as one
slice on parity failure, row-floor miss, Unicode guard regression, or REDRESS
106 replay.

Downstream effect: W6 may consume W5 span APIs only if W5 admits or CHALLENGE
accepts a compatible rejected-but-reusable scalar proof with no behavior source.

Pre-blocked routes: REDRESS 28/33, 60-62, 72, 83, 106; parser-owned decoded
scratch; retained semantic string facts.

## 10. W6 - Escaped Segment And Hex Decode Slice

Candidates: C3 escape segment/hex decode, P2-E `pt_escaped_string_segments`,
P2-B `HEX_QUARTET_X4_PROOF` support only unless a new source delta exists.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/bbnf-simd/tests/`
- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W5 has admitted or CHALLENGE accepts an independent segment plan;
the plan names a new source delta beyond the already-consuming
`unescape_string` path.

Tasks:

- Add scalar escaped-segment visitor or hex-run oracle.
- Add x4 scalar oracle and strict checkasm if x4 is routed.
- Wire a new direct/typed/non-JSON escaped-segment consumer.
- Keep JSON surrogate policy in generated JSON caller; CSS variable-width
  escapes and BBNF literal policy stay per-grammar.

Exit gate `G-W6-ESCAPE-SEGMENT-DIRECT`:

- Selected direct rows from `unicode_escapes`, `unicode_mixed`, and
  `y_string_unicode` meet their §0.4 floors.
- If no JSON direct row can close, a non-JSON escaped-string or hex-color
  consumer may admit the wave only if W2 has not already satisfied the
  non-JSON close axis and all JSON rows record honest measurements.
- x4 proof cannot admit production without new source delta and same-wave
  consumer.
- No decoded scratch, output hash side channel, or JSON surrogate policy leaks
  into generic crates.
- Guard floors hold.

Revert protocol: revert parse-that/simd/generated/bench/gate/RESULTS as one
slice on parity failure, row-floor miss, missing new source delta, or policy
leak.

Downstream effect: closes or records uncloseable proof for Unicode direct
residuals.

Pre-blocked routes: REDRESS 64, 66-69, 82, 83, 107, 108; reuse of existing
`unescape_string` as same-wave production.

## 11. W7 - Output Digest/Hash Host Sink

Candidate: C8 output digest/hash oracle or per-product host sink only.

Owner paths:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- selected non-JSON oracle/report files if W1b uses digest output
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W3-W6 have dispositions; CHALLENGE accepts that output digest/hash
is an observed limiting hot leaf for a bounded selected product-row subset and
names exact scalar fold/mix source, output plane, and independent oracle.

Tasks:

- Refactor or specialize only the output digest/host sink, not parser
  semantics.
- Prove bit-exact digest equivalence after raw and decoded segment boundaries.
- Measure selected direct rows or non-JSON host-sink row.

Exit gate `G-W7-DIGEST-SINK`:

- Selected direct rows whose fresh post-W6 profile still names
  `output_digest_hash` as limiting meet §0.4 floors on Track 1 and
  Track 2/oracle, or the selected non-JSON host sink improves >= 1.0% with
  strict output equality.
- No digest/hash state enters generic parser crates as parser semantics.
- No cache hint or prefetch-only change admits the wave without measured row
  movement and no-regression.
- Guard floors hold.

Revert protocol: revert host-sink/report/gate/RESULTS changes as one slice on
digest mismatch, row-floor miss, missing hot-leaf proof, or generic policy
leak.

Downstream effect: final output-sink chance before W8 direct fixpoint.

Pre-blocked routes: output digest as parser primitive, hidden semantic string
facts, PRFM/STNP/cache hints as standalone row movers.

## 12. W8 - Direct Residual Fixpoint And Row Reclamation

Candidate surface: no new primitive by default; consumes any remaining
measured C1-C8 residual route accepted by CHALLENGE. Source work is not part of
W8 unless split as W8a and the bracket has its one spare slot available.

Owner paths:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- exact remaining source owner paths only if CHALLENGE accepts one final narrow
  candidate
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W3-W7 have admitted or rejected with measurement; W2 admitted the
non-JSON axis or recorded a BLOCKED route; every remaining direct residual row
has a named candidate or a candidate-exhaustion proof plan.

Tasks:

- Re-evaluate every remaining direct residual under the strict direct contract.
- Admit only rows that meet §0.4 on both generated Track 1 and independent
  Track 2/oracle.
- For rows that miss, record a REDRESS uncloseable proof naming attempted
  candidate, measured tracks, comparator, floor, and guard status.
- If source work is required, route to W8a first with exactly one candidate and
  one row subset; W8 then remains docs/gate/result accounting.

Exit gate `G-W8-DIRECT-FIXPOINT`:

- All §0.4 rows are `A / GO` or have a measured REDRESS proof.
- W0-clamped rows are admitted only with W3-W8 measured provenance.
- Existing direct and typed guards satisfy §0.5.
- No new source route is used unless CHALLENGE accepted it before redress.

Revert protocol: revert any W8 source/gate/report/RESULTS changes as one slice
on row-floor miss, guard regression, or unsupported row movement; preserve
per-row measurements in REDRESS.

Downstream effect: enables W9 close or escalates BLOCKED for unresolved direct
or non-JSON gates.

Pre-blocked routes: paper fixpoint, W0 clamp bypass, stale SK-V10 row
admission, future-phase promises, direct digest as typed proof.

## 13. W9 - Close And Alpha Feedback

Owner paths:

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/research/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W8 has closed or escalated and every W1a-W8 wave has admitted,
proof-closed, or rejected with measurement.

Exit gate `G-W9-CLOSE-SK-V11`:

- Every §0.4 residual direct row is `A / GO` or has a REDRESS uncloseable proof
  naming attempted intervention, Track 1, Track 2/oracle, comparator, floor,
  guard result, and routed remainder.
- At least one non-JSON generated direct/typed parser intervention is admitted
  and benchmarked, unless Close escalates `BLOCKED` for grammar-generalization
  fixpoint.
- Existing direct and typed guard rows satisfy §0.5.
- Parse-only remains diagnostic and W3 remains closed.
- All close documents agree.

Revert protocol: close docs revert until contradictions are resolved; accepted
source/measurement commits are not reverted by close accounting.

Downstream effect: triggers G-Alpha SK-V11 -> SK-V12 and Pass Alpha feedback.

## 13. Pre-Blocked Route Ledger

Hard pre-blocks:

1. W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
   repair, including `UnionTape`, structural-position vectors, parser-owned
   projections, and W4 cascade-lock through W3.
2. Parse-only SOTA movement or parse-only row admission.
3. Sidecar or parallel substrate producer: aux columns, whitespace cursor,
   structural cursor, event cursor, retained position vector, or hidden bitmap
   table.
4. Direct digest evidence as typed proof, or typed admission from direct row
   measurements.
5. JSON policy in generic crates or runtime outside generated per-grammar
   modules.
6. x86 implementation work.
7. PMULL prefix-XOR and CSSC CTZ/bulk emission as default production hot paths.
8. String materialization replays: retained wide scans, `StringBlock16`
   retained wrappers, decoded scratch/stats side channels, semantic string
   facts, x4 proof-to-production, and already-wired `unescape_string` reuse.
9. Numeric fallback/mantissa widening, f64 policy rewrites, or primitive-owned
   number grammar.
10. Object next-key carry, value-byte compaction, or direct receiver/scratch
    planes outside a same-loop generated product consumer.
11. PMU/cycles/structural-scan/masking/lazy materialization facts as behavior
    producers.
12. New directives, BIR variants, public substrate API, or hidden host schema
    facts.

Every REDRESS-adjacent wave plan must write a material-differential paragraph
before CHALLENGE. A narrower name is not a material differential.

## 14. G-Alpha And Dispatch Scope

G-Alpha SK-V10 -> SK-V11 is presented by the SK-V11 opening packet. This V3
SPEC is a draft until S-P3 converges. After convergence, dispatch scope is:

- W0 is closed unless the orchestrator explicitly invalidates the opening
  telemetry lock.
- W1a dispatches first, followed by W1b to make grammar generalization measurable.
- W2-W8 dispatch only when their SPEC entry gates pass.
- W9 Close dispatches after all behavior waves admit, reject, or proof-close with
  measurement.

No implementation agent may skip S-P3 convergence, bypass the wave entry gate,
or edit paths outside the selected wave's owner list.
