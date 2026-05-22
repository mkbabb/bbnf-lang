# SK-V13 SPEC - S-P3 Wave Plan V2 Draft

Date: 2026-05-21.

Status: user-pinned final implementation dispatch authority. S-P3 converged at
`G-S-P3-SPEC-DISPATCH-CONVERGED` in
`research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`; G-Omega closed by
explicit user sign-off at `2026-05-22T03:52:18Z`; Pass Omega CRUD completed in
`restart/audit/totality/astral/V1/CRUD-LOG.md`. This file folds the SK-V13
SYNTHESIS/HANDOFF, the 2026-05-21 full-SOTA user pin, converged S-P1/S-P2
evidence, P3-A through P3-E, and the S-P3 CHALLENGE accepted packet.

Authority:

- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/tranches/sk-v13/scoping/`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock status:

- `G-S-P3-SPEC-DISPATCH-CONVERGED`: PASS, with two accepted CHALLENGE cycles.
- `G-Omega`: CLOSED by user sign-off; totality V1.1 CRUD is complete.
- The pre-W0 block is lifted for SK-V13 Wave 0 and later waves, subject to the
  per-wave triumvirate contract.
- All waves below remain conditional. Each requires a wave plan with exact owner
  paths, thresholds, revert slice, same-wave consumer, and pre-block list before
  redress. No wave may edit outside the owner paths named by its accepted plan.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V13 closes only when all of these are true:

1. G-Omega has ratified totality V1.1 and folded SK-V12/SK-V13 lessons before
   W0 implementation starts.
2. `SK-V13-open` exists with gate-consumed telemetry for every extant JSON row,
   CSS row, comparator, run id, host, and hot leaf.
3. G1: all 24 non-OUT_OF_SCOPE CSS parity features are `ADMITTED-PARITY` above
   lightningcss on the same strict plane, or carry an architectural-level
   intrinsic-block proof. The SK-V12 declaration-values row counts as 1; 23
   remain open at draft time.
4. G2: the decision-engine fold has landed or been architecturally blocked:
   regex facts extracted, e-graph language wired, active cost as
   `egg::CostFunction`, CSP resolver or measured abrogation, and old P1-P8
   cascade deleted or fail-closed for JSON, CSS, Sheets, and BBNF-self.
5. G3: at least one fresh same-substrate union variant admits with a material
   differential against REDRESS 96/97/98, or the union category records an
   architectural-level block.
6. G4: aarch64 production orphan count is zero. Every primitive is wired to a
   same-wave measured production consumer, or deleted/demoted with REDRESS
   evidence. The `a64_ascii_set_run_skip` production split cannot defer again.
7. G5: all 51 JSON rows, including all 17 `parse_only` rows, beat strict
   sonic-rs on the same plane by at least 1 Mbps, or carry per-row
   architectural-level intrinsic-block proof.
8. G6: totality V1.1 is ratified and the V1.1 CRUD surfaces have landed under
   the G-Omega authority.
9. G7: no silent demotion. Any row or feature that regresses from the prior
   tranche fails close unless it carries architectural-block proof and user
   re-pin.

No ordinary fixpoint closes SK-V13. REDRESS-119/120 are history only. A row
family with implementation-limited misses remains open and brackets forward.
If any G1-G7 item is unmet without architectural-level intrinsic-block proof,
SK-V13 close is REJECT and Pass Alpha brackets SK-V14 immediately under the
same campaign bar.

### Section 0.2 - Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict for JSON; lightningcss strict for CSS; cssparser/golden as independent CSS oracle | May support admission only on the same corpus, output plane, strictness, and measured equality path. |
| Same-run flaw probe | sonic-rs lossy, permissive RapidJSON, CSS recovery modes | Planning only; never strict admission. |
| Sidecar planning signal | simdjson, yyjson, asmjson, historical C++ sidecars, architecture papers | Planning only until freshness, plane, and strictness are gate-consumed. |

Strict admission is executable. Gate code must reject mixed output planes,
permissive anchors, stale sidecars, report-only Mbps, missing equality
artifacts, and validation paths that occur outside the measured row.

### Section 0.3 - Outcome And Disposition Vocabulary

The rendered `skinny/RESULTS.md` JSON outcome surface may retain the existing
values:

```text
A
C
G
K
L
N-direct
S
```

For SK-V13 close accounting, companion reports and REDRESS entries also use
these dispositions:

```text
ADMITTED-PARITY
ADMITTED-SOTA
ARCHITECTURAL-BLOCK
IMPLEMENTATION-BLOCK
REJECTED-MEASURED
ROUTED-TO-NEXT-TRANCHE
```

Only `ADMITTED-*` and `ARCHITECTURAL-BLOCK` close a pinned target.
`IMPLEMENTATION-BLOCK`, `REJECTED-MEASURED`, `S`, `N-direct`, and historical
fixpoint language do not close G1/G5.

### Section 0.4 - Required Telemetry

SK-V13 carries the SK-V8 telemetry discipline and extends it for full-SOTA CSS,
JSON all-plane, decision-engine, union, SIMD, and rolling delta reporting.
Missing required fields fail closed.

Required common fields:

```text
schema_version
row_id
grammar_id
domain
corpus
workload
row_state
outcome
verdict
strictness
output_plane
track1_mbps
track2_mbps_or_oracle
strict_anchor_id
strict_anchor_mbps
comparator_id
comparator_plane
comparator_strictness
comparator_freshness
measured_validation_path
delta_vs_sota
delta_vs_prior_tranche
profile_artifact
hot_leaf
source_commit
criterion_root_or_report_root
artifact_sha256
producer_id
consumer_gate
evidence_timestamp_utc
pass_alpha_goal_id
sample_count
build_flags
host_triple
feature_mask
wave_id
run_id
redress_id
gate_artifact
g_alpha_status
g_omega_status
totality_surface_version
same_wave_consumer_class
track2_independence_status
substrate_surface
structural_projection_status
substrate_cardinality
lock14_status
lock16_status
rolling_sota_delta_status
```

Required JSON-specific fields:

```text
parse_utf8
escape_complete
flaw_probe
sonic_rs_strict_mbps
sonic_rs_lossy_mbps
serde_json_mbps
simdjson_dom_mbps
simdjson_ondemand_mbps
yyjson_default_mbps
asmjson_swar_mbps
asmjson_avx512_mbps
rapidjson_default_mbps
```

Required CSS-specific fields:

```text
css_feature
css_feature_id
css_feature_status
lightningcss_mbps
lightningcss_version
lightningcss_artifact
cssparser_or_golden_oracle
golden_oracle_status
feature_coverage_status
strict_equality_artifact
fixture_id
corpus_id
grammar_checksum
input_checksum
generated_loc
generated_module_bytes
json_guard_state
```

Required decision-engine fields:

```text
regex_fact_source
egraph_node_count
egraph_iteration_count
cost_function_source
candidate_cost_stale_rate
csp_solve_ms
cascade_fallback_status
abrogate_status
```

Every emitted field must be consumed by `gate-json`, a CSS companion gate, or
the rolling SOTA delta gate in the same wave. Producer-only telemetry rejects.
The gate also rejects a missing JSON/CSS row universe, stale run ids, mixed
planes, producer-only fields, SIMD orphans, union stale-route claims,
decision-engine paper close, generated LOC opacity, rolling demotion, and any
implementation-wave telemetry with `g_omega_status` still
`blocked-pre-w0`.

### Section 0.5 - Opening Goalset

JSON target set:

| Plane | Required rows | Admission threshold |
|---|---|---|
| `parse_only` | 17 corpora: `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode` | `Track 1 Mbps > same-run sonic-rs strict parse_only Mbps + 1` |
| `direct_to_struct` | same 17 corpora | `Track 1 Mbps > same-run sonic-rs strict direct_to_struct Mbps + 1` |
| `real_typed_struct` | same 17 corpora; 10 currently missing product surfaces | `Track 1 Mbps > same-run sonic-rs strict real_typed_struct Mbps + 1` after row generation |

Specific current sonic anchors are too large and too easy to stale-copy into
this SPEC. W0 must regenerate a gate-consumed `SK-V13-open` threshold table
from `skinny/RESULTS.md` and the live same-run benches using the formula above.
P3-A §3 carries the compact current threshold table for planning only.

CSS target set:

| Target | Current state | SK-V13 posture |
|---|---|---|
| 24 non-OUT_OF_SCOPE CSS parity features | 1 admitted declaration-values row; 23 open | Every feature must be `ADMITTED-PARITY` above lightningcss + 1 Mbps, or architectural-blocked. No `PARTIAL` feature may close. |

Rolling delta artifact:

```text
restart/skinny/ROLLING-SOTA-DELTA.md
```

W0 must create or refresh a gate-consumed table with:

```text
| row | plane | T1_current | T1_sota | margin | tranche_admitted |
```

The rolling table covers all 51 JSON rows plus every CSS feature. A negative
margin remains open unless architectural-blocked. A backward margin movement
fails G7.

## Section 1 - Non-Negotiables

- No new BBNF directives.
- No new BIR variants.
- No new `BackendShape` variant.
- No public substrate API and no `UnionTape`.
- No parallel substrate, parser-owned structural cursor, aux density table,
  retained class side vector, sidecar event vector, or second source scanner.
- No grammar-name branch or JSON policy in generic crates.
- No strict admission except strict-vs-strict on a matching output plane.
- No stale sidecar, permissive, lossy, historical, or view-boundary evidence as
  strict admission.
- No primitive, kernel, generated path, resolver, union substrate, or telemetry
  producer without a same-wave measured consumer.
- Scalar reference and strict checkasm/parity are required before SIMD/ASM
  wiring.
- Research, plan, challenge when required, and redress remain distinct phases.
- Every miss becomes REDRESS evidence or an explicit routed residual.
- No wave closes on "wired", "integrated", "scaffolded", "future consumer", or
  "paper close" language.
- No support-only behavior wave. Every behavior wave moves at least one row or
  records an architectural-block proof for the touched family.

## Section 2 - Wave Manifest, Caps, And Reruns

All implementation waves are initially blocked. The `Initial dispatch status`
column names the first condition; each wave still requires its own plan and
orchestrator dispatch.

The table below is the canonical V2 dispatch manifest. P3-B's W0-W11 labels
are retained only as V1 packing aliases and map to this table. W10.N, W11.N,
and W14.N are planning subwave series until a wave plan declares a concrete
triumvirate; every declared real subwave counts against the active skinny
bracket. If bracket accounting overflows, W15 closes the tranche as
`REJECT-BRACKET` and Pass Alpha opens SK-V14 immediately; no pinned CSS feature
or JSON row is dropped to satisfy the ceiling.

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Redress cap |
|---|---|---|---|---|---:|
| Pre-W0 | Section 21 | G-Omega Totality V1.1 Block | Mandatory before W0 | Totality CRUD only; no skinny source | n/a |
| W0 | Section 3 | Baseline, Telemetry, Rolling Delta | Blocked until G-Omega + S-P3 convergence | 0 behavior LOC; <=350 gate/report/test/doc LOC | 30 min impl + 15 min measure |
| W1 | Section 4 | CSS Comparator/Oracle Harness Expansion | Conditional on W0 | <=450 CSS gate/report/oracle LOC | 30 + 15 |
| W2 | Section 5 | CSS Stylesheet Root + Selectors | Conditional on W0/W1 | <=500 source/test LOC; generated named separately | 30 + 15 |
| W3 | Section 6 | CSS Declaration-Value Expansion | Conditional on W1/W2 | <=840 source/test LOC | 30 + 15 |
| W4 | Section 7 | CSS Visual, At-Rule, Nesting Pack | Conditional on W2; may split | <=950 per selected pack, split if exceeded | 30 + 15 |
| W5 | Section 8 | Decision Fold A: Regex Extraction + Feature Gate | Conditional on W0; first-of-class challenge | <=330 source/test LOC | 45 + 15 |
| W6 | Section 9 | Decision Fold B: E-Graph + Active Cost | Conditional on W5 | <=1250 source/test LOC, split if exceeded | 45 + 15 |
| W7 | Section 10 | Decision Fold C: CSP + Cascade Fail-Closed | Conditional on W6 | <=970 source/test LOC, split if exceeded | 45 + 15 |
| W8 | Section 12 | Per-Grammar Policy, Sink/View, Flag Surface | Conditional on W0/W5 where touching resolver facts | <=650 source/test LOC | 45 + 15 |
| W9 | Section 13 | Same-Substrate Union Material Differential | Conditional on W0/W8 and challenge | <=650 source/test LOC | 45 + 15 |
| W10.N | Section 14 | CSS Parity Expansion Subwaves | Conditional on W1-W4 disposition | <=550 default; <=950 with challenge-accepted pack | 30 + 15 |
| W11.N | Section 15 | JSON Direct Residual Reopen Subwaves | Conditional on W5-W9 routed material differential | <=450 per row family | 30 + 15 |
| W12 | Section 16 | SIMD/ASM Production Wiring + Zero Orphans | Conditional on W0/W8 and challenge | <=450 source/test LOC | 45 + 15 |
| W13 | Section 17 | Typed Product Surface Completion | Conditional on W8/W5 and row plan | <=650 per typed surface batch | 30 + 15 |
| W14.N | Section 18 | `parse_only` Admission Subwaves | Conditional on W5/W9/W12 routed route | <=450 per row family | 30 + 15 |
| W15 | Section 19 | Close Or Bracket | Conditional on all waves admitted/rejected/routed | 0 behavior LOC; docs/gates only | 30 + 15 |

The W10/W11/W14 subwave series is authorized by the 2026-05-21 user pin. If an
orchestrator applies the ordinary 12-wave escalation mechanically to individual
subwaves, it must escalate for user accounting without dropping any pinned row
or feature. The campaign continues until full ADMIT or architectural blocks.

Phase caps:

| Phase | Default cap | SK-V13 note |
|---|---:|---|
| Research | 20 min per agent | 30 min pass-contract ceiling remains the maximum; use 20 min unless wave plan justifies more. |
| Plan | 15 min | Must produce exact owner paths, row gates, revert slice, and same-wave consumer. |
| CHALLENGE | 90 min | Mandatory for first-of-class, substrate-touching, decision-engine fold, union, SIMD/ASM, or high-risk CSS grammar expansion. |
| Redress | 30 min implementation + 15 min measurement | W5-W9 and W12 use 45 + 15 under the pin. |

Rerun ceilings:

| Wave family | Focused verification | Rerun ceiling |
|---|---|---|
| W0 | schema/gate/rolling-delta tests, full-table threshold capture | one gate refresh plus one confirm rerun |
| CSS W1-W4/W10 | strict equality, lightningcss/cssparser or golden, feature coverage, JSON guard | one full gate refresh |
| Decision W5-W7 | unit/property tests, JSON/CSS guard, abrogate metrics | one focused guard rerun; second requires REDRESS cost note |
| Union W9 | parity/checkasm if SIMD, row consumer, full JSON/CSS guard | one full gate refresh |
| JSON W11/W13/W14 | strict equality, same-plane sonic, Track 2/oracle independence | one full gate refresh |
| SIMD W12 | strict checkasm, corpus parity, production consumer, zero-orphan audit | one full gate refresh |
| W15 close | document reconciliation and rolling delta | no performance rerun unless source moved |

Extra reruns beyond ceiling are REDRESS cost evidence, not retry room.

### Section 2.1 - Generality, Lock 14, And Lock 16 Gate

Every wave has this exit gate. Generic-crate edits add the non-JSON proof.

- Public API scan: no new public JSON/CSS/Sheets/BBNF-specific API appears in
  generic crates.
- Grammar branch scan: no generic branch selects behavior by grammar name,
  corpus name, object/array role, field name, string role, layout role, or
  CSS feature name.
- Primitive/table scan: generic SIMD/classifier code may consume generated
  byte sets and opaque class ordinals, but may not embed JSON quote/escape/
  control policy or CSS feature semantics.
- Template/provider boundary: per-grammar providers/templates own policy;
  generic codegen consumes grammar-derived facts.
- Non-JSON proof: any generic edit must prove a strict CSS L4 positive lane
  and both Sheets and BBNF-self fail-closed, compile/lower/cost,
  unchanged-output, or generated-role fact-row witnesses before making
  fleet-wide grammar-neutral claims. With CSS L4 plus only one of Sheets or
  BBNF-self, the wave may proceed only with a scoped witness label naming the
  covered grammars; it may not use fleet-wide, universal, or grammar-neutral
  closure wording.
- Lock 16 proof: every SIMD/ASM primitive must have scalar reference,
  strict checkasm/differential coverage, corpus parity, feature-mask
  disclosure, same-wave production consumer, and no public substrate API.

`G-SIMD-GRAMMAR-POLICY` is mandatory whenever `bbnf-simd` enters CSS, union,
JSON `parse_only`, or shared generated code. The wave plan must name the
consuming grammar's quote, escape, control, delimiter, string, and number
policy, or an explicit no-string/no-number policy.

## Section 3 - W0 Baseline, Telemetry, Rolling Delta

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/` using `wave-0-*` naming
- `skinny/REDRESS.md` only if W0 rejects

Entry gate:

- G-Omega closed by user.
- S-P3 converged or user-pinned.
- No behavior source edit is in W0 scope.

Tasks:

1. Capture `SK-V13-open` for every extant JSON row, CSS row, comparator, run
   id, host, build, feature mask, and hot leaf.
2. Render the full 51-row JSON target inventory and 24-feature CSS target
   inventory, including missing typed rows and open CSS features.
3. Create or refresh `restart/skinny/ROLLING-SOTA-DELTA.md`.
4. Make gates reject missing required telemetry, stale run ids, permissive
   anchors, mixed planes, and producer-only fields.

Exit gate:

- All required Section 0.4 fields exist or carry explicit absent reasons.
- Rolling delta table covers all 51 JSON rows and every CSS feature.
- No parser, scanner, SIMD, codegen, generated runtime, or product behavior
  changes.

Same-wave consumer: `gate-json`, CSS companion gates, and rolling-delta gate
consume every emitted required field.

Revert protocol: revert W0 gate/report edits and restore opening RESULTS
schema; record REDRESS naming the missing telemetry or stale comparator.

Downstream effect: W0 rejection blocks all behavior waves.

## Section 4 - W1 CSS Comparator/Oracle Harness Expansion

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- CSS fixtures/reports under `restart/skinny/tranches/sk-v13/research/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected

Entry gate: W0 admitted.

Tasks:

1. Generalize the SK-V12 lightningcss/cssparser companion gate to additional
   CSS row families without counting any row as admitted by harness existence.
2. Add feature-coverage match reporting: every variant lightningcss accepts
   must be accepted and every variant lightningcss rejects must be rejected.
3. Define same-plane output facts for W2-W4/W10 rows.

Exit gate:

- The admitted declaration-values row maintains strict equality and
  `Track1_after >= max(lightningcss_open + 1.0, 0.98 * SK-V13-open Track1)`.
- New harness rows are gate-consumed, freshness-bound, and reject report-only
  Mbps.
- JSON guards show no silent demotion.

Same-wave consumer: CSS companion gate consumes comparator/oracle fields.

Pre-blocks: no CSS `PARTIAL` close, no diagnostic source/comment/whitespace row
as parity admission, no lightningcss recovery mode as strict anchor.

Revert protocol: revert CSS gate/report changes; retain research notes; record
REDRESS if comparator cannot be made same-plane.

## Section 5 - W2 CSS Stylesheet Root + Selectors

Owner paths:

- `skinny/crates/codegen/src/css_*`
- `skinny/crates/runtime/src/grammars/css_l4_*`
- CSS bench/oracle fixtures
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected

Entry gate: W0/W1 admitted; W2 plan names corpus, selectors, output facts, and
strict equality artifacts.

Tasks:

1. Add stylesheet root, rule list, selector list, and qualified rule facts.
2. Emit selector tokens/facts for type/class/id, combinators, attributes,
   pseudo-classes, and pseudo-elements selected by the W2 plan.
3. Use Bootstrap/W1b or another named fixture only after it is gate-captured.

Exit gate:

- `css_l4/stylesheet_and_selectors/direct_to_struct/main` passes strict
  equality, feature coverage, oracle, and `Track 1 > lightningcss + 1`.
- Declaration-values admitted row and JSON guards maintain.
- Lock 14 non-JSON proof passes for any generic edits.

Same-wave consumer: generated CSS stylesheet/selectors row.

Revert protocol: revert CSS codegen/runtime/bench/RESULTS slice; add REDRESS
naming failed selector features and fixtures.

## Section 6 - W3 CSS Declaration-Value Expansion

Owner paths: same CSS codegen/runtime/bench families as W2.

Entry gate: W1 admitted; W3 plan names exact value families and recursion
limits.

Tasks:

1. Add var(), calc(), URL, color-function, string, escaped-ident, and extended
   declaration-value facts selected by the plan.
2. Keep recursion bounded and gate-visible.
3. Pair any generic string/number policy edit with Section 2.1 proof.

Exit gate:

- `css_l4/declaration_values_extended/direct_to_struct/main` or narrower
  selected rows pass strict equality, oracle, and lightningcss + 1.
- Existing declaration-values row maintains.
- No JSON string/number policy enters generic crates.

Same-wave consumer: generated CSS declaration-values extended row.

Pre-blocks: no JSON number/string reuse without generated policy; no source
mapping/comment/whitespace diagnostic substitution.

Revert protocol: revert selected CSS expansion and generated outputs as one
slice; record REDRESS with row and feature coverage failures.

## Section 7 - W4 CSS Visual, At-Rule, Nesting Pack

Owner paths: CSS codegen/runtime/bench/oracle paths named by plan.

Entry gate: W2 admitted or explicitly routed; W4 plan selects one pack if LOC
would exceed cap.

Tasks:

1. Add one or more packs: visual functions, at-rule/media/keyframes, nested
   rules, or vendor/custom at-rule taxonomy.
2. Use named real-world corpus and same-plane lightningcss facts.
3. Split packs if estimated LOC or redress cap would overflow.

Exit gate:

- Each selected row passes strict equality, feature coverage, independent
  oracle, and lightningcss + 1.
- Previously admitted CSS and JSON rows maintain.

Same-wave consumer: generated CSS pack row.

Revert protocol: revert pack source/generated/gate/RESULTS slice; record
measured rejection per feature family.

## Section 8 - W5 Decision Fold A: Regex Extraction + Feature Gate

Owner paths:

- `skinny/crates/parse-that-regex/`
- proposed scoped `skinny/crates/bbnf-regex/` if the plan accepts new crate
- `skinny/crates/ir/src/`
- `skinny/crates/passes/src/`
- tests and reports named by plan

Entry gate: W0 admitted; challenge accepts new crate/path and Lock 11/14 shape.

Tasks:

1. Extract nullable, first-set, byte-class, and regex HIR analysis into a
   grammar-neutral API.
2. Replace hardcoded regex predicate helpers in IR/passes with the extracted
   analysis.
3. Feature-gate the decision fold so pre-fold behavior fails closed.

Exit gate:

- Regex analysis API has unit/property tests and is consumed by IR/passes in
  the same wave.
- Zero hardcoded JSON regex pattern strings remain in generic decision logic.
- JSON/CSS guard rows maintain.
- The extracted facts are consumed by a named generated selection path and
  either move at least one JSON or CSS row by P3-C `row_move_toward_sota`, admit
  a row, or record a measured architectural block. Support-only extraction
  rejects.

Same-wave consumer: IR/passes analysis calls.

Pre-blocks: no support-only crate extraction; no hardcoded JSON pattern strings
under neutral names.

Revert protocol: revert extraction and call-site changes; record REDRESS if
the API cannot remain grammar-neutral.

## Section 9 - W6 Decision Fold B: E-Graph + Active Cost

Owner paths:

- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/src/`
- codegen lowering paths named by plan

Entry gate: W5 admitted.

Tasks:

1. Wire an e-graph language over backend expressions or an equivalent
   challenge-accepted representation.
2. Add conservative rewrites over IR facts, not grammar names.
3. Convert passive cost facts into active extraction cost.

Exit gate:

- E-graph run is bounded by node/iteration/memory telemetry.
- Active cost chooses a deterministic candidate and emits cost facts consumed
  by the gate.
- Stale cost rate is <=30% of candidate expressions or REDRESS abrogates.
- JSON/CSS guard rows maintain.
- The selected candidate is consumed by generated backend selection and either
  moves at least one JSON or CSS row by P3-C `row_move_toward_sota`, admits a
  row, or records a measured architectural block. Bounded e-graph/cost
  telemetry alone rejects.

Same-wave consumer: generated backend selection consumes extracted candidate.

Pre-blocks: no fused solver, no order-dependent rewrite set with >10% variance,
no support-only e-graph scaffold.

Revert protocol: revert e-graph/cost integration; preserve research evidence;
record REDRESS with abrogate metrics.

## Section 10 - W7 Decision Fold C: CSP + Cascade Fail-Closed

Owner paths:

- `skinny/crates/passes/src/`
- `skinny/crates/passes/Cargo.toml`
- `skinny/crates/codegen/src/`
- `skinny/crates/bbnf-bench/`

Entry gate: W6 admitted.

Tasks:

1. Add CSP resolver or measured abrogation route after cost extraction.
2. Encode parity, recognizer, substrate, SIMD, and capacity constraints.
3. Delete or fail-close the old P1-P8 cascade for JSON, CSS, Sheets, and
   BBNF-self.

Exit gate:

- CSP solve time stays under the wave threshold or REDRESS abrogates to the
  measured fallback.
- Old cascade cannot silently serve rows after resolver fold.
- JSON and CSS guard rows maintain or improve.
- Resolver output is consumed by `compile()` / generated backend selection and
  either moves at least one JSON or CSS row by P3-C `row_move_toward_sota`,
  admits a row, or records a measured architectural block. Cascade retirement
  without a row-consumed result is a measured reject, not an admit.

Same-wave consumer: `compile()` / generated backend selection consumes resolver
output.

Pre-blocks: no silent fallback to old cascade, no feature-gated scaffold that
does not affect lowering.

Revert protocol: revert CSP/cascade changes; record measured UNSAT/timeout or
regression evidence.

## Section 12 - W8 Per-Grammar Policy, Sink/View, Flag Surface

Owner paths:

- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_*`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/parse-that-regex/`
- `skinny/crates/bbnf-bench/`

Entry gate: W0 admitted; W8 plan names the policy surfaces and row consumers.

Tasks:

1. Convert dispatch, string, escape, number, sink/view, and sparse flag policy
   into generated per-grammar surfaces where touched.
2. Keep generic storage stable: `Tape`, `ValueRef`, `TapeBuilder`, and physical
   flag bytes may remain, but semantics live in generated grammar modules.
3. Pair JSON guards with at least one CSS row when generic code changes.

Exit gate:

- At least one touched JSON/CSS row consumes the new policy surface in the same
  wave and either moves by P3-C `row_move_toward_sota`, admits, or records a
  measured architectural block. "JSON output unchanged" is guard evidence only,
  not an admission.
- Lock 14 proof passes.

Same-wave consumer: generated JSON and CSS rows named by plan.

Pre-blocks: no public `GrammarConfig`, no generic `JsonSink` acceleration, no
JSON quote/backslash/control constants in generic code.

Revert protocol: revert policy/sink/flag changes and generated outputs as one
slice; record REDRESS with leakage path.

## Section 13 - W9 Same-Substrate Union Material Differential

Owner paths:

- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_*`
- `skinny/crates/codegen/src/`
- `skinny/crates/passes/src/`
- `skinny/crates/bbnf-simd/` if SIMD-first route is selected
- `skinny/crates/bbnf-bench/`

Entry gate:

- W0 and W8 admitted or routed.
- Challenge accepts the material differential against REDRESS 96/97/98.
- W9 plan names one variant: C1 codegen-private per-rule projection, C2
  e-graph selected shape, or C3 SIMD-first mask-to-tape writer.

Tasks:

1. Attempt one fresh union variant with a row consumer.
2. Prove single-substrate ownership: if structural projection is retained, it
   is the tape/fact stream itself, not a sidecar.
3. Measure at least one JSON or CSS row, or record architectural block.

Exit gate:

- At least one union variant is ADMITTED with strict row movement, or
  architectural-blocked with intrinsic evidence.
- No class column, retained structural index, parser-owned cursor/list, aux
  table, sidecar vector, second scan, or public `UnionTape` survives.
- Full JSON/CSS guard maintain.
- If W9 touches `skinny/crates/bbnf-simd/` or selects C3 SIMD-first routing,
  `orphan_count_after = 0`, strict checkasm status, scalar-reference status,
  delete/demote/revert protocol, and production consumer row evidence are
  same-wave exit predicates. W9 cannot rely on W12 for later orphan cleanup.

Same-wave consumer: generated CSS fact stream, JSON retained parse, JSON direct
projection, or another plan-named production row.

Revert protocol: revert runtime/tape/SIMD/codegen/generated/gate/RESULTS slice;
save rejected patch path in REDRESS.

## Section 14 - W10.N CSS Parity Expansion Subwaves

Owner paths: CSS codegen/runtime/bench/oracle paths named per subwave.

Entry gate: W1-W4 have admitted, rejected, or routed the foundation and packs.

Tasks:

1. Generate one subwave per remaining non-OUT_OF_SCOPE CSS feature not already
   admitted by W2-W4.
2. Name the feature, corpus, oracle, lightningcss facts, generated output, and
   strict equality artifact before redress.
3. Dispatch non-overlapping feature subwaves concurrently only when owner paths
   and RESULTS/REDRESS writes can be serialized safely.

Exit gate:

- Each subwave either admits its feature above lightningcss + 1 with strict
  equality and oracle proof, or records architectural-level intrinsic block.
- No feature remains `PARTIAL` at SK-V13 close.

Same-wave consumer: the generated CSS feature row named by the subwave, plus
its production fact-stream caller in `skinny/crates/runtime/src/grammars/css_l4_*`.

Revert protocol: revert feature row source/generated/gate/RESULTS; record
REDRESS with exact feature and failed variants.

## Section 15 - W11.N JSON Direct Residual Reopen Subwaves

Owner paths:

- JSON generated runtime/templates/direct sink paths
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/parse-that-regex/`
- `skinny/crates/bbnf-simd/` if selected
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate:

- W0 threshold table exists.
- W5-W9 have produced a fresh material differential for the row family, or the
  subwave records architectural-block evidence.

Tasks:

1. Reopen every direct residual row under the full-SOTA pin, including rows
   formerly covered by REDRESS-119.
2. Cite REDRESS-119/120 history and name the fresh differential per row.
3. Use same-plane sonic strict + 1 and strict equality.

Exit gate:

- Selected direct rows admit above sonic strict + 1 or are architecturally
  blocked.
- Existing A/GO rows and other planes do not silently demote.

Same-wave consumer: the generated JSON direct sink / digest production path
selected by the row plan and exercised by `skinny/crates/bbnf-bench/src/direct_struct.rs`.

Revert protocol: revert row changes and generated outputs; append REDRESS with
row, material differential, comparator, and failed threshold.

## Section 16 - W12 SIMD/ASM Production Wiring + Zero Orphans

Owner paths:

- `skinny/crates/bbnf-simd/`
- `skinny/crates/bbnf-simd/tests/`
- CSS/JSON runtime consumers named by plan
- `skinny/crates/bbnf-bench/`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` if row movement occurs

Entry gate:

- W0 admitted.
- Challenge accepts scalar reference, checkasm matrix, feature mask, and
  production consumer.

Tasks:

1. Complete `a64_ascii_set_run_skip` production split with a CSS scan-block
   consumer, or record measured rejection.
2. Wire any selected new primitive only with same-wave production consumer.
3. Audit all aarch64 primitive files for orphan status.

Exit gate:

- Zero aarch64 orphans.
- Every wired primitive has strict checkasm, corpus parity, and row movement or
  measured rejection.
- Checkasm-only or microbench-only admissions reject.

Revert protocol: revert primitive and consumer together, or delete/demote
orphan with REDRESS evidence.

## Section 17 - W13 Typed Product Surface Completion

Owner paths:

- `skinny/crates/codegen/`
- generated real-typed JSON outputs
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate: W0 admitted; W13 plan names missing typed rows and Track 2/oracle
independence.

Tasks:

1. Generate product surfaces for typed rows absent from S-P1.
2. Capture same-run sonic strict typed anchors.
3. Prove Track 1 generated path and independent Track 2/oracle.

Exit gate:

- Each selected typed row admits above sonic strict + 1 or records
  architectural block.
- Existing typed admits maintain.
- Direct digest rows are not counted as typed product proof.

Same-wave consumer: the generated real-typed product parser and its independent
Track 2/oracle harness for the selected corpus batch.

Revert protocol: revert typed row generation and reports; REDRESS missing
schema/oracle/threshold failures.

## Section 18 - W14.N `parse_only` Admission Subwaves

Owner paths:

- JSON parse runtime/templates
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/parse-that-regex/`
- `skinny/crates/bbnf-simd/` if selected
- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate:

- W0 threshold table exists.
- The subwave has a fresh material differential from decision, policy, union,
  or SIMD outputs.

Tasks:

1. Reopen all 17 `parse_only` rows under strict sonic parse comparison.
2. Remove diagnostic-only close language from row accounting.
3. Admit rows or record architectural block.

Exit gate:

- Selected `parse_only` rows beat sonic strict parse + 1 with strict equality
  and measured validation path.
- No parse row remains closed as `S / NO-GO` by diagnostic exemption.

Same-wave consumer: the generated JSON parse path or selected parse runtime
caller exercised by `bbnf-bench` `parse_only` for the named corpus row.

Revert protocol: revert parse changes and generated outputs; record REDRESS
with row, threshold, and differential.

## Section 19 - W15 Close Or Bracket

Owner paths:

- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- close artifact under `restart/skinny/tranches/sk-v13/research/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`

Entry gate: every planned wave/subwave has admitted, rejected, routed, or
architectural-blocked status.

Tasks:

1. Reconcile G1-G7 against RESULTS, REDRESS, rolling delta, and wave reports.
2. Confirm no implementation-limited miss is being treated as close evidence.
3. If any target remains open, prepare immediate Pass Alpha bracket to SK-V14.

Exit gate:

- Full ADMIT or architectural-block proof covers every target; or close is
  REJECT and SK-V14 bracket is triggered.
- Rolling delta shows no silent demotion.
- Zero aarch64 orphan audit passes.

Revert protocol: no source revert by default. Reopen the producing wave or mark
close blocked with exact files, rows, and missing evidence.

## Section 20 - Pre-Blocked Routes

Every wave packet must copy the exact row below that applies to the wave. A
generic "inherits Section 20" reference is insufficient for redress dispatch.

Global blocks for every wave: no new directive, BIR variant, `BackendShape`,
`UnionTape`, public substrate API, parser-owned cursor/facts, sidecar substrate,
parallel substrate, generic JSON/CSS policy in generic crates, stale/permissive
comparator evidence as strict admission, Track 1/Track 2 coupling,
benchmark-private parser equality, orphan primitive, checkasm-only admission,
harness-only hardening as row movement, or telemetry-only producer.

Route-state vocabulary:

```text
BLOCKED-HISTORICAL
REOPEN-CONDITIONAL
GATE-FEED
HISTORY-LIFTED
MIXED
```

| SPEC wave family | Pre-blocked REDRESS entries and route-state handling |
|---|---|
| Pre-W0/W0 | 75, 77, 78, 99-102, 111, and 119-127 are `GATE-FEED`; 119/120 cannot close; no source, RESULTS, or REDRESS work before G-Omega. |
| W1-W4 CSS + W10.N CSS | 112, 113, 123-127 are `GATE-FEED`; 28/33, 50-55, 60-72, 82-84, 88/89, and 126 are `BLOCKED-HISTORICAL` whenever CSS uses string, escape, or SIMD routes; 123-125/127 cannot count as full CSS close. |
| W5-W7 decision engine | 84, 87, 114, 115 plus 85-87/121 are gate/block families; no JSON-specific generic branch, support-only regex/egraph/CSP extraction, fused hidden solver, or old P1-P8 cascade fallback admission. |
| W8 policy/sink/view | 121 is `GATE-FEED`; 54/55/66-69, 80, 82, and 84 are `BLOCKED-HISTORICAL`; no public `GrammarConfig`, generic `JsonSink` acceleration, JSON policy in generic code, source-hook, decoded-string stats/hash, one-row number patch, or control compaction replay. |
| W9 union | 50, 51, 53, 92, 96, 97, 98, 88, 89, and 126 bind; REDRESS 96/97/98 are `REOPEN-CONDITIONAL` only for a named material differential. Exact class-column, streaming-cursor, class-lane-only, `StructuralIndex`, parser-owned cursor/list, aux table, sidecar vector, scalar-delegate body, and `UnionTape` routes stay blocked. |
| W11.N direct | 54, 55, 66-69, 73, 80, 82, 84, 106-108, and 114-119 bind; 119/120 are `HISTORY-LIFTED` and must be cited but cannot close a row. Source-hook/digest/hash/string/number/control replays stay blocked. |
| W12 SIMD/ASM | 88, 89, 90, 122, 126, and relevant 121-127 gate feed bind; PMULL/CTZ tested bodies, microbench-only admission, checkasm-only admission, second production-split deferral, and retained orphans stay blocked. |
| W13 typed product | 70-72 and 103-110 are `MIXED`: typed product precedent is allowed, but direct digest rows, hidden typed sinks, proof-only escape routes, and no-op production rows are not typed admission. |
| W14.N parse-only | 28, 33, 50, 51, 53, 60-65, 72 overgeneralization, 82-84, 88, 89, 92, 96-98, and 102 bind. Parse rows are target-eligible, but docs-only `S` to `A` movement and stale retained string/control/union replays remain blocked. |
| W15 close | 119, 120, 123-127 plus the full-SOTA addendum bind; no ordinary fixpoint, implementation-limited miss, one-CSS-row close, or REDRESS-history close. |

## Section 21 - G-Omega And Dispatch Scope

G-Omega must close before W0. The Omega cycle must fold at least:

- SK-V12 CSS L4 strict lightningcss admission.
- REDRESS-119/120 history as superseded by the full-SOTA addendum.
- REDRESS-121 GrammarConfig/Lock 14 evidence.
- REDRESS-122 escape-mask/Lock 16 discipline.
- REDRESS-123 through REDRESS-127 CSS, comparator, zero-orphan, and close
  evidence.
- Non-JSON telemetry schema and rolling SOTA delta requirement.
- Zero-orphan and same-wave-consumer discipline.

S-P3 convergence must also close before implementation waves. Until both gates
close, the only allowed SK-V13 work is planning/research under
`restart/skinny/tranches/sk-v13/` and read-only inspection of `skinny/RESULTS.md`
and `skinny/REDRESS.md`.
