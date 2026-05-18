# SK-V8 SPEC - S-P3 Wave Plan

Date: 2026-05-18.

Status: S-P3 converged planning packet. This file is not an implementation
dispatch. It folds the converged S-P2 substrate-ceiling cohort, P3-A through
P3-E, P3 hardening through V5, and the existing Alpha packet into a conditional
W0-W6 wave plan.

Authority:

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V5-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/alpha/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock:

- No SK-V8 implementation wave dispatches from S-P3 itself.
- G-Alpha closed by user on 2026-05-18T05:26:48Z.
- Current dispatch authority covers W0 only.
- W1-W6 are conditionally gated by this packet, but each remains blocked until
  W0 closes, the wave plan names exact owner paths and row gates, required
  challenge accepts, and the orchestrator/user dispatches that wave.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V8 closes only when all of these are true:

1. W0 creates a checked `SK-V8-open` baseline with no placeholder hot leaves.
2. Every current main row has required profile, comparator, run, host, build,
   cost, freshness, and delta telemetry.
3. `gate-json` rejects rows missing required SK-V8 telemetry.
4. W1 makes CostFacts and strict comparator evidence gate-consumed before any
   behavior wave can admit route quality.
5. The four current `real_typed_struct` GO rows maintain GO.
6. Any new typed product row uses generated Track 1 plus structurally
   independent Track 2 or oracle proof.
7. Any parse/direct behavior wave either meets its named row threshold and
   full-table maintain gate or rejects with REDRESS evidence.
8. No pre-blocked route reopens without fresh W0 evidence, same-wave consumer,
   REDRESS citation, no-regression gate, and challenge acceptance.
9. Lock 14 and Lock 15 gates pass at every wave close.
10. `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
    `restart/skinny/tranches/sk-v8/HANDOFF.md` agree at close.

### Section 0.2 - Comparator Classes

SK-V8 uses three comparator classes:

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict, serde_json where output plane matches | May support strict admission if row output plane matches and validation occurs in the measured row. |
| Same-run flaw probe | sonic-rs lossy, permissive rows | Planning only; never strict admission. |
| Sidecar planning signal | simdjson, yyjson, RapidJSON, asmjson unless refreshed under same-run rules | Planning only until freshness, strictness, and output-plane rules are satisfied. |

Strict admission is executable, not prose-only: `gate-json` must reject strict
admission unless the comparator plane matches the row output plane,
`comparator_strictness=strict`, the comparator is a same-run native strict
anchor admitted by id, and UTF-8/control/escape validation occurs inside the
measured row. W0 has no structured sidecar same-run manifest; any
`sidecar-same-run` claim rejects until a later accepted wave adds a structured
manifest parser and gate. `Strictness=deferred`, `parse_utf8=view-boundary`,
stale sidecars, sidecar-only evidence, historical deltas, and plane mismatch
are guard telemetry only.

### Section 0.3 - Outcome Enum

The current schema supports:

```text
A
C
G
K
L
N-direct
S
```

`L`, `N-direct`, and `S` are valid current W0 outcomes. `S` is the explicit
substrate-guard / non-SOTA spelling for admission-capable parse outcomes; hard
parse failures such as the current canada SIMD-throughput failure remain
specific (`L`) instead of being demoted. Neither hard-failure outcomes nor `S`
may support strict SOTA admission.

### Section 0.4 - Required Telemetry

The rendered `skinny/RESULTS.md` table may keep the existing 26-column schema
surface. SK-V8 adds required report/gate fields after W0; they may be rendered
as columns, a gate-consumed manifest, or a gate-consumed JSON payload, but they
must be consumed by `gate-json` in the same wave.

Required fields:

```text
row_id
grammar_id
domain
comparator_id
comparator_plane
comparator_strictness
comparator_freshness
measured_validation_path
Profile artifact
Cycles per byte or equivalent sample cost
Sample count
Build flags
Host triple
Feature mask
CostFacts rule id
CostFacts chosen shape
CostFacts rejected alternative ids
Redress entry
Wave id
Run id
Sidecar freshness
SK-V8-open delta
substrate_surface
structural_projection_status
substrate_cardinality
same_wave_consumer_class
track2_independence_status
```

Every emitted field must be consumed by `gate-json` in the same wave. Missing
required fields, unsupported outcome, strictness mismatch, stale sidecar,
producer-only telemetry, W0 behavior drift, missing W1 CostFacts, W3 side
substrate, W3 telemetry substitution, Lock 14 generic leak, or cap overflow
rejects the wave.

### Section 0.5 - Opening Row Goalset

Current main-table state from W0-rendered `skinny/RESULTS.md` after
`SK-V8-open`:

| Family | Current state | SK-V8 posture |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | Substrate-guard non-admission plus one hard SIMD-throughput failure. W3 may use selected rows as strict guard/behavior evidence only under measured-path predicates. |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` | Digest guard plane. W4 may triage selected misses; direct digest is not typed product proof. |
| `real_typed_struct` | 4 `A / GO` | Product plane. W2 must maintain these and may add generated typed rows. |

W0 target for all 38 current rows: capture `SK-V8-open`, populate required
telemetry, and keep every throughput cell within +/-1.0% of the captured seed.

W2 existing real-typed GO maintain floors, from the current opening rows:

| Row | Current Track 1 | sonic strict | Sonic GO floor | No-regression floor |
|---|---:|---:|---:|---:|
| `twitter/real_typed_struct` | 15333 | 13646 | 12406 | 15027 |
| `update_center/real_typed_struct` | 11958 | 11952 | 10866 | 11719 |
| `mesh/real_typed_struct` | 9623 | 9305 | 8460 | 9431 |
| `marine_ik/real_typed_struct` | 11783 | 6951 | 6319 | 11548 |

W2 existing direct GO guard floors:

| Row | Minimum Track 1 Mbps | Minimum Track 2 Mbps |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 21151 | 19434 |
| `marine_ik/direct_to_struct` | 9357 | 9488 |
| `unicode_basic/direct_to_struct` | 9363 | 8420 |

The remaining W2/W3/W4 numeric seed tables from S-P3 are archived pre-W0
planning data, not dispatch floors. Later W2/W3/W4 plans may select only rows
named by these candidate lists unless an accepted S-P3 revision expands them,
but each plan must recompute thresholds from current `SK-V8-open` strict-anchor
and row telemetry before challenge or redress.

| Wave | Candidate rows retained from S-P3 | Floor status |
|---|---|---|
| W2 typed | `canada`, `numbers`, `unicode_basic`, `citm_catalog`, `apache_builds` `real_typed_struct` | Recompute typed same-plane floors from `SK-V8-open`. |
| W3 parse | `twitter`, `apache_builds`, `update_center`, `github_events`, `gsoc-2018`, `distinct_values`, `y_string_unicode`, plus guard rows `canada`, `mesh`, `numbers`, `marine_ik` | Recompute parse guard/behavior thresholds from `SK-V8-open`. |
| W4 direct | `twitter`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, `y_string_unicode` `direct_to_struct` | Recompute direct guard floors from `SK-V8-open`. |

## Section 1 - Non-Negotiables

- No new BBNF directives.
- No new BIR variant.
- No new `BackendShape` variant.
- No `UnionTape`.
- No new substrate surface; W3 is representation replacement inside one
  retained `Tape`, not a new substrate.
- No new public substrate API.
- No parser-owned structural cursor/facts.
- No parallel or sidecar substrate.
- No JSON policy in generic crates.
- No strict admission except strict-vs-strict on a matching output plane.
- No stale sidecar, permissive, lossy, historical, or view-boundary evidence as
  strict admission.
- No primitive, kernel, generated path, or substrate representation without a
  same-wave hot-path consumer.
- Scalar reference and checkasm parity are required before primitive wiring.
- Research, plan, challenge when required, and redress remain distinct phases.
- Every miss becomes REDRESS evidence or an explicit routed residual.
- No deferrals: a wave cannot close on "wired", "advisory", "future consumer",
  "integrated", or "paper close" language without measured evidence.

## Section 2 - Wave Manifest, Caps, And Reruns

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Implementation/redress cap |
|---|---|---|---|---|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | Dispatchable only after G-Alpha | 0 production behavior LOC; <=350 report/gate/schema/test/doc LOC | <=90 min |
| W1 | Section 4 | CostFacts And Comparator Gate Binding | Conditional on W0 close | 0 parser/generated behavior LOC; <=300 CostFacts/report/gate/test LOC | <=90 min |
| W2 | Section 5 | Typed Product Plane Expansion | Conditional on W0/W1 close | <=650 source/test LOC; generated output and row tables named separately | <=90 min |
| W3 | Section 6 | Tier A Tape Plus Structural-Projection Union | Conditional on W0/W1 close and challenge | <=450 source/test LOC default; <=650 only with accepted pre-redress fit proof | <=90 min |
| W4 | Section 7 | Direct Guard Triage | Conditional on W0/W1 and W2/W3 disposition or route | <=300 source/test LOC and <=3 selected rows | <=90 min |
| W5 | Section 8 | Grammar-Neutral Audit And Lock 14 Preservation | Conditional on W1-W4 dispositions | 0 source LOC default; <=150 named Lock 14 cleanup LOC | <=90 min |
| W6 | Section 9 | Close And Alpha Feedback | Conditional on W0-W5 dispositions | 0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only | <=90 min |

LOC budgets are conjunctive with the 90-minute cap and rerun ceilings. They
count hand-edited source, tests, gate/report/schema code, and hand-written doc
or result edits named by the row. Generated outputs do not consume the source
LOC budget, but every generated file must be named, diff-audited, and included
in the revert slice. A wave plan that exceeds either its LOC budget or the
90-minute implementation/redress cap must split before dispatch or return
REVISE.

Phase caps:

| Phase | Cap |
|---|---:|
| Research | 30 min per agent, max 6 agents |
| Plan | 30 min |
| Challenge | 90 min when first-of-class, substrate-touching, primitive, or high-risk |
| Implementation/redress | 90 min maximum, including source edits, generation, verification, RESULTS/REDRESS updates, and rollback |

If a planned implementation cannot fit the 90-minute redress cap, the plan must
split before dispatch or return REVISE. Older 120-300 minute hard caps are not
dispatch authority for SK-V8.

Rerun ceilings:

| Wave | Focused verification | Rerun ceiling |
|---|---|---|
| W0 | report/gate tests, malformed sidecar-evidence rejection, full-table schema validation | one gate refresh plus one confirm rerun if variance invalidates telemetry |
| W1 | CostFacts tests, `gate-json --with-cost-facts`, generated-output diff, full-table maintain | one gate refresh |
| W2 | typed/product tests, generated diff audit, Track 1/2 independence, full-table maintain | one full gate refresh; second rerun requires REDRESS cost note |
| W3 | parser/primitive tests, scalar/checkasm if primitive, generated diff audit, full-table maintain | one full gate refresh; second rerun requires REDRESS cost note |
| W4 | direct guard tests, Track 1/2 independence, full-table maintain | one full gate refresh |
| W5 | Lock 14 grep/audit, generated-output zero-diff, RESULTS zero-diff unless fixing drift | no performance rerun unless source moved |
| W6 | close-honesty checklist and document reconciliation | no performance rerun |

Extra reruns beyond the ceiling are REDRESS cost evidence, not retry room.

### Section 2.1 - Generality And Lock 14 Gate

Every wave has this exit gate, with extra checks when generic crates are
edited:

- Public API scan: no new public JSON-named API appears in generic crates.
- Grammar branch scan: no generic branch selects behavior by JSON grammar name,
  corpus name, object/array role, field name, string role, or layout role.
- Primitive/table scan: no generic primitive, SIMD table, or classifier embeds
  JSON structural policy unless it is generated byte-set data plus opaque class
  ordinals with scalar reference and same-wave consumer.
- Role/fact boundary: generic code may store and search generated structural
  class ordinals or opaque fact ids, but event-role, recovery, layout,
  record-boundary, indentation, and reused-punctuation meaning live only inside
  generated grammar modules keyed by parser state plus class/byte.
- Template/provider boundary: JSON-specific templates/providers remain
  per-grammar surfaces. Generic codegen consumes grammar-derived facts, not
  hard-coded JSON policy under neutral names.
- Non-JSON proof: CSS L4, Sheets, and BBNF-self must compile, lower, cost, or
  run without JSON structural roles for any generic CostFacts, codegen, runtime,
  SIMD, or parser-template edit. Acceptable proof is a named no-op dry run,
  focused test, or unchanged-output audit.

Allowed JSON-specific surfaces are grammar inputs, generated JSON output,
per-grammar providers/templates, tests, and host/API schema facts. The audit
must cover REDRESS 36, 37, and 38 residue clusters and renamed JSON policy.

## Section 3 - W0 Baseline Profile And Telemetry Lock

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v8/research/` using the
  `wave-0-<topic>.md` naming pattern.
- `skinny/REDRESS.md` only if W0 rejects.

Doc links: `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`,
`skinny/RESULTS.md`, `skinny/REDRESS.md`.

Entry gate:

- G-Alpha is closed by the user.
- `skinny/RESULTS.md` is the SK-V7 close baseline.
- W0 plan names the `SK-V8-open` capture method and no-behavior-change proof.

Tasks:

1. Capture the current report as `SK-V8-open`.
2. Add SK-V8 telemetry fields from Section 0.4.
3. Populate hot leaf, profile artifact, run id, host/build metadata, feature
   mask, sample cost, and `SK-V8-open` delta for every current main row.
4. Add sidecar freshness/source validation and reject any same-run sidecar
   claim until a structured sidecar manifest parser exists.
5. Make `gate-json` reject unsupported outcomes, missing required fields, stale
   sidecar strict claims, and strict admission failing Section 0.2.
6. Create the Lock 14 baseline allowlist.

Exit gate:

- All 38 current main rows satisfy Section 0.4.
- Throughput cells stay within +/-1.0% of `SK-V8-open`.
- Every current `parse_only` row reports substrate-guard non-admission (`S`) or
  a preserved hard-failure outcome such as `L`.
- Missing sidecar values have explicit `sidecar_freshness=absent:<reason>`.
- Populated sidecar values are historical non-manifest planning signals with
  source/freshness coverage; W0 admits no sidecar same-run manifest.
- `gate-json` rejects malformed sidecar evidence and any `sidecar-same-run`
  claim without a structured manifest.
- No parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or
  generated parser output change lands.

Same-wave consumer: `gate-json` consumes every emitted telemetry field and
rejects malformed/missing evidence in the same W0 slice.

Pre-blocked routes: all behavior routes, all `skinny/` parser/codegen/runtime
changes, stale sidecars as anchors, row-close claims from schema completion,
and any source edit not required for telemetry/gate/report validation.

Revert protocol: revert report/gate/schema/RESULTS changes as one slice,
restore the opening RESULTS schema, and record a W0 REDRESS rejection naming
the missing profiler, gate, or row.

Downstream effect: W0 rejection blocks W1-W6.

## Section 4 - W1 CostFacts And Comparator Gate Binding

Owner paths:

- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/`
- `skinny/crates/codegen/`
- `skinny/xtask/src/`
- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`,
`skinny/REDRESS.md` item 87.

Entry gate:

- W0 admitted.
- `SK-V8-open` telemetry exists for every current main row.

Tasks:

1. Bind CostFacts rule ids, chosen shape, rejected alternative ids, evidence
   source, wave id, and REDRESS reference into the gate report.
2. Bind comparator id, plane, strictness, freshness, and measured-validation
   path into strict-admission refusal.
3. Make `gate-json --with-cost-facts` reject missing evidence after W1.
4. Keep CostFacts and comparator/report fields grammar-neutral.
5. Keep generated JSON output and parser behavior unchanged unless a separate
   challenged behavior consumer is accepted.

Exit gate:

- Every materialized JSON rule reports chosen shape, rejected alternatives,
  evidence source, REDRESS references, and wave id.
- `gate-json --with-cost-facts` rejects missing CostFacts evidence.
- Strict admission fails closed on plane/strictness/freshness/measured-path
  mismatch.
- Generic CostFacts paths contain no JSON policy.
- Non-JSON proof from Section 2.1 passes.
- Full-table maintain holds within +/-1.0% of `SK-V8-open`.

Same-wave consumer: `gate-json --with-cost-facts` and the strict-admission gate
consume the CostFacts/comparator fields emitted by the report.

Pre-blocked routes: behavior changes, CostFacts-as-performance claims, global
route-fact policy ignoring rejected alternatives, generic JSON policy, generated
output drift, and producer-only CostFacts/telemetry.

Revert protocol: revert CostFacts/report/gate changes together, keep read-only
audit evidence in the wave research artifact, and add REDRESS naming the
missing or non-neutral fact class.

Downstream effect: W1 rejection blocks W2-W6 behavior waves.

## Section 5 - W2 Typed Product Plane Expansion

Owner paths:

- `skinny/crates/codegen/`
- `skinny/crates/bbnf-bench/`
- `skinny/crates/runtime/` only for generated typed runtime consumers named by
  the W2 plan.
- `skinny/RESULTS.md`
- generated real-typed bench outputs named by the W2 plan.
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`,
`skinny/REDRESS.md` items 71 and 81.

Entry gate:

- W0 and W1 admitted.
- W2 plan names exact typed rows, host/API schema facts, owner paths,
  thresholds, Track 1 generated path, Track 2/oracle path, and rollback
  boundaries.
- Selected typed candidates come from the Section 0.5 W2 retained candidate
  list unless a later accepted S-P3 revision expands that list.

Tasks:

1. Add at least two generated typed product rows from explicit host/API schema
   facts, or reject with REDRESS.
2. Preserve all four existing real-typed GO rows and existing direct GO guard
   rows.
3. Prove Track 2/oracle structural independence.
4. Keep direct digest rows as guard rows, not typed product proof.

Exit gate:

- At least two new generated typed rows pass their declared same-plane gate.
- Existing `twitter`, `update_center`, `mesh`, and `marine_ik`
  `real_typed_struct` rows maintain GO and Section 0.5 floors.
- Existing direct GO rows maintain GO.
- Every non-target parse/direct/typed row is no worse than -2.0% Track 1 and
  Track 2 versus `SK-V8-open`, with no correctness or verdict downgrade.
- Track 1 is generated from grammar facts plus explicit host/API schema facts.
- Track 2/oracle does not call generated Track 1, generated SinkOnly,
  generated typed helpers, or a shared benchmark-private parser.
- Lock 14 and non-JSON proof pass if generic code changed.

Same-wave consumer: every new typed row has a generated Track 1 product-plane
consumer and an independent Track 2/oracle proof in the same wave.

Pre-blocked routes: hand typed sinks as product proof, hidden schema directives,
direct digest as typed proof, capacity prescan, Track 2 coupling, retained or
direct routes reopened through typed Vec admission, benchmark-private parser,
and generic JSON schema facts.

Revert protocol: revert row additions, generated outputs, host/API schema facts,
gate changes, RESULTS changes, and bench wiring as one slice unless failed rows
are left explicitly disabled with rejected status. Preserve generated diff audit
and row table in research, and add REDRESS.

Downstream effect: W2 disposition informs W4 direct guard triage.

## Section 6 - W3 Tier A Tape Plus Structural-Projection Union

Owner paths:

W3 is not pre-authorized beyond its plan. The W3 plan must name exact files
before implementation and start from SC-3's Tier A owner/cost table. Expected
owner families include:

- `skinny/crates/bbnf-simd/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/codegen/src/json_templates/`
- generated JSON output named by the plan
- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`,
and `restart/skinny/tranches/sk-v8/research/p3/`.

Entry gate:

- W0 and W1 admitted.
- Fresh W3 plan names one parse candidate, exact owner files, selected rows,
  same-wave production consumer, revert protocol, measured-path proof, Lock 1
  fork, scalar/checkasm requirements, and pre-block differences.
- The exact W3 plan estimates touched source/test LOC, generated LOC,
  gate/report LOC, docs/RESULTS/REDRESS edits, and the revert slice. If the
  estimate exceeds the W3 LOC budget or the 90-minute implementation/redress
  cap, W3 must split before dispatch or return REVISE.
- Challenge accepts that the plan is not a renamed REDRESS 50-55, 60-72,
  82-84, 88, or 89 route.
- W3 either waits for Pass Omega ratification of SC-6-L1-R1 or proves it
  satisfies Lock 1 as written and routes the Omega residual.

Lead hypothesis:

The only S-P2-ready W3 shape is Tier A structural-class cursor migration. It
retains the stage-1 structural index inside one `Tape`, adds scan-written
opaque structural-class ordinals, migrates generated retained JSON Track 1
parsing plus retained view/`ValueRef` to consume that cursor, and deletes
scalar structural rediscovery. Tier A does not claim string-boundary closure,
quote/backslash/parity closure, CostFacts-template parity, non-JSON production
migration, or direct/SinkOnly/path closure.

The structural projection is admissible only as representation replacement
inside the singular retained `Tape`. It fails if it is retained beside the old
offset append path or if a parser-owned cursor/fact slot survives.

Exit gate:

- W3 selects at least two structural-heavy parse rows and crosses the declared
  post-W0 thresholds.
- All 38 current main rows maintain no worse than -2.0% Track 1 and Track 2
  versus `SK-V8-open`, unless the accepted W3 plan sets stricter guards.
- Measured rows prove strict validation, comparator evidence, structural cursor
  work, and admitted tape facts occurred inside the measured row.
- Exactly one retained tape survives; old offset append API and parser-owned
  cursor/fact slots are absent.
- Generated JSON retained parser is the Tier A production consumer.
  `tape_vs_tape`, direct/SinkOnly rows, `path!`, and Track 2 are audit rows or
  residuals, not the Tier A production consumer.
- Scalar oracle and checkasm parity pass before primitive wiring.
- Retained view/`ValueRef` parity and Track 2 independence proof pass.
- Lock 14 and non-JSON proof pass.
- `parse_only` status remains substrate-guard non-admission unless a separate
  schema/gate amendment proves plane-matched strict eligibility.

Same-wave consumer: generated JSON retained Track 1 parsing, plus retained
view/`ValueRef` as touched or proven-untouched per plan. No telemetry-only row
counts.

Pre-blocked routes: new directive, BIR variant, `BackendShape`, `UnionTape`,
public substrate API, sidecar event vector, retained cursor, aux table, density
cache, parser-owned class/fact slot, second source scan, old offset append path,
Tier B string-boundary/parity, CostFacts-template union, `tape_vs_tape` as
consumer, unconditional PMULL/CTZ, object-pair value-byte carry, StringBlock16
tiny probe, single-quartet Unicode classifier, and local string/materialization
families.

Revert protocol: revert runtime/tape, SIMD, codegen templates, generated JSON
output, retained view/value, gate, RESULTS, and REDRESS changes as one slice.
Save the rejected patch under the wave research directory and add REDRESS
naming target rows and guard rows.

Downstream effect: W3 rejection blocks further parse candidates until challenge
accepts a new frame. W4 may proceed only after W3 is admitted, rejected, or
explicitly routed/blocked.

## Section 7 - W4 Direct Guard Triage

Owner paths:

- Exact owner paths named by the W4 plan.
- Likely families: `skinny/crates/codegen/`, generated direct JSON output,
  `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`, and `skinny/REDRESS.md` if
  rejected.

Doc links: `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`,
and `skinny/REDRESS.md` items 54/55 and 66-72.

Entry gate:

- W0 and W1 admitted.
- W2 and W3 have admitted, rejected, or been explicitly routed, or W3 is
  explicitly blocked before W4.
- W4 plan selects one to three `N-direct` rows and names strict direct
  thresholds, Track 1/Track 2 independence proof, owner paths, and residual
  routing.

Tasks:

1. Triage selected direct digest rows under direct guard rules.
2. Preserve Track 1/Track 2 structural independence.
3. Route residual direct rows honestly without presenting digest as product
   plane proof.

Exit gate:

- Every selected row meets Track 1 and Track 2 floors from the W4 plan.
- Correctness parity, sonic-rs strict same-run anchor, and measured validation
  path are present for each selected row.
- Track 2 does not call generated SinkOnly, generated typed helpers, generated
  Track 1, or a shared benchmark-private parser.
- All non-target rows are no worse than -2.0% Track 1 and Track 2 versus
  `SK-V8-open`; existing direct GO and real-typed GO rows maintain GO.
- Lock 14 and non-JSON proof pass if generic code changed.

Same-wave consumer: selected direct rows consume generated Track 1 direct or
SinkOnly work and independent Track 2 proof in the same wave.

Pre-blocked routes: sink-local decoded stats, quote-source streaming hash,
direct source-hook folding, parser-owned scratch, byte-output unescape, semantic
string fact hashing for the current digest workload, raw f64 shortcut, stale
canada mantissa widening, Track 2 coupling, direct cap-16 reruns, and digest as
typed product proof.

Revert protocol: revert behavior changes, generated outputs, bench wiring,
RESULTS, and gate changes as one slice. Keep a direct-triage report that routes
residuals and add REDRESS for failed behavior attempts.

Downstream effect: W4 disposition feeds W5 audit and W6 close.

## Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation

Owner paths:

- `restart/skinny/tranches/sk-v8/research/`
- source paths only if a W5 plan names a small Lock 14 cleanup.
- `skinny/RESULTS.md` only if fixing a recorded drift.
- generated output only if fixing a recorded drift.

Doc links: `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`,
and `skinny/REDRESS.md` items 36-38, 85, and 86.

Entry gate:

- W1-W4 have admitted, rejected, or been explicitly routed.

Tasks:

1. Audit generic crates for JSON policy, renamed JSON residue, and hidden role
   semantics.
2. Reconcile REDRESS 36, 37, and 38 clusters.
3. Prove CSS L4, Sheets, and BBNF-self implications for any generic edit.
4. Fix only named Lock 14 drift if still inside the 90-minute and 150 source
   LOC cleanup cap.

Exit gate:

- No JSON policy enters generic crates.
- Allowed JSON surfaces remain grammar inputs, generated JSON output,
  per-grammar templates/providers, tests, and host/API schema facts.
- Grep/audit covers renamed JSON policy, not only old names.
- Public API scan, grammar-branch scan, primitive/table scan, role/fact
  boundary, template/provider boundary, and non-JSON proof pass.
- Generated JSON output and `skinny/RESULTS.md` have zero behavior drift unless
  W5 explicitly fixed prior routed drift and recorded it.

Same-wave consumer: the audit gate itself, or a named Lock 14 cleanup consumed
by existing codegen/runtime tests in the same W5 slice.

Pre-blocked routes: generic JSON public APIs, grammar-name branches,
`StructuralAlphabet::json`, `skip_json`, `match_json`, `unescape_json`,
`StrictJson`, renamed JSON helpers, generated behavior drift disguised as
audit, and performance claims from cleanup.

Revert protocol: fix drift inside W5 only if in named scope and cap. Otherwise
revert the offending wave slice or mark close blocked with exact owner paths.

Downstream effect: W5 rejection blocks W6 close.

## Section 9 - W6 Close And Alpha Feedback

Owner paths:

- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- a future wave-6 close artifact under `restart/skinny/tranches/sk-v8/research/`
- `skinny/REDRESS.md` only if close reconciliation needs a redress entry.
- `skinny/RESULTS.md` only if reconciling a documented mismatch without source
  behavior change.

Doc links: `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`, `skinny/RESULTS.md`, and
`skinny/REDRESS.md`.

Entry gate:

- W0-W5 each have admitted, rejected, or routed status.
- Their REDRESS/RESULTS/HANDOFF updates are present.

Tasks:

1. Reconcile every wave disposition.
2. Ensure `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
   `restart/skinny/tranches/sk-v8/HANDOFF.md` agree.
3. Route residuals to SK-V9 or Pass Omega.
4. Feed Alpha/S-P3 lessons back into the close note.

Exit gate:

- Every SK-V8 wave has admitted, rejected, or routed status.
- Final row/status artifacts match latest wave evidence and `SK-V8-open`
  deltas.
- Current real-typed GO rows still maintain GO.
- Any W2-added typed row status agrees with W2 REDRESS and RESULTS.
- Any W3/W4 behavior target status agrees across REDRESS, RESULTS, and HANDOFF.
- No accepted source change lacks profile artifact, row threshold, REDRESS id,
  Lock 14 proof, or same-wave consumer proof.
- SC-6-L1-R1 is either ratified, proven under Lock 1 as written, or routed to
  Pass Omega as a residual.

Same-wave consumer: close checklist and document reconciliation.

Pre-blocked routes: paper close, missing REDRESS, missing RESULTS rows,
unresolved Lock 1/Omega fork, strict admission from sidecar/permissive evidence,
PMULL/CTZ/B6 canary as performance evidence, architecture analogy without row
data, and dropping falsifier rows.

Revert protocol: no source revert by default. Reopen the producing wave or mark
close blocked with a mismatch list naming file paths, rows, and missing
evidence.

## Section 10 - Pre-Blocked Routes

Every wave inherits this route ledger. A route may reopen only with fresh W0
evidence, same-wave consumer, scalar/checkasm where relevant, no-regression
gate, REDRESS citation, and challenge acceptance.

Global blocks:

- New directive, BIR variant, substrate surface, `BackendShape`, `UnionTape`,
  public substrate API, parser-owned cursor/facts, sidecar substrate, and
  parallel substrate.
- Generic JSON policy in generic crates, including renamed helper policy.
- Sidecar/permissive/lossy/stale comparator evidence as strict admission.
- `tape_vs_tape`, `parse_only`, or telemetry rows as W3 production consumer.
- Orphan primitives, checkasm-only admission, and harness-only hardening as
  performance proof.
- Track 1/Track 2 coupling or benchmark-private parsers.
- Automatic implementation dispatch.

Specific REDRESS and Alpha blocks:

- REDRESS 16, 17, 18, 25: pair-token fusion, function-pointer dispatch,
  skipless/12-byte width churn, separator/generic alternates as-is.
- REDRESS 28+33: Class A NEON/TBL tiny-string wiring as parse close.
- REDRESS 36-38, 85-86: Lock 14 residue, old JSON helpers, generic JSON
  branches, and `StructuralAlphabet::json`.
- REDRESS 49-55: no-allocation visitor, parse-time aux side tables,
  EventCursor, parser-local structural-mask cursor, decoded stats sink, and
  quote-source fused string materializer.
- REDRESS 59-65, 72/83: retained string-boundary collapse, always-wide or
  delayed-wide scanning, Unicode validator/classifier retries, object/key carry,
  global/direct/Track 2 cap-16, generated-retained StringBlock16 tiny probe.
- REDRESS 66-72, 80: direct source-hook/materialization families, parser-owned
  scratch, byte-output unescape, semantic string facts, hand typed sinks as
  proof, stale mantissa widening, and raw f64 shortcut.
- REDRESS 74-79, 81, 87: architecture/comparator/CostFacts evidence can be
  cited only under their admitted boundaries; they do not authorize behavior by
  analogy.
- REDRESS 82-84: single-quartet Unicode classifier, StringBlock16 tiny probe,
  object-pair value-byte control compaction.
- REDRESS 88-90: PMULL prefix-XOR default hot body, CTZ/bulk production
  consumer, and B6 canary hardening as performance evidence.
- Alpha-E bitmap density-gated route remains reserve research only; it is not
  in W0-W6 unless a future plan challenges it.
- Tier B string-boundary / quote-backslash-parity / CostFacts-template union is
  blocked from W3 Tier A by default.

## Section 11 - G-Alpha And Dispatch Scope

G-Alpha closed on 2026-05-18T05:26:48Z with limited dispatch scope:

- W0 is authorized.
- W1-W6 remain conditional. They require W0 closure, a wave plan with exact
  owner paths and gates, required challenge acceptance, and orchestrator/user
  dispatch before redress.

No W3 implementation dispatches from S-P2 or S-P3 alone.
