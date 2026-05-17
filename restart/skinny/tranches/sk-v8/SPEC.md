# Implementation Packet SK-V8

Date: 2026-05-17.

Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Authority:

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`.
- Alpha cohort under `restart/skinny/tranches/sk-v8/research/alpha/`.
- Alpha hardening V1 under
  `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/`.
- `skinny/RESULTS.md` and `skinny/REDRESS.md`.
- `restart/prompts/pass-contracts/PASS-ALPHA.md`.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

## Section 0 - Close Condition And Goalset

SK-V8 does not start from a green SK-V7 performance close. It starts from a
measured `N-direct / NoGo` report with incomplete hot-leaf and delta telemetry.
The first close condition is therefore observability, not a speculative parser
rewrite.

### Section 0.1 - Global Close Condition

SK-V8 closes only when all of these are true:

1. W0 creates a checked `SK-V8-open` baseline with no placeholder hot leaves.
2. Every main row has a profile artifact path, run id, host/build metadata,
   cycles-per-byte or equivalent sample cost, and delta versus `SK-V8-open`.
3. `gate-json` rejects rows missing required SK-V8 telemetry after W0.
4. CostFacts chosen/rejected route evidence is consumed by the gate before any
   behavior wave can admit route quality.
5. All four current `real_typed_struct` GO rows maintain GO.
6. Any new typed product row uses generated Track 1 plus a structurally
   independent Track 2 or oracle.
7. Any parse/direct behavior wave either meets its named row threshold and
   full-table maintain gate or is rejected with REDRESS evidence.
8. No pre-blocked route is reopened without fresh profile evidence, same-wave
   consumer, and challenge acceptance.
9. Lock 14 and Lock 15 gates pass at every wave close.
10. `RESULTS.md`, `REDRESS.md`, and `HANDOFF.md` agree at close.

### Section 0.2 - Comparator Classes

SK-V8 uses three comparator classes:

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict, serde_json | May support strict admission if output plane matches. |
| Same-run flaw probe | sonic-rs lossy, permissive rows | Planning only; never strict admission. |
| Sidecar planning signal | simdjson, yyjson, RapidJSON, asmjson unless refreshed under same-run rules | Planning only until freshness and output-plane rules are satisfied. |

Direct and typed workload rows compare only to sonic-rs strict and serde_json
unless W0 or a later telemetry wave adds same-run C++ product-plane evidence.

### Section 0.3 - Outcome Enum

SK-V8 extends the current schema-v3 outcome enum to match existing
`skinny/RESULTS.md` rows:

```text
A
C
G
K
L
N-direct
```

`K` and `N-direct` are current, valid outcomes. `gate-json` must reject any
other outcome after W0 unless the enum is deliberately amended in REDRESS and
the SPEC.

### Section 0.4 - Required Telemetry

The existing schema-v3 surface remains required. SK-V8 adds required fields
after W0:

```text
grammar_id
domain
comparator_id
comparator_plane
comparator_strictness
Profile artifact
Cycles per byte
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
```

Every emitted field must be consumed by `gate-json` in the same wave. Emitting
a profile path, sidecar manifest, CostFacts id, or freshness field without
validation is a producer-only artifact and fails the wave.

`gate-json` is the JSON instance of a grammar-aware report contract. The
rendered JSON table may keep JSON-specific comparator columns, but internal
telemetry must model comparator evidence as `(grammar_id, domain,
comparator_id, comparator_plane, comparator_strictness, run_id,
sidecar_freshness)`. Non-JSON domains require their own strict anchors or an
explicit absence-of-strict-comparator reason.

### Section 0.5 - Opening Row Goalset

W0 has the only dispatchable per-row target before G-Alpha: every current row
must become telemetry-bound and maintain throughput within the telemetry-only
budget. Later behavior targets are provisional until W0 closes.

| Row | Current state | W0 target | Later posture |
|---|---|---|---|
| twitter parse_only | K/NO-GO, 15752 T1, 12285 T2, 21020 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate parse residual only after W0 names hot leaf. |
| citm_catalog parse_only | K/NO-GO, 31784 T1, 20817 T2, 25509 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Diagnose why faster-than-sonic row remains K. |
| canada parse_only | K/NO-GO, 17765 T1, 17070 T2, 13885 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row for any parser change. |
| apache_builds parse_only | K/NO-GO, 12482 T1, 12151 T2, 17381 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate only if W0 names a non-blocked owner. |
| github_events parse_only | K/NO-GO, 15198 T1, 13046 T2, 23034 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate only if W0 names a non-blocked owner. |
| update_center parse_only | K/NO-GO, 11193 T1, 9227 T2, 19684 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate only if W0 names a non-blocked owner. |
| mesh parse_only | K/NO-GO, 14265 T1, 13287 T2, 11754 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row for numeric/bitmap changes. |
| random parse_only | K/NO-GO, 9838 T1, 7804 T2, 15457 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate only after W0. |
| gsoc-2018 parse_only | K/NO-GO, 23026 T1, 21881 T2, 49292 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate only after W0. |
| marine_ik parse_only | K/NO-GO, 13797 T1, 12384 T2, 10070 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row for typed/numeric changes. |
| instruments parse_only | K/NO-GO, 18038 T1, 11678 T2, 16312 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row for control/key changes. |
| numbers parse_only | K/NO-GO, 20609 T1, 18514 T2, 13626 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row for numeric/bitmap changes. |
| unicode_mixed parse_only | K/NO-GO, 8035 T1, 7698 T2, 16180 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Do not reopen W4/W5 shape without new evidence. |
| unicode_escapes parse_only | K/NO-GO, 12042 T1, 11146 T2, 18415 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Do not reopen REDRESS 82 as-is. |
| unicode_basic parse_only | K/NO-GO, 11416 T1, 10653 T2, 15596 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Candidate only after W0. |
| distinct_values parse_only | K/NO-GO, 6655 T1, 5633 T2, 17148 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Do not reopen W5 shape without new evidence. |
| y_string_unicode parse_only | K/NO-GO, 6216 T1, 6038 T2, 13537 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Do not reopen REDRESS 82 as-is. |
| twitter direct_to_struct | N-direct/NO-GO, 11832 T1, 10986 T2, 14885 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row; not typed product proof. |
| citm_catalog direct_to_struct | A/GO, 21438 T1, 20280 T2, 19966 sonic | Profile-bound; maintain GO | Guard row. |
| canada direct_to_struct | N-direct/NO-GO, 10773 T1, 10296 T2, 12421 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row; no stale EL fallback assumption. |
| apache_builds direct_to_struct | A/GO, 11116 T1, 10187 T2, 11122 sonic | Profile-bound; maintain GO | Guard row. |
| github_events direct_to_struct | N-direct/NO-GO, 12270 T1, 11366 T2, 16041 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| update_center direct_to_struct | N-direct/NO-GO, 8401 T1, 7667 T2, 11081 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| mesh direct_to_struct | A/GO, 8259 T1, 8483 T2, 8789 sonic | Profile-bound; maintain GO | Guard row. |
| random direct_to_struct | N-direct/NO-GO, 7727 T1, 7123 T2, 8936 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| gsoc-2018 direct_to_struct | N-direct/NO-GO, 15097 T1, 14306 T2, 23407 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| marine_ik direct_to_struct | A/GO, 8943 T1, 9151 T2, 8147 sonic | Profile-bound; maintain GO | Guard row. |
| instruments direct_to_struct | N-direct/NO-GO, 11972 T1, 11086 T2, 12673 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| numbers direct_to_struct | A/GO, 12615 T1, 12362 T2, 12838 sonic | Profile-bound; maintain GO | Guard row. |
| unicode_mixed direct_to_struct | N-direct/NO-GO, 4579 T1, 4431 T2, 9679 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| unicode_escapes direct_to_struct | N-direct/NO-GO, 4866 T1, 4973 T2, 14028 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| unicode_basic direct_to_struct | A/GO, 8576 T1, 8059 T2, 8502 sonic | Profile-bound; maintain GO | Guard row. |
| distinct_values direct_to_struct | N-direct/NO-GO, 6105 T1, 5362 T2, 11344 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| y_string_unicode direct_to_struct | N-direct/NO-GO, 5029 T1, 3766 T2, 9019 sonic | Profile-bound; no throughput move beyond +/-1.0 percent | Guard row. |
| twitter real_typed_struct | A/GO, 18513 T1, 16193 T2, 15486 sonic | Profile-bound; maintain GO | Product-plane guard. |
| update_center real_typed_struct | A/GO, 11879 T1, 10451 T2, 12627 sonic | Profile-bound; maintain GO | Product-plane guard. |
| mesh real_typed_struct | A/GO, 9466 T1, 8089 T2, 8696 sonic | Profile-bound; maintain GO | Product-plane guard. |
| marine_ik real_typed_struct | A/GO, 12020 T1, 9630 T2, 8750 sonic | Profile-bound; maintain GO | Product-plane guard. |

## Section 1 - Non-Negotiables

- No new BBNF directives.
- No new BIR variant.
- No new substrate without a same-wave consumer.
- No JSON policy in generic crates.
- Scalar reference and checkasm before any primitive wiring.
- Strict-vs-strict only for strict admission.
- Sidecar/permissive rows are flaw probes or planning signals.
- Research, plan, and redress remain distinct commits per wave.
- Every miss becomes REDRESS evidence or an explicit routed residual.
- No SK-V8 implementation wave dispatches before G-Alpha.
- After G-Alpha, only W0 dispatches from this packet.

## Section 2 - Wave Manifest

| Wave | Section | Name | Dispatch status | Hard cap |
|---|---|---|---|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | Dispatchable after G-Alpha | 180 min |
| W1 | Section 4 | CostFacts Gate Binding | Conditional on W0 close | 240 min |
| W2 | Section 5 | Typed Product Plane Expansion | Conditional on W0/W1 plan update | 300 min |
| W3 | Section 6 | Profile-Selected Parse Candidate | Conditional on W0/W1 challenge | 300 min |
| W4 | Section 7 | Direct Guard Triage | Conditional on W0/W1 plan update | 240 min |
| W5 | Section 8 | Grammar-Neutral Audit And Lock 14 Preservation | Conditional on W1-W4 close | 180 min |
| W6 | Section 9 | Close And Alpha Feedback | Conditional on all prior dispositions | 120 min |

Default phase caps per wave:

| Phase | Cap |
|---|---:|
| Research | 30 min per agent, max 6 agents |
| Plan | 30 min |
| Redress | Wave-specific remainder |
| Challenge | 90 min when required |

Hard caps are inclusive wall-clock budgets. Research, plan, implementation,
verification, generated-output review, RESULTS refresh, REDRESS, and doc update
all count against the wave hard cap. Parallel research agents count by elapsed
wall-clock, not summed agent-minutes.

Source LOC caps:

| Wave | Source LOC cap | Generated / RESULTS cost |
|---|---:|---|
| W0 | 350 | Outside source LOC cap, inside review and verification budget |
| W1 | 300 | Outside source LOC cap, inside review and verification budget |
| W2 | 650 | Generated output outside source LOC cap but byte-diff audited |
| W3 | 450 default, 650 only if template parity is in the W3 plan | Generated output outside source LOC cap but byte-diff audited |
| W4 | 300 | RESULTS refresh inside verification budget |
| W5 | 0 by default, 150 only for a named Lock 14 fix | Zero generated drift unless fixing routed drift |
| W6 | 0 source LOC except docs/REDRESS/HANDOFF/SPEC updates | RESULTS/REDRESS reconciliation inside close budget |

Verification and rerun ceilings:

| Wave | Focused verification | Full bench / gate rerun ceiling |
|---|---|---|
| W0 | report/gate tests, malformed-manifest rejection, full-table schema validation, `git diff --check` | one gate refresh plus one confirm rerun if variance invalidates telemetry |
| W1 | CostFacts unit tests, gate-json with CostFacts, generated-output diff check, full-table maintain check | one gate refresh |
| W2 | typed/product tests named by plan, generated-output diff audit, Track 1/2 independence proof, full-table maintain check | one full gate refresh; second rerun requires REDRESS cost note |
| W3 | parser/primitive tests named by plan, scalar/checkasm if primitive, generated-output diff audit, full-table maintain check | one full gate refresh; second rerun requires REDRESS cost note |
| W4 | direct guard tests named by plan, Track 1/2 independence proof, full-table maintain check | one full gate refresh |
| W5 | Lock 14 grep/audit, generated-output zero-diff, RESULTS zero-diff unless fixing routed drift | no performance rerun unless a fix moved source |
| W6 | close-honesty checklist, doc checks, RESULTS/REDRESS/HANDOFF reconciliation | no performance rerun |

Extra reruns beyond the ceiling are not retry room. They become REDRESS cost
evidence or require plan augmentation.

## Section 2.1 - Generality And Lock 14 Gate

Every wave has this exit gate, with extra checks when generic crates are edited:

- Public API scan: no new public JSON-named API appears in generic crates.
- Grammar branch scan: no generic branch selects behavior by JSON grammar name,
  JSON corpus name, JSON object/array role, or JSON field name.
- Primitive/table scan: no generic primitive, SIMD table, or classifier embeds
  JSON structural policy unless it is a grammar-neutral byte-set primitive with
  a scalar reference and same-wave consumer.
- Template/provider boundary: JSON-specific templates/providers remain
  per-grammar surfaces. Generic codegen may consume grammar-derived facts, not
  hard-coded JSON policy under neutral names.
- Non-JSON proof: generic CostFacts, codegen, runtime, SIMD, or
  parser-template edits must prove CSS L4, Sheets, and BBNF-self do not require
  JSON structural roles to compile, lower, cost, or run. Acceptable proof is a
  no-op dry run, focused test, or explicit unchanged-output audit named by the
  plan.

W0 also creates the Lock 14 baseline allowlist. Allowed JSON-specific surfaces
are grammar inputs, generated JSON output, per-grammar providers/templates,
tests, and host/API schema facts. The audit scope includes REDRESS 36, 37, and
38 concerns: generic-crate JSON residue, detached scanner surfaces, JSON
structural alphabets, JSON binding helpers, and public `Json*` generic APIs.

## Section 3 - W0 Baseline Profile And Telemetry Lock

### Owner paths

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-*.md`
- `skinny/REDRESS.md` only if W0 rejects.

### Tasks

1. Capture the current `skinny/RESULTS.md` as `SK-V8-open`.
2. Add SK-V8 telemetry fields from Section 0.4.
3. Populate hot leaf, profile artifact, run id, host/build metadata, feature
   mask, and c/B or equivalent sample cost for every current main row.
4. Add sidecar freshness validation and at least one malformed-manifest test.
5. Make `gate-json` reject placeholder hot leaves, missing profile artifacts,
   missing `SK-V8-open` deltas, missing run ids, and unsupported outcomes.
6. Refresh `skinny/RESULTS.md` only through the checked gate.

### Exit gate

- All 38 current main rows satisfy Section 0.4.
- Missing sidecar values are allowed only when the row records an explicit
  `sidecar_freshness=absent:<reason>` non-admission value; populated sidecar
  cells require manifest coverage.
- W0 creates the Lock 14 baseline allowlist from Section 2.1.
- Throughput cells move no more than +/-1.0 percent versus `SK-V8-open`.
- `gate-json` rejects one intentionally malformed sidecar manifest.
- No parser, scanner, SIMD, asm, codegen behavior, or product-plane behavior
  changes land.
- W0 produces a research artifact naming the profile command, artifact paths,
  host, build flags, and row coverage.

### Revert protocol

If any required telemetry field cannot be populated or validated, revert
report/gate/schema changes together, restore the opening RESULTS schema, commit
a W0 redress rejection, and record the missing profiler or bench path in
`skinny/REDRESS.md`.

### Downstream effect

W0 rejection blocks W1-W6.

## Section 4 - W1 CostFacts Gate Binding

### Owner paths

- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/`
- `skinny/crates/codegen/`
- `skinny/xtask/src/`
- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected.

### Entry gate

- W0 is admitted.
- `SK-V8-open` telemetry exists for every current main row.

### Tasks

1. Bind CostFacts rule ids, chosen shape, rejected alternatives, evidence
   source, wave id, and REDRESS reference into the gate report.
2. Make `gate-json --with-cost-facts` reject missing evidence after W1.
3. Keep CostFacts types grammar-neutral.
4. Do not change parser behavior or generated JSON output unless the W1 plan
   explicitly names a behavior consumer and challenge accepts it.

### Exit gate

- Every materialized JSON rule reports chosen shape, rejected alternatives,
  evidence source, REDRESS references, and wave id.
- `gate-json --with-cost-facts` rejects missing evidence.
- Generic CostFacts paths contain no JSON policy.
- Generic CostFacts/report fields use the grammar-aware comparator fields from
  Section 0.4.
- Non-JSON proof from Section 2.1 passes for CostFacts, codegen, and report
  edits.
- Full-table throughput maintain holds within +/-1.0 percent unless a W1 plan
  names a stricter no-throughput-change gate.

### Revert protocol

Revert CostFacts report/gate changes together. Keep read-only audit evidence in
the wave research artifact. Record a REDRESS entry naming the missing fact
class. W1 rejection blocks behavior waves.

## Section 5 - W2 Typed Product Plane Expansion

### Dispatch status

Conditional. W2 may not dispatch until W0 and W1 close and a W2 plan update
names exact typed rows, host/API schema facts, owner paths, and row thresholds.

### Allowed owner path families

- `skinny/crates/codegen/`
- `skinny/crates/bbnf-bench/`
- `skinny/crates/runtime/` only for generated typed runtime consumers named in
  the W2 plan.
- `skinny/RESULTS.md`
- generated real-typed bench outputs named in the W2 plan.

### Exit gate

- Existing `twitter`, `update_center`, `mesh`, and `marine_ik`
  `real_typed_struct` rows maintain GO.
- Existing `direct_to_struct` GO rows maintain GO.
- All 38 current main rows compare against `SK-V8-open`.
- Non-target parse/direct rows stay within the W2 plan budget; any row outside
  budget rejects W2 with REDRESS evidence.
- W2 plan names the full-table maintain budget before implementation.
- At least two new generated typed rows pass their declared same-plane gate, or
  W2 is rejected with REDRESS evidence.
- Track 1 is generated from grammar facts plus explicit host/API schema facts.
- Track 2 or oracle is structurally independent and does not call generated
  Track 1, generated SinkOnly, generated typed helpers, or a shared
  benchmark-private parser.
- Direct digest rows do not count as typed product proof.

### Revert protocol

Revert row additions or leave them disabled only with explicit rejected status.
Restore generated outputs if behavior changed. Preserve generated diff audit
and row table in research, and add REDRESS.

## Section 6 - W3 Profile-Selected Parse Candidate

### Dispatch status

Conditional. W3 may not dispatch until W0 and W1 close, then a fresh plan names
one parse candidate, exact owner paths, exact rows, and challenge proves it is
not a renamed REDRESS 82, 83, 84, 88, or 89 route.

### Owner paths

Not pre-authorized beyond the W3 plan. The W3 plan must name exact files before
implementation. If paths under `scan.rs`, parser templates, `bbnf-simd`, or
parse-that scanners are included, the plan must prove the change is consumed in
the same parser/tape or SinkOnly loop.

### Exit gate

- Selected parse rows cross the declared threshold.
- All 38 current main rows respect the W3 maintain budget; default is no Track
  1 or Track 2 regression worse than -2.0 percent.
- Generic-code edits pass the Section 2.1 generality and Lock 14 gate.
- Any primitive has scalar reference and checkasm parity before wiring.
- Any source-byte second scan, retained cursor, aux table, density cache,
  sidecar event vector, or parser-owned structural projection fails Lock 1.

### Revert protocol

Revert runtime/template/generated/gate/RESULTS changes as one slice. Save the
rejected patch under the wave research directory. Add REDRESS naming target and
guard rows. Rejection blocks further parse candidates until challenge accepts a
new frame.

## Section 7 - W4 Direct Guard Triage

### Dispatch status

Conditional. W4 may not dispatch until W0 and W1 close and the W4 plan names
selected direct guard rows and exact owner paths.

### Exit gate

- Selected `N-direct` rows either close under digest guard rules or are routed
  as guard residuals.
- No direct digest result is presented as product-plane SOTA proof.
- Track 1 and Track 2 remain structurally independent.
- Full-table maintain holds under the W4 plan budget.
- Generic-code edits pass the Section 2.1 generality and Lock 14 gate.

### Revert protocol

Revert behavior changes. Keep a triage report that routes residuals. Add
REDRESS if a behavior candidate was attempted and failed.

## Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation

### Owner paths

- `restart/skinny/tranches/sk-v8/research/`
- source paths only if a W5 plan names a small cleanup.

### Entry gate

- W1-W4 have admitted, rejected, or been explicitly routed.

### Exit gate

- No JSON policy enters generic crates.
- Allowed JSON surfaces are grammar inputs, generated JSON output,
  per-grammar templates/providers, tests, and host/API schema facts.
- Grep/audit covers renamed JSON policy, not only old symbol names.
- Audit covers REDRESS 36, 37, and 38 Lock 14 residue clusters.
- CSS L4, Sheets, and BBNF-self implications from Section 2.1 are reconciled.
- Generated JSON output and `skinny/RESULTS.md` have zero behavior drift unless
  W5 explicitly fixed a prior wave and recorded it.

### Revert protocol

Fix drift inside W5 if in bounds. Otherwise revert the offending wave slice or
mark close blocked with a named owner. W5 rejection blocks W6 close.

## Section 9 - W6 Close And Alpha Feedback

### Owner paths

- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/wave-6-close.md`
- `skinny/REDRESS.md` only if close reconciliation needs a redress entry.

### Exit gate

- Every SK-V8 wave has admitted, rejected, or routed status.
- `RESULTS.md`, `REDRESS.md`, and `HANDOFF.md` agree.
- No open brittleness window remains.
- Residuals name SK-V9 or Pass Omega destinations.

### Revert protocol

No source revert by default. Reopen the producing wave or mark close blocked
with a close-honesty mismatch list.

## Section 10 - Pre-Blocked Routes

Do not reopen these without fresh W0 evidence, same-wave consumer, scalar
reference/checkasm where relevant, no-regression gate, REDRESS citation, and
challenge acceptance:

- REDRESS 28+33: Class A NEON tiny-string wiring as parse close.
- REDRESS 50-55: SK-V5 UTF-8 fusion routes.
- REDRESS 60-72: SK-V6 retained-parse and direct-materialization rejected
  families, including sidecar producers and digest/Track 2 cap-16 routes.
- REDRESS 80: stale canada mantissa-widen/fallback-elimination assumption.
- REDRESS 82: single-quartet Unicode escape classifier.
- REDRESS 83: generated-retained StringBlock16 tiny probe.
- REDRESS 84: object-pair value-byte control compaction.
- REDRESS 88: PMULL prefix-XOR as default hot production body.
- REDRESS 89: CSSC CTZ next-bit plus bulk production consumer.
- Historical blocked routes in `skinny/REDRESS.md`: function-pointer dispatch,
  pair-token fusion, 12-byte token churn, separator elision, generic SWAR
  whitespace, capacity prescan, EventCursor or other sidecar prepass, raw f64
  shortcut, and orphan primitive admission.

## Section 11 - G-Alpha

This packet is suitable for G-Alpha review with a limited dispatch scope:

- `G-Alpha closed` authorizes W0 only.
- W1-W6 remain conditional and require W0 closure plus plan augmentation before
  dispatch.
- `G-Alpha revise` returns to Alpha hardening with named revisions.
