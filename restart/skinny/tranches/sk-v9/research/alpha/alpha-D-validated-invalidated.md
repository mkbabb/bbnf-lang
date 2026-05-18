# SK-V9 Alpha-D: SK-V8 Validated / Invalidated Ledger

Date: 2026-05-18.

Scope: PASS-ALPHA SK-V8 -> SK-V9 alpha-D. This artifact updates the
validated, invalidated, demoted, and still-open ledger for the completed SK-V8
cycle. It does not dispatch SK-V9 implementation work.

Sources read:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-reconciliation-research.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
- `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- recent git log through SK-V8 close head `32870fea`

## 1. Current Measured Authority

- SK-V8 close head for this ledger: `32870fea`
  (`docs(sk-v8-wave6-close): close SK-V8 after V2 convergence`).
- Current measured authority remains the W0-rendered `skinny/RESULTS.md`
  (`skinny/RESULTS.md:3-42`;
  `restart/skinny/tranches/sk-v8/HANDOFF.md:250-260`).
- Overall outcome remains `N-direct / NoGo`.
- Main-row state is unchanged by W1-W6:
  - `parse_only`: 16 `S / NO-GO`, 1 `L / NO-GO`.
  - `direct_to_struct`: 3 `A / GO`, 14 `N-direct / NO-GO`.
  - `real_typed_struct`: 4 `A / GO`.
- The four measured real-typed `A / GO` rows remain `twitter`,
  `update_center`, `mesh`, and `marine_ik` (`skinny/RESULTS.md:7`,
  `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).
- W2's `apache_builds/real_typed_struct` and
  `citm_catalog/real_typed_struct` are source/product parity rows only; they
  are not measured `skinny/RESULTS.md` rows in SK-V8
  (`skinny/REDRESS.md:2620-2659`).
- Every current main row still records `Strictness=deferred` and
  `parse_utf8=view-boundary`; parse rows are substrate-guard non-admission
  telemetry, not strict SOTA admissions (`skinny/RESULTS.md:3-42`).

Primary close authorities:

- W0: `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`
- W1: commit `c6345e4d` plus `restart/skinny/tranches/sk-v8/HANDOFF.md`
- W2: `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V5/HARDENING-W2-V5-CONSOLIDATED.md`; REDRESS 91
- W3: `restart/skinny/tranches/sk-v8/research/wave-3-hardening/V1/HARDENING-W3-V1-CONSOLIDATED.md`; REDRESS 92
- W4: `restart/skinny/tranches/sk-v8/research/wave-4-hardening/V4/HARDENING-W4-V4-CONSOLIDATED.md`; REDRESS 93
- W5: `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md`
- W6: `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`

## 2. Validated / Load-Bearing Wins

### V1. W0 telemetry/report gate is load-bearing

- Commits: research `cc54ff9a`, plan `0bd16f6d`, implementation/folds
  `6d8cb701`, `cb0fdba0`, `61d5d304`, `077aadad`, `0c49fabd`,
  `6c0bc15d`, `f452e837`, `00c3485a`, `3a9fa326`, `61d5cc3b`, hardening close
  `b34dbeb8`, `826af889`, status fold `27aea746`.
- Authority:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`.
- Result: W0 established `SK-V8-open` as a consumed gate/report artifact with
  38 manifest rows, frozen run ids, row identity/outcome/verdict pinning,
  profile artifact strings, sample-cost metadata, host/build/feature facts,
  comparator freshness, sidecar refusal, substrate tuple fields, and Lock 14
  baseline validation.
- Validation: V11 accepted 6/6 and V12 accepted the unchanged target 6/6 with
  minimum confidence 96. V12 records 38 manifest rows, 38 `gate_only` rows, 38
  `SK-V8-open` rows, and 38 frozen-run-id rows.
- Boundary: W0 is telemetry/report/gate only. It admits no parser, scanner,
  SIMD, asm, codegen behavior, product-plane behavior, generated output, or
  row-performance movement.

### V2. W1 CostFacts and strict comparator binding are gate-consumed

- Commits: research `1455b8d3`, plan `78796bca`, Lock 14 fold `7e490271`,
  redress `c6345e4d`, status fold `07923600`.
- Authority: commit `c6345e4d`; W1 closure record in
  `restart/skinny/tranches/sk-v8/HANDOFF.md`.
- Result: `cargo xtask gate-json --with-cost-facts` now emits and validates a
  `sk-v8-costfacts-v1` manifest with 15 materialized JSON rules, chosen shapes,
  rejected alternatives, evidence sources, REDRESS references, and
  `SK-V8-W1` wave ids. Strict admission binds `comparator_id` and rejects
  lossy, sidecar, stale, unknown, and plane-mismatched evidence as strict
  anchors.
- Validation: W1 recorded zero gate-level
  `BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostics while preserving producer
  diagnostics for audit visibility. Generated/parser/product surfaces and
  `skinny/RESULTS.md` stayed unchanged.
- Boundary: CostFacts are route-quality evidence and rejected-alternative
  accounting, not a performance claim and not permission to reopen rejected
  routes by bookkeeping.

### V3. W2 typed source/product expansion validated Apache and CITM as source rows

- Commits: research `eacba76a`, plan `9923b804`, Canada routing
  `6b4f46ae`, source admit `12aff1e4`, hardening folds `8ce03af4` and
  `74fe4e1b`, close `bf2f073d` and `ab106386`.
- Authority:
  `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V5/HARDENING-W2-V5-CONSOLIDATED.md`;
  REDRESS 91.
- Result: generated real typed source/product rows were added for
  `apache_builds` and `citm_catalog` through the existing typed DirectBuild
  schema path. The rows use generated Track 1 product-plane consumers,
  serde_json as the Track 2/oracle path, a separate sonic-rs parity lane,
  checksums, and full-fixture parity tests.
- Validation: `12aff1e4` added the source/product slice. `8ce03af4` folded
  Lock 14 parent-diff and source/product wording. `74fe4e1b` fixed the report
  gate so source-only typed fixtures do not require unadmitted Criterion
  metadata rows. V4 and V5 hardening accepted the folded disposition.
- Boundary: this validates source/product parity only. It does not admit
  Apache/CITM as measured `RESULTS.md` rows and does not claim six
  `real_typed_struct A / GO` rows in SK-V8.

### V4. W5 Lock 14 provider-boundary cleanup landed

- Commits: research `4ff53f6f`, plan `a311d643`, cleanup `6e159f5c`,
  hardening folds `b71a8aed`, `181202f0`, `d3398a68`, V4 accept
  `42d5f034`, close `e51816c6`.
- Authority:
  `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md`.
- Result: V1 hardening found live provider-boundary residue in
  `skinny/crates/codegen/src/lib.rs`. `6e159f5c` moved JSON profile guard and
  JSON template/runtime provider material to
  `skinny/crates/codegen/src/json_provider.rs`, added the
  `per_grammar_provider` Lock 14 class, and limited parent-diff authorization
  to the named W5 owner paths.
- Validation: V4 and V5 accepted 6/6 with minimum confidence 95. Lock 14
  baseline, generated-output checks, conformance, package tests, root
  `cargo xtask regen --check`, forbidden-policy scans, grammar-branch scans,
  and provider-residency scans were clean.
- Boundary: W5 admits only the named cleanup. It makes no performance claim,
  refreshes no row table, and leaves generated output and `skinny/RESULTS.md`
  unchanged.

### V5. W6 close reconciliation is honest and bounded

- Commits: research `1ed31cea`, plan `d936205d`, V1 accept `e500ad00`,
  close `32870fea`.
- Authority:
  `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`.
- Result: W6 reconciled W0-W5 against `skinny/RESULTS.md`,
  `skinny/REDRESS.md`, and `restart/skinny/tranches/sk-v8/HANDOFF.md`.
- Validation: V1 accepted 6/6 with minimum confidence 96 and V2 re-challenged
  the unchanged packet with 6/6 ACCEPT, minimum confidence 96.
- Boundary: W6 admits no source, generated-output, benchmark-row,
  `skinny/RESULTS.md`, or `skinny/REDRESS.md` change. Closing SK-V8 authorizes
  only SK-V9 planning through Pass Alpha and the skinny pass substrate, not an
  implementation wave.

## 3. Invalidated / Rejected SK-V8 Candidates

### I1. W2 measured typed row-table admission was rejected for SK-V8

- Commits: `12aff1e4`, `8ce03af4`, `74fe4e1b`, `ab106386`.
- Redress item: 91.
- Falsifier: `canada/real_typed_struct` failed full-fixture generated
  DirectBuild versus serde checksum parity on long decimal coordinate payloads.
  Apache/CITM source rows were valid, but the local Criterion report path was
  already blocked by W0 run-id metadata drift unrelated to W2 source.
- Outcome: source/product parity admitted; benchmark row-table admission
  rejected/routed. `skinny/RESULTS.md` remains at four measured real-typed rows.

### I2. W3 Tier A storage-swap implementation was rejected before source redress

- Commits: research `9deb2aed`, plan `fc91c217`, redress `a9d0d69a`.
- Redress item: 92.
- Falsifier: the scanner structural index and retained tape are not isomorphic.
  The scanner records structural punctuation plus real quotes; the retained
  tape records generated parser events: container opens/closes, opening quotes,
  number starts, and literal starts. Retained `ValueRef` traversal depends on
  that event stream.
- Outcome: no source patch, no rejected patch artifact, no row-table
  admission. The bounded W3 representation-replacement implementation is
  rejected/routed for SK-V8 because the necessary retained class/event grammar
  and cursor-contract proof exceed W3 scope and caps.

### I3. W4 Track 2 scalar-parent fold was rejected after selected-row falsification

- Commits: research `bdd4473c`, plan `5b79d04a`, V1 hardening
  `643ca5fc`, redress `a88e9725`, V3 accept `53aecc20`, close `b6da4754`.
- Redress item: 93.
- Falsifier: the selected rows were `apache_builds/direct_to_struct`,
  `numbers/direct_to_struct`, and `random/direct_to_struct`. The candidate
  improved Apache enough to clear sonic/1.10, but random remained below
  sonic/1.10 and numbers regressed by +6.3287% Track 2 time in Criterion.
- Outcome: the patch was reverted and saved at
  `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`. No Lock 14 allowance was
  added and `skinny/RESULTS.md` remains unchanged.

### I4. W5 no-source close was invalidated, but the named cleanup was admitted

- Commits: `4ff53f6f`, `a311d643`, `6e159f5c`, `e51816c6`.
- Falsifier: W5 V1 hardening found JSON provider material still living in
  generic `codegen/src/lib.rs`.
- Outcome: a pure no-source W5 close was invalidated. The narrower
  provider-boundary cleanup was admitted and then closed by V4+V5 convergence.

### I5. Paper close and implicit SK-V9 dispatch remain invalid

- Commits: `1ed31cea`, `d936205d`, `e500ad00`, `32870fea`.
- Falsifier class: W6 hardening explicitly rejects missing wave dispositions,
  Apache/CITM presented as measured rows, W3/W4 presented as admitted source
  work, W5 presented as performance movement, SC-6-L1-R1 silently ratified, or
  any W6 source/generated/RESULTS/REDRESS change without a mismatch-specific
  plan.
- Outcome: SK-V8 closes only as a reconciled ledger close. SK-V9 must be
  planned and challenged before any new `G-Alpha closed` decision can authorize
  implementation.

## 4. Demoted Claims

### D1. Parse rows are substrate guards, not SOTA admissions

All current `parse_only` rows are `S / NO-GO` except the `canada` hard failure
`L / NO-GO`. Some rows beat same-run sonic strict numerically, but every main
row still records `Strictness=deferred` and `parse_utf8=view-boundary`.
Therefore parse rows are telemetry for future substrate work, not strict
admission rows.

### D2. Direct digest rows are guard-plane evidence, not product proof

The three current direct `A / GO` rows are `citm_catalog`, `marine_ik`, and
`unicode_basic`; the remaining 14 direct rows are `N-direct / NO-GO`. The
digest workload remains a guard plane. It cannot be promoted to typed product
proof, and W4 confirmed that a narrow Track 2 scalar-parent fold does not close
the direct residual.

### D3. Apache/CITM typed work is source/product parity, not measured row close

W2 proved generated source/product parity for Apache and CITM under the typed
schema path, but SK-V8 does not include their measured Criterion rows. Future
admission requires a row-table wave that owns run-id/metadata validation and
fresh measured row evidence.

### D4. CostFacts are required evidence, not performance movement

W1 made CostFacts gate-consumed and strict comparator identity explicit. That
is load-bearing for route discipline, but it cannot admit parser/runtime
behavior or reopen REDRESS routes without fresh W0 evidence, a same-wave
consumer, no-regression gate, REDRESS citation, and challenge acceptance.

### D5. The structural-projection thesis survives, but W3 Tier A does not

S-P2's architectural diagnosis still matters: bbnf scans structural bytes then
re-discovers structural and string boundaries scalar-side. SK-V8 W3 demotes the
immediate Tier A implementation from "ready storage swap" to "requires a
retained class/event grammar plus `ValueRef` cursor-contract proof first."

### D6. Lock 14 cleanup is architectural hygiene, not a throughput win

W5 validates provider-boundary cleanup and the `per_grammar_provider`
allowlist class. It does not move rows, update generated output, or change
`skinny/RESULTS.md`.

### D7. W6 close is a ledger close, not new evidence

W6 reconciles status and routes residuals. It does not add data, source, or
row-table movement.

## 5. Still Open for SK-V9 Alpha Candidate Consideration

These are planning candidates only. They are not dispatched by this artifact.

1. Typed row-table admission for Apache/CITM.
   - Carry from W2: source/product parity exists for Apache and CITM.
   - Missing proof: fresh measured Criterion row evidence, run-id/metadata
     ownership, checked report path, and `skinny/RESULTS.md` admission rules.
   - Required pre-block: do not count source rows as measured rows.

2. Retained class/event grammar plus `ValueRef` cursor contract.
   - Carry from W3: event-model mismatch is now explicit.
   - Missing proof: a retained grammar that represents structural classes,
     numbers/literals, container events, and string quote ownership inside one
     retained substrate, plus `ValueRef` traversal parity.
   - Required pre-block: do not reopen W3 as a storage-only swap, sidecar,
     `UnionTape`, parser-owned cursor, `tape_vs_tape` production consumer, new
     `BackendShape`, new BIR variant, or new directive.

3. Direct output contract or direct control-path tranche.
   - Carry from W4: Track 2 scalar-parent folding is falsified for the selected
     W4 rows, and digest-only evidence remains guard-plane evidence.
   - Missing proof: a direct contract that changes what must be materialized or
     a measured control-path intervention with independent Track 2 proof,
     W4/V9-aware checked gate, full-table maintain measurement, and Lock 14
     accounting.
   - Required pre-block: do not reopen scalar-parent folding by another name
     without the W4/V9 gate prerequisites.

4. Strictness and validation boundary.
   - Carry from W0/W1: strict comparator ids and freshness are gate-consumed.
   - Missing proof: bbnf-side measured validation still records deferred
     strictness and view-boundary UTF-8 in current rows. A future strict
     admission route needs measured-row validation, matching output plane, and
     strict native comparator evidence.

5. Sidecar same-run manifest, if future comparator claims need it.
   - Carry from W0: C++ sidecar values are historical or absent planning
     signals. `sidecar-same-run` rejects until a structured manifest parser and
     gate exist.
   - Missing proof: structured sidecar manifest ingestion, freshness binding,
     output-plane binding, and fail-closed tests.

6. Pass Omega residuals, not SK-V9 wave work by default.
   - SC-6-L1-R1 remains unratified and unproven under Lock 1 as written.
   - Broad lock amendments, canonical path cleanup, and top-level CRUD/surface
     refresh stay outside SK-V8 W6 and should route to Pass Omega unless the
     SK-V9 contract explicitly scopes them.

## 6. Load-Bearing Wins to Preserve in SK-V9

- Keep `SK-V8-open` as the measured baseline until SK-V9 creates its own
  checked opening baseline.
- Keep W0-style manifest consumption: row ids, outcomes, verdicts, run ids,
  profile artifacts, sample costs, host/build facts, feature masks, substrate
  status, Track 2 independence, and strict sidecar refusal are gate inputs, not
  prose.
- Keep W1 CostFacts/comparator admission binding: no behavior wave can cite
  route quality without CostFacts evidence and a selected strict native
  comparator id.
- Keep the W2 source/product versus measured-row distinction. Generated typed
  source rows can be valid without being `RESULTS.md` admissions.
- Keep the W3 event-model falsifier as a precondition. No structural projection
  implementation should proceed without the retained class/event grammar and
  `ValueRef` proof.
- Keep the W4 direct guard falsifier. Direct digest residuals need a contract or
  control-path frame, not another scalar-parent fold under a renamed route.
- Keep the W5 provider-boundary separation and `per_grammar_provider`
  allowlist class.
- Keep W6's close discipline: no source or row-table movement in close-only
  reconciliation.

## 7. SHA Index

Validated/admitted:

- `6d8cb701` - initial W0 telemetry manifest gate.
- `61d5cc3b` - W0 V10 cost/metadata blocker fold that V11/V12 accepted.
- `826af889` - W0 V12 closure challenge archive.
- `27aea746` - W0 closed and W1 handed off.
- `c6345e4d` - W1 CostFacts manifest and strict comparator id binding.
- `07923600` - W1 closure status fold.
- `12aff1e4` - W2 Apache/CITM typed source/product rows.
- `74fe4e1b` - W2 measured-baseline metadata expectation fold.
- `ab106386` - W2 V5 close as source/product parity with row-table admission rejected.
- `6e159f5c` - W5 JSON provider-boundary cleanup.
- `e51816c6` - W5 V5 close.
- `32870fea` - W6 V2 close and SK-V8 final status fold.

Rejected/routed/demoted:

- `6b4f46ae` - Canada typed candidate routed after parity failure.
- `8ce03af4` - W2 hardening fold that records source/product scope and
  measured-row demotion.
- `9deb2aed` - W3 research identifying tape structural event mismatch.
- `fc91c217` - W3 plan rejecting Tier A implementation on fit gate.
- `a9d0d69a` - W3 REDRESS 92 route on event-model fit gate.
- `bdd4473c` - W4 research selecting direct Track 2 scalar fold candidate.
- `a88e9725` - W4 REDRESS 93 scalar fold rejection after V2 revise.
- `b6da4754` - W4 V4 close as rejected/routed.
- `4ff53f6f` / `a311d643` - W5 initial audit/plan, later revised by provider residue.
- `1ed31cea` / `d936205d` - W6 research/plan bounding close reconciliation.
- `e500ad00` - W6 V1 accept cycle.

REDRESS:

- REDRESS 91 - W2 source/product parity admitted; Canada and benchmark row-table admission rejected/routed.
- REDRESS 92 - W3 Tier A rejected/routed on scanner/tape event-model mismatch.
- REDRESS 93 - W4 scalar-parent fold rejected/routed on selected-row falsification.
