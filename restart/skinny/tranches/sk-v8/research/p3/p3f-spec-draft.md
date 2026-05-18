# SK-V8 P3-F: SPEC/DISPATCH Draft Fold

Pass: S-P3 Synthesis-Plan. Cycle: V4 exact traceability fold after V3 challenge.
Date: 2026-05-18.
Scope: fold P3-A through P3-E into
`SPEC Section 0.1 - Global Close Condition`, `SPEC Section 0.2 - Comparator Classes`, `SPEC Section 0.3 - Outcome Enum`, `SPEC Section 0.4 - Required Telemetry`, `SPEC Section 0.5 - Opening Row Goalset`, `SPEC Section 2 - Wave Manifest, Caps, And Reruns`, `SPEC Section 2.1 - Generality And Lock 14 Gate`, `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`, `SPEC Section 4 - W1 CostFacts And Comparator Gate Binding`, `SPEC Section 5 - W2 Typed Product Plane Expansion`, `SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`, `SPEC Section 7 - W4 Direct Guard Triage`, `SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`, `SPEC Section 9 - W6 Close And Alpha Feedback`, `SPEC Section 10 - Pre-Blocked Routes`, and `SPEC Section 11 - G-Alpha And Dispatch Scope` and
`DISPATCH sections Wave Manifest, Conditional Gates, and Entry Condition`.
Output: this file, `SPEC Section 0.1 - Global Close Condition`, `SPEC Section 0.2 - Comparator Classes`, `SPEC Section 0.3 - Outcome Enum`, `SPEC Section 0.4 - Required Telemetry`, `SPEC Section 0.5 - Opening Row Goalset`, `SPEC Section 2 - Wave Manifest, Caps, And Reruns`, `SPEC Section 2.1 - Generality And Lock 14 Gate`, `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`, `SPEC Section 4 - W1 CostFacts And Comparator Gate Binding`, `SPEC Section 5 - W2 Typed Product Plane Expansion`, `SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`, `SPEC Section 7 - W4 Direct Guard Triage`, `SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`, `SPEC Section 9 - W6 Close And Alpha Feedback`, `SPEC Section 10 - Pre-Blocked Routes`, and `SPEC Section 11 - G-Alpha And Dispatch Scope`, and
`DISPATCH sections Wave Manifest, Conditional Gates, and Entry Condition`.
Traceability note: inline citations use exact SPEC/HANDOFF section labels or current file:line anchors. RESULTS row claims resolve to `skinny/RESULTS.md:3-42`; Track 2 independence resolves to `skinny/RESULTS.md:217-218`; named REDRESS ids resolve to `skinny/REDRESS.md`, with cited live spans `skinny/REDRESS.md:1214-1219`, `skinny/REDRESS.md:1301-1312`, and `skinny/REDRESS.md:1331-2605`.


## Synthesis

P3-F preserves the dispatch lock: S-P3 is planning only, G-Alpha remains
required, and `G-Alpha closed` initially dispatches W0 only. The revised packet
gates W1-W6 as conditional plan material, but no later implementation wave
dispatches until W0 closes and the wave entry gate, plan artifact, and required
challenge are satisfied.

The folded plan keeps the seven-wave P3-A/P3-B shortlist:

| Wave | Folded objective |
|---|---|
| W0 | Baseline profile and telemetry lock. |
| W1 | CostFacts and strict comparator gate binding. |
| W2 | Typed product plane expansion. |
| W3 | Tier A tape plus structural-projection union, after W0/W1 and challenge. |
| W4 | Direct guard triage. |
| W5 | Lock 14 grammar-neutral audit. |
| W6 | Close, REDRESS reconciliation, and Alpha feedback. |

The W3 lead is bounded to the S-P2 Tier A candidate: structural-class cursor
migration inside one retained `Tape`, with generated JSON retained parsing as
the same-wave production consumer. Tier B string-boundary,
quote/backslash/parity, CostFacts-template work, `tape_vs_tape`, default
PMULL/CTZ rewires, and all sidecar/parser-owned cursor routes remain blocked
unless a future plan prices and challenges them separately.

## Fold Disposition

| Input | Folded into |
|---|---|
| P3-A candidate shortlist | SPEC wave manifest, W0-W6 objectives, W3 lead boundary, non-shortlisted routes. |
| P3-B sequencing | SPEC dispatch order, W4 dependency on W2/W3 disposition, W0-only initial dispatch. |
| P3-C falsifiability gates | SPEC per-wave entry/exit gates, row thresholds, maintain budgets, revert protocols. |
| P3-D telemetry schema | SPEC required telemetry, strict-admission refusal predicates, failure states. |
| P3-E pre-block ledger | SPEC and DISPATCH global/per-wave pre-blocked routes. |

## Governance

V6 and V7 are treated as the two consecutive qualifying S-P2 ACCEPT cycles.
They authorize S-P3 synthesis only. They do not authorize implementation,
G-Alpha close, W3 redress, new substrate surface, relaxed strictness, or
automatic dispatch.

The folded SPEC preserves:

- strict-vs-strict admission only;
- Lock 14 grammar neutrality and non-JSON proof for generic edits;
- no new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate
  API, parser-owned cursor/facts, or parallel substrate;
- no deferrals in gates;
- same-wave consumer for every primitive, substrate, or generated path;
- implementation/redress hard cap of 90 minutes per wave slice;
- explicit per-wave source/edit LOC budgets in SPEC, DISPATCH, and HANDOFF.

## V1 Hardening Fold

| Challenge | Disposition |
|---|---|
| CH1 correctness | Folded. SPEC Section 0.5 now carries the W2 candidate typed seed table and constrains W2 selection to that table unless a later accepted S-P3 revision expands it. Local P3 references were normalized away from stale line-number citations. Future wave artifacts use naming patterns under concrete directories rather than unresolved wildcard links. |
| CH4 cost | Folded. SPEC, DISPATCH, and HANDOFF now carry per-wave source/edit LOC budgets as a conjunctive gate with the 90-minute implementation/redress cap. W3 now has a mandatory pre-redress split gate covering source/test LOC, generated LOC, gate/report LOC, docs/RESULTS/REDRESS edits, and the revert slice. |
| CH2, CH3, CH5, CH6 | Preserved. Their ACCEPT findings remain binding: no relaxed generality, regression, hidden-coupling, or paper-close language was introduced by the V2 fold. |

## V2 Hardening Fold

| Challenge | Disposition |
|---|---|
| CH1 correctness | Folded for V3. P3-A through P3-F now use stable section labels for material inline claims, retain concrete local paths in Sources, and state that RESULTS rows and REDRESS ids are resolving anchors. The V3 fold avoids stale line numbers without leaving material claims as repeated bare file paths. |
| CH2, CH3, CH4, CH5, CH6 | Preserved. Their V2 ACCEPT findings remain binding; the citation fold does not change W2 seed gates, LOC/time budgets, W3 split gates, dispatch lock, pre-block coverage, Lock 14, or same-wave-consumer requirements. |

## V3 Hardening Fold

| Challenge | Disposition |
|---|---|
| CH1 correctness | Folded for V4. P3-A through P3-F now resolve material SPEC and HANDOFF citations to exact section labels and resolve mutable RESULTS/REDRESS row claims through current file:line anchors. The fold removes the broad multi-section SPEC/HANDOFF bundles and generic RESULTS/REDRESS placeholders CH1 rejected. |
| CH2, CH3, CH4, CH5, CH6 | Preserved. Their V3 ACCEPT findings remain binding; the exact-traceability fold does not alter G-Alpha/W0 dispatch lock, W2 seed gates, W3 Tier A/Tier B split, 90-minute/LOC gates, strict-vs-strict comparator discipline, Lock 14 neutrality, no-new-surface constraints, or pre-blocked route coverage. |

## Residual Risks

- W3 Tier A may still be too broad for one 90-minute implementation/redress
  slice once scalar oracle, checkasm, generated audit, retained view parity, and
  gate refresh are counted. The SPEC now requires a LOC/time fit estimate and
  split or REVISE before redress if it cannot fit.
- Seed Mbps floors are planning floors only. After W0 creates `SK-V8-open`, the
  implementing wave must recompute thresholds from same-run strict anchors.
- `parse_only` remains substrate-guard non-admission unless an explicit schema
  amendment and measured-path strict gate make a plane-matched row eligible.
- SC-6-L1-R1 remains a Pass Omega candidate. W3 must either wait for Omega or
  prove Lock 1 as written and route the Omega residual.

## Blockers

None for P3-F production. Implementation remains blocked by G-Alpha, W0, and
the per-wave entry gates.

## Required Folds If REVISE

- Reconcile any P3-C threshold changes back into
  `SPEC Section 0.1 - Global Close Condition`, `SPEC Section 0.2 - Comparator Classes`, `SPEC Section 0.3 - Outcome Enum`, `SPEC Section 0.4 - Required Telemetry`, `SPEC Section 0.5 - Opening Row Goalset`, `SPEC Section 2 - Wave Manifest, Caps, And Reruns`, `SPEC Section 2.1 - Generality And Lock 14 Gate`, `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`, `SPEC Section 4 - W1 CostFacts And Comparator Gate Binding`, `SPEC Section 5 - W2 Typed Product Plane Expansion`, `SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`, `SPEC Section 7 - W4 Direct Guard Triage`, `SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`, `SPEC Section 9 - W6 Close And Alpha Feedback`, `SPEC Section 10 - Pre-Blocked Routes`, and `SPEC Section 11 - G-Alpha And Dispatch Scope`.
- Recompute W2/W3/W4 row floors after W0 if `SK-V8-open` changes the seed
  values.
- Split W3 if Tier A cannot be implemented, measured, reported, and reverted
  inside 90 minutes.
- Add a new challenge before any Tier B, `tape_vs_tape`, PMULL/CTZ, or direct
  materialization route is promoted.

## Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
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

Self-verdict: ACCEPT.

Confidence: 96%.
