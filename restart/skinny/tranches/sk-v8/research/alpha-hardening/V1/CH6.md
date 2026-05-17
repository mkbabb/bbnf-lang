# CH6 Next-Tranche-Impact Challenge - SK-V8 Alpha V1

Date: 2026-05-17.

Scope: challenge the SK-V8 Alpha A-F packet for next-tranche dispatch impact:
revert protocols, hard caps, telemetry goalset, G-Alpha signoff posture, wave
ordering, and whether SK-V8 can dispatch while critical defects remain open.

Read set:

- `restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `docs/precepts/instructions/tranche/SPEC.md`
- `restart/skinny/tranches/sk-v7/SPEC.md` sections 13-15.

Overall disposition: REVISE.

Dispatch disposition: REJECT dispatch from the current Alpha-F draft. A W0-only
SK-V8 dispatch can become acceptable only after the final SK-V8 `SYNTHESIS.md`,
`SPEC.md`, `HANDOFF.md`, and `DISPATCH-PROMPT.md` exist, CHALLENGE convergence
has zero critical defects, G-Alpha is closed by the user, and the final packet
states that later waves remain conditional on W0 evidence.

## Disposition Table

| Topic | Disposition | Reason | Required fix |
|---|---|---|---|
| Revert protocols | REVISE | Alpha-E gives concrete candidate-level revert rules, but Alpha-F only spells explicit wave revert blocks for W0 and W1. PASS-ALPHA requires every wave to carry a revert protocol and same-wave consumer rule. | Add a W0-W6 revert matrix to the final SPEC with failure trigger, rollback slice, rejected-patch/evidence path, REDRESS entry rule, and downstream block/unblock rule. |
| Hard caps | REVISE | Alpha-F has wave caps and default phase caps, but it does not bind agent count, phase caps, and challenge caps per wave in one dispatchable table. | For every wave, state agent count, research cap, plan cap, redress cap, challenge cap when applicable, total wall cap, and timeout disposition. |
| Telemetry goalset | REVISE | The draft correctly makes W0 observability-first, but exact row targets, schema names, enum mapping, and W1-W3 thresholds remain open. | Materialize a per-row goalset table before G-Alpha; define the current `K` and `N-direct` outcomes or remap them into the PASS-ALPHA enum; make `gate-json` fail missing SK-V8 telemetry after W0. |
| G-Alpha posture | ACCEPT with REVISE item | Alpha-F correctly says the draft cannot close G-Alpha and no implementation wave should dispatch from it. The remaining defect is the unresolved Pass Omega blocker question. | Keep the no-dispatch posture, and add a binary Pass Omega decision: blocks G-Alpha, or routes after SK-V8 opens with owner and gate. |
| Wave ordering | REVISE | W0 first is correct. CostFacts gate integration is too late if W2/W3 can change route facts before W4 binds CostFacts evidence into `RESULTS.md` and `gate-json`. | Move CostFacts gate integration immediately after W0 and before any typed, parse, or direct behavior wave, or split behavior waves so route-fact changes cannot land until CostFacts binding is green. |
| Dispatch with open critical defects | REJECT | PASS-ALPHA requires zero open critical defects and no orphan REVISE before convergence; Alpha-F itself says it is not a final contract and must not dispatch SK-V8 directly. | Do not dispatch SK-V8 implementation work until the critical list below is closed. If the user wants only W0, final docs must explicitly scope G-Alpha to W0 entry and mark W1-W6 conditional. |

## Evidence Basis

PASS-ALPHA makes CH6 responsible for revert protocol, hard caps,
triumvirate discipline, and measurable bench-verifiable goalsets
(`restart/prompts/pass-contracts/PASS-ALPHA.md:47`). It requires each wave to
name owner paths, entry gate, exit gate, hard cap, revert protocol, same-wave
consumer, and pre-blocked routes
(`restart/prompts/pass-contracts/PASS-ALPHA.md:114`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:120`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:121`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:122`).

The tranche precept limits parallelism to six agents, forbids overlapping write
bounds, and requires every wave to land substrate with its consumer or delete the
substrate (`docs/precepts/instructions/tranche/SPEC.md:42`,
`docs/precepts/instructions/tranche/SPEC.md:50`,
`docs/precepts/instructions/tranche/SPEC.md:53`). It also rejects narrative-only
or "consumer will be wired later" hard gates
(`docs/precepts/instructions/tranche/SPEC.md:72`,
`docs/precepts/instructions/tranche/SPEC.md:76`,
`docs/precepts/instructions/tranche/SPEC.md:78`).

SK-V7 section 13 routes SK-V8 toward the hard residual around twitter parse,
the yyjson gap, remaining Lock 14 residue, and remaining bbnf.asm primitive body
fills (`restart/skinny/tranches/sk-v7/SPEC.md:403`,
`restart/skinny/tranches/sk-v7/SPEC.md:404`,
`restart/skinny/tranches/sk-v7/SPEC.md:405`). SK-V7 section 15 forbids closing
on "future phase will fix it"; every miss must be a blocker, rejected route, or
next concrete wave input (`restart/skinny/tranches/sk-v7/SPEC.md:435`,
`restart/skinny/tranches/sk-v7/SPEC.md:436`,
`restart/skinny/tranches/sk-v7/SPEC.md:437`).

## Revert Protocols

Disposition: REVISE.

Alpha-E is stronger than Alpha-F here. Candidate 1 saves rejected patches,
reverts runtime/template/gate changes together, restores pre-wave RESULTS values
unless there is a rejected comparison table, and records failed rows in REDRESS
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:95`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:98`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:100`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:102`).
The telemetry candidate reverts report/gate/schema changes together and restores
the prior RESULTS schema if validation is not reproducible
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:166`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:168`).
The Lock 14 candidate reverts relocation and manifest changes as one slice
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:225`).
The bitmap candidate reverts runtime selection, predicate plumbing, and
bench/gate changes together, and records both target and falsifier rows in
REDRESS
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:293`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md:297`).

Alpha-F does not carry that discipline through every wave section. W0 has a
revert block for failed profiling schema population
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:399`).
W1 has a revert block for failed typed row additions
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:427`).
W2 requires rejection with REDRESS if thresholds fail, but does not state the
rollback slice, patch evidence path, or whether generated/runtime output is
restored
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:446`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:451`).
W3, W4, W5, and W6 define exit gates without explicit revert protocols
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:469`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:491`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:509`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:528`).

Concrete fix:

Add this table shape to the final SPEC and HANDOFF:

| Wave | Failure trigger | Revert action | Evidence retained | Downstream effect |
|---|---|---|---|---|
| W0 | Any required telemetry field cannot be populated or validated. | Revert schema/report/gate changes together; restore opening RESULTS schema. | REDRESS entry plus profiler/bench path that failed. | Blocks all behavior waves. |
| W1 | CostFacts binding missing evidence, rejected alternatives, REDRESS references, or wave id. | Revert CostFacts gate/report changes together; keep read-only audit evidence. | REDRESS entry naming missing fact class. | Blocks typed, parse, and direct behavior waves. |
| W2 | Typed product rows miss same-plane slack or Track 2/oracle honesty. | Revert row additions or leave disabled only with explicit rejected status; restore generated outputs if changed. | Rejected row table, generated diff audit, REDRESS entry. | Does not block parse unless shared code changed. |
| W3 | Parse candidate misses threshold or guard rows regress. | Revert all runtime/template/generated/gate/RESULTS changes as one slice. | Rejected patch under the wave research directory plus REDRESS row table. | Blocks further parse candidates until CHALLENGE accepts a new frame. |
| W4 | Direct guard rows cannot close or product claim becomes digest-only. | Revert behavior changes; keep triage report that routes guard residuals. | REDRESS or HANDOFF residual entry. | Does not block close if residuals are explicitly routed. |
| W5 | Audit finds Lock 14 drift. | Fix inside W5 if in bounds; otherwise revert offending wave slice or route named owner before close. | Drift report and grep evidence. | Blocks W6 close while unresolved. |
| W6 | RESULTS, REDRESS, and HANDOFF disagree. | No source revert by default; reopen the producing wave or mark close blocked. | Close-honesty mismatch list. | Blocks SK-V8 close. |

The wave ids above assume the ordering fix below. If the final SPEC keeps the
Alpha-F numbering, the same table must be renumbered.

## Hard Caps

Disposition: REVISE.

Alpha-F proposes total wave caps: W0 180 minutes, W1 300, W2 300, W3 240,
W4 240, W5 180, and W6 120
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:367`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:375`).
It also proposes default phase caps of 30 minutes for research agents, 30 for
plan synthesis, 75 for redress implementation, and 90 for high-risk CHALLENGE
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:537`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:544`).
It preserves the six-agent ceiling and forbids role merger in one commit
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:553`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:555`).

This is close but not dispatchable. A dispatch agent needs per-wave phase caps,
not a global default that can be read loosely. The final SPEC should contain one
table with:

- wave id and name;
- max parallel agents;
- research cap;
- plan cap;
- redress cap;
- high-risk CHALLENGE cap, or `n/a`;
- total wall cap;
- timeout action: reject, split, or escalate.

Concrete fix:

Use the smallest cap that preserves evidence quality. W0 should stay 180
minutes because it is profiling and schema work. Behavior waves may keep 240 or
300 minutes only if they name the profile input and no more than one intervention
family. If a behavior wave needs more than one intervention family, split it
before dispatch.

## Telemetry Goalset

Disposition: REVISE.

Alpha-A establishes the measured opening reality: current RESULTS has schema-v3
columns, but overall state remains `N-direct / NoGo`
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:11`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:15`).
All parse rows are `K / NO-GO`, direct rows are split between 11 `N-direct /
NO-GO` and 6 `A / GO`, and real typed rows are 4 `A / GO`
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:21`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:22`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:23`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:24`).
Alpha-A also records two schema defects: current outcomes include `K` and
`N-direct`, which are outside the PASS-ALPHA enum, and `Delta vs SK-V6` is
non-derivable from current artifacts
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:34`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:40`).
It records missing cycles-per-byte, missing workload rows, missing usable hot
leaf attribution, missing simdjson On Demand and asmjson rows, and incomplete
sidecar coverage
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:137`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:140`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:144`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:147`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:149`).

Alpha-B correctly downgrades sidecar competitor rows to planning signals unless
they become same-run telemetry. It says simdjson, yyjson, RapidJSON, and asmjson
values are sidecar profile values when populated, not same-run strict anchors
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md:27`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md:29`).
It also marks yyjson twitter parse as a sidecar gap until rerun as same-run
telemetry
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md:117`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md:119`).

Alpha-F has the right telemetry direction: every row should gain hot leaf,
profile artifact path, c/B or equivalent sample cost, and delta versus the
SK-V7 opening baseline
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:241`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:245`).
It also lists required SK-V8 additions such as profile artifact, cycles per
byte, sample count, build flags, host triple, feature mask, CostFacts ids,
REDRESS entry, wave id, run id, and sidecar freshness
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:334`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:350`).
Its gate rules reject placeholder hot leaves, missing profile artifacts,
missing previous-SK deltas, strict claims from lossy comparators, admitted rows
without REDRESS or wave id, and CostFacts-selected shapes without evidence
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:352`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:360`).

The defect is that Alpha-F still calls exact row thresholds open for
finalization
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:233`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:235`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:620`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:628`).
PASS-ALPHA requires the implementation packet to name current state, target
state, expected intervention, and fallback per row
(`restart/prompts/pass-contracts/PASS-ALPHA.md:55`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:63`).

Concrete fix:

The final SPEC section 0 must have a row table with:

- corpus and workload;
- current outcome/verdict and output plane;
- current Track 1 and Track 2 Mbps;
- strict comparator anchors and sidecar status;
- target threshold or explicit `route-only`;
- expected wave;
- fallback or REDRESS action;
- guard rows;
- required telemetry fields after W0.

Rows outside the active SK-V8 close set must not disappear. They must be marked
`maintain`, `guard`, or `routed residual` with an owner wave.

The final schema must either add `K` and `N-direct` to the allowed outcome enum
or define a lossless mapping from current RESULTS outcomes to the PASS-ALPHA
`A / C / G / L` template before `gate-json` enforces the table.

## G-Alpha Signoff Posture

Disposition: ACCEPT with one REVISE item.

Alpha-F states the correct posture. The file is a draft, not the final
contract, and must not be used to dispatch SK-V8 directly
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:13`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:15`).
It says G-Alpha can be presented only after Alpha A-E are folded, final
SYNTHESIS/SPEC/HANDOFF/DISPATCH-PROMPT exist, CHALLENGE returns at least 95
percent ACCEPT with zero critical defects and no orphan REVISE, and the user
receives a summary of rows, interventions, hard caps, telemetry schema,
pre-blocked routes, and predicted close state
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:598`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:607`).
It gives only two signoff outcomes, `G-Alpha closed` and `G-Alpha revise`, and
forbids SK-V8 implementation dispatch from the draft
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:611`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:616`).

That matches PASS-ALPHA, which requires user G-Alpha signoff after challenge
convergence and says no SK-V{N+1} dispatch without G-Alpha
(`restart/prompts/pass-contracts/PASS-ALPHA.md:166`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:177`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:200`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:204`).

The revision item is Pass Omega. SK-V7 section 14 says Pass Omega is triggered
after SK-V7 close and notes that the current totality cycle has not run
V7-paired hardening
(`restart/skinny/tranches/sk-v7/SPEC.md:408`,
`restart/skinny/tranches/sk-v7/SPEC.md:412`,
`restart/skinny/tranches/sk-v7/SPEC.md:416`,
`restart/skinny/tranches/sk-v7/SPEC.md:418`). Alpha-F leaves open whether a
Pass Omega lock amendment blocks G-Alpha or routes after SK-V8 opens
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:629`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:630`).

Concrete fix:

The final G-Alpha summary must include:

- `Pass Omega blocks G-Alpha`: yes/no;
- if yes, the blocking amendment and evidence path;
- if no, the routed Omega item, owner, and receiving gate;
- statement that W10 bitmap body fills were rejected and W10c admitted only B6
  stack-canary Stage 1.

## Wave Ordering

Disposition: REVISE.

W0 first is accepted. Alpha-F correctly makes W0 a profile and telemetry lock
with no source-performance patch
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:367`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:397`).
This is necessary because Alpha-A says current hot leaf cells are not usable
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:144`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:146`).

The proposed ordering after W0 is not accepted. Alpha-F puts typed product
expansion at W1, parse candidate at W2, direct guard triage at W3, and CostFacts
gate integration at W4
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:367`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:375`).
But Alpha-C says CostFacts must be the gate for any route-fact change and must
record chosen plane plus rejected alternatives rather than globalizing policy
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md:144`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md:146`).
Alpha-F itself says W4 is the point where every materialized JSON rule reports
chosen shape, rejected alternatives, evidence source, REDRESS references, and
wave id
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:491`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:496`).

This creates a next-tranche hazard: W2 or W3 could land route-fact behavior
before the gate that records route facts is load-bearing.

Concrete fix:

Use this order in the final SPEC:

| Order | Wave | Purpose |
|---:|---|---|
| 0 | W0 Baseline Profile And Telemetry Lock | Populate hot leaf, profile path, c/B or equivalent, run id, sidecar freshness, and SK-V7-open delta. No performance patch. |
| 1 | W1 CostFacts Gate Integration | Make CostFacts evidence, rejected alternatives, REDRESS references, and wave id load-bearing in RESULTS and gate-json. |
| 2 | W2 Typed Product Plane Expansion | Add generated real typed rows only from explicit host/API schema facts. |
| 3 | W3 Parse Candidate From Fresh Profiles | Select one parse intervention after W0/W1; reject if thresholds or guards fail. |
| 4 | W4 Direct Guard Triage | Close a small selected digest guard set or route it as guard residual. |
| 5 | W5 Grammar-Neutral Audit And Lock 14 Preservation | Verify W1-W4 did not reintroduce generic-crate JSON policy. |
| 6 | W6 Close, Redress Reconciliation, And Alpha Feedback | Reconcile RESULTS, REDRESS, HANDOFF, and residual routing. |

If CostFacts integration cannot move before behavior waves, then W2-W4 must be
declared unavailable at G-Alpha and opened only by a post-W0/post-W1 plan gate.

## Open Critical Defects

Disposition: REJECT dispatch until closed.

Critical defect 1: final dispatch documents do not exist in the alpha packet.
Alpha-F requires final `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, and
`DISPATCH-PROMPT.md` before G-Alpha presentation
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:600`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:602`).

Critical defect 2: exact W1-W3 row thresholds, selected typed rows, parse rows,
direct guard policy, exact schema field names, W2/W3 owner paths, and Pass Omega
blocking status are open
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:620`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md:630`).

Critical defect 3: final SPEC does not yet include a per-wave revert protocol
for W2-W6, although PASS-ALPHA requires it
(`restart/prompts/pass-contracts/PASS-ALPHA.md:119`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:120`).

Critical defect 4: CostFacts ordering allows route-fact changes before the
CostFacts gate is load-bearing.

Critical defect 5: telemetry enum and missing-field semantics are not yet
dispatchable because current RESULTS outcomes include `K` and `N-direct`, while
PASS-ALPHA's template enum does not
(`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:36`,
`restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md:37`).

PASS-ALPHA convergence requires at least 95 percent ACCEPT, zero open critical
defects, no orphan REVISE, and user G-Alpha signoff
(`restart/prompts/pass-contracts/PASS-ALPHA.md:181`). Therefore SK-V8 cannot
dispatch from V1 Alpha hardening as it stands.

## Final CH6 Disposition

REVISE Alpha-F before G-Alpha.

ACCEPT the following:

- W0 observability-first posture.
- Candidate-level pre-blocking in Alpha-C and Alpha-E.
- No-dispatch G-Alpha posture in Alpha-F.
- Rejection of W10/W10b primitive body fills as SK-V7 wins.

REVISE the following:

- Per-wave revert protocols.
- Hard-cap table with agent counts and timeout actions.
- Per-row telemetry goalset and enum mapping.
- CostFacts-before-behavior wave ordering.
- Pass Omega blocking decision.

REJECT the following:

- Any SK-V8 implementation dispatch from the Alpha-F draft.
- Any G-Alpha presentation that still has the open critical defects above.
- Any broad SK-V8 dispatch that treats W1-W6 as executable before W0 and
  CostFacts evidence make their owner paths and row thresholds concrete.
