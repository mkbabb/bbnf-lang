# Handoff SK-V8

Date: 2026-05-18.

Status: G-Alpha closed by user on 2026-05-18T05:26:48Z. SK-V8 W0 and W1 are
closed. W2 has a redress disposition: source/product parity admitted, benchmark
row-table admission rejected for this wave. W3 is the next dispatchable wave
after its own research, plan, challenge, and redress gate. W4-W6 require prior
wave dispositions and their own gates before implementation.

Next move: SK-V8 W3.

## 1. Read First

1. `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
2. `restart/skinny/tranches/sk-v8/SPEC.md`
3. `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
4. `restart/skinny/tranches/sk-v8/research/alpha/`
5. `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/`
6. `restart/skinny/tranches/sk-v8/research/alpha-hardening/V2/CONSOLIDATED.md`
7. `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/` (the S-P2 substrate-ceiling cohort, SC-1..SC-6)
8. `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`
9. `restart/skinny/tranches/sk-v8/research/p3/`
10. `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V5-CONSOLIDATED.md`
11. `restart/skinny/tranches/sk-v8/research/g-alpha/G-ALPHA-PRESENTATION.md`
12. `skinny/RESULTS.md`
13. `skinny/REDRESS.md`
14. `restart/prompts/skinny/PASS-2-RESEARCH.md` + `PASS-3-SYNTHESIS-PLAN.md`
15. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`

## 2. Current Measured State

Current authority is W0-rendered `skinny/RESULTS.md` after `SK-V8-open`. The
report remains `N-direct / NoGo`.

| Family | State |
|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` | 4 `A / GO` |

Current telemetry caveats:

- Every current main row uses `Strictness=deferred`.
- `Delta vs SK-V6` is non-derivable in the current report.
- W0 renders criterion slope profile artifacts and non-placeholder hot-leaf
  bindings for each current main row.
- C++ comparator values are historical sidecar planning signals. W0 admits no
  structured sidecar same-run manifest and rejects `sidecar-same-run` claims
  until a later accepted wave adds a manifest parser and gate.

## 3. W10 Honesty Note

SK-V7 W10 did not admit the original bitmap body-fill target.

- REDRESS 88 rejects PMULL prefix-XOR as a default hot production body.
- REDRESS 89 rejects CSSC CTZ/bulk production consumption.
- REDRESS 90 admits only B6 stack-canary Stage 1 with zero production and
  `RESULTS.md` diff.

PMULL and CTZ/bulk enter SK-V8 only as reserve research after W0/W1 evidence
names bitmap work as a hot owner and challenge accepts the changed framing.

## 3a. Substrate-Ceiling Finding

A new-lens skinny S-P2 research cohort ran six agents SC-1 through SC-6 under
`research/p2-substrate-ceiling/`. It produced a lead hypothesis: bbnf
produces a stage-1 SIMD structural index (`scan_structurals`, ~69 Gbps) and
discards it; the parser re-discovers structural bytes and delimiter boundaries
in a scalar pass. SK-V7's six rejected kernels each optimised that scalar
rediscovery — a pass that should not exist. V3 splits the nominated tape ⊕
structural-projection union into two scopes. Tier A is the S-P3-ready
structural-class cursor migration: retain the stage-1 index inside the single
`Tape`, add scan-written opaque structural-class ordinals, and migrate generated
retained JSON Track 1 parsing plus retained view/`ValueRef` to consume that
cursor in the same wave. Tier B owns string-boundary /
quote-backslash-parity / CostFacts-template closure and is not part of Tier A.
String-density/knee evidence is diagnostic telemetry only until a later gate
names row set, formula, target, maintain budget, and pass/fail rule.

The cohort is S-P2 research output. Per the pass framework
(`restart/prompts/skinny/PASS-2-RESEARCH.md` §3) S-P2 advances only after two
consecutive CHALLENGE cycles at >=95% ACCEPT with no unresolved REVISE, or an
explicit user pin. V1, V2, V3, and V4 did not converge, and V5 reached the pass
hard ceiling with CH4 REVISE
(`research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md`).
The user revised the hard-ceiling instruction on 2026-05-17 by authorizing an
exceptional V6 challenge. V6 ACCEPTed 6/6
(`research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md`),
as the first qualifying cycle after V5 REVISE. On 2026-05-18 the user
authorized an exceptional V7 challenge, not a convergence shortcut. V7 ACCEPTed
6/6
(`research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`),
forming the second consecutive qualifying ACCEPT cycle. S-P2 is converged;
S-P3 convergence is recorded in
`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V5-CONSOLIDATED.md`.
The cohort does not change the
G-Alpha-W0-only constraint; it nominates a lead W3 hypothesis so W0 telemetry
confirms or falsifies the finding executably rather than discovering it from
scratch. SC-1..SC-6 authorize no W3 plan by themselves: W3 remains blocked on
W0/W1 closure, fresh plan owner paths, same-wave production consumer, revert
protocol, measurement thresholds, measured-path strict validation proof, and
challenge acceptance. `tape_vs_tape` is W0/W1 telemetry only, not a W3
production consumer. For this packet it is explicitly a routed residual, not
default W0/W1 scope; adding it later requires a named owner/LOC/test/rerun plan
before it can consume wave budget.

## 3b. S-P3 Convergence

S-P3 Synthesis-Plan is converged. The live wave packet is
`restart/skinny/tranches/sk-v8/SPEC.md` plus
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`, with P3-A through P3-F
under `restart/skinny/tranches/sk-v8/research/p3/`.

Challenge history:

- V1 and V2 exposed hardening folds for W2 seed traceability, LOC/time gates,
  and exact citations.
- V3 returned CH1 REVISE for broad SPEC/HANDOFF/RESULTS/REDRESS citation
  bundles.
- V4 folded exact traceability and returned 6/6 ACCEPT, minimum confidence 96.
- V5 re-challenged the unchanged V4-folded packet and returned 6/6 ACCEPT,
  minimum confidence 96.

`research/p3/hardening/HARDENING-S-P3-V5-CONSOLIDATED.md` is the convergence
authority. It closes S-P3 planning only. It does not close G-Alpha and does not
dispatch W0.

## 4. Dispatch Posture

| Wave | Name | Status | Source/edit LOC budget |
|---|---|---|---|
| W0 | Baseline Profile And Telemetry Lock | Closed by V11+V12 challenge convergence | 0 production behavior LOC; reauthorized telemetry gate/report/Lock14 scope per SPEC Section 3 accounting; post-V6 folds <=120 report/gate/test/doc LOC |
| W1 | CostFacts Gate Binding | Closed by W1 redress commit `c6345e4d` | 0 parser/generated behavior LOC; 272 insertions / 13 deletions in CostFacts/report/gate/test scope |
| W2 | Typed Product Plane Expansion | Source/product parity admitted by `12aff1e4`; benchmark row-table admission rejected/routed in REDRESS 91; W2 hardening V1 REVISE folds applied before V2 re-challenge | <=650 source/test LOC; generated output and row tables named separately |
| W3 | Profile-Selected Parse Candidate — lead candidate: tape ⊕ structural-projection union (S-P2 cohort) | Conditional on W0/W1 challenge | <=450 source/test LOC default; <=650 only with accepted pre-redress fit proof |
| W4 | Direct Guard Triage | Conditional on W0/W1 plan update | <=300 source/test LOC and <=3 selected rows |
| W5 | Grammar-Neutral Audit And Lock 14 Preservation | Conditional on W1-W4 close | 0 source LOC default; <=150 named Lock 14 cleanup LOC |
| W6 | Close And Alpha Feedback | Conditional on all prior dispositions | 0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only |

W0 is telemetry-only. If W0 changes parser, scanner, SIMD, asm, codegen
behavior, or product-plane behavior, reject it.

Every implementation/redress slice is also capped at 90 minutes, including
source edits, generation, verification, RESULTS/REDRESS updates, and rollback.
Generated outputs must be named, diff-audited, and included in the revert
slice. Any plan that exceeds either its LOC budget or the 90-minute cap must
split before dispatch or return REVISE.

W0 V7 cost fold: CH4 V6 rejected the old `<=350` cap as stale after the
implemented telemetry gate reached 3532 insertions / 253 deletions across
`skinny/RESULTS.md`, `skinny/crates/bbnf-bench/src/bin/gate.rs`,
`skinny/crates/bbnf-bench/src/gate.rs`, `skinny/crates/bbnf-bench/src/lib.rs`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
`skinny/crates/bbnf-bench/src/report.rs`, and `skinny/xtask/src/main.rs` from
baseline `0bd16f6d` to V6 target `6c0bc15d`. W0 remains admissible only if the
frozen behavior-surface diff stays empty and rollback is commit-sliced through
the W0 implementation commits named in SPEC Section 3 plus any post-V6 fold.

W0 closure record: V11 accepted 6/6 as the first qualifying cycle after the V10
reset, and V12 accepted 6/6 as the unchanged second qualifying cycle. The
closure authority is
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`.
W1 closure record: commit `c6345e4d` binds `gate-json --with-cost-facts` to a
`sk-v8-costfacts-v1` manifest with 15 materialized JSON rules, SK-V8-W1 wave
ids, evidence sources, and REDRESS refs. The gate has zero gate-level
`BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostics while preserving producer
diagnostics for audit visibility. Strict admission now binds native comparator
ids and rejects lossy, sidecar, and unknown comparator ids as strict anchors.
W1 left generated/parser/product surfaces and `skinny/RESULTS.md` unchanged.
The single W1 benchmark refresh attempt was rejected by the W0 run-id strict
validator after local Criterion metadata drifted, so W1 records no RESULTS
update.

W2 disposition record: commit `12aff1e4` adds two generated typed
source/product rows, `apache_builds` and `citm_catalog`, through the existing
real typed schema, generated DirectBuild output, serde_json as the
Track 2/oracle path, a separate sonic-rs parity lane, checksums, and
full-fixture parity tests. The measured `skinny/RESULTS.md` manifest remains at
the W0 four `real_typed_struct` rows; W2 does not claim six measured
`real_typed_struct A / GO` rows. `canada` was falsified during W2 pre-redress on
full-fixture DirectBuild-vs-serde checksum mismatch over long decimal
coordinates and is routed in `skinny/REDRESS.md` rather than admitted with
weakened proof. W2 left parser/runtime/substrate/direct guard surfaces and
`skinny/RESULTS.md` unchanged; benchmark row-table admission is rejected for
this wave because the W0 run-id validator was already known to reject local
Criterion metadata drift unrelated to W2 source. W2 hardening V3 then found
that the standard checked report gate was deriving required real typed metadata
from the source fixture map rather than W0 measured baseline rows. That fold is
scoped to the report gate: Apache/CITM source/product fixtures no longer
require unadmitted Criterion metadata rows, and the W0 run-id strict validator
remains intact.

W3 is now the active wave.

## 5. Entry Gates

W0 entry:

- G-Alpha closed by user.
- Worktree clean except unrelated user changes already isolated.
- `skinny/RESULTS.md` is the SK-V7 close baseline.
- W0 creates the Lock 14 baseline allowlist: grammar inputs, generated JSON
  output, per-grammar providers/templates, tests, and host/API schema facts.

W1 entry:

- W0 admitted by V11+V12 challenge convergence.
- Every current main row has `SK-V8-open` telemetry in `skinny/RESULTS.md`.
- W1 must keep generated JSON output and parser behavior unchanged unless a
  separate challenged behavior consumer is accepted.
- `gate-json --with-cost-facts` must become the same-wave consumer for
  CostFacts evidence and must reject missing evidence after W1.

W2-W6 entry:

- W0 and W1 admitted.
- The wave plan names exact owner paths, row gates, pre-blocked routes, revert
  protocol, and same-wave consumer.
- High-risk behavior waves receive challenge acceptance before redress.
- Any generic-crate edit includes public API scans, grammar-branch scans,
  primitive/table scans, template/provider-boundary proof, and a non-JSON proof
  for CSS L4, Sheets, and BBNF-self when relevant.

## 6. Exit Condition

SK-V8 closes when:

1. W0 creates `SK-V8-open` with required telemetry on all 38 current main rows.
2. W1 binds CostFacts into `gate-json`.
3. Behavior waves either admit on named row gates or reject with REDRESS.
4. Current typed GO rows maintain GO.
5. Direct digest rows remain guard rows, not product-plane proof.
6. Lock 14 and Lock 15 gates pass.
7. `RESULTS.md`, `REDRESS.md`, and this handoff agree.

## 7. Pre-Blocked Routes

Do not reopen without fresh W0 evidence, same-wave consumer, no-regression
gate, REDRESS citation, and challenge acceptance:

- REDRESS 28+33: Class A NEON tiny-string wiring.
- REDRESS 50-55: SK-V5 UTF-8 fusion routes.
- REDRESS 60-72: SK-V6 retained-parse and direct-materialization rejected
  families.
- REDRESS 80, 82, 83, 84, 88, and 89.
- Historical blocked routes in `skinny/REDRESS.md`, including function-pointer
  dispatch, pair-token fusion, 12-byte token churn, separator elision, generic
  SWAR whitespace, capacity prescan, sidecar prepasses, raw f64 shortcut, and
  orphan primitive admission.

## 8. Pass Omega

Pass Omega remains queued and separate. It does not block G-Alpha for SK-V8 W0.
Omega owns broad lock amendments, path cleanup, top-level CRUD, and canonical
surface refresh. Omega may add enforcement or clarification, but it cannot
weaken Lock 14 or authorize generic JSON policy leaks.

## 9. Status Discipline

Use the triumvirate per wave:

1. Research commit.
2. Plan commit.
3. Redress admit or reject commit.

No role merger. No wave closes without REDRESS or an explicit no-source
telemetry close. Status ticks during waits use:

```text
[sk-v8-W{N}] {phase}: {agents} agents in flight; {returned} returned; ETA {time}
```

Reconcile task state, process state, and artifact mtimes before reporting
"still running".

## 10. G-Alpha Decision

Decision recorded:

- `G-Alpha closed` by user on 2026-05-18T05:26:48Z.

Authority granted: dispatch SK-V8 after `G-Alpha closed`, with W0 and W1 now
closed under their recorded gates and W2 disposition recorded in REDRESS 91.

W3-W6 remain blocked until each later wave satisfies its own entry gates,
plan/challenge requirements, prior-wave disposition requirements, and dispatch
authority.
