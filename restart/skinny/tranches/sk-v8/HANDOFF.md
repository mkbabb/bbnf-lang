# Handoff SK-V8

Date: 2026-05-17.

Status: SK-V8 Pass Alpha packet materialized. G-Alpha sign-off is required
before dispatch. If G-Alpha closes, only W0 is dispatchable from this packet.
W1-W6 require W0 closure and plan augmentation before implementation.

## 1. Read First

1. `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
2. `restart/skinny/tranches/sk-v8/SPEC.md`
3. `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
4. `restart/skinny/tranches/sk-v8/research/alpha/`
5. `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/`
6. `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/` (the S-P2 substrate-ceiling cohort, SC-1..SC-6)
7. `skinny/RESULTS.md`
8. `skinny/REDRESS.md`
9. `restart/prompts/skinny/PASS-2-RESEARCH.md` + `PASS-3-SYNTHESIS-PLAN.md`
10. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`

## 2. Current Measured State

Current authority is `skinny/RESULTS.md` after SK-V7 W10c. The report remains
`N-direct / NoGo`.

| Family | State |
|---|---|
| `parse_only` | 17 rows, all `K / NO-GO` |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO` |
| `real_typed_struct` | 4 `A / GO` |

Current telemetry caveats:

- Every current main row uses `Strictness=deferred`.
- `Delta vs SK-V6` is non-derivable in the current report.
- `Hot leaf` is a placeholder: `unprofiled in W0b; no kernel prescription from
  this row`.
- C++ comparator values are sidecar planning signals unless refreshed under a
  later same-run/freshness gate.

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
explicit user pin. V1, V2, V3, and V4 did not converge, so a future V5 ACCEPT
cycle is still only the first ACCEPT cycle after a REVISE unless followed by
another ACCEPT cycle or pinned by the user. The cohort does not change the
G-Alpha-W0-only constraint; it nominates a lead W3 hypothesis so W0 telemetry
confirms or falsifies the finding executably rather than discovering it from
scratch. SC-1..SC-6 authorize no W3 plan by themselves: W3 remains blocked on
W0/W1 closure, fresh plan owner paths, same-wave production consumer, revert
protocol, measurement thresholds, measured-path strict validation proof, and
challenge acceptance. `tape_vs_tape` is W0/W1 telemetry only, not a W3
production consumer. For this packet it is explicitly a routed residual, not
default W0/W1 scope; adding it later requires a named owner/LOC/test/rerun plan
before it can consume wave budget.

## 4. Dispatch Posture

| Wave | Name | Status |
|---|---|---|
| W0 | Baseline Profile And Telemetry Lock | Dispatchable after G-Alpha |
| W1 | CostFacts Gate Binding | Conditional on W0 close |
| W2 | Typed Product Plane Expansion | Conditional on W0/W1 plan update |
| W3 | Profile-Selected Parse Candidate — lead candidate: tape ⊕ structural-projection union (S-P2 cohort) | Conditional on W0/W1 challenge |
| W4 | Direct Guard Triage | Conditional on W0/W1 plan update |
| W5 | Grammar-Neutral Audit And Lock 14 Preservation | Conditional on W1-W4 close |
| W6 | Close And Alpha Feedback | Conditional on all prior dispositions |

W0 is telemetry-only. If W0 changes parser, scanner, SIMD, asm, codegen
behavior, or product-plane behavior, reject it.

## 5. Entry Gates

W0 entry:

- G-Alpha closed by user.
- Worktree clean except unrelated user changes already isolated.
- `skinny/RESULTS.md` is the SK-V7 close baseline.
- W0 creates the Lock 14 baseline allowlist: grammar inputs, generated JSON
  output, per-grammar providers/templates, tests, and host/API schema facts.

W1-W6 entry:

- W0 admitted.
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

The next user decision is:

- `G-Alpha closed`: dispatch SK-V8 W0.
- `G-Alpha revise`: revise the named packet sections.

No SK-V8 implementation wave should dispatch before that decision.
