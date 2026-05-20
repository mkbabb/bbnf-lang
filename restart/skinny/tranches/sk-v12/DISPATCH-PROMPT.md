# SK-V12 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V12. It binds to the SK-V12 packet at
`restart/skinny/tranches/sk-v12/`. Each wave of the SK-V12 SPEC is executed by
one triumvirate per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

Status: S-P3 V2 draft. Do not dispatch behavior waves until S-P3 CHALLENGE
converges and the orchestrator promotes this packet.

## Required Reading

Read in order:

1. `restart/prompts/ORCHESTRATOR.md`.
2. `restart/prompts/skinny/PASS-1-PROFILE.md`.
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
5. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
6. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
7. `restart/skinny/tranches/sk-v12/SPEC.md`.
8. `restart/skinny/tranches/sk-v12/HANDOFF.md`.
9. `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
10. `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`.
11. `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`.
12. `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
13. The S-P2 cohort:
    - `research/p2/p2a-sota-teardown.md`
    - `research/p2/p2b-dav1d-process.md`
    - `research/p2/p2c-arch-esoterica.md`
    - `research/p2/p2d-substrate-tape.md`
    - `research/p2/p2e-parse-that-gaps.md`
    - `research/p2/p2f-grammar-neutral.md`
14. `skinny/RESULTS.md`.
15. `skinny/REDRESS.md` through REDRESS 120.

## Wave Manifest

| Wave | SPEC section | Title | Dispatch status | LOC / risk | Wall cap | Redress cap |
|---|---|---|---|---:|---:|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | First after S-P3 convergence | <=180 report/gate/test/doc LOC; low-medium | <=90 min | <=75 min |
| W1 | Section 4 | Generated Non-JSON Baseline | Conditional on W0 close | <=520 CSS / <=480 Sheets / <=460 BBNF-self; high | <=90 min | <=75 min |
| W2 | Section 5 | Selected-Baseline Measured Intervention | Conditional on W1 admit | <=430 source/test/gate LOC; high | <=90 min | <=75 min |
| W3 | Section 6 | Conditional JSON Direct Companion | Conditional on W1/W2 disposition plus material reopen gate | <=300 source/test/gate LOC or 0 if entry blocks; high | <=90 min | <=75 min |
| W4 | Section 7 | Close And Alpha Feedback | Conditional on W0-W3 dispositions | <=120 docs/report/gate LOC; medium | <=90 min | <=75 min |

The order is firm. W1 is the first material behavior wave and admits exactly
one generated non-JSON baseline in this preference order: CSS L4 declaration
values, Sheets formula, BBNF-self grammar. W2 consumes the W1 row and measures
one grammar-generalized intervention on that row. W3 is conditional; it does
not run JSON direct behavior unless the SPEC Section 6 entry gate passes.

## Per-Wave Triumvirate Protocol

Every wave is one triumvirate per `SKINNY-TRIUMVIRATE.md`: research, plan,
optional/mandatory CHALLENGE, and redress in distinct commits.

### Phase 1 - Research

- Up to six parallel research agents on disjoint scope rows; 30 min cap each.
- Research edits no source and writes under
  `restart/skinny/tranches/sk-v12/research/`.
- Commit: `docs(sk-v12-wave{W}-research): archive {scope} cohort`.

### Phase 2 - Plan

- One or two plan agents; 30 min cap.
- The plan selects one SPEC intervention and names owner paths, entry gate,
  exit gate, scalar-reference state, parity/checkasm state, micro-proof,
  same-wave consumer, LOC budget, risk class, wall cap, redress cap, revert
  protocol, guard rows, and
  pre-blocked routes.
- A W2 plan also includes the SPEC Section 5 five-part cost table: scalar
  reference LOC, parity/checkasm LOC, microbench LOC, generated consumer LOC,
  and report/gate LOC.
- Plan agents edit no source.
- Commit: `docs(sk-v12-wave{W}-plan): select {intervention}`.

### Phase 2.5 - CHALLENGE

Mandatory for W1, W2, and any W3 behavior dispatch. Optional for W0 and W4
unless their plans touch gate semantics or source. Six lenses review
correctness, generality, regression, cost, hidden coupling, and anti-paper
close. A REJECT or unresolved REVISE routes the wave back to plan.

### Phase 3 - Redress

- One redress agent; single implementation thread.
- Every wave has a <=75 min redress cap. W0 and W4 may carry a separate <=90
  min wall cap for gate/docs overhead outside redress.
- The redress agent implements only the SPEC owner paths. Any other source path
  returns REVISE before editing.
- Every primitive or generated path must include its same-wave consumer and
  measurement gate.
- Commit on PASS: `feat(sk-v12-wave{W}): admit {intervention}`.
- Commit on FAIL or BLOCKED: `docs(sk-v12-wave{W}-redress): reject {intervention}`
  or `docs(sk-v12-wave{W}-redress): block {intervention}`, with REDRESS
  evidence and `/tmp/skv12-wave{W}-rejected.patch` when a patch was attempted.

## Falsifiability Gates

The SPEC is the single source for gate details:

- `G-W0-SK-V12-OPEN` (SPEC Section 3)
- `G-W1-GENERATED-NONJSON-BASELINE` (SPEC Section 4)
- `G-W2-SELECTED-NONJSON-INTERVENTION` (SPEC Section 5)
- `G-W3-CONDITIONAL-JSON-COMPANION` (SPEC Section 6)
- `G-W4-CLOSE` (SPEC Section 7)

Load-bearing gate facts:

- W1 admits one generated non-JSON baseline only. The selected row must have
  generated Track 1, independent oracle or Track 2, strict output equality,
  Track 1 >= 1 Mbps, oracle/Track 2 >= 1 Mbps, sample count >= 30, provenance,
  run/build/host/sample telemetry, and gate consumption.
- W2 admits one measured intervention only on the W1 row. Track 1 must be >=
  `ceil(W1_baseline_track1_mbps * 1.01)`.
- W3 has no default behavior authority. It dispatches only with fresh material
  evidence beyond REDRESS 114-119 and CHALLENGE acceptance.
- parse_only rows never count as SK-V12 SOTA admission.
- All behavior waves preserve the 4 direct JSON guards and 7 typed JSON guards
  named in SPEC Section 0.5.

## Pre-Blocked Routes

The full ledger is SPEC Section 8. Load-bearing blocks:

- REDRESS 96/97/98 W3 union/event/class-column/streaming-cursor/class-lane and
  sidecar-substrate family.
- REDRESS 28/33 active TBL/NEON tiny-string dispatch, with REDRESS 72 scalar
  cap widening not authorizing that rejected active-dispatch path.
- REDRESS 70/71 typed-output boundary: no hand-authored typed sink, direct
  digest proof, hidden directive/BIR extension, hidden host schema, or
  benchmark-private Track 1 parser.
- REDRESS 111 report-lane evidence as a generated baseline.
- REDRESS 112/113 non-JSON baseline blocker and intervention entry block as a
  future-phase promise.
- REDRESS 114-119 JSON direct residual routes: numeric slot, container-tail,
  bounded string span, escaped segment, output digest host-sink, and residual
  fixpoint.
- REDRESS 120 close authority: SK-V12 solves generated non-JSON baseline first.
- New directive, BIR variant, `BackendShape`, public substrate API, retained
  sidecar, parser-owned scratch/facts, parser-owned structural projection,
  retained structural cursor or cursor list, aux density table, aux projection
  column, event side vector, whitespace bitmap, retained class lane,
  structural-position vector, decoded-byte sidecar, renamed scanners retaining
  facts outside the single tape/direct sink contract, generic JSON policy, x86
  target, stale sidecar strictness, and parse_only SOTA admission.

Any wave adjacent to a pre-block must cite the REDRESS entry, state the
material differential, and pass CHALLENGE before redress.

## Non-Negotiables

- No primitive ships without scalar reference, parity/checkasm where
  applicable, micro-proof, and same-wave consumer.
- No behavior source change without a named row gate.
- No generated non-JSON close by prose, report fixture, hand-only parser, or
  stale witness module.
- No JSON direct work before W1/W2 generated non-JSON priority resolves.
- No producer-only telemetry. Every emitted field is consumed by a same-wave
  gate.
- No new outcome enum value.
- No generic-crate JSON policy.
- No W3 substrate route, retained cursor/list, aux projection, or decoded-byte
  sidecar route.
- No parse_only admission.
- No x86 implementation work.
- Research, plan, CHALLENGE, redress, and close remain separate commits.

## Status Discipline

Before any status reply, reconcile agent status, running cargo/rustc processes,
artefact mtimes, and dirty worktree state. Stage only the intended wave slice.
Preserve unrelated dirty or staged work.

Every dispatch carries an explicit minute cap. At 0.9x the cap, commit or
record the blocking state. At the cap, halt and surface the decision point.

## Convergence And Escalation

SK-V12 converges when W0-W4 have admitted, rejected, or routed with measurement
or gate evidence, the SPEC Section 0 close condition holds, and close documents
agree. Convergence triggers G-Alpha for SK-V12 -> SK-V13.

If W1 proves no generated non-JSON baseline can be created inside the accepted
owner surface, close may be a measured `BLOCKED` route. If W1 admits but W2
misses the >=1% lift, W2 records a measured rejection and W4 routes the
remaining intervention family to Alpha. If W3 lacks material reopen evidence,
it records a routed block rather than reopening JSON direct work.

If any gate cannot be made measurable, escalate as `BLOCKED` naming the
unresolved gate. If the bracket exceeds 12 waves without convergence, escalate
as `BLOCKED: skinny bracket V12 exceeded 12 waves; user adjudicate scope or
abandon`.
