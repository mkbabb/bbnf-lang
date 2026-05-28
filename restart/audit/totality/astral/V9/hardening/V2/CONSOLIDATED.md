# Pass Omega V9 CHALLENGE V2 Consolidated

Date: 2026-05-28.
Cycle: V2.
Source packet: `9d336c606` (`docs(omega-v9): fold V1 hardening into V2 source packet`).
Disposition: `ACCEPT`.
Acceptance: 6 / 6 lenses ACCEPT; zero orphan `REVISE`; zero `REJECT`.

## Lens Results

| Lens | Disposition | Finding |
|---|---:|---|
| CH1 Correctness / applyability | `ACCEPT` | `master-plan-diff.md` is now an anchored operation list, active V9 source files have no stale T-P2 authority tokens or retired MASTER/SPEC diff language, and `locks-diff.md` is G-Omega-gated and apply-checks clean. |
| CH2 Invariant / lock surface | `ACCEPT` | `LOCKS.md` remains 16 numbered locks, `BackendShape` remains exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, `FactStream` remains outside `BackendShape`, Pattern H is 67 with the required `-mindepth 2` command, and Apple M5 Max/aarch64-only admission is preserved. |
| CH3 Regression / wave graph | `ACCEPT` | Folded V9 routes to actual SK-V15 W0-W11 after G-Omega, keeps SK-V15 SPEC/DISPATCH read-only for V9, preserves PRUNE-before-REBUILD, blocks W12/challenge overflow/SK-V16 deferral, and marks SK-V14 V8/W5B authority historical. |
| CH4 CRUD / gate / scope | `ACCEPT` | G-Omega V9 remains mandatory before CRUD, CRUD scope is explicit and document-bounded, CRUD-2 is MASTER-PLAN only, the handoff directive uses master-plan edit operations, and CRUD-3 LOCKS is a concrete G-Omega-gated amendment. |
| CH5 Hidden coupling / overfit | `ACCEPT` | V9 does not re-admit CSS broadcast, `CSS_GENERATED_RS`, brace-counter, or fact-stream-only CSS proof; typed CSS value and same-workload `cssparser` retime stay routed to W5/W6; Lock14/16, Pattern H, Decision, lowerers, and FNV stay routed to SK-V15 waves. |
| CH6 Source-map / evidence hygiene | `ACCEPT` | Current authority files exist, commit anchors resolve, active V9 source files no longer carry absent T-P2 authority tokens, Lock 14 source-map cleanup is CRUD-6-only, and evidence commands reproduce at HEAD. |

## V1 Defect Closure

V1 carried two open defects:

1. CH1 found `master-plan-diff.md` was labelled an exact proposed diff but was
   not a mechanically applyable unified patch; it also carried a fake SK-V15
   SPEC no-op diff and literal stale T-P2 authority tokens in active packet
   prose.
2. CH4 found ambiguous CRUD/SPEC language and missing consolidated scope that
   could imply SK-V15 SPEC/DISPATCH, source, generated output, gates,
   `skinny/RESULTS.md`, or `skinny/REDRESS.md` movement during V9 CRUD.

The V2 fold closes both:

- `master-plan-diff.md` is an operation list for `restart/MASTER-PLAN.md`, not
  an exact unified diff.
- SK-V15 `SPEC.md` and `DISPATCH-PROMPT.md` are explicit read/no-op surfaces for
  V9.
- The authorized touch scope is consolidated and forbids source, generated
  output, gates, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and SK-V15
  SPEC/DISPATCH edits during V9 CRUD.
- Active V9 source files no longer contain `HARDENING-T-P2-V5`, `T-P2 V5`, or
  `T-P2-V5` tokens.

## Verification

Commands run across the V2 packet:

```sh
git diff --check -- restart/audit/totality/astral/V9/hardening/V2 restart/audit/totality/astral/V9/hardening/CH6.md
rg -n "^(## Verdict|Verdict:|Disposition:|ACCEPT|REVISE|REJECT|No revise|No REVISE)" restart/audit/totality/astral/V9/hardening/V2/*.md
rg -n "HARDENING-T-P2-V5|T-P2 V5|T-P2-V5" restart/audit/totality/astral/V9/*.md
rg -n "CRUD/SPEC|SPEC/dispatch surfaces|master/spec diff|SK-V15 SPEC Proposed Diff|MASTER-PLAN Exact Proposed Diff|diff --git a/restart/skinny/tranches/sk-v15/SPEC.md" restart/audit/totality/astral/V9/*.md
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/locks-diff.md | git apply --check -
```

Observed results:

- `git diff --check`: clean.
- Stale T-P2 authority token scan over active V9 source files: no hits.
- Retired CRUD/SPEC and pseudo SPEC diff phrase scan over active V9 source
  files: no hits.
- Lock count: `16`.
- Pattern H census: `67`.
- V9 `locks-diff.md` extraction: `git apply --check` exits clean.

## G-Omega Readiness

Pass Omega V9 CHALLENGE V2 converges. The G-Omega V9 packet may be surfaced with
these proposed post-authorization operations:

- CRUD-3 applies the explicit `LOCKS.md` addendum from `locks-diff.md`, preserving
  the 16-lock count and five-shape `BackendShape` canon.
- CRUD-1 aligns `ARCHITECTURE.md` implementation-status authority without adding
  a substrate, directive, BIR variant, public API, sidecar, lock, or shape.
- CRUD-2 applies only the `MASTER-PLAN.md` operations in `master-plan-diff.md`;
  SK-V15 SPEC/DISPATCH remain read-only for V9.
- CRUD-4 aligns `HANDOFF.md` and `MIGRATION.md` to SK-V15 W0-W11 current
  authority and removes stale SK-V14 W5B/Omega V8 next-dispatch routing.
- CRUD-5 performs limited alignment in the six skinny corpus docs only.
- CRUD-6 records the authorization, verification, and source-map cleanup.

Until G-Omega V9 is authorized, all V9 CRUD operations remain proposal-only.
