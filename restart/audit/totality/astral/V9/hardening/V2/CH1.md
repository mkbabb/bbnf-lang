# Pass Omega V9 Hardening V2 - CH1 Coherence / Applyability Audit

Date: 2026-05-28.
Worker: CH1.
Scope: folded V9 source packet coherence and applyability.
Write target: `restart/audit/totality/astral/V9/hardening/V2/CH1.md`.

## Verdict

ACCEPT.

The folded V9 source packet is coherent and applyable for CH1. The prior
malformed MASTER exact-diff failure is repaired: `master-plan-diff.md` is now an
anchored operation list, not a pseudo unified diff, and the named anchors exist
in current `restart/MASTER-PLAN.md`. Active V9 source files do not retain stale
T-P2 V5 tokens or the retired MASTER/SPEC exact-diff surface language. The
locks diff remains a proposed G-Omega-gated unified diff, applies cleanly as a
candidate patch, and explicitly preserves both the 16-lock count and the exact
five-BackendShape canon before authorization.

## Commands / Evidence

Worktree/HEAD check:

```sh
git status --short
git rev-parse HEAD
```

Observed HEAD: `9d336c6062898b0ce70b4df6787c3538aa7f74b9`.
The worktree already contained unrelated dirty files outside this report path;
none were modified by this audit.

Required/actual V9 packet read:

```sh
nl -ba restart/audit/totality/astral/V9/ΩA-coherence-audit.md
nl -ba restart/audit/totality/astral/V9/ΩB-skinny-lessons.md
nl -ba restart/audit/totality/astral/V9/ΩC-locks-amendments.md
nl -ba restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md
nl -ba restart/audit/totality/astral/V9/ΩE-skinny-corpus.md
nl -ba restart/audit/totality/astral/V9/ΩF-migration-handoff.md
nl -ba restart/audit/totality/astral/V9/locks-diff.md
nl -ba restart/audit/totality/astral/V9/master-plan-diff.md
nl -ba restart/audit/totality/astral/V9/hardening/CH1.md
nl -ba restart/audit/totality/astral/V9/hardening/CONSOLIDATED.md
nl -ba restart/MASTER-PLAN.md
nl -ba restart/locks/LOCKS.md
nl -ba restart/HANDOFF.md
```

Note: the requested names
`restart/audit/totality/astral/V9/ΩB-lock-invariant-audit.md` and
`restart/audit/totality/astral/V9/ΩC-locks-diff.md` are absent in the current
packet. The active V9 equivalents are `ΩB-skinny-lessons.md` and
`ΩC-locks-amendments.md`; both were read.

Stale token / retired surface scan:

```sh
rg -n "HARDENING-T-P2-V5|T-P2 V5|T-P2-V5|CRUD/SPEC|SPEC/dispatch|master/spec diff|SK-V15 SPEC Proposed Diff|MASTER-PLAN Exact Proposed Diff|SK-V15 SPEC diff|Exact Proposed Diff" restart/audit/totality/astral/V9/*.md
```

Result: no matches.

MASTER operation anchor scan:

```sh
rg -n "^### §13\.3 SK-V14 W0\.\.W11 Receiver Block|^### §13\.4 New Waves From T-P3 V4 LOCK|the refusal entry IS its consumer per CH6 anti-paper-close discipline\.|^## 14\. Tranche I - Recovery, Incremental, LSP|^## 25\. Implementation Order|Complete Pass Omega convergence|SK-V13 source/generated/gate/result waves" restart/MASTER-PLAN.md
```

Relevant results:

```text
751:### §13.3 SK-V14 W0..W11 Receiver Block (per MP-3B-V1-D02 + MP-NW-SK14-W0..W11-INHERIT)
837:### §13.4 New Waves From T-P3 V4 LOCK (14 NEW; per MP-NW-01..14)
869:the refusal entry IS its consumer per CH6 anti-paper-close discipline.
871:## 14. Tranche I - Recovery, Incremental, LSP
1218:## 25. Implementation Order
1223:2. Complete Pass Omega convergence and G-Omega before applying any V1.1
1241:Those documents are inputs. SK-V13 source/generated/gate/result waves remain
```

Lock diff applyability / invariant checks:

```sh
awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/locks-diff.md | git apply --check -
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
rg -n "G-Omega|preserves the 16|adds no directive|sixth shape|not a lock-count change|not a new" restart/audit/totality/astral/V9/locks-diff.md restart/audit/totality/astral/V9/ΩC-locks-amendments.md
```

Results: `git apply --check` succeeded; current lock count is `16`; the lock
proposal states it is G-Omega gated, preserves the 16 numbered locks, preserves
the five `BackendShape` variants, adds no directive/BIR/substrate/public
substrate API/retained sidecar/lock/lock retirement/sixth shape, and keeps
`FactStream` outside `BackendShape` (`locks-diff.md:3`-`11`,
`locks-diff.md:45`-`49`, `locks-diff.md:63`, `ΩC-locks-amendments.md:10`-`23`).

## Findings

No REVISE findings.

CH1-ACCEPT-01: `master-plan-diff.md` is no longer a malformed exact unified
diff. It declares itself a mechanically consumable operation list and states it
contains no extractable `diff --git` block (`master-plan-diff.md:12`-`15`).
It contains no pseudo SK-V15 SPEC proposed diff or MASTER exact proposed diff
language. Its anchors exist in current `restart/MASTER-PLAN.md` at lines 751,
837, 869/871, 1218, 1223, and 1241.

CH1-ACCEPT-02: active V9 source files under
`restart/audit/totality/astral/V9/*.md` contain no `HARDENING-T-P2-V5`,
`T-P2 V5`, or `T-P2-V5` tokens.

CH1-ACCEPT-03: active V9 source files contain no stale `CRUD/SPEC`,
`SPEC/dispatch`, `master/spec diff`, `SK-V15 SPEC Proposed Diff`, or
`MASTER-PLAN Exact Proposed Diff` surfaces. The remaining `SPEC/DISPATCH`
wording in `master-plan-diff.md:202`-`215` is an explicit read-no-op instruction
for the locked SK-V15 SPEC/DISPATCH surfaces, not the retired authorized
SPEC/dispatch edit surface.

CH1-ACCEPT-04: `locks-diff.md` is coherent enough for G-Omega review. It is
explicitly proposed and G-Omega gated (`locks-diff.md:3`), inserts only an
addendum before the existing `## v+1 Governance Boundary`, and its extractable
diff applies cleanly to current `restart/locks/LOCKS.md`.

CH1-ACCEPT-05: `locks-diff.md` does not silently mutate the lock count or
five-shape canon before authorization. Current `restart/locks/LOCKS.md` has 16
numbered locks. The diff text says it preserves 16 numbered locks and the exact
five variants `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`;
it also blocks a sixth shape, new directive, or new BIR variant
(`locks-diff.md:6`-`11`, `locks-diff.md:47`, `locks-diff.md:63`,
`locks-diff.md:67`).

## Acceptance Rationale

The CH1 hardening target is applyability/coherence, not authorization to edit
live governance surfaces. On that standard, the folded V9 source packet is ready
for G-Omega review:

- MASTER changes are expressed as anchored operations against the current
  `restart/MASTER-PLAN.md`.
- Stale T-P2 V5 and old exact-diff/spec-dispatch tokens are absent from active
  V9 source files.
- LOCKS changes remain proposed-only, mechanically checkable, and explicitly
  preserve the lock count and five-shape canon.
- No live edit was made outside this report.
