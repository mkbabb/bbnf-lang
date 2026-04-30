# AZ-III - Progress Log

**Status**: planned continuation of AZ-II.
**Opened by**: AZ-II continuation handoff on 2026-04-30.
**Parent**: [`AZ-III.md`](AZ-III.md).

AZ-III opens because AZ-II is closed as a continuation handoff, not a
terminal green close. The carried work is explicit: O5 close evidence,
O6 semantic/performance truth, O7 close conversion, and the audit-found
grammar-general fact/type/CSP/projection authority substrate.

## Opening Evidence

- AZ-II O0-O4 landed.
- O5 implementation partially landed, including `crates/tape` deletion
  work, but no refreshed green O5 close packet exists.
- Latest audit evidence reports no-default build repair is stale-good,
  while `cargo xtask regen --check` remains the active O5 blocker.
- O6 and O7 did not run.
- The only legitimate substrate expansion is grammar-general authority
  over facts, type obligations, CSP decisions, and projection emission.

## Wave Status

| Wave | Status | Notes |
|---|---|---|
| W0 - Quarantine and Dispatch Repair | in_progress | state ledger, commit history repair, and lint/format baseline recorded |
| W1 - O5 Reclose | planned | AZ-II.O5 reclose |
| W2 - Semantic Parity and Bootstrap Canonicalization | planned | semantic parity and BBNF self-host canonicalization |
| W3 - Fact, Type, CSP, and Projection Authority | planned | fact/type/CSP/projection authority |
| W4 - Benchmark, Profile, and Workspace Truth | planned | bench/profile/workspace truth |
| W5 - Terminal Close and Handoff | planned | terminal close and BA/BB handoff |

## Current Blockers

1. Main worktree is dirty with the restored AZ-II implementation/source
   slice and two untracked docs artefacts; implementation dispatch remains
   blocked until W0 slices or routes that work.
2. AZ-II O5 close artifact is stale and must be regenerated under W1.
3. Root, parse-that, and pprint format checks are green, but clippy is red
   across all three workspaces; see `audit/W0-state-ledger.txt`.
4. Parity and benchmark truth are stale or partial until W2/W4.
5. BA/BB remain blocked until W5 terminal close.

## 2026-04-30 - W0 Quarantine Evidence

Recorded W0 state, history repair, and dispatch packet evidence:

- `audit/W0-state-ledger.txt`
- `audit/W0-commit-repair-plan.md`
- `audit/W0-dispatch-packets.md`

Root commit history from `53d3e6b2..HEAD` was rewritten message-only to
replace terse AZ-II subjects and missing bodies with concrete scopes and
evidence-bearing bodies. The backup branch is
`codex/az-history-before-reword-20260430-114057`.
