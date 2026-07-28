# SK-V26 state

This is the sole mutable execution authority for SK-V26.

Last observed: 2026-07-28, America/New_York.

## Current status

| Item | State | Evidence / next boundary |
|---|---|---|
| Tranche formation | PROVISIONAL_ACCEPT | architecture seam adjudicated; receipt generated; W0 audit/authority receipts still owed |
| Formation durability | IMPORTED_FILESYSTEM | immutable formation epoch verifies at `epochs/STATE-FORMATION.md` |
| Formation review tasks | ADJUDICATED | `/root/v26_critic_architecture`; `/root/v26_critic_history`; `/root/v26_adjudicator` |
| Product source mutation | AUTHORIZED_ISOLATED_ONLY | never mutate either protected main |
| W0 truth seal | IN_PROGRESS | isolated worktrees created; epoch/crossing/audit/authority/corpus receipts materialized |
| W1 parse-that transaction | BLOCKED_ON_W0 | separate `codex/sk-v26-parse-that` worktree exists; no semantic source edit yet |
| W2 BBNF semantic spine | NOT STARTED | blocked on W0 and the W1 crossing contract |
| W3 grammar-neutral vertical | NOT STARTED | blocked on W2 |
| W4 profile/cutover | NOT STARTED | blocked on W3 semantic parity |
| Consumer/playground lane | FORMATION-ONLY | explicitly off the core parser critical path |

## Protected epochs

- BBNF main: `af15f63e0`, ahead of origin, 204 tracked files changed at audit
  time, plus broad untracked SK-V25/source evidence.
- Parse-that main: `ef10d5b`, matching origin but dirty at audit time.
- Value: branch `tranche-u`, HEAD `f36f780c`, moving and heavily dirty; read-only
  consumer evidence.
- SK-V25: approximately 1.7 GiB on disk; only three files tracked at BBNF HEAD.

No operation may mutate either protected main. Cleanup, regeneration, staging,
commit, revert, or deletion remains bounded to the isolated branches and the
currently admitted wave. This packet is not git-durable and must not be called
sealed.

## Exact next action

Verify the immutable formation epoch from this directory with:

```sh
shasum -a 256 -c FORMATION-RECEIPT.sha256
```

The four primary W0 tables now exist. The exact next command is:

```sh
cd ../../../.. &&
node scripts/skv26-authority-lint.mjs &&
node scripts/skv26-w0-audit-manifest.mjs &&
node scripts/skv26-w0-dirty-manifest.mjs
```

Then finish `artifacts/W0-REPLAY.md`, replay it from fresh shells, and adjudicate
the remaining RED rows before admitting W1.

## Handoff rule

At a session wall, update only:

1. the table above;
2. exact in-flight task IDs;
3. immutable artifact paths and hashes;
4. the exact next command.

Narrative progress does not advance a state.
