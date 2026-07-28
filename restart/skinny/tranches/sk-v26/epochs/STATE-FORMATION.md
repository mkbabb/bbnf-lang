# SK-V26 state

This is the sole mutable execution authority for SK-V26.

Last observed: 2026-07-28, America/New_York.

## Current status

| Item | State | Evidence / next boundary |
|---|---|---|
| Tranche formation | PROVISIONAL_ACCEPT | architecture seam adjudicated; receipt generated; W0 audit/authority receipts still owed |
| Formation durability | FILESYSTEM_ONLY | untracked; verify with `shasum -a 256 -c FORMATION-RECEIPT.sha256` |
| Formation review tasks | ADJUDICATED | `/root/v26_critic_architecture`; `/root/v26_critic_history`; `/root/v26_adjudicator` |
| Product source mutation | FORBIDDEN | this session is tranche development only |
| W0 truth seal | NOT STARTED | requires clean BBNF/PT worktrees, receipts, consumer census, and authority lint |
| W1 parse-that transaction | NOT STARTED | future separate worktree from `ef10d5b`; main remains untouched |
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

No bulk cleanup, regeneration, formatting, staging, commit, revert, or deletion is
authorized by this state file. This SK-V26 packet is not git-durable and must not
be called sealed.

## Exact next action

Verify formation from this directory with:

```sh
shasum -a 256 -c FORMATION-RECEIPT.sha256
```

W0 must then produce `artifacts/W0-EPOCH.tsv`, `artifacts/W0-CROSSING.tsv`,
`artifacts/W0-AUTHORITY.tsv`, `artifacts/W0-AUDIT-MANIFEST.tsv`, and
`artifacts/W0-REPLAY.md`. Until those exist, the next product command is **none**.

## Handoff rule

At a session wall, update only:

1. the table above;
2. exact in-flight task IDs;
3. immutable artifact paths and hashes;
4. the exact next command.

Narrative progress does not advance a state.
