# SK-V26 state

This is the sole mutable execution authority for SK-V26.

Last observed: 2026-07-28, America/New_York.

## Current status

| Item | State | Evidence / next boundary |
|---|---|---|
| Tranche formation | ADMITTED | architecture seam adjudicated; immutable formation receipt and W0 git-object receipt verify |
| Formation durability | GIT_DURABLE | immutable formation epochs landed in W0 payload `47d51a190` |
| Formation review tasks | ADJUDICATED | `/root/v26_critic_architecture`; `/root/v26_critic_history`; `/root/v26_adjudicator` |
| Product source mutation | AUTHORIZED_W1_PARSE_THAT_ONLY | never mutate either protected main; BBNF semantic source remains blocked on W1 package |
| W0 truth seal | ADMITTED_GIT_DURABLE | paired payload commits, 63-path overlay, clean dependency pin, replay, and receipts verify |
| W1 parse-that transaction | IN_PROGRESS_BORN_RED | begin external kernel laws and remove runtime dependency backedges; no semantic source edit yet |
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
currently admitted wave. W0 is git-durable at BBNF payload `47d51a190` and
parse-that payload `627c9b46a`; later waves must add their own receipts.

## Exact next action

Verify the immutable formation epoch and W0 git-object seal from this directory:

```sh
shasum -a 256 -c FORMATION-RECEIPT.sha256
cd ../../../.. && node scripts/skv26-w0-seal.mjs --verify
```

The exact next command is the born-RED W1 kernel test:

```sh
cd /Users/mkbabb/Programming/parse-that-skv26/rust &&
CARGO_TARGET_DIR=/Users/mkbabb/Programming/parse-that-skv26/target/skv26-w1 \
cargo test -p parse_that --no-default-features \
  --test kernel --test kernel_allocation
```

The first result must be RED on the absent selected kernel surface. Implement
only the W1 contract after that receipt; do not repair the legacy regex-parser
test by restoring competing syntax authority.

## Handoff rule

At a session wall, update only:

1. the table above;
2. exact in-flight task IDs;
3. immutable artifact paths and hashes;
4. the exact next command.

Narrative progress does not advance a state.
