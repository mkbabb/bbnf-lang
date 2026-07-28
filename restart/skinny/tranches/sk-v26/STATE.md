# SK-V26 state

This is the sole mutable execution authority for SK-V26.

Last observed: 2026-07-28, America/New_York.

## Current status

| Item | State | Evidence / next boundary |
|---|---|---|
| Tranche formation | ADMITTED | architecture seam adjudicated; immutable formation receipt and W0 git-object receipt verify |
| Formation durability | GIT_DURABLE | immutable formation epochs landed in W0 payload `47d51a190` |
| Formation review tasks | ADJUDICATED | `/root/v26_critic_architecture`; `/root/v26_critic_history`; `/root/v26_adjudicator` |
| Product source mutation | AUTHORIZED_W2_PAIRED_SELECTED_SLICE | parse-that may add only the typed module/resource boundary required by W2; BBNF may replace only the selected semantic spine; never mutate either protected main |
| W0 truth seal | ADMITTED_GIT_DURABLE | paired payload commits, 63-path overlay, clean dependency pin, replay, and receipts verify |
| W1 parse-that transaction | ADMITTED_GIT_DURABLE | parse-that `9c9714dfd`; BBNF evidence `ad7aec5c6`; 70 workspace laws, 7 bridge laws, clean packages, 148-path crater, tri-fold ACCEPT |
| W2 BBNF semantic spine | IN_PROGRESS_BORN_RED | first add the missing typed parse-that module/resource boundary, then genuine emitted direct control in both sink modes; failure reopens W1 |
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
parse-that payload `627c9b46a`. W1 is git-durable at parse-that
`9c9714dfd` and BBNF evidence payload `ad7aec5c6`. Later waves must add their
own receipts.

## Exact next action

Verify the W1 subjects and replay from this directory:

```sh
git -C /Users/mkbabb/Programming/parse-that-skv26 rev-parse HEAD HEAD^{tree}
git -C /Users/mkbabb/Programming/bbnf-lang-skv26 rev-parse \
  ad7aec5c6b3731fd6d57008d051fb0799377ae22 \
  ad7aec5c6b3731fd6d57008d051fb0799377ae22^{tree}
```

The exact next command is the born-RED W2 parse-that carrier test:

```sh
cd /Users/mkbabb/Programming/parse-that-skv26/rust &&
CARGO_TARGET_DIR=/Users/mkbabb/Programming/parse-that-skv26/target/skv26-w2 \
cargo test -p parse_that --no-default-features --test module_result --locked
```

The first result must be RED on the absent W2 carrier surface. Implement only
the smallest checked `ModuleResult` and typed resource-boundary delta justified
by W2. Then make genuine BBNF-emitted literal, regex, sequence, choice, repeat,
and EOF control green in both discarding and retaining sink modes. Do not
restore diagnostics compatibility features or adapt the legacy fleet.

## Handoff rule

At a session wall, update only:

1. the table above;
2. exact in-flight task IDs;
3. immutable artifact paths and hashes;
4. the exact next command.

Narrative progress does not advance a state.
