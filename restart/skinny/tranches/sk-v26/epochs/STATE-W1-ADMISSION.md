# SK-V26 state

This is the immutable W1-admission snapshot. `../STATE.md` remains the sole
mutable execution authority.

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

## Fixed W1 subjects

- parse-that commit `9c9714dfdfa6d33b70f5738c311a9abccb4a1bdf`,
  tree `76141a76dee446249233056e94ea9b043968bdd4`;
- BBNF evidence payload `ad7aec5c6b3731fd6d57008d051fb0799377ae22`,
  tree `a6b75de6b07995a741d455d81acc246f7067e061`.

Both protected main worktrees remain outside mutation authority.

## Exact next action

```sh
cd /Users/mkbabb/Programming/parse-that-skv26/rust &&
CARGO_TARGET_DIR=/Users/mkbabb/Programming/parse-that-skv26/target/skv26-w2 \
cargo test -p parse_that --no-default-features --test module_result --locked
```

The first result is required to be RED on the absent W2 carrier. The next BBNF
gate is genuine emitted direct control in both sink modes; failure reopens W1.
