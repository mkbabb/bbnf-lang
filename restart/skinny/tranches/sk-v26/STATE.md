# SK-V26 state

This is the sole mutable execution authority for SK-V26.

Last observed: 2026-07-29, America/New_York.

## Current status

| Item | State | Evidence / next boundary |
|---|---|---|
| Tranche formation | ADMITTED | architecture seam adjudicated; immutable formation receipt and W0 git-object receipt verify |
| Formation durability | GIT_DURABLE | immutable formation epochs landed in W0 payload `47d51a190` |
| Formation review tasks | ADJUDICATED | `/root/v26_critic_architecture`; `/root/v26_critic_history`; `/root/v26_adjudicator` |
| Product source mutation | AUTHORIZED_W3_IMPLEMENTATION_FROM_BORN_RED | BBNF payload `dadd6fa77` freezes the accepted instance/syntax/Pratt/replay/public-witness RED boundary; implement only that generic surface without a compatibility path; never mutate either protected main |
| W0 truth seal | ADMITTED_GIT_DURABLE | paired payload commits, 63-path overlay, clean dependency pin, replay, and receipts verify |
| W1 parse-that transaction | ADMITTED_GIT_DURABLE | parse-that `9c9714dfd`; BBNF evidence `ad7aec5c6`; 70 workspace laws, 7 bridge laws, clean packages, 148-path crater, tri-fold ACCEPT |
| W2 BBNF semantic spine | ADMITTED_GIT_DURABLE | parse-that `282ccb6f6`; carrier `28a91db51`; evidence `60ee1a51d`; reviews `f46eb9497`; adjudication `3f44a40c1`; tri-fold ACCEPT |
| W3 grammar-neutral vertical | BORN_RED_GIT_DURABLE_IMPLEMENTATION_AUTHORIZED | public/candidate packet `1ab3e5640`; parse-that ABI-3 `ed1ffa24`; accepted BBNF born-RED payload `dadd6fa77`; implementation is next; held-out selection remains forbidden |
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
currently authorized wave. W0 is git-durable at BBNF payload `47d51a190` and
parse-that payload `627c9b46a`. W1 is git-durable at parse-that
`9c9714dfd` and BBNF evidence payload `ad7aec5c6`. W2 is git-durable at
parse-that `282ccb6f6`, BBNF carrier `28a91db51`, evidence `60ee1a51d`,
reviews `f46eb9497`, and adjudication `3f44a40c1`. Later waves must add their
own receipts. W3 has durably presealed its public/candidate inputs at BBNF
`1ab3e5640`; parse-that's born-RED contract is `88a71c24`, and the accepted
0.5.0 / ABI-3 implementation is `ed1ffa24`. BBNF's accepted W3 born-RED
instance/frontend, replay/public, and semantic/Pratt boundary is git-durable
at payload `dadd6fa775a78d9adf08aa9c40699bc89775a689`, tree
`3865680cb6fccd79ff31f9bf4f038ef43e0fc87a`.

## Exact next action

Verify the current W3 born-RED payload from this directory:

```sh
git rev-parse dadd6fa775a78d9adf08aa9c40699bc89775a689^{tree}
jq -e . restart/skinny/tranches/sk-v26/artifacts/W3-BBNF-BORN-RED.json
shasum -a 256 \
  crates/core/tests/w3_instance_frontend_contract.rs \
  crates/core/tests/w3_replay_public_contract.rs \
  crates/core/tests/w3_semantic_pratt_contract.rs
```

The expected payload tree is
`3865680cb6fccd79ff31f9bf4f038ef43e0fc87a`. The accepted contract hashes are
`d20ef0a176758305e2e3adc1f63f34229df3087c7248d5570a4dbe7e99f60c8f`,
`ce60dbcf8b26e2934644136e46046c4ac0e34e916c02a7d98906faa6f03f0239`,
and `8c6f7018a8b19f72fb7a8de599efe0ba8b4616e42c37a7ec49d7bdc93947a6ae`.

The exact next boundary is:

1. implement the accepted generic instance, syntax, Projection, Difference,
   Trim, Pratt, scalar-event, emission, and replay surface without a
   compatibility path;
2. migrate every active consumer and atomically delete `ModuleInput`,
   `compile_module`, edit sessions, raw source/path replay duplication, and
   implicit entry fallback;
3. pass public-witness, package, no-overfit, regeneration, and zero-diff
   controls plus tri-fold hostile review;
4. create the immutable freeze basis and receipt.

Do not calculate, name, materialize, or special-case the held-out selection
before the freeze receipt is git-durable.

## Handoff rule

At a session wall, update only:

1. the table above;
2. exact in-flight task IDs;
3. immutable artifact paths and hashes;
4. the exact next command.

Narrative progress does not advance a state.
