# SK-V26 state

This is the sole mutable execution authority for SK-V26.

Last observed: 2026-07-28, America/New_York.

## Current status

| Item | State | Evidence / next boundary |
|---|---|---|
| Tranche formation | ADMITTED | architecture seam adjudicated; immutable formation receipt and W0 git-object receipt verify |
| Formation durability | GIT_DURABLE | immutable formation epochs landed in W0 payload `47d51a190` |
| Formation review tasks | ADJUDICATED | `/root/v26_critic_architecture`; `/root/v26_critic_history`; `/root/v26_adjudicator` |
| Product source mutation | AUTHORIZED_W3_PREFREEZE_SELECTED_SLICE | parse-that may add only an independently witnessed transactional primitive required by the frozen grammar union; BBNF may add only the generic instance/syntax/Pratt/public-witness surface; never mutate either protected main |
| W0 truth seal | ADMITTED_GIT_DURABLE | paired payload commits, 63-path overlay, clean dependency pin, replay, and receipts verify |
| W1 parse-that transaction | ADMITTED_GIT_DURABLE | parse-that `9c9714dfd`; BBNF evidence `ad7aec5c6`; 70 workspace laws, 7 bridge laws, clean packages, 148-path crater, tri-fold ACCEPT |
| W2 BBNF semantic spine | ADMITTED_GIT_DURABLE | parse-that `282ccb6f6`; carrier `28a91db51`; evidence `60ee1a51d`; reviews `f46eb9497`; adjudication `3f44a40c1`; tri-fold ACCEPT |
| W3 grammar-neutral vertical | IN_PROGRESS_PARSE_THAT_ABI3_GREEN | public/candidate packet `1ab3e5640`; parse-that RED `88a71c24`; ABI-3 implementation `ed1ffa24`; BBNF born-RED laws are next; held-out selection remains forbidden |
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
0.5.0 / ABI-3 implementation is `ed1ffa24`.

## Exact next action

Verify the current W3 subjects from this directory:

```sh
git rev-parse 1ab3e56408c7c92e931d18b2b0bbb9848fd8595c^{tree}
git -C /Users/mkbabb/Programming/parse-that-skv26 rev-parse \
  88a71c24f22dc8e53ae482aaead2e4865f0dbc36^{tree} \
  ed1ffa24e862b797b01dc4f9215d6be8f7fa0955^{tree}
shasum -a 256 artifacts/W3-PARSE-THAT-ABI3.json
```

Expected parse-that trees are `ae06f60f96a4b76410a214d126bdfc700614e048`
and `65d1d58cee5e052804b062a17de571620ba463aa`. The ABI-3 receipt
SHA-256 is `eb8678899905d725926ebf892a8d60874aa9343c60cd7bfd24dac33b9ae92e55`.

The exact next boundary is:

1. commit born-RED BBNF instance, syntax, Trim, semantic, Pratt, replay, and
   public-witness laws against exact parse-that `0.5.0` / ABI 3;
2. implement the accepted generic surface without a compatibility path;
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
