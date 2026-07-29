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
| W3 grammar-neutral vertical | AUTHORIZED_BORN_RED | two independent designs plus final adjudication select the minimal pre-freeze instance/difference/Trim/Pratt surface; held-out selection remains forbidden |
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
own receipts.

## Exact next action

Verify the admitted W2 subjects from this directory:

```sh
git rev-parse \
  28a91db51177495efb61cd1c664a85b37fdd9529^{tree} \
  60ee1a51d694b5cf3fd76779336ef165e593129f^{tree} \
  f46eb9497522ea97e164e8a1e4160784aa53eb1e^{tree} \
  3f44a40c1297a962c4a227433814864598f44ae1^{tree}
git -C /Users/mkbabb/Programming/parse-that-skv26 rev-parse \
  282ccb6f6da15ef1e5b355b5588a23cc6a76ae7f^{tree}
```

The exact next boundary is:

1. seal the corrective public-witness and five-candidate manifest/case packet
   described by `artifacts/W3-DESIGN-ADJUDICATION.md`, without compiling the
   candidates;
2. commit born-RED parse-that ABI-3 difference laws;
3. implement and package only the accepted difference primitive;
4. commit born-RED BBNF instance, syntax, Trim, semantic, Pratt, replay, and
   public-witness laws;
5. implement the accepted generic surface without a compatibility path;
6. pass public-witness, package, no-overfit, regeneration, and zero-diff
   controls plus tri-fold hostile review;
7. create the immutable freeze basis and receipt.

Do not calculate, name, materialize, or special-case the held-out selection
before the freeze receipt is git-durable.

## Handoff rule

At a session wall, update only:

1. the table above;
2. exact in-flight task IDs;
3. immutable artifact paths and hashes;
4. the exact next command.

Narrative progress does not advance a state.
