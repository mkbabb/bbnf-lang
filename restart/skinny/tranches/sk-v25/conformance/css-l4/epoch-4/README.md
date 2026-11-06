# CSS L4 W0 successor observation assay — epoch 4

This directory is a compact, executable repair of the epoch-3 Gestalt 1
adjudication. Its terminal boundary is deliberately disjoint:

```text
retained_byte_replay = VALID
local_semantics      = BLOCKED
downstream_planes    = RED_OPEN
candidate_product    = ABSENT_RED
```

It proves deterministic offline replay of the retained bytes and the checked
observation ledger. It does **not** prove CSS support, CSS Level 4 coverage, a
BBNF mapping, an exact WPT owner, a WPT result, a control result, VT.W0 closure,
a candidate, a product, a hostile pass, or permission to move production.

## Sealed inputs

`inputs/objects/sha256/` contains exactly the 109 objects referenced by
`inputs/manifest.json` (43,013,202 bytes): the 96 selected CSSWG source files,
the full CSSWG source-key catalog, ten primary parsing/publication authorities,
the complete WPT path/blob-OID catalog, and the compressed inherited obligation
bundle. There are no private checkout paths in the replay contract.

The retained identities are:

- CSSWG commit `c7573530343759ace8e46438a1fa2c44515b5554`, 96 files,
  11,676,246 bytes;
- WPT commit `37cd7ff74eb974fd41600bf7bc01d37576abf8b5`, tree
  `12fa64c9afe86e199ee0e737755b6507f363ceff`, 160,788 path/blob-OID entries,
  uncompressed catalog SHA-256
  `cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699`;
- 8,796 inherited epoch-3 open obligation identities, uncompressed SHA-256
  `c4c1b87cb157d8f9bd1aeaf533a1619bc313c15a450e48c5eaff504ad9074365`.

`src/capture_inputs.py` is a genesis-only capture tool. It requires clean pinned
CSSWG and WPT checkouts and the exact `/usr/bin/git` binary. Normal replay does
not invoke Git or read either checkout.

## Direct pipeline

1. `src/source.py` provides one byte tokenizer and one inherited normativity
   state for source carriers, Bikeshed/rendered bibliography occurrences,
   comments and edit state, algorithms, `wpt`, and `wpt-rest`.
2. `src/pipeline.py` gives every dependency occurrence an explicit decision over
   a collision-total source-key multimap. It independently inventories 4,954
   source-native semantic occurrences and preserves mapped, excluded, and typed
   blocker dispositions.
3. `src/languages.py` consumes CSS VDS, railroad, Yacc, Lex, and WebIDL inputs.
   It reports typed blockers for unconsumed or invalid carriers; a partial parse
   cannot become `PARSED`.
4. WPT directive membership is resolved against the complete pinned tree before
   ownership. Exact feature ownership remains `UNMAPPED`, execution remains
   `UNRUN`, outcomes remain zero, and document/section Cartesian fanout is zero.
5. `src/crater.py` records that no candidate exists. Its future-candidate checker
   follows source, macro/build, generated/binary, symlink, and runtime-read edges
   and accepts only exact digest-qualified inputs.
6. `src/generate.py` regenerates the deterministic artifacts into an empty root.
   `src/manifest.py` seals the cache-free, symlink-free subject, with an explicit
   self-reference rule for the final receipt.

All A-01…A-10 and B-F01…B-F10 adjudicated findings map to concrete executable
mutations in `fixtures/mutations.json`; each mutation runs exactly once against
each regenerated artifact set.

## Checked artifacts

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| `data/observations.json` | 3,582,193 | `d320298d5cf2fd33f23b58830c2cf77eaaf4d91493812504d404eba234bd7267` |
| `data/boundary.json` | 408 | `4e61a7f1384dca31803cf45cbb91b3a19f84569ee7966c0d5b94e8d3e95604d5` |

`data/receipt.json` contains generation timing, RSS, toolchain, and artifact
receipts. `data/file-manifest.json` seals every owned non-review byte except
itself and `data/verification.json`; `data/verification.json` is written last and
binds that manifest and its subject. Future root `GESTALT-*.md` reviews are the
only other declared exclusion.

## Honest RED ledger

The inherited 8,796 obligations remain atomic. One interned plane vector and one
terminal decision serve them without cloning row payloads:

| Plane | Exact open state |
| --- | ---: |
| BBNF | 8,796 `UNMAPPED` |
| Independent fixtures | 8,796 `UNMAPPED` |
| Candidate owner | 8,796 `UNMAPPED` |
| WPT ownership / execution | 8,796 `UNMAPPED`; 8,796 `UNRUN`; 0 outcomes |
| Recovery | 8,796 `UNPROVEN` |
| Diagnostics | 8,796 `UNPROVEN` |
| Serialization | 8,796 `UNPROVEN` |

The local assay is also honestly blocked: 1,220 typed blocker instances cover
unmapped source occurrences, dialect failures, unreviewed or ambiguous
dependencies, unsealed authorities/source bytes, unreached inherited rows, and
the still-unproved WPT blob-content/feature relation. Candidate count and
complete-candidate count are both zero. Lightning CSS and sonic-rs correctness,
selection, and performance are all `UNRUN`.

The broader adjudication step 13 is represented only by exact RED placeholders:
the co-equal JSON oracle, incumbent profiles/hot leaves, four private compiling
placeholders, and the two external-control freezes. None is implemented here.

## Replay

Replay requires the exact CPython 3.14.5 binary at SHA-256
`2477b47fa3ae65b9574eb18a15edb364e96948eaa1875ad3f1c80d780efc9c12`.
From this directory:

```sh
env PYTHONDONTWRITEBYTECODE=1 python3 -B tests/offline_runner.py

proof_root="$(mktemp -d /tmp/css-epoch4-proof.XXXXXX)"
env PYTHONDONTWRITEBYTECODE=1 python3 -B tests/proof.py \
  --run-root "$proof_root/runs" \
  --output "$proof_root/proof.json"

env PYTHONDONTWRITEBYTECODE=1 python3 -B src/manifest.py --verify-receipt
```

The proof creates two distinct empty output roots, regenerates both artifact
sets offline, runs the twenty-mutation suite once against each set, and performs
byte-for-byte A/B and stored-artifact comparisons. See `AUTHOR.md` for the exact
author identity, adjudication-step mapping, censuses, timings, RSS, and no-credit
disposition.
