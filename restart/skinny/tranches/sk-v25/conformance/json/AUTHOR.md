# JSON W0 Gestalt-3 ordered repair ledger

Disposition: **INCOMPLETE_ASSAY / oracle input only**. This is an implementation
ledger, not a hostile verdict. All three Gestalt-3 findings were adjudicated
correct and keep the local clean count at zero pending two new hostile passes. No
candidate, PT/BB lock, complete VT.W0, profile, product, or production credit is
claimed.

## Identity and authority

| Item | Exact identity |
| --- | --- |
| Role | fresh Gestalt-3 repair author; neither critic nor adjudicator |
| Model / effort | `gpt-5.6-sol` / `xhigh` |
| Task / session | `/root/json_w0_g3_repair` / `019f79b6-ffad-7041-904d-1f9b1949782f` |
| Date / zone | 2026-07-19 / `America/New_York` |
| Constitution | `../../LOCK-CONSTITUTION.md`, `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| Wave | `../../waves/BB-VALUE-TAPE-P0.md`, `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| Binding adjudication | `GESTALT-3-ADJUDICATION.md`, `27a17d61977aa20082421f67e75c4762ebf8c6ce712bf1a8bba05f53b2ef97af` |
| Preserved Gestalt-3 critics | A `dce15ce008a464f326b2c5dff3acc8de669855f67255d0ecb4ed5fac3854e906`; B `a9cb13f0d610903f1b744be4a726b07376171a4fbd11c38aeceae426b1ce5593` |
| Reopened subject | `data/file-manifest.json`, `f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5` |

## Ordered repair result

**G3R1 — external RED gates first.** The test constructs the adjudicated exact
simultaneous-default object: 1,048,576 input bytes, 131,072 decoded UTF-16 units,
59,999 nodes, 29,999 unique members, and depth one. Independent fresh base,
`--require-scalars`, and `--jcs` processes assert accepted counters, a separate
16 MiB stdout construction policy, and the unchanged 128 MiB RSS ceiling. Before
repair they failed at 147,259,392 B, 181,534,720 B, and 183,779,328 B. The existing
library object gate remains unchanged at 10,000,000 observation bytes.

**G3R2 — one checked compound path.** Public standalone `observe`, scalar, and JCS
operations still reparse the owned source and compare the complete immutable
document, so every corruption mutation retains `projection_integrity`. The CLI
performs that exact boundary once, then runs optional scalar/JCS work and directly
constructs canonical observation bytes. The former observation-bytes -> Python
tree -> observation-bytes route and projection-specific integrity reparses are
deleted.

**G3R3 — explicit construction bounds.** Observation construction uses bounded
incremental chunks with an inclusive 16 MiB cap and typed
`resource:output_byte_budget` exhaustion. Canonical projection uses a bounded byte
buffer with an inclusive 1 MiB cap and typed
`typed_projection:projection_byte_budget` exhaustion. Exact-limit and
limit-minus-one tests cover both. No input, depth, node, aggregate, decoded-unit,
library 10,000,000-byte object, or 128 MiB process limit was weakened.

**G3R4 — target-absent number recipe.** `generate_numbers()` no longer reads
`data/ecmascript-number-corpus.json`. A small checked source recipe supplies only
the semantic scope/focus/revision and differential seed/count/uniqueness rule.
Runtime closure facts select and check Node; program/binary hashes, live Node/V8
identity, commands, corpus rows, and both output digests are derived on every run.

**G3R5 — non-inheritance proof.** One test poisons authority, focus, and revision
in a disposable target and proves none flow. A second removes the target and
generates two absent-output roots equal to one another and the checked artifact.
A third independently checks every emitted producer identity field. The 104-row
corpus and 100,000-row unique finite-binary64 comparison remain intact.

**G3R6 — ordered reseal.** Suite observations, fixtures, complete number corpus,
source manifest, and file manifest are regenerated only after G3R1–G3R5 pass. The
external sibling reseal and append-only `GESTALT-3-REPAIR-AUTHOR.md` carry the final
non-circular seal, complete commands, resources, and retained/rejected boundaries.

## Public resource measurements

Fresh Apple arm64 processes used the exact simultaneous-default object:

| Route | Input | Stdout incl. newline | Wall range | Peak RSS range |
| --- | ---: | ---: | ---: | ---: |
| base | 1,048,576 B | 11,677,745 B | 0.53–0.54 s | 121,929,728–122,322,944 B |
| `--require-scalars` | 1,048,576 B | 11,677,745 B | 0.54–0.55 s | 121,962,496–122,322,944 B |
| `--jcs` | 1,048,576 B | 12,018,925 B | 0.58–0.62 s | 119,734,272–120,782,848 B |

All remain below the named 128 MiB envelope and 16 MiB stdout-construction cap.
These measurements are assay resource proof, not product performance claims. The
prior library-only one-dimensional table and its thresholds remain in `README.md`
and the external tests.

## Current-wave fit and explicit RED remainder

This remains the smallest independent JSON observation carrier permitted by
VT.W0. It adds no product schema, value/tape, query API, recovery, streaming,
incremental engine, package, backend, candidate code, or handwritten semantic
sidecar. It still provides no BBNF-derived types, Rust/TypeScript/WASM/ARM parity,
CSS closure, consumer, profile ranking, clean hostile pass, or production
authority. Graph work and graph findings are not JSON evidence. New fresh critics
and a fresh adjudicator must inspect these corrected bytes; the consecutive-clean
count remains zero.
