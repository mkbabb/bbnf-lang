# JSON W0 Gestalt-3 ordered repair author reseal

Disposition: **INCOMPLETE_ASSAY / oracle input only**. This is the non-circular
author reseal for `conformance/json`; it awards no clean hostile pass, complete
VT.W0, candidate, PT/BB lock, profile, product, graph, or production credit. The
Gestalt-3 adjudication keeps the consecutive-clean count at zero.

## Exact identity and authority

| Field | Value |
| --- | --- |
| Role | fresh Gestalt-3 repair author; not critic or adjudicator |
| Model / effort | `gpt-5.6-sol` / `xhigh` |
| Task / session | `/root/json_w0_g3_repair` / `019f79b6-ffad-7041-904d-1f9b1949782f` |
| Date / zone | 2026-07-19 / `America/New_York` |
| Constitution | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| Wave | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| Gestalt-3 critic A | `dce15ce008a464f326b2c5dff3acc8de669855f67255d0ecb4ed5fac3854e906` |
| Gestalt-3 critic B | `a9cb13f0d610903f1b744be4a726b07376171a4fbd11c38aeceae426b1ce5593` |
| Gestalt-3 adjudication | `27a17d61977aa20082421f67e75c4762ebf8c6ce712bf1a8bba05f53b2ef97af` |
| Reopened subject seal | `f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5` |

## Externally anchored corrected subject

`data/file-manifest.json` excludes itself and root `GESTALT-*.md` reports. This
sibling reseal and the append-only repair receipt are also outside the subject, so
the root below is non-circular.

| Item | SHA-256 / count |
| --- | --- |
| `json/data/file-manifest.json` | `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a` |
| `json/data/source-manifest.json` | `0e40aa023462dd67f41e07d1c0bbaadcf11293af03f381db4fddf894eada4897` |
| `json/AUTHOR.md` | `6d0756b3f589211d55bb0cb93d47ab84b4a6f80ba41a03d24d9247330d7fe242` |
| `json/README.md` | `ae5e6f868897d4d1d58ad12e72acbbc5ab0f198269cc24e426b43b8f38b8d91b` |
| 318-row expected observations | `69647f10714a3ad20c6615fcc6ad55d56c825af52d83c9de0e9da02dc9d648d6` |
| suite manifest | `b4df1d9d69075bce2d1b082803986b00845422b258daca77c5da0b2dcb7427df` |
| fixture manifest | `12f9da36fea88a33787e552a614d2798a39e283e72a2708168ff005931a5231e` |
| complete number corpus | `0c679aeafc8de364eb2e2ddbc02357950f611cbb5cb508c562d9f2b35e302d19` |
| runtime closure manifest | `0ee773adcc05c245f7fe5487410fc4dbd8490d74240e3c3a2c3cb4c7790f1f7f` |
| Node producer program | `921cbcb65c2997eb4afffcf930bf816a985d589ecd07b27088003bc9a90a7bbe` |
| Subject files | 42 = 41 sealed + self-excluded file manifest; root review reports are externally counted |

## Corrected owning mechanisms

The public CLI now validates the immutable source-derived document exactly once
for one compound request. It checks optional scalar/JCS policy on that validated
document and directly emits bounded canonical observation bytes. It no longer
materializes `observe` bytes as a Python tree, and optional work no longer runs a
second integrity reparse while retaining the observation. Public standalone
`observe`, scalar, and canonicalization functions still enforce the complete
source-reparse/equality boundary.

Observation construction has a 16 MiB inclusive byte cap with typed
`resource:output_byte_budget`; canonical projection has a 1 MiB inclusive cap with
typed `typed_projection:projection_byte_budget`. The unchanged library object gate
remains 10,000,000 bytes, and every parse/resource ceiling is unchanged.

The number producer no longer reads the corpus target. Its small source recipe and
checked runtime closure derive the entire document: semantic metadata, program and
binary identities, live Node/V8 identity, commands, seed/count/uniqueness rule,
104 case rows, and corpus/differential output digests. Poison and absent-target
tests establish non-inheritance; two absent outputs equal the checked artifact.

## Public simultaneous-ceiling proof

The exact test input reaches 1,048,576 bytes and 131,072 decoded UTF-16 units,
contains 59,999 nodes and 29,999 unique object members, and has depth one. Before
repair, fresh RED processes measured 147,259,392 B base, 181,534,720 B scalar, and
183,779,328 B JCS peak RSS. Three corrected samples per route measured:

| Route | Stdout incl. newline | Wall range | Peak RSS range |
| --- | ---: | ---: | ---: |
| base | 11,677,745 B | 0.53–0.54 s | 121,929,728–122,322,944 B |
| `--require-scalars` | 11,677,745 B | 0.54–0.55 s | 121,962,496–122,322,944 B |
| `--jcs` | 12,018,925 B | 0.58–0.62 s | 119,734,272–120,782,848 B |

Every sample is below 134,217,728 B RSS and 16,777,216 B plus the CLI newline.
These are external-oracle resource facts, not product performance evidence.

## Two complete top-level verifier replays

Both invocations used the same exact command under `/usr/bin/time -l`:

```sh
env LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 \
  PYTHONPATH=src python3 -S -B verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md
```

| Run | Result | Wall | Peak RSS | Disposable roots | Tests per root |
| --- | --- | ---: | ---: | ---: | ---: |
| A | exit 0 | 24.81 s | 197,607,424 B | 2 | 41 |
| B | exit 0 | 20.51 s | 196,313,088 B | 2 | 41 |

Each run checked the current governance and runtime closures, all manifests, the
104-row corpus, the 100,000 unique finite-binary64 differential at SHA-256
`330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8`,
two complete test runs, and two independent suite/number/fixture/manifest
regenerations. Network use was `unused`; the seal was
`d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`.

## Retained and rejected claims

Retained on the exercised scope: strict RFC 8259 byte behavior, exact lexemes and
spans, duplicate retention, immutable observation facts, RFC 8785 typed projection,
runtime/spec/suite identity, deterministic generation, the 104/100,000 number
proofs, and the `INCOMPLETE_ASSAY` boundary. Corrected and now retained: public
base/scalar/JCS simultaneous-ceiling resources and complete target-absent corpus
generation. Rejected and deleted: bytes -> tree -> bytes CLI composition, repeated
compound integrity reparses, unbounded output/projection construction, and corpus
metadata inheritance.

## Residual RED boundary

This oracle has no product value/tape, grammar-derived type surface, query API,
recovery, streaming, incremental engine, Rust/TypeScript/WASM/ARM backend, CSS,
package, consumer, candidate, graph closure, product profile, production movement,
or clean hostile pass. Two fresh consecutive hostile Gestalt passes remain routed
outside this author repair.
