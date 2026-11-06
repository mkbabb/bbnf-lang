# Gestalt-3 repair author receipt — JSON W0 byte oracle

Date: 2026-07-19 (`America/New_York`)  
Disposition: **CORRECTED INPUT ORACLE / AWAITING HOSTILE REVIEW**  
Product disposition: **`INCOMPLETE_ASSAY`, ZERO PRODUCT OR LOCK CREDIT**

This append-only receipt is outside the subject seal through the root
`GESTALT-*.md` exclusion. It records author evidence and cannot award a clean
hostile pass.

## Model truth and authority

Before repository inspection, the local session recorded:

| Field | Value |
| --- | --- |
| Model | `gpt-5.6-sol` |
| Reasoning effort | `xhigh` |
| Canonical task | `/root/json_w0_g3_repair` |
| Session/thread | `019f79b6-ffad-7041-904d-1f9b1949782f` |

| Governing input | SHA-256 |
| --- | --- |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| `GESTALT-3-CRITIC-A.md` | `dce15ce008a464f326b2c5dff3acc8de669855f67255d0ecb4ed5fac3854e906` |
| `GESTALT-3-CRITIC-B.md` | `a9cb13f0d610903f1b744be4a726b07376171a4fbd11c38aeceae426b1ce5593` |
| `GESTALT-3-ADJUDICATION.md` | `27a17d61977aa20082421f67e75c4762ebf8c6ce712bf1a8bba05f53b2ef97af` |
| Reopened subject seal | `f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5` |

## RED-first counterexamples

The ordered repair began only with external tests. This exact command ran the
three fresh CLI routes plus poison and delete proofs against the reopened code:

```sh
LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 \
  PYTHONPATH=src JSON_TEST_SUITE=/private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  python3 -S -B -m unittest \
  tests.json_oracle.test_cli.CliTests.test_simultaneous_default_ceilings_fit_public_cli_envelope \
  tests.json_oracle.test_generate.GenerateTests.test_number_generation_ignores_poisoned_target_metadata \
  tests.json_oracle.test_generate.GenerateTests.test_number_generation_from_absent_target_is_repeatable -v
```

The simultaneous input was exactly 1,048,576 bytes, 131,072 decoded UTF-16
units, 59,999 nodes, 29,999 unique members, and depth one. Base, scalar, and JCS
failed the unchanged 128 MiB process gate at 147,259,392 B, 181,534,720 B, and
183,779,328 B. The poison test copied all three sentinels, and the absent-target
test raised `FileNotFoundError`. The independent library object gate remained
unchanged at 10,000,000 observation bytes; the added public construction policy
is a separate 16 MiB bound, not a relaxation.

## Smallest owning-mechanism repair

- `_integrity` still reparses source with source-sized ceilings and compares the
  complete immutable document, but returns the checked original after the fresh
  equal witness is discarded.
- Public standalone observation, scalar, and canonical projection enter that
  source-derived boundary independently. The compound CLI enters once, runs
  optional scalar/JCS checks on the checked document, and emits the canonical
  observation directly. The bytes -> tree -> bytes route and optional repeated
  integrity reparses are gone.
- Observation construction is incrementally capped at 16,777,216 bytes with
  typed `resource:output_byte_budget`. RFC 8785 construction is capped at
  1,048,576 bytes with typed `typed_projection:projection_byte_budget`. Exact
  limit/limit-minus-one tests prove both and no partial result is written.
- `generate_numbers()` never reads its target. A small checked source recipe owns
  semantic labels/revision and the differential seed/count/uniqueness rule;
  checked runtime and producer bytes derive Node/V8 identity, program/binary
  hashes, commands, 104 rows, and both output digests.
- Disposable poison, missing-target, two-absent-output, and complete independent
  producer-field tests prove non-inheritance. The existing 104-row and 100,000-row
  checks are retained.

No product, candidate, schema, value/tape, CSS, backend, package, consumer, or
graph code was read into or changed by this repair.

## Corrected public resource proof

Three fresh processes per route used `/usr/bin/time -l` under the sealed runtime:

| Route | Stdout incl. newline | Wall range | Peak RSS range |
| --- | ---: | ---: | ---: |
| base | 11,677,745 B | 0.53–0.54 s | 121,929,728–122,322,944 B |
| `--require-scalars` | 11,677,745 B | 0.54–0.55 s | 121,962,496–122,322,944 B |
| `--jcs` | 12,018,925 B | 0.58–0.62 s | 119,734,272–120,782,848 B |

Every observation reported the exact accepted counters above. Every RSS sample
is at most 134,217,728 bytes, and every stdout is at most 16,777,216 bytes plus
the CLI newline.

## Refreeze and target-absent regeneration

Only after all 41 external tests passed were generated artifacts refreshed:

```sh
LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
  python3 -S -B -m json_oracle.generate \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite --output data
LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
  python3 -S -B -m json_oracle.generate --numbers --output data
LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
  python3 -S -B -m json_oracle.generate --fixtures --output fixtures
LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
  python3 -S -B -m json_oracle.generate --manifests --output .
```

Two new empty output roots then regenerated suite data, the complete number
document, and fixtures under `PYTHONHASHSEED=17` and `91`. `diff -qr` between
roots and against checked fixtures was empty; `cmp` against all three checked data
artifacts was empty. Exact generated identities in both roots were:

| Artifact | SHA-256 |
| --- | --- |
| number corpus | `0c679aeafc8de364eb2e2ddbc02357950f611cbb5cb508c562d9f2b35e302d19` |
| 318 expected rows | `69647f10714a3ad20c6615fcc6ad55d56c825af52d83c9de0e9da02dc9d648d6` |
| suite manifest | `b4df1d9d69075bce2d1b082803986b00845422b258daca77c5da0b2dcb7427df` |
| fixture manifest | `12f9da36fea88a33787e552a614d2798a39e283e72a2708168ff005931a5231e` |

The number-specific missing-target test also constructed a source-shaped root with
no checked corpus target, generated two absent output roots, and matched the same
number-corpus hash. The poison test changed target authority/focus/revision to
sentinels and still matched those bytes.

## Complete proof matrix twice

Both top-level runs used:

```sh
/usr/bin/time -l env LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 \
  PYTHONHASHSEED=0 PYTHONPATH=src python3 -S -B verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md
```

| Run | Exit | Wall | Peak RSS | Roots | Tests/root | Regenerations/root |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| A | 0 | 24.81 s | 197,607,424 B | 2 | 41 | suite, numbers, fixtures, source/file manifests |
| B | 0 | 20.51 s | 196,313,088 B | 2 | 41 | suite, numbers, fixtures, source/file manifests |

Both reported 318 expected rows, network `unused`, 104 corpus rows, and 100,000
unique differential rows at
`330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8`.
The Python closure remained
`a3c45710552b7fc8dda24e8cc25615b3f22c56302d725f1359c2c4d38085e94a`;
the Node closure remained
`be37fca7d3ed1504a2ad774b13780b3e6808e9fa39e583d807af1f664a038314`.
Across the two top-level runs, four cache-empty subject roots executed 164 tests
and four complete regenerations of every generated evidence plane.

Hash-seed determinism remained byte-identical for `0`, `1`, `17`, and `random`:
accepted JCS CLI SHA-256
`98014fb9b35a4d600ec6e15e1f8c38d0df715b47552038addd554b4ac3ed0892`;
rejected invalid-UTF-8 CLI SHA-256
`a1e98e20e85f667a85503698a57d7c9d9946ae17ac848ce41d1bac9d479a4630`.
No subject bytecode cache remained.

## Corrected subject identity

| Item | SHA-256 |
| --- | --- |
| `data/file-manifest.json` | `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a` |
| `data/source-manifest.json` | `0e40aa023462dd67f41e07d1c0bbaadcf11293af03f381db4fddf894eada4897` |
| `README.md` | `ae5e6f868897d4d1d58ad12e72acbbc5ab0f198269cc24e426b43b8f38b8d91b` |
| `AUTHOR.md` | `6d0756b3f589211d55bb0cb93d47ab84b4a6f80ba41a03d24d9247330d7fe242` |
| external `../JSON-W0-AUTHOR-RESEAL.md` | `1969c88d6d30f89e262e2296e8582b78dbc6a2a730761ab78228e0e3ddeefdf3` |

The seal contains 41 path-unique files and excludes only itself plus root review
reports. All sealed bytes and source records regenerated exactly.

## Claim disposition and routed remainder

Retained on the exercised scope: strict UTF-8/RFC 8259 behavior, exact number
lexemes, decoded string facts, duplicates, byte spans, complete input, immutable
source-derived integrity, RFC 8785 typed projection, deterministic observations,
spec/runtime/suite identities, 104/100,000 number comparisons, and honest
`INCOMPLETE_ASSAY` status. Corrected and retained for new review: public
base/scalar/JCS simultaneous-ceiling resources and complete target-absent number
artifact generation.

Rejected and removed: CLI bytes -> tree -> bytes materialization, repeated
compound integrity reparses, unbounded observation/projection construction, and
target-metadata inheritance. No product or graph claim is inferred from their
removal.

Routed remainder: two fresh consecutive hostile Gestalt passes over this corrected
seal. The larger VT.W0 remains born RED. Product schema, value/tape, CSS, query,
recovery, streaming, incremental, Rust/TypeScript/WASM/ARM, packages, consumers,
profiles, candidate ranking, graph closure, and production movement remain outside
this oracle and receive zero credit.
