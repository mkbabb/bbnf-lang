# GESTALT-3-CRITIC-B — JSON W0 routine hostile review

Model: `gpt-5.6-sol`  
Effort: `xhigh`  
Role: independent critic B  
Sealed input: `data/file-manifest.json` SHA-256
`f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5`

This review treats the implementation only as an input oracle. It grants no
candidate, architecture, feature-product, performance, or production credit.
The review read only the assigned constitution, wave, and JSON `src`, `tests`,
`data`, and `verify.py` surfaces. It did not inspect the excluded prose, prior
review reports, campaign ledgers, repository history, or repository diffs.

## Numbered findings

1. **Correct, wave-blocking resource/efficiency finding: the real CLI exceeds
   the resource envelope asserted by the tests on a default-valid combined
   boundary input, while the verifier tests only a smaller in-process route.**

   `tests/json_oracle/test_parser.py:48-82` names a combined/maximum-shape gate,
   sets both the object output ceiling to 10,000,000 bytes and fresh-process RSS
   to 128 MiB, but invokes only `observe(parse(source))`. Its object has 29,999
   copies of the one-unit key `"a"`; it does not combine the independently live
   input, node, and decoded-unit ceilings, and it does not execute the CLI.

   A generated, standards-valid object under the unchanged default `Budgets`
   simultaneously reached the inclusive 1,048,576-byte input ceiling, the
   inclusive 131,072 decoded-UTF-16-unit ceiling, 59,999 of 60,000 nodes, and
   29,999 aggregate entries. All 29,999 names were unique, so both `--jcs` and
   `--require-scalars` were applicable. The CLI accepted it with these counters:

   ```json
   {"aggregate_entries":29999,"decoded_utf16_units":131072,"input_bytes":1048576,"max_depth":1,"nodes":59999}
   ```

   Three fresh-process samples on the sealed runtime produced:

   | CLI route | stdout bytes, including newline | peak RSS bytes | peak RSS MiB |
   |---|---:|---:|---:|
   | base | 11,668,804 | 150,224,896–153,485,312 | 143.27–146.38 |
   | `--require-scalars` | 11,668,804 | 183,697,408–186,531,840 | 175.19–177.89 |
   | `--jcs` | 12,009,984 | 185,614,336–188,940,288 | 177.02–180.19 |

   The base observation therefore exceeds both checked ceilings: 11,668,804 is
   greater than 10,000,000 bytes, and every base CLI sample is greater than
   134,217,728 bytes (128 MiB). The optional projections exceed the RSS ceiling
   by roughly 47–52 MiB.

   The cause is unnecessary whole-result duplication on the public command
   route. `cli.py:81-96` parses, obtains already serialized observation bytes,
   materializes them again with `json.loads`, and serializes the resulting tree
   again for stdout. `parser.py:440-510` calls `_integrity` before constructing
   that observation, and `projection.py:44-69` reparses the complete source for
   every integrity/scalar check; `canonicalize` does the same. Thus base CLI work
   holds source-derived nodes plus a large decoded observation tree, while either
   optional flag performs another complete integrity reparse after that tree is
   resident.

   This is not an RFC 8259 or RFC 8785 semantic discrepancy. It is a false-clean
   resource gate and a parsimony defect in the actual supported CLI. Repair must
   measure the CLI itself on a combined-ceiling shape and remove or justify the
   serialized-bytes -> Python tree -> serialized-bytes round trip and repeated
   full-document validation without weakening the typed corruption boundary.

No additional routine semantic, artifact-identity, or determinism finding was
reproduced on the sealed bytes.

## Retained facts

1. The requested file-manifest hash matched exactly before review. Its sealed
   source, tests, verifier, specifications, fixtures, producer, and generated
   data all matched their recorded sizes and SHA-256 values during normal
   verification.

2. The authoritative runtime description reproduced byte-for-byte: CPython
   3.14.5/arm64 closure
   `a3c45710552b7fc8dda24e8cc25615b3f22c56302d725f1359c2c4d38085e94a`,
   Node 26.0.0/arm64 closure
   `be37fca7d3ed1504a2ad774b13780b3e6808e9fa39e583d807af1f664a038314`,
   macOS 26.4.1 build 25E253, and the pinned startup environment. The governing
   constitution and wave hashes also matched the verifier constants.

3. Normal verification passed twice from two disposable subject roots: all 35
   tests passed in each root; all 318 frozen JSONTestSuite rows regenerated; the
   104 checked number rows and 100,000-value finite-binary64 differential
   regenerated; fixtures, source manifest, and file manifest were deterministic;
   and no bytecode cache was admitted.

4. Complete generated assay outputs did not need an existing output directory.
   Suite manifest, 318 expected rows, ECMAScript number corpus, and the complete
   fixture tree were regenerated into empty directories under three distinct
   hash seeds and were byte-identical to the sealed artifacts.

5. Within the exercised RFC 8259 scope, the parser retained exact number lexemes,
   decoded string facts, source order, duplicate names after unescaping, half-open
   UTF-8 byte spans, complete-input rejection, UTF-8/BOM policy, and independent
   depth/node/entry/decoded-unit/input ceilings. The 318-case frozen corpus and
   ordinary zero/limit-minus-one/limit/limit-plus-one tests found no semantic
   discrepancy.

6. Within the exercised RFC 8785 scope, primitive serialization, lowercase
   control escapes, recursive ordering by unsigned UTF-16 units, array-order
   retention, duplicate-name rejection, lone-surrogate rejection, non-finite
   binary64 rejection, and UTF-8 output held. In addition to the sealed 100,000
   number differential, a fresh 250,000-value finite-binary64 differential under
   a different seed matched Node/V8 exactly, and 20,000 generated decimal JSON
   number lexemes matched `JSON.stringify(JSON.parse(input))` or the declared
   non-finite rejection.

7. Accepted `--jcs` output and rejected invalid-UTF-8 output were byte-identical
   for `PYTHONHASHSEED=0`, `1`, `17`, and `random`. The accepted hash was
   `98014fb9b35a4d600ec6e15e1f8c38d0df715b47552038addd554b4ac3ed0892`;
   the rejected hash was
   `a1e98e20e85f667a85503698a57d7c9d9946ae17ac848ce41d1bac9d479a4630`.

8. The oracle has one syntax parser and one source-derived immutable observation
   authority. No second semantic parser was found. The repeated validation and
   representation materialization described in finding 1 are execution
   duplication, not an independent semantic authority.

## Verdicts

- **Tranche fit: RETAIN AS AN INPUT-ORACLE ASSAY, RED FOR CLEAN CLOSURE.** JSON
  remains a useful W0 falsification carrier, and this narrow oracle is an
  appropriate external control. It remains solely an incomplete assay and cannot
  choose or credit a generalized value/tape architecture.

- **Wave fidelity: REOPEN / RED.** The sealed RFC and artifact gates are useful,
  but the actual CLI resource behavior does not satisfy the implementation's own
  checked envelope. W0 cannot claim a clean resource plane until finding 1 is
  corrected and rerun on the corrected seal.

- **Feature truth:** RFC 8259 behavior **retained on tested scope**; RFC 8785
  projection **retained on tested scope**; complete fresh artifact generation
  **retained**; runtime/input identity **retained**; deterministic output
  **retained**; CLI resource/efficiency behavior **FAIL**; parsimony
  **FAIL at the repeated full materialization/reparse path**.

- **Greater-product effect: CONTAINED, NOT ADOPTED.** The implementation is a
  read-only oracle with no product credit. Its semantic authority need not move
  into a candidate, and its redundant CLI execution path must not be copied into
  any product runtime.

- **Product verdict: `INCOMPLETE_ASSAY`, ZERO PRODUCT CREDIT.** It is not a VT
  candidate, not a value/tape feature implementation, not performance evidence
  for a candidate, and authorizes no production movement.

## Commands and observed results

The frozen JSONTestSuite input was acquired into a disposable directory at exact
HEAD `1ef36fa01286573e846ac449e8683f8833c5b26a`; the verifier independently checked
that HEAD, tree `b936f9acdd24b9f5fefe68b90b9beab2c681137a`, and a clean worktree.

```sh
shasum -a 256 restart/skinny/tranches/sk-v25/conformance/json/data/file-manifest.json
# f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5

LC_ALL=C TZ=UTC PYTHONHASHSEED=0 PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
  python3 -S -B verify.py \
  --suite /tmp/json-w0-critic-b.QmOZ0q/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md
# PASS: 35 tests x 2 roots; 318 suite rows; 104 + 100000 number rows;
# sealed_root_sha256=f158e3...79fdb5
```

Fresh empty-output regeneration:

```sh
mkdir /tmp/json-w0-critic-b.QmOZ0q/fresh-data \
      /tmp/json-w0-critic-b.QmOZ0q/fresh-fixtures
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src PYTHONHASHSEED=17 \
  python3 -S -B -m json_oracle.generate --suite "$SUITE" \
  --output /tmp/json-w0-critic-b.QmOZ0q/fresh-data
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src PYTHONHASHSEED=91 \
  python3 -S -B -m json_oracle.generate --numbers \
  --output /tmp/json-w0-critic-b.QmOZ0q/fresh-data
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src PYTHONHASHSEED=31337 \
  python3 -S -B -m json_oracle.generate --fixtures \
  --output /tmp/json-w0-critic-b.QmOZ0q/fresh-fixtures
cmp fresh-data/json-test-suite-manifest.json data/json-test-suite-manifest.json
cmp fresh-data/expected-results.jsonl data/expected-results.jsonl
cmp fresh-data/ecmascript-number-corpus.json data/ecmascript-number-corpus.json
diff -qr fresh-fixtures fixtures
# PASS: no output from any comparison
```

Independent number checks:

```sh
/opt/homebrew/Cellar/node/26.0.0/bin/node --no-warnings \
  data/producers/ecmascript-numbers.mjs differential \
  0x9e3779b97f4a7c15 250000 \
| PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -S -B \
  -c '<stream each finite binary64 row; compare _number(repr(value), 0)>'
# 250000; no mismatch

PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -S -B \
  -c '<generate 20000 decimal lexemes with seed 0x82598785; compare
      canonicalize(parse(raw)) with Node JSON.stringify(JSON.parse(raw))>'
# 20000; no mismatch
```

Determinism probe:

```sh
for seed in 0 1 17 random; do
  PYTHONHASHSEED=$seed PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
    python3 -S -B -m json_oracle.cli \
    fixtures/positive/source-facts.json --jcs | shasum -a 256
done
# all four hashes identical; the rejected invalid-UTF-8 probe was also identical
```

The combined-ceiling fixture used for the resource finding was generated in a
disposable directory from 29,999 unique base-36 names, lengthened to exactly
131,072 UTF-16 units, then padded with legal object whitespace to exactly
1,048,576 bytes. The acceptance/counter probe and RSS command were:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -S -B \
  -m json_oracle.cli /tmp/json-w0-critic-b.QmOZ0q/memory/all-ceilings.json \
| python3 -c 'import json,sys; v=json.load(sys.stdin); print(v["status"],v["source_bytes"],v["counters"])'

for mode in base require-scalars jcs; do
  # option is empty, --require-scalars, or --jcs respectively
  /usr/bin/time -l env LC_ALL=C TZ=UTC PYTHONHASHSEED=0 \
    PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
    python3 -S -B -m json_oracle.cli \
    /tmp/json-w0-critic-b.QmOZ0q/memory/all-ceilings.json "$option" \
    >/dev/null
done
# repeated three times per route; ranges are recorded in finding 1
```

## Clean-count effect

**Consecutive clean count resets to 0.** Finding 1 is a correct resource and
parsimony defect on the sealed implementation. The pre-report verifier observed
seven files in the append-only review namespace; adding this eighth filename does
not create clean credit because report count is not adjudication. After correction,
the owning implementation must refreeze and both fresh hostile passes must cover
the actual CLI and combined-ceiling resource shape. Until then the JSON W0 assay
and its wave remain RED.
