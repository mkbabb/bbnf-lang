# GESTALT-3 critic A — JSON W0 byte oracle

Date: 2026-07-19 (`America/New_York`)  
Disposition: **NOT CLEAN / REOPEN LOCAL ASSAY**  
Subject seal: `data/file-manifest.json` SHA-256
`f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5`

## Independence and model-truth gate

The gate was read from the exact turn request metadata before subject review:

- model: `gpt-5.6-sol`
- reasoning effort: `xhigh`
- thread: `019f798a-293b-7df1-b1ca-36b926c71509`
- turn: `019f798a-2a19-7943-b285-9fadcacf0ed5`
- session / parent thread: `019f7685-254a-7a22-9917-1da91f977861`
- source / kind: `subagent` / `thread_spawn`

The gate passed. I did not read any `GESTALT-*.md`, campaign/run/challenge
ledger, sibling receipt, or prior review. The verifier's numeric count of excluded
root review files disclosed no review content. No prohibited context appeared.
Review was limited to the current constitution and wave, the complete permitted
assay corpus, ordinary local static inspection, and disposable examples under
`/private/tmp`.

Governing identities:

| Input | SHA-256 |
| --- | --- |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| `data/file-manifest.json` | `f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5` |

## Actionable findings

1. **The real CLI exceeds the documented 128 MiB default resource envelope on
   a fully accepted boundary shape, while the resource gate measures only the
   library path.** `README.md:92-104` states a 128 MiB process-RSS target for
   accepted adversarial shapes. `tests/json_oracle/test_parser.py:48-80`
   measures `observe(parse(source))`, but never the CLI. The CLI adds a full
   `json.loads(observe(document))` materialization at
   `src/json_oracle/cli.py:81-90`; `--jcs` then performs another integrity
   reparse and retains the materialized observation while building and embedding
   canonical bytes. A 239,993-byte object with 29,999 distinct three-byte keys is
   within every default limit: 59,999 nodes, 29,999 aggregate entries, 89,997
   decoded UTF-16 units, and depth 1. The normal CLI accepted it with exit 0 at
   139,575,296 bytes maximum RSS, above 134,217,728 bytes. The `--jcs` CLI also
   accepted it with exit 0 and used 173,654,016, 173,801,472, and 173,555,712
   bytes in three fresh processes. This is both a resource-truth gap and a
   compactness defect: the parse-to-ASCII-to-dict round trip is removable for
   the ordinary observation path. Fix the CLI so it does not materialize a
   second complete observation merely to print it, bound projection/output
   construction, and gate the actual CLI with and without `--jcs` over all
   default boundary combinations in fresh processes. Refreeze the resource
   table only from the public command surface or explicitly narrow and justify
   a separately tested library-only claim.

2. **The ECMAScript number “regeneration” inherits most of its output from the
   checked-in output, so the full corpus artifact is not independently
   reproducible.** `src/json_oracle/generate.py:181-195` opens
   `data/ecmascript-number-corpus.json` as a template and replaces only
   `cases`. The test at `tests/json_oracle/test_generate.py:52-59` consequently
   proves equality after copying the expected artifact's `authority_scope`,
   `focus`, `revision`, producer identity, commands, seed, counts, and expected
   digests. In a disposable subject containing only the permitted source,
   producer, and corpus input, I changed `authority_scope`, `focus`, and
   `revision` to sentinels; `generate_numbers()` emitted all three sentinels
   unchanged while freshly emitting 104 cases. Thus the advertised full-byte
   clean regeneration can pass after arbitrary inherited metadata changes and
   cannot start with the target artifact absent. Make producer metadata an
   explicit independent input or construct it from checked runtime/program
   facts; generate the complete corpus into an absent output path; and add a
   poison/delete test proving that stale target metadata cannot flow into the
   result. Verify every emitted producer identity field rather than using the
   expected output as its own recipe.

## Retained facts

- The supplied subject seal matches exactly. All 41 declared sealed paths are
  unique and match their declared sizes and SHA-256 values; the live sealed path
  set has neither an omission nor an undeclared non-review file. Source-manifest
  paths are likewise unique and exact.
- The strict UTF-8, complete-input, BOM, number-lexeme, string-unit/scalar,
  duplicate-name, byte-span, iterative-container, depth, node, entry, and
  decoded-unit behavior agrees with the declared policies in static inspection
  and functional replay. The mutable builder does not escape; the frozen source
  and complete reparse/equality integrity boundary detect the exercised private
  mutations.
- All 15 fixture records reproduce byte-for-byte, including base and RFC 8785
  observations. All 318 pinned JSONTestSuite rows are present once: 95 `y`, 188
  `n`, and 35 explicitly decided `i` rows. The frozen results contain 116 base
  accepts and 202 base rejects; every label/policy contract is true.
- RFC 8785 string escaping, UTF-16 property ordering, duplicate rejection,
  scalar rejection, binary64 overflow rejection, Appendix B vectors, and the
  primitive example pass. The sealed 100,000-value Node/V8 differential passes.
  A separate deterministic one-million-value differential at seed
  `0x9e3779b97f4a7c15` also found zero `_number`/`JSON.stringify` discrepancies.
- CLI input acquisition is bounded to `max_input_bytes + 1`; its five numeric
  options reject negative, nondecimal, overlong, and out-of-domain values before
  file I/O. Library and small CLI zero/limit-minus-one/limit/limit-plus-one gates
  pass. Finding 1 is specifically the untested public-surface memory composition,
  not an unbounded file read.
- The exact enforced macOS/arm64 CPython 3.14.5 and Node 26.0.0 / V8
  14.6.202.33-node.19 closures match. Their closure SHA-256 values are
  `a3c45710552b7fc8dda24e8cc25615b3f22c56302d725f1359c2c4d38085e94a`
  and `be37fca7d3ed1504a2ad774b13780b3e6808e9fa39e583d807af1f664a038314`.
  The Node binary separately matches
  `08dad0581f00a0cabf4d49ec92ca1f25fdfd01c2c18fa8e92b35f04d4c24c164`.
- The official verifier passed 35 tests in each of two review-free,
  cache-empty copied roots; regenerated suite rows, fixtures, number cases,
  source manifest, and file manifest agreed across roots; network was unused.
  Finding 2 narrows that retained result: number *cases* are regenerated, but
  the complete corpus document is partly inherited.
- Scope language is honest and compact: this remains an
  `INCOMPLETE_ASSAY / oracle input only`, with no BBNF-derived product schema,
  candidate, package, query/value/tape API, production import, architecture
  selection, performance credit, or clean-pass credit. The custom parser is
  justified by exact lexemes, duplicate retention, byte spans, and failure
  facts; the removable CLI materialization is identified in finding 1.

## All-plane verdict and clean-count effect

| Plane | Verdict | Reason |
| --- | --- | --- |
| Tranche fit | **RETAIN / PASS** | A byte-level external JSON observation oracle is an appropriately narrow VT.W0 organ, and it does not pretend to be a candidate. |
| Wave fidelity | **REOPEN** | The assay stays within the born-RED/product boundary, but its public resource proof and full-corpus regeneration claim are not yet true. |
| Feature truth | **FAIL** | Parser/canonicalization semantics retain strong evidence, but findings 1 and 2 are reproducible defects in resource behavior and producer reproducibility. |
| Greater-product effect | **PASS WITH NO CREDIT** | No new product authority or dependency edge is introduced; the assay remains read-only oracle input and supplies no portfolio closure. |

Overall verdict: **NOT CLEAN**. The consecutive-clean count is **0 -> 0**; this
review cannot count as a clean hostile pass. Both findings must be corrected in
their owning implementation/evidence, the seal must be refreshed, and two fresh
consecutive hostile passes remain required.

## Commands and rerunnable evidence

Seal and governing inputs:

```sh
shasum -a 256 ../../LOCK-CONSTITUTION.md \
  ../../waves/BB-VALUE-TAPE-P0.md data/file-manifest.json
```

Official two-root replay (passed, sealed root
`f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5`):

```sh
LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
  python3 -S -B verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md
```

The replay reported two passes, 318 rows, 100,000 unique differential rows,
number-output SHA-256
`330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8`,
and the Python/Node closure hashes recorded above.

Independent one-million-row number check:

```sh
set -o pipefail
/opt/homebrew/Cellar/node/26.0.0/bin/node --no-warnings \
  data/producers/ecmascript-numbers.mjs differential \
  0x9e3779b97f4a7c15 1000000 |
  PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -S -B \
  /private/tmp/json-w0-compare-number-stream.py
```

The temporary comparator decoded each `binary64_hex`, called
`_number(repr(value), 0)`, compared it with `json_stringify`, and printed
`1000000` with exit 0.

Accepted CLI resource counterexample (the same invocation was repeated in fresh
processes; temporary inputs and timing files were removed):

```sh
input=$(mktemp /private/tmp/json-w0-critic-a.XXXXXX)
perl -e '@c=(0..9,"A".."Z","a".."z"); print "{";
  for($i=0;$i<29999;$i++){print "," if $i;
  $a=$c[int($i/3844)]; $b=$c[int($i/62)%62]; $d=$c[$i%62];
  print "\"$a$b$d\":0";} print "}"' > "$input"
/usr/bin/time -l env LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 \
  PYTHONHASHSEED=0 PYTHONPATH=src python3 -S -B \
  -m json_oracle.cli "$input" --jcs > /dev/null
```

Producer inheritance counterexample: in a disposable copy containing
`src/json_oracle`, the producer program, and the corpus JSON, set
`authority_scope="critic-sentinel-not-produced"`,
`focus="critic-sentinel-focus"`, and `revision=999` in the corpus JSON, then run:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH="$copy/src" python3 -S -B \
  -m json_oracle.generate --numbers --output "$copy/out"
jq '{authority_scope,focus,revision,case_count:(.cases|length)}' \
  "$copy/out/ecmascript-number-corpus.json"
```

Observed output retained all three sentinels and reported `case_count: 104`.
