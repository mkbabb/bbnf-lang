# JSON W0 Gestalt-2 independent adjudication

Date / time zone: 2026-07-19 / `America/New_York`  
Role: fresh independent adjudicator; neither implementation author nor critic  
Canonical task: `/root/json_w0_g2_adjudicator`  
Workspace: `/Users/mkbabb/Programming/bbnf-lang`  
Subject: `restart/skinny/tranches/sk-v25/conformance/json`  

Disposition: **RED — REPAIR AND RESEAL THE LOCAL JSON ORACLE**  
Local consecutive-clean count: **0**  
Product, candidate, PT/BB lock, complete VT.W0, and production credit: **0**  

This report supersedes the two Gestalt-2 critic reports as the ruling for this
pass. It changes no implementation, test, fixture, corpus, manifest, receipt,
governing document, or campaign ledger. The critics are allegations; every ruling
below rests on a fresh execution or a direct proof from current bytes.

## 1. Review boundary and exact inputs

I inventoried and read the complete pre-adjudication subject: 46 files and
1,873,938 bytes, comprising the 40-file sealed implementation/evidence set,
`data/file-manifest.json`, and five root review reports. I read the cached RFC 8259
and RFC 8785 text and extracted all 16 pages of the cached ECMA-404 PDF. I also read
the complete current constitution, current wave, source, tests, fixtures, data,
`README.md`, `AUTHOR.md`, `verify.py`, the sibling author reseal, and both current
Gestalt-2 critics. Historical Gestalt-1 prose was provenance context, not verdict
authority.

The exact adjudication inputs, rechecked immediately before this file was created,
are:

| Input | SHA-256 |
| --- | --- |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| `data/file-manifest.json` | `a1dc2769c74753212df6c12efc2eeb6e3f1bad635968bd8dcfdf88134f58825a` |
| `GESTALT-2-CRITIC-A.md` | `1b194ece2993ff29543928d932f183570a0986ca41502621c52e75d2d2985392` |
| `GESTALT-2-CRITIC-B.md` | `351f4b4585ffbe558ce9a3172d9f768fff9ae8dd484b4edb78c8532d37efcbd2` |

The governing hashes above are the current authorities for this ruling. Critic B's
intermediate observed hashes `0304c493...` and `995972ef...` are not the current
bytes. The author ledger and sibling reseal are older still: both name
`3622262d...` and `710e50f4...`. Critic A's statement that the current governing
hashes match `AUTHOR.md` is therefore false. The underlying stale-governance defect
identified by critic B remains correct and has grown through more than one
governance revision.

The constitution requires the adjudicator to label each finding against rerunnable
evidence, treats a correct finding as reopening the affected cluster, and forbids
credit from author receipts or one hostile pass (`LOCK-CONSTITUTION.md:17-24,
29-56`). The wave calls this narrow JSON carrier an assay organ, keeps production
movement prohibited, requires typed failures and no unbounded resource path, and
permits only independent-oracle evidence at VT.W0 (`BB-VALUE-TAPE-P0.md:1-21,
127-170,360-389,414-428`).

## 2. Independent commands and evidence

### 2.1 Complete sealed replay and census

The ordinary documented replay succeeds now; critic A's present-tense replay
failure does not reproduce:

```sh
cd restart/skinny/tranches/sk-v25/conformance/json
/usr/bin/time -l env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
  python3 -B verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite
```

It ran all 29 tests in each disposable root, regenerated the suite, fixtures,
source manifest, and file manifest in each root, and exited 0. The summary reported
318 rows, two passes, five observed review reports, sealed root
`a1dc2769...`, and peak RSS 66,338,816 bytes. Thus critic A's historical
`_random` load-denial observation is not a current blocker. It does not cure the
runtime-identity defect in section 2.6.

A complete structured read of the subject found:

```text
sealed records                         40; exact path/size/hash matches 40
physical files before adjudication     46
JSONTestSuite rows                     318 unique: y=95, n=188, i=35
base results                           accepted=116, rejected=202
JCS results                            accepted=99, rejected=17, N/A=202
implementation-defined policies        35 exact
local fixtures                         15 unique
number cases                           104 unique binary64 patterns
__pycache__ / .pyc                     0
```

`pdftotext -layout data/specs/ecma-404-2nd-edition.pdf -` extracted all 16
pages; the extracted stream hashed to
`9079fae86b7450103e6db59662a92d6d51b7499bcba529e2df99e99ea7f4ca8a`.

### 2.2 Accepted observation amplification is outside the policy

The direct critic-A counterexample reproduces:

```sh
/usr/bin/time -l env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
  python3 -B -c \
  'from json_oracle import parse,observe; s=b"["*500+b" "*250000+b"0"+b"]"*500; d=parse(s); o=observe(d); print(len(s),len(o),dict(d._counters))'
```

Observed:

```text
251001 125621753 {'input_bytes': 251001, 'nodes': 501,
                  'aggregate_entries': 500,
                  'decoded_utf16_units': 0, 'max_depth': 500}
411074560 maximum resident set size
```

The input is legal under every default. `parser.py:384-405` copies each complete
container slice into `node.raw`; `observe()` publishes every copy at
`parser.py:435-500`. The raw UTF-8 total grows from 251,001 bytes with no padding to
125,251,001 bytes with 250,000 padding bytes, while the source itself grows only to
251,001 bytes. This is a direct depth-times-input amplification. The selected
boundary examples in `README.md:75-88` do not establish the stated 128 MiB policy
for accepted adversarial shapes.

The CLI has a separate pre-parse acquisition defect. This command:

```sh
probe=$(mktemp /private/tmp/json-w0-read.XXXXXX)
truncate -s 67108864 "$probe"
/usr/bin/time -l env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
  python3 -B -m json_oracle.cli "$probe"
```

returned the typed one-MiB budget rejection but peaked at 93,274,112 bytes RSS.
`cli.py:45` calls `Path.read_bytes()` before `parse()` applies the ceiling at
`parser.py:414-424`; acquisition therefore scales with the whole file rather than
the configured limit. This is not the same mechanism as nested observation
amplification, but both belong to one resource-bound cluster.

### 2.3 Projection/source integrity and typed failure are incomplete

The following current-byte probe is sufficient to reproduce the shared critic
finding:

```sh
env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B - <<'PY'
from dataclasses import replace
from json_oracle import canonicalize, parse, require_unicode_scalars

document = parse(b'"a"')
node = replace(document._nodes[0],
               utf16_units=(98,), scalars=(98,), scalar_valid=True)
print(canonicalize(replace(document, _nodes=(node,))))

document = parse(b'true')
node = replace(document._nodes[0], kind='false')
print(canonicalize(replace(document, _nodes=(node,))))

document = parse(b'"a"')
node = replace(document._nodes[0], span=(0,))
try:
    require_unicode_scalars(replace(document, _nodes=(node,)))
except Exception as error:
    print(type(error).__name__, str(error))
PY
```

Observed:

```text
b'"b"'
b'false'
ValueError not enough values to unpack (expected 2, got 1)
```

`projection.py:44-195` compares `raw` with the source slice but never reparses that
slice to bind decoded string units, literal kind, or complete relations to source.
It also unpacks a merely tuple-typed span before checking arity. `canonicalize()`
has an incidental-exception wrapper; `require_unicode_scalars()` at lines 198-202
does not. The README's promise that every corrupt identity, span, relation, kind,
counter, or root becomes one `projection_integrity` failure is false.

Fresh review found a narrower source-ownership consequence in the same cluster:

```python
source = bytearray(b'"a"')
document = parse(source)
source[1] = ord('b')
print(type(document._source).__name__, document.source_preserving_bytes())
```

This prints `bytearray bytearray(b'"b"')`. `parse()` does not require exact immutable
`bytes`, so the purported owned frozen source can be mutated after acceptance; an
optional projection then rejects a document produced by `parse()` itself. This is
not a new architecture request. It confirms that source type/ownership belongs in
the direct integrity repair.

### 2.4 CLI configuration is not total

The all-digit counterexample reproduces exactly:

```sh
env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B - <<'PY'
import subprocess, sys
result = subprocess.run(
    [sys.executable, '-B', '-m', 'json_oracle.cli',
     'fixtures/positive/jcs-sort.json', '--max-nodes', '9' * 5000],
    text=True, capture_output=True,
)
print(result.returncode, repr(result.stdout))
print(result.stderr.splitlines()[-1])
PY
```

Observed:

```text
1 ''
ValueError: Exceeds the limit (4300 digits) for integer string conversion: ...
```

`cli.py:17-19` admits every nonempty digit string, then performs a conversion that
can throw outside the caught typed failure algebra. The current tests cover `-1`
and `NaN`, not this accepted lexical class. Argument parsing also occurs outside the
typed block, so a complete CLI configuration gate must cover parser-level argument
failures as well as budget conversion; no accepted CLI configuration class may
produce a traceback or an empty observation.

### 2.5 Current governance is not sealed by the verifier

Exact command:

```sh
shasum -a 256 \
  ../../LOCK-CONSTITUTION.md \
  ../../waves/BB-VALUE-TAPE-P0.md \
  AUTHOR.md \
  ../JSON-W0-AUTHOR-RESEAL.md
```

The governing files are `b45d1e6a...` and `745b6e82...`; `AUTHOR.md:17-18` and the
sibling reseal at lines 36-40 name `3622262d...` and `710e50f4...` instead.
`verify.py:77-123` verifies cached specs and local manifests but never resolves or
hashes either governing file or the sibling anchor. Its green result therefore
proves the stale-governance check is absent, not that governance is current.

### 2.6 Python/Node identity and JCS delegation are behavioral samples, not closure

The current machine is arm64 macOS 26.4.1 build 25E253. Python reports CPython
3.14.5; Node reports v26.0.0 / V8 14.6.202.33-node.19. The advertised executable
hashes match, but the launchers do not contain the semantic runtimes:

```sh
otool -L /opt/homebrew/bin/python3
otool -L /opt/homebrew/bin/node
shasum -a 256 \
  /opt/homebrew/Cellar/python@3.14/3.14.5/Frameworks/Python.framework/Versions/3.14/Python \
  /opt/homebrew/Cellar/node/26.0.0/lib/libnode.147.dylib
```

Observed:

```text
Python launcher       52,448 bytes; recorded SHA 2477b47f...
Python framework   5,438,480 bytes; omitted  SHA b0cd4b1c...
Node launcher         68,384 bytes; recorded SHA 08dad058...
libnode/V8         70,269,024 bytes; omitted  SHA 0afe378c...
```

The Python standard library and extensions are omitted as well. Examples loaded by
the verifier include `_random.cpython-314-darwin.so` SHA `5fea56f5...` and
`_hashlib.cpython-314-darwin.so` SHA `cc93c51d...`. Node loads additional Homebrew
dylibs and OS frameworks. Neither a few version strings nor the launcher hash binds
this dynamic closure, startup environment, or OS ABI. Because `verify.py` imports
standard-library modules before `enforce_python()`, the present check cannot serve
as a pre-execution trust boundary.

RFC 8785 requires ECMA-262 7.1.12.1 including Note 2 for number serialization
(`data/specs/rfc8785.txt:282-295`). The current code uses CPython `float()` and
`repr()` plus local formatting (`projection.py:205-231`). The spec manifest honestly
states that RFC 7493, ECMA-262 2019, IEEE 754, and Unicode are not cached and that
the normative chain is not closed. The 104 Node rows have no checked-in producer
program or regeneration command; `generate.py` generates JCS expectations with the
implementation under test.

Positive behavior is strong and retained. In addition to the 104 checked rows and
RFC vectors, I ran this deterministic current-runtime differential:

```text
seed=0x878520260719
finite unique binary64 values=100000
Python _number versus Node JSON.stringify mismatches=0
```

No JCS byte error was found. The defect is reproducible authority and producer
closure, not a demonstrated formatting mismatch.

### 2.7 Current-review inputs are outside every enforced digest set

`generate.py:203-220` intentionally excludes all root `GESTALT-*.md` files from
implementation identity. That is correct. `verify.py:115-123`, however, verifies
only the three Gestalt-1 rows in `data/review-census.json` and accepts any superset
of review filenames. The successful replay observed five reports while checking no
digest for either Gestalt-2 critic. Either current critic could therefore have been
changed or removed without invalidating the verifier.

The remedy must not make review prose part of implementation identity. This report
freezes the exact two critic hashes in section 1, and its handoff supplies this
report's final byte hash. Future passes need the same reviewer-owned before/after
input hash check; the product file manifest should continue to exclude review prose.

## 3. Per-finding rulings

Verdicts apply to each numbered finding's defensible core, not automatically to
every sentence, priority label, or historical observation in its proof.

### 3.1 Critic A

| Finding | Classification | Independent ruling |
| --- | --- | --- |
| F1 — accepted nested input defeats 128 MiB | **CORRECT** | The exact 251,001-byte input is accepted, emits 125,621,753 bytes, and peaks at 411,074,560 bytes RSS. Per-container raw copies are the direct cause. |
| F2 — source/value disagreement and untyped corruption | **CORRECT** | Both `"a" -> "b"` and `true -> false` forgeries canonicalize; malformed span arity escapes `require_unicode_scalars()` as `ValueError`. This combines B C2 and C3. |
| F3 — Python identity insufficient and replay unavailable | **CORRECT, NARROWED** | The runtime-closure defect is correct. The present-tense replay claim is not retained: the exact top-level command now passes. The prior system-policy denial is historical and does not prove current non-replayability. |
| F4 — digit-only budget escapes typed CLI | **CORRECT** | A 5,000-digit value passes the lexical test and exits through raw `ValueError`, empty stdout, and traceback. |
| F5 — RFC 8785 number route not reproducibly closed | **CORRECT** | No byte mismatch was found, including 100,000 fresh cases, but the delegated sources, actual Python/Node runtime closures, corpus producer bytes, command, and regeneration gate are absent. |

### 3.2 Critic B

| Concern | Classification | Independent ruling |
| --- | --- | --- |
| C1 — author ledger names non-current governance | **CORRECT, NARROWED** | The defect is current, but B's printed “actual” hashes were an intermediate revision. Current authority is exactly `b45d1e6a...` / `745b6e82...`; the author still names `3622262d...` / `710e50f4...`. |
| C2 — semantic corruption passes integrity | **CORRECT** | Both string facts and primitive literal kind can disagree with source and change canonical bytes. |
| C3 — scalar projection leaks implementation exceptions | **CORRECT** | One-element span tuple escapes as raw `ValueError`; no single typed integrity boundary covers all projection entry points. |
| C4 — CLI reads the complete file before byte limit | **CORRECT** | A 64 MiB sparse file peaks at 93,274,112 bytes before returning the one-MiB rejection. |
| C5 — numeric CLI budget can escape typed configuration | **CORRECT** | Same reproduced defect as A F4. |
| C6 — exact binaries omit executing runtime code | **CORRECT** | The recorded 52 KiB/68 KiB launchers load omitted 5.4 MiB Python and 70.3 MiB libnode runtimes plus unsealed libraries and OS components. |
| C7 — current reports are mutable outside the census | **CORRECT** | The verifier accepts five reports while digest-checking only three. Keep prose excluded from implementation identity, but hash the exact current-pass inputs at the review boundary. |

No whole numbered finding or concern is **INCORRECT** or **UNPROVEN** after the
narrowing above. The non-retained subclaims are exact: critic A's current replay
failure is false now, critic A's author/governance match is false, and critic B's
intermediate governing hashes are not current.

## 4. Merged defect clusters

| Cluster | Inputs | Adjudicated defect |
| --- | --- | --- |
| Observation and acquisition bounds | A F1; B C4 | Accepted publication duplicates nested container slices and violates the named RSS envelope; CLI acquisition materializes bytes beyond its configured ceiling. |
| Source integrity and typed projections | A F2; B C2; B C3; fresh bytearray probe | Stored semantic facts are not fully bound to immutable source, and projection entry points do not share one total typed integrity boundary. |
| CLI configuration totality | A F4; B C5 | An admitted digit-only option can traceback; parser-level CLI configuration is outside the observation algebra. |
| Governing identity | B C1; correction to A's retained facts | Author and sibling receipts are not bound to the exact current constitution/wave, and the verifier cannot detect that fact. |
| Dynamic runtime and JCS authority | A F3; A F5; B C6 | Current behavior is promising, but launcher hashes do not identify Python/Node execution closure and the ECMAScript delegation/corpus cannot be independently regenerated. |
| Current-review evidence identity | B C7 | Implementation identity is correctly prose-free, but the two reports actually adjudicated were not immutable verifier inputs. |

These are six proof clusters, not twelve independent mechanisms. C2 and C3 are two
outcomes of one incomplete validator; A F4 and B C5 are one conversion path; A F3,
A F5, and B C6 share one authority/runtime boundary.

## 5. Narrow truths retained

The following facts survive and must be carried by exact hash and rerun through the
repair:

- The 40-file sealed subject is internally consistent at root
  `a1dc2769c74753212df6c12efc2eeb6e3f1bad635968bd8dcfdf88134f58825a`.
- The complete 318-row frozen suite replay passes: 95 mandatory accepts, 188
  mandatory rejects, and 35 explicit implementation-defined policies; base results
  are 116 accept / 202 reject.
- Accepted observations retain ordered relations, duplicate association, exact
  number lexemes, UTF-8 byte spans, decoded UTF-16/scalar facts, source hash/base64,
  root, and counters. Publication bytes are immutable; their current representation
  is simply too duplicative for nested containers.
- The parser is iterative through inclusive container depth 500, rejects depth 501,
  applies strict UTF-8, gives the declared BOM policy precedence, and separates
  configuration, encoding, syntax, depth, resource, scalar, projection, and I/O
  categories on the exercised valid-configuration paths.
- JCS string escaping, UTF-16 key ordering, duplicate rejection, lone-surrogate
  rejection, nonfinite rejection, Appendix B vectors, 104 checked Node cases, and
  100,000 fresh deterministic binary64 cases show no discrepancy. This is strong
  behavioral evidence, not closed delegation authority.
- The current verifier really does run two disposable roots, all 29 external tests,
  two suite/fixture/source/file regenerations, cache rejection, and cross-root byte
  comparison. No cache exists in the subject.
- The four substantive test owners remain external and source-isomorphic; the
  parser/oracle is compact and candidate-neutral. Direct repair is smaller than a
  new framework.
- `INCOMPLETE_ASSAY / oracle input only` is the honest terminal scope. The subject
  has no product query/navigation, recovery, streaming, incremental, BBNF-derived
  type, CSS, Rust/TypeScript/WASM/ARM, package, consumer, profile, candidate, lock,
  or production authority.

## 6. Ordered KISS repair and reseal authorization

Only the following direct repair is authorized. Order matters because regenerated
observations and receipts must describe the final implementation, not an
intermediate one.

### R1 — bound input acquisition and observation construction

1. Retain the owned source exactly once. Stop storing and emitting complete `raw`
   slices for array/object containers; source bytes plus half-open spans already
   preserve them. Keep only non-redundant leaf lexemes/facts.
2. Make the CLI read at most `max_input_bytes + 1` bytes after budgets are parsed;
   do not use unbounded `Path.read_bytes()`. Return the existing typed input-budget
   observation when the extra byte exists, including for growing/special files.
3. Add combined depth-plus-padding and maximum-shape gates that mechanically assert
   observation-size and fresh-process RSS bounds. Three favorable one-dimensional
   examples are insufficient.

### R2 — establish one source-derived typed integrity boundary

1. Require exact immutable `bytes` at `parse()` or make one explicit immutable copy
   before any document can escape; wrong input types must have a named typed result.
2. Reparse/rederive source spelling, decoded units, scalar facts, literal kind,
   container relations, spans, counters, and root before any observation or optional
   projection consumes a private document. Prefer comparison with one freshly
   derived immutable document over a second hand-maintained partial parser.
3. Route `observe`, `require_unicode_scalars`, and `canonicalize` through that one
   validator and normalize every malformed representation to
   `ProjectionIntegrityError`; do not depend on incidental catch lists.
4. Add external mutations for every field family, including string/source mismatch,
   literal kind, span arity/type, nested relation/source mismatch, counters, root,
   mutable source, and malformed private tuples.

### R3 — make CLI configuration total

1. Convert oversized decimal strings and argument-parser failures into the canonical
   `configuration` observation. No lexically accepted option, missing value, unknown
   option, or budget conversion may emit a traceback or empty stdout.
2. Test every option class before file I/O, including very long digits, and assert
   deterministic JSON plus nonzero exit for every rejection.

### R4 — bind current governance without coupling reviews to implementation

1. Update `AUTHOR.md` and the sibling reseal to the exact current constitution
   `b45d1e6a...` and wave `745b6e82...`, with a real clause delta from the prior
   sealed claims.
2. Make the top-level verifier hash the two supplied governing files before replay;
   a green local manifest alone must not pass stale governance.
3. Keep root `GESTALT-*.md` excluded from `data/file-manifest.json`. At each review
   boundary, hash the exact critics before and after adjudication and record those
   identities in the adjudication. Externally retain the final adjudication digest.
   Do not add mutable review prose to implementation identity.

### R5 — close the retained JCS and dynamic-runtime claim reproducibly

1. Replace launcher-only identity with a content-addressed execution closure checked
   before subject imports: Python launcher, framework library, standard-library and
   extension tree, transitive non-system libraries, architecture, startup flags and
   environment, and macOS build. Use one hermetic bundle/root digest rather than a
   manually curated partial list.
2. Seal the Node producer closure separately: executable, `libnode`/V8, transitive
   non-system libraries, architecture/OS build, producer program, command, seed,
   exact count, uniqueness rule, and output digest.
3. If the name `RFC 8785` is retained, cache or exactly provenance-bind its delegated
   RFC 7493, ECMA-262 2019, IEEE-754, and Unicode inputs; include a checked-in
   deterministic number-corpus producer and an independently regenerated large
   differential. Preserve the compact `_number` implementation only after an
   auditable equivalence argument plus the independent gate. Renaming the feature to
   a sampled non-authoritative projection is the only smaller alternative, but then
   it cannot serve as RFC 8785 oracle authority.

### R6 — regenerate once, externally anchor, and restart review

1. Regenerate every affected fixture/result row and source/file manifest after
   R1-R5. Record exact commands and the complete runtime/governance roots.
2. Run the full two-root verifier from a clean environment, independently reproduce
   every counterexample above, and externally anchor the new file-manifest digest.
3. Preserve the two current critic files and this adjudication by exact hash. New
   fresh critics and a new fresh adjudicator review the corrected bytes. Because
   this pass contains correct findings, the clean count remains zero.

## 7. Prohibited scope inflation

The repair does **not** authorize:

- a product value/tape, schema, HIR, query API, navigation layer, visitor, recovery,
  streaming, incremental engine, package, backend, or candidate import;
- a new generalized framework, registry tower, sidecar semantic/type authority,
  duplicate parser, or promotion of Python private dataclasses to product API;
- adding critic/adjudication prose to the implementation file manifest or making
  implementation identity change whenever a review is appended;
- modifying prior reports, governing documents, campaign ledgers, lock coverage,
  CSS evidence, candidates, product source, or production paths in this local
  repair;
- claiming that a finite Node differential is a proof of all ECMAScript numbers, or
  that a launcher digest is a complete runtime identity; or
- awarding local clean-pass, VT.W0, VT.W4/VT.W5, candidate, PT/BB lock, profile,
  performance, product, or production credit.

## 8. Final verdict and digest

**Every critic item is correct at the narrowed scope above. The JSON W0 oracle is
NOT READY. Repair R1-R6, reseal, and restart the local clean count at zero.**

The base RFC 8259 behavior and sampled JCS behavior remain useful evidence. They do
not outweigh the reproduced 411 MiB accepted-shape peak, unbounded CLI acquisition,
source-inconsistent canonicalization, untyped CLI/projection escapes, stale current
governance, partial dynamic-runtime identity, unreproducible ECMAScript producer,
and mutable current-review inputs.

A literal final-file SHA-256 cannot be embedded in the bytes it hashes without a
self-reference. The adjudicator handoff therefore reports the final byte-for-byte
file SHA-256 externally. The following embedded digest is non-circular and exactly
verifiable: replace only its 64 hexadecimal characters with 64 ASCII zeroes, then
hash the complete file.

`canonical_report_sha256: 28dc5b7b37ecc84b9370421c920aa1b776f43fee7e89de3bcd800029da5a2e74`
