# JSON W0 hostile Gestalt-1 review — critic A

Date: 2026-07-18  
Reviewer role: first fresh hostile Gestalt-1 critic A  
Declared model: `gpt-5.6-sol`  
Recorded model: `gpt-5.6-sol`  
Disposition: **NOT CLEAN — reopen the JSON W0 oracle**  
Consecutive clean hostile Gestalt count after this review: **0**

This is a correctness review of the sealed JSON W0 oracle. It is not an
authorization for product use, candidate credit, a repair, a regeneration, or
a claim that the enclosing tranche is complete. The oracle continues to call
itself `INCOMPLETE_ASSAY`; this review preserves that boundary.

## 1. Independence and review boundary

I treated the subject as wrong until the evidence survived an independent
attack. I read the constitution, current wave instructions, author report,
README, all source and tests, `verify.py`, all fixture bytes and manifests, all
318 suite decisions and all 35 implementation-defined policies, and the full
cached RFC 8259, ECMA-404, and RFC 8785 publications. The ECMA-404 PDF was
checked as a 16-page PDF using full-text extraction plus rendered normative
pages; a stalled local `pdftotext` attempt was abandoned without changing the
subject. No network was used.

The upstream corpus was the independently pinned, clean checkout at
`/private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite`. I did not use an
author-supplied editable clone, change the corpus, regenerate checked-in data,
edit implementation/data/test files, or repair defects. The only file produced
by this pass is this review.

The independent attacks were not limited to replaying author tests. They
included an independent Node JSON parser classification of every corpus file,
UTF-8 boundary bytes, decoded-equivalent duplicate names, scalar-pairing
patterns, byte-span probes with multibyte text, resource-boundary values,
100,000-deep input, a five-million-code-unit decoded-budget stress input, and
invalid public `Budgets` values. Passing results and defects are both recorded
below.

## 2. Frozen subject ledger

The subject inspected before this review file was appended was the following.
All digests are SHA-256.

| Subject | Frozen identity |
|---|---|
| Lock constitution | `LOCK-CONSTITUTION.md`, `3622262dcbf638dc911564bef8ee1babd65cb81a9175c72141da3166634ca85a` |
| Current wave instructions | `restart/skinny/README.md`, `710e50f4901b63642b0645051b0ef055d942b8ce377529ae7f793f1e9f35c6c1` |
| Author report | `AUTHOR.md`, `aa5364a4c432fe690e1009d2cd8f5ca26648f0961b62b096be69e70844ab9f75`, 9,883 bytes |
| Oracle README | `README.md`, `ab8c78f9453e588549ad99ae1cc66520694c10e8761f40e810dc95602aea4dce`, 6,931 bytes |
| File census | `data/file-manifest.json`, 42 declared files plus the excluded self-reference; manifest content records the exact leaf hashes and sizes |
| Source manifest | `data/source-manifest.json`, `6941fd4aa6a82d6543ca01a271a6f1722c9ad01c4219c59f3afcaf70ca02fd76`, 1,999 bytes |
| Fixture manifest | `fixtures/manifest.json`, `5929dd25dd340275a84ac4970c091d87b2110b614dd1c93ba0130206f2aa8430`, 10,155 bytes |
| Corpus manifest | `data/json-test-suite-manifest.json`, `b4df1d9d69075bce2d1b082803986b00845422b258daca77c5da0b2dcb7427df`, 62,730 bytes |
| Expected corpus results | `data/expected-results.jsonl`, `a3ce8544c0fbee1ca38cde9857864bc78ba4a8563ff21356f9969b099406d2ad`, 133,940 bytes |
| `i_` policy | `data/i-case-policy.json`, `f93fe7bda7ac329a1ed15614f411ffb7e3f1ccb82881c06c6b57c892a33c518c`, 6,097 bytes |
| RFC 8259 cache | `data/specs/rfc8259.txt`, `61a5378f4255c720beb2a4b4a63b29540147c140f36988bf086291989b4cd2d7`, 28,360 bytes |
| ECMA-404 cache | `data/specs/ecma-404-2nd-edition.pdf`, `760a6f77c38172f4de333412292eaed092f33852592ca0cce9b7535a6e415fcf`, 639,695 bytes |
| RFC 8785 cache | `data/specs/rfc8785.txt`, `63d52294eb0e3f0014174288186d388b4ddbf2c67d1ce8af1d9726eb0c3ab240`, 41,879 bytes |

The source-manifest leaves were:

| File | SHA-256 | Bytes |
|---|---:|---:|
| `src/json_oracle/__init__.py` | `8d87fc75c254206ef3453e2d3402ff8242bba44a4e58ece4c1ed48c51a66348d` | 427 |
| `src/json_oracle/cli.py` | `0a68feb9d25cb948e14c1ff938a04d901fe8fba0290d4c9ae4e8c04945911ee8` | 2,024 |
| `src/json_oracle/generate.py` | `718de2494d74719432b02d649e0b49b575c89b8d41701f2cb6d6342db5f3fcb0` | 9,256 |
| `src/json_oracle/parser.py` | `8f4a51c69d0f5cbfbfb9a3e3dcbb7128088981c7af1ab5147d3b03b40aab66c9` | 13,379 |
| `src/json_oracle/projection.py` | `7dbf57d21c244c46bfae65c0bf9d778d83c5fd956267a198132631521237a0e7` | 5,222 |
| `tests/__init__.py` | `7e69d62e303ff5c9b096d41733b95cb56ddbd2072f0e369d237c368604455d22` | 56 |
| `tests/json_oracle/__init__.py` | `fbadf3135906c6bce09a79094c0c3ec40927123bb3432e49279ee06ad560e5b1` | 63 |
| `tests/json_oracle/test_cli.py` | `0ae22efa6fde6234794212dfe9ac6bbb171a04797cfee701cf588ab792934ba9` | 1,677 |
| `tests/json_oracle/test_generate.py` | `519ae4ab50cbb317be9c70d4b3d0702e8d432af4f1979e9665be9c3ac907c4d9` | 1,549 |
| `tests/json_oracle/test_parser.py` | `f4296f110b7636f4a43267e042371b175315c49adce730ee7dd66540b6fd0868` | 3,367 |
| `tests/json_oracle/test_projection.py` | `320c95c74b3a9fed9c4b98648bab1d3f68f09368290bd97dce6f0966d7166035` | 4,413 |
| `verify.py` | `1eed919d59833e43302e346c82227cdf43b27fea8d025ffbe716059c9f3fe36b` | 4,745 |

The upstream corpus pin was clean and exact:

| Upstream item | Identity |
|---|---|
| Git commit | `1ef36fa01286573e846ac449e8683f8833c5b26a` |
| Commit tree | `f3d680c9b9065eaa6bfa6dac14adade610d6e054` |
| `test_parsing` tree | `b936f9acdd24b9f5fefe68b90b9beab2c681137a` |
| Corpus files | 318 |
| Git status | clean |

There is one ledger discrepancy: the author report still records an earlier
wave digest beginning `3b5b2308…`, while the wave document actually reviewed is
the exact `710e50f4…` object above. Inspection showed a candidate-count wording
update rather than a JSON semantic change, but the sealed author ledger is no
longer an exact root ledger. See finding F-08.

## 3. Reproduced commands and observations

All commands below were run from
`restart/skinny/tranches/sk-v25/conformance/json` unless stated otherwise.

### 3.1 Documented gates

```sh
JSON_TEST_SUITE=/private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  PYTHONPATH=src python3 -m unittest discover -s tests -t . -v
```

Result: exit 0, 18/18 tests passed. `unittest` reported 0.331 seconds. The
single outer `/usr/bin/time -p` observation was `real 17.85`, `user 0.23`,
`sys 0.13`; the anomalous outer wall value was not used as a performance claim.

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite
```

Two fresh pre-review runs both exited 0 under Python 3.14.5:

| Run | tests-1 | tests-2 | regeneration-1 | regeneration-2 | outer real/user/sys |
|---|---:|---:|---:|---:|---:|
| A | 0.324080 s | 0.318562 s | 0.109564 s | 0.103733 s | 0.91 / 0.56 / 0.29 s |
| B | 0.330284 s | 0.294619 s | 0.107123 s | 0.112833 s | 0.90 / 0.53 / 0.30 s |

Each run executed the 18-test suite twice, regenerated both corpus artifacts
twice, byte-compared the results, checked the manifests, and printed a summary
with `expected_rows:318`, `passes:2`, and `network:"unused"`.

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -m json_oracle.cli \
  fixtures/positive/jcs-sort.json --jcs
```

Result: exit 0; `real 0.04`, `user 0.03`, `sys 0.01` seconds. The canonical
bytes had SHA-256
`baf64787d19287b9673c6e34f53a8ec7552ef67be5ad05c5e76f3b25c82ce5f2`.

### 3.2 Independent corpus decision check

The clean upstream `test_parsing` files were independently fed to Node v26.0.0
using fatal UTF-8 decoding and `JSON.parse`, with the oracle's explicit BOM
campaign rule applied separately. The classification result was:

| Class | Files | Accepted | Rejected | Required mismatches |
|---|---:|---:|---:|---:|
| `y_` | 95 | 95 | 0 | 0 |
| `n_` | 188 | 0 | 188 | 0 |
| `i_` | 35 | 21 | 14 | 0 against the 35-row policy |
| Total | 318 | 116 | 202 | 0 |

The independently enumerated corpus manifest hash was
`b4df1d9d69075bce2d1b082803986b00845422b258daca77c5da0b2dcb7427df`.
This is useful independent evidence for the base accept/reject decisions; it is
not independent evidence for the oracle's detailed node observations because
those observations are produced by this oracle's own generator.

### 3.3 Independent semantic and adversarial probes

The following completed probes were separate from the checked-in unit cases:

- Ten strict-UTF-8 boundary encodings were accepted, including U+007F,
  two-/three-/four-byte lower and upper boundaries, and U+10FFFF. Ten malformed
  forms were rejected at their first offending byte: lone continuation,
  overlong forms, truncations, raw surrogate encoding, values above U+10FFFF,
  and illegal `F5` lead bytes.
- The decoded-equivalent duplicate pairs `"a"`/`"\u0061"`, `"/"`/`"\/"`,
  raw U+1D11E/a surrogate-pair escape, and `"\\"`/`"\u005c"` remained distinct
  ordered base members with equal decoded UTF-16 names. JCS rejected each as
  `duplicate_object_name`.
- For ` ["é",{"𝄞":0}] `, the array span was `[1,18]`, the first string
  `[2,6]`, object `[7,17]`, key `[8,14]`, and number `[15,16]`; every slice
  selected the exact original UTF-8 bytes.
- Raw and escaped U+1D11E both produced UTF-16 units `[55348,56606]` and scalar
  `[119070]`. High-high-low and low-high-low escape runs preserved the lone
  surrogate and paired the adjacent valid high-low run; the optional scalar
  gate rejected the remaining lone scalar.
- The full RFC 8785 sorting example, including CR, ASCII `1`, U+0080,
  U+00F6, U+20AC, U+1F600, and U+FB33 names, was emitted in UTF-16 code-unit
  order.
- One decoded unit passed a decoded limit of one; two units failed. `[0]`
  passed `max_nodes=2,max_aggregate_entries=1`; `[0,1]` failed the entry limit
  at byte 3. Raised `OracleError` instances exposed only typed category, code,
  and byte offset; no partial public `Document` was returned.
- A 100,000-deep balanced array parsed and base-canonicalized with deliberately
  raised limits, without Python recursion: 200,000 bytes, 100,000 nodes,
  observed depth 100,000, identity output, `real 0.28` seconds, peak RSS
  91,881,472 bytes, and `tracemalloc` peak 81,527,288 bytes.
- A five-million-ASCII-unit string with
  `max_decoded_utf16_units=1` failed only at its closing quote (byte 5,000,001)
  after `real 0.68` seconds, peak RSS 114,180,096 bytes, and `tracemalloc` peak
  103,809,528 bytes. This is the counterexample behind F-01.
- Invalid public budget values produced the outcomes in F-02, including an
  impossible negative byte offset, bypass by NaN, scalar acceptance under a
  negative depth limit, and an untyped `TypeError`.

Representative exact resource commands were:

```sh
/usr/bin/time -lp env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 - <<'PY'
import tracemalloc
from json_oracle import Budgets, OracleError, parse
s = b'"' + b'a' * 5_000_000 + b'"'
tracemalloc.start()
try:
    parse(s, Budgets(max_input_bytes=len(s), max_decoded_utf16_units=1))
except OracleError as exc:
    print(exc.category, exc.code, exc.offset, tracemalloc.get_traced_memory())
PY
```

```sh
/usr/bin/time -lp env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 - <<'PY'
import tracemalloc
from json_oracle import Budgets, parse, canonicalize_base
n = 100_000
s = b'[' * n + b']' * n
tracemalloc.start()
d = parse(s, Budgets(max_input_bytes=len(s), max_depth=n, max_nodes=n + 1,
                     max_aggregate_entries=n + 1))
print(d.counters, canonicalize_base(d) == s, tracemalloc.get_traced_memory())
PY
```

These measurements show that iterative nesting is real, while decoded-unit
accounting is not an effective allocation guard.

## 4. Standards matrix

| Authority / policy | Requirement examined | Evidence | Disposition |
|---|---|---|---|
| RFC 8259 | Complete JSON grammar and top-level scalar support | 95 mandatory positives, 188 mandatory negatives, local grammar probes | PASS |
| RFC 8259 §8.1 | UTF-8 interoperability | Strict boundary and malformed-byte probes; first offending byte retained | PASS |
| RFC 8259 §8.1 plus declared local policy | Generators omit BOM; parsers may ignore it; this oracle deliberately rejects it | Ordinary BOM fixture rejects, but BOM followed by malformed UTF-8 is reported as UTF-8 at byte 3 rather than the declared BOM error at byte 0 | FAIL (F-04) |
| RFC 8259 §§6–7 | Exact number grammar and string escapes | Corpus decisions and exact raw-number preservation | PASS for base syntax/preservation |
| RFC 8259 §8.2 | Escaped surrogate edge cases | Ordered UTF-16 units and optional scalar validity retained separately | PASS |
| RFC 8259 §4 | Object member ordering/duplicate interoperability | Base mode preserves all ordered members; policy does not invent uniqueness authority | PASS |
| ECMA-404, 2nd ed. | JSON syntax only, without semantic/type invention | Full cached publication reviewed; syntax behavior agrees with corpus | PASS |
| RFC 8785 §3.2.2.2 | Number serialization | Checked fixture/Appendix-B gate cases passed; raw inputs are converted through binary64 | PASS on exercised cases; offline authority closure remains incomplete |
| RFC 8785 §§3.2.2.2–3 | String serialization, scalar validity, UTF-16 name sorting | Independent escape/scalar/sort probes passed | PASS |
| RFC 8785 / I-JSON | No duplicate decoded object names | Four independently constructed decoded-equivalent pairs rejected | PASS |
| RFC 8785 dependency chain | I-JSON, ECMA-262 number/string algorithms, IEEE 754 and Unicode dependencies available offline | Only RFC 8259, ECMA-404, and RFC 8785 are cached | FAIL (F-03) |
| Local `i_` campaign policy | All implementation-defined cases are explicit and reproducible | 35/35 rows agree with independent classification: 21 accept, 14 reject | PASS |

The BOM rejection is a declared campaign policy, not a false attribution to RFC
8259. The defect is the implementation's error precedence relative to that
declared policy.

## 5. Feature matrix

| Feature | Hostile evidence | Result |
|---|---|---|
| Full base grammar | Entire 318-file corpus plus independent Node decision pass | PASS |
| UTF-8 strictness and byte offsets | Boundary/malformed byte campaign | PASS |
| Top-level scalars | Corpus and direct scalar probes | PASS |
| Exact numeric lexemes | Raw numeric bytes retained and base canonicalization preserves them | PASS |
| Duplicate/order preservation | Multiple raw/escaped decoded-equivalent names retained in base order | PASS |
| Source preservation | Owned immutable input bytes, exact multibyte spans and byte slices | PASS |
| UTF-16 units and scalar pairing | Raw/escaped supplementary scalar and mixed surrogate runs | PASS |
| Optional scalar-validity gate | Lone surrogate rejected only when requested | PASS |
| Base canonicalization | Whitespace removal with lexeme/member preservation; deep identity probe | PASS |
| JCS string escaping | Control/quote/backslash/scalar probes and gate cases | PASS on exercised cases |
| JCS UTF-16 member sorting | Full seven-name RFC ordering example | PASS |
| JCS duplicate handling | Four decoded-equivalent duplicate constructions | PASS |
| JCS number conversion | Gate cases passed; no hostile mismatch established | PASS on exercised cases, not sufficient to cure F-03 |
| Iterative deep nesting | 100,000-depth direct probe | PASS |
| Input/node/entry/depth limits at valid boundaries | Direct boundary probes and fixtures | PASS |
| Budget domain validation | Negative, NaN, and non-integer values | FAIL (F-02) |
| Decoded-unit resource bound | Five-million-unit, limit-one stress input | FAIL (F-01) |
| Typed failures/no partial publication | Ordinary parse failures | PASS; invalid budget type escapes as `TypeError` (F-02) |
| BOM failure identity | BOM plus malformed suffix | FAIL (F-04) |
| Offline deterministic regeneration | Two fresh generations per verifier run, byte-identical | PASS pre-review |
| Review-aware sealed census | Required review file is not an allowed exclusion | FAIL (F-05) |
| Independent detailed expected observations | Golden rows and two evidence copies come from the same generator | PARTIAL/FAIL (F-06, F-07) |

## 6. Tranche matrix

| JSON W0 tranche claim | Status | Reason |
|---|---|---|
| Candidate-neutral assay | PASS | No candidate implementation is imported or selected. |
| Official syntax/canonicalization basis | PARTIAL | Three primary publications are cached, but RFC 8785's normative algorithm/dependency chain is not closed offline. |
| Complete pinned upstream syntax corpus | PASS | Clean 318-file tree, exact commit/tree/subtree identities, all required decisions agree. |
| Source-preserving exact structural oracle | PASS on examined semantics | Exact bytes, spans, raw numbers, duplicate order, UTF-16 units, and scalar facts survived hostile probes. |
| Optional JCS projection | PASS on examined output cases | Escaping, scalar gate, decoded duplicate rejection, UTF-16 sorting, and gate number cases behaved correctly. |
| Explicit bounded adversarial behavior | FAIL | Invalid budget domains and post-allocation decoded accounting defeat the claim. |
| Typed, stable failures | FAIL | Invalid public budget values can emit impossible offsets or an untyped Python exception; BOM precedence is unstable against its declared identity. |
| Offline reproducibility | PARTIAL/FAIL | The current pre-review gate reproduces, but exact JCS authority is incomplete and the required critic file breaks the file census. |
| Independent evidence | PARTIAL | Corpus decisions have an independent Node check; detailed generated observations and the two evidence directories share one producer. |
| KISS/parsimony | PARTIAL/FAIL | Core parser is compact and iterative, but three default limits are unreachable and 393,340 checked-in evidence bytes duplicate primary artifacts. |
| Product/candidate credit | NOT AUTHORIZED | The wave remains an incomplete, candidate-neutral assay. |

## 7. KISS audit

| Dimension | Observation | Disposition |
|---|---|---|
| Runtime dependencies | Production code is standard-library-only. | GOOD |
| Structural algorithm | One explicit parser stack; no recursion dependence; 100,000-depth case works. | GOOD |
| Production size | Five source modules, 821 physical lines; parser 341 and projection 148 lines. | REASONABLE |
| Verification size | `verify.py` is 124 lines and performs two fresh generations. | GOOD locally, but see F-05 |
| Budget surface | Five knobs, but default 4,000,000 decoded units cannot be reached under the default 1,048,576-byte input cap; default one-million node and entry caps are likewise dominated by the input cap. | NOT PARSIMONIOUS |
| Budget implementation | Decoded units are counted only after full string accumulation. | INCORRECT (F-01) |
| Checked-in evidence | Two directories duplicate the 133,940-byte expected file and 62,730-byte suite manifest exactly: 196,670 bytes per directory, 393,340 bytes total. | REDUNDANT (F-07) |
| Golden independence | Primary expected output and both evidence copies are made by the same generator. | WEAK (F-06) |

The good iterative parser design does not offset a resource knob that consumes
the resource before enforcing the bound. Dominated defaults also add concepts
without adding protection.

## 8. Greater-product boundary

| Question | Answer |
|---|---|
| Does this oracle choose among the four current complete candidates? | No. It is candidate-neutral. |
| Does it establish semantic/type authority for the greater language? | No; its own spec manifest disclaims that authority. |
| Does a passing pre-review verifier authorize product use? | No. |
| Can JSON W0 receive clean hostile credit from this review? | No; F-01 and F-02 alone reset the clean count. |
| Does this review claim tranche or project completion? | No. |
| Required next state | Reopen the oracle, preserve `INCOMPLETE_ASSAY`, repair and reseal under a new ledger, then begin hostile counting again at zero. |

## 9. Numbered findings

### F-01 — P1 — decoded-unit limit is enforced after allocation

**Location:** `src/json_oracle/parser.py:108-119`, especially the check at
lines 118-119.

**Proof:** The string loop appends every decoded UTF-16 unit and scalar fact to
Python collections. It compares the aggregate count with
`max_decoded_utf16_units` only when it reaches the closing quote. A
5,000,000-unit ASCII string under a limit of one failed at byte 5,000,001 only
after peak RSS reached 114,180,096 bytes and traced Python allocation reached
103,809,528 bytes.

**Why it matters:** This is not a bounded adversarial parser with respect to the
advertised decoded-unit resource. The bound describes a post-hoc rejection,
not an allocation guard. An attacker can spend memory proportional to the
raised input allowance regardless of a much smaller decoded-unit allowance.
The default decoded limit is additionally unreachable beneath the smaller
default byte limit.

**Disposition:** correctness blocker; reopen and reset clean count.

### F-02 — P1 — the public budget domain is unvalidated

**Location:** `src/json_oracle/parser.py:12-17,97-104,118-119,219-221,320-321`;
CLI values enter directly at `src/json_oracle/cli.py:19-34`.

**Observed counterexamples:**

| Construction | Observed outcome |
|---|---|
| `Budgets(max_input_bytes=-1)`, input `b"0"` | typed resource failure at impossible byte offset `-1` |
| CLI `--max-input-bytes -1`, input `0` | same impossible offset, exit 1 |
| `Budgets(max_depth=-1)`, scalar `b"0"` | accepted; a container at the same setting rejects at byte 0 |
| CLI `--max-depth -1`, scalar `1e400` | accepted, exit 0 |
| `Budgets(max_input_bytes=float("nan"))` | input accepted because the comparison is false |
| `Budgets(max_nodes="1")` | uncaught `TypeError`, not `OracleError` |
| Negative node/entry/decoded values | inconsistent rejection dependent on whether the relevant event occurs; an empty aggregate can bypass an entry limit of `-1` |

**Why it matters:** `Budgets` is exported as public API and the CLI explicitly
admits negative integers. The oracle promises stable typed failures and exact
byte offsets, but invalid configuration can bypass a limit, produce an
impossible offset, or escape as an implementation exception. Type annotations
do not validate Python runtime inputs.

**Disposition:** correctness blocker; define and enforce a finite,
non-negative-integer domain before parsing.

### F-03 — P1 — the cached JCS authority is not closed offline

**Location:** `data/spec-manifest.json`; `README.md:15`; RFC 8785 §§3.1,
3.2.2.2, 3.2.3 and its references.

**Proof:** The spec manifest contains only RFC 8259, ECMA-404, and RFC 8785.
RFC 8785 delegates admissible data to I-JSON/RFC 7493, string and number
serialization to the ECMAScript specification, numeric representation to IEEE
754, and Unicode behavior to the Unicode standard. In particular, RFC 8785
does not itself contain a complete binary64-to-decimal algorithm. None of those
delegated authorities or a pinned conforming implementation identity is in the
offline manifest.

**Why it matters:** The cached publications are sufficient to understand the
shape of JCS but not to derive or independently verify its exact serialization
algorithm without unpinned ambient knowledge. That contradicts the README's
claim that byte-hashed cached publications make verification network-free.
Passing exercised output cases does not repair an authority/provenance hole.

**Disposition:** reopen the spec ledger or narrow the offline-authority claim.

### F-04 — P2 — a leading BOM does not reliably receive the declared BOM error

**Location:** `src/json_oracle/parser.py:320-327`; declaration at
`README.md:38`.

**Counterexample:** `b"\xef\xbb\xbf\x80"` yields
`encoding:invalid_utf8` at byte 3. The implementation decodes the entire input
at line 323 and checks the leading BOM only afterward at lines 326-327. The
README says a leading BOM is rejected as `encoding:bom_forbidden`.

**Why it matters:** This is an attribution and precedence defect in the local
campaign contract. It is not a claim that RFC 8259 requires rejection; RFC
8259 permits a parser to ignore a BOM, and this oracle deliberately chose the
stricter rule.

**Disposition:** reopen; make the stated leading-byte policy and offset stable.

### F-05 — P2 — appending the required critic makes the verifier's file census fail

**Location:** `verify.py:99-110`; corresponding generator exclusion at
`src/json_oracle/generate.py:222-234`.

**Proof:** The verifier defines `actual_files` as every file beneath the oracle
except `data/file-manifest.json`. The sealed manifest declares 42 files and has
no review-file exclusion. Therefore this required
`GESTALT-1-CRITIC-A.md` becomes an undeclared 43rd non-self file as soon as it
exists. This follows directly from the enumerated set comparison; I did not
mutate or regenerate the manifest to hide the issue.

**Why it matters:** The documented pre-review gate passed twice, but the sealed
subject cannot coexist with its required review while retaining replayable file
integrity. A review artifact needs an explicit boundary in the census.

**Disposition:** reopen the sealing convention. The pre-review pass remains
valid evidence for its frozen subject; it is not a post-review pass.

### F-06 — P2 — the test gate omits load-bearing resource/configuration mutations

**Location:** `tests/json_oracle/test_parser.py`,
`tests/json_oracle/test_projection.py`, `tests/json_oracle/test_cli.py`, and
`src/json_oracle/generate.py:96-153`.

**Proof:** The 18 tests cover ordinary valid limits, corpus labels, projections,
and deterministic generation, but do not test negative/non-integral/non-finite
budget values, decoded-budget enforcement before allocation, a BOM followed by
invalid bytes, or the required review-file census transition. Those omissions
allowed F-01, F-02, F-04, and F-05 to pass both documented verifier runs.
Detailed expected rows and both checked-in evidence copies are also emitted by
the same generator they are used to check; only the independent corpus class
labels constrain the base decision from outside that producer.

**Disposition:** add mutation-sensitive tests with independent invariants;
do not count copy equality as independent semantic evidence.

### F-07 — P3 — two checked-in regeneration directories are byte-for-byte redundant

**Location:** `evidence/regeneration-a`, `evidence/regeneration-b`, and
`verify.py:70-79`.

**Proof:** In both directories, `expected-results.jsonl` has the same
`a3ce8544…` digest and 133,940-byte size as the primary, and
`json-test-suite-manifest.json` has the same `b4df1d9d…` digest and 62,730-byte
size as the primary. Each directory is therefore a 196,670-byte duplicate;
together they add 393,340 bytes. The verifier already performs two fresh
temporary regenerations and compares them to the primary artifacts.

**Why it matters:** Identical copies made by one generator add storage and
manifest surface, not independent evidence. This is a parsimony defect rather
than a base-parser semantic defect.

**Disposition:** remove the copies on the next authorized reseal or document a
distinct evidentiary property they provide.

### F-08 — P2 — the author root ledger names a stale wave object

**Location:** the immutable-input ledger in `AUTHOR.md`.

**Proof:** The report records a wave digest beginning `3b5b2308…`; the current
wave object read for this review is
`710e50f4901b63642b0645051b0ef055d942b8ce377529ae7f793f1e9f35c6c1`.
The observed content change concerns the current four complete candidates
versus an older eight-candidate bank, not JSON parsing semantics.

**Why it matters:** A sealed immutable-input ledger must identify the object it
claims governed the seal. Even a wording-only upstream edit makes the claimed
root digest stale and prevents exact replay from that report alone.

**Disposition:** record the exact governing wave object on reseal; no candidate
credit follows from this observation.

## 10. Gate dispositions and clean count

| Gate | Disposition |
|---|---|
| Frozen pre-review file/source/spec hashes | PASS, subject as ledgered |
| Clean pinned upstream corpus | PASS |
| 18-test gate, twice through verifier | PASS pre-review |
| Two fresh deterministic regenerations per run | PASS pre-review |
| Independent mandatory and policy corpus decisions | PASS, 318/318 |
| Base grammar/source/duplicates/numbers/UTF-16 facts | PASS on hostile probes |
| Exercised JCS escaping/sorting/duplicates/numbers | PASS on exercised cases |
| Resource guarantees | **FAIL: F-01, F-02** |
| Stable typed failure contract | **FAIL: F-02, F-04** |
| Offline JCS authority | **FAIL: F-03** |
| Review-compatible reproducibility | **FAIL: F-05** |
| Mutation sensitivity / independent golden evidence | **FAIL: F-06** |
| KISS/parsimony | **FAIL: F-01, F-07; dominated default limits** |
| Immutable author root ledger | **FAIL: F-08** |

Because this first fresh hostile review found correctness defects, the resulting
clean hostile Gestalt count is **0**, not 1. The appropriate status is
**reopen JSON W0, retain `INCOMPLETE_ASSAY`, and do not grant candidate or
greater-product credit**.
