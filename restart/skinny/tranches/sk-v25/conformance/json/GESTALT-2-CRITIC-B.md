# GESTALT-2 critic B — JSON W0 repaired assay

## Identity, premise, and boundary

- Role: fresh hostile critic B; no author or adjudicator authority.
- Model / reasoning: `gpt-5.6-sol` / `xhigh`.
- Canonical task: `/root/json_w0_g2_review_b_r2`.
- Canonical session ID: `019f794f-7640-7520-b012-0b4332de987b`.
- Date / time zone: 2026-07-19 / `America/New_York`.
- Premise: acceptance was not assumed or inherited from author receipts, checked-in
  baselines, prior review state, or a green verifier.
- Read boundary: current subject source, external tests, fixtures, data and
  manifests, `README.md`, `AUTHOR.md`, `verify.py`, the sibling
  `../JSON-W0-AUTHOR-RESEAL.md`, current `../../LOCK-CONSTITUTION.md`, and current
  `../../waves/BB-VALUE-TAPE-P0.md`. No content from any `GESTALT-*.md`,
  `RUN-LEDGER.md`, `CHALLENGE-LEDGER.md`, or other reviewer report was displayed,
  inspected, or used as evidence; the subject verifier performed only its opaque
  declared-report digest checks.
- Mutation boundary: no implementation, test, fixture, manifest, receipt, campaign,
  product, or ledger file was edited. This report is the only written file.

The checked subject anchor itself is internally consistent:
`data/file-manifest.json` hashes to
`a1dc2769c74753212df6c12efc2eeb6e3f1bad635968bd8dcfdf88134f58825a`,
enumerates the exact 40 non-review sealed paths, and matches every current size and
digest. That fact is evidence, not acceptance.

## Independent execution census

The ordinary sealed command completed successfully on the declared Apple arm64
CPython 3.14.5 interpreter:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite
```

It ran 29 tests in each of two disposable roots, regenerated the suite, fixtures,
source manifest, and file manifest in both roots, and returned exit 0. The summary
reported 318 expected rows, four pre-existing root review reports, Python launcher
hash `2477b47fa3ae65b9574eb18a15edb364e96948eaa1875ad3f1c80d780efc9c12`,
and the subject anchor above. Independent census scripts also found:

- 318 unique suite inputs; 116 base accepts and 202 base rejects; 99 JCS accepts,
  17 typed JCS rejects, and 202 base-rejected/not-applicable rows;
- 15 unique local fixtures with matching bytes; 104 unique number-corpus bit
  patterns; all cached specification, source, and subject-manifest hashes match;
- all 104 checked-in binary64 rows replay against the installed Node 26.0.0
  `JSON.stringify` behavior; and
- an additional deterministic differential over 100,000 finite pseudorandom
  binary64 values found no `_number`/Node `JSON.stringify` discrepancy.

Those positive facts support the narrow RFC 8259/JCS behavior exercised by the
sealed corpus. They do not cure the counterexamples below.

## Concerns

### C1 — BLOCKER: the sealed author ledger and sibling receipt name governing bytes that are not current

`AUTHOR.md:17-18` declares current governing hashes
`3622262d...` for `LOCK-CONSTITUTION.md` and `710e50f4...` for
`BB-VALUE-TAPE-P0.md`. The external reseal repeats those identities and calls the
second one the “current wave” at `../JSON-W0-AUTHOR-RESEAL.md:36-40`. The current
files actually hash to different bytes:

```sh
shasum -a 256 ../../LOCK-CONSTITUTION.md ../../waves/BB-VALUE-TAPE-P0.md
```

Observed:

```text
0304c49365d63483563271b21d2f9cb39ca0bdd802ba96ede350c979e2f0e51c  ../../LOCK-CONSTITUTION.md
995972ef76b689a1d4847c7ab8df3708867cc3011aaf155080208bed5cc056ee  ../../waves/BB-VALUE-TAPE-P0.md
```

This is not an unsealed prose typo: `AUTHOR.md` is itself bound by
`data/file-manifest.json:8-11`, and the sibling receipt correctly anchors that
stale-ledger subject. `verify.py:77-123` checks local specifications and subject
manifests but never checks either governing file or the sibling receipt, so the full
green verifier above reproduces the failure to detect the drift. The repaired claim
therefore has no current constitution/wave source seal, and its clause delta cannot
establish current wave fidelity.

### C2 — BLOCKER: projection integrity accepts source-inconsistent semantic corruption and emits different JCS bytes

For strings, `_integrity` checks only that the raw span is quoted and that scalars
are self-consistent with the stored UTF-16 tuple
(`src/json_oracle/projection.py:77-91`). It never decodes the raw string again and
compares that result with the stored units. For primitive literals, the generic leaf
branch at `projection.py:145-146` likewise never binds `kind` to the raw spelling.
Consequently, a corrupt private document can pass integrity and silently change the
canonical value, contrary to `README.md:61-66` and the corrupt-representation typed
failure required by wave §4 (`BB-VALUE-TAPE-P0.md:152-155`) and mutation §12
(`BB-VALUE-TAPE-P0.md:468-474`).

Ordinary local reproducer from the subject root:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B - <<'PY'
from dataclasses import replace
from json_oracle import canonicalize, parse

document = parse(b'"a"')
node = replace(document._nodes[0], utf16_units=(98,), scalars=(98,), scalar_valid=True)
print(canonicalize(replace(document, _nodes=(node,))))

document = parse(b'true')
node = replace(document._nodes[0], kind='false')
print(canonicalize(replace(document, _nodes=(node,))))
PY
```

Observed, with no `ProjectionIntegrityError`:

```text
b'"b"'
b'false'
```

The only direct corruption test changes `_root`
(`tests/json_oracle/test_projection.py:61-68`), so neither semantic relation is
covered by the green suite.

### C3 — BLOCKER: the scalar projection leaks implementation exceptions for corrupt documents

`require_unicode_scalars` calls `_integrity` directly at
`src/json_oracle/projection.py:198-202`. Unlike `canonicalize`, whose wrapper at
`projection.py:271-313` maps a list of implementation exceptions to
`ProjectionIntegrityError`, it has no failure wrapper. `_integrity` destructures a
span at `projection.py:70` before verifying its tuple arity. A corrupt span therefore
escapes as raw `ValueError`, violating the single typed projection-integrity failure
claimed at `README.md:61-66`.

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B - <<'PY'
from dataclasses import replace
from json_oracle import parse, require_unicode_scalars

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
ValueError not enough values to unpack (expected 2, got 1)
```

This is a distinct totality defect from C2: some corruption is silently accepted;
other corruption escapes the declared failure algebra.

### C4 — BLOCKER: the CLI allocates the complete file before enforcing `max_input_bytes`

The CLI calls `Path.read_bytes()` at `src/json_oracle/cli.py:45`; only after that
allocation does `parse` compare the resident byte string with the input ceiling at
`src/json_oracle/parser.py:414-424`. The default one-MiB input limit therefore does
not bound CLI input acquisition. This is an unbounded resource path despite the
active resource envelope in `README.md:68-88` and the wave’s “no ... unbounded
resource path” entry requirement at `BB-VALUE-TAPE-P0.md:350-354`.

An ordinary fresh-process sparse-file probe gave the same immediate typed
one-MiB rejection but memory proportional to the whole file:

```text
probe-bytes=8388608  returncode=1  maximum resident set size=34455552
probe-bytes=67108864 returncode=1  maximum resident set size=93519872
```

The reproducer is simply:

```sh
probe=$(mktemp /private/tmp/json-w0-read.XXXXXX)
truncate -s 67108864 "$probe"
/usr/bin/time -l env PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
  python3 -B -m json_oracle.cli "$probe"
```

The output reports `resource:input_byte_budget@1048576`, but the peak shows that all
64 MiB was first materialized. Increasing the sparse file increases peak RSS without
reference to the configured ceiling. The CLI boundary tests at
`tests/json_oracle/test_cli.py:57-73` use only tiny temporary inputs and cannot see
this path.

### C5 — MAJOR: syntactically numeric CLI budgets can escape the typed configuration plane

`_budget` accepts every non-empty decimal digit string and then calls `int(value)`
at `src/json_oracle/cli.py:14-19`. CPython’s active integer-string conversion guard
rejects sufficiently long digit strings with `ValueError`. The handler at
`cli.py:56-59` does not catch that exception, so a configuration-only invocation
prints a traceback, produces no JSON observation, and violates the declared typed
`configuration` failure surface (`README.md:54-66`). The tests at
`tests/json_oracle/test_cli.py:75-90` cover `-1` and `NaN`, but not a digit-only
conversion failure.

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B - <<'PY'
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
ValueError: Exceeds the limit (4300 digits) for integer string conversion: value has 5000 digits; use sys.set_int_max_str_digits() to increase the limit
```

### C6 — MAJOR: the “exact binary” identities do not seal the runtime code that executes the oracle

`verify.py:32-40` hashes only `Path(sys.executable).resolve()` plus logical version,
implementation, cache tag, and architecture. On this declared Homebrew runtime,
that executable is a dynamically linked launcher. The Python parser, decoder,
binary64 conversion, and `repr` implementation execute from an unrecorded framework
library:

```sh
otool -L /opt/homebrew/bin/python3
shasum -a 256 \
  /opt/homebrew/Cellar/python@3.14/3.14.5/Frameworks/Python.framework/Versions/3.14/Python
```

Observed dependency and currently unrecorded digest:

```text
/opt/homebrew/Cellar/python@3.14/3.14.5/Frameworks/Python.framework/Versions/3.14/Python
b0cd4b1cded9b174e8e216635e10ae95168c9e4d8ac5c10c92425fd3c05ecf08
```

The Node provenance has the same structural gap. The corpus records only the
`node` executable digest at `data/ecmascript-number-corpus.json:526-532`, while
`otool -L /opt/homebrew/Cellar/node/26.0.0/bin/node` shows V8 loaded through
`@rpath/libnode.147.dylib`; its current digest
`0afe378c904a35194671ca1b19c7e1df8947fd1da01a7ce2efa1576d9ce21b10`
is not recorded. Current replay happens to match all 104 rows, but the receipt does
not byte-bind the runtime closure, and neither macOS build nor these semantic
libraries is checked by the verifier. Runtime/toolchain provenance is therefore not
deterministically sealed by the advertised launcher hashes.

### C7 — MAJOR: reports added after the three-row review census are mutable and removable without verifier failure

`generate_manifests` excludes every root `GESTALT-*.md` from the subject manifest
at `src/json_oracle/generate.py:203-220`. `verify_live_manifests` verifies the three
declared review rows but requires only `declared_reviews <= actual_reviews` at
`verify.py:115-123`. The checked-in policy test explicitly adds
`GESTALT-FUTURE.md` and proves that the file-manifest bytes do not change
(`tests/json_oracle/test_generate.py:46-58`).

The ordinary full verifier is already a reproducer: `data/review-census.json:7-22`
declares three hashes, the pre-report run summarized
`"review_reports_observed":4`, and the verifier exited 0. The fourth report’s bytes
were neither checked nor returned. Before this report was written, that undeclared
report could be changed or removed while the declared three-row subset still passed;
after this report, both current-pass critic reports remain in the same unbound state.
This policy correctly keeps review prose from perturbing implementation bytes, but
it does not provide current-pass review sealing or an immutable input set for the
fresh adjudicator. A count is not a digest identity.

## Total-wave and claim-scope disposition

The narrow base parser exercised by the sealed corpus is substantially stronger
than a generic host JSON decoder: it retains exact number lexemes, decoded UTF-16
facts, duplicate associations, byte spans, container order, deterministic counters,
and strict UTF-8/BOM policy. The cached RFC 8259, ECMA-404, and RFC 8785 bytes are
correctly hash-bound, the manifest expressly does not pretend to close RFC 8785’s
delegated normative chain, and no ordinary parse/JCS discrepancy was found across
the sealed rows or the additional 100,000-number differential.

The module split remains small and source/test-isomorphic. However, the projection
integrity tests cover only a corrupt root and miss the semantic and failure-totality
counterexamples above. Deterministic two-root replay proves current self-consistency,
not correctness under untested mutations, current governance, runtime closure, or
current review identity.

The `INCOMPLETE_ASSAY / oracle input only` label is the only defensible product
scope. `README.md:144-151` and `AUTHOR.md:124-131` explicitly leave query/navigation,
recovery, streaming, incremental behavior, BBNF-derived types, cross-runtime and
ARM surfaces, package/consumer edges, CSS closure, profiles, candidates, locks, and
production authority RED. In particular, the file named as a query fixture carries
only ordinary ordered observation facts; the subject correctly says it provides no
query API. No complete VT.W0, candidate, PT/BB lock, clean-pass, production,
performance, or greater-product credit is awarded here.

## Verdict

**NOT READY for independent adjudication.**

C1 leaves the reviewed receipt bound to superseded governing bytes; C2-C5 are
ordinary local semantic/resource/configuration counterexamples; C6 leaves the
claimed runtime identities open; and C7 leaves the current review plane unsealed.
The green author verifier and the positive corpus checks are preserved as evidence,
but they do not earn acceptance. No repair or product credit is provided by this
review.
