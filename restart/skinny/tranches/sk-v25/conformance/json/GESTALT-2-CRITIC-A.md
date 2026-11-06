# JSON W0 Gestalt 2 critic A — routine static and functional review

**Decision: NOT READY for independent acceptance review.**

The narrow RFC 8259 syntax core replays the frozen corpus, but acceptance has not
been earned. A legal input at one quarter of the byte ceiling exceeds the stated
accepted-input RSS policy by more than 3x, the typed projection integrity check can
canonicalize facts that disagree with the source, the supposedly authoritative
Python identity binds only a 52 KiB launcher rather than the runtime it loads, and
a digit-only CLI budget can escape the typed failure API as a traceback. The
optional JCS number route also remains sample-gated rather than reproducibly closed
over its delegated ECMAScript algorithm.

## Review identity and constraints

- Reviewer model / reasoning: `gpt-5.6-sol` / `xhigh`.
- Review posture: acceptance was presumed unearned; author receipts and passing
  baselines were treated as claims to check, not verdicts.
- The review covered the current Python source, all four substantive external test
  modules, `README.md`, `AUTHOR.md`, the sibling
  `../JSON-W0-AUTHOR-RESEAL.md`, every data/fixture manifest, the frozen result and
  number corpora, cached RFC 8259/RFC 8785 requirements, the lock constitution, the
  current `BB-VALUE-TAPE-P0` wave, and the terminated historical `BB-JSON-P0` wave.
- No file matching `GESTALT-*.md` was opened. This report is the only file written.

## Governing scope

The tranche classification is honest and must remain narrow. The constitution says
a JSON-only artifact is an `INCOMPLETE_ASSAY` even if its tests pass
(`../../LOCK-CONSTITUTION.md:58-70`). The current wave likewise calls narrow JSON
carriers assay organs (`../../waves/BB-VALUE-TAPE-P0.md:18-21`), permits an
independent oracle in VT.W0 (`../../waves/BB-VALUE-TAPE-P0.md:372-386`), and keeps
production movement prohibited (`../../waves/BB-VALUE-TAPE-P0.md:1-6`). The former
JSON vertical wave is explicitly terminated and says not to execute its stale
continuation (`../../waves/BB-JSON-P0.md:3-12`). `README.md:3-8` and
`README.md:144-151` accurately disclaim product, candidate, lock, and production
credit. This review therefore judges only whether this directory is ready to serve
as the independent JSON/JCS oracle it claims to be.

## Findings

### F1 — P1: accepted nested input defeats the stated 128 MiB resource envelope

The default limits permit 1,048,576 input bytes and 500 container levels
(`src/json_oracle/parser.py:32-47`). During publication, every container copies and
decodes its entire source slice into `node.raw`
(`src/json_oracle/parser.py:384-405`), and `observe()` emits that `raw` value for
every node (`src/json_oracle/parser.py:435-500`). For nested containers, this makes
document and observation size proportional to `depth * input`, even when node,
entry, and decoded-string counters are tiny.

That contradicts the documented policy that accepted adversarial shapes, including
canonical observation construction, fit a 128 MiB process-RSS envelope
(`README.md:68-86`; `AUTHOR.md:98-108`). The boundary tests exercise each ceiling on
one favorable shape but never combine legal depth with padding or a long inner
payload (`tests/json_oracle/test_parser.py:173-190`). The checked depth fixture is
only 1,000 bytes (`data/file-manifest.json:128-136`), so it cannot expose the
amplification.

Ordinary cache-disabled reproducer from the repository root:

```sh
/usr/bin/time -l env PYTHONDONTWRITEBYTECODE=1 \
  PYTHONPATH=restart/skinny/tranches/sk-v25/conformance/json/src \
  python3 -B -c 'from json_oracle import parse,observe; s=b"["*500+b" "*250000+b"0"+b"]"*500; d=parse(s); o=observe(d); print(len(s),len(o))'
```

Observed on the exact recorded Apple arm64 CPython 3.14.5 launcher:

```text
251001 125621753
409763840 maximum resident set size
```

The input is below one quarter of `max_input_bytes`, uses 500 entries and 501 nodes,
contains no decoded string units, and is accepted. Nevertheless, its 125,621,753-byte
observation peaks at 409,763,840 bytes RSS, versus the claimed 134,217,728-byte
policy. A full-size legal variant has still larger amplification. This is not a
benchmark nicety: the wave requires no unbounded resource path
(`../../waves/BB-VALUE-TAPE-P0.md:331-347`), and the oracle's own public claim
includes observation construction.

Required repair: stop materializing complete container slices per node. The owned
source bytes plus half-open span already preserve the spelling; retain `raw` only
where it is non-redundant, or derive it on an explicitly budgeted projection. Add
accepted combined-limit fixtures and a mechanically enforced RSS/output-size gate,
not three hand-selected measurements.

### F2 — P1: projection integrity accepts source/decoded-value disagreement and can leak untyped corruption errors

The README promises that projection validates the complete private document and
maps any corrupt identity, span, relation, kind, counter, or root to the one typed
`projection_integrity` failure (`README.md:61-66`). The implementation verifies that
a string's `raw` spelling equals its source slice, but it independently recomputes
scalars only from the already-supplied `utf16_units`
(`src/json_oracle/projection.py:44-91`). It never decodes the quoted source spelling
and compares that result to the supplied units. `_string()` then serializes the
supplied scalar tuple (`src/json_oracle/projection.py:234-253`).

Because `parse()` returns a frozen dataclass rather than an opaque capability,
ordinary `dataclasses.replace` can expose this gap without mutating any object:

```sh
env PYTHONDONTWRITEBYTECODE=1 \
  PYTHONPATH=restart/skinny/tranches/sk-v25/conformance/json/src \
  python3 -B - <<'PY'
from dataclasses import replace
from json_oracle import canonicalize, parse
original = parse(b'"a"')
node = replace(original._nodes[0], utf16_units=(98,), scalars=(98,))
forged = replace(original, _nodes=(node,))
print(canonicalize(original))
print(canonicalize(forged))
PY
```

Observed:

```text
b'"a"'
b'"b"'
```

The forged document retains source spelling `"a"`, the same span, counters, and
valid raw value, yet canonicalizes as `"b"` rather than raising
`ProjectionIntegrityError`. This defeats the exact corruption invariant that the
integrity pass exists to provide.

There is a second failure mode in the same guard. `_integrity()` checks that a span
is a tuple but unpacks it without checking length (`src/json_oracle/projection.py:59-72`).
`canonicalize()` wraps incidental exceptions (`src/json_oracle/projection.py:271-313`),
but `require_unicode_scalars()` calls `_integrity()` outside such a wrapper
(`src/json_oracle/projection.py:198-202`). Replacing a string span with `(0,)` makes
the latter leak `ValueError: not enough values to unpack`, not the promised typed
integrity failure. The only corruption test changes `_root`; it does not mutate
decoded facts or malformed field shapes
(`tests/json_oracle/test_projection.py:61-68`).

Required repair: either make the projection consume an actually opaque parse result
whose representation cannot be reconstructed through supported Python mechanisms,
or fully rederive every projected fact from source and relations. All projection
entry points must normalize malformed private state to `ProjectionIntegrityError`,
and external mutations must cover every checked field family, especially
source/string agreement.

### F3 — P1: the verifier's “authoritative Python” identity does not bind the Python runtime and currently cannot replay

`verify.py` calls its toolchain check an authoritative Python identity, but hashes
only `Path(sys.executable).resolve()` plus reported strings
(`verify.py:18-45`). On this exact installation, that hash names a 52,448-byte
launcher. `otool -L` shows that it dynamically loads the separate 5,438,480-byte
`Python.framework/.../Python`; neither that library, the Python standard library,
nor extension modules are sealed. Yet parser/JCS behavior depends on that runtime,
including `float()` and `repr()` (`src/json_oracle/projection.py:205-231`). The
author ledger and external receipt repeat the launcher hash as if it identified the
toolchain (`AUTHOR.md:17-26`; `../JSON-W0-AUTHOR-RESEAL.md:44-61`).

The gap is observable now. The exact documented replay command
(`README.md:123-138`) failed before `main()` could call `enforce_python()` at
`verify.py:233-239`. Top-level import of `tempfile` at `verify.py:14` imported
`random`, and macOS rejected the unbound `_random.cpython-314-darwin.so` under its
current process policy:

```text
ImportError: ... _random.cpython-314-darwin.so ... library load denied by system policy
```

The launcher itself still had the exact sealed SHA-256
`2477b47fa3ae65b9574eb18a15edb364e96948eaa1875ad3f1c80d780efc9c12`.
Thus the verifier can neither establish its identity nor replay in the current
review environment, while the asserted binary hash continues to match. This does
not disprove the author-reported historical runs, but it does disprove that the
recorded identity is a sufficient reproducibility boundary.

Required repair: use a hermetic, content-addressed runtime or bind and verify the
actual interpreter library, standard-library tree, extension modules, OS/runtime
requirements, and executable before importing unbound modules. The independent
acceptance command must then pass from a clean environment; an author receipt is not
a substitute under `../../LOCK-CONSTITUTION.md:29-50`.

### F4 — P2: a digit-only CLI budget escapes the typed configuration contract

The CLI explicitly accepts any nonempty all-decimal argument and calls `int(value)`
(`src/json_oracle/cli.py:14-19`). CPython limits decimal-to-integer conversion to
4,300 digits by default. The resulting `ValueError` is not among the caught typed
errors (`src/json_oracle/cli.py:37-61`), so a value that passes the CLI's own lexical
validator prints a traceback and no JSON result.

Ordinary reproducer (the subprocess only avoids a shell-sized literal):

```python
import subprocess, sys
r = subprocess.run(
    [sys.executable, "-B", "-m", "json_oracle.cli", "/no/such/file",
     "--max-nodes", "9" * 5000],
    env={"PYTHONDONTWRITEBYTECODE": "1", "PYTHONPATH": "src"},
    text=True, capture_output=True,
)
print(r.returncode, repr(r.stdout), r.stderr.splitlines()[-1])
```

Observed:

```text
1 '' ValueError: Exceeds the limit (4300 digits) for integer string conversion: ...
```

That contradicts the documented typed configuration/failure plane
(`README.md:54-66`). Tests cover `-1` and `NaN` but no oversized digit string
(`tests/json_oracle/test_cli.py:75-90`). Parse decimal configuration without a
potentially throwing unbounded conversion, or catch conversion failure and return
the declared canonical configuration observation. Add a no-traceback invariant for
every CLI input class.

### F5 — P2: RFC 8785 number conformance is not reproducibly closed

The string escaping, UTF-16 property sort, duplicate rejection, lone-surrogate
rejection, and nonfinite rejection match the cached RFC requirements. In
particular, RFC 8785 requires nonduplicate names and Unicode/IEEE-754-compatible
values (`data/specs/rfc8785.txt:176-198`), lowercase/control string escaping
(`data/specs/rfc8785.txt:256-280`), and UTF-16 code-unit sorting
(`data/specs/rfc8785.txt:315-363`); the corresponding implementation is direct
(`src/json_oracle/projection.py:234-309`).

Number serialization is different. RFC 8785 requires the ECMA-262 7.1.12.1
algorithm, including Note 2 (`data/specs/rfc8785.txt:282-295`), and recommends a
large reference corpus or substantial random V8 differential for stronger
validation (`data/specs/rfc8785.txt:666-670`). This implementation derives output
from CPython `float()` and `repr()` plus local formatting rules
(`src/json_oracle/projection.py:205-231`). The manifest expressly admits that
ECMA-262, IEEE 754, and Unicode are not cached and the offline normative chain is
not closed (`data/spec-manifest.json:1-13`). The 104 Node rows are checked in, but no
producer script or regeneration command exists in `generate.py`; the test only
asserts `len(cases) >= 60`, one Node version string, and equality to those rows
(`tests/json_oracle/test_projection.py:117-124`). Suite regeneration computes JCS
goldens with the implementation under test (`src/json_oracle/generate.py:41-60,
99-129`), so it proves stability rather than independent JCS correctness.

I ran an additional deterministic 20,000-finite-binary64 comparison against the
recorded Node 26 executable and found **zero mismatches**. The checked Appendix B
and 104-row gates also pass. Those are positive samples, not a proof that Python's
runtime-specific `repr()` is the required ECMAScript algorithm, and the extra run
is not part of the sealed replay.

Required repair: either narrow the public name/claim to a non-authoritative sampled
projection, or seal a reproducible implementation/reference for the delegated
algorithm and a large independently generated differential corpus, including the
generator bytes, command, producer identity, exact count, uniqueness, and replay.

## Functional and evidence results

The following checks passed and should be preserved through repair:

- The external receipt's file-manifest, author-ledger, source-manifest,
  318-row-baseline, and fixture-manifest hashes all match current bytes. The current
  constitution and current-wave hashes also match `AUTHOR.md:17-20`.
- There are 40 sealed files, 318 unique JSONTestSuite paths (95 `y`, 188 `n`, 35
  explicit `i` policies), 116 accepted base observations, 202 base rejections, 99
  JCS accepts, 17 typed JCS rejections, and no false declared suite contract. These
  counts agree with `data/json-test-suite-manifest.json:1913-1927` and the sibling
  receipt at lines 70-84.
- A cache-disabled direct replay recomputed all 318 base observations and JCS
  results from the frozen checkout and matched every checked-in row byte-for-value.
- The 21 parser/projection tests that do not import the blocked `tempfile` path
  passed. The full 29-test top-level verifier did not run for the F3 reason above;
  that failure is reported, not silently converted to green.
- No `__pycache__` directory or `.pyc` file was created.

The RFC 8259 state machine itself is small and maintainable. It correctly separates
strict UTF-8, the deliberate BOM policy, syntax, depth, and resource categories;
retains exact number lexemes; preserves decoded UTF-16 units, byte spans, object
order, and decoded duplicate identity; and parses containers iteratively
(`src/json_oracle/parser.py:98-432`). The 35 implementation-defined suite policies
are explicit and exhaustive (`data/i-case-policy.json:1-40`). Library budget fields
reject booleans, floats, strings, and negative integers as intended
(`src/json_oracle/parser.py:32-47`), and the ordinary zero/boundary tests are useful
(`tests/json_oracle/test_parser.py:108-171`). None of those strengths cures F1-F5.

## Claim audit

| Claim | Review result |
| --- | --- |
| Strict RFC 8259 byte parser over the pinned 318-case suite | Supported for the frozen corpus; no syntax discrepancy found. |
| Ordered duplicates, exact lexemes, decoded units, and UTF-8 spans | Supported on ordinary parse results and frozen observations. |
| Immutable observation bytes | `bytes` publication is immutable, but per-container raw duplication violates the resource contract. |
| Complete typed projection-integrity failure | False; F2 changes canonical value and separately leaks `ValueError`. |
| Accepted adversarial shapes fit the 128 MiB policy | False; F1 peaks at 409,763,840 bytes on a 251,001-byte legal input. |
| Every CLI configuration failure is typed | False; F4 is an uncaught digit-conversion traceback. |
| Enforced authoritative CPython identity / reproducible two-root replay | Incomplete and currently unreplayable; F3 binds a launcher, not its runtime. |
| RFC 8785 projection | Core string/sort/I-JSON checks agree with the cached RFC; number serialization remains finite-sample behavioral evidence, not closed normative/reproducible authority. |
| Product/candidate/lock/production credit | Correctly disclaimed throughout. |

## Acceptance gate

Independent acceptance review should not start on these bytes. Repair F1-F4, decide
and accurately seal the F5 authority level, add external counterexamples for every
finding, regenerate and externally anchor all affected manifests, and demonstrate a
clean full verifier replay in a genuinely bound runtime. Any repaired root is new
evidence and must restart the required clean-pass sequence.

**Final disposition: NOT READY.**
