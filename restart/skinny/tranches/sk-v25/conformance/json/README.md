# JSON W0 byte oracle

Status: **INCOMPLETE_ASSAY / oracle input only**. This directory is an
independent conformance oracle for the JSON portion of VT.W0. It is not product
JSON support, a BBNF or parse-that implementation, a handwritten semantic/type
schema, a value/tape API, or a credited VT candidate. Production and candidate
implementations may consume only the frozen observation bytes; they may not import
the Python implementation.

The oracle reads one complete byte string, requires strict UTF-8, recognizes the
RFC 8259 / ECMA-404 grammar with an iterative container stack, and returns a
canonical immutable JSON byte observation. The mutable builder is private and is
discarded before `parse` returns. The internal frozen document exists only so the
optional projections can validate and consume one parse; it is not exported.

## Authority and corpus boundary

`data/spec-manifest.json` byte-binds the locally cached RFC 8259, ECMA-404 2nd
edition, and RFC 8785 publications. RFC 8785 remains the named projection
authority: RFC 7493, ECMA-262 10th edition, IEEE Std 754-2019, and Unicode 17.0.0
are provenance-bound by immutable publication identities and official permalinks.
IEEE text remains access-controlled rather than copied.

`data/ecmascript-number-corpus.json` is a reproducible independent gate produced
by Node 26.0.0 / V8 14.6.202.33-node.19. Its 104 rows cover adjacent binary64
encodings on both signs around fixed/scientific formatting thresholds, range
endpoints, and RFC 8785 rounding examples. The checked-in producer also generates
100,000 unique deterministic finite binary64 values; every row is compared with
the compact Python formatter. Program, commands, seed, counts, output digests, and
both executing dynamic closures are sealed. `generate_numbers()` does not read the
checked corpus target: a small checked recipe owns the semantic labels, revision,
seed, count, and uniqueness rule, while the producer program, live checked Node
identity, commands, and both output digests are derived anew. The complete document
is therefore byte-identical from two roots in which the target is absent; a poisoned
target is ignored.

The compact number route has one auditable equivalence argument. JSON number
syntax is converted once to the sealed CPython binary64 using round-to-nearest,
ties-to-even. The sealed `repr(float)` supplies the shortest round-tripping decimal
coefficient for that same bit pattern. `_number` does not choose or round another
coefficient: it only maps negative zero to zero and moves the decimal point or
normalizes the exponent at RFC 8785's `1e-6` / `1e21` notation boundaries.
Nonfinite results are rejected before serialization. Appendix B, all adjacent
boundary rows, and the independently regenerated 100,000-value differential make
each premise executable and falsifiable; the provenance-bound ECMA-262 and IEEE
identities remain the normative authorities rather than the finite sample.

The external JSONTestSuite pin is commit
`1ef36fa01286573e846ac449e8683f8833c5b26a`; its `test_parsing` tree is
`b936f9acdd24b9f5fefe68b90b9beab2c681137a`. The exact 318-file path, size,
label, and SHA-256 census is in `data/json-test-suite-manifest.json`: 95 mandatory
accepts, 188 mandatory rejects, and 35 explicitly decided `i` cases.
`data/expected-results.jsonl` freezes one full result per input.

## Base and failure policy

- Only RFC 8259 whitespace may surround one value; trailing content is syntax
  failure. Bytes must be strict UTF-8.
- A leading UTF-8 BOM is deliberately rejected as `encoding:bom_forbidden@0`.
  The three BOM bytes are checked before suffix UTF-8 validation, so malformed
  suffixes cannot change that identity.
- Object members remain ordered pairs. Duplicate decoded names are accepted,
  retained, and associated with the first equal decoded key in the observation.
- Numbers remain exact source lexemes. Base syntax does not silently narrow them
  to binary64.
- Strings retain quoted spelling, decoded UTF-16 units, scalar facts, and scalar
  validity. Escaped lone surrogates remain base-valid facts; raw UTF-8 surrogate
  encodings are encoding failures.
- Spans are half-open UTF-8 byte offsets. Container depth counts open arrays and
  objects; the root container has depth one and the default inclusive limit is
  500.
- The library accepts only exact immutable `bytes`; mutable or foreign source
  objects fail as typed `configuration`. Every budget accepts only `type(value)
  is int` and `value >= 0`. Booleans,
  floats, NaN/infinity, strings, negative values, and arbitrary objects fail as
  typed `configuration`, before input is read or inspected.
- Nodes and aggregate entries are reserved before their payload; decoded UTF-16
  units are charged before append or scalar construction. Resource failures expose
  deterministic consumed counters but never publish a partial document.

Failure categories are `configuration`, `encoding`, `syntax`, `scalar_validity`,
`depth`, `resource`, `typed_projection`, and CLI-only `io`. Projection validates
the complete private document first. Any corrupt identity, span, relation, kind,
counter, or root becomes the single typed
`typed_projection:projection_integrity` failure rather than an implementation
exception. The one validator reparses the owned source with source-sized ceilings
and compares the complete fresh immutable document. Every public standalone
`observe`, scalar-check, and RFC 8785 call enters that boundary. The compound CLI
enters it once, performs optional scalar/JCS work on that checked document, and
constructs the observation directly; it never decodes observation bytes into a
second Python result tree or reparses once per selected projection.

The two public byte-producing APIs validate their exact non-negative integer
output caps before document integrity. One `_output_cap` owner also serves both
private constructors. A valid cap retains document-integrity precedence; a
boolean, negative integer, float (including NaN/infinity), string, null, or
arbitrary object is always `configuration` even when every private document field
family is corrupt.

## Active default resource envelope

The defaults are 1,048,576 input bytes, depth 500, 60,000 nodes, 40,000 aggregate
entries, and 131,072 decoded UTF-16 units. Nodes, entries, and decoded units are
all independently reachable below the input ceiling: dense objects reach nodes,
dense arrays reach entries, and one string reaches decoded units.

The policy targets a 128 MiB process-RSS envelope for accepted adversarial shapes
on the sealed Apple arm64 / CPython 3.14.5 platform. Base observation construction
has a separate inclusive 16 MiB output cap, and RFC 8785 projection construction
has an inclusive 1 MiB cap. Exhaustion is typed as `resource:output_byte_budget`
or `typed_projection:projection_byte_budget`; neither route publishes a partial
document. These caps do not replace any input, node, entry, decoded-unit, or
depth limit.

Decoded UTF-16 words are retained as immutable big-endian bytes. Scalar facts and
raw leaf spelling are derived on demand from those words and the one owned source
span; they are not duplicated as per-unit integers, lone-surrogate tuples, or raw
strings in every node. Slotted builders are replaced in place by frozen slotted
nodes so construction state is released progressively. This representation is
generic over every string and has no fixture, input-name, or grammar-name branch.

The existing library gates remain unchanged. Fresh processes enforce these
inclusive observation/RSS bounds over `observe(parse(source))`:

| Accepted boundary shape | Observation gate | Peak-RSS gate |
| --- | ---: | ---: |
| depth 500 plus 250,000 padding bytes | 400,000 B | 134,217,728 B |
| 40,000-entry dense array | 5,000,000 B | 134,217,728 B |
| 29,999-entry duplicate-key object / 59,999 nodes | 10,000,000 B | 134,217,728 B |

In particular, the 29,999-entry library object observation remains independently
gated at 10,000,000 bytes. It is not relaxed by the additional 16 MiB public-output
construction bound.

The public regressions combine the live default ceilings without reducing them.
The scalar-valid base/scalar/JCS matrix remains gated. The stronger base-valid
shape uses 29,999 unique pairs of lone high-surrogates and fills its first key to
reach exactly 1,048,576 input bytes, 131,072 decoded UTF-16 units, 59,999 nodes,
29,999 object entries, and depth one. Three governed fresh processes measured:

| Sample | Stdout incl. newline | Wall | Peak RSS |
| --- | ---: | ---: | ---: |
| 1 | 14,956,823 B | 0.693653 s | 95,846,400 B |
| 2 | 14,956,823 B | 0.680067 s | 95,223,808 B |
| 3 | 14,956,823 B | 0.673647 s | 95,600,640 B |

These are platform-bound envelope measurements, not product performance claims.
Lower caller input ceilings remain available, and every parse budget still has
zero/limit-minus-one/limit/limit-plus-one library and CLI coverage.

## Immutable full observation

`observe(parse(bytes))` returns canonical ASCII JSON bytes, never a mutable alias.
Every accepted suite row includes source size/hash/base64 and roundtrip status,
the root ID, exact counters, and every node in parse order. Nodes include kind,
exact span, ordered array children or object key/value
relations, member position and duplicate association, plus decoded string facts,
scalar validity, and exact leaf spelling/number lexemes. Container spelling is
never duplicated: the one source and half-open span own it. Mutation tests change child order,
key/value association, span, decoded units, scalar validity, and literal kind and
prove each is visible in the frozen bytes.

Source-preserving serialization is only the unchanged owned input bytes for a
base acceptance. It is not a writer or repair API. The optional RFC 8785
projection rejects decoded duplicate names, non-scalar strings, and binary64
overflow; sorts names by UTF-16 units; and uses the compact number formatter gated
by RFC 8785 Appendix B plus the independent 104-row Node corpus.

## Reproducible layout and verification

```text
src/json_oracle/       parser, projections, CLI, generator, process governance
tests/json_oracle/     external source-isomorphic tests
fixtures/              positive, negative, resource, mutation, and query bytes
data/                  cached specs, policies, corpora, baselines, and manifests
verify                 pre-import exact-startup launcher
verify.py              enforced-toolchain two-root offline verifier
```

The sealed implementation census excludes only `data/file-manifest.json` for
self-reference and root `GESTALT-*.md` files. Review inputs are hashed externally
before and after each adjudication, so mutable prose does not enter implementation
identity. Bytecode caches are rejected, never silently excluded.

Before any Python import, the executable `verify` launcher rejects ambient Python
optimization/startup and Node preload/resolver controls, constructs one exact
three-name environment, and starts the content-bound CPython executable with
`-I -S -B -X utf8`. The verifier then enforces `optimize == 0`, isolated and safe
path mode, no user/site imports, no bytecode, UTF-8 mode, the complete flag vector,
exact executable/argv prefix, and exact effective environment before importing
the assay. The runtime receipt binds command schemas and identities for the shell,
environment helper, Git, `otool`, `sw_vers`, Python, and Node; the complete
Python/standard-library and Node package trees; and recursively discovered
non-system Mach-O dependencies. Every normal Python and Node child uses this one
argv/environment owner without ambient inheritance. It copies the
review-free subject into two fresh cache-empty roots. In each root it runs all
external tests and independently regenerates the 318 suite rows, fixtures, the
target-absent complete number corpus, source manifest, and file manifest. It
separately regenerates and checks the 100,000-value differential. Poison/delete
tests prove number metadata cannot inherit from the target. Both roots must match
each other and the checked-in bytes.

From this directory:

```sh
./verify \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md

PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -B -m json_oracle.cli \
  fixtures/positive/jcs-sort.json --jcs
```

The latest non-circular root anchor is the SHA-256 of `data/file-manifest.json`
recorded in the minimal sibling receipt `../JSON-W0-GESTALT-4-RESEAL.md`. That
receipt is outside this subject census by construction; earlier receipts remain
unchanged append-only history.

## Honest product boundary

This assay provides no product query/navigation/value/tape API, recovery,
streaming, incremental edits, BBNF-derived types, Rust/TypeScript/WASM/ARM parity,
package consumer, architecture choice, production edge, profile credit, or hostile
gestalt acceptance. Its query fixture proves only ordered observation retention.
Local repair and passing verification earn no candidate, PT/BB lock, clean-pass,
complete VT.W0, or production credit.
