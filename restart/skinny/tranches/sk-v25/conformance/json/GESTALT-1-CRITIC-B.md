# GESTALT 1 - hostile critic B: JSON W0 byte oracle

## Session declaration and review boundary

- Model declaration: `gpt-5.6-sol`.
- Canonical session/task: `/root/json_w0_critic_b`.
- Role: fresh hostile critic B; independent implementation review.
- Review date and zone: 2026-07-19, `America/New_York`.
- Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
- Subject: the ordinary local JSON parser/oracle at
  `restart/skinny/tranches/sk-v25/conformance/json`, excluding all
  `GESTALT-*.md` review files from the sealed whole-file subject.
- Governing material read: `LOCK-CONSTITUTION.md`, `TRANCHE-PORTFOLIO.md`,
  `waves/BB-VALUE-TAPE-P0.md`, the historical superseded
  `waves/BB-JSON-P0.md`, and this assay's `AUTHOR.md`, `README.md`, source,
  tests, fixtures, policies, manifests, and cached standards. ECMA-404 was
  extracted and visually checked from the pinned PDF; RFC 8259 and RFC 8785
  were checked from their pinned text publications.
- Blindness: no other critic's reasoning or verdict is used as evidence here.

Priority meanings are P0 blocking authority/semantic failure, P1 high-impact
correctness or gate failure, P2 material but bounded defect, and P3 cleanup or
friction. Every credit statement is governed by the constitution's
complete-candidate law, not by the author ledger.

## Executive verdict

**RED.** The implementation is a useful, small, mostly correct JSON syntax/JCS
oracle, but it is neither a complete VT candidate nor a clean W0 closure. The
live parser passes its frozen acceptance corpus and gets important JSON facts
right. The hostile review nevertheless found a stale governing-wave receipt,
an observation corpus that does not freeze much of the value/span/order surface,
invalid and largely non-operative default budget policy, mutable/unvalidated
projection inputs, and verifier/regeneration gaps. The checked-in whole-file
verifier is also incompatible with append-only review reports.

This assay earns **zero candidate credit and zero lock credit**. At most it is
reusable read-only VT.W0 oracle evidence after repair. This review is not VT.W4:
VT.W4 occurs only after all four independent complete verticals exist. The clean
count is therefore **0**, and any purported consecutive-clean count resets to
zero.

## Gate replay and immutable facts

I copied the subject to `/tmp/json-critic-b.wpg0s6/json` while excluding
`GESTALT-*.md`, `__pycache__`, and `*.pyc`, then ran:

```sh
cd /tmp/json-critic-b.wpg0s6/json
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite
```

That sealed replay passed. It ran all 18 external tests twice, regenerated the
318-row JSONTestSuite manifest/results twice, compared both fresh generations
with checked-in data and both evidence copies, checked cached specification,
source, and file hashes, and reported Python 3.14.5 with `network=unused`.
Observed timings were 0.355930167 s and 0.294246625 s for the test passes and
0.110334958 s and 0.106254917 s for regenerations.

The same command in the checked-in directory ran both test passes and then
failed with:

```text
file manifest path set is stale
```

The undeclared path at that point was an appended `GESTALT-*.md` review. Adding
this report necessarily adds another. Thus pre-review integrity is reproducible
only on the explicitly review-free subject, while the checked-in verifier is not
append-only-review compatible.

Digest checks found:

```text
LOCK-CONSTITUTION.md actual 3622262dcbf638dc911564bef8ee1babd65cb81a9175c72141da3166634ca85a
AUTHOR.md records          3622262dcbf638dc911564bef8ee1babd65cb81a9175c72141da3166634ca85a

BB-VALUE-TAPE-P0.md actual 710e50f4901b63642b0645051b0ef055d942b8ce377529ae7f793f1e9f35c6c1
AUTHOR.md records          3b5b2308e7fe32dacfb22f22a27127f335d9598370cc466b5e0fd520ee66a1c6
```

The source and README hashes match their manifests. The pinned suite checkout
was clean at HEAD `1ef36fa01286573e846ac449e8683f8833c5b26a`, with the expected
`test_parsing` tree `b936f9acdd24b9f5fefe68b90b9beab2c681137a`. No subject symlink
was present.

## Level 1 - total tranche fit, optimality, fidelity, and friction

### F1 - P0 - the author receipt is not bound to the governing wave reviewed now

**Evidence/reproducer.** `AUTHOR.md:13` records wave digest
`3b5b2308...66a1c6`, while `shasum -a 256
../../waves/BB-VALUE-TAPE-P0.md` from the assay resolves to
`710e50f4...35c6c1`. The constitution digest recorded beside it does match.

**Impact.** The immutable author ledger proves work against some other wave
bytes, not the current binding wave. Reviewers cannot infer clause coverage or
intent from the receipt. This is especially material because the current wave
classifies narrow JSON artifacts as assay organs, requires JSON and exhaustive
CSS as co-equal controls, and sequences hostile candidate gestalts at VT.W4.

**Smallest principled repair.** Refreeze the author ledger against the exact
current wave bytes, list every changed obligation and its disposition, and make
the governing digest a verifier-checked input. Do not merely replace the hash
without the clause delta.

**Credit effect.** Blocks even W0 evidence closure. Candidate/lock credit remains
zero; clean count is zero.

### F2 - P1 - this is a useful W0 organ, but this gestalt is not at the wave's gestalt stage

**Evidence/reproducer.** `BB-VALUE-TAPE-P0.md` says VT.W0 must freeze the JSON
oracle *and* exhaustive CSS union, map incumbent/missing rows, recapture profiles,
seal the common contract/resource policy, and create four private compiling RED
placeholders. VT.W4 then reviews each of four complete candidates across every
lock, backend, API, corpus, profile, and product edge. This directory supplies
only the handwritten JSON observation oracle, and `AUTHOR.md:118-132` correctly
lists CSS, BBNF type derivation, candidates, packages, consumers, profiles, ARM,
and hostile closure as RED.

**Impact.** Calling this pass candidate gestalt credit would invert the wave: a
narrow founding-grammar oracle could influence architecture before CSS, types,
costs, backends, and four independent implementations can falsify it. It also
cannot answer whether VT-1..VT-4 or the full portfolio is optimal.

**Smallest principled repair.** Keep it quarantined as `INCOMPLETE_ASSAY / oracle
input only`; repair its local defects; finish the rest of VT.W0; and rerun the
constitution's actual hostile gestalts only on complete candidates at VT.W4 and
VT.W5. Do not import this parser or its Python dictionaries into a candidate.

**Credit effect.** Zero candidate and zero PT/BB lock credit. A corrected artifact
may support, but cannot independently close, the JSON portion of VT.W0.

### Level-1 passing facts

- The author does not claim product or candidate completion and explicitly lists
  the greater-product remainder. That honesty prevents accidental partial credit.
- The parser is isolated from production, has no package dependency, and exposes
  no BBNF, parse-that, CSS, Value, WASM, TypeScript, or Rust edge.
- A handwritten syntax oracle is permissible as read-only comparison evidence;
  the wave forbids it only as candidate semantic/type authority.
- The four-module split (`parser`, `projection`, `generate`, `cli`) is cohesive and
  smaller than a framework. External tests mirror source modules.

## Level 2 - wave architecture and evidence plane

### F3 - P1 - the frozen 318-row corpus does not freeze the claimed observation surface

**Evidence/reproducer.** `generate.py:41-72` reduces every accepted suite input to
only status, root kind, node count, maximum depth, duplicate count, non-scalar
count, number lexeme list, and the original source hash. It omits the node tape,
decoded strings/UTF-16 units/scalars, every span, child/member relationships,
ordered duplicate associations, literal values, and full accepted value shape.
JCS output covers these indirectly only when JCS accepts; duplicate, non-scalar,
or overflowing inputs stop at a projection error.

A benign local counterexample on `fixtures/query/nested.json` is to parse it,
reverse the two children of its first two-element array, and alter a value span.
The full `observe(document)` changes, but the `_base` summary is identical; JCS
still rejects at the unchanged root duplicate before serializing the changed
nested array. The checked-in expected row therefore stays identical despite an
order and span regression. Equivalent reproducer:

```python
from json_oracle import observe, parse
from json_oracle.generate import _jcs

source = open("fixtures/query/nested.json", "rb").read()
d = parse(source)
before = observe(d)
array = next(n for n in d.nodes if n["kind"] == "array" and len(n["children"]) == 2)
array["children"].reverse()
next(n for n in d.nodes if n["kind"] == "number")["span"] = [0, 0]
after = observe(d)
assert before != after
assert _jcs(parse(source)) == _jcs(d)
# generate._base's recorded fields are unchanged as well.
```

**Impact.** A candidate comparing only frozen rows can agree while returning the
wrong accepted tree, duplicate association, decoded string, or UTF-8 span. This
fails the wave's positive/value/span/order oracle purpose and overstates “all 318
rows match frozen observations.” The 318 rows strongly prove accept/reject and
JCS where applicable, but not the advertised general observation tape.

**Smallest principled repair.** Freeze one canonical, representation-neutral
observation per accepted input: ordered typed events or normalized nodes with
kind, decoded value facts, relationships, and byte spans. Keep source hashes and
JCS outcomes. Add mutations that alter one span, child order, duplicate value
association, decoded unit, and literal kind while preserving current summary
counts.

**Credit effect.** Blocks JSON oracle closure and resets clean count. No candidate
or lock credit.

### F4 - P1 - resource defaults do not form an effective independent policy and charges occur too late

**Evidence/reproducer.** Defaults are 1,048,576 input bytes, 1,000,000 nodes,
1,000,000 aggregate entries, and 4,000,000 decoded UTF-16 units. Under the input
ceiling, the latter three cannot be reached: each node/entry consumes source
bytes, and decoded UTF-16 units are fewer than input bytes. A maximum-sized dense
array demonstrated the practical shape:

```text
input bytes: 1,048,575
nodes: 524,288
aggregate entries: 524,287
maximum resident set size: 240,992,256 bytes
peak memory footprint: 230,703,704 bytes
```

The reproducer was a root array with 524,287 single-digit elements, parsed under
defaults. Thus only input and depth meaningfully stop default inputs, while a
one-megabyte document can consume about 241 MB RSS on the review machine.

Accounting is also post-work: `string()` accumulates all decoded units before
checking `max_decoded_utf16_units` and before `add()` checks the node ceiling;
array/object entries are charged only after their entire child parses. With
`Budgets(max_nodes=1)`, a large string child under a one-node root is scanned and
allocated before `node_budget`; with `max_aggregate_entries=0`, a large first
child is parsed before `aggregate_entry_budget`.

**Impact.** The counters cap published logical objects but do not bound transient
work/memory at the point their names imply. Three advertised defaults are
ornamental, and adversarial-but-size-valid input has a large amplification. This
is not an unbounded-input bug, but it is not a credible W0 resource contract for
candidate comparison.

**Smallest principled repair.** Derive active, mutually meaningful ceilings from
the input ceiling; reserve a node/entry before scanning its payload; charge decoded
units incrementally; and expose an operation/allocation forecast or explicit RSS
policy. Add exact `limit-1/limit/limit+1` tests for every counter and an amplification
fixture. Avoid platform-specific RSS as a semantic result; use it as a measured
ceiling receipt.

**Credit effect.** Blocks the resource part of W0 and any later candidate parity
use. Zero candidate/lock credit; clean count resets.

### F5 - P1 - budget configuration is not validated and resource coverage is incomplete

**Evidence/reproducer.** `Budgets` accepts arbitrary integers and `cli.py` passes
them through without validation. This produces invalid observations rather than a
configuration failure:

```python
parse(b"0", Budgets(max_input_bytes=-1))
# OracleError("resource", "input_byte_budget", offset=-1)

parse(b"0", Budgets(max_aggregate_entries=-1))
# accepted, because this input happens not to consume an aggregate entry
```

The first error publishes an impossible negative byte offset; the second makes an
invalid policy input-dependent. Tests exercise only input and node overrides.
There is no boundary/override test for aggregate entries or decoded UTF-16 units,
and no invalid-budget CLI test, despite the feature matrix's “five immutable
defaults plus override fixtures” wording.

**Impact.** Callers cannot distinguish malformed policy from document exhaustion,
and two implemented counters lack executable evidence.

**Smallest principled repair.** Validate all budget fields once before decoding;
return a separate typed configuration error for negative/non-integral limits; add
zero and exact boundary tests for all five fields in both library and CLI paths.

**Credit effect.** Blocks resource/API validation closure; zero candidate/lock
credit and clean count zero.

### F6 - P2 - the exported projection surface is mutable and corruption does not fail typed

**Evidence/reproducer.** `Document` is a mutable dataclass containing public
mutable dictionaries/lists. `observe()` returns the same `nodes` object, not a
snapshot. `canonicalize()` assumes tags, IDs, ranges, and relationships are valid.
For example:

```python
d = parse(b'{"a":1}')
observe(d)["nodes"][d.root]["members"][0]["value"] = 999
canonicalize(d)  # IndexError, not ProjectionError
```

Other corrupt kinds/fields produce `KeyError`, `TypeError`, or assertion failure.
The author honestly lists corrupt-tape handling as product RED, but the Python
package still exports `Document`, `observe`, and `canonicalize` as a callable API
and calls JCS failures `typed_projection`.

**Impact.** The “read-only oracle observation” boundary is enforced only by prose.
An accidental consumer mutation can alter later results or escape the advertised
typed failure plane. Treating this object as a candidate value/tape would create a
second handwritten authority.

**Smallest principled repair.** Prefer a serialized immutable observation at the
oracle boundary. Otherwise make document internals private/frozen, return deep
immutable snapshots, and validate projection input into one typed corruption
error. Continue to forbid candidate imports.

**Credit effect.** No product value/tape credit exists; this blocks describing the
library projection as robust. It does not negate the live parser's syntax results.

### F7 - P1 - the verifier is incompatible with append-only reviews

**Evidence/reproducer.** `verify.py:102-111` treats every regular file other than
the self-referential file manifest and bytecode caches as sealed. A disposable
copy excluding `GESTALT-*.md` passes; the checked-in root fails `file manifest path
set is stale` after a review is appended. This report adds the same condition.

**Impact.** The command documented as the ordinary offline verifier becomes
permanently red as soon as the constitution-required hostile process writes its
report. Regenerating the subject manifest to include each review would move the
sealed implementation boundary during review and invalidate pre-review identity.

**Smallest principled repair.** Define an explicit sealed-subject manifest whose
path census excludes only a documented review namespace, then separately census
review files append-only. Preserve review-free whole-file replay in a disposable
copy. Do not weaken source/test/data coverage with a broad Markdown exclusion.

**Credit effect.** Blocks artifact/gate closure but does not invalidate the
review-free pass. Clean count remains zero.

### F8 - P1 - only suite products are regenerated; fixture and manifest derivations are hash-checked, not replayed

**Evidence/reproducer.** `generate.py` has three modes: suite generation, fixture
generation, and source/file manifest generation. `verify.py:58-79` invokes only
suite generation. It verifies current fixture, policy, source-manifest, and
file-manifest bytes through hashes but never derives fresh fixtures/manifests and
compares them. Therefore a stale relationship between fixture bytes and
`fixtures/manifest.json` is not tested as a relationship; both are merely entries
under a mutable local file manifest.

**Impact.** The “one deterministic generator” and whole-file claims are stronger
than the executed regeneration gate. Changes to fixture-generation semantics or
manifest path rules can remain latent.

**Smallest principled repair.** In temporary review-free roots, regenerate fixtures
twice and compare with checked-in fixtures/manifest; regenerate source/file
manifests from the sealed path policy and compare. Keep the file manifest itself
externally anchored or clearly describe it as a census, not tamper proof.

**Credit effect.** Blocks reproducibility closure. Zero candidate/lock credit.

### F9 - P2 - interpreter and executable-byte inputs are not fully pinned

**Evidence/reproducer.** The author records Python 3.14.5, but `verify.py:112-119`
only prints `sys.version`; it does not require a version or compatibility range.
JCS number spelling directly depends on Python `float` parsing and `repr`. The
file census also explicitly ignores `__pycache__` and all `.pyc` files even though
Python may read valid bytecode; such files were present in the working assay.
`PYTHONDONTWRITEBYTECODE=1` prevents writes, not reads.

**Impact.** A green run does not prove execution under the author toolchain or only
the declared source bytes. This is an evidence gap, not a demonstrated JCS output
mismatch.

**Smallest principled repair.** Pin/validate the interpreter used for authoritative
regeneration (or prove a supported-version matrix), reject bytecode/cache files in
the sealed subject, and run package subprocesses from a cache-empty temporary
prefix. Record the exact command/toolchain in generated provenance.

**Credit effect.** Blocks strong reproducibility credit; semantic credit remains
subject to the passing standards tests below.

### F10 - P2 - the JCS number proof is promising but its independent differential is not a gate

**Evidence/reproducer.** Checked-in tests cover RFC 8785's primitive example,
Appendix B finite vectors, and important exponent thresholds. The author reports a
20,000-value Node/V8 differential but explicitly calls it uncommitted. I repeated
a deterministic local sample over 299,827 finite random binary64 values; Python
`_number(repr(value))` agreed with Node `JSON.stringify` for all sampled values.
Neither sample is exhaustive or reproduced by `verify.py`.

**Impact.** There is no concrete number-spelling discrepancy, but the custom
`repr` normalization has less checked-in proof than the README's
ECMAScript-compatible claim suggests and is interpreter-sensitive.

**Smallest principled repair.** Check in a deterministic edge corpus generated
from a pinned independent ECMAScript/Ryu oracle, including adjacent binary64
values at every formatting threshold, and gate it without requiring network.
Retain Appendix B as the normative core.

**Credit effect.** JCS is provisionally green as assay behavior, not complete
product/type credit. This evidence gap contributes to RED closure.

### F11 - P3 - duplicate regeneration snapshots add bytes but no independent invariant

**Evidence/reproducer.** `data/expected-results.jsonl` and
`data/json-test-suite-manifest.json` are copied byte-for-byte into both
`evidence/regeneration-a` and `evidence/regeneration-b`. The verifier already
performs two fresh temporary regenerations and compares them.

**Impact.** Roughly three copies of the same generated corpus increase manifest
and whole-file friction; simultaneous stale copies prove no more than one checked
baseline plus two fresh runs.

**Smallest principled repair.** Retain one checked-in expected baseline and the two
fresh runtime generations. If historical run evidence is required, store a small
receipt with input/output hashes, command, toolchain, and status rather than full
duplicate payloads.

**Credit effect.** No credit change by itself; KISS debt only.

### Level-2 passing facts

- The review-free verifier is fast, offline, deterministic in this environment,
  and fails on a stale suite output or source/file hash.
- The suite pin checks HEAD, the `test_parsing` tree, clean status, exact file count,
  license hash, and all 35 implementation-policy filenames.
- Source and tests are externally source-isomorphic. There are no inline tests,
  third-party Python dependencies, symlinks, or product adapters.
- The implementation is compact enough to audit directly. Iterative parsing and
  iterative JCS writing avoid host recursion at the declared depth boundary.

## Level 3 - every implemented feature

| Feature | Hostile result | Exact evidence and disposition |
| --- | --- | --- |
| Strict UTF-8 byte input | GREEN locally | Full input is decoded strictly before parsing; invalid, overlong, truncated, surrogate, and non-UTF-8 suite cases reject with byte offsets. Raw UTF-8 surrogates reject while escaped surrogate code units remain a separate base policy. |
| RFC 8259 / ECMA-404 grammar | GREEN on frozen syntax corpus | All 95 `y` cases accept and 188 `n` cases reject; all 35 `i` choices have explicit policies. Complete input and the four RFC whitespace bytes are enforced. Top-level scalars work. No contrary standards case was found. |
| BOM policy | GREEN deliberate policy | Leading UTF-8 BOM rejects as `encoding:bom_forbidden`. RFC 8259 permits a parser to ignore BOM but does not require tolerance; ECMA syntax has no BOM token. The deviation is prominently declared. |
| Iterative arrays/objects and depth | GREEN semantics; resource caveat | Depth 500 accepts and 501 rejects typed without Python recursion. Empty/trailing-comma/delimiter states are correct over the suite. Resource concerns route to F4-F5. |
| Duplicate names and source order | GREEN live parser; RED frozen evidence | Objects use ordered key/value ID pairs and compare decoded UTF-16 units for JCS duplicates. Local tests prove one escaped-equivalent duplicate. Full order/association is missing from expected suite rows (F3), and the query fixture has no direct assertion. |
| Number syntax and exact lexemes | GREEN base assay | Grammar preserves exact source strings, including huge exponents, without imposing binary64 on base JSON. The suite row records every number lexeme. It intentionally has no typed numeric product value. |
| String raw/UTF-16/scalar facts | GREEN selected cases; RED corpus depth | Escapes, surrogate pairs, lone surrogates, raw non-BMP input, and UTF-16 units are handled coherently. The full per-string facts are not frozen for 318 inputs (F3). |
| UTF-8 byte spans | GREEN selected cases; RED broad claim | Multibyte value/container slices are correctly tested. Accepted suite rows omit all spans, so a broad exact-span oracle claim is under-tested (F3). |
| Typed failure planes | GREEN for ordinary input; RED configuration/corruption | Encoding, syntax, depth, resource, scalar, and JCS failures are distinct. Invalid budgets can emit offset `-1` (F5), and corrupt mutable documents escape via ordinary Python exceptions (F6). |
| Logical counters | GREEN success counts; RED policy | Input, nodes, entries, decoded units, and maximum depth are reported on success. Defaults/charging/boundaries are not an adequate resource policy (F4-F5); failures expose no consumed-counter receipt. |
| Source-preserving serialization | GREEN only under its narrow definition | It returns the original owned bytes unchanged. This is a useful identity/retention check, not a serializer, repair path, detached subtree writer, or candidate value round-trip. The README says so honestly. |
| Optional scalar gate | GREEN | Base accepts escaped lone surrogates and `require_unicode_scalars` rejects them separately without replacement. This matches the stated RFC interoperability policy. |
| Optional RFC 8785 JCS | GREEN on checked vectors, evidence caveat | Duplicate decoded names, non-scalar strings, and non-finite binary64 adaptations reject; UTF-16 key ordering, recursive compact output, lowercase escapes, and Appendix B spellings pass. F10 remains a reproducibility/proof limitation, not a reproduced semantic mismatch. |
| CLI | GREEN ordinary paths; RED validation | Success/failure JSON and exit 0/1 work, including separate scalar/JCS categories. File I/O and budget configuration are not typed/validated; F5 is the concrete defect. |
| JSONTestSuite generation | GREEN acceptance/JCS determinism; RED observation completeness | Two fresh 318-row generations are byte-identical and pinned. F3 limits what those rows prove. |
| Fixture generation | RED gate coverage | Generator exists and checked-in bytes hash correctly, but ordinary verification does not replay this mode (F8). |
| Source/file manifests | GREEN current review-free census; RED lifecycle | Current implementation bytes match. The wave digest is stale (F1), review append breaks the gate (F7), bytecode is ignored (F9), and manifest derivation is not replayed (F8). |
| Query fixture | RED as a feature | It proves only that bytes exist and produce a summary. There is no query API or direct ordered-navigation assertion. The author correctly marks product query RED. |
| Typed value/tape/navigation/visitor/detach/crossing/corruption API | NOT IMPLEMENTED | No schema-qualified value, typed path, visitor, detached subtree, UTF-16 map, corrupt-tape check, Rust/TS/WASM wire API, or revision ownership exists. The flat dictionaries are oracle observations only and earn no product credit. |

## Standards and policy conclusion

The ordinary parser's declared base choices are defensible. RFC 8259 and
ECMA-404 allow a top-level value, permit implementation limits, do not require
unique object names, and make escaped unpaired-surrogate behavior unpredictable;
the assay preserves rather than repairs that spelling. Strict network UTF-8 and
complete-input parsing are correct. Rejecting rather than ignoring a leading BOM
is an explicit permitted parser policy. RFC 8785 is correctly kept as an optional
projection so that its I-JSON restrictions do not narrow base syntax.

The principal standards problem is therefore evidence granularity, not a found
grammar acceptance bug: accepted values, relationships, decoded facts, and spans
are insufficiently frozen outside the small authored fixtures.

## KISS and modularity ruling

The parser/projection/generator/CLI split is appropriately small. The iterative
frame machine is justified by the depth-500 oracle; preserving number lexemes and
ordered member pairs is simpler and more faithful than Decimal/float/dict
projection. No framework or dependency should be added.

The removable mechanisms are the duplicate full regeneration payloads and the
prose-only read-only boundary. The necessary additions are narrow: full normalized
observations, eager budget validation/reservation, complete boundary tests, and a
review-aware sealed manifest. These should fit the existing modules; a new registry,
schema sidecar, package system, or generalized product API would be the wrong
repair.

## Terminal disposition

| Plane | Verdict |
| --- | --- |
| Total tranche fit / was this wave optimal? | **RED.** Useful VT.W0 oracle organ, but current wave receipt is stale and this cannot substitute for full W0 or VT.W4. |
| Wave architecture | **RED.** Quarantine is correct, but evidence/resource/verifier boundaries are not closed. |
| Parser syntax on pinned corpus | **GREEN with bounded policies.** No RFC/ECMA acceptance counterexample reproduced. |
| Value/span/order observation truth | **RED.** F3 leaves accepted semantics under-frozen. |
| Resource/API validation | **RED.** F4-F6. |
| Optional JCS behavior | **GREEN on current normative vectors; not closure-complete.** F10. |
| Manifests/regeneration/testing | **RED.** F1 and F7-F9. |
| KISS/modularity | **Mostly GREEN**, with F11 removable duplication. |
| Candidate/lock credit | **0 / 0.** `INCOMPLETE_ASSAY` only. |

**Final: RED. Consecutive-clean implication: 0 clean hostile passes.** Correct
findings reopen this assay's affected JSON W0 evidence cluster. After repair it
needs fresh independent review, and no local clean pass can count as either of the
two complete-candidate gestalt passes required later by VT.W4/VT.W5 and the
constitution.
