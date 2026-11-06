# JSON W0 hostile Gestalt-1 adjudication

Date: 2026-07-19  
Time zone: `America/New_York`  
Reviewer role: third fresh independent adjudicator; neither author nor critic  
Model declaration: `gpt-5.6-sol`  
Recorded turn model: `gpt-5.6-sol`  
Canonical session/task: `/root/json_w0_adjudicator`  
Workspace: `/Users/mkbabb/Programming/bbnf-lang`  
Subject: `restart/skinny/tranches/sk-v25/conformance/json`  
Disposable review-free replay: `/tmp/json-w0-adjudication.conUED/json`  
Opaque platform session UUID: not exposed to the adjudicator

Disposition: **RED — REOPEN AND RESEAL THE JSON W0 ORACLE**  
Consecutive clean local hostile Gestalt count: **0**  
Candidate credit: **0**  
PT/BB lock credit: **0**  
VT.W4/VT.W5 credit: **0**  
Production movement: **not authorized**

This file is supersession evidence. It does not edit either critic report,
implementation, tests, data, manifests, author ledger, wave ledger, or campaign
ledger.

## 1. Executive ruling

The subject is a useful, compact, candidate-neutral JSON syntax and observation
assay. Its base parser survived the complete pinned JSONTestSuite, an independent
Node decision pass, strict UTF-8 probes, duplicate/order probes, exact-number
checks, byte-span checks, surrogate checks, and deep iterative parsing. Its JCS
projection passed the checked normative examples and an additional deterministic
200,404-value Node differential with no mismatch.

It is not clean. The independently reproduced blockers are:

1. decoded units, nodes, and aggregate entries are charged after material work or
   allocation, and the public budget domain is not validated;
2. three advertised default counters are dominated by the input limit, while a
   valid 1,048,575-byte dense array consumed 240,582,656 bytes peak RSS;
3. the 318 frozen rows do not preserve accepted tree relationships, decoded
   facts, or spans, so order/span/value regressions can leave the corpus unchanged;
4. a leading BOM can lose the declared `bom_forbidden@0` identity to a malformed
   suffix;
5. the exported document/observation plane is mutable and corrupt projections
   escape through ordinary Python exceptions;
6. the author names a stale governing-wave digest;
7. the verifier is not append-only-review compatible, does not replay fixture or
   manifest generation, does not enforce its interpreter, and may read excluded
   bytecode caches;
8. the offline JCS authority and independently gated number proof are incomplete;
9. two full regeneration directories duplicate one producer's output without an
   independent invariant.

Every numbered critic finding is therefore **CORRECT** at its defensible narrow
scope. Two proof qualifications matter:

- Critic B's F3 code block is wrong as written: `observe()` aliases
  `document.nodes`, so its `before != after` assertion is false after mutation.
  A deep-copied before-image independently proves the underlying F3 claim.
- Critic A's F-08 stale-digest fact is correct, but its description of the unseen
  old-to-current wave delta as only candidate-count wording is **UNPROVEN** because
  the old bytes were not available to this adjudicator.

The correct terminal state remains `INCOMPLETE_ASSAY / oracle input only`. The
artifact can support the JSON portion of VT.W0 after repair and reseal, but this
pass cannot close VT.W0, choose a candidate, establish portfolio optimality, or
count as either complete-candidate pass at VT.W4 or VT.W5.

## 2. Review boundary and frozen identities

I read in full the governing `LOCK-CONSTITUTION.md`,
`TRANCHE-PORTFOLIO.md`, `waves/BB-VALUE-TAPE-P0.md`, `AUTHOR.md`,
`README.md`, all implementation and test source, `verify.py`, both critic
reports, the policies and manifests, the 318 expected rows, and the cached RFC
8259, ECMA-404, and RFC 8785 publications. The ECMA-404 PDF was extracted from
all 16 pages. The critics were treated as allegations, not evidence authority.

Current SHA-256 identities are:

| Item | SHA-256 |
| --- | --- |
| `LOCK-CONSTITUTION.md` | `3622262dcbf638dc911564bef8ee1babd65cb81a9175c72141da3166634ca85a` |
| `TRANCHE-PORTFOLIO.md` | `729202d59650a8c5a42b568bda11091af8ca631add143d039dfd84238b941e10` |
| `waves/BB-VALUE-TAPE-P0.md` | `710e50f4901b63642b0645051b0ef055d942b8ce377529ae7f793f1e9f35c6c1` |
| `AUTHOR.md` | `aa5364a4c432fe690e1009d2cd8f5ca26648f0961b62b096be69e70844ab9f75` |
| `README.md` | `ab8c78f9453e588549ad99ae1cc66520694c10e8761f40e810dc95602aea4dce` |
| `verify.py` | `1eed919d59833e43302e346c82227cdf43b27fea8d025ffbe716059c9f3fe36b` |
| Critic A | `225b0e8196c58c6ae1196ba08b19d14d181a8389452dd32da9c6998fc4bbd661` |
| Critic B | `67bb4ed7320aa802231f4ec968f60df56716278a11e0d63f311e5f3885a10808` |

`AUTHOR.md:13` instead records the governing wave as
`3b5b2308e7fe32dacfb22f22a27127f335d9598370cc466b5e0fd520ee66a1c6`.
That mismatch is exact and independent of any semantic judgment.

The pinned suite checkout was clean at
`1ef36fa01286573e846ac449e8683f8833c5b26a`, with `test_parsing` tree
`b936f9acdd24b9f5fefe68b90b9beab2c681137a`. The adjudication platform was
Apple arm64, macOS 26.4.1 build 25E253, Python 3.14.5, and Node 26.0.0.

## 3. Independent replay evidence

### 3.1 Review-free and live verification

I copied the subject while excluding only `GESTALT-*.md`, `__pycache__`, and
`*.pyc`. The copy contained the 42 manifest-declared files plus the
self-excluded `data/file-manifest.json`, for 43 physical files.

From that copy:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src \
  /usr/bin/time -p python3 verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite
```

passed. It ran 18 tests twice and two additional suite regenerations. Observed
test times were 0.542032 and 0.481314 seconds; regeneration times were 0.162843
and 0.157591 seconds; outer real time was 1.43 seconds. The summary reported
318 rows, two passes, Python 3.14.5, and no network use.

The same verifier in the live subject ran both test passes and then exited 1
with `file manifest path set is stale`. The only new ordinary files were the two
critic reports; this adjudication necessarily adds a third. Thus the sealed
pre-review replay is valid, but the live lifecycle gate is not review-aware.

### 3.2 Independent corpus decisions and row shape

A single independent Node 26 process used fatal UTF-8 decoding, `JSON.parse`,
and the declared leading-BOM rejection. It observed:

| Class | Files | Accepted | Rejected | Policy mismatches |
| --- | ---: | ---: | ---: | ---: |
| `y_` | 95 | 95 | 0 | 0 |
| `n_` | 188 | 0 | 188 | 0 |
| `i_` | 35 | 21 | 14 | 0 |

All 318 generated rows are unique and declare their suite contract met. Accepted
base rows contain only status, root kind, node count, maximum depth, duplicate
count, non-scalar count, number lexemes, and source-preserving hash. Exactly zero
accepted rows contain a span, node relationship, event stream, decoded string,
literal value, or complete value shape. There are 99 JCS accepts, 17 JCS rejects,
and 202 base-rejected/not-applicable rows.

### 3.3 Resource and configuration probes

The following observations were independently reproduced:

| Probe | Observation |
| --- | --- |
| `max_input_bytes=-1`, input `0` | `resource:input_byte_budget@-1` |
| `max_depth=-1`, input `0` | accepted |
| `max_depth=-1`, input `[]` | `depth:depth_budget@0` |
| `max_input_bytes=NaN`, input `0` | accepted |
| `max_nodes="1"`, input `0` | uncaught `TypeError` |
| `max_aggregate_entries=-1`, scalar or empty array | accepted |
| `max_aggregate_entries=-1`, input `[0]` | resource rejection at byte 1 |
| BOM plus invalid byte, `EF BB BF 80` | `encoding:invalid_utf8@3` |

A one-million-unit ASCII string under a decoded-unit limit of one was fully
accumulated and rejected only at its closing quote; `tracemalloc` observed a
16,900,590-byte peak. A 300,000-unit child was likewise scanned and allocated
before either `max_nodes=1` or `max_aggregate_entries=0` rejected it, with about
5.8 MB traced in each probe.

The densest default-limit array used 524,287 one-digit elements:

```text
input bytes             1,048,575
nodes                     524,288
aggregate entries         524,287
maximum resident set   240,582,656 bytes
peak memory footprint  230,277,720 bytes
```

It passed all defaults. This independently confirms that the one-million node
and entry defaults and four-million decoded-unit default cannot independently
fire below the 1,048,576-byte input ceiling. Depth and input are the only active
default stopping policies.

### 3.4 Observation mutability and invisible mutations

On `fixtures/query/nested.json`, reversing the two children of a nested array and
changing a number span altered a deep-copied full observation. The compact
`generate._base` fields remained identical, and JCS remained the same duplicate-
name rejection at node 9. The frozen row therefore cannot detect either defect.

Critic B's literal F3 script produced `before == after`, not `before != after`,
because `observe(document)["nodes"] is document.nodes`. That alias is itself
independent evidence for B F6.

Mutating an observed object member's value ID to 999 and calling `canonicalize`
raised `IndexError: list index out of range`, not `ProjectionError`.

### 3.5 JCS behavior and authority

The checked RFC 8785 vectors pass. An additional deterministic sample of 200,404
finite binary64 values, including adjacent values around formatting thresholds,
produced zero differences between `projection._number` and Node
`JSON.stringify`.

This is strong passing behavior, not closure evidence. RFC 8785 expressly delegates
number serialization to ECMA-262 and says the algorithm is not included. Its
normative references also include RFC 7493, IEEE 754, and Unicode authorities.
The local spec manifest caches none of those delegated publications, and the
independent Node differentials are not part of `verify.py`. No JCS output mismatch
was reproduced; the defect is the overbroad offline-authority/provenance claim and
the absence of an independent number gate.

### 3.6 Regeneration, manifests, and executable bytes

I separately regenerated fixtures twice into fresh directories. The two outputs
and checked-in fixtures were byte-identical. I separately regenerated the source
and file manifests inside another review-free disposable copy; their hashes were
unchanged (`6941fd4a...` and `372f8586...`). This proves the present bytes are
reproducible, while also confirming that the ordinary verifier does not execute
those modes.

Both evidence directories contain exact copies of the primary 133,940-byte
expected-results file and 62,730-byte suite manifest. They add 393,340 bytes and
no independent producer or invariant.

The live subject contains Python 3.14 bytecode caches that the file census
silently excludes. `PYTHONDONTWRITEBYTECODE=1` stops writes but not reads; verbose
import evidence showed Python loading an excluded local `.pyc`. The verifier only
prints `sys.version` and does not enforce the author-recorded interpreter.

## 4. Numbered finding adjudication

Verdicts apply to the numbered claim, not automatically to every sentence in its
proof or its critic-assigned priority.

### 4.1 Critic A

| Finding | Verdict | Independent ruling |
| --- | --- | --- |
| F-01 | **CORRECT** | `parser.py:108-128` builds units and scalar facts before the decoded limit check. The one-million-unit/limit-one probe allocated 16.9 MB before rejection. Overlaps B F4. |
| F-02 | **CORRECT** | Negative, NaN, and wrong-type budgets reproduced acceptance, impossible offsets, input-dependent behavior, and uncaught `TypeError`. Overlaps B F5. |
| F-03 | **CORRECT, narrowly** | The cached set does not close RFC 8785's delegated normative algorithm/authority chain. This is an evidence/provenance failure, not a reproduced JCS byte mismatch. Related to B F9/F10. |
| F-04 | **CORRECT** | Full UTF-8 decode precedes the BOM check at `parser.py:318-327`; `EF BB BF 80` reports invalid UTF-8 at 3 instead of the declared BOM error at 0. |
| F-05 | **CORRECT** | The manifest has no review namespace; the live verifier fails once required reports exist. The review-free pass remains valid. Overlaps B F7. |
| F-06 | **CORRECT** | The 18 tests omit the reproduced configuration, early-charge, BOM-precedence, review-lifecycle, and full-observation mutations. Detailed rows and both evidence copies share the implementation producer. This does not erase the independent suite labels or normative vector tests. |
| F-07 | **CORRECT** | The two 196,670-byte snapshot directories are byte-for-byte redundant. Overlaps B F11. |
| F-08 | **CORRECT for stale identity; delta characterization UNPROVEN** | Current and recorded wave hashes differ. The old bytes were unavailable, so the claimed exact content delta cannot be adjudicated. Overlaps B F1. |

### 4.2 Critic B

| Finding | Verdict | Independent ruling |
| --- | --- | --- |
| F1 | **CORRECT** | The author receipt is not bound to the current governing wave. Calling it P0 rather than P2 does not change the required reseal. Overlaps A F-08. |
| F2 | **CORRECT, narrowly** | This is a legitimate local hostile W0 audit, but it is not VT.W4/VT.W5 and cannot earn candidate or lock credit. The local audit itself is not mistimed or forbidden. |
| F3 | **CORRECT; supplied reproducer incorrect** | The frozen rows omit every accepted span/tree/event/decoded-value fact, and a corrected deep-copy mutation is invisible. The critic's aliasing `before != after` assertion is false as written. |
| F4 | **CORRECT** | Three defaults are dominated; decoded/node/entry work is charged after payload work; the dense default input used 240.6 MB RSS. This is bounded by input bytes but not an effective independent resource policy. Overlaps A F-01. |
| F5 | **CORRECT** | Budget configuration is unvalidated and tests cover only input and node overrides, not full boundaries or invalid domains. Overlaps A F-02. |
| F6 | **CORRECT** | `Document`, nodes, members, spans, and counters are mutable; `observe()` aliases them; corrupt projection raises untyped implementation exceptions. This is an oracle API defect, not product value/tape credit. |
| F7 | **CORRECT** | The ordinary live verifier is incompatible with append-only reviews. The disposable review-free replay remains valid. Overlaps A F-05. |
| F8 | **CORRECT as a gate gap** | Fixture and manifest generator modes currently reproduce exactly, but `verify.py` does not execute them. No current byte mismatch was found. |
| F9 | **CORRECT** | Python identity is reported, not enforced, and excluded local bytecode can be read. Review-free replay avoided the caches but the sealed execution policy does not guarantee that. |
| F10 | **CORRECT as an evidence gap** | No semantic mismatch appeared in 200,404 additional cases, but the independent ECMAScript differential is absent from the gate and behavior depends on the unpinned Python runtime. |
| F11 | **CORRECT** | Duplicate full snapshots provide no independent invariant. Overlaps A F-07. |

No numbered finding is globally **INCORRECT** or **UNPROVEN**. The two narrower
subclaims identified above are the only proof limitations that change critic
wording.

## 5. Merged finding clusters

| Cluster | Critic IDs | Coherent adjudicated claim |
| --- | --- | --- |
| Resource charging | A F-01; B F4 | Decoded units, nodes, and entries are charged too late; defaults do not form independent ceilings; measured amplification is material. |
| Budget domain/API | A F-02; B F5 | Public and CLI budgets lack one finite non-negative exact-integer domain and complete boundary evidence. |
| JCS authority/toolchain | A F-03; B F9; B F10 | Current JCS bytes look correct on substantial evidence, but delegated authority, interpreter identity, cache-free execution, and an independent differential gate are not closed. |
| BOM identity | A F-04 | The deliberate rejection policy is standards-permitted, but its declared error precedence is unstable. |
| Review lifecycle | A F-05; B F7 | Pre-review sealing works; append-only hostile reports invalidate the live whole-file census. |
| Observation/test depth | A F-06; B F3 | Suite decisions are strong, while accepted value/order/span/string facts are under-frozen and key mutations are absent. |
| Generator replay | B F8 | Suite artifacts are replayed; fixtures and source/file manifest derivations are not part of the ordinary gate. |
| Projection integrity | B F6 | The supposedly read-only observation plane is mutable and corruption does not fail typed. |
| Redundant evidence | A F-07; B F11 | Two full copies add storage and manifest friction, not independent evidence. |
| Governing identity | A F-08; B F1 | The author ledger names stale wave bytes and must be refrozen with a clause delta. |
| Stage and credit | B F2 | The assay may serve W0 after correction but cannot substitute for complete-candidate gestalts. |

## 6. Tranche fit, optimality, fidelity, and friction

### 6.1 Fit and optimality

- **Local tranche fit: PASS with quarantine.** A candidate-neutral JSON oracle is
  a necessary VT.W0 organ, and the author correctly forbids production/candidate
  imports. The handwritten parser is justified as an external oracle because
  ordinary host parsers lose duplicate associations, raw number lexemes, or byte
  spans.
- **Current JSON W0 closure: FAIL.** The local resource, observation, public
  surface, provenance, and gate defects prevent closure.
- **Whole VT.W0 closure: NOT ESTABLISHED HERE.** The current wave also requires the
  exhaustive CSS union, profiles, common policy, and four independent RED
  placeholders. This directory and its author ledger do not establish them.
- **Portfolio/candidate optimality: UNPROVEN AND NOT YET REVIEWABLE.** No complete
  VT-1 through VT-4 implementation is in this subject. This assay must not select
  an architecture or let a founding grammar dominate CSS, types, backends, or
  product consumers.
- **Gestalt timing: locally valid, not VT.W4.** The constitution permits hostile
  review of an implemented evidence organ. The present pass is local W0 closure
  work only; critic B is right that it earns no complete-candidate pass.

### 6.2 Wave/spec fidelity and friction

The artifact honors the important boundary: it is isolated, candidate-neutral,
standard-library-only, and explicitly incomplete. It does not branch on a candidate
or grammar implementation and introduces no production graph edge. Its four main
modules are cohesive and small; external tests mirror source ownership.

Fidelity fails where the wave requires a stable independent oracle and resource
policy. The author overstates five green budgets, a complete frozen observation
surface, exact root identity, and offline authority. Review files and bytecode
caches expose a sealing-policy friction. Duplicate payloads and dominated defaults
violate the constitution's parsimony rule.

The wave itself remains the binding product experiment. Nothing in this narrow
artifact proves or disproves whether four complete orthogonal value/tape verticals
are the optimal portfolio.

## 7. Standards and implemented-feature adjudication

| Feature / authority | Verdict | Narrow fact preserved |
| --- | --- | --- |
| RFC 8259 / ECMA-404 grammar | **PASS on the frozen corpus** | Independent classification accepted 95/95 mandatory positives and rejected 188/188 mandatory negatives. Top-level scalars and complete-input semantics work. |
| `i_` policy | **PASS** | All 35 named decisions match: 21 accept, 14 reject. |
| Strict UTF-8 and byte offsets | **PASS on exercised inputs** | Raw invalid UTF-8, overlong/truncated forms, raw surrogates, and multibyte spans behave coherently. |
| BOM policy | **PARTIAL / FAIL precedence** | Rejecting BOM is a permissible declared local policy; ordinary BOM input gets `bom_forbidden@0`, but malformed suffixes can preempt it. |
| Arrays/objects/depth | **PASS semantics** | Iterative depth 500 accepts and 501 rejects without host recursion. Resource charging remains RED. |
| Duplicate names/source order | **PASS live parser; FAIL frozen breadth** | Ordered member pairs and decoded-equivalent duplicate detection work in authored probes. The 318 rows do not preserve associations/order. |
| Exact number lexemes | **PASS base assay** | Base parsing retains exact decimal source without binary64 narrowing. |
| Strings/UTF-16/scalars | **PASS selected cases; FAIL frozen breadth** | Raw and escaped supplementary values, surrogate pairing, and lone-surrogate facts work. Full accepted facts are absent from corpus rows. |
| UTF-8 byte spans | **PASS selected cases; FAIL frozen breadth** | Authored multibyte slices are exact. Corpus-wide span regressions are invisible. |
| Ordinary typed failures | **PASS narrowly** | Encoding, syntax, scalar, depth, resource, and JCS failures are separated for valid API configuration. |
| Budget configuration | **FAIL** | Invalid public/CLI policy can bypass limits, emit impossible offsets, or raise `TypeError`. |
| Resource accounting | **FAIL** | Success counters are logical and deterministic, but charging is late, defaults are dominated, transient work is not bounded by the named counters, and failures expose no counter receipt. |
| No partial publication | **PASS ordinary parse failure** | No partial `Document` is returned on parser failure. This does not cure transient allocation or mutable successful documents. |
| Source-preserving serialization | **PASS only as defined** | Returning the original owned bytes is exact identity, not a general serializer, repair writer, or detached-subtree API. |
| Optional scalar gate | **PASS** | Retained lone surrogates fail separately when requested. |
| JCS strings, duplicates, sorting | **PASS on exercised cases** | Lowercase escapes, scalar rejection, decoded duplicate rejection, recursive output, and UTF-16 name ordering agree with RFC 8785 examples. |
| JCS numbers | **PROVISIONAL PASS; authority/gate RED** | Appendix B, threshold cases, and 200,404 independent values agree with Node. No mismatch was found; the normative/toolchain proof is incomplete. |
| CLI | **PARTIAL** | Ordinary success/rejection JSON and exit status work. Invalid budgets and OS/file errors are not one typed configuration/I/O plane. |
| JSONTestSuite generation | **PASS decisions; FAIL observation completeness** | Pins, cleanliness, file count, policies, and deterministic suite bytes are strong. Accepted semantic shape is summarized too coarsely. |
| Fixture generation | **CURRENT BYTES PASS; ORDINARY GATE FAIL** | Two independent temporary regenerations matched; `verify.py` does not replay them. |
| Source/file manifests | **CURRENT REVIEW-FREE BYTES PASS; LIFECYCLE FAIL** | Review-free regeneration matches. Reviews, cache exclusions, no external root anchor, and absent regeneration in the gate remain defects. |
| Query fixture | **NOT IMPLEMENTED AS A FEATURE** | It is only ordered observation input; there is no query API, as the author states. |
| Product value/tape/navigation/visitor/detach/crossing/corruption API | **NOT IMPLEMENTED / NO CREDIT** | The Python dictionaries are oracle internals and cannot serve as BBNF-derived product types. |
| Recovery/streaming/incremental/backends/packages/ARM/consumers | **NOT IMPLEMENTED / OUTSIDE LOCAL ASSAY** | Their honest absence does not make base syntax false, but it prevents candidate or lock credit. |

## 8. KISS and greater-product impact

The core parser should be repaired, not replaced. One iterative parser stack,
source slices for numbers, ordered member pairs, UTF-16 facts, and a separate JCS
projection are proportionate to the independent invariants. A recursive host parser,
`dict` projection, or embedded generalized framework would lose evidence or add
unneeded mechanism.

The greater-product boundary is currently mostly good:

- there is no BBNF, parse-that, CSS, Value, Rust, TypeScript, WASM, ARM, package,
  or consumer edge;
- no candidate implementation imports the oracle;
- no product semantic/type authority is claimed;
- no architecture is selected from JSON alone.

The mutable exported `Document` is the one dangerous boundary: prose says
read-only while the API permits mutation and exposes handwritten dictionaries.
Repair must make the comparison artifact immutable/private, not promote it into a
generalized product API. Full observations should remain external oracle bytes
consumed only by candidate tests.

The removable material is clear: two full regeneration snapshot directories,
dominated default values or their false independent-policy claim, excluded local
bytecode, and mutable public observation aliases. No new registry, framework,
schema sidecar, package system, or embedded Ryu implementation is justified.

## 9. Smallest coherent KISS repair set

The following four changes are jointly sufficient; partial fixes would leave the
same proof holes.

### R1 — make resource and failure semantics real

- Validate every `Budgets` field once, before inspecting input: require an exact
  finite non-negative integer (reject booleans and arbitrary runtime values) and
  return one typed configuration error distinct from document exhaustion.
- Check the leading three BOM bytes before whole-input UTF-8 decoding so the
  declared local policy always owns offset 0.
- Reserve node and aggregate-entry capacity before scanning the corresponding
  payload; charge each decoded UTF-16 unit before appending it or constructing
  scalar facts.
- Choose active defaults from a named memory envelope and the measured dense/string
  amplifications, or explicitly remove dominated defaults from the default policy
  while retaining lower caller overrides.
- Attach deterministic consumed counters to resource failures without publishing
  a partial document. Add zero, limit-minus-one, limit, and limit-plus-one tests for
  all fields in library and CLI paths, plus late-work and amplification mutations.

### R2 — freeze one immutable, representation-neutral observation

- Keep the mutable builder/document private. Export canonical immutable bytes (or
  an equivalently frozen value) containing ordered events/nodes, kind, literal and
  decoded string facts, parent/child or member relationships, duplicate association,
  and byte spans.
- Freeze that full normalized observation for every accepted suite input, while
  retaining source hash and JCS outcome. Add mutations for child order, key/value
  association, span, decoded unit, scalar validity, and literal kind.
- Make JCS consume only a validated internal document or translate all corruption
  into one typed projection-integrity error. Do not create a product tape/schema or
  let candidates import the Python implementation.

### R3 — close authority, toolchain, and independent JCS evidence

- Correct the spec claim: identify RFC 8785's delegated RFC 7493, ECMA-262 2019,
  IEEE 754, and Unicode authorities with exact versions/provenance, or narrow the
  offline claim to the material actually cached.
- Pin and enforce the authoritative Python identity (or a proved compatibility
  matrix), reject adjacent bytecode caches, and run all subprocesses from a
  cache-empty prefix/copy.
- Check in a compact deterministic number corpus produced by a pinned independent
  ECMAScript/Ryu authority, emphasizing adjacent binary64 values at every formatting
  threshold, and gate it offline alongside RFC 8785 Appendix B. Preserve the current
  small `_number` implementation if it continues to agree.

### R4 — reseal one review-aware, replay-complete evidence plane

- Refreeze `AUTHOR.md` against the exact current wave bytes and record the clause
  delta; do not replace only the hash.
- Define the sealed implementation namespace explicitly: exclude only root
  `GESTALT-*.md` reports from the subject census, census those reports separately
  as append-only supersession evidence, and reject caches rather than silently
  ignoring them.
- Extend the verifier to regenerate fixtures, fixture manifest, source manifest,
  and file manifest twice in disposable review-free roots and compare them.
- Delete both full regeneration-copy directories. Retain one checked-in expected
  baseline and two fresh runtime generations; if run receipts are desired, store
  only command/toolchain/input/output hashes and status.
- Add mutation tests for the review namespace and every defect above. Record one
  externally anchored root digest for the resealed subject.

## 10. Exact reset and credit disposition

1. Critic A and critic B are the two hostile critics of one local Gestalt pass;
   they are not two consecutive clean passes.
2. Because this adjudication confirms correct findings, this pass is **not clean**
   and the local consecutive-clean count is exactly **0**.
3. The affected JSON W0 evidence cluster reopens. After R1-R4 land in the owning
   implementation and a new immutable ledger is sealed, two new fresh critics and
   a new fresh adjudicator may attempt clean local pass 1. A second consecutive
   clean local pass on unchanged corrected bytes is then required for local oracle
   acceptance under the constitution.
4. Neither this failed pass nor any future local oracle pass counts as VT.W4 or
   VT.W5. Those occur only for each complete independent VT candidate after all
   PT/BB locks, JSON and exhaustive CSS, types, backends, packages, consumers,
   resources, profiles, and artifacts exist.
5. Existing narrow passing evidence may be carried forward only by exact hash and
   rerun under the new ledger. It earns **no candidate credit, no PT/BB lock
   credit, no complete VT.W0 credit, and no production authority**.
6. The terminal artifact label remains **`INCOMPLETE_ASSAY / oracle input only`**.
   No production source movement, candidate import, ranking, architecture choice,
   or greater-product convergence is authorized.

## 11. Final disposition

**REOPEN, REPAIR R1-R4, RESEAL, AND RESTART THE LOCAL CLEAN COUNT AT ZERO.**

The strong base syntax results, iterative deep behavior, exact lexemes, selected
spans/string facts, and exercised JCS outputs remain valuable evidence. They do not
outweigh the independently reproduced resource, observation, API, authority, and
reproducibility defects. The smallest path is a direct repair of this compact
oracle and its evidence gate, not a new framework and not promotion into product
architecture.
