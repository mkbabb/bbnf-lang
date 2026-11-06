# GESTALT-3 adjudication — JSON W0 byte oracle

Date: 2026-07-19 (`America/New_York`)  
Disposition: **NOT CLEAN / REOPEN LOCAL ASSAY**  
Subject seal: `data/file-manifest.json` SHA-256
`f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5`

## Independence and model-truth gate

The gate was stated before repository inspection:

- model: `gpt-5.6-sol`;
- reasoning effort: `xhigh`;
- canonical task/thread: `/root/json_w0_g3_adjudicator`;
- turn: `1`;
- session: the current shared Codex workspace session; no opaque session UUID
  was exposed to this agent.

The required model/effort gate passed. This adjudicator did not read any earlier
`GESTALT-*` report or adjudication, any run/campaign/challenge/walls ledger, agent
messages, or repository history/diff. Review was limited to the current
constitution and wave, the complete current JSON assay source, tests, data,
fixtures, manifests, verifier and author/receipt claims, and exactly the two
Gestalt-3 critic reports. The cached ECMA-404 PDF was hash-checked, rendered across
all 16 pages, visually inspected, and text-extracted; both cached RFC texts were
also read.

| Governing input | SHA-256 |
| --- | --- |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| `GESTALT-3-CRITIC-A.md` | `dce15ce008a464f326b2c5dff3acc8de669855f67255d0ecb4ed5fac3854e906` |
| `GESTALT-3-CRITIC-B.md` | `a9cb13f0d610903f1b744be4a726b07376171a4fbd11c38aeceae426b1ce5593` |
| `data/file-manifest.json` | `f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5` |

This is an external input-oracle adjudication. It awards zero candidate, lock,
feature-product, architecture, profile, or production credit. The absence of
BBNF-derived types, a value/tape API, CSS, packages, consumers, or target backends
is not a defect in this oracle and is not part of the repair ordered below.

## Numbered finding rulings

| Finding | Ruling | Effect |
| --- | --- | --- |
| Critic A finding 1 — public CLI resource envelope | **CORRECT** | Wave-blocking resource and parsimony defect. |
| Critic A finding 2 — corpus-metadata self-inheritance | **CORRECT** | Wave-blocking artifact-reproducibility defect. |
| Critic B finding 1 — public CLI combined-ceiling resource envelope | **CORRECT** | Same owning mechanism as A1, independently and more strongly exercised; not a second weakened formulation. |

There are no incorrect or unproven numbered findings.

### A1 — correct

The checked resource test at `tests/json_oracle/test_parser.py` measures only
`observe(parse(source))`. The public command instead executes
`json.loads(observe(document))` and then `json.dumps(result)` in
`src/json_oracle/cli.py`. It therefore retains and materializes a second complete
observation tree. `observe`, `require_unicode_scalars`, and `canonicalize` each
enter `_integrity`; using either optional projection after constructing the CLI
tree reparses the entire source again while that tree is resident.

An independent exact-default probe used 29,999 unique fixed-width base-36 names,
lengthened deterministically to 131,072 total decoded UTF-16 units, and legal
trailing whitespace to reach exactly 1,048,576 input bytes. It was accepted with:

```json
{"aggregate_entries":29999,"decoded_utf16_units":131072,"input_bytes":1048576,"max_depth":1,"nodes":59999}
```

On the sealed Apple arm64 runtime, fresh processes produced:

| Route | Output bytes, including CLI newline where applicable | Peak RSS |
| --- | ---: | ---: |
| library `observe(parse(source))` | 11,602,772 | 121,864,192 B |
| CLI base | 11,602,773 | 148,045,824 B |
| CLI `--require-scalars` | 11,602,773 | 183,336,960 B |
| CLI `--jcs` | 11,943,953 | 182,681,600 B |

The equivalent library route remained below 128 MiB, while every public CLI route
exceeded 134,217,728 bytes. This independently reproduces A's mechanism and a
stronger simultaneous-ceiling form. A's smaller distinct-key object and its
run-specific RSS samples are consistent with the same defect; exact RSS values are
measurements, not portable artifact identities.

### A2 — correct

`generate_numbers()` reads
`ROOT / "data/ecmascript-number-corpus.json"` as `template`, obtains its entire
`producer` object from that target, replaces only `cases`, and writes the result.
Thus the checked producer freshly emits 104 case rows, but it does not independently
produce `authority_scope`, `focus`, `revision`, producer identity, commands, seed,
counts, uniqueness rule, or expected output digests.

In a disposable source-shaped subject, the checked-in corpus target was poisoned
with:

```json
{"authority_scope":"adjudicator-sentinel-authority","focus":"adjudicator-sentinel-focus","revision":777}
```

Generation to a destination path that did not previously exist succeeded and
copied all three sentinels unchanged while emitting 104 fresh cases. In a second
copy containing the implementation and checked producer but no target corpus,
`--numbers` exited 1 with `FileNotFoundError` for
`data/ecmascript-number-corpus.json` and emitted no corpus.

Critic B's retained observation that a fresh or absent **destination directory**
can be populated is true but does not contradict this finding. The generator still
reads the checked-in **target artifact** from the subject root as its recipe.
Accordingly, B's broader phrase “complete fresh artifact generation retained” is
narrowed: destination creation and case-row regeneration are retained; complete
corpus-document regeneration is not.

### B1 — correct

B's combined-ceiling finding is independently reproduced by the exact probe above:
the input simultaneously reaches the input-byte and decoded-unit ceilings, remains
one node below the node ceiling and 10,001 entries below the entry ceiling, and is
valid for both scalar and JCS projection. Its base output is also above the test's
10,000,000-byte object observation bound. The public base, scalar, and JCS routes
all exceed the documented 128 MiB process-RSS target.

A1 and B1 therefore merge at one owning cause without losing either critic's
falsifier: A identifies the removable serialized-bytes-to-tree-to-serialized-bytes
path and requires public-route coverage; B adds the simultaneous live-ceiling
shape, output-size evidence, and the repeated-integrity-reparse composition. Both
must remain in the repaired regression gate.

## Author and verifier claim disposition

| Claim | Disposition |
| --- | --- |
| Exact subject, source, specification, fixture, runtime, and suite identities | **RETAINED** on the current seal. |
| Strict UTF-8, complete input, BOM policy, exact number lexemes, decoded string facts, duplicate retention, byte spans, iterative depth and typed failures | **RETAINED ON THE EXERCISED SCOPE**. |
| RFC 8785 primitive/string/order/duplicate/scalar/finite-number behavior | **RETAINED ON THE EXERCISED SCOPE**. |
| CLI reads at most `max_input_bytes + 1` and totalizes tested argument failures before file I/O | **RETAINED**; this does not imply bounded whole-command memory. |
| Accepted default shapes fit the 128 MiB public-process envelope | **REJECTED FOR THE CLI**. The existing measurements and gate cover the library route, not the advertised public composition. |
| The complete ECMAScript number corpus document is independently regenerated | **REJECTED**. Only producer case rows and the differential stream are independently regenerated; corpus metadata is inherited. |
| Generation into a fresh destination works | **RETAINED BUT NARROWED**. It still depends on the checked-in target artifact as input. |
| The assay is only an `INCOMPLETE_ASSAY / oracle input` | **RETAINED** with zero product credit. |

The official verifier passing is real but not dispositive. Its equality check
repeats the same self-inheriting number generator, and its resource tests never run
the simultaneous-ceiling public CLI. Author tests and receipts cannot score those
planes clean under the implemented-wave acceptance law.

## Exact retained machine facts

- The subject seal matched the requested
  `f158e3e57bd6abfad28753a6e332c6f8ea43122afd64e034a9e107d9dc79fdb5`.
  All 41 sealed records were path-unique and matched their declared sizes and
  SHA-256 values; the live non-review sealed path set also contained exactly 41
  files. Source-manifest records were exact. Before this report, eight root
  `GESTALT-*.md` files existed outside the seal; this adjudication becomes the
  ninth and does not alter implementation identity.
- One independent official verifier invocation passed two cache-empty disposable
  roots with 35 tests in each root. It reported `expected_rows=318`, `passes=2`,
  `network="unused"`, the requested sealed root, and 104 producer rows plus
  100,000 unique finite-binary64 differential rows at SHA-256
  `330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8`.
  This retains the row/differential facts, not the inherited corpus metadata.
- The frozen JSONTestSuite identity remained commit
  `1ef36fa01286573e846ac449e8683f8833c5b26a`, tree
  `b936f9acdd24b9f5fefe68b90b9beab2c681137a`, with 318 unique paths: 95 `y`,
  188 `n`, and 35 `i`. All 318 declared contracts held; the frozen observations
  contain 116 base accepts and 202 base rejects. All 35 `i` rows have one policy
  (21 accepted and 14 rejected).
- Across the 318 frozen rows, all base accepts retained source-preserving
  roundtrip. JCS recorded 99 accepts, 202 base-not-applicable results, and 17 typed
  rejects: 2 duplicate-name, 10 non-scalar-string, and 5 non-finite-binary64.
- All 15 fixture paths were unique and exact. Their base/JCS observations and the
  two custom resource failures regenerated in both verifier roots.
- The 104 checked number rows were unique and covered 11 named threshold groups.
  The producer program remained SHA-256
  `921cbcb65c2997eb4afffcf930bf816a985d589ecd07b27088003bc9a90a7bbe`;
  its case-stream digest remained
  `5a98ae4059f67693b2d0994c3f2d570787ed2be523718e4ffd4defb591fa5c3c`.
- The enforced runtime closures matched CPython 3.14.5/arm64 at
  `a3c45710552b7fc8dda24e8cc25615b3f22c56302d725f1359c2c4d38085e94a`
  and Node 26.0.0 / V8 14.6.202.33-node.19/arm64 at
  `be37fca7d3ed1504a2ad774b13780b3e6808e9fa39e583d807af1f664a038314`.
- The fresh public resource counterexample and producer poison/delete results are
  exact adjudicator evidence recorded above. Auxiliary million/quarter-million
  critic probes are not needed to decide these findings and are not promoted into
  new artifact identities.

## All-plane verdict and clean-count effect

| Plane | Verdict | Reason |
| --- | --- | --- |
| Tranche fit | **RETAIN / PASS** | A small, independent byte-observation oracle is a useful VT.W0 falsification organ. It remains narrower than a candidate and honestly says so. |
| Wave fidelity | **REOPEN / RED** | The public resource policy and full-corpus regeneration claim are false on the sealed implementation. |
| Feature truth | **FAIL** | Exercised RFC 8259/RFC 8785 semantics and artifact identities retain evidence, but public CLI resources/parsimony and complete corpus reproducibility fail. |
| Greater-product effect | **CONTAINED / PASS WITH NO CREDIT** | No production import, second product authority, candidate edge, or architecture selection is authorized. The duplicated CLI execution path must not be copied into product code. |
| Product | **`INCOMPLETE_ASSAY`, ZERO PRODUCT CREDIT** | This pass neither demands nor supplies candidate features, lock closure, performance standing, or production movement. |

Overall verdict: **NOT CLEAN**. The consecutive-clean count remains **0 -> 0**.
Both correct mechanisms reopen the local assay; neither critic can count as a clean
hostile pass. The broader VT.W0 wave remains born RED independently of this local
repair.

## Smallest ordered repair prescription

1. **Land external failing gates first.** Generate the exact simultaneous-ceiling
   object in `tests/json_oracle/test_cli.py`; run fresh base,
   `--require-scalars`, and `--jcs` processes; assert accepted counters, declared
   output policy, and the 128 MiB ceiling. Retain the existing independent
   depth/array/object/string boundaries rather than replacing them with the one
   combined shape.
2. **Remove public-path duplication without weakening integrity.** Validate the
   source-derived immutable document once for one compound CLI observation, then
   stream or directly write the already-canonical observation bytes. Add JCS or
   scalar results without an `observe bytes -> json.loads tree -> json.dumps`
   round trip and without retaining another full observation during a second
   integrity reparse. Public standalone projection functions must continue to
   enforce the same complete source-derived equality boundary. Explicitly bound
   output/projection construction or fail typed on exhaustion.
3. **Make number generation target-absent.** Remove the read of
   `data/ecmascript-number-corpus.json` from `generate_numbers()`. Construct
   semantic metadata from a small independent checked recipe and derive runtime,
   program, command, count, seed, digest, and identity fields from the checked
   producer/runtime facts. Generate the complete document when both destination
   and checked-in target are absent.
4. **Prove non-inheritance.** Add delete and poison tests: stale target metadata
   must not flow, a missing target must still generate, every emitted producer
   identity field must be checked, and two absent-output roots must equal the
   checked artifact. Preserve the existing 104-row and 100,000-row comparisons.
5. **Refreeze only after both mechanisms pass.** Regenerate source/file manifests,
   fixtures and data; update `README.md`, `AUTHOR.md`, and the external reseal with
   public-CLI resource measurements and an honest producer boundary; issue a new
   subject seal. Then run two fresh consecutive hostile gestalt passes. Do not add
   product schema, value/tape, CSS, backend, package, or consumer work to this
   input-oracle repair.
