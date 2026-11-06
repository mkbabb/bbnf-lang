# JSON W0 hostile correctness review — critic B

Model: `gpt-5.6-sol`  
Effort: `xhigh`  
Canonical task: `/root/json_w0_g4_review_b`  
Delegation: none

This was the second independent local correctness review of the unchanged sealed
JSON W0 subject. I read the current source, external tests, fixtures, declared
data, cached specifications, verifier, README, tranche constitution, and VT.W0
wave. I did not read any excluded review, author, reseal, campaign, run,
challenge, walls, history/diff, session, or agent/session material. The only
workspace byte written by this review is this report; root review reports remain
outside the sealed implementation census by declared construction.

## Numbered observations

1. **CORRECT FINDING — the verifier admits executed startup code and Python
   modes that are absent from its claimed exact execution identity.**

   Locations: `verify.py:86-155`, especially the partial startup record at
   `verify.py:125-141`; inherited environments at `verify.py:240-272` and
   `verify.py:335-341`; inherited Node execution at
   `src/json_oracle/generate.py:193-227`; and the exact-identity claim at
   `README.md:170-179`.

   `runtime_description()` records six Python environment variables and four
   Python flags, but it neither records nor rejects `PYTHONOPTIMIZE` and it
   records no Node startup environment or options. All Python and Node children
   inherit the ambient environment. Consequently, `NODE_OPTIONS` can execute an
   additional ES module before every Node identity query and number producer,
   while `PYTHONOPTIMIZE=2` changes bytecode semantics (`__debug__ == false`),
   and neither condition changes the enforced runtime descriptor.

   Exact reproduction from the JSON subject root:

   ```sh
   LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
     python3 -S -B verify.py --describe-runtime | /usr/bin/shasum -a 256

   LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
     PYTHONOPTIMIZE=2 \
     NODE_OPTIONS='--import=data:text/javascript,globalThis.__json_w0_unsealed=1' \
     python3 -S -B verify.py --describe-runtime | /usr/bin/shasum -a 256
   ```

   Both commands print the same descriptor hash:
   `0ee773adcc05c245f7fe5487410fc4dbd8490d74240e3c3a2c3cb4c7790f1f7f`.
   Separately,

   ```sh
   LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 PYTHONHASHSEED=0 PYTHONPATH=src \
     PYTHONOPTIMIZE=2 python3 -S -B -c \
     'import json,sys; print(json.dumps({"optimize":sys.flags.optimize,"debug":__debug__},sort_keys=True))'
   ```

   prints `{"debug": false, "optimize": 2}`. Most decisively, the full
   documented verifier command with the harmless inline `NODE_OPTIONS` preload
   also exits 0: both roots pass 41 tests, both regenerations match, and the
   final summary still reports 318 expected rows, two passes, the same 100,000
   row differential digest, and sealed-root hash
   `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`.
   The extra executed code is in neither the subject manifest nor the Node
   closure. A preload can preserve outputs while performing other work or can
   wrap producer globals, so output equality does not repair the evidence-
   identity gap.

   Required correction: launch every governed subprocess with a constructed,
   minimal environment rather than `os.environ.copy()` or implicit inheritance;
   reject unexpected Python and Node startup controls before subject import;
   record and enforce all relevant `sys.flags` (including `optimize`) and exact
   child argv/environment; require `NODE_OPTIONS`/`NODE_PATH` and other preload,
   ICU, OpenSSL, loader, and resolver controls to be absent unless their complete
   executable closure is deliberately bound. Bind the executable resolution
   used for system helpers as well. Add a negative gate proving the reproduction
   above is rejected before any producer runs, then regenerate source/file
   manifests and the external root receipt.

2. **Retained fact — base JSON behavior is complete and internally
   consistent under the declared policy.**

   Locations: `src/json_oracle/parser.py:15-553`,
   `tests/json_oracle/test_parser.py:14-251`, `data/i-case-policy.json`, and the
   pinned suite/expected-result manifests. Strict UTF-8 validation, byte-zero BOM
   precedence, complete-input parsing, RFC 8259 whitespace, exact number
   lexemes, decoded UTF-16 facts, escaped lone-surrogate retention, ordered
   duplicate members, half-open UTF-8 spans, source ownership, flat parse-order
   nodes, and canonical observation bytes agree with the cached RFC 8259 and
   ECMA-404 grammar. The 318-row census remains 95 `y`, 188 `n`, and 35 explicit
   `i` decisions; all declared contracts replayed. The base parser deliberately
   accepts exact overflow/underflow number spellings and escaped non-scalar UTF-16
   facts, leaving finite/scalar restrictions to typed projections.

3. **Retained fact — RFC 8785 projection behavior survived independent
   differential checks.**

   Locations: `src/json_oracle/projection.py:47-204`,
   `data/producers/ecmascript-numbers.mjs`,
   `data/ecmascript-number-corpus.json`, and cached RFC 8785 sections 3.1-3.2.4.
   Duplicate decoded names, lone surrogates, and nonfinite binary64 conversions
   fail typed; strings are serialized without normalization; object names sort by
   unsigned UTF-16 units recursively; arrays retain order; literals and numbers
   use ECMAScript-compatible spelling; projection construction is bounded.

   An independently generated 30,014-row decimal-lexeme probe compared
   `canonicalize(parse(raw))` with Node 26 ECMAScript conversion and found zero
   mismatches across 21,056 finite acceptances and 8,958 typed nonfinite
   rejections (observation digest
   `e7569971f737e2daf0c5ffb48aab109fdad879b60d23b6a9250df22777a46685`).
   A separate 5,000-row nested string/array/object probe, including controls,
   escapes, BMP/non-BMP names, combining sequences, and binary64 rounding, found
   zero mismatches against an independent ECMAScript canonicalizer (digest
   `4b3237d79290932616e03fd349eb968eddc06ea9ec7d07af00f26d1f63dfd497`).
   The governed proof also regenerated all 104 checked threshold rows and matched
   100,000 unique finite binary64 rows at digest
   `330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8`.

4. **Retained fact — all public configuration domains and observed
   precedence rules are typed and deterministic.**

   Locations: `src/json_oracle/parser.py:15-74,415-440,524-553`,
   `src/json_oracle/projection.py:47-75,129-204`, and
   `src/json_oracle/cli.py:20-117`. Every library budget and both output caps
   require an exact non-negative `int`; `bytes`, `Budgets`, and private document
   identities are exact. Configuration precedes input I/O; source type precedes
   source length; input budget precedes BOM, BOM precedes suffix UTF-8, and global
   UTF-8 precedes syntax. Depth reservation precedes node reservation; aggregate
   entry reservation precedes key/child node reservation. Public document
   integrity precedes output-cap validation for corrupt documents, while valid
   documents expose the typed cap configuration failure. JCS document-wide
   validity checks precede projection-byte exhaustion. The compound CLI performs
   scalar validity before JCS regardless of flag order, repeated budget options
   use the last value, all rejected routes produce one ASCII JSON line and status
   1, and accepted routes produce status 0.

5. **Retained fact — admitted resource shapes, bounded construction, and
   output determinism remain sound.**

   Locations: `src/json_oracle/parser.py:136-180,281-412,500-541`,
   `src/json_oracle/projection.py:144-198`, and the resource/CLI tests. A fresh
   14-shape matrix covered scalars, empty and populated arrays/objects, duplicate
   objects, escaped controls, non-BMP strings, and depth-three mixed containers.
   Every shape accepted at its exact five-dimensional counter vector; reducing
   each positive input/depth/node/entry/decoded-unit component by one produced
   the corresponding typed failure, with no mismatch. Repeated observation/JCS
   output was byte-identical at digest
   `cf13c1d8a9ea39670e6d21c269cef12f51dad24fd4aca8c0fe7e4170d85c762e`.
   The verifier's fresh-process maximum and simultaneous-ceiling routes stayed
   below the declared output/RSS envelopes. CLI output for the JCS sorting
   fixture also had the same SHA-256
   `6253900a1706aac4a901427b322fbf1e19ea912a200ed1e5c40b787859fb35a6`
   under hash seeds 0, 1, 777, and random.

6. **Retained fact — generation is target-independent and the sealed file
   evidence is otherwise coherent.**

   Locations: `src/json_oracle/generate.py:53-300`,
   `tests/json_oracle/test_generate.py:17-153`, `data/source-manifest.json`,
   `data/file-manifest.json`, and `verify.py:167-449`. Suite rows derive from the
   exact clean pinned checkout; fixture bytes derive from checked source literals;
   number metadata derives from constants, producer bytes, the live selected Node
   identity, and fresh output digests rather than the corpus target. Absent-target
   generation repeats byte-for-byte and poisoned target metadata is ignored.
   Source and complete sealed path sets are checked; caches are rejected; two
   disposable roots regenerate identical suite, number, fixture, source-manifest,
   and file-manifest bytes. The retained sealed implementation manifest hash is
   `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`.
   Observation 1 is the one exception to accepting the execution evidence as
   exact.

7. **Retained fact — the assay is the right W0 oracle organ but has no
   candidate or production standing.**

   The subject is a compact, external, read-only falsification oracle matching
   VT.W0's JSON-freeze duty. A code search found no import of its Python
   implementation outside its own tests and verifier. It supplies no BBNF-derived
   types, hybrid public value/tape API, CSS L4 coverage, Rust/TypeScript/WASM/ARM
   parity, package consumer, profile, or independent full vertical. It therefore
   remains `INCOMPLETE_ASSAY`, must be consumed only through frozen observation
   bytes, and cannot count toward any of the four VT candidates. The execution-
   identity correction is local to oracle evidence and creates no justification
   for candidate or greater-product movement.

## Final disposition

- Result: **NOT CLEAN — REOPEN JSON W0 evidence identity**.
- Correct findings: **1**.
- Other unresolved or unproven findings: **0**.
- Consecutive clean count: **0**; this correct finding resets the count.
- Review-report count: baseline verifier observed **11** existing root reports;
  this report makes the root namespace count **12** while remaining outside the
  sealed implementation manifest.
- Candidate state: **0 credited candidates**; JSON W0 remains
  `INCOMPLETE_ASSAY / oracle input only`.
- Movement state: **none; production and candidate movement remain prohibited**.
  Correct the environment/argv identity gate, reseal its evidence, and rerun two
  fresh hostile passes before this implementation can accumulate a clean count.
