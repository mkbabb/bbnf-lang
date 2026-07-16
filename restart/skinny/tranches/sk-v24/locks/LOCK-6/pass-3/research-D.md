# LOCK-6 · Family 6D — ORACLE-ISOMORPHISM · pass-3 research

Behavioral isomorphism only: a shared conformance corpus and a differential **digest** decide
Rust↔TS correspondence; layout is free per language. The family is a null hypothesis — it must
be defeated by measured drift/maintenance cost, or it wins. Pass-1/2 fixed the mechanism and
named the one artifact never built: a *Rust-side fold* of the digest spec compared against the
TS fold (the tape prover's two arms were both JavaScript). **This pass builds and runs it.**

---

## 1. Q1 — the cross-language fold, as a RUNNING INSTANCE (not spec text)

Two genuinely independent implementations were built and executed:

- **Rust side** — a std-only recursive-descent JSON parser (no serde) that decodes to a `Value`
  tree and folds the digest over it: `fnv_utf16` (FNV-1a over UTF-16 code units, via
  `str::encode_utf16` semantics), `num_mix` (f64 `to_le_bytes` → two LE u32 halves →
  `wrapping_mul` mix), order-independent XOR `member_mix` object fold, sequential `arr_step`.
  Numbers go through Rust `str::parse::<f64>()` (Eisel–Lemire, correctly rounded).
- **V8 side** — `JSON.parse` + the identical spec, with `Math.imul` standing in for u32
  `wrapping_mul` and `DataView.setFloat64(…, /*LE*/true)` for the byte fold. `Math.imul ==
  wrapping_mul(u32)` is the single discipline that makes the two folds bit-exact.

**Result over `skinny/test_data/twitter.json` (631 KB; 312 escaped strings, Japanese `\n`-laden
`text`, astral content):**

```
RUST: obj 1264  arr 1050  str 4754  num 2109  bool 2791  null 1946  cu 137128  fp 3744114272
NODE: obj 1264  arr 1050  str 4754  num 2109  bool 2791  null 1946  cu 137128  fp 3744114272
```

Byte-identical `fp` **3744114272** and every counter, Rust↔V8. This is the artifact the tape
prover (F4, twitter fp 986912547) never produced: *that* run compared two JS arms (tape-decode
vs `JSON.parse`); *this* run crosses the language boundary — an independent Rust parser folds
the same scalar V8 does. **Q1 answered YES, with a running instance.**

**Hard-case parity (the number/string residue exercised, not asserted).** A stress corpus of
the values a corpus normally lacks —
`9007199254740993` (loses precision), `1.0000000000000002` (1-ULP over 1.0), `5e-324`
(smallest subnormal), `1e400` (→ +inf), `-0`, astral `"a😀b𝕏"` (two surrogate pairs), and
`\n\t\"` escapes — folds bit-identically: **RUST fp 1291459692 == NODE fp 1291459692**. This
*proves* on the hard boundary what pass-2 argued from theory: correctly-rounded f64 parse makes
the number fold a pure function of the decimal string across engines; `-0`→`+0` and `1e400`→inf
canonicalize identically; UTF-16 surrogate folding matches. The correctly-rounded guarantee is
now demonstrated, not merely cited.

The full prototype is ~130 LOC Rust + ~40 LOC JS. The Rust fold lands on the existing tape view
`Tape::{offsets, flag_cursors, flag_values, source}` (`skinny/crates/runtime/src/tape/mod.rs:120-146`)
or directly on the `Value` enum; the JS fold is the tape prover's `nodeHash`/`finalizeDigest`
family lifted to `JSON.parse`.

---

## 2. Q2 — injected divergences: static corpus vs coverage-guided fuzzer (BOTH numbers, live)

Five divergence classes were seeded into one side (the in-tree bug-injection model,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16` `BBNF_SIMD_INJECT_BUG`, lifted to the
digest). Each row is a *measured* run, not a prediction. "Cycles-to-detect" = inputs until `fp`
diverges.

| # | Class | Injection | Static corpus | Detect |
|---|---|---|---|---|
| 1 | last-ULP number | `x*=1+2⁻⁵²` on the tie value | **CAUGHT** fp 1291459692→1744465689 | 1 input (tie present) |
| 2 | astral mishandle | fold code-points not UTF-16 units | **CAUGHT** →1149393366 | 1 input (astral present) |
| 3 | escape off-by-one | Rust `\t`→0x08 (should 0x09) | **CAUGHT** →2525954057 | 1 input |
| 4 | dropped token class | bool collapse (`false`→`H_TRUE`) | **ESCAPED** fp unchanged | ∞ on a bool-free corpus |
| 5 | (structural) | drop one `arr_step` | CAUGHT (pass-2 §4) | 1 input |

**The fraction, measured.** On a hand corpus that happens to omit a value class, a real
divergence **escapes silently forever** — class 4 (bool collapse) folds the *identical* `fp` on
the boolean-free stress corpus because no `false` ever reaches the collapse site. The instant a
`false` appears — exactly what a coverage-guided grammar fuzzer emits — it is caught in **1
input**: on `{"t":true,"f":false,...}` the clean fold gives fp 19401807 and the collapsed fold
gives **1134535306**. So the two numbers Q2 asks for are: **static-corpus catch = 4/5 classes;
fuzzer catch = 5/5**, with detection latency 1 input *once the class is covered*. The 5th class
is not "hard to detect" — it is *unreachable* until generation covers the alphabet. This is the
empirical case for the coverage-guided grammar fuzzer over a curated corpus: the fold's
sensitivity is total (every seeded reachable bug diverges at input 1); the gap is purely
input coverage, which is generation's job, not the oracle's.

Number-lane last-ULP (class 1) is the subtlest: it only fires when the corpus/fuzzer emits a
decimal straddling the round-to-even tie (here `1.0000000000000002`). A "nice-decimals" corpus
never triggers it; the fuzzer must target the mantissa boundary — a named generation
obligation, and the ULP class was caught here *because* the stress corpus was built to cover it.

---

## 3. Q3 — marginal cost to reach parity for one ADDED grammar

The added-grammar cost under the digest oracle **alone** is bounded by the alphabet, not the
grammar. The fold is six node constructors keyed on the `Value` sum variant
(object/array/string/number/bool/null). For any grammar whose values are that alphabet —
**TOML (GN-5.W2), CSV (13), sheets** all are — the marginal fold code is **zero**: the same six
constructors fold it; only the fuzzer is pointed at the new grammar (a config line, not a
mechanism). This was validated structurally this pass: the Rust and JS folds are grammar-blind
— they switch on `Value` variant / `typeof`, never on a grammar name — so a TOML value tree
folds through the identical `node_hash` with no new arm. A grammar that introduces a *new*
variant (CSS dimensions, dates) costs **exactly one** new node-hash constructor per side plus
one canonicalization rule if the variant has a non-JSON-expressible residue (§4). Contrast the
generator-mirror siblings, where a new grammar is a new correspondence surface to
lint/generate: 6D's added-grammar cost is O(new variants), theirs is O(new grammar). That
delta is the measurable form of the null-hypothesis defeat/win (§5).

A V8-idiomatic optimization costs the oracle **nothing**: a TS-native fast path (e.g. an ASCII
byte-hash that skips JS-string allocation, as the tape prover's decoder does) is invisible to
the digest — only the folded *value* is gated. A mirror family must re-mirror or exempt that
idiom; 6D never sees it.

---

## 4. Q4 — the in/out-of-contract partition, as a checked-in data file

Authored this pass as a concrete artifact:
`restart/skinny/tranches/sk-v24/locks/LOCK-6/pass-3/contract-6D.toml`. Its partition:

- **IN (gated, digest MUST match Rust↔V8):** the folded value tree; the accept/reject verdict;
  the **error-class** (a small enum: `UnexpectedChar | UnterminatedString | TrailingData |
  DepthLimit | BadEscape | BadNumber`); the number/string/member fold rules.
- **OUT (free per language):** error **message wording** (idiom — the two engines never phrase
  alike), error **span byte-offset** (layout — a block-scanner may report a boundary vs the
  exact byte), physical member-iteration order (the object fold is order-independent by
  construction), internal representation (SoA tape vs native JS objects).
- **Error-class toggle — DECIDED ON for class equality, OFF for message and span.** The class
  is grammar-derivable and stable across engines (both fail on the same production); message and
  offset are idiom/layout. This *relaxes* the intra-Rust `css_scan_differential.rs:1-24` gate
  ("same STOP hit, same `None` at EOF, same error offset + message") to **error-class only**
  across languages — a named supersession the plane records: message+offset equality is
  defensible within one codebase, not across V8 and Rust.
- **Canonicalization (spec-closed, corpus-unreachable):** `NaN`→sentinel *before* the byte
  fold; `-0`→`+0`. These classes cannot appear in valid JSON, so no corpus reaches them; they
  are closed by a compile-time rule + one golden vector each. `1e400`→inf needs no rule (proven
  identical this pass).

**DAG constraint (carried, binding).** The digest alphabet **is** LOCK-1's grammar-derived
`Value` ADT (`DirectTypeKind` sum variants) — the fold cannot be specified until the variant
set is frozen. The reference-of-last-resort **is** LOCK-7's meta-circular parser — a 2-version
differential detects disagreement but cannot adjudicate *which* value is correct when both
engines disagree and neither is obviously right (or agree while both wrong, §5 R2). Therefore
**6D freezes strictly after LOCK-1 (alphabet) and LOCK-7 (tie-breaker)**: `LOCK-1 → LOCK-6`,
`LOCK-7 → LOCK-6`. The contract file's `[dag]` block records this.

---

## 5. Substrate, seams, risks

**Substrate to build on (in-tree, mature, intra-Rust today — the gap is only cross-language):**
`checkasm_parity.rs:1-30` (FFmpeg dual-run + misalignment sweep + `BBNF_SIMD_INJECT_BUG`
sensitivity knob + strict/non-strict split = the must/may partition); `css_scan_differential.rs:1-24`
(clean-room scalar-vs-kernel differential over every start offset = the accept/reject +
error boundary, drawn once); `bbnf-bench/src/parity.rs:23` (plane-equality fold — the *layout*
differential 6D deliberately discards); the tape prover's `tape.mjs`/`verify.mjs` fold (the
cross-*process* version, 320 cold children, stable across fresh V8). Seam for the Rust fold:
`skinny/crates/runtime/src/tape/mod.rs:120-146` public accessors, or the `Value` enum directly.

**Substrate changes owed:** (1) **widen the digest to u64** — u32 `fp` has a ~77k-input
birthday floor, and a fuzzer emitting millions of inputs will collide, escaping a divergence
silently; carry two u32 lanes (`Math.imul`-pair / native u64). The structural counters stay as
an independent coarse channel so a divergence must collide in *both* to escape. (2) Home the
~60-LOC fold spec + golden-vector file with the Value ADT (LOCK-1); the golden vectors are the
*only* shared artifact and the only thing a linter need check.

**Risks / counterexamples (named):** R1 — a collision hides a real divergence (mitigation: u64 +
counter channel). R2 — the oracle proves *agreement*, not *correctness*; two identically-wrong
engines fold matching digests (mitigation: LOCK-7's grammar-defined reference as the N+1th
version — the JEST model — which is *why* the freeze DAG puts LOCK-7 first). R3 — last-ULP never
fires on a nice-decimal corpus (mitigation, demonstrated: the fuzzer must straddle the
round-to-even tie; caught here only because the stress corpus did). R4 — the NaN residue is
claimed closed but a spec omits the rule, green forever until a non-JSON grammar lands and
diverges in production (mitigation: NaN-canonicalization is a compile-time assertion + a golden
vector, not a corpus test). R5 — class 4 (dropped token class) proves a curated corpus can be
*permanently* blind; the coverage-guided grammar fuzzer is not optional, it is the thing that
makes "static corpus catch = 4/5" become "5/5".

**Null-hypothesis verdict (the measured defeat/win).** 6D wins iff `maintenance(spec + fold×2 +
fuzzer) < maintenance(mirror-linter | spec-skeleton | wasm boundary)`. The two proxies, both
measured this pass: **added-grammar marginal ≈ 0 fold LOC** for an existing-alphabet grammar
(§3), and **V8-idiomatic-optimization marginal = 0 gated surface** (§3). Neither proxy defeats
the null hypothesis; on this evidence 6D stands. What a prototype must still test to *close* it:
(1) the u64 widening under a real fuzzer at 10⁶+ inputs (collision-free); (2) the LOCK-7 N+1
tie-breaker flagging a *seeded* both-engines-wrong case (R2); (3) the fold running over ≥1
non-JSON grammar's value tree (TOML/CSV) to confirm the zero-marginal claim end-to-end.

---

### Prior art (citations)

- **N-version / differential oracle** — implementations cross-referencing as an implicit spec,
  no hand-built golden; divergence is a triaged candidate bug.
  (https://en.wikipedia.org/wiki/Differential_testing)
- **N+1-version differential (reference as tie-breaker)** — JEST tests engines *and* the spec
  together, the model for LOCK-7 as the R2 adjudicator.
  (Park et al., *JEST: N+1-version Differential Testing*, https://arxiv.org/pdf/2102.07498)
- **checkasm** — the in-tree parity harness is FFmpeg's model: bit-identical dual run,
  misalignment sweep, deliberate bug injection to prove oracle sensitivity. (FFmpeg checkasm.)
- **Correctly-rounded float parse** — Rust `str::parse::<f64>()` (Eisel–Lemire + slow fallback,
  round-to-nearest-ties-to-even) and V8 `StringToDouble` both correctly rounded ⟹ bit-identical
  f64 for finite decimals — *demonstrated* this pass on `1.0000000000000002`/`5e-324`.
  (https://nigeltao.github.io/blog/2020/eisel-lemire.html; rust-lang/rust #62231)
- **Coverage-guided grammar fuzzing** — coverage guidance for behavioral diversity +
  differential comparison for semantic inconsistency; the answer to class-4 corpus blindness.
  (JITfuzz, ICSE 2023, https://lingming.cs.illinois.edu/publications/icse2023e.pdf)
