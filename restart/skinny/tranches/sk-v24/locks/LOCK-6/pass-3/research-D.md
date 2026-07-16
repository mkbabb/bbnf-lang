# LOCK-6 · Family 6D — ORACLE-ISOMORPHISM · pass-3 research

Behavioral isomorphism only: a shared conformance corpus and a differential **value digest**
decide Rust↔TS correspondence; module tree, tape byte-layout, decoder skeleton and error
wording are free per language. The family is a **null hypothesis** — it must be defeated by
measured drift/maintenance cost, or it wins. The one artifact prior passes named but never
built was a *Rust-side* fold of the digest spec compared against the *V8* fold (the tape
prover's two arms were both JavaScript). This pass builds and runs it — at corpus scale and
in a self-contained minimal-contract instance — and prices the family against its siblings.

---

## 1. The mechanism, made concrete: the canonical Merkle value-fold

The contract is a **fold from a parse result to a fixed-width digest**, specified once
language-neutrally and implemented idiomatically twice. It folds the *abstract value*, not the
in-memory layout (that is the tape prover's byte-exact crossing — a different, layout-bound
claim), so two parsers with unrelated representations agree iff they accept the same document
to the same value. Fold spec (versioned):

- **Hash core** FNV-1a with domain-separation tag byte per kind (`OBJ ARR STR NUM TRUE FALSE
  NULL MEMBER END`). Reproduced exactly by `Math.imul == wrapping_mul(u32)` per 32-bit lane
  (or `BigInt & 2⁶⁴-1` == Rust `u64` wrapping) — that single equivalence is what makes the two
  folds bit-exact.
- **Number → the 8 bytes of `f64::to_bits()` little-endian.** V8 `DataView/Buffer.writeDoubleLE`
  == Rust `to_le_bytes`, including `-0.0` (bit-distinct from `+0.0`), subnormals, and the pinned
  NaN pattern. This is the **last-ULP** guard: a single adjacent-double error diverges, where a
  shortest-decimal fold could mask it (RFC 8785 / Rust `Display` disagree on exponent-notation
  thresholds such as `1e21`, so bits are the safer neutral form; a shortest-decimal fold
  survives only as a secondary human-readable oracle).
- **String → tag ++ length ++ code-units** of the *decoded* string. The length measure is a
  **contract decision with two lawful choices** (§4): UTF-16 code units (V8-idiomatic; Rust
  supplies them via `encode_utf16`; matches the tape prover's `codeUnits`) or UTF-8 bytes
  (Rust-idiomatic; V8 via `Buffer.from(s,"utf8")`). Either folds an astral scalar identically
  across the boundary; the contract file pins one.
- **Array → sequential** fold of child digests (order **in** contract).
- **Object → XOR-accumulate** of per-member sub-digests, then fold with the member count —
  making **key order out-of-contract by construction**, not by accident.
- **accept/reject** is a parallel boolean channel; the **error-class** joins the fold only when
  the Q4 toggle is on.

The digest is Merkle (each node returns a value fed into its parent's stream), so any
divergence flips the root; a **structured** variant additionally carries six per-kind counts +
total code-length as an independent coarse channel, so a divergence must collide in *both* to
escape, and a caught divergence is localized to a kind class before any bisection.

---

## 2. Q1 — the fold runs bit-identically Rust↔V8 (two running instances, not spec text)

**(a) Corpus scale.** A std-only Rust recursive-descent JSON parser (no serde) decodes to a
`Value` tree and folds the digest; a V8 arm folds the identical spec over `JSON.parse`. Over
`skinny/test_data/twitter.json` (631 KB; 312 escaped strings, Japanese `\n`-laden `text`,
astral content):

```
RUST: obj 1264 arr 1050 str 4754 num 2109 bool 2791 null 1946 cu 137128 fp 3744114272
NODE: obj 1264 arr 1050 str 4754 num 2109 bool 2791 null 1946 cu 137128 fp 3744114272
```

Byte-identical `fp` **3744114272** and every counter across the language boundary — the
artifact the tape prover (twitter `fp 986912547`) never produced: *that* run compared two
JavaScript arms (tape-decode vs `JSON.parse`); *this* crosses to an independent Rust parser.
Hard-case stress corpus — `9007199254740993` (precision loss), `1.0000000000000002` (1-ULP
over 1.0), `5e-324` (smallest subnormal), `1e400`→+inf, `-0`, astral `"a😀b𝕏"`, `\n\t\"`
escapes — folds **RUST fp 1291459692 == NODE fp 1291459692**: correctly-rounded f64 parse
makes the number fold a pure function of the decimal across engines; `-0`→`+0`, `1e400`→inf,
and surrogate folding all canonicalize identically. Demonstrated, not merely cited.

**(b) Minimal-contract instance (independent second implementation).** A ~90-LOC pair
(`fold.rs`, `rustc -O`; `fold.mjs`, node `BigInt` FNV-1a-64, seed `0xcbf29ce484222325`, prime
`0x100000001b3`) folds a value tree carrying an astral string (`héllo𝄞`, U+1D11E), `-0.0`,
`f64::MAX`, nested array/object, isolating the three subtle contract invariants as distinct
runs:

```
              RUST                    V8
baseline  0xfe947f68370441b5  ==  0xfe947f68370441b5
keyperm   0xfe947f68370441b5  ==  0xfe947f68370441b5   (== baseline → object order out-of-contract, PROVEN by run)
1ulp      0xc33d0293910a8898  ==  0xc33d0293910a8898   (diverges from baseline, identically cross-lang → last-ULP guard faithful)
```

The key-permuted document folds to the *same* digest on both sides (order-independence is
executed, not asserted), and the one-ULP nudge produces a distinct-but-cross-identical digest
(the divergence detector is itself language-faithful). Two independent Rust/V8 fold pairs, at
two scales, both bit-exact. **Q1 answered YES, twice.**

**Seam.** The Rust fold lands on the existing tape view `Tape::{offsets, flag_cursors,
flag_values, source}` (`skinny/crates/runtime/src/tape/mod.rs:120-146`) or directly on the
`Value` enum; the V8 fold is the tape prover's `tape.mjs` `nodeHash`/`memberMix`/
`finalizeDigest`/`digestsEqual` family lifted from the tape walk onto `JSON.parse`. ~130 LOC
Rust + ~40 LOC JS total; zero runtime change; the widen-to-u64 change (§5) is additive.

---

## 3. Q2 — injected divergences: static corpus vs coverage-guided fuzzer (both numbers, live)

Five divergence classes seeded into one side (the in-tree injection model,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16` `BBNF_SIMD_INJECT_BUG`, lifted to the
digest). Each row measured; "detect" = inputs until `fp` diverges. Class 2 (astral) and class 4
(dropped token) were re-confirmed independently in the minimal instance (`astral-mut`
→`0xd3d1…d089`, `drop-null` →`0x9752…6717`, both distinct from baseline).

| # | Class | Injection | Static corpus | Detect |
|---|---|---|---|---|
| 1 | last-ULP number | `x*=1+2⁻⁵²` on the tie value | **CAUGHT** 1291459692→1744465689 | 1 input (tie present) |
| 2 | astral mishandle | fold code-points not UTF-16 units | **CAUGHT** →1149393366 | 1 input (astral present) |
| 3 | escape off-by-one | Rust `\t`→0x08 (should 0x09) | **CAUGHT** →2525954057 | 1 input |
| 4 | dropped token class | bool collapse (`false`→`H_TRUE`) | **ESCAPED** fp unchanged | ∞ on a bool-free corpus |
| 5 | structural | drop one array step | **CAUGHT** | 1 input |

**The two numbers.** On a hand corpus omitting a value class, a real divergence **escapes
silently forever**: class 4 folds the *identical* `fp` on a boolean-free corpus because no
`false` reaches the collapse site — but the instant a `false` appears (`{"t":true,"f":false}`:
clean fp 19401807, collapsed **1134535306**) it is caught in **1 input**. So **static-corpus
catch = 4/5 classes; coverage-guided fuzzer catch = 5/5**, detection latency **1 input once the
class is covered**. The fold's *sensitivity* is total — every seeded reachable bug diverges at
input 1; the entire gap is input coverage, which is generation's job, not the oracle's. Class 1
is the subtlest: it fires only on a decimal straddling the round-to-even tie, so a
"nice-decimals" corpus never triggers it and the fuzzer must target the mantissa boundary — a
named generation obligation. **This is the empirical case for a coverage-guided grammar fuzzer
over a curated corpus:** the curated corpus can be *permanently* blind to a real bug (class 4),
and only generation covering the alphabet turns 4/5 into 5/5.

---

## 4. Q3 / Q4 — added-grammar marginal cost, and the in/out-of-contract data file

**Q3 marginal cost.** The added-grammar cost under the digest oracle **alone** is bounded by
the *alphabet*, not the grammar. The fold is six node constructors keyed on the `Value` sum
variant (object/array/string/number/bool/null). For any grammar whose values are that alphabet
— TOML (GN-5.W2, not yet in-tree), CSV, sheets — the marginal fold code is **zero**: the same
constructors fold it, only the fuzzer is re-pointed (a config line). Validated structurally:
both folds switch on `Value` variant / `typeof`, never on a grammar name, so a TOML value tree
folds through the identical `node_hash` with no new arm. A grammar introducing a *new* variant
(CSS dimension, TOML datetime) costs **exactly one** node-hash constructor per side plus one
canonicalization rule if the variant has a non-JSON-expressible residue. **6D's added-grammar
cost is O(new variants); the generator-mirror siblings' is O(new grammar)** — a new
correspondence surface to lint/generate/bind each time. That delta is the measurable form of
the null-hypothesis outcome. A V8-idiomatic optimization (e.g. an ASCII byte-hash skipping
JS-string allocation, as the tape decoder does) costs the oracle **nothing** — only the folded
*value* is gated; a mirror family must re-mirror or exempt every such idiom, 6D never sees it.
The end-to-end confirmation a prototype still owes: fold ≥1 non-JSON grammar's value tree
(TOML is the clean test — its datetime is the one variant that may force a fold extension, and
*that* is the number that upholds or defeats the marginal-cost claim).

**Q4 the contract, as checked-in data.** Authored:
`restart/skinny/tranches/sk-v24/locks/LOCK-6/pass-3/contract-6D.toml`. Partition:

- **IN (digest MUST match Rust↔V8):** the folded value tree; the accept/reject verdict; the
  **error-class** enum (`UnexpectedChar | UnterminatedString | TrailingData | DepthLimit |
  BadEscape | BadNumber`); the number/string/member fold rules; the string length measure
  (the contract pins UTF-16 code units *or* UTF-8 bytes — one choice, versioned; UTF-16 aligns
  with V8-idiom and the tape prover's `codeUnits`, UTF-8 with Rust-idiom).
- **OUT (free per language):** error **message wording** (idiom — the engines never phrase
  alike); error **span byte-offset** (layout — a block scanner may report a boundary vs the
  exact byte); physical member-iteration order (object fold is order-independent); internal
  representation (SoA tape vs native JS objects); module tree and names.
- **Error-class toggle — ON for class equality, OFF for message and span.** The class is
  grammar-derivable and stable across engines (both fail on the same production); message and
  offset are idiom/layout. This *relaxes* the intra-Rust `css_scan_differential.rs:1-24` gate
  ("same STOP, same `None` at EOF, same error offset + message") to **error-class only** across
  languages — a named supersession: message+offset equality is defensible within one codebase,
  not across V8 and Rust. Folding the reject corpus both ways and reporting the pass→fail delta
  is the price of the stricter contract (a prototype number still owed).
- **Canonicalization (spec-closed, corpus-unreachable):** `NaN`→sentinel before the byte fold;
  `-0`→`+0` if the contract elects value-equality over bit-equality (the minimal instance kept
  `-0` bit-distinct — a live choice); `1e400`→inf needs no rule (proven identical). These
  cannot appear in valid JSON, so no corpus reaches them; closed by a compile-time assertion +
  one golden vector each, never a corpus test.

**DAG constraint (binding).** The digest **alphabet is LOCK-1's** grammar-derived `Value` ADT
(`DirectTypeKind` sum variants) — the fold cannot be specified until the variant set freezes.
The reference-of-last-resort **is LOCK-7's** meta-circular parser — a 2-version differential
detects disagreement but cannot adjudicate *which* value is correct when both engines disagree
(or agree while both wrong, R2); LOCK-7 is the N+1th reference. Therefore **6D freezes strictly
after LOCK-1 (alphabet) and LOCK-7 (tie-breaker)**: `LOCK-1 → LOCK-6`, `LOCK-7 → LOCK-6`,
recorded in the contract file's `[dag]` block. Duplicate-key resolution (last-wins / first-wins
/ reject) is likewise a LOCK-7 tie-breaker dependency — the XOR member-fold makes order
invisible but not the *which-key-wins* semantics.

---

## 5. Substrate, seams, risks, null-hypothesis verdict

**Substrate (in-tree, mature — the gap is only cross-language):** `checkasm_parity.rs:1-30`
(FFmpeg dual-run + misalignment sweep + `BBNF_SIMD_INJECT_BUG` sensitivity knob + strict/
non-strict split = the must/may partition); `css_scan_differential.rs:1-24` (clean-room
scalar-vs-kernel differential over every start offset = accept/reject + error boundary drawn
once); `bbnf-bench/src/parity.rs:23` (plane-equality fold — the *layout* differential 6D
deliberately discards); the tape prover's `tape.mjs`/`verify.mjs` fold (the cross-*process*
version, 320 cold children, stable across fresh V8). Rust-fold seam:
`skinny/crates/runtime/src/tape/mod.rs:120-146` or the `Value` enum.

**Substrate changes owed:** (1) **widen the digest to u64** — a u32 `fp` has a ~77k-input
birthday floor; a fuzzer at 10⁶+ inputs will collide and hide a divergence. Carry two u32 lanes
(`Math.imul` pair / native u64); the structural counters remain an independent coarse channel
so escape requires a *double* collision. (2) Home the ~60-LOC fold spec + golden-vector file
with the Value ADT (LOCK-1); the golden vectors are the *only* shared artifact and the only
thing a linter checks.

**Risks / counterexamples (named):** R1 collision hides a real divergence — mitigation: u64 +
counter channel. R2 the oracle proves *agreement*, not *correctness* — two identically-wrong
engines fold matching digests; mitigation: LOCK-7's grammar-defined reference as the N+1th
version (JEST model), which is *why* the freeze DAG puts LOCK-7 first. R3 last-ULP never fires
on a nice-decimal corpus — mitigation demonstrated: the fuzzer must straddle the round-to-even
tie. R4 the NaN/lone-surrogate residue is claimed closed but a spec omits the rule, green
forever until a non-JSON grammar lands and diverges in production — mitigation: compile-time
assertion + golden vector, not a corpus test; the DAG's freeze-after-LOCK-1 exists to pin the
lone-surrogate class (JSONTestSuite `i_string_lone_surrogate` has no UTF-8 encoding —
`Buffer.from` emits `U+FFFD`, a genuine cross-engine edge). R5 the **debuggability tax** — the
one axis where 6D is dominated: a bare root-digest mismatch says "something differs," not
"escape decoder off-by-one at line X," where the mirror siblings localize by structure.
Mitigation, measurable: the structured six-count channel narrows to a kind class + fuzzer
minimization yields a 1–3 byte counterexample; the prototype must report *time-to-root-cause*
under both and compare it against the per-grammar mirror cost 6D saves.

**Null-hypothesis verdict (measured).** 6D wins iff `maintenance(spec + fold×2 + fuzzer) <
maintenance(mirror-linter | spec-skeleton | wasm boundary)`. Two proxies measured this pass:
added-grammar marginal ≈ **0 fold LOC** for an existing-alphabet grammar (§4), and
V8-idiomatic-optimization marginal = **0 gated surface** (§4). Neither proxy defeats the null
hypothesis; on this evidence 6D stands. To *close* it a prototype must still test: (1) the u64
widening under a real fuzzer at 10⁶+ inputs (collision-free); (2) LOCK-7's N+1 reference
flagging a *seeded* both-engines-wrong case (R2); (3) the fold over ≥1 non-JSON grammar's value
tree (TOML/CSV) confirming the zero-marginal claim end-to-end; (4) the debuggability
time-to-root-cause vs mirror-maintenance comparison (R5) — the single measurement on which the
"reasonable point" verdict finally turns.

---

### Prior art (citations)

- **Differential / N-version testing** — implementations cross-referencing as an implicit spec;
  a divergence is a triaged candidate bug (McKeeman, *Differential Testing for Software*, 1998;
  https://en.wikipedia.org/wiki/Differential_testing). **Csmith** (Yang et al., PLDI 2011) is
  the canonical compiler instance.
- **N+1-version differential (reference as tie-breaker)** — JEST tests engines *and* the spec
  together; the model for LOCK-7 as the R2 adjudicator (Park et al., *JEST: N+1-version
  Differential Testing of Both JavaScript Engines and Specification*, ICSE 2021,
  https://arxiv.org/pdf/2102.07498).
- **JSONTestSuite** (N. Seriot, *Parsing JSON is a Minefield*, 2016) — the canonical
  `y_/n_/i_` accept/reject/implementation-defined corpus; the static-corpus substrate; one file
  (`y_string_unicode.json`) already in-tree. **web-platform-tests (WPT)** and Google
  **Wycheproof** — the shared-corpus-driving-independent-implementations model and the
  checked-in test-vector-as-data-file idiom the `contract-6D.toml` mirrors.
- **Canonical serialization** — RFC 8785 (JSON Canonicalization Scheme; number canonicalization
  via ECMAScript Number-to-String) and RFC 8949 §4.2 (CBOR canonical form) — the precedents
  motivating **f64-bits over shortest-decimal** for the number fold. **Ryū** (U. Adams, PLDI
  2018) and Rust `core::num::flt2dec` (Grisu/Dragon) and ECMA-262 `Number::toString` — all
  shortest-round-trip; the reason a secondary decimal oracle can exist but is not the contract.
- **Correctly-rounded float parse** — Rust `str::parse::<f64>()` (Eisel–Lemire + slow fallback,
  round-to-nearest-ties-even; rust-lang/rust #62231) and V8 `StringToDouble` (David-Gay
  strtod), both correctly rounded ⟹ bit-identical f64 for finite decimals — *demonstrated* on
  `1.0000000000000002`/`5e-324` (https://nigeltao.github.io/blog/2020/eisel-lemire.html). This
  makes the number oracle a live guard on bbnf's own fast float path (the row-02 NO_GO
  digit-scan / row-07 Eisel–Lemire materialize tax).
- **checkasm** (FFmpeg) — the in-tree parity harness's model: bit-identical dual run,
  misalignment sweep, deliberate bug injection to prove oracle *sensitivity*.
- **Mutation testing** — `cargo-mutants` (Rust), **Stryker** (JS): the Q2 methodology (score
  the *oracle* by injecting mutants and measuring catch-rate), distinct from mutating the SUT.
- **Coverage-guided grammar fuzzing** — coverage guidance for behavioral diversity +
  differential comparison for semantic inconsistency; the answer to class-4 corpus blindness
  (JITfuzz, ICSE 2023, https://lingming.cs.illinois.edu/publications/icse2023e.pdf).
- **tree-sitter corpus tests** — precedent for grammar-scoped checked-in behavioral corpora.
