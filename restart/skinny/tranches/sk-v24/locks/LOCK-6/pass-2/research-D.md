# LOCK-6 · Family 6D — ORACLE-ISOMORPHISM / DIGEST (pass-2 research)

Behavioral isomorphism only: a shared conformance corpus and a differential digest
decide correspondence between the Rust core and the TypeScript backbone; layout is free
per language. The family is a stated null hypothesis — it must be *defeated* by measured
maintenance / drift cost, or it wins. This report makes the mechanism concrete, prices its
substrate, names the seams it lands on, and states exactly what a prototype must measure.

---

## 1. The mechanism, made concrete

**The digest is the oracle.** A digest is a small scalar (u32 today, u64 recommended —
§6) folded deterministically over the *value tree* a parse yields. Two implementations are
behaviorally isomorphic on an input iff they fold the identical digest. The fold already
exists in prototype and is the concrete substrate to generalize: `numHash` /
`strHash` / `nodeNumber` / `memberMix` / `arrayStep` /
`finalizeDigest` at `crates/bbnf-bench/ts-tape/tape.mjs:34-68` (FNV-1a string fold,
Murmur-style number mix, order-independent XOR member fold with count salt). The structural
counters (`objects/arrays/strings/numbers/bools/nulls/codeUnits/fp`,
`tape.mjs:70-83`) are a *coarse* pre-digest; `fp` is the fine one. Both are compared as
plain integers (`verify.mjs:25-41`).

**What the fold buys.** It collapses "did two whole value trees materialize identically?"
into one `u32 == u32`. That is the entire value proposition of 6D over the sibling families:
no mirrored module tree to lint, no shared skeleton to generate, no wasm boundary — just
run both engines over a corpus and compare a number. The correspondence surface a linter or
a generator would police is replaced by a runtime differential whose granularity is the
node, not the file.

**The strongest form of 6D.** Three refinements lift it from "diff-test the JSON walk" to a
load-bearing plane:

1. **One digest specification, folded twice.** The fold algorithm is *specified once*
   (constants, mix functions, endianness, canonicalization rules) and each language
   implements that spec over its *own native value*. This is the only shared artifact —
   ~60 LOC of arithmetic — and it is the thing that must not drift. Crucially it is folded
   over the *decoded value* (the LOCK-1 Value ADT), not over bytes, so it certifies
   semantic materialization, not layout.

2. **The alphabet is the grammar-derived Value ADT.** Object / Array / String / Number /
   Bool / Null are exactly `DirectTypeKind` sum variants (GN-3.W1) and the `Value` enum
   of `crates/runtime/src/value/mod.rs` (GN-4.W5). One node-hash constructor per variant;
   new variants (a grammar that admits dates, or CSS dimensions) extend the alphabet, not
   the transport. The digest is therefore *grammar-total by construction*: every `->`
   projection that reaches the value surface reaches the fold.

3. **The corpus is coverage-generated, not curated.** A grammar-driven fuzzer (the same
   bootstrapped grammar, run as a generator) plus the static canon (`twitter/citm/canada`,
   the four css-l4 corpora, sheets) is the input stream; every input is folded on both
   sides and the two scalars compared. Divergence is a candidate bug; the triage partition
   (§4) sorts benign from real.

This is the classic **N-version differential oracle** — multiple implementations cross-
referencing as an implicit specification, no hand-built golden output — instantiated over a
value digest rather than over program output.

---

## 2. The substrate (what already exists to build on)

The family does **not** start from zero. The in-tree differential machinery is mature; it
is simply *intra-Rust* (scalar reference ↔ SIMD candidate) rather than *cross-language*.

- **`checkasm_parity.rs`** (`crates/bbnf-simd/tests/checkasm_parity.rs:1-30`) is FFmpeg's
  `checkasm` model verbatim: run a primitive twice on bit-identical inputs, `memcmp` the
  halves, sweep every misalignment `0..64`, stack-canary + `SIGSEGV/SIGBUS/SIGILL`
  longjmp trampoline, and a **deliberate-bug injection knob** (`BBNF_SIMD_INJECT_BUG=1`
  xors the first mask byte so the oracle's own sensitivity is provable). It carries a
  strict / non-strict split (`BBNF_SIMD_STRICT`) — record-and-log vs fail — which is the
  exact "may-either vs must" partition 6D needs (§4).
- **`css_scan_differential.rs`** (`crates/runtime/tests/css_scan_differential.rs:1-24`)
  is the clean-room-reference pattern: an independent fully-scalar recognizer vs the
  production kernel, swept over *every start position* of the real corpora, asserting the
  same STOP hit, the same `None` at EOF, **and the same error offset + message**. That is
  precisely the in-contract / out-of-contract boundary Q9 asks about, already drawn once.
- **`parity.rs`** (`crates/bbnf-bench/src/parity.rs:23-...`) folds parity to plane
  equality (`offsets()` / `flag_cursors()` / `flag_values()`) plus a payload-counter
  invariant — a layout differential, the complement 6D deliberately discards.
- **PROVE-14's digest** (`tape.mjs`, `verify.mjs`) is the cross-*process* version:
  320 cold children, tape-arm vs JSON.parse-arm, `fp 986912547` identical, structural
  counts identical. It proves the fold is stable across fresh V8 instantiations.

The gap is narrow and nameable: **every one of these folds runs inside one language.**
PROVE-14's two "arms" are *both JavaScript* — the tape arm decodes number bytes with V8's
own `Number(src.toString("latin1", …))` (`tape.mjs:181`) and the reference arm uses
`JSON.parse`; the Rust dumper `tape_dump.rs` emits only the four raw planes and folds
**no digest at all** (`patch.diff:45-82` writes `offsets/flag_cursors/flag_values/source`;
the only Rust byte-fold is `v.to_le_bytes()` for the u32 plane serializer, `patch.diff:76`).
So the artifact that is missing — and that a prototype must build — is a **Rust-side fold
of the same spec**, compared against the TS fold. Until that exists, "the digest crosses
languages" is proven for *layout* and for *JS-internal consistency*, not for
Rust↔TS value-fold bit-identity.

---

## 3. Q10 — the un-exercisable residue and the IEEE-754 bit-fold

Q10 is the family's sharpest question and its answer is largely favorable, with one exactly
bounded residue.

**The float fold, as built, is little-endian, not big-endian.** `numHash`
(`tape.mjs:42-48`) does `DataView.setFloat64(0, n, /*littleEndian=*/true)` then folds the
two LE u32 halves. Endianness inside the fold is immaterial to correctness *provided both
sides agree*; aarch64 is natively little-endian and the Rust mirror is `f64::to_le_bytes()`,
so LE is the right, portable choice. The "big-endian" framing in the question does not match
the code and would itself be a drift bug if a naive Rust mirror used `to_be_bytes()`.

**Do two independent parsers fold bit-identical f64 over a corpus? For valid JSON: yes, by
the correctly-rounded guarantee.** Rust's `str::parse::<f64>()` implements Eisel–Lemire with
a slow-path fallback and is **correctly rounded** — round-to-nearest, ties-to-even — since
the fast-float adoption. V8's `StringToDouble` is likewise correctly rounded. Correct
rounding is a *unique* function of the decimal string: two correctly-rounded parsers return
the bit-identical f64 for every finite in-range decimal. Therefore over any corpus of valid
JSON numbers the 8-byte fold is bit-identical across the language boundary — **the number
lane's cross-language digest parity holds by construction, not by luck**, and does not need
a corpus to be believed (though the prototype must still *exercise* it, because PROVE-14
never did — both its arms parse with V8).

**The residue — where drift can hide that neither corpus nor fuzzer can provably reach —
is exactly the set of f64 values JSON cannot express:**

- **NaN.** JSON has no NaN literal, so NaN never enters through a JSON parse; but if the
  digest alphabet is the *general* LOCK-1 Value ADT (a grammar that computes, or admits
  `NaN`/`Infinity` extensions, or a non-JSON numeric grammar), NaN has 2⁵²−1 quiet payloads
  plus the signalling set. aarch64 default-NaN is `0x7FF8000000000000`; V8 canonicalizes
  arithmetic NaN to the same quiet pattern but a *bit-preserved* NaN carried through a
  typed-array can differ; Rust `f64::NAN` is one specific pattern and bit-ops can produce
  others. A raw 8-byte fold over NaN is therefore **not** guaranteed bit-identical. Fix:
  the digest spec **must canonicalize** — fold a fixed sentinel for `is_nan()` on both
  sides before the byte fold. This is a one-line rule that must be in the spec, and its
  absence is a silent-divergence class a corpus cannot surface (JSON never emits NaN).
- **Signed zero.** `-0.0` and `+0.0` differ only in the sign bit. Both Rust
  `"-0".parse::<f64>()` and V8 `Number("-0")` yield `-0.0` (sign set), so the raw fold is
  *consistent* across languages — but it makes the digest distinguish `-0` from `0`, which
  may or may not be the intended value semantics (JSON `==` treats them equal). This is a
  **policy decision the spec must state**, not a bug: fold-raw (distinguish) or
  normalize-zero (`if x == 0.0 { fold +0.0 }`). Either is defensible; leaving it unstated is
  the drift.
- **±Infinity, subnormals.** Both parsers overflow to `±inf` identically and round
  subnormals correctly; no residue.

Net: the bit-fold is **safe and total over valid JSON** and needs only two spec rules
(NaN-canonicalize, decide signed-zero) to be safe over the general Value ADT. The
un-exercisable residue is entirely the non-JSON-expressible float set, and it is closed by
spec, not by test — which is the honest characterization Q10 demands.

---

## 4. Q8 / Q9 — turning the deciders into measured questions

**Q8(a) — escape fraction and cycles-to-detection.** The injected-divergence classes, and
the partition, are directly modeled on the in-tree bug-injection knob
(`checkasm_parity.rs:16`, `BBNF_SIMD_INJECT_BUG`). The prototype seeds each class into one
side and measures (i) fraction that escape a fixed corpus + a coverage-guided grammar
fuzzer, (ii) inputs-to-first-divergence:

| Divergence class | Injection | Corpus-reachable? |
|---|---|---|
| Structural miscount (drop/dupe a node) | skip one `arrayStep` | yes, ~1 input |
| String decode (escape handling: `\uXXXX`, surrogate pair, overlong) | mis-decode one escape | yes, but needs the escape in-corpus (312 flagged strings in twitter) |
| Number rounding (last-ULP) | truncate before Eisel–Lemire fallback | **only if the fuzzer emits a halfway-case decimal** — coverage-guided generation over the mantissa boundary is required |
| NaN / signed-zero | raw-fold without canonicalization | **unreachable by JSON corpus** — see §3 residue |
| Order-dependence (object member order) | replace XOR member fold with sequential | reachable iff the corpus has duplicate-key or reordered objects |

The must-accept / must-reject / may-either partition maps onto the strict flag
(`BBNF_SIMD_STRICT`): **must-accept** = valid-JSON conformance corpus, digest must match
(RED on mismatch); **must-reject** = malformed inputs, both engines must error at the same
class (Q9); **may-either** = the implementation-defined set (duplicate-key resolution
order, number canonical form, error *message wording*) — recorded, not gated. The number to
report is the **escape fraction of injected divergences that survive both corpus and fuzzer**
— a budget that goes RED only when a *reachable* class escapes; the unreachable classes (§3)
are closed by spec and excluded from the fraction, else the budget can never be green.

**Q8(b) — marginal cost on a *new* grammar under the oracle alone.** This is where 6D beats
the generator families. Adding a grammar (TOML, GN-5.W2) to a mirror-tree or a spec-skeleton
family costs a new correspondence surface to lint/generate. Under 6D it costs: one node-hash
constructor per *new* Value variant (zero if the grammar reuses object/array/string/number/
bool/null) + wiring the fuzzer to the new grammar. For a grammar whose values are the
existing alphabet (TOML, CSV, sheets all are), the marginal oracle cost is **≈0 new fold
code** — the same six constructors fold it. That near-zero marginal cost *is* the measurable
defeat-or-win of the null hypothesis (§7).

**Q9 — the in/out-of-contract line, decided.** In-contract (gated, must match): the
folded **values** and the accept/reject verdict — `css_scan_differential.rs` already gates
"same STOP hit, same `None` at EOF." Out-of-contract (free per language): **error message
wording and span byte offsets** — V8 and Rust will never phrase a message identically and
should not try. The **decided toggle** for error-class: gate the *error class* (a small
enum — UnexpectedChar / UnterminatedString / TrailingData / DepthLimit), **not** the
message and **not** the byte offset. Rationale: the class is grammar-derivable and stable
across languages (both engines fail on the same production); the offset is layout (a scanner
may report the block boundary vs the exact byte) and the message is idiom. So the toggle is
**decided ON for error-class equality, OFF for message and span** — the same split
`css_scan_differential` draws for scans, lifted to the value oracle. Note this contradicts
`css_scan_differential.rs:6`'s intra-Rust "same error offset + message"; that is defensible
*within one language* (one codebase, one message table) but must **relax to error-class only
across languages** — a named supersession the plane must record.

---

## 5. Cross-lock adjudications (proposals, not decrees)

**Emitter-derived name map — this plane vs LOCK-3's transposition spine.** A digest-only
family needs *no* name map: there is no mirrored tree whose names must correspond, because
correspondence is proven at runtime by the fold, not statically by names. The grammar-derived
half (Value-variant → node-hash constructor) is a tiny table (~6 entries) that belongs with
the **alphabet definition = LOCK-1's Value ADT**, not with LOCK-3. Proposal: if 6D is chosen,
the name-map question dissolves; if a sibling mirror/skeleton family is chosen, the map homes
in LOCK-3's spine (it is an emission product). Either way it is not 6D's to own.

**Regex exemption (Rust-DFA ↔ TS-`RegExp`).** 6D reframes this from a *permanent mirroring
exemption* to a *behavioral conformance obligation*: the two regex engines need not share
layout or even algorithm (`RegExp` vs the bootstrapped DFA), they need only fold identical
match results (matched span, capture set, accept/reject) over a shared regex conformance
corpus. This is *compatible* with a LOCK-5 ONE-HIR verdict (one HIR, two automata backends)
and does not require an allow-list freeze — the digest oracle is the allow-list, evaluated
continuously. Proposal owed jointly to LOCK-5: 6D removes the need for a static regex-mirror
exemption by making regex isomorphism a differential-corpus obligation.

**Freeze-ordering DAG.** The dependency is real and one-directional: (i) the digest
**alphabet IS LOCK-1's grammar-derived Value ADT** — the fold cannot be specified until the
variant set is frozen (a new sum variant is a new node-hash constructor); (ii) the
**reference-of-last-resort IS LOCK-7's meta-circular oracle** — when Rust and TS disagree and
neither is obviously right, the bbnf-in-bbnf / grammar-defined reference parser (7D) is the
tie-breaker that says which value is *correct*, not merely which two agree. Therefore this
plane **freezes strictly after LOCK-1 (alphabet) and LOCK-7 (tie-breaker)**: `LOCK-1 →
LOCK-6`, `LOCK-7 → LOCK-6` in the freeze DAG. A differential oracle with no ground-truth
reference can only detect disagreement, not adjudicate it; LOCK-7 supplies the adjudicator,
so 6D's close gate depends on 7D's existence.

**Q4 cross-lock (type-gen flattening vs layout-gen).** ts-rs / typeshare / specta flatten
conditional *types*; that bears on a spec-skeleton family (Q3/Q4's sibling), **not** on 6D —
a digest oracle generates nothing, so there is no conditional-type surface to flatten. The
two claims are distinct: type-generation flattening is a *skeleton* concern (owed to
LOCK-8C / LOCK-4C's shared-instruction stream); the behavioral digest is layout-agnostic and
inherits none of it. 6D's adjudication: the flattening critique does not touch this family;
it is a cost the generator families carry and 6D does not.

---

## 6. Substrate recommendations (concrete)

- **Widen the digest to u64.** The u32 `fp` (`tape.mjs:39`) has a ~1/4B collision floor; a
  coverage fuzzer emitting millions of inputs will hit a birthday collision (~77k inputs for
  50% on u32) and a colliding divergence escapes silently. Fold to u64 (two u32 lanes carried
  separately, or `Math.imul`-pair on the TS side, `u64` native on Rust). This is the single
  most load-bearing substrate change; the counters (`objects/…/codeUnits`) stay as a coarse
  cross-check.
- **Specify canonicalization in the fold spec:** NaN→sentinel, signed-zero policy (§3),
  UTF-8 vs UTF-16 code-unit counting (PROVE-14 counts UTF-16 code units, `codeUnits 304329`;
  a Rust fold must count the *same* units or normalize to code points — a real divergence
  class for non-BMP text, exercised by the twitter Japanese/emoji strings).
- **Home the fold spec with the Value ADT** (LOCK-1) as ~60 LOC of arithmetic + a golden
  vector file; both engines implement against the golden vectors. The golden-vector file is
  the *only* shared artifact and the only thing a linter (if any) need check.

---

## 7. Named risks, counterexamples, and the null-hypothesis test

- **R1 — the digest hides a real divergence behind a collision.** Counterexample: two value
  trees differing in one leaf fold to the same u32. Mitigation: u64 (§6) + the structural
  counters as an independent coarse channel; a divergence must collide in *both* to escape.
- **R2 — the oracle proves agreement, not correctness.** Two engines can be identically
  wrong (both mis-handle a surrogate pair) and the digest matches. This is the intrinsic
  limit of N-version differential testing. Mitigation: LOCK-7's meta-circular reference as
  the third version (N+1-version differential, the JEST model) — the grammar-defined oracle
  breaks ties. This is *why* the freeze DAG puts LOCK-7 before LOCK-6.
- **R3 — number rounding drift never fires because the corpus lacks halfway cases.**
  Counterexample: a hand corpus of "nice" decimals; the last-ULP class (§4) is
  fuzzer-only-reachable. Mitigation: the coverage-guided generator must target the mantissa
  boundary (emit `d.dddde±ee` strings straddling the round-to-even tie) — a named fuzzer
  obligation, not an assumption.
- **R4 — the residue is claimed closed but the spec omits a rule.** Counterexample: the
  Rust mirror folds raw NaN bits; the class is unreachable by JSON so CI is green forever,
  then a non-JSON grammar (or a computed value) lands and silently diverges in production.
  Mitigation: NaN-canonicalization is a *compile-time* assertion in the fold, not a test —
  and a golden vector containing a NaN sentinel exercises it even though no grammar emits one.
- **The null-hypothesis defeat/win, as data.** 6D wins iff `maintenance(digest spec + fold ×2
  + fuzzer) < maintenance(mirror-tree linter | spec skeleton | wasm boundary)`. The
  measurable proxy: **marginal cost of adding a grammar** (§4b: ≈0 fold LOC for an
  existing-alphabet grammar vs a new correspondence surface for the sibling families) and
  **marginal cost of a V8-idiomatic optimization** (a TS-native fast path — e.g. PROVE-14's
  byte-hash ASCII fold, `tape.mjs:130-149` — costs 6D *nothing*, because only the folded
  value is gated, whereas a mirror family must re-mirror the idiom or exempt it). Those two
  marginals are the numbers the prototype reports; if they are small, 6D is not defeated.

---

## 8. What a prototype must test to move this family

1. **Build the missing Rust fold.** Implement the `tape.mjs:34-68` spec in Rust over the
   `Value` enum (or the tape view `crates/runtime/src/tape/mod.rs:121-137`), fold
   `twitter/citm/canada`, and assert the Rust u64 digest **equals** the TS u64 digest node-
   for-node. This is the artifact PROVE-14 never produced (its two arms are both JS) and is
   the whole cross-language claim. **Gate: byte-exact digest equality, Rust ↔ TS, over the
   three corpora, including the 312 escaped/non-BMP twitter strings.**
2. **Exercise the float residue explicitly.** Feed halfway-case decimals, subnormals, `1e400`
   (→inf), `-0`, and a NaN sentinel through both folds; confirm bit-identity for finite JSON
   and confirm the canonicalization rules fire for NaN/±0. **Gate: no divergence on valid
   JSON; NaN/±0 behave per the decided spec policy.**
3. **Measure the injected-divergence escape fraction and cycles-to-detection** (§4 table)
   under the coverage-guided grammar fuzzer + static corpus, using the
   `BBNF_SIMD_INJECT_BUG`-style knob. **Report: fraction of *reachable* classes that escape
   (target 0), inputs-to-first-divergence per class.**
4. **Measure the two null-hypothesis marginals** (§7): LOC to add a new-alphabet grammar to
   the oracle (target ≈0) and LOC cost of a V8-native fast path under the oracle (target 0
   gated surface). **Report both as the defeat/win evidence.**
5. **Prove the N+1 tie-breaker.** Wire LOCK-7's grammar-defined reference as a third fold and
   confirm it adjudicates a *seeded* both-engines-wrong case (R2). **Gate: the reference
   flags the identical-but-wrong divergence the two production engines hide.**

The family's concrete deliverable is thus a ~60-LOC fold specification with golden vectors,
one Rust fold, one TS fold, a coverage-guided grammar fuzzer feeding both, and a u64
differential — landing on the existing `checkasm` / `css_scan_differential` /
`parity.rs` substrate, homed after LOCK-1's alphabet and LOCK-7's reference in the freeze DAG.

---

### Prior art (citations)

- **N-version / differential oracle** — the digest-diff IS differential testing: multiple
  implementations as cross-referencing oracles, no hand-built golden output; divergence is a
  candidate bug subject to triage. (Wikipedia, *Differential testing*;
  *Differential Fuzzers*, emergentmind.com.)
- **N+1-version differential (reference as tie-breaker)** — JEST tests JS engines *and* the
  spec together, using the spec as the extra version; the model for LOCK-7 as the R2
  adjudicator. (Park et al., *JEST: N+1-version Differential Testing*, arXiv 2102.07498.)
- **checkasm** — the in-tree `checkasm_parity.rs` is FFmpeg's `tests/checkasm/` model:
  bit-identical dual run, misalignment sweep, deliberate bug injection to prove oracle
  sensitivity. (FFmpeg checkasm.)
- **Correctly-rounded float parse** — Rust `str::parse::<f64>()` uses Eisel–Lemire (from
  fast-float-rust) with a slow fallback, round-to-nearest-ties-to-even; V8 `StringToDouble`
  is likewise correctly rounded ⟹ bit-identical f64 for finite decimals. (Lemire/Eisel
  ParseNumberF64, Nigel Tao, nigeltao.github.io/blog/2020/eisel-lemire.html; rust-lang/rust
  #62231 float rounding semantics.)
- **Coverage-guided grammar fuzzing** — coverage guidance for diverse behavior + differential
  comparison for semantic inconsistency; grammar-based generation over seeds. (JITfuzz,
  ICSE 2023; *Differential testing*, HandWiki.)

Sources: [Differential testing — Wikipedia](https://en.wikipedia.org/wiki/Differential_testing) ·
[Differential Fuzzers](https://www.emergentmind.com/topics/differential-fuzzers) ·
[JEST N+1-version Differential Testing](https://arxiv.org/pdf/2102.07498) ·
[Eisel-Lemire ParseNumberF64 — Nigel Tao](https://nigeltao.github.io/blog/2020/eisel-lemire.html) ·
[rust-lang/rust #62231 float rounding](https://github.com/rust-lang/rust/issues/62231) ·
[JITfuzz coverage-guided fuzzing](https://lingming.cs.illinois.edu/publications/icse2023e.pdf)
