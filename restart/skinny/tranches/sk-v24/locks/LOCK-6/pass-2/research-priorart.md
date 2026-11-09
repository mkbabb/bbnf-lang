# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split) · PASS-2 PRIOR-ART RESEARCH

Prior art + in-tree evidence answering the pass-2 steering questions. Constitution-bound:
no crown, no ranking; every question answered with cited art, an in-tree grep, or a measured
number. Where a question names another lock, an adjudication *proposal* is delivered, not a
decree. The extant GN-4/PT-3 codex is the baseline this re-cuts, never silently contradicts.

Evidence base already binding: SPEC §0 rows 14 (tape-in-TS CONFIRMED), 06 (stop-early),
09 (shim-perf REFUTED — the two-arm-asymmetry law), 11 (TS lazy alloc); GN-4.md W4 (the TS
seed, wasm-bindgen export, full-walk documented non-goal); PT-3.md (TS native RegExp, no
bespoke engine); R4-parse-that-ts.md; PROVE-14 REPORT+VERIFY.

---

## Mirror-tree linter (Q1, Q2)

**No off-the-shelf cross-language structural-parity linter exists.** The monorepo-consistency
tools that surface (monorepolint — JS package.json/config consistency; Pants, Please — build
systems) enforce *within-language* config uniformity, not cross-language module-tree
correspondence with a name map [monorepolint; monorepo.tools]. A LOCK-6 mirror linter is
bespoke by necessity — no prior tool to adopt.

**Q1 — the RED-line ceiling.** An *absolute* exemption constant (e.g. "≤15 % hand-idiom") is
arbitrary and indefensible: nothing in the evidence base fixes 15 vs 20. The defensible form
is a **ratchet**, not a constant — the exemption fraction may not *increase* versus the
committed baseline (monotone non-increasing), which can actually go RED the moment drift
grows. This is exactly the shape already law in this repo: the *generated-size budget* precept
(per-tranche line-count budget that blocks the wave on overflow) and coverage-ratchet CI
(codecov "coverage must not decrease"). Justification: **drift is the failure mode, not the
current level** — the gate must fire on the derivative, not the value. A one-time absolute
floor is admissible only as a coarse backstop (e.g. exemption < 100 %, i.e. *something* is
mirrored); the load-bearing RED line is the ratchet.

**Q2 — the identity fork.** For a grammar-derivable "clean" name the two options are genuinely
distinct and only one is non-tautological:
- (b) grammar-derived on *both* sides → the linter compares one generator against itself; the
  check is **tautological** (it can only fail on a generator bug that already breaks both
  sides identically). This is the ts-rs/typeshare condition: generated bindings are
  single-source, so a linter over them proves nothing the generator's own tests don't
  [Aleph-Alpha/ts-rs; docs.rs/specta].
- (a) hand-authored twice (Rust names vs an independent TS authoring) → **real, non-anchored**
  value: the two sources can disagree, so the check bites — but it is not grammar-anchored and
  costs a second authoring.

**Decision:** the non-tautological surface is NOT the generated name correspondence — it is the
**hand-idiom residue** (the bodies the generator does not emit, and the boundary layer). Check
*that*, plus a cheap generated-name presence assertion. Trace evidence: PROVE-14's TS decoder
(`tape.mjs`) and the Rust dumper (`tape_dump.rs`) both derive their 4-plane layout from ONE
Rust `Tape` accessor set (`crates/runtime/src/tape/mod.rs ~121-137`) — one grammar source, two
readings → a byte-exact digest (`fp 986912547`, both sides). A linter asserting "the TS names
match the Rust names" here is tautological; what the digest actually proves is *behavioral*
(node-for-node), i.e. LOCK-6 should lean oracle over name-tree for the generated face.

---

## Spec-generated skeleton (Q3, Q4)

**Q3 — bound the declarative surface (the null hypothesis).** For PROVE-14's certified tape the
declarative surface is *tiny*: four planes (`offsets:u32`, `flag_cursors:u32`,
`flag_values:u8`, `source:bytes`), one classify-on-source-byte rule (no stored
discriminator — kind re-derived from `source[offsets[cursor]]`), a sparse-flag binary search,
and a single flag constant `STRING_NEEDS_DECODE = 0x01` (REPORT §"layout that crossed"). That
is a handful of constants + one decode rule. **A shared constants header + a per-name linter
plausibly beats a generator at this size** — generation earns its cost only on the
higher-leverage face (the decoder *logic* and the oracle tables), not the 4-constant ABI.

*In-tree caveat, confirmed:* the directive flags that prior citations (`materialization.rs`,
a `STRING_NEEDS_DECODE` constant under `crates/`) "did not check out" — **they still do not**.
`grep` finds `STRING_NEEDS_DECODE`/`0x01` only in PROVE-14's REPORT prose and the worktree
`.mjs`; `GRAMMAR_BIT0` and "materialization" live in the CSS/type-plane subsystem
(`crates/core/src/runtime/builder.rs:34`, css tests), a *different* concern. The ts-tape
decoder itself is a prover-worktree artifact (`wf_386900ed-462-14`), not yet in `crates/`.
**So the claimed grammar→layout-fact derivation is NOT yet a real seam** — PROVE-14 hand-wrote
the 4-plane decode. Therefore generation is speculative until that seam exists; the honest
floor today is a hand-authored constants header the linter guards. Generation over-builds if
it targets the ABI; it earns its keep only if it targets the decoder + oracle body.

**Q4 [sharpest cross-lock] — type-generation vs layout-generation are DISTINCT claims.**
Documented prior art on ts-rs/typeshare/specta: *"TypeScript generics, conditional types, or
advanced type inference patterns cannot be expressed via Rust struct derives; generated types
will be simpler than hand-written"* [dev.co/ai/ts-rs]; typeshare's static analysis "results in
a loss of information," and ts-rs cannot even export a type's dependency closure
[Aleph-Alpha/ts-rs]. This bears on **type** generation — and it means PT-E2's `ExtractValue`
type-level inference and the typed combinator algebra (LOCK-8B) are **NOT derivable from Rust**
and must stay hand-authored in TS (R4 §5: the `Parser<any>` leaks are a TS-inference problem,
not a Rust-mirror problem).

It does **NOT** bear on **layout** generation. A shared instruction / tape-layout stream
(LOCK-4C's VM formulation, tied by 8C to 4A/4C) is a **data ABI** — bytes, offsets,
discriminants — not a TypeScript *type*. The conditional-type flattening limit is irrelevant
to it: a byte stream has no conditional types to flatten. PROVE-14 is the proof such layout
crosses byte-exactly (node-for-node across 320 cold children; all 6 structural counts +
codeUnits + fp identical). **Adjudication owed jointly to LOCK-4/LOCK-8:** the
instruction/tape stream (4C) is *generable* (layout, proven crossable); the typed combinator
algebra (8B) is *not generable from Rust* (types, ts-rs limit) and stays hand-authored. Do not
conflate "we can generate the layout" with "we can generate the types" — the first is
PROVE-14-true, the second is ts-rs-false.

---

## Wasm core (Q5, Q6, Q7)

**Q5 — the decision-relevant comparator is wasm-SIMD vs TS-native SCALAR, both
browser-deployable.** A wasm-vs-native-NEON figure is *not* decision-relevant (agreed). Public
art: wasm-SIMD delivers 6–15× over pure JS *for compute-bound, parallelizable* work
[markaicode; dev.to/dataformathub], but the JS↔wasm **boundary crossing** erodes this for
string/data-passing workloads — nickb.dev measures native modules 1.75–2.5× over wasm,
wasm-SIMD 4× *slower* than native at large payloads (128-bit vs AVX2 256-bit), and the
crossover favoring wasm only "at smaller payloads … [where] overhead was less than N-API
calls" [nickb.dev]. The in-house measurement is sharper still: **PROVE-14 shows cold TS-native
scalar tape-walk at PARITY with native `JSON.parse` on full materialization (0.9–0.98×)** —
cold-JIT JS cannot beat the hand-optimized C++ parser on a full walk (REPORT §"Why full-walk
is parity"). So wasm-SIMD is decision-relevant *only if the batch parser materializes more than
`JSON.parse` does or scans structure `JSON.parse` skips*. **Named needs to decide:** comparator
= wasm-SIMD structural-index scan vs a TS-native scalar structural scan (NOT wasm-vs-NEON);
corpus = twitter/citm/canada loaded in-browser; figures = cold throughput MB/s **including**
wasm instantiate + the ArrayBuffer copy across the boundary (PROVE-14 counted the 118 KB
typed-array copy against the tape and still won 4.4× stop-early). **No public benchmark isolates
wasm-SIMD-scan vs TS-scalar-scan for in-browser JSON structural indexing — it must be measured.**

**Q6 — the synchronous-instantiation constraint: REAL but MOVING, do not carry on faith.**
Re-confirmed against current web docs: `new WebAssembly.Module`/`Instance` (synchronous) has a
**4 KB size limit on the main thread** because it blocks the event loop; no limit in Workers;
`instantiateStreaming` (async) is the recommended main-thread path [MDN; web.dev/loading-wasm].
This *would* confine a wasm core below the synchronous combinator plane (parse-that's `parse()`
is sync — R4 §2) — a wasm core needing async instantiation forces top-level-await / a warmup
phase, a Worker, or reliance on the relaxation. **Critical update:** Chrome has *"Intent to
Ship: Deprecate module size limit for `WebAssembly.Module()`"* because V8 lazy compilation
dropped instantiation below 1 s even for the biggest modules [blink-dev]. So the constraint is
a **moving target** — carrying it as a hard F6-bound assumption is risky; flag it as
time-sensitive, re-verify at execution. **One cdylib vs two:** two crates = two `.wasm` fetches
+ two cold instantiations (each pays its own compile); one consolidated cdylib amortizes
instantiation once. Price of splitting ≈ one extra fetch RTT + one extra compile pass per
module; consolidation wins unless the two are loaded on strictly disjoint code paths.

**Q7 [cross-lock → LOCK-9] — the second kernel's dual-maintenance cost re-attributes to
LOCK-9.** A second in-tree kernel (bbnf-simd NEON vs parse-that's scanners) is the *rust↔rust
cross-repo* mirror — LOCK-9's question (9A ONE-KERNEL-LIBRARY unifies the home; 9B keeps them
separate under a no-regression gate). It is **not** LOCK-6's: this plane owns the *rust↔ts
cross-language* mirror. **Interlock proposal:** if LOCK-9 lands 9A (one aarch64 kernel home),
the wasm-SIMD path (Q5) exports over that single home and LOCK-6 inherits one kernel; if 9B,
each repo keeps its kernel and LOCK-6 does not adjudicate their duplication — LOCK-9 does. The
duplication cost attributes to LOCK-9 regardless; LOCK-6 only consumes whichever kernel LOCK-9
blesses for the wasm boundary.

---

## Oracle-isomorphism / digest (Q8, Q9, Q10)

**Q8 — turn both deciders into measured questions; the divergence taxonomy is settled art.**
Cross-Language Differential Testing of JSON Parsers [ACM AsiaCCS 2024, 10.1145/3634737.3657003]
and LLM-based JSON-parser fuzzing [arXiv 2410.21806] give the canonical injected-divergence
classes for this problem-class: **(1) number precision/rounding** (the IEEE-754 hazard — see
Q10), **(2) duplicate-key resolution**, **(3) escape/unicode** (surrogate pairs, `\u`, lone
surrogates), **(4) whitespace/BOM/trailing bytes**, **(5) depth/recursion limits**,
**(6) numeric-literal edge forms** (leading zero, ±0, Inf/NaN, huge exponents). A **strict**
oracle fails on any deviation; practice needs a **statically-filtered/lenient** oracle to
suppress platform-specific intentional variation [r9295 differential-fuzzing-across-languages;
HandWiki]. So (a) — the escape fraction and cycles-to-detection are *empirical* and must be
measured under a coverage-guided grammar fuzzer over the six classes, with a stated
**must-accept / must-reject / may-either** partition (classes 2 and 6 are the classic
*may-either* bucket where RFC 8259 permits implementation choice — those must be *may-either*,
not counted as divergences). (b) — the marginal cost to reach parity on a *new* grammar under
the oracle alone is one differential corpus + one reference impl; the fuzzer's escape rate
bounds the residual risk.

**Q9 — the in/out-of-contract line, as data, with the toggle DECIDED.** IN-contract: parsed
**values** + **accept/reject** verdict. OUT-of-contract: **message wording**, **span bytes**.
The error-**class** toggle: **decide IN-contract.** Rationale — an error-class is a *value-level
fact a caller branches on* (R4 §5: kf's `ResolvedKeyframes.diagnostics`; PT-E1's per-parse
scoped diagnostics consume the class, not the wording), whereas message text is pure idiom.
Making error-class IN means the conformance corpus carries a **shared error-class enum** both
impls must agree on ("number-overflow" vs "unexpected-token"), while wording and span bytes stay
free per language. This is the lenient-oracle boundary drawn precisely: agree on *what kind* of
failure, not *how it is phrased*.

**Q10 — the un-exercisable residue is FLOAT bit-reproducibility; the certified oracle already
sidesteps it.** The divergence neither corpus nor fuzzer can provably reach is the **8-byte
IEEE-754 bit-fold**. Prior art: JS and Rust both use IEEE-754 doubles and both satisfy
`nan!==nan` [pzarycki; Wikipedia IEEE-754], BUT (a) **NaN has 2⁵² non-canonical bit patterns** —
V8 canonicalizes NaN on boxing while **Rust arithmetic can produce a *signaling* NaN**
[rust-lang/rust#107247], and (b) **signed zero**: `-0.0 == +0.0` compares equal but the bits
differ. So a raw 8-byte big-endian IEEE-754 fold does **NOT** fold bit-identically across the
two languages once any NaN or −0 appears — it is a *false-divergence generator*.

**The decisive in-house finding:** PROVE-14's certified digest **never enters this residue**.
Its `fp = 986912547` folds **UTF-16 code units + structural counts**, and numbers are held as
**source-byte offset spans** (the tape stores offsets; values materialize on demand — REPORT
§"layout that crossed"). It bit-folds no decoded double. That is *why* it is bit-reproducible
across the boundary. **Recommendation (binding on any LOCK-6 digest):** fold source-byte number
*spans* — or the ES2019 `Number::toString` canonical decimal (RFC 8785 JSON Canonicalization,
17-sig-fig Ryu/Grisu) [jsonic.io; RFC 8785] — **NEVER raw IEEE-754 bytes.** The residue is
neutralized by never entering it. A digest that ever folds decoded doubles re-imports the
NaN-canonicalization + signed-zero hazard and manufactures false divergences the fuzzer cannot
distinguish from real ones.

---

## Cross-lock adjudications (proposals, not decrees)

**Emitter-derived name map — LOCK-3 derives, LOCK-6 checks.** The grammar-derived half (rule
name → tape discriminant → decoder symbol) is emitter output — it belongs to **LOCK-3's
transposition spine** (grammar→symbol is the emitter's job). LOCK-6 **consumes** it as one side
of the correspondence and owns the *isomorphism check*, not the derivation. Proposal: name map =
LOCK-3 artifact; parity check = LOCK-6. (Unnamed in the specs — this fills the gap without
double-homing the work.)

**Regex exemption — lawful at the RUNTIME layer, single-source at the HIR layer.** R4 §3
confirms the TS runtime uses native JS `RegExp` (`leaf.ts:317-369`, sticky `.test()`
zero-alloc) with **no bespoke TS NFA/DFA** anywhere. LOCK-5's ONE-HIR (5A) unifies the regex
*HIR* on the Rust side — it does not forbid the TS *runtime* crossing to V8-native RegExp, which
is a runtime-engine boundary, not an HIR fork (the same class of finding as PROVE-14's
full-walk: native beats hand-ported cold JS). **Proposal:** the exemption is permanent and
lawful — regex **HIR/grammar** stays single-source (LOCK-5 owns it), regex **execution engine**
is per-language (Rust DFA / TS RegExp), justified by the same native-beats-hand-JS measurement.
Adjudicate this BEFORE any allow-list freezes: the one allow-list entry is "regex execution
engine," measurement-justified, not a blanket mirroring waiver.

**Freeze ordering — LOCK-6 freezes AFTER LOCK-1 and LOCK-7.** The digest alphabet IS LOCK-1's
grammar-derived Value ADT (the sum types authored at GN-3.W1 — the conformance corpus's value
space is undefined until the variant set exists); the reference-of-last-resort IS LOCK-7's
meta-circular oracle (the tie-breaker when two impls disagree and the corpus is silent). **DAG
constraint:** LOCK-1 → LOCK-6 and LOCK-7 → LOCK-6. This matches the extant SPEC ordering
(GN-3 type plane → GN-4 value fabric → GN-4.W4/W5 the TS seam + four-way parity oracle), so the
plane's freeze does not contradict the codex baseline (constitution ix).

---

## Extant-baseline consciousness (do-not-contradict)

GN-4.md W4 already cuts the bbnf face of this plane: wasm-bindgen export of the four tape
planes, a **born-RED wasm-plane-drift cross-check** (planes byte-identical to the `tape_dump`
fixture, not merely JSON-parity — R4-guard against endianness/stride mis-alignment), and the
**full-walk documented as a measured non-goal** (~0.9×, not hidden). PT-3.md fixes the TS
combinator backbone (native RegExp, PT-E1/E2, V8 hot-shape audit). LOCK-6's verdict must weave
these, naming any divergence as a supersession — it may not silently re-open the full-walk goal
(SPEC §0/14 refutation) nor the shim-perf claim (SPEC §0/09 law: two-arm asymmetry is the
classic self-deception; paired interleaving mandatory even for "obvious" wins).

---

## Sources

- Aleph-Alpha/ts-rs (github.com/Aleph-Alpha/ts-rs); docs.rs/specta; dev.co/ai/coding-agents/ts-rs — Rust→TS type gen + conditional-type flattening limit.
- nickb.dev/blog/the-webassembly-value-proposition-is-write-once-not-performance — wasm-vs-JS/native boundary-crossing erosion, 128-bit SIMD ceiling.
- markaicode.com/webassembly-4-simd-…; dev.to/dataformathub/rust-webassembly-2025 — wasm-SIMD 6–15× over pure JS (compute-bound).
- MDN WebAssembly.instantiate / Instance; web.dev/articles/loading-wasm; blink-dev "Deprecate module size limit for WebAssembly.Module()" — 4 KB sync limit, Worker exemption, deprecation intent.
- ACM AsiaCCS 2024, Cross-Language Differential Testing of JSON Parsers (10.1145/3634737.3657003); arXiv 2410.21806 (LLM JSON-parser fuzzing); r9295.github.io differential-fuzzing-across-languages — divergence classes, strict-vs-lenient oracle.
- rust-lang/rust#107247 (signaling NaN); pzarycki.com js-nan; en.wikipedia.org/wiki/IEEE_754 — NaN/−0 bit non-reproducibility.
- jsonic.io/guides/json-canonicalization (RFC 8785) — ES2019 canonical-decimal number folding.
- On-Demand JSON (Keiser & Lemire, Wiley SPE 2024 / arXiv 2312.17149); simdjson_nodejs — lazy/stop-early parse vs JSON.parse.
- In-tree: PROVE-14 REPORT+VERIFY; SPEC §0 rows 06/09/11/14; GN-4.md W4; PT-3.md; R4-parse-that-ts.md; grep of `crates/` for STRING_NEEDS_DECODE/GRAMMAR_BIT0/materialization.
