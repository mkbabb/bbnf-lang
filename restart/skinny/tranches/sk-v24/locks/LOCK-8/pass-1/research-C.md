# LOCK-8 family 8C — THE ISOMORPHIC MIRROR (TS plane as lawful mirror of the Rust core)

The TypeScript backbone is not a re-implementation to be kept honest by discipline; it is one
face of a two-language artifact whose correspondence can be *machine-checked* rather than
asserted. This family develops the strongest form of that mirror: a **stratified
correspondence** where each stratum is enforced by the tightest mechanism it admits — byte
oracles where layout is shared, generation where a single spec suffices, a tree linter where
only naming and placement correspond — and hand-written TS survives only where V8 measurably
beats a generated shape. The design consumes LOCK-6's verdict on *where* isomorphism ends and
supplies LOCK-6 the parse-that face of that answer; it ties LOCK-4A (the parser graph) and
LOCK-4C (a shared instruction vocabulary) at their TS boundary.

The governing empirical fact is already banked: **SPEC §0 row 14 (CONFIRMED)** — the skinny
offset-tape SoA layout decodes *byte-exact, node-for-node* in TS across 320 cold child
processes, stop-early access 4.4–4.6× over `JSON.parse`, full-walk ~0.91× (native parse won,
measured). This is the single hardest isomorphism proof in the campaign, and it dictates the
family's shape: the mirror is strongest at the **data plane** and must *not* over-reach into a
wasm-core-everything design that the same measurement refutes.

---

## 1. The substrate that already exists (census, not aspiration)

### 1.1 The mirror tree is real but partial and divergent

`parse-that/rust/parse_that/src/` is a file-for-file structural mirror of
`parse-that/typescript/src/parse/`:

| concern | TS (`typescript/src/parse/`) | Rust (`rust/parse_that/src/`) | correspondence |
|---|---|---|---|
| parser core | `parser.ts:24-32` (`Parser<T>` class, 711 LOC) | `parse.rs:61` (`Parser<'a,O>` struct, SmallBox) | **shape-divergent** (see §1.3) |
| state | `state.ts:36-53` (8 fields incl `value:T`) | `state.rs:202` (12 fields incl `ws_bitmap` u64) | **field-divergent** (transient; oracle-only) |
| leaf | `leaf.ts` (399) | `leaf.rs` (370) + `scanners.rs` (370) | name-map + engine-divergent |
| lazy | `lazy.ts:43` | `lazy.rs` | corresponds |
| debug | `debug.ts:383` | `debug.rs:372` | corresponds |
| split | `split.ts:58` | `split.rs:69` | corresponds |
| combinator methods | inline on `Parser` (`parser.ts:81-707`) | `combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,recover}.rs` | **Rust is a directory module; TS is one file** |
| graph/enum form | *(none)* | `span_parser/{mod,leaves,constructors,methods,combinators,sep_by}.rs` — enum-dispatched, **vtable-free** | **Rust-only; no TS counterpart** |

The name-map is non-trivial and un-recorded: TS `any`↔Rust `one_of` (`leaf.rs:298`); TS
`dispatch`↔Rust `dispatch_byte`/`dispatch_byte_multi` (`leaf.rs:315,340`); TS `all`/`fuseAll`↔
Rust `then`-chain + the `seq!`/`alt!` macros (`combinators/macros.rs`); TS `.then/.or/.many/.map`
↔ `combinators/methods/{sequence,alternation,repetition,map}.rs`. **No checked-in artifact
records this correspondence**, so the mirror is currently maintained by memory — precisely the
drift surface a linter kills.

### 1.2 The banked isomorphism proof (data plane)

PROVE-14 built the crossing and it is SPEC-certified. The shared layout is Structure-of-Arrays,
offset-only, no stored discriminator (kind re-derived from `source[offsets[cursor]]`), flags a
sparse sorted side-channel:

```
offsets.u32       118,292 B   29,573 tokens (u32 LE src byte-offset each)
flag_cursors.u32    1,248 B      312 escaped-string cursors (sorted)
flag_values.u8        312 B      STRING_NEEDS_DECODE = 0x01 (GRAMMAR_BIT0)
source.bin        631,515 B
payload arena           0 B      (offset-only invariant — proven empty)
```

Artifacts (in the PROVE-14 worktree `crates/bbnf-bench/ts-tape/`): Rust dumper
`crates/bbnf-bench/src/bin/tape_dump.rs` (78 LOC); TS decoder `tape.mjs` (364, `TapeDoc`
monomorphic decoder), `load.mjs` (34, planes→typed arrays), `pointer.mjs` (34, stop-early
`tapeGet`/`jsonGet`), `verify.mjs` (55, **the differential oracle** — full+partial digest
parity, payload/flag cross-checks, 32-bit fingerprint + 6 structural counts), `bench.mjs` (86,
N cold children paired). The offset-only invariant is enforced Rust-side by the tape builder's
`generated.rs consume_delimiter` (separators advance without emit). VERIFY re-measured
CONFIRMED (stop-early 4.44–4.56×, full-walk 0.89–0.92×).

The SPEC-sanctioned graduation seam is **GN-4.W4** (SPEC §0 ~line 137): "wasm-bindgen export of
the four public tape planes; stop-early get in TS ships (4.4× proven); full-walk lane documented
as non-goal (native parse won, measured)."

### 1.3 The three structural divergences that threaten the mirror claim

1. **Two parser representations Rust-side, one TS-side.** Rust carries both the closure-boxed
   `Parser<'a,O>` (`parse.rs:61`, `SmallBox<dyn ParserFn, S32>`) *and* the enum-dispatched
   vtable-free `SpanParser` (`span_parser/`). The SpanParser is exactly the *analyzable graph*
   LOCK-4A posits and the *instruction vocabulary* LOCK-4C posits — but it has **no TS
   counterpart**. TS has only the closure core, whose central `this.parser(state)` indirect call
   (`parser.ts:14-16`, `ParserFunction<T>`; R4 §6c) is *the* megamorphic IC of the engine.
2. **Regex engine is not isomorphic.** Rust `regex()` compiles to the bespoke DFA
   (`leaf.rs:85`, `crate::regex::dfa::Dfa` via `cached_dfa`); TS `regex()` drives native V8
   `RegExp` sticky-`test()` (`leaf.ts:317-369`). Same *behavior*, unrelated *machinery*. LOCK-5
   owns the verdict; 8C must classify this seam as **oracle-only, layout-exempt**.
3. **Diagnostics gating model differs.** Rust erases diagnostics at *compile time* via
   `#[cfg(feature = "diagnostics")]` (`parse.rs:129`, `leaf.rs:40-48`) — true zero cost, the
   constitution's clause-v realized. TS gates at *runtime* via the module-global
   `diagnosticsEnabled` latch (`utils.ts:6`), which PT-3 W2 dissolves into per-parse scope.
   These are *different mechanisms* for the same semantics — the mirror here is behavioral, not
   structural. `ParserState` fields likewise diverge (Rust `ws_bitmap` u64 cache `state.rs:241`,
   `context_ptr`, SmallVec inline storage; TS `value:T`, `id`) — but `ParserState` is transient
   per-parse scratch that never crosses the language boundary, so it is *correctly* oracle-only.

---

## 2. The mechanism made concrete — a three-stratum mirror + a linter

The family's thesis: **match the enforcement to the correspondence.** Do not demand layout
isomorphism where only behavior is shared (state, regex); do not settle for behavioral oracles
where a single spec can *generate* both sides (tape, node vocabulary); and record the residual
hand-maintained correspondence in a linted name-map so drift is impossible silently.

### Stratum A — DATA (byte-exact, single-spec-generated both sides)

The tape SoA layout becomes **one machine-readable schema** (grammar-driven per constitution
clause i: the plane set, widths, endianness, the sparse-flag encoding, the offset-only
invariant). A single generator emits *both* the Rust dumper/builder and the TS `TapeDoc`
decoder from that schema — `tape.mjs`/`load.mjs` cease to be hand-written and become generated
artifacts under the clean-regen discipline (regen∘regen = regen, byte-identical). This is where
"generated skeletons, hand-written TS minimized" bites hardest and where isomorphism is
*provable* byte-for-byte, not argued. The wasm-bindgen export of the four planes (GN-4.W4 seam)
is the crossing; TS decodes with a monomorphic native decoder (no wasm on the *read* path — the
read is typed-array indexing, which is where V8 wins).

Enforcement: PROVE-14's `verify.mjs` fingerprint (32-bit + 6 structural counts) is the gate;
generated decoder must pass it *unchanged* against the Rust dump.

### Stratum B — SHAPE (shared node vocabulary, generated enum/table, hand bodies)

The parser **node-kind vocabulary** — Rust's `SpanKind` leaves (`span_parser/leaves.rs`:
`StringLiteral`, `RegexMatch`, `Eof`, plus `Sequence`/`Alternation`/`SepBy` combinators) — is a
shared spec. From it, a generator emits *both* the Rust enum + dispatch router
(`span_parser/mod.rs`) and a TS dispatch surface. **Crucially, the TS shape is not a naive
discriminated-union mirror** — a large tagged union re-introduces exactly the megamorphic IC the
hand code fights (R4 §6c). The generated TS is a *typed dispatch table* in the idiom that
already wins there: the `Int8Array(128)` first-byte LUT (`leaf.ts:100-151`) and the arity-
unrolled `fuseAll` arms (`leaf.ts:183-251`) stay hand-written and V8-tuned; only the *vocabulary
and the name-map* are generated. This is the LOCK-4A/4C tie realized *conservatively*: shared
node names and a shared dispatch *contract*, idiomatic bodies per language. Whether the full
SpanParser graph should be mirrored to TS at all is an open question §5 marks for measurement —
the null hypothesis (it deopts, keep the closure core) is live.

### Stratum C — ORACLE (behavioral differential over a shared corpus)

A shared conformance corpus — the grammars the real consumers exercise: JSON (value.js), CSS
(value.js `dispatch()` + 59 `all()` sites, R4 §7), math/units (value.js), BibTeX + LaTeX math
(latex-paper's 7 grammar sites) — is parsed by *both* the Rust core and the TS core; the emitted
Stratum-A tape must be **byte-identical**, checked by the fingerprint harness across cold
children in CI. This is LOCK-6D's behavioral-isomorphism idea, but here it is a *CONFIRMED* win,
not a null hypothesis: it drift-kills the regex-engine and state divergences (§1.3.2/3) *without*
requiring their layout to mirror, because two unrelated machines that emit the same tape on the
same corpus are behaviorally isomorphic by construction.

### The isomorphism LINTER (LOCK-6A face — the residual)

Everything not generated (Stratum B bodies, the leaf name-map, module placement) is pinned by a
checked-in **name-map file** (Rust module/fn ↔ TS module/fn, with `EXEMPT:` rows carrying a
*reason* — `regex → oracle-only`, `span_parser/ → rust-ahead`, `ParserState → transient`). A CI
linter verifies (a) tree correspondence (every mirrored file present both sides), (b) every
name-map row resolves on both sides, (c) every Stratum-A/B generated artifact is a fresh regen.
A divergence is either a *named exemption* or a *lint failure* — never silent.

---

## 3. Integration seams (path:line where extant code serves)

- **Rust tape dumper / builder:** `crates/bbnf-bench/src/bin/tape_dump.rs`; offset-only invariant
  at `generated.rs consume_delimiter`. → Stratum A generator target (Rust side).
- **TS tape decoder (to be generated):** `crates/bbnf-bench/ts-tape/{tape.mjs:364, load.mjs:34,
  pointer.mjs:34}`. → Stratum A generator target (TS side); `verify.mjs:55` → Stratum A/C gate.
- **wasm crossing seam:** GN-4.W4 (SPEC §0 ~line 137) — wasm-bindgen export of the four planes.
- **Rust parser core:** `parse.rs:61` (`Parser`, SmallBox `S32`), `parse.rs:57-59` (flags:
  `TRIM_WS`/`SAVE_STATE`/`EOF`), `state.rs:202` (`ParserState`), `state.rs:241` (`ws_bitmap`
  Rust-only), `leaf.rs:85` (regex→DFA), `leaf.rs:298` (`one_of`), `leaf.rs:315,340`
  (`dispatch_byte`/`_multi` LUT). Enum shape: `span_parser/leaves.rs` (SpanKind → Stratum B spec).
- **TS parser core:** `parser.ts:24-32` (`Parser<T>`), `parser.ts:14-16` (`ParserFunction<T>` —
  the megamorphic IC), `leaf.ts:100-151` (`dispatch` Int8Array LUT — keep hand), `leaf.ts:183-251`
  (`fuseAll` arity-unrolled — keep hand), `leaf.ts:317-369` (regex native RegExp — oracle-only),
  `state.ts:36-53` (`ParserState`).
- **Diagnostics mirror seam:** Rust `#[cfg(feature="diagnostics")]` (`parse.rs:129`) ↔ TS
  module-global `diagnosticsEnabled` (`utils.ts:6`) → PT-3 W2 per-parse scope
  (`diagnostics/scope.ts`). 8C's question: should TS *also* build-time-erase (tree-shake /
  `sideEffects:false` dead-code, `package.json` already `sideEffects:false`) to match Rust's
  compile-time model, making the *mechanism* mirror, not just the behavior?
- **Consumers (Stratum-C corpus sources, read-only):** value.js `src/parsing/{math,units,
  color/color,stylesheet/stylesheet}.ts` (R4 §7); latex-paper `src/grammar/*.ts` +
  `src/bibtex/parser.ts:7`; keyframes.js transitive.

---

## 4. Prior art (with citations)

- **FlatBuffers / Cap'n Proto** (Google; Kenton Varda) — one schema → zero-copy accessors
  generated in many languages; reading is offset arithmetic, no parse step. The offset-tape SoA
  *is* this thesis; PROVE-14's "same bytes decoded in TS, stop-early 4.4×" is precisely Cap'n
  Proto's "infinitely faster because there is no decode." Directly grounds Stratum A.
- **Protocol Buffers / MLIR ODS TableGen** (LLVM) — a single declarative op/message spec
  generates C++ + language bindings; the model for Stratum B (node vocabulary generated from one
  `.td`-like spec, hand-written idiomatic bodies). rustc's own use of TableGen-style single-
  source instruction/intrinsic definitions is the "generated skeleton, hand bodies" precedent.
- **tree-sitter** (Max Brunsfeld) — one grammar → generated C parser + N language bindings over a
  *language-agnostic node-kind vocabulary*; the canonical precedent for a shared node vocabulary
  + differential node trees across languages. Stratum B/C together.
- **Differential testing** — Csmith (Yang, Chen, Eide, Regehr, PLDI 2011); the fingerprint-
  across-cold-children method PROVE-14 uses is a differential oracle in this lineage. Stratum C.
- **WASM boundary cost** — the documented JS↔wasm call/copy overhead (small, boundary-heavy
  workloads often lose to native JS) is *why* the read path stays typed-array-native and only the
  bulk plane export crosses via wasm-bindgen; grounds the §5 refutation of a wasm-core design.
- **serde / bincode** (Rust) — deliberately *not* used for the tape (SPEC §0 ~line 53: AoS-vs-SoA
  is moot, tape is offsets+sparse-flags+arena, a custom SoA); noted as the road not taken.

---

## 5. Named risks and counterexamples

- **R-A (wasm-core over-reach — the primary trap).** *Counterexample:* a design declares "compile
  Rust to wasm, TS is bindings" (a LOCK-6C-flavored answer). PROVE-14 refutes it for the hot
  path: cold full-walk in TS is **0.91×** — native `JSON.parse` beats a wasm/JS full
  materialization, and the combinator hot path is hand-V8-tuned (PT-3 productionizes *hand* TS).
  Cure: wasm crosses only the *bulk tape planes* (GN-4.W4); the read and the combinators stay
  native. 8C's mirror is stratified, not monolithic-wasm.
- **R-B (the Rust SpanParser has no TS twin, and a naive twin deopts).** *Counterexample:*
  Stratum B generates a big TS discriminated union to mirror `span_parser/`; the union flows
  through one `switch`, re-creating the megamorphic dispatch the `Int8Array` LUT and arity-unroll
  exist to avoid (R4 §6c/d). Cure: generate *vocabulary + dispatch contract only*; keep hand-
  tuned bodies; treat "mirror the full graph to TS" as measured-or-refuted (§6.3), null
  hypothesis = keep the closure core.
- **R-C (regex non-isomorphism mislabeled as a defect).** Rust DFA vs TS `RegExp` will *never*
  layout-mirror. *Counterexample:* the linter flags `leaf.rs:85` vs `leaf.ts:317` as drift and a
  session "fixes" it by porting the DFA to TS — abandoning V8's optimized RegExp for a cold-JIT
  hand DFA (a likely regression). Cure: `regex → oracle-only` is a *named exemption*; parity is
  Stratum-C behavioral, LOCK-5 owns the verdict.
- **R-D (diagnostics mechanism mismatch).** Rust erases at compile time (zero cost, clause v); TS
  per-parse-scopes at runtime (PT-3 W2). *Counterexample:* forcing TS to compile-time-erase to
  "match" Rust breaks the live per-parse diagnostic surface keyframes.js consumes
  (`ResolvedKeyframes.diagnostics`, R4 §5). Cure: behavioral mirror, not mechanism mirror;
  document the two zero-cost strategies as *both* satisfying clause v.
- **R-E (the mirror tree lints as isomorphic while semantics drift).** Tree/name correspondence
  is necessary, not sufficient — two same-named functions can diverge in behavior. Cure: Stratum
  C (the byte-tape differential over the consumer corpus) is the *semantic* gate; the linter is
  only the *structural* one; both are required to claim "mirror."
- **R-F (non-clobber law).** All parse-that work is READ-ONLY to the bbnf campaign (PT-3 §2); the
  generator, name-map, and linter land only via the owner or a parse-that session, and the ts-
  tape lives bbnf-side (`crates/bbnf-bench/ts-tape`) — the crossing is deliberately homed where
  the campaign may write. Prototyping is against scratch copies.

---

## 6. What a prototype must test to move this family

1. **Generate-both-diff (Stratum A proof).** Author one tape-plane schema; generate the Rust
   dumper and the TS decoder; assert the generated TS decoder passes PROVE-14's `verify.mjs`
   fingerprint *unchanged* against the Rust dump, and that generated Rust dumper output is byte-
   identical to `tape_dump.rs`'s. **Success = one spec, two artifacts, byte-exact, regen-stable.**
2. **Differential-corpus CI (Stratum C gate).** Run Rust `parse_tape` and the TS decoder over a
   corpus spanning JSON + value.js CSS + latex math; assert byte-identical planes + fingerprint
   across cold children. **Success = the drift-killer runs green; regex/state divergence
   invisible to the tape.** Establishes the born-RED gate for the mirror.
3. **Node-vocabulary mirror cost (Stratum B, the LOCK-4C-in-TS decision).** Generate a TS
   dispatch mirroring `span_parser/leaves.rs` (discriminated-union *vs* Int8Array-LUT table) and
   measure against hand `dispatch`/`fuseAll` under the certified paired-interleaving method (cold
   per-op, single build, A/B/A/B, packrat state disclosed). **Success/refutation = does the
   generated shape hold the arity-unroll floor, or deopt?** Answers whether the graph mirrors to
   TS or the closure core stands.
4. **Isomorphism-linter coverage.** Build the name-map (with the §1.1 divergences as exemptions);
   run the tree/name check; measure false-positive rate and count real divergences caught (regex,
   SpanParser, ws_bitmap). **Success = zero false positives, all three §1.3 divergences surfaced
   as named exemptions.**
5. **wasm-bindgen tape-export cost (GN-4.W4 crossing).** PROVE-14 measured a *native file dump*,
   not a wasm export. Measure the crossing cost of exporting the four planes via wasm-bindgen vs
   the native dump; confirm stop-early 4.4× **survives the wasm boundary** (not just the file
   round-trip). **Success = the SPEC-sanctioned seam holds the certified number end-to-end.**

Each test is falsifiable and produces a running artifact or a named negative result; together
they convert the mirror from an assertion into a machine-enforced, measured correspondence with
its exemptions honestly bounded.
