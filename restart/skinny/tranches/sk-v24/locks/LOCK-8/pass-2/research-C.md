# LOCK-8 · FAMILY 8C — ISOMORPHIC MIRROR (correspondence): research, pass 2

*The TS plane as the lawful mirror of the Rust parse-that core. This family does not build
the general mirror machinery (linter, generator, differential harness) — those are LOCK-6's.
It builds the parse-that-specific residue: the DIVERGENCE LEDGER that names, per structural
divergence, the minimal genuine parse-that-side artifact that either closes the divergence or
certifies it a lawful non-mirror with a behavioral-parity floor.*

---

## 0. The deliverable face, and the residue boundary (directives #1, #2)

**8C's deliverable is a PARSE-THAT face, not a bbnf-bench face.** The three named structural
divergences — SpanParser no-twin, regex DFA-vs-RegExp, diagnostics behavioral-vs-mechanism —
all live entirely inside parse-that's own rust↔ts split: Rust at
`/Users/mkbabb/Programming/parse-that/rust/parse_that/src/`, TS at
`/Users/mkbabb/Programming/parse-that/typescript/src/parse/`. None of them touches skinny.
The one artifact that *does* live in bbnf-bench — the offset-tape TS decoder
(`crates/bbnf-bench/ts-tape/`, PROVE-14) — is a **bbnf/GN-4/LOCK-6 face** and is adjudicated
OUT of the parse-that plane below (§5). 8C therefore delivers a parse-that ledger; the tape
crossing is named as *not-ours*.

**All three general mirror mechanisms are LOCK-6-owned; 8C owns none of them.** The
directive's triple maps cleanly onto LOCK-6's family charters:

| general mechanism | LOCK-6 family | 8C stake |
|---|---|---|
| isomorphism **linter** (tree/name-map CI check) | **6A MIRROR-TREE** | consumer only — 8C supplies the name-map *data* + divergence exemptions, 6A runs the check |
| single-spec **→ two-artifact generator** | **6B SPEC-GENERATED-SKELETON** | consumer only — 8C supplies which combinators are spec-emittable vs lawful non-mirror |
| behavioral **differential oracle** | **6D ORACLE-ISOMORPHISM** | consumer only — 8C supplies the parse-that conformance corpus + the per-divergence parity assertions |

**The genuine parse-that residue = the DIVERGENCE LEDGER.** LOCK-6 decides *how* to enforce
correspondence in general; it does not know *what* in parse-that lawfully diverges and *why*,
nor the *parity floor* each divergence must clear. That knowledge is parse-that-intrinsic and
is 8C's sole deliverable: a machine-checkable ledger with one row per combinator/subsystem,
each classified `MIRRORED` / `LAWFUL-NON-MIRROR(reason)` / `DRIFT(defect)`, and each
non-mirror carrying a behavioral-parity assertion that 6D's harness runs and 6A's linter
whitelists. Scope 8C to exactly this residue; do not re-derive the linter, the generator, or
the harness.

---

## 1. The correspondence baseline — what already mirrors (the MIRRORED rows)

Verified by direct signature census. The combinator method surface is **near-1:1** across
the two cores, which is what makes the divergences legible as exceptions rather than chaos.

**Combinator methods (Rust `combinators/methods/*.rs` ↔ TS `parser.ts`):**

| Rust | TS | note |
|---|---|---|
| `then` (`sequence.rs:10`) | `then` (`parser.ts:81`) | Rust yields `(O,O2)` tuple; TS yields `[T,S]` array — same shape |
| `skip` (`sequence.rs:23`) · `next` (`sequence.rs:36`) | `skip` (`:189`) · `next` (`:212`) | 1:1 |
| `or` (`alternation.rs:11`) · `chain` (`alternation.rs:37`) | `or` (`:105`) · `chain` (`:124`) | 1:1 (TS `chain` C-16 falsy-seed fix has no Rust analogue — Rust never had the bug) |
| `map` (`map.rs:10`) · `map_with_state` (`map.rs:32`) | `map` (`:146`) · `mapState` (`:162`) | `map_with_state` ↔ `mapState` naming skew |
| `opt` (`methods/mod.rs:18`) | `opt` (`:234`) | Rust `Option<O>` ↔ TS `T\|undefined` (the 8B totality seam) |
| `not` (`minus.rs:14`) | `minus` (`:309`) | **NAME DRIFT**: Rust `not` = TS `minus` (EBNF `-`); TS *also* has a distinct `not`/`negate` (`:251`) |
| `many` (`repetition.rs:14`) · `sep_by` (`sep_by.rs:16`) | `many` (`:523`) · `sepBy` (`:569`) | `sep_by`↔`sepBy` casing |

**Leaf/primitive surface (Rust `leaf.rs` ↔ TS `leaf.ts`):** `string`/`regex`/`epsilon`/
`one_of`(≈`any`)/`dispatch_byte`(≈`dispatch`) all present both sides; the monomorphization
strategy corresponds (Rust `dispatch_byte`/`dispatch_byte_multi` `leaf.rs:315,340` ↔ TS
`dispatch` `Int8Array(128)` LUT `leaf.ts:100`; both O(1) first-byte).

**Rust-only combinator extras (not divergences — additive, no TS consumer demands them):**
`many_small`/`sep_by_small` (SmallVec-returning, `repetition.rs:56`, `sep_by.rs:90`),
`map_with_ctx` (`map.rs:19`), `sep_by_ws`/`sep_by_ws_until` (`sep_by.rs:76,142`). These are
Rust allocation-discipline conveniences; the ledger marks them `LAWFUL-NON-MIRROR(rust-alloc-
convenience)` — no V8 need, no behavioral surface a consumer reads.

**TS-only combinator extras:** `wrap` (`:392`, inlined `start.next(this).skip(end)`), `peek`
(`:339`), `lookAhead` (`:365`), `trim` (`:480`), `recover` (`:653`), `debug` (`:690`). Rust
covers `wrap`/`peek` semantics through composition; `recover` corresponds to Rust's
`combinators/methods/recover.rs`. Ledger rows: mostly `MIRRORED` via composition, a few
`LAWFUL-NON-MIRROR(idiom)`.

The point of the baseline is that the mirror is *dense* — the exceptions are the three named
divergences plus packrat (§4), and the ledger's job is to make each an explicit, reasoned,
parity-floored row rather than silent drift.

---

## 2. Divergence I — SpanParser no-twin (the LAWFUL performance non-mirror)

**The asymmetry, concrete.** The Rust core carries an entire parallel combinator subsystem
that the TS core deliberately does **not** have:

- Rust `span_parser/` directory module — ~1264 LOC: `mod.rs:249` (enum `SpanKind`,
  zero-boxing dispatch), `constructors.rs:222`, `leaves.rs:203`, `methods.rs:186`,
  `scan.rs:184`, plus `span_trait.rs:220`. `SpanParser` is an **enum-dispatched** parser
  (`SpanKind::{StringLiteral, …}`, `mod.rs:60+`) explicitly built "zero-boxing for Span hot
  path" (`mod.rs:16`), using `aho-corasick` for multi-pattern span scans.
- Rust `leaf.rs` `*_span` variants: `string_span:55`, `regex_span:102`, `take_while_span:118`,
  `take_while_byte_span:153`, `take_until_any_span:177`, `next_span:257`, `any_span:270`.
- **TS side: excised.** The `*Span` combinator family was removed wholesale in the 1.0.0 cut
  (S.H2), and a standing gate `proof:no-span-surface` (`package.json`) *forbids reintroduction*.
  TS keeps only a passive `Span` **interface** + `mergeSpans`/`spanToString`
  (`state.ts:8-31`) — the data type, never the combinator subsystem.

**Why this is lawful, not drift (the reason the ledger records).** SpanParser exists to dodge
a cost that only exists in Rust: a boxed combinator (`SmallBox<dyn ParserFn>`, `parse.rs`
uses `smallbox::space::S32`) allocates/indirects per Span leaf. The enum-dispatch `SpanKind`
collapses that to a tag test — a monomorphization win with no V8 counterpart, because V8
already represents every combinator closure as a tagged pointer and the arity-unrolled
`fuseAll` (`leaf.ts:183-251`) already captures the sequence-fusion win SpanParser's zero-boxing
targets. A TS `SpanParser` twin would be **pure dead weight**: more shapes flowing through the
one megamorphic IC (`this.parser(state)`, `parser.ts:53` etc.), strictly worse for hidden-class
stability. The constitution's clause (iii) — "isomorphism *to a reasonable point*, measured" —
is exactly satisfied by declaring this the point where the mirror stops.

**The minimal genuine parse-that-side artifact (directive #1).** Not a TS SpanParser. Instead:
a **behavioral-equivalence assertion** in 6D's harness plus a **linter exemption** in 6A's
name-map. Concretely — a conformance row asserting that for every `*_span` Rust leaf, the
`(matched, start_offset, end_offset)` triple it produces equals the `(matched, offset)` a TS
`string`/`regex`/`take*` produces on the same input (TS recovers the span from
`state.offset` deltas rather than a Span value). The artifact is ~1 corpus file + N assertions,
authored parse-that-side; it certifies the excision lost no *behavior*, only a Rust-local
allocation strategy. The ledger row: `SpanParser :: LAWFUL-NON-MIRROR(rust-zero-boxing; V8
has no boxed-combinator cost; behavioral span-triple parity gated by 6D corpus)`.

---

## 3. Divergence II — regex Rust-DFA vs native RegExp (mechanistic split, behavioral mirror)

**The asymmetry.** Rust has a bespoke NFA→DFA regex engine — `rust/regex/` with HIR + e-graph
canonicalization (`egraph_*` tests), `classify.rs`, DFA compile benches — and grammar leaves
lower into it. TS has **no engine**: `leaf.ts:317-369` builds a sticky-flag `RegExp` clone
(`new RegExp(r, flags+"y")`) and drives `sticky.test()` (zero-alloc) / `sticky.exec()`
(`whitespace = regex(/\s*/)`, `leaf.ts:397`). There is no NFA/DFA/Thompson code under
`typescript/src/` at all. This is the deepest mechanistic divergence in the plane.

**Why it must stay divergent (the reason).** JS ships a mature, JIT-compiled RegExp engine in
every runtime; re-implementing a DFA in TS to "mirror" Rust would be a large, slower, bug-prone
subsystem competing with V8's Irregexp. The constitution's clause (ii) demands the *HIR* be
truly isomorphic (LOCK-5's verdict) — but HIR isomorphism is a **source/authoring**-time
property, not a runtime-engine property. Two engines recognizing the same language from the
same HIR is precisely "isomorphism to a reasonable point." The ledger records this as the
canonical case where the mechanism *cannot* mirror and *should not*.

**The minimal genuine parse-that-side artifact (directive #1).** A **behavioral differential
oracle** (6D-run, 8C-supplied corpus): the same regex source + same input string fed to
(a) Rust `regex` leaf, (b) TS `regex()` leaf, asserting identical `(matched?, consumed_span)`.
The corpus must cover the sticky-anchoring semantics TS relies on (`/y` at `state.offset`) vs
Rust's DFA `find`-at-offset — the one place a subtle divergence could hide is
leftmost-longest vs leftmost-first and anchoring. **Named counterexample risk:** JS RegExp
is leftmost-*first* with backtracking (PCRE-style alternation order matters:
`/a|ab/` matches `"a"` in `"ab"`), whereas a classical DFA is leftmost-*longest*. If any
grammar regex leaf uses ordered alternation, Rust-DFA and JS-RegExp **will disagree** on the
match — this is the single highest-value assertion in the whole 8C corpus and MUST be probed,
not assumed. The ledger row is `LAWFUL-NON-MIRROR(two-engines; HIR-isomorphic per LOCK-5;
behavioral parity gated by 6D corpus INCLUDING alternation-order + anchoring cases)`.

---

## 4. Divergence III — diagnostics behavioral-vs-mechanism; and packrat (the hidden 4th)

**Diagnostics — the mechanism cannot mirror, the behavior can.** Rust gates diagnostics at
**compile time**: `Cargo.toml:16` `diagnostics = ["dep:colored"]`, and the whole
`Suggestion`/`SecondarySpan`/`Diagnostic` apparatus is `#[cfg(feature = "diagnostics")]`
(`state.rs:8-45`) — zero code, zero cost, monomorphized away when off. TS gates at **runtime**:
today a module-global `diagnosticsEnabled` flag (`utils.ts:6`), moving under PT-E1 to a
**per-parse scope** hung off the parse epoch (`parseState` try/finally, `parser.ts:34`). A
compile-time feature and a per-parse runtime scope are structurally incommensurable
mechanisms — and both are *correct* for their language (Rust erases at monomorphization; TS
cannot erase a runtime option, so it scopes it).

**The behavioral surface *is* mirror-able**, and that is the residue. Rust's `Diagnostic`
(`state.rs:36-45`) carries `{offset, furthest_offset, line, column, expected: Vec<String>,
suggestions, secondary_spans, found}`; TS's `ParseError` (`parse.rs`-analogue in
`parser.ts:51-75` furthest-view build) carries `{offset, furthest_offset, line, column,
expected}`. The **field triple** the consumers actually read — `furthest`, `expected:
string[]`, `getLineAndColumn` — is identical in shape across both. The minimal parse-that-side
artifact: a **behavioral parity assertion** that, on a corpus of deliberately-failing inputs,
Rust-diagnostics-on and TS-diagnostics-on emit the *same* `{furthest_offset, sorted expected
set, line, column}` — the `found`/`suggestions` extras allowed to differ (Rust-richer) as a
documented widening. Ledger row: `diagnostics :: LAWFUL-NON-MIRROR(compile-feature vs
per-parse-scope) + MIRRORED(behavioral field-triple, gated by 6D failing-input corpus)`.

**Packrat is a FOURTH, undocumented divergence — flagged here (directive: PLANE-WIDE).**
Grep for `packrat|memoize|MemoCell|Cij` across the entire Rust `parse_that/src/` returns
**zero hits**. Packrat is a **TS-only** subsystem (`packrat.ts:488` LOC: `getCijKey` float64
key `id*MEMO_OFFSET_SPAN+offset` `:71-100`, module-global `Map<number,MemoCell>` `:133-193`,
`PACKRAT_ARMED` latch true-no-op until first `memoize()` `:156/290`). The Rust core relies on
lifetime-bounded borrows and has no left-recursion memoization surface at all. So the
PLANE-WIDE packrat questions resolve, for correspondence, as:
- *Where does memoization attach?* TS-only, at the parse epoch (`packratEnter`/`packratExit`,
  `packrat.ts:216-250`), keyed per `(parserId, offset)`. No Rust attachment point exists.
- *What MemoCell/memo-key shape stays shape-stable?* The TS `MemoCell` in the module-global
  `Map` — the shape-stability question is intra-TS (LOCK-4/8A), not a correspondence question,
  because there is no Rust twin to stay stable *against*.
- *Type of a memoized parser?* TS-only; `makeMemoized` returns a `Parser<T>` of the same
  static type (arming is invisible to the type). The correspondence ledger row is
  `packrat :: LAWFUL-NON-MIRROR(TS-only; Rust uses borrow-scoped recursion)` — and the
  binding evidence law holds: **every** perf table discloses armed vs unarmed, and arming is
  opt-in (default LL(1) pays nothing, `packrat.ts:156`).

---

## 5. The tape-crossing / wasm-boundary adjudication (directive #4)

**Where the artifact physically lives.** The offset-tape TS decoder lives in **skinny, not
parse-that**: `crates/bbnf-bench/ts-tape/` (7 files, 703 LOC — `tape.mjs:364`, `load.mjs`,
`pointer.mjs`, `child.mjs`, `verify.mjs`, `bench.mjs`), fed by `crates/bbnf-bench/src/bin/
tape_dump.rs`. The Rust seam it consumes is `crates/runtime/src/tape/mod.rs` — the public
plane accessors `Tape::{source, offsets, flag_cursors, flag_values}` (`mod.rs:120-137`,
verified). **This is a LOCK-6 / GN-4 face; 8C adjudicates it OUT of the parse-that plane.**
It is not a parse-that combinator, not a parse-that rust↔ts divergence — it is the bbnf value
fabric's own cross-language layout proof.

**The stop-early number must be kept out of any wasm-boundary claim.** PROVE-14's headline
stop-early **4.78× paired** (extract `statuses[0]`×3 leaves, CI [4.20, 5.37]; full-walk is
*parity* 0.98×, not 2×) is a **pure-JS decode** measurement: the timed region includes
typed-array construction (incl. a copy of the offset plane) but **explicitly excludes the
wasm→JS slice handoff**. The report itself states a real backend "needs one wasm-bindgen (or
ABI) export handing those four slices to JS" — that export/boundary-crossing cost is
**UNMEASURED**. Therefore: the 4.78× is a file-round-trip decode ratio, and coupling it to a
"wasm boundary wins 4×" claim would be exactly the shim-perf self-deception SPEC §0 outlaws.
The adjudication: **the single tape-crossing path is LOCK-6-owned (6C WASM-CORE territory);
8C neither claims the number nor homes the artifact.** If any 8C-adjacent doc cites the
4.78×, it must be labelled "pure-JS decode, wasm boundary cost unmeasured."

---

## 6. Regen-stability of a single-spec generator (directive #3) — the un-run probe

**The prior state.** parse-that TS is **hand-written, not generated** (PT-3 §5 explicit;
LOC ledger "N/A — hand-written"). There is **no generator emitting both parse-that sides**
today. The working PROVE-14 *decoder* proves a layout crosses byte-exactly; it does **not**
prove any generator emits both sides stably — the directive's caution is exactly right, the
two are unrelated claims.

**Why regen∘regen byte-identity is un-provable until 6B stands up a generator.** A regen-
stability probe requires (a) a machine-readable spec and (b) a generator with two backends.
Neither exists for parse-that. 8C's residue is *not* to build the generator (that is 6B) but
to answer the prerequisite question 6B needs: **which combinators are spec-emittable
(divergence-free) vs which are lawful non-mirrors that a generator must NOT try to emit
symmetrically?** From §1-§4:
- **Spec-emittable candidates:** the leaf/primitive signatures + the near-1:1 method surface
  (`then/or/map/opt/skip/next/many/sepBy` — modulo the `not`↔`minus` name-map and the
  `Option`↔`undefined` totality skew). These *could* be generated both ways.
- **Must-not-symmetric-emit:** SpanParser (Rust-only), packrat (TS-only), the regex engine
  (two engines), diagnostics mechanism (feature vs scope). A generator that tried to mirror
  these would produce the exact dead weight §2/§4 forbids.

**The minimal probe to run (moves both 8C and 6B).** Pick ONE divergence-free unit — the
`string` leaf + its state-advance contract, or better the **tape decoder** (already
divergence-free: it is pure layout, no combinator). Author a tiny spec, emit the Rust reader
and the TS reader from it, run the generator twice, `diff` the two emissions byte-for-byte.
Success = `regen(regen(spec)) ≡ regen(spec)` byte-identical on *both* backends. This is the
smallest experiment that discriminates 6B's central claim, and it is **un-run**. 8C's
contribution is the divergence classification that tells 6B what the spec may and may not
contain; running it is 6B's.

---

## 7. Prior art (citations)

- **rustc/rustfmt idempotence & LLVM verifier** — the regen∘regen = regen discipline is the
  compiler-world "idempotent formatter / fixpoint" law; MLIR's round-trip `--verify-roundtrip`
  is the direct analogue of the §6 byte-identity probe (parse → print → parse ≡ identity).
- **tree-sitter's `grammar.js` → generated C parser** — a *single-spec → one-artifact*
  generator; the two-artifact (Rust+TS) form 6B proposes is the harder generalization, and
  tree-sitter's experience is that only the *table* (not the runtime) generates cleanly —
  matching §6's finding that only divergence-free units are spec-emittable.
- **regex-automata (BurntSushi)** vs **V8 Irregexp** — the canonical two-engine, one-language
  pair; regex-automata is DFA/leftmost-longest, Irregexp is backtracking/leftmost-first. This
  is the literature that makes §3's alternation-order counterexample a *known* hazard, not a
  speculative one — the two engines provably disagree on ordered alternation.
- **GHC's `-ddump-simpl` / cargo-expand** — the "inspect the emitted artifact" discipline the
  divergence ledger relies on: SpanParser's zero-boxing win and packrat's arming are only
  legible in expanded output, not signatures.
- **Rust `#[cfg(feature)]` monomorphization vs runtime feature flags** — the diagnostics
  mechanism split (§4) is the textbook compile-time-erasure-vs-runtime-scope tradeoff;
  Rust erases, JS cannot, and that asymmetry is *structural*, not a defect.

---

## 8. Named risks & counterexamples

- **R-C1 (highest value): regex alternation-order disagreement.** Rust-DFA leftmost-longest
  vs JS-RegExp leftmost-first will silently diverge on any ordered-alternation grammar regex
  leaf (`/a|ab/` on `"ab"`). If unprobed, the "behavioral mirror" claim for regex is false.
  The 6D corpus MUST include ordered-alternation + anchoring cases. This is the single
  assertion whose absence would invalidate §3.
- **R-C2: SpanParser excision hid a behavior, not just an allocation.** Counterexample — if
  any Rust `*_span` leaf produces a span that a TS `take*`/`regex` cannot reconstruct from
  `state.offset` deltas (e.g. a span that spans trimmed whitespace differently), the excision
  was a behavior loss mislabelled as a perf non-mirror. The §2 span-triple corpus is the
  falsifier; it must include trim-boundary and zero-width cases.
- **R-C3: diagnostics `found`/`suggestions` asymmetry leaks into a consumer.** If keyframes.js
  `ResolvedKeyframes.diagnostics` reads a Rust-richer field the TS side never populates, the
  "widening is documented and safe" claim breaks. The failing-input corpus must assert the
  consumer reads *only* the mirrored field-triple.
- **R-C4: treating 8C as if it owned the linter/generator/harness.** The constitution forbids
  consumer-less substrate and orthogonal subsystems; if 8C re-builds a differential harness
  that duplicates 6D, it violates system-cohesion. The residue is DATA (ledger + corpora),
  not machinery.
- **R-C5: the wasm-boundary number contamination.** Any reuse of PROVE-14's 4.78× as a
  boundary claim is a §0 shim-perf violation until the export cost is measured. Keep it
  labelled pure-JS-decode.
- **R-C6 (parsimony/non-clobber): the ledger tempts a TS SpanParser reintroduction.** Building
  a TS twin "for symmetry" both fails `proof:no-span-surface` and regresses V8 IC stability.
  The ledger must record the non-mirror as *intended*, never as a gap to close.

---

## 9. What a prototype must test to MOVE family 8C (the concrete asks)

1. **The DIVERGENCE LEDGER, mechanically generated.** An AST/grep census over
   `parse-that/rust/parse_that/src/` and `parse-that/typescript/src/parse/` emitting one row
   per combinator/leaf/subsystem, classified `MIRRORED` / `LAWFUL-NON-MIRROR(reason)` /
   `DRIFT`. Output is the name-map 6A consumes + the exemption list. **Un-built.** Deliverable:
   a single machine-readable ledger file (the parse-that residue), authored scratch-side, landed
   only via BBNF-PT-G.
2. **Span-triple behavioral corpus (Divergence I).** For every Rust `*_span` leaf, N inputs
   asserting `(matched, start, end)` == TS `(matched, offset-delta)` — including trim-boundary
   and zero-width. Proves the 1.0.0 excision lost no behavior.
3. **Regex differential corpus (Divergence II) — with the alternation-order probe.** Same
   regex + input → Rust `regex` leaf vs TS `regex()` → identical `(matched, consumed_span)`.
   MUST include ordered alternation and sticky-anchor cases (R-C1). This is the highest-value
   single experiment in 8C.
4. **Diagnostics field-triple parity corpus (Divergence III).** Failing inputs → assert
   `{furthest_offset, sorted expected, line, column}` identical Rust-on vs TS-on;
   `found`/`suggestions` allowed to widen (documented). Confirms the mechanism split is
   behaviorally invisible.
5. **The regen∘regen byte-identity probe (directive #3, prerequisite for 6B).** One
   divergence-free unit (the tape decoder or the `string` leaf), a tiny spec, two-backend
   emit, `diff` twice. Discriminates 6B's central claim; **un-run**. 8C supplies the
   divergence classification that bounds the spec's content.

All prototyping is against scratch COPIES; `parse-that/typescript` stays READ-ONLY and every
artifact lands only through letter BBNF-PT-G (non-clobber law). Every number cold, paired,
packrat-state disclosed; the wasm-boundary cost stays separated from the pure-JS-decode ratio.
