# LOCK-8 · FAMILY 8C — ISOMORPHIC MIRROR (correspondence): research, pass 3

*The TS plane as the lawful mirror of the Rust parse-that core. 8C does not build the general
mirror machinery (linter, generator, differential harness) — those are LOCK-6's. It builds the
parse-that-specific residue: the machine-checkable DIVERGENCE LEDGER, the regex hazard-class
enumeration with its discriminating corpus, and — newly this pass — the adjudication of whether a
PRIORITY-PRESERVING DFA construction dissolves the regex divergence (routed to LOCK-5 as a
decision). Every landing rides letter BBNF-PT-G; parse-that is READ-ONLY; prototypes are scratch
copies base-pinned to HEAD `ef10d5b`.*

---

## 0. What pass 3 nails that pass 2 asserted

Pass 2 named three structural divergences and *predicted* the regex alternation-order hazard (R-C1)
as "known, not speculative." Pass 3 turns prediction into **measured code-level fact** and
discovers the divergence is **live in a real consumer**, not hypothetical:

- The Rust NFA **builds** priority-ordered ε-edges (`nfa.rs:19-23` `EpsilonEdge{priority:u8}`;
  alternation `priority = i` for leftmost-first, `nfa.rs:311-314`; greedy/lazy priorities
  `nfa.rs:343-382`) — **then the subset construction throws priority away**: `epsilon_closure`
  returns a `BTreeSet<StateId>` ordered by numeric state id, never by priority
  (`dfa.rs:189-201`), and `find_at` returns `last_accept` = **leftmost-LONGEST** (`dfa.rs:116-138`).
  The engine's stated intent (leftmost-first) is silently overwritten by leftmost-longest at
  determinization. This is directive #3's premise, confirmed at `path:line`.
- The TS regex leaf is JS `RegExp` with the sticky `/y` flag anchored at `state.offset`
  (`leaf.ts:5-31`), i.e. **leftmost-FIRST backtracking** (Irregexp/PCRE order). The two engines
  therefore realize **different match-selection semantics by construction**, not by accident.
- **The live corpus exercises the divergence.** latex-paper's math grammar contains
  `regex(/[\s\S]*?(?=\\\])/)` and `regex(/[\s\S]*?(?=\\\))/)` (`math.ts:45,57`) — each a **lazy
  `*?` quantifier PLUS a lookahead assertion `(?=…)`**. R-C1 is not a contrived `/a|ab/`; it is
  shipping consumer code.
- **The Rust HIR cannot even represent the lookahead.** `Hir` (`hir/mod.rs:216-224`) has variants
  `Empty · Literal · Class · Look(Look) · Repetition · Group · Concat · Alternation` — and `Look`
  (`hir/mod.rs:170+`) is a **fixed** assertion enum (`Start End StartLF EndLF StartCRLF EndCRLF` +
  word boundaries), with **no** `Look(Box<Hir>)` general-lookahead variant. `(?=\\\])` is
  **inexpressible Rust-side**. No determinization strategy recovers an assertion the HIR cannot
  hold.

The consequence reshapes the pass-2 regex row: it is **not one row** but a *partition* by
expressiveness class — and the priority-DFA question (directive #3) resolves only the **regular
fragment**, never the non-regular one. §3-§5 develop this; it is the pass-3 centre of gravity.

---

## 1. THE DIVERGENCE LEDGER (directive #1) — machine-checkable, one row per unit

Census method: signature grep/AST over `rust/parse_that/src/{combinators/methods/*.rs, leaf.rs,
span_parser/*, state.rs}` (Rust) ↔ `typescript/src/parse/{parser.ts, leaf.ts, state.ts,
packrat.ts, utils.ts}` (TS). Classification vocabulary: **MIRRORED** (same behavioral contract,
name-mapped) · **LAWFUL-NON-MIRROR(reason)** (intended asymmetry, parity-floored) · **DRIFT(defect)**
(unintended — none found). The ledger is the artifact 6A whitelists and 6D asserts; it is emitted
as a data file (`scratch/8c-ledger.tsv`, landed via BBNF-PT-G), reproduced here as its authoritative
content.

### 1a. Method surface (`combinators/methods/*.rs` ↔ `parser.ts`)

| unit | Rust | TS | class | parity floor |
|---|---|---|---|---|
| then | `sequence.rs:10` | `parser.ts:81` | MIRRORED | tuple `(O,O2)` ≡ array `[T,S]` on same input |
| skip · next | `sequence.rs:23,36` | `:189,:212` | MIRRORED | consumed-span equality |
| or · chain | `alternation.rs:11,37` | `:105,:124` | MIRRORED | `chain` C-16 falsy-seed fix TS-only (Rust never had bug) — behavioral parity holds |
| map · map_with_state | `map.rs:10,32` | `:146,:162` | MIRRORED(name: `map_with_state`↔`mapState`) | value-map equality |
| opt | `methods/mod.rs:18` | `:234` | MIRRORED(type: `Option<O>`↔`T\|undefined`) | present/absent parity (the 8B totality seam) |
| **not↔minus** | `minus.rs:14` (`not`) | `:309` (`minus`) | **MIRRORED(NAME-INVERSION)** | Rust `not`=EBNF `-`=TS `minus`; TS also has distinct `not`/`negate` (`:251`) — **the name-map MUST record the inversion or the linter mis-pairs** |
| many · sep_by | `repetition.rs:14` · `sep_by.rs:16` | `:523` · `:569` | MIRRORED(name: `sep_by`↔`sepBy`) | collected-list equality |
| recover | `recover.rs` | `:653` | MIRRORED | recovered-value + error-set equality |

### 1b. Rust-only / TS-only method extras

| unit | side | class | reason |
|---|---|---|---|
| `many_small` `sep_by_small` `map_with_ctx` `sep_by_ws{,_until}` | Rust | LAWFUL-NON-MIRROR(rust-alloc-convenience) | SmallVec/ctx conveniences; no V8 need, no behavioral surface a consumer reads |
| `wrap` `peek` `lookAhead` `trim` `debug` | TS | LAWFUL-NON-MIRROR(idiom) / MIRRORED-via-composition | Rust expresses `wrap`/`peek` by composition; `trim`/`debug` idiomatic |

### 1c. Leaf/primitive surface (`leaf.rs` ↔ `leaf.ts`)

| unit | Rust | TS | class |
|---|---|---|---|
| string · epsilon · one_of(≈any) | `leaf.rs` | `leaf.ts` | MIRRORED |
| dispatch_byte(_multi) ↔ dispatch | `leaf.rs:315,340` | `:100` (`Int8Array(128)` LUT) | MIRRORED(both O(1) first-byte) |
| **regex** | `leaf.rs:85` → `dfa.find_at` (**longest**) | `:317` → sticky `/y` (**first**) | **see §3 — partitioned by expressiveness** |
| `*_span` family (`string_span:55` … `any_span:270`) | Rust only | — | LAWFUL-NON-MIRROR(rust-zero-boxing) — see §2 |

### 1d. Whole subsystems

| subsystem | Rust | TS | class | reason |
|---|---|---|---|---|
| **SpanParser** (`span_parser/` ~1264 LOC, enum `SpanKind` `mod.rs:60+`, aho-corasick scan) | present | **excised** (gate `proof:no-span-surface`) | LAWFUL-NON-MIRROR(rust-zero-boxing) | dodges a `SmallBox<dyn ParserFn>` box cost V8 lacks; a TS twin is pure dead weight (§2) |
| **packrat** (`packrat.ts:488`, `getCijKey` `:71`, module-global Map `:133`, `PACKRAT_ARMED` `:156`) | **absent** (grep `packrat\|memoize\|MemoCell` in `parse_that/src` = 0) | present | LAWFUL-NON-MIRROR(ts-only) | Rust uses borrow-scoped recursion; no attachment point exists; shape-stability is intra-TS (8A), not a correspondence question |
| **diagnostics** | `#[cfg(feature)]` compile-erased (`state.rs:8-45`, `Cargo.toml:16`) | runtime scope (PT-E1: module-global → per-parse, `parser.ts:34`) | LAWFUL-NON-MIRROR(feature-vs-scope) **+** MIRRORED(behavioral field-triple) | mechanisms structurally incommensurable; the read field-triple `{furthest_offset, sorted expected, line, column}` mirrors — §4 |

**DRIFT rows: zero.** Every asymmetry has an intended reason and a parity floor. The mirror is
*dense* (method surface near-1:1); the exceptions are the four subsystem rows plus the regex
partition. This is what makes correspondence legible rather than chaotic — and what a generator
(6B) must respect: it may symmetric-emit rows in 1a/1c-MIRRORED, and must **never** try to mirror
1d.

---

## 2. Divergence I — SpanParser no-twin (re-verified)

Re-verified `path:line` this pass: `span_parser/mod.rs` (SpanKind zero-boxing dispatch),
`leaf.rs` `*_span` variants, TS excision guarded by `proof:no-span-surface`. Ledger row:
`SpanParser :: LAWFUL-NON-MIRROR(rust-zero-boxing; V8 has no boxed-combinator cost; behavioral
span-triple parity gated by 6D)`. Residue artifact (8C-supplied, 6D-run): for every `*_span`
leaf, assert `(matched, start, end)` == TS `(matched, offset-delta)` on a corpus **including
trim-boundary and zero-width cases** (R-C2 falsifier — the excision must have lost no *behavior*,
only a Rust-local allocation strategy). No TS SpanParser is built — that would fail the gate AND
regress V8 IC stability (R-C6).

---

## 3. Divergence II — regex: the pass-3 partition + the FULL hazard-class set (directive #2)

The pass-2 single "two-engines" row is refined into a **partition by expressiveness class**,
because the two engines diverge along *two independent axes*: **selection** (which match, among
those a regular language admits) and **expressiveness** (whether the pattern is regular at all).

### 3a. The FULL hazard-class enumeration

Each class carries a paired-triple probe `{regex, input}` → expected under {Rust-current-DFA-longest
(a), Rust-priority-DFA-prototype (b), JS-RegExp (c)}. This is the differential corpus (directive #2).

| # | hazard class | probe | (a) DFA-longest | (c) JS-first | axis | priority-DFA (b) fixes? |
|---|---|---|---|---|---|---|
| H1 | ordered alternation | `/a\|ab/` @ `"ab"` | `"ab"` | `"a"` | selection | **YES** |
| H2 | lazy quantifier | `/a+?/` @ `"aaa"` | `"aaa"` | `"a"` | selection | **YES** |
| H3 | lazy under bound | `/[\s\S]*?x/` @ `"aaxbx"` | `"aax"` (forced, agrees) | `"aax"` | selection | YES (agrees once bounded) |
| H4 | nested greedy/lazy submatch | `/(a+?)(a+)/` | n/a (leaf has no submatch) | leftmost-first split | selection+capture | partial (needs tagged/TDFA) |
| H5 | **lookahead** `(?=…)` | `/[\s\S]*?(?=\])/` @ `"a]b"` (**live: math.ts:45**) | **inexpressible** (`Hir::Look` fixed-only) | `"a"` | **expressiveness** | **NO** |
| H6 | lookbehind `(?<=…)` | `/(?<=\$)\d+/` | inexpressible | matches | expressiveness | NO |
| H7 | backreference `\1` | `/(a)\1/` @ `"aa"` | inexpressible | `"aa"` | expressiveness | NO |
| H8 | flag skew `i`/`u`/`s`/`m` | `/A/i` @ `"a"`; `/./s` @ `"\n"` | depends on `classify.rs` flag honoring | JS semantics | normalization | orthogonal |
| H9 | zero-width / empty match | `/a*/` @ `"b"` | `""` | `""` | selection | agrees |
| H10 | anchoring at offset | `/x/y` at `lastIndex` vs `find_at(bytes,0)` | anchored | anchored | anchoring | agrees (both anchored — verify slice base) |

**The corpus is a `.tsv` of `{id, regex, input, expect_a, expect_b, expect_c}`** rows, one per
class plus the two **live latex-paper leaves** as named fixtures, run by 6D against all three
engines. H5-H7 are the expressiveness axis; H1-H4/H9 the selection axis; H8/H10 normalization.

### 3b. The classification verdict

The regex row **cannot be a single ledger cell**. It partitions:

- **Regular fragment, selection-divergent (H1-H4, H9):** today `DRIFT-RISK` — the DFA silently
  answers leftmost-longest where JS answers leftmost-first. **Directive #3 (§5) resolves this into
  MIRRORED** iff LOCK-5 adopts priority-preserving determinization; else it stays
  `LAWFUL-NON-MIRROR(two-engine-selection)` with a build-time constraint (§5, route B).
- **Non-regular fragment (H5-H7):** **HARD LAWFUL-NON-MIRROR(inexpressible-in-Hir).** The Rust
  engine *cannot represent* lookaround/backref; no determinization saves it. This is the deepest
  correspondence fact of the plane, and it is **live** (H5 in math.ts). Two sub-routes, both
  LOCK-5-owned: (i) a Rust-side backtracking/PikeVM sub-engine for the non-regular fragment,
  behaviorally matching JS; or (ii) a build-time grammar constraint forbidding non-regular regex
  leaves — which **breaks latex-paper unless its two leaves are rewritten** (e.g.
  `[\s\S]*?(?=\])` → a `take_until("]")` combinator, moving the assertion out of the regex into
  the combinator layer where both engines agree). The rewrite route is the parsimonious one and it
  is 8C's recommendation to LOCK-5: **hoist context-sensitive assertions out of regex leaves into
  combinators** rather than grow a second Rust regex engine.
- **Normalization (H8, H10):** `LAWFUL-NON-MIRROR(flag-normalization)` — a `classify.rs`↔RegExp
  flag-honoring audit, not a determinization concern.

---

## 4. Divergence III — diagnostics (re-verified) + packrat 4th (re-verified)

Diagnostics: `LAWFUL-NON-MIRROR(compile-feature vs per-parse-scope)` + `MIRRORED(behavioral
field-triple)`. The consumers read only `{furthest, expected:string[], getLineAndColumn}`
(`parser.ts:51-75`); Rust `Diagnostic` (`state.rs:36-45`) carries a **superset** (`found`,
`suggestions`, `secondary_spans`) — a documented widening. Residue: failing-input corpus asserting
the mirrored triple identical Rust-on vs TS-on, extras allowed to widen; R-C3 falsifier is a
consumer read of a Rust-only field (keyframes `ResolvedKeyframes.diagnostics` must read only the
triple). Packrat: `LAWFUL-NON-MIRROR(ts-only; Rust borrow-scoped recursion)`; grep confirms zero
Rust attachment; armed-vs-unarmed disclosed in every table (default LL(1) pays nothing,
`packrat.ts:156/290`).

---

## 5. NEWLY OPENED (directive #3, shared with LOCK-5) — priority-preserving DFA vs grammar constraint

**The premise, confirmed:** priority is *built* (`nfa.rs:311-314,343-382`) and *discarded*
(`dfa.rs:189-201` `BTreeSet`, `find_at` `last_accept`). The engine is leftmost-longest wearing
leftmost-first's clothes.

**Route A — priority-preserving determinization.** Replace the `BTreeSet<StateId>` closure with a
**priority-ordered thread list** (the RE2/Pike-VM discipline): the DFA state becomes an *ordered*
NFA-state sequence; ε-closure carries `EpsilonEdge.priority` and preserves thread order; at accept,
the match is the **highest-priority thread** that reaches accept, not the longest run. This is
exactly Russ Cox's "leftmost-first / Perl semantics via thread priority" (*Regular Expression
Matching: the Virtual Machine Approach*, Part 2) and RE2's Perl mode (RE2 offers BOTH
leftmost-longest/POSIX and leftmost-first/Perl, selected by ordering). **It dissolves H1-H4/H9** —
the selection axis. Costs, named:
- **Minimization weakens.** Hopcroft (`dfa.rs:~299+`) partitions on accept/non-accept; a
  priority-carrying DFA must **also** partition on match-priority tag (a *tagged* DFA / TDFA,
  Laurikari), so fewer states merge — larger tables. A real, measurable cost, not a wash.
- **State blowup risk.** Priority-ordered subset sets are finer than unordered ones; pathological
  patterns can blow the DFA up. RE2's answer is a DFA-size cap with PikeVM fallback — which LOCK-5
  would inherit.
- **It does NOT touch H5-H7.** Priority cannot conjure an assertion the HIR cannot hold. Route A is
  necessary-but-insufficient for the *live* corpus.

**Route B — build-time grammar constraint.** Constrain the grammar regex surface so leftmost-longest
== leftmost-first *observably*: forbid/normalize ordered alternation (auto-rewrite `a|ab`→`ab|a`
longest-first), forbid lazy quantifiers, forbid lookaround/backref. Then the current simple DFA is
already correct-by-construction. Cheaper engine; but it **constrains the author** and — critically —
**breaks the live latex-paper leaves** (H5), which forbid-without-rewrite cannot accept.

**The discriminating evidence (the deliverable that moves the family):** a **census of the LIVE
grammar regex leaves** for divergence-triggering constructs, run this pass:
- value.js: no `regex(/…/)` leaves with lazy/alternation-order hazards found in `src` (its parsing
  is combinator-structured, regex leaves are simple classes).
- latex-paper: **two live H5 leaves** (`math.ts:45,57`, lazy+lookahead) + literal-star false
  positives (`commands.ts:182,269` are `\*?` = optional escaped asterisk, **not** lazy — a probe
  the corpus generator must not miscount).

**8C's adjudication routed to LOCK-5:** the live corpus proves the divergence is **real but narrow**
— it is *not* the selection axis (H1-H4) that bites live code; it is the **expressiveness axis (H5)**.
Therefore priority-preserving determinization (route A), while the *elegant* fix for the selection
axis, **does not fix the one class the live corpus exercises** — lookahead. The parsimonious,
constitution-fit (clause iii "reasonable point, measured"; extreme-parsimony) resolution: **hoist
context-sensitive assertions out of regex leaves into the combinator layer** (a `take_until`/`peek`
composition both engines mirror), reserve route A only if a measured *selection-axis* divergence
appears in a live grammar, and mark H5-H7 a permanent `LAWFUL-NON-MIRROR` with the rewrite as the
migration. The **scratch prototype LOCK-5 must run** to decide: a priority-carrying
`epsilon_closure` variant + the §3a paired-triple corpus, reporting (i) whether (b)≡(c) on H1-H4,
(ii) the minimization state-count delta (route-A cost), (iii) confirmation that H5-H7 remain
inexpressible regardless. That triple output *is* the decision input; the engine choice is LOCK-5's,
not 8C's.

---

## 6. Coordination: ledger↔linter handshake + tape-crossing home (directive #4)

**Handshake with LOCK-6.** 8C emits the ledger (§1) as `scratch/8c-ledger.tsv`; **6A's linter
consumes the MIRRORED rows as its name-map** (including the mandatory `not↔minus` inversion, 1a) and
the LAWFUL-NON-MIRROR rows as its **exemption whitelist** — the linter must not flag SpanParser's
Rust-only-ness, packrat's TS-only-ness, or the diagnostics mechanism split. **6D consumes the
parity-floor column** as its differential-oracle assertion set (§2 span-triple, §3a regex triple,
§4 diagnostics triple). **6B consumes the class column** to know which rows are symmetric-emittable
(1a/1c MIRRORED) vs forbidden-to-mirror (1d). 8C supplies *data*; the three mechanisms are LOCK-6's
(R-C4 — 8C must not rebuild a harness).

**Tape-crossing / decode-speed fence (SPEC §0).** The offset-tape TS decoder
(`crates/bbnf-bench/ts-tape/`, PROVE-14) is a **bbnf/GN-4/LOCK-6 face, adjudicated OUT of the
parse-that plane.** Its headline **4.78× paired stop-early** (full-walk is *parity* 0.98×) is a
**pure-JS-decode** number: the timed region includes typed-array construction but **excludes the
wasm→JS slice handoff** — the real backend's `wasm-bindgen` export cost is **UNMEASURED**. Coupling
4.78× to a "wasm boundary wins 4×" claim is exactly the shim-perf self-deception SPEC §0 outlaws
(R-C5). Any 8C-adjacent citation of the number carries the label *"pure-JS decode; wasm boundary
cost unmeasured"* and the home stays LOCK-6 (6C WASM-CORE territory).

---

## 7. Prior art (citations)

- **RE2 (Cox) — leftmost-first vs leftmost-longest as a MODE, not an accident.** RE2 realizes Perl
  (leftmost-first) semantics via NFA-thread **priority ordering**; its DFA carries match priority
  and falls back to PikeVM on size cap. Directly the route-A design and its cost (*Regular
  Expression Matching: the Virtual Machine Approach*, Parts 1-3; *…Can Be Simple And Fast*).
- **Laurikari tagged DFA (TDFA)** — determinization preserving submatch/priority tags; the formal
  statement of why priority-carrying determinization weakens minimization (H4, §5 cost).
- **regex-automata (BurntSushi)** — the canonical DFA/leftmost-longest engine; its explicit
  leftmost-first PikeVM and the DFA-size-cap-with-fallback architecture LOCK-5 would inherit.
- **V8 Irregexp** — backtracking, leftmost-first; the TS-side engine 8C mirrors *against*. The RE2↔
  Irregexp pair is the literature that makes H1-H7 *known* hazards, not speculation.
- **tree-sitter `grammar.js`→C** — single-spec→one-artifact generator; its lesson (only the *table*
  generates cleanly, not the runtime) bounds what 6B may symmetric-emit from §1's ledger.
- **Rust `#[cfg(feature)]` monomorphization vs runtime flags** — the diagnostics feature-vs-scope
  split (§4) is the textbook compile-erasure-vs-runtime-scope asymmetry; structural, not defect.

---

## 8. Named risks & counterexamples

- **R-C1 (RESOLVED to fact):** regex leftmost-longest↔leftmost-first divergence is confirmed at
  `dfa.rs:189-201`/`leaf.ts:5-31` and **live** at `math.ts:45,57`. The §3a corpus MUST include the
  two live leaves as named fixtures.
- **R-C7 (NEW, highest value): the live hazard is EXPRESSIVENESS (H5), not selection (H1).** A route-A
  priority-DFA prototype that "fixes `/a|ab/`" and declares victory **misses the class the live
  corpus actually exercises** — lookahead the Rust HIR cannot hold. The prototype's success gate
  MUST assert H5-H7 remain inexpressible, so route A is not oversold as a total dissolution.
- **R-C8 (NEW): the `\*?` false positive.** `regex(/\\[vh]space\*?/)` (`commands.ts:182`) and
  `regex(/[a-zA-Z@]+\*?/)` (`commands.ts:269`) are optional *literal* asterisk, **not** lazy
  quantifiers; a naive `grep '*?'` miscounts the hazard census. The corpus generator must PARSE the
  regex, not string-match it.
- **R-C2:** SpanParser excision must lose no behavior — trim-boundary/zero-width span-triple corpus
  is the falsifier (§2).
- **R-C3:** diagnostics `found`/`suggestions` widening must not leak into a consumer read (§4).
- **R-C4:** 8C owns DATA (ledger + corpora), not machinery — no duplicate harness (system-cohesion).
- **R-C5:** the 4.78× stays labelled pure-JS-decode; never a wasm-boundary claim (§6).
- **R-C6:** the ledger records SpanParser/packrat non-mirrors as *intended*, never as gaps to close;
  a "symmetry" TS SpanParser fails `proof:no-span-surface` and regresses IC stability.

---

## 9. What a prototype must test to MOVE family 8C

1. **The ledger, mechanically generated** (§1) — AST/grep census → `scratch/8c-ledger.tsv`, one
   classified row per unit, the `not↔minus` inversion recorded. Feeds 6A name-map + exemptions,
   6B emit-eligibility, 6D assertions. **Un-built; the deliverable.**
2. **The regex paired-triple corpus** (§3a) — `{id,regex,input,expect_a,expect_b,expect_c}` over
   H1-H10 **plus the two live latex-paper fixtures**, parsed not string-matched (R-C8). Run by 6D
   against Rust-current / Rust-priority-prototype / JS.
3. **The priority-preserving determinization scratch prototype** (§5, routed to LOCK-5) — a
   priority-carrying `epsilon_closure` + ordered-thread accept resolution; report (i) (b)≡(c) on
   H1-H4, (ii) minimization state-count delta vs current, (iii) H5-H7 remain inexpressible. This
   triple is LOCK-5's decision input (engine choice is theirs).
4. **The live-corpus regex census** (§5, run this pass, to be widened) — every `regex(/…/)` leaf in
   value.js/latex-paper/keyframes grammars classified by hazard class; the evidence that the live
   divergence is narrow-and-expressiveness, discriminating route A vs the assertion-hoist rewrite.
5. **Span-triple + diagnostics-triple corpora** (§2/§4) — behavioral parity floors for the two
   non-regex non-mirrors.

All prototyping against scratch COPIES base-pinned to HEAD `ef10d5b`; `parse-that` stays READ-ONLY;
every artifact lands only through letter BBNF-PT-G. Every number cold, paired, packrat-state
disclosed; the wasm-boundary cost stays fenced from the pure-JS-decode ratio.
