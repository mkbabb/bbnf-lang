# LOCK-8 · 8C — THE CORRESPONDENCE PLANE (pass-3 research-C)

*Family 8C is the ISOMORPHIC MIRROR: the TS combinator plane as the lawful mirror of the
Rust `parse_that` core. Its deliverable is not a runtime — it is a **machine-checkable
divergence ledger** over `parse_that/src ↔ typescript/src/parse`, a **regex-correspondence
adjudication** with a differential paired-triple corpus, and the **priority-preserving
subset-construction** question routed to the regex continuum. The mirror is proven by census
and oracle, not asserted. Anchors are `path:line`; the Rust tree is `parse-that/rust/parse_that`,
the regex engine `parse-that/rust/regex`, the TS tree `parse-that/typescript/src/parse`.
parse-that is READ-ONLY; every landing rides letter BBNF-PT-G; prototypes are scratch copies.*

---

## 1. The mechanism: a generated ledger + differential oracle, not a hand map

8C does not build the general mirror machinery (linter, skeleton generator) — that is LOCK-6's.
It builds the parse-that-specific residue as two artifacts:

1. **The ledger** — one row per public combinator / leaf / subsystem, keyed on the symbol,
   carrying `{symbol, rust_anchor, ts_anchor, class, reason, discriminating_test, perf_fence}`
   where `class ∈ {MIRRORED, LAWFUL-NON-MIRROR(reason), DRIFT}`. It is **generated**, never
   hand-maintained: a census walks both trees (TS via the `tsc`/ts-morph symbol table over
   `typescript/src/parse`; Rust via a `pub fn`/`impl` census over `parse_that/src`, or
   `cargo public-api`) and emits `LEDGER.tsv`. The generator is the substrate; the file is the
   deliverable.
2. **The oracle** — a differential harness running one input through both engines, asserting the
   row's class holds: MIRRORED rows must agree byte-for-byte; DRIFT rows must reproduce their
   documented divergence; LAWFUL-NON-MIRROR rows are agreement-exempt but their *reason* is pinned.

**The ledger IS LOCK-6A's isomorphism-linter whitelist.** A census re-run that yields a *new*
DRIFT row (a symbol on one side with no classified counterpart) is a CI failure — drift is
impossible by construction where enforced. LAWFUL-NON-MIRROR rows are the whitelist entries (each
with its reason); MIRRORED rows are enforced correspondences; DRIFT rows are red until an owner
converts them (to MIRRORED by porting, or to LAWFUL-NON-MIRROR by signing the reason).

---

## 2. The divergence ledger (census-derived, this HEAD)

Enumerated from the `pub fn`/method census across `parse_that/src` and the TS combinator census.
Anchors are representative; the generated `LEDGER.tsv` carries every arm.

### 2a. Core sequencing & alternation — MIRRORED / lawful arity-idiom

| symbol | rust_anchor | ts_anchor | class |
|---|---|---|---|
| `then` | `combinators/methods/sequence.rs:10` | `parser.ts:81` | MIRRORED |
| `skip` | `sequence.rs:23` | `parser.ts:189` | MIRRORED |
| `next` | `sequence.rs:36` | `parser.ts:212` | MIRRORED |
| `wrap` | `sequence.rs:48` | `parser.ts:392` | MIRRORED |
| `trim` / `trim_keep` | `sequence.rs:97,111` | `parser.ts:480` | MIRRORED (Rust splits `trim_keep`) |
| `or` | `combinators/methods/alternation.rs` | `parser.ts:105` | MIRRORED |
| `opt`·`peek`·`look_ahead`·`not`/`negate`·`minus` | `leaf.rs`/`minus.rs` | `parser.ts:234,339,365,251,309` | MIRRORED |
| `recover` | `combinators/methods/recover.rs` | `parser.ts:653` | MIRRORED |
| `lazy` | `lazy.rs` | `lazy.ts` | MIRRORED |
| `map` | `combinators/methods/map.rs` | `parser.ts:146` | LAWFUL-NON-MIRROR (Rust adds `map_closure`/`map_with_ctx`/`map_with_state`; TS keeps `map`+`mapState`) |
| `eof`/`epsilon` | `parse.rs:210` / `leaf.rs:7` | `leaf.ts:11` | LAWFUL-NON-MIRROR (TS has no `epsilon` leaf) |

**The arity row (the sharp one).** TS `all`→`fuseAll` monomorphizes arity-2 (`leaf.ts:183`) and
arity-3 (`leaf.ts:214`), general arm a classic `for` (`leaf.ts:255`). Rust achieves the same
*shape* by comptime: the `seq!` macro (`combinators/macros.rs:6`) unrolls arities 2..N and
`impl_parser_flat!` (`span_trait.rs:184`) is instantiated for tuple arities **B..N**
(`span_trait.rs:209-220`). **Row = LAWFUL-NON-MIRROR(idiom):** the *decision* (specialize per
arity) mirrors; the *mechanism* diverges lawfully — V8 hand-unroll + runtime dispatch vs LLVM
macro/trait monomorphization. The linter enforces "both sides specialize arities ≥2," not
identical source. `any`/`one_of` is the same (`leaf.ts:28` arity-2 unroll ↔ `leaf.rs:298`
`one_of(Vec)`).

### 2b. Repetition & dispatch — DRIFT (Rust-only variants)

| symbol | rust | ts | class |
|---|---|---|---|
| `many` | `repetition.rs` + `many_small` (arena) | `parser.ts:523` | DRIFT — `many_small` no TS analog |
| `sep_by` | `sep_by.rs` + `sep_by_small`/`sep_by_ws`/`sep_by_ws_until` | `parser.ts:569` | DRIFT — `_ws`/`_until` family TS lacks |
| `dispatch` | `dispatch_byte` `leaf.rs:315` + `dispatch_byte_multi` `leaf.rs:340` | `leaf.ts:100` (`Int8Array(128)`) | DRIFT — `_multi` (byte-seq keys) no TS analog |

These rows the linter forces to a disposition: port the variant (→ MIRRORED) or sign
LAWFUL-NON-MIRROR (arena/lifetime-only, no V8 payoff). Left unclassified they are silent drift —
the exact failure 8C exists to close.

### 2c. Deliberate non-mirrors — LAWFUL-NON-MIRROR(excision), the strongest rows

- **The `*Span`/`span_parser` family.** Rust keeps a whole zero-copy `span_parser/` module (`sp_*`
  leaves, `*_span` methods, `span_parser/scan.rs`, ~40 `_span` symbols at `constructors.rs`) on
  `Span<'a>` borrow slices. **TS EXCISED the whole `*Span` family in the 1.0.0 cut**, guarded by
  `proof:no-span-surface`. Reason = lifetimes: zero-copy `Span<'a>` has no V8 payoff (JS returns
  substrings). The model LAWFUL-NON-MIRROR row — deliberate, reasoned, gated. **Not DRIFT.**
- **The SIMD scan layer.** Rust `scanners.rs` + `parsers/scan/*` (quoted, number, digits,
  ws_comment, `structural_bitmap`, `quote_parity`, `number_simd`, `decode`, `eisel_lemire`) is
  NEON-first with no V8 equivalent; TS's only analogs are `skipWhitespace`/`skipBlockComments`
  (`utils.ts:159-186`). LAWFUL-NON-MIRROR; this plane is LOCK-9's — 8C's row points there, no
  double-home.
- **CSS parsers.** Rust `parsers/css/*` (media, selector, value, declaration, specificity); TS has
  **no CSS surface** (`proof:no-css-surface`) — value.js owns CSS on the TS side.
  LAWFUL-NON-MIRROR(domain-ownership).

### 2d. Diagnostics & state — MIRRORED, including a mirrored defect

TS diagnostics (`utils.ts`: `diagnosticsEnabled` module-global latch `:6`, collected buffer
`:138-146`) mirror Rust `add_expected`/`add_suggestion`/`add_secondary_span`/
`get_collected_diagnostics`/`clear_collected_diagnostics` (`debug.rs`, `state.rs`). **Both sides
carry the same module-global collected-diagnostics defect** the PT-E1 per-parse-scoping cure
targets — so the fix should be MIRRORED (per-parse scope both sides), and the row records that the
*defect* is isomorphic, not just the API. `ParserState` is LAWFUL-NON-MIRROR(shape): TS
8-mutable-field V8 hidden-class shape (`state.ts:36-53`) vs Rust struct + `PaddedView` + `Span<'a>`
(`state.rs:256,278`).

### 2e. Packrat — DRIFT flagged for adjudication

TS ships opt-in packrat (`packrat.ts`, 488 LOC — WDM + left-recursion memo, armed at first
`memoize()` `packrat.ts:290`). The Rust census surfaces **no packrat module**; Rust leans on arena
(`bump_slab.rs`). Row = **DRIFT — TS-only packrat**, pending LOCK-4/6 adjudication (port to Rust,
or sign LAWFUL-NON-MIRROR because the arena core makes packrat unnecessary there).

### 2f. The regex row — the crux (§3).

---

## 3. The regex correspondence row: two engines, a hazard taxonomy

**The fact.** TS has **no bespoke regex engine** — `regex()` (`leaf.ts:317-369`) drives native JS
`RegExp` via a sticky `/y` clone and `test()` (zero-alloc). Rust drives a **bespoke NFA→DFA
engine** (`parse-that/rust/regex`, 6742 LOC: `classify.rs` 1148, `automata/nfa.rs` 449,
`automata/dfa.rs` 438, `egraph/` HIR canon, `info/`, `first.rs`). These are **different
computational models** — backtracking (JS) vs finite automata (Rust) — so the regex row is **DRIFT
by default**, and 8C's work is (a) enumerating the hazard classes and (b) deciding whether the
divergence can be *dissolved* (→ MIRRORED-modulo-Unicode) or must be *fenced* (→ LAWFUL-NON-MIRROR
via a grammar-surface constraint).

### 3a. The priority-discard, confirmed at source

`automata/nfa.rs:18-32` builds `EpsilonEdge { target, priority: u8 }` with ε-edges "ordered by
priority (lower = preferred)"; `nfa.rs:313` comments "Priority = index → leftmost-first semantics";
greedy/lazy fork on priority at `nfa.rs:326-334`. **But subset construction throws it away:**
`epsilon_closure` (`dfa.rs:189-202`) folds ε-edges into a `BTreeSet<StateId>` — ordered by StateId,
**priority-blind** (line 195 `closure.insert(edge.target)` ignores `edge.priority`);
`subset_construction` (`dfa.rs:217-286`) keys DFA states on that set and marks `is_accept` a plain
bool (`dfa.rs:266`). The DFA is therefore a **recognizer** (accept/reject + anchored scan,
`dfa.rs:141` "anchored shortest"), **not** a leftmost-first submatch engine. This is the mechanical
root of every hazard below and is shared verbatim with LOCK-5.

### 3b. The full hazard-class set (the paired-triple corpus)

Each class = a `(pattern, input)` triple run through three columns: **J** = native JS RegExp
(sticky), **D** = current priority-blind Rust DFA, **P** = a priority-preserving DFA (§4). The
corpus discriminates *which route dissolves the class*.

| # | hazard class | pattern · input | J | D | dissolved by P? |
|---|---|---|---|---|---|
| H1 | greedy vs lazy quantifier | `/a*?b/`·`"aaab"` ; `/<.+?>/`·`"<a><b>"` | lazy-min | longest (priority lost) | **yes** — P restores lazy via thread rank |
| H2 | alternation order (first vs longest) | `/(a\|ab)/`·`"ab"` | `"a"` | `"ab"` | **yes** — P prefers first branch |
| H3 | overlapping-literal set order | any-of `["for","foreach"]`·`"foreach"` | first | leftmost-longest (Aho-Corasick) | **yes** — measure vs `any_span`/`classify.rs` |
| H4 | empty / zero-width loop | `/(?:a?)*b/`·`"b"` ; `/(a*)*/` | terminates-first | loop-guard differs | **partly** — needs empty-loop guard in P |
| H5 | anchoring / sticky default | `/.*c/`·`"aXc"` at offset | leftmost-first-longest | anchored-shortest default (`dfa.rs:141`) | **route-dependent** — match-len policy |
| H6 | Unicode class coverage | `/\w+/`·`"café"` ; `/\p{L}/u` | full Unicode | ASCII+minimal-BMP (`unicode.rs:30`) | **NO** — structural table gap |
| H7 | flags (`i`,`s`,`m`,`u`) | `/a.b/s`·`"a\nb"` | dotall | flag coverage varies | **NO** — flag-support gap, not priority |
| H8 | lookaround (non-regular) | `/a(?=b)/`·`"ab"` ; `/(?<=x)y/` | supported | rejected (`nfa::from_pattern → None`) | **NO** — not a DFA at all |
| H9 | backreference (non-regular) | `/(a)\1/`·`"aa"` | supported | rejected | **NO** — not a DFA at all |

**The taxonomy splits cleanly.** H1–H4 (and H5 modulo policy) are the **regular, priority-only**
classes — a priority-preserving DFA dissolves them. H6–H9 are **not** priority problems: H6/H7 are
table/flag coverage gaps; H8/H9 are non-regular and **no DFA can realize them**. So the regex row
is not a single verdict — it is **MIRRORED on H1–H5 under route-A, LAWFUL-NON-MIRROR
(grammar-fenced) on H6–H9 under route-B**. The routes are complementary, not rival.

### 3c. The recognizer-only fact — decisive for sizing the used-pattern-set

A measured constraint collapses the hazard surface for parse-that's *actual* usage. `regex()`
returns the **matched slice**, not capture values, on both sides: `leaf.rs:85` yields `&'a str`
(the span), `leaf.ts:317` drives sticky `test()`/`exec()` and returns the matched substring — the
combinator layer, not the regex, captures structure. A grep for capture-group *consumption*
(`.exec(`, `$1`, `.groups`, numbered backrefs) over value.js `src/parsing` returns **zero**. So
H4 (value-bearing captures), H8 (lookaround), and H9 (backref) are **outside the used-pattern-set**
— parse-that uses `regex()` purely as a recognizer that reports *extent*. The classes that actually
discriminate parse-that's row are therefore the *extent-affecting* ones: **H1, H2, H3, H5, H10**
(match length under leftmost-first vs longest), plus the coverage gap **H6** on non-ASCII. This is
the decisive input §4's corpus needs: the route-A bucket (priority determinization) covers the
*entire live extent-divergence set*, and route-B's non-regular fence (H8/H9) is needed only if a
grammar literal — not a consumer — introduces lookaround/backref. The census of grammar literals,
not consumer patterns, is thus the true route-B sizer.

---

## 4. Route-A (priority DFA) vs route-B (grammar constraint) — the scratch to build

### Route-A — priority-preserving subset construction (routed to LOCK-5 as a decision)

**Mechanism.** Replace the priority-blind `BTreeSet<StateId>` closure with an **ordered closure**:
traverse ε-edges in priority order, first-visit-wins, key each DFA state on the *ordered sequence*
of NFA states (or the set + a per-thread rank). On reaching an accept NFA state, **truncate
lower-priority threads** (the match-state sentinel) so the highest-priority thread that reached
accept determines the result — realizing leftmost-first. This is the exact discipline of rustc's
`regex-automata` determinizer under `MatchKind::LeftmostFirst` (ordered `StateBuilderNFA` +
match-state insertion dropping lower-priority states), of RE2's Pike-VM ordered thread lists (Russ
Cox), and of Laurikari's TDFA (2000). The prototype forks `dfa.rs:189-286` in a **scratch copy** of
`parse-that/rust/regex` (never the read-only tree), leaving `nfa.rs`'s already-priority-ordered
edges untouched.

**What the scratch must report** (three decision-grade numbers): (i) of H1–H5, how many triples move
`D≠J → P=J` (classes dissolved); (ii) whether the existing `rust/regex` parity suite + `classify.rs`
prefilter stay green (no regression on leftmost-longest paths consumers already rely on); (iii) the
determinization state-count + build-time delta (ordered states can blow up; `opts.state_limit`
`dfa.rs:262` is the guard). The KEEP/REJECT verdict is **LOCK-5's** — 8C supplies the corpus and the
classification, not the engine decision.

### Route-B — build-time grammar-surface constraint

The bootstrapped regex grammar (LOCK-5/7) admits only the DFA-safe sublanguage: lazy quantifiers,
prefix-overlapping alternations, lookaround (H8), backrefs (H9) are either compile errors or route
to a documented native-`RegExp` path. This keeps the DFA a pure recognizer and moves the burden to
the grammar type/lint layer — analogous to tree-sitter resolving lexer conflicts by declared
precedence rather than richer automata.

### The discriminating corpus between A and B

The corpus is the set of consumer + grammar regex literals that are (i) observably `D≠J` today and
(ii) legal under the current grammar. **The regex census decides the mix:** H8/H9 are *non-regular*
— any live lookaround/backref makes route-A provably insufficient there, forcing route-B regardless.
H6 (Unicode) is a table gap route-A cannot touch. So the honest prediction is **hybrid**: route-A
dissolves H1–H5 (adopt priority determinization in `rust/regex`, LOCK-5); route-B fences H6–H9
(grammar surface, LOCK-5/7). The corpus's job is to *size each bucket* from the actual regex
inventory, not to crown one winner.

---

## 5. LOCK-6 coordination (three handshakes)

1. **Ledger ↔ linter-whitelist.** The `LEDGER.tsv` schema *is* LOCK-6A's isomorphism-linter config:
   every LAWFUL-NON-MIRROR row is a whitelist entry (symbol + reason); every MIRRORED row an
   enforced correspondence; every DRIFT row a CI failure until dispositioned. The linter re-runs the
   §1 census and diffs against the committed ledger — a new unclassified symbol on either side fails
   the build. 8C authors the ledger; LOCK-6 owns the linter that consumes it.
2. **Tape-crossing artifact home.** The byte-exact Rust→TS tape decoder (monomorphic, imports *no
   combinators*, oracle `JSON.parse`) is a **LOCK-6 subsystem row, not a combinator row** — 8C's
   ledger points at LOCK-6 for the tape/oracle-table correspondence and does **not** double-home it
   in the combinator census. Combinator backbone and tape decoder are complementary, neither the
   other's precondition.
3. **Decode-speed fence (SPEC §0).** Every perf-bearing ledger row carries `perf_fence ∈
   {pure-js-decode, wasm-boundary}` with its HEAD — a stop-early / decode-throughput number is
   meaningless without stating which path measured it. The ledger metadata makes the fence
   structural, not a footnote.

---

## 6. Consumer liveness (P1/P2) — a re-run census contradicts the seed snapshot

A per-tree import census at *this* HEAD (not the recon snapshot) finds:

- **value.js is now 4.0.0 on branch `tranche-u`** (recon recorded 3.1.0). Its `package.json`
  `dependencies` is **empty**; a `grep` over `src/parsing` returns **zero** `@mkbabb/parse-that`
  imports and zero combinator-symbol references. `node_modules/@mkbabb/parse-that@1.0.0` is present
  but phantom/transitive (glass-ui / keyframes.js also installed). Prior commits *added* parse-that
  ("use @mkbabb/parse-that from npm, drop vendored tarball"; "parser migration"), so the current
  empty state reads as **either a completed migration OFF parse-that or a WIP-branch artifact**
  (active campaign branch, uncommitted `CLAUDE.md` deletion). **This is exactly the P2 question and
  it requires owner confirmation — carry both hypotheses (XL-G).**
- **keyframes.js** depends on `@mkbabb/value.js@4.0.0` (transitive), **not** parse-that directly — so
  its parse-that link is *severed* if value.js has dropped it.
- **latex-paper** is 0.2.1, still pins `@mkbabb/parse-that ^0.7.1` (`<0.8.0`), commits actively but
  on latex/VIRT concerns — **frozen relative to parse-that**, never migrated past 0.7.x. A consumer
  of an *old* cut, not a forward-looking consumer of the 1.x surface; the 0.7→1.1 jump is untested.

**Consequence (feeds 8D + the Y.13 constitution clause):** the only unambiguous live consumer of the
*current* combinator surface may be **parse-that's own test/bench suite** (13 `test/*.test.ts` + the
benchmark matrix). If the owner confirms value.js has left, the PT-N2 "consumer suites green" gate
loses its primary external consumer and the public surface should prune to the actually-live
contract — a real consumer-less-substrate risk for the whole plane. The arity histogram that would
justify the arity-unroll specialization (§2a, 8A) must be rebuilt from whatever live sites remain;
if none, the justification rests solely on parse-that's own benches, and that must be *stated*.

**Spec-fidelity divergence (plane-wide ask):** the recon's "value.js 3.1.0, 59 `all()` sites,
`dispatch()` live" is superseded by "value.js 4.0.0 `tranche-u`, zero parse-that imports in
`src/parsing`." That is the per-family row for the spec re-cut.

---

## 7. Prior art (citations)

- **Russ Cox**, *Regular Expression Matching Can Be Simple And Fast* + *…: the Virtual Machine
  Approach* (swtch.com/~rsc/regexp) — Pike VM, ordered thread lists, leftmost-first via priority; the
  reference model for route-A.
- **rustc `regex-automata`** (BurntSushi/regex) — `MatchKind::LeftmostFirst`; the `determinize`
  module preserves NFA state order + inserts match states to truncate lower-priority threads. The
  canonical *implemented* priority-preserving subset construction; route-A adopts its discipline
  rather than reinventing it.
- **Ville Laurikari**, *NFAs with Tagged Transitions, their Conversion to Deterministic Automata…*
  (2000) — TDFA; priority-preserving determinization with submatch tags.
- **RE2** (Google) — DFA / OnePass / BitState / PikeVM tiers; leftmost-longest default, PCRE
  leftmost-first via the NFA — precedent that no single automaton serves all match semantics (hence
  the §4 hybrid).
- **Owens, Reppy, Turon**, *Regular-expression derivatives reexamined* — derivative-based DFA
  construction preserving alternation priority via ordered derivatives (an alternative to ordered
  subset construction).
- **tree-sitter** — GLR + DFA lexer with declared precedence conflict resolution — the analog of
  route-B's grammar-surface constraint.
- **egg / egglog** (already in `rust/regex/src/egraph`) — HIR canonicalization by e-graph
  saturation; the LOCK-5 continuum's canon side.
- **McKeeman**, *Differential Testing for Software* (1998) — the oracle discipline the §1 harness
  instantiates; `cargo public-api` / TS API-Extractor are the surface-census tools.

---

## 8. Named risks & counterexamples

- **R-C1 (priority-DFA changes match length).** A priority-DFA may shorten a match a consumer
  depends on (leftmost-first ≠ leftmost-longest). *Counterexample:* a grammar rule relying on `D`'s
  current longest match silently reparses. *Cure:* route-A gated behind LOCK-5's `rust/regex` parity
  suite as the floor; corpus includes `D=P` agreement cases a consumer leans on.
- **R-C2 (non-regular escape).** Any live regex using lookaround/backref (H8/H9) makes route-A
  provably insufficient there. *Cure:* census the *actual* regex literals in consumers + bbnf/regex
  grammars for `(?=`/`(?<=`/`\1` **before** choosing; those rows are route-B regardless.
- **R-C3 (ledger drift).** A hand-maintained ledger rots — a new Rust-only variant (e.g.
  `sep_by_ws_until`) with no TS analog goes unclassified. *Cure:* the ledger is GENERATED + CI-diffed
  by LOCK-6's linter; unclassified symbols fail the build (§1).
- **R-C4 (vacuous consumer gate).** If value.js has dropped parse-that, "value.js suite green" passes
  trivially and the combinator surface is externally consumer-less (Y.13 breach). *Cure:* adjudicate
  the consumer set (8D) on the *re-run* census, not the recon snapshot; owner confirms P2.
- **R-C5 (permanent Unicode DRIFT).** H6 is structural: `unicode.rs:30` is ASCII + minimal BMP vs JS
  full Unicode. No construction fixes it; either complete the tables (LOCK-5) or the grammar forbids
  non-ASCII classes (route-B). The row is permanent DRIFT until one lands — do not mislabel it
  MIRRORED.

---

## 9. What a prototype must test to move 8C

1. **Generate `LEDGER.tsv`** from the dual census; report MIRRORED / LAWFUL-NON-MIRROR / DRIFT counts
   and the exact DRIFT rows (the artifact IS the deliverable). Wire it as LOCK-6's linter whitelist
   and prove a synthetic new symbol fails CI.
2. **Run the paired-triple corpus** (H1–H9) through {J, D, P}; tabulate which classes show `D≠J` and
   which move to `P=J`. The table is the regex-row classification evidence.
3. **Build the priority-preserving subset-construction scratch** (fork `dfa.rs:189-286` in a scratch
   `rust/regex` copy): report classes-dissolved, `rust/regex` parity-suite regression, and
   determinization state-count/build-time delta. Route KEEP/REJECT to LOCK-5.
4. **Re-run the consumer import census** over value.js@`tranche-u`, latex-paper, keyframes.js at HEAD;
   produce the arity histogram from live sites (empty if value.js has left) and record the P2 posture
   for owner confirmation. Feed 8A (arity justification) and 8D (consumer-set pruning).
