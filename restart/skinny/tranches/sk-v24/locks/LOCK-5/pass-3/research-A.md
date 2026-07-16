# LOCK-5 · Family 5A — ONE-HIR · pass-3 research

Family charter: the regex HIR is a sub-language of the bbnf HIR — one type in one home; regex
parsing rides the bootstrapped grammar; automata construction consumes bbnf IR; classify becomes
an IR analysis pass. This pass answers the LEAF-GROWTH questions (per-pass dispatch census; typing
through the real HM; automata parity re-derived from the narrowed type; a Look-consumer test that
can fail) against the concrete substrate, and reports where ONE-HIR strengthens and where it is
refuted by measured cost.

## 0. The substrate as it stands (two disjoint regex worlds, plus a shim)

There are **two independent regex representations today**, and they never meet in a shared type:

- **Grammar tier — opaque string.** `ExprKind::Regex { pattern: String }`
  (`skinny/crates/ir/src/lib.rs:250`). The grammar carries the raw pattern text, nothing more. The
  grammar parser produces it at `skinny/crates/grammar/src/lib.rs:1510` (`parse_regex`).
- **Engine tier — rich Hir.** `pub enum Hir { Empty, Literal(Vec<u8>), Class(CharClass), Look(Look),
  Repetition, Group, Concat, Alternation }` (`parse-that/rust/regex/src/hir/mod.rs:216`), with
  `CharClass::{Bytes{ranges,negated}, Unicode{ranges,negated}}` (`:45`) and
  `Look::{Start,End,StartLF,EndLF,StartCRLF,EndCRLF}` (`:172`). This is the leaf-grown Hir the
  charter would fold into the bbnf HIR.

Critically, **the two never touch**. Every bbnf-side consumer of a regex goes through a THIRD
object: the `bbnf-regex` shim crate (`skinny/crates/bbnf-regex/src/lib.rs`, 322 LOC), a standalone
*string-heuristic* analyzer with its own `RegexKind {Whitespace, QuotedString, Numeric, Unknown}`
(`:16`), its own `FirstSet` (`:24`), and its own `classify(pattern:&str)` that does prefix
string-matching (`:92`), `first_from_prefix` (`:142`). It does **not** parse to the engine `Hir`; it
never sees `Class` or `Look`. Both `ir` and `passes` depend on this shim (`ir/Cargo.toml:10`,
`passes/Cargo.toml:10`; `use bbnf_regex::{analyze,FirstSet,RegexKind}` at `passes/src/lib.rs:1`). The
engine `Hir` lives only in parse-that and is reached by no bbnf pass. **ONE-HIR is therefore not a
refactor of an existing seam — it is the creation of a seam that does not exist.**

## 1. Q1 — the per-pass dispatch census (built, not estimated)

**bbnf-side arms that touch `ExprKind::Regex` (all treat it as an opaque `String`):**

| Pass / site | path:line | what it does with the regex |
|---|---|---|
| Display/round-trip | `ir/src/lib.rs:171` | `push('/'); push_str(pattern); push('/')` |
| Nullability | `ir/src/lib.rs:331` | `analyze(pattern).nullable` (shim re-parse) |
| Type inference | `passes/src/lib.rs:194` | `Type::Builtin(regex_type(pattern))` (stub, §2) |
| Structural predicate | `passes/src/lib.rs:348` | pattern → span/kind test (shim) |
| `has_regex` guards ×3 | `passes/src/lib.rs:790,814,836` | leaf terminus → `false` |
| First-set | `passes/src/lib.rs:911` | `regex_first_bytes(pattern)` (shim re-parse) |
| Structural walk | `passes/src/lib.rs:1108` | leaf terminus |
| Backend lowering | `passes/src/lib.rs:1341` | `BackendExpr::RegexProgram { pattern.clone(), backend_span_kind(pattern) }` |
| Span-kind predicate | `passes/src/lib.rs:1512` | `span_kind(pattern) == kind` (shim) |
| Materialization guards | `passes/src/lib.rs:1562,1605,1638` | leaf terminus |

≈ **15 production arms** (12 in `passes`, 3 in `ir`) plus three `ir` test fixtures
(`ir/src/lib.rs:655,661,667`). **Every one carries `pattern:&str` and re-derives structure by
re-parsing the string through the 322-LOC shim** (`analyze`/`regex_first_bytes`/`span_kind`/
`backend_span_kind`). None dispatches on `Class`/`Look` — the leaves are not in scope.

**Engine-side passes that ALREADY dispatch on the grown leaves** (what ONE-HIR would *relocate* into
bbnf IR): NFA build `build_hir` (`parse-that/rust/regex/src/automata/nfa.rs:125`, 8 arms,
`Class`→`build_class:179`, `Look`→`build_look:254`); `classify_regex_from_hir` (`classify.rs:177`,
a 1148-LOC file); e-graph `simplify_hir` (`egraph/mod.rs:143`, over `Empty/Literal/Class/Look`);
`info/first.rs` (`regex_first_chars`), `info/width.rs`, `info/one_pass.rs`.

**Verdict on "does classify re-home SHRINK or RELOCATE": it RELOCATES.** The 1148-LOC classifier is
*already* an HIR analysis pass — `classify_regex_from_hir` (`classify.rs:177`) walks the `Hir`
(`try_classify_numeric`, `_quoted_string`, `_hex`, …); there is nothing to fold away by moving it.
Re-homing it into bbnf IR deletes no line; it drags the whole `Hir` + `CharClass` + `Look` +
`CharSet128` (`sets/charset.rs`) + `ByteRange` into the bbnf `ir` crate, and forces the ~15
opaque-string arms above to each **grow** a `Class`/`Look` case (or keep a delegated sub-analysis —
no net simplification). **The LOC delta of ONE-HIR is positive:** +1148 relocated classify, +1064
relocated HIR/parser (`hir/` per the engine map), ~15 arms widened, against **0** lines removed from
the grammar tier (`pattern: String` is the minimal representation and cannot shrink). This is the
measured case *for* the ratified two-type split: the opaque grammar tier collapses 15 arms into
"call the bridge", and the rich Hir confines `Class`/`Look` to the ~4 engine passes that genuinely
branch on them.

## 2. Q2 — routing `Class`/`Look` typing through the real HM (not the `BuiltinTy` stub)

**The stub is explicit.** `regex_type(pattern) -> BuiltinTy` (`passes/src/lib.rs:224`) returns
`BuiltinTy::Unit` iff `analyze(pattern).hir.kind == RegexKind::Whitespace`, else `BuiltinTy::Span`.
A two-valued nominal classifier over a coarse `BuiltinTy {Unit,Bytes,Span,Str,F64,U8,Bool}` (`:246`);
`Type` (`:234`) is a closed enum whose `Var` arm the regex path never produces — no variables, no
rows, no widths.

**Where ONE-HIR routes it.** LOCK-1's HM (row-poly + widths) would type a regex leaf as *the shape
its Hir extracts*: `classify_regex_from_hir` → `RegexClass` (`classify.rs:55`) already carries the
structural evidence a width/row judgment needs — `RegexClass::Numeric {allows_sign, allows_fraction,
allows_exponent, reject_leading_zero, allow_leading_dot}` and `ClassRangeInfo {chars, negated, min,
max}` (`classify.rs:36`, the width bounds). The typing derivation is a **total functor `RegexClass →
Type`** feeding LOCK-1's constraint store in place of the `Unit`/`Span` fork: `Numeric{…}`→numeric
width type, quoted-string Hir→`Str`, bare literal-prefix class→`Span`. This must be confirmed with
LOCK-1 as the concrete `->` representation (their Q11): whether the projection edge is a typed edge
or an inner-`Map` barrier decides whether this functor lands in the type layer or one hop below it.

**The byte-expansion confinement boundary — the load-bearing answer.** `[α-ω]*` expansion (negation
complement; Unicode→UTF-8 range explosion) is **confined to automata construction** and must NEVER
enter the shared type. Proof in the extant code: `CharClass::to_positive_byte_ranges`
(`hir/mod.rs:74`), free `negate_byte_ranges` (`:107`) and `negate_codepoint_ranges` (`:125`) are
called **only** from `NfaBuilder::build_class` (`nfa.rs:182,199`). The `Hir` stores the class
*intensionally* — `Bytes{ranges, negated}` keeps the flag rather than materializing the 256-wide
complement (the module header at `hir/mod.rs:1` states this is the whole departure from
`regex_syntax`). So the type plane only ever sees `Class(CharClass)` (an intensional range set + a
flag); only the NFA `ByteSet` holds the expanded 256-membership. **ONE-HIR is sound on typing iff it
preserves this: the shared bbnf HIR carries the intensional `CharClass`; expansion stays behind
`build_class`.** The hazard it *introduces*: a bbnf IR pass that can now see `Class` is tempted to
expand it for a first-set or width query, leaking the 256-blowup into the type layer — a regression
the two-type split structurally prevents (the grammar tier cannot see `Class` at all).

## 3. Q3 — automata parity re-derived from the narrowed type (`from_expr` ≡ `from_hir`)

**`from_expr` does not exist.** Grep over `parse-that/rust/regex/src` yields only `Nfa::from_pattern`
(`nfa.rs:63`) and `Nfa::from_hir` (`nfa.rs:71`); no constructor consumes a bbnf IR node. ONE-HIR's
"automata construction consumes bbnf IR" is a *new* constructor to build and prove byte-identical to
the incumbent — not a rename.

**The parity obligations are small and named.** `from_hir`→`build_hir` (`nfa.rs:125`) is an 8-arm
structural fold: `Empty`→single state; `Literal`→byte chain (`build_literal:156`,
`ByteSet::singleton`); `Class`→`build_class:179`; `Look`→`build_look:254`;
`Repetition`→`build_repetition`; **`Group(sub)`→transparent `build_hir(sub)` (`nfa.rs:143` — Group
elision)**; `Concat`→ε-chained fragments with priority-tagged `EpsilonEdge{target,priority}`
(`build_concat:274`, `add_epsilon:118`); `Alternation`→ε-split (`build_alternation:298`). The two
directive-named parity hazards are both live: (a) **Group-elision** — bbnf HIR has no capture-group
node, so `from_expr` must elide exactly as `nfa.rs:143`, or introduce a spurious state; (b)
**`Optional ↔ Repeat{0,1}`** — bbnf `ExprKind::Optional(body)` must lower to the same fragment as
regex `Repetition{min:0, max:Some(1)}`, or the NFA state count diverges.

**Prototype gate = a byte-identity harness.** For each pattern, build the `Hir` via the engine parser
AND the bbnf-IR node via grammar lowering, then assert `Nfa::from_hir(hir).states ==
Nfa::from_expr(ir).states` state-for-state, including each transition `ByteSet` and each priority
epsilon. Throughput is **derived fresh from the narrowed type** (no inherited band): run
`regex_dfa_compile` (full parse→HIR→NFA→DFA→minimize + `find_at`, `parse-that/rust/regex/benches/`)
on both constructors. Only this reveals whether feeding bbnf IR — which may present
`Optional`/`Repeat` differently — perturbs the DFA the incumbent produces.

## 4. Q4 — a Look-consumer test that FAILS today (a real defect, not a hypothetical)

**Finding: the `Look` leaf is grown but its consumer is a no-op — the automaton silently drops
anchors.** `build_look` (`nfa.rs:254`) matches every `Look` variant (`Start | End | StartLF | … |
EndCRLF`) to a single zero-width state with `start == accept` — it emits no assertion and checks no
position. And `automata/dfa.rs` has **zero** anchor-consulting logic (grep for
`Look`/`anchored`/`Start`/`End` in `dfa.rs` returns nothing). Consequence:

- `/foo$/` → `Concat[Literal("foo"), Look::End]` → NFA where `Look::End` is a bare ε-state → a DFA
  that accepts `"foo"` as a *prefix* of `"foobar"`. **`/foo$/` wrongly accepts `"foobar"`.**
- `/foo/` (unanchored) correctly accepts `"foobar"`. The engine cannot distinguish the two — the `$`
  is compiled away.

The assertion to exercise (the directive's "test that can FAIL", already failing in the substrate):
```
assert!( Dfa::compile("foo").find("foobar").is_some() );   // passes (correct)
assert!( Dfa::compile("foo$").find("foobar").is_none() );  // FAILS today — anchor dropped
```
**Bearing on ONE-HIR:** growing `Look` into the shared type is *worse than useless until its consumer
exists* — a leaf with no consuming pass violates the constitution's consumer-less-substrate
prohibition (Y.13). Any design admitting `Look` (ONE-HIR or two-type alike) MUST land, same wave, a
`LookMatcher` consulted at DFA search time (the regex-automata pattern: a `Look` bitset checked
against the byte before/after the position). This is a **named gap blocking any regex type carrying
anchor semantics**, independent of the type-plane debate — the grammar's `$`-anchored and
line-anchored leaves are presently mis-compiled.

## 5. Prior art (citations)

- **rust `regex-automata` (BurntSushi).** The canonical *multi*-representation design:
  `regex_syntax::hir::Hir` is a typed IR *distinct* from the parser AST; the `thompson::NFA` compiler
  consumes it; `Look` is a first-class HIR node (a `hir::Look` bitset) enforced at search time by
  `LookMatcher::matches_set` against surrounding bytes — the very consumer `build_look`
  (`nfa.rs:254`) omits. The meta engine tiers literal-prefilter → lazy/hybrid DFA → one-pass DFA →
  backtracker (`sk-v24/research/R7-sota-sweep.md:85`). Lesson for 5A: even BurntSushi keeps HIR
  *separate* from both the parser AST and the analysis structs — a functor, not a single type. This
  is evidence *against* literal ONE-HIR and *for* the ratified two-type (5B-shaped) split.
- **regex-syntax intensional-vs-eager classes.** Upstream materializes class complements eagerly; the
  bespoke engine deliberately does not (`hir/mod.rs:1`, the `negated` flag) — the mechanism that keeps
  byte-expansion out of the type (§2). The departure is the right one.
- **egg / equality saturation.** The engine's `egraph/` (1253 LOC) already treats `Hir` as an e-graph
  language and canonicalizes via `simplify_hir` (`egraph/mod.rs:143`) over `Empty/Literal/Class/Look`,
  with a per-compile `simplify_hir_cached` (`:171`) keyed on `Hir: Eq+Hash` (`hir/mod.rs:215`). This
  is the "shared rewrite library" a lawful-functor design keeps common while each side holds a
  domain-fit type — the shared part is the *rewrite algebra*, not the *type*.
- **Thompson construction / Group transparency.** `build_hir` (`nfa.rs:125`) is textbook Thompson with
  transparent capture groups (`:143`); Group-elision and `Optional↔Repeat{0,1}` are the standard
  NFA-equivalence pitfalls (§3).

## 6. Named risks & counterexamples to ONE-HIR (family-honest)

1. **Positive LOC delta (§1).** ONE-HIR relocates ~2200 LOC (classify + HIR/parser) into bbnf `ir`
   and widens ~15 opaque-string arms; it removes nothing. The `pattern: String` grammar tier is
   already minimal.
2. **Byte-expansion leak (§2).** A shared type exposing `Class` to bbnf IR passes invites a first-set
   or width query to expand the 256-complement into the type layer. The two-type split forbids this
   structurally; ONE-HIR must forbid it only by discipline (weaker).
3. **The `Look` consumer gap (§4) is orthogonal to the type debate** yet must close regardless — it is
   a live correctness bug (`/foo$/` accepts `"foobar"`). Growing `Look` into *any* shared type without
   a `LookMatcher` violates Y.13.
4. **Cross-crate `Hir` ownership inversion.** Folding the engine `Hir` into bbnf `ir` inverts the
   dependency (today `ir`→`bbnf-regex`); parse-that — the "one regex home" (constitution ii) — would
   then depend on bbnf's IR crate for its own core type, a layering the union architecture resists.
5. **The 322-LOC shim is a THIRD representation** neither ONE-HIR nor the engine `Hir` accounts for.
   Whichever type-plane wins, `bbnf-regex/src/lib.rs`'s string-heuristic
   `classify`/`first_set`/`nullable` (`:92,:142,:107`) should be replaced by real `Hir` analysis
   (call `classify_regex_from_hir` rather than prefix-matching strings). This is the genuine
   simplification available — and it is available under the two-type split too (the bridge just calls
   the engine classifier), so it is not a ONE-HIR-exclusive win.

## 7. What a prototype MUST test to move family 5A

1. **The Look-consumer harness (highest value; unblocks any anchor semantics):** implement a
   `LookMatcher` consulted in `Dfa::find`, prove `/foo$/` rejects `"foobar"` and `/foo/` accepts,
   with `Look::Start`/`End` actually read. GO/refute for growing `Look` at all.
2. **The `from_expr` byte-identity harness (§3):** build `Nfa::from_expr(bbnf-IR)` and assert
   state-for-state equality with `Nfa::from_hir` over a tagged corpus, exercising Group-elision and
   `Optional↔Repeat{0,1}`; carry fresh `regex_dfa_compile` throughput, no inherited band.
3. **The typing functor (§2):** replace `regex_type`'s `Unit`/`Span` fork (`passes/src/lib.rs:224`)
   with a total `RegexClass → HM-Type` map feeding LOCK-1's store, and confirm `[α-ω]*` expansion
   never appears in the resulting type (the confinement invariant). Coordinate the `->` representation
   with LOCK-1.
4. **The census delta measured, not asserted:** stand up a scratch branch folding the engine `Hir`
   into bbnf `ir`, count arms actually widened + LOC moved, to confirm §1's positive-delta prediction
   against the ratified two-type baseline.
