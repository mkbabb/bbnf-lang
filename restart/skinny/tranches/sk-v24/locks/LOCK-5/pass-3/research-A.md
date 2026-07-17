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
  (`skinny/crates/ir/src/lib.rs:250`). The grammar carries the raw pattern text, nothing more; the
  grammar parser produces it at `skinny/crates/grammar/src/lib.rs:1512`.
- **Engine tier — rich Hir.** `pub enum Hir { Empty, Literal(Vec<u8>), Class(CharClass), Look(Look),
  Repetition, Group, Concat, Alternation }` (`parse-that/rust/regex/src/hir/mod.rs:216`), with
  `CharClass::{Bytes{ranges,negated}, Unicode{ranges,negated}}` (`:45`) and
  `Look::{Start,End,StartLF,EndLF,StartCRLF,EndCRLF}` (`:172`). This is the leaf-grown Hir the
  charter would fold into the bbnf HIR.

Critically, **the two never touch**. Every bbnf-side consumer of a regex goes through a THIRD
object: the `bbnf-regex` shim (`skinny/crates/bbnf-regex/src/lib.rs`, 322 LOC), a standalone
*string-heuristic* analyzer with its own `RegexKind {Whitespace, QuotedString, Numeric, Unknown}`
(`:16`), its own `FirstSet` (`:24`), and a `classify(pattern:&str)` (`:92`) that literally string-
matches: `pattern == r"[ \t\n\r]*"` → Whitespace; `starts_with('"')` → QuotedString;
`starts_with("-?(0|[1-9]") && contains(r"\d"|"[0-9]") && contains("[eE]")` → Numeric; else Unknown
(`:92-105`). It does **not** parse to the engine `Hir`; it never sees `Class` or `Look`. Both `ir`
and `passes` depend on it (`use bbnf_regex::{analyze, …}`). The engine `Hir` lives only in parse-that
and is reached by no bbnf pass. **ONE-HIR is therefore not a refactor of an existing seam — it is the
creation of a seam that does not exist.**

## 1. Q1 — the per-pass dispatch census (built, not estimated)

**bbnf-side arms that touch `ExprKind::Regex` (all treat it as an opaque `String`):**

| Pass / site | path:line | what it does with the regex |
|---|---|---|
| Display/round-trip | `ir/src/lib.rs:171` | `push('/'); push_str(pattern); push('/')` |
| Nullability | `ir/src/lib.rs:331` | `analyze(pattern).nullable` (shim re-parse) |
| Type inference | `passes/src/lib.rs:194` | `Type::Builtin(regex_type(pattern))` (stub, §2) |
| ws structural test | `passes/src/lib.rs:225` | `analyze(pattern).hir.kind == Whitespace` |
| string-decode materialization | `passes/src/lib.rs:348` | `… == QuotedString` guard (shim) |
| `has_regex` termini ×3 | `passes/src/lib.rs:790,814,836` | leaf → `false` |
| First-set | `passes/src/lib.rs:911` | `regex_first_bytes(pattern)` (shim re-parse) |
| Structural walk | `passes/src/lib.rs:1108` | leaf terminus |
| Backend lowering | `passes/src/lib.rs:1341` | `BackendExpr::RegexProgram { pattern, backend_span_kind(pattern) }` |
| Recognizer selection ×2 | `passes/src/lib.rs:1407,1412` | `rule_has_regex_kind(_, SpanKind::String|Number)` |
| Span-kind predicate | `passes/src/lib.rs:1512` | `span_kind(pattern) == kind` (shim) |
| Materialization termini | `passes/src/lib.rs:1562,1605,1638` | leaf terminus |
| local classify | `passes/src/lib.rs:1660-1669` | `span_kind`/`backend_span_kind`: `RegexKind → SpanKind` |

≈ **15 production arms** (12 in `passes`, 3 in `ir`) plus three `ir` test fixtures
(`ir/src/lib.rs:655,661,667`). Every one carries `pattern:&str` and re-derives structure by
re-parsing the string through the 322-LOC shim (`analyze`/`regex_first_bytes`/`span_kind`/
`backend_span_kind`). None dispatches on `Class`/`Look` — the leaves are not in scope.

**Engine-side passes that ALREADY dispatch on the grown leaves** (what ONE-HIR would *relocate* into
bbnf IR): NFA build `build_hir` (`nfa.rs:125`, 8 arms — `Class`→`build_class:179`,
`Look`→`build_look:254`, `Group(sub)`→transparent `build_hir(sub):143`);
`classify_regex_from_hir(&Hir)` (`classify.rs:177`, in a 1148-LOC file); e-graph `simplify_hir`
(`egraph/mod.rs:143`, over `Empty/Literal/Class/Look`); `info/first.rs` (`regex_first_chars`),
`info/width.rs`, `info/one_pass.rs`.

**Verdict — does the classify re-home SHRINK or RELOCATE? It RELOCATES *and FORKS*.** Two independent
lines of evidence:

*(a) upstream — nothing to fold away.* The 1148-LOC classifier is *already* an HIR analysis pass:
`classify_regex_from_hir` (`classify.rs:177`) walks the `Hir` (`try_classify_numeric`,
`_quoted_string`, `_hex`, …). Re-homing it into bbnf IR deletes no line; it drags the whole `Hir` +
`CharClass` + `Look` + `CharSet128` (`sets/charset.rs`) + `ByteRange` into the bbnf `ir` crate and
widens ~15 opaque-string arms.

*(b) downstream — the one coarse fact has been TRIPLICATED, not centralized.* The single datum "which
of {ws,string,number,other} is this pattern" is currently re-encoded across **four enums joined by
three mapping functions**: `RegexKind` (shim, `bbnf-regex:16`) → `SpanKind{String,Number,Whitespace}`
(`ir:440`) via `span_kind`/`backend_span_kind` (`passes:1660-1669`) → `SinkOnlySpanKind{String,
Number,Whitespace}` (`codegen/src/lower/sink_only.rs:103`) via `sink_span_kind(SpanKind)`
(`sink_only.rs:255`) → `ArmKind{String,Number}` (`codegen/src/json_sink_direct/dispatch.rs:88-89`).
First-sets are reimplemented **three times** — `bbnf-regex::first_set` (`lib.rs:120`),
`regex_first_bytes` (`passes:911`), and again `regex_first_bytes(SinkOnlySpanKind, pattern)`
(`codegen/src/json_sink_direct/first_set.rs:97`). Adding a fifth span kind (say an identifier class)
today means editing four enums + three mappers + the hardcoded string `classify`.

**The LOC delta of literal ONE-HIR is positive** (+1148 relocated classify, +~1064 relocated
HIR/parser, ~15 arms widened, **0** removed from the minimal `pattern: String` grammar tier). BUT
part (b) is the concrete, in-substrate ONE-HIR-flavoured win the charter is really after and it is
*measurable*: a single `classify_from_hir(&Hir) → SpanFacts` pass computed once at the leaf collapses
the four enums and three first-set copies into projections of one fact. That collapse — the mirror
taxonomies, not the canonical `Hir` relocation — is where lines actually disappear. The prototype
must report the before/after LOC across the four-enum fork, distinct from the (net-positive) Hir move.

## 2. Q2 — routing `Class`/`Look` typing through the real HM (not the `BuiltinTy` stub)

**The stub is explicit.** `regex_type(pattern) -> BuiltinTy` (`passes/src/lib.rs:224`) returns
`BuiltinTy::Unit` iff `analyze(pattern).hir.kind == RegexKind::Whitespace`, else `BuiltinTy::Span`
(`:226-228`), wired at the type arm `:194`. A two-valued nominal fork over a coarse `BuiltinTy`
(`:246`); the `Type` enum (`:234`) HAS a `Type::Var(layout::types::TypeVarId)` arm (`:236`) — LOCK-1's
unification variable — that the regex path **never produces**. Regex is type-opaque: `[a-z]` and `^`
are indistinguishable (both → `Span`).

**Where ONE-HIR routes it.** Type a regex leaf as *the shape its Hir extracts*: `RegexClass`
(`classify.rs:55`) already carries the structural evidence a width/row judgment needs —
`Numeric{allows_sign, allows_fraction, allows_exponent, reject_leading_zero, allow_leading_dot}` and
`ClassRangeInfo{chars, negated, min, max}` (`classify.rs:36`, the width bounds). The derivation is a
**total functor `RegexClass → Type`** feeding LOCK-1's store in place of the `Unit`/`Span` fork:
`Numeric{…}`→numeric-width type, quoted-string→`Str`, bare literal-prefix class→`Span`, `Look(_)`→a
**zero-width** unit that unifies but projects no captured value. Must be confirmed with LOCK-1's `->`
representation (their Q11): whether the projection edge is a typed edge or an inner-`Map` barrier
decides whether this functor lands in the type layer or one hop below.

**The byte-expansion confinement boundary — the load-bearing answer.** `[α-ω]*` expansion (negation
complement; Unicode→UTF-8 range explosion) is **confined to automata** and MUST stay there. Proof in
the extant code: `CharClass::to_positive_byte_ranges` (`hir/mod.rs:74`), `negate_byte_ranges`
(`:107`), `negate_codepoint_ranges` (`:125`) are called **only** from `NfaBuilder::build_class`
(`nfa.rs`). The `Hir` stores the class *intensionally* — `Bytes{ranges, negated}` keeps the flag
rather than materializing the 256-wide complement (`hir/mod.rs:1` states this is the whole departure
from `regex_syntax`). So the type plane sees only `Class(CharClass)` (intensional ranges + flag +
cardinality count); only the NFA `ByteSet` holds the expanded membership. **ONE-HIR is sound on
typing iff it preserves this.** The hazard it *introduces*: a bbnf IR pass that can now see `Class` is
tempted to expand it for a first-set or width query, leaking the 256-blowup into the type layer — a
regression the two-type split structurally prevents (the grammar tier cannot see `Class` at all),
and ONE-HIR can prevent only by lint/discipline (weaker). Confinement test: `negate_byte_ranges` /
`ByteSet::insert` must be unreachable from `passes::infer_expr`.

## 3. Q3 — automata parity re-derived from the narrowed type (`from_expr` ≡ `from_hir`)

**`from_expr` does not exist.** Grep over `parse-that/rust/regex/src` yields only `Nfa::from_pattern`
(`nfa.rs:63`, = `parse` then `from_hir`) and `Nfa::from_hir` (`nfa.rs:71`); no constructor consumes a
bbnf IR node. And the skinny substrate has **no automata at all** — `RegexHir` stops at the 4-variant
`RegexKind`; there is no NFA/DFA/minimizer in `bbnf-regex`. So parity is neither a rename nor an
inheritance: it is a *new* `from_expr` proven byte-identical to the incumbent `from_hir`.

**The parity obligations are small and named.** `build_hir` (`nfa.rs:125`) is an 8-arm structural
fold: `Empty`→single state; `Literal`→byte chain (`ByteSet::singleton`); `Class`→`build_class:179`;
`Look`→`build_look:254`; `Repetition`→`build_repetition`; **`Group(sub)`→transparent `build_hir(sub)`
(`nfa.rs:143`, Group elision)**; `Concat`→ε-chained fragments with priority-tagged
`EpsilonEdge{target,priority}` (`add_epsilon:118`); `Alternation`→ε-split. The two directive-named
hazards are both live: (a) **Group-elision** — bbnf HIR has no capture-group node, so `from_expr`
must elide exactly as `nfa.rs:143` or introduce a spurious state; (b) **`Optional ↔ Repeat{0,1}`** —
bbnf `ExprKind::Optional(body)` (`ir:245`) and `ExprKind::Repeat{min:0,max:Some(1)}` (`ir:240`) are
distinct nodes that must both canonicalize to regex `Repetition{min:0,max:Some(1),greedy:true}` and
yield an identical fragment (same ε-priority order), or the NFA state count diverges.

**Prototype gate = a byte-identity harness.** For each pattern build the `Hir` via the engine parser
AND the bbnf-IR node via grammar lowering, then assert `Nfa::from_hir(hir).states ==
Nfa::from_expr(ir).states` state-for-state, including each transition `ByteSet` and each priority
epsilon. Throughput is **derived fresh** (no inherited band): run `regex_dfa_compile` (full
parse→HIR→NFA→DFA→minimize + `find_at`, `parse-that/rust/regex/benches/`) on both constructors — a
different NFA shape changes start-state entry-byte sets and thus prefilter tiering (PROVE-10's
`Dfa::find`, certified 8.8–29.5× on prefilterable / 0.97–1.01× controls, is a *canonical-engine*
number the narrowed-type NFA must re-earn).

## 4. Q4 — a Look-consumer test that FAILS today (a real defect, not a hypothetical)

**Finding: the `Look` leaf is grown but its consumer is a no-op — the automaton silently drops
anchors.** `build_look` (`nfa.rs:254`) matches every variant `Look::{Start|End|StartLF|…|EndCRLF}`
to a single zero-width state with `start == accept` — it emits no assertion and checks no position.
`automata/dfa.rs` has **zero** anchor logic (grep for `Look`/`anchored`/`Start`/`End` in `dfa.rs`
returns nothing). Consequence:

- `/foo$/` → `Concat[Literal("foo"), Look::End]` → NFA where `Look::End` is a bare ε-state → a DFA
  that accepts `"foo"` as a *prefix* of `"foobar"`. **`/foo$/` wrongly accepts `"foobar"`.**
- `/foo/` (unanchored) correctly accepts `"foobar"`; the engine cannot distinguish the two — the `$`
  is compiled away. The skinny shim is worse still: `RegexKind` cannot even *represent* an anchor, so
  `/foo$/` classifies `Unknown` and the `$` vanishes one layer earlier.

The assertion to exercise (the directive's "test that can FAIL", already failing in the substrate):
```
assert!( Dfa::compile("foo").find("foobar").is_some() );   // passes (correct)
assert!( Dfa::compile("foo$").find("foobar").is_none() );  // FAILS today — anchor dropped
```
**Bearing on ONE-HIR:** growing `Look` into the shared type is *worse than useless until its consumer
exists* — a leaf with no consuming pass violates the constitution's consumer-less-substrate
prohibition (Y.13). Any design admitting `Look` (ONE-HIR or two-type alike) MUST land, same wave, a
`LookMatcher` consulted at DFA search time — the `regex-automata` pattern: a `Look` bitset checked
against the byte before/after the position (`Look::End` legal only at `pos == len`, before `\n` for
`EndLF`). This is a **named gap blocking any regex type carrying anchor semantics**, independent of
the type-plane debate — the grammar's `$`-anchored and line-anchored leaves are presently
mis-compiled.

## 5. Prior art (citations)

- **rust `regex-automata` (BurntSushi).** The canonical *multi*-representation design:
  `regex_syntax::hir::Hir` is a typed IR *distinct* from the parser AST; the `thompson::NFA` compiler
  consumes it; `Look` is a first-class HIR node enforced at search time by `LookMatcher` against
  surrounding bytes — the very consumer `build_look` (`nfa.rs:254`) omits. The meta engine tiers
  literal-prefilter → lazy/hybrid DFA → one-pass DFA → backtracker
  (`sk-v24/research/R7-sota-sweep.md`). Lesson for 5A: even BurntSushi keeps HIR *separate* from both
  the parser AST and the analysis structs — a functor, not a single type. Evidence *against* literal
  ONE-HIR and *for* the ratified two-type (5B-shaped) split.
- **regex-syntax intensional-vs-eager classes.** Upstream materializes class complements eagerly; the
  bespoke engine deliberately does not (`hir/mod.rs:1`, the `negated` flag) — the mechanism that keeps
  byte-expansion out of the type (§2).
- **egg / equality saturation.** The engine's `egraph/` (1253 LOC) already treats `Hir` as an e-graph
  language and canonicalizes via `simplify_hir` (`egraph/mod.rs:143`) over `Empty/Literal/Class/Look`,
  with a per-compile cache keyed on `Hir: Eq+Hash` (`hir/mod.rs:215`). The "shared rewrite library" a
  lawful-functor design keeps common while each side holds a domain-fit type — the shared part is the
  *rewrite algebra*, not the *type*.
- **Thompson construction / Group transparency.** `build_hir` (`nfa.rs:125`) is textbook Thompson with
  transparent capture groups (`:143`); Group-elision and `Optional↔Repeat{0,1}` are the standard
  NFA-equivalence pitfalls (§3). tree-sitter compiles token/lexer regex fragments through one
  automaton pipeline — the precedent for "regex is a sub-language of the grammar's IR."

## 6. Named risks & counterexamples to ONE-HIR (family-honest)

1. **Positive LOC delta on the Hir move (§1a).** Relocating ~2200 LOC (classify + HIR/parser) into
   bbnf `ir` and widening ~15 opaque-string arms removes nothing; `pattern: String` is already
   minimal. The genuine line savings live in collapsing the four-enum span-kind fork (§1b), which is
   available under the two-type split too (the bridge calls the engine classifier once).
2. **Byte-expansion leak (§2).** A shared type exposing `Class` to bbnf IR passes invites a first-set
   or width query to expand the 256-complement into the type layer. Two-type forbids structurally;
   ONE-HIR only by discipline.
3. **The `Look` consumer gap (§4) is orthogonal to the type debate** yet must close regardless — a
   live correctness bug (`/foo$/` accepts `"foobar"`). Growing `Look` into *any* shared type without a
   `LookMatcher` violates Y.13.
4. **Cross-crate `Hir` ownership inversion.** Folding the engine `Hir` into bbnf `ir` inverts the
   dependency (today `ir`→`bbnf-regex`); parse-that — the "one regex home" (constitution ii) — would
   then depend on bbnf's IR crate for its own core type, a layering the union architecture resists.
5. **The 322-LOC shim is a THIRD representation** that string-matches three hardcoded JSON patterns
   (`classify:92`) and degrades every real pattern to `Unknown`. Whichever type-plane wins, its
   `classify`/`first_set`/`nullable` should be replaced by real `Hir` analysis (call
   `classify_regex_from_hir`). This is the largest genuine simplification available — and it is
   available under the two-type split, so it is not ONE-HIR-exclusive.

## 7. What a prototype MUST test to move family 5A

1. **The Look-consumer harness (highest value; unblocks any anchor semantics):** implement a
   `LookMatcher` consulted in `Dfa::find`, prove `/foo$/` rejects `"foobar"` and `/foo/` accepts, with
   `Look::Start`/`End` actually read. GO/refute for growing `Look` at all.
2. **The four-enum collapse, measured (§1b):** implement `classify_from_hir(&Hir) → SpanFacts` once at
   the leaf; rewrite `SpanKind`/`SinkOnlySpanKind`/`ArmKind` and the three `regex_first_bytes` copies
   as projections; report exact before/after LOC — this is where ONE-HIR *shrinks*, distinct from the
   net-positive Hir relocation.
3. **The `from_expr` byte-identity harness (§3):** build `Nfa::from_expr(bbnf-IR)` and assert
   state-for-state equality with `Nfa::from_hir` over a tagged corpus (≥50, acyclic/cyclic),
   exercising Group-elision and `Optional↔Repeat{0,1}`; carry fresh `regex_dfa_compile` throughput.
4. **The typing functor (§2):** replace `regex_type`'s `Unit`/`Span` fork (`passes:224`) with a total
   `RegexClass → HM-Type` map producing `Type::Var` into LOCK-1's store, and lint that `[α-ω]*`
   expansion never appears in the resulting type (the confinement invariant). Coordinate `->` with
   LOCK-1.
