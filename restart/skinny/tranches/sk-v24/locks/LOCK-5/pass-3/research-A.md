# LOCK-5 · Family 5A (ONE-HIR / LEAF-GROWTH) — pass-3 research

Charter reframed by the ratified two-type settlement: the grammar tier keeps a regex leaf
**opaque**; the engine tier keeps a **rich `Hir` grown with `Class`/`Look`**; a cheap
`string`/`format!` bridge (reusing the engine's `simplify_hir`/`canonical_pattern`) joins
them. Family 5A's residual thesis is therefore not "one physical type" but **one lawful
sub-language**: `Class`/`Look` grow on the engine `Hir`, and the question is whether that
growth stays confined to automata or leaks into the shared plane, and whether `classify`
re-homes as an IR analysis pass with a real LOC win. This report makes the mechanism
concrete against the two homes and answers the LEAF-GROWTH questions Q1–Q4.

## The two homes, as they physically stand

**Grammar tier (skinny, mutable).** The regex leaf lives in the backend IR as an opaque
program: `ir::BackendExpr::RegexProgram { pattern, span_kind }` at
`skinny/crates/ir/src/lib.rs:389` (enum `BackendExpr` at `:376`; sibling leaf `ByteLiteral`
`:388`). The pattern travels as a **string**; the only structured payload is
`span_kind: SpanKind` (`:440`), a coarse tag (String/Number/…), **not** a byte-expanded
class. No `Class`, `Look`, `ByteClass`, `RegexId`, or `from_expr` exists anywhere in
`crates/{ir,codegen,passes,grammar}` (grep: zero hits) — the grammar plane has never seen a
grown leaf.

**Engine tier (parse-that `rust/regex`, READ-ONLY canonical, 6742 LOC).** The rich `Hir`
(`src/hir/mod.rs:216`) already carries the grown leaves: `Empty`, `Literal(Vec<u8>)`,
`Class(CharClass)`, `Look(Look)`, `Repetition`, `Group(Box<Hir>)`, `Concat`, `Alternation`.
`Look` (`:172`) is a 6-arm enum: `Start`/`End`/`StartLF`/`EndLF`/`StartCRLF`/`EndCRLF`.
`CharClass` (`:45`) holds `ByteRange`/`CodepointRange` sets. So the leaves are *already grown*
on the engine — 5A's work is not to invent them but to (a) census who dispatches on them,
(b) confine their byte-expansion, (c) prove a bbnf-sourced constructor is byte-identical, and
(d) prove `Look` is actually consumed.

**Substrate caveat (names a real gap in the charter's premise).** The "vendored
parse-that-regex" that XL-H designates the MEASUREMENT SUBSTRATE
(`skinny/crates/parse-that-regex/src/lib.rs`, 1419 LOC) is **not** the automata engine — it
is the string/number primitive slice: `match_string`/`match_string_at_quote`,
`validate_number`, `unescape_string`, `StringMode`/`StringFlags`, plus `number/` (Eisel–
Lemire) and `unicode/` (Höhrmann UTF-8). It contains **no `Hir`, no `Class`/`Look`, no
`classify`, no `nfa`/`dfa`**. Consequently every Q3/Q4 automata harness and every `Class`/
`Look` throughput number must be base-pinned against the **native `bbnf-regex`** (READ-ONLY)
on a scratch copy (rsync + external-patch repoint + isolated `CARGO_TARGET_DIR`), never
against the vendored substrate. Any pass-3 prototype that claims a "vendored" automata number
is measuring the wrong crate.

## Q1 — the per-pass dispatch census (built, not estimated)

There are **two disjoint dispatch surfaces**; the settlement pins the growth to exactly one.

**Engine surface — the arms that DO carry `Class`/`Look` (native `bbnf-regex`).** Enumerated,
each an exact site:

| Pass | File:line | `Class` arm | `Look` arm |
|---|---|---|---|
| NFA build | `automata/nfa.rs:135` | `Hir::Class → build_class` (`:180`) | `Hir::Look → build_look` (`:254`) |
| first-sets | `first.rs` | class → `CharSet128` from ranges | `:79` `Look(_) → CharSet128::new()`; `:156` `Look(_) → true` (nullable) |
| anchor info | `info/classify.rs:130` | — | `:133` `Look(Start\|End) → true`; `detect_anchored` walks `Concat` first/last (`:137`,`:142`) |
| one-pass | `info/one_pass.rs` | `:40` class is one-pass leaf | `:22`,`:40` `Look(_) → eligible` |
| e-graph node | `egraph/translate.rs:27/98` | class ↔ `HirENode::Class` | `Look ↔ HirENode::Look` round-trip |
| e-graph split | `egraph/mod.rs:195` | `Class` non-splittable leaf | `Look` non-splittable leaf |
| taxonomy | `classify.rs` (1148 LOC) | `RegexClass::CharClassQuantified(ClassRangeInfo)`, `HexDigits`, `Identifier`, `PrefixThenClass{prefix,tail}` | anchoring folded via `detect_anchored`, not a `RegexClass` variant |

`build_look` (`nfa.rs:254`) collapses **all six** `Look` arms into one zero-width state
(`start == accept`) — the automata treat every anchor identically (see Q4, this is
load-bearing). `build_repetition` (`nfa.rs:326`) fans on `(min,max)`: `(0,Some(1))→
build_optional`, `(0,None)→build_star`, `(1,None)→build_plus`, `(n,None)→build_at_least`,
`(n,Some(m))→build_bounded` (`:329-334`). `Group` is elided transparently (`:143`
`Hir::Group(sub)→build_hir(sub)`).

**Grammar/BIR surface — the arms that DO NOT (skinny).** Every skinny pass sees the leaf only
as opaque `RegexProgram`, one arm apiece:
`crates/codegen/src/lower/sink_only.rs:206` (`RegexProgram{pattern,span_kind}` →
`SinkOnlyExpr`), `crates/codegen/src/lower/tape_plan.rs:79` (`render_expr`),
`crates/codegen/src/lib.rs:1151` (`strip_direct_builds` no-op arm),
`crates/codegen/src/general_descent/regex_first.rs` (leading-byte-set derivation from the
pattern), `crates/codegen/src/json_sink_direct/{first_set.rs:5,dispatch.rs,skeleton.rs:143,
mod.rs:148}`. Roughly **seven** BIR dispatch sites, each a single opaque arm.

**Answer to the sharp sub-question ("does classify re-home SHRINK or merely RELOCATE?").**
Under the settlement it does **neither** on the BIR side: because the grammar tier keeps the
leaf opaque, the ~7 skinny dispatch sites gain **zero** new `Class`/`Look` arms — the LOC
delta on the grammar plane is **+0**. `classify` stays entirely engine-homed and is consumed
across the bridge by string (`RegexClass::canonical_pattern() -> Option<&'static str>`,
`classify.rs:132`, e.g. feeding `key_class_regex_pattern`). So the honest census finding is:
the two-type split is precisely what makes leaf-growth cheap — growth is **confined to the ~8
engine arms above**, and the ~7 BIR arms are insulated by construction. The 5A "classify
becomes an IR analysis pass" ambition is only realizable if `classify` is re-expressed as an
`Hir`-walk pass on the **engine** IR (which `info/classify.rs` + `classify.rs` already are —
they are pure structural walks, not string re-parses); re-homing them under a shared
`egraph`-based analysis pass RELOCATES 1148+153 LOC without shrinking it (the taxonomy arms
are irreducible domain knowledge). The measured claim a prototype must make is the LOC-delta
of that relocation, not a one-liner assertion — and the honest prior is ≈0 net shrink.

## Q2 — routing `Class`/`Look` through LOCK-1's real HM (not a `BuiltinTy` stub)

The confinement boundary is clean and does not need a stub type. The byte-expansion of
`[α-ω]*` lives **entirely in automata**: `build_class` (`nfa.rs:180`) lowers `CharClass` into
`ByteSet` transitions, and `dfa.rs` folds those into byte-**equivalence** classes
(`compute_byte_classes`, `byte_classes:[u8;256]`), so the 256-way interior never escapes the
NFA/DFA. The shared HM type therefore sees only the leaf's **projection codomain**, not its
class cardinality: a regex leaf's synthesized type is the declared `->` map's result type (or
the default `Span`/`&str`), a backend-agnostic `TypeDesc::Named` node per the backend-agnostic-
types law. Class cardinality **never enters unification**. The only place a class datum
touches the type plane is numeric width: `RegexClass::Numeric { allows_sign, allows_fraction,
allows_exponent, reject_leading_zero, allow_leading_dot }` (`classify.rs:57`) can feed a
**width/decode-class constraint** into LOCK-1's bounded CSP post-solve (the "widths, overloads
→ CSP" seam), keyed off the classified pattern, not off byte ranges. Derivation, concretely:
`[0-9]+ -> Number` gives HM the equation `leaf : Number`; the class `[0-9]` and its `+`
quantifier are invisible to the solver; if `Number` carries a width variable, the CSP reads
`RegexClass::Numeric` flags to bound it. **No `BuiltinTy` stub is warranted** — the leaf types
as its codomain, and its interior is automata-private. That IS the confinement boundary the
question demands; a prototype proves it by showing a type derivation over a grammar with three
regex leaves in which the type environment contains no byte-set term.

## Q3 — automata parity re-derived from the narrowed type (`from_expr` ≡ `from_hir`)

`from_hir` (`nfa.rs:71`) drives `build_hir` (`:135`). A bbnf-sourced constructor `from_expr`
(building an `Nfa` directly from bbnf IR, the ONE-HIR path) is byte-identical **iff** it
replicates these four behaviors exactly:

1. **Group-elision** — `nfa.rs:143`: a capture/non-capture group adds **no state**; bbnf's
   grouping must lower to the same transparent recursion.
2. **`Optional ↔ Repeat{0,1}`** — `nfa.rs:329`: `(0,Some(1))` routes to `build_optional`. A
   bbnf `Optional`/`?` leaf must materialize as `Repetition{min:0,max:Some(1),greedy}` to hit
   that arm, else the state/epsilon shape diverges.
3. **`Look` zero-width collapse** — `build_look` (`:254`): all six arms produce one state,
   `start==accept`. `from_expr` must emit the identical single-state fragment for bbnf's
   anchor leaves.
4. **Priority-tagged epsilons** — `add_epsilon` carries a `priority:u8` for greedy/lazy
   disambiguation (`nfa.rs:118`, `EpsilonEdge{target,priority}` `:21`); `build_optional`/
   `build_star` set priority from `rep.greedy` (`:326`). Byte-identity requires the greedy
   flag to thread through bbnf's quantifier lowering with the same priority ordering.

**What a prototype must exhibit (Q3 evidence):** a state-by-state byte-identity harness —
`Nfa::from_expr(bbnf_ir)` vs `Nfa::from_hir(engine_hir)` over the grown ≥50-pattern corpus,
comparing `state_count()` (`nfa.rs:83`), the per-state `transitions` (ByteSet + target), and
the per-state `epsilon` priorities. Throughput must be **re-derived from the narrowed type**
and carry **no inherited band**: the PROVE-10 9.6×–29× figures are the `Dfa::find` **prefilter**
axis (a start-state memchr skip), orthogonal to construction; they may not be cited for
`from_expr`. Fresh `regex_dfa_compile` medians (cold, paired, N≥200) on the scratch copy are
the only admissible construction numbers.

## Q4 — a `Look`-consumer test that can FAIL (the sharp counterexample)

This is where leaf-growth has a real trap. Because `build_look` (`nfa.rs:254`) makes **every**
`Look` — including `Start`/`End` — a zero-width no-op state, the NFA for `/foo$/` is
structurally the automaton for `/foo/`; the `$` is an epsilon that consumes nothing. Anchoring
is enforced **outside** the automaton, by `info/classify.rs:130 detect_anchored` (which
returns `true` when a `Concat`'s first is `Look::Start` or last is `Look::End`, `:137`/`:142`)
plus the search driver's choice of anchored vs unanchored scan (`find_at` vs `Dfa::find`).

The consequence — and the concrete can-fail assertion: a test that `/foo$/` **rejects**
`"foobar"` while `/foo/` **accepts** `"foobar"` passes **only if the search layer consults
`detect_anchored`**; the NFA alone accepts both. So the buildable failing/passing evidence is:

```
assert!( search(compile("foo$"), "foobar").is_none() );   // Look::End consulted
assert!( search(compile("foo"),  "foobar").is_some() );   // no anchor → match
```

driven so that the first assertion exercises `detect_anchored` seeing the trailing
`Hir::Look(End)`. **The named risk for the ONE-HIR path:** `from_expr` must **preserve `Look`
nodes in the expression tree** even though `build_look` no-ops them into automata — because
`detect_anchored` reads the *`Hir`/expr*, not the NFA. If a bbnf lowering "optimizes away" a
trailing anchor (seeing it produce a no-op state), `/foo$/` silently starts accepting
`"foobar"`. The test above is precisely the regression guard; a prototype that runs it green
proves `Look::Start`/`End` survive to the search boundary.

## Cross-lock and grammar-driven notes (5A-adjacent, for the record)

- **Q11 (representation of `->`).** The BIR carries projection as `ValueProject{body,target,ty}`
  and `DirectBuild` (`ir/lib.rs`, dispatched at `lower/sink_only.rs:228`). Whether the inner
  regex-class `Map` is a barrier vs a typed edge is LOCK-1's call and binds the bridge's
  projection soundness; 5A only requires that a regex leaf's codomain type reach the tape via
  that node (it does: `ValueProject.ty`).
- **Q9 (grammar-driven path).** The extant bootstrapped regex grammar is
  `grammar/misc/regex.bbnf` (28 lines): `expression = (term+, "|"?)+`, anchors mapped as
  `anchor = "^" | "$" | "\b" | "\B"` (`:22`) — these are the `Look` producers. Note a latent
  bug in `lookbehind = "(?<=(" , expression , "))"` (`:26`, a spurious paren pair) and that
  `decimal = /d+/` (`:18`) is missing its backslash — de-dup against `bootstrap/regex.bbnf`
  should reconcile both. Promoting this grammar and making `parse_generated()` return the AST
  (break-the-discard) is gated on LOCK-7/7A, SPEC-ONLY.
- **PT-1 alignment.** The engine-side productionization (PROVE-10 prefilter → `Dfa::find`,
  shufti/Teddy, lazy DFA, the bridge-repair `pub use bbnf_regex::hir::parser`) executes in
  parse-that under the BBNF-PT-G handshake; 5A's leaf-growth is consumed there, not authored
  bbnf-side.

## What a pass-3 prototype must test to move family 5A

1. **Byte-identity harness** `from_expr` vs `from_hir` over the ≥50 corpus (state count,
   transitions, epsilon priorities) — the ONE-HIR correctness floor (Q3).
2. **The confinement proof** (Q2): a type derivation over a 3-regex-leaf grammar whose type
   environment contains no byte-set/class term — proving `Class` byte-expansion never reaches
   HM.
3. **The `Look`-survival regression test** (Q4): `/foo$/`↛`"foobar"`, `/foo/`→`"foobar"`,
   asserting `detect_anchored` sees the preserved trailing `Look` after `from_expr`.
4. **The classify-relocation LOC ledger** (Q1): move `info/classify.rs`+`classify.rs`'s
   structural walks under a shared analysis pass and report the exact before/after LOC —
   expected ≈0 net shrink (relocation, not reduction), which is itself the decisive negative
   result for the "classify shrinks" claim.

## Named risks / counterexamples

- **Wrong-crate measurement.** The vendored `parse-that-regex` substrate has no automata; any
  `Class`/`Look` or `from_expr` number sourced from it is void — pin to native `bbnf-regex`.
- **Anchor erasure.** `build_look` no-ops anchors; a ONE-HIR lowering that drops them breaks
  anchored matching silently (Q4 guard).
- **`Optional` shape drift.** If bbnf `?` does not lower to `Repetition{0,Some(1),greedy}`, the
  NFA fragment diverges from `from_hir` and byte-identity fails (Q3).
- **classify "shrink" mirage.** Re-homing the 1148-LOC taxonomy relocates irreducible domain
  arms; claiming a shrink without the paired LOC ledger is unearned.
- **Greedy-priority mismatch.** Epsilon priorities encode greedy/lazy; a `from_expr` that omits
  them yields a same-shape-but-different-match automaton (accepts the same language, differs on
  leftmost-longest).
