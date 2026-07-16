# LOCK-5 · Family 5A — ONE-HIR / LEAF-GROWTH — pass-3 research

## Thesis (the strongest form under the ratified two-type ground)

The regex leaf lives twice in the greenfield today, and the second copy is a brittle string
scanner. The **grammar tier** carries it opaque — `ir::ExprKind::Regex { pattern: String }`
(`skinny/crates/ir/src/lib.rs:250`) and `ir::BackendExpr::RegexProgram { pattern: String,
span_kind: SpanKind }` (`:389`). The **engine tier** (parse-that `rust/regex`, 6742 LOC) carries
the real rich `Hir` — `Empty | Literal(Vec<u8>) | Class(CharClass) | Look(Look) |
Repetition | Group(Box<Hir>) | Concat | Alternation` (`rust/regex/src/hir/mod.rs:107`), with
`Look` = `Start|End|StartLF|EndLF|StartCRLF|EndCRLF` (`:63`) and `CharClass::{Bytes,Unicode}`
byte-range leaves (`:45`). These `Class`/`Look` leaves are the "grown leaves"; they are **already
grown and already tested** in the engine. The vendored skinny `bbnf-regex` (322 LOC,
`skinny/crates/bbnf-regex/src/lib.rs`) is NOT a HIR — its `RegexHir { kind: RegexKind }` holds a
four-way tag (`Whitespace|QuotedString|Numeric|Unknown`) computed by literal string comparison:
`classify()` does `pattern == r"[ \t\n\r]*"`, `pattern.starts_with('"')`,
`pattern.starts_with("-?(0|[1-9]")` (`:92`). This is a shadow re-implementation of the engine's
parser + `classify_regex`, drifting on raw bytes.

**ONE-HIR made concrete = delete the shadow, consume the engine.** The engine's parsed `Hir` is
the single rich home for `Class`/`Look`; the bbnf passes stop re-deriving regex facts from raw
pattern bytes and instead read them off the engine's analysis. The grammar tier stays opaque
(`Regex(pattern)` unchanged, per ratified XL-C); the bridge is the engine's string-entry wrappers
(`classify_regex(&str)` `rust/regex/src/classify.rs:157`, `regex_first_chars(&str)`
`rust/regex/src/first.rs:13`), each of which parses the pattern internally. `classify` is not
"re-homed onto the bbnf side" — the bbnf side's duplicate collapses **into** the engine's one
classify. This is the lawful reconciliation of "ONE-HIR" with the two-type ratification: one
parser, one first-set, one classify — the engine's — consumed by both planes.

## Q1 — the per-pass dispatch census (built, not estimated)

The grown `Class`/`Look` add **zero** new match arms to the grammar/bbnf IR, because that tier
keeps `Regex(pattern)` opaque. What the census actually enumerates is the set of bbnf-side sites
that today re-derive regex facts by string-scanning and would instead read them off the engine.
Every site verified by path:line:

| Pass site | current arm | LOC today | under ONE-HIR |
|---|---|---|---|
| `ir::nullability` | `ir/src/lib.rs:331` `ExprKind::Regex{pattern} => analyze(pattern).nullable` | 1 arm | `hir_is_nullable(&hir)` or `RegexInfo.nullable` |
| passes routing (ws) | `passes/src/lib.rs:225` `analyze(pattern).hir.kind == RegexKind::Whitespace` | 1 | `RegexClass::WhitespaceWithBlockComment` |
| passes routing (str) | `passes/src/lib.rs:349` `== RegexKind::QuotedString` | 1 | `RegexClass::QuotedString` |
| passes facts | `passes/src/lib.rs:944` `analyze(pattern)` | 1 | `RegexInfo::analyze(&hir)` |
| passes match | `passes/src/lib.rs:1665` `match analyze(pattern).hir.kind` | 1 | `match classify_regex_from_hir(&hir)` |
| first-byte set (general) | `codegen/general_descent/regex_first.rs` (`first_bytes`, `head_bytes`, `class_bytes`, `closing_class_index`, `class_byte`, `split_top_level_alternation`, `is_nullable`, `leading_group_spans_all`) | **230** | `regex_first_chars_from_hir(&hir)` |
| first-byte set (json) | `codegen/json_sink_direct/first_set.rs:77` `regex_first_bytes(span_kind,pattern)` | ~40 | same engine call |
| span-kind dispatch | `codegen/json_sink_direct/dispatch.rs:86,165` | small | carried on the node (below) |
| skinny analyzer | `bbnf-regex/src/lib.rs` `classify`/`first_set`/`first_from_prefix`/`parse_leading_class`/`class_bytes`/`closing_class_index`/`class_byte` | ~200 of 322 | DELETED |

**SHRINK, not RELOCATE — measured LOC delta.** The engine facilities that replace all of the above
already exist and are already under test: `first.rs` (158 LOC), `classify.rs` (1148 LOC, 69 `Hir::`
arms), `info/mod.rs` (`RegexInfo`). Re-homing deletes ≈ **230 (`regex_first.rs`) + ≈200 (skinny
string-scanner) + ≈40 (`json/first_set.rs`) ≈ 470 bbnf-side LOC** and adds **~0 engine-side LOC**
(the classify/first machinery is not moved, it is already the canonical home). So classify does not
relocate to the bbnf side and it does not grow; the bbnf side's shadow re-implementations collapse
into it. Net: −470, driven by DRY, with the engine's 6742 LOC untouched. This is the one-liner the
directive forbade turned into a real ledger: the shrink is on the CONSUMER side, and it is large.

**Engine-internal census (the arms that DO exist for `Class`/`Look`).** `Hir::` match sites, per
file: `classify.rs` 69, `info/width.rs` 20, `info/classify.rs` 16, `first.rs` 16,
`egraph/translate.rs` 16, `hir/parser/escape.rs` 16, `info/one_pass.rs` 11,
`info/literal_prefix.rs` 10, `info/dfa_size.rs` 8, `automata/nfa.rs` 8, plus parser arms. `Look::`
is consumed in exactly three files — `automata/nfa.rs`, `hir/parser/atom.rs`, `info/classify.rs` —
a narrow, auditable surface. This is the leaf-growth surface, and it is already complete in the
engine; 5A adds nothing to it.

**The one node-level addition worth making (kills repeated parsing).** `classify_regex(&str)`
re-parses on every call. To avoid N parses across N passes, compute the facts ONCE at the
`ExprKind::Regex → BackendExpr::RegexProgram` lowering boundary and cache them on the node: extend
`RegexProgram { pattern, span_kind }` with a `class: RegexClass` (and optional `first: CharSet128`,
`nullable: bool`). Downstream passes (`tape_plan.rs:117`, `sink_only.rs:82`, `emit.rs:365`,
`lib.rs:1444`) read the cached facts. This is "compute at the bridge, carry on the node" — it
removes both the duplicate scanners AND redundant parsing, and it is the concrete substrate 5A adds.

## Q2 — typing `Class`/`Look` through LOCK-1's HM, not a stub

Byte-expansion (`[α-ω]* → ByteRange` sets) lives **entirely in automata** — `CharClass::Bytes`
consumed by `Nfa::from_hir` (`automata/nfa.rs`), `sets/byteset.rs`, `sets/equiv.rs`
(`compute_byte_classes`). It **never leaks** into a shared type: the bbnf type plane never sees a
byte range. The grammar-plane "type" of a regex leaf is its `RegexClass`/`SpanKind` tag — a finite
nominal value (`SpanKind::{String,Number,Whitespace}` `ir/src/lib.rs:440`; `RegexClass`'s
`Numeric{…}/QuotedString{…}/Identifier{…}` flag-carrying but still finite). LOCK-1 types the leaf
as a **`Named`/base type keyed on the class tag**, not a byte set; unification is over class
identity, not over automaton alphabets. The confinement boundary is the funnel
`classify_regex_from_hir(&Hir) → RegexClass → SpanKind` (`classify.rs:177`): byte-level detail is
consumed by classify and discarded there; only the finite tag crosses into the shared type. This
satisfies the zero-runtime-cost erasure invariant — the type is a compile-time tag, the byte set is
an automaton detail. Derivation: `Γ ⊢ /p/ : Regex⟨classify(p)⟩`, and `->` on the leaf (Q11)
narrows the result type by the projection's declared `ty`, never by the byte set.

## Q3 — automata parity re-derived from the narrowed type (`from_expr` ≡ `from_hir`)

The skinny side builds **no** NFA — `analyze` computes only first-set + nullable; `Nfa::from_hir`
(`automata/nfa.rs:71`) is the sole automaton builder. So parity is: does a grammar `ExprKind`
fragment, lowered to `Hir`, produce a byte-identical NFA to the engine parsing the equivalent
regex? The functor: `Seq→Concat`, `Alt→Alternation`, `Repeat{min,max}→Repetition{min,max}`,
`Optional(x)→Repetition{min:0,max:1,greedy}`, `Literal(bytes)→Literal(bytes)`, `Regex(p)→parse(p)`.
Two normalizations the harness MUST apply before comparing (both already law in the engine):
**Group-elision** — `Hir::Group` is transparent ("only the sub-expression matters",
`hir/mod.rs:107` doc); and **`Optional ↔ Repeat{0,1}`** — the engine's `egraph/rules/repetition.rs`
canonicalizes `?` and `{0,1}`. Compare post-`simplify_hir` (or compare the minimized DFA, which
quotients both). **Prototype to build:** lower a grammar fragment and its regex twin to `Hir`, run
`Nfa::from_hir` on each, assert identical state graphs (or identical minimized `Dfa`); throughput
derived fresh from `regex_dfa_compile`, carrying NO inherited band. **Named risk:** priority-tagged
epsilon edges (`add_epsilon`, `nfa.rs:118`) make raw NFA state-id equality fragile; compare at the
minimized-DFA level (congruence-quotiented) to make the oracle robust, not at raw NFA state ids.

## Q4 — a `Look`-consumer test that can FAIL

Today the bbnf plane has no `Look` — regex is opaque and the skinny `first_from_prefix` treats
bytes positionally, never consulting `^`/`$`. `Look::Start`/`End` are consumed only in
`automata/nfa.rs` (+ `atom.rs` parse, `info/classify.rs` analysis). **Buildable failing→passing
oracle:** compile `Dfa` for `/foo$/` and `/foo/`; assert `dfa.find("foobar")` on the anchored
pattern returns `None` (end-anchor rejects the trailing `bar`) while `/foo/` returns the `0..3`
match — the assertion exercises `Look::End` in `from_hir`. A wrong lowering that dropped the anchor
(or the skinny scanner, which would treat `$` as literal byte `0x24` and mis-seed the first-set)
would FLIP the first assertion to a match — the test discriminates. On the bbnf path this is honored
transitively (the generated parser calls the engine `Dfa`) **iff** the lowering does not strip
anchors; the ONE-HIR route guarantees it because first-set comes from `regex_first_chars_from_hir`
(zero-width-aware) rather than the byte-positional skinny scanner. This is the concrete correctness
argument FOR the re-home: the string scanner cannot see `Look`; the engine can.

## Q9 / Q10 / Q11 — cross-lock (SPEC-ONLY where gated)

**Q9 (grammar-driven path, gated LOCK-7/7A).** No regex `.bbnf` exists *inside the greenfield*
(`skinny/`), but the **extant tree carries a complete one**: `grammar/misc/regex.bbnf` (repo root,
non-greenfield) is a full recognizer grammar — `expression = (term+, "|"?)+`, `term = factor - end`,
`factor = sub_expression, ("*"|"+"|"?")?`, `char_class = "[", ["^"], char_set, {char_set}, "]"`,
`range = character, "-", character`, `anchor = "^"|"$"|"\b"|"\B"`, `lookahead = "(?=" …`,
`lookbehind`, `non_capturing_group`, `back_reference`, plus a `regex = "/" >> expression, end`
entry. Two consequences for Q9: (i) it is a **port source**, not a pure from-scratch bootstrap — its
`char_class`/`range`/`anchor`/`factor` rules map 1:1 onto the engine leaves `Hir::Class` / a
`ByteRange` / `Hir::Look` / `Hir::Repetition`, so a projected version (`char_class -> Class`,
`anchor -> Look`, `factor -> Repetition`) is where ONE-HIR becomes *literally one* HIR — the
bootstrapped grammar's projected AST **is** the engine `Hir`; (ii) it has **zero `->`**
(`grep -c '\->' = 0`), confirming break-the-discard concretely: the generated recognizer returns
`bool`/unit and discards the parse precisely because no rule projects. The de-dup the directive
mentions is a no-op in skinny (the dual copies live only in stale `.claude/worktrees/*`).
Disposition: SPEC-ONLY, deferred to LOCK-7/7A; 5A's re-home does not depend on it and does not block
it, but the extant `grammar/misc/regex.bbnf` is the concrete artifact to promote when the gate opens.

**Q10 (e-graph rule fire census).** The engine's HIR rewrite library is five rules — `flatten`,
`redundant`, `union`, `repetition`, `superset` (`rust/regex/src/egraph/rules/`), each an `apply`
(`redundant.rs:60`, `flatten.rs:100`, `union.rs:92`, `repetition.rs:132`, `superset.rs:82`). The
`rules/mod.rs` doc already draws the seam 5A needs: this tier does **intra-regex** rewrites; a
**grammar-tier** Alt fusion is a distinct rewrite family. So the shared e-graph part (per the
constitution) is these intra-regex rules; grammar Alt-fusion is not double-homed here. A fresh
per-rule fire-count over the corpus (bare vs `Project`-wrapped branches) is the prototype the census
demands; the harness is `regex_hir_egraph` bench + a saturation trace.

**Q11 (representation of `->`).** In the extant code `->` is a **wrapping Map barrier**, not a typed
edge that fuses into the leaf: grammar `ir::ExprKind::Project { body, target, ty }`
(`ir/src/lib.rs:261`) → backend `ir::BackendExpr::ValueProject { body, target, ty }` (`:407`), and
`classify_projection(target, ty)` (`codegen/general_descent/view.rs:35`) maps `ty` →
`ProjectionKind::{Span,Float,Bool,Tag}`. A regex leaf under `->` is `ValueProject{ body:
RegexProgram, target, ty }` — the projection wraps the leaf; the `RegexClass` is computed on the
**bare** inner pattern and the projection's `ty` selects the decode (`span→Raw`, `f64→NumberScalar`,
etc.). Binding on the bridge: classify the inner regex independently of the projection, then let the
projection narrow the type — the inner-class Map barrier is the safe, extant representation. LOCK-1
must ratify this (barrier vs typed edge); the code today is a barrier.

**A named tier-disagreement LOCK-1 must reconcile.** The barrier is enforced only at *codegen*
(`view.rs:35` `classify_projection(target, ty)`), NOT at *type inference*: the inferencer arm is
`ExprKind::Project { body, .. } => self.infer_expr(*body)` (`passes/src/lib.rs:204`) — it returns
the **body** type and drops `target`/`ty` outright. So inference treats `->` as a transparent
pass-through (it does not even open a fresh type variable), while codegen treats `ty` as a decode
barrier. For the bridge's projection soundness this is a live gap: the narrowed regex leaf type
(Q2's `RegexClass`-derived tag) never meets the declared `ty` in the type plane, so the
`string`/`format!` round-trip is decode-selected by codegen but **type-unchecked** by inference. 5A
needs LOCK-1 to make `->` a typed edge that unifies `infer_expr(body)` against the declared `ty`
(closing the gap), while keeping the codegen decode barrier — the two are not in tension, but today
inference is silent where codegen is load-bearing.

## What a prototype must test to move 5A

1. **The re-home ledger, executed.** Delete `codegen/general_descent/regex_first.rs` (230) + the
   skinny `bbnf-regex` string-scanner (~200) + `json_sink_direct/first_set.rs::regex_first_bytes`,
   route all consumers through `bbnf_regex::regex_first_chars` / `classify_regex`, and measure the
   real LOC delta + a green `cargo test` across `passes`/`codegen`/`runtime`. Confirms SHRINK, not
   relocate; confirms the engine facilities cover every skinny call.
2. **Node-cached facts.** Add `class: RegexClass` to `RegexProgram`; verify parse-once (no repeated
   `classify_regex` across passes) and byte-identical generated output (regen idempotent).
3. **`from_expr ≡ from_hir` at minimized-DFA level** (Q3 harness), over a ≥50-pattern corpus.
4. **The `Look` discriminating oracle** (Q4) as a `tests/` subtest that fails on a stripped anchor.

## Named risks / counterexamples

- **Parse cost regression.** `classify_regex` parses per call; without the node-cache (item 2) the
  re-home trades duplicate code for duplicate parsing. Counterexample: a grammar with many regex
  leaves each hit by 4+ passes. Mitigation = compute-at-bridge, carry-on-node.
- **Skinny scanner covers a case the engine's string entry rejects.** The skinny `classify`
  hard-codes exact patterns (`"-?(0|[1-9]"`); if a live grammar's number regex differs by a byte,
  the skinny path and `classify_regex_from_hir` may disagree. The re-home must diff `RegexClass`
  outputs against the current `RegexKind` routing on the actual grammar corpus before deletion.
- **NFA state-id equality is too strict** (Q3) — compare minimized DFAs, not raw NFAs.
- **`->` fused instead of wrapped** would corrupt projection soundness (Q11); keep the barrier until
  LOCK-1 ratifies.
