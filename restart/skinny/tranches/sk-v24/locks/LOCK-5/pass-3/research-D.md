# LOCK-5 pass-3 — Family 5D (GRAMMAR-FRAGMENT) — research-D

Charter (5D): regex literals in grammars lower DIRECTLY into the grammar's own IR — no
separate engine on the generator path — and the standalone parse-that regex API survives as a
thin front over the same machinery. Settled ground (XL-C, RATIFIED): grammar tier keeps
`Regex(StringId)` opaque; engine tier keeps a rich `Hir`; a `string`/`format!` bridge joins them.
5D's concern is the *generator path*: what the string becomes AFTER the grammar tier, at lowering
and emission, and whether that path re-implements a regex engine badly instead of lowering once.

The finding that governs this family: **the greenfield generator path today has no regex lowering
at all — the pattern travels as a `String` from grammar IR to backend IR to three independent
hand-rolled leading-byte re-parsers.** 5D is the mechanism that collapses those three into one
byte-class lowering. Every claim below is anchored to the vendored greenfield
(`skinny/crates/`), the real engine (`parse-that/rust/regex`, READ-ONLY), and the grammar tree.

---

## The concrete state of the generator path (the substrate 5D acts on)

**Grammar tier.** `skinny/crates/ir/src/lib.rs:250` — `ExprKind::Regex { pattern: String }`. Opaque
string, matching XL-C's `Regex(StringId)`. Consumed for nullability at `ir/src/lib.rs:331`
(`ExprKind::Regex { pattern } => analyze(pattern).nullable`).

**Backend tier.** `ir/src/lib.rs:389` — `BackendExpr::RegexProgram { pattern: String, span_kind:
SpanKind }`. **The pattern stays a `String` all the way to the backend IR.** `SpanKind`
(`ir/src/lib.rs:439`) is a 3-way heuristic tag `{String, Number, Whitespace}`. There is **no
`BackendExpr::ByteClass`, no compiled DFA, no `dfa.find`** anywhere on the generator path (verified:
`grep dfa.find|find_at` across `crates/codegen` = 0 hits). The only recognizer variant is
`Recognizer::SimdScan { mode: SimdMode::Exact, alphabet: StructuralAlphabet, site: PreEntry }`
(`ir/src/lib.rs:416`) — a structural-alphabet prefilter for the GN-2.W1 self-loop seam, unrelated to
regex leaves.

**The vendored `bbnf-regex` is NOT the engine.** `skinny/crates/bbnf-regex/src/lib.rs` is **322 LOC
of heuristic string-sniffing**, not the 6742-LOC HIR→NFA→DFA engine (that lives only at
`parse-that/rust/regex`, R3 §3). `classify` (`bbnf-regex/src/lib.rs:92`) is literal pattern-equality:
`pattern == r"[ \t\n\r]*"` → `Whitespace`; `starts_with('"')` → `QuotedString`; a hard-coded numeric
prefix `starts_with("-?(0|[1-9]")` → `Numeric`; else `Unknown` — a 4-arm `RegexKind`
(`:16`). It computes `RegexFacts { nullable, first: FirstSet, byte_classes: Vec<ByteClass>, hir, string }`
(`:45`), where `ByteClass { bytes: ByteSet256 }` (`:30`) is real bitset data — **but `byte_classes` is
consumer-less** (grep across skinny: the field is read nowhere; `analyze()` is called only for
`.nullable` at `ir:331` and `.hir.kind` at `passes/src/lib.rs:225,349`). That is a live Constitution
viii / Y.13 violation (consumer-less substrate) sitting in the substrate 5D inherits.

**Three parallel leading-byte re-parsers (the DRY catastrophe 5D exists to cure).** Because the
backend keeps the pattern as a string and the runtime has no matcher, every consumer re-parses the
string itself:

1. `bbnf-regex/src/lib.rs:120` `first_set` + `:142` `first_from_prefix` + `:162` `parse_leading_class`
   — one leading-class byte parser.
2. `skinny/crates/codegen/src/general_descent/regex_first.rs` (231 LOC) — a SECOND full leading-byte +
   nullability parser. Its own module doc (`regex_first.rs:3`) states the reason verbatim: *"The
   runtime has no general regex matcher and the `bbnf-regex` HIR is a coarse classifier, so the
   general descent emitter derives each leaf's first-byte set HERE."* `first_bytes` (`:81`),
   `head_bytes` (`:120`), `class_bytes` (`:165`), `is_nullable` (`:18`) — a near-verbatim
   re-implementation of the bbnf-regex functions.
3. `skinny/crates/codegen/src/json_sink_direct/first_set.rs:97` `regex_first_bytes(span_kind, pattern)`
   — a THIRD, span-kind-switched first-byte derivation.

Three hand-written regex-fragment parsers, none a real engine, each degrading to "empty set → fall
through to ordered-try" on anything it cannot read. **This is the negative space that defines 5D's
win: one lowering, one representation, zero re-parsing.**

---

## Q7 — the FOLLOW-sound one-pass fraction (straight-line vs `Seq[Repeat,suffix]`+FOLLOW)

**Question.** Over the engine corpus with real leaves, what fraction lowers to a sound one-pass
(no-backtrack) straight-line scan, measured (a) under a straight-line lowering and (b) under a
`Seq[Repeat, suffix]` split plus a grammar-FOLLOW check — a *sound* predicate that checks the class
against its own FOLLOW, not a byte-blind star.

**Why it matters for 5D.** The grammar-fragment claim is that a regex leaf becomes ordinary grammar
IR (`Repeat`, `Seq`, byte-class leaves — the shapes already at `ir/src/lib.rs:240` `Repeat`,
`:235` `Seq`). A starred class `[a-z]+` lowered naively to a self-loop is one-pass ONLY if its class
is disjoint from its FOLLOW set; otherwise the greedy loop over-consumes and the suffix fails
(`[a-z]+[a-z]` is the classic hazard). The existing one-pass fact lives in the real engine
(`parse-that/rust/regex/src/info/one_pass.rs`, 72 LOC, R3 §3) but is **absent from the greenfield**
(`grep one_pass|OnePass` in skinny codegen/passes = 0). So the greenfield currently has NO soundness
guard on its self-loop lowering — it relies on the three re-parsers producing disjoint first-sets and
degrading to ordered-try otherwise.

**Mechanism to prototype.** A `follow_sound(class, follow)` predicate: for a regex leaf lowered to
`Seq[Repeat{body: ByteClass(C), min}, suffix]`, the loop is FOLLOW-sound iff `C ∩ first(suffix) = ∅`
(then the greedy scan is a total one-pass; no backtrack). `first(suffix)` is the grammar FOLLOW of
the leaf, computable from the existing dispatch machinery (`ir/src/cost.rs:252` `FirstSetDisjoint` /
`:253` `FirstSetOverlap`, `passes/src/decision_csp.rs:255` `ShapeRationale::FirstSetOverlap` — the
FIRST/FOLLOW disjointness apparatus already exists for `Alt` dispatch and is the natural home for
this predicate). The `ByteSet256` from `bbnf-regex::analyze(...).byte_classes` (currently dead) is the
class operand — activating it is the Q8 consumer.

**What to measure.** Grow the engine corpus to ≥50 patterns tagged by leaf shape; for each starred
class leaf, evaluate both lowerings and report the sound one-pass fraction. Prior art: this is
exactly regex-automata's one-pass DFA gate (a DFA is one-pass iff no state has two transitions into
distinct match-carrying states on the same input;
https://docs.rs/regex-automata/latest/regex_automata/dfa/onepass/) — the byte-class-∩-FOLLOW test is
the grammar-fragment specialization of that gate. Straight-line lowering will over-count one-pass
(it ignores the suffix); the `Seq[Repeat,suffix]`+FOLLOW lowering is the sound number. The DELTA
between the two is the family's core evidence — it quantifies how many leaves the current greenfield
mislowers as "one-pass" without a soundness check.

---

## Q8 — the prototype battery (the family's load-bearing evidence)

Four concrete artifacts, each with a pass/fail oracle:

**(a) Cold-paired class-scan vs prefiltered `Dfa::find`.** Today the greenfield emits
`RegexProgram { pattern }` → a textual `scan_span` plan (`codegen/src/lower/tape_plan.rs:117`:
`{prefix}_{span_op}({span_kind:?}, pattern={pattern}) -> ParserState::scan_span`). It never scans by
byte class. The prototype: for a prefilterable leaf (`0x[0-9a-fA-F]+`, `#[0-9a-fA-F]{3,6}`), emit two
lanes — a byte-class self-loop scan (the byte set from `analyze().byte_classes[0]`) vs a
`Dfa::find` from the real engine's PROVE-10 prefilter (`parse-that/rust/regex/src/automata/prefilter.rs`
+ `Dfa::find`, patch at `prototypes/PROVE-10/patch.diff`). PROVE-10 already certified 9.6–29× on
these patterns cold/paired/N=200; the 5D-specific measurement is the *class-scan* lane as baseline
(the thing the grammar-fragment path would actually emit if it lowered to a byte class) — does the
in-grammar byte-class self-loop close enough of the gap that a separate DFA is unwarranted for the
single-class leaf, and where does `Dfa::find` (multi-byte/anchored) become mandatory? This is the
concrete "no separate engine on the generator path" test: measure the point at which the grammar-IR
byte-class scan is provably sufficient vs where it must call the thin engine front.

**(b) A `ByteClassRecognizer` that actually fires (not the masked wildcard).** The gap is stark:
`ByteClass`/`ByteSet256` exist in `bbnf-regex` facts but never reach `BackendExpr`. The prototype
adds `BackendExpr::ByteClass(ByteSet256)` (or `RepeatLoop{body: ByteClass, ...}`) as a first-class
backend node, populated from `analyze(pattern).byte_classes[0].bytes` when the leaf is a single
class (or a starred single class). The recognizer emits a real membership scan — reusing the
`crates/bbnf-simd` shufti/truffle kernel (`crates/simd-scan/src/neon.rs:208` `classify_chunk_nibble`,
`:335` `classify_chunk_wide`, R7 §3) via `Recognizer::SimdScan`'s existing `StructuralAlphabet` seam
(`ir/src/lib.rs:435`). "Fires" oracle: `cargo expand` / the emitted plan shows a byte-set membership
loop, NOT a `scan_span(pattern=String)` pass-through; a `tests/` oracle asserts the recognizer
accepts/rejects at class boundaries. The "masked wildcard" failure mode to avoid: a recognizer whose
byte set is `[0..256)` (accepts everything) because the class parse failed and silently widened — the
oracle must include a NEGATIVE case (`[a-z]+` must reject `'0'`).

**(c) `BackendExpr::ByteClass` reaching the tape.** Thread the new node through the tape lowerings
(`codegen/src/lower/tape_plan.rs:79` `render_expr` — add a `ByteClass` arm beside `ByteLiteral` at
`:110`; `codegen/src/lower/sink_only.rs:206` `RegexProgram` handling — add the `ByteClass` sink arm).
Oracle: the generated tape plan for a byte-class leaf shows a `{prefix}_match_class_bytes(...)` op
that writes the span to the tape, and the first-byte set flows into the `Alt` dispatch
(`json_sink_direct/first_set.rs:77`) from the SAME `ByteSet256` — eliminating consumer #3's re-parse.

**(d) Capture-leaf → `Project` byte-identical to a hand-authored grammar capture.** A regex leaf that
captures (`([a-z]+)` projected to a field) must lower to the SAME `BackendExpr::ValueProject`
(`ir/src/lib.rs:407`) a hand-written grammar `[a-z]+ -> field` produces. Oracle: emit both, diff the
`BackendExpr` trees, assert structural identity. This is the isomorphism proof for the family — a
captured regex fragment is indistinguishable from grammar-authored capture at the IR, which is the
literal meaning of "lower DIRECTLY into the grammar's own IR."

---

## Q9 — one grammar-driven regex path (promote `bootstrap/regex.bbnf`, break-the-discard, de-dup)

**Current state.** The regex grammar EXISTS and is bbnf-defined: `grammar/misc/regex.bbnf` (28 lines,
full read below-worthy): `expression = (term+, "|"?)+`, `char_class = "[" , ["^"], char_set, {char_set},
"]"`, `range`, `escaped`, `lookahead`/`lookbehind`, `anchor = "^"|"$"|"\b"|"\B"`, `regex = "/" >>
(expression), end`. It is self-descriptive (regex-in-bbnf). But there is **no committed stage-0
bootstrap of it**: `find bootstrap -name '*.bbnf'` = 0 files; `crates/bootstrap` is Rust binaries
(`dump_ir`, `cost_grid_sweep`, `debug_parse`), no `regex.bbnf`. And **no `parse_generated`** exists
anywhere (`grep parse_generated` across the tree = 0) — the "return AST instead of bool /
break-the-discard" refactor named in Q9 has no extant callsite yet; it is a target, not a repair.

**Mechanism.** Promote `grammar/misc/regex.bbnf` to the bootstrapped stage-0 (LOCK-7A staging: commit
the generated parser, gate `regen∘regen = regen`), make the generated regex parser return the regex
`Hir`/`ExprKind::Regex`-subtree instead of a boolean accept, and de-dup: the SINGLE grammar drives
both the standalone `parse-that` front (`hir/parser/` hand-written, `parse-that/rust/regex/src/hir/parser/mod.rs:153`
`parse`) and the grammar-fragment lowering. Under 5D the standalone API becomes a thin front: the
bbnf-generated regex parser IS the parser, and `bbnf_regex::hir::parser::parse` re-exports it (this is
also the W4 bridge-repair surface, PT-1 §W4: `pub use bbnf_regex::hir::parser`).

**Binding constraint.** Q9 is gated on LOCK-7/7A and is SPEC-ONLY(grammar-driven) — the bootstrap
staging is LOCK-7's verdict; 5D specifies the *consumption* (the generated AST threads into
`ExprKind::Regex` lowering) and inherits the staging. The grammar-totality picture: with promotion,
regex parsing is grammar-driven end to end (Constitution i satisfied for the regex sub-language); the
hand-written `hir/parser/` (`class.rs` 217, `escape.rs` 168, `group.rs` 75, etc., R3 §3) survives only
as the LOCK-7D differential oracle until measured parity + perf promote the generated parser.

**Evidence to produce.** The generated parser's AST actually threaded through one lowering
(a `.bbnf` grammar with a regex leaf → `ExprKind::Regex` → the lowering → a tape plan), the
regen-idempotence check, and the LOC delta from deleting the duplicate `grammar/misc/regex.bbnf`
copies (48 worktree copies exist — the canonical single home is `grammar/misc/regex.bbnf`).

---

## Q10 — census the grammar-tier regex e-graph rewrite-rule fire counts

**Where the rules live.** The e-graph is in the REAL engine only:
`parse-that/rust/regex/src/egraph/rules/` — six rule files: `repetition.rs` (156 LOC), `union.rs`
(108), `flatten.rs` (107), `superset.rs` (90), `redundant.rs` (68), plus `util.rs` (76) and
`mod.rs` (73); driven by `translate.rs` (132), `cost.rs` (102), `node.rs` (73),
`saturation_cache.rs` (68) — 1253 LOC total (R3 §3). **The greenfield has zero e-graph** (`grep
egraph|saturat|rewrite` in `skinny/crates/bbnf-regex` = 0). So the census runs against the real
engine's egraph over the grown corpus, and the 5D question is: which rules fire on the leaves that
actually appear in *grammars* (as opposed to standalone regex torture patterns)?

**Mechanism.** Instrument the saturation loop (`egraph/mod.rs` runner) with a per-rule fire counter
keyed by rule name; run over the corpus in two modes — **bare** (the raw pattern) and
**projection-wrapped** (the pattern as it appears under a `->` capture, which changes the e-node
context, Q11). The existing rule tests (`parse-that/rust/regex/tests/egraph_rules_{flatten,
repetition,redundant,superset,union}.rs`, 3+3+2+2+2 = 12 subtests, R3 §7) confirm each rule is
individually live; the census measures FIELD firing, not unit liveness. A rule that never fires on a
real grammar leaf is a candidate for the negative-result ledger (LOCK-5C's "losses are named").

**Prior art / prediction.** egg/egglog fire-rate profiling is the model (rebuild-cost vs match-count;
https://egraphs-good.github.io/). Prediction to test: `flatten` (nested `Seq`/`Alt` normalization) and
`redundant` (drop provably-dead alternatives) fire densely on grammar leaves because grammar-authored
regexes are shallow; `superset`/`union` (class-merge) fire mostly on machine-generated or hand-torture
patterns and may be near-zero on the grammar corpus — if so, that is the evidence that the grammar-tier
e-graph can be a SMALLER rule set than the standalone engine's (the family's parsimony dividend).

---

## Q11 — the concrete representation of `->` (inner-class Map barrier vs typed edge)

**Current representation (greenfield).** `ir/src/lib.rs:261` — `ExprKind::Project { body: ExprId,
target: String, ty: Option<String> }`, lowered to `BackendExpr::ValueProject { body, target, ty }`
(`:407`). It is a **typed edge**, NOT an inner-class `Map` barrier: `Project` wraps the body
sub-expression and carries a target name + optional type; the pretty-printer renders it `body -> target:
ty` (`ir/src/lib.rs:185`). Nullability passes THROUGH it (`:337` `ExprKind::Project { body, .. } =>
nullable[body.0]`) — the projection is transparent to first/nullability analysis, i.e. it does not
form an opaque barrier. This is the ratified shape the bridge must respect.

**Why it is binding on the bridge's projection soundness (5D concern).** For a captured regex leaf
`([a-z]+) -> name`, the `->` sits BETWEEN the regex sub-language and the grammar type plane. If `->`
were an opaque `Map` barrier (a closure the analyses cannot see through), the captured regex could not
share a first-set with its grammar context and Q8(d)'s byte-identity would fail — the regex capture
would lower differently from a grammar capture. Because `Project` is a transparent typed edge, the
regex fragment's `ByteClass` first-set propagates up through the projection to the dispatch
(`json_sink_direct/first_set.rs` resolves `ValueProject.body`), which is precisely what makes Q8(d)
achievable. **Confirm with LOCK-1** that the ratified `->` is the transparent typed edge
(`ExprKind::Project`, first/nullability see-through) and not a `Map` barrier — this is a cross-lock
dependency, and it is *binding*: if LOCK-1 ratifies an opaque `Map`, the family's byte-identity claim
(Q8d) and its shared-first-set lowering (Q8c) both break, and the report's mechanism must be
re-derived. The evidence to record is the ratified variant name + the four analysis passes' treatment
of it (nullability `:337`, first-set via `ValueProject` resolution, type via `passes/src/lib.rs`
`regex_type`/`node_types`, tape via `tape_plan.rs:152` `ValueProject`).

---

## Prior art (cited)

- **regex-automata one-pass DFA + prefilter tiers** (Q7/Q8): the FOLLOW-sound gate is the
  grammar-fragment specialization of the one-pass DFA property
  (https://docs.rs/regex-automata/latest/regex_automata/dfa/onepass/); prefilter ladder
  memchr→Teddy→Aho-Corasick, anchored-confirm-on-hit (https://burntsushi.net/regex-internals/), the
  PROVE-10 seed.
- **Hyperscan shufti/truffle/teddy** (Q8b): nibble-LUT byte-set membership in ≤4 NEON ops
  (https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf), locally vendored at
  `crates/simd-scan/src/neon.rs:208,335` — the `ByteClassRecognizer` kernel already exists.
- **egg/egglog fire-rate profiling** (Q10): rebuild-cost vs match-count discipline
  (https://egraphs-good.github.io/), the model for the per-rule census.
- **tree-sitter / lexer-as-DFA** (Q9): grammar-driven token DFAs from a declared grammar are prior
  art for the bootstrap-the-regex-grammar move; the standalone engine as a thin front mirrors
  tree-sitter's external-scanner boundary.

## Named risks & counterexamples

1. **The byte-class self-loop is NOT sound without the FOLLOW guard (Q7).** `[a-z]+[a-z]` lowered to a
   greedy self-loop over-consumes and the suffix fails — the greenfield has no `one_pass` guard
   (0 hits) and relies on the three re-parsers degrading to ordered-try. Counterexample the prototype
   must build: a grammar where a starred class abuts a suffix in its own class; assert the straight-line
   lowering mis-accepts/mis-rejects while the `Seq[Repeat,suffix]`+FOLLOW lowering is correct.
2. **The masked-wildcard recognizer (Q8b).** A `ByteClassRecognizer` whose set silently widens to all
   256 bytes on a parse failure accepts everything — the current `class_bytes` returns empty on failure
   (`regex_first.rs:167` negated-class → empty → "falls through"), which is the *opposite* failure
   (over-narrow, misses matches). The oracle must test BOTH a negative accept and a false-empty.
3. **Consumer-less `byte_classes` today (Constitution viii).** `bbnf-regex::analyze` computes
   `byte_classes` that nothing reads — the family MUST make Q8(b/c) its consumer or delete the field;
   shipping the lowering without the recognizer firing re-creates the dead substrate.
4. **Three-way re-parse divergence (the DRY risk the family removes, but must not re-introduce).** If
   5D adds `BackendExpr::ByteClass` but leaves `general_descent/regex_first.rs` and
   `json_sink_direct/first_set.rs` in place, there are now FOUR representations. The family's success
   condition is DELETION of consumers #2 and #3 (231 + the first-byte half of `first_set.rs`), replaced
   by reads of the single `ByteSet256` — measure the LOC delta as evidence.
5. **`->` ratified as opaque Map (Q11 cross-lock).** If LOCK-1 makes `->` a barrier, Q8(c/d) break;
   the mechanism is contingent on the transparent-typed-edge ratification. Named, not assumed.

## What a prototype must test to move this family (the minimal battery)

1. **`BackendExpr::ByteClass` end-to-end**: add the variant, populate from `analyze().byte_classes`,
   thread through `tape_plan.rs` + `sink_only.rs`, emit a real membership scan via the `bbnf-simd`
   nibble kernel; oracle = the emitted plan shows a class-membership loop (not `scan_span(pattern)`),
   with a negative-accept `tests/` case. **This single artifact discharges Q8(b) and Q8(c) and creates
   the consumer for the dead `byte_classes`.**
2. **The FOLLOW-sound predicate + the two-lowering fraction (Q7)** over a ≥50-pattern grown corpus
   tagged by leaf shape; report the straight-line vs `Seq[Repeat,suffix]`+FOLLOW one-pass fractions and
   their delta, reusing the `FirstSetDisjoint`/`FirstSetOverlap` apparatus (`ir/src/cost.rs:252`).
3. **Capture byte-identity (Q8d)**: diff `BackendExpr` of `([a-z]+) -> f` vs grammar `[a-z]+ -> f`;
   assert structural identity — contingent on Q11's transparent `Project`.
4. **The delete-the-duplicates LOC ledger**: show consumers #2/#3 removed once #1 lands.
5. **The e-graph fire census (Q10)** on the real engine over the corpus, bare vs projection-wrapped,
   naming any never-firing rule for the negative-result ledger.

The through-line: 5D's whole thesis reduces to one falsifiable move — **make `analyze()`'s already-computed
`ByteSet256` reach `BackendExpr` as a firing recognizer, and delete the three re-parsers.** If that
lands with a sound FOLLOW guard and byte-identical captures, the grammar-fragment path is real and the
standalone engine is a thin front; if the byte-class scan cannot match `Dfa::find` on multi-byte leaves,
the named negative result is "the engine front is mandatory beyond the single-class leaf" — either way,
concrete and measured.
