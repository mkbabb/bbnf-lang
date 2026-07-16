# LOCK-5 pass-2 · family 5D — GRAMMAR-FRAGMENT

## Thesis, restated and narrowed

A regex literal in a grammar is grammar in a terser notation: concatenation is `Seq`, `|` is
`Alt{mode}`, `*`/`+`/`?`/`{m,n}` are `Repeat`/`Optional`, a literal byte is `Literal`; the ONLY
atom regex adds that the IR lacks is the character class. 5D dissolves the regex leaf at the
front — one `regex_desugar` pass parses each `/pat/` ONCE into native `ExprKind` nodes of the same
arena — so that nullability, first/follow, typing, lowering and emission all run through machinery
the grammar already owns, with no separate engine on the generator path. The classical NFA→DFA
survives only as ONE optional lowering target the scan-plan cost model selects for leaves that
provably need it. This is the deepest reading of Constitution (i)/(ii): one HIR, no functor.

Pass-1's maximal claim ("no engine on the generator path, and the class scan is faster") is
**narrowed by measurement pressure**. This pass establishes the load-bearing facts by reading the
committed predicates rather than asserting from them, and returns three hard results: the routing
predicate is **unsound as committed**, the SIMD chooser **does not fire** on the desugared leaf,
and the structured class **never reaches the tape emitter** today. All three are fixable and each
names its fix; none is a blocker; every one was a pass-1 named gap now closed to a concrete edit.

## Two live trees hold incompatible substrate — 5D must land in both, or name one

- **skinny (graduating) tree.** `ExprKind` has 10 variants — `Seq`/`Alt`/`Repeat`/`Optional`/
  `Literal`/`Regex`/`Ref`/`Annotation`/`Project`/`Discard` (`skinny/crates/ir/src/lib.rs:235-266`);
  the regex leaf is opaque `Regex{pattern:String}` (`:250`), lowered to
  `BackendExpr::RegexProgram{pattern:String, span_kind:SpanKind}` (`:389-391`). There is **no
  `ByteClass`** anywhere in this tree — neither in `ExprKind` nor `BackendExpr`. The recognizer
  chooser is `passes::recognizers::derive_recognizers` (`skinny/crates/passes/src/lib.rs:338`).
- **top-level tree.** Richer: a `RecognizerInfo` trait abstraction
  (`crates/ir/src/recognizer/mod.rs`) with `RegexRecognizer`/`LiteralRecognizer`/`TokenRecognizer`/
  `DispatchGroupRecognizer`/`DelimScanRecognizer`, each surfacing `scanable()`, `regex_class()`,
  `feasible_engines()`; `RegexRecognizer::scanable()` returns `info.one_pass_eligible`
  (`recognizer/mod.rs:72-74`). A grammar-tier regex e-graph (434 LOC) also lives here.

A result in one tree is not a result in the other. 5D's `ExprKind::ByteClass` +
`BackendExpr::ByteClass` + `regex_desugar` edits are LOCK-3-governed enum mutations that must land
in the tree of record and be re-measured there. The `RecognizerInfo` trait in the top-level tree
is the **correct architectural home** for 5D's structural chooser (below); the skinny
`derive_recognizers` is its degenerate ancestor.

## Q3 — the eligibility predicate is UNSOUND for its own use (CONFIRMED, in-tree)

The committed predicate `check_one_pass_eligible(hir)`
(`parse-that/rust/regex/src/info/one_pass.rs:38`) checks **only alternation FIRST-set
disjointness**, recursing structurally. Its repetition arm is verbatim:

```rust
Hir::Repetition(rep) => check_one_pass_eligible(&rep.sub),
Hir::Concat(seq) => seq.iter().all(check_one_pass_eligible),
```

It recurses into a repetition body but **never checks the repetition's class against its FOLLOW
set**. Therefore on `[a-z]+z`: `Concat[Repetition(Class[a-z]), Literal(z)]` →
`Repetition→check(Class[a-z])=true`, `Literal→true`, `Concat→all true` → **returns `true`**. But
`[a-z]+z` is NOT one-pass: on `"abz"` greedy `[a-z]+` eats `abz`, then the `z`-literal fails at
EOF and the match requires give-back of one byte. A straight-line 5D lowering
(`while set.contains(input[pos]) { pos+=1 }` then match `z`) would **reject "abz", a string the
regex accepts** — a silent correctness fault. This is the greedy give-back counterexample the
directive names, and the predicate does not catch it. Confirmed by source, not assertion.

**Blast radius.** Both trees route on this flag. `RegexRecognizer::scanable()` returns
`one_pass_eligible` directly (`crates/ir/src/recognizer/mod.rs:72-74`), so the top-level recognizer
selection inherits the unsoundness; skinny's would too once 5D reads the flag. **5D cannot trust
`check_one_pass_eligible` until the FOLLOW gap is closed.**

**The fix, and why it is elegant, not a patch.** For a greedy `R{min,max}` (max unbounded, or
bounded-with-slack) at position *i* in a `Concat`, one-pass requires
`FIRST(body_of_R) ∩ FIRST(suffix_after_R) = ∅` — the classical Aho–Sethi–Ullman FOLLOW-disjointness
that makes a greedy quantifier committed (no give-back). This is the **same FOLLOW analysis the
grammar's `Alt`-dispatch already computes** for `AltMode::Dispatch`. So 5D does not extend the
regex predicate at all: it desugars the leaf to `Seq[Repeat, suffix]` and lets the grammar's own
FOLLOW machinery decide — a greedy `Repeat` whose body-FIRST overlaps its FOLLOW is emitted as a
give-back-aware form (or routed to DFA), exactly as a grammar-authored `Repeat` in the same shape
would be. The soundness obligation moves out of the regex engine and into the one place the grammar
already reasons about FOLLOW. The prototype MUST add this check and prove `[a-z]+z` routes away from
straight-line lowering before any `scanable()`-driven emission is trusted.

**Give-back that does NOT bite (the subtlety that keeps the fraction high).** FOLLOW-overlap is
unsound only when give-back changes the match *end*. `\w+(?:[-_]\w+)*` on `"a_b"` overlaps on `_`
(`_ ∈ \w`), yet greedy `\w+` and the give-back form yield the same match end because the tail is
also `\w`-led — so a straight-line lowering is match-equivalent here. The FOLLOW check must reject
only true give-back (suffix FIRST ⊄ body class where the suffix is not itself re-enterable), or it
over-rejects and needlessly routes match-safe patterns to the DFA. This precision is a prototype
gate, not a hand-wave.

## Q1 — the one-pass fraction (hand-evaluated over the committed corpus; a number to re-run)

The engine corpus is `parse-that/rust/regex/benches/regex_hir_egraph.rs` `PATTERNS` (~26 patterns,
matching the corpus/gate mismatch the SPEC records — the gate cites 50, the file holds 26; re-cut
the gate to 26). Hand-evaluating the **committed** `check_one_pass_eligible` over it:

- **Reported one-pass = true: ~24/26 (~92%).** Only the two alternations with overlapping FIRST
  are rejected: `-?[a-zA-Z_][\w-]*|--[\w-]+` and `[\-]?[a-zA-Z_][\w-]*|--[\w-]+` (arm-1 FIRST
  includes `-` via the optional `-?`, arm-2 FIRST is `{-}` → overlap → false). Every class-led
  repetition (`[a-z]+`, `[0-9a-fA-F]{3,8}`, `0x[0-9a-fA-F]+`, `#[0-9a-fA-F]{3,6}`, the uuid,
  `[A-Za-z0-9+/]+`, `\$[a-zA-Z_]...`, `https?://...`) and every disjoint-FIRST alternation
  (`(0|[1-9]\d*)`, `(?:[^"\\]|\\...)`, `"..."|'...'`) reports true.
- **The reported fraction is inflated by the unsoundness.** It counts as one-pass every pattern
  whose repetition overlaps its FOLLOW, because the predicate never checks that. Under the
  FOLLOW-corrected predicate the SOUND fraction is lower by the count of true give-back patterns.
  Over THIS corpus the give-back-dangerous set is small (the corpus is engineered so most tails are
  disjoint from their repetition classes), but the exact sound fraction is the single number the
  prototype must report — and it must report BOTH: the reported fraction (~92%) and the
  FOLLOW-sound fraction (the honest one). The three real `json.bbnf` leaves (`number`, `string`,
  `ws`) and PROVE-10's four prefilterable patterns are all one-pass AND FOLLOW-sound (their tails
  are delimiter/EOF, disjoint from their classes) — so "no engine on the generator path" holds for
  the real generator corpus, but is NOT a corpus-wide truth for adversarial standalone input.

**Consequence for the thesis.** The high fraction means the DFA is dead weight on the *generator*
path (the load-bearing claim survives), but the DFA is NOT deletable — it is the retained lowering
target for the FOLLOW-unsound and non-`is_hir_walkable` minority (`is_hir_walkable`,
`one_pass.rs:20`, already rejects lazy-outside-`.*?literal`, Unicode-props, backrefs, non-anchor
lookaround). Report the fraction whatever it is; it decides deletion vs re-homing, not perf.

## Q2 — straight-line class scan vs prefiltered `Dfa::find` (the head-to-head, UNMEASURED — a prototype deliverable)

This is the decisive unmeasured figure and must be run cold-paired (N≥200, same-binary lanes,
`count_scalar == count_prefilter` gate), never asserted. The honest prior, from the two mechanisms:

- A single-`ByteClass` repetition (`[a-z]+`, `[0-9a-fA-F]+`) lowers to
  `while set.contains(input[pos]) { pos+=1 }`, residualized to the shufti/`vqtbl1q_u8` nibble-LUT
  class kernel (`crates/simd-scan/src/neon.rs` `classify_chunk_nibble`; R7 §3). No transition
  table, one membership test per 16-byte lane.
- PROVE-10's prefiltered `Dfa::find` is a **start-state memchr** over the DFA's start-transition
  row (`prototypes/PROVE-10/patch.diff`, `Prefilter{Byte1,Byte2,Byte3}::from_dfa`), 8.8–29.5× on
  literal-prefix-led patterns, 0.97–1.01× on the `[a-z]+`/`\w+` controls.

The prediction the prototype must confirm or refute: **the two TIE on class-led repetitions**
(both reduce to a class-membership scan — the start-state memchr on a *class* start-state is the
same NEON idiom as the shufti class scan), and the **prefiltered DFA WINS on literal-prefix-led
patterns** (`0x[0-9a-fA-F]+`, `https?://…`, `#…`) because memchr on a rare prefix byte skips faster
than scanning a common class. PROVE-10's own controls (`[a-z]+` at ~1.0×) are direct evidence the
class case is a tie: the prefilter buys nothing there, so the class scan cannot lose to it and can
only win by dropping the transition-table indirection. **A tie is a tie (R6): 5D still wins the
architecture axis (four parsers → one, no functor, no automaton on the hot path) even at parity,
but claims NO perf win absent a measured one.** The measured result feeds the scan-plan cost model
that chooses per-site: class-led → straight-line scan; literal-prefix-led → prefiltered DFA.

## Q4 — `derive_recognizers` does NOT fire on `ByteClass` — the `_ => {}` swallows it (CONFIRMED)

`skinny/crates/passes/src/lib.rs:338` `derive_recognizers` matches exactly two shapes and drops
the rest:

```rust
ExprKind::Literal { bytes, .. } if bytes.len()==1 => { /* structural byte */ }
ExprKind::Regex { pattern } if analyze(pattern).hir.kind == RegexKind::QuotedString => {
    present.insert(b'"');
}
_ => {}
```

Two confirmed faults for 5D. **(a)** The QuotedString arm keys on `analyze(pattern)` — the
3-pattern string-oracle 5D deletes; after `regex_desugar` removes `ExprKind::Regex`, this arm is
**dead code matching a variant that no longer exists**. **(b)** The desugared `ExprKind::ByteClass`
falls into `_ => {}` and is **silently swallowed** — the structural SIMD-recognizer chooser 5D's
entire fast-path argument rests on **is not firing**. Pass-1's celebrated "0-edit absorption" and
5D's "recognizer chosen by IR analysis over `ByteClass` shapes" are contradictory on this exact
line.

**The fix (structural, over the arena — the elegant replacement of the string-oracle).** Rewrite
`derive_recognizers` to recognize the desugared *shape*, not a pattern string:

- a quoted-string recognizer is `Seq[Literal("\""), Repeat(ByteClass{negated, contains '"','\\'}),
  Literal("\"")]` — structurally, the `"`-delimited negated-class star;
- a numeric/accel recognizer is a digit-`ByteClass`-led `Seq` whose leading class is `[0-9]`
  (reusing `QuantifiedClassInfo`/`detect_quantified_class`, `info/mod.rs:29`);
- the general single-`ByteClass{set}` repetition contributes its `set` as a shufti alphabet.

In the top-level tree this is a **new `ByteClassRecognizer: RecognizerInfo`**
(`crates/ir/src/recognizer/mod.rs`) beside `LiteralRecognizer` — `first_bytes()` = the class set
∩ ASCII, `scanable()` = the FOLLOW-corrected one-pass flag, `feasible_engines()` = `SHUFTI |
ONE_PASS`, `accel_candidate()` from the class if singleton. The chooser then dispatches on
`RecognizerKind`, and `ByteClass` gets a real arm — the wildcard no longer decides. The prototype
gate: build a grammar with a `ByteClass` leaf, run the chooser, and assert a `SimdScan`/shufti
recognizer is emitted (not `Vec::new()` from the empty `_` fall-through). **Proof the chooser fires
is a required deliverable, not an inheritance.**

## Q5 — projections on transitions + `BackendExpr::ByteClass` reaching the tape (survivor count + the Ragel boundary)

**Structured class never reaches the emitter today.** The regex leaf travels to the tape as
`BackendExpr::RegexProgram{pattern:String, span_kind}` (`skinny/crates/ir/src/lib.rs:389-391`) —
an **opaque String**, not a structured class. There is no `BackendExpr::ByteClass`. The
typed-materialization invariant (LOCK-3-governed; every `->` reaches the tape emitter; no
value-discard; no silent epsilon) therefore obliges 5D to build the whole chain and prove it:

1. `regex_desugar: GrammarIr → GrammarIr` parses each `Regex{pattern}` via the real engine's `Hir`
   parser (`parse-that/rust/regex/src/hir/mod.rs:216`) and translates variants to `ExprKind` nodes
   (`Concat→Seq`, `Alternation→Alt{Dispatch if disjoint-FIRST else Speculative}`,
   `Repetition→Repeat`, `Literal→Literal`, `Class(Bytes)→ByteClass{set,negated}` via
   `ByteSet256::insert_range`, `Class(Unicode)→byte-expanded Alt/Seq of ByteClass` via `utf8.rs`,
   `Look→zero-width guard or Annotation`, `Empty/Group→transparent`).
2. **Survivor count (the invariant):** assert **zero `ExprKind::Regex` survive** post-pass; the
   emitter catch-alls panic on residue (no-silent-epsilon). This is checkable in a unit test over
   the three `json.bbnf` leaves + the Sheets ident `\$?[A-Za-z]{1,3}\$?\d+` (which today falls
   through to `RegexKind::Unknown`).
3. **`ByteClass` reaches the tape:** add `BackendExpr::ByteClass{set, negated}`; lower
   `ExprKind::ByteClass` to it (`passes` lowering); add the tape-emitter arm
   (`skinny/crates/codegen/src/general_descent/emit.rs`, `lower/sink_only.rs`, `lower/tape_plan.rs`)
   emitting the shufti class scan. Delete `BackendExpr::RegexProgram` and every match arm. The gate:
   a `ByteClass` leaf emits a correct, non-inert scan (contrast the consumer-less `Look` risk) and
   `RegexProgram` is gone.

**Ragel transition-actions — the `->` boundary, resolved by the thesis.** A whole-leaf projection
(`number = /…/ -> Number`, the common case) wraps the desugared subtree root ONCE — trivially one
DFA+prefilter per site, the projection applied to the whole scanned span. A **sub-pattern capture
projection** (a `->` on a capture group inside the leaf) is the genuinely hard case: it is exactly
Ragel's leaving/transition action at a capture boundary, and compiling it into one DFA forces
submatch tracking (Laurikari **tagged-DFA**), which is NOT free in one DFA+prefilter. The elegant
resolution follows directly from 5D's thesis: **a capturing regex desugars its captures into
grammar `Seq`/`Project` nodes — the grammar's OWN native span capture — not into automaton
transition-actions.** So the DFA path carries only capture-free leaves (one DFA+prefilter, clean);
capturing leaves become ordinary grammar subtrees with `Project` at the capture boundary, reaching
the tape emitter by the same path every grammar projection does. The TDFA is never built; the
grammar's projection machinery is the transition-action. The prototype must exhibit a capturing
regex leaf desugaring to `Seq[…, Project(sub), …]` and prove the projected sub-span reaches the
tape identically to a hand-authored grammar capture.

## Integration seams (path:line, tree of record noted)

- New leaf `ExprKind::ByteClass{set:ByteSet256, negated:bool}`: skinny `ir/src/lib.rs:250` (replace
  `Regex`); salvage `ByteSet256` from the `bbnf-regex` demolition. Top-level tree: mirror in its
  `ExprKind`.
- New backend leaf `BackendExpr::ByteClass{set, negated}`: skinny `ir/src/lib.rs:389` (replace
  `RegexProgram`); tape arm at `codegen/src/general_descent/emit.rs`, `lower/sink_only.rs`,
  `lower/tape_plan.rs`.
- `regex_desugar` pass: `skinny/crates/passes/src/lib.rs` (sibling of nullability/resolve_refs),
  front-end = `parse-that/rust/regex/src/hir/mod.rs:216` parser; runs before all downstream walks.
- FOLLOW-soundness fix: strengthen the `Concat`/`Repetition` arms of
  `parse-that/rust/regex/src/info/one_pass.rs:38`, OR (preferred) desugar first and let the grammar
  FOLLOW machinery decide via `AltMode::Dispatch` — `one_pass_eligible` then reads a grammar fact.
- Recognizer chooser: rewrite `skinny/crates/passes/src/lib.rs:338` `derive_recognizers` to
  structural arms; in the top-level tree add `ByteClassRecognizer: RecognizerInfo`
  (`crates/ir/src/recognizer/mod.rs`, beside `LiteralRecognizer:86`); `scanable()` reads the
  FOLLOW-corrected flag (`recognizer/mod.rs:72-74`).
- Deletions justified: `bbnf-regex` `classify`/`analyze` (3-pattern oracle); the four redundant
  first/nullability re-scans (`codegen/general_descent/regex_first.rs`,
  `json_sink_direct/first_set.rs`, `passes` `regex_first_bytes`/`regex_type`/`span_kind`); the
  QuotedString arm's `analyze` call.

## Cross-lock interlocks (named, per Constitution ix)

- **LOCK-3 governs the enum edits.** `ExprKind::ByteClass` + `BackendExpr::ByteClass` are the IR
  tower; the typed-materialization invariant binds the new leaf — `ByteClass` MUST reach the tape
  emitter (Q5). LOCK-3-3C is contingent on this ONE-HIR choice.
- **LOCK-9-9C (scanner-as-DFA) double-homes the classify re-home and the SIMD-recognizer
  selection.** Recommend the settled supersession: LOCK-5 owns the HIR→DFA→classify substrate and
  the `ByteClass`-shape recognizer chooser; 9C consumes it or retires. The shufti class kernel is
  the shared residual.
- **The prefilter/tier CSP is double-homed with LOCK-2's scan-plan CSP** (`SimdMode ×
  SimdSite × kernel`). A single home must be decided; the Q2 head-to-head feeds whichever owns it.
- **LOCK-1 owns the real type of `ByteClass`.** Does the class reuse the `Literal` byte type, or
  does a scalar-byte/UTF-8 concern leak? Route the verdict through LOCK-1's HM+row-poly+widths, not
  a stub `BuiltinTy`.
- **LOCK-6 (TS face).** The merged `ByteClass` leaf + the shufti scan crossing to TS (PROVE-14
  tape) is an unaddressed rust/ts isomorphism gap for the parse-that gestalt.

## Prior art (cited)

- **Regex-as-grammar / derivatives.** Owens–Reppy–Turon, "Regular-expression derivatives
  re-examined," *JFP* 19(2) 2009; Might–Darais–Spiewak, "Parsing with derivatives," *ICFP* 2011 —
  a char-class derivative IS the `ByteClass` scan; one language, not two, hence no functor.
- **One-pass DFA class / FIRST-FOLLOW.** burntsushi "regex internals"; `regex-automata`
  `dfa::onepass`; Aho–Sethi–Ullman FOLLOW-disjointness — the exact criterion the committed
  predicate under-implements (Q3).
- **Tagged / submatch DFA.** Laurikari, "NFAs with tagged transitions" (2000); Ragel
  entering/leaving/transition actions (Thurston) — the model 5D DECLINES for sub-captures, routing
  them to grammar `Project` instead (Q5).
- **Separate-lexer null hypothesis.** re2c / logos / flex / tree-sitter keep a separate generated
  lexer DFA; 5D wins only where the FOLLOW-sound one-pass fraction is high enough that the separate
  automaton is dead weight on the generator path (Q1).
- **PROVE-10** — start-state memchr prefilter over an untouched DFA, 8.8–29.5×; evidences the
  common case is prefilterable-and-one-pass, NOT that class-scan beats it (the null to tie/beat, Q2).
- **shufti/NEON class kernel.** Langdale (Hyperscan), R7 §3; `crates/simd-scan/src/neon.rs`
  `classify_chunk_nibble` — the residual scan for a `ByteClass`.

## Named risks + counterexamples

- **R1 (the FOLLOW-unsound predicate, Q3 — the primary risk).** `[a-z]+z` on `"abz"` reports
  one-pass and would be lowered to a give-back-free scan that rejects a matching string. Closed
  only when the desugar routes on the FOLLOW-corrected flag; the prototype MUST fail if the naive
  flag is trusted.
- **R2 (over-rejection).** A too-eager FOLLOW check routes match-safe patterns like
  `\w+(?:[-_]\w+)*` (overlap on `_`, but tail is `\w`-led → match-equivalent) to the DFA
  needlessly. The check must reject only true match-changing give-back.
- **R3 (the chooser silently no-ops, Q4).** If `derive_recognizers` keeps `_ => {}`, every
  `ByteClass` leaf falls to the scalar path and the SIMD fast-path claim is vacuous. Gate: assert
  a shufti recognizer is emitted for a `ByteClass` grammar.
- **R4 (`ByteClass` inert at the tape, Q5).** The new `BackendExpr::ByteClass` could be as
  consumer-less as `Look` if no emitter arm produces a real scan. Gate: emit + differential-match
  vs `RegexProgram` on the JSON corpus, then delete `RegexProgram`.
- **R5 (perf tie mis-sold, Q2).** If the class scan merely ties the prefiltered DFA, 5D wins the
  architecture axis but MUST report the tie as a tie — no inherited-band perf claim.
- **R6 (Unicode leaves).** `\p{L}`/`[α-ω]*` byte-expand via `utf8.rs` to codepoint-range automata —
  rare in generator leaves, routed to DFA; a named boundary, not a hot path.

## What a prototype must test to move 5D (the decisive five)

1. **Close R2's FOLLOW gap:** strengthen the eligibility criterion (or fold it into the grammar
   FOLLOW analysis) and prove `[a-z]+z` routes AWAY from straight-line lowering, while
   `\w+(?:[-_]\w+)*` stays straight-line (match-equivalence preserved). Report both the reported
   (~92%) and FOLLOW-sound one-pass fractions over the 26-pattern corpus.
2. **Build `regex_desugar` + `ExprKind::ByteClass`** over the three `json.bbnf` leaves + the Sheets
   ident; assert **zero `ExprKind::Regex` survive** (panic-on-residue).
3. **Prove `derive_recognizers` (or a `ByteClassRecognizer`) FIRES on `ByteClass`** — emits a
   shufti/`SimdScan` recognizer, not `Vec::new()` from the wildcard fall-through.
4. **Build `BackendExpr::ByteClass` → tape** and assert byte-identical match parity vs
   `RegexProgram` on the JSON corpus; then delete `RegexProgram`. Exhibit a capturing regex leaf
   desugaring to `Seq[…, Project(sub), …]` reaching the tape as a native grammar capture (no TDFA).
5. **Run the cold-paired head-to-head:** straight-line `ByteClass` scan vs PROVE-10 prefiltered
   `Dfa::find` (N≥200, paired same-binary, `count_a == count_b`). Tie is a tie; only a measured win
   is a perf claim. Feed the result to the per-site scan-plan cost model.
