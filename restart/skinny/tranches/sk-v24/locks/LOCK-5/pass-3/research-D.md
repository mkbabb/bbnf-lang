# LOCK-5 pass-3 · family 5D — GRAMMAR-FRAGMENT

## Thesis at pass-3, sharpened by two findings

5D dissolves the regex leaf at the front: a `/pat/` in a grammar is grammar in terser notation
(`Seq`/`Alt`/`Repeat`/`Optional`/`Literal`; the only atom regex adds is the character class), so
one `regex_desugar` pass parses each leaf ONCE into native `ExprKind` nodes and every downstream
walk — nullability, first/follow, typing, lowering, emission — runs through machinery the grammar
already owns. The classical NFA→DFA survives only as ONE optional lowering target a scan-plan cost
model selects for the minority that provably needs it.

Pass-3 lands two load-bearing facts the earlier passes only asserted around. **First: the
grammar-driven regex PARSE path already exists and already threads its AST — it is not a design,
it is running code (Q9).** Second: the `->` projection is represented as an opaque *node barrier*,
not a typed edge, which is measured, not argued (Q11), and it fixes exactly where the desugar must
sit. Together they cleave 5D into a solved half (parsing) and the remaining battleground (the
generator's emit side), and they hand 5D the very fold-template it needs.

## Q9 — the grammar-driven regex path is ALREADY LIVE; only a bench wrapper still discards

The regex grammar is bootstrapped and self-hosting today. `parse-that/rust/bootstrap/regex.bbnf`
(the full L4 spec: `literal`, `class_escape`, `class_item`, `char_class`, `escape`, `quantifier`,
`group`, `atom`, recursive `alternation`) is the productive source. It generates
`parse-that/rust/parse_that/src/regex/generated.rs` (`RegexParser`, 2293 LOC), whose entry
`__regex` (`generated.rs:2276`) returns a full **typed** AST — `Option<RegexParserEnum<'a>>` — not
a bool. The AST is threaded, not discarded: `parse-that/rust/parse_that/src/regex/mod.rs:34`
`parse_generated(&str) -> Result<Hir, ParseError>` → `parse_generated_with` (`:39`) →
`host::lower(&ast, pattern, opts)` (`parse-that/rust/parse_that/src/regex/host.rs:19`, 524 LOC).
`host::lower` is the hybrid-grammar-host law made concrete: a semantic fold of the
grammar-produced `RegexParserEnum` into the bespoke `Hir` — `RegexParserEnum::alternation`,
`::concat`, `::quantified`, `::atom`, `::group_0` each map to `Hir` constructors, including
`Hir::Class(CharClass::Bytes{…})` (`host.rs:131,140,152`) and `Hir::Look(Look::Start/End)`
(`host.rs:103,104`). This satisfies constitution clause (i) for the regex parser: grammar
expresses leaf structure via the bootstrapped grammar; typed host functions carry the
context-dependent fold; the AST reaches `Hir` intact.

**The single surviving discard is a diagnostic bench wrapper, not the real path.** Two
`parse_generated` names exist: the productive one above, and `generated::parse_generated(&str) ->
bool` (`generated.rs:2288`), which runs the parser and throws the AST away (`result.is_some() &&
state.offset >= pattern.len()`) for the `regex_parse` bench; `bootstrap/src/lib.rs:22` re-exports
THAT one. So "break-the-discard" is ~90% already done — the residue is a naming collision to
resolve (rename the bool wrapper to `matches_generated`/`accepts`, or delete it and let the bench
call `parse_generated(..).is_ok()`), a de-dup, not new machinery. Evidence to bank: the productive
`mod.rs::parse_generated` returns `Hir`; the bool one is bench-only.

**De-dup `grammar/misc/regex.bbnf` (Q9's third clause).** The bbnf-lang tree carries a stale,
28-line legacy regex grammar (`grammar/misc/regex.bbnf`) that duplicates the productive spec — and
buggily: `decimal = /d+/` (`:18`) drops its backslash (should be `\d+`), it mixes old `>>`/`<<`
notation, and its `char_class`/`escape` rules diverge from the productive grammar. The canonical,
tested, self-hosting grammar is `parse-that/rust/bootstrap/regex.bbnf`. De-dup = delete the misc
copy (or make it an `@import` of the canonical) so there is ONE regex grammar, homed in parse-that
(one-regex-home, per the settled XL-C ground). Grammar-totality picture: the regex grammar is
bootstrapped and consumed today — 5D inherits a validated self-host, not a promise.

**Consequence for 5D.** The parse half of the continuum is solved by grammar. What remains is the
EMIT half: a regex leaf *inside a bbnf grammar* still travels to the tape as an opaque string
(Q8), never as grammar IR. `host::lower` is the exact template for closing it — the same fold that
turns `RegexParserEnum → Hir` re-targets to turn a grammar `Regex` leaf → grammar `ExprKind`
(`ByteClass`). The machinery exists; it needs re-pointing from `Hir` to the grammar's own node
arena. SPEC-ONLY(grammar-driven) rides on this until LOCK-7/7A.

## Q11 — the `->` is a MAP-BARRIER NODE, not a typed edge (measured); it fixes desugar placement

The concrete representation of `->` is settled by measurement. Over a corpus of 33 `Alt`-of-regex
patterns built in two topologies — bare `Alt([Regex,…])` versus projected `Alt([Map(Regex,Tag),…])`,
the exact shape `(a -> A | b -> B)` lowers to — the grammar-tier regex e-graph
(`crates/ir/src/egraph/rules/regex.rs`) applied **zero** rewrites on the projected form
(`total_applied=0`, `final_nodes==initial_nodes`, no unions), against 3/11/19/30 fires per rule on
the bare form. The reason is structural: each regex rule resolves a branch via `class_regex(child)`,
which scans the child e-class for a bare `GrammarENode::Regex`; a `Map{inner, fn_id}` e-class holds
no such node, so every rule returns `None`. **The `->` is an opaque node that inner-class analysis
cannot see through.** This is binding on 5D's bridge projection soundness in a precise way:

- If the desugar runs AFTER projection wrapping, the `Map` barrier hides the inner `Regex` — the
  desugar (and any FIRST/FOLLOW/e-graph reasoning about the class) never fires. So **`regex_desugar`
  MUST run before projection lowering**, on the raw `Regex` leaf, OR the projection must be
  re-expressed as a *typed edge* the class analysis can traverse. LOCK-1 owns which; the measured
  fact is that TODAY it is a barrier, so desugar-first is the safe, no-new-machinery route.
- The whole-leaf projection (`number = /…/ -> Number`, the common case) wraps the desugared subtree
  root ONCE — the projection applies to the whole scanned span, trivially sound: desugar first, wrap
  after. This is the majority shape and needs nothing from tagged automata.
- A sub-pattern capture projection (a `->` on a capture group *inside* the leaf) is the genuinely
  hard case — Ragel's leaving/transition action at a capture boundary, which one DFA would force
  into Laurikari tagged-DFA submatch tracking. 5D DECLINES the TDFA: a capturing regex desugars its
  captures into grammar `Seq`/`Project` nodes — the grammar's OWN native span capture — so the DFA
  path carries only capture-free leaves (one DFA+prefilter, clean), and the capture boundary is an
  ordinary grammar `Project` reaching the tape the way every grammar projection does. The `Map`
  barrier is then not a problem but the mechanism: the capture IS a projection node.

So Q11's ratified representation — a `Map`/`Project` node barrier — is not a hazard to route around;
it is the resolution, provided `regex_desugar` precedes it. The one obligation on LOCK-1: confirm
the `->` stays a node (Map barrier) and that whole-leaf projection composes as
`Project(desugared_root)`, so inner-class rewrites operate pre-wrap and the projection wraps the
result. If LOCK-1 instead makes `->` a typed edge, desugar could run either side — but the measured
status quo is the barrier, and desugar-first is correct under both.

## Q7 — the FOLLOW-sound fraction: raw 18.8%, effective ~75%; the DFA retreats to a narrow fallback

The single number gating the generator-path architecture is measured. Over the live corpus — 15
`json.bbnf` rules + 39 `google-sheets.bbnf` rules + an injected `word=/[a-z]+/; sentence = word
"and" word;` counterexample, 16 `Regex` leaves — a per-`ExprId` FOLLOW fixed point (LLVM-style
`while changed`, dual to `first_bytes` at `skinny/crates/passes/src/lib.rs:911`) with
`first(C)∩FOLLOW(leaf)=∅`:

- **raw brief-literal sound = 3/16 (18.8%)** — the sound FLOOR of a *conservative* predicate that
  over-flags on leading-first bytes.
- Adjudicated: **6/16 bounded** (`{1,3}`, fixed strings, delimiter-terminated `\(`) — give-back
  structurally impossible, one-pass by construction, DFA never needed. **3/16 genuine give-back**
  (`word` vs following `"and"`; `sheets::string` doubled-quote `""` escape; `sheets::identifier`
  trailing `[A-Za-z0-9_.]*` abutting letters) — these and ONLY these require `Seq[Repeat,
  FOLLOW-guard]` or a DFA/backtracking fallback. **3/16 conservative false-positives** (`json::ws`,
  `sheets::cell_ref`, `sheets::range_end`) — actually one-pass-safe (loop-class FOLLOW-disjoint, or
  follower re-consumes the same nullable class). **3/16 truly sound**, **1/16 analyzer-opaque**
  (`sheets::number`: `analyze().first=UNKNOWN`, over-approximated to all-bytes → forced
  non-disjoint — an analyzer limitation, not a grammar property).
- **Effective one-pass ≈ 12/16 (75%)**: only 3 of 16 leaves genuinely keep the DFA in play.

The counterexample lands correctly: `word=/[a-z]+/` before `"and"` is flagged NON-disjoint
(`overlap=[a]`), exactly where the committed `check_one_pass_eligible`
(`parse-that/rust/regex/src/info/one_pass.rs`) is UNSOUND — its `Repetition` arm recurses into
`[a-z]` and returns one-pass=`true`, greenlighting a straight-line lowering that would reject
`"a and b"`. The FOLLOW test refuses it. So I-1 resolves toward the survivor's policy: **the direct
class-scan is the default generator path, gated by a FOLLOW-guard** (`desugar → Seq[Repeat,
suffix]`, let the grammar's own FOLLOW / `AltMode::Dispatch` decide), with the DFA/backtracking
retreating to a narrow fallback for the genuine-give-back trio. Read correctly, the number says the
DFA retreats.

**The one open soundness gap (pass-3's named residue).** `analyze().first` UNDER-approximates
alternations — for `sheets::sheet_prefix` (`/'…'!|[A-Za-z_]\w*!/`) it returns `Exact{'\''}`,
silently dropping the `[A-Za-z_]…` branch. A production FOLLOW-sound predicate MUST draw `first(C)`
from an OVER-approximation — the DFA start-state class set (or a fixed-up analyzer). As-is, the
"truly sound" verdicts (`sheet_prefix` in particular) are not reliably sound. This is the single
prototype gate that hardens Q7 from "measured with a caveat" to "measured sound": re-run the
fraction with `first(C)` sourced from `Dfa::from_hir(...)` start-state class, confirming the
counterexample and give-back trio stay non-disjoint while the conservative trio moves into the sound
set (target ≈12/16 sound, not merely effective).

## Q8 — the emit side: RegexProgram is opaque today; the ByteClass→tape chain is the deliverable

Today the regex leaf reaches the tape as `BackendExpr::RegexProgram{pattern:String, span_kind:
SpanKind}` (`skinny/crates/ir/src/lib.rs:389`) — an OPAQUE String, matched at `codegen/src/lib.rs:1444`,
`codegen/src/general_descent/emit.rs:365` (`SinkOnlyExpr::RegexProgram`), `json_sink_direct/first_set.rs:77`,
`json_sink_direct/dispatch.rs:86`. There is NO `BackendExpr::ByteClass`, and the generator does NOT
yet emit `dfa.find` (that is the PT-1.W5 consumption seam, interlocked with GN-2.W1 — the recognizer
dispatch decides `find_member` vs `dfa.find` per site, no double-emit). The typed-materialization
invariant (every `->` reaches the tape; no value-discard; no silent epsilon — LOCK-3-governed)
obliges 5D to build and prove the full chain:

1. **`regex_desugar: GrammarIr → GrammarIr`** (new `passes` pass, sibling of nullability/resolve_refs
   at `skinny/crates/passes/src/lib.rs`), front-ended by the SAME fold the grammar-driven path
   already runs — `host::lower` re-targeted from `Hir` to `ExprKind`: `Concat→Seq`,
   `Alternation→Alt{Dispatch if disjoint-FIRST else Speculative}`, `Repetition→Repeat`/`Optional`,
   `Literal→Literal`, `Class(Bytes)→ByteClass{set:ByteSet256, negated}`, `Class(Unicode)→byte-expanded
   Alt/Seq of ByteClass` (via `utf8.rs`), `Look→zero-width guard or Annotation`,
   `Empty/Group→transparent`. Runs BEFORE projection wrapping (Q11).
2. **Survivor invariant:** assert ZERO `ExprKind::Regex` survive post-pass; emitter catch-alls panic
   on residue (no-silent-epsilon). Checkable over the three `json.bbnf` leaves + the Sheets ident
   `\$?[A-Za-z]{1,3}\$?\d+`.
3. **`ByteClass` reaches the tape:** add `ExprKind::ByteClass{set, negated}`
   (`skinny/crates/ir/src/lib.rs:250`, replacing `Regex`; nullability arm today at `:331`,
   `analyze(pattern).nullable`, becomes structural) and `BackendExpr::ByteClass{set, negated}`
   (`:389`, replacing `RegexProgram`); the emit arm (`general_descent/emit.rs:365`,
   `lower/sink_only.rs`, `lower/tape_plan.rs`) emits the shufti class scan
   (`while set.contains(input[pos]) { pos+=1 }`, residualized to `crates/simd-scan/src/neon.rs`
   `classify_chunk_nibble`, the `vqtbl1q_u8` nibble-LUT). Delete `RegexProgram` and its four
   redundant re-scans (`general_descent/regex_first.rs`, `json_sink_direct/first_set.rs`, `passes`
   `regex_first_bytes`/`regex_type`/`span_kind`).
4. **The chooser must FIRE:** `skinny/crates/passes/src/lib.rs` `derive_recognizers` matches only two
   shapes and drops the rest to `_ => {}` — a desugared `ByteClass` is silently swallowed, so the
   SIMD fast-path is vacuous unless rewritten to recognize the *shape* (quoted-string =
   `Seq[Literal("\""), Repeat(ByteClass{negated,∋'"','\\'}), Literal("\"")]`; digit-led numeric;
   general single-`ByteClass` repetition → its `set` as a shufti alphabet). In the richer tree add a
   `ByteClassRecognizer: RecognizerInfo` beside `LiteralRecognizer` (`crates/ir/src/recognizer/mod.rs`)
   — `first_bytes()`=class∩ASCII, `scanable()`=the FOLLOW-corrected flag,
   `feasible_engines()`=`SHUFTI|ONE_PASS`. Gate: build a `ByteClass` grammar, run the chooser, assert
   a shufti recognizer is emitted (not `Vec::new()`).

**The decisive unmeasured figure (Q8's head-to-head).** Cold-paired (N≥200, same-binary lanes,
`count_scalar==count_prefilter`): straight-line `ByteClass` scan vs PROVE-10's prefiltered
`Dfa::find` (start-state memchr over the DFA start-transition row; 8.8–29.5× on literal-prefix-led
patterns; 0.97–1.01× on `[a-z]+`/`\w+` controls). The honest prior: the two **TIE on class-led
repetitions** (both reduce to a class-membership scan; PROVE-10's own `[a-z]+` control at ~1.0× is
direct evidence the prefilter buys nothing there, so the class scan cannot lose and can only win by
dropping the transition-table indirection), and the **prefiltered DFA WINS on literal-prefix-led
leaves** (`0x[0-9a-fA-F]+`, `https?://…`, `#…`) because memchr on a rare prefix byte skips faster
than scanning a common class. A tie is a tie (R6): 5D wins the architecture axis (four parsers →
one, no functor, no automaton on the class-led hot path) even at parity, but claims NO perf win
absent a measured one. The result feeds a per-site scan-plan cost model: class-led → straight-line
scan; literal-prefix-led → prefiltered DFA.

**Capture-leaf → `Project`, no TDFA (Q8's last clause).** Exhibit a capturing regex leaf desugaring
to `Seq[…, Project(sub), …]` and prove the projected sub-span reaches the tape byte-identically to a
hand-authored grammar capture — the grammar's projection machinery IS the transition-action; the
TDFA is never built.

## The 1148-LOC classify: re-home under 5D, not re-argue

`parse-that/rust/regex/src/classify.rs` (1148 LOC; `RegexClass` enum `:55`, `classify_regex` `:157`,
`classify_regex_from_hir` `:177`, `canonical_pattern` `:132`) is a pattern-string oracle 5D largely
DELETES on the generator path: once `regex_desugar` translates the leaf to `ExprKind::ByteClass`
shapes, the structural recognizer reads the desugared *shape*, not a classified pattern string. The
`analyze()`/`classify` 3-pattern oracle (the QuotedString keying in `derive_recognizers`) becomes
dead code matching a variant that no longer exists. What survives is the thin `info/classify.rs`
(153 LOC) feeding `RegexInfo` for the DFA fallback minority. A re-home that SHRINKS the
generator-path surface, not a relocation — the class analysis moves into the IR passes the grammar
already runs.

## Integration seams (path:line)

- `regex_desugar` pass: `skinny/crates/passes/src/lib.rs` (sibling of nullability); front-end =
  re-targeted `parse-that/rust/parse_that/src/regex/host.rs:19` `lower` fold (proven template).
- New leaf `ExprKind::ByteClass{set:ByteSet256, negated}`: `skinny/crates/ir/src/lib.rs:250`
  (replace `Regex`; salvage `ByteSet256` from the `bbnf-regex` demolition). Nullability arm at
  `:331` → structural on `ByteClass`.
- New backend leaf `BackendExpr::ByteClass{set, negated}`: `skinny/crates/ir/src/lib.rs:389`
  (replace `RegexProgram`); tape arm at `codegen/src/general_descent/emit.rs:365`,
  `codegen/src/lib.rs:1444`, `lower/sink_only.rs`, `lower/tape_plan.rs`.
- Chooser: rewrite `skinny/crates/passes/src/lib.rs` `derive_recognizers` to structural arms; add
  `ByteClassRecognizer: RecognizerInfo` at `crates/ir/src/recognizer/mod.rs` beside
  `LiteralRecognizer:86`; `scanable()` reads the FOLLOW-corrected flag (`recognizer/mod.rs:72-74`).
- FOLLOW soundness: source `first(C)` from the DFA start-state class set (over-approximation), not
  `analyze().first` (under-approximates alternations); OR desugar first and let the grammar FOLLOW
  machinery decide via `AltMode::Dispatch` — `one_pass_eligible` then reads a grammar fact.
- Grammar-driven parse (Q9): productive path `parse-that/rust/parse_that/src/regex/mod.rs:34` +
  `host.rs:19`; bench-only bool discard `generated.rs:2288` (rename/de-dup); delete legacy
  `grammar/misc/regex.bbnf` in favor of `parse-that/rust/bootstrap/regex.bbnf`.

## Cross-lock interlocks (Constitution ix)

- **LOCK-1** owns the real type of `ByteClass` (reuse `Literal`'s byte type, or a scalar-byte/UTF-8
  concern that must NOT leak into the shared type — route via HM+row-poly+widths, not a stub
  `BuiltinTy`) AND ratifies the `->` representation (Q11): the measured status quo is a `Map`
  barrier node, which makes desugar-first correct; a typed edge would let desugar run either side.
- **LOCK-3** governs the `ExprKind`/`BackendExpr` enum edits and binds the typed-materialization
  invariant to the new leaf — `ByteClass` MUST reach the tape (Q8); LOCK-3-3C (grammar-homoiousia)
  is contingent on this ONE-HIR choice.
- **LOCK-7/7A** owns the grammar-totality gate: the regex grammar is bootstrapped and self-hosting
  TODAY (Q9) — 5D's SPEC-ONLY(grammar-driven) rides on a validated self-host, and the de-dup
  (`grammar/misc/regex.bbnf` retired) is a LOCK-7 housekeeping consequence.
- **LOCK-9-9C** (scanner-as-DFA) double-homes the `ByteClass`-shape recognizer chooser; recommended
  supersession: LOCK-5 owns the HIR→ByteClass→shufti substrate, 9C consumes or retires; the shufti
  class kernel (`classify_chunk_nibble`) is the shared residual.
- **PT-1.W5 ⇄ GN-2.W1**: the generator's regex-leaf lowering must pick ONE of `dfa.find`
  (prefiltered DFA, literal-prefix-led) vs the shufti class scan (`ByteClass`, class-led) per site —
  the recognizer dispatch is the single decision point; no double-emit.
- **LOCK-6** (TS face): the merged `ByteClass` leaf + shufti scan crossing to TS (PROVE-14 tape) is
  an open rust/ts isomorphism gap for the parse-that gestalt.

## Prior art (cited)

- **Regex-as-grammar / derivatives.** Owens–Reppy–Turon, "Regular-expression derivatives
  re-examined," JFP 19(2) 2009; Might–Darais–Spiewak, "Parsing with derivatives," ICFP 2011 — a
  char-class derivative IS the `ByteClass` scan; one language, hence no functor.
- **One-pass DFA / FIRST-FOLLOW.** `regex-automata` `dfa::onepass`; Aho–Sethi–Ullman
  FOLLOW-disjointness — the exact criterion the committed `check_one_pass_eligible`
  under-implements (its `Repetition` arm never checks the class against FOLLOW).
- **Self-hosted parser + semantic fold.** The bootstrap grammar + `host::lower` fold is the
  hybrid-grammar-host pattern — grammar for structure, typed host fns for the context-dependent fold
  (cf. tree-sitter external scanners, `happy` semantic actions). Here it is already running (Q9), not
  proposed.
- **Tagged / submatch DFA.** Laurikari, "NFAs with tagged transitions" (2000); Ragel
  entering/leaving/transition actions (Thurston) — the model 5D DECLINES for sub-captures, routing
  them to grammar `Project` (Q8/Q11).
- **Separate-lexer null hypothesis.** re2c / logos / flex / tree-sitter keep a separate generated
  lexer DFA; 5D wins only where the FOLLOW-sound one-pass fraction is high enough (≈75% effective,
  Q7) that the separate automaton is dead weight on the generator path.
- **PROVE-10** — start-state memchr prefilter over an untouched DFA, 8.8–29.5×; its `[a-z]+` control
  at ~1.0× is the tie evidence for the class-scan head-to-head (Q8).
- **shufti/NEON class kernel.** Langdale (Hyperscan); `crates/simd-scan/src/neon.rs`
  `classify_chunk_nibble` — the residual scan for a `ByteClass`.

## Named risks + counterexamples

- **R1 (FOLLOW-unsound first-set source, the pass-3 primary).** `analyze().first`
  under-approximates alternations (`sheet_prefix` drops its `[A-Za-z_]…` branch), so a "sound"
  verdict can be unsound. Closed only when `first(C)` is drawn from the DFA start-state class
  over-approximation; the prototype MUST re-run the fraction soundly and keep the give-back trio +
  counterexample non-disjoint.
- **R2 (desugar after projection wrapping).** If `regex_desugar` runs after the `Map` barrier is
  placed, the inner `Regex`/class is invisible (Q11: proj e-graph work=0) — FOLLOW/e-graph/typing
  never fire on the class. Gate: desugar-first ordering, asserted by a pass-order test.
- **R3 (the chooser silently no-ops).** `derive_recognizers` `_ => {}` swallows `ByteClass`; the
  SIMD fast-path is vacuous. Gate: assert a shufti recognizer is emitted for a `ByteClass` grammar.
- **R4 (`ByteClass` inert at the tape).** The new `BackendExpr::ByteClass` could be as consumer-less
  as `Look` if no emit arm produces a real scan. Gate: emit + differential-match vs `RegexProgram`
  on the JSON corpus, THEN delete `RegexProgram`.
- **R5 (perf tie mis-sold).** If the class scan ties the prefiltered DFA, 5D wins the architecture
  axis but MUST report the tie as a tie — no inherited-band perf claim.
- **R6 (Unicode leaves).** `\p{L}`/`[α-ω]*` byte-expand via `utf8.rs` to codepoint-range automata —
  rare in generator leaves, routed to DFA; a named boundary, not a hot path; byte-expansion confined
  to automata, never leaking into the shared type.

## What a prototype must test to move 5D (pass-3's decisive four)

1. **Re-run Q7 SOUND.** Source `first(C)` from the DFA start-state class over-approximation; report
   the sound fraction (target ≈12/16), confirm the give-back trio (`word`/`sheets::string`/
   `sheets::identifier`) + the `word`/`"and"` counterexample stay non-disjoint and the conservative
   trio (`ws`/`cell_ref`/`range_end`) moves into the sound set.
2. **Build `regex_desugar` + `ExprKind::ByteClass`** over the three `json.bbnf` leaves + the Sheets
   ident, re-targeting the `host::lower` fold from `Hir` to `ExprKind`; assert ZERO `ExprKind::Regex`
   survive (panic-on-residue) AND desugar runs before projection wrapping.
3. **`BackendExpr::ByteClass` → tape**, chooser FIRES (shufti recognizer, not `Vec::new()`),
   byte-identical match parity vs `RegexProgram` on the JSON corpus, then delete `RegexProgram`;
   exhibit a capturing leaf desugaring to `Seq[…, Project(sub), …]` reaching the tape as a native
   capture (no TDFA).
4. **Cold-paired head-to-head:** straight-line `ByteClass` scan vs PROVE-10 prefiltered `Dfa::find`
   (N≥200, paired, `count_a==count_b`); feed the per-site scan-plan cost model; tie is a tie.
