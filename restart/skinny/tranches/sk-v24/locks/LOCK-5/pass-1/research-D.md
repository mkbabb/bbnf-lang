# LOCK-5 pass-1 · family 5D — GRAMMAR-FRAGMENT

## Thesis (strongest form)

A regex literal in a grammar is not a foreign engine embedded in a rule — it *is* grammar in a
terser notation. Concatenation is `Seq`, `|` is `Alt`, `*`/`+`/`?`/`{m,n}` are `Repeat`/`Optional`,
a literal byte is `Literal`; the ONLY construct a regex adds that the grammar IR lacks is the
character-class atom. So 5D dissolves the regex leaf at the front: a single desugar pass parses
each `/pat/` ONCE into native `ExprKind` nodes of the same IR arena, and from that point there is
**no separate regex engine on the generator path** — nullability, first/follow, typing, lowering,
and emission all run through the machinery the grammar already owns. The standalone regex API
survives as a thin front: it parses a pattern to a one-rule IR and rides the same lowering; the
classical NFA→DFA path is retained only as ONE *optional lowering target* the scan-plan cost model
selects for the leaves that provably need it (ambiguous/backtracking patterns, adversarial
standalone input), never as the default generator path.

This is the deepest reading of Constitution clause (i) (grammar totality) and clause (ii)
(isomorphism): there is no functor to maintain between two HIRs (contrast 5B) because there is one
HIR — the grammar's — and no automaton on the hot path to keep in sync (contrast 5A/5C).

**The decisive in-tree fact that makes 5D concrete, not aspirational:** the real regex engine's
`Hir` enum already maps almost 1:1 onto `ExprKind`, and the exact combinator-vs-DFA decision
predicate (`check_one_pass_eligible`) **already exists and is committed**. 5D is a *translation +
routing* pass over pre-built analyses, not the invention of a new criterion.

## Two crates, do not conflate them

- `skinny/crates/bbnf-regex/` (~300 LOC) is the DEGENERATE classifier on the generator path today:
  `classify` (`src/lib.rs:92`) string-equals `"[ \t\n\r]*"`, tests `starts_with('"')`, matches one
  numeric prefix; everything else → `RegexKind::Unknown` (`:16`). `analyze` (`:45`) wraps it. This
  is a three-pattern lookup table, and it is 5D's demolition target.
- `parse-that/rust/regex/` (6742 LOC, READ-ONLY; GN-1 vendors it in place of the current partial
  `skinny/crates/parse-that-regex/` which holds only the string/number/unicode kernels) is the REAL
  engine: `hir/mod.rs`, `automata/{nfa,dfa,accel}.rs`, `info/` (classify, one_pass, first, width,
  literal_prefix), `egraph/`, `sets/{byteset,charset,equiv}`. This is the desugar's front-end (its
  `Hir` parser) and the retained DFA lowering target.

## The disease this cures (measured, in-tree)

The regex pattern travels the skinny pipeline as an **opaque `String`** re-parsed by **four
independent, mutually-redundant, overfit hand parsers**, each degrading to empty on shapes it cannot
read:

1. `bbnf-regex/src/lib.rs:45` `analyze` → `:92` `classify` (the 3-pattern table above). Consumed by
   nullability at `ir/src/lib.rs:331` (`ExprKind::Regex { pattern } => analyze(pattern).nullable`)
   and by `passes/src/lib.rs:911`.
2. `codegen/src/general_descent/regex_first.rs` (231 LOC) — a *second* leading-token parser
   (`first_bytes`, `is_nullable`) whose own doc admits it "degrades to an empty set on anything it
   cannot read," re-deriving nullability the classifier already computed.
3. `codegen/src/json_sink_direct/first_set.rs:97` `regex_first_bytes` — a *third* first-byte deriver
   on the sink path (dispatched at `:77`).
4. `passes/src/lib.rs:943` `regex_first_bytes`, `:224` `regex_type`, `:1664` `span_kind` — a *fourth*
   set of string re-scans for the type plane and span selection.

The leaf is carried opaque all the way to `BackendExpr::RegexProgram { pattern, span_kind }`
(`ir/src/lib.rs:389`), lowered at `passes/src/lib.rs:1341`, emitted at
`codegen/src/general_descent/emit.rs:365`, `codegen/src/lib.rs:1444`, `json_sink_direct/skeleton.rs`.
The real grammar carries exactly three regex leaves (`skinny/grammars/json.bbnf:4-6`):

```
number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;
string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
ws     = /[ \t\n\r]*/ ;
```

— precisely the three the classifier hard-codes. A fourth leaf (the Sheets ident
`\$?[A-Za-z]{1,3}\$?\d+`) already falls through to `Unknown`/empty. This is not an engine; it is
overfit pattern recognition, and it is 5D's whole target.

## The mechanism, made concrete

**One new IR leaf.** `ExprKind` (`ir/src/lib.rs:234`) today has `Seq(children)` (`:235`),
`Alt { branches, mode }` (`:236`), `Repeat { body, min, max }` (`:240`), `Optional(ExprId)` (`:245`),
`Literal { bytes }` (`:246`), `Regex { pattern }` (`:250`) — every regex operator except the class
atom. Add exactly one and delete `Regex`:

```rust
ByteClass { set: ByteSet256, negated: bool }   // ByteSet256 already in-tree: bbnf-regex/src/lib.rs:35
```

`ByteSet256` (four `u64` words, `insert`/`insert_range`/`contains`/`to_vec`) is the exact
class-membership primitive; it survives the `bbnf-regex` demolition as the one salvaged type.
Codepoint classes beyond ASCII lower to a small `Alt`/`Seq` of `ByteClass` over UTF-8 byte ranges —
the automaton's byte-expansion job (`parse-that/rust/regex/src/utf8.rs`), done once at desugar.

**One desugar pass**, `regex_desugar`, a `GrammarIr → GrammarIr` pass sited in `passes/` alongside
`resolve_refs`/`nullability`, running before every downstream analysis. For each
`ExprKind::Regex { pattern }`: parse with the REAL engine's HIR parser
(`parse-that/rust/regex/src/hir/mod.rs`, the `Hir` type at `:216`), then translate its variants into
`ExprKind` children allocated via `GrammarIr::add_expr`, replacing the `Regex` node with the subtree
root. The translation is near-mechanical because the shapes coincide:

| `Hir` variant (`hir/mod.rs`) | → `ExprKind` |
|---|---|
| `Concat(Vec<Hir>)` (`:230`) | `Seq(children)` |
| `Alternation(Vec<Hir>)` (`:232`) | `Alt { mode }` — `Dispatch` when branch FIRST-sets disjoint (see below), else `Speculative` |
| `Repetition { sub, min, max }` (`:194/:226`) | `Repeat { min, max }` |
| `Literal(Vec<u8>)` (`:220`) | `Literal { bytes }` |
| `Class(CharClass::Bytes { ranges, negated })` (`:222/:45`) | **`ByteClass { set, negated }`** (ranges → `ByteSet256::insert_range`) |
| `Class(CharClass::Unicode …)` | byte-expanded via `utf8.rs` → `Alt`/`Seq` of `ByteClass` |
| `Look(Look)` (`:224`) | boundary predicate (zero-width guard node) or DFA-routed |
| `Empty` (`:218`), `Group(sub)` | dropped / made transparent |

**Post-pass invariant:** zero `ExprKind::Regex` survive (the emitter catch-alls panic on residue —
the no-silent-epsilon law). This DELETES, wholesale: `bbnf-regex`'s `classify`+`analyze`,
`general_descent/regex_first.rs` (all 231 LOC), `json_sink_direct/first_set.rs`'s `regex_first_bytes`,
`passes`' `regex_first_bytes`/`regex_type`/`span_kind`, and `BackendExpr::RegexProgram` itself.
Nullability (`ir/src/lib.rs:320`) loses the `analyze` import (`:11`/`:171`) — the `Regex` arm at
`:331` vanishes, `ByteClass` is a trivial non-nullable leaf (`min==0` repeat is the only nullable
source, already handled at `:328`). First-set (`passes/src/lib.rs:880+`) loses its `Regex` arm;
`ByteClass` contributes its `set` directly. Four ad-hoc parsers collapse to zero because the ONE real
parse now feeds the ONE set of grammar walks.

## The combinator-vs-DFA decision is ALREADY BUILT (not a new prototype)

The single load-bearing question for 5D — "which leaves can lower to straight-line combinator scans
vs which must route to the DFA" — is answered by a committed predicate:
`parse-that/rust/regex/src/info/one_pass.rs:38` `check_one_pass_eligible(hir)`. It returns true iff
**no alternation has overlapping FIRST sets** — pairwise `CharSet128::is_disjoint` over
`first_chars_of_hir` (`:44-60`), recursing through `Concat`/`Repetition`/`Group`. Its sibling
`is_hir_walkable` (imported at `info/mod.rs:33`) returns false for the constructs the inline byte
walker cannot handle (lazy quantifiers outside `.*?literal`, Unicode props beyond ASCII,
backreferences, non-anchor lookaround). Both surface on `RegexInfo` (`info/mod.rs:137`) as
`one_pass_eligible: bool` (`:162`), computed at `analyze_from_hir` (`:234`, calls
`check_one_pass_eligible` at `:305`).

The profound alignment: `check_one_pass_eligible`'s disjoint-FIRST criterion is the SAME
first/follow disjointness the grammar's `Alt`-dispatch already computes (`passes/src/lib.rs` first-set
machinery, `AltMode::Dispatch` at `ir/src/lib.rs:274`). So the determinism of a regex fragment is a
**grammar analysis 5D already owns**, and the `Alt { mode }` it emits is `Dispatch` exactly when the
engine's `one_pass_eligible` is true. 5D reads a flag; it does not build a subset construction.

**Both real json leaves ARE one-pass eligible** (the prototype checks this by *running*
`check_one_pass_eligible`): `number`'s alternation `(0|[1-9]\d*)` has FIRST `{0}` vs `{1..9}` —
disjoint; `string`'s `(?:[^"\\]|\\…)` has FIRST `[^"\\]` vs `{\}` — disjoint. This is the evidence
that "no engine on the generator path" holds for the real corpus, and it is *checkable in-tree*, not
asserted.

## The SIMD-recognizer chooser is also ALREADY BUILT

`span_kind`/`RegexKind::{QuotedString,Numeric,Whitespace}` (the SIMD-scanner selectors, used at
`json_sink_direct/dispatch.rs:163`, `passes/src/lib.rs:1664`) are recovered *structurally* from the
desugared shape — and the detectors exist: `info/mod.rs:29` imports `detect_accel_candidate`,
`detect_negated_class`, `detect_quantified_class`, backed by `info/classify.rs` (the 1148-LOC
`classify` the charter names — re-homed, not re-derived) and `RegexInfo::quantified_class`
(`info/mod.rs:148`). A `ByteClass{negated:true}*` bracketed by `"`-delimiter `Literal` IS the
quoted-string recognizer; a digit-`ByteClass`-led `Seq` with `detect_quantified_class == Some(…)` IS
the numeric/accel recognizer. So `Recognizer::SimdScan` (`ir/src/lib.rs:417`; the bbnf-simd
shufti/NEON kernels, PROVE-01/03) is chosen by an IR analysis over `ByteClass` shapes and the
existing `QuantifiedClassInfo`, not a pattern-string equality test. This is the seam that keeps the
PROVE-04/PROVE-01 structural-scan fast paths while erasing the string oracle.

## Why no DFA on the generator path (the determinism thesis — and its honest limit)

A one-pass leaf emits as a straight-line char-class scan loop — the same inner loop as a DFA `find`
but with *no transition table*, because a single `ByteClass` needs no states:
`while set.contains(input[pos]) { pos += 1 }`, residualized to branch-free NEON via the
shufti/`vqtbl1q_u8` class kernel (`crates/simd-scan/src/neon.rs` `classify_chunk_nibble`; R7 §3,
PROVE-01). This should be *faster* than a table DFA (no indirection, one membership test).

**Honest limit (correcting a tempting over-claim):** PROVE-10 (`prototypes/PROVE-10/REPORT.md`) is a
start-state **memchr prefilter over an untouched DFA** — 9.6–29× on prefilterable patterns, 44/44
tests, additive. It proves the *prefiltered-DFA* path is fast; it does NOT prove combinator lowering
beats it. What PROVE-10 *does* evidence for 5D is that its four certified prefilterable patterns
(`0x[0-9a-fA-F]+`, `#[0-9a-fA-F]{3,6}`, `https?://…`, `(?:foo|bar)[a-z]*`) are all one-pass eligible
(checkable via `check_one_pass_eligible`) — i.e. the common case IS combinator-lowerable. Whether the
straight-line class scan *outperforms* the prefiltered DFA on those leaves is THE unmeasured question
the prototype must settle head-to-head; the prefiltered DFA is the null hypothesis to beat or match.

The DFA is not deleted; it is re-homed as one lowering target chosen by the scan-plan cost model
(LOCK-2's engine) only for leaves where `one_pass_eligible == false || !is_hir_walkable`. On the
generator path that set is measured-near-empty; on the standalone API it is whatever the caller throws.

## The standalone API as a thin front

`Regex::new(pat).find(text)` parses `pat` → a one-rule `GrammarIr` → the same `regex_desugar` → the
same lowering. The public contract (linear time, no catastrophic backtracking) is preserved because
the thin front routes patterns with `one_pass_eligible == false` to the DFA lowering — the front is
thin in CODE (shares the IR, the passes, the emit spine) while the combinator-vs-DFA choice is the
same cost-model decision the generator uses. Thin does not mean lossy: `(a+)+b` (not one-pass) goes
to the DFA and stays linear; `#[0-9a-fA-F]{3,6}` goes to the straight-line class scan. The engine's
full test suite + the `egraph_*`/`algebra_*`/`info` test corpus (`parse-that/rust/regex/tests/`) is
the byte-identical parity floor.

## Integration seams (path:line)

- New leaf: `ir/src/lib.rs:234` (`ExprKind`), delete `Regex` (`:250`), salvage `ByteSet256`
  (`bbnf-regex/src/lib.rs:35`).
- New pass: `passes/src/lib.rs` (sibling of `nullability`), consumes `parse-that/rust/regex`'s `Hir`
  parser (`hir/mod.rs:216`) and reads `RegexInfo::one_pass_eligible` (`info/mod.rs:162`).
- Deletions: `bbnf-regex/src/lib.rs:92` `classify` + `:45` `analyze` (crate reduced to `ByteSet256`);
  `codegen/src/general_descent/regex_first.rs` (whole file, 231 LOC);
  `json_sink_direct/first_set.rs:97` + `:77`; `passes/src/lib.rs:943`/`:224`/`:1664`/`:911`;
  `BackendExpr::RegexProgram` (`ir/src/lib.rs:389`) and every match arm (`passes:1341`, `emit.rs:365`,
  `codegen/lib.rs:1444`, `skeleton.rs`).
- Nullability edit: `ir/src/lib.rs:11`/`:171` (drop `analyze` import/use), `:331` (drop `Regex` arm).
- Grammar front already parses `/pat/` → `ExprKind::Regex` at `grammar/src/lib.rs:1512` (`skip_regex`
  `:1166`) — unchanged; the desugar runs after it.
- SIMD recognizer selection: `Recognizer::SimdScan` (`ir/src/lib.rs:417`); the structural chooser
  reads `detect_quantified_class`/`detect_negated_class`/`detect_accel_candidate` (`info/mod.rs:29`)
  and replaces `span_kind` at `dispatch.rs:163`, `passes:1664`.

## Prior art (cited)

- **Brzozowski derivatives / regex-as-grammar** — Owens, Reppy, Turon, "Regular-expression
  derivatives re-examined," *JFP* 19(2), 2009; Might, Darais, Spiewak, "Parsing with derivatives,"
  *ICFP* 2011. The footing that a regex and a CFG are the *same object* under derivation — a
  char-class derivative *is* the `ByteClass` scan. This is why 5D needs no functor: one language, not
  two related ones.
- **One-pass DFA class** — burntsushi, "regex internals" (https://burntsushi.net/regex-internals/);
  `regex-automata` `dfa::onepass`. The exact set of regexes a single left-to-right scan matches
  without backtracking; the criterion is FIRST/FOLLOW disjointness (Aho–Sethi–Ullman). Realized
  in-tree at `info/one_pass.rs:38`.
- **Separate-lexer null hypothesis** — `re2c`, `logos`, `flex`, tree-sitter keep a *separate*
  generated lexer DFA. 5D wins only if the measured one-pass fraction is high enough that the separate
  automaton is dead weight on the generator path; where it is not, name the residual DFA surface.
- **PROVE-10** (`prototypes/PROVE-10/REPORT.md`) — prefilter tier over an untouched DFA, 9.6–29×;
  evidences the common case is prefilterable-and-one-pass, NOT that combinator lowering wins (see the
  honest limit above).
- **shufti/NEON class kernel** — Langdale (Hyperscan), R7 §3; `crates/simd-scan/src/neon.rs`
  `classify_chunk_nibble` — the residual for a `ByteClass` scan.

## Named risks + counterexamples

- **R1 ambiguous/backtracking leaf** — `(a|ab)c`, `\d+\.\d+|\d+`, catastrophic `(a+)+b`. Naive
  `Seq/Alt/Repeat` lowering backtracks or is exponential. *Mitigation:* `one_pass_eligible` is false →
  route to DFA lowering. The load-bearing number is the measured one-pass fraction of real leaves.
- **R2 greedy give-back** — `[a-z]+z` on `"abz"`: greedy `+` eats `z`, must back up one.
  *Counterexample the prototype must build.* *Mitigation:* FOLLOW-disjointness catches it exactly
  (`z ∈ [a-z]` ⇒ not one-pass ⇒ DFA); the common grammar shape (token followed by ws/delimiter) is
  disjoint and stays straight-line. Note: `check_one_pass_eligible` currently tests FIRST-set
  disjointness across *alternations* — verify it also rejects a `Repetition` whose class overlaps its
  own FOLLOW; if not, that is a named gap the desugar must close before trusting the flag.
- **R3 negated + unicode** — `[^"\\]*` is a clean `ByteClass{negated}` scan (the SIMD quote path);
  `\p{L}` expands via `utf8.rs` to UTF-8 codepoint-range automata — rare in leaves, routed to DFA.
  Named boundary.
- **R4 standalone linear-time contract** — a too-thin front loses the no-backtracking guarantee.
  *Mitigation:* the public API routes every non-one-pass pattern to the DFA; thin in code, not in the
  cost-model decision.
- **R5 leftmost-longest / anchoring** — the DFA gives leftmost-longest by construction; combinator
  `Repeat` is greedy but `Seq`-composed. The boundary node for `^ $ \b` (from `Hir::Look`) must
  preserve zero-width anchor semantics. *Mitigation:* differential test against the DFA on the engine's
  test corpus.
- **R6 the desugar is not faster, only cleaner** — if the head-to-head shows the straight-line scan
  merely ties the prefiltered DFA, 5D still wins on the *architecture* axis (four parsers → one, no
  functor, no automaton on the hot path) but must NOT claim a perf win. Report the tie as a tie.

## What a prototype must test to move 5D

1. Implement `ExprKind::ByteClass` + `regex_desugar` (`Hir → ExprKind` translation) over the three
   real `json.bbnf` leaves + the Sheets ident; assert **zero `ExprKind::Regex` survive** post-pass.
2. **Run `check_one_pass_eligible`** on all four real leaves and the 50+-pattern engine test corpus
   (`parse-that/rust/regex/tests/`) and **report the one-pass fraction** — the single number that
   decides whether "no engine on the generator path" holds. Also verify R2's FOLLOW-overlap gap.
3. Prove the EXISTING grammar `nullability` + first-set walks over the desugared subtree return
   **byte-identical** results to `analyze`/`regex_first`/`regex_first_bytes` — the deletion
   justification (four parsers → zero).
4. Emit the desugared JSON string/number/ws via the grammar's own emit; assert **match parity** vs
   `RegexProgram` on the JSON corpus AND run the head-to-head: **cold paired throughput of the
   straight-line class scan vs the PROVE-10 prefiltered `Dfa::find`** (N≥200, paired same-binary lanes,
   the `count_a == count_b` gate). A tie is a tie; only a measured win is a perf claim.
5. Show the standalone thin-front routes `(a+)+b` to the DFA (linear time preserved) and
   `#[0-9a-fA-F]{3,6}` to the straight-line combinator via the same `one_pass_eligible` read — **one
   decision point**, with the engine's test suite green either way.
