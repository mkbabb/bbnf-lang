# LOCK-5 pass-3 — family 5D (GRAMMAR-FRAGMENT) research

## The family's claim, made concrete

5D lowers a grammar's regex literal DIRECTLY into the grammar's own combinator IR —
`ByteClass`/`Repeat`/`Seq`/`Optional`/`Project` nodes the general-descent emitter already
recognizes — so **no automata engine sits on the generator path at all**. The full
parse-that HIR→NFA→DFA engine (`parse-that/rust/regex`, 6742 LOC, READ-ONLY, R3 §3) survives
as the backing of the STANDALONE public regex API only; the grammar-embedded regex subset
never touches it. The decisive 5D fact is that this is ALREADY the status quo on the
generator path — but the current realization is (a) unsound against grammar FOLLOW and (b)
built on a duplicated first-byte deriver and a masked-wildcard continuation. 5D's work is to
make the direct lowering *sound* and *single-sourced*, with a byte-class recognizer that
recognizes the actual class instead of a fixed text mask.

## The substrate as it stands (there is no DFA on the generator path today)

- Grammar-tier leaf: `ExprKind::Regex { pattern: String }` — `ir/src/lib.rs:250` (opaque,
  per the XL-C settled ground); pretty at `ir:171`; nullability via `analyze()` at `ir:331`.
- Backend node: `BackendExpr::RegexProgram { pattern: String, span_kind: SpanKind }` — def
  `passes/src/lib.rs:389`, lowered from `ExprKind::Regex` at `passes:1341`; `span_kind` chosen
  by `backend_span_kind` (`passes:1660`) from the coarse classifier. `SpanKind ∈
  {String, Number, Whitespace}` — `ir:440`. **`RegexProgram` carries only a `String` and a
  3-valued `SpanKind` — no byte-class, no automaton.**
- Emission: `codegen/src/general_descent/emit.rs:365` maps `RegexProgram` → a `match_leaf(...)`
  call (`emit.rs:158-224`). `match_leaf` gates the head byte on the leaf's first-set, then does
  a GREEDY delimiter-scan: a balanced quoted span (`consume_quoted`), else the maximal
  `is_text_byte` run (`emit.rs:227`, a FIXED `[A-Za-z0-9_.$]` mask), else the maximal first-set
  run; then `tape.push_offset(start, OffsetFlags(kind))` (`emit.rs:220`). Its own comment names
  it "the grammar-sanctioned Span delimiter-scan (typed leaves deferred)." No regex engine is
  invoked; there is no `Dfa::find`, no prefilter, no `BackendExpr::ByteClass` anywhere in
  `skinny/crates/codegen` or `passes` (grep-verified).

- **Two duplicated first-byte derivers — the primary 5D consolidation target.**
  1. `bbnf_regex::analyze` (`skinny/crates/bbnf-regex/src/lib.rs:45`, 322 LOC total) →
     `RegexFacts { nullable, first: FirstSet, byte_classes: Vec<ByteClass>, hir:
     RegexHir{kind}, string }`. `RegexKind ∈ {Whitespace, QuotedString, Numeric, Unknown}`
     (coarse `classify` at `bbnf-regex:92`); `ByteSet256` with `insert/insert_range/contains/
     to_vec` (`bbnf-regex:66`). Consumed by `ir:1`, `passes:1` — first-set at `passes:943`
     (`regex_first_bytes`), types at `passes:194`/`:224`, nullability at `ir:331`.
  2. `regex_first::{first_bytes, is_nullable}` (`codegen/src/general_descent/regex_first.rs`,
     230 LOC) — a SEPARATE, strictly more general leading-token parser (handles `(?:...)`,
     negated classes → empty → fallthrough, `\s`/`\d`/`\w`, top-level alternation split).
     Consumed only by `emit.rs:366,370`. Its private `is_single_atom`/`closing_class_index`/
     `class_byte`/`insert_range` (`regex_first:38-230`) DUPLICATE bbnf-regex's byte-identical
     private fns (`bbnf-regex:186-234`).

- The vendored `skinny/crates/parse-that-regex` is NOT the automata engine: its tree is
  `number/eisel_lemire`, `unicode`, `integration/simd_scan_hook` only (verified — no
  `automata/`, no `hir/`, no `Dfa`). The real matcher lives solely in `parse-that/rust/regex`
  (READ-ONLY). So "the vendored copy is the measurement substrate" (settled ground) refers to
  the base-pin engine there; the grammar-tier consumer in skinny is the 322-LOC coarse
  classifier, and the generator path has no matcher at all.

## Q7 — the FOLLOW-sound one-pass fraction

The `match_leaf` greedy run IS one-pass but UNSOUND: it consumes the maximal continuation run
with no reference to the leaf's own class OR to the grammar symbol that follows the leaf. A
regex leaf of shape `C*`/`C+` may lawfully lower to a straight-line greedy byte-run **iff
`first(C) ∩ FOLLOW(leaf) = ∅`** — otherwise the greedy scan steals bytes that belong to the
next grammar symbol (the classical longest-match-vs-FOLLOW / maximal-munch hazard; dragon-book
lexing, re2c longest-match, the "lexer hack"). This is the grammar analog of regex-automata's
*one-pass* class (R7 §4): patterns that need no backtracking because each step is
FOLLOW-determined.

- The predicate does not exist today. `first(C)` is available (`analyze().byte_classes` →
  `ByteSet256`; or `passes:911` `first_bytes` for grammar symbols). FOLLOW is its dual and is
  NOT yet computed — it must be built (fixed-point over the grammar, the LL/LR FOLLOW closure,
  a Changed-flag convergence not a structural hash), then the disjointness test run per
  `RegexProgram` leaf at its actual grammar position.
- A prototype must measure the disjoint FRACTION over the engine corpus under BOTH forms the
  directive names: (a) straight-line greedy lowering (today's `match_leaf`), and (b)
  `Seq[Repeat(C), suffix]` where the `Repeat` continuation is bounded by grammar FOLLOW (a
  Repeat-with-FOLLOW-guard) — and name the specific non-disjoint leaves that REQUIRE form (b).
  Today's JSON/Sheets corpora pass only because their leaves are delimiter-terminated (`,`,
  `)`, `!`, whitespace) that happen to sit outside the text mask — an accident of corpus, not a
  soundness proof. The measured fraction is what tells 5D whether straight-line lowering
  suffices or the FOLLOW-guarded Repeat is mandatory as the default.

## Q8 — the ByteClass recognizer prototype

The gap is exact: `match_leaf`'s continuation is the MASKED WILDCARD `is_text_byte`
(`emit.rs:227`), a fixed `[A-Za-z0-9_.$]`, so a `[0-9]+` leaf today over-consumes letters — it
does NOT recognize its own class. A prototype must:

1. Add `BackendExpr::ByteClass { set: ByteSet256, min: u32, max: Option<u32> }`, lowered from
   `ExprKind::Regex` whenever `analyze()` yields a single-class quantified atom (bbnf-regex
   already returns `byte_classes: Vec<ByteClass>` + nullability + the min/max shape). This is
   a REAL `ByteClassRecognizer`: `while set.contains(byte) && count < max { p += 1 }`, bounded
   by `min` — firing on the ACTUAL class, not the wildcard, and not the masked-wildcard
   fallthrough of the current path.
2. Reach the tape: after the class run, `tape.push_offset(start, OffsetFlags(kind))` — the
   `match_leaf` tail (`emit.rs:220`) is reused verbatim; only the continuation predicate
   changes.
3. Cold-paired: the new direct class-scan vs the PROVE-10 prefiltered `Dfa::find`
   (`parse-that/rust/regex`, `patch.diff` in `prototypes/PROVE-10/`). This pairs the 5D world
   (direct grammar-IR class-scan, zero engine) against the 5C world (full engine + start-state
   memchr prefilter, PROVE-10 certified 9.6–29× on skip-heavy). Expectation to earn: on short
   in-tier classes the direct scan matches/beats (no DFA setup, no prefilter branch); on
   skip-heavy inputs the prefilter's memchr skip wins — the honest crossover 5D must expose,
   not assume. Evidence = cold pairs (N≥200, paired same-binary, per SPEC §0 law) on a GROWN
   corpus + the emitted tape output.
4. Capture-leaf → `Project` byte-identity: `/(...)/ -> name` must lower to `ByteClass … ;
   ValueProject { target: name }` (`ExprKind::Project` `ir:250` → `BackendExpr::ValueProject`
   `passes:1350`) and produce a tape BYTE-IDENTICAL to the hand-authored grammar capture
   `name = [class]+ -> name`. The `->` inner representation is Q11/LOCK-1's ratification; 5D's
   projection soundness rides on it.

## Q9–Q11 — the grammar-driven path (5D's outer ring), SPEC-ONLY, gated LOCK-7/7A

- **Q9.** `grammar/misc/regex.bbnf` (27 lines) IS the bootstrapped regex grammar — a full EBNF
  regex definition (`expression = (term+, "|"?)+`, `char_class`, `range`, `escaped`, `anchor`,
  `back_reference`, `lookahead/lookbehind`), itself written with regex leaves (`/d+/`,
  `/[^\\...]/`, `>>`, `-`) — self-referential. It is NOT threaded: no `parse_generated()`
  returns its AST; the engine parses the pattern string with the hand-written RD parser
  (`parse-that hir/parser`, R3 §3). 5D's grammar-driven path = promote `regex.bbnf`, make the
  generated parser return the regex AST (break the `-> bool` discard), and DE-DUP the two
  first-byte derivers into ONE analysis over that AST. A prototype threads the AST through and
  reports the grammar-totality picture; SPEC-ONLY until LOCK-7/7A.
- **Q10.** Grammar-tier regex e-graph rewrite rules FIRE ZERO times today — `ExprKind::Regex`
  is opaque, so no grammar-tier rule matches it. The 1253-LOC e-graph rules (flatten/union/
  superset/redundant/repetition) live ENGINE-side (`parse-that/rust/regex/src/egraph`, R3 §3)
  over the engine `Hir`, not over grammar IR. IF 5D lowers regex into grammar-IR nodes, the
  grammar's own e-graph (LOCK-2) rewrite rules (Seq-flatten, Alt-dedup, `first`-canon) become
  applicable — the census must count fires POST-lowering, both bare and projection-wrapped, per
  the directive.
- **Q11.** `->` is `ExprKind::Project { body, target, ty }` (`ir:250`) → `BackendExpr::
  ValueProject { body, target, ty }` (`passes:1350`). Whether the inner class is a `Map`
  barrier or a typed edge is LOCK-1's call and is BINDING on the 5D bridge: a capture-class
  `/(...)/` → `ByteClass` + `ValueProject` is only byte-identical to `[class]+ -> name` if the
  projection is a transparent typed edge over the class span, not an opaque Map that re-tokenizes.

## Prior art (citations)

- Maximal-munch / longest-match FOLLOW hazard: dragon-book lexing; re2c longest-match &
  tunneling; the "lexer hack." The Q7 disjointness predicate is exactly this soundness test,
  lifted to grammar FOLLOW.
- rust `regex-automata` one-pass DFA (burntsushi, regex-internals; R7 §4): one-pass = no
  backtracking because each transition is uniquely determined — the automaton analog of the
  FOLLOW-determined grammar leaf.
- tree-sitter external scanners + LR-bounded token boundaries: the "grammar drives the scan
  boundary" model 5D wants — the parser's FOLLOW set, not a greedy scan, bounds the class run.
- PEG/packrat longest-match vs ordered choice: `match_leaf` is a PEG longest-match; soundness
  under composition needs the FOLLOW guard (Ford, packrat).
- Brzozowski/Antimirov derivatives (egg/egglog rewrite): lowering a regex to grammar
  productions is derivative unrolling; the LOCK-2 e-graph is the rewrite substrate for Q10.

## Named risks & counterexamples

- **Greedy over-consumption steals a FOLLOW byte (Q7).** Counterexample a prototype must build:
  a leaf `[a-z]+` immediately followed by a keyword whose first byte is in `[a-z]` (e.g.
  `word = [a-z]+ ; sentence = word "and" word`) — the greedy run swallows the `and`; the
  `Seq[Repeat, FOLLOW-guard]` form fixes it. Measure how many corpus leaves are of this shape.
- **The masked wildcard mis-scans adjacent distinct classes (Q8).** `[0-9]+` today consumes
  trailing letters via `is_text_byte`; any grammar with `[0-9]+[A-Za-z]+` adjacency is
  mis-parsed unless a delimiter intervenes. The current path "works" ONLY on delimiter-heavy
  corpora — a real `ByteClass` recognizer is mandatory, not an optimization.
- **Deriver drift.** `regex_first` (handles `(?:...)`, `\s`, negated-class fallthrough, alt
  split) is a strict superset of the emit-time needs; `bbnf-regex analyze` adds Numeric/
  QuotedString `RegexKind` classification. Consolidation must be a byte-identical UNION over the
  corpus first sets, else a silent parse divergence. The 5D LOC test: deleting `regex_first.rs`
  (−230) and routing `emit.rs` through `analyze` must SHRINK, not merely relocate.
- **Standalone-API expressivity.** Routing parse-that's public regex API as a "thin front" over
  grammar-IR lowering would LOSE backrefs/lookaround (`regex.bbnf` declares them but grammar IR
  cannot express them). 5D keeps the FULL engine for the standalone API and routes only the
  class-restricted GRAMMAR-embedded subset (`ByteClass`/`Repeat`/`Seq`/`Alt`/`Optional`) through
  direct lowering — the boundary is the expressivity of `ExprKind`, and it must be stated.

## What a prototype must test to move 5D

1. The FOLLOW-disjoint fraction over the engine corpus, straight-line vs `Seq[Repeat, FOLLOW]`,
   with the non-disjoint leaves named (Q7) — requires building the grammar FOLLOW closure.
2. `BackendExpr::ByteClass { set, min, max }` from `analyze().byte_classes`, emitted as a real
   class-run recognizer replacing the `is_text_byte` mask, reaching `push_offset`; cold-paired
   vs PROVE-10 `Dfa::find` on a grown (≥50, tagged) corpus (Q8).
3. Capture-leaf `/(...)/ -> name` → `ByteClass + ValueProject` byte-identical tape vs
   hand-authored `name = [class]+ -> name` (Q8/Q11).
4. Single-source: delete `regex_first.rs` (−230 LOC), route `emit.rs` through `bbnf-regex
   analyze`, prove byte-identical first sets over the corpus — report the LOC delta (SHRINK?).
5. Thread `regex.bbnf`: generated parser returns the regex AST (break the discard), de-dup
   `grammar/misc/regex.bbnf` — the grammar-totality picture (Q9, SPEC-ONLY, LOCK-7/7A gated).

## Integration seams (path:line)

- `ir/src/lib.rs:250` `ExprKind::Regex`; `:389` `BackendExpr::RegexProgram`; `:440` `SpanKind`;
  `:331` nullability via `analyze`.
- `passes/src/lib.rs:1341` `Regex→RegexProgram` lower; `:943` `regex_first_bytes` (via
  `analyze`); `:194`/`:224` `regex_type`; `:1660` `backend_span_kind`; `:911` grammar
  `first_bytes` (FOLLOW's dual — to build).
- `codegen/src/general_descent/emit.rs:365` `RegexProgram` emission → `match_leaf`; `:158`
  `match_leaf`; `:227` `is_text_byte` (the masked wildcard to replace); `:220` `push_offset`
  (the tape reach-point).
- `codegen/src/general_descent/regex_first.rs:81` `first_bytes`, `:18` `is_nullable` (the DUP to
  fold into `bbnf-regex analyze`).
- `skinny/crates/bbnf-regex/src/lib.rs:45` `analyze`, `:30` `ByteClass`, `:34` `ByteSet256`,
  `:92` `classify` — the single-source target.
- `grammar/misc/regex.bbnf` (27 lines) — the bootstrapped regex grammar (Q9).
- `parse-that/rust/regex` (READ-ONLY): `automata/dfa.rs`, PROVE-10 `patch.diff` (prefilter +
  `Dfa::find`), `hir/`, `egraph/` — the standalone-API thin-front's backing engine.
