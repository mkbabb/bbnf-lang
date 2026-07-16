# LOCK-5 pass-1 · family 5D — GRAMMAR-FRAGMENT

## Thesis (strongest form)

A regex literal in a grammar is not a foreign engine embedded in a rule — it *is* grammar,
written in a terser notation. Concatenation is `Seq`, `|` is `Alt`, `*`/`+`/`?`/`{m,n}` are
`Repeat`/`Optional`, a literal char is a one-byte `Literal`; the ONLY construct a regex adds
that the grammar IR lacks is the character-class atom. So 5D dissolves the regex leaf at the
front: a single desugar pass parses each `/pat/` ONCE into native `ExprKind` nodes of the same
`GrammarIr` arena, and from that point there is **no separate regex engine on the generator
path** — nullability, first/follow, typing, lowering, and emission all run through the machinery
the grammar already owns. The standalone `parse-that` regex API survives as a thin front: it
parses a pattern to a one-rule `GrammarIr` and rides the same lowering; the classical
NFA→DFA path is retained only as ONE *optional lowering target* the scan-plan cost model selects
for the patterns that provably need it (adversarial standalone input, ambiguous leaves), never
as the default generator path.

This is the deepest reading of Constitution clause (i) (grammar totality) and clause (ii)
(isomorphism): there is no functor to maintain between two HIRs (contrast 5B) because there is
one HIR — the grammar's — and no automaton on the hot path to keep in sync (contrast 5A/5C).

## The disease this cures (measured, in-tree)

The regex pattern travels the skinny pipeline as an **opaque `String`** and is re-parsed by
**four independent, mutually-redundant, overfit hand parsers**, each degrading to empty on
shapes it cannot read:

1. `skinny/crates/bbnf-regex/src/lib.rs:45` `analyze(pattern)` — but its `classify` (`:92`) is
   DEGENERATE: it string-equals `"[ \t\n\r]*"`, tests `starts_with('"')`, and matches one
   numeric prefix; everything else is `RegexKind::Unknown`. A three-pattern lookup table, not a
   parser. Consumed by `ir/src/lib.rs:331` for nullability and `passes/src/lib.rs:944`.
2. `codegen/src/general_descent/regex_first.rs` (231 LOC) — a *second*, separate leading-token
   parser (`first_bytes`, `is_nullable`) whose own doc admits it "degrades to an empty set on
   anything it cannot read" and re-derives nullability the classifier already computed.
3. `codegen/src/json_sink_direct/first_set.rs:97` `regex_first_bytes` — a *third* first-byte
   deriver on the sink path.
4. `passes/src/lib.rs:943` `regex_first_bytes`, `:224` `regex_type`, `:1664` `span_kind` — a
   *fourth* set of string re-scans for the type plane and span selection.

The leaf is then carried opaque all the way to `BackendExpr::RegexProgram { pattern, span_kind }`
(`ir/src/lib.rs:389`; lowered at `passes/src/lib.rs:1341`; emitted at
`codegen/src/general_descent/emit.rs:365`, `codegen/src/lib.rs:1444`,
`json_sink_direct/skeleton.rs:143`). The real grammar carries exactly three regex leaves
(`skinny/grammars/json.bbnf:4-6`: number, string, ws) — precisely the three the classifier
hard-codes. A fourth leaf (the Sheets ident `\$?[A-Za-z]{1,3}\$?\d+`) already falls through to
`Unknown`/empty. This is not an engine; it is overfit pattern recognition, and it is 5D's whole
target.

## The mechanism, made concrete

**One new IR leaf.** `ExprKind` (`ir/src/lib.rs:234`) today has `Seq`, `Alt`, `Repeat`,
`Optional`, `Literal { bytes }` — every regex operator except the class atom. Add exactly one:

```rust
ByteClass { set: ByteSet256, negated: bool }   // ByteSet256 already exists: bbnf-regex/src/lib.rs:35
```

`ByteSet256` (four `u64` words, `insert`/`insert_range`/`contains`/`to_vec`) is already in-tree
and is the exact class-membership primitive. Codepoint classes beyond ASCII lower to a small
`Alt`/`Seq` of `ByteClass` over UTF-8 byte ranges (the automaton's job, done once at desugar).

**One desugar pass**, `regex_desugar`, a `GrammarIr → GrammarIr` pass sited in `passes/`
(alongside `resolve_refs`/`nullability`), running before every downstream analysis. For each
`ExprKind::Regex { pattern }`: parse the pattern with a *real* recursive-descent regex parser
(the `bbnf-regex` HIR parser on the parse-that side — `hir/parser/mod.rs:153` `parse`, the
6742-LOC engine's front, vendored per GN-1), then translate its HIR into `ExprKind` children
allocated via `GrammarIr::add_expr`, and replace the `Regex` node with the subtree root:

| regex HIR | → `ExprKind` |
|---|---|
| concatenation | `Seq(children)` |
| alternation | `Alt { mode }` (`Dispatch` when branch first-sets are disjoint, else `Speculative`) |
| `*`/`+`/`{m,n}` | `Repeat { min, max }` |
| `?` | `Optional` |
| literal byte / string | `Literal { bytes }` |
| char class / `\d \w \s` / `[^…]` | **`ByteClass { set, negated }`** |
| anchors `^ $ \b` | boundary predicate (a zero-width `ByteClass`-guard node, or lowered via lookahead) |

**Post-pass invariant:** zero `ExprKind::Regex` survive (the emitter's catch-alls panic on
residue — the no-silent-epsilon law). This DELETES, wholesale: `bbnf-regex`'s `classify`
3-pattern table, `general_descent/regex_first.rs` (all 231 LOC), `json_sink_direct/first_set.rs`'s
`regex_first_bytes`, `passes`' `regex_first_bytes`/`regex_type`/`span_kind`, and
`BackendExpr::RegexProgram` itself. Nullability (`ir/src/lib.rs:320`) loses its
`bbnf_regex::analyze` import (`:1`) — the `Regex` arm vanishes and `ByteClass` is a trivial
non-nullable leaf. First-set (`passes/src/lib.rs:880+`) loses its `Regex` arm; `ByteClass`
contributes its `set` directly. The four ad-hoc parsers collapse to zero because the ONE
real parse now feeds the ONE set of grammar walks.

`span_kind`/`RegexKind::{QuotedString,Numeric,Whitespace}` (the SIMD-scanner selectors, used at
`json_sink_direct/dispatch.rs:165`, `lower/tape_plan.rs:117`) are recovered *structurally* from
the desugared shape — a `ByteClass{negated:true}*` bracketed by a `"`-delimiter `Literal` IS the
quoted-string recognizer; a digit-`ByteClass`-led `Seq` IS the numeric recognizer — so the
SIMD-scan recognizer (`Recognizer::SimdScan`, `ir/src/lib.rs:417`; the bbnf-simd shufti/NEON
kernels) is chosen by an IR analysis over `ByteClass` shapes, not a pattern-string equality test.
This is the seam that keeps the PROVE-04/PROVE-01 structural-scan fast paths while erasing the
string oracle.

## Why no DFA on the generator path (the determinism thesis)

The grammar regex leaves that actually occur are, nearly without exception, **one-pass** regexes
(regex-automata's `one_pass` class; `bbnf-regex/.../info/one_pass.rs`): a greedy char-class
`*`/`+` is unambiguous exactly when its follow-set is disjoint from its own class — which is the
identical first/follow disjointness the grammar's `Alt`-dispatch already computes
(`passes/src/lib.rs` first-set machinery). So the determinism of a regex fragment is a **grammar
analysis**, not a subset construction. A one-pass leaf emits as a straight-line char-class scan
loop — the same inner loop as PROVE-10's `Dfa::find` but with *no transition table*, because a
single `ByteClass` needs no states: `while set.contains(input[pos]) { pos += 1 }`, residualized
to branch-free NEON via the shufti/`vqtbl1q_u8` class kernel (R7 §3, PROVE-01). This is *faster*
than a DFA (no table indirection, one class-membership test), and it is precisely PROVE-10's
result read the other way: the four certified prefilterable patterns (`0x[0-9a-fA-F]+`,
`#[0-9a-fA-F]{3,6}`, `https?://…`, `(?:foo|bar)[a-z]*`) are ALL one-pass — the prefilter win is
evidence that the common case never needs the DFA's full power.

The DFA is not deleted; it is re-homed as one lowering target of the unified IR, chosen by the
scan-plan cost model (LOCK-2's engine) only for leaves that fail the one-pass check. On the
generator path that set is measured-near-empty; on the standalone API it is whatever the caller
throws.

## The standalone API as a thin front

`Regex::new(pat).find(text)` parses `pat` → a one-rule `GrammarIr` → the same `regex_desugar` →
the same lowering. The public contract (linear-time, no catastrophic backtracking) is preserved
because the *thin front routes patterns that fail the one-pass check to the DFA lowering* — the
front is thin in CODE (shares the IR, the passes, the emit spine) while the combinator-vs-DFA
choice is the same cost-model decision the generator uses. Thin does not mean lossy: `(a+)+` still
goes to the DFA and stays linear; `#[0-9a-fA-F]{3,6}` goes to the straight-line class scan. The
44-test `bbnf-regex` suite + the 49 dfa / 22 parser / 17 byteset bridge subtests (R3 §7) are the
byte-identical parity floor.

## Integration seams (path:line)

- New leaf: `ir/src/lib.rs:234` (`ExprKind`), reuse `ByteSet256` `bbnf-regex/src/lib.rs:35`.
- New pass: `passes/src/lib.rs` (sibling of `nullability`), consumes `bbnf-regex` HIR parser.
- Deletions: `bbnf-regex/src/lib.rs:92` `classify` + `:45` `analyze`;
  `codegen/src/general_descent/regex_first.rs` (whole file); `json_sink_direct/first_set.rs:97`;
  `passes/src/lib.rs:943/224/1664`; `BackendExpr::RegexProgram` `ir/src/lib.rs:389` and all its
  match arms (`passes:1341`, `emit.rs:365`, `lib.rs:1444`, `tape_plan.rs:117`, `sink_only.rs:206`).
- Nullability edit: `ir/src/lib.rs:1` (drop import), `:331` (drop `Regex` arm).
- Grammar front already parses `/pat/` to `ExprKind::Regex` at `grammar/src/lib.rs:1512`
  (`skip_regex` `:1166`) — unchanged; the desugar runs after it.
- SIMD recognizer selection: `Recognizer::SimdScan` `ir/src/lib.rs:417`; structural chooser
  replaces `span_kind` at `dispatch.rs:165`, `tape_plan.rs:117`.

## Prior art (cited)

- **Brzozowski derivatives / regex-as-grammar** — Owens, Reppy, Turon, "Regular-expression
  derivatives re-examined," *JFP* 19(2), 2009; Might, Darais, Spiewak, "Parsing with
  derivatives," *ICFP* 2011. The theoretical footing that a regex and a CFG are the *same object*
  under derivation — a char-class derivative *is* the `ByteClass` scan. This is why 5D needs no
  functor: there is one language, not two related ones.
- **One-pass DFA class** — burntsushi, "regex internals" (https://burntsushi.net/regex-internals/);
  `regex-automata` `dfa::onepass`. The exact set of regexes a single left-to-right scan matches
  without backtracking; the criterion is first/follow disjointness (Aho-Sethi-Ullman first/follow).
- **PROVE-10** (`prototypes/PROVE-10/REPORT.md`) — start-state class scan, 9.6–29× over
  byte-by-byte; its prefilterable patterns are all one-pass, evidencing the common case.
- **shufti/NEON class kernel** — Langdale (Hyperscan), R7 §3; `crates/simd-scan/src/neon.rs:208`
  `classify_chunk_nibble` — the residual for a `ByteClass` scan.
- **The null hypothesis to defeat** — `re2c`/`logos`/tree-sitter keep a *separate* generated
  lexer DFA. 5D wins only if the measured one-pass fraction is high enough that the separate
  automaton is dead weight on the generator path; where it is not, name the residual DFA surface.

## Named risks + counterexamples

- **R1 ambiguous/backtracking leaf** — `(a|ab)c`, `\d+\.\d+|\d+`, catastrophic `(a+)+b`. Naive
  `Seq/Alt/Repeat` lowering backtracks or is exponential. *Mitigation:* the desugar runs the
  one-pass/first-follow check; failing leaves route to the DFA lowering. The load-bearing number
  is the measured one-pass fraction of real leaves.
- **R2 greedy give-back** — `[a-z]+z` on `"abz"`: greedy `+` eats `z`, must backtrack one.
  *Counterexample the prototype must build.* *Mitigation:* follow-set disjointness catches it
  exactly (`z ∈ [a-z]` ⇒ not one-pass ⇒ DFA); the common grammar shape (token followed by
  whitespace/delimiter) is disjoint and stays straight-line.
- **R3 negated + unicode** — `[^"\\]*` is a clean `ByteClass{negated}` scan (the SIMD quote path);
  `\p{L}` expands to UTF-8 codepoint-range automata — rare in leaves, routed to DFA. Named boundary.
- **R4 standalone linear-time contract** — a too-thin front loses the no-backtracking guarantee.
  *Mitigation:* the public API routes every non-one-pass pattern to the DFA; the front is thin in
  code, not in the cost-model decision.
- **R5 leftmost-longest / anchoring semantics** — DFA gives leftmost-longest by construction;
  combinator `Repeat` is greedy but Seq-composed. The boundary node for `^ $ \b` must preserve
  zero-width anchor semantics. *Mitigation:* differential test against the DFA on the bridge corpus.

## What a prototype must test to move 5D

1. Implement `ExprKind::ByteClass` + `regex_desugar` over the three real `json.bbnf` leaves + the
   Sheets ident; assert **zero `ExprKind::Regex` survive** post-pass.
2. Prove the EXISTING grammar `nullability` + first-set walks over the desugared subtree return
   **byte-identical** results to `analyze`/`regex_first`/`regex_first_bytes` — the deletion
   justification (four parsers → zero).
3. Emit the desugared JSON string/number/ws via the grammar's own emit; assert **match parity**
   vs `RegexProgram` on the JSON corpus AND **cold paired throughput ≥** the RegexProgram path
   (the determinism thesis: no table, straight-line class scan; N≥200, paired same-binary lanes,
   the PROVE-10 `count_a == count_b` gate).
4. Build the one-pass classifier and **measure the fraction** of the 50+-pattern `bbnf-regex`
   test corpus (R3 §6) that is one-pass (combinator-lowerable) vs needs-DFA. This single number
   decides whether "no engine on the generator path" holds; a low fraction is a named negative
   result, not a hidden failure.
5. Show the standalone thin-front routes `(a+)+` to the DFA (linear time preserved) and
   `#[0-9a-fA-F]{3,6}` to the straight-line combinator — **one decision point**, measured, with
   the 44+49 bridge tests green either way.
