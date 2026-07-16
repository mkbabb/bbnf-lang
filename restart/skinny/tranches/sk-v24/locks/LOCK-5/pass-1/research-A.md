# LOCK-5 pass-1 — Family 5A: ONE-HIR (regex HIR as a sub-language of bbnf HIR)

## The thesis, crystallized

Family 5A asserts that there is not a regex HIR and a bbnf HIR joined by a functor (that is
5B's world); there is **one node algebra**, and regex is the fragment of it that carries no
rule references. Regex operators *are* grammar operators — alternation is alternation,
concatenation is sequence, `*`/`+`/`?`/`{m,n}` are repetition, a literal char is a byte
literal. The only genuinely regex-specific leaves are a **character-class** node and a
**zero-width assertion** node. Add exactly those two to bbnf's expression enum and the regex
HIR *is* a sub-set of the bbnf HIR: a single Rust type, in a single crate. Regex text parses,
through the bootstrapped `grammar/misc/regex.bbnf`, straight into that enum; automata are
built by consuming that enum; "classification" stops being a string oracle and becomes a
structural analysis pass over the same tree every other bbnf analysis already walks.

This is the GHC-Core / MLIR move applied to the regex boundary: everything desugars into one
small core, and the "regex dialect" is just the core restricted to its class/assertion/repeat
fragment. It is the maximal reading of constitution clause (ii) — not *isomorphism to a
reasonable point* but *identity*: the reasonable point is "there is nothing to map."

## The current state is fragmentation across three crates (the evidence)

There is **no distinct "bbnf HIR" type** — a search for `enum Hir`/`struct Hir`/`mod hir`
across `skinny/crates/bbnf` and `skinny/crates/grammar` returns nothing. What the constitution
calls the bbnf HIR is `ir::ExprKind` (`skinny/crates/ir/src/lib.rs:234`) with its typed lowering
`ir::BackendExpr` (`:376`). Regex, inside that enum, is a single opaque string:

- `ir::ExprKind::Regex { pattern: String }` (`skinny/crates/ir/src/lib.rs:250`) — the leaf to
  dissolve. Every fact about the regex is recovered by *re-parsing this string* on demand.
- `ir::nullability` (`:320`) handles every node structurally **except** regex, which escapes
  to `analyze(pattern).nullable` (`:331`) — a string re-parse inside the fixpoint loop.
- The lowering tower carries the string onward: `ir::BackendExpr::RegexProgram { pattern:
  String, span_kind }` (`:389`), so the *string* reaches the backend and is re-parsed a third
  time for leading-byte derivation (`codegen/src/general_descent/regex_first.rs:1`, "leading-
  byte-set derivation for `RegexProgram` leaf patterns").

The string-keyed dispatch is spread over three crates and at least five helpers, each a
private re-parse of the same pattern text:

- `passes/src/lib.rs:224 fn regex_type(pattern:&str)`, `:943 fn regex_first_bytes(pattern:&str)`,
  `:1664 fn span_kind(pattern:&str)`, plus the twelve `ExprKind::Regex { pattern }` match arms
  at `:194,348,790,814,836,911,1108,1341,1512,1562,1605,1638`.
- `codegen/src/json_sink_direct/dispatch.rs:87` walks the lowered tree calling
  `span_kind(&callee_rule.expr)` (`:163`) to find a `RegexProgram` leaf and read a tag.
- The classification *engine* underneath is the degenerate stub `bbnf-regex`
  (`skinny/crates/bbnf-regex/src/lib.rs`): `RegexKind ∈ {Whitespace, QuotedString, Numeric,
  Unknown}` (`:19`), assigned by literal pattern-string comparison — `pattern == r"[ \t\n\r]*"`,
  `pattern.starts_with('"')`, `pattern.starts_with("-?(0|[1-9]")` (`classify`, `:98`). `RegexHir`
  is `{ kind: RegexKind }` (`:12`) — a HIR in name only. This is a name-gated overfit to the
  three JSON leaves and violates constitution clauses (i), (vi); ONE-HIR **deletes it outright**,
  it is not ported.

Meanwhile the *real* structured regex HIR lives in a separate crate with a separate type:
the vendored `skinny/crates/parse-that-regex/` (origin `parse-that/rust/regex`, `bbnf-regex`
6742 LOC, R3 §3) whose `Hir` enum (`hir/mod.rs:216`), `CharClass`/`ByteRange`/`CodepointRange`
(`:45/:12/:27`), `Look` (`:172`), `Repetition` (`:194`) is exactly the node set ONE-HIR folds
into `ir::ExprKind`. Its `automata/{nfa,dfa}.rs` (Thompson NFA → subset construction → Hopcroft)
and its 1148-LOC `classify.rs` are the machinery to re-home. Two HIRs, two homes, is the defect
5A abolishes.

## The mechanism made concrete

### 1. The node algebra — what `ir::ExprKind` gains

The intersection (regex ↔ grammar) already exists in `ExprKind`: `Seq` (`:235`) = regex concat;
`Alt` (`:236`) = regex `|`; `Repeat { min, max }` (`:240`) = `*`/`+`/`{m,n}`; `Optional` (`:245`)
= `?`; `Literal { bytes }` (`:246`) = a literal run. Two leaves are added — the entire cost of
ONE-HIR:

```
ExprKind::Class  { set: ByteClassSet, negated: bool }   // regex [...], ., \d, \w  — the ONE new consuming leaf
ExprKind::Look   { kind: LookKind }                     // ^ $ \b \B (?=) (?<=)     — the ONE new zero-width leaf
```

`ByteClassSet` is the existing `ByteSet256` (`bbnf-regex/src/lib.rs:31`, `[u64;4]`), promoted
into `ir`. `Class` subsumes single chars too, so a regex literal char can lower to `Class` or
`Literal`; canonicalization (below) picks one. This makes the *regex sub-language* precisely
`{Seq, Alt, Repeat, Optional, Literal, Class, Look}` — the bbnf HIR minus `Ref`, `Project`,
`Annotation`, `Discard`. Regex is "bbnf HIR with no rule references and no projection." That
sentence is the family's whole claim and it is checkable.

### 2. Regex text → `ExprKind` via the bootstrapped grammar

**Current reality (verified, and it strengthens the charter):** there is no `regex.bbnf` —
the only skinny grammar file is `skinny/grammars/json.bbnf`. A regex literal is *not*
structurally parsed on the skinny side at all: `grammar/src/lib.rs:1561 fn parse_regex`
returns the raw pattern as an opaque `String`, and `:1166 fn skip_regex` merely scans to the
closing delimiter. So the charter clause "regex parsing rides the bootstrapped grammar" is
today **wholly unmet** — regex is a hand-scanned string, re-parsed downstream by the
`analyze()` stub. This is a to-BUILD deliverable, not an extant asset.

Under ONE-HIR a new bootstrapped `grammar/regex.bbnf` defines regex syntax in bbnf
(expression/term/factor/sub_expression/char_class/range/anchor/lookahead/lookbehind) and its
`->` projections target `ExprKind` constructors directly (the way json.bbnf leaves project to
values): `expression -> Alt`, `term -> Seq`, `factor -> Repeat/Optional`, `char_class ->
Class`, `range -> ByteClassSet::insert_range`, `anchor/lookahead -> Look`. The hand-rolled
`parse_regex`/`skip_regex` (`grammar/src/lib.rs:1561/1166`) and the vendored recursive-descent
`parse-that-regex/src/hir/parser/` both retire — constitution clause (i) satisfied by
construction. **This makes 5A a hard dependent of LOCK-7 (7A staged bootstrap)** — see risk 4.

### 3. Classification becomes an IR analysis pass

`ir::nullability` (`:320`) already fixpoints over `ExprKind`; adding total arms `Class => false`
(non-empty) / `Look => true` (zero-width) removes the `analyze(pattern)` escape hatch at `:331`.
First-set, `regex_first_bytes` (`passes:943`), and `span_kind` (`passes:1664`) collapse into
one structural first-byte / shape analysis over the unified tree — the 1148-LOC `classify.rs`
re-expressed as passes over `ExprKind`, deriving the `{Whitespace, QuotedString, Numeric}`
predicates *structurally* (is-this-a-`Class`-star, is-this-a-quote-delimited-Seq) rather than
by pattern-string equality. This is the "classify becomes an IR analysis pass" charter clause
made literal.

### 4. Automata construction consumes bbnf IR

Thompson construction reads `ExprKind` nodes directly: `Class → byte transitions`,
`Seq → concat states`, `Alt → epsilon fork`, `Repeat → back-edge`, `Look → zero-width guard`.
No `Hir → Nfa` step because there is no separate `Hir`. The vendored `automata/nfa.rs:71
from_hir` becomes `from_expr(&GrammarIr, ExprId)`; `dfa.rs` is unchanged downstream (it consumes
the NFA). The backend leaf stops being `RegexProgram { pattern: String }` and becomes a
reference to a compiled automaton (or, for the prefilterable case, the PROVE-10 `Dfa::find`
seam — LOCK-5's continuum feeds PT-1.W5 / GN-2.W1 the *structured* leaf, not a string).

### 4b. The single home (resolving the primacy tension)

The constitution homes LOCK-5 operationally with parse-that (primacy), yet ONE-HIR says the
regex HIR *is* `ir::ExprKind` (bbnf-side, `skinny/crates/ir`). These reconcile without a
functor: the HIR **type** lives in `ir`; the **engine** (nfa/dfa/prefilter/egraph) stays homed
in parse-that, rewritten to *consume* `bbnf_ir::ExprKind`. This is physically feasible today —
parse-that already path-depends on bbnf-ir (R3 §1 patch table: `bbnf-ir ->
../../bbnf-lang/crates/ir`). parse-that/rust/regex deletes its own `hir::Hir`, does
`use bbnf_ir::{ExprKind, GrammarIr, ExprId}`, and `from_hir → from_expr`. The shared type is
`#![no-feature]` stable-clean (`ir/src/lib.rs` already is), so it crosses the
nightly-2026-04-11 / greenfield toolchain boundary. The standalone parse-that regex API
survives as a thin front (`parse(pattern)` builds a throwaway one-root `GrammarIr` and runs the
engine) — 5D's clause satisfied *by* 5A. Of the vendored `Hir`'s 8 variants, **5 map onto
existing `ExprKind` variants byte-for-byte** (Literal, Concat=Seq, Alternation=Alt,
Repetition=Repeat, Group→transparent), Empty is trivial, and only Class+Look are new: the
"single type" claim is a two-leaf extension, not a merge of two large enums.

### 5. One e-graph Language

The `egraph`/`egraph-derive` crates and the `derive(Language)` seam
(`passes/src/backend_egraph.rs`) already exist; the vendored regex engine ships its own
`egraph/` (1253 LOC, R3 §3). ONE-HIR yields **one** `Language` impl derived from `ExprKind`
(the derive-language-macro precedent), so regex canonicalization (union flattening, repetition
normalization, redundant-class removal — the vendored `egraph/rules/`) and grammar
canonicalization share one saturation engine. This is the shared-part the constitution wants
without a functor.

## Substrate (extant code that serves, path:line)

| Role | Path:line |
|---|---|
| The enum to extend (bbnf HIR) | `skinny/crates/ir/src/lib.rs:234` `ExprKind`; `:250` the `Regex` leaf to dissolve |
| Backend tower to re-type | `skinny/crates/ir/src/lib.rs:376` `BackendExpr`; `:389` `RegexProgram { pattern }` |
| Structural analysis to extend | `skinny/crates/ir/src/lib.rs:320` `nullability` (`:331` the string escape to delete) |
| String-keyed helpers to collapse | `skinny/crates/passes/src/lib.rs:224/943/1664` (`regex_type`/`regex_first_bytes`/`span_kind`); 12 arms `:194…:1638` |
| Codegen string re-parse to delete | `skinny/crates/codegen/src/general_descent/regex_first.rs`; `.../json_sink_direct/dispatch.rs:87,163` |
| Degenerate classifier to DELETE | `skinny/crates/bbnf-regex/src/lib.rs:19` `RegexKind`, `:45` `analyze`, `:98` `classify` |
| Structured HIR + automata to re-home | `skinny/crates/parse-that-regex/` — `Hir`(`hir/mod.rs:216`), `CharClass/ByteRange/CodepointRange`(`:45/:12/:27`), `Look`(`:172`), `automata/nfa.rs:71 from_hir`, `automata/dfa.rs`, `classify.rs`(1148 LOC), `egraph/`(1253) |
| Hand-rolled string scanner to RETIRE (no grammar today) | `skinny/crates/grammar/src/lib.rs:1561` `parse_regex` (returns `String`), `:1166` `skip_regex` — the bootstrapped `grammar/regex.bbnf` is to-BUILD, not extant |
| `ByteSet256` to promote into `ir` | `skinny/crates/bbnf-regex/src/lib.rs:31` |
| Shared e-graph engine (bbnf-lang root, NOT skinny/crates) | `crates/egraph`, `crates/egraph-derive`; `skinny/crates/passes/src/backend_egraph.rs` |

## Prior art (with citations)

- **rust `regex-syntax` HIR** — `HirKind = Empty | Literal | Class(Unicode|Bytes) | Look |
  Repetition | Capture | Concat | Alternation`
  (https://docs.rs/regex-syntax/latest/regex_syntax/hir/enum.HirKind.html). This *is* the target
  node set: Concat=Seq, Alternation=Alt, Repetition=Repeat, Literal=Literal, Class=`Class`,
  Look=`Look`. The ONE-HIR claim reduces to "`ir::ExprKind` ⊇ `HirKind` \ {Capture} with a shared
  Class leaf." `regex-automata` builds the NFA straight off this Hir
  (https://docs.rs/regex-automata/latest/regex_automata/nfa/thompson/) — precedent that automata
  construction consumes the HIR with no intermediate.
- **tree-sitter** compiles lexical (regex-shaped) rules and syntactic rules from *one* grammar
  representation into one NFA/DFA lexer; regex `RegExp` rules and `seq/choice/repeat` grammar
  rules are the same DSL nodes (https://tree-sitter.github.io/tree-sitter/creating-parsers).
  Direct precedent for regex-in-grammar sharing a node algebra.
- **Ragel / re2c** — scanner generators whose regex language *is* the machine-construction
  language; char-class + concat/union/star over one AST → FA
  (https://www.colm.net/open-source/ragel/, https://re2c.org/). Precedent that the class leaf +
  the four combinators are a *complete* regex core.
- **GHC Core / MLIR** — one small core (~10 constructors) into which every surface construct
  desugars (https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-
  coercions/); MLIR "one IR, dialects as sub-languages sharing the op/type infrastructure"
  (https://mlir.llvm.org/docs/LangRef/). ONE-HIR is exactly "regex is a dialect of the bbnf core."
- **egg / egglog** — one `Language` trait, congruence closure over the shared node type
  (https://egraphs-good.github.io/). Justifies a single derived `Language` for regex+grammar
  canonicalization. R7 §3–4 (shufti/truffle/teddy; regex-automata lazy DFA + quit-byte fallback)
  supplies the automata-tier precedents the *consuming* families (5C) build on.

## Named risks and counterexamples

1. **The match-arm tax (the load-bearing risk).** Every match over `ExprKind` must grow arms
   for `Class` and `Look`. `passes/src/lib.rs` alone has ~12 regex arms today; type inference,
   cost, first-set, lowering, and codegen each match `ExprKind`. If the grammar-plane arms for
   `Class`/`Look` are *non-trivial* (not one-liners), ONE-HIR taxes every bbnf consumer to buy
   regex unification — a possible net complexity loss vs 5B's functor. *Counterexample to hunt:*
   a pass where `Class` needs genuinely regex-specific handling (e.g. the type plane must give
   `Class` a scalar-byte type unlike any grammar leaf), forcing regex knowledge into a grammar
   pass. The prototype must count arms and grade each as trivial/total or "regex leaks in."
2. **Codepoint vs byte (the UTF-8 risk).** bbnf `Literal { bytes }` is byte-oriented; the vendored
   HIR distinguishes `ByteRange` from `CodepointRange` (`:12/:27`) and carries a 243-LOC
   `utf8.rs` expander. If `Class` carries codepoint ranges, the *shared* enum acquires Unicode
   semantics no grammar leaf has; if `Class` is byte-only, a codepoint class must UTF-8-expand
   to byte-range alternations. *Counterexample:* `[α-ω]*` — byte-expansion inside a star can
   blow the shared tree into a large `Alt` of byte-`Seq`s. Mitigation to test: keep `Class`
   byte-clean and confine UTF-8 expansion to *automata construction* (the `utf8.rs` step), so the
   tree stays small and Unicode never touches the grammar plane.
3. **Consumer-less `Look` (Y.13 risk).** Lookahead/lookbehind/`\b` are zero-width and *only regex*
   emits them; if no grammar-plane consumer ever reads `Look`, it is consumer-less substrate in
   the shared enum (constitution clause viii forbids this). *Counterexample:* a `Look` variant
   that only the NFA builder ever matches, dead in every other pass. Mitigation to test: prove a
   grammar-side consumer (the start-anchored vs unanchored distinction PT-1.W5 needs is a `Look`
   at the leaf) — or demote `Look` to an `Annotation` (`:257`) if the automata builder can read it
   there, keeping `ExprKind` free of a regex-only variant.
4. **Bootstrap circularity (LOCK-7 interlock).** regex.bbnf is bbnf-defined, yet bbnf grammars
   contain regex leaves (json.bbnf uses `/[ \t\n\r]*/`). The engine that parses regex.bbnf must
   itself already lower regex. *Counterexample:* the very first regen has no committed regex
   lowering to parse its own `Class`-producing grammar. Mitigation: a committed stage-0
   regex→`ExprKind` lowering (hand or generated-once), fixpoint `regen∘regen` byte-identical —
   this is 5A's hard dependency on LOCK-7 (7A staged bootstrap) and must be named as such.
5. **Existing-test regression.** The `ir` oracle at `lib.rs:652` asserts the three JSON regex
   leaves' nullability via the *current* `analyze()` stub. Structural nullability over `ExprKind`
   must reproduce those answers bit-for-bit (`ws` nullable, string/number not) or ONE-HIR breaks
   a green test — the cheapest, sharpest refutation gate.

## What a prototype must test to move this family

1. **Add `Class` + `Look` to `ir::ExprKind`; census the tax.** Compile and count every match
   site across `ir`+`passes`+`codegen` that must grow arms; grade each grammar-plane arm
   trivial/total vs "regex leaked in." Report the LOC delta and the worst arm. (Refutes/confirms
   risk 1 — the family's central question.)
2. **Structural parity oracle.** Parse the three JSON leaves (`[ \t\n\r]*`, the quoted-string
   pattern `lib.rs:662`, the number pattern `:668`) through regex.bbnf into `ExprKind`, run the
   *extended* `ir::nullability`, and assert equality with the current `analyze()` answers
   (`lib.rs:652` oracle). Green = ONE-HIR preserves behavior; red = it breaks parity.
3. **Automata from `ExprKind` — at the certified numbers.** Build a Thompson NFA directly from
   a `Class/Seq/Alt/Repeat` subtree (`from_hir → from_expr`) and show `Dfa::find` byte-identical
   match sets vs the vendored `Hir`→NFA path (reuse `parse-that-regex/automata/dfa.rs`
   unchanged), THEN run PROVE-10's prefilter lane and reproduce its certified band —
   **8.8–29.5×** on the four prefilterable patterns (`0x[0-9a-fA-F]+`, `#[0-9a-fA-F]{3,6}`,
   `https?://…`, `(?:foo|bar)[a-z]*`) with **0.97–1.01×** controls. This proves the ONE type
   hosts the *whole* engine at the frozen §0/PROVE-10 numbers, so ONE-HIR costs nothing the
   two-HIR world has — the strongest positive result available to move the family.
4. **UTF-8 placement decision.** Lower one codepoint class (`[α-ω]` or unicode `\d`) both ways
   (byte-only `Class` + build-time expansion vs codepoint-carrying `Class`); measure shared-tree
   node count and confirm no Unicode leaks into a grammar pass. (Resolves risk 2.)
5. **Bootstrap fixpoint stub.** Commit a stage-0 regex→`ExprKind` lowering and show
   regen∘regen byte-identical on regex.bbnf + json.bbnf — the LOCK-7 interlock evidence
   (risk 4). If this cannot be shown, 5A is SPEC-ONLY until 7A lands, and must say so.

The two decisive numbers this family lives or dies by: **the match-arm tax LOC/grade delta**
(step 1) and **the structural-nullability parity result** (step 2). If the tax is trivial-total
and parity is green, ONE-HIR dominates 5B (no functor to maintain, no round-trip oracle). If the
tax leaks regex into grammar passes or parity diverges, 5A concedes to a functor and this becomes
a measured negative result.
