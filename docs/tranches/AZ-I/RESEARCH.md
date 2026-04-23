# Tranche AZ — Supplementary Research

External grounding and concrete design sketches that inform AZ's
direct-to-struct-with-tape-dissolution thesis. Consumed alongside
`AZ.md`; the file holds technique detail that would otherwise bloat
the plan.

## 1. Why dissolve the tape

The tape substrate served bbnf well through AQ and AR: a flat
16-byte record array gave cache-friendly traversal, a uniform
representation across grammars, and a single code-path for debug
and materialised views. It was the right shape for an era in which
grammar-derived typing was incomplete and the emitter had to route
through a generic structural substrate.

`project_types` and `StructRegistry` close that era. Once a grammar
closes through `project_types`, the typed struct shape is known at
build time; writing the parse output into a flat record array and
then projecting it back into a typed struct at view time is a round
trip through a generic intermediate that no longer serves a purpose.
Every non-test consumer of the tape is a materialised view whose
shape is the same struct the emitter would write directly.

The state-of-the-art JSON parsers already reflect this. sonic-rs's
ondemand path operates struct-direct on the source bytes, guided by
simdjson-style structural offsets; it does not materialise a tape
between the bytes and the user struct. yyjson, the pure-C89 parser
that leads simdjson on modern EPYC, allocates its tree directly; its
"tape" is the tree itself. simdjson's own ondemand mode, introduced
after the original tape API, likewise skips tape materialisation in
favour of on-the-fly lazy parsing.

bbnf's position is strictly stronger than sonic-rs's or simdjson's.
Their struct shape comes from a user's `Deserialize` impl or an
ondemand accessor path; bbnf's struct shape comes from the grammar
itself. No user work, no second source of truth, no drift between
the user's declared type and the parser's runtime type.

The anti-pattern AZ explicitly avoids is Era V's DTA/PSI episode
(see `docs/tranches/meta-audit/06-commit-archaeology.md`). DTA and
PSI were substrates landed in anticipation of consumers that were
supposed to follow in a later wave. The consumers never arrived on
schedule; the substrates accumulated for ~572 commits; the whole
stack unwound. AZ's discipline is the inverse: every wave ships
substrate and consumer in the same commit, and the final substrate
(tape) is dissolved entirely rather than carried forward.

The tape also carries hidden coupling cost. `crates/tape/` currently
houses the structural-scan path, the DTA interpreter, the PSI
cursor, the finaliser, and the deduplicator. Each of those has its
own profile, its own test surface, its own set of invariants. Under
AZ, those responsibilities collapse: the struct builder replaces the
finaliser, the struct graph replaces the cursor, the IR-level typing
pass replaces runtime deduplication. One code path with one set of
invariants is cheaper to maintain, cheaper to profile, and cheaper
to reason about.

## 2. Concrete struct shapes for the four primary grammars

AZ's emission path writes into grammar-derived structs whose shapes
come from `project_types`. The following sketches are illustrative,
not authoritative; `project_types` produces the canonical form at
build time.

### JSON

A `JsonValue` enum with a variant per alternation in the grammar's
`value` production:

```rust
enum JsonValue<'a> {
    Null,
    Bool(bool),
    Number(JsonNumber),          // i64 | u64 | f64 witness
    String(&'a str),             // arena-allocated slice
    Array(&'a [JsonValue<'a>]),  // arena-allocated slice
    Object(&'a [JsonPair<'a>]),  // arena-allocated slice of (key, value)
}

struct JsonPair<'a> { key: &'a str, value: JsonValue<'a> }

enum JsonNumber { Int(i64), UInt(u64), Float(f64) }
```

The arena-allocation discipline keeps the shape single-threaded and
single-arena, matching sonic-rs's zero-copy ondemand approach.

### Sheets

A `Cell` struct with a typed value union and an `CellAddr`
identifier:

```rust
struct Cell<'a> {
    addr: CellAddr,              // row, col
    value: CellValue<'a>,
}

enum CellValue<'a> {
    Empty,
    Number(f64),
    Integer(i64),
    Bool(bool),
    Text(&'a str),
    Formula(&'a Formula<'a>),    // arena-allocated
    Error(CellError),
}

struct Formula<'a> { source: &'a str, ast: &'a FormulaExpr<'a> }
```

### CSS L4

A rich, lightningcss-parity shape with one Rust enum per typed value
kind. The sketch below covers the first-order types; the full
derivation is whatever `project_types` produces from `css-l4.bbnf`:

```rust
struct StyleSheet<'a> { rules: &'a [CssRule<'a>] }

enum CssRule<'a> {
    Style(StyleRule<'a>),
    AtMedia(MediaRule<'a>),
    AtKeyframes(KeyframesRule<'a>),
    // ... per grammar alternation
}

struct StyleRule<'a> {
    selectors: &'a [Selector<'a>],
    declarations: &'a [Declaration<'a>],
}

struct Declaration<'a> {
    property: PropertyId,
    value: TypedValue<'a>,
    important: bool,
}

enum TypedValue<'a> {
    Length(Length),
    Color(Color),
    Dimension(Dimension),
    Time(Time),
    Resolution(Resolution),
    Percentage(Percentage),
    Angle(Angle),
    Keyword(KeywordId),
    List(&'a [TypedValue<'a>]),
    // ... per grammar
}

enum Length { Px(f32), Em(f32), Rem(f32), Vw(f32), /* ... */ Calc(&'a CalcExpr) }

enum Color { Rgb(u8,u8,u8), Rgba(u8,u8,u8,f32), Hsl(f32,f32,f32), /* ... */ Named(NamedColor) }
```

The parity gate is that each typed variant is convertible to
lightningcss's equivalent — `bbnf::css::Length` ⇄
`lightningcss::values::length::Length`, and so on — through the
parity harness on the full lightningcss fixture corpus.

### BBNF

BBNF's own AST derived from `grammars/bbnf/bbnf.bbnf`:

```rust
struct BbnfAst<'a> {
    imports: &'a [Import<'a>],
    directives: &'a [Directive<'a>],
    rules: &'a [Rule<'a>],
    comments: &'a [Comment<'a>],
}

struct Rule<'a> {
    name: Ident<'a>,
    params: &'a [Param<'a>],
    return_type: Option<TypeExpr<'a>>,
    body: Expr<'a>,
}

enum Expr<'a> {
    Alt(&'a [Expr<'a>]),
    Seq(&'a [Expr<'a>]),
    Call { target: Ident<'a>, args: &'a [Expr<'a>] },
    Regex(&'a RegexPattern<'a>),
    Ident(Ident<'a>),
    Literal(&'a str),
    Repeat { inner: &'a Expr<'a>, min: u32, max: Option<u32> },
    // ... per grammar alternation
}
```

This shape mirrors the compiler's existing in-memory IR surface.
The W4 cutover test is that `BbnfAst` parsed via the derived struct
path is byte-equal to the compiler's current internal representation
on every grammar in the corpus.

## 3. Parent-pointer vs root-traversal under struct-tree navigation

Without a tape, there is no sidecar column to widen; the question of
how to navigate from a struct node back to its enclosing parent is
re-posed as a direct struct-tree question. The two candidate shapes
are:

### Parent pointer

Every non-root struct carries `parent: &'a ParentKind<'a>`, an
arena-borrowed reference to its enclosing compound:

```rust
struct Declaration<'a> {
    parent: &'a StyleRule<'a>,
    property: PropertyId,
    value: TypedValue<'a>,
    important: bool,
}
```

**Cost.** One pointer (8 bytes on 64-bit) per non-root node. On
twitter-scale JSON (~600 KB parsed → ~300K nodes), this is ~2.4 MB
of pointer overhead — non-trivial but well within the arena.

**Benefit.** O(1) parent access, no rewalk from root, no auxiliary
index structure.

**Risk.** The `'a` lifetime must thread through every struct, and
mutation of a parent invalidates children — but the AZ shape is
immutable-after-construction, so the mutation concern does not
apply to the parse-then-read workflow.

### Root-traversal

No parent pointer; access paths are always rooted at the document
root. Parent access is a rewalk:

```rust
fn find_parent<'a>(root: &'a CssRule<'a>, target: &'a Declaration<'a>) -> Option<&'a StyleRule<'a>>
```

**Cost.** Per-access: O(depth) on average, O(n) worst case.
Per-struct: zero.

**Benefit.** No lifetime thread, no pointer overhead, no
backwards dependency edge.

**Risk.** Repeated parent access on deep trees is expensive; BA's
pointer-path query design must cache or pre-compute traversals.

### Disposition

AZ does not choose. The choice is BA's concern, in BA.W0, because
BA is the tranche that introduces the query surface (pointer-path
queries, lazy skip) on top of the struct tree. AZ produces a struct
tree that supports either shape: the default is no parent pointer
(cheapest structs), and BA.W0 measures the actual access profile on
the 17-entry matrix and adds the pointer only if the measurement
warrants it. This is the inverse of Era V's pattern (add the
substrate hoping for a consumer); AZ hands BA a clean floor and BA
measures what it needs.

## 4. Bootstrap cutover for BBNF

BBNF's parser is produced by bbnf-derive. bbnf-derive is a proc-
macro that expands `#[bbnf_grammar = "grammars/bbnf/bbnf.bbnf"]`
into a parser specialised to that grammar. The parser, when run on
any BBNF grammar file (including bbnf.bbnf itself), produces an IR
representation the compiler consumes.

Pre-AZ flow:

1. bbnf-derive expands the grammar into a parser that writes into a
   tape.
2. The compiler's IR loader reads the tape and builds the in-memory
   IR.
3. The in-memory IR feeds `project_types`, `StructRegistry`, and
   every other pass.

Post-AZ flow:

1. bbnf-derive expands the grammar into a parser that writes into a
   derived `BbnfAst` struct.
2. The compiler's IR loader reads `BbnfAst` directly and builds the
   in-memory IR.
3. Identical from step 3 onward.

The cutover problem is that the pre-AZ parser is what builds bbnf-
derive, which is what builds the post-AZ parser. A naive cutover
(swap the emission target in bbnf-derive's code generator) produces
a post-AZ bbnf-derive that can only be built by a pre-AZ compiler,
breaking reproducibility.

The two-stage bootstrap in AZ.W4 threads the cutover:

**Stage A (pre-W4 → W4-candidate).** The pre-AZ compiler (tape-
based) builds the W4-candidate compiler (struct-based). The W4-
candidate compiler's bbnf-derive now emits struct-writing parsers,
but the W4-candidate compiler itself was built from a tape-writing
parser.

**Stage B (W4-candidate → W4-final).** The W4-candidate compiler
rebuilds itself from its own source. The W4-final compiler is now
built from a struct-writing parser and produces struct-writing
parsers. The tape has been unwired in both directions.

**Reproducibility check.** The W4-final compiler, run on every
grammar in the corpus, must produce IR byte-equal to the pre-AZ
compiler's IR on the same input. This is the load-bearing W4 close
gate: anything less than byte-equality on the corpus means the
cutover introduced a semantic drift and the W4 substrate is
reverted.

The reproducibility gate is non-trivial. Possible drift sources:

- AST ordering: the derived struct may order fields differently than
  the tape's cursor traversal. Fix: the derivation follows the
  grammar's declaration order, which is stable.
- Comment/whitespace handling: the tape preserved certain trivia
  via `Columns` side-channel; the struct may or may not preserve
  the same trivia. Fix: the derived struct's comment / trivia
  fields are explicit per grammar, so the preservation contract is
  visible at the type level.
- Numeric formatting: f64 precision roundtrip. Fix: the struct
  carries the source span for every numeric leaf, so exact-byte
  recovery is available if the display form drifts.

If any of these drift sources proves intractable on the corpus, AZ
invokes its defensible floor and closes with tape retained for BBNF
only (see `AZ.md` §Defensible floor).

## 5. simdjson / sonic-rs / lightningcss / yyjson techniques

The external parsers that inform AZ's substrate choices:

**simdjson ondemand.** After the tape API proved limited for user
ergonomics, simdjson introduced ondemand as a lazy, struct-direct
accessor model. The parser tracks structural offsets into the input
and constructs typed accessors over the source bytes directly; no
tape is materialised. AZ's direct-to-struct is the eager dual of
ondemand — eager materialisation of the same shape ondemand
materialises lazily. BA then introduces laziness on AZ's substrate,
matching ondemand's access pattern at query time.

**sonic-rs LazyValue + pointer!.** sonic-rs exposes a `pointer!`
macro that compiles a path expression into a `PointerTree` and
traverses it over a `LazyValue` without fully parsing. Sibling
keys and off-path array elements are skipped via structural
offsets. bbnf's equivalent is BA's pointer-path query surface over
AZ's struct tree. AZ makes that surface possible by guaranteeing
the struct shape is grammar-derived and stable.

**lightningcss typed values.** Every CSS property has a specific
Rust type produced by `lightningcss-derive`, a proc-macro that
reads property declarations and emits `Deserialize` + `ToCss`
implementations. AZ's position is inverted: the grammar declares
the types; the IR infers them; the emitter materialises them. No
proc-macro, no hand-maintained enum list. The parity harness gate
at CSS L4's close is node-for-node equivalence to lightningcss on
its own fixture corpus.

**yyjson's dispatch and allocation frontier.** yyjson outperforms
simdjson on modern EPYC by exploiting ILP, branch prediction, and
low misaligned-access penalty rather than SIMD. The lesson: SIMD is
not where the next 10% lives; key dispatch (AP.4) and in-place
payload allocation are. AZ's direct-to-struct is the in-place
payload allocation partner for AP.4 + AP.5 NibbleLut on the
dispatch side.

**simdjson tape (historical reference).** bbnf's `TapeRec` record
layout was directly inspired by simdjson's tape: paired open/close
records, payload offsets, byte-tape side channel for decoded
strings. AZ does not abandon the insights — the struct builder's
traversal order is still a DFS matching simdjson's tape order —
but the materialised artefact changes from a flat record array to
a struct graph.

## 6. Residual combinator / SpanParser references

The AZ invariant that there is a single codegen path requires the
absence of combinator / SpanParser fallback code in the struct-only
world. A scan of the current tree finds three residual references:

- `crates/analysis/src/state/types.rs` — `SpanParser` in an IDE-
  facing analysis state. Not on the parse hot path; AZ does not
  touch it.
- `crates/ir/src/types/rule.rs` — `SpanParser` referenced in a
  type-lattice test. Test-only; AZ does not touch it.
- `crates/ir/src/passes/span.rs` — `SpanParser` referenced in the
  span-inference pass. IR-internal; not on the parse hot path; AZ
  does not touch it.

None of the three leaks into the struct-only codegen path. AZ's
`no-combinator-fallback` invariant is enforced on the emitter and
runtime surfaces, not on IR-internal representations. If a future
tranche folds any of these into the parse path, the invariant
triggers.

The more load-bearing check is the tape-residual scan at AZ close:

- `rg '^crates/tape/' .` → zero matches.
- `rg 'use bbnf_tape' crates/` → zero matches.
- `rg '\bTapeRec\b|\bTapeBuilder\b|\bTapeCursor\b|\bColumns\b' crates/ --type rust` → zero matches outside parity harnesses.

W5 enforces these scans as part of the tape-crate-deleted verification.
If any scan finds residual references at W5 close, the wave does not
close until the references are resolved. The tape-crate-deleted CI
job runs the same scans on every commit to AZ's branch post-W5, so
any accidental re-introduction of a tape symbol blocks the merge.

## Cross-reference: how AZ's research threads into BA and BB

- **BA.** Parent-pointer vs root-traversal (§3) is handed off as
  the BA.W0 entry-point. BA's pointer-path query surface is built
  on AZ's struct tree; BA's opening measurement determines whether
  the tree carries parent pointers or performs root-traversal.
- **BB.** IR stability is the only AZ-side contract BB consumes.
  AZ does not rewire IR edges; `project_types` closes tighter and
  `StructRegistry` populates, but the IR's edge structure and pass
  ordering do not change. BB's egraph rule inference operates over
  the same IR surface pre-AZ and post-AZ, with a richer registry
  available as auxiliary input.

## Anti-precedents reaffirmed

1. **AW-V shape-emitter-for-JSON-only.** The direct-to-struct
   thesis must work for every grammar at every wave, not one
   grammar at one wave.
2. **AO phase-0 activation failure.** Ship the runtime consumer
   with the substrate, never before.
3. **AM.1 EmissionTier lattice.** One decision surface, not two.
4. **AX.W1.A / AX.W1.B hand-coded values.** Grammar-derived or not
   at all.
5. **AW-IV "every entry exceeds post-AU" with zero entries
   exceeding.** Gate at the 20% floor; revert on miss.
6. **Era V DTA/PSI.** Substrate-first-consumer-later is the failure
   mode AZ exists to preclude. Every wave lands substrate and
   consumer together.
