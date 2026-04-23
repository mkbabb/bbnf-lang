# Tranche AZ-I — Supplementary Research

External grounding and concrete design sketches that inform AZ-I's
direct-to-struct thesis for JSON, CSS L4, and Sheets. Consumed
alongside `AZ-I.md`; the file holds technique detail that would
otherwise bloat the plan.

AZ-II's BBNF-specific research (bootstrap cutover, byte-equal
reproducibility, tape-deletion risk, drift-source mitigations, and
the re-plan discipline when byte-equal fails) lives in
`docs/tranches/AZ-II/RESEARCH.md`.

## 1. Why dissolve the tape on the data grammars

The tape substrate served bbnf well through AQ and AR: a flat
16-byte record array gave cache-friendly traversal, a uniform
representation across grammars, and a single code-path for debug
and materialised views. It was the right shape for an era in which
grammar-derived typing was incomplete and the emitter had to route
through a generic structural substrate.

`project_types` and `StructRegistry` close that era for the data
grammars. Once JSON, CSS L4, and Sheets close through
`project_types`, the typed struct shape is known at build time;
writing the parse output into a flat record array and then
projecting it back into a typed struct at view time is a round trip
through a generic intermediate that no longer serves a purpose.
Every non-test consumer of the tape on these three grammars is a
materialised view whose shape is the same struct the emitter would
write directly.

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

The anti-pattern AZ-I explicitly avoids is Era V's DTA/PSI episode
(see `docs/tranches/meta-audit/06-commit-archaeology.md`). DTA and
PSI were substrates landed in anticipation of consumers that were
supposed to follow in a later wave. The consumers never arrived on
schedule; the substrates accumulated for ~572 commits; the whole
stack unwound. AZ-I's discipline is the inverse: every wave ships
substrate and consumer in the same commit on the three data grammars.
The final substrate (tape) is dissolved on those three, but is left
intact on BBNF for AZ-II to migrate.

The tape also carries hidden coupling cost. `crates/tape/` currently
houses the structural-scan path, the DTA interpreter, the PSI
cursor, the finaliser, and the deduplicator. Each of those has its
own profile, its own test surface, its own set of invariants. Under
AZ-I, those responsibilities collapse on the three data grammars:
the struct builder replaces the finaliser, the struct graph replaces
the cursor, the IR-level typing pass replaces runtime deduplication.
One code path with one set of invariants is cheaper to maintain,
cheaper to profile, and cheaper to reason about. The same collapse
then rolls across BBNF in AZ-II.

## 2. Concrete struct shapes for the three data grammars

AZ-I's emission path writes into grammar-derived structs whose
shapes come from `project_types`. The following sketches are
illustrative, not authoritative; `project_types` produces the
canonical form at build time.

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

A `Cell` struct with a typed value union and a `CellAddr`
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

## 3. Parent-pointer vs root-traversal under struct-tree navigation

Without a tape on the three data grammars, there is no sidecar
column to widen; the question of how to navigate from a struct
node back to its enclosing parent is re-posed as a direct struct-
tree question. AZ-I produces the struct tree; BA consumes it. The
two candidate shapes are:

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
mutation of a parent invalidates children — but the AZ-I shape is
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

AZ-I does not choose. The choice is BA's concern, in BA.W0, because
BA is the tranche that introduces the query surface (pointer-path
queries, lazy skip) on top of the struct tree. AZ-I produces a
struct tree that supports either shape: the default is no parent
pointer (cheapest structs), and BA.W0 measures the actual access
profile on the 17-entry matrix and adds the pointer only if the
measurement warrants it. This is the inverse of Era V's pattern
(add the substrate hoping for a consumer); AZ-I hands BA a clean
floor and BA measures what it needs. AZ-II inherits the same
decision surface on BBNF once its struct tree lands.

## 4. simdjson / sonic-rs / lightningcss / yyjson techniques

The external parsers that inform AZ-I's substrate choices on the
data grammars:

**simdjson ondemand.** After the tape API proved limited for user
ergonomics, simdjson introduced ondemand as a lazy, struct-direct
accessor model. The parser tracks structural offsets into the input
and constructs typed accessors over the source bytes directly; no
tape is materialised. AZ-I's direct-to-struct is the eager dual of
ondemand — eager materialisation of the same shape ondemand
materialises lazily. BA then introduces laziness on AZ-I's
substrate, matching ondemand's access pattern at query time.

**sonic-rs LazyValue + pointer!.** sonic-rs exposes a `pointer!`
macro that compiles a path expression into a `PointerTree` and
traverses it over a `LazyValue` without fully parsing. Sibling
keys and off-path array elements are skipped via structural
offsets. bbnf's equivalent is BA's pointer-path query surface over
AZ-I's struct tree. AZ-I makes that surface possible by
guaranteeing the struct shape on JSON, CSS L4, and Sheets is
grammar-derived and stable.

**lightningcss typed values.** Every CSS property has a specific
Rust type produced by `lightningcss-derive`, a proc-macro that
reads property declarations and emits `Deserialize` + `ToCss`
implementations. AZ-I's position is inverted: the grammar declares
the types; the IR infers them; the emitter materialises them. No
proc-macro, no hand-maintained enum list. The parity harness gate
at CSS L4's close is node-for-node equivalence to lightningcss on
its own fixture corpus.

**yyjson's dispatch and allocation frontier.** yyjson outperforms
simdjson on modern EPYC by exploiting ILP, branch prediction, and
low misaligned-access penalty rather than SIMD. The lesson: SIMD is
not where the next 10% lives; key dispatch (AP.4) and in-place
payload allocation are. AZ-I's direct-to-struct is the in-place
payload allocation partner for AP.4 + AP.5 NibbleLut on the
dispatch side.

**simdjson tape (historical reference).** bbnf's `TapeRec` record
layout was directly inspired by simdjson's tape: paired open/close
records, payload offsets, byte-tape side channel for decoded
strings. AZ-I does not abandon the insights — the struct builder's
traversal order on the three data grammars is still a DFS matching
simdjson's tape order — but the materialised artefact changes from
a flat record array to a struct graph. This informs AZ-II's BBNF
cutover as well: the `TapeRec` → struct field mapping is a
mechanical projection, not a redesign.

## 5. Residual combinator / SpanParser references

The AZ-I invariant that there is a single codegen path on the three
data grammars requires the absence of combinator / SpanParser
fallback code in the struct-only world for those grammars. A scan
of the current tree finds three residual references:

- `crates/analysis/src/state/types.rs` — `SpanParser` in an IDE-
  facing analysis state. Not on the parse hot path; AZ-I does not
  touch it.
- `crates/ir/src/types/rule.rs` — `SpanParser` referenced in a
  type-lattice test. Test-only; AZ-I does not touch it.
- `crates/ir/src/passes/span.rs` — `SpanParser` referenced in the
  span-inference pass. IR-internal; not on the parse hot path; AZ-I
  does not touch it.

None of the three leaks into the struct-only codegen path on JSON,
CSS L4, or Sheets. AZ-I's `no-combinator-fallback` invariant is
enforced on the emitter and runtime surfaces, not on IR-internal
representations. If a future tranche folds any of these into the
parse path, the invariant triggers.

The more load-bearing check is the tape-scoped-to-BBNF scan at AZ-I
close:

- `rg 'use bbnf_tape' crates/` → hits exclusively under
  `crates/bbnf_derive/` and `crates/core/src/runtime/bbnf/`.
- `rg '\bTapeRec\b|\bTapeBuilder\b|\bTapeCursor\b|\bColumns\b' \
   crates/core/src/runtime/{json,css_l4,sheets}/ --type rust` →
  zero matches.

W4 enforces these scans as part of the three-grammar slice
verification. If any scan finds residual tape references on the
three data grammars' paths at W4 close, the wave does not close
until the references are resolved.

## 6. `StructRegistry` coverage on the three data grammars

`project_types` closes to a fixed point over a grammar; each pass
resolves named-rule types, field types on `->` markers, and
compound closures on alternations/repetitions. For the three data
grammars the expected coverage surface is:

**JSON.** Four Named rules (`value`, `array`, `object`, `pair`),
plus anonymous compounds inside each. Closure is shallow — the
grammar recurses but does not parameterise — and each Named rule
emits a single `StructLayout`. Expected `StructRegistry` entry
count: 4 Named + ~3 anonymous = ~7 layouts.

**Sheets.** Five to seven Named rules (`sheet`, `row`, `cell`,
`formula`, `reference`, plus error/value sub-shapes). Closure is
shallow; formula recursion introduces one self-referencing layout
for `FormulaExpr`. Expected entry count: ~8 layouts.

**CSS L4.** The largest coverage surface — ~40 Named rules
corresponding to `stylesheet`, `at-rule` kinds, `selector` kinds,
`declaration`, and every typed-value kind (`<length>`, `<color>`,
`<angle>`, `<time>`, `<resolution>`, `<percentage>`, `<dimension>`,
`<image>`, `<gradient>`, `<function>`, `<calc>`). Closure threads
through typed-value enums (one `StructLayout` per enum variant set).
Expected entry count: ~60 layouts total.

The W1 gate is that every count above is met exactly (modulo grammar
edits) and every `Named` rule has a non-empty `StructLayout`. A
partial-close that leaves a single Named rule empty is a build-stop.

## 7. Hand-off to BA and downstream tranches

- **BA.** Parent-pointer vs root-traversal (§3) is handed off as
  the BA.W0 entry-point. BA's pointer-path query surface is built
  on AZ-I's struct tree for the three data grammars and, after
  AZ-II, on BBNF's as well. BA's opening measurement determines
  whether the tree carries parent pointers or performs root-
  traversal.
- **BB.** IR stability is the only AZ-I-side contract BB consumes.
  AZ-I does not rewire IR edges; `project_types` closes tighter and
  `StructRegistry` populates, but the IR's edge structure and pass
  ordering do not change. BB's egraph rule inference operates over
  the same IR surface pre-AZ-I and post-AZ-I, with a richer
  registry available as auxiliary input.
- **AZ-II.** The BBNF-bootstrap cutover that AZ-II owns consumes
  AZ-I's `StructRegistry` and `project_types` infrastructure
  unchanged. The only AZ-II-side change to AZ-I's artefacts is the
  addition of BBNF's Named rules to the registry; the IR audit
  pass is then extended to cover BBNF as well.

## Anti-precedents reaffirmed

1. **AW-V shape-emitter-for-JSON-only.** The direct-to-struct
   thesis must work for JSON, CSS L4, and Sheets in AZ-I — not one
   grammar at one wave. BBNF awaits AZ-II; that is a tranche-level
   scope decision, not a wave-local shortcut.
2. **AO phase-0 activation failure.** Ship the runtime consumer
   with the substrate, never before.
3. **AM.1 EmissionTier lattice.** One decision surface on the
   three data grammars, not two.
4. **AX.W1.A / AX.W1.B hand-coded values.** Grammar-derived or not
   at all.
5. **AW-IV "every entry exceeds post-AU" with zero entries
   exceeding.** Gate at the 20% floor; revert on miss.
6. **Era V DTA/PSI.** Substrate-first-consumer-later is the failure
   mode AZ-I exists to preclude. Every wave lands substrate and
   consumer together on the three data grammars.
