# LOCK-5 · FAMILY 5A · ONE-HIR — regex HIR as a fragment of the one grammar HIR

## The mechanism, made concrete

Regex literals in a grammar cease to be an opaque `String` leaf and become *structure in the
same enum the grammar plane already walks*. Two variants are promoted into the shared
expression node — `Class(CharClass)` (byte/codepoint set + negation flag) and `Look(Look)`
(zero-width position assertion) — joining the combinators (`Seq/Concat`, `Alt/Alternation`,
`Repeat/Repetition`, `Optional`) that regex and grammar *already share by construction*. A
regex literal parses (via the bootstrapped regex grammar, projected with `->`) directly into
that enum; automata build directly from the merged fragment; and the 1148-LOC classifier
re-homes as an analysis pass over the shared node rather than over a private `Hir`. The
standalone parse-that regex API survives as a thin front that calls the same `parse →
fragment → NFA` path. No second owned tree, no functor, no conversion cast.

The load-bearing enabler is that the e-node form is *derived, not authored*. `egraph-derive`'s
`Language` impl classifies fields by type (`Id`, `Box<[Id]>`, `[Id;N]`) with no per-field
attributes (`crates/ir/src/egraph/node.rs` header: "Mirrors DagNode variant-for-variant but
with egraph::Id children … Language auto-derived"). So one *authored* enum with `Class`/`Look`
yields the Id-children e-node for the saturation cache for free. This is precisely egg's
`define_language!` discipline (author variants once; recursive positions are `Id`) and the
"Trees That Grow" technique (Najd & Peyton Jones, *Haskell '17*): a single AST type with an
extension point, so the regex fragment is an *extension* of the grammar HIR, not a god-type.

## The two live trees (every number below names its tree)

- **skinny `crates/ir`** (`skinny/crates/ir/src/lib.rs:234`): `ExprKind` has **10 variants incl.
  `Project`** (`:261`). Regex leaf = opaque `Regex{pattern:String}` (`:250`). Backed by a
  **separate 322-LOC** skinny `bbnf-regex` (`skinny/crates/bbnf-regex/src/lib.rs:45 analyze`)
  exposing a *stub* `RegexFacts{nullable, first:FirstSet, byte_classes:Vec<ByteClass>,
  hir:RegexHir{kind:RegexKind}, string}` over a 4-variant `RegexKind{Whitespace, QuotedString,
  Numeric, Unknown}`. Nullability invokes `analyze(pattern)` *inline at the shared-enum walk*
  (`skinny/crates/ir/src/lib.rs:331`; consumers also at `crates/passes/src/lib.rs:1`).
- **top-level `crates/ir`** (`crates/ir/src/types/node.rs:30`): `IrNode` has **13 variants**;
  regex leaf = interned `Regex(StringId)` (`:36`). It ALSO carries the grammar-tier regex
  e-graph — `GrammarENode::Regex(StringId)` (`crates/ir/src/egraph/node.rs`) + the **434-LOC**
  `crates/ir/src/egraph/rules/regex.rs` (Dedup/Superset/Union/**FuseAltRegexBranches**). Backed
  by the **6742-LOC** parse-that `bbnf-regex` whose real `Hir` (`parse-that/rust/regex/src/hir/
  mod.rs:216`) is `Empty|Literal|Class(CharClass)|Look(Look)|Repetition|Group|Concat|Alternation`,
  with `CharClass::{Bytes,Unicode}{ranges,negated}` (`:44`), `Look` 6-variant (`:172`), and
  1148-LOC classify (`parse-that/rust/regex/src/classify.rs:55`).

The reconciled tree of record is the **top-level `crates/ir` + parse-that `bbnf-regex`** pair;
the skinny 4-variant `RegexKind` world is a stub whose measurements do not transfer. Any 5A
prototype runs there.

## Per-arm leak census (the census the directive demands)

Census over `IrNode` (HEAD, 13 variants, node.rs:30) and skinny `ExprKind` (10 incl. `Project`,
lib.rs:234): **no combinator arm carries a regex-specific concern.** `Seq/Alt/Repeat/Optional/
Skip/Next/Minus/Negate/Map/Ref/TokenDispatch/Project` are domain-neutral. The *only* regex-bearing
arm is the leaf itself (`Regex(StringId)` / `Regex{pattern}`). What leaks today is not into the
arms but into a **side-table**: the top tree threads regex facts through `regex_info` pass
(`crates/ir/src/passes/regex_info.rs:24 compute_regex_info`, StringId→RegexInfo cache) and the
skinny tree calls `analyze()` at the nullability walk. Promoting `Class`/`Look` therefore does
*not* pollute the combinator arms — it replaces one opaque leaf with the leaf's own internal
structure, and lets the side-table facts (FIRST/nullable/width) be read *off the structure*
by the shared analysis fabric that already computes FIRST/nullability for the combinators.
Under real `Project`/`->` typing this is unchanged: `Project` composes over whatever the leaf
produces (a `Span`), regardless of whether the leaf is opaque or a `Class`.

## Class/Look under LOCK-1's real type system (not skinny's stub)

`Class` **reuses the literal byte type**: both a literal and a `[a-z]+` recognize bytes and
project to `TypeDesc::Span` at the tape (`crates/ir/src/types/type_desc.rs:13`, `Span` = the
borrowed byte span — same node a `Literal` yields). No scalar-byte type is needed in the shared
enum. The one genuine two-world concern is `CharClass::Unicode` (`CodepointRange{char,char}`,
mod.rs:27) vs `CharClass::Bytes` (`ByteRange{u8,u8}`, mod.rs:12): the *codepoint* form is a
scalar (`char`) concern that would ride in the shared enum. But `[α-ω]*` **expansion to UTF-8
byte sequences is confined to automata** — `to_positive_byte_ranges()` *panics* on `Unicode`
(mod.rs:82), forcing the byte↔codepoint split at the NFA boundary (`from_hir`/`build_hir`),
never in the shared type nor in `TypeDesc`. So typing is clean (`Class ⊢ Span`); the residual
judgment is only whether carrying `CodepointRange` (a `char`, not a byte) inside the shared
enum is admissible — it is POD and `Serialize`-clean, so yes.

## A grammar-side consumer that READS `Look` (and a test that can fail)

Grammar-embedded regex is *anchored at the rule seam* (the combinator consumes from the current
offset), so `^`/`Look::Start` at pattern start is a **no-op** — the redundancy is real and
`Look::Start` risks being carried-but-never-read (Constitution viii forbids consumer-less
substrate). The consumer that *does* read `Look` is the automaton's **accept condition** for
end-anchors: `Look::End`/`$` forces the match to consume to a boundary. Falsifiable test in the
merged fragment: rule bodies `/foo$/` and `/foo/` must diverge on input `"foobar"` — `/foo$/`
**rejects**, `/foo/` accepts. If `Look` is dropped or ignored, both accept → test FAILS. This
proves `Look::End`/`EndLF` is read; it simultaneously exposes that `Look::Start` may be dead for
grammar-embedded regex, so the honest minimal admissible set is `{End, EndLF}` (± CRLF), not the
full six the standalone parse-that engine carries. The prefilter interacts too: `Prefilter::from_dfa`
(PROVE-10) fires only on unanchored search — a `Look::Start` would have to *disable* the memchr
skip, another reader. Net: `Look` belongs as an enum leaf *only for the variants a
grammar-embedded consumer honors*; the rest are an annotation on the automaton, not shared leaves.

## Automata directly from the merged fragment (`from_expr`), byte-identical

The proof obligation: add `Nfa::from_expr(&IrNode)` as a sibling of `Nfa::from_hir`
(`parse-that/rust/regex/src/automata/nfa.rs:71`; recursive builder `build_hir` at `:125`) that
matches `Class/Look/Repeat/Concat/Alt` on the *shared* node, and assert its NFA state+transition
table is **byte-identical** to `from_hir(hir)` over the parse-that corpus. It *is* byte-identical
because the mapping is 1:1: `IrNode::Class(CharClass)→Hir::Class`, `Look→Look`,
`Repeat{lo,hi}→Repetition`, `Seq→Concat`, `Alt→Alternation`. The two reshapings to test
explicitly: (a) **Group elision** — parse-that `Hir::Group(Box<Hir>)` is *transparent* (mod.rs:227:
"only the sub-expression matters"), while `IrNode` has **no `Group`** (desugared), so
`Concat[Group[x]]` vs `Concat[x]` must produce identical NFAs (Group is a no-op fragment —
provable, must be asserted); (b) **Optional** — skinny/top desugar `?` to `Repeat{0,1}` where
parse-that keeps `Repetition{min:0,max:1}` — same fragment. Throughput (find/scan) is then
re-derived *from this merged type* through the existing `Dfa::compile`→`Dfa::find` +
`Prefilter::from_dfa` path (PROVE-10, GO, 9.6–29× on prefilterable / 0.99× controls, in the
reconciled tree), inheriting the tier at zero new cost.

## Can ONE type serve the saturation cache (Eq+Hash) AND the IR (arena/span/Serialize)?

This is the sharpest tension and the honest answer is **one *authored* type, two *derived*
views** — not one monomorphic god-type. The three roles pull different derives:

- `IrNode` derives `Serialize,Deserialize,Clone,Debug,PartialEq` — **no `Eq`, no `Hash`**;
  spanned (via the `Expr`/`GrammarSpan` wrapper); interned `StringId`; owned `Box`/`Vec`.
- parse-that `Hir` derives `Clone,Debug,PartialEq,Eq,Hash` — **no `Serialize`, no span**; keys
  the per-compile `bbnf_regex::egraph::SaturationCache` (`regex_info.rs` imports it).
- `GrammarENode` derives `Eq,Hash` with **`Id` children** (the e-graph canonical form).

The collision: a spanned `Class` appearing twice at different offsets is *semantically equal*
but structurally `Eq`-unequal — so the cache **must key on a span-erased form**, which is
exactly why the Id-interned `GrammarENode` exists. The rescue is that `GrammarENode` is
*derived* from the authored enum by `egraph-derive` (node.rs header). Therefore ONE authored
`IrNode` (+`Class`/`Look`) → `Serialize`-owned view for arena/IR/span, and an auto-derived
Id-hashed view for the cache. The parse-that **owned `Hir` (Box/Vec) is what DIES** — it is the
duplicate. This does require `IrNode`+`CharClass`+`Look` to *also* be `Eq`-able where the derive
runs; `CharClass`/`ByteRange`/`CodepointRange`/`Look` already derive `Eq+Hash` (mod.rs:11,26,44,171),
and `CharClass` is `Serialize`-clean (POD ranges) — so the extended enum satisfies both derive
sets. Named counterexample that pins the design: the cache MUST NOT key on `IrNode` directly
(span defeats the hit) — proving the span-erased Id-form is *load-bearing*, not incidental.

Re-homing classify: `classify_regex_from_hir(&Hir)→RegexClass` (classify.rs:177) becomes a pass
over `IrNode::{Class,Look,Concat,...}`. It **shrinks only where it duplicates** the grammar
plane's existing analyses: the FIRST-set (`first.rs`), width (`info/width.rs`), and nullability
sub-computations fold into the shared analysis fabric that already runs for combinators; the
*taxonomy-detection core* (Numeric/QuotedString/Identifier/HexDigits/…) relocates 1:1. Net
projection: modest shrink, mostly relocation — the exact ΔLOC is the prototype measurement. IR-side
consumers that already read the taxonomy and must be repointed: `passes/regex_info.rs:76
has_kernel_coverage`, `types/recognizer_configs.rs:16`, `types/fn_descriptor.rs:33`,
`types/grammar.rs:142`.

## Promote + de-dup + break-the-discard (never greenfield)

Three `regex.bbnf` already exist and 5A *promotes*, not authors:
- `parse-that/rust/bootstrap/regex.bbnf` (full L4 spine) is the keeper — but its generated
  parser **discards structure**: `parse_generated(pattern:&str) -> bool`
  (`parse-that/rust/parse_that/src/regex/generated.rs:2288`, "Returns true if the parse consumed
  the entire input"). The hand-parser bridge `parse_generated -> Result<Hir>`
  (`.../src/regex/mod.rs:34`) is the *only* path that yields a fragment. **Break-the-discard**:
  extend `bootstrap/regex.bbnf` with `->` projections so the generated parser emits the merged
  fragment, retiring the hand parser as the structure source.
- `grammar/misc/regex.bbnf` is **divergent** (different rule names/EBNF; e.g. `char_class = "["
  , ["^"] , char_set , {char_set} , "]"`) — **de-dup**: delete in favor of the bootstrap spine.
- The skinny 4-variant `RegexKind` (`skinny/crates/bbnf-regex/src/lib.rs`) is a stub — **delete**;
  its consumers (skinny `ir/lib.rs:331`, `passes/lib.rs:1`) repoint to the rich fragment.

The cross-tier de-dup dividend is already visible: `FuseAltRegexBranches`
(`crates/ir/src/egraph/rules/regex.rs:19`) fuses `Alt([Regex,Lit,Regex])→Regex("a|b|c")` at the
grammar tier by delegating to `bbnf_regex::algebra` — with ONE-HIR the grammar-tier rule and the
HIR-tier rule operate on the *same* node, so the duplicate rule copy is a candidate for death
(the count is a 5A-adjacent measurement, formally owned by the functor family; note it here only
as evidence the two tiers already share algebra).

## Prior art (cited)

- **Trees That Grow** (Najd & Peyton Jones, Haskell '17): one AST type + extension point → the
  regex fragment as an extension of the grammar HIR, not a god-type. The direct theoretical anchor.
- **egg / egglog** `define_language!`: variants authored once, recursive positions are `Id` — the
  exact shape `egraph-derive` reproduces (`crates/ir/src/egraph/node.rs`), making the e-node a
  derived view of the authored enum.
- **regex-syntax `Hir`**: the ancestor this bespoke HIR deliberately *flattens* ("Flat enum —
  match directly, no `.kind()` indirection", mod.rs:205) and improves (`negated` flag retained
  vs complement-materialization, mod.rs:42).
- **regex-automata meta engine / prefilter ladder** (burntsushi regex-internals; R7 §4): the
  NFA→lazy-DFA→prefilter pipeline the merged fragment feeds; PROVE-10 already retargets the
  memchr tier onto parse-that's DFA.
- **MLIR `unrealized_conversion_cast`**: the *alternative* 5A rejects — no cross-dialect cast is
  needed when the two "dialects" share one enum.
- **rustc HIR / GHC Core**: single-IR-with-analyses-in-side-tables precedent for keeping FIRST/
  nullable/classify as passes over one node rather than a private tree.

## Named risks / counterexamples

1. **Eq/Hash ⊥ Serialize/span** on one authored enum: resolved only by the *derived* span-erased
   Id-view keying the cache; a naive "cache on `IrNode`" is a correctness bug (span defeats hits).
   The two-derived-views claim is the whole ballgame and must be proven, not asserted.
2. **`Look::Start` dead surface**: grammar-embedded regex is seam-anchored, so start-anchors are
   no-ops → carried-but-never-read unless a reader (prefilter disable) is wired. Admit only the
   Look variants a consumer honors.
3. **Unicode `CodepointRange` in the shared enum**: a `char` (scalar) concern rides in the enum;
   byte-expansion is confined to automata (panic guard at mod.rs:82) but the two-world class type
   is inherent — a judgment, not a defect.
4. **Group-elision non-identity**: the `from_expr`≡`from_hir` proof must cover parse-that's
   transparent `Group` (absent in `IrNode`); untested, it is a silent NFA divergence.
5. **Wrong-tree measurement**: skinny's 4-variant `RegexKind` numbers do not transfer; the
   prototype must run in top `crates/ir` + parse-that `bbnf-regex`.

## What a prototype MUST test to move 5A

1. Extend a scratch `IrNode` (node.rs:30) with `Class(CharClass)`,`Look(Look)`; **derive
   `GrammarENode` via `egraph-derive`** over it and prove the `Language` projection stays total
   and the `SaturationCache` keys on the derived span-erased Id-form (the two-views claim).
2. `Nfa::from_expr(&IrNode)` byte-identical to `Nfa::from_hir` (nfa.rs:71) over the parse-that
   regex corpus (44 engine tests + the dfa subtests); explicitly assert Group-elision and
   Optional↔Repeat{0,1} identity.
3. Re-home `classify_regex_from_hir` (classify.rs:177) as an IR pass; report classify ΔLOC and
   which of FIRST/width/nullable fold into the shared fabric.
4. Look-consumer test: `/foo$/` rejects `"foobar"`, `/foo/` accepts; decide `Look::Start`'s fate.
5. Break-the-discard: `->`-project `bootstrap/regex.bbnf` so `parse_generated`
   (generated.rs:2288) returns the fragment (not `bool`); differential-oracle it against the
   hand-parser `Result<Hir>` (regex/mod.rs:34); retire `grammar/misc/regex.bbnf`.
6. Serialize/Eq/Hash derive check on the extended enum + freshly-run find/scan throughput
   *from the merged type* through `Dfa::find`+`Prefilter` (no inherited PROVE-10 band).

## Cross-lock interlocks (shareable)

- The Axis-1 type-plane choice gates whether `Class ⊢ Span` reuses LOCK-1's literal type; every
  `IrNode`/`BackendExpr` edit is LOCK-3-governed; the typed-materialization invariant binds the
  new leaves — a `Class`-derived `ByteClass`/`RegexProgram` must reach the tape emitter
  (`crates/ir/src/lib.rs` `BackendExpr::RegexProgram`).
- ONE-HIR's viability *depends on* the derived-e-node (LOCK-2 territory): if the reconciled tree
  keeps a grammar-tier e-graph, 5A's "one authored type" holds only because `GrammarENode` is
  auto-derived; a hand-authored e-node would reintroduce the duplicate 5A claims to kill.
- The rust/ts face of `Class`/`Look` + automata crossing to TS is a named LOCK-6 gap.
