# Tranche AE — Complete the Tape-First Migration

## Context

Tranche AC.2 transposed the Rust backend to tape-first parsing —
emitter, view layer, runtime, schema directive accessors. Every
generated rule returns `Option<TapeOffset>`, every parse result is an
owning `Parsed<R>`, every view walks a `TapeCursor`. The substrate is
real and load-bearing for every production grammar.

But the lowering pipeline that bridges CST tape views back into
`GrammarIR` was only API-migrated. It uses `view.rule_kind()`,
`view.child(n)`, `view.children()` — tape-first accessors, all of
them — yet its semantics still assume the pre-AC.2 flat shape, where
the optimizer had pre-inlined transparent wrapper rules and flattened
the bodies of `+`/`*` Repeat compounds. Under the structural
bootstrap path — the path the bootstrap parser itself takes, where
preservation of every rule's identity is mandatory and the optimizer
is gated out — that semantic gap collapsed every rule body to
`IrNode::Epsilon`. The lowering walked, the lowering succeeded, the
lowering produced empty IR. No panic, no diagnostic, just an
optimizer-shaped hole where the grammar used to be.

The bug is structural, not local. `lower/expression.rs` and
`lower/value_expr.rs` were written when the tape's first
characteristic — that `Seq` and `Alt` push nothing, that only
`Repeat`, `Optional`, and `Rule` reach the tape — was hidden behind
the optimizer's wrapper-elision pass. Once the optimizer was gated
out, every layer of the lowering pipeline began seeing wrappers it
had never been taught to peel: an explicit `TapeKind::Repeat`
compound around `(content, optional_pipe)+` patterns; a single-child
`directive` rule wrapping a single-child `pretty_directive` rule
wrapping a single-child `pretty_directive_compound` rule; preserved
`Optional` records sitting where the pre-AC.2 lowering used to read
positional children directly. Every layer's positional reads
(`child(1)`, `child(2)`) shifted by one or two depending on which
wrappers happened to survive. The shifted reads landed on the wrong
nodes. The wrong nodes failed to match. The catch-all returned
`Epsilon`.

Two collateral cracks fell out of the same incompleteness. The
schema emitter shipped two directive-accessor APIs — the direct
`try_as_*_directive(cursor, input)` helpers in `cst_directives` and
the walking `as_*_directive()` methods on each rule view — that
disagreed about who owned the wrapper-peeling. Callers in `host.rs`
had standardized on the direct surface, but the walking surface
stayed live, kept compiling, and silently failed under structural
mode wherever it was reached. And the driver's rule-emission skip
predicate conflated call strategy with rule identity: under
`preserve_identity`, where every rule must emit a standalone
function regardless of inline-planning preferences, the predicate
hollowed out the bootstrap's rule-function set whenever the CSP
strategy solver decided some rule was a candidate for inline fusion.

AE completes the tape-first migration. It rewrites the lowering
pipeline as a shape-agnostic tape walker that produces correct IR
from any well-formed tape, regardless of which wrappers the
optimizer has elided. It deletes the walking directive accessor
surface and the four dead schema-emit stub modules left over from
AC.2. It teaches the driver that `preserve_identity` rules always
emit standalone functions. And it discards the proc-macro pipeline's
last hand-patch on `generated.rs` by running a fresh end-to-end
regen as the final substrate gate.

## The shape-agnostic lowering

The discipline is straightforward and every layer enforces it:

**Dispatch by `rule_kind`, never by child index.** Optional
wrappers shift positions; `rule_kind` is the only stable reference.
A lowering layer that reads `child(1)` is wrong on the day a
`?w` appears in front of its target. The replacement walks
`view.children()` and dispatches each child on its own
`rule_kind()`, accumulating the semantically meaningful pieces
along the way.

**Single private flattening primitive.** A new helper
`iter_rep_children` peels a top-level `TapeKind::Repeat` compound
when present and returns its iterator; otherwise it returns
`view.children()` unchanged. This is the canonical Repeat unwrap
that AE.0's tape-shapes reference identifies as the universal
pattern, and it mirrors the loop already live in
`host.rs::extract_grammar`'s top-level walker. Every list-shaped
construct in the lowering — top-level grammar items, alternation
branches, concatenation factors — pulls its children through this
single primitive. There is no second peeling helper; there is no
inline `match view.children().next()` test sitting in some other
file.

**The catch-all is a panic.** The bbnf.bbnf grammar is closed:
every reachable `rule_kind` has an explicit handler. A panic
surfaces the bug at the regen site rather than corrupting
downstream rule bodies invisibly. The pre-AE catch-all returned
`IrNode::Epsilon`, which is what made the AC.2-era lowering bug
silent for as long as it was. AE inverts the failure mode —
unknown `rule_kind` is a programmer error in `lower/`, not a
runtime fall-through, and the panic includes the offending kind
in its message so the regen run identifies the missing handler
on the first failure.

**Layer functions have strict preconditions.** Each `lower_*`
function asserts the `rule_kind` it expects via a paired
`*_dispatch` sibling that fans out to the right handler.
Falling through to a generic fallback is the original AC.2
lowering bug pattern, deliberately removed. A function written
to lower a `concatenation` body never sees an `alternation`
view, and the dispatch surface guarantees that statically.

The canonical example is `alternation = ( concatenation ?w , "|" ? ) +`.
Under structural mode the trailing `+` quantifier emits an explicit
`TapeKind::Repeat` compound containing one child per iteration's
side-effect chain. Each iteration's chain consists of a
`concatenation` rule compound followed by an `Optional` compound
wrapping the `"|"` literal — the `?w` itself pushes nothing, since
`OptionalWhitespace` is a side-effect-only modifier. The
lowering iterates the unwrapped `Repeat` as `(content,
optional_pipe)` pairs, dispatches `content` recursively, and
discards the `optional_pipe` wrapper since the alternation
operator has no semantic payload at the IR level. The same
shape recurs for `concatenation = ( bf , "," ? ) +` — the
trailing comma is structural cosmetic, and the lowering pulls
its `(content, optional_comma)` pairs through the same iterator
discipline.

The wrapper-tolerance falls out as a consequence. A single-child
`directive` rule wrapping a single-child `pretty_directive` rule
becomes a transparent descent through `child(0)` until the layer
function reaches a `rule_kind` it knows. A
`mapped_factor = factor , ( "->" , ( value_expr , type? ) )?`
construction becomes a `factor` lowering followed by a span-
emptiness check on the trailing `Optional` — if the span is
non-empty, the inner `(value_expr, type?)` Seq is unwrapped via
the same `iter_rep_children` primitive and lowered into a `Map`
IR node; otherwise the bare `factor` IR is returned. No layer
function reads `child(2)` to find out whether the optional was
present.

The lowering as a result is small enough to fit in two files
(`expression.rs` for the structural lowering and `value_expr.rs`
for the typed-value lowering), with a private `tape_walk.rs`
sibling holding the four shape primitives — `iter_rep_children`,
`find_child_by_kind`, `peel_transparent`, `iter_pairs_rep` — that
every layer composes against. The four-primitive substrate is
the entire AE.1 contract; everything in `expression.rs` and
`value_expr.rs` is layer-specific dispatch that reads from it.

## The directive API consolidation

Schema directives now have one API. The walking
`as_*_directive()` accessors emitted on each rule view were a
design mismatch from the moment AC.2 landed: they assumed callers
had not peeled wrappers, but every caller — `host.rs::absorb_item`
chief among them — had already peeled. The direct
`cst_directives::try_as_*_directive(cursor, input)` helper is what
`host.rs` uses, what every `absorb_item` clause dispatches
through, and what the AE lowering rewrite settles on. The walking
surface is deleted in its entirety, along with the four dead
emit stub modules
(`directive_walker.rs`, `directive_view.rs`, `directive_match.rs`,
`directive_helpers.rs`) that AC.2 left in `grammar/schema/emit/rust/`
when their consumers migrated to the direct API but the modules
themselves stayed orphaned. The schema emitter is one surface
fewer.

The direct accessor signature is universal: `try_as_*_directive`
takes a `TapeCursor` and the source `&str`, peels any wrapper
the structural mode might have preserved, returns
`Option<<Directive>Args>` with the typed argument fields filled
from the underlying spans. The wrapper-peeling lives inside the
helper, not in every caller. This is the same design discipline
as AE.1's `iter_rep_children` — one place owns the unwrap, one
place that every consumer goes through.

## Identity-aware rule emission

Under `preserve_identity` — set by `#[parser(structural)]` and
the bootstrap path's default — every rule emits a standalone
function regardless of what the inline-planning CSP would prefer.
The driver's skip predicate now respects this. The pre-AE
predicate consulted `CallStrategy` and skipped any rule the
strategy solver had marked as inline-only; under structural mode
that hollowed out the bootstrap's rule-function set whenever the
CSP picked inline fusion for a hot rule. The replacement reads
`rule.preserve_identity` first and short-circuits the skip whenever
it is set. `CallStrategy` retains its meaning at non-rule call
sites; only the rule-function emission gate is identity-aware.

The previous workaround that forced all-`DirectCall` strategies
under structural mode is reverted in the same change. The CSP
strategy solver runs as it always did; the structural mode no
longer needs to lobotomize it, because the emission gate is doing
the right thing one layer down. Strategy and identity are now
orthogonal concerns: strategy decides how a rule is called,
identity decides whether the function exists.

## The clean regen

`generated.rs` is now reproducible from `scripts/bootstrap-bbnf.sh`.
The HEAD-era hand-patches that crept in during AC.2 debugging are
gone; the file is the deterministic output of running the current
pipeline against `grammar/bbnf/bbnf.bbnf` end-to-end. The
bootstrap script regenerates the same file the manual edits had
been patching, and the round-trip stabilizes on its own.

A round-trip test (`crates/core/tests/grammar_roundtrip.rs`)
parses every production grammar — `bbnf.bbnf`, `json.bbnf`,
`css_l4.bbnf`, `google_sheets.bbnf`, `ebnf.bbnf` — through
`bbnf::grammar::parse` and asserts the resulting `GrammarIR` rule
counts match a frozen snapshot. The snapshot is committed
alongside the test. Any future drift in the lowering pipeline,
the schema emitter, the directive API, or the bootstrap regen
trips the test on the first commit that introduces it. This is
a permanent regression gate against the kind of silent
shape-mismatch failure AE was built to fix in the first place.

## Architectural commitments

1. **Shape-agnostic lowering.** Every layer in `lower/` walks the
   tape CST faithfully without depending on optimizer wrapper
   inlining. The lowering produces correct IR whether the optimizer
   pre-elided every transparent wrapper or preserved every one of
   them. The four shape primitives in `tape_walk.rs` are the entire
   wrapper-tolerance surface; layer functions compose them.

2. **One directive API.** Schema emits only
   `cst_directives::try_as_*_directive(cursor, input)`. The walking
   `as_*_directive()` accessors and the four orphan emit stub
   modules are deleted. There is one directive accessor surface,
   and `host.rs` is its only consumer.

3. **Rule emission is identity-aware.** `preserve_identity` rules
   always emit standalone functions, regardless of `CallStrategy`.
   The driver's skip predicate consults identity first, strategy
   second. Structural mode no longer needs to clamp the strategy
   solver to all-`DirectCall`.

4. **Clean regen.** `crates/core/src/grammar/generated.rs` is
   always the output of `scripts/bootstrap-bbnf.sh` against
   `grammar/bbnf/bbnf.bbnf`. Hand-patches are forbidden. The
   round-trip test gate makes drift impossible to land silently.

5. **Panic, never Epsilon.** Unknown `rule_kind` during lowering is
   a programmer error and surfaces as a panic at the failing site,
   not as a silent fall-through to `IrNode::Epsilon`. The bbnf.bbnf
   grammar is closed; the panic is the contract that says so.

6. **No legacy code.** Every workaround introduced during AE
   debugging — the structural-mode strategy clamp, the directive
   walker stubs, the manual `generated.rs` patches, the dead
   schema-emit modules — is removed by the end of AE. The final
   tree contains only the new substrate, never the scaffolding it
   replaced.

## What this tranche does NOT do

This is a substrate completion. The IR, the optimizer passes, the
e-graph, the CSP strategy solver, the cost model — all unchanged.
AE fixes the layers that straddle the tape boundary; everything
above and below those layers is left as-is.

The aggressive optimization work — universal cost model, cross-rule
CSP decomposition via the dormant Y.5 `UnionFind` substrate, three-tier
emission with direct-to-struct projection for leaf rules with typed
`->` bodies, lazy view-layer scalar extraction — is captured in
`docs/tranches/AF-prototype.md` and deferred until the AE substrate
stabilizes. AF is gated on AE: nothing in AF executes until the
shape-agnostic lowering, the directive API consolidation, the
identity-aware emission gate, and the clean regen are all live on
master and the round-trip gate is green.

The three-tier emission lattice that AF proposes — Tier A (tape only),
Tier B (direct-to-struct projection), Tier C (lazy typed AST at the
view layer) — would have been impossible to build on the AC.2
substrate, because the lowering pipeline was producing empty IR for
the bootstrap path and the cost model had no way to read morphology
facts from a hollowed-out grammar. AE makes those facts trustworthy
again. AF is the consumer.

## The key insight

The pre-AE lowering was tape-first in form but pre-tape in semantics.
Every accessor it called returned the right type; every
`view.children()` produced the right cursor; every `view.rule_kind()`
returned the right kind. What was wrong was the assumption that the
tape it was walking had been pre-flattened by an optimizer pass that
no longer ran on the bootstrap path. The bug was nowhere in the
accessor surface; the bug was in every line that read `child(1)`
expecting the optimizer to have already removed `child(0)`.

AE rewrites the lowering against the unflattened tape — the tape as
the emitter actually produces it under structural mode, with every
preserved wrapper, every Repeat compound, every Optional record sitting
where the grammar put it. The result is a lowering pipeline that no
longer needs the optimizer to be running upstream. Any well-formed
tape produces correct IR. The four shape primitives in `tape_walk.rs`
are the only place that knows what wrappers exist; everywhere else in
`lower/` is stable against future wrapper additions, future
optimizer-layer toggles, future schema-emit changes. The substrate is
finally what AC.2 promised it would be: shape-agnostic from the tape
boundary upward.
