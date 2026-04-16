# W4 Scope Reveal — DTA Self-Host Round-Trip

## Summary

AW-I.W4ζ set out to close Wave 4 by recovering a working `generated.rs`
on master and verifying the DTA self-host round-trip. The one-shot
regen (W4ζ step 1) produced a 21198-line DTA-based `generated.rs`
under the post-W4 emitter/walker/lifter; compiling against it let
the library check `cargo check -p bbnf --lib` succeed. The second
bootstrap against the new library, however, collapsed to a 290-line
stub — the smoking gun that the DTA driver misparses `bbnf.bbnf` even
with the regenerated table.

Diagnosis established a two-layer defect:

1. **Tape-level rule identity** — the walker's driver stamped
   `variant_idx = 0` on every rule-entry compound (and leaf rule),
   so the view layer's `rule_kind()` decoded every record as the
   first-indexed rule (`int_lit`). **Fixed** in this wave (commits
   below).
2. **Lowering pipeline** — multi-file structural assumptions across
   the expression lowering that the fn-per-rule tape shape has
   direct-child rule_kind matches. DTA wraps each grammar construct
   in an extra Seq compound, nesting semantic children one or two
   levels deeper. **Scope-reveal**: closing this requires a
   systematic migration of `lower/expression.rs`, `lower/tape_walk.rs`
   consumers, plus every call site that uses `find_child_by_kind` /
   direct-child iteration for semantic dispatch.

The architectural gains from W4α–W4ε stay on master. The audit
enumerates the remaining surface for a successor tranche — **AZ**.

## What W4ζ Landed

### The walker fix (landed)

Commit `fix(bbnf-tape): stamp rule-entry variant_idx via pending_variant_idx`
adds two fields to the DTA driver:

- `Frame::variant_idx: u8` — per-frame captured stamp, `u8::MAX` sentinel
  means "no rule context".
- `FrameStack::pending_variant_idx: u8` — driver-local pending stamp.

`DtaState::Ref { rule, .. }` writes `rule.0 as u8` into
`pending_variant_idx`; the next compound frame push (Seq / AltLinear /
Repeat / ShuntingYard) consumes it into `frame.variant_idx`, then
clears the pending slot. Leaf states (Literal / Regex) consume the
stamp inside `emit_leaf` and clear. `close_compound` stamps the low-6
`flags` bits from `frame.variant_idx` first, falling through to the
existing Alt-cursor branch-index stamping only when no rule context
was captured — anonymous Alts still get their sub-variant discriminant.

Backtracking paths preserve correctness: AltLinear snapshots
`pending_variant_idx` at its post-push savepoint and restores it on
branch failure; `handle_repeat_failure` (both variants) clears it at
iter-savepoint restore so a failed iteration body's Refs don't leak
into the next sibling's compound push. `WsTrim` is the lone
emit-skipping arm that preserves pending so `rule = ?w rule_body`
tags correctly.

### The directive-decoder fix (landed)

Commit `fix(grammar/host): descendant-based decoders for DTA structural
nesting` migrates the host-level directive decoders to descendant-based
lookups:

- `decode_recover`, `decode_pretty`, `decode_single_name`, `decode_ws`,
  `decode_host` — now use `find_descendant_by_kind` / recursive walks
  to see through DTA's Seq wrappers.
- `absorb_item`'s rule handling picks the `rhs` descendant directly
  (the "last non-identifier child" heuristic lands on the terminator
  `;|.` Alt under DTA).
- `collect_pretty_hint_descendants` — new helper mirroring
  `collect_identifier_descendants`.

Verified: `bbnf::grammar::parse` extracts 31 rules, 2 imports (with
correct `items` lists), 3 pretties (with correct `rule_name` + `hints`).

### The lowering surface fix (landed, partial)

Commit `fix(lower/expression): extend wrapper detection for DTA tape shapes`:

- `dispatch_expression` wrapper-sentinel fallback now admits
  `TapeKind::Seq | TapeKind::Alt` alongside the existing Rule / Repeat.
- `lower_leaf_by_span_text` accepts Seq / Alt kinds (the
  `is_single_token_span` caller gate guarantees valid leaves).
- `collect_binary_operands` flattens both `TapeKind::Repeat`
  (fn-per-rule) and `TapeKind::Rule` (DTA's Repeat-as-Rule) trailing
  wrappers.

These unblock the outer expression shape but leave the remainder of
the lowering pipeline un-migrated.

### The regen (landed)

- `chore(generated): transient entry field patch for pre-W3 regen` —
  restore `08658746^` generated.rs + patch DtaTable literal's `entry`
  field.
- `chore(generated): one-shot regen under post-W4 emitter/walker/lifter`
  — 21198-line DTA-based generated.rs.
- `chore(bootstrap/debug_parse): dump imports and pretties` — harness
  augmentation for diagnostics.

Five commits total under the W4ζ sub-phase. The tape produced by
`BbnfBootstrap::parse` now correctly encodes rule identity; the
directive extractors correctly decode it; the outer expression
lowering no longer trips on a single-operand binary_factor or a
single-token Seq-wrapped regex leaf.

## What Remains Un-migrated

The proc-macro panic taxonomy under `cargo check --workspace`:

### `lower_term (grouped): missing inner expression in span "(...)"`

Fires on every grouped term `(...)`, `[...]`, `{...}`, `@{...}` whose
body is a compound alternation. `lower_grouped_term` walks `node.children()`
to find the inner expression; under DTA the inner expression is wrapped
in a Seq compound that sits alongside the `(` / `)` literal leaves the
walker now emits (my leaf fix made these Literal emissions visible).
Fix: either strip the open/close delimiter leaves explicitly, or
descend through the Seq wrapper via `find_descendant_by_kind` against
the expression-layer rule kinds.

### `binary_factor could not resolve operator — empty gap`

Fires on multi-operand binary_factor chains where the inline operator
child (`binary_operators` rule_kind=33) isn't caught because the Alt
compound for `<< | >> | -` has the sub-variant branch_idx in its
`variant_idx` (per my walker fix's fall-through), not the binary_operators
rule id. The decoder's `child.rule_kind() == binary_operators` test
fails, falls through to `recover_binary_op` which sees an empty source
gap (the operator was consumed but its span wrapped in an Alt compound).
Fix: either add a rule-entry Ref on the AltLinear itself so the Alt
compound tags as `binary_operators`, or recurse one level into the
Alt to find the binary_operators span.

### `lower_term: unknown leading byte ';' (span = ";")`

Fires when lower_term is handed the rule terminator `;` Alt compound
directly. This happens for rules whose rhs body is a single
`identifier , "=" ?w , rhs ?w , ( ";" | "." )` where the grammar-level
`rule` absorber walks past the rhs into the terminator. My
`absorb_item` fix handles this in the `rule` branch; other call
sites (directive sub-rules like `token_directive`, `debug_directive`)
may still walk into the terminator.

### The broader pattern

Every consumer that does `view.children().find(|c| c.rule_kind() == X)`
needs evaluation. DTA nests semantic children one Seq compound deeper;
the fix is uniformly `find_descendant_by_kind(view, X)`. The
call-site audit:

- `crates/core/src/lower/expression.rs` — `find_child_by_kind` for
  `term`, `modifier`, `binary_operators`, and numerous call-arg /
  mapped-factor decoders.
- `crates/core/src/lower/tape_walk.rs` — the underlying helper; may
  itself need a `find_descendant_by_kind` companion.
- `crates/core/src/lower/value_expr.rs` — value_expr lowering for
  the `->` map expressions; similar rule_kind direct-child assumptions.
- `crates/core/src/graph/*` — dep/metadata analyzers that walk
  `rule_kind` for cross-references.
- `crates/core/src/types.rs` — type-annotation decoding.
- `crates/core/src/backend/rust/analysis/inline.rs` — inline-heuristic
  decoders (per-line comments suggest this already has some DTA
  awareness but may need completion).

## The Tranche AZ Seed

**AZ — DTA self-host round-trip**. Goal: the lib's DTA parse path
produces a grammar `bbnf::grammar::parse` can drive through the
lowering pipeline to a buildable grammar IR. The successor tranche
runs end-to-end on `cargo expand -p bbnf-bootstrap --lib`.

Suggested sub-phases (five waves, one sub-phase each wave):

- **AZ-W1 — lower/expression grouped terms**. Migrate
  `lower_grouped_term` + its interior-expression picker to
  descendant-based traversal. Delete the literal delimiter leaves
  from the grouped inner child search. Tests: every `( rhs )`, `[ rhs ]`,
  `{ rhs }`, `@{ rhs }` shape across bbnf, expressions, types sub-grammars.

- **AZ-W2 — binary_factor operator recognition**. Recurse into Alt
  compounds when scanning for `binary_operators` children, or stamp
  `binary_operators` rule-entry onto the Alt via a companion Ref
  (if the lifter can be taught to wrap the AltLinear in a Ref).
  Tests: every `<<` / `>>` / `-` chain, plus value_expr's
  `add_op`, `mul_op`, `cmp_op`, `&&`, `||` chains.

- **AZ-W3 — `find_child_by_kind` audit**. Catalog every call site in
  `crates/core/src/lower/**`, `crates/core/src/graph/**`,
  `crates/core/src/types.rs`; convert direct-child scans to
  descendant walks where the target rule_kind is a nested-rule output.
  Keep direct-child scans only for leaf-immediate children (identifier,
  literal, regex).

- **AZ-W4 — value_expr lowering**. Migrate the `->` map-expression
  lowering in `crates/core/src/lower/value_expr.rs` to the DTA tape
  shape. Tests: every `int_lit = /regex/ -> i64`-style rule across
  expressions.bbnf and types.bbnf.

- **AZ-W5 — round-trip verification**. Clear caches, run bootstrap
  twice, verify idempotency (same line count + content). Run
  `cargo test --workspace --no-fail-fast`; close any snapshot-delta
  residuals. Commit a fresh regen under the fully-migrated lowering
  pipeline.

Budget: ~4-5 waves of ~1k-line changes each, predominantly in
`crates/core/src/lower/**`. No further changes to bbnf-tape's DTA
driver should be needed — the walker fix is complete.

## Hard-Gate Status (W4ζ)

- **Gate 8** (fn-per-rule epilogue count): n/a under the committed
  21198-line DTA regen (no `__<rule>` fns remain).
- **Gate 9** (line count): 21198 — within the 15k-21k realistic
  envelope from `w4-close.md`.
- **Gate 10** (`parse_dta`/DtaDfaScanner fns): clean; the DTA is a
  const + impl.
- **Gate 11** (emitter dir layout): unchanged from W3 close;
  `grammar.rs`, `dta.rs`, `mod.rs`, `profile.rs`, `visitor.rs`,
  `prettify/` all present.
- **Gate 12** (CSS L4 state_count < 2000): NOT VERIFIED — requires
  the full lowering pipeline to succeed first. Deferred to AZ-W5.
- **Gate 13** (workspace 0 failures): NOT MET — proc-macro panics
  in dev-dependent grammars (JsonParser, CssParser, etc.) blocked
  by AZ-W1 through AZ-W4.

## Why the Gains Stay

The W4 deletions (fn-per-rule emitter, sibling modules, 11 backends)
and the DTA walker additions are load-bearing architectural
improvements. Reverting them would lose:

- DTA-only emitter path (W3);
- The 11-emitter-module collapse (W4α);
- Cyclic fuse/inline activation (W4α);
- AltLinear savepoint + Repeat lo..=hi walker arms (W4β);
- ShuntingYard + Ref resolution + counter slot release (W4γ/W4δ);
- PSI refresh + pre-order finalise gating (W4δ);
- Lifter's `DtaState::WsTrim` + invisible-structural peeling (W4ε);
- The walker's rule-identity stamping (W4ζ — this commit set).

All of these are prerequisites to AZ-W1 through AZ-W5. The
lowering pipeline's fn-per-rule tape-shape assumption is the
missing piece, and it's orthogonal — it can be migrated
incrementally without touching the walker.

## Commits (W4ζ)

| Hash        | Subject                                                                        |
| ----------- | ------------------------------------------------------------------------------ |
| 87f65214    | chore(generated): transient entry field patch for pre-W3 regen (AW-I.W4ζ)      |
| 940ef3b9    | chore(generated): one-shot regen under post-W4 emitter/walker/lifter (AW-I.W4ζ)|
| 3392af05    | fix(bbnf-tape): stamp rule-entry variant_idx via pending_variant_idx (AW-I.W4ζ)|
| 56d43771    | fix(grammar/host): descendant-based decoders for DTA structural nesting        |
| 5658b763    | fix(lower/expression): extend wrapper detection for DTA tape shapes            |
| 3ec93d21    | chore(bootstrap/debug_parse): dump imports and pretties (AW-I.W4ζ)             |
