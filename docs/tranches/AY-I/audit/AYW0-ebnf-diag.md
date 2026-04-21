# AYW0 — ebnf_prettify failure diagnosis

**Status**: halt-with-audit. Defer to a follow-on AY wave (W2 e-graph /
classifier work or a dedicated patch wave).

## Failure mode

`EbnfParser::parse("digit = \"0\" | \"1\" | \"2\" ;")` returns
`Err(ParseErr::Syntax { offset: 0, rule: None })` immediately at
position 0; the same input via the BBNF self-host
(`BbnfEmit::parse(grammar/ebnf/ebnf.bbnf)`) succeeds because `BbnfEmit`
parses **BBNF source describing EBNF**, not **EBNF source itself** —
different grammar entry, different code path, different shape
classifications.

Failing tests at HEAD `6516086f`:

- `crates/core/tests/ebnf_prettify.rs::parse_single_rule`
- `crates/core/tests/ebnf_prettify.rs::parse_multi_rule`
- `crates/core/tests/serialize_roundtrip.rs::ebnf_rule`

`ebnf_prettify::prettify_idempotent` passes because it routes through
`EbnfParser::grammar_prettify().parse(...)` (the combinator-shaped
prettify side channel), not through the tape-first `parse()` entry.

## Root cause

EBNF grammar (`grammar/ebnf/ebnf.bbnf`):

```
terminal = "'" , character - "'" , { character - "'" } , "'"
         | '"' , character - '"' , { character - '"' } , '"' ;
```

The `terminal` rule is classified as `ShapeTag::Keyword` (Alt of
literal-led Seq branches; each branch starts with `'` or `"`). The
keyword emitter (`crates/core/src/backend/rust/emitter/shapes/keyword.rs`)
delegates per-branch Seq emission to `emit_seq_branch_structural_tape`
which calls `emit_branch_position_core`
(`crates/core/src/backend/rust/emitter/shapes/inline.rs:505`).

That helper has a catch-all for `Negate / Minus / Alt / TokenDispatch`
positions:

```rust
// inline.rs:633-639
IrNode::Alt(_, _)
| IrNode::Negate(_)
| IrNode::Minus(_, _)
| IrNode::TokenDispatch { .. } => {
    // Fall through — parent attempt rolls back.
    quote! { return Err(()); }
}
```

The block comment above it claims:

> Today's grammars (JSON / CSS L4 / Sheets / BBNF / EBNF / BNF /
> BbnfBootstrap) route these at rule boundaries.

This claim is **wrong for EBNF**. The `character - '"'` and
`character - "'"` constructs (set-difference / `IrNode::Minus`) sit
**inside** the Seq positions of each `terminal` branch, not at a rule
boundary. The emitter therefore stubs every Minus position with
`return Err(());`, which makes both terminal Seq branches always fail.

Cascade (visible in the cached generated code at
`target/.bbnf-cache/03006ec01b4ca350.rs`):

1. `parse_keyword_EbnfParser_terminal` — both `34u8` (`"`) and `39u8`
   (`'`) arms enter their Seq attempt, push the opening quote literal,
   then immediately hit `return Err(())` from the Minus stub. The
   attempt's outer `if seq_attempt.is_err()` rolls back `*p` to
   `span_lo` and returns `DtaError::Syntax`.
2. `parse_altdispatch_EbnfParser_term` — for first byte `"` or `'`,
   tries `parse_keyword_EbnfParser_terminal` (fails), then falls
   through to `parse_flat_EbnfParser_identifier` (fails because letter
   doesn't accept `"`). Returns `DtaError::Syntax`.
3. `parse_flat_EbnfParser_factor` → `parse_flat_EbnfParser_concatenation`
   → `parse_flat_EbnfParser_alternation` → `parse_flat_EbnfParser_rule`
   each propagate the error.
4. `parse_array_EbnfParser_grammar` (root) — its `loop` saves
   `iter_save_p = *p`, calls `parse_flat_EbnfParser_rule`, sees `Err`,
   resets `*p = iter_save_p` (= 0) and breaks the loop with zero
   iterations matched. Returns `Ok(repeat_off)` (the `*` repeat
   accepts zero iterations).
5. `EbnfParser::parse` runs trailing `skip_space` (no-op since no
   leading ws), then `if pos != input.len()` returns
   `ParseErr::Syntax { offset: 0, rule: None }`.

The offset-0 / rule-none error is the trailing-EOF check firing on the
zero-advance dispatcher return, not a direct emission from the Minus
stub itself. Either way the proximate cause is the Minus stub at
`inline.rs:639`.

## Why bbnf_self_parity::ebnf doesn't catch it

`bbnf_self_parity` parses each `.bbnf` fixture **using BbnfEmit** —
the BBNF grammar's own parser. The BBNF grammar describes EBNF as a
sequence of bbnf rules; it has no `Minus` inside a Keyword-classified
Seq branch. The bug fires only when the **EBNF parser itself** is
asked to parse **EBNF source** — a code path no parity harness
exercises today.

## What a fix requires

The catch-all needs to emit a real per-position body for `Minus` (and
`Negate`, `Alt`, `TokenDispatch`) inside a Keyword Seq branch. For
`Minus` specifically the analogue exists in
`crates/core/src/backend/rust/emitter/shapes/inline.rs:1096`
(`emit_minus_tape` for the rule-level Minus position). Lifting that
into the per-position context requires:

1. Rewriting the guard-attempt + primary emission to use `Err(())` /
   `Ok(())` returns instead of `DtaError::Syntax` propagation, so it
   composes with the surrounding attempt closure.
2. Threading `emit_branch_position_core` recursion into the primary
   path (the primary side of `character - '"'` is `IrNode::Ref(character)`,
   which already has a per-position arm at `inline.rs:534`, so the
   recursion is mostly a `match` arm + delegating call).
3. The same treatment for the visitor-path
   (`emit_inline_position_visitor` / `emit_branch_position_core`'s
   visitor sibling — though no visitor sibling exists for
   `emit_branch_position_core` today; the keyword-Seq visitor path is a
   separate concern).
4. Repeating for `Alt` (delegate to `emit_alt_byte_dispatch_tape` /
   linear), `Negate` (lift the rule-level `emit_negate_tape`), and
   `TokenDispatch` (lift `emit_token_dispatch_tape`).

Files affected:

- `crates/core/src/backend/rust/emitter/shapes/inline.rs:633-640`
  (catch-all replacement; ~80-150 LOC of new emission logic).
- Probable touch in
  `crates/core/src/backend/rust/emitter/shapes/keyword.rs` if the
  per-branch attempt closure's success/failure semantics need to
  thread through Minus's interior records (the keyword branch
  collapses inner records to one Span leaf via
  `builder.columns_mut().truncate(seq_save_cols)`, so Minus's record
  emission is fine to discard — but the `*p` advancement of the
  primary side must be preserved).
- A wire-contract test loading `grammar/ebnf/ebnf.bbnf` and asserting
  `EbnfParser::parse("digit = \"0\" ;")` succeeds, plus a parity test
  asserting the typed AST matches what BBNF self-host yields when
  given the same bytes (separate grammars, separate ASTs — but the
  parse should at least succeed).

This is **not** a mechanical fix in the W0 sense (no grammar tweak,
no `#[ignore]`-only patch). It is a small but real codegen extension
inside the shape-emitter pipeline. Per W0's "halt + diag" path the
correct landing is a follow-on wave.

## Recommended landing

**AY.W2** — the Named-preservation + e-graph G1-G9 + wrap-compound
elision wave already touches the IR + classifier surface. The Minus-in-
Keyword-Seq fix sits naturally adjacent: either fold into a W2
sub-item (codegen catch-all completion, ~150 LOC new emission) or
spawn a focused W2.x sub-wave with bound `crates/core/src/backend/rust/
emitter/shapes/inline.rs` + a new wire-contract test. The
`emit_branch_position_core` rewrite has zero overlap with W2's other
file bounds (`crates/ir/src/egraph/`, `crates/ir/src/passes/metadata.rs`,
`crates/core/src/backend/rust/emitter/shapes/wrap.rs`); a parallel
sub-agent works fine.

**Alternative**: if AY.W2 is already at-capacity, defer to AY.W7
"FINAL" wave's clean-up sweep alongside the cssparser parity work.
The current `#[ignore]` annotations carry the deferral cleanly until
then.

## Ignored tests + verification

Tests carrying `#[ignore = "AY.W0.2 deferred — see audit/AYW0-ebnf-diag.md"]`:

- `crates/core/tests/ebnf_prettify.rs::parse_single_rule`
- `crates/core/tests/ebnf_prettify.rs::parse_multi_rule`
- `crates/core/tests/serialize_roundtrip.rs::ebnf_rule`

Verification command (post-ignore):

```
cargo test -p bbnf --test ebnf_prettify --profile ax-iter
cargo test -p bbnf --test serialize_roundtrip ebnf_rule --profile ax-iter
```

Both should report the parse_* tests as `ignored` and exit zero. The
`prettify_idempotent` + non-ebnf serialize_roundtrip tests continue
to pass.
