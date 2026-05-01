# AZ-IV.W0.3 Regen Totality — HALT and Triumvirate Trigger

**Agent**: AZ-IV.W0.3 Regen Totality
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen`
**Trigger**: regen-drift root-cause spans more than `xtask` + strategy registry + one lowering / emitter surface (per `docs/tranches/AZ-IV/waves/W0.md` §Triumvirate Dispatch).

## Summary

`cargo xtask regen --check` against the W0 base commit (`2678ed44`)
reports 7 of 9 manifest grammars drifted (`bbnf`, `json`, `css_l4`,
`css_pretty`, `google_sheets`, `ebnf`, `bnf`); `csv` and `math` are
already regen-equivalent.

Running `cargo xtask regen` (no `--check`) successfully writes new
output for all 9 grammars on the **first** invocation. The new
`bbnf.rs` then breaks the bootstrap parser:

```
Parsing grammar/bbnf/bbnf.bbnf (3448 bytes)
BbnfBootstrap::parse Err: Syntax { offset: 36, rule: None }
```

A second `cargo xtask regen` invocation now fails because xtask
recompiles `bbnf` core against the freshly-written `bbnf.rs`, and the
new self-host parser cannot round-trip its own grammar source. The
regen is therefore not idempotent and `regen --check` cannot pass
live — closing the W0.3 hard gate is impossible without a deeper
fix.

## Two Independent Regressions In The Regen Output

### R1 — HRegex int / float typed-leaf collapse (lower / emitter surface)

The diff against `crates/core/src/grammar/generated/bbnf.rs` shows
the HRegex emitter now emits `push_leaf_with_str` for the `int` and
`float` rules instead of `push_leaf_with_i64` / `push_leaf_with_f64`
(see lines 2291-2329 of the diff snippet). The emitter dispatch in
`crates/core/src/backend/rust/emitter/shapes/hregex.rs` is
descriptor-driven and does the right thing when the descriptor's
`return_type` is `Some(TypeDesc::I64)` or `Some(TypeDesc::F64)`.

The break is upstream: the descriptor for these rules is no longer
`FnDescriptor::Expr { expr: MapExpr::Input, return_type:
Some(TypeDesc::{I64, F64}) }` post-flow through
`crates/core/src/lower/expression/wrap.rs:380-454`. One of those
projections regressed in a commit between `248d3ac6` (last regen)
and the current HEAD, causing the typed-leaf push to degrade to the
fallback `push_leaf_with_str`.

**Owner surface:** `crates/core/src/lower/expression/wrap.rs` (one
emitter / lower file beyond the strategy registry — within W0.3's
single-surface budget if R2 were absent).

### R2 — BBNF self-host parser cannot parse `grammar/bbnf/bbnf.bbnf`

The freshly-emitted `BbnfBootstrap::parse` halts at offset 36 of the
grammar source — past the leading `// BBNF — Better Backus-Naur
Form` comment, around the `// Self-hosted grammar definition.` line
or the `@import` directive that follows. The pre-flip parser
accepted the same source byte-for-byte (commit `26d76206` annotates
it with Span / i64 / f64 markers; the regen pipeline through commit
`248d3ac6` produced a working self-host parser).

Possible owner surfaces:
- BBNF comment / whitespace shape emission (`shapes/wrap`,
  `shapes/flat`, or comment-rule projection)
- `@import` directive structural emission
- `ee3e6c28 fix(lower/factor): recover modifier from source gap`
- `954d166b feat(grammar/bbnf-self-host): replace bootstrap_parser
  with canonical generated path`
- `2ec275bb fix(lower/term): structural dispatch in lower_term to
  consume codegen Term compound`

This is a **second** lowering / emitter surface. With R1, the regen
drift root-cause spans **at least two** lowering / emitter surfaces
plus the strategy registry — exactly the W0.md §Triumvirate Dispatch
trigger.

## Work Landed In This Worktree (Within W0.3 Bounds)

1. Audit artefact `docs/tranches/AZ-IV/audit/W0-regen.txt` — pre-W0.3
   `cargo xtask regen --check` baseline showing 7-of-9 grammars
   drifted.
2. Walker-tape emitter doc-comment scrubs at
   `crates/core/src/backend/rust/emitter/shapes/object.rs:120` and
   `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:315`,
   plus the test fixture
   `crates/core/tests/fixtures/shape_dispatch_emission/object.rs.expected`.
   The substantive sense of the comment is preserved; only the
   "Walker-tape compound emission is replaced by typed" framing is
   excised (per W0.3 sub-gate post-regen
   `rg -n 'Walker-tape|__dta_walker_inline' crates/core/src/grammar/generated/`
   returning zero hits).
3. Manifest-driven strategy resolver scaffold —
   `crates/ir/src/registry/strategy.rs::for_grammar_with_manifest`
   plus the `ManifestStrategyEntry` row shape; both re-exported from
   `crates/ir/src/registry/mod.rs`. The literal arm-list at L143-262
   is preserved unchanged for fall-through.
4. Synthetic grammar binding test scaffold —
   `crates/core/tests/synthetic_grammar_strategy.rs`. Four tests
   prove totality of the manifest path (synthetic ident resolves
   without a source arm, manifest row wins over source arm, empty
   manifest falls through, empty registry panics).

The walker-tape generated-file scrub did **not** land — running the
post-edit regen hits R1 + R2, so the generated tree cannot be
republished cleanly without resolving the deeper regression.

## Triumvirate Ask

1. Research agent: locate the commits that caused R1 (HRegex
   descriptor regression) and R2 (BBNF self-host parser break) by
   bisecting `248d3ac6..HEAD` against `cargo xtask regen` followed
   by `cargo run -p bbnf-bootstrap --bin debug_parse --
   grammar/bbnf/bbnf.bbnf`.
2. Plan agent: scope the redress against the resulting commit list;
   if more than one lowering / emitter surface needs to change to
   restore parity, raise the W0.3 file-bounds amendment.
3. Redress agent: land the fix(es), regen all 9 grammars, verify
   `cargo xtask regen --check` is green live, and complete the
   W0.3 hard gates (Walker-tape generated scrub + this file's
   replacement with the post-fix evidence).

## Files Modified In This Halt-Commit

- `crates/core/src/backend/rust/emitter/shapes/object.rs` —
  Walker-tape doc-comment scrub.
- `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`
  — Walker-tape doc-comment scrub.
- `crates/core/tests/fixtures/shape_dispatch_emission/object.rs.expected`
  — fixture mirror of the emitter comment scrub.
- `crates/ir/src/registry/strategy.rs` — manifest-driven resolver
  scaffold (`for_grammar_with_manifest` + `ManifestStrategyEntry`).
- `crates/ir/src/registry/mod.rs` — re-export
  `ManifestStrategyEntry`.
- `crates/core/tests/synthetic_grammar_strategy.rs` (new) —
  synthetic grammar binding totality test scaffold.
- `docs/tranches/AZ-IV/audit/W0-regen.txt` — pre-W0.3 regen baseline.
- `docs/tranches/AZ-IV/audit/W0-regen-HALT.md` (this file) — halt /
  triumvirate trigger evidence.
