# Tranche AW-I — Activation

AW-I ships one parsing path. `parse()` dispatches through
`dta_run`; every `fn __<rule>` helper retires. The walker's
`AltLinear`, `Repeat`, `ShuntingYard` arms implement full
semantics — savepoint backtracking, `lo..=hi` iteration,
operator-precedence reduction over the emitted
`DtaPrecedenceEntry` table. `MemoStore` deletes.
`inline_acyclic` + `fuse_single_use` fire with SCC metadata
recomputed between passes; the snapshot tests that fossilised
un-fused shape as-correctness migrate.

The AV-era cleanup — Stage-C gating, `compute_sibling_skip`
deletion, Span-rule emitter elision, colour-function
`LargeAggregate` consumer, inline-test migration, bootstrap CI
gate, white-colour WideScalar routing — lands alongside.
`cursor.child(0)` is O(1) `idx + 1` under pre-order.
`dta-replay` feature-gates the decision log + `DtaSnapshot`.

AW-II (follow-on tranche, see `AW-II.md`) carries the
optimisation levers — PSI rayon, ShapeRef dispatch, PHF /
SIMD keyword tables, selector classifier, document-level
parallel parse, bloom + GADT dedup, Pratt generalisation,
parity harnesses, visitor API, bench parity confirmation.

## Architectural thesis

SoA stays. The DTA activates. Stage-C runs with DTA-emitted
`frame_depth`; `derive_frame_depth` deletes. Tape emission
is pre-order natively from the forward walk.

The walker is the sole runtime path. `parse_dta()` retires as
a separate entry — `parse()` consumes `dta_run` directly.
Every emitter helper that produced per-rule `fn __<rule>`
bodies deletes from the source tree. The `__rule_kind()`
dispatch table survives (read by `bbnf-analysis` / LSP at
compile time, not parse time).

The fuse/inline activation is non-optional. Snapshot tests
that fail under fused IR are incorrect tests, not a blocking
invariant — they encode optimiser-suppressed shape as
expected output. Snapshot migration is in-scope and lands
here.

## Invariants

1. **One path.** `parse()` dispatches to `dta_run`. No
   additive surfaces. No feature flag gating. No dual-path
   build.
2. **No legacy code.** Every per-rule fn-emission helper
   deletes. `MemoStore` deletes. `compute_sibling_skip`
   deleted (W0). `derive_frame_depth` deletes once every
   caller has the inline flag.
3. **No stubs at tranche close.** `AltLinear`, `Repeat`,
   `ShuntingYard` walker arms implement full semantics by W2
   close. The tranche is not closed until no arm of the
   runtime match is `Unsupported` / single-probe / forward-
   ref-to-head.
4. **Intentional unworkability across W3–W4 declared.**
   `parse()` swap (W3) reads the to-be-deleted emitter
   helpers until W4 closes; the workspace does not build
   through that window. FINAL-I.md + `post-AW-I.json` land
   at W5 close once green.
5. **Typed-AST parity is total** — inherited from AW.md.
   Every `->` grammar annotation reaches the tape emitter;
   inference composes types end-to-end.
6. **Bootstrap regen is CI-enforced** (W0 landed).
7. **Workspace green at W5 close.** Snapshot migration in
   W4 brings test count back to pre-activation levels or
   higher (some tests un-ignore as the DTA exposes
   correctness the legacy path approximated).

## Wave schedule

| Wave | Agents | Status | Workspace at close |
|------|--------|--------|--------------------|
| W0 cleanup | 5 parallel | landed | green (1101/0/67) |
| W1 DTA substrate | 1 serial | landed | green |
| W2 walker + memo + SCC + audit | 4 parallel | pending | green |
| W3 parse() swap + regen | 1 serial | pending | intentionally unworkable |
| W4 legacy deletion + fuse + snapshots | 5 parallel | pending | returns green |
| W5 FINAL-I + bench + close | 1 serial | pending | green |

## Phases

### W0 — Cleanup + hygiene [landed]

Five parallel agents on disjoint file bounds. AW.0.1 Stage-C
conditional gate, AW.0.2 `compute_sibling_skip` deletion,
AW.0.3 Span-rule emitter elision, AW.0.4 `__aggregate_buf`
right-sizing, AW.0.5 Colour-function `LargeAggregate` consumer
+ Color view, AW.0.6 inline-test migration (6 gorgeous
files), AW.0.7 bootstrap-clean CI gate, AW.0.8 white-colour
WideScalar routing, AW.0.9 profile population matrix, AW.0.10
fuse/inline guard drop — deferred; re-lands in W4.4.

Commits: `ba4e1e79`…`bfe17d7f`. See `PROGRESS.md` §"Wave 0β".

### W1 — DTA substrate skeleton [landed]

Single agent. `crates/bbnf-tape/src/driver.rs` with
`dta_run` walker + `FrameStack` (inline + overflow +
counters), stubbed `AltLinear` / `Repeat` / `ShuntingYard`
arms. `cursor.child(0)` O(1) `idx + 1` fast path. DTA
`Literal` / `Regex` `StringId` resolution to real byte
text. `TapeBuilder` owns `frame_depth: Vec<u8>`; `finish()`
consumes inline stream. `dta-replay` feature (decision log
+ `DtaSnapshot`). Additive `parse_dta()` emitted alongside
legacy `parse()` (retires in W3).

Commits: `11f22f1f`…`08658746`. See `PROGRESS.md` §"W1b
continuation".

### W2 — Walker completion + MemoStore retirement + SCC plumbing + snapshot audit [4 parallel]

Workspace green at close. Each sub-phase owns a disjoint
file set.

#### W2.1 — Walker arm completion

Owner: `crates/bbnf-tape/src/driver.rs` (+
`advance_or_pop`).

Three arms, one agent (shared file):

- **AltLinear** (today line 531): implement savepoint
  backtracking. Capture `(columns.len, frame_depth.len,
  psi.len, pos)` on branch entry. Attempt `branches[0]`; on
  `DtaError::Syntax`, truncate to savepoint, try
  `branches[1]`; continue. Emit `DtaError::Syntax`
  (`failing_rule` = innermost rule id) when branches
  exhaust. Discriminant recorded on enclosing Alt frame at
  successful branch close — unchanged from current.
- **Repeat** (line 552 + advance-or-pop line 696): full
  `lo..=hi` loop semantics. `advance_or_pop` for
  `DtaFrameKind::Repeat` consults the frame's counter (via
  the parallel counters column); re-enters `inner` while
  counter < hi AND position advanced. Emit compound close
  at counter ≥ lo and either counter == hi or position
  stagnant. `DtaCounterOptional` marker (AV.3.2) admits
  empty-body iterations by treating position-stagnant
  rounds as valid-iteration-that-counts-toward-lo.
- **ShuntingYard** (line 577 + advance-or-pop line 700):
  operator-precedence reducer. Enter `head`. On successful
  operand close: peek next byte at `pos`; binary-search
  `precedence: &[DtaPrecedenceEntry]` by `(byte,
  second_byte)`; on miss, reduce operator mini-stack to
  completion, pop frame. On hit, push operator; if
  precedence ≤ mini-stack top's (left-assoc) or <
  (right-assoc), reduce before push. Operator reduction
  emits one `push_compound(op_rule, ...)` with
  `op_discriminant` stamped as the typed u8 payload.

Bug-1 alt-lit per-branch payload (AV.0.1): carry-forward
into the new `AltLinear` savepoint path — each branch's
payload writes go through the savepoint truncation boundary
correctly.

Hard gate: every `match state { ... }` arm in
`driver.rs::step` executes full semantics; zero
`Unsupported`-returning placeholders; zero "W1 implements"
comments remain.

#### W2.2 — MemoStore retirement

Owner: `parse-that/rust/parse_that/src/state.rs` (sibling
repo, orchestrator-dispatched inside the main checkout).

Delete `MemoStore`, `drop_memo_table`, `ParserState::memo`,
and every `memo`-related plumbing path in
`parse-that/rust/parse_that/src/parse.rs` +
`combinators/`. Memo-dependent tests port to
deterministic-replay fixtures or delete with rationale.

Hard gate: `grep -rn 'memo' parse-that/rust/parse_that/`
returns no hits in production paths.

#### W2.3 — SCC recompute plumbing

Owner: `crates/core/src/pipeline/compile.rs`.

Inside `structural_normalizer_loop` (lines 510-533), insert
`bbnf_ir::passes::compute_scc(&mut ir)` after
`canonicalize_aliases` AND between `inline_acyclic` and
`fuse_single_use`. No guard drop yet (lands in W4.4); this
sub-phase lands the infrastructure so the W4.4 activation
only flips the guards without cascading SCC-staleness.

Hard gate: `compute_scc` appears in the normalizer loop at
least twice (top + inter-pass); workspace tests remain
1101/0/67 (no behaviour change yet).

#### W2.4 — Fuse-dependent snapshot audit

Owner: `docs/tranches/AW/audit/fuse-snapshot-migration.md`
(new).

Read-only analysis agent. Enumerate every test currently
passing that asserts against un-fused IR shape. Primary
corpus from W1b's observation: sheets parity snapshots,
payload layouts tests, grammar roundtrips. Produce a
per-test migration plan: delete (fossilised test with no
correctness value), update snapshot (shape change is
correct), or architectural concern (surface requires deeper
fix).

Output: audit markdown with three lists plus per-test
one-line rationale. No code changes.

Hard gate: audit covers every test that `W4.4`'s agent
would need to touch; zero "unknown status" entries.

### W3 — `parse()` swap + regen [1 serial]

Workspace enters intentionally unworkable state. Exits
unworkable once W4 lands.

#### W3.1 — `grammar.rs` rewrite

Owner: `crates/core/src/backend/rust/emitter/grammar.rs`.

Replace `emit_grammar_impl`'s `parse()` body with direct
`dta_run` dispatch:

```rust
pub fn parse(input: &[u8]) -> Result<Parsed<'_, Self>, ParseErr> {
    let padded = PaddedView::new(input);
    let mut builder = TapeBuilder::with_capacity(expected);
    builder.enable_inline_frame_depth();
    let mut psi = PayloadStream::with_capacity_for(&GRAMMAR_PROFILE, input.len());
    dta_run_into(&DTA_TABLE, &GRAMMAR_PROFILE, padded, &mut builder, &mut psi, &DFA_SCANNER)?;
    psi.fill_columns(builder.columns_mut());
    let tape = builder.finish()?;
    Ok(Parsed::new(tape))
}
```

Retire the emitted `parse_dta()` entry and the parallel
`DtaDfaScanner` / `parse_dta` helper functions. `parse()`
is the sole entry point.

Where the emitter still references per-rule helper functions
(`emit_alt_body`, `emit_seq_body`, etc.), strip the calls —
the helpers' outputs land in generated.rs as dead functions
that W4 deletes alongside the helpers themselves.

#### W3.2 — Bootstrap regen + workspace probe

`bash scripts/bootstrap-bbnf.sh`. Verify `generated.rs`
contains the new `parse()` body dispatching to `dta_run`,
carries zero `fn __<rule>` definitions beyond prettify-path
hold-overs, and `wc -l` returns a substantially smaller file
than pre-swap.

Run `cargo test --workspace --no-fail-fast`. Record the
failure surface: emitter helper files still exist but aren't
called; tests that exercise the emitted per-rule fns fail;
tests that exercise parse() may pass (if the walker is
feature-complete from W2.1) or fail in enumerated ways.

Hard gate for W3: `grep -cE 'fn __[a-zA-Z_]+<'
crates/core/src/grammar/generated.rs` returns 0 outside
prettify. `wc -l` returns ≤ 12000.

### W4 — Legacy emitter deletion + fuse activation + snapshot migration [5 parallel]

Workspace returns to green at W4 close. Each agent owns
disjoint source files + non-overlapping hunks in shared
integration files (`emitter/mod.rs`).

#### W4.1 — Emitter group 1

Delete:
- `crates/core/src/backend/rust/emitter/alt.rs` (807 lines)
- `crates/core/src/backend/rust/emitter/tape_prelude.rs`
  (610 lines)

Remove `pub mod alt;` and `pub mod tape_prelude;` entries in
`emitter/mod.rs`. Remove any remaining `emit_alt_body` /
prelude-emitting references inside `grammar.rs` (if W3.1
left stubs).

#### W4.2 — Emitter group 2

Delete:
- `crates/core/src/backend/rust/emitter/leaves.rs` (374
  lines)
- `crates/core/src/backend/rust/emitter/map_value.rs` (526
  lines)

Remove module declarations in `emitter/mod.rs`.

#### W4.3 — Emitter group 3

Delete:
- `crates/core/src/backend/rust/emitter/seq.rs`
- `crates/core/src/backend/rust/emitter/repeat.rs`
- `crates/core/src/backend/rust/emitter/binary.rs`
- `crates/core/src/backend/rust/emitter/operator_chain.rs`

Remove module declarations.

#### W4.4 — Emitter group 4

Delete:
- `crates/core/src/backend/rust/emitter/dispatch.rs`
- `crates/core/src/backend/rust/emitter/ws.rs`
- `crates/core/src/backend/rust/emitter/string_decode.rs`

Remove module declarations. Post-deletion: re-run bootstrap
regen. `generated.rs` shrinks further (no helper outputs).

#### W4.5 — Fuse/inline activation + snapshot migration

Drop the always-true `r.meta.scc_id.is_none()` guards at
`crates/ir/src/passes/transform/inline.rs:42` and
`crates/ir/src/passes/transform/fuse.rs:55`. With W2.3's
SCC recompute already in place, the passes fire with fresh
metadata.

Regen `generated.rs`. CSS L4 DTA state count drops from the
AV.3.6 baseline of 2473 to < 2000.

Consume W2.4's audit. Per-test: regenerate snapshots,
update assertion thresholds, or delete fossilised tests
per the audit's rationale. Expected ~45 tests touched.

Hard gate: `cargo test --workspace --no-fail-fast` returns
workspace-green; ignored count stays ≤ 67 + any
post-DTA-exposed category-A items.

### W5 — FINAL-I + bench + close [1 serial]

Single agent. Compose `docs/tranches/AW/FINAL-I.md`: per-
phase recap, hard-gate status table, invariant verification
with commit citations, cross-tranche debt reconciled, AW-II
seed-items enumerated. Run the 19-entry parse-bench matrix
cold once; compose `docs/benchmarks/post-AW-I.json` per the
research/05 schema. Confirm `cargo test --workspace`
workspace-green. Update `PROGRESS.md` with AW-I close
entry.

Hard gate: `FINAL-I.md` exists + enumerates every W0-W4
hard gate with status + artefact citation. `post-AW-I.json`
covers the 19-entry matrix. Workspace tests 0 failures.

## Critical files

| File | Wave |
|------|------|
| `crates/bbnf-tape/src/driver.rs` (walker completion) | W2.1 |
| `crates/bbnf-tape/src/driver.rs::advance_or_pop` (frame-kind arms) | W2.1 |
| `parse-that/rust/parse_that/src/state.rs` (MemoStore delete) | W2.2 |
| `parse-that/rust/parse_that/src/parse.rs` (memo plumbing) | W2.2 |
| `crates/core/src/pipeline/compile.rs` (SCC recompute) | W2.3, W4.5 |
| `docs/tranches/AW/audit/fuse-snapshot-migration.md` (new audit) | W2.4 |
| `crates/core/src/backend/rust/emitter/grammar.rs` (parse swap) | W3.1 |
| `crates/core/src/grammar/generated.rs` (regen) | W3.2, W4.4, W4.5 |
| `crates/core/src/backend/rust/emitter/*.rs` (deletion set) | W4.1-W4.4 |
| `crates/core/src/backend/rust/emitter/mod.rs` (module decls) | W4.1-W4.4 |
| `crates/ir/src/passes/transform/{inline,fuse}.rs` (guard drop) | W4.5 |
| `docs/tranches/AW/FINAL-I.md` (new) | W5 |
| `docs/benchmarks/post-AW-I.json` (new) | W5 |

## Hard gates summary

### W2

1. Walker `AltLinear` arm: savepoint backtracking;
   `branches.len() ≥ 2` backtracks correctly; syntax error
   on exhaustion.
2. Walker `Repeat` arm: lo..=hi iteration; `{0,}`, `{1,}`,
   `{2, 5}` all pass unit tests against canonical inputs.
3. Walker `ShuntingYard` arm: operator-precedence reducer;
   `1+2*3` parses as `(1, +, (2, *, 3))` with correct
   discriminants; right-associative `^` parses `2^3^4` as
   `(2, ^, (3, ^, 4))`.
4. Zero `Unsupported` / single-probe / forward-ref
   placeholders remain in `driver.rs::step` or
   `advance_or_pop`.
5. `MemoStore` deleted; `grep -rn 'memo' parse-that/` in
   production paths returns 0.
6. `compute_scc` appears in normaliser loop twice (top +
   inter-pass); workspace unchanged at 1101/0/67.
7. `fuse-snapshot-migration.md` audit covers every test at
   risk; zero "unknown" entries.

### W3

8. `grep -cE 'fn __[a-zA-Z_]+<' crates/core/src/grammar/
   generated.rs` returns 0 outside prettify.
9. `wc -l crates/core/src/grammar/generated.rs` returns
   ≤ 12000.
10. `parse()` dispatches through `dta_run`; no `parse_dta`
    or `DtaDfaScanner` function remains.

### W4

11. Emitter directory contains only `grammar.rs`, `dta.rs`,
    `mod.rs`, `profile.rs`, `visitor.rs`, `prettify/`
    (sub-dir).
12. `inline_acyclic` + `fuse_single_use` fire; CSS L4 DTA
    state count < 2000 (AV.3.6 baseline 2473).
13. Workspace `cargo test --workspace --no-fail-fast`
    returns 0 failures.

### W5

14. `docs/tranches/AW/FINAL-I.md` exists.
15. `docs/benchmarks/post-AW-I.json` covers the 19-entry
    matrix.
16. Every W2-W4 hard gate verified with commit / artefact
    citation.

## Cross-tranche debt closed in AW-I

| Item | Origin | AW-I wave |
|------|--------|-----------|
| Colour-function `LargeAggregate` consumer | AV.0.5 | W0.5 (landed) |
| Inline `#[cfg(test)]` in `crates/gorgeous/src/` | memory feedback | W0.6 (landed) |
| Bootstrap regen CI gate | AV FINAL seeds | W0.7 (landed) |
| White-colour `0xFFFFFFFFu32` collision | Agent C V0 | W0.8 (landed) |
| GrammarProfile stub-field ledger | AV.1.3 | W0.9 (landed) |
| Stage-C conditional + compute_sibling_skip delete | AV V4 | W0.1-0.2 (landed) |
| Double Span pack | AV V0.2 emitter | W0.3 (landed) |
| Always-true `if __has_payload` | AV V0.2 emitter | W0.3 (landed) |
| `mark_children` leaf-route | AV emitter | W0.3 (landed) |
| `__aggregate_buf` right-sizing | AV emitter | W0.4 (landed) |
| Pre-order cursor O(1) `child(0)` | AV.2 substrate inheritance | W1 (landed) |
| DTA walker `AltLinear`/`Repeat`/`ShuntingYard` stubs | V3 substrate | W2.1 |
| `MemoStore` (AW.1.8) | AU era | W2.2 |
| SCC staleness between inline + fuse | AU PROGRESS | W2.3 |
| Fuse/inline activation (AW.0.10) | AU PROGRESS | W4.5 |
| CSS L4 DTA state count < 2000 | AV.3.6 | W4.5 |
| Legacy `fn __<rule>` emission | AU era | W3.1 + W4.1-4.4 |
| `parse_dta` additive surface | W1 substrate landing | W3.1 |

## Research artefacts — inputs

- `research/01-dta-driver-design.md` feeds W2.1 (walker
  contract, FrameStack + counters, replay signature
  variance).
- `research/02-shaperef-runtime-dispatch.md` feeds AW-II.W1
  (not consumed in AW-I).
- `research/03-pratt-lowering-generality.md` feeds W2.1
  `ShuntingYard` arm (precedence-LUT format, reducer
  semantics) and AW-II.W3 (Pratt generalisation).
- `research/04-named-struct-abi-finalisation.md` consumed
  by W0.5 (landed).
- `research/05-bench-checkpoint-protocol.md` feeds W5
  bench composition and AW-II close.
