# AZ-II.cutover.O1 — StructDirect Builder Transactions
**Opens after**: AZ-II.cutover.O0 close
**Agents**: up to 10 parallel
**Hard gate**: every speculative StructDirect branch restores builder state, not only input position, before EBNF activation proceeds.
**Status**: complete

## Scope

1. Add a grammar-general checkpoint/rollback/commit ABI to the
   StructDirect builder trait.
2. Implement checkpoints for every grammar-specific builder, including
   arena cursors, open-frame stacks, root slots, next handles, pending
   leaves, and CSS pending-value state.
3. Wire speculative alternate, repeat, minus, negate, keyword-ref,
   AltDispatch, array, arglist, unordered, flat, and wrap emission paths
   through the transactional ABI.
4. Refresh generated parser source through a single orchestrator-owned
   regen after runtime and emitter edits compose.
5. Add wire-contract tests that fail if failed speculative branches leak
   roots, frames, or arena allocations.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/runtime/builder.rs` | modify |
| `crates/core/src/runtime/{bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}/builder.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/{alt_dispatch,array,arglist,flat,keyword,pratt,unordered,wrap}/**/*.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/{minus,negate,repeat}.rs` | modify |
| `crates/core/tests/json_parity_struct.rs` | modify |
| `crates/core/tests/css_l4_substrate.rs` | modify |
| `crates/core/tests/projection_totality.rs` | modify |
| `crates/core/src/grammar/generated/*.rs` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover.md` | modify |

**Do NOT touch**: resolver-arm policy, `Parsed<R>`, `TapeDirect`,
`crates/tape/**`, benchmark result JSON, or close-matrix scripts. O2
owns EBNF activation; O4/O5 own return-model and tape deletion.
Deployment invariant: every sub-agent runs in a sibling
fully-contained worktree seeded with `scripts/seed-worktree.sh`, with
explicit allow/forbidden lists; only the orchestrator performs final
fleet regen after accepting non-overlapping runtime/emitter commits.

## Phase sub-items

### AZ-II.cutover.O1.1 Builder Trait ABI

Mechanism: add an associated checkpoint type plus
`checkpoint`, `rollback`, and `commit` methods to the shared
StructDirect builder trait.

Files touched: `crates/core/src/runtime/builder.rs`.

Sub-gate: builder trait implementors fail to compile until they expose
checkpoint semantics.

### AZ-II.cutover.O1.2 JSON and Sheets Builder Checkpoints

Mechanism: snapshot/restore roots, handles, open frames, pending leaves,
and arena cursors for JSON and Google Sheets.

Files touched:
`crates/core/src/runtime/{json,google_sheets}/builder.rs`.

Sub-gate: focused rollback tests show failed speculative branches do
not leave completed roots or open frames.

### AZ-II.cutover.O1.3 CSS Builder Checkpoints

Mechanism: snapshot/restore CSS arena state and pending-value state
that can be mutated before a branch fails.

Files touched: `crates/core/src/runtime/css_l4/builder.rs`,
`crates/core/src/runtime/css_pretty/builder.rs`.

Sub-gate: CSS failed-branch contract no longer reaches
`finalise called with open frame(s)`.

### AZ-II.cutover.O1.4 Grammar Fleet Builder Checkpoints

Mechanism: implement the same ABI for BBNF, BNF, CSV, Math, and EBNF
builders.

Files touched:
`crates/core/src/runtime/{bbnf,bnf,csv,math,ebnf}/builder.rs`.

Sub-gate: all grammar-specific builders compile against the shared
checkpoint trait without local compatibility shims.

### AZ-II.cutover.O1.5 AltDispatch Transactions

Mechanism: wrap speculative AltDispatch branch attempts in
checkpoint/commit/rollback so failed branches do not leak builder
mutation.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/**/*.rs`.

Sub-gate: generated AltDispatch code restores both `*p` and builder
state on failed branch attempts.

### AZ-II.cutover.O1.6 Keyword and Ref-Led Transactions

Mechanism: apply the same transaction pattern to keyword shape
speculation, especially Ref-led transparent/structural branches.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/keyword/**/*.rs`.

Sub-gate: keyword branch failure cannot leak branch tags or child
handles.

### AZ-II.cutover.O1.7 Repeat/Minus/Negate Transactions

Mechanism: wire repeat, minus, and negate speculative probes through
builder checkpoints.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/{minus,negate,repeat}.rs`.

Sub-gate: probes that must not consume output leave the builder exactly
at the entry checkpoint.

### AZ-II.cutover.O1.8 Compound Shape Transactions

Mechanism: apply transaction handling to array, arglist, flat, pratt,
unordered, and wrap shape families where speculative child calls can
mutate builders.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/{array,arglist,flat,pratt,unordered,wrap}/**/*.rs`.

Sub-gate: `cargo check -p bbnf --lib --profile ax-iter` passes before
fleet regen.

### AZ-II.cutover.O1.9 Wire-Contract Tests

Mechanism: add or extend tests that create failed branch attempts after
mutating builders and then require a clean finalise.

Files touched: `crates/core/tests/json_parity_struct.rs`,
`crates/core/tests/css_l4_substrate.rs`,
`crates/core/tests/projection_totality.rs`.

Sub-gate: focused JSON and CSS rollback tests fail before checkpoint
wiring and pass after integration.

### AZ-II.cutover.O1.10 Orchestrator Regen and Progress

Mechanism: run canonical `cargo xtask regen`, review generated diffs,
run `cargo xtask regen --check`, and record O2 as next active gate.

Files touched: `crates/core/src/grammar/generated/*.rs`,
`docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover.md`.

Sub-gate: generated fleet is idempotent and progress docs agree that
O2 owns EBNF activation.

## Hard gate

1. `cargo check -p bbnf --lib --profile ax-iter` passes.
2. Focused JSON StructDirect checkpoint tests pass.
3. Focused CSS StructDirect checkpoint tests pass.
4. `cargo xtask regen --check` passes across all nine grammars.
5. Speculative StructDirect emitter paths use builder
   checkpoint/commit/rollback instead of input-position-only rollback.

## Verification artefacts

- `/tmp/az-ii-o1-cargo-check.txt`
- `/tmp/az-ii-o1-json-rollback.txt`
- `/tmp/az-ii-o1-css-rollback.txt`
- `/tmp/az-ii-o1-regen-check.txt`
- O1 close commit hashes recorded in `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.O0
- **Blocks**: AZ-II.cutover.O2

## Archaeology

The cutover.N EBNF attempt exposed a grammar-general correctness gap:
speculative parse paths restored input position but not builder state.
O1 closes that root cause before any EBNF-specific repair is allowed to
land.
