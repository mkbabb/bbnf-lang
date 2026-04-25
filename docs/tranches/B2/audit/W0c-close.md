# B2.W0.c — Close

W0.c re-executes against the post-B3 + post-B4.W0 substrate. The
parser hang B3 closed and the SIMD bitmap kernel `syn::parse2` defect
B4.W0 closed are both upstream of W0.c's mechanism: with both fixes
in place, `cargo xtask regen --grammar bbnf` runs the parser, the
17-pass IR pipeline, the `generate_all` codegen, and `prettyplease`
end-to-end in milliseconds, writing a populated, parseable per-grammar
source file at `crates/core/src/grammar/generated/bbnf.rs`.

## Pre-state

- Master HEAD: `a5fdda6b` (B4.W0 close).
- Worktree HEAD: same; clean.
- xtask substrate (B2.W0.a/b) landed at `dec67806` + `3c68e8c4`.
- W0.c partial cherry-pick at `21881591` migrated
  `crates/bootstrap/src/lib.rs` to
  `pub use ::bbnf::grammar::generated::BbnfBootstrap;`.
- `crates/core/src/grammar/generated.rs` (33 279 lines) was the
  pre-B2 monolithic emission product of `scripts/bootstrap-bbnf.sh`.
  Its `BbnfBootstrap` symbol resolves through `pub mod generated;`
  in `crates/core/src/grammar/mod.rs`.

## Restructure

- `crates/core/src/grammar/generated.rs` (33 279 lines) — DELETED.
- `crates/core/src/grammar/generated/bbnf.rs` (34 048 lines) —
  CREATED via `cargo xtask regen --grammar bbnf`. The line-count
  delta (~770 lines) reflects post-B1 IR-pipeline accumulation
  (FusedOutput projection emitter, structural-scan emission updates)
  that the bootstrap script never re-ran on the bbnf grammar.
- `crates/core/src/grammar/generated/mod.rs` — CREATED. Aggregator:
  `pub mod bbnf;` + `pub use bbnf::*;`. Resolves
  `bbnf::grammar::generated::BbnfBootstrap` through the per-grammar
  module.
- `crates/core/src/grammar/mod.rs` — UNCHANGED (`pub mod generated;`
  declaration resolves to `generated/mod.rs` once the directory module
  exists).

The aggregator's `pub use bbnf::*;` lifts both `pub struct BbnfBootstrap;`
and the grammar-emit-impl module's `pub use __bbnfbootstrap_emit_impl::*;`
re-export to `bbnf::grammar::generated::*`, preserving every public
symbol the pre-B2 monolith exposed.

## Post-W0.c emitter fixes

The first-cycle regen exposed two emitter defects that surfaced only
when the post-AY-II.W0'.b ValueRoot projection emission ran on a
grammar with admissions. The pre-B2 `bootstrap-bbnf.sh` had not
re-run on bbnf since these emitter landings, so the monolithic
`generated.rs` carried a pre-W0'.b shape that did not exercise either
defect:

1. **`FusedOutput` missing generic argument** — `emit_projection_fn`
   in `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
   and `emit_value_root_impl` in
   `crates/core/src/backend/rust/view/value.rs` emitted
   `output: &::bbnf::runtime::FusedOutput` without the `<R>` type
   parameter. `FusedOutput<R>` is parameterised by the grammar root
   marker (`R = BbnfBootstrap` for the bbnf grammar). The fix threads
   `grammar_ident` (e.g. `BbnfBootstrap`) into all four emitted
   `&FusedOutput<#grammar_ident>` slots: the materializer fn signature
   in `value_materialize.rs:220`, and the three projector fns in
   `view/value.rs:409`, `:442`, `:465`.

2. **`output.value_frames()` method does not exist** — the panic
   messages in `emit_value_root_impl`'s `frame_fn` and `root_fn`
   referenced `output.value_frames().len()`, but `FusedOutput<R>`
   exposes the slab as `frames()`, not `value_frames()`. The fix
   renames both call sites in `view/value.rs:419` and `:453` to
   `output.frames().len()`.

Both fixes are single-source at the responsible emitter; no shadow
surface, no `try`-style fallback at the consumer, no compositional
change elsewhere in the codegen pipeline. The fixes follow the
B4.W0 precedent (single-source emitter correction at the source of
the malformed fragment).

## cost_grid_sweep + dump_ir resolution

Option (c) — re-add `bbnf-ir` as a regular dep on `bbnf-bootstrap`.
The pre-B2 build-orphan flagged in B3.FINAL §"Out of scope" and
B4.B2.md §"Pre-existing build-orphan" affects two dev-only diagnostic
binaries:

- `crates/bootstrap/src/bin/cost_grid_sweep.rs` —
  `use bbnf_ir::passes::lift_dta;` (live infra per AX.W13).
- `crates/bootstrap/src/bin/dump_ir.rs` —
  `use bbnf_ir::{GrammarIR, IrNode, RuleId};` (live infra per
  `docs/tranches/AW/audit/full-codebase-prune.md`).

Option (a) (delete) is rejected — both binaries are referenced as
live diagnostic tooling. Option (b) (re-export through `bbnf`) would
add a parallel surface that complicates the IR-crate boundary the
workspace established at `21881591`. Option (c) is the minimal
correction: `bbnf-ir = { path = "../ir" }` lands as the second entry
in `[dependencies]` on `crates/bootstrap/Cargo.toml`, with a comment
noting the bin-only consumption (the library crate `src/lib.rs` does
not consume `bbnf-ir`).

This closes the latent build break that has gated `cargo check
--workspace` since `21881591`.

## Verification

| Gate | Wall | Exit |
|---|---|---|
| `cargo xtask regen --grammar bbnf` (cycle 1, cold xtask compile) | 1 m 41 s (xtask compile 60 s + regen 73 ms) | 0 |
| `cargo xtask regen --grammar bbnf` (cycle 2, post-fix re-emit) | 1 m 10 s (xtask incremental + regen 82 ms) | 0 |
| Regen idempotence (`git diff` on third run) | zero-line diff | 0 |
| `cargo check --workspace --profile ax-iter` | 6 s warm | 0 |
| `cargo test --release -p bbnf --test bbnf_parity --exact bbnf_parses_its_own_grammar` | 35 s release compile + 0.00 s exec | 0 |
| `cargo iter-check-full` (warm under W0.c substrate) | 5.62 s | 0 |
| `cargo nextest run --workspace --test projection_totality` | derive-time E0599 on 4 test fixtures | 101 (pre-existing) |

The xtask cycle-1 regen wall (1 m 41 s) sits orders of magnitude
under the pre-B2 80-min `bootstrap-bbnf.sh` ceiling; the IR pipeline
itself runs in ~73 ms (`compile_paths_request` 3 ms + `generate_all`
9 ms + `prettyplease` 60 ms). The xtask incremental compile dominates
the wall on first-cycle runs; subsequent regens are sub-second.

The `projection_totality` test compiles `JsonG`, `CssL4G`, `SheetsG`,
`BbnfG` via `#[derive(Parser)]` (the proc-macro path the xtask
substrate is gradually retiring). It fails at derive-time with E0599
on `<G>::PROJECTION_*` consts and `<G>::parse` — pre-existing test
debt B3.FINAL §"Test results" attributed to AY-II.W0' migration
polish; B4.W1 owns the consumer-fixture polish for these test
fixtures. Master at `a5fdda6b` shows 83 errors against the same
target; the post-W0.c emitter fixes for `FusedOutput<R>` reduce
the count to 32. The remaining 32 are derive-cache resolution
defects in the proc-macro path, not in the xtask path W0.c lands.

## W0.c verdict

CLOSED. The xtask substrate writes a per-grammar source file at
`crates/core/src/grammar/generated/bbnf.rs`; the file compiles clean
in the workspace; the self-host parity test passes; the regen is
idempotent; the latent build-orphan in `cost_grid_sweep` /
`dump_ir` is resolved.

## Hand-off

W1 inherits a substrate where the xtask emits per-grammar source on
disk and consumers `include!` (or `pub use` re-export) the product.
The bbnf-bootstrap consumer migration landed at `21881591`; the
remaining ~50 consumer sites (gorgeous's 5, tests' ~50) wait on W1's
mechanical extension under the xtask-emitter contract.

The two emitter fixes from this close are forward-applicable to every
grammar that admits projections; W1's xtask regen on json, css_l4,
sheets, ebnf, bnf, and the gorgeous prettify variants will exercise
the same projection emission path on its own corpora. Any defect
those grammars surface lands as an emitter fix, not a consumer
work-around.
