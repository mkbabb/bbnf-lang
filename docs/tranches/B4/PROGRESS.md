# B4 — Progress Log

Dated execution log for tranche B4, the codegen-emission correctness +
W0' re-land hardening tranche.

- `Status`: complete
- `Current wave`: W0 + W1 both closed; tranche closed
- `Next wave`: AY-II.W1 (JSON semantic parity + peer-referenced perf)

---

## 2026-04-25 — B4 close (W1 close + AY-II.W0' close ceremony folded)

W1 closes the AY-II.W0' migration debt at source. The 327-failure
runtime-parser regression that surfaced across the workspace as
`FusedBuilder::finish called with N open value frames remaining`
traces to a contract mismatch in the rollback path: every shape
emitter rolled back via `builder.columns_mut().rollback_to(X)`,
which truncates only the tape column family, while the public
`FusedBuilder::rollback_to` carried a single-pop limit that could
not unwind nested compounds even when invoked. Both paths leak
value-frame opens across every retry.

Two changes at the canonical fused-substrate boundary fix the
regression. `ValueCheckpoint` gains a `tape_idx: u32` field
recording the tape row offset its paired compound was stamped at;
the field is captured at `value_begin_compound` from the tape-side
`idx` `begin_compound` returned. `FusedBuilder::rollback_to(open_offset)`
pops every open-stack entry whose `tape_idx >= open_offset` and
truncates the value column families
(`value_frames`, `value_payloads_narrow`, `value_payloads_wide`)
to the outermost popped checkpoint's pre-open state in one pass.
The first survivor's `direct_child_count` decrements once — the
failed branch's outermost compound was registered as one direct
child — so a successful retry re-opens + closes symmetrically
without double-counting.

Every shape emitter migrates from
`builder.columns_mut().rollback_to(...)` to
`builder.rollback_to(...)`, routing through the unified atomic
path. Generated parser code regenerates against the new emission;
`cargo xtask regen --check` is clean across 9 grammars after a
cycle-1 regen sweep.

The transitional alias surface retires on the same landing:

- `pub type TapeBuilder = FusedBuilder` → DELETED
  (`crates/tape/src/builder/mod.rs`).
- `pub use ... TapeBuilder ...` from `crates/tape/src/lib.rs`
  → re-export removed.
- `pub type ValueBuilderOutput<R> = FusedOutput<R>` → DELETED
  (`crates/core/src/runtime/mod.rs`).
- `pub mod value_builder { ... }` shim module + `_ValueBuilderShim<R>`
  ZST + `pub type ValueBuilder<R> = _ValueBuilderShim<R>` +
  `value_builder_new_call_count` /
  `reset_value_builder_new_call_count` shim fns → DELETED entirely.
- 4-arg `Parsed::new_fused(tape, input, root_offset,
  value_builder_output)` bridge → DELETED
  (`crates/core/src/runtime/parsed.rs`); the grammar emitter calls
  the 3-arg `Parsed::new_fused_output` directly.
- `Parsed::value_builder_output` / `Parsed::into_value_builder_output`
  accessors → RENAMED to `value_frames_output` /
  `into_value_frames_output` (un-aliased).

Consumer test fixtures migrate to the canonical fused-builder
counter accessors at `tape::builder::{fused_builder_new_call_count,
reset_fused_builder_new_call_count}`. The
`shape_dispatch_emission` golden fixtures regenerate against the
post-W1 emitter (one-time `BLESS_SHAPE_GOLDENS=1` run; the
`assert_matches_golden` helper now supports the env-var override
for intentional emitter changes).

Hard-gate evidence (all closed):

- `cargo nextest run --workspace --profile ax-iter --no-fail-fast`
  reaches `1480 / 10 / 27 / 1490` (pass / fail / skip / total)
  in 43.93 s — a +317 pass-count delta from the pre-W1 baseline
  of `1163 / 327 / 0 / 1490`. The 10 remaining failures are all
  pre-existing bugs unrelated to the W1 migration scope (5
  cursor-shape projection stubs in W0'.b, 1 string-materializer
  payload-byte addressing bug, 1 ay_w3b smoke trip path, 2
  ir_enums populator-gap assertions, 1 timing-sensitive
  packed_cache ratio test).
- `cargo check --workspace --profile ax-iter` exit 0; warm
  `cargo iter-check` 0.27 s.
- `cargo xtask regen --check` exit 0; clean across 9 grammars.
- `rg -n 'TapeBuilder|ValueBuilderOutput|_ValueBuilderShim|new_fused\b|value_builder' --type rust`
  returns zero matches outside `docs/tranches/B2/audit/W0-bbnf-surface-snapshot.rs`
  (historical pre-W1 frozen snapshot intentionally left intact).

**AY-II.W0' close ceremony lands at B4 close.** The W0'.a
compose-boundary aliases retire entirely; `docs/tranches/AY-II/waves/W0p.md`
moves to `complete`; `docs/tranches/AY-II/AY-II.md` reflects the
close in the W0' row; `docs/tranches/AY-II/PROGRESS.md` carries
the close-ceremony entry. Per `AY-II/PATH-FORWARD.md` §5 the W0'
close ceremony lands at B4 close: that lands here, with the same
commit chain that ships W1's fix.

Worktree HEAD pre-cherry-pick: TBD (filled in at cherry-pick).
Audit: `docs/tranches/B4/audit/W1-close.md`.
FINAL: `docs/tranches/B4/FINAL.md`.

## 2026-04-25 — W0 closed (codegen-emission `syn::parse2` fix landed)

W0 dispatched and closed in a single execution round. The defect
traced to `crates/core/src/generate/regex/emit/simd.rs` lines 179
and 200 — the `emit_structural_bitmap_kernel` AVX2 nibble-LUT scan
emitter produced two `break 'avx2_scan ::core::option::Option::Some(...)`
forms whose bare-path break-values syn's expression parser rejects
with "expected loop or block expression". Syn admits only a loop, a
brace block, or a labelled block as the break-value, not a path
expression. The bbnf grammar exercises the kernel through prettify
codegen (negated character-class regexes inside `@pretty`-marked
rules: `big_comment`, `comment`, `pretty_hint`, `grammar_item`); the
json grammar does not.

Fix wraps each break-value in `{ ... }` so the emitted form becomes
`break 'avx2_scan { ::core::option::Option::Some(...) }`. The block
satisfies syn's expectation while preserving identical runtime
semantics. Single-source change at the offending emitter; no shadow
surface, no `try`-fallback at the consumer, no compositional change
elsewhere in the codegen pipeline.

Hard-gate evidence (all closed):

- `cargo xtask regen --grammar bbnf` exit 0; `crates/core/src/grammar/generated/bbnf.rs`
  written populated at 34,048 lines (~3% above the existing
  `crates/core/src/grammar/generated.rs` reference of 33,279 lines).
  Per-phase walls: compile_paths_request 3.15 ms; generate_all
  9.22 ms; prettyplease 59.23 ms.
- `cargo xtask regen --grammar json` exit 0 (regression sanity).
  Per-phase walls: compile_paths_request 1.46 ms; generate_all
  3.00 ms; prettyplease 10.27 ms.
- `cargo check -p bbnf -p xtask -p bbnf_derive --profile ax-iter`
  exit 0 in 5.89 s.
- `cargo nextest run -p tape --profile ax-iter` 100/100 passed in
  0.186 s.

Worktree HEAD: `8a1c1c94`. Fix commit ready for cherry-pick to master.

## 2026-04-25 — Plan authored + W0 opened

B4 opens as a focused two-wave tranche. The predecessor tranche closed
on parser-baseline restoration; its FINAL noted that the bbnf self-host
xtask regen reaches the codegen TokenStream emission stage cleanly and
then fails at `syn::parse2` with "expected loop or block expression".
The TokenStream produced by `generate_all` for the bbnf grammar
contains a position-context defect: an expression slot receives a
syntactic form that Rust's grammar admits only as a statement or
block. The defect surfaces only on the bbnf grammar (which exercises
shapes the smaller json grammar does not — Pratt operator chains, deep
alternation, recursion-elimination dispatchers, visitor codegen).

W0's scope: reproduce the panic, capture the offending TokenStream
excerpt, attribute the defect to a single emitter at file:line, fix at
source, verify end-to-end (bbnf regen exit 0, json regen sanity,
workspace check, tape tests), cherry-pick to master.

W1's scope: forward-pointer to the consumer-fixture polish that
AY-II.W0' migration deferred (per `AY-II/PATH-FORWARD.md` §"Immediate
cleanup targets"). Implementation lands in a later dispatch.

Authored in this initial state:

- `B4.md` — 6 invariants, two-wave schedule, cross-tranche debt
  ledger pointing at AY-II.W0' close ceremony at B4 close, escape
  clause covering scope expansion if multiple emitter shapes
  contribute to the defect.
- `waves/W0.md` — phase decomposition: reproduce + capture, root-
  cause analysis, fix at source, verify, cherry-pick + close.
- `waves/W1.md` — forward-pointer for consumer-fixture polish.
- `AGENT_DISPATCH.md` — W0 sub-agent dispatch surface with explicit
  anti-patterns (no `try-or-fallback`, no callsite work-around, no
  `git reset --hard`, no γ-η revert).
- `PROGRESS.md` — this file.

W0 dispatched in this state. Master HEAD: `573cc672` (predecessor
tranche close).
