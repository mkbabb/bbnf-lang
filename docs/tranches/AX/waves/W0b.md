# AX.W0b — Interpreter Deletion + Substrate-Without-Consumer Purge + Crate Renames

**Opens after**: W0a close
**Agents**: 4 parallel (deletion, emit/ purge, renames, tests)
**Hard gate**: `nm` + `grep` zero DTA symbols; `cargo test --workspace` green; bootstrap idempotent; generated.rs shrinks ~57K lines; ~78,500 LOC total reclaim.

## Scope

Five categories of deletion, all atomic to this wave per the AW documentation trail. The W0a gate repair makes every grammar's `parse()` reach shape emission; this wave deletes every path that no longer serves a consumer.

1. **DTA interpreter machinery** (~12,000 LOC source + ~57,481 LOC generated.rs walker output).
2. **Substrate-without-consumer purge** (~2,500 LOC): `tape::psi`, `simd-scan::emit/*`, 7 dead `GrammarProfile` slots, `bbnf-tape-codegen` entire crate, Lever 4, `state_visit_frequency` miner.
3. **Deprecated code** (~20 LOC): `csp_strategy` `#[deprecated]` aliases + `#[allow(deprecated)]` suppressor.
4. **DTA-coupled test suites** (~3,900 LOC): walker_arms, aq5_regression, driver_dual_cursor, dta_walker_codegen, dta_counter_states, dta_shunting_yard, dta_diagnostic_replay, aw3_w1_payload_lift, aw_ii_w5b_minus, aw_ii_w2_binary_factor, aw3_w1_walker_trace, aw5_w13_substrate (Lever 4 section only).
5. **Crate renames**: `bbnf-tape` → `tape`, `bbnf-simd-scan` → `simd-scan`, `bbnf-json-prototype` → `json-prototype`.

## File bounds

Delete (absolute):

| File / Directory | LOC | Source |
|---|---:|---|
| `crates/bbnf-tape/src/dta.rs` | 550 | DTA types |
| `crates/core/src/backend/rust/emitter/dta_walker/` (5 files) | 4,360 | Walker emitter |
| `crates/core/src/backend/rust/emitter/dta.rs` | 935 | DTA-table emitter |
| `crates/bbnf-tape-codegen/` (entire crate) | 672 | Zero consumers (Agent 2) |
| `crates/bbnf-simd-scan/src/emit/` (9 modules + mod.rs) | 1,344 | Zero non-test consumers (Agent 2) |
| `crates/ir/src/passes/recognizers/state_visit_frequency.rs` | 405 | DTA-only miner (Agent 5 L12) |
| Walker-only test suites (12 files) | ~3,900 | DTA tests |

Modify (surgical carve):

| File | Delete | Retain |
|---|---|---|
| `crates/bbnf-tape/src/driver.rs` | `dispatch_one`, `try_branch`, `advance_or_pop_with`, `handle_repeat_failure*`, `dta_run_cold`, `dta_run_with_replay`, `dta_run_parallel*`, `FrameStack`, `Frame`, `DtaSnapshot`, `StepResult`, `IterSavepoint`, `pop_and_release`, `frame_at`, `stack_top`, `advance_seq_fast`, `frame_to_tape_kind` (~2,873 LOC) | `emit_leaf`, `emit_leaf_with_payload`, `emit_reducer_compound`, `close_compound`, `trim_with_pattern`, `trim_ascii_ws`, `first_ws_pattern`, `saturating_u16`, `stage_literal_payload_in_arena`, `lookup_precedence`, `DtaError` surface (~450 LOC) |
| `crates/ir/src/passes/recognizers/dta.rs` | DtaState-lifting path (~813 LOC) | SHAPE_DICT + GRAMMAR_PROFILE mining (~700 LOC) |
| `crates/bbnf-tape/src/columns.rs` | `push_compound_fused_v32` + supporting `Packed` struct (lines 897-1062, ~165 LOC; Lever 4 self-alias per R4 §5) | Add paired `stp` method for span_lo/span_hi writes |
| `crates/bbnf-tape/src/profile.rs` | 7 dead slots + 6 `emit_*` helpers (~400 LOC): `active_columns`, `branch_priors`, `reorder_unroll_visitors`, `keyword_tables` (duplicate with `shapes/keyword.rs`), `dedup_eligible_rules`, `payload_bytes_per_input_byte`, `expected_ns_per_byte` (repopulated in W9 from `compounds_per_input_byte × 1.5`) | All live slots |
| `crates/ir/src/passes/csp_strategy/mod.rs` | `#[deprecated] solve_strategy_and_materialization` (lines 434-442), `#[deprecated] solve_strategy_decisions` (447-453) | — |
| `crates/ir/src/passes/mod.rs` | `#[allow(deprecated)]` at line 53 | — |
| `crates/core/src/backend/rust/emitter/grammar.rs` | All remaining `has_w4_classified` / `has_full_shape_coverage` / `has_shape_dispatcher_entrypoint` gates (retire entirely; return `true` OR delete call sites) | `parse()` emission body |
| `crates/bbnf-tape/src/builder.rs` | `parse()` shim at line 768; `enable_inline_frame_depth` helper (lines ~100) | All non-DTA builder surface |
| Cargo.toml workspace | `bbnf-tape-codegen` membership | — |

Rename (directory + Cargo.toml + every `use bbnf_*::` import):

| Old | New |
|---|---|
| `crates/bbnf-tape/` | `crates/tape/` |
| `crates/bbnf-simd-scan/` | `crates/simd-scan/` |
| `crates/bbnf-json-prototype/` | `crates/json-prototype/` |

## Phase sub-items

### W0b.1 DTA + support code deletion

Four agents parallel:

- **Agent A** (deletion): `dta_walker/`, `emitter/dta.rs`, `dta.rs` types, driver carve, IR lifter carve, bbnf-tape-codegen entire crate, Lever 4, `state_visit_frequency`.
- **Agent B** (emit/ purge): `simd-scan/src/emit/` directory + its test file `tests/emit_fragments.rs` + `pub mod emit;` export line in `lib.rs`.
- **Agent C** (renames): workspace Cargo.toml + directory renames + `cargo fix --edition` style import migration for every `use bbnf_tape::`, `use bbnf_simd_scan::`, `use bbnf_json_prototype::`.
- **Agent D** (tests): delete 12 DTA-coupled test files; revise multi-file tests to drop walker assertions.

### W0b.2 csp_strategy + profile carve

Agent A extends to delete deprecated aliases + suppressor, delete the 7 dead `GrammarProfile` slots, delete their 6 `emit_*` helpers.

### W0b.3 Miner inheritance decisions

The four W0b **decision records**: `pattern_alphabets`, `ctns_lifts`, `delim_scan_configs`, `key_dispatch_configs`. Each is walker-only-consumed today. Three options per miner:
- **Consume** in a shape emitter module now (in-scope sub-task of W0b or fold to W4/W5).
- **Delete** miner if no consumer decision can be made.
- **Preserve as W0b investigation queue** resolved at W3 or W4 open.

Orchestrator must decide at wave open. Default: consume-in-W4/W5 per Agent 5's L1/L2/L3 recommendations; miners survive W0b.

### W0b.4 Bootstrap recipe

Per README.md §Self-host circular-dependency escape (commits `87f65214` + `49656fd4` template). Procedure:

1. Keep `dta_walker/` emitter for BBNF's generated.rs for one commit pre-deletion.
2. Regen bootstrap; verify new shape-emitter output works for BBNF.
3. Delete `dta_walker/` in follow-up commit.
4. Re-regen; idempotent diff empty.

### W0b.5 Workspace test audit

Run `cargo test --workspace`, `cargo test --workspace --release`, `cargo bench --no-run`. Any failure = wave blocker; investigate per test, either fix by refactoring test away from DTA coupling, or delete test if DTA-only.

## Hard gate

1. `nm target/release/deps/{json,css_l4,google_sheets,bbnf}_monolithic-*` zero symbols matching `dispatch_one|try_branch|advance_or_pop_with|dta_run|DtaTable|DtaState|FrameStack|PayloadStream|__emit_fragment`.
2. `grep -rE 'dispatch_one|DtaState|dta_run|PayloadStream' crates/ --include '*.rs'` zero hits.
3. `cargo test --workspace` green.
4. `cargo bench -p bbnf --no-run` compiles all bench targets.
5. Bootstrap regen idempotent: two consecutive runs produce byte-identical `generated.rs`.
6. Generated.rs shrinks ~57,481 lines (walker output evaporates).
7. Workspace `Cargo.toml` lists `tape`, `simd-scan`, `json-prototype` (no `bbnf-` prefix on workspace-local crates).
8. All `use bbnf_tape::`, `use bbnf_simd_scan::`, `use bbnf_json_prototype::` migrated.
9. `cargo check --workspace` exit 0.

## Verification artefacts

- `nm` outputs for all four bench binaries.
- `grep` exit codes confirming zero hits.
- LOC delta: `git diff --stat master...HEAD` showing ~78,500 LOC reclaim.
- Bootstrap regen diff: `diff /tmp/gen1.rs crates/core/src/grammar/generated.rs` empty.
- `cargo test --workspace` output in `docs/benchmarks/post-AX-W0b-close-tests.txt`.

## Dependencies

- Depends on: W0a (deleting the walker before routing fix makes tests fail).
- Blocks: W0c, W1 (every subsequent wave operates on the renamed workspace with no DTA).

## Investigation queue (resolved at later waves)

Items not delete-able today without more analysis (Agent 2 §Investigation queue):

1. **`emitter/visitor.rs`** (~360 LOC) — `@visitor` directive populating `VisitorDescriptor` on any grammar? If no, delete at W3 open.
2. **`passes/transform/{alias,fuse,inline,optimize,prune}`** (~500-800 LOC) — call-graph trace from `pipeline/compile.rs`. Orphan modules delete at W3 open.
3. **`pattern_alphabet.rs:369,383` `#[allow(dead_code)]`** (~30 LOC) — tests-only consumers; retire tests + gated internals together at W3 open.
4. **`crates/csp-solver/src/py.rs` + `feature = "py"`** — upstream csc411 vendor question; orchestrator decision at W3 open.
5. **`parse-that/parsers/css/*`** (1,625 LOC, out-of-workspace) — flag to parse-that orchestrator.

## Archaeology

DTA interpreter shipped across AW-I → AW-V (five tranches); proven un-inlinable per `aw3-r6-path-b-rip-dta.md` §7 + R3's state-count + non-uniform body analysis. PSI shipped AV.V4; zero shape-emitter consumer per `psi-and-dead-substrate.md` §1. `simd-scan/emit/` shipped AW-V.W1.2 with stated intent to feed shape emitters; grep verifies zero non-test consumers per Agent 2. `bbnf-tape-codegen` shipped AW-V.W1.1 for the same walker consumer; evaporates when walker dies. The 7 dead profile slots have histories spanning V1→V8 per Agent 3's accumulation pattern, each landed substrate-side without the consumer that justified the slot.
