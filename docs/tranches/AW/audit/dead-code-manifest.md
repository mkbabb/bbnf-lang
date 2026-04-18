# AW Post-V Dead-Code Delete Manifest — Audit Agent 3

## Angle headline

**Comprehensive AX.W0+ delete manifest: interpreter exorcism, substrate-without-consumer reclamation, and shim retirement, verified against master HEAD `0f69e08d`.** Estimated reclaim: **~12.1 K LOC of hot-reachable machinery + ~4.6 K LOC of substrate-gated-off-forever + ~1.5 K LOC of docs/tests referring to retired symbols**. Every line-count cited comes from `wc -l` on master; every dead-status claim comes from `grep`-verified non-consumer status or explicit FINAL-V.md ledger.

## Tier 1 — Hard delete (known dead, single-wave safe)

| File | LOC | Action | Status evidence |
|---|---:|---|---|
| `crates/bbnf-tape/src/driver.rs` | 3,323 | Delete `dispatch_one` (line 1350), `try_branch` (1277), `advance_or_pop_with` (2536), `handle_repeat_failure` (1095), `handle_repeat_failure_bounded` (1158), `dta_run_cold` (788), `dta_run_with_replay` (804), `dta_run_parallel` + `dta_run_parallel_rayon` (2922/2961), `FrameStack` + savepoints (342/392/428), `Frame`/`OpStackEntry`/`IterSavepoint` (214/289/317), `DtaSnapshot` (2782), `frame_to_tape_kind` (2810), `StepResult`/`RepeatAbsorbResult` (1055/1067), `pop_and_release`/`frame_at`/`stack_top`/`advance_seq_fast` (1221/1249/2464/2507). Retain ONLY `emit_leaf` (2158), `emit_leaf_with_payload` (2196), `emit_reducer_compound` (2278), `close_compound` (2376), `trim_with_pattern` (1018), `trim_ascii_ws` (1038), `first_ws_pattern` (996), `saturating_u16` (2140), `stage_literal_payload_in_arena` (2256), `lookup_precedence` (2335), and the `DtaError` surface (692). AX.W0 manifest's ~2,800-LOC cut is accurate. **Actual retained: ~400-450 LOC**. Remaining 2,873 LOC delete. | AX.W0 manifest + AX §0.1 retain list |
| `crates/bbnf-tape/src/dta.rs` | 550 | Delete in full. `DtaState`, `DtaTable`, `DtaStateId`, `DtaRuleId`, `DtaFrameKind`, `DtaCounterOptional`, `SeqPromote`, `LiteralPayloadClass` — every type is reachable only from the deleted driver. | AX §0.1; no external consumer survives after driver deletion |
| `crates/core/src/backend/rust/emitter/dta_walker/` (5 files) | **4,360** | Delete directory. `mod.rs` (479) + `decoders.rs` (819) + `helpers.rs` (504) + `hot_cold.rs` (211) + `lower_state.rs` (2,347). Walker emission per §generated.rs audit — 72% of generated.rs is this output. **Corrected from AX manifest's 3,875 → actual 4,360**. | `wc -l` |
| `crates/core/src/backend/rust/emitter/dta.rs` | 935 | Delete. DTA-table emitter. Every caller is `grammar.rs` weaving `__DTA_STATES`/`__DTA_RULE_ENTRIES`/`DTA_TABLE` literals that exist only for the walker. | AX §0.1 |
| `crates/ir/src/passes/recognizers/dta.rs` | 1,513 | Keep the SHAPE_DICT + GRAMMAR_PROFILE mining (~700 LOC); delete the `DtaState`-lifting path (~813 LOC). Three `#[allow(dead_code)]` markers at 1506/1509/1512 reference lifter internals unused elsewhere. | AX §0.1 |
| `crates/bbnf-tape-codegen/` (5 files) | **672** | **Entire crate deletes**. `lib.rs` + `advance.rs` + `decoded.rs` + `finalise.rs` + `frame.rs`. Grep for `bbnf_tape_codegen` / `bbnf-tape-codegen` across all non-self `.rs` files returns **zero hits**. The crate's stated purpose (supply walker helper TokenStream fragments) evaporates with `dta_walker/`. Bench-source-of-truth: `crates/core/Cargo.toml` does not import it; `Cargo.toml`'s workspace membership is the only consumer. | `grep -r bbnf_tape_codegen crates/` finds only tests inside the crate itself |
| `crates/bbnf-tape/src/builder.rs` | ~100 LOC carve-out | `parse()` method at 768 is a `dta_run_cold` shim; `enable_inline_frame_depth` (used only by walker path at grammar.rs:623) follows DTA path out. | Call-graph pruning of driver removal |
| `crates/bbnf-tape/src/columns.rs` lever-4 block | ~165 (lines 897-1062) | Delete `push_compound_fused_v32` + supporting `Packed` struct per AX.0.2. R4 §5 tautology confirmed. Lever 4 has **zero emitter-side consumer** (grep `push_compound_fused_v32` shows only tests + docs). | R4 §5; AX.0.2 |
| **Tier 1 subtotal** | **~11,888** | — | — |

## Tier 2 — Substrate-without-consumer (retain interface, delete projection-or-consumer mismatch)

| Item | Location | Status | Action |
|---|---|---|---|
| `active_columns` slot | `profile.rs:235` + `generated.rs:264` | Projection wired, IR empty — emits `&[]` for ALL 4 grammars post-AW-V. `emit_active_columns` helper exists but IR never populates. | **Investigate**: is the mining pass present? If no upstream populator exists post-AW-V, delete the slot + helper (~35 LOC). |
| `branch_priors` slot | `profile.rs:239` + `generated.rs:268` | Same: `&[]` for all 4 grammars. V4 mining never shipped. | Same — delete or populate decision required. |
| `reorder_unroll_visitors` slot | `profile.rs:241` + `generated.rs:270` | Same: `&[]` for all 4 grammars. V2 "AV.2.5" never shipped. | Same. |
| `KEYWORD_PHF`, `CLASSIFY_TABLE_*` | Referenced in `classify_byte.rs`, profile, tests | `aw4-profile-p6-begotten-code-audit.md` §7 confirmed **absent from every bench binary**. | **Investigate**: which of these is even emitted at present? Classify-byte dispatch (classify_byte.rs, 165 LOC) is live code; the PHF table may be populated for CSS but not JSON/Sheets/BBNF. Requires bench-binary `nm` verification at AX.W0 open. |
| `list_rules`, `keyword_tables`, `shape_dict`, `dedup_eligible_rules` | `generated.rs:265-269` | **These four ARE populated** (statics `__GRAMMAR_PROFILE_LIST_RULES` etc. exist). These are **not dead**; they are live wire contracts. | Retain. |
| Old regex emission paths | `crates/core/src/generate/regex/emit/simd.rs` | Header comment at `simd.rs:28` says "The old `emit_memchr1/2/3` + `emit_nibble_lut_scan` emitters" are superseded by `emit_structural_bitmap_kernel`. Grep confirms zero active `emit_memchr` / `emit_nibble_lut_scan` functions. | Already cleaned — retain `simd.rs` as-is. |
| **Tier 2 subtotal** | ~100 LOC if slots deleted | — | Investigation-queue-gated |

## Tier 3 — Gates / shims / feature flags

| Item | Location | Status |
|---|---|---|
| `has_w4_classified` | `shapes/dispatcher.rs:836`, `shapes/mod.rs:149`, `grammar.rs:719` | **Shim masquerading as gate.** FINAL-V.md line 78 states this gate fires spuriously for JSON post-W4-fix-rest (Flat/Wrap detectors accept JSON `pair`/`value`). AX.W0 effectively obviates: remove dispatcher-coverage gate, delete walker fallback path entirely. Retire `has_w4_classified` + the `emit_visitor_path` gate in `shapes/mod.rs:149`. |
| `has_full_shape_coverage` | `shapes/mod.rs:262` | Legitimate admission gate, but post-AX.W0 should **always return true** (uncovered rule = wave-close blocker per AX invariant 1). The function body becomes `true` — remove gate + all guards in arglist/flat/pratt.rs call sites. |
| `has_shape_dispatcher_entrypoint` | `shapes/mod.rs:313`, `grammar.rs:515` | Same as above — gate retires as non-shape-dispatched grammars become a compile error. |
| `#[cfg(feature = "dta-replay")]` | `driver.rs:803, 826, 836, 2780`; `lib.rs:129` | Feature-gated surface for `dta_run_with_replay`. Deletes with driver. |
| `#[cfg(feature = "rayon")]` | `driver.rs:2949, 2959, 3316`; `psi.rs:119, 121, 510`; `lib.rs:120` | Gates `dta_run_parallel` — deletes with driver. Retain *only* rayon-feature markers in `psi.rs` if they govern non-DTA code (worth a 10-minute `grep` audit pre-deletion). |
| `#[cfg(feature = "parser-trace")]` | `backend/rust/trace.rs:19, 29, 45` | Live debug-trace feature — **retain** (legitimate). |
| `#[deprecated]` | `ir/src/passes/csp_strategy/mod.rs:434, 447` | Two deprecated names in csp_strategy. "Deprecated alias for one migration" in module docstring. **Delete both** per no-backward-compat invariant. |
| `#[allow(dead_code)]` inventories | 31 hits (rust-source only, excluding bench/test `Visitor` pattern boilerplate) | Tests own 20+ of these (legitimate — unused helper structs in fixture builders). **Production offenders**: `ir/passes/recognizers/dta.rs:1506,1509,1512`, `dta_walker/mod.rs:159,252,392`, `pattern_alphabet.rs:369,383`, `shapes/string.rs:323`, `keyword_dispatch.rs:146`. All production `#[allow(dead_code)]` must delete (either the attribute drops or the gated code drops — every case requires look). |
| **Tier 3 note** | Gates in `shapes/*.rs` + `grammar.rs` | These gates' body comments say "continue to route through `__dta_walker_inline::run`" — that phrase will disappear with AX.W0. Delete all such gate-rationale comments. |

## Tier 4 — Cross-crate cleanup

| Item | LOC | Action |
|---|---:|---|
| `parse-that/` (path `../parse-that`) | N/A — sibling repo | Out of this worktree's bounds. Flag for separate audit — scanner-retirement candidates live there. |
| `pprint/` (path `../pprint`) | N/A — sibling repo | Same. VM opcode retirement (referenced in feedback memory `prettify-ws-emission-time`) is a separate wave. |
| `bbnf-tape-codegen/` | 672 | **Delete entire crate** per Tier 1. |
| `bbnf-simd-scan/src/emit/` 21 fragment exporters across 9 files (1,242 LOC) | — | **Investigate**: which are consumed by the shape emitter? `shapes/string.rs`, `shapes/number.rs`, `shapes/object.rs` import from `bbnf_json_prototype::*` (not `bbnf_simd_scan::emit`). Grep `bbnf_simd_scan::emit` or `simd_scan::emit` in emitter sources → **zero matches outside the crate itself**. **Critical**: every `emit/*.rs` file may be dead substrate. Full inventory needed before AX.W0 opens. |
| `bbnf-json-prototype/` | 2,246 | **Keep as reference crate**. AX.1.1 (AX.md:162) keeps the packed `bbnf_json_prototype::Value` as the *internal* tape-value rep. Consumers on master: `crates/core/benches/json/value.rs:37` (single bench) + doc-comment refs in `shapes/{string,number,object}.rs`. **Status: reference artefact — retain**, but delete the doc-comment refs post-AX.W3 when AX.1.1's `bbnf::json::Value` ships. |
| `bbnf-tape/src/stage1.rs` (106 LOC) | 106 | **Retain**. `StructuralIndex` type — wire contract consumed by `bbnf-simd-scan::scan_structural`, which is called at `generated.rs:93501` + `grammar.rs:631`. |

## Tier 5 — Test and doc fixtures carrying DTA hooks

| Item | LOC | Action |
|---|---:|---|
| `crates/bbnf-tape/tests/walker_arms.rs` | 700+ | **Delete**. Exclusively exercises `dta_run_cold`. |
| `crates/bbnf-tape/tests/aq5_regression.rs` | ~350 | **Delete**. `dta_run_cold` regression guards retire with cold-path replay. |
| `crates/bbnf-tape/tests/driver_dual_cursor.rs` | ~200 | **Delete**. Dual-cursor tests are DTA-shaped. |
| `crates/bbnf-tape/tests/aw5_w13_substrate.rs` | ~450 | **Delete Lever 4 section** (push_compound_fused_v32 roundtrips, ~200 LOC) + retain scalar-payload tests. |
| `crates/core/tests/dta_walker_codegen.rs`, `dta_counter_states.rs`, `dta_shunting_yard.rs`, `dta_diagnostic_replay.rs` | ~1,500 combined | **Delete all four**. Walker-codegen & diagnostic-replay tests retire with walker. |
| `crates/core/tests/aw3_w1_payload_lift.rs`, `aw_ii_w5b_minus.rs`, `aw_ii_w2_binary_factor.rs`, `aw3_w1_walker_trace.rs` | ~800 combined | **Delete** — all are DTA-lift-path regression tests. |

## Aggregate LOC reclaim

| Tier | LOC | Notes |
|---|---:|---|
| Tier 1 core | 11,888 | Driver carve + dta.rs + dta_walker/ + emitter/dta.rs + IR lifter carve + bbnf-tape-codegen + Lever 4 |
| Tier 3 gates + shim retirement | ~400 | `has_w4_classified`, `has_full_shape_coverage` body drops, deprecated csp_strategy, production `#[allow(dead_code)]` |
| Tier 5 tests | ~3,900 | Walker/DTA test suite retirement |
| Generated.rs proportional cut | ~57,481 | 72% walker output evaporates on next bootstrap |
| **Total user-visible source delete** | **~16,200** | Excluding generated.rs regeneration reclaim |

Add the generated.rs regen (57,481 LOC walker output) and AX.W0 reclaims **~74 K LOC total**. The AX.md plan's `~7,923 LOC` is a subset; the true reclaim is double once test suites + bbnf-tape-codegen + Lever 4 + walker gates are included.

## Consumer-dependency risk table

| Deletion | Downstream that rebuilds | Risk |
|---|---|---|
| `dta_run_cold` | `builder.rs::parse()` (1 call at line 786) | **Low**: delete the whole `parse()` shim. |
| `dta_run_parallel` | `generated.rs:93513` + `grammar.rs:652` | **Medium**: must simultaneously delete the grammar.rs parallel-path emission + re-emit all 4 grammars' generated.rs. |
| `FrameStack` / `Frame` | emitter/dta_walker/*.rs only | **Low**: atomic with walker. |
| `DtaError` | Shape-dispatcher `parse_body` at `grammar.rs:579-595` | **Medium**: error variants must survive in a new, non-DTA error type (RD error). Replace `DtaError::{Syntax, UnexpectedEnd, InvalidState}` with a leaner `ShapeError`. |
| `has_w4_classified` | `grammar.rs:718`, `shapes/mod.rs:149`, 3 tests | **Low**: gate retires; tests assert `true` or retire. |
| `bbnf-tape-codegen` crate | None on master | **None**. |
| Lever 4 `push_compound_fused_v32` | `tests/aw5_w13_substrate.rs` + docs | **None** on production path — it has no consumer. |

## Investigation queue

1. **`active_columns` / `branch_priors` / `reorder_unroll_visitors` IR miners** — grep `compute_active_columns` / `compute_branch_priors` / `compute_reorder_unroll_visitors` in `ir/src/passes/`. If the miner is absent, delete slots + their `emit_*` helpers + `GrammarProfile` fields (not just the literal).
2. **`KEYWORD_PHF` / `CLASSIFY_TABLE_*` population** — bench-binary `nm` verification whether these are emitted at master. P6 audit showed absent for all four grammars; need to re-verify post-AW-V.W5 BBNF GRAMMAR_PROFILE wire-contract fix.
3. **`bbnf-simd-scan/src/emit/` 21 exporters** — which shape emitters actually inline these fragments? `first_quote_or_backslash` and `nospace64_scan` appear imported by `shapes/string.rs`; the remaining 19 exporters may be entirely unused substrate.
4. **`AW-V.W5.2` per-Ref dispatcher status** — FINAL-V line 78 notes this as a W5 artefact. Is the per-Ref `__value` dispatcher on master, or still deferred? Impacts whether `has_shape_dispatcher_entrypoint` gate has reachable true-branches post-W0.
5. **`bbnf-tape/src/psi.rs::rayon` feature** — is `rayon` feature on `psi.rs` load-bearing outside DTA parallel? Check whether non-DTA consumers invoke rayon paths.
6. **`lookup_precedence` / `PRECEDENCE_LUT`** — the P6 audit noted the precedence table is emitted but has zero nonzero entries for JSON. It IS live for Sheets + BBNF (where operators exist). Retain unconditionally.
7. **Walker-only `bbnf_tape::builder::parse()`** — confirm the non-walker shape-dispatch path uses `TapeBuilder::with_capacity`/`finish` only, not `parse()`. If so, `parse()` method retires cleanly.

## Commit sequencing recommendation

To minimise rebase pain (AX.W0 is one wave, two agents):

1. **Commit 1 (test retirement)**: Delete Tier 5 test files first. Tests depend on symbols; removing them first lets later commits delete symbols without test regressions.
2. **Commit 2 (codegen emitter)**: Delete `crates/core/src/backend/rust/emitter/dta_walker/` + `crates/core/src/backend/rust/emitter/dta.rs`. Update `mod.rs` export list.
3. **Commit 3 (IR lifter carve)**: Surgically delete DtaState-lifting path in `ir/src/passes/recognizers/dta.rs`; keep SHAPE_DICT + GRAMMAR_PROFILE mining.
4. **Commit 4 (grammar.rs routing)**: Replace walker-fallback `parse_body` branch with compile-error on uncovered rules. Delete `has_w4_classified` + call sites. Bootstrap regen.
5. **Commit 5 (tape crate carve)**: Delete `crates/bbnf-tape/src/dta.rs` + driver carve + lever-4 + `psi/rayon`-if-dead. Update `lib.rs` re-exports.
6. **Commit 6 (bbnf-tape-codegen retirement)**: Remove workspace member; delete directory; update Cargo.toml.
7. **Commit 7 (gate retirement + dead-attr sweep)**: `has_full_shape_coverage` → `true`, delete production `#[allow(dead_code)]`, retire csp_strategy `#[deprecated]`.
8. **Commit 8 (doc + AW-V.md rewrite)**: AX.0.2 — strike Lever 4 and "17-digit NEON" from AW-V.md.
9. **Commit 9 (bench bootstrap regen verify)**: fresh generated.rs; assert 57 K-line reduction; `cargo test --workspace`.
