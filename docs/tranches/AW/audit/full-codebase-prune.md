# Full-Codebase Prune Audit (non-DTA remainder)

## Angle headline

**The post-DTA/post-V codebase is mostly live wire; the pre-DTA skeletons have been steadily pruned (Y.1/Y.2/Y.4, AE.2, AF.1, AQ.5, AM.1, AK.0). What remains for W0b beyond `dead-code-manifest.md` + `psi-and-dead-substrate.md` is small but real: ~1.8 K LOC across four confirmed-dead surfaces (bbnf-simd-scan fragment exporters, DTA-referring IR statics, deprecated csp_strategy aliases, walker-only emitter helpers), plus ~2–4 K LOC in five investigation queues whose verdict depends on whether AW-V's shape-dispatch entrypoint retires the Rust walker fallback in W0b or W1.**

Verdict shorthand: **1** = hard delete in W0b, **2** = investigate at wave open, **3** = retain (live wire). Every row is grep-verified against master HEAD `0f69e08d` and not duplicated from the two prior manifests.

## Per-crate prune manifest

### crates/bbnf-simd-scan/src/emit/ — 1,344 LOC, **Tier 1**

| File | LOC | Pre/Post | Tier | Evidence |
|---|---:|---|---|---|
| `emit/clmul_parity.rs` | 134 | Post-DTA (AV era) | 1 | `fragment_aarch64` / `fragment_x86_64` — no `bbnf_simd_scan::emit::*` imports anywhere in workspace. Only consumer is `bbnf-simd-scan/tests/emit_fragments.rs`. |
| `emit/eisel_lemire_body.rs` | 283 | Post-DTA | 1 | Five exported `fragment_*` functions; zero non-test consumers. The actual eisel-lemire splice in `dta_walker/lower_state.rs:615` inlines `parse_that::parsers::eisel_lemire::compute_f64` directly, not the fragment module. |
| `emit/first_quote_or_backslash.rs` | 219 | Post-DTA | 1 | Three fragment fns (neon/avx2/scalar); zero consumers. Shape emitters call `parse_that::parsers::scan::quoted_simd::scan_quoted_string_simd` directly. |
| `emit/multi_cmp_scan.rs` | 125 | Post-DTA | 1 | Two fragment fns; no consumer. |
| `emit/nibble_lut_scan.rs` | 146 | Post-DTA | 1 | Two fragment fns; the old `emit_nibble_lut_scan` regex path is superseded by `emit_structural_bitmap_kernel` (comment at `generate/regex/emit/simd.rs:28`). |
| `emit/nospace64_scan.rs` | 224 | Post-DTA | 1 | Four fragment fns; no consumer. |
| `emit/quoted_string_simd_body.rs` | 114 | Post-DTA | 1 | One fragment fn; no consumer. |
| `emit/shift_xor_parity.rs` | 57 | Post-DTA | 1 | One fragment fn; no consumer. |
| `emit/tzcnt_compact.rs` | 68 | Post-DTA | 1 | One fragment fn; no consumer. |
| `emit/mod.rs` | 74 | Post-DTA | 1 | Module declaration — deletes with contents. |

**Subtotal: 1,344 LOC.** The PSI-and-dead-substrate manifest flagged this; this audit confirms it. Delete the entire `emit/` directory and its doc-claims in `emit/mod.rs:3-5` (which promise fragments will be spliced into shape emitters — a promise never wired).

### crates/ir/src/passes/csp_strategy/ — ~20 LOC, **Tier 1**

| Item | Line | Pre/Post | Tier | Evidence |
|---|---:|---|---|---|
| `#[deprecated] solve_strategy_and_materialization` | `mod.rs:434-442` | Pre-DTA (AF.3) | 1 | Deprecated alias; zero non-deprecated callers in workspace. Retire per no-backward-compat. |
| `#[deprecated] solve_strategy_decisions` | `mod.rs:447-453` | Pre-DTA (AB.1) | 1 | Same — deprecated alias. |
| `#[allow(deprecated)]` at `passes/mod.rs:53` | — | — | 1 | The suppressor that exists only to re-export the two above. Deletes alongside. |

### crates/ir/src/passes/recognizers/ — ~20 LOC retention prep, **Tier 1 embedded in dead-code-manifest**

dta.rs lifter `#[allow(dead_code)]` at 1506/1509/1512 are already listed in the prior manifest. `pattern_alphabet.rs:369, 383` carry two additional `#[allow(dead_code)]` markers NOT in prior manifests — grep confirms the two gated items (`collect_pattern_alphabet` helper internals) are used only by tests. **Tier 2** — investigate whether the tests are still meaningful (shape_dispatch superseded pattern_alphabet for three of the four grammars post-AW-V); retire tests + gated helpers together.

### crates/core/src/backend/rust/emitter/ — walker-only helpers

| File | LOC | Pre/Post | Tier | Evidence |
|---|---:|---|---|---|
| `keyword_dispatch.rs` — the `#[allow(dead_code)]` at line 146 | ~30 LOC block | Post-DTA | 2 | The suppressed item is a PHF diagnostic helper. Shape-dispatch KeywordShape uses a different path. Investigate: is any PHF path active for the JSON grammar post-W0b? If not, delete. |
| `classify_byte.rs` | 165 | Post-DTA | 2 | Full module. Emits `CLASSIFY_TABLE_*` + `classify_byte` fn for walker arms. P6 audit row 7: absent from all four bench binaries. On W0b walker deletion, this emitter's output evaporates — module deletion is consequence, not separate action. |
| `precedence.rs` | 181 | Post-DTA | 3 | Live — `PRECEDENCE_LUT` emitted and consumed (P6 audit §7) for CSS/Sheets/BBNF. Retain. |
| `profile.rs` | 454 | Post-DTA | 2 | Many slots dead per psi-and-dead-substrate manifest. The emitter functions `emit_active_columns` / `emit_branch_priors` / `emit_reorder_unroll_visitors` / `emit_dedup_eligible_rules` / `emit_payload_bytes_per_input_byte` / `emit_expected_ns_per_byte` (~110 LOC cumulative) delete with their slot consumers. |
| `dfa_codegen.rs` | 380 | Post-DTA | 3 | Live — `emit_dfa_inline_body` is called from both `shapes/string.rs` path AND walker `lower_state.rs:895, 1802`. Walker portion deletes, but `emit_regex_scan_adapter` + `regex_scan_adapter_ident` survive for non-walker shapes. Keep module, trim if shape path narrows. |
| `visitor.rs` | 360 | Post-DTA | 2 | Emits `pub fn <rule>_visitor_*` kernels per `VisitorDescriptor`. Consumers: `core/tests/visitor_reduce.rs` + `visitor_reorder.rs`. **No generated.rs consumer, no hot-path invocation.** This is the AV-era "reduce kernel" substrate that the reduce_sum_of_f64 example documents. Investigate: is any grammar's `@visitor` directive populating `VisitorDescriptor` at present? If not — substrate-without-consumer, ~360 LOC delete. |

### crates/ir/src/passes/lr.rs (direct/indirect LR) — 318 LOC, **Tier 3**

Consumed by `pipeline/compile.rs:479-483` + tests (`optimize.rs`). Live.

### crates/ir/src/passes/prefix.rs — 482 LOC, **Tier 3**

`factor_common_prefixes` is called at `pipeline/compile.rs:560`. Live.

### crates/ir/src/passes/transform/ — 1,500 LOC total, mixed

`hoist_recurring_patterns` (pattern_dedup) — live (pipeline:581). Other modules (`alias`/`fuse`/`fuse_token`/`inline`/`optimize`/`prune`) — **Tier 2**, investigate: none show as pipeline consumers in the grep sweep. Some may be called transitively through `transform::optimize::optimize_grammar` as an entry point. Requires targeted trace; likely ~500–800 LOC of deferred-era IR transforms waiting on a consumer.

### crates/ir/src/vm/ — 1,782 LOC, **Tier 3**

Pre-DTA era "interpreter" for the debug adapter. Consumed by:
- `gorgeous/src/vm.rs` — `bbnf_ir::interpreter::Value` (format_value runtime pretty-print path)
- `lsp/src/dap/adapter.rs` — `bbnf_ir::bytecode::BytecodeProgram`, `Interpreter`, `DebugState`, `DebugAction`, `StepMode`
- `analysis/state/diagnostics/ir_analysis.rs` — via `try_compile_ir` in the LSP hover path

**Verdict: live** — forms the DAP/LSP diagnostic substrate. This is the AX.W9+ "debug trace" infrastructure.

### crates/core/src/backend/wasm/ — 1,411 LOC, **Tier 3**

Consumed by pipeline compile.rs (WASM emission target) + two benches. Live.

### crates/core/src/backend/ts/ — 1,543 LOC, **Tier 3**

Consumed by pipeline/compile.rs:189-190 (TypeScript emission target). Live.

### crates/analysis/ — 5,961 LOC, **Tier 3**

Every module has LSP-side consumers in `crates/analysis/src/features/*` paired with `server/protocol.rs` at `crates/lsp/`. The one-and-only production `#[allow(deprecated)]` at `features/document_symbols.rs:6-7` is an upstream `ls-types 0.0.3` constraint, not internal dead code — retain until the ls-types dependency is bumped.

### crates/gorgeous/ — 887 LOC, **Tier 3**

jit.rs + vm.rs both consumed. The per-grammar thin stubs (bbnf.rs, bnf.rs, css.rs, ebnf.rs, google_sheets.rs, json.rs) are 13–21 LOC each and serve as CLI grammar registrations. Live.

### crates/egraph/ — 1,871 LOC, **Tier 3**

Every module surface has a consumer — egraph.rs, unionfind.rs, extract.rs, rewrite.rs, scheduler.rs, csp_scheduler.rs, analysis.rs, cost_weights.rs, cost_config.rs, eclass.rs, id.rs, language.rs all have live consumers in bbnf-ir (`write_back_optimized`, `build_and_saturate`, `GrammarCostModel`, `CspScheduler::from_config`). `BackoffScheduler` is test-only but provides the baseline-scheduler semantics for the test matrix — retain.

### crates/csp-solver/ — 3,172 LOC, **Tier 2/3 mixed**

- `src/puzzles/*` (~350 LOC — sudoku + futoshiki). Vendored from csc411 per Cargo.toml comment. **Tier 2**: investigate whether dog-fooding puzzle tests are load-bearing for CSP correctness guarantees, or if they're sample-code that could move to examples. Every puzzle is `#[cfg(test)]`-equivalent via the `crates/csp-solver/tests/solver.rs` sudoku path.
- `src/py.rs` (405 LOC). Gated behind `feature = "py"` that is NEVER enabled by the bbnf-lang workspace. Upstream csc411 consumes. **Tier 2**: should the feature-gate retire in the workspace, or does csc411 still vendor-upstream this file? (Orchestrator question.)
- `src/solver/{local_search,backjump,nogoods,optimize,gac_alldiff,ac3,monotonic,propagate}.rs` — all consumed by the exported Csp API; grep-verified every solver module has test or internal consumer.

### crates/lsp/ — 1,667 LOC, **Tier 3**

dap/ (948 LOC total) + server/ (793). Live.

### crates/ser/ — 530 LOC, **Tier 3**

Grammar-guided serializer. Live.

### crates/derive/ — 340 LOC, **Tier 3**

Proc-macro — consumed by every `#[derive(BbnfBootstrap)]` grammar attribute.

### crates/bootstrap/src/bin/ — 416 LOC, **Tier 3**

`cost_grid_sweep`, `debug_parse`, `dump_ir` — three dev-only binaries. All referenced in recent tranche docs (`post-AW-IV-W5.json`, PROGRESS logs). Live infra.

### ../parse-that/ patched dep — 14,288 LOC, **Tier 2 pockets**

Two candidates:
- `parsers/css/{declaration,media,mod,selector,specificity,types,value}.rs` — 1,625 LOC. Pre-DTA legacy (AE-era "standalone parser" prototypes). Grep `parse_that::parsers::css` across workspace → zero hits. **Tier 2** (out-of-worktree, flag for parse-that orchestrator). Deletion requires upstream discussion.
- `parsers/csv.rs` (wc lookup) + `parsers/json.rs`. json.rs is consumed by the competitors bench only; csv.rs grep shows zero workspace consumer. **Tier 2** (parse-that orchestrator decision).

### ../pprint/ patched dep — 3,343 LOC, **Tier 3**

`op.rs` — 18 FmtOp variants, all used by the FmtBuilder API and generated prettify code. Spot-check at `generated.rs:86387` (prettify path) shows FmtBuilder + FmtOp actively emitted. No dead opcodes identified.

## Total reclaim beyond existing W0b scope

| Bucket | LOC | Confidence |
|---|---:|---|
| bbnf-simd-scan/src/emit/ (9 modules + mod.rs) | 1,344 | High — grep-verified zero consumers |
| csp_strategy `#[deprecated]` aliases + suppressor | ~20 | High |
| emitter/profile.rs dead-slot emit helpers (6 fns) | ~110 | Medium — depends on psi-and-dead-substrate slot deletion |
| visitor.rs (if no grammar @visitor directive active) | ~360 | Investigate |
| transform/ orphan entries (alias/fuse/inline/optimize/prune) | ~500–800 | Investigate |
| pattern_alphabet.rs gated internals | ~30 | Investigate |
| **Total W0b-deliverable delete** | **~1,864** | (confirmed) |
| **Total W0b + investigation reclaim** | **~2,400–3,000** | (pending traces) |

## Feature-flag + cfg-gated cleanup

- **`feature = "py"` on csp-solver** — never enabled in workspace. Investigation queue.
- **`feature = "rayon"` on bbnf-tape** — retires with DTA driver (already in dead-code-manifest).
- **`feature = "dta-replay"` on bbnf-tape** — retires with DTA driver (already in dead-code-manifest).
- **`feature = "vm"` on gorgeous** — live; retain.
- **`feature = "dhat-heap"` on core** — dhat profiling; retain for future heap profiling.

## Full-crate deletion recommendation

Beyond `bbnf-tape-codegen` (already in dead-code-manifest):

- **`crates/bbnf-json-prototype`** — flagged as "reference artefact" in dead-code-manifest. If AX.1.1's `bbnf::json::Value` lands in AX.W3, the crate can retire; until then retain.
- **No other full-crate deletion candidates identified.** All remaining crates have at least one live consumer.

## Investigation queue (pre-W0b orchestrator questions)

1. **`emitter/visitor.rs`**: Does any bench grammar on master exercise `@visitor` directive today? `nm` target/release/deps/<bench> for `*_visitor_*` symbol — absent → delete module + `VisitorDescriptor` IR type.
2. **`passes/transform/{alias,fuse,inline,optimize,prune}`**: Trace call graph from `pipeline/compile.rs`. Anything only reachable through `optimize_grammar` entry that is itself called by nothing? Deletion candidate.
3. **`pattern_alphabet.rs:369,383 #[allow(dead_code)]`**: The helpers are unused in production; grep confirms tests-only consumers. Should the tests stay, or have they been subsumed by shape_dispatch tests?
4. **`crates/csp-solver/src/py.rs` + `feature = "py"`**: Orchestrator question — does csc411 still vendor-upstream this file? If not, retire feature + file.
5. **`parse-that/parsers/css/*` 1,625 LOC**: Zero workspace consumer. Out-of-worktree — flag to parse-that orchestrator for retirement.

## Commit sequencing within W0b

(Non-interfering with the 9-commit sequence in dead-code-manifest.)

- **Commit 7-bis (this audit's additions)**: Delete `bbnf-simd-scan/src/emit/` + update `bbnf-simd-scan/src/lib.rs` `pub mod emit;` export + delete `tests/emit_fragments.rs`. Atomic.
- **Commit 7-ter**: Delete csp_strategy `#[deprecated]` aliases + `#[allow(deprecated)]` suppressor in `ir/passes/mod.rs:53`.
- **Commit 7-quater**: After profile.rs dead slots land (Commit 7 sequencing), delete the six `emit_*` helpers for dead slots.

Investigation-queue items produce **their own commits only after orchestrator verification**; they do not block W0b close.
