# Tranche BA — Direct-Projection Codegen

**Status**: planned. Opens after AZ-IV close + the 8-lane meta-audit hardening pass per `docs/HARDENING-AUDIT-PROMPT.md`. BA.W0 absorbs the 3 cleanup commits (18 zero-caller substrate DELETE + 3 module-cluster retirements + `merge_path_seed` decision) into the wave itself; there is no separate pre-BA cleanup phase.
**Base**: master `40e1835d` (post-Phase 1 plan-surgery).
**Letter discipline**: un-recycled at master `40092b28`. The previous BA tranche (typed pointer-path queries; absorbed into AZ-IV.W2) and the subsequently-recycled BA tranche (rule-discovery; routed to BB) are archived at `docs/tranches/BA/historical/recycled-rule-discovery/`. The canonical post-AZ-IV letter sequence is **AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+ (TS/WASM re-engineering)** per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`.

## Thesis

Direct-projection codegen restores the GESTALT §2 *direct-to-struct* invariant verbatim: every grammar rule's TypeDesc — annotated with `->` or inferred for `->`-less rules — reaches the emitter and produces a typed Rust struct/enum at codegen time. The parse fn writes directly to typed fields. The runtime arena/builder template registry indirection retires. The lazy parse path becomes canonical; eager is its degenerate case. `Document::get<T>(path)` mirrors sonic-rs's `pointer!` API with superior ergonomics: compile-time grammar-aware diagnostics, type-inferred return type, zero-allocation wildcard iterator.

For the project-level synthesis (architecture from first principles, SOTA union, generalization vision, fleet shape, measurement discipline) read `docs/GESTALT.md`. This file is the BA plan only.

## Active Contradictions / Architectural Defect

The post-AZ-IV deep audits (DEEP-A assay, DEEP-B samply profile, DEEP-C path-forward, DEEP-D reordering) converged on **one defect with three measurable manifestations**. Each maps to one or more carry-ledger rows below; closing BA resolves every contradiction.

1. **Type inference output is thrown away at the parse boundary.** `project_types` produces a complete `TypeDesc` for every rule (annotated or `->`-less); `populate_struct_registry` emits one `StructLayout` per Named compound rule. The emitter consumes `StructRegistry` for compound *layout decisions* but the *generated parse path still routes through* `FooStructBuilder::begin_compound(layout: &StructLayout)` with a runtime `StructRegistry::compound_kind_for_layout` resolution to determine the compound kind. Nine emission sites construct `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` — codegen-known type info is re-derived at runtime per leaf push.
2. **`SimpleStructBuilder::push_leaf_with_*` deposits `V::unit()` for 5 grammars** (BBNF, BNF, CSV, EBNF, Math), discarding typed leaf payloads. The arena is a heterogeneous slab even when typed projections exist.
3. **`Vec<OpenFrame>::clone` dominates samply** at 86.07% of inclusive samples (DEEP-B, 25,963 samples, fat-LTO `[profile.bench]`). `<JsonStructBuilder as StructBuilder>::checkpoint` deep-clones the in-flight stack on every speculative branch entry across `parse_wrap_*` byte-dispatch towers.
4. **The 18/19 AU floor BELOW status** is the same mechanism observed at the bench level: every typed builder requires checkpoint discipline; AU's flat per-grammar arenas had no equivalent. AF is a MASKED-DEFERRAL routed here.
5. **5.22× `bbnf_value_twitter` sonic-rs gap** is the same mechanism observed at the value-API level (clone cost on speculative branches).
6. **4196× `bbnf_get_twitter` sonic-rs gap** is mechanism #5 *plus* a second defect: `bbnf_get_*` calls eager `JsonParser::parse(input)?.get(path)` instead of routing to `parse_with`. The lazy substrate exists but the value API does not consume it (`feedback_no_orthogonal_codepaths` violation). F2 is a MASKED-DEFERRAL routed here.
7. **`Document::get<T>(path)` walks a materialized AST.** `parse(input)?.get(path)` is two operations; sonic's `pointer!` is one. The architectural inversion is not a constant-factor improvement — it is collapse to one codepath where eager is the degenerate case of lazy with `&EMPTY_PATH`.
8. **`__EAGER_EMPTY_PATH<Json,_>` cross-grammar literal** survives in `crates/core/src/grammar/generated/**` as a symptom of the eager/lazy split rather than a unified codepath.
9. **`crates/core/src/runtime/{arena_template,builder_template}.rs`** are the W5.3 dedup substrate. Once direct-projection lands, the per-grammar value-API hot path no longer threads through these templates; they retire.
10. **18 zero-caller substrates from W5.4** were whitelisted for AZ-IV close discipline; under direct-projection most disappear by mechanism (no runtime registry indirection ⇒ no callers needed). The substrate-audit denominator refreshes.
11. **`LegacyPath`/`LegacySegment` shim** across `runtime/{json,css_l4,sheets,bbnf}/parse_with.rs` was the AZ-IV.W3 bridge from runtime path types to the typed `path!`-resolved path. Under direct-projection's `Document::get<T>(path)` consuming `TypedPath` directly, the shim retires.
12. **`cursor.match_field` + `cursor.match_index` + `cursor.decide`** are three call points that all answer the same question against a parsed segment. They collapse into one `cursor.consult(&ParsedSegment)` call.
13. **Per-grammar `__path_plan` re-exports** (`pub use crate::path::cursor::{Decision, SegmentKind}`) exist in 4 generated modules; they should source from `crate::path::cursor` directly without the per-grammar indirection.

## Invariants

(BA-scoped; AZ-IV invariants persist + are extended.)

1. **Direct-to-struct.** Every compound-typed rule (annotated OR `->`-less) projects to a typed Rust struct/enum at codegen time. The emitter consumes `StructRegistry` output; runtime layout literals (`__layout: StructLayout { ... }`) are deleted from generated parse fns.
2. **One parse path.** Eager `parse(input)` collapses to `parse_with(input, &EMPTY_PATH)`. The cross-grammar `__EAGER_EMPTY_PATH<Json,_>` literal retires. `feedback_no_orthogonal_codepaths` is enforced by mechanism, not policy.
3. **Cheap checkpoints.** `Checkpoint` is a value (`(stack_depth, arena_count)`), not a clone. Speculative branches use predictive first-byte dispatch where alphabets are disjoint. `Vec<OpenFrame>::clone` is not in the samply top-3 on any production bench.
4. **Sonic-class `get`.** `Document::get<T>(path)` reroutes through `parse_with` for path-resolved leaves. The lazy lane is the value-API hot path; eager-then-walk is not. `JsonParser::get<T>(input, path)` is the same-harness sonic-class entry.
5. **Type inference is the source of truth.** `->`-less compound rules project the same as annotated rules; the annotation becomes a naming hint, not a typing hint. The `inverse-layout-audit` IR pass fails-closed when a compound-typed rule has no `StructLayout`.
6. **No legacy code.** Per the user: NO quick solutions, NO workarounds, idiomatic gestalt, KISS, ONE PATH. Stale eager/lazy split language, dead arena/builder templates, `LegacyPath` shim, `cursor.match_*` family, `__path_plan` re-exports, and `__EAGER_EMPTY_PATH` literals are wave-owned deletion targets.
7. **Substrate with consumer.** Every BA substrate (typed `<Grammar>Document` projection, predictive byte dispatch tables, `parse_with`-routed `get` API) is consumed in the same wave it lands. The permanent `substrate_audit.rs` test (landed at AZ-IV.W5) stays green at every wave close.
8. **Evidence closes gates.** No gate closes on API existence, grep-only runtime claims, disabled tests, or "consumer later" scaffolding. Samply 7-artefact contract per claim becomes the canonical close discipline (closes Audit-C F10 watchdog and environmental-gating debt).
9. **Failing-test census is canonical.** Workspace nextest 100% pass at every wave close, including the closure of AZ-IV's RED `ts_node_execute` and `substrate_audit` tests (the former via direct-projection's TS aggregate emit OR via explicit `#[ignore]` with named successor letter per the TS/WASM punt; the latter via W0 cleanup).

## Carry Ledger — AZ-IV Routed Items

| Carry | Source | BA destination | Close condition |
|---|---|---|---|
| F2 sonic-rs ≤ 5× | Audit-C MASKED-DEFERRAL | W4 | `bbnf_get_twitter` ≤ 5× `sonic_get_twitter` same-harness samply-attributed; samply 7-artefact contract preserved |
| AF AU floor 18/19 BELOW | Audit-C MASKED-DEFERRAL | W2 + W3 | `docs/benchmarks/post-BA.json` `floors.post-AU.rows_at_or_above` = 19/19 |
| F8 32 zero-caller substrates | Audit-C CHRONIC-RISK | W0 | `crates/ir/tests/substrate_audit.rs` GREEN; the 32 substrates are deleted-or-whitelisted with a documented rationale per row |
| F4 Tailwind regex_scan timeout | Audit-C CHRONIC-RISK | W2 OR W3 | direct-projection eliminates per-call layout-construction overhead; if profile evidence shows the regex_scan path resolves under direct-projection, F4 closes here; otherwise routes to BB rule-discovery as a path-shape rewrite candidate |
| F10 watchdog rows | Audit-C CHRONIC-RISK | W6 | zero watchdog rows in fat-LTO + bench-iter matrices on `post-BA.json` |
| F5 TS Node-execute | Audit-C MASKED-DEFERRAL | routes to BD (TS/WASM) per user punt | named successor letter at BA close; OR closes here if direct-projection's TS aggregate emit naturally fixes the W5.2 RED gate by mechanism |
| Speculative-checkpoint Vec-clone | DEEP-B 86.07% inclusive | W3 | `Vec<OpenFrame>::clone` not in samply top-3; speculative-checkpoint cost ≤ 14% inclusive (≥ 80% reduction per DEEP-B) |
| Inverse-layout-audit pass | DEEP-A | W1 | `cargo build` fails when a compound-typed rule has no `StructLayout`; named test fixture |
| `arena_template` + `builder_template` retirement | DEEP-A | W2 | `crates/core/src/runtime/{arena_template,builder_template}.rs` deleted |
| `LegacyPath`/`LegacySegment` retirement | DEEP-C | W5 | `LegacyPath`, `LegacySegment` deleted from `crates/core/src/runtime/path/**` |
| `cursor.match_*` collapse | DEEP-C | W5 | `cursor.match_field`, `cursor.match_index`, `cursor.decide` deleted; `cursor.consult` is the only call |
| `__path_plan` re-export retirement | DEEP-C | W5 | per-grammar `__path_plan` modules deleted; single source from `crate::path::cursor` |
| Sonic-class `get` API | DEEP-C | W4 | `JsonParser::get<T>(input, path)` lands; per-grammar parity; routes through `parse_with` |

## Non-Routable Carries

The 18 items below cannot route to a successor letter. BA closes inside these or BA does not close. A non-routable carry that survives close is a process failure (per `feedback_no_deferrals`).

| # | Item | Owner wave | Closure proof |
|---|---|---|---|
| 1 | Strict regen drift | W0 | `cargo xtask regen --check` green 9/9 at every wave close |
| 2 | 18 zero-caller substrates retired or whitelisted | W0 | `crates/ir/tests/substrate_audit.rs` GREEN; rationale per row in `docs/tranches/BA/audit/W0-substrate-audit.txt` |
| 3 | 3 module clusters retired | W0 | `generate/serialize/`, `generate/regex/phf.rs` survivor fold, `backend/strategy/` collocate; AST scan returns zero stale references |
| 4 | `merge_path_seed` decision | W0 | wired to canonical egraph saturation site OR deleted; named decision in W0 close commit body |
| 5 | Worktree fixture symlink contract | W0 | `data/{json,css,bbnf,sheets}` materializes on worktree open via `xtask worktree-init` or equivalent; documented contract |
| 6 | Inverse-layout-audit IR pass | W1 | named test fixture: `cargo build` fails when a compound-typed rule has no `StructLayout`; `->`-less rules covered |
| 7 | Per-grammar `<Grammar>Document` typed struct emit | W2 | generated `<Grammar>Document` for JSON/CSS L4/Sheets/BBNF; codegen-emit replaces runtime `__layout` construction |
| 8 | Per-grammar `<Grammar>Value` typed enum emit | W2 | `JsonValue`, `CssTypedValue`, `SheetsValue`, `BbnfValue` are generated artefacts (facade re-exports for back-compat at most one wave) |
| 9 | `arena_template` + `builder_template` deletion | W2 | files deleted; `git log --all -- crates/core/src/runtime/arena_template.rs` shows the deletion commit |
| 10 | `Vec<OpenFrame>::clone` retirement | W3 | not in samply top-3; ≥ 80% inclusive-sample reduction proved by samply 7-artefact contract |
| 11 | Predictive first-byte dispatch | W3 | JSON byte-alphabet disjoint check; generated dispatch table; named test for branch determinism |
| 12 | `Checkpoint = (stack_depth, arena_count)` | W3 | type alias replacing `Vec<OpenFrame>::clone`; named test for restore semantics |
| 13 | AU floor 19/19 at-or-above | W3 + W2 | `docs/benchmarks/post-BA.json` `floors.post-AU.rows_at_or_above` = 19/19 |
| 14 | `parse_with`-routed `Document::get<T>(path)` | W4 | `Document::get<T>` reroutes through `parse_with`; eager `parse` collapses to `parse_with(input, &EMPTY_PATH)`; named bench |
| 15 | Sonic-class `JsonParser::get<T>(input, path)` | W4 | per-grammar `<Grammar>Parser::get<T>(input, path!)` shape; same-harness sonic-class API |
| 16 | `__EAGER_EMPTY_PATH` literal absent | W4 | `rg __EAGER_EMPTY_PATH crates/core/src/grammar/generated/` returns zero hits |
| 17 | `LegacyPath` + `cursor.match_*` + `__path_plan` retirement | W5 | files/symbols deleted; `Document::get<T>` consumes `TypedPath` directly |
| 18 | Samply 7-artefact contract per claim canonical | W6 | every Hard Gate citing performance has a saved 7-artefact contract under `.profiles/samply/post-BA/`; environmental gating retires |

## Wave Table

| Wave | Agents | Closes on evidence | Status |
|---|---:|---|---|
| W0 - Truth, Regen, Cleanup Absorption | 5 parallel | strict regen 9/9 green; 18 zero-caller substrates DELETED or whitelisted; 3 module clusters retired; `merge_path_seed` decision; worktree fixture symlink contract codified | planned |
| W1 - Inverse-Layout-Audit IR Pass | 5 parallel | every compound-typed rule has a non-empty `StructLayout`; `project_types` integration; `cargo build` fails on un-inferred compound rule | planned |
| W2 - Direct-Projection Codegen | 6 parallel | per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum emitted from `StructRegistry`; `arena_template` + `builder_template` retired from value-API hot path; AU floor 18/19 BELOW closes | planned |
| W3 - Speculative Checkpoint Redesign | 5 parallel | `Vec<OpenFrame>::clone` no longer in samply top-3; `Checkpoint = (stack_depth, arena_count)`; predictive first-byte dispatch in JSON; ≥ 80% inclusive-sample reduction per DEEP-B | planned |
| W4 - `parse_with` As Value-API Hot Path | 5 parallel | `Document::get<T>(path)` reroutes through `parse_with`; eager `parse` collapses to `parse_with(input, &EMPTY_PATH)`; sonic-class `get` API lands; `bbnf_get_twitter` ≤ 5× `sonic_get_twitter` | planned |
| W5 - Cursor Consult + LegacyPath Retirement | 5 parallel | `cursor.match_field` + `cursor.match_index` + `cursor.decide` collapse into `cursor.consult(&ParsedSegment)`; `LegacyPath`/`LegacySegment` shim retires; `Document::get<T>` consumes `TypedPath` directly | planned |
| W6 - Measurement And Close | 3 parallel | AU floor 19/19 at-or-above; sonic-rs floor MET; samply 7-artefact contract per claim becomes canonical close discipline; FINAL.md cites resolving artefact for every Hard Gate | planned |

## Critical Files And Ownership

| Surface | Owner wave | Primary paths |
|---|---|---|
| Active plan truth + regen + substrate-audit refresh | W0 | `docs/GESTALT.md`, `docs/codegen-paths.md`, `docs/tranches/BA/**`, `xtask/src/regen.rs`, `crates/ir/src/registry/strategy.rs`, `crates/ir/tests/substrate_audit.rs`, `crates/core/src/grammar/generated/**` |
| Module-cluster retirement | W0 | `crates/core/src/generate/serialize/**`, `crates/core/src/generate/regex/phf.rs`, `crates/core/src/backend/strategy/**`, `crates/core/src/backend/driver/alt.rs` |
| Worktree fixture contract | W0 | `xtask/src/worktree_init.rs` (new), `data/**` symlink contract, `docs/tranches/BA/audit/W0-worktree-contract.md` |
| Inverse-layout-audit IR pass | W1 | `crates/ir/src/passes/inverse_layout_audit.rs` (new), `crates/ir/src/passes/mod.rs`, `crates/ir/src/passes/types/mod.rs` (modify-carve), `crates/ir/tests/inverse_layout_audit.rs` (new) |
| Direct-projection codegen — emitter | W2 | `crates/core/src/backend/rust/emitter/document.rs` (new), `crates/core/src/backend/rust/emitter/value_enum.rs` (new), `crates/core/src/backend/rust/emitter/shapes/**` (modify-carve) |
| Direct-projection codegen — generated outputs | W2 | `crates/core/src/grammar/generated/{json,css_l4,sheets,bbnf,bnf,csv,ebnf,math,css_pretty}/**` (modify generated) |
| Arena/builder template retirement | W2 | `crates/core/src/runtime/{arena_template,builder_template}.rs` (delete), `crates/core/src/runtime/{json,css_l4,sheets,bbnf,bnf,csv,ebnf,math,css_pretty}/{arena,builder}.rs` (delete where direct-projection subsumes) |
| Speculative checkpoint | W3 | `crates/core/src/runtime/checkpoint.rs` (new), `crates/core/src/backend/rust/emitter/dispatch.rs` (new — predictive first-byte), generated parse fns |
| `parse_with`-routed value API | W4 | `crates/core/src/runtime/{json,css_l4,sheets,bbnf}/document.rs` (modify-carve), `crates/core/src/runtime/{json,css_l4,sheets,bbnf}/parser.rs` (modify-carve) |
| Sonic-class `get` API | W4 | `crates/core/src/runtime/{json,css_l4,sheets,bbnf}/parser.rs` (`get<T>`, `get_iter`, `get_dyn` methods) |
| Cursor consult + LegacyPath retirement | W5 | `crates/core/src/path/cursor.rs` (modify-carve to add `consult`, delete `match_*`), `crates/core/src/runtime/path/**` (delete `LegacyPath`/`LegacySegment`), per-grammar `__path_plan` (delete) |
| Benchmark + profiling | W6 | `crates/core/benches/**`, `docs/benchmarks/post-BA.json` (per `docs/benchmarks/SPEC.md`), `.profiles/samply/post-BA/**`, `docs/tranches/BA/audit/W6-*.{txt,md,json}`, FINAL.md |

## Orchestration Rules

1. Max six agents per wave (W2 maxes at six; others are five or three).
2. Parallel writers use named sibling worktrees with distinct `CARGO_TARGET_DIR`; no parallel implementation or docs-writing agent writes in main.
3. The orchestrator records `git status --short`, staged paths, base commit, worktree list, and target dirs before dispatch.
4. Empty/null/no-evidence return is a failed dispatch: redispatch verbatim once with the same worktree pointer; a second empty/no-evidence return triggers the triumvirate.
5. Triumvirate means research, plan augment/synthesis, and redress. It is mandatory for scope reveal that invalidates file bounds, hard gates, or substrate-with-consumer wiring (per `docs/precepts/instructions/ORCHESTRATION.md` §Triumvirate Auto-Triggers).
6. Read-only agents do not commit at hard cap. Write-authorized agents commit at 0.9N only when the staged slice is clean and owned.
7. Before every commit, use the local `commit-discipline` skill: inspect dirty/staged state, preserve unrelated staged work, stage only intended paths, review `git diff --cached`, and stop if the slice cannot be isolated.
8. Broad, generated, deletion, benchmark, profiling, gate/status, and cross-repo commits require bodies naming why, what landed, evidence, and routed remainder. No AI/tool authorship.
9. Profiling agents may share one prepared absolute target only after preparation; no two cargo invocations run concurrently against the same target dir (per `feedback_single_cargo_per_target`).
10. Every dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt.` Defaults: research 20, plan 15, redress 30, audit 25.
11. **HARD CAP expansion on overrun.** If a wave's HARD CAP is exceeded by an in-flight write-authorized agent without commit, the orchestrator extends the cap (not split the work) and records the extension reason in `PROGRESS.md`.
12. Triumvirate auto-triggers (no user prompt required): JSONL transcript quiet >15 minutes, first-pass return with no commit and no evidence, three diagnostic-loop iterations without isolating root cause, or scope reveal that invalidates file bounds / hard gates / substrate-with-consumer wiring.
13. Sub-agent prompts must remain self-contained and stay within ~700 words of instructions; if a prompt grows larger, the task is mis-scoped and decomposes into sequential mini-units before dispatch.

## Hard Gates

1. `cargo xtask regen --check` 9/9 green at every wave close. Archived at `docs/tranches/BA/audit/W{N}-regen.txt` per wave.
2. `cargo nextest run --workspace --cargo-profile ax-iter` 100% pass at BA close. Closes the AZ-IV `ts_node_execute` and `substrate_audit` RED tests; the former via direct-projection projecting TS aggregates as iterables OR via explicit `#[ignore]` with named successor letter per the TS/WASM punt.
3. **Speculative-checkpoint cost ≤ 14% of inclusive samples** on `bbnf_value_twitter` samply trace (DEEP-B's ≥ 80% reduction met or exceeded). Same-harness fat-LTO `[profile.bench]`. Saved samply 7-artefact contract under `.profiles/samply/post-BA/`.
4. **`bbnf_get_twitter` ≤ 5× `sonic_get_twitter`** same-harness, samply-attributed. F2 MASKED-DEFERRAL closes here.
5. **AU floor 19/19 at-or-above** on `docs/benchmarks/post-BA.json` `floors.post-AU.rows_at_or_above`. AF MASKED-DEFERRAL closes here.
6. `Vec<OpenFrame>::clone` site does not appear in samply top-3 hotspots on any production bench.
7. `__EAGER_EMPTY_PATH` cross-grammar literal absent from `crates/core/src/grammar/generated/**` (`rg __EAGER_EMPTY_PATH crates/core/src/grammar/generated/` returns zero hits).
8. `crates/core/src/runtime/{arena_template,builder_template}.rs` deleted.
9. `crates/core/src/runtime/{json,css_l4,sheets,bbnf,bnf,csv,ebnf,math,css_pretty}/{arena,builder}.rs` deleted (where direct-projection subsumes them).
10. `LegacyPath` and `LegacySegment` deleted from `crates/core/src/runtime/path/**` (the W3 shim).
11. `cursor.match_field`, `cursor.match_index`, `cursor.decide` deleted; `cursor.consult` is the only call.
12. Per-grammar `__path_plan { pub use crate::path::cursor::{Decision, SegmentKind}; }` re-exports deleted; single source from `crate::path::cursor`.
13. **Type inference covers every `->`-less compound rule** — BA.W1 inverse-layout-audit pass green; named test fixture covers a `->`-less Seq, Alt, Repeat, and HeterogeneousAltJoin.
14. `Document::get<T>(path)` consumes `TypedPath<G, T>` directly; no `LegacyPath` lowering.
15. `JsonParser::get<T>(input, path)` (and per-grammar equivalents) — sonic-class API lands and routes through `parse_with`.
16. `path!` proc-macro return type is type-inferred from path's terminal TypeDesc (no turbofish required for the common case).
17. Wildcard `JsonParser::get_iter(input, path!(..., "*", ...))` returns a zero-allocation iterator (dhat-verified).
18. **Substrate-audit test green at every wave close.** `crates/ir/tests/substrate_audit.rs` enumerates every `pub` substrate; the 32 zero-caller substrates from W5.4 are deleted-or-whitelisted at W0.
19. **Samply 7-artefact contract per claim** per `docs/instructions/PROFILING.md`; environmental gating retires.
20. `docs/benchmarks/post-BA.json` lands per `docs/benchmarks/SPEC.md` schema; `floors` block compares row-by-row against `post-AU.json` and `post-AZ-IV.json`; `competitors` block carries same-harness sonic-rs / lightningcss / simdjson rows.
21. **`merge_path_seed` decision documented**: wired to canonical egraph saturation site OR deleted. Named decision in W0 close commit body; deletion is preferred unless BB.W1 wants the rules as seed bag.
22. **Worktree fixture symlink contract codified**: `data/{json,css,bbnf,sheets}` materializes on worktree open via `xtask worktree-init`; W6.2 known miss closes.
23. `cargo fmt --all -- --check` and focused `cargo clippy --profile ax-iter` pass.
24. `git diff --check` passes; FINAL.md cites resolving artefact for every gate, miss, deletion, and handoff.

## Deletion Bias

BA deletes before adding. Forbidden patterns in the BA diff:

- no `*_v2` modules, no compatibility feature flags
- no per-grammar `from_rule_name` arm-list (T1 transposition was AZ-IV.W4.4; BA inherits)
- no runtime `__layout: StructLayout` literal at parse-fn entry (replace with codegen-emitted typed projection)
- no `Vec<OpenFrame>::clone` on speculative branch entry (replace with `(stack_depth, arena_count)` value-typed checkpoint)
- no two parse codepaths (eager + lazy collapse to one)
- no `Option<&mut PathCursor>` parameter (cursor mandatory; eager passes `&mut PathCursor::eager()`)
- no `__EAGER_EMPTY_PATH<Grammar, _>` cross-grammar literal in generated code
- no `arena_template` or `builder_template` import from generated modules
- no `LegacyPath` / `LegacySegment` lowering in `Document::get<T>`
- no `cursor.match_field` / `cursor.match_index` / `cursor.decide` call (collapse to `cursor.consult`)
- no `__path_plan` per-grammar re-export
- no Python binding path (Python is dropped from the thesis per Q-final-4)
- no TS or WASM emitter regression compensation (TS/WASM punt is honored; W5.2 RED gate routes to BD)

If deletion is unsafe because a current consumer exists, the wave must name the consumer and refactor the surface to match its real role.

## Cross-Tranche Debt

- **BA opens after AZ-IV close + the 8-lane meta-audit hardening pass** (handoff at `docs/HARDENING-AUDIT-PROMPT.md`); BA.W0 absorbs the 3 cleanup commits as its first scope items rather than as a separate pre-BA phase.
- **BB opens after BA closes** with the rule-discovery scope (the original BB scope, un-subsumed; identical to the recycled-BA plan that lives at `docs/tranches/BA/historical/recycled-rule-discovery/`).
- **BC opens after BB closes** as the cleanup pass (Audit-A's TRANSPOSE bucket + AUDIT-B's routed splits + worktree fixture symlink contract finalisation + samply 7-artefact contract canonicalization + cross-repo discipline).
- **BD+ reserved** for TS/WASM re-engineering or shared-ABI tranche (per user punt).

If a non-routable item cannot land inside BA without changing the BA thesis, the response is a triumvirate review of the thesis — not a new tranche letter.

## TS / WASM Position

The user explicitly punted: *"Ignore our TS and WASM backends for now, these are not relevant and will likely need to be fully re-engineered at some point (or can we leverage a shared ABI?)."*

BA scopes to Rust only. TS and WASM backends are not load-bearing for direct-projection. Three options for the future tranche:

- **Option 1 — `wasm-bindgen-shared`**: works, but binds to Wasm runtime semantics and pays the JS-bridge marshalling cost the W5.2 Node-execute test surfaced.
- **Option 2 — `abi_stable`**: stable Rust ABI for plugin-style cross-crate use; not a TS bridge per se.
- **Option 3 — Custom IR-based ABI**: emit a flat byte-encoding of the typed IR + per-grammar reader. Both Rust and TS read the same encoding. The encoding becomes the contract; no marshalling. Closes the W5.2 RED gate by mechanism.

Decision deferred to the post-BC tranche (BD candidate). The deep audits do not select an option; the user requested explicit punt and that is honored.

## Brittleness Window

No tranche-wide brittleness window is declared. A wave may declare a local brittleness window only in its wave spec, with suspended gates, restoration wave, and reason. BA cannot close while any brittleness window is open.
