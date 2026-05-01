# AZ-IV — Canonical Activation, Grammar Generality, Path/Value API, Test Redress

AZ-IV is the union tranche. It absorbs the AZ-III carry burn-down, every overfit-elimination and substrate-activation item the third hardening pass surfaced, the typed path/query product (formerly BA), the lazy bail-out parse + sonic-rs same-harness floor (formerly BA stretch), the value-API and per-grammar projection consolidation (formerly BB perf items), and a complete failing-test redress. The post-AZ-IV residual is one tranche of pure rule-discovery work; the BA letter is recycled for that.

## Thesis

The system already chose the right architecture: grammar-derived Rust struct graphs are the materialized parse form, `cargo xtask regen` is the canonical Rust generation path, and CSP/egraph/shape/Pratt/regex/SIMD/view substrate exist to make that path general and fast. AZ-IV closes the remaining gap by consuming or deleting every existing substrate, eliminating every overfit, landing the typed compile-time `path!` macro and the path-driven lazy recognizer, executing the TS binding to parity, and redressing every failing test. No parallel parser, no shadow path system, no unconsumed substrate, no chronic deferral — every named carry closes inside this tranche or AZ-IV does not close.

For the project-level synthesis (architecture from first principles, SOTA union, generalization vision, fleet shape, measurement discipline, instruction-layer discipline) read `docs/GESTALT.md`. This file is the AZ-IV plan only.

## Active Contradictions (motivation)

These eleven contradictions between the chosen architecture and the actual code are why AZ-IV exists. Each maps to one or more carry-ledger rows below; closing AZ-IV resolves every contradiction.

1. BA's typed `path!` macro and lazy bail-out parse were planned but never landed; the current `Path<'a>` is an untyped slice. Eager-then-walk lazy lane is 2953x slower than sonic-rs `get`. `parse()` materializes the full tree before path resolution.
2. `xtask regen` is canonical, yet strict regeneration is red for most manifest grammars.
3. CSP decisions exist, but selected engines/layout/dispatch choices still pass through sidecars or independent emitter ladders.
4. Rewrite/ruler storage exists, but loaded rules do not drive production egraph saturation/codegen — `RuleSet` is loaded into an `eprintln`-only sink.
5. DTA is live as a fact source, but comments still describe walker/tape runtime emission. `dfa_codegen.rs` is the regex-scan adapter, not a DFA codegen module.
6. Runtime CSS color has a current typed model, but `backend/rust/view/color.rs` (290 LOC, zero production consumers) still ships as a shim.
7. TS backend builds, but tests prove only string-presence; no Node-execute proof; no TS template-tag binding for `path!`.
8. Sibling repositories still carry derive (parse-that bootstrap + 2 Cargo.lock entries), csp-solver canonical-source split (bbnf-lang vs csc411 with 22 shared files diverging), npm staleness, and docs-sync drift.
9. Per-grammar arena/builder pairs (9 of them) re-encode the rule vocabulary the `StructRegistry` already knows; `from_rule_name(&str) -> Kind` impls + `(layout.kind, rule_name)` builder dispatches violate grammar generality.
10. Substrate-with-consumer rule is declared but unenforceable; 5 WIRED-NOT-CONSUMED + 3 DEAD substrates surfaced by Babbage's third-pass audit.
11. 118 failing tests at AZ-III close (workspace nextest 92.1 % pass); 36 `#[ignore]`d tests; the failure backlog has compounded across 4 tranches without close-discipline enforcement.

## Invariants

1. **One parse path**: generated StructDirect parsers are the Rust runtime path. Tape, bootstrap parser, derive-generated parser, and DTA walker fallbacks stay retired. Two parse modes live on this path — eager (full-tree materialization) and lazy (path-driven bail-out); they share generated code and differ only in entry-point dispatch.
2. **Grammar generality**: parser binding, shape dispatch, regex payloads, typed projections, and backend strategy selection derive from grammar/manifest facts, not literal parser names or JSON-family assumptions. A static AST scan enforces no-grammar-name-branch in production runtime.
3. **Substrate with consumer**: rewrites, ruler, CSP decisions, regex HIR/egraph facts, shape dictionaries, structural scan, Pratt metadata, and views either change generated/runtime behavior in the same wave or are deleted. The substrate-audit test is permanent (`crates/ir/src/passes/tests/substrate_audit.rs`) and CI-gated.
4. **Semantic parity is type-inference driven**: `TypeDesc`, `StructRegistry`, obligations, grammar facts, and generated projection tables define parity. Hand-coded normalizers, rule-name dispatch tables, host-shim duplicates, and synthetic payload defaults are supplementary diagnostics, not parity proof.
5. **Semantic parity is current**: JSON uses sonic-rs as oracle, CSS uses lightningcss, Sheets uses the full parity corpus, BBNF self-host uses generated BbnfBootstrap, and TS is executable rather than string-checked.
6. **Direct struct projection must perform**: parse-only speed is not enough. StructDirect document/value/path projection rows must beat or match same-harness competitor rows where a competitor exists, especially sonic-rs JSON value/path access.
7. **Path is grammar-typed at compile time**: `path!(Json, "statuses", 0, "text")` resolves at compile time against the grammar's `StructRegistry` produced by `project_types`; an invalid path fails to compile with a grammar-aware diagnostic; runtime path errors do not exist for compiled paths.
8. **Path resolution uses source rule names**: the `path_check` IR pass runs after `project_types` and re-resolves inlined paths through the inline-trace sidecar. A path that names a rule the user wrote always resolves, even when post-pipeline transformations inline it.
9. **Lazy parse is a parse mode**: `JsonParser::parse_with(input, &path)` runs the path-driven recognizer that skips subtrees the path does not visit. Lazy mode silently elides parse errors past the path's reach (the contract); eager mode reports all parse errors. Same `Option<T>` return semantics; mode choice is parse-time.
10. **Wildcard returns lazy iterators**: `path!(..., "*", ...)` returns `Iter<Item = T>` with no allocation; `.with_anchors()` yields `(Path<'_>, T)` for re-anchorable usage; `.collect()` materializes if the caller wants.
11. **Variant-selection path step**: when the typed value is a sum, a name-keyed step selects the variant per grammar `->` annotation. `path!(CssL4, ..., "value", "color")` returns `Option<&CssColor>`.
12. **No legacy code**: stale DTA/walker/tape wording, dead functions, compat exports, no-op emitter hooks, and fallback-to-JSON surfaces are wave-owned deletion/refactor targets.
13. **No silent fallback**: no production code path swallows a malformed substrate path, missing rule, unrecognised parser ident, or unknown grammar by routing into a default builder, default discriminant, or per-rule allowlist. Failure is a `panic!` with a named binding string at construction time, not a runtime divergence.
14. **Evidence closes gates**: no gate closes on API existence, grep-only runtime claims, disabled tests, or "consumer later" scaffolding.
15. **Failing-test census is canonical**: workspace nextest is 100 % pass — fail-count zero, ignore-count justified per spec. Every `#[ignore]` carries an owner, deadline commit, and reason; tests that cannot be fixed inside AZ-IV are deleted with a per-test commit-body justification (per `commit-discipline` and the W1 redress workflow).

## Carry Ledger — AZ-III + BA + BB Absorption

| Carry | Source | AZ-IV destination | Close condition |
|---|---|---|---|
| Strict regen drift (7/9 grammars red) | AZ-III C1 | W0 - Truth And Canonical Regen | `cargo xtask regen --check` green live for 9/9 |
| Egraph `Map { fn_id }` preservation | AZ-III C2 | W0 - Truth And Canonical Regen | extraction preserves typed wrapper; named test fails before / passes after |
| Sheets parity gap (115/133 regression) | AZ-III C3 | W1 - Grammar Generality + Test Redress | full Sheets parity GREEN from regenerated tempdir output; 133/133 |
| TS discriminated union test | AZ-III C4 | W1 - Grammar Generality + Test Redress (parity) and W5 - TS Binding + Value-API + Substrate Audit (executable) | TS backend emits, typechecks, executes representative grammars |
| Tailwind regex_scan perf timeout | AZ-III C5 | W4 - Optimization Substrate Activation | emitted scanner path resolves timeout class without per-call map overhead |
| Cross-profile watchdog bench rows | AZ-III C6 | W6 - Measurement And Close | fat-LTO + bench-iter matrices have zero watchdog rows |
| WASM/derive residue | hardening pass 2 | W0 - Truth And Canonical Regen | zero `bbnf_derive` references in active workspace/wasm/sibling paths |
| Rewrite/ruler substrate unconsumed | hardening pass 2 | W4 - Optimization Substrate Activation | unconsumed `RuleSet` and `egraph::ruler::*` deleted; BA recreates clean |
| Full substrate denominator | hardening pass 3 (Babbage) | W5 - TS Binding + Value-API + Substrate Audit | permanent `substrate_audit.rs` test enumerates every `pub` substrate; CI fails on zero-caller substrate |
| Path IR + `path!` macro (compile-time typed) | BA — formerly BA.W0/W1 | W2 - Path IR + Typed Path<G,T> + AscentStrategy | `path!(Json, "a", 0, "b")` resolves at compile time; invalid path = compile error with grammar-aware diagnostic |
| Lazy bail-out parse (sonic-class) | BA — formerly BA.W1 stretch | W3 - Lazy Bail-Out Parse | `parse_with(input, &path)` skips unvisited subtrees on JSON, CSS L4, Sheets, BBNF |
| AscentStrategy + hybrid sidecar | BA — formerly BA.W0 | W2 - Path IR + Typed Path<G,T> + AscentStrategy | `AscentStrategy` trait; sidecar default; reversal seam preserved |
| Per-grammar value-enum dedup (structural skeleton) | BA — formerly BA.W1 | W5 - TS Binding + Value-API + Substrate Audit | one generic `Arena<G>` + `Builder<G>` parameterised by `StructRegistry`; per-grammar `*Value` enums survive |
| TS template-literal tag binding | BA.W2 | W5 - TS Binding + Value-API + Substrate Audit | `crates/bbnf-path-ts/` cdylib + wasm-bindgen; isomorphic error taxonomy |
| Sonic-rs same-harness performance floor | BA.W3 + BB perf | W6 - Measurement And Close | same-harness `bbnf_value_*` parity-or-better against `sonic_value_*` (fat-LTO); lazy lane closes ≤ 5x sonic on `bbnf_get_*` |
| post-AU 17-row floor | AU through AZ-III | W6 - Measurement And Close | `floors` block in post-AZ-IV.json holds AU floor row-by-row |
| Rule discovery + Ruler + VM oracle + ranker | BB — formerly BB.W0-W4 | **routed to BA (recycled)** | post-AZ-IV tranche; BA opens after AZ-IV close |

## Non-Routable Carries (Expanded)

The 30 items below cannot route to a successor letter. AZ-IV closes inside these or AZ-IV does not close. A non-routable carry that survives close is a process failure.

| # | Item | Owner wave | Closure proof |
|---|---|---|---|
| 1 | Strict regen drift (7/9 grammars red) | W0 | `cargo xtask regen --check` green 9/9 |
| 2 | Egraph `Map { fn_id }` preservation | W0 | named test fails before / passes after |
| 3 | Sheets parity (133/133) | W1 | regenerated-tempdir parity green |
| 4 | TS backend executable (Node-execute) | W5 | tempdir TS typechecks + Node executes representative grammars |
| 5 | Tailwind regex_scan perf timeout | W4 | profile.json.gz + named hot regex op + non-watchdog measured row |
| 6 | Cross-profile watchdog rows | W6 | fat-LTO + bench-iter matrices have zero watchdog rows |
| 7 | JSON value/path vs sonic-rs perf | W6 | `bbnf_value_*` parity-or-better; `bbnf_get_*` ≤ 5x sonic same-harness |
| 8 | CSS named_color runtime activation | W1 (parity) + W4 (egraph extractor binding) | named_color payload parity vs lightningcss |
| 9 | PatternAnnotations migration | W4 | every consumer migrated or PatternAnnotations deleted |
| 10 | Bootstrap/derive residue (sibling) | W0 | `cargo metadata --locked` + `cargo deny` rule rejects `bbnf_derive` |
| 11 | DTA/dfa naming + cleanup | W4 | every DTA reference enumerated; non-consumed deleted, consumed renamed |
| 12 | `backend/rust/view/color` shim | W1 | shim deleted; CSS uses `runtime::css_l4::CssColor`; legacy decoder test-support only |
| 13 | Substrate denominator (permanent test) | W5 | `crates/ir/src/passes/tests/substrate_audit.rs` CI-gated; zero unconsumed substrate |
| 14 | Unconsumed `RuleSet` deletion | W4 | `pipeline.rs` `CompileOptions::rewrites` field deleted; `egraph::ruler::*` deleted |
| 15 | WASM/sibling derive residue | W0 | locks clean root + wasm/ + parse-that |
| 16 | csp-solver canonical-source split | W0 | diff-clean between bbnf-lang and csc411 sibling |
| 17 | bbnf-bootstrap cache nuke | W0 | cycle-2 wall ≤ 10 % of cycle-1 wall |
| 18 | Dev-iteration baseline gate | W0 | `W0-dev-baseline.txt` row-by-row deltas vs AZ-III |
| 19 | Generated-size budget | W0 | per-grammar LOC ±5 % of pre-W0 baseline |
| 20 | 7 `from_rule_name(&str)` impls eliminated | W1 | static AST scan returns zero match arms keyed on literal rule names |
| 21 | `(layout.kind, rule_name)` builder dispatches eliminated | W1 | `OpenFrame::from_layout(layout, &registry)` projects discriminator |
| 22 | `EmitStrategy::for_grammar` 9-arm allowlist eliminated | W1 | manifest-driven binding registry; synthetic-grammar test passes |
| 23 | `substrate_path` JSON-builder fallback retired | W1 | `panic!` on invalid binding; W0 manifest gate enforces well-formed paths |
| 24 | `recover_modifier`/`recover_binary_op` deleted | W1 | alt_dispatch typed-leaf push activated; `rg` returns zero hits |
| 25 | Per-grammar arena/builder dedup (skeleton) | W5 | one `Arena<G>` + `Builder<G>` template; per-grammar `*Value` enums preserved |
| 26 | All failing tests redressed (1527/1527 pass) | W1 | nextest workspace pass-count = total-count; ignores justified per spec |
| 27 | Path IR + compile-time `path!` macro | W2 | `path!(Json, ...)` compile-time typed; invalid path = compile error |
| 28 | `path_check` IR pass after `project_types` | W2 | inline-trace sidecar; source rule names always resolve |
| 29 | AscentStrategy hybrid sidecar | W2 | trait + reversal seam; default impl picked by W2 micro-bench |
| 30 | Lazy bail-out parse on 4 production grammars | W3 | path-driven recognizer skips unvisited subtrees on JSON/CSS/Sheets/BBNF |
| 31 | TS template-literal tag binding | W5 | `crates/bbnf-path-ts/` cdylib + wasm-bindgen |
| 32 | Variant-selection path step (typed-enum step) | W2 | `path!(CssL4, ..., "color")` returns `Option<&CssColor>` from sum type |
| 33 | Wildcard yields `Iter<Item = T>` (default) | W2 | zero-allocation default lane; `.with_anchors()` and `.collect()` adapters |

## Wave Table

| Wave | Agents | Closes on evidence | Status |
|---|---:|---|---|
| W0 - Truth And Canonical Regen | 5 parallel | strict regen, manifest binding, `Map` preservation, sibling derive eradication, dev-iteration baseline, **failing-test census + GESTALT.md excision** | planned |
| W1 - Grammar Generality + Test Redress | 5 parallel | overfit elimination + view/color delete + substrate panic + recover_* delete + EmitStrategy manifest + Sheets/CSS/JSON/BBNF parity green + **all failing tests redressed (fix-with-spec or delete-with-justification)** | planned |
| W2 - Path IR + Typed Path<G,T> + AscentStrategy | 5 parallel | source-rule-resolved path checker; `path_check` IR pass after `project_types`; inline-trace sidecar; bespoke path lexer via bbnf-regex HIR; hybrid sidecar `AscentStrategy`; `path!` proc-macro; compile-time variant-selection step; wildcard `Iter<Item = T>` with `.with_anchors()` adapter | planned |
| W3 - Lazy Bail-Out Parse | 5 parallel | path-driven recognizer; floor: JSON + CSS L4 + Sheets + BBNF; lazy + eager same `Option<T>` semantics; lazy mode silently elides errors past path reach (documented contract) | planned |
| W4 - Optimization Substrate Activation | 5 parallel | CSP authority globalized; SIMD consumed; Pratt generality; tailwind regex; DTA cleanup; **`RuleSet` field + `egraph::ruler::*` deleted** (BA recreates clean); PatternAnnotations migration | planned |
| W5 - TS Binding + Value-API + Substrate Audit | 5 parallel | `crates/bbnf-path-ts/` cdylib + wasm-bindgen + template-literal tag; per-grammar value-enum dedup (structural skeleton, leaves preserved); permanent `substrate_audit.rs` CI test; isomorphic error taxonomy; TS Node-executes representative grammars | planned |
| W6 - Measurement And Close | 3 parallel | post-AZ-IV.json (rows grow per `docs/benchmarks/SPEC.md` §D5; AU floor preserved in `floors` block); samply 7-artefact contract per `docs/instructions/PROFILING.md`; close-honesty checklist; FINAL.md | planned |

## Critical Files And Ownership

| Surface | Owner wave | Primary paths |
|---|---|---|
| Active plan truth + GESTALT excision | W0 | `docs/GESTALT.md`, `docs/codegen-paths.md`, `docs/tranches/REMAINING-TRAJECTORY.md`, `docs/tranches/AZ-IV/**`, `docs/tranches/BA/**` (recycled), `docs/tranches/BB/**` (subsumed banner) |
| Regen + manifest binding | W0 | `xtask/src/regen.rs`, `Cargo.toml`, `crates/ir/src/registry/strategy.rs`, `crates/core/src/grammar/generated/**` |
| Egraph payload preservation | W0 | `crates/ir/src/egraph/cost.rs`, `crates/egraph/src/extract/**`, `crates/ir/src/rewrites/**` |
| Failing-test census | W0 | `docs/tranches/AZ-IV/audit/W0-failing-test-census.txt` |
| Runtime overfit elimination + test redress | W1 | `crates/core/src/runtime/{bbnf,bnf,csv,css_pretty,ebnf,google_sheets,json,math}/**`, `crates/core/src/backend/rust/view/**`, `crates/core/src/backend/rust/emitter/shapes/substrate.rs`, `crates/core/src/lower/expression/{repeat,pratt,wrap}.rs`, `grammar/**`, `crates/core/tests/**` |
| Path IR + macro + AscentStrategy | W2 | `crates/core/src/path/{ir,type_check,error,ascent,lexer}.rs`, `crates/ir/src/passes/path_check.rs`, `crates/bbnf-path/src/path_macro.rs`, `crates/bbnf-regex/src/path_lexer.rs` (custom HIR API) |
| Lazy bail-out parse | W3 | `crates/core/src/path/executor.rs`, `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs`, generated parser dispatch tables |
| CSP/regex/shape/Pratt/SIMD/DTA cleanup | W4 | `crates/ir/src/passes/csp_strategy/**`, `crates/core/src/generate/regex/**`, `crates/core/src/backend/rust/emitter/shapes/**`, `crates/simd-scan/**`, `crates/core/src/backend/rust/emitter/dfa_codegen.rs` (rename + content rewrite) |
| TS binding + value-API dedup + substrate audit | W5 | `crates/bbnf-path-ts/**` (new cdylib), `crates/core/src/runtime/{arena_template,builder_template}.rs` (new), `crates/ir/src/passes/tests/substrate_audit.rs` (new permanent test) |
| Benchmark + profiling | W6 | `crates/core/benches/**`, `scripts/prepare-profile-wave.sh` + `scripts/profile-bench-headless.sh` (per `docs/instructions/PROFILING.md`), `docs/benchmarks/post-AZ-IV.json` (per `docs/benchmarks/SPEC.md`), `docs/benchmarks/profiles/post-AZ-IV/**`, wave evidence at `docs/tranches/AZ-IV/audit/W6-*.{txt,md,json}`, `.profiles/samply/post-AZ-IV/` |
| Sibling topology | W0 | `wasm/**`, `/Users/mkbabb/Programming/parse-that/**`, `/Users/mkbabb/Programming/pprint/**`, `.cargo/config.toml`, package locks |

## Orchestration Rules

1. Max six agents per wave. Older ten-agent text is historical and non-normative.
2. Parallel writers use named sibling worktrees with distinct `CARGO_TARGET_DIR`; no parallel implementation or docs-writing agent writes in main.
3. The orchestrator records `git status --short`, staged paths, base commit, worktree list, and target dirs before dispatch.
4. Empty/null/no-evidence return is a failed dispatch: redispatch verbatim once with the same worktree pointer; a second empty/no-evidence return triggers the triumvirate.
5. Triumvirate means research, plan augment/synthesis, and redress. It is mandatory for scope reveal that invalidates file bounds, hard gates, or substrate-with-consumer wiring.
6. Read-only agents do not commit at hard cap. Write-authorized agents commit at 0.9N only when the staged slice is clean and owned.
7. Before every commit, use the local `commit-discipline` skill: inspect dirty/staged state, preserve unrelated staged work, stage only intended paths, review `git diff --cached`, and stop if the slice cannot be isolated.
8. Broad, generated, deletion, benchmark, profiling, gate/status, and cross-repo commits require bodies naming why, what landed, evidence, and routed remainder. No AI/tool authorship.
9. Profiling agents may share one prepared absolute target only after preparation; no two cargo invocations run concurrently against the same target dir.
10. Every dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt.` Defaults: research 20, plan 15, redress 30, audit 25 (per `docs/precepts/instructions/ORCHESTRATION.md` §Triumvirate). Read-only audit/research agents do not commit at the cap; write-authorized agents commit at 0.9N only when the staged slice is clean and owned.
11. **HARD CAP expansion on overrun.** If a wave's HARD CAP is exceeded by an in-flight write-authorized agent without commit, the orchestrator extends the cap (not split the work) and records the extension reason in `PROGRESS.md`. Triumvirates are not pre-allocated per wave; they fire on the auto-triggers in §11.
12. Triumvirate auto-triggers (no user prompt required): JSONL transcript quiet >15 minutes, first-pass return with no commit and no evidence, three diagnostic-loop iterations without isolating root cause, or scope reveal that invalidates file bounds / hard gates / substrate-with-consumer wiring.
13. Sub-agent prompts must remain self-contained and stay within ~700 words of instructions; if a prompt grows larger, the task is mis-scoped and decomposes into sequential mini-units before dispatch.
14. The W0.3 lowering triad (`crates/core/src/lower/expression/{wrap,repeat,alt}.rs`) is one unit of repair landed in one commit. Predicate-driven structural detection in lowering is the underlying defect class; redress agents replace silent-skip predicates with structural detection plus loud panic on unmatched annotations (per `feedback_typed-materialization-invariant` and §13 No silent fallback). Mechanical regen output lands as a follow-on commit so the `regen --check` invariant holds across the wave.

## Hard Gates

1. `cargo xtask regen --check` passes live for all manifest grammars and is archived at `docs/tranches/AZ-IV/audit/W0-regen.txt` (wave evidence; per `docs/benchmarks/SPEC.md` archive policy).
2. Parser strategy binding is manifest/registry driven; a synthetic grammar rename/addition test fails if a new literal parser-name arm is required (`crates/core/tests/synthetic_grammar_strategy.rs`).
3. Regenerated tempdir outputs run the parity matrix; checked-in freshness and runtime parity cannot be proven by separate stale artefacts.
4. JSON, CSS, Sheets, BBNF, and TS parity gates are current, green, regenerated-output based, and type-inference driven. CSS/Sheets parity cannot close on early-return payload gaps, hand normalizer equivalence, rule-name projection, or synthetic default payloads.
5. Egraph extraction preserves semantic wrappers such as `Map { fn_id }`; a named test fails if extraction strips typed payloads.
6. **Workspace nextest is 100 % pass.** Fail-count zero. Every `#[ignore]` carries an owner-named comment with a deadline commit, reason, and follow-up ticket; ignores without that triplet fail the close-honesty checklist.
7. **Path IR + compile-time `path!` macro**: `path!(Json, "statuses", 0, "text")` expands to a typed accessor at `cargo build` time. An invalid path fails to compile with a `proc_macro2::Span`-anchored grammar-aware diagnostic naming the segment, the resolved struct type, and valid alternatives at that position.
8. **`path_check` IR pass + inline-trace sidecar**: paths resolve against source rule names; an inlined rule remains addressable through the inline trace.
9. **Lazy bail-out parse coverage**: `parse_with(input, &path)` works on JSON, CSS L4, Sheets, BBNF; the recognizer skips subtrees the path does not visit. Lazy + eager modes share generated code and dispatch on entry-point only.
10. **Variant-selection path step**: `path!(CssL4, "rules", 0, "declarations", 0, "value", "color")` returns `Option<&CssColor>` from `CssTypedValue::Color(_)`; the macro reads the grammar's `->` annotations to know variant names.
11. **Wildcard returns lazy iterators**: `path!(..., "*", ...)` default lane is `Iter<Item = T>` zero-allocation; `.with_anchors()` adapter yields `(Path<'_>, T)`; `.collect()` materializes when caller wants.
12. CSP decisions that select a regex/layout/dispatch engine are reflected at emitted consumers or the dead decision surface is deleted. Sidecars may carry payloads after CSP selection; they may not choose strategy.
13. **Permanent substrate-audit test**: `crates/ir/src/passes/tests/substrate_audit.rs` enumerates every `pub` substrate at compile time and fails the build if any has zero callers in production code (excluding `tests/`, `examples/`, `#[cfg(test)]`). CI-gated.
14. Legacy audit closes: `emit_dfa_inline_body`, DTA walker/tape wording, old color compatibility, fallback-to-JSON substrate path, `RuleSet` + `egraph::ruler::*` (deleted; BA recreates), discarded Rust per-rule compile work, derive/bootstrap residue, duplicated host shims, stale package locks, and sidecar authority are deleted, renamed, or justified by current consumers.
15. Fat-LTO `post-AZ-IV.json` carries row-by-row post-AU floor in the `floors` block, post-AZ same-profile deltas, AZ-III bench-iter deltas, status, and pass/fail per `docs/benchmarks/SPEC.md`. No row may be watchdog-routed. Row count grows from the AU 17 baseline as the lazy/path/TS lanes add new rows.
16. Same-harness JSON direct struct projection rows close parity-or-better against sonic-rs (`bbnf_value_* <= sonic_value_*` on time, or equivalent throughput ratio). Lazy lane (`bbnf_get_twitter`) closes ≤ 5x sonic on same-harness comparison; the AZ-IV stretch target of ≤ 1.0x routes only with profile evidence.
17. **Grammar-overfit static scan green**: `crates/core/tests/no_grammar_name_branch.rs` (a CI-enforced AST scan over `crates/core/src/runtime/**` and `crates/core/src/backend/rust/emitter/shapes/**`, excluding `generated/` and `#[cfg(test)]`) fails closed if any literal-rule-name match arm appears in production code.
18. **Manifest-driven strategy binding**: `EmitStrategy::for_grammar` reads parsed manifest metadata, not a literal Rust source arm-list.
19. **Substrate path hard-fail**: `crates/core/src/backend/rust/emitter/shapes/substrate.rs` no longer falls back to `JsonStructBuilder` (or any default builder); it `panic!`s with the offending binding string.
20. **TS binding executable**: `crates/bbnf-path-ts/` cdylib + wasm-bindgen template-literal tag executes against representative grammars; isomorphic error taxonomy with the Rust frontend.
21. **Per-grammar value-enum dedup (skeleton)**: one `Arena<G>` + `Builder<G>` template parameterised by `StructRegistry`; per-grammar typed `*Value` enums survive untouched (semantic richness preserved).
22. **AscentStrategy hybrid sidecar**: trait + reversal seam landed in W2; default sidecar implementation chosen by W2 micro-bench on citm/tailwind/sheets fixtures; results commit at W2 close.
23. **Non-routable carry blockers**: every row in §Non-Routable Carries closes inside AZ-IV with cited evidence or AZ-IV does not close. A non-routable carry that survives close is a process failure, not a deferral.

## Deletion Bias

AZ-IV deletes before adding. Forbidden patterns in the AZ-IV diff:

- no `*_v2` modules;
- no compatibility feature flags;
- no restored derive pipeline;
- no DTA walker fallback;
- no second path-query crate;
- no generated tape/view bridge;
- no "consumer later" hooks;
- no `from_rule_name(&str) -> Kind` arm-list per grammar (registry projects);
- no `(layout.kind, rule_name)` builder dispatch (registry projects);
- no `dfa_codegen` misnomer (it is the regex-scan adapter; rename or fold);
- no `backend/rust/view/color` shim (CSS uses `runtime::css_l4::CssColor`);
- no `recognize_*_legacy` patterns (rename or migrate Pratt then delete);
- no `substrate_path` JSON-builder fallback (panic on invalid binding);
- no per-grammar value-enum dedup that touches typed `*Value` enums (semantic richness preserved per `feedback_preserve-rich-ast`; only the structural skeleton dedups);
- no Python binding path (Python is dropped from the thesis per Q-final-4).

If deletion is unsafe because a current consumer exists, the wave must name the consumer and refactor the surface to match its real role.

## Cross-Tranche Debt

AZ-IV absorbs BA's pre-recycle scope and BB's perf/value/struct-projection scope into one tranche. The BA letter is recycled for the post-AZ-IV residual: pure rule-discovery work (Ruler CVC enumerator, VM oracle on residue, ranker, Class-1/2/3 tiering, `crates/ir/src/rewrites/`, grammar-colocated rewrite dirs, Tranche H rediscovery ≥ 80 %, ≥ 5 accepted rules per production grammar). Old BA scope is preserved at `docs/tranches/BA/historical/` for archaeology; the new BA is the rule-discovery successor.

BB is subsumed: perf items into AZ-IV; rule-discovery items into recycled BA. `docs/tranches/BB/` carries a `STATUS: SUBSUMED` banner pointing at the two destinations.

If a non-routable item cannot land inside AZ-IV without changing the AZ-IV thesis, the response is a triumvirate review of the thesis — not a new tranche letter.

## Cross-Repo Future Work (Out of AZ-IV scope)

Future tranches (not AZ-IV) will move bench/optimization sub-crates into their own repositories or relocate them inside parse-that:

- `crates/csp-solver` → its own repo (canonical-source policy already declared between bbnf-lang and csc411 sibling)
- `crates/egraph` → its own repo (general-purpose infra crate per `feedback_general-infra-crates`)
- `crates/simd-scan` → its own repo or into parse-that
- `xtask` → relocated within `crates/` repo as `crates/xtask`
- `bbnf-regex` → sub-crate of parse-that (regex source-of-truth lives in one place)

These are recorded for plan continuity. AZ-IV does not move them; AZ-IV exposes a custom path-lexer API from bbnf-regex (W2.D1) that the future relocation can preserve cleanly.

## Brittleness Window

No tranche-wide brittleness window is declared. A wave may declare a local brittleness window only in its wave spec, with suspended gates, restoration wave, and reason. AZ-IV cannot close while any brittleness window is open.
