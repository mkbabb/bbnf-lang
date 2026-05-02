# AUDIT-E — Path-Forward Synthesis Lane (W3-W6 + post-AZ-IV trajectory)

**Date**: 2026-05-02
**Auditor**: AUDIT-E (read-only research + plan synthesis lane)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-audit-e-pathforward`
**HEAD at audit**: `10ac5448` (post-mid-tranche audit landing)
**HARD CAP**: 30 min; one doc commit allowed
**Read-first**: `docs/precepts/instructions/{README,STYLE,ORCHESTRATION}.md`, `docs/GESTALT.md`, `docs/tranches/AZ-IV/{AZ-IV.md,PROGRESS.md}`, `docs/tranches/AZ-IV/waves/{W3,W4,W5,W6}.md`, `docs/tranches/AZ-IV/audit/{AUDIT-2026-05-02-mid-tranche,HARDENING-2026-05-01-boole}.md`, `docs/tranches/BA/BA.md`, `docs/tranches/REMAINING-TRAJECTORY.md`

This audit reads existing evidence only. No `cargo`, `make`, or bench commands invoked.

## §1 W3-W6 Critical-Path

### W3 — Lazy Bail-Out Parse

**Stated scope.** Make the parser path-aware. `JsonParser::parse(input)?.get(path)` materialises the full document then walks it (~859 µs on twitter; sonic-rs `get` is 291 ns; 2953x). W3 introduces `parse_with<P: PathSchema>(input, path) -> Option<P::Output>` that drives the recognizer to satisfy the path while skipping subtrees the path does not visit. Lazy and eager share generated parse functions; only the entry-point dispatches differently. Floor: JSON, CSS L4, Sheets, BBNF.

**Dependencies.**
- *Blocked by*: W2 close (consumes `TypedPath<G, T>`; uses `AscentStrategy::HybridSidecar` for wildcard + sibling-anchor paths; `path_check` IR pass output drives the codegen plan emitter).
- *Blocks*: W4 (the path-plan emitter must precede CSP authority globalisation; W4's regex-scan adapter rename and DTA cleanup must respect path-plan static arrays).

**Carries from W0-W2 absorbed.**
- *Mid-tranche §6 row 1* — `crates/ir/src/rewrites/path_seed.rs` hand-authored egraph rewrites (W2 Hard Gate 9; deferred). W3 opens by landing the small ruleset (duplicate-prefix elimination, redundant downcast removal, adjacent-accessor fusion) before path-plan codegen runs. The seed is a sub-unit prefix to W3.3.
- *Mid-tranche §7 R5* — bbnf-regex sibling repo path-patch coupling with parse-that. W3.3's path-plan emitter consumes the same lexer; if parse-that drifts, the regen output drifts. Mitigation already in place.

**Risks (cross-ref §7).**
- *R1* — synthetic registry fixture in `bbnf-path` is still consumed by the macro; W3's path-plan codegen reads the *production* `StructRegistry` from the IR. The two registries must agree on shape before the path plan emitter runs. The plan: W3.3 verifies this in its sub-gate by emitting the path plan against production registry, comparing structurally to fixture-keyed expectations.
- *R2* — inline-trace pipeline ordering. W3's recognizer plan calls into the same `path_check` output that W2.2 emits. If a future pass reorders `inline_acyclic` / `fuse_single_use`, both W2 and W3 break. Regression test exists (`fixture_grammar_fused_rule_still_resolves`); strengthen W3 sub-gate to assert the plan compiles against fused fixture.
- *new R6* — same-harness sonic-rs floor (≤ 5x) is a single hard gate riding on samply attribution. Three diagnostic loops should isolate any miss; the mid-tranche audit's §7 carries that the watchdog-without-named-cause failure mode chronically defers (Boole §b.2 row 4). W3 hard-gate item 9 already names the consumer requirement; carry the language verbatim into W3.5's sub-gate.

**Dispatch shape.** Five sub-units, all writers, one orchestrator-owned profiling slot.

| Sub-unit | Worktree | Role | Writes |
|---|---|---|---|
| W3.0 | `bbnf-wt-aziv-w3-seed` (new) | path egraph seed (carry) | `crates/ir/src/rewrites/path_seed.rs` |
| W3.1 | `bbnf-wt-aziv-w3-executor` | path executor + cursor + schema | `crates/core/src/path/{executor,cursor,schema}.rs` |
| W3.2 | `bbnf-wt-aziv-w3-entry` | per-grammar `parse_with` entry points | `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs` |
| W3.3 | `bbnf-wt-aziv-w3-emit` | codegen path-plan emitter (regen authority) | `crates/core/src/backend/rust/emitter/path_plan.rs` + carve in `grammar.rs` + regen output |
| W3.4 | `bbnf-wt-aziv-w3-tests` | parse-with test suite (4-grammar floor) | `crates/core/tests/parse_with_*.rs` |
| W3.5 | `bbnf-wt-aziv-w3-bench` | bench harness + profiling | `crates/core/benches/{json/value,css/path,sheets/path,bbnf/path}.rs` |

W3.0 is sequenced before W3.1-W3.5 (small commit; opens the wave). W3.3 is the regen integration point — W3.1, W3.2, W3.4 cherry-pick its regen output. W3.5 sequences last (consumes the prepared profile-wave artefacts; profiling-prepare is orchestrator-owned).

**Triumvirate triggers.** W3.3 (codegen path-plan emitter) carries the highest scope-reveal risk: if the recognizer plan grows per-grammar branches, this is a thesis violation per AZ-IV.md §Invariants 2 (grammar generality). W3.5 carries the watchdog-without-cause risk per Boole §b.2 row 4. Both auto-trigger triumvirate on diagnostic-loop count ≥ 3 or empty-evidence return.

**Target commits.** Six landing commits at minimum.
1. `feat(rewrites/path-seed): land hand-authored path egraph seed` — closes W2 Hard Gate 9 carry.
2. `feat(path-executor): PathExecutor + PathCursor + PathSchema` — W3.1.
3. `feat(emitter/path-plan): grammar-general recognizer plan codegen` — W3.3 (regen body required).
4. `feat(runtime/parse-with-{json,css-l4,sheets,bbnf}): per-grammar entry points` — W3.2 (one per grammar; bodies needed).
5. `test(parse-with): 4-grammar floor with three path shapes each` — W3.4.
6. `bench(path): per-grammar path bench harness + W3 profile artefacts` — W3.5.

**Hard-gate evidence path.**
- `docs/tranches/AZ-IV/audit/W3-executor-types.txt`
- `docs/tranches/AZ-IV/audit/W3-parse-with-coverage.md`
- `docs/tranches/AZ-IV/audit/W3-path-plan-regen-diff.txt`
- `docs/tranches/AZ-IV/audit/W3-error-elision-contract.txt`
- `docs/benchmarks/profiles/post-AZ-IV/W3/<harness>/<entry>/{bench.txt,build.txt,record.txt,load.txt,profile.json.gz,profile.json.syms.json,syms-proof.txt}`
- `docs/tranches/AZ-IV/audit/W3-sonic-comparison.json`

### W4 — Optimization Substrate Activation

**Stated scope.** Wire every loaded `RuleSet`/ruler output into production saturation (or delete). Make CSP engine/layout/dispatch decisions authoritative at consumers. Make regex HIR/egraph/CSP decisions select concrete emitted scanner paths. Reconcile DTA/dfa naming. Resolve Tailwind timeout-class behaviour. Produce a substrate-denominator ledger with disposition for every active mined fact.

**Dependencies.**
- *Blocked by*: W3 close (path-plan emitter must respect W4's CSP authority globalisation; the regen output must agree).
- *Blocks*: W5 (substrate-audit test enumerates W4-close surface; if W4 leaves WIRED-NOT-CONSUMED rows, W5's audit test red-or-skips).

**Carries from W0-W2 absorbed.**
- *Mid-tranche §6/§8* — typed-Alt-of-single-byte-literals (W1.4 halt; egraph saturation losing typed annotations on `item = "x" -> 0u8 | "y" -> 1u8`). W4.1 owns the egraph-rule preservation fix.
- *Mid-tranche §6 row "2 timeouts"* — tailwind perf + LSP completion. W4.2 (CSP regex authority) owns the tailwind regex profile. The LSP timeout is sibling-state; route to W6 if non-actionable in W4.
- *Boole §b.1 item 2* — `Map { fn_id }` cost-extractor preservation already landed at W0; W4.1 inherits the test as a regression guard.
- *Boole §b.1 items 9, 11, 12* — `PatternAnnotations` migration, DTA/dfa naming, view/color shim. W4.4 owns DTA/dfa cleanup; W4.5 owns Pratt/view; the view/color shim already deleted at W1, so this carry retires.
- *Boole §b.1 item 14* — rewrite/ruler production wiring. W4.1 either wires the chain or deletes per AZ-IV.md §Hard Gates 14 (BA recreates clean). The non-routable carries §14 sets deletion as the floor disposition; wiring is the stretch.

**Risks.**
- *R7 (new)* — W4.1's "every non-empty loaded ruleset proves the chain" gate is incompatible with the W4 close-text "delete the unconsumed `RuleSet`". The plan currently reads as "either wire or delete". Decide the disposition in §8 before W4 dispatch.
- *R8 (new)* — `audit/HARDENING-2026-05-01-cantor.md` Amendment 2 split W4.2 (CSP regex/strategy/dispatch) from W4.3 (CSP shape/scan + shapes/{dispatcher,inline,array}/**). W4.5 owns shapes/pratt and view rendering. The leaf-level split must be in the dispatch packets verbatim; regression on the split is a triumvirate trigger per W4 §Triumvirate.
- *Babbage matrix* — five WIRED-NOT-CONSUMED + three DEAD substrates. W4 must close all eight in the denominator ledger.

**Dispatch shape.** Five sub-units (per spec); leaf-level path split per Cantor Amendment 2.

| Sub-unit | Worktree | Owns leaves |
|---|---|---|
| W4.1 | `bbnf-wt-aziv-w4-rewrites` | rewrites + ruler + egraph chain + pipeline.compile |
| W4.2 | `bbnf-wt-aziv-w4-csp-regex` | `csp_strategy/{strategy,regex}.rs`, regex emit, alt_strategy |
| W4.3 | `bbnf-wt-aziv-w4-shape-simd` | `csp_strategy/{shape,structural_scan}.rs`, shapes/{dispatcher,inline,array}, simd-scan |
| W4.4 | `bbnf-wt-aziv-w4-dta-dfa` | dfa_codegen→regex_scan_adapter rename, dta→grammar_facts rename, naming sweep, substrate_path panic |
| W4.5 | `bbnf-wt-aziv-w4-pratt-view` | shapes/pratt, view rendering |

W4.1's pipeline.rs change is the one shared file; review before parallel cherry-pick. W4.4's renames are mass-renames; isolate in dedicated commits per `chore(rename/...)` scope.

**Triumvirate triggers.** W4.1 carries the highest risk (rewrite-or-delete decision; non-empty ruleset proof). The wave spec already auto-triggers triumvirate when WIRED-NOT-CONSUMED count exceeds 5 at audit start OR CONSUMED count is < 60% at mid-point. W4.5 carries the `PatternAnnotations` migration risk: if Pratt cannot migrate to `node_facts` reads cleanly, the dispatch grows to span W4.4 + W4.5 (triumvirate scope-reveal).

**Target commits.** Twelve+ landing commits.
1. `feat(rewrites): chain RuleSet load → search → apply → extract → writeback` (or `chore(pipeline): delete unconsumed RuleSet field`).
2. `feat(csp-strategy/regex): consumer-authoritative CSP regex selection`.
3. `feat(emitter/regex-scan): scanner-class dispatch from CSP`.
4. `chore(rename/dfa-codegen-to-regex-scan-adapter)`.
5. `chore(rename/dta-to-grammar-facts)`.
6. `chore(rename/recognize-pattern)`.
7. `feat(csp-strategy/shape): consume shape_dict at runtime` (or `chore(ir): delete shape_dict_templates field`).
8. `feat(simd-scan): activate structural-scan facts` (or delete).
9. `feat(shapes/pratt): grammar-general operator dispatch`.
10. `chore(annotations): migrate PatternAnnotations consumers` then `chore(annotations): delete PatternAnnotations`.
11. `fix(types/obligation-drain)` (substrate denominator row 19).
12. `chore(docs/codegen-paths-irpass-fix)` (Babbage finding f).

**Hard-gate evidence path.**
- `docs/tranches/AZ-IV/audit/W4-{rewrite-chain,csp-regex,shape-simd,legacy-deletion-ledger,substrate-denominator,ruler-oracle-ranker}.{txt,md,json}`
- `docs/benchmarks/profiles/post-AZ-IV/W4/tailwind/<entry>/{seven artefacts}` (timeout root-cause profile)

### W5 — TS Binding + Value-API + Substrate Audit

**Stated scope.** Land the TS binding for the `path!` macro (`crates/bbnf-path-ts/` cdylib + wasm-bindgen + template-literal tag). Land per-grammar value-API consolidation (structural skeleton dedup; typed `*Value` enums survive untouched per `feedback_preserve-rich-ast`). Land permanent `substrate_audit.rs` test that prevents future "substrate without consumer" regressions. Each per-grammar `arena.rs` ≤ 30 LOC; each `builder.rs` ≤ 50 LOC.

**Dependencies.**
- *Blocked by*: W4 close (substrate-audit test enumerates the post-W4 surface; W4 must close WIRED-NOT-CONSUMED rows or the audit test fails on its first run).
- *Blocks*: W6 (close-honesty checklist consumes W5's substrate-denominator artefact).

**Carries from W0-W2 absorbed.**
- *Mid-tranche §7 R3* — `unknown` TS Color type at W1-CLOSE.C. W5.1 cdylib lands the executable type; `crates/bbnf-path-ts/` exposes the path-binding surface and the `Color` struct routes through wasm-bindgen.
- *Mid-tranche §6* — TS Node-execute proof carry (W1 closed at build-time; W5 owns end-to-end Node execution per AZ-IV §Hard Gates 20).
- *Mid-tranche §6 R1* — synthetic registry fixture swap-out. W5.3's per-grammar `arena.rs` instantiation is the natural seam to consume the production `StructRegistry` const that codegen now emits (post-W4); the bbnf-path proc-macro remains pointing at the fixture *until W5.4's substrate-audit test forces the swap or proves the fixture has a production-only consumer*. Decide in §8.

**Risks.**
- *Original R3 (W1-CLOSE.C)* mitigated by W5.1.
- *new R9* — wasm-bindgen FFI shape divergence (per `feedback_isomorphic-api`). W5.1's isomorphism test (`isomorphic_path_error.rs`) catches it pre-Node. The risk is that error rendering differs between `syn::Error::new` (Rust) and `throw new PathError(...)` (TS). Mitigation: shared struct definition; renderers live host-side.
- *new R10* — substrate-audit test enumeration cost. W5.4's test walks every `pub` symbol in five crates; the spec requires < 60s. Cost depends on `cargo metadata` overhead and AST walk speed. Risk: the test runs > 60s on some host configurations; mitigation already in spec (`cargo nextest` partition).

**Dispatch shape.** Five sub-units (per spec); disjoint primary write paths.

| Sub-unit | Worktree | Owns |
|---|---|---|
| W5.1 | `bbnf-wt-aziv-w5-cdylib` | bbnf-path-ts cdylib + template-tag + isomorphism tests |
| W5.2 | `bbnf-wt-aziv-w5-node` | TS Node execution proof |
| W5.3 | `bbnf-wt-aziv-w5-dedup` | per-grammar arena/builder dedup (structural skeleton) |
| W5.4 | `bbnf-wt-aziv-w5-audit` | permanent substrate-audit test |
| W5.5 | `bbnf-wt-aziv-w5-profile` | profiling pass + wasm-crossing attribution |

W5.1 must land before W5.2 (Node test consumes the cdylib). W5.4's enumeration target is the post-W5.3 surface (the dedup might collapse some pub items; the audit test must run *after* dedup). Sequencing: W5.1 + W5.3 in parallel; W5.2 + W5.4 after; W5.5 last.

**Triumvirate triggers.** W5.4 carries the highest scope-reveal risk: if substrate-audit reveals a load-bearing-but-unconsumed substrate, this is a genuine W5 scope reveal. The spec already names this trigger.

**Target commits.** Eight+ commits.
1. `feat(bbnf-path-ts/cdylib): wasm-bindgen exports compile_path + execute_path`.
2. `feat(bbnf-path-ts/template-tag): TS template-literal tag wrapper`.
3. `feat(bbnf-path-ts/ts-frontend): TS-side path.ts + index.ts`.
4. `test(bbnf-path-ts/isomorphic-error): PathError rendering parity`.
5. `test(bbnf-path-ts/node-execute): twitter.json end-to-end leaf-extract proof`.
6. `feat(runtime/arena-template): generic Arena<G> over StructRegistry`.
7. `feat(runtime/builder-template): generic Builder<G, T>`.
8. `chore(runtime/{grammar}/arena-instantiate)` × 9 — per-grammar instantiations.
9. `feat(ir-passes/substrate-audit-test): permanent zero-caller-fail enumeration test`.
10. `feat(ci/substrate-audit-gate)` — workflow run.

**Hard-gate evidence path.**
- `docs/tranches/AZ-IV/audit/W5-{bbnf-path-ts-build,isomorphic-error,node-execute,arena-builder-dedup,substrate-audit-pass,substrate-denominator,profiling-pass}.{txt,md}`
- `docs/benchmarks/profiles/post-AZ-IV/W5/<harness>/<entry>/{seven artefacts}`

### W6 — Measurement And Close

**Stated scope.** Refresh the post-AZ-IV benchmark matrix under fat-LTO close profile and bench-iter comparison profile. Active measured rows for `json.data_xl`, `css.tailwind`, `compile_css_l4` — no watchdog rows. Substrate activation counters where benches hide whether rewrites/CSP/SIMD/regex paths fired. Workspace health gates green. Direct StructDirect document/value/path projection performance proven (not parse-only). Same-harness sonic-rs `bbnf_value_*` parity-or-better; `bbnf_get_*` ≤ 5x sonic. Close-honesty checklist; FINAL.md.

**Dependencies.**
- *Blocked by*: W5 close (substrate-denominator artefact feeds the close-honesty checklist).
- *Blocks*: any successor tranche (BA can only open after W6 close per BA.md §AZ-IV dependency).

**Carries from W0-W2 absorbed.**
- *Mid-tranche §6 W1 carry* — 26 `#[ignore]` triplet enumeration. W6 close-honesty owns this. Per-ignore audit names owner + deadline-commit + reason + ticket.
- *Mid-tranche §6 W3 carries* — sonic-rs floor (W3 hard-gate already names) feeds W6 row.
- *Mid-tranche §6 W4 carries* — tailwind perf + LSP timeout. W6 closes these or routes to triumvirate review of thesis (no successor letter).

**Risks.**
- *R11 (new)* — non-routable thesis-review trigger. AZ-IV.md §Non-Routable Carries declares 33 items that cannot route. If any reaches W6 without resolving evidence, the response is triumvirate review of AZ-IV thesis itself, not a new tranche letter. The audit doc, the hard gate text, and the close-honesty checklist all align on this; the failure mode is the orchestrator quietly routing one row forward at FINAL.md drafting time. Mitigation: §8 lists the residual high-risk rows as decisions that should resolve in W3-W5, not slide to W6.

**Dispatch shape.** Three writer slots (per spec). Profiling sub-agents sequence inside W6.1 worktree, not as a sixth slot — preserves the three-slot count while honouring `PROFILING.md`'s 5-agents-per-bench-harness contract.

| Sub-unit | Worktree | Owns |
|---|---|---|
| W6.1 | `bbnf-wt-aziv-w6-bench` | benchmark matrix + profiling pass |
| W6.2 | `bbnf-wt-aziv-w6-gates` | workspace gates |
| W6.3 | `bbnf-wt-aziv-w6-close` | close docs + FINAL.md |

**Triumvirate triggers.** W6.1's watchdog-row gate is the chronic-deferral surface (Boole §b.2 row 6). Three diagnostic loops on a watchdog row without isolating root cause auto-trigger triumvirate. The thesis-invalidating scope reveal trigger is in the spec (`audit/HARDENING-2026-05-01-boole.md` Amendment 3).

**Target commits.** Three+ landing commits.
1. `bench(post-az-iv/matrix): fat-LTO + bench-iter rows; sonic-rs same-harness; floors block`.
2. `chore(workspace-gates/W6): regen + fmt + clippy + nextest + metadata + sibling-topology evidence`.
3. `docs(az-iv/close): PROGRESS reconcile + FINAL.md + GESTALT update + REMAINING-TRAJECTORY status update`.

**Hard-gate evidence path.**
- `docs/benchmarks/post-AZ-IV.json` (canonical close matrix per SPEC.md)
- `docs/benchmarks/profiles/post-AZ-IV/<harness>/<entry>/{seven artefacts}`
- `docs/tranches/AZ-IV/audit/W6-{fat-lto,bench-iter,sonic-projection,iai,workspace-gates,profiling-pass}.{txt,md,json}`
- `docs/tranches/AZ-IV/FINAL.md`

## §2 Refinements Toward Generalised Grammar Optimum

### W3 — three refinements

1. **PathSchema as the single decision surface for parse-mode dispatch.** Today the spec has `parse(input)` (eager, returns `Result`) and `parse_with(input, &path)` (lazy, returns `Option`). The two surfaces are co-defined; the dispatch difference is the entry-point. *Refinement*: collapse to one entry `parse_with::<P: PathSchema>(input, path)` where `P = ()` denotes eager. The cursor becomes a no-op when `P = ()` (compiler eliminates), eager and lazy unify at the type system. Per `feedback_one-codegen-path` (one regex system, one codegen path, one parser entry-point). The `Result` vs `Option` divergence routes through the `PathSchema::Output` associated type. KISS DRY.
2. **Path egraph seed pluggability.** W2's hand-authored seed (`crates/ir/src/rewrites/path_seed.rs`) is hand-coded; per `feedback_pluggable-components`, the rule registry must be the decision point. *Refinement*: land the seed under the schema BA.W0 will introduce (RON-keyed; provenance: HandAuthored; tier: Class1). When BA opens, the same RON files round-trip through the discovery pipeline; no migration. This is one of two ways to retire the hand-authored Tranche H rule path; the other is BA's discovery sweep (Tranche H rediscovery ≥ 80%). Architectural transposition: **the path_seed file is the BA seed**; AZ-IV.W3 lands it in BA's eventual storage layout, not in a transient location.
3. **Profile attribution as machine-checkable contract.** W3.5's seven-artefact contract is verbatim from PROFILING.md. *Refinement*: add a `tools/check-profile-artefacts.sh` script that asserts the seven files exist non-empty and `syms-proof.txt` names a function under the path-relevant rule (not the rules the path skips). Boole §b.1 row 6 chronic-defers because watchdog rows close on "profile saved" without a named consumer; machine-checkable artefact validation eliminates the failure mode.

### W4 — four refinements

4. **Rewrite-OR-delete decision pre-W4 dispatch.** Per AZ-IV.md §Hard Gates 14, `RuleSet` field + `egraph::ruler::*` are deleted (BA recreates clean). The W4 spec carries text that reads "wire or delete". *Refinement*: kill the ambiguity. Set the floor as **delete**; wiring is non-existent stretch. Per `feedback_no-workarounds`, half-wired substrate without a runtime consumer is debt; per `feedback_abrogate-before-patch`, deletion is the canon. W4.1's mechanism reduces to: delete `CompileOptions::rewrites`; delete `egraph::ruler::*`; delete `pipeline/compile.rs:560-573` eprintln-sink. Recovery is BA's job. The substrate-audit test (W5.4) catches any zombie pub item.
5. **One regex system end-to-end.** W4.2's CSP regex authority gate names "every regex decision routes through CSP-selected scanner class". Currently `crates/core/src/backend/strategy/alt_strategy.rs:163-184` re-overrides CSP-chosen `AltMode`. *Refinement*: per `feedback_one-codegen-path` and `feedback_no-orthogonal-codepaths`, retire the override surface by deletion. The CSP becomes the single source of truth for regex strategy; sidecars carry payloads only. Mid-tranche audit's `feedback_no-orthogonal-codepaths` precept names this exactly.
6. **Substrate denominator as machine-checkable artefact.** W4.9's denominator covers every active surface; W5.4's substrate-audit test enumerates `pub` symbols. *Refinement*: align the two. The W4 denominator artefact is a *manual* census; the W5 audit test is *automated*. Per `feedback_no-workarounds`, manual census drift is the chronic failure mode. Move the W4 denominator into a generated artefact: `cargo run --bin substrate_census > docs/tranches/AZ-IV/audit/W4-substrate-denominator.md`. Census tool reuses W5's enumeration logic; W4 lands the tool, W5 lands the test that consumes it. Architectural transposition: the audit infrastructure is one tool, two consumers (W4 census doc, W5 CI test).
7. **DTA naming sweep with structural scan.** W4.4's renames touch many files (935 refs per Heisenberg census). *Refinement*: do not hand-rename. Land a naming-audit static scan (`crates/core/tests/no_dta_walker_naming.rs`) that fails on any production-code reference to `dta_walker`/`DTA_TABLE`/`tape walker` outside `#[cfg(test)]` and explicitly archived docs. The scan is the substrate-with-consumer; the renames retire when scan passes. Per `feedback_pluggable-components`: the naming policy is data, not 935 hand-edits.

### W5 — three refinements

8. **Production registry consumption by `bbnf-path` proc-macro.** Mid-tranche §6 row 2 — synthetic fixture in `crates/bbnf-path/src/registry.rs` is consumed by the macro instead of the production const. *Refinement*: W5.3's per-grammar `arena.rs` instantiation is the natural seam. The codegen emits a `pub const REGISTRY: RegistryDescriptor = ...` per grammar; the proc-macro reads through `bbnf_core::grammar::generated::<G>::REGISTRY`. Fixture goes away. This refinement crosses W5.3 and W5.1: **architectural transposition** — the proc-macro consumes through the same const the runtime arena consumes. Decision packet: in §8.
9. **Substrate-audit test as the substrate it tests.** W5.4's test enumerates every `pub` substrate. The test itself is a `pub` substrate (the `passes::tests` module). Per `feedback_pluggable-components`, the test is the consumer of itself; the rule is reflexive. *Refinement*: land the test as a small public function (`pub fn enumerate_pub_substrates() -> Vec<SubstrateRow>`) and a `#[test]` that asserts every row has a caller. The function is the substrate; the test is the consumer; the function is also useful at W4 census time per refinement 6.
10. **Per-grammar arena dedup as type-parameterised template.** W5.3 lands `Arena<G: StructRegistry>` and `Builder<G: StructRegistry, T: TypeDesc>`. *Refinement*: ensure the type bounds are precise enough that mistyping `Arena<JsonRegistry>` vs `Arena<JsonValue>` fails at compile time. The current spec leaves this implicit. Tighten W5.3's sub-gate to require: `cargo test test_arena_type_bounds -- --ignored` runs a trybuild fixture that asserts `Arena<JsonValue>` (wrong; not a registry) fails to compile.

### W6 — two refinements

11. **Floors block as the single trend artefact.** W6.1's `floors` block in `post-AZ-IV.json` compares against `post-AU.json` and `post-AZ-III.json`. *Refinement*: extend to compare against per-wave snapshots (§5 below), so the close matrix shows W3→W4→W5→W6 deltas in addition to AZ-III→AZ-IV. Trend visibility before final-close.
12. **Close-honesty checklist as machine-grep-able.** Per `audit/HARDENING-2026-05-01-boole.md`, the Carry Ledger format is necessary but insufficient; close-by-routing was the chronic failure. *Refinement*: every row in §Non-Routable Carries (33 items) gets a marker comment in PROGRESS.md (`<!-- CARRY-1 -->` through `<!-- CARRY-33 -->`); FINAL.md must reference each marker by ID with a resolving artefact path. The orchestrator runs `grep -c CARRY- docs/tranches/AZ-IV/FINAL.md` and asserts ≥ 33. Mechanical close-honesty.

## §3 Chronic Deferrals (≥ 3 tranches per Boole)

Boole's hardening identifies 13 of 15 items deferred ≥ 3 tranches. Disposition for AZ-IV:

| # | Item | Boole tranches | Disposition (proposal) | Defence |
|---|---|---:|---|---|
| 1 | Strict regen drift | 4 | **CLOSED at W0** | `audit/W0-regen.txt` 9/9 green; mid-tranche §2 W0 row 1 confirms. |
| 2 | Egraph Map preservation | 2 | **CLOSED at W0** | `audit/W0-map-preservation.txt`; not yet chronic. |
| 3 | Sheets parity | 4 | **CLOSED at W1** | mid-tranche §2 W1 row 5 confirms 133/133. |
| 4 | Tailwind regex_scan timeout | 4 | **W4** (W4.2 owns) | profile artefact + named hot regex op + non-watchdog measured row required at W4 close. NON-ROUTABLE per AZ-IV.md. |
| 5 | TS backend executable parity | 4 | **W5** (W5.2 owns) | Node-execute proof on twitter.json. NON-ROUTABLE. |
| 6 | Watchdog bench rows | 2 | **W6** (W6.1 owns) | `data_xl`, `tailwind`, `compile_css_l4` active measured rows; samply attribution names hotspot. NON-ROUTABLE. |
| 7 | JSON value/path vs sonic-rs | 4 | **W3 + W6** | W3.5 lands ≤ 5x sonic floor; W6.1 lands fat-LTO same-harness `bbnf_value_* ≤ sonic_value_*`. NON-ROUTABLE. |
| 8 | CSS named_color | 4 | **CLOSED at W1** | mid-tranche §2 W1 row 5 confirms CSS field-level GREEN; W0 Map preservation unblocked it. |
| 9 | PatternAnnotations migration | 5+ | **W4** (W4.5 owns) | every consumer migrated or PatternAnnotations deleted. NON-ROUTABLE. |
| 10 | Bootstrap/derive residue (sibling) | 4 | **CLOSED at W0** | mid-tranche §2 W0 row 10 confirms zero `bbnf_derive` hits. |
| 11 | DTA/dfa naming/cleanup | 5 | **W4** (W4.4 owns) | rename `dfa_codegen.rs` → `regex_scan_adapter.rs`; `dta.rs` → `grammar_facts.rs`; static scan per refinement 7. NON-ROUTABLE. |
| 12 | Backend/rust/view/color hack | 5+ | **CLOSED at W1** | mid-tranche §2 W1 row 12 confirms shim + peel + re-export deleted. |
| 13 | Substrate denominator | 4 | **W4 + W5** | W4 lands the census (refinement 6); W5 lands the CI test (refinement 9). NON-ROUTABLE. |
| 14 | Rewrite/ruler production wiring | 4+ | **W4** (delete per refinement 4) | floor disposition: delete; BA recreates clean. NON-ROUTABLE. |
| 15 | WASM/sibling derive residue | 4 | **CLOSED at W0** | sibling locks clean; csp-solver canonicalised. |

**Closed-at-W0/W1**: items 1, 2, 3, 8, 10, 12, 15 (7 items; 54%). The mid-tranche audit's evidence ledger confirms each.

**Live in W3-W6**: items 4, 5, 6, 7, 9, 11, 13, 14 (8 items; landing in waves named).

**No chronic deferral routes to recycled BA.** BA's seed (path_seed.rs per refinement 2; rewrites/ deletion per refinement 4) is *new substrate*, not chronic carry. Item 14's deletion (closed at W4) is precisely the precondition for BA opening clean per BA.md §AZ-IV dependency item 1.

**No item routed to deletion as obsolete.** Item 12 (view/color shim) was deleted at W1; item 14 (RuleSet) deletes at W4. Both are operational deletions, not obsolescence routings.

## §4 Bench/Test Readiness

**User asks: "when can we start benching and testing?"**

### Benching

The bench harness is alive at HEAD. `cargo bench-iter-{json,css,bbnf,sheets,compile}` aliases work today (`.cargo/config.toml:192-197`). `cargo bench-{json,css,bbnf,sheets,compile,iai}` carry the publication-grade fat-LTO numerics. Divan is the canonical harness; `feedback_no-warm-benches` and `feedback_bench-sequential-regression` hold.

**Early benches landable in W3 (before W6 full close-matrix).**

| Bench | Wave | Justification |
|---|---|---|
| `bbnf_get_twitter` lazy lane (samply attribution) | W3.5 | sonic-rs ≤ 5x floor is W3 hard gate 7. Cannot defer to W6. |
| `bbnf_parse_with_*` × 4 grammars (sub-30s) | W3.5 | W3 sub-gate requires per-grammar same-harness sonic comparison. |
| Tailwind regex profile (samply) | W4.2 | Boole §b.2 row 4 chronic-defer cause: no profile artefact ever produced. W4 close requires the profile + named hot op. |
| Wasm-crossing attribution (W5.5) | W5.5 | wasm-bindgen FFI cost claim must cite a samply artefact. |
| Per-grammar dedup regression check (W5.5) | W5.5 | ≤ 5% regression on twitter parse vs W4 close (W5 hard gate 10). |

**Yes — benches start now in W3, with W3.5 dispatching the per-grammar `parse_with` bench harnesses + samply pass within the wave.** W6.1 lands the canonical close matrix (5 harnesses × multiple entries) with seven-artefact contract; W3-W5 land focused snapshots (§5 proposal) so trend is visible before final close.

### Testing

**Workspace nextest at HEAD: 1582 passed / 0 failed / 2 timed out / 26 skipped** (mid-tranche §9 confirms; W2 close).

**Test gaps blocking W6 close.**

*The 2 timeouts.*
1. **Tailwind perf** — sibling of carry #4. W4.2's profile artefact + named hot op resolves it. Per AZ-IV §Hard Gates 5, the timeout closes when the regex_scan adapter resolves the timeout class without per-call map overhead. Profile-driven; no test rewrite.
2. **LSP completion** — sibling-state test (per Boole §a.2 Bottleneck 3). Two paths: (a) fix the LSP binding root cause (preferred per `feedback_no-workarounds`); (b) `#[ignore]` with owner + deadline + ticket. Decision in §8.

*The 26 ignored tests.*
- Mid-tranche §2 W1 row 3 flags this: "Every `#[ignore]` carries owner/deadline/reason/ticket — NOT YET VERIFIED EXPLICITLY". The 26 skips include legitimate cfg-gates (e.g., `cfg(target_os = "linux")` for iai-callgrind). Per AZ-IV §Hard Gates 6, every `#[ignore]` must carry the triplet; ignores without it fail close-honesty.
- **Triage triggered at W6.2** (workspace gates sub-unit): orchestrator-owned audit. Each ignored test gets one row in `docs/tranches/AZ-IV/audit/W6-ignore-triplet-audit.md`. Triple: owner = name, deadline-commit = future commit hash, reason = one sentence, ticket = `docs/tranches/AZ-IV/audit/W6-ignore-tickets/<n>.md` (or routes back to a W4/W5 fix).

**Per-test triage lives at W6.2** with audit doc; the work is mechanical (inspect each `#[ignore]` source, classify, document). Estimated wall: ≤ 30 min per ignored test × 26 = 13 hours; absorb across W6 timeline.

**No new test-gap blockers.** The mid-tranche audit's §10 verdict ("close cleanly with residual carries enumerated") holds.

## §5 Bench Gates Suggestion — Wave-Bridging Snapshots

**Proposal**: after each wave close (W3, W4, W5), commit a focused bench snapshot to `docs/benchmarks/wave-az-iv-W{N}.json` (≤ 5 rows). Trend visibility before W6 full matrix.

**Justification.**
- Boole §b.2 chronic-defer rows 4, 6, 7 share a common cause: bench evidence does not refresh between waves. Tailwind perf, watchdog rows, sonic-rs perf — all four-tranche carries — were never measured row-by-row across waves. The W6 full matrix arrives once at close; if it surfaces a regression rooted in W3 or W4, the orchestrator cannot bisect cheaply.
- Trend snapshots are cheap to commit (≤ 5 rows, ≤ 10 KB JSON each); they ride the existing `cargo bench-iter-*` aliases (105 s per harness cold per Boole §a.1); the orchestrator runs them at wave close in one alias each.
- Per `feedback_bench-sequential-regression` and `feedback_no-warm-benches`, trend snapshots use the same discipline as the close matrix (cold per-parse, sequential).

**Snapshot row spec.**
- Fixture: `twitter.json` (JSON value), `bootstrap.css` + `tailwind.css` (CSS L4), `simple.json` (sheets), `bbnf-self-host.bnf` (BBNF).
- Measure: median, p99, sample count.
- Schema: subset of `docs/benchmarks/SPEC.md` (`tag`, `tranche`, `kind: wave-mid`, `commit`, `arch`, `profile: bench-iter`, `benches`); skip `floors`, `competitors` (W6 owns those).

**Per-wave snapshot.**
- `wave-az-iv-W3.json` — `bbnf_value_twitter` + `bbnf_get_twitter` + `bbnf_parse_with_{json,css_l4,sheets,bbnf}` (5 rows). Closes W3.
- `wave-az-iv-W4.json` — `bbnf_value_twitter` (regression check) + `compile_pipeline.css_l4` + `tailwind` (post-fix) + `data_xl` + `bbnf_monolithic` (5 rows). Closes W4.
- `wave-az-iv-W5.json` — `bbnf_value_twitter` (post-dedup) + `compile_pipeline.json` + `bbnf-path-ts.compile_path` + `bbnf-path-ts.execute_path` + `wasm-crossing` (5 rows). Closes W5.

**Accept.** The proposal lands as a one-line addition to W3.md, W4.md, W5.md `## Verification Artefacts` and `## Hard Gate` sections. Implementation is one Bash script per wave (`scripts/wave-snapshot.sh <wave>`); script invokes `cargo bench-iter-<harness> --output-format json` with focused `--bench` filters. Falls under `feedback_pluggable-components`: snapshots are data, not policy.

## §6 Post-AZ-IV Trajectory

### Recycled BA scope (rule discovery)

BA opens after AZ-IV close per BA.md §AZ-IV dependency. The eight opening preconditions:
1. `crates/ir/src/rewrites/` does not exist (deleted at W4 per §3 row 14).
2. `StructRegistry` populated for JSON / CSS L4 / Sheets / BBNF (W2 + W4).
3. Tape path fully deleted (long-since closed; W2 hardening confirms).
4. `TypedPath<G, T>` + `path!` macro live (W2 close).
5. Lazy bail-out parse on 4 grammars (W3 close).
6. Permanent substrate-audit test passing (W5 close).
7. Workspace nextest 100% pass (W6 close, post-W6.2 triplet audit).
8. `cargo xtask regen --check` green 9/9 (W0 close).

**Seed delivered by AZ-IV.W4 (refinement 4 + 7).**

| AZ-IV deliverable | BA consumes |
|---|---|
| `egraph::ruler::*` deleted | BA recreates clean per BA.md §AZ-IV dependency item 1 |
| `CompileOptions::rewrites` deleted | BA recreates clean (no eprintln-sink anti-pattern) |
| `pipeline/compile.rs:560-573` rewrite | BA W0 lands the new chain pre-tested at W3-W5 |
| W4.4 naming-audit static scan | BA inherits the discipline; `recognize_*_pattern` is the canon |
| W3.0 `path_seed.rs` (refinement 2) | BA W0 ingests the seed under its RON schema; provenance: HandAuthored |
| W5.4 substrate-audit test | BA's new substrates (rewrites/, ruler/, schema, ranker, tiering) pass at every wave close |

**Grammar-colocated rewrite dirs scaffold.** AZ-IV does NOT scaffold `grammar/<name>/rewrites/`. BA W0 owns the regen-pipeline scan (`cargo xtask regen` discovers and compiles `grammar/<name>/rewrites/*.ron`). AZ-IV's W4 deletion is the floor; BA's W0 is the rebuild.

### Future cross-repo motion (per GESTALT §5)

| Item | Priority | Defence |
|---|---|---|
| `crates/csp-solver` → own repo | **HIGH (post-BA)** | canonical-source policy already declared between bbnf-lang and csc411 sibling. AZ-IV.W0 enforced diff-clean (22 shared files byte-identical). Repo split is the structural close on the canonical-source split; defer indefinitely is harmful (the split *exists* today). |
| `crates/egraph` → own repo | **HIGH (post-BA)** | per `feedback_general-infra-crates`. BA discovers rules over `IrNode`; the egraph machinery is the engine; `Language` impl is derived from existing enums per `feedback_derive-language`. The egraph crate is general-purpose infra by definition. Split after BA validates the API. |
| `crates/simd-scan` → own repo or into parse-that | **MEDIUM** | parse-that absorption may be cleaner (parse-that owns the regex engine; structural-scan is the SIMD complement). Decide post-BA when the consumer fingerprint is settled. |
| `xtask` → `crates/xtask` (rename + relocate) | **LOW (cosmetic)** | rename-only; can land in any future tranche. |
| `bbnf-regex` → sub-crate of parse-that | **MEDIUM** | regex source-of-truth lives in one place. AZ-IV.W2's path-lexer API was designed to survive this relocation cleanly per AZ-IV §Cross-Repo Future Work. |

**Highest priority post-AZ-IV cross-repo motion**: `crates/csp-solver` repo split. The canonical-source policy already exists; the split is the operational closure. Per `feedback_general-infra-crates`, csp-solver is general-purpose infrastructure that bbnf-lang patches in via `[patch.crates-io]`; making the repo authoritative dissolves the split's accidental complexity.

**Defer indefinitely**: `xtask` rename. The canonical generation entrypoint works; cosmetic relocation is not load-bearing.

## §7 Refined Wave Timing

Cadence anchor (orchestrator's actual W0/W1/W2 cadence per mid-tranche audit):
- W0 ≈ 4 hrs incl. triumvirate (REGEN: research + plan + 2 redress).
- W1 ≈ 4 hrs incl. triumvirate (W1-CLOSE: research + plan + 3 parallel redress).
- W2 ≈ 2 hrs round-based (clean rounds; minor scope deviation absorbed).

**W3 — Lazy Bail-Out Parse.**
- Headline: 5 sub-units (W3.0 + W3.1 + W3.2 + W3.3 + W3.4 + W3.5). Codegen-heavy (W3.3 emits new generated code per grammar).
- Estimate: 4-5 hrs. Rounds: (1) W3.0 + W3.3 sequential (regen integration); (2) W3.1 + W3.2 + W3.4 parallel; (3) W3.5 profile pass.
- Triumvirate likely on W3.3 (path-plan codegen scope reveal) — adds ~2 hrs if fired. Wall: 4-7 hrs.

**W4 — Optimization Substrate Activation.**
- Headline: 5 sub-units, leaf-level Cantor split. Highest-risk wave (rewrite-or-delete decision; tailwind profile; PatternAnnotations migration).
- Estimate: 5-6 hrs. Rounds: (1) W4.1 (delete `RuleSet` per refinement 4) + W4.4 (renames + static scan); (2) W4.2 + W4.3 parallel (CSP authority + shape/SIMD); (3) W4.5 (Pratt/view; consumes W4.1's deletions).
- Triumvirate likely on W4.1 (PatternAnnotations migration may exceed file bounds) — adds ~2 hrs. Wall: 5-8 hrs.

**W5 — TS Binding + Value-API + Substrate Audit.**
- Headline: 5 sub-units. Cdylib + wasm-bindgen + Node-execute + dedup template + audit test.
- Estimate: 4-5 hrs. Rounds: (1) W5.1 + W5.3 parallel (cdylib + dedup template); (2) W5.2 + W5.4 sequential (Node-execute consumes cdylib; substrate-audit consumes post-dedup surface); (3) W5.5 profile pass.
- Triumvirate unlikely if W4 delivers clean rewrite/ruler deletion. Wall: 4-5 hrs.

**W6 — Measurement And Close.**
- Headline: 3 sub-units; close ceremony.
- Estimate: 3-4 hrs. Rounds: (1) W6.1 (5-agent profile pass per PROFILING.md §Orchestration contract); (2) W6.2 (workspace gates) + W6.3 (close docs) parallel; (3) FINAL.md drafting + close-honesty checklist.
- Triumvirate possible if a non-routable carry triggers thesis-review (per AZ-IV.md §Non-Routable Carries discipline + Boole Amendment 3). Wall: 3-6 hrs.

**Total walltime W3 → W6 close: 16-26 hrs.** No wave can run truly parallel (each blocks the next per dependency analysis in §1). Only intra-wave sub-units run parallel.

**Wave-bridging snapshots (§5)**: ~30 min per wave × 3 waves = 1.5 hrs added, absorbed at wave close. Worth it: trend visibility prevents Boole-pattern chronic deferrals.

## §8 Decision-Point List (orchestrator decisions before W3 dispatch)

Each carries a default recommendation + rationale.

### D1 — Path egraph seed location

*Question.* Where does `path_seed.rs` (W2 Hard Gate 9 carry) land — `crates/ir/src/rewrites/path_seed.rs` (per W2 spec) or BA's eventual `crates/ir/src/rewrites/inferred/path_seed.ron` (refinement 2)?

*Default.* **BA's eventual schema (RON-keyed)**, with provenance: HandAuthored, tier: Class1. Per `feedback_pluggable-components`, hand-coded rules and inferred rules share the same registry.

*Rationale.* AZ-IV §Hard Gates 14 deletes `crates/ir/src/rewrites/` at W4 (and W4.1 absorbs the deletion per refinement 4). The W3 carry from W2 must land in a non-deletion-targeted location, OR the deletion at W4 must spare path_seed.rs. RON-keyed under BA's schema is the single canonical home; the deletion at W4 wipes only `pipeline/compile.rs:560-573` eprintln-sink and the `egraph::ruler::*` skeleton. BA W0 re-ingests path_seed.ron unchanged.

### D2 — RuleSet wire-or-delete

*Question.* W4.1's mechanism reads "either wire or delete". Is the floor wire (proves chain), or delete (BA recreates clean)?

*Default.* **Delete.** Per AZ-IV §Hard Gates 14 + `feedback_no-workarounds` + `feedback_abrogate-before-patch`.

*Rationale.* Wiring half-broken substrate to "prove the chain" is the Boole §b.2 row 14 chronic-defer pattern (4+ tranches). BA W0 recreates clean with the architectural improvements (e-graph residue split + VM oracle + ranker + tiering). AZ-IV's job is the deletion floor; BA's job is the rebuild. Halt-and-report if any sub-agent attempts to wire the chain mid-W4 (scope reveal).

### D3 — LSP completion timeout

*Question.* Two timeouts in mid-tranche §6: tailwind (W4) and LSP completion (sibling-state). Fix LSP root cause or `#[ignore]` with triplet?

*Default.* **`#[ignore]` with triplet at W6.2**, route owner = LSP-binding maintainer.

*Rationale.* Per Boole §a.2 Bottleneck 3, LSP timeout is sibling-repo state; bbnf-lang's W6 cannot fix it without scope leak. The triplet (owner + deadline + reason + ticket) is the close-honesty close per AZ-IV §Hard Gates 6. Ticket: `docs/tranches/AZ-IV/audit/W6-ignore-tickets/lsp-completion.md`. Halt-and-report if the LSP completion timeout proves fixable inside bbnf-lang scope (then route to W4 binding fix; otherwise the ignore-with-triplet path holds).

### D4 — Production registry consumption by `bbnf-path` proc-macro

*Question.* Mid-tranche §6 row 2 — synthetic fixture in `crates/bbnf-path/src/registry.rs`. Swap to production const at W4 or W5?

*Default.* **W5** (W5.3 per-grammar `arena.rs` instantiation is the natural seam; refinement 8).

*Rationale.* W4 is already heavy (5 sub-units, rewrite-or-delete, naming sweep). W5.3 already touches per-grammar `arena.rs`; adding the registry-const consumption is a one-line `pub const REGISTRY: RegistryDescriptor = ...` per grammar plus a proc-macro re-target. Mid-tranche §6 names this as "W4 or W5" disposition; W5 is cleaner.

### D5 — Per-wave bench snapshots (§5)

*Question.* Adopt the wave-bridging snapshot proposal?

*Default.* **Adopt.** One-line addition to W3.md, W4.md, W5.md `## Verification Artefacts`.

*Rationale.* §5 justification: 1.5 hrs total walltime; trend visibility eliminates Boole-pattern chronic deferral. Halt-and-report if W3.5's first snapshot reveals a measurement-discipline failure (e.g., the bench-iter profile diverges > 2x from bench profile expected baseline).

### D6 — substrate_audit infrastructure split (refinement 6 + 9)

*Question.* W4 census tool + W5 audit test share enumeration logic. Land the tool at W4 (refinement 6) or W5 (refinement 9)?

*Default.* **W4 lands the tool; W5 lands the test.** The test imports the tool's public function.

*Rationale.* W4.9 denominator is currently a *manual census* (per W4.md §Hard Gate 9). A manual census is the chronic failure mode (Boole §b.1 row 13). Land the tool at W4 (one new file, `crates/ir/src/passes/substrate_census.rs`); W5.4 imports it and adds the `#[test]`. Saves W5 effort; W4 census is machine-checkable from day one.

### D7 — Wave-snapshot script lifetime

*Question.* `scripts/wave-snapshot.sh` (§5 implementation). Land it at W3 (consumed) or in a meta-pre-W3 commit?

*Default.* **W3.5 lands it** (consumed within W3).

*Rationale.* Pre-wave commits drift from the wave they prepare. W3.5 is the first consumer; the script lands with its consumer per `feedback_substrate-with-consumer`.

### D8 — `simd-scan` future-work decision

*Question.* Per §6, `crates/simd-scan` → own repo OR into parse-that. Defer to BA close, or decide now?

*Default.* **Defer to post-BA.** Out of AZ-IV scope per AZ-IV.md §Cross-Repo Future Work. Record the priority MEDIUM in §6 of this audit; revisit at BA close.

*Rationale.* parse-that absorption may be cleaner once BA closes (the regex engine + structural-scan symmetry crystallises after BA's rule-discovery validates the API surface). Decision deferred is not the same as item deferred: the work is named, sequenced, and out of scope per the plan.

## §9 Conclusion (audit verdict)

W3-W6 critical-path is tractable on the cadence W0-W2 established (16-26 hrs walltime; no parallel waves; intra-wave sub-units parallel where bounds disjoint). Eight chronic-defer items (Boole §b.2 rows 4, 5, 6, 7, 9, 11, 13, 14) land in W3-W6 with NON-ROUTABLE language; seven items already closed at W0-W1 (Boole rows 1, 2, 3, 8, 10, 12, 15).

Twelve refinements proposed across W3-W6 align AZ-IV's invariants (typed materialisation, no orthogonal codepaths, direct-to-struct, grammar-authoritative) with the in-flight wave specs. The most load-bearing refinements: (1) W3 PathSchema unification (one parse entry-point for eager + lazy); (4) W4 RuleSet deletion as floor; (6) W4 substrate census tool fed to W5 audit test; (8) W5 production registry consumption replacing fixture; (12) W6 close-honesty as machine-grep-able.

Eight orchestrator decisions surface in §8; default recommendations align with `feedback_no-workarounds` and `feedback_pluggable-components`.

Bench/test readiness: harness alive; W3 starts focused benches now (W3.5 sub-unit). Wave-bridging snapshots (§5) provide trend visibility before W6 final close.

Post-AZ-IV trajectory: BA opens cleanly on the eight preconditions in BA.md §AZ-IV dependency. Highest-priority post-BA cross-repo motion is csp-solver repo split (canonical-source policy is operationally complete; repo split dissolves accidental complexity).

No discovered chronic deferral requires thesis amendment. All 13 Boole-identified chronic items have a defensible AZ-IV-internal path. The thesis (`AZ-IV.md` §Thesis) holds.

---

Authored 2026-05-02 by AUDIT-E for AZ-IV path-forward synthesis lane. Read-only audit; no source/plan changes outside this file.
