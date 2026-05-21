---
agent: 1E
pass: T-P1-excavation
cycle: V1
generated_at: 2026-05-21T05:57:03Z
spec_surfaces_audited: [restart/locks/LOCKS.md, skinny/REDRESS.md, skinny/RESULTS.md, restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/MIGRATION.md, restart/skinny/WORKSPACE.md, skinny/crates/]
files_audited_count: 2023
live_truth_method: "nl -ba + rg + find + wc -l over restart locks/skinny surfaces and skinny/crates; no cargo build or bench rerun"
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [L01-lazy-offset-tape, L08-css-l4-admit-drift, L13-bench-loc-exception, L14-generated-nonjson-admit, L15-skinny-profile-fat-lto, L16-asm-allowlist-audit-gap]
divergence_count:
  spec_claims_implemented: 7
  spec_claims_unimplemented: 5
  impl_exceeds_spec: 2
  unknown: 2
locks_amendment_candidates: 11
---

## Executive Summary

The locks are directionally intact but not all are current enough for the skinny evidence. Locks 1, 5, 6, 8, 10, 15, and 16 have live honoured evidence in the skinny; Locks 2, 7, 9, 11, 12, 13, and 14 are drifted or over-stated against the current root workspace and/or the skinny implementation. Lock 4 is partly silent on live implementation because the skinny carries layout facts and a recognizer/cost pass but not real egraph/CSP bridge crates. Lock 3 is UNKNOWN: the current skinny JSON path has no path cursor or `__EAGER_EMPTY_PATH` evidence, while the V1 spec keeps the API promise.

The largest amendment candidates are: update Lock 8 for SK-V12 CSS L4 admission while preserving full-SOTA user-pin gates; add Lock 14 allowances/criteria for generated non-JSON rows without weakening generic-crate fences; refine Lock 13 for generated and bench/reporting exceptions; refine Lock 15 to distinguish skinny workspace enforcement from root workspace drift; and require Lock 16 traceability from every intrinsic/`asm!` site to the allowlist plus parity tests.

## Spec-Claim ↔ Implementation Table

| Lock | Claim evidence | Implementation / skinny evidence | Verdict | LOC / risk | Wave alignment hint |
|---|---|---|---|---:|---|
| L01 tape substrate union | `restart/locks/LOCKS.md:52` | `skinny/crates/runtime/src/tape/mod.rs:94` stores source + offset/flag vectors; `skinny/crates/runtime/src/grammars/json/parser.rs:47` returns `JsonRoot` from `TapeBuilder`; REDRESS says Track 1/2 use same `TapeBuilder` at `skinny/REDRESS.md:110` and lazy-offset union at `skinny/REDRESS.md:246` | honoured, with amendment candidate | 100-300 LOC / medium | T-P3 A/F substrate closure |
| L02 layout lowering canon | `restart/locks/LOCKS.md:54` | `passes::layout` and `LayoutFacts` exist at `skinny/crates/passes/src/lib.rs:84`; but `Layout` and `LayoutSink` are absent in skinny, and `TypeFacts` is a public submodule at `skinny/crates/passes/src/lib.rs:112` | drifted | 150-300 LOC / medium | C.W1 layout API hardening |
| L03 cursor parse + byte skip unified | `restart/locks/LOCKS.md:56` | JSON parse is one implementation at `skinny/crates/runtime/src/grammars/json/generated.rs:17`, but no cursor/path implementation or `__EAGER_EMPTY_PATH` symbol was found by `rg` | UNKNOWN | 250-600 LOC / medium | G path/runtime cursor wave |
| L04 per-domain orthogonal optimization | `restart/locks/LOCKS.md:58` | Cost facts and priority shape derivation exist at `skinny/crates/passes/src/lib.rs:372`; `passes::bridge`, live `egraph`, and live `csp-solver` are spec-only in V1 docs (`restart/ARCHITECTURE.md:838`) not skinny code | silent-must-add | 500-1500 LOC / high | C.W4 bridge proof |
| L05 IR + per-backend lower | `restart/locks/LOCKS.md:60` | `BackendIr` and 5 `BackendShape`s exist at `skinny/crates/ir/src/lib.rs:392`; shape lowerer dispatch exists at `skinny/crates/codegen/src/lower/mod.rs:17`; SinkOnly lowers from BIR at `skinny/crates/codegen/src/lower/sink_only.rs:19` | honoured | 100-250 LOC / medium residual | C/F lowerer completion |
| L06 xtask emits committed source | `restart/locks/LOCKS.md:62` | Generated files carry `// @generated` at `skinny/crates/runtime/src/grammars/json/parser.rs:1`; `xtask` writes/checks generated JSON at `skinny/xtask/src/main.rs:121` and `skinny/xtask/src/main.rs:128` | honoured | 40-120 LOC / low | A/J regen discipline |
| L07 path crate split | `restart/locks/LOCKS.md:64` | V1 docs name `path`/`path-core` (`restart/ARCHITECTURE.md:72`), but root workspace still has `crates/bbnf-path` and `crates/bbnf-path-ts` in members at `Cargo.toml:2`; skinny skips path entirely per `restart/skinny/WORKSPACE.md:85` | drifted | 800-2000 LOC / high | G.W0-G.W4 |
| L08 surpass SOTA | `restart/locks/LOCKS.md:66` | RESULTS carries many JSON NO-GO rows (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:35`) and CSS L4 generated row beating lightningcss (`skinny/RESULTS.md:94`), while notes close overall `A / Go` at `skinny/RESULTS.md:145` | drifted / over-stated | 300-900 LOC / high | H + SK-V12 fold |
| L09 slice-borrow primary | `restart/locks/LOCKS.md:68` | Facade exposes borrowed `parse`, stub `parse_in`, and owned serde conversion at `skinny/crates/bbnf/src/lib.rs:75`; it does not accept a bump arena and `parse_owned` reparses canonical JSON via serde at `skinny/crates/bbnf/src/lib.rs:83` | drifted | 200-500 LOC / medium | B/G runtime API |
| L10 auto Pratt/SIMD/materialization | `restart/locks/LOCKS.md:70` | 5 shapes + 8 priority steps exist at `skinny/crates/ir/src/cost.rs:56`; `derive_backend_shape_with_diagnostics` chooses shape without directives at `skinny/crates/passes/src/lib.rs:387`; six-directive V1 surface is spec-side at `restart/ARCHITECTURE.md:1294` | honoured, partial | 300-800 LOC / high | C/H cost model |
| L11 path-deps + sister crates | `restart/locks/LOCKS.md:72` | Skinny has `parse-that-regex` and `bbnf-simd` only (`skinny/Cargo.toml:3`); root still has `crates/ser`, `crates/gorgeous`, `crates/simd-scan`, `crates/bbnf-path`, and `crates/bbnf-path-ts` as workspace members at `Cargo.toml:2` | drifted | 600-1800 LOC / medium | A.W0/A.W1/J.W3 |
| L12 ser + gorgeous archive before A.W0 | `restart/locks/LOCKS.md:74` | Root workspace still includes `crates/ser` and `crates/gorgeous` at `Cargo.toml:2`; `restart-archive-2026-05-04/` exists but is not the live workspace archive removal | drifted | 50-150 LOC / low | A.W0 precondition |
| L13 no god dirs / file >500 LOC | `restart/locks/LOCKS.md:76` | Skinny generated and bench files exceed 500 LOC: `runtime/.../json/generated.rs` 835 LOC, `bbnf-bench/src/report.rs` 3732 LOC, `passes/src/lib.rs` 1748 LOC (`wc -l` audit); REDRESS explicitly redresses bench/xtask caps at `skinny/REDRESS.md:299` | over-stated | 800-2500 LOC / medium | A tree-shape + bench exception |
| L14 full grammar generalisation | `restart/locks/LOCKS.md:78` | Old pending JSON leaks were recorded at `skinny/REDRESS.md:460`; later neutralization passed at `skinny/REDRESS.md:2418` and `skinny/REDRESS.md:2452`; current runtime exports grammar-named modules at `skinny/crates/runtime/src/lib.rs:3`, and codegen profiles are still grammar-specific at `skinny/crates/codegen/src/grammar_profile.rs:89` | drifted, with admitted generated exceptions | 700-2000 LOC / high | SK-V12 -> T-P3 3C |
| L15 profile + inlining + i-cache | `restart/locks/LOCKS.md:80` | Skinny `[profile.release]` has fat LTO, CGU=1, panic abort, debug true at `skinny/Cargo.toml:74`; generated hot functions use inline attrs at `skinny/crates/runtime/src/grammars/json/generated.rs:405`; root release remains thin LTO at `Cargo.toml:80` | honoured in skinny, drifted in root | 40-120 LOC / medium | A/J profile discipline |
| L16 SIMD/ASM allowlist | `restart/locks/LOCKS.md:87` | Grammar-neutral alphabet table dispatch exists at `skinny/crates/bbnf-simd/src/lib.rs:20`; checkasm command exists at `skinny/xtask/src/main.rs:26`; `asm!` uses exist for `udot`/`sdot` and prefetch/STNP at `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:39` and `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:6` | honoured, traceability UNKNOWN | 200-600 LOC / medium | H.W0 primitive admission |

## Divergences Catalogued

| ID | Lock | Divergence | Evidence | Classification | LOC / risk |
|---|---|---|---|---|---:|
| D-1E-01 | L02 | Lock claims public `Layout` and `LayoutSink`; skinny exposes `LayoutFacts` but not those two public names. | `restart/locks/LOCKS.md:54`; `skinny/crates/passes/src/lib.rs:84` | spec over-stated vs skinny | 150-300 / medium |
| D-1E-02 | L03 | Cursor/path elision claim has no live skinny symbol evidence. | `restart/locks/LOCKS.md:56`; `rg __EAGER_EMPTY_PATH skinny/crates` returned none | UNKNOWN | 250-600 / medium |
| D-1E-03 | L07 | Canonical unprefixed path crates not reflected in root workspace. | `restart/locks/LOCKS.md:64`; `Cargo.toml:2` | drifted | 800-2000 / high |
| D-1E-04 | L08 | Lock says surpass sonic/simdjson/lightning-css; current RESULTS has JSON NO-GO rows and a CSS L4 PASS-ADMIT row, not full universal SOTA closure. | `restart/locks/LOCKS.md:66`; `skinny/RESULTS.md:5`; `skinny/RESULTS.md:94`; `skinny/RESULTS.md:145` | over-stated | 300-900 / high |
| D-1E-05 | L09 | `parse_in` is not arena-aware in skinny; `parse_owned` returns serde JSON, not a self-contained generated owned document. | `restart/locks/LOCKS.md:68`; `skinny/crates/bbnf/src/lib.rs:79`; `skinny/crates/bbnf/src/lib.rs:83` | drifted | 200-500 / medium |
| D-1E-06 | L11 | Root workspace still carries crates the lock says are renamed/archived/removed from live workspace. | `restart/locks/LOCKS.md:72`; `Cargo.toml:2` | drifted | 600-1800 / medium |
| D-1E-07 | L12 | `ser` and `gorgeous` remain live root workspace members despite archive precondition. | `restart/locks/LOCKS.md:74`; `Cargo.toml:2` | drifted | 50-150 / low |
| D-1E-08 | L13 | 500 LOC rule is false for current skinny/reporting code unless generated/bench exceptions are explicit. | `restart/locks/LOCKS.md:76`; `skinny/REDRESS.md:299`; `wc -l` shows `skinny/crates/bbnf-bench/src/report.rs` 3732 LOC | over-stated | 800-2500 / medium |
| D-1E-09 | L14 | Generic codegen/runtime still contains grammar-profile shells; later REDRESS admits scoped generated non-JSON exceptions but LOCKS has only the older SK-V9 allowance. | `restart/locks/LOCKS.md:1`; `skinny/crates/codegen/src/grammar_profile.rs:89`; `skinny/REDRESS.md:3555`; `skinny/REDRESS.md:3824` | drifted / silent-must-add | 700-2000 / high |
| D-1E-10 | L15 | Skinny release profile honours fat LTO, root release profile still uses thin LTO. | `restart/locks/LOCKS.md:80`; `skinny/Cargo.toml:74`; `Cargo.toml:80` | drifted outside skinny | 40-120 / medium |
| D-1E-11 | L16 | Allowlist exists, but this pass did not prove every `core::arch::*`/`asm!` use-site traces to a current cited row. | `restart/locks/LOCKS.md:108`; `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:39`; `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:6` | UNKNOWN | 200-600 / medium |

## LOCKS-AMENDMENTS-CANDIDATE

| Candidate | Type | Lock(s) | Proposed amendment candidate | Supporting evidence | LOC / risk | Wave alignment hint |
|---|---|---|---|---|---:|---|
| LAC-1E-01 | refinement | L01 | Add lazy-offset tape with sparse flags as an admitted JSON-class shape under the existing tape union, while retaining EventTape/EagerTape/SinkOnly/CollapsedStage as cost-model choices. | REDRESS lazy-offset migration `skinny/REDRESS.md:246`; sparse flags `skinny/REDRESS.md:274`; tape storage `skinny/crates/runtime/src/tape/mod.rs:94` | 100-300 / medium | A/F substrate + C cost model |
| LAC-1E-02 | refinement | L02 | Clarify that `LayoutFacts` is live first; require `Layout` and `LayoutSink` to land before V1 public API freeze or remove them from the lock. | `restart/locks/LOCKS.md:54`; `skinny/crates/passes/src/lib.rs:84` | 150-300 / medium | C.W1 |
| LAC-1E-03 | addition | L03 | Add an explicit UNKNOWN verification row for cursor elision: require a unit/golden test proving empty path emits no cursor calls. | No `__EAGER_EMPTY_PATH` found; path work deferred by `restart/skinny/WORKSPACE.md:85` | 250-600 / medium | G.W1/G.W2 |
| LAC-1E-04 | refinement | L08 | Replace blanket skinny success wording with row-plane accounting: JSON parse/direct rows remain separately gated; CSS L4 declaration-values is a SK-V12 PASS-ADMIT, not full CSS parity. | JSON rows `skinny/RESULTS.md:5`-`skinny/RESULTS.md:35`; CSS row `skinny/RESULTS.md:94`; SK-V12 close `skinny/REDRESS.md:3824` | 300-900 / high | H + SK-V12 fold |
| LAC-1E-05 | refinement | L09 | State that `parse_in(input, &bump)` and true generated owned documents are V1 runtime obligations not yet proved by skinny facade. | `skinny/crates/bbnf/src/lib.rs:75`; `skinny/crates/bbnf/src/lib.rs:79`; `skinny/crates/bbnf/src/lib.rs:83` | 200-500 / medium | B/G runtime API |
| LAC-1E-06 | refinement | L11/L12 | Distinguish root legacy workspace drift from skinny workspace truth; require A.W0 archive/removal proof for `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, `bbnf-path-ts`. | Root members `Cargo.toml:2`; migration target `restart/MIGRATION.md:70`; `restart/MIGRATION.md:604` | 600-1800 / medium | A.W0/A.W1 |
| LAC-1E-07 | refinement | L13 | Add explicit exceptions and budgets for generated files and bench/report gate files; keep the 500 LOC rule for non-generated production modules. | REDRESS budget redress `skinny/REDRESS.md:299`; `wc -l` evidence for `report.rs` 3732 and generated JSON 835 | 800-2500 / medium | A tree-shape + bench hardening |
| LAC-1E-08 | addition | L14 | Add a post-SK-V12 generated non-JSON allowance: generated runtime under `runtime/src/grammars/<name>/` may contain grammar names if produced by the rostered generator and guarded by `lock14_baseline::validate`. | CSS row `skinny/RESULTS.md:94`; W1a legality `skinny/REDRESS.md:3555`; W5 close `skinny/REDRESS.md:3824`; runtime modules `skinny/crates/runtime/src/lib.rs:3` | 700-2000 / high | 3C lock amendment |
| LAC-1E-09 | refinement | L15 | Split enforcement clauses: skinny workspace release/bench profiles already fat-LTO; root workspace remains thin-LTO until V1 migration. | Skinny profile `skinny/Cargo.toml:74`; root profile `Cargo.toml:80`; REDRESS enforcement `skinny/REDRESS.md:258` | 40-120 / medium | A/J profile gate |
| LAC-1E-10 | refinement | L16 | Require a generated traceability manifest mapping every intrinsic/`asm!` use to Lock 16 allowlist row + parity test. | Allowlist verification claim `restart/locks/LOCKS.md:112`; `asm!` sites `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:39`, `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:6`; checkasm entry `skinny/xtask/src/main.rs:26` | 200-600 / medium | H.W0 primitive admission |
| LAC-1E-11 | removal/refinement | L14 top allowance | Retire or supersede the SK-V9-only allowance block once T-P3 folds SK-V12; it is now too narrow and stale as the only explicit allowance text. | Existing allowance `restart/locks/LOCKS.md:1`; later CSS/non-JSON admission `skinny/RESULTS.md:94`; `skinny/REDRESS.md:3824` | 30-80 / low | 3C lock amendment |

## Gaps / Missing Primitives

| Gap | Evidence | verify_action |
|---|---|---|
| `Layout` / `LayoutSink` public API is not live in skinny. | `restart/locks/LOCKS.md:54`; `skinny/crates/passes/src/lib.rs:84` | Add or remove from lock in C.W1; verify with `rg -n "struct Layout|trait LayoutSink" skinny/crates/passes/src`. |
| Path cursor empty-path elision has no live evidence. | `restart/locks/LOCKS.md:56`; `restart/skinny/WORKSPACE.md:85` | In G.W1/G.W2, add a golden emitted parser test for an empty path and assert no cursor consult symbol appears. |
| Root workspace still violates archive/name locks. | `Cargo.toml:2` | Before A.W0/A.W1 close, run `cargo metadata` and verify `ser`, `gorgeous`, `bbnf-path`, `bbnf-path-ts`, and `simd-scan` disposition matches locks. |
| Generic-crate grammar-name grep is not zero under current skinny because generated/runtime profile shells still carry grammar names. | `skinny/crates/runtime/src/lib.rs:3`; `skinny/crates/codegen/src/grammar_profile.rs:89` | Define generated/rostered exception scope, then run lock14 baseline plus direct `rg` over generic crates. |
| Lock 16 traceability is not mechanically proved by this pass. | `restart/locks/LOCKS.md:112`; `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:39` | Generate or hand-audit a primitive manifest: use-site, allowlist row, scalar parity test, corpus parity test, bench/admission row. |

## Open Questions

| UNKNOWN | Why unknown | verify_action |
|---|---|---|
| L03 cursor elision | No `__EAGER_EMPTY_PATH`, path cursor, or path crate in skinny; V1 docs only. | Implement or locate G-wave path cursor code; run `rg -n "__EAGER_EMPTY_PATH|Cursor|Skip" crates skinny/crates restart` and add emitted-code golden evidence. |
| L16 full allowlist coverage | This pass found allowlisted-looking `asm!` sites but did not trace every `core::arch::*` use-site to citations and tests. | Run `rg -n "core::arch|asm!" skinny/crates/bbnf-simd/src`, then build a row-by-row manifest against `restart/locks/LOCKS.md:87`-`restart/locks/LOCKS.md:112` and `skinny/crates/bbnf-simd/tests/`. |
