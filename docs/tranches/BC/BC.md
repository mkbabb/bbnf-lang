# Tranche BC — Cleanup Pass + Discipline Codification

**Status**: planned. Opens after BB close.
**Base**: master (post-BB close commit).
**Letter discipline**: repurposed at master `40092b28`. The previous BC tranche ("Shared Precepts Consumer Rollout" — orchestration meta-tranche; closed cleanly with the precepts submodule pinned at `e490e8ed`) is archived unmodified at `docs/tranches/BC/orchestration-archive-2026-04-30/`. Per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`, the canonical post-AZ-IV letter sequence is **AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+**. The BC letter is repurposed as the cleanup tranche; the orchestration content is preserved unchanged in the archive directory.

## Thesis

BC absorbs the residual carries that survived BA (direct-projection) and BB (rule-discovery) without invalidating either's thesis. The Audit-A TRANSPOSE bucket, the AUDIT-B routed splits (`runtime/css_l4/builder.rs` 1014 LOC, `passes/types/mod.rs` 786 LOC, `csp_strategy/mod.rs` further splits), the worktree fixture symlink contract finalisation, the samply 7-artefact contract canonicalization (no more environmental gating), the post-BA-and-BB substrate-audit residual, and the cross-repo discipline (csp-solver canonical-source split refresh; bbnf-regex sub-crate-of-parse-that resolution) close inside BC. The TS/WASM re-engineering tranche (BD candidate) opens after BC close; BC does not touch TS or WASM.

## Active Contradictions / Architectural Defect

The cleanup is residue, not a new architecture. Five residue classes survive BA + BB close:

1. **Audit-A TRANSPOSE bucket (12 items).** Items the deep audits classified as architectural transpositions that must happen but were not load-bearing for direct-projection or rule-discovery. They are non-trivial; they require their own waves.
2. **AUDIT-B routed splits.** `crates/core/src/runtime/css_l4/builder.rs` (1014 LOC), `crates/ir/src/passes/types/mod.rs` (786 LOC), and `crates/ir/src/passes/csp_strategy/mod.rs` are oversized files whose split was routed by AUDIT-B. Per `feedback_no-god-modules`, these split into directory-modules.
3. **Worktree fixture symlink contract finalisation.** BA.W0 codified the contract; BC verifies it survives BB and codifies any cross-grammar fixture symlink that BB's grammar-colocated `rewrites/*.ron` introduced. W6.2 known-miss closes here for the fleet.
4. **Samply 7-artefact contract canonicalization.** BA's W6 canonicalised the 7-artefact contract per perf claim; BC promotes it to the workspace lint cadence. Every PR that adds a perf claim must include the contract; CI gates it. Environmental gating retires for good.
5. **Cross-repo discipline.** csp-solver canonical-source split (bbnf-lang in-tree bench-authoritative; csc411 sibling algorithm-evolution-authoritative) needs a refresh after BB's e-graph saturation work touched the cost extractor. bbnf-regex resolution to sub-crate of parse-that is the path forward (per `feedback_general-infra-crates` and the AZ-IV §Cross-Repo Future Work section).

## Invariants

(BC-scoped; AZ-IV + BA + BB invariants persist + are extended.)

1. **Cleanup, not new architecture.** Every BC change retires residue. No new substrate lands; no new mechanism is invented. Where a substrate must be created (e.g., a new directory-module after a split), the substrate is consumed in the same wave.
2. **Substrate-audit GREEN.** Every BC change passes `crates/ir/tests/substrate_audit.rs`.
3. **No regression on `post-BB.json` close matrix.** Any BC change that regresses the close matrix reverts the change.
4. **Cross-repo discipline is named.** csp-solver, bbnf-regex, parse-that, pprint, csc411 — every cross-repo touch names the canonical-source policy and the verification path.
5. **TS/WASM punt persists.** BC does not touch TS or WASM backends. The re-engineering routes to BD with named successor letter.
6. **Close-honesty is the gate.** Every BC claim resolves to an artefact; every status word matches the latest gate run; every cross-tranche debt entry names BD or "no successor letter required" per the gate's actual disposition.

## BB Dependency (hard opening gate)

BC opens after BB close. The opening contract:

1. **Direct-projection codegen GREEN.** BA's value-API direct-projection persists; substrate-audit GREEN at BB close.
2. **Rule-discovery substrate landed and consumed.** `crates/ir/src/rewrites/` exists with discovered rules; `grammar/<name>/rewrites/` is populated; `cargo xtask regen` discovers per-grammar files.
3. **`post-BB.json` close matrix is the floor.** BC's close matrix `post-BC.json` cannot regress against `post-BB.json`.
4. **Workspace nextest 100% pass** at BB close.
5. **Cross-tranche debt rows from BA + BB are named** in BB.W6's FINAL.md; every row routes to BC explicitly OR is closed.

If any of these is not true at BB close, BC does not open. The carry routes back to BB per the non-routable-carries discipline.

## Carry Ledger — BA + BB Routed Items

| Carry | Source | BC destination | Close condition |
|---|---|---|---|
| AUDIT-B `runtime/css_l4/builder.rs` 1014 LOC split | AUDIT-B routed | W1 | directory-module split lands; max-file-LOC budget enforced |
| AUDIT-B `passes/types/mod.rs` 786 LOC split | AUDIT-B routed | W1 | directory-module split lands; cross-crate isomorphism per `feedback_directory_modules` |
| AUDIT-B `csp_strategy/mod.rs` further splits | AUDIT-B routed | W1 | directory-module split lands |
| Worktree fixture symlink contract — fleet-wide | BA.W0 + BB grammar-colocated rewrites | W2 | every grammar's `rewrites/*.ron` materializes via `xtask worktree-init` on worktree open |
| Samply 7-artefact contract canonicalization | BA.W6 | W3 | CI gates every PR with a perf claim; environmental gating retires; documented standing close discipline |
| Audit-A TRANSPOSE residue (12 items) | Audit-A | W4 | each item has a named close criterion; closed-or-routed at W4 close |
| csp-solver canonical-source split refresh | AZ-IV.W0 + BB cost-extractor work | W5 | diff-clean between bbnf-lang in-tree and csc411 sibling; canonical-source policy documented |
| bbnf-regex sub-crate-of-parse-that resolution | AZ-IV §Cross-Repo Future Work | W5 | `bbnf-regex` relocates to `parse-that/rust/bbnf-regex/`; bbnf-lang consumes via path-dep |
| TS Node-execute (F5 from BA) | routed to BD | not BC | BC does not touch TS or WASM; F5 routes to BD with named successor letter |

## Non-Routable Carries

The 15 items below cannot route to a successor letter. BC closes inside these or BC does not close.

| # | Item | Owner wave | Closure proof |
|---|---|---|---|
| 1 | `runtime/css_l4/builder.rs` directory-module split | W1 | each sub-module ≤ 500 LOC; no flat siblings |
| 2 | `passes/types/mod.rs` directory-module split | W1 | each sub-module ≤ 500 LOC; cross-crate isomorphism |
| 3 | `csp_strategy/mod.rs` directory-module split | W1 | each sub-module ≤ 500 LOC |
| 4 | Per-grammar `rewrites/` symlink contract | W2 | every grammar materializes via `xtask worktree-init` |
| 5 | `xtask worktree-init` covers BB-introduced fixtures | W2 | named test in `xtask/tests/worktree_init.rs` |
| 6 | Samply CI gate | W3 | `.github/workflows/perf-claim.yml` exits non-zero on PR with perf claim missing 7-artefact contract |
| 7 | Environmental-gating retirement | W3 | `docs/tranches/BC/audit/W3-env-gating-retired.md` cites every former env-gated row's resolution |
| 8 | Audit-A TRANSPOSE 12 items | W4 | each item has named close criterion at W4 close |
| 9 | csp-solver canonical-source policy refresh | W5 | bbnf-lang in-tree vs csc411 sibling diff-clean; policy doc updated |
| 10 | bbnf-regex relocates to parse-that sub-crate | W5 | `bbnf-regex` lives at `parse-that/rust/bbnf-regex/`; bbnf-lang consumes via path-dep |
| 11 | Cross-repo bench parity | W5 | csc411 + bbnf-lang both run the bench harness against their respective canonical sources |
| 12 | Substrate-audit GREEN at every wave close | W0..W6 | `crates/ir/tests/substrate_audit.rs` passes after each wave |
| 13 | No regression on `post-BB.json` matrix | W6 | `post-BC.json` shows zero regressions |
| 14 | TS/WASM punt routes to BD | W6 | `docs/tranches/BC/FINAL.md` cross-tranche debt names BD with named successor letter |
| 15 | Close-honesty checklist | W6 | every claim in FINAL.md resolves to artefact |

## Wave Table

| Wave | Agents | Closes on evidence | Status |
|---|---:|---|---|
| W0 - Truth + Cleanup-Substrate Inventory | 5 parallel | post-BB regen 9/9 green; substrate-audit denominator refreshed; AUDIT-B target file LOC verified; Audit-A residue inventoried | planned |
| W1 - AUDIT-B Routed Splits | 5 parallel | `runtime/css_l4/builder.rs`, `passes/types/mod.rs`, `csp_strategy/mod.rs` split into directory-modules; max-file-LOC budget enforced | planned |
| W2 - Worktree Fixture Symlink Contract | 5 parallel | `xtask worktree-init` materializes every grammar's fixture and `rewrites/*.ron`; named test; W6.2 known-miss fully closes | planned |
| W3 - Samply 7-Artefact Contract Canonicalization | 5 parallel | `.github/workflows/perf-claim.yml` gates every PR with a perf claim; environmental-gating retired; standing close discipline documented | planned |
| W4 - Audit-A TRANSPOSE Residue Absorption | 5 parallel | each of the 12 Audit-A items has named close criterion + evidence; closed-or-routed at W4 close | planned |
| W5 - Cross-Repo Discipline | 5 parallel | csp-solver canonical-source split refreshed; bbnf-regex relocated to parse-that sub-crate; cross-repo bench parity | planned |
| W6 - Measurement And Close + FINAL.md | 3 parallel | `post-BC.json` per SPEC.md; substrate-audit GREEN; samply 7-artefact contract; FINAL.md cites resolving artefact for every Hard Gate; close-honesty checklist | planned |

## Critical Files And Ownership

| Surface | Owner wave | Primary paths |
|---|---|---|
| Truth + cleanup-substrate inventory | W0 | `docs/GESTALT.md`, `docs/codegen-paths.md`, `docs/tranches/BC/**`, `crates/ir/tests/substrate_audit.rs`, `xtask/src/regen.rs` |
| AUDIT-B routed splits | W1 | `crates/core/src/runtime/css_l4/builder/**` (new dir), `crates/ir/src/passes/types/**` (split), `crates/ir/src/passes/csp_strategy/**` (split) |
| Worktree fixture symlink contract | W2 | `xtask/src/worktree_init.rs` (extend), `xtask/tests/worktree_init.rs`, `data/**`, `grammar/**/rewrites/**` |
| Samply CI gate | W3 | `.github/workflows/perf-claim.yml`, `xtask/src/perf_claim_check.rs`, `docs/precepts/instructions/PROFILING.md` |
| Audit-A TRANSPOSE residue | W4 | (per-item; named in W0 inventory) |
| Cross-repo discipline | W5 | `crates/csp-solver/**`, `parse-that/rust/bbnf-regex/**`, `Cargo.toml` (path-dep), `.cargo/config.toml`, `/Users/mkbabb/Programming/csc411/**` (sibling refresh) |
| Benchmark + profiling | W6 | `crates/core/benches/**`, `docs/benchmarks/post-BC.json`, `.profiles/samply/post-BC/**`, `docs/tranches/BC/FINAL.md` |

## Orchestration Rules

(Same as BA + BB: max six agents per wave; parallel writers use named sibling worktrees; orchestrator records `git status` before dispatch; empty-return = redispatch verbatim once then triumvirate; HARD CAP 30 min default; triumvirate auto-triggers per ORCHESTRATION.md §Triumvirate Auto-Triggers; sub-agent prompts ≤ 700 words.)

## Hard Gates

1. `cargo xtask regen --check` 9/9 green at every wave close.
2. `cargo nextest run --workspace --cargo-profile ax-iter` 100% pass at every wave close.
3. AUDIT-B target files: each post-W1 sub-module ≤ 500 LOC; cross-crate isomorphism per `feedback_directory_modules`.
4. Worktree fixture symlink contract codified for the fleet: every grammar's data + `rewrites/*.ron` materializes via `xtask worktree-init`. W6.2 known-miss fully closes.
5. Samply 7-artefact contract gated in CI; `.github/workflows/perf-claim.yml` exists; environmental-gating retired (named in `docs/tranches/BC/audit/W3-env-gating-retired.md`).
6. Each of the 12 Audit-A TRANSPOSE items has named close criterion + evidence at W4 close.
7. csp-solver canonical-source split refreshed: bbnf-lang in-tree vs csc411 sibling diff-clean; policy documented.
8. bbnf-regex relocated to parse-that sub-crate (`parse-that/rust/bbnf-regex/`); bbnf-lang consumes via path-dep; CI passes against the new location.
9. Cross-repo bench parity: csc411 + bbnf-lang both run the bench harness against their respective canonical sources.
10. `crates/ir/tests/substrate_audit.rs` GREEN at every wave close.
11. `docs/benchmarks/post-BC.json` per SPEC.md schema; zero regressions vs `post-BB.json`.
12. `docs/tranches/BC/FINAL.md` cites resolving artefact for every Hard Gate, miss, deletion, and handoff.
13. Cross-tranche debt names BD (TS/WASM re-engineering) explicitly; F5 routes to BD with named successor letter.
14. Close-honesty checklist passes.
15. `cargo fmt --all -- --check`, focused `cargo clippy --profile ax-iter`, and `git diff --check` pass.

## Deletion Bias

BC deletes before adding. Forbidden patterns:

- no `_v2` modules; no compatibility feature flags
- no flat-siblings split (`hir_leaf.rs` next to `hir.rs`); use directory-modules per `feedback_directory_modules`
- no Python binding path (Python is dropped from the thesis)
- no TS or WASM emitter regression compensation (TS/WASM punt; W5.2 RED gate routes to BD)
- no per-tranche static lint rule (lint discipline is workspace-level)

If deletion is unsafe because a current consumer exists, the wave names the consumer and refactors the surface to match its real role.

## Cross-Tranche Debt

- **BC opens after BB close**.
- **BD+ reserved** for TS/WASM re-engineering or shared-ABI tranche.

If a non-routable item cannot land inside BC without changing the BC thesis, the response is a triumvirate review of the thesis — not a new tranche letter.

## Brittleness Window

No tranche-wide brittleness window. A wave may declare a local brittleness window only in its wave spec.
