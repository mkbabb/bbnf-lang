# Tranche B — bbnf-error + bbnf-pipeline Foundation

## Gestalt

Tranche B builds the foundational substrate that every subsequent tranche path-deps on. The unified error trait + canonical wrapper land first per Pass A facility #4 — a single `BbnfError` trait that every per-crate error type implements, with a canonical `BbnfErrorKind` enum carrying the cross-crate composition boundary. Per-crate error types remain (each crate has its own concerns); the boundary type is the integration substrate. The pipeline coordinator consolidates `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/` into a single `bbnf-pipeline` crate per `feedback_directory-modules` and Pass B §1.c. The Lock 2 directory-rename pass lands here as the mechanical precursor to tranche C's substantive Layout-vocabulary fold — `bbnf-passes/src/passes/types/` renames to `bbnf-passes/src/passes/layout/` (the directory move is mechanical; the symbol-level renames belong to C). The naming canon audit fires across the workspace, identifying every retired-term reference (`TypeDesc`, `StructLayout`, `TypeMap`) for tranche C to fold.

The substrate's bone structure forms here. Tranche B is small in line-count but architecturally load-bearing: every error-handling concern across every subsequent tranche routes through `bbnf-error`; every pipeline-orchestration concern routes through `bbnf-pipeline`. The compositional discipline per Lock 4 (per-domain orthogonal optimisation) requires this foundation; the IR contract per Lock 5 requires the unified error wrapper.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| `bbnf-error` substantive impl + per-crate adoption | B.W0 | `BbnfError` trait + `BbnfErrorKind` enum compile; ≥ 5 downstream crates impl `BbnfError` |
| Pipeline directory module fix | B.W1 | `find crates/bbnf-pipeline/src -maxdepth 1 -name 'pipeline.rs'` returns nothing (only `pipeline/mod.rs`); `feedback_directory-modules` honoured |
| `bbnf-pipeline` consolidation | B.W2 | `crates/bbnf-pipeline/src/{pipeline, directives, validate}.rs` per master plan §4.2; pipeline compile driver works |
| Lock 2 directory rename complete | B.W3 | `find crates/bbnf-passes/src/passes/types` returns nothing; `find crates/bbnf-passes/src/passes/layout` returns the directory; downstream consumers unchanged (tranche C handles symbol rename) |
| Naming canon audit | B.W3 | `rg -nE 'TypeDesc\|StructLayout\|TypeMap\|type_projection\|type_collapsing\|schema_synthesis' crates/ docs/` returns the candidate-rename list; deferred to C.W2 for fold |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| B.W0 — bbnf-error substantive impl + per-crate adoption | `BbnfError` trait + `BbnfErrorKind` enum land; per-crate error types adopt impl | 2 parallel | trait compiles; ≥ 5 downstream impls; cross-crate composition test passes |
| B.W1 — Pipeline directory module fix | Collapse `crates/core/src/pipeline.rs` → `crates/bbnf-pipeline/src/pipeline/mod.rs`; eliminate flat-file + sibling-dir pair | 1 | directory module honoured; tests pass |
| B.W2 — bbnf-pipeline consolidation | All `crates/core/src/pipeline/{compile, directives, validate}.rs` move to `bbnf-pipeline`; compile driver wired | 2 parallel | pipeline crate compiles; pipeline driver smoke-tests parse one grammar |
| B.W3 — Lock 2 directory rename + naming canon audit | `passes/types/` → `passes/layout/` (mechanical); audit identifies every retired-term reference | 2 parallel | directory rename complete; audit doc lists every retired-term reference (deferred to C) |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| Skeletal `bbnf-error/` ready for substantive impl | A | A.W2 |
| Skeletal `bbnf-pipeline/` ready for substantive impl | A | A.W2 |
| Sister-crate path-deps registered | A | A.W2 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| `bbnf-error` ready for cross-crate consumption | C, D, E, F, G, H, I, J | (continuous; every tranche path-deps `bbnf-error`) |
| `bbnf-pipeline` ready for parser/codegen orchestration | C (parse), D (codegen), E (runtime template) | C.W0, D.W0, E.W0 |
| Lock 2 directory rename complete; naming canon list ready | C | C.W2 (substantive `TypeDesc` → `Layout` fold) |
| Audit doc lists retired-term references | C | C.W2 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | n/a (no tape concern in B) | — |
| 2 — Layout canon | partial-honoured | B.W3 (directory rename); substantive at C.W2 |
| 3 — Cursor + byte-skip | n/a | — |
| 4 — Per-domain orthogonal | substrate-prep | (bbnf-error enables clean cross-domain composition) |
| 5 — IR + per-backend | n/a | — |
| 6 — xtask source emit | n/a | — |
| 7 — `crates/path/` consolidated | n/a | — |
| 8 — Surpass SOTA | n/a | — |
| 9 — Slice-borrow primary | n/a | — |
| 10 — Pratt + SIMD auto-detected | n/a | — |
| 11 — Path-deps for sister crates | honoured | (continuous from A) |
| 12 — ser + gorgeous archive | honoured | (continuous from A) |
| 13 — No god directories | substrate-prep | (bbnf-pipeline directory cohesion) |
| 14 — Full grammar generalisation | n/a | — |
| `feedback_directory-modules` | honoured | B.W1 |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| `BbnfError` trait shape diverges between per-crate impls | B.W0 ratified shape: `BbnfError` carries `kind() -> BbnfErrorKind` + `context() -> &BbnfContext`; per-crate impls follow uniform pattern; CI gate runs trait-method-count check |
| Pipeline consolidation breaks downstream callers | B.W2 staged: pipeline crate lands compile-driver-only; downstream crates path-dep onto `bbnf-pipeline` after compile-driver smoke-tests pass |
| Directory rename (B.W3) introduces import-path breakage in transitive consumers | B.W3 mechanical: `git mv` + sed-based import rewrite + `cargo check --workspace` confirms green |
| Naming canon audit list is incomplete | B.W3 audit re-runs with broader pattern: `rg -nE '\b(TypeDesc\|StructLayout\|TypeMap\|LayoutDesc\|type[_-]projection\|type[_-]collapsing\|type[_-]elaboration\|schema[_-]synthesis)\b' crates/`; cross-checked against Pass A §1.7 + §1.13 + §6 |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| `bbnf-error` per-crate adoption rebuild time | ≤ 30s incremental | `cargo check -p bbnf-error -p <consumer-crate>` |
| Pipeline directory rename rebuild | ≤ 60s | `cargo check --workspace` post-rename |
| Generated-LOC budget | unchanged from A.exit (168,750 LOC) | per master plan §12.2 |

## Voice locks

Per master plan §14. Tranche B's prose register: unpretentious-academic; lilt at ~3% (lower because the substrate is mechanical); domain verbiage from compiler theory and applied analysis welcome.

## Closing posture

Tranche B closes with the foundational substrate ready for tranche C's parse + IR foundation work. The bbnf-error + bbnf-pipeline + Lock 2 directory rename together constitute the smallest viable substrate every subsequent tranche path-deps on. The naming canon audit list seeds tranche C.W2's substantive symbol-level fold.

The greenfield mandate carries: no quick solutions in error handling (the canonical wrapper is the integration substrate, not a per-call-site `unwrap()` discipline); no workarounds in pipeline orchestration (the consolidated pipeline is the singular driver, not a per-grammar bespoke flow). Lock 2's directory rename lands as mechanical precursor; the substantive fold belongs to tranche C.
