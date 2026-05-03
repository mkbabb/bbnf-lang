# Legacy Source Inheritance Index

The new tranche set at `restart/tranches/{A..J}/` is NOT a stub set. Every tranche inherits its substantive specifications from the legacy BA-BD plan-set at `docs/tranches/{BA,BB,BC,BD}/` — the comprehensive plans landed across Phase-2 and Phase-4 (BA at `7c6cec96`; BB at `4f34144b`; BC at `99556a9d`; BD at `58e108ad`).

## The legacy plan-set (~18,200 lines total)

| Tranche | Scope | Lines | Top-level | Wave files | Audit artefacts |
|---|---|---:|---|---|---|
| BA | Surgical foundation | ~3,850 | `docs/tranches/BA/BA.md` (~250) | `BA/waves/W{0,1,2,3,3a,3b,3c,4,4a,4b,4c,5,5a,5b,5c,5d,5e,6}.md` | `BA/audit/{research-anchors, W1-workspace-metadata-schema, W2-file-size-distribution, W2-fail-explicit-table, W5-substrate-identity-decision, W5-generated-parser-shape, W6-bbnf-aggregator-disposition}.md` |
| BB | Generality + optimisation | ~5,578 | `docs/tranches/BB/BB.md` (~165) | `BB/waves/W{0,0a,0b,1,1a,1b,1c,2,2a,2b,2c,3,3a,3b,3c,4,4a,4b,5,5a,5b,5c,6}.md` | `BB/audit/{research-anchors, W1-substrate-migration-per-grammar, W2-cohort-template-spec, W3-rank-tier-with-consumer, W5-pointer-syntax-decision, W5-visitor-bitflag-spec}.md` + cookbook stubs |
| BC | Backend ABI + multi-backend | ~4,458 | `docs/tranches/BC/BC.md` (~233) | `BC/waves/W{0,0a,0b,0c,1,1a,1b,2,3,3a,3b,3c,3d,3e,4,5,5a,5b,5c,5d,6}.md` | `BC/audit/{research-anchors, W0-typed-ir-variant-table, W0-sibling-baseline, W0-ascent-strategy-disposition, W3-crate-dependency-dag, W3-generated-output-relocation, W5-bbnf-regex-endpoint-decision, W5-parse-that-disposition, W6-bd-carry-contract}.md` + migration cookbook |
| BD | TS/WASM activation + sister-crate publication + parity | ~4,329 | `docs/tranches/BD/BD.md` (~178) | `BD/waves/W{0..6}.md` | `BD/audit/{research-anchors, W0-ts-procmacro-spec, W1-ts-emitter-spec, W2-wasm-pipeline-spec, W3-publication-order, W4-worktree-fixture-spec, W5-cross-backend-parity-spec}.md` |

## Inheritance mapping (master plan §5 ratifies)

Each new tranche absorbs substantively from one or more legacy tranches. Drafting agents consult the legacy paths during full specification.

| New | Title | Legacy inheritance source | Substantive carries |
|---|---|---|---|
| A | Workspace genesis | BA W0, W1, W2, W3a, W3b, W3c, W6 | 9-directory layered re-org; tape-residue scrub; grammar-leak excision from `bbnf-ir` (`shape_dict_bbnf.rs` deletion, `GrammarAuditTag::Custom`, registry resolver from workspace metadata); god-module splits; layout-lowering rename; path crate triplet rename; archive-ceremony precondition |
| B | bbnf-error + bbnf-pipeline | NEW (no direct legacy); references CENSUS shared-error pathology | unified error type construction; pipeline coordinator |
| C | Parse + IR foundation | BA W4a/W4b/W4c, partial; BC W0 (IR contract precursors); BA Phase-A audit Proposal 2 (IR fracture into bbnf-ir + bbnf-passes + bbnf-vm) | cursor + byte-skip unification; private parse core; public wrappers; legacy parse_with deletion sequenced; IR fracture |
| D | Codegen IR contract | BC W0a/W0b/W0c (typed IR contract spec + 22-variant table + smoke test); BC W1a/W1b (Rust emitter refactor + regen-equality) | the typed IR contract document; Emitter trait; Rust emitter refactor to consume typed IR |
| **E** | **Per-grammar declaration crates + runtime template (convergent pivot)** | **BA W5a/W5b/W5c/W5d/W5e (per-grammar direct-to-struct migration); BB W1a/W1b/W1c (CSS L4/BBNF/Sheets); BB W2a/W2b/W2c (cohort template emission)** | **the convergent pivot per Pass B §6: Lock 1 + Lock 13 + Lock 14 retire as one architectural movement** |
| F | Optimiser pipeline | BB W3a/W3b/W3c (CSP layout passes + e-graph + miners + cost model + Pratt + SIMD detection + rank/tier with same-wave consumer) | optimiser composition by output-piping; auto-detection from grammar shape; rank/tier rewrites |
| G | Slice-borrow API + pointer macro + visitor | BB W4a/W4b (slice-borrow + escape hatches); BB W5a/W5b/W5c (pointer! + LazyValue + Visitor + VisitTypes); cookbook stubs | three-surface API; sonic-class pointer macro; lightning-css-class visitor surface |
| H | TS + WASM emitters | BC W2 (scaffold deferred-activation); BD W0 (TS proc-macro implementation); BD W1 (TS runtime emitter); BD W2 (WASM pipeline) | TS proc-macro; TS source generator; wasm-bindgen pipeline; cdylib targets; npm shipping |
| I | Sister-crate publication | BC W5a/W5b/W5c/W5d (sister-crate API freeze + bbnf-regex endpoint reconciliation + parse-that disposition + worktree fixture closure); BD W3 (publication order) | egraph + csp-solver + bbnf-regex publication; cargo-release + semver-checks + docs.rs metadata |
| J | Cross-backend parity + close | BD W4 (worktree fixtures); BD W5 (cross-backend parity); BD W6 (close) | 81-cell parity matrix; final perf gates against sonic-rs / simdjson / lightning-css; close ceremony |

## Master-plan reconciliations (already ratified)

The synthesizer's `restart/audit/master-plan/MASTER-PLAN.md` ratified these prior-pass outputs. Tranche-drafting agents do NOT relitigate these:

- **33 workspace crates** total — 9 per-grammar declaration + 8 core substrate (`bbnf-error`, `bbnf-pipeline`, `bbnf-grammar`, `bbnf-parse`, `bbnf-ir`, `bbnf-passes`, `bbnf-vm`, `bbnf-codegen-ir`) + 5 codegen/runtime + 3 path crates (`path-core` / `path` / `path-ts`) + 1 each of bench/LSP/CLI/aggregator + 5 sister/optimiser
- **Convergent pivot at Tranche E** — Lock 1 + Lock 13 + Lock 14 retire as one
- **IR fracture** — `bbnf-ir` (types/registry/DAG only) + `bbnf-passes` (every transformation) + `bbnf-vm` (bytecode VM)
- **Path triplet** — `path-core/` shared + `path/` Rust proc-macro + `path-ts/` TS cdylib
- **Commit chain Option 3** — keep verbatim + branch reset (tag `pre-restart-2026-05-03`; new branch `master-greenfield-2026-05-03`)
- **Docs structure** — `docs/{lang, perf, howto, process, spec}/` + GESTALT + README

## Inheritance discipline

When drafting a new tranche from legacy BA-BD waves:

1. **Consult the legacy waves verbatim** — read `docs/tranches/BA/waves/W*.md`, `BB/waves/W*.md`, etc. — do NOT paraphrase from memory.
2. **Inherit milestones, gates, exit-criteria** — the new wave keeps the legacy wave's milestones (M0-M{n}) where the substance still applies; renames where the substrate has shifted; deletes where the convergent pivot retires the milestone.
3. **Re-anchor to the new workspace shape** — every legacy `crates/core/src/<x>/` reference rewrites to its post-restart path (e.g., `crates/core/src/source/` → `crates/bbnf-parse/src/source/`).
4. **Honour Lock 14** — the legacy wave specifications were drafted before Lock 14 codification (commit `74f2ed25`); audit every legacy wave for grammar-overfitting residue and excise.
5. **Cite path:line** — every claim in the new wave cites either the legacy source (`docs/tranches/BA/waves/W3a.md:42`) or the new substrate (`crates/bbnf-parse/src/source/mod.rs:18`).
6. **Voice migration** — legacy waves carry the prior-era voice; the new tranche carries `docs/precepts/instructions/STYLE.md` voice. Calibrate.

## Closing posture

Hereupon the inheritance source is named, mapped, and disciplined. The drafting agents at `restart/tranches/{A..J}/` consume from the legacy paths, ratify against the master plan, and produce fully-specified tranches honouring the 14 locks + the precepts + the greenfield mandate.
