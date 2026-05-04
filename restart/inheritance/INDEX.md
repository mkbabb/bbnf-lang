# Inheritance Index — Legacy BA / BB / BC / BD

The greenfield restart at `restart/` does not start from blank — it starts informed. The legacy plan-set at `docs/tranches/{BA, BB, BC, BD}/` (~18,200 lines combined; landed across Phase-2, Phase-4, and the BA-redress) carries substantive inheritance for the new tranche set named A through J (or further).

This index is the pointer + survival ledger. Tranche-drafting agents (post-SYNTHESIS, post-HARDENING) consult per-wave from the legacy paths; the SYNTHESIS pass produces the consolidated migration plan in `restart/MIGRATION.md`.

---

## Legacy plan-set provenance

| Tranche | Title | Lines | Wave count | Phase-4 commit |
|---|---|---:|---:|---|
| BA | Surgical foundation | ~3,850 | 13 (W0/W1/W2/W3a-c/W4a-c/W5a-e/W6) | `7c6cec96` (post-redress) |
| BB | Generality + optimisation | ~5,578 | 15 sub-waves | `4f34144b` |
| BC | Backend ABI + multi-backend | ~4,458 | 14 sub-waves | `99556a9d` |
| BD | TS/WASM activation + parity | ~4,329 | 7 waves | `58e108ad` |
| **Total** | | **~18,200** | **49** | |

Status: drafted, committed, never executed. The plans exist as text; the codebase has NOT been modified per them. Per the prior Stage-1 PASS-B finding, even the BA option-(a) flip (workspace-wide OpenFrame retirement) was a planning decision — the code still has OpenFrame alive in 6 files / 109 mentions.

Disposition under the greenfield: relocate to `docs/tranches/archive/legacy-Y-BD/` at Tranche-A.W0 execution time per Pass C's recommendation. Until then, the legacy paths remain in place as the inheritance source.

---

## Per-new-tranche inheritance map

The synthesizer ratifies the final mapping; this index gives the starting point.

| New tranche (greenfield) | Title | Layer | Legacy substantive inheritance |
|---|---|---|---|
| **A** | Workspace genesis | meta | BA W0 (layered re-org); BA W1 (grammar-leak excision from `bbnf-ir`); BA W2 (god-module splits + layout-lowering rename); BA W3a/b/c (path triplet rename + path-core extraction); Pre-BA archive ceremony (Lock 12); commit-chain Option-3 execution; docs tree restructure (Pass-C) |
| **B** | Substrate foundation | bottom | NEW — `bbnf-error` design rationale from prior PASS-A §3; CENSUS error-handling pathology entries; `bbnf-pipeline` phase-state coordinator from prior MASTER-PLAN §5 row B |
| **C** | Optimisation pipeline | bottom | BB W3a (CSP layout passes); BB W3b (e-graph + miners); BB W3c (rank/tier rewrites + Pratt + SIMD detection with same-wave consumer per Era V abrogation); BB W3 audit `W3-rank-tier-with-consumer.md` |
| **D** | BBNF extensions + bootstrap | bottom | NEW (no direct legacy); ffuzzy three primitives (rewrite-mode + lookbehind + Unicode sets); generics + `@host fn` + `@error` + `@layout` + multi-function chaining + closure semantics (greenfield additions per `restart/README.md` §5) |
| **E** | Codegen IR contract | middle | BC W0a (typed IR contract spec); BC W0b (Rust lowerer smoke test); BC W0c (sibling baseline + AscentStrategy); BC W1a (full Rust emitter refactor); BC W1b (regen-equality); BC audit `W0-typed-ir-variant-table.md` (22-variant starting point) |
| **F** | Rust lowerer + runtime template (convergent pivot) | middle | BA W4a/W4b/W4c (cursor + byte-skip unification; private parse core; public wrappers); BA W5a/W5b/W5c/W5d/W5e (per-grammar direct-to-struct migration); BB W1a/W1b/W1c (CSS L4/BBNF/Sheets — REDUNDANT under option (a) per BA-redress; the per-grammar OpenFrame retiral folds into Tranche F); BB W2a/W2b/W2c (cohort template emission + cursor unification across all 9 grammars + byte-equal regression); BB audit `W2-cohort-template-spec.md` |
| **G** | Value API + path + visitor | top | BB W4a/W4b (slice-borrow + escape hatches; lifetime-surfaces cookbook); BB W5a (pointer + LazyValue); BB W5b (Visitor + VisitTypes); BB W5c (cookbook + diagnostic gates); BB audit `W5-pointer-syntax-decision.md` (option iii); BB audit `W5-visitor-bitflag-spec.md`; cookbook stubs at `docs/cookbook/{path-macro, lifetime-surfaces, visitors}.md` |
| **H** | Pratt + SIMD + WASM lowerer | middle | BC W2 (TS+WASM emitter scaffolds; deferred activation); BD W0 (TS proc-macro implementation — TS scope-deferred per Q28); BD W1 (TS runtime emitter — deferred); BD W2 (WASM compilation pipeline — substantive); BD audit `W2-wasm-pipeline-spec.md` |
| **I** | Error recovery + incremental + LSP | top | NEW (error recovery + incremental are greenfield additions per Q30 + Q32); BC W5a (sister-crate API freeze); BC W5b (bbnf-regex endpoint reconciliation — Option A: rename); BC W5c (parse-that disposition — Option i: permanent path-dep); BC W5d (worktree fixture closure); BC audit `W5-bbnf-regex-endpoint-decision.md`, `W5-parse-that-disposition.md`; the consolidated `bbnf-language-server` (analysis + lsp merger per Pass C) |
| **J** | Cross-backend parity + sister-crate publication + close | meta | BD W3 (sister-crate publication order; cargo-release + semver-checks + docs.rs); BD W4 (worktree fixture infrastructure); BD W5 (cross-backend parity verification — 81-cell matrix); BD W6 (BD close → carry-ledger CLOSED); BD audit `W3-publication-order.md`, `W4-worktree-fixture-spec.md`, `W5-cross-backend-parity-spec.md`, `W6-bd-carry-contract.md` |

---

## Inheritance discipline (per tranche-drafting agent)

When drafting a new tranche from legacy waves:

1. **Consult legacy waves verbatim** — read `docs/tranches/<BX>/waves/W*.md` directly; do NOT paraphrase from memory or from this index.
2. **Inherit milestones, gates, exit-criteria** — keep where substance still applies; rename where substrate has shifted (e.g., `crates/core/src/source/` → `crates/source/src/` per the new workspace shape with `bbnf-` prefix dropped from internal crates); delete where the convergent pivot or other greenfield decision retires the milestone.
3. **Re-anchor to the new workspace shape** — every legacy `crates/core/src/<x>/` reference rewrites per `restart/ARCHITECTURE.md` §1 (the 24-crate workspace; no `bbnf-` prefix on internal crates; `bbnf-` retained on user-facing aggregator + CLI + LSP + bench).
4. **Honour Lock 14** — the legacy waves were drafted before Lock 14 codification (commit `74f2ed25`); audit every legacy wave for grammar-overfitting residue and excise. Particular sites per the prior Stage-1 hardening reports: 28 cumulative Amendment-01-conflict sites in BA-BD plans needing reanchor to template-emitted subdirs + `host-prims`.
5. **Honour the BBNF extensions** — legacy waves predate ffuzzy three primitives + generics + `@host fn`; the new tranche text incorporates the extensions where they obviate a legacy milestone.
6. **Cite path:line** — every concrete claim cites either the legacy source (`docs/tranches/BA/waves/W5a.md:42`) or the new substrate (`crates/runtime/src/grammars/json/parser.rs:18`).
7. **Voice migration** — calibrated, archaic-permissive per `docs/precepts/instructions/STYLE.md`.

---

## What does NOT inherit

The following legacy artefacts do not survive the greenfield:

- **Per-grammar declaration crates** (Amendment 01 retraction; settled position Q1 + Q5 + Q14): no `crates/json/`, `crates/css-l4/`, `crates/bbnf-meta/`, etc. Per-grammar artefacts emit as template-generated subdirs at `crates/runtime/src/grammars/<name>/`.
- **The `bbnf-` prefix on internal crates** (settled position Q1): `bbnf-passes` → `passes`, `bbnf-ir` → `ir`, `bbnf-runtime` → `runtime`, `bbnf-codegen` → `codegen`, `bbnf-error` → `error`, `bbnf-pipeline` → `pipeline`, `bbnf-host` → `host`, `bbnf-grammar` → `grammar`, `bbnf-vm` → `vm`, `bbnf-cost-model` → `cost-model`. The `bbnf-` prefix is retained on user-facing crates only.
- **The 22-variant Backend IR table as final** (Q9 + PASS-1 refinement): the BC.W0 22-variant table is the **starting point**; PASS-1's IR Architect refines against grammar-IR shape + Backend IR shape distinction. Final cardinality is settled in the new `ir` crate.
- **The "convergent pivot at Tranche E" framing** (Stage-2 PASS-B Lane 2C finding): the convergent pivot sharpens to **staggered closures** — Lock 13 (god-directories) lands at Tranche A; Locks 1 + 14 land at Tranche F (the runtime template + ParseStream union).
- **The Tape rebranding moratorium under Lock 1** (Q25 settled position): tape's *name* dies; tape's *structural insight* survives as **ParseStream** — a contiguous-token-stream-with-offset-references that direct-to-struct values borrow into. The 2,000-commit failed unioning of the prior is the failure mode the greenfield's ParseStream union avoids.
- **Two-stage hardening protocol** (the prior contrivance): single-round hardening only. No Stage-2 EXTERNAL. No Stage-3 SUITE-META. The user has flagged the compounded prior `restart/`'s contrivance; the greenfield collapses to one PASS suite + one synthesis + one hardening.

---

## Closing posture

Hereupon the inheritance is named, mapped, and disciplined. The legacy plan-set survives at `docs/tranches/{BA, BB, BC, BD}/` until Tranche-A.W0 archives it; the survival ledger above gives the inheritance carries; tranche-drafting agents post-HARDENING consume per-wave with the disciplines above honoured. The greenfield is greenfield; what survives is what is viable, idiomatic, performant.
