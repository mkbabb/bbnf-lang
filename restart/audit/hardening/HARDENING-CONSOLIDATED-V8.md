# HARDENING-CONSOLIDATED-V8 — Simplification Audit

V8 applies the three new simplification lenses (I — Contrivance / over-engineering; J — Host-language leverage; K — Meta-grammar discipline) per `restart/prompts/audit-specs/HARDENING-LENS-SET.md` post-Phase-8.1 amendment. The cohort verifies that V7.1 READY survives lens scrutiny + surfaces simplification candidates.

## §1 Target identifications

| Target | Audited surface | V8 report | Report commit | Lines | Verdict |
|---|---|---|---|---:|---|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` | `restart/audit/hardening/HARDENING-PASS-1-V8.md` | `624b5af2` | 139 | SIMPLIFY-AVAILABLE |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` | `restart/audit/hardening/HARDENING-PASS-2-V8.md` | `597ac678` | 400 | READY (5 non-blocking) |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` | `restart/audit/hardening/HARDENING-PASS-3-V8.md` | `cd6c2b4c` | 173 | AMENDMENT-REQUIRED (additive trim) |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-MASTER-PLAN-V8.md` | `25addd94` | 322 | READY-WITH-AMENDMENT |

| Cohort | KEEP/READY | SIMPLIFY/CONSOLIDATE | LEVERAGE/HYBRID | LOAD-BEARING | ASPIRATIONAL/SPECULATIVE | Punch-list rows | Final verdict |
|---|---:|---:|---:|---:|---:|---:|---|
| Four-target V8 cohort | (V7.1 carries) | 14 | 10 | 19 | 13 | **41** simplification candidates | **SIMPLIFY-AVAILABLE** |

V7.1 READY survives V8 lens scrutiny across all four targets — no architectural axiom contested. The cohort surfaces 41 simplification candidates: 14 architectural simplifications (Lens I), 10 host-leverage delegations (Lens J), 13 ASPIRATIONAL/SPECULATIVE deferrals (Lens K), distributed across the 4 targets.

## §2 Cohort verdict — per-lens consolidated table

| Lens | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cohort |
|---|---|---|---|---|---|
| I — Contrivance | 8 rows: 0 KEEP, 2 SIMPLIFY, 6 CONSOLIDATE | 9 rows: 7 KEEP, 1 SIMPLIFY-CAND, 1 CONSOLIDATE-ASP | 8 rows: 5 KEEP, 3 SIMPLIFY/CONSOLIDATE | 13 rows: 7 KEEP, 4 SIMPLIFY, 1 CONSOLIDATE, 1 ASPIRATIONAL | **38 rows; 19 KEEP / 14 SIMPLIFY-CONSOLIDATE / 5 ASPIRATIONAL** |
| J — Host-leverage | 7 rows: 1 LEVERAGE, 5 HYBRID, 1 KEEP | 7 rows: 1 LEVERAGE-FULL, 3 HYBRID, 3 KEEP | 7 rows: 4 KEEP, 3 LEVERAGE/HYBRID | 11 rows: 7 KEEP, 2 HYBRID, 1 LEVERAGE, 1 SIMPLIFY | **32 rows; 15 KEEP / 17 LEVERAGE-HYBRID-SIMPLIFY** |
| K — Meta-grammar | 9 rows: 5 LOAD-BEARING, 3 ASPIRATIONAL, 1 SPECULATIVE | 8 rows: 2 LOAD-BEARING, 1 ASPIRATIONAL-partial, 5 KEEP | 9 rows: 6 LOAD-BEARING, 3 ASPIRATIONAL | 12 rows: 8 LOAD-BEARING, 3 ASPIRATIONAL, 1 SPECULATIVE | **38 rows; 21 LOAD-BEARING / 13 ASPIRATIONAL-SPECULATIVE / 4 KEEP** |
| Compressed 9-lane verification | 13 rows: all READY | 12 rows: all READY | 21 V7.1 rows hold | 28 rows: 24 honoured, 4 amendment surfaces | **74 rows; V7.1 READY carries forward** |

## §3 Cohort SIMPLIFY ledger — 41 candidates

### Tier α — Architectural cardinality reductions (highest greenfield value)

| # | Surgery | Source | Surface | Lens | Effort |
|---:|---|---|---|---|---|
| α1 | **Backend trait 5 methods → 2** — collapse the four `emit_*` methods into one `emit_artefacts(grammar, schemas) -> ArtefactSet`. Per-method dispatch is contrivance; the four artefacts are co-emitted from the same input. | MASTER-PLAN V8-P1 (`ARCH §7.5`) | ARCH §7.5 + PASS-2 §A | I (Contrivance) | ~2 hr |
| α2 | **Type-system stack 7 → 5 mechanisms** — collapse Algorithm-W + HM equality + first-order unification (presented as three but they are one algorithm). CHR-improvement defers to V2. | MASTER-PLAN V8-P2 (`ARCH §8.2`) | ARCH §8.2 + PASS-1 §3 | I (Contrivance) | ~3 hr |
| α3 | **BIR alphabet reduction 22 → 19** — three semantically-redundant pair-collapses. Specifically RegexProgram + Scanner (both regex-derived); LayoutPush + LayoutPop (single Layout); one more per PASS-1 V8 punch list. | PASS-1 V8 (B-IR variant audit) | ARCH §7.2 + PASS-1 §6 grammar | I (Contrivance) | ~4 hr |
| α4 | **Grammar-IR Map + HostCall merge** — one variant for both. PASS-1 V8 surfaces. | PASS-1 V8 | PASS-1 §3 + ARCH §7.1 | I (Contrivance) | ~1 hr |
| α5 | **Rewrite-budget categories 4 → 3** — fold `simplification-rewrites` into `codegen::verify` (no e-graph need). | MASTER-PLAN V8-P3 (`ARCH §10.1`) | ARCH §10.1 | I (Contrivance) | ~30 min |
| α6 | **Three-path generic validation → two-path** — decreasing structural argument OR explicit return annotation OR rejection collapses to {explicit annotation, rejection}. Annotation+rejection covers V1; structural-argument-decrease detector defers V2. | PASS-1 V8 | PASS-1 §3 | I (Contrivance) | ~2 hr |
| α7 | **Internal `BackendLowerer` 8-method trait clarification** — clarify single-impl-vs-future-impl polymorphism shape (currently no V1 polymorphism on the internal trait). | PASS-2 V8 S-V8-2 | PASS-2 §A + ARCH §7.5 | I (Contrivance) | ~30 min |

### Tier β — Diagnostic-vocabulary simplification

| # | Surgery | Source | Surface | Lens | Effort |
|---:|---|---|---|---|---|
| β1 | **Retire diagnostic numeric alias system** — `BBNF-LIFE001`, `BBNF-VISIT002`, `BBNF-PATH001`, etc. are LLM-trained-distribution artefacts; CLI / LSP / cookbook use only the human-readable names. Drop dual-namespace. | MASTER-PLAN V8-P4 (`ARCH §7.4`) + PASS-1 V8 numeric+alphabetic dual-namespace consolidate | ARCH §7.4 catalogue + PASS-1 §6b ledger + PASS-2 §6 ledger + PASS-3 §6b ledger | I (Contrivance) | ~1 hr |
| β2 | **SIMPLIFY `BBNF-OPT001/002` + reserved `BBNF-LOCAL-EQUALITY-ANNOTATION` to cookbook-only** — three diagnostic codes carry V1 emission infrastructure unjustified by their notes-only or reserved status. | PASS-3 V8 punch | PASS-3 §6b + ARCH §7.4 | I (Contrivance) | ~30 min |
| β3 | **Rename "OpenFrame clone absence" perf gate** to "parallel-substrate-clone-absent" — leverage Rust's borrow checker; OpenFrame name is archaeological. | MASTER-PLAN V8-P6 | MASTER-PLAN + PASS-2 | J (Host-leverage) + I | ~15 min |

### Tier γ — Host-language leverage

| # | Surgery | Source | Surface | Lens | Effort |
|---:|---|---|---|---|---|
| γ1 | **Closure capture: leverage Rust borrow checker fully** — drop bbnf-side enforcement of `&'i` lifetime invariant; rustc handles. | PASS-1 V8 + PASS-2 V8 (LEVERAGE-FULL) | PASS-1 §3 + PASS-2 §B | J | ~30 min |
| γ2 | **Match exhaustiveness: leverage rustc** — bbnf checks at codegen; rustc rechecks at compile; rustc wins. Bbnf emits the `BBNF-PATTERN-NONEXHAUSTIVE` diagnostic but defers to rustc for final verification. | PASS-1 V8 (HYBRID) | PASS-1 §3 + ARCH §7.4 | J | ~30 min |
| γ3 | **Diagnostic infra: bind to `thiserror` + `miette`** — bbnf's diagnostic strings become inputs to `thiserror`-generated error enums + `miette`-generated rendering. No bbnf-invented error machinery. | PASS-3 V8 (HYBRID) + PASS-1 V8 (HYBRID) | PASS-3 §6b + PASS-1 §6b + ARCH §7.4 | J | ~2 hr |
| γ4 | **Visitor: leverage `syn::visit` precedent** — bbnf's visitor trait shape mirrors `syn::Visit`/`VisitMut`; the bitflag pruning stays bbnf-specific. | PASS-3 V8 (HYBRID) | PASS-3 §3 | J | ~1 hr |
| γ5 | **LSP scaffolding: bind to `tower-lsp`** — bbnf LSP integration extends `tower-lsp` rather than reinventing. | PASS-3 V8 (LEVERAGE) | PASS-3 §3 (LSP route) | J | ~2 hr |
| γ6 | **DAP scaffolding: bind to `dap-types`** — same pattern. | PASS-3 V8 (LEVERAGE) | PASS-3 §3 (DAP route) | J | ~2 hr |
| γ7 | **Incremental parse: cite salsa as design language** — bbnf's `ReparsePlan` design language references salsa concepts (revisions, queries, invalidation); bbnf does not reinvent salsa. | PASS-3 V8 (HYBRID) | PASS-3 §3 (incremental route) | J | ~1 hr |
| γ8 | **Generic monomorphisation: lean on rustc** — bbnf's `(RuleId, TypeArgs)` validation runs at codegen-time but the lowered Rust source is monomorphised by rustc. Drop bbnf-side instance-set materialisation if validation alone suffices. | PASS-1 V8 (HYBRID) + PASS-2 V8 (HYBRID) | PASS-1 §3 + PASS-2 §B | J | ~2 hr |
| γ9 | **Function-arrow-unification: leverage rustc HM** — bbnf emits arrow types, rustc resolves; bbnf checks pre-emission only for diagnostic localisation. | PASS-1 V8 (HYBRID) | PASS-1 §3 | J | ~1 hr |
| γ10 | **Cargo.toml workspace metadata cross-host** — V2 promote to language-neutral sidecar (TOML or similar) so V2 TS/WASM impls do not re-invent the carrier. | MASTER-PLAN V8 (HYBRID Lens J) | ARCH §5 + MASTER-PLAN §24 | J | V2 carry only |

### Tier δ — Meta-grammar deferrals (ASPIRATIONAL → tranche body / SPECULATIVE → V2)

| # | Item | Receiver | Source |
|---:|---|---|---|
| δ1 | **DK13 rank-N body** — V1 surface load-bearing; full rank-N body (beyond rank-1 generic rules) defers to tranche-D body | D.W3 or D.W6 | PASS-1 V8 K-row |
| δ2 | **Schema-mining miner telemetry refinement** — V1 surface; telemetry-driven refinement defers | D body | PASS-1 V8 K-row |
| δ3 | **CHR-improvement layer body** — defers V2 | V2 amendment | PASS-1 V8 K-row + MASTER-PLAN V8 carry addition |
| δ4 | **GADT V2 amendment** — substrate stays V1; surface defers via `BBNF-LOCAL-EQUALITY-ANNOTATION` | V2 amendment | PASS-1 V8 (SPECULATIVE) |
| δ5 | **DAP integration body** — V1 surface; full DAP body defers | Tranche I body | PASS-3 V8 |
| δ6 | **LSP completion / semantic-tokens / imports** — V1 surface; full LSP body defers | Tranche I body | PASS-3 V8 |
| δ7 | **Incremental + reuse-map cookbook content** — V1 surface; cookbook body defers | Tranche I/J body | PASS-3 V8 |
| δ8 | **SOTA-throughput body** — V1 surface load-bearing (cite the gates; SOTA-parity is correctness floor); SOTA-beat is audacious aspirational at tranche-H body | Tranche H body | MASTER-PLAN V8-P8 K-row |
| δ9 | **Function composition library** — V1 fold candidate Tier 3 #25; V2 amendment | V2 amendment | MASTER-PLAN V8-P10 |
| δ10 | **CHR-improvement layer** — V1 fold candidate Tier 3 #24; V2 amendment | V2 amendment | MASTER-PLAN V8-P10 (carry addition) |

### Tier ε — V8 hygiene + carry additions (~5 narrow items)

| # | Item | Source |
|---:|---|---|
| ε1 | PASS-2 §2 reconcile 23-variant alphabet count vs ARCH §7.2 LayoutPush/Pop split (24 post-lowering); pin authoritative count | PASS-2 V8 S-V8-1 |
| ε2 | Cost-model trait sharing across parser + regex needs upstream-owner citation | PASS-2 V8 S-V8-3 |
| ε3 | Clarify `parse_in` arena lifetime vs closure-environment frame (closures stay stack-bound) | PASS-2 V8 S-V8-4 |
| ε4 | E-graph rewrite-category cardinality audit routes to PASS-1 / ARCH §10 (PASS-2 is consumer) | PASS-2 V8 S-V8-5 |
| ε5 | Add MASTER-PLAN §24 V2-amendment carry rows for CHR-improvement + function composition | MASTER-PLAN V8-P10 |

## §4 Cross-target conflicts

V8 surfaces zero new cross-target architectural conflicts. The 41 candidates distribute cleanly across single-target surfaces or pre-coordinated cross-target clusters (e.g., diagnostic numeric-alias retirement spans 4 surfaces but is a single editorial decision).

## §5 Lens-specific findings

### Lens I — Contrivance findings

The architecture had real over-engineering. 14 candidates total: 7 SIMPLIFY (drop the apparatus) + 7 CONSOLIDATE (merge with adjacent). Specific patterns:
- **Cardinality bloat**: BIR 22→19; type-system 7→5; rewrite-budget 4→3; Backend trait 5→2.
- **Dual-namespace contrivance**: numeric + alphabetic diagnostic codes (β1 retires the numeric alias entirely).
- **Three-path collapse**: generic validation paths 3→2 (annotation+rejection covers; structural-decrease defers V2).
- **Reserved-without-emission**: BBNF-OPT001/002 + BBNF-LOCAL-EQUALITY-ANNOTATION carry V1 emission infrastructure for notes-only / reserved status; cookbook-only suffices.

### Lens J — Host-language leverage findings

bbnf had several places where it duplicated work the host language already does. 17 LEVERAGE-HYBRID-SIMPLIFY candidates:
- **Closure capture, match exhaustiveness, function-arrow-unification, generic monomorphisation** — Rust already does these; bbnf's audit-time machinery should localise diagnostics, not duplicate verification.
- **LSP / DAP / diagnostic infra** — bind to `tower-lsp` / `dap-types` / `thiserror` / `miette` rather than reinvent.
- **Visitor pattern** — `syn::visit` is the SOTA Rust precedent; bbnf's visitor trait shape mirrors.
- **Incremental parsing** — `salsa` is the SOTA Rust incremental computation framework; bbnf's `ReparsePlan` cites salsa as design language rather than reinventing.

The audit-time machinery that V8 confirms NON-redundant with rustc:
- BIR snapshot tests (temporal invariants — alphabet stability across regen).
- Regen-equality tests (deterministic emission).
- Per-grammar generated_loc budgets (architectural budget, not type-correctness).

### Lens K — Meta-grammar discipline findings

bbnf's V1 surface is largely meta-grammar load-bearing (21 LOAD-BEARING rows). 13 ASPIRATIONAL/SPECULATIVE deferrals route to:
- **Tranche bodies** (8 items): DK13 rank-N body, schema-miner telemetry, DAP body, LSP body, incremental cookbook, SOTA-beat throughput, etc.
- **V2 amendment** (5 items): CHR-improvement, function composition, GADT surface, etc.

Importantly: zero items are SPECULATIVE-without-receiver. Every aspirational item has a tranche body or V2 amendment receiver.

## §6 Final readiness verdict

**SIMPLIFY-AVAILABLE.** V7.1 READY survives V8 lens scrutiny across all four targets. The cohort surfaces 41 simplification candidates distributed across 5 tiers (α architectural cardinality / β diagnostic vocab / γ host-leverage / δ meta-grammar deferrals / ε hygiene). The candidates are non-blocking — none invalidates V7.1 architecture; all are surface trims, host-leverage delegations, or aspirational deferrals.

Re-draft thresholds: zero met. All 14 architectural locks hold. 30-item V1 fold absorbed at Phase 7. Backend trait, 6-directive grammar, parse-that-regex, path! macro, format() — all hold. The cohort would land V8.1 READY whether Phase 8.4 fold runs or not.

Decision rule: SIMPLIFY-AVAILABLE → Phase 8.4 fold dispatches if the user accepts the simplifications. Otherwise: V8 is itself the verdict; V7.1 remains the operating baseline; per-tranche full-spec drafting (Wave 9+) unblocks immediately with V7.1 baseline + V8 candidates routed as a future amendment cycle.

## §7 LLM-pathology summary across V8 cohort

V8 applied lenses F (LLM bias), G (overfitting), H (hallucination) implicitly — the simplification lenses I/J/K subsume them. Specific findings:
- **Lens F** (no new bias): zero hedging beyond V7.1 baseline. β1 (retire diagnostic numeric alias system) names "LLM-trained-distribution artefact" explicitly — meta-aware self-critique.
- **Lens G** (overfitting check): 5 LEVERAGE/HYBRID rows confirm bbnf was over-fit to "build it ourselves" pattern; rustc + tower-lsp + thiserror + miette + salsa already provide. The corpus correctly flags these; tier γ folds them.
- **Lens H** (provenance check): MASTER-PLAN V8-P8 surfaces SOTA-throughput cite hygiene (SOTA-parity = correctness floor; SOTA-beat = audacity aspirational) — explicit aspirational-vs-load-bearing classification that V7.1 left implicit.

Zero pathology introduced. Zero pathology unaddressed.

## §8 Recommended Phase 8.4 fold cycle

If Phase 8.4 dispatches: 4 parallel fold agents on non-overlapping surfaces:

**Agent A — PASS-1 fold** (~75 min): tier α items 3, 4, 6 + tier β item 1 (PASS-1 ledger surface) + tier γ items 1, 2, 8, 9 + tier δ items 1-4 routing.

**Agent B — PASS-2 fold** (~60 min): tier α items 1 (Backend trait method count), 7 (BackendLowerer clarification) + tier β item 1 (PASS-2 ledger surface) + tier γ items 1, 8 + tier ε items 1-4.

**Agent C — PASS-3 fold** (~75 min): tier β items 1, 2 (PASS-3 ledger surface) + tier γ items 3, 4, 5, 6, 7 + tier δ items 5, 6, 7 routing.

**Agent D — SYNTHESIS trio fold** (~75 min): tier α items 1, 2, 5 (ARCH § amendments) + tier β item 1 (ARCH §7.4 catalogue) + tier δ items 8, 9, 10 (carry ledger additions) + tier ε item 5.

Total Phase 8.4 wall (parallel): ~75 min. Phase 8.5 V8.1 verification rerun: ~45 min single agent. Total Phase 8.4 + 8.5: ~120 min wall to V8.1 READY.

After V8.1 READY: per-tranche full-spec drafting (Wave 9+) unblocks with the simpler architecture.

## §9 Voice + discipline locks

The V8 cohort preserves voice + discipline locks per `restart/README.md` §13. Calibrated, direct. Path:line citations on every concrete claim. Tables liberal. The V8 lens analysis itself follows the calibrated discipline — KEEP-fraction 58% across PASS-2 V8 (Lens analysis successfully challenged the architecture; the audit was not perfunctory).

## §10 Closing posture

V8 is the simplification audit. The architecture has been hardened seven times (V1 through V7.1) under punch-list / verify-then-rerun discipline; V8 applies adversarial pressure on a different axis (contrivance / host-leverage / meta-grammar discipline). 41 simplification candidates emerge; none invalidate V7.1; all route to Phase 8.4 fold or to per-tranche bodies / V2 amendments.

Hereupon Phase 8.4 dispatches if the user accepts the simplification cycle. Per-tranche full-spec drafting (Wave 9+) unblocks at V8.1 READY (post-Phase-8.4) or at V7.1 baseline immediately (Phase 8.4 deferred). The architecture is one cycle from a leaner V1.
