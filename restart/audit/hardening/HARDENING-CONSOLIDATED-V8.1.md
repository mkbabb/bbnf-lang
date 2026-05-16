# HARDENING-CONSOLIDATED-V8.1 — Phase 8 Simplification Verification

V8.1 verifies that Phase 8.3.1 (corpus cleanup per user adjudications Q1-Q8) + Phase 8.4 (V8 simplification fold across 4 parallel agents) closed correctly + the cohort is ready for Wave 9 per-tranche full-spec drafting.

## §1 Target identifications

| Target | Audited surface | V8.1 report | Report commit | Lines | Verdict |
|---|---|---|---|---:|---|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` (post-Phase-8.4; commits `4c69b848` + `23311ff8`) | `restart/audit/hardening/HARDENING-PASS-1-V8.1.md` | `277910df` | 196 | READY |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` (post-Phase-8.4; commits `831b2f90` + `1a75ea53`) | `restart/audit/hardening/HARDENING-PASS-2-V8.1.md` | `fe36af42` | 330 | READY |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` (post-Phase-8.4; commits `85187a74` + `bd213632`) | `restart/audit/hardening/HARDENING-PASS-3-V8.1.md` | `7d8f03ea` | 266 | AMENDMENT-REQUIRED-NARROW |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` (post-Phase-8.4; commits `c72318cd` + `e5cb1e4b`) | `restart/audit/hardening/HARDENING-MASTER-PLAN-V8.1.md` | `0374d7ef` | 651 | READY |

| Cohort | READY | AMENDMENT-REQUIRED-NARROW | RE-DRAFT | Active blocking items | Final verdict |
|---|---:|---:|---:|---:|---|
| Four-target V8.1 cohort | 3 of 4 | 1 of 4 | 0 of 4 | **0** | **READY-WITH-NARROW-RESIDUE** |

The single AMENDMENT-REQUIRED-NARROW (PASS-3) carries 2 narrow residues. Neither blocks Wave 9 per-tranche full-spec drafting. The trio is the executable authority for Wave 9; per-tranche drafting agents read ARCH first, where the post-fold authoritative content lives.

## §2 Cohort verdict — per-target summary

### PASS-1 — READY

Phase 8.3.1 GADT V1 + CHR V1 + composition delete all closed. Phase 8.4 PASS-1 fold (10 V8 items + 4 cross-target absorbed) closed: Grammar-IR Map+HostCall merged into `Call { kind: Map | Host }`; BIR 22→19 referenced via ARCH §7.2 authoritative cite; generic validation 3-path → 2-path; numeric diagnostic aliases retired; 5 host-leverage delegations to rustc; rank-N body and schema-miner telemetry routed to tranche D body. Zero active V2-amendment language.

### PASS-2 — READY

Phase 8.4 PASS-2 fold (5 V8 punch items) closed: Backend trait 5→2 (`emit_artefacts` + `lower`); BIR 22→19 with 3 pair-collapses; BackendLowerer no-poly clarified; 7 numeric diagnostic aliases retired; γ1 + γ8 host-leverage to rustc; ε2/3/4 hygiene; bonus Lock-4 GADT-V1 drift fixed at PASS-2:201. Zero V2-amendment language. Backend trait + BIR alphabet cohort-coherent with ARCH §7.5/§7.2.

### PASS-3 — AMENDMENT-REQUIRED-NARROW

Phase 8.4 PASS-3 fold landed γ3-7 host-leverage (thiserror/miette/syn::visit/tower-lsp/dap-types/salsa) + δ5-7 tranche-body routing (DAP/LSP/incremental → tranche I body) + Phase 8.3.1 GADT V1 emission for `BBNF-LOCAL-EQUALITY-ANNOTATION` cleanly. **Two narrow residues remain**:

1. **PASS-3 §6b numeric alias retirement scoped only to BBNF1004**. The β1 retirement landed in ARCH §7.4 catalogue (29-row human-readable canon) but did not extend to PASS-3's §6b producer ledger, which still carries 14 numeric-aliased rows: `BBNF-LIFE001`/`BBNF-VISIT001`/`BBNF-PATH001`/`BBNF-LAYOUT001`/etc. ARCH §7.4 cites PASS-3 as source-of-truth for verbatim strings; cohort-coherence breaks at the identifier level.
2. **PASS-3:191 carries "V2 amendment surface" phrasing for closure broadening tied to Lock 1 reuse-map amendment**. Architecturally legitimate (broadening is real and tied to a real future Lock 1 amendment), but lies outside the V8.1 prompt's three named exclusions (TS/WASM via Backend, path-ts, WASM ABI). Single-line rephrase to "Lock 1 amendment surface" suffices.

Both residues fixable in ~5 min single-agent narrow amendment.

### MASTER-PLAN — READY

Phase 8.3.1 corpus cleanup verified at trio surfaces (Q1 GADT V1 in Lock 4 + ARCH §8.2; Q3 function composition library deleted; Q4 V5.1 prune; Q7 README §12 update; Q8 HANDOFF additions). Phase 8.4 SYNTHESIS-trio fold closed all 8 trio items: Backend trait 5→2 (ARCH §7.5); type-system 7→5 (ARCH §8.2); BIR 22→19+Return (ARCH §7.2); rewrite-budget 4→3 (ARCH §10.1); diagnostic numeric retire (ARCH §7.4 — 29 human-readable codes); cross-host metadata sidecar (ARCH §5 + MASTER-PLAN §24); SOTA-parity-vs-beat (MASTER-PLAN §4); V2-amendment ledger sweep complete. Zero V1-folded items survive as V2-deferred. Wave 9 readiness affirmed across all 10 per-tranche drafting input layers.

5 non-blocking residues documented at the MASTER-PLAN-V8.1 report — none invalidates Wave 9 dispatch.

## §3 Cross-target conflict resolution

V8.1 surfaces one cohort-level conflict:

| Conflict | Sources | Resolution |
|---|---|---|
| **Diagnostic numeric alias retirement: ARCH retired; PASS-3 §6b producer ledger did not** | ARCH §7.4 (post-Phase-8.4 SYNTHESIS fold `e5cb1e4b`) carries human-readable codes; PASS-3 §6b (post-Phase-8.4 PASS-3 fold `bd213632`) still carries 14 numeric aliases | **PASS-3 narrow amendment**: extend β1 retirement scope to all 14 PASS-3 §6b numeric aliases; align verbatim strings with ARCH §7.4 catalogue. Single agent; ~30 min. |

No other cross-target conflicts. No re-draft thresholds met. The architectural axioms hold — tape/direct union, Backend IR ownership, yaml two-surface, numeric SOTA, sequencing, generated-code budgets, carry ledgers, `path!`/`select!` macros, `@error(recover)`, OpenFrame archaeology, GADT V1 surface, CHR V1 fold.

## §4 Punch list consolidation — narrow residue

The V8.1 cohort surfaces 2 narrow PASS-3 amendments + 5 documented non-blocking residues at MASTER-PLAN. Total: 7 items, none blocking Wave 9.

### Blocking (PASS-3 narrow amendment)

| # | Surgery | Path:line | Severity |
|---:|---|---|---|
| V8.1-A | Extend β1 retirement to PASS-3 §6b — rename 14 numeric aliases (`BBNF-LIFE001`/`BBNF-VISIT001`/`BBNF-PATH001`/`BBNF-LAYOUT001`/etc.) to human-readable canon matching ARCH §7.4 | `restart/audit/pass-3-runtime/PASS-3.md:452-471` | Cohort-coherence (cite hygiene) |
| V8.1-B | Rephrase "V2 amendment surface" to "Lock 1 amendment surface" for closure broadening | `restart/audit/pass-3-runtime/PASS-3.md:191` | Voice + V2-retirement-discipline |

### Non-blocking (deferred to next pass-through)

| # | Item | Path:line |
|---:|---|---|
| V8.1-NB1 | MASTER-PLAN §27 synthesis verdict ledger says "REINVENT exact contract around 23 variants" (trio-internal; ARCH §7.2 authoritative) | `restart/MASTER-PLAN.md:51` |
| V8.1-NB2 | Tranche E §10 inheritance bullet says "PASS-2 BIR table. \| 23 variants" (trio-internal cite) | `restart/MASTER-PLAN.md:391` |
| V8.1-NB3 | ARCH §11 perf gate still names "OpenFrame clone absence" not "parallel-substrate-clone-absent" (β3 explicitly deferred per Phase 8.4 SYNTHESIS §27) | `restart/ARCHITECTURE.md:1501` |
| V8.1-NB4 | ARCH §3.1 missing V8-P11 V1-Rust-line cross-host divergence note (Lock 9 + §7.5 carry the truth implicitly) | `restart/ARCHITECTURE.md:191-237` |
| V8.1-NB5 | HANDOFF §3 still cites V8 SIMPLIFY-AVAILABLE as current verdict — V8.1 supersedes once consolidated lands (this commit) | `restart/HANDOFF.md:47` |

## §5 Final readiness verdict

**READY-WITH-NARROW-RESIDUE.**

3 of 4 targets return READY. PASS-3 returns AMENDMENT-REQUIRED-NARROW with 2 surgical edits. Neither residue blocks Wave 9 per-tranche full-spec drafting:
- Per-tranche drafting agents read ARCH first (where post-fold authoritative content lives); PASS-3 §6b numeric aliases are cited authoritatively from ARCH §7.4 (post-fold), not from PASS-3.
- Closure broadening at PASS-3:191 is a documented real Lock 1 amendment surface (legitimate scope partition); the wording change is voice-only, not architectural.

Decision rules:
- **Option A — Halt at V8.1 with documented residue + Wave 9 unblocks immediately.** Per-tranche drafting absorbs the 2 PASS-3 narrow surgeries as part of tranche-D drafting (since both touch substrate diagnostics + Lock 1 reuse-map closure). Recommended path per user's "halt before Wave 9" directive.
- **Option B — Phase 8.6 narrow amendment cycle (~30 min single agent) + V8.2 verification (~30 min)** before Wave 9. Tighter cohort coherence; ~60 min wall.

Per user mandate: HALT at V8.1 for independent Codex hardening. Codex's V9 cycle will absorb the 2 PASS-3 narrow surgeries naturally as part of its own punch-list pass.

Re-draft thresholds: zero met. Tape/direct union, Backend IR ownership, yaml two-surface, numeric SOTA, B/C and C/E/H sequencing, generated-code budgets, carry ledgers, path!/select! macros, @error(recover), OpenFrame archaeology, GADT V1 surface, CHR V1 fold — all hold.

## §6 Phase 8 cycle history

| Phase | Outcome | Commits |
|---|---|---|
| 8.0 — Prune + HANDOFF rewrite | DONE | `94873cf0` |
| 8.1 — Restructure prompts + add lenses I/J/K | DONE | `bc31560c` |
| 8.2 — V8 simplification audit (4 parallel) | DONE | `624b5af2` / `597ac678` / `cd6c2b4c` / `25addd94` |
| 8.3 — V8 consolidation | DONE | `28987de4` |
| (Corpus audit — 4 parallel) | DONE | `56610d55` / `c9f35afb` / `f3ffe523` / `124b0068` / `b6082f94` |
| 8.3.1 — Corpus cleanup (Q1-Q8 user adjudications) | DONE | `2145577c` / `a74cdc52` |
| 8.4 — V8 simplification fold (4 parallel) | DONE | `4c69b848` / `23311ff8` / `831b2f90` / `1a75ea53` / `85187a74` / `bd213632` / `c72318cd` / `e5cb1e4b` |
| 8.5 — V8.1 verification rerun (4 parallel + consolidation) | DONE (this commit) | `277910df` / `fe36af42` / `7d8f03ea` / `0374d7ef` + this consolidation |

Total Phase 8 cycle: ~30 commits across 8 sub-phases. From V7.1 READY → V8 SIMPLIFY-AVAILABLE → V8.1 READY-WITH-NARROW-RESIDUE.

## §7 Cumulative architectural reductions

The Phase 8 cycle reduced architectural cardinality + delegated host-language work + folded V1 user adjudications:

| Reduction | Before | After | Mechanism |
|---|---|---|---|
| Backend trait method count | 5 (`lower` + 4 `emit_*`) | 2 (`lower` + `emit_artefacts`) | α1 fold; ArtefactSet bundles runtime/value/visitor/path |
| Type-system stack mechanisms | 7 (HM-equality + Algorithm-W + first-order unification + Pierce-Turner + DK13 + finite CSP + GADT-refinement) | 5 (Algorithm-W + Pierce-Turner + DK13 + finite CSP + GADT-refinement) | α2 fold; HM-equality and first-order unification are Algorithm-W |
| BIR alphabet variants | 22 | 19 + Return | α3 fold; 3 semantic pair-collapses (LayoutPush+LayoutPop, DispatchAlt+SpeculativeAlt, CallHost+HostChain) |
| Rewrite-budget categories | 4 | 3 | α5 fold; simplification-rewrites → codegen::verify |
| Generic-rule validation paths | 3 (annotation OR rejection OR structural-decreasing-arg) | 2 (annotation OR rejection) | α6 fold; structural-decreasing-arg routes to tranche D body |
| Diagnostic codes (cataloged) | 28+ numeric+alphabetic dual-namespace | 29 human-readable single-namespace | β1 retire |
| `@directive` count | 8 (was 6 V1 + 2 lock-deferred) | 6 V1 only | Lock 10 amendment (Phase 7.1) |
| Lock count | 14 | 14 (no Lock 15+) | All amendments via lock-text edits |

Plus Phase 8.3.1 V1 folds (GADT user-facing surface; CHR-improvement layer) + composition library deletion + V5.1 intermediate prune (4 files / 60K) + orchestrator restructure.

## §8 Voice + discipline locks summary

The V8.1 cohort preserves voice + discipline locks per `restart/README.md` §13 across all four targets. Calibrated, direct prose. Path:line citations on every concrete claim. No metalanguage. Per-X tables liberal where they serve.

LLM-pathology check (lenses F/G/H from V5+ spec): zero new pathology introduced by Phase 8 cycle. The simplification fold itself meta-aware retired LLM-trained-distribution artefacts (β1 numeric alias retirement explicitly cites "LLM-trained-distribution artefact"; γ delegations explicitly cite host-language facilities bbnf was reinventing).

Simplification check (lenses I/J/K from Phase 8.1 spec): all 41 V8 candidates resolved (CLOSED via fold, RETIRED via deletion, or ROUTED to tranche bodies with explicit receivers). Zero V1-folded items survive as V2-deferred.

## §9 Wave 9 readiness assessment

The trio (ARCHITECTURE + MASTER-PLAN + MIGRATION) carries Wave-9-ready detail across 10 per-tranche drafting input layers:

1. **Architectural foundations** — ARCH §1-§3 (gestalt + 14 locks reference + executable shape)
2. **Crate boundary** — ARCH §2 (24-crate workspace; bbnf-prefixed user-facing)
3. **Grammar IR + Backend IR** — ARCH §7.1 + §7.2 (post-fold 19-variant alphabet)
4. **Backend trait** — ARCH §7.5 (post-fold 2-method)
5. **Diagnostic catalogue** — ARCH §7.4 (post-fold 29-row human-readable canon)
6. **Type system** — ARCH §8 + §8.1 (post-fold 5-mechanism stack; GADT V1 surface; CHR V1)
7. **Optimization** — ARCH §10 + §10.1 (post-fold 3 rewrite categories)
8. **Runtime** — ARCH §11 (host-leverage delegations to syn/tower-lsp/dap-types/thiserror/miette/salsa)
9. **Lint manifest** — ARCH §13.1 + §13.2 (4 lint categories + cookbook page contract)
10. **Tranche plan** — MASTER-PLAN A→J + §24 carry ledger + §25 cookbook + §27 Phase 8.4 ledger

Every layer carries post-fold authoritative content. Wave 9 dispatches 10 parallel per-tranche full-spec agents (one per A-J; ~3,000-5,000 lines per tranche; inheritance per `restart/inheritance/INDEX.md`).

## §10 Closing posture

Phase 8 closed. The architecture is the leanest it has been across 8 hardening cycles (V1-V8.1). 30 V1 fold candidates absorbed (Phase 7); 41 V8 simplification candidates resolved (Phase 8); zero V1-folded items survive as V2 deferrals. The trio carries Wave-9-ready detail; PASS-1 / PASS-2 / PASS-3 syntheses align with the trio post-fold; the 5 prompts at `restart/prompts/` form a clean orchestrator structure.

**Per the user's "halt before Wave 9" directive: this is the halt point.** Independent Codex hardening (V9 cycle) dispatches against this V8.1-READY corpus per `restart/prompts/ORCHESTRATOR.md` cold-start protocol:

1. Codex reads `restart/HANDOFF.md` (orientation; current verdict V8.1)
2. Codex reads `restart/prompts/ORCHESTRATOR.md` (phase fan-out)
3. Codex reads this consolidation (`HARDENING-CONSOLIDATED-V8.1.md`)
4. Codex identifies "post-Phase-8.5; pre-Wave-9" phase
5. Codex dispatches fresh V9 hardening cycle per `restart/prompts/sub-orchestrators/HARDENING.md` with full lens set A-K (per `restart/prompts/audit-specs/HARDENING-LENS-SET.md` lens contract)

Codex's V9 will naturally absorb the 2 PASS-3 narrow residues (V8.1-A, V8.1-B) as part of its own punch-list pass; the residue is intentionally documented for V9 to find.

Hereupon: HALT. Codex V9 hardening dispatches at user direction.
