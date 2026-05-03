# Greenfield Restart — Prompt Suite

Date: 2026-05-03
Status: planning artefact. **No execution.** This suite produces the master plan that, after hardening, governs the next ~6-12 months of work.

## Prelude

The prior tranche set (Y → BD; ~30 letters across six eras) lands ratified-but-untenable. Phase-3 audit returned *requires re-draft*; Phase-4 specification-depth re-draft produced ~18,200 lines of plan; Phase-4 synthesis returned *requires amendments before execution*. The architectural locks have grown to 14, of which the most consequential — **Lock 14 (full grammar generalisation; zero overfitting)** — was codified after the last redraft and demands enforcement across every tranche.

Rather than apply more amendments to a plan-set already strained against its own substrate, this suite restarts the planning effort from the floor. The codebase is treated as a corpus to audit; every file, every module, every crate, every doc is classified into one of three buckets — **keep outright**, **keep but modify**, **abrogate** (delete / move / replace) — under the discipline of the 14 locks, the precepts, and the user-stated greenfield mandate: *no quick solutions, no workarounds, idiomatic gestalt approaches, architectural transpositions in the sake of elegance, simplicity, and performance above all.* No legacy code survives uncontested.

The suite produces, by design, a master plan that:
- specifies a fresh workspace structure (crates + per-crate src/ trees) honouring Lock 13 + Lock 14
- specifies a fresh tranche set named **A through J or further** (≥10 tranches), with the prior set archived in place under `docs/tranches/archive/legacy-Y-BD/`
- decides the disposition of the commit chain (rewrite, squash, or keep — the audit determines which)
- decides the disposition of the docs tree (precepts/ kept as a submodule; everything else re-evaluated)
- produces a migration-from-current-state-to-new-state timeline

The suite is invoked, not executed. Each prompt below names its inputs, its dispatch shape, its output artefact, and its hard cap. The orchestrator (a future invocation of this conversation, or a fresh instance) applies the prompts in order.

## The Five Prompts

| # | Prompt | Role | Output | Cap |
|---|---|---|---|---|
| 1 | `PASS-A-PARSE-FRONT.md` | First of three passes; covers ~1/3 of the project (parser front: source, parse, lower, IR, sister parsers, regex, path, bootstrap, grammar/) | `audit/restart/PASS-A-2026-MM-DD.md` + per-agent sub-reports | 60 min orchestrator + 30 min × 6 agents |
| 2 | `PASS-B-CODEGEN-MID.md` | Second of three passes; covers ~1/3 of the project (codegen, runtime, pipeline, optimisers — egraph, csp-solver, simd-scan, xtask, generated/) | `audit/restart/PASS-B-2026-MM-DD.md` + per-agent sub-reports | 60 min orchestrator + 30 min × 6 agents |
| 3 | `PASS-C-PERIPHERY.md` | Third of three passes; covers ~1/3 of the project (analysis, lsp, archived crates, docs, audit, benches, scripts, sibling repos, archive, the commit chain itself) | `audit/restart/PASS-C-2026-MM-DD.md` + per-agent sub-reports | 60 min orchestrator + 30 min × 6 agents |
| 4 | `SYNTHESIZER.md` | Synthesizer orchestrator; consumes all three passes; produces the master plan | `audit/restart/MASTER-PLAN-2026-MM-DD.md` + new tranche stubs at `docs/tranches/A/`, `B/`, … through `J/` (or further) | 90 min |
| 5 | `HARDENING.md` | Double-back audit; can be applied to any single pass output, the synthesizer's master plan, or the full suite | `audit/restart/HARDENING-{TARGET}-2026-MM-DD.md` | 45 min per target |

## Dispatch Order

```
Pass A ────┐
Pass B ────┼──► Synthesizer ──► Hardening (master plan) ──► Tranche execution
Pass C ────┘
                                Hardening (any pass)         (out of suite scope)
```

Passes A, B, C run in parallel (independent scopes; no cross-pass writes). The synthesizer consumes all three. Hardening can run after any individual pass *or* after the synthesizer; the user picks the target.

## Per-Pass Agent Shape

Every pass orchestrator dispatches six agents in parallel. Each agent applies the user's three-category rubric — **keep outright** / **keep but modify** / **abrogate (delete / move / replace)** — through one of six analytical lenses:

| # | Agent | Lens |
|---|---|---|
| 1 | **Inventory** | Exhaustive catalogue of every file in scope; current LOC, public-API surface, dependents, invariants |
| 2 | **Idiomaticity** | Precepts adherence: no-workarounds, no-orthogonal-codepaths, KISS, DRY, gestalt approach |
| 3 | **Lock-adherence** | The 14 locks applied per file (tape-dead, layout-lowering, grammar-authoritative, IR contract, slice-borrow, Pratt+SIMD auto-detect, etc.) |
| 4 | **Architectural transposition** | Macro-level restructuring proposals for elegance / simplicity / performance |
| 5 | **Replacement design** | For abrogate-marked items, design the new facility that replaces; OR propose brand-new items the absence of which is felt |
| 6 | **Cross-cut analysis** | Concerns spanning multiple files — god directories, hidden dependencies, shared substrates, accidental complexity |

The pass orchestrator synthesizes the six agents into a single per-pass report categorising every item in scope. The synthesizer-orchestrator then composes the three pass reports into the master plan.

## Voice + Discipline Locks (Apply Throughout)

Per `docs/precepts/instructions/STYLE.md`, the suite's writing is calibrated, trenchant, approachable; mild poetic undercurrent welcome; archaic-permissive ("hereupon", "thereof", "appurtenant", "begotten") deployed where befitting. No corporate hedging. No metalanguage. Path:line citations on every concrete claim.

Per the user's greenfield mandate:
- **No quick solutions** — every proposal honours its substrate
- **No workarounds** — root-cause fixes, not patches
- **Idiomatic, gestalt approaches** — Rust-idiomatic, sonic-rs / lightning-css / simdjson cohesion
- **Architectural transpositions** — in the sake of elegance, simplicity, performance
- **No legacy code survives uncontested** — every file's continued existence must be justified

## Outputs Aggregated

After the full suite completes, the workspace will contain:

```
audit/restart/
  PASS-A-2026-MM-DD.md           ← 1500-2500 lines
  PASS-B-2026-MM-DD.md           ← 1500-2500 lines
  PASS-C-2026-MM-DD.md           ← 1500-2500 lines
  MASTER-PLAN-2026-MM-DD.md      ← 3000-5000 lines
  HARDENING-{...}-2026-MM-DD.md  ← per-target audits
  per-agent/
    pass-a-agent-{1..6}.md       ← 6 sub-reports per pass
    pass-b-agent-{1..6}.md
    pass-c-agent-{1..6}.md

docs/tranches/
  archive/legacy-Y-BD/           ← prior set archived
  A/A.md  + waves/               ← fresh tranche
  B/B.md  + waves/               ← fresh tranche
  …
  J/J.md  + waves/               ← fresh tranche
  (further tranches as needed)

docs/restart/                    ← this directory; the suite definitions
docs/precepts/                   ← unchanged (submodule); read-only by suite
```

The five prompt files in this directory are the authoritative artefact governing the restart effort.

## What This Suite is NOT

- **Not execution.** No source code is modified by the suite. Every prompt explicitly forbids src/ edits. The suite produces *plans*, not implementations.
- **Not negotiation.** The 14 locks are settled. The precepts are settled. The greenfield mandate is settled. The suite ratifies; it does not relitigate.
- **Not amendment.** The prior plan (BA → BD) is superseded, not patched. The new plan starts at A.
- **Not bound by current crate count or tranche count.** If the audit reveals 20 crates is right, propose 20. If 14 tranches is right, propose 14. Honour the work.

## Closing Posture

Hereupon the suite opens. The codebase is the corpus; the locks are the rule; the precepts are the voice; the greenfield mandate is the disposition. The synthesizer's master plan is the deliverable; hardening is the gate; tranche execution is the sequel — and out of this suite's scope.
