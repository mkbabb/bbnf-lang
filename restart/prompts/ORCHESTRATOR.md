# ORCHESTRATOR — bbnf-lang Greenfield Restart

This is the **single main orchestrator prompt** for the bbnf-lang greenfield restart. All phase dispatch flows through this document. The orchestrator-agent (you) reads this prompt end-to-end, identifies the current phase from git state + corpus, then fans out to one of three encapsulated sub-orchestrators per the phase-type table at §3.

The pipeline is composable: each sub-orchestrator runs its own waves of sub-agents independently. This permits any phase to re-execute many times over without contract drift.

## §1 — Required reading (the orchestrator-agent reads end-to-end before any dispatch)

1. `restart/HANDOFF.md` — current-state orientation; latest verdict; next move.
2. `restart/README.md` — gestalt anchor; 14 locks; SOTA synthesis.
3. `restart/locks/14-LOCKS.md` — settled architectural commitments (post-Phase-7.1 amendments).
4. `restart/audit/hardening/HARDENING-CONSOLIDATED-V7.1.md` — terminal verdict baseline.
5. `restart/research/V1-FOLD-CANDIDATES.md` — Phase 7 contract; 30-item synthesis.
6. `restart/prompts/HARDENING.md` — per-target audit specification (the contract each hardening agent reads).
7. `restart/prompts/HARDENING-ORCHESTRATOR.md` — sub-orchestrator for hardening cycles.
8. `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` — sub-orchestrator for research deep-dives + fold cycles.
9. `restart/prompts/AMENDMENT-DISPATCH.md` — sub-orchestrator for verify-then-patch amendment cycles.
10. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` — voice + discipline.

## §2 — Phase identification protocol

The orchestrator-agent identifies the current phase by:

1. Reading `git log --oneline -10` to find the most recent phase commit.
2. Cross-referencing the latest commit against `restart/HANDOFF.md` §3 (current state).
3. If phase identification is ambiguous, the orchestrator-agent reads the most recent `HARDENING-CONSOLIDATED-V*.md` to find the active verdict + residue.
4. If the user has explicitly named a phase, that phase wins.

## §3 — Phase-type table (the fan-out)

Each phase type maps to one sub-orchestrator. The sub-orchestrator owns its own wave / agent dispatch internally.

| Phase type | Trigger | Sub-orchestrator | Dispatched cycles |
|---|---|---|---|
| **Hardening** — verify the corpus against a lens set | Latest commit is fold/amendment; verify-then-rerun mandate | `restart/prompts/HARDENING-ORCHESTRATOR.md` | 4 parallel hardener agents per `HARDENING.md` lens spec → 1 consolidation. `HARDENING-{TARGET}-V{N}.md` outputs. |
| **Research-fold** — ground SOTA assertions in primary literature, then absorb | Architectural surfaces assert SOTA without grounding; user explicit | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | 8 parallel topic deep-dives (Phase 1) → 4 parallel fold agents (Phase 2) → escalation summary if structural (Phase 2.5) → V{N+1} hardening (Phase 3) → V{N+1} consolidation (Phase 4). |
| **Amendment-dispatch** — verify-then-patch a punch list | Hardening returns AMENDMENT-REQUIRED with ≤20 narrow items | `restart/prompts/AMENDMENT-DISPATCH.md` | 1-4 parallel narrow-amendment agents → V{N}.1 verification rerun. |
| **Surface fold** — absorb a settled architectural decision into the per-pass surfaces | User adjudicates V1-fold candidates; lock amendments demand cascade | `restart/prompts/HARDENING-ORCHESTRATOR.md` then `AMENDMENT-DISPATCH.md` | 1 single SYNTHESIS agent (locks + ARCH amendments) → 4 parallel surface-fold agents (PASS-1 / PASS-2 / PASS-3 / SYNTHESIS trio) → V{N+1} hardening verification. |
| **Per-tranche full-spec drafting** — Wave 9+; out of orchestrator scope | All audits + folds returned READY | (separate spec-drafting orchestrator; not yet authored) | 10 parallel tranche-spec agents (one per tranche A-J; ~3,000-5,000 lines each). |

## §4 — Phase 8 dispatch table (current — updated after each phase commits)

| Phase | Status | Sub-orchestrator | Owner |
|---|---|---|---|
| 8.0 — Prune + HANDOFF rewrite | DONE (commit `94873cf0`) | (direct edit; no sub-orchestrator) | orchestrator-agent |
| 8.1 — Restructure prompts + add lenses I/J/K | DONE (commit `bc31560c`) | (direct edit; no sub-orchestrator) | orchestrator-agent |
| 8.2 — V8 simplification audit (4 parallel) | DONE (`624b5af2` / `597ac678` / `cd6c2b4c` / `25addd94`) | `HARDENING-ORCHESTRATOR.md` (per Phase-3 dispatch pattern) | dispatched hardener cohort |
| 8.3 — V8 consolidation | DONE (`28987de4`) | `HARDENING-ORCHESTRATOR.md` (per Phase-6 consolidation pattern) | orchestrator-agent |
| 8.4 — Simplification fold | DONE (`4c69b848` / `23311ff8` / `831b2f90` / `1a75ea53` / `85187a74` / `bd213632` / `c72318cd` / `e5cb1e4b`) | `AMENDMENT-DISPATCH.md` | dispatched fold cohort |
| 8.5 — V8.1 verification rerun | DONE (`277910df` / `fe36af42` / `7d8f03ea` / `0374d7ef` / `af3d1a73`) | `HARDENING-ORCHESTRATOR.md` | dispatched verification cohort + orchestrator-agent |

After Phase 8.5 returned READY-WITH-NARROW-RESIDUE, the user-directed V9 hardening cycle became the pre-Wave-9 gate. V9.1 now returns READY after narrow verification amendments; Wave 9 per-tranche full-spec drafting is the active next phase.

## §5 — Hardening-cycle naming canon

Across the cycle V1 through V8+, the hardening cohort's verdicts are named:

| Cycle | Predecessor | Trigger | Outputs |
|---|---|---|---|
| V1 | (initial) | First-pass after PASS dispatch + SYNTHESIS trio | `HARDENING-CONSOLIDATED.md` (no version suffix) |
| V2 | V1 | Single serial author (insufficient pressure; adversarially weak) | `HARDENING-CONSOLIDATED-V2.md` |
| V3 | V2 | 4-parallel independent (surfaces what V2 missed) | `HARDENING-CONSOLIDATED-V3.md` |
| V4 | V3 | Post-narrow-amendment verification | `HARDENING-CONSOLIDATED-V4.md` |
| V5 | V4 | Carry-aware metahardening (5 carry-aware lenses A-E + 3 LLM-pathology lenses F-H) | `HARDENING-CONSOLIDATED-V5.md` + `V5.1.md` (post-narrow-amend) |
| V6 | V5 | Research-fold verification (Phase 5+ pipeline) | `HARDENING-CONSOLIDATED-V6.md` |
| V7 | V6 | Phase 7 fold verification (V1-FOLD-CANDIDATES absorption) | `HARDENING-CONSOLIDATED-V7.md` + `V7.1.md` |
| **V8** | **V7.1** | **Simplification audit (lenses I/J/K + extant A-H)** | `HARDENING-CONSOLIDATED-V8.md` + `V8.1.md` (if needed) |
| **V9** | **V8.1** | **Independent Codex hardening before Wave 9 (full lens set A-K)** | `HARDENING-CONSOLIDATED-V9.md` + `V9.1.md` (if needed) |

Future cycles (V10+) follow the same pattern. Each cycle's lens set is documented in `HARDENING.md`.

## §6 — Lens registry (full set; the audit specification)

`HARDENING.md` carries the per-target lens contract. The full lens set as of Phase 8.1:

**Carry-aware lenses (A-E)** — surface what punch-list cycles structurally missed:
- **A** — Inter-document narrative coherence
- **B** — Vocabulary drift
- **C** — Worked-example scarcity
- **D** — Coverage gaps
- **E** — Architectural axiom cumulative consistency

**LLM-pathology lenses (F-H)** — guard against authorship pathologies:
- **F** — LLM bias (hedging, reference-stuffing, pseudo-precision, unfalsifiable claims, ornament substituted for commitment, buzzword reliance, confident generality)
- **G** — Overfitting (SOTA-only justification, pattern-lift, missing alternative-considered, mimetic convergence, training-corpus inheritance)
- **H** — Hallucination + provenance gaps (non-existent citations, wrong-line refs, unverified externals, derived claims from unstated premises)

**Simplification lenses (I-K)** — surface complexity exceeding the meta-grammar mandate:
- **I** — Contrivance / over-engineering (speculative generality, cardinality bloat, premature optimization, cardinality redundancy)
- **J** — Host-language leverage (places where Rust / TS / WASM provide the facility cleanly at a higher layer)
- **K** — Meta-grammar discipline (architectural complexity exceeding bbnf's role as parser-generator for extant target languages)

Hardening cycles V1-V4 ran lenses A-E + 9-lane standard audit only. V5+ added F-H. V8+ adds I-K.

## §7 — Sub-orchestrator dispatch protocol

When the orchestrator-agent dispatches a sub-orchestrator, the dispatch carries:

1. **Phase identifier** (e.g., "Phase 8.2 — V8 simplification audit").
2. **Reference to this orchestrator** (`restart/prompts/ORCHESTRATOR.md` §3 row N).
3. **Reference to the sub-orchestrator** (`HARDENING-ORCHESTRATOR.md` / `RESEARCH-FOLD-ORCHESTRATOR.md` / `AMENDMENT-DISPATCH.md`).
4. **Lens set or item list** (e.g., "lenses I/J/K" for V8 simplification audit).
5. **Output path** (e.g., `restart/audit/hardening/HARDENING-{TARGET}-V8.md`).
6. **Hard cap** (per the sub-orchestrator's per-phase wall budget).
7. **Cross-tranche scope boundary** (the sub-orchestrator enforces).

The sub-orchestrator owns wave/agent dispatch internally; the main orchestrator only fans out the phase + collects the consolidation.

## §8 — Cross-tranche scope boundary (the orchestrator-agent's own scope)

The orchestrator-agent touches ONLY:
- Sub-orchestrator dispatch invocations.
- `restart/HANDOFF.md` (after each phase completes; document the new state).
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V{N}.md` (consolidation phase outputs).

The orchestrator-agent does NOT touch:
- `restart/prompts/` (locked; sub-orchestrator content is owned by sub-orchestrator authoring).
- `restart/README.md`, `restart/locks/`, `restart/inheritance/`, `restart/corpora/` (governance surfaces).
- `restart/audit/pass-*/`, `restart/research/` (each sub-orchestrator owns its agent outputs).
- The per-target hardening reports (each hardener owns).
- `crates/`, `docs/`, `restart-archive-2026-05-04/`.

## §9 — Hardening cycle hard cap

Per-phase wall budgets (from per-sub-orchestrator):

| Phase | Wall budget (parallel) |
|---|---|
| Phase 0 sub-prep | 30 min |
| Hardening cycle (4 parallel + consolidation) | ~75-100 min |
| Research-fold cycle (8 parallel research + 4 parallel fold + 2 consolidation) | ~6-9 hours |
| Amendment-dispatch cycle (≤4 parallel + verification) | ~90-120 min |
| Per-tranche full-spec (Wave 9+; 10 parallel) | ~5-7 hours per tranche |

A single hardening cycle is ~2 hours. The corpus has been hardened 7 times; the architecture supports many more. Each cycle is a discrete phase commit; no cycle blocks future cycles.

## §10 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive. No metalanguage. Path:line citations on every concrete claim. Per-X tables for "all targets" / "all lenses" claims. No quick solutions. No legacy code uncontested. Lock 14 binds.

## §11 — Closing posture

The orchestrator is composable and re-runnable. Each phase commits autonomously; the sub-orchestrators encapsulate their own waves; the lens registry grows monotonically (A-E → F-H → I-K → future). After any READY verdict, per-tranche full-spec drafting unblocks; before any per-tranche dispatch, this orchestrator can re-execute any prior phase to harden further.

Hereupon the orchestrator-agent identifies the current phase + dispatches the appropriate sub-orchestrator.
