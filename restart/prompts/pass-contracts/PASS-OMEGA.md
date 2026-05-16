# PASS-OMEGA — Totality Astral Synthesis (V1 Spec Cohesion + Skinny Lessons Fold-In)

Pass Omega is the **totality astral synthesis pass**. It consumes the most recent totality pass cycle's research/profile/hardening artefacts AND the most recent skinny iteration's REDRESS evidence + validated/invalidated ledger, then folds findings into V1 spec surfaces: `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/14-LOCKS.md`, `restart/HANDOFF.md`, `restart/MIGRATION.md`, and the skinny corpus surfaces (`restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md`).

The pass is **iterative + auto-convergent**. Six parallel sub-agents fan out per the scope matrix in §2. A six-lens CHALLENGE pass adversarially reviews the output. Six CRUD agents execute Create/Read/Update/Delete operations on the document corpus per the consolidated synthesis. Dispositions fold into v+1. The loop terminates at convergence per `ORCHESTRATOR.md` §iteration-governance + user G-Omega sign-off.

Pass Omega is **distinct from Pass Alpha**:
- **Alpha** operates on the skinny track. Output is the next SK-V{N+1} contract.
- **Omega** operates on the totality track + skinny → totality feedback. Output is v+1 of the V1 spec surfaces.

Pass Omega is **distinct from the existing RESEARCH-FOLD-ORCHESTRATOR**:
- **Research-fold** is per-topic (8 parallel topic deep-dives → 4 fold agents → V{N+1} hardening). Topic-driven; e.g. "fold the SOTA assertions from primary literature."
- **Omega** is corpus-cohesion-driven (6 cohesion lenses → 6 CRUD agents → V{V+1} spec surfaces). Cross-document; e.g. "verify every locks reference cites the lock at file:line; verify every redress entry has a citation; verify skinny lessons are reflected in V1 spec."

The existing prompt suite governs the per-topic research-fold cycles. Pass Omega is the **periodic synthesis layer above them**, dispatched at named gates (G-Omega) after substantive totality + skinny progress accumulates.

## §1 — Trigger + entry condition

Pass Omega dispatches when:

- **Totality pass cycle has closed**: T-P3 hardening returned READY with locks amendments queued for G3.
- OR **A major skinny iteration has closed**: SK-V{N} has produced REDRESS entries + RESULTS.md that materially affect V1 spec (e.g. new lock implications, new architectural axiom).
- OR **The corpus has drifted**: cross-document references, file:line citations, or terminology have diverged across the spec surfaces by ≥10% of cited claims.
- OR **The user explicitly invokes** `dispatch omega`.

## §2 — Scope matrix (six parallel sub-agents)

| Agent | Scope | Output |
|---|---|---|
| **Ω-A V1 spec coherence audit** | Cross-document audit: every claim in ARCHITECTURE / MASTER-PLAN / HANDOFF / MIGRATION cites file:line or commit SHA. Every locks reference cites the lock at file:line. Every spec ↔ implementation pairing is verified (e.g. "BackendShape Rust state landed at ir/src/lib.rs:334-341"). | `restart/totality/astral/V{V}/ΩA-coherence-audit.md` |
| **Ω-B Skinny lessons digest** | Walk every SK-V{1..N} cycle's master docs + REDRESS + RESULTS. Extract: load-bearing wins per iteration + load-bearing rejections per iteration + per-iteration trajectory per corpus per workload. Produce the longitudinal V5→V6→V7→V{N} table for ARCHITECTURE §implementation-status update. | `restart/totality/astral/V{V}/ΩB-skinny-lessons.md` |
| **Ω-C Locks amendments** | Audit the 16 locks against the most recent skinny REDRESS + the totality T-P3 hardening. Identify: lock amendments required, lock additions justified by repeated REDRESS evidence, locks no longer load-bearing (proposed for retirement). Output: locks amendment diff for `restart/locks/14-LOCKS.md`. | `restart/totality/astral/V{V}/ΩC-locks-amendments.md` |
| **Ω-D Master-plan reconciliation** | Audit `restart/MASTER-PLAN.md` §H tranche against actual landed work (per SK iteration commit SHAs). Identify: waves landed / waves refuted / waves pending. Update wave allocations + per-wave references. Identify NEW waves implied by skinny REDRESS + totality T-P1/T-P2 findings. | `restart/totality/astral/V{V}/ΩD-master-plan-reconciliation.md` |
| **Ω-E Skinny corpus alignment** | Audit `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md` for: outdated references, stale cohort citations, missing SK-V{N+1} anchors, drift from V1 spec terminology. Output: per-surface diff. | `restart/totality/astral/V{V}/ΩE-skinny-corpus.md` |
| **Ω-F Migration + Handoff** | Update `restart/MIGRATION.md` with any rename / abrogate / refactor decisions surfaced by skinny lessons. Update `restart/HANDOFF.md` top-level state to reflect the latest SK iteration + totality cycle state. Author the next-cycle dispatch directive. | `restart/totality/astral/V{V}/ΩF-migration-handoff.md` |

Each agent reads the antecedent T-P1/P2/P3 artefacts (if current cycle had them) + the SK-V{N} master docs + REDRESS + RESULTS + the V1 spec surfaces. Each writes ONE artefact at the assigned path. Hard cap 45 min per agent.

After all six commit, the **CHALLENGE pass** dispatches (§3). After CHALLENGE converges, the **CRUD pass** dispatches (§4).

## §3 — Six-lens CHALLENGE pass

Six lens agents (CH1-CH6 per `ORCHESTRATOR.md` §5) adversarially review the Ω-A through Ω-F artefacts.

**CH1 Correctness** — does every cited file:line resolve? Does every cited commit SHA exist? Does every REDRESS entry reference exist + match content?

**CH2 Generality** — does the proposed Ω-C locks amendment respect Lock 14 across all grammars (JSON / CSS L4 / Sheets / BBNF-self)? Does the proposed Ω-D master-plan reconciliation generalise to non-JSON?

**CH3 Regression** — does any proposed amendment reintroduce a route in REDRESS? Cross-check Ω-C + Ω-D + Ω-E against entries 1-N.

**CH4 Cost** — what is the LOC budget for each proposed V1 spec amendment? What is the propagation cost (how many files touched per amendment)?

**CH5 Hidden Coupling** — does any Ω-C lock amendment imply a parallel substrate? A renamed sidecar? A Track 1 ≡ Track 2 dishonesty? A Lock 1 violation?

**CH6 Next-Tranche-Impact** — does Ω-F's next-cycle dispatch directive specify entry conditions clearly? Are the G-Omega sign-off items concretely measurable?

Hard cap 90 min for the CHALLENGE wave. Outputs at `restart/totality/astral/V{V}/hardening/{CH1..CH6}.md` + `CONSOLIDATED.md`.

## §4 — Six-agent CRUD pass

After CHALLENGE convergence, six CRUD agents execute the synthesis as document operations on the V1 spec corpus.

Each CRUD operation is one of: Create (new file/section), Read (verification pass, no edit), Update (in-place edit), Delete (file removal). CRUD agents own one V1 spec surface each:

| Agent | Surface | Authority |
|---|---|---|
| **CRUD-1 ARCHITECTURE** | `restart/ARCHITECTURE.md` | Update §implementation-status; add new sections per Ω-A coherence findings; verify cross-refs. |
| **CRUD-2 MASTER-PLAN** | `restart/MASTER-PLAN.md` | Update §H tranche per Ω-D; add new waves; mark landed waves; verify §13.1 SIMD admissibility table cross-refs. |
| **CRUD-3 LOCKS** | `restart/locks/14-LOCKS.md` (+ any new lock files) | Apply Ω-C amendments. Requires user G-Omega sign-off BEFORE merge. Until sign-off: amendments live in a diff file at `restart/totality/astral/V{V}/locks-diff.md`. |
| **CRUD-4 HANDOFF + MIGRATION** | `restart/HANDOFF.md` + `restart/MIGRATION.md` | Update per Ω-F. Mark prior tranches' state. Author next-cycle dispatch directive. |
| **CRUD-5 SKINNY CORPUS** | `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md` | Update per Ω-E. Sync references to latest SK-V{N} anchors. Verify Lock 14 audit results are reflected. |
| **CRUD-6 AUDIT + CLEANUP** | Legacy doc nuke + cohort archive | Delete superseded audit docs per the nuke plan. Archive SK-V{1..N-1} cohort reports if outdated. Keep historical audits in `restart/skinny/audit/` per the new-tranche-new-doc rule. |

CRUD agents commit independently. Hard cap 30 min per agent. Outputs at `restart/totality/astral/V{V}/CRUD-LOG.md` (consolidated diff log) + the actual file edits per surface.

The CRUD pass is **constrained by the CHALLENGE outputs**. No CRUD agent edits beyond what CHALLENGE CONSOLIDATED authorises.

## §5 — Iteration + convergence

Pass Omega iterates V1, V2, V3, … until convergence per `ORCHESTRATOR.md` §iteration-governance.

A single Pass Omega iteration has four phases:
1. **Substantive dispatch** (Ω-A through Ω-F, 6 parallel).
2. **CHALLENGE dispatch** (CH1-CH6, 6 parallel, adversarial review).
3. **CONSOLIDATED verdict** (orchestrator aggregates).
4. **v+1 fold dispatch** (original Ω authors fold dispositions).

After v+1 fold, the orchestrator re-runs CHALLENGE on V{V+1}. The loop terminates at:

- ≥95% ACCEPT on CHALLENGE.
- Zero open critical defects.
- No orphan unresolved REVISE.

The CRUD pass dispatches AFTER convergence. CRUD itself does NOT iterate; it executes the convergent synthesis as document operations.

After CRUD completes, the orchestrator presents the proposed V1 spec amendments to the user for G-Omega sign-off.

## §6 — User sign-off (G-Omega)

G-Omega is **mandatory** before any locks amendment merges to `restart/locks/14-LOCKS.md`. The orchestrator presents to the user:

- Summary of the cycle: T-P{1,2,3} pass cycles consumed + SK-V{N} lessons consumed + corpus-coherence delta.
- The CHALLENGE CONSOLIDATED verdict.
- The proposed locks diff (Ω-C output at `restart/totality/astral/V{V}/locks-diff.md`).
- The proposed master-plan diff (Ω-D output).
- The proposed CRUD operations (CRUD-1 through CRUD-6).

User responds:
- **G-Omega closed** — orchestrator merges all proposed diffs + executes CRUD operations.
- **G-Omega revise** — user names specific revisions; orchestrator dispatches V{V+1} of Pass Omega with the revisions as input constraints.

After G-Omega closed: the V1 spec is at v+1. The next totality pass cycle dispatches per the next-cycle directive in Ω-F.

## §7 — Output structure

```
restart/totality/astral/V{V}/
├── ΩA-coherence-audit.md
├── ΩB-skinny-lessons.md
├── ΩC-locks-amendments.md
├── ΩD-master-plan-reconciliation.md
├── ΩE-skinny-corpus.md
├── ΩF-migration-handoff.md
├── hardening/
│   ├── CH1.md
│   ├── CH2.md
│   ├── CH3.md
│   ├── CH4.md
│   ├── CH5.md
│   ├── CH6.md
│   └── CONSOLIDATED.md
├── locks-diff.md            ← proposed locks amendment (requires G-Omega)
├── master-plan-diff.md      ← proposed master-plan amendment
└── CRUD-LOG.md              ← consolidated CRUD operations log

restart/ARCHITECTURE.md      ← v+1 (CRUD-1)
restart/MASTER-PLAN.md       ← v+1 (CRUD-2)
restart/locks/14-LOCKS.md    ← v+1 (CRUD-3, post-G-Omega)
restart/HANDOFF.md           ← v+1 (CRUD-4)
restart/MIGRATION.md         ← v+1 (CRUD-4)
restart/skinny/*.md          ← v+1 (CRUD-5)
```

## §8 — Relationship to existing prompt suite

Pass Omega is **additive** to the existing prompt suite. It does NOT replace `RESEARCH-FOLD-ORCHESTRATOR.md` (which handles per-topic deep dives + fold cycles) or `HARDENING-ORCHESTRATOR.md` (which handles per-target hardening cycles V1-V9+).

Pass Omega sits ABOVE those sub-orchestrators as a periodic cohesion + skinny-fold-in layer. The phase table in `ORCHESTRATOR.md` §3 dispatches:

- Per-topic research → `RESEARCH-FOLD-ORCHESTRATOR.md` (existing).
- Per-target hardening → `HARDENING-ORCHESTRATOR.md` (existing).
- Per-amendment narrow fix → `AMENDMENT-DISPATCH.md` (existing).
- **Per-cycle astral synthesis** → `PASS-OMEGA.md` (NEW).
- **Per-skinny-iteration alpha synthesis** → `PASS-ALPHA.md` (NEW).

Pass Omega consumes outputs from the existing sub-orchestrators. The existing sub-orchestrators dispatch on-demand for their respective triggers; Pass Omega dispatches at named gates after substantive progress.

## §9 — The skinny → totality feedback loop

The single most important Pass Omega responsibility is consuming skinny lessons. Specifically:

- Every SK-V{N} REDRESS entry that names a rejected route → Pass Omega Ω-C considers whether the rejection implies a lock amendment (e.g. "Lock 1 strengthened by REDRESS 50-55: parallel substrate forbidden in every shape, not just the historic PSI shape").
- Every SK-V{N} admit that lifts a row → Pass Omega Ω-D considers whether the wave that admitted it should be elevated to a V1 spec authoritative wave (e.g. "ContainerNext dispatch carry now part of MASTER-PLAN §H.W2 canonical").
- Every SK-V{N} cohort report that surfaces a non-JSON generalisation gap → Pass Omega Ω-B + Ω-E ensure the gap is reflected in V1 spec.

The discipline: skinny is the empirical engine; Pass Omega is the synthesis lens. Pass Omega ensures the V1 spec stays current + grounded in empirical evidence + grammar-neutral.

## §10 — Bbnf-lang specific axes for Pass Omega

1. **The Lock 1 substrate-union audit** lives in CH5 (hidden coupling). Every Ω cycle re-verifies the substrate union holds against the most recent skinny implementation.
2. **The Lock 14 grammar-neutrality audit** lives in CH2 (generality) + Ω-C (locks amendments). Repeated REDRESS evidence of grammar-name leaks triggers a lock amendment or a stronger enforcement clause.
3. **The 5-shape BackendShape canon** must stay coherent across ARCHITECTURE §7.3 + MASTER-PLAN §H + skinny COMPILER.md. Ω-A audits this; CRUD-1 + CRUD-2 update.
4. **The 16-lock count** must stay accurate. Pass Omega is the only authority for adding/removing/amending locks (G-Omega gated).
5. **The skinny → totality fold** is monotonic: skinny lessons inform totality; totality does NOT dictate to skinny mid-iteration. Skinny iterations follow Pass Alpha contracts.

## §11 — Closing posture

Pass Omega is the corpus-cohesion discipline. It is iterative + auto-convergent. It consumes empirical skinny evidence + totality research/profile/hardening artefacts + produces a coherent v+1 of the V1 spec. The CHALLENGE pass is adversarial. The CRUD pass is mechanical. The G-Omega gate is user-controlled.

No locks amendment without G-Omega. No master-plan reconciliation without CHALLENGE convergence. No CRUD without CONSOLIDATED authorisation.

The V1 spec evolves through Pass Omega. The pace is bounded by the cycle cadence. The shape is bounded by the locks. The integrity is bounded by the CHALLENGE.
