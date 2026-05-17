# PASS-3-SYNTHESIS — T-P3 Totality Synthesis (P1 + P2 Distilled Into V1 Spec Amendments)

T-P3 is the **totality-track synthesis pass**. It consumes the converged
T-P1 evidence base (`restart/audit/totality/p1/`) and the converged T-P2
research dossiers (`restart/audit/totality/p2/`) and distils them into
**proposed V1 spec amendments** — surface deltas for `ARCHITECTURE.md`,
wave reconciliations for `MASTER-PLAN.md`, a single crystallised
LOCKS v+1 diff, the skinny→totality fold, the grammar-generalisation
story, and the MIGRATION + HANDOFF + next-cycle dispatch directive.

The pass is **iterative + auto-convergent**. Six parallel sub-agents 3A–3F
fan out per the scope matrix in §2; each writes one synthesis artefact. A
six-lens CHALLENGE wave (§3) adversarially reviews the artefacts;
dispositions fold into v+1; the loop terminates at the convergence
criterion in §4. T-P3's converged output is consumed by **Pass Omega's
CRUD wave** (`pass-contracts/PASS-OMEGA.md` §4), which executes the
amendments as document operations on the V1 spec corpus, post-G-Omega. The
pass is self-contained: an agentic system handed only this prompt and
`ORCHESTRATOR.md` runs it end-to-end.

T-P3 **proposes**; it does not write the V1 spec surfaces directly.
`ARCHITECTURE.md`, `MASTER-PLAN.md`, and `LOCKS.md` are governance
surfaces touched only by Pass Omega CRUD, post-G-Omega. T-P3's artefacts
are proposed-diff documents; the actual edits land in the astral pass.

## §1 — Trigger + entry condition

T-P3 dispatches when **T-P2 has converged** — CHALLENGE returned ≥95%
ACCEPT for two consecutive cycles or the user pinned G2 — and
`restart/HANDOFF.md` declares ready-for-T-P3.

**Entry artefacts** the orchestrator confirms present: the converged T-P1
inventories + the T-P1 CONSOLIDATED verdict; the converged T-P2 dossiers +
the T-P2 CONSOLIDATED verdict; the V1 spec surfaces `restart/ARCHITECTURE.md`,
`restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`, `restart/HANDOFF.md`,
`restart/MIGRATION.md`; `skinny/REDRESS.md` + `skinny/RESULTS.md`; the
skinny corpus surfaces. If an entry artefact is absent the orchestrator
fails the dispatch loudly.

## §2 — Scope matrix (six parallel sub-agents)

Each row is one sub-agent. The agent reads the full T-P1 + T-P2 evidence
base + the V1 spec surface it synthesises against, then writes ONE
synthesis artefact at the assigned path. 3C is the LOCKS-crystallisation
agent and reads every 1E + 2X LOCKS-AMENDMENTS-CANDIDATE table; the others
own disjoint surfaces. All six run in parallel. Hard cap 45 min per agent.

| Agent | Scope | Output |
|---|---|---|
| **3A — ARCHITECTURE.md surface synthesis** | Distil T-P1's substrate/codegen/runtime divergences + T-P2's grounded techniques into a proposed-delta document for `restart/ARCHITECTURE.md` — section by section. Every proposed delta cites the T-P1 divergence-id or T-P2 grounding it answers. Carry refuted-technique consequences into the surface. | `restart/audit/totality/p3/3A-architecture-synthesis.md` |
| **3B — MASTER-PLAN.md wave reconciliation** | Audit `restart/MASTER-PLAN.md` §5 tranche set + §H waves against T-P1's implemented-vs-unimplemented census + the skinny REDRESS ledger. Classify every wave: **landed / refuted / pending / new**. Propose wave allocations, per-wave references, and any NEW wave implied by T-P1/T-P2 findings. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md` |
| **3C — LOCKS crystallisation (G3-gated)** | Consolidate every T-P1 1E + T-P2 2X LOCKS-AMENDMENTS-CANDIDATE into ONE v+1 LOCKS diff. Per candidate, one disposition: **ACCEPT / REJECT / MODIFY / DEFER** — with the proposing agent, the affected `LOCKS.md` section, the supporting path:line evidence, and the rationale. Silent drops forbidden. This artefact is the G3 gate object. | `restart/audit/totality/p3/3C-locks-crystallisation.md` + `restart/audit/totality/p3/3C-locks-v+1-diff.md` |
| **3D — Skinny→totality fold synthesis** | Distil T-P1's skinny-lessons digest (1D) into the durable totality fold: which SK-V{N} wins become V1-spec-authoritative; which SK-V{N} rejections become locks-strengthening evidence (cross-ref 3C); which non-JSON generalisation gaps the totality spec must absorb (cross-ref 3E). Monotonic — skinny informs totality, never the reverse. | `restart/audit/totality/p3/3D-skinny-fold.md` |
| **3E — Grammar-generalisation synthesis** | Distil T-P2's grammar-neutrality research (2C) into the non-JSON generality story: the per-grammar `BackendShape` matrix, the primitive-vocabulary transfer to CSS L4 / Sheets / BBNF-self, the future-grammar onboarding test. Surface every Lock 14 hardening clause T-P1/T-P2 evidence warrants (cross-ref 3C). | `restart/audit/totality/p3/3E-grammar-generalisation.md` |
| **3F — MIGRATION + HANDOFF + next-cycle dispatch** | Synthesise the rename/abrogate/refactor decisions surfaced by T-P1/T-P2 into a proposed `restart/MIGRATION.md` delta; synthesise the proposed `restart/HANDOFF.md` top-level state delta; author the next-cycle dispatch directive (entry conditions for the cycle Pass Omega's CRUD wave hands forward). | `restart/audit/totality/p3/3F-migration-handoff.md` |

### Output-schema frontmatter (every 3X artefact emits this block)

```yaml
---
agent: 3X
pass: T-P3-synthesis
cycle: V{N}
generated_at: <ISO-8601>
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: <ARCHITECTURE.md | MASTER-PLAN.md | LOCKS.md | MIGRATION.md | HANDOFF.md | n/a>
proposed_deltas_count: <int>
delta_summary:
  carried_from_prior_cycle: [<delta-id>, ...]
  removed: [<delta-id>, ...]
  answered: [<delta-id>, ...]
  newly_added: [<delta-id>, ...]
prior_cycle_dispositions_folded:
  accepted: [<finding-id>, ...]
  rejected: [<finding-id>, ...]
  revised: [<finding-id>, ...]
---
```

Body sections, every artefact: **Executive Summary** (≤200 words); **V{N}
Delta Summary** (carried / removed / answered / newly-added); **Proposed
Delta Table** (proposed delta | source T-P1/T-P2 finding-id cited |
affected V1-surface section | rationale); **Consequences** (positive /
cost / propagation — how many surfaces a delta touches); **Open
Questions** tagged to a CHALLENGE lens. 3C additionally emits the
disposition matrix (every candidate with one of ACCEPT/REJECT/MODIFY/DEFER)
and the line-level `3C-locks-v+1-diff.md`. Every claim cites a T-P1
inventory, a T-P2 dossier, or a V1 spec surface at path:line; an uncited
delta is a CH1 REJECT.

## §3 — Six-lens CHALLENGE pass (CH1–CH6)

Every cycle closes with the six-lens CHALLENGE wave per `ORCHESTRATOR.md`
§3W. One lens, one agent; six agents; each writes
`restart/audit/totality/p3/hardening/V{N}/CH{n}.md`. One aggregator writes
`.../hardening/HARDENING-T-P3-V{N}-CONSOLIDATED.md` carrying the six
dispositions + the cycle verdict. Dispositions are **ACCEPT / REJECT /
REVISE**. Cycle V1 expects ≥30% REVISE; an all-ACCEPT wave is paper-close.

What each lens scans inside T-P3's output:

**CH1 CORRECTNESS** — every proposed delta cites a real T-P1 finding-id or
T-P2 grounding; every cited V1-surface section resolves at path:line; 3C's
disposition matrix references real amendment candidates; the
`3C-locks-v+1-diff.md` applies cleanly to the current `LOCKS.md`.

**CH2 GENERALITY** — Lock 14 holds: 3A's surface deltas and 3B's wave
reconciliation generalise to non-JSON; 3E's grammar-generalisation story
is concrete for CSS L4 / Sheets / BBNF-self; 3C accepts no amendment that
narrows a lock to JSON. The future-grammar onboarding test survives.

**CH3 REGRESSION** — no proposed delta re-opens a route in
`skinny/REDRESS.md`; 3B does not propose reviving a refuted wave; 3D's
skinny fold does not promote a rejected route; 3C does not weaken a lock
that REDRESS evidence strengthened.

**CH4 COST** — every delta states a LOC budget, a propagation cost (how
many surfaces it touches), a risk class, and a wave alignment; 3B's NEW
waves carry a same-wave consumer; 3C dispositions are realistic.

**CH5 HIDDEN COUPLING** — no proposed delta implies a parallel substrate,
a sidecar producer, a renamed-scanner Lock 1 violation, or a Track 1 ≡
Track 2 dishonesty; the substrate union holds across every 3A surface
delta; 3C's accepted amendments do not introduce a coupling.

**CH6 ANTI-PAPER-CLOSE** — no synthesis artefact claims a delta
"validated" without the T-P1/T-P2 evidence chain; no delta is deferred to
"a future cycle" without a named receiver + blocker + receiving gate; 3C
DEFER dispositions name the re-entry trigger; 3F's next-cycle directive
specifies concrete, measurable entry conditions. No engineered-defer.

The lens registry is monotonically extensible (CH7+); CH1–CH6 are never
renumbered. A CHALLENGE agent auditing artefact prose may compose the A-K
lens set (`audit-specs/HARDENING-LENS-SET.md`) by reference.

## §4 — Iteration + auto-convergence

T-P3 executes cycles V1, V2, … per `ORCHESTRATOR.md` §3Z; the cycle
counter is per-pass and independent.

Per cycle: **(1)** the six 3X agents fan out and write their artefacts;
**(2)** the pass output commits before CHALLENGE; **(3)** the six CH
agents fan out; **(4)** the aggregator writes the CONSOLIDATED verdict;
**(5)** dispositions fold into V{N+1} — each 3X author addresses every
REJECT with a corrected delta and every REVISE with new evidence or a
revised disposition, citing the source; the V{N} Delta Summary block is
regenerated. Hardening without folding is paper-hardening; the pass does
not advance.

**Convergence criterion** (advances to G3): CHALLENGE returns **≥95%
ACCEPT for two consecutive cycles**, with zero open critical defects and
no orphan unresolved REVISE; OR the user pins the cycle as final at G3.

**Hard ceiling V ≤ 5**; a V5 non-convergence escalates to the user with a
`BLOCKED` verdict naming the unresolved REVISE set.

## §5 — Output structure

```
restart/audit/totality/p3/
├── 3A-architecture-synthesis.md
├── 3B-master-plan-reconciliation.md
├── 3C-locks-crystallisation.md         ← disposition matrix
├── 3C-locks-v+1-diff.md                ← line-level LOCKS diff (G3 gate object)
├── 3D-skinny-fold.md
├── 3E-grammar-generalisation.md
├── 3F-migration-handoff.md
└── hardening/
    ├── V{N}/
    │   ├── CH1.md   CH2.md   CH3.md
    │   ├── CH4.md   CH5.md   CH6.md
    └── HARDENING-T-P3-V{N}-CONSOLIDATED.md
```

Each cycle overwrites the 3X artefacts in place; git history preserves
V1, V2, … . The `hardening/V{N}/` directory is per-cycle.

## §6 — User sign-off gate (G3)

Per `ORCHESTRATOR.md` §6, T-P3 convergence reaches **G3** — **mandatory**.
The orchestrator does not advance past G3 without explicit user
confirmation. The orchestrator presents at G3: the cycle's CONSOLIDATED
verdict; 3A's proposed `ARCHITECTURE.md` deltas; 3B's wave reconciliation
(landed / refuted / pending / new); the `3C-locks-v+1-diff.md` (the
crystallised LOCKS v+1 — the user reads this before authorising); 3F's
proposed `MIGRATION.md` + `HANDOFF.md` deltas + next-cycle directive.

The user responds: **G3 closed** — the orchestrator updates
`restart/HANDOFF.md` and the synthesis flows into Pass Omega, whose CRUD
wave executes the amendments as document operations on the V1 spec corpus
(LOCKS merge is itself G-Omega-gated per `PASS-OMEGA.md` §6); OR **G3
revise** — the user names specific revisions; the orchestrator dispatches
V{N+1} of T-P3 with the revisions as input constraints. Sign-off is
recorded verbatim in `restart/HANDOFF.md` with a UTC timestamp.

No V1 spec surface is amended by T-P3 itself. T-P3 produces the proposed
diffs; Pass Omega CRUD applies them, post-G-Omega.

## §7 — Hard caps

Per `ORCHESTRATOR.md` §9: substantive pass ~45 min per agent, ~60 min wall
incl. commit; CHALLENGE wave ~90 min wall. Every dispatch carries an
explicit minute cap. At 0.9× the cap the agent commits what it has; at the
cap it halts. An overrun surfaces to the user as an extension decision;
the orchestrator engineers no silent deferral.

## §8 — bbnf-lang specific axes for T-P3

1. **3C is the locks-crystallisation singularity.** Every 1E + 2X amendment candidate from both prior passes converges into ONE v+1 diff with per-candidate disposition. The disposition vocabulary is ACCEPT / REJECT / MODIFY / DEFER; a silent drop is a CH1 + CH6 REJECT. 3C never renumbers a lock; the 16-lock count is amended only by addition or retirement, both G-Omega-gated.
2. **The 5-shape `BackendShape` canon** must stay coherent across 3A (`ARCHITECTURE.md` §7.3), 3B (`MASTER-PLAN.md` §13), and 3E (the per-grammar matrix). A delta that touches one without the others is a CH3 coherence REJECT.
3. **Lock 14 is the binding generalisation discipline.** 3E carries the non-JSON story; 3C accepts no JSON-narrowing amendment; the generic crates stay grammar-neutral. The future-grammar onboarding test is a 3E deliverable.
4. **The skinny→totality fold is monotonic** (3D). Skinny wins become V1-authoritative; skinny rejections become locks-strengthening evidence; the totality spec never dictates back to a live skinny iteration.
5. **No new directive, no new BIR variant, no new substrate.** A T-P1/T-P2 finding that would require one is dispositioned explicitly by 3C (or surfaced by 3A/3B as a REJECTed delta with rationale) — never silently synthesised into the spec.
6. **T-P3 feeds Pass Omega, not the spec.** The boundary is firm: T-P3 proposes proposed-diff documents; Pass Omega CRUD edits the governance surfaces, post-G-Omega. T-P3 that writes `ARCHITECTURE.md` directly is a §7 cross-scope violation.

## §9 — Closing posture

T-P3 is the totality track's distillation pass. It is iterative +
auto-convergent. It converts the converged T-P1 evidence and T-P2 grounded
research into proposed V1 spec amendments — architecture surface deltas, a
wave reconciliation, one crystallised LOCKS v+1 diff, the skinny fold, the
grammar-generalisation story, and the migration + handoff + next-cycle
directive. The CHALLENGE wave is the firewall against uncited deltas and
engineered-defer. The G3 gate is mandatory; Pass Omega CRUD applies what
G3 ratifies.

No delta without an evidence chain. No amendment candidate without a
disposition. No lock renumbered. No spec surface touched by T-P3 itself.
No pass advance without convergence on the prior cycle.

Hereupon the six 3X agents fan out per §2; the CHALLENGE wave hardens per
§3; the loop converges per §4; the orchestrator presents the mandatory G3
gate per §6 and, on G3 close, flows the synthesis into Pass Omega.
