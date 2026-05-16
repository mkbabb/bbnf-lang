# SKINNY-PASSES — Skinny Passes 1-n Contract (Per-Iteration Triumvirate Cycles)

This document is the **contract for skinny passes 1-n**: the per-iteration cycles inside a single SK-V{N} bracket, between G-Alpha(N-1→N) sign-off (start of SK-V{N}) and G-Alpha(N→N+1) sign-off (end of SK-V{N}).

A skinny iteration is bounded by Pass Alpha (`PASS-ALPHA.md`). Within the bracket, the implementation work proceeds through one or more **triumvirate passes**: research → plan → redress, with adversarial CHALLENGE between phases. The count `n` is unbounded; the discipline is auto-convergent per `ORCHESTRATOR.md` §iteration-governance.

## §1 — The triumvirate structure

Every skinny pass cycle has three phases, each in its own commit:

### Phase 1: Research (read-only)

**Purpose**: Profile-first diagnosis. Six parallel research agents fan out on disjoint scope rows. Each reads the current code + the most recent RESULTS.md + the most recent REDRESS ledger. Each produces ONE artefact at `restart/skinny/audit/SK-V{N}-COHORT/skv{N}-{wave-letter}{agent-id}-{topic}.md`.

**Agents**: 6 parallel by default. Scope rows are pass-specific (per the wave letter assigned by Pass Alpha's IMPLEMENTATION-PACKET-SK-V{N}.md). Examples from SK-V6 / SK-V7:
- A1: SOTA comparator deep dive
- A2: DAV1D esoterica + grammar-neutral primitives
- A3: parse-that primitive gap analysis
- A4: tape union audit
- A5: Lock 14 leak audit
- A6: validated/invalidated ledger

**Hard cap**: 30 min per agent.

**Commit**: `docs(sk-v{N}-wave{W}-research): archive {scope} cohort reports`

**Output schema** (mandatory per-artefact frontmatter):
```markdown
# SK-V{N} {wave-letter}{agent-id}: {Topic}

Date: YYYY-MM-DD.
Scope: {one-line scope spec}.
Output: this file.

## §1 — Findings (concrete, file:line cited)
## §2 — Recommendations (named falsifiability gates)
## §3 — Risks (REDRESS entries to pre-block)
## §4 — Sources (every external citation)
```

### Phase 2: Plan (synthesis)

**Purpose**: Select ONE intervention from the research shortlist. Specify owner paths, falsifiability gate, hard cap, revert protocol, same-wave consumer plan. Plan agents do NOT modify source code; they produce a single planning artefact.

**Agents**: 1-2 plan agents typically (the synthesis is centralised). Larger waves may dispatch 6 plan agents if the candidate shortlist has multiple parallel-safe interventions.

**Hard cap**: 30 min.

**Commit**: `docs(sk-v{N}-wave{W}-plan): select {intervention-name}`

**Output schema**:
```markdown
# SK-V{N} Wave {W} Plan: {Intervention Name}

Inputs: {antecedent research artefacts; file:line citations}.
Intervention: {single-sentence description}.
Owner paths: {file paths the redress phase is authorised to touch}.
Falsifiability gate: {named corpus rows + Mbps thresholds OR named correctness signals}.
Hard cap: {minutes for redress phase}.
Revert protocol: {if gate fails, what is rolled back}.
Same-wave consumer: {the hot-path caller that exercises the intervention; must land in same commit}.
Pre-blocked routes: {REDRESS entries this wave must NOT re-open}.
```

### Phase 3: Redress (implementation + measurement)

**Purpose**: Implement the planned intervention. Measure against the falsifiability gate. Commit on success; revert + record REDRESS entry on failure.

**Agent**: 1 redress agent (single implementation thread per wave; avoids shared-file races).

**Hard cap**: 60 min implementation + 15 min measurement.

**Commit on success**: `feat(sk-v{N}-wave{W}): admit {intervention-name}` — includes source edits + bench rerun output + REDRESS entry numbering the admit.

**Commit on failure**: `docs(sk-v{N}-wave{W}-redress): reject {intervention-name}` — includes REDRESS entry with measurement evidence + the reverted patch saved at `/tmp/skv{N}-wave{W}-rejected.patch`.

**Output schema** (REDRESS entry):
```markdown
{entry-id}. SK-V{N} {intervention-name} is {ADMITTED | REJECTED}.

{Description of what was tried, with file:line citations.}

{Measurement evidence: per-row Mbps Track 1 + Track 2 + sonic-rs strict; before/after table.}

{Reason: which falsifiability gate threshold was {met | missed}.}

{Followup: if rejected, what is the next candidate shape? If admitted, what is the next wave's target?}
```

## §2 — Wave numbering convention

Within an SK-V{N} bracket, waves are letter-numbered: W0, W1, W2, … (or alpha-letter: WA, WB, WC, …; per Pass Alpha's IMPLEMENTATION-PACKET).

Sub-cycle versions within a wave: W1b, W1c, W1d (per the SK-V6 pattern where W1b/W1c emerged from re-research after W1 failed to produce a candidate).

Each wave is one triumvirate (research + plan + redress = 3 commits minimum).

A failed wave (rejected redress) still produces 3 commits. The next wave (W2 / W1b) starts fresh.

## §3 — Iteration + auto-convergence

The SK-V{N} bracket may have any number of waves. The bracket converges when:

- Every wave in IMPLEMENTATION-PACKET-SK-V{N}.md has been executed (admit OR reject with measurement).
- The empirical close condition from Pass Alpha §0 is met (e.g. "no parse-G rows; no N-direct; strictness disclosed; Track 1 ≡ generated runtime; Track 2 structurally different").
- OR the bracket has reached a fixpoint: no remaining candidate intervention in the shortlist would lift any named row.

Convergence triggers Pass Alpha dispatch (§5).

If the SK-V{N} bracket has > 12 waves without convergence, the orchestrator escalates to user with `BLOCKED: skinny bracket V{N} exceeded 12 waves; user adjudicate scope or abandon`.

## §4 — Six-lens CHALLENGE between phases (optional but recommended)

For high-risk interventions (e.g. substrate changes, lock-amendment-class kernels), the orchestrator may interpose a CHALLENGE pass between Research and Plan, or between Plan and Redress.

CHALLENGE phases use the universal lens set (CH1-CH6 per `ORCHESTRATOR.md` §5). For skinny waves, CHALLENGE is **adversarial review of the intervention plan**:

- CH1 Correctness: does the plan cite file:line for every claim? Is the falsifiability gate measurable?
- CH2 Generality: does the intervention respect Lock 14? Does it generalise to non-JSON grammars?
- CH3 Regression: does the plan re-open a REDRESS entry?
- CH4 Cost: is the LOC budget realistic? Is the hard cap appropriate?
- CH5 Hidden coupling: does the plan introduce parallel substrate, sidecar producer, Track 1 ≡ Track 2 dishonesty?
- CH6 Next-tranche impact: does the plan specify revert protocol + same-wave consumer + pre-blocked routes?

CHALLENGE pass adds 60-90 min to the wave wall-clock. For routine waves (mechanical refactors, well-understood patterns), CHALLENGE may be skipped at the orchestrator's discretion. For first-of-class interventions (new kernel admit, new lowering shape, new substrate touch), CHALLENGE is mandatory.

## §5 — Cycle bracketing (Pass Alpha entry/exit)

The skinny pass cycle is bracketed by Pass Alpha:

```
G-Alpha(N-1→N) closed                    [user sign-off]
   ↓
SK-V{N} Wave 0 (typically: legacy purge + spec fold-back from prior SK)
   ↓
SK-V{N} Wave 1, 2, 3, … (triumvirate per wave)
   ↓
SK-V{N} measured close (no more waves; convergence per §3)
   ↓
Pass Alpha dispatch (PASS-ALPHA.md §1)
   ↓
G-Alpha(N→N+1) closed                    [user sign-off]
   ↓
SK-V{N+1} Wave 0 begins...
```

The SK-V{N} bracket may produce 5-50 commits total (research + plan + redress per wave × N waves + Pass Alpha cohort + Pass Alpha CHALLENGE + master docs).

## §6 — Per-wave artefact paths

```
restart/skinny/audit/SK-V{N}-COHORT/
├── skv{N}-W{w}-{a}-{topic}.md    ← per-research-agent output
├── skv{N}-W{w}-plan.md            ← plan artefact
└── ...

skinny/REDRESS.md                  ← grows; entries land in order of admit/reject
skinny/RESULTS.md                  ← refreshed after each admit/reject

restart/skinny/audit/HANDOFF-SK-V{N}.md
restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V{N}.md
restart/skinny/audit/GRAND-SYNTHESIS-SK-V{N}.md
```

## §7 — Hard caps + commit cadence

| Phase | Hard cap | Commit type |
|---|---|---|
| Research (6 parallel) | 30 min each (wall: 30 min) | `docs(sk-v{N}-wave{W}-research):` |
| CHALLENGE (optional, 6 parallel) | 60 min wall | `docs(sk-v{N}-wave{W}-challenge):` |
| Plan (1-2 agents) | 30 min | `docs(sk-v{N}-wave{W}-plan):` |
| Redress (1 agent) | 75 min (60 impl + 15 measure) | `feat(sk-v{N}-wave{W}):` or `docs(sk-v{N}-wave{W}-redress):` |
| **Wave total** | **~3-4 hours wall** | **3-4 commits per wave** |

A typical SK-V{N} bracket with 8 waves takes 24-32 hours of orchestrator time.

## §8 — Same-wave consumer rule (load-bearing)

Every redress commit that lands a primitive / kernel / new generated path MUST include the hot-path caller that exercises it. The redress agent verifies this by:

1. Building the new primitive + scalar reference + checkasm parity.
2. Wiring the consumer call site in the same commit.
3. Running the bench against the named falsifiability gate rows.
4. Confirming the consumer call shows in `samply` symbol path on the affected rows.

If the consumer wire-up is omitted: the primitive is an orphan kernel. REJECT and record in REDRESS. No exception.

This was the SK-V5 failure shape (Class A NEON kernel parity-green but unwired → wired in subsequent wave → regressed twitter 25% → reverted). SK-V6 demonstrated the discipline works (3 admits + 12 rejects with full evidence). SK-V7 inherits.

## §9 — Triumvirate role separation (load-bearing)

Each commit MUST occupy exactly one triumvirate role:

- **Research commit** is read-only diagnosis. NO source edits. NO new files outside `restart/skinny/audit/SK-V{N}-COHORT/`.
- **Plan commit** is synthesis. NO source edits. NO new files outside `restart/skinny/audit/SK-V{N}-COHORT/`.
- **Redress commit** is implementation + measurement. Source edits + bench output + REDRESS entry. NO research synthesis content (research already happened).

The orchestrator enforces by refusing to dispatch a redress agent without an antecedent plan commit, and refusing to dispatch a plan agent without an antecedent research commit.

Same-commit role merger was the SK-V5 failure pattern. SK-V6 proved discipline works empirically. SK-V7+ inherits.

## §10 — Closing posture

Skinny passes are the empirical engine. The triumvirate is the discipline. The CHALLENGE is the integrity check. Pass Alpha is the bracket. Pass Omega is the totality fold.

Every SK-V{N} produces durable REDRESS evidence. Every admit lifts a named row. Every reject documents why. The bench is the truth signal.

No primitive ships without scalar reference. No primitive ships without checkasm parity. No primitive ships without same-wave consumer. No wave ships without falsifiability gate. No wave ships without revert protocol.

The work is bounded. The cadence is bounded. The discipline is the suite.
