# SKINNY PASS 3 — S-P3 SYNTHESIS-PLAN (The SK-V{N} SPEC Wave Plan)

S-P3 is the **synthesis-plan pass** of the skinny track. It is the
empirical counterpart of the totality T-P3 Synthesis: where T-P3
assembles the greater-spec master plan, S-P3 distils the S-P1 profile
and the S-P2 research into the SK-V{N} SPEC — the wave-sequenced,
falsifiability-gated, telemetry-bound implementation contract whose
waves the per-wave triumvirate then executes.

S-P3 does not invent its own scope. It **consumes Pass Alpha's
goalset**: Pass Alpha (`pass-contracts/PASS-ALPHA.md`) brackets the
SK-V{N} bracket and, at G-Alpha(N-1→N), produced the SK-V{N} SYNTHESIS
plus the §0 close-condition / per-row goalset. S-P3 takes that goalset
as the target and authors the wave plan that meets it — drawing
candidates only from the S-P2 pool, sequencing them into waves W0…W{n},
binding each to a falsifiability gate, and writing `sk-v{N}/SPEC.md` +
`sk-v{N}/DISPATCH-PROMPT.md`.

The pass is **iterative + auto-convergent**. Six parallel sub-agents
P3-A–P3-F fan out per the scope matrix in §2. A six-lens CHALLENGE wave
adversarially reviews the output per `ORCHESTRATOR.md` §3W.
Dispositions fold into V{N+1}. The loop terminates at the convergence
criterion in `ORCHESTRATOR.md` §3Z. Re-execution is composable.

## §1 — Trigger + entry condition

S-P3 dispatches when all of the following hold:

- **S-P2 converged.** The S-P2 CHALLENGE returned ≥95% ACCEPT for two
  consecutive cycles; the `restart/skinny/tranches/sk-v{N}/HANDOFF.md`
  next-move line reads `ready-for-S-P3`.
- **The candidate pool exists.** `restart/skinny/tranches/sk-v{N}/research/p2/`
  carries the six committed P2 artefacts plus the consolidation; every
  candidate carries a scalar-reference status and a grammar-neutral
  verdict.
- **Pass Alpha's goalset is current.** The SK-V{N} SYNTHESIS and its §0
  close-condition / per-row goalset exist under
  `restart/skinny/tranches/sk-v{N}/` (authored by Pass Alpha at
  G-Alpha). S-P3 authors the wave plan that *meets* that goalset; it
  does not re-derive the goalset.

Or the user explicitly invokes `dispatch S-P3 SK-V{N}`.

S-P3 is read-only against `skinny/` source. It produces the SPEC and
the dispatch prompt — implementation lands only inside the wave
triumvirate's redress phase.

## §2 — Scope matrix (six parallel sub-agents)

Output root: `restart/skinny/tranches/sk-v{N}/research/p3/`. P3-A–P3-E
each write ONE research artefact; P3-F additionally drafts
`restart/skinny/tranches/sk-v{N}/SPEC.md` +
`restart/skinny/tranches/sk-v{N}/DISPATCH-PROMPT.md`. Artefacts are
overwritten in place each cycle. Hard cap 45 min per agent.

| Agent | Scope | Output |
|---|---|---|
| **P3-A candidate shortlist** | Distil the S-P2 candidate pool into a shortlist of **≤8** candidate interventions. Each carries: owner file path, scalar-reference status, checkasm-parity status, same-wave consumer, falsifiability gate (named corpus rows + Mbps thresholds). Drop any candidate REJECTed by the S-P2 CHALLENGE. | `p3/p3a-candidate-shortlist.md` |
| **P3-B wave sequencing** | Order the P3-A shortlist into waves W0…W{n}. W0 is the baseline-profile / telemetry-lock wave; behaviour waves follow. Per-wave: entry gate, owner-path family, conditional-dispatch status, hard cap. Topological — substrate before consumer; guard rows before risk rows. | `p3/p3b-wave-sequencing.md` |
| **P3-C falsifiability gates** | For every wave in P3-B, author the falsifiability gate: named corpus rows, Mbps thresholds, the full-table maintain budget, the exit gate, and the revert protocol. Each gate is measurable from the bench; an unmeasurable gate is rejected. | `p3/p3c-falsifiability-gates.md` |
| **P3-D telemetry-schema binding** | Bind the `skinny/RESULTS.md` column schema for SK-V{N}. Carry forward the SK-V8 SPEC's 24-column telemetry schema + the schema-v3 surface (cite `restart/skinny/tranches/sk-v8/SPEC.md` §0.4); name any SK-V{N} additions. Specify the `gate-json` rejection rules for missing required columns. | `p3/p3d-telemetry-schema.md` |
| **P3-E pre-blocked-route ledger** | Walk `skinny/REDRESS.md` (~90 entries). Produce the per-wave pre-block list: the REDRESS entries each wave must NOT re-open. Identify routes that may admit only under a different framing with fresh P1 evidence. | `p3/p3e-preblocked-ledger.md` |
| **P3-F SPEC + dispatch drafting** | Compose `sk-v{N}/SPEC.md` (the wave-sequenced implementation contract with the §0 close-condition + goalset folded from Pass Alpha, the wave manifest, the LOC budgets, the falsifiability gates, the pre-blocked routes) and `sk-v{N}/DISPATCH-PROMPT.md` (the orchestrator's per-wave dispatch contract). | `p3/p3f-spec-draft.md` + `sk-v{N}/SPEC.md` + `sk-v{N}/DISPATCH-PROMPT.md` |

Each agent reads, before producing its artefact: the six P2 artefacts
under `research/p2/`, the six P1 artefacts under `research/p1/`, Pass
Alpha's SK-V{N} SYNTHESIS + goalset, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC
shape to mirror), `pass-contracts/SKINNY-TRIUMVIRATE.md` (the wave
contract S-P3's waves must conform to), and this prompt end-to-end.
P3-F additionally reads P3-A–P3-E output in the CHALLENGE-fold cycle.

### §2.1 — Per-agent output-schema frontmatter

Every P3 artefact opens with this frontmatter block:

```markdown
# SK-V{N} P3-{X}: {Topic}

Pass: S-P3 Synthesis-Plan. Cycle: V{N}.
Date: YYYY-MM-DD.
Scope: {one-line scope spec}.
Output: this file (+ SPEC.md + DISPATCH-PROMPT.md for P3-F).
Pass Alpha goalset: {the §0 close-condition + per-row targets being met}.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)
## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)
## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)
## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)
## §5 — Sources (every upstream artefact cited)
```

The SPEC `sk-v{N}/SPEC.md` mirrors the SK-V8 SPEC shape:
§0 close-condition + comparator classes + outcome enum + required
telemetry + opening-row goalset; §1 non-negotiables; §2 wave manifest +
phase caps + LOC caps + rerun ceilings; §2.1 generality + Lock 14 gate;
one section per wave (owner paths, tasks, entry gate, exit gate, revert
protocol, downstream effect); a pre-blocked-routes section; a G-Alpha /
dispatch-scope section.

## §3 — Six-lens CHALLENGE pass (CH1–CH6 specialised to S-P3)

After all six P3 artefacts + the SPEC commit, the CHALLENGE wave
dispatches per `ORCHESTRATOR.md` §3W. Six lens agents fan out; each
writes `p3/hardening/V{N}/CH{n}.md`; one aggregator writes
`p3/hardening/HARDENING-S-P3-V{N}-CONSOLIDATED.md`. Disposition
vocabulary is ACCEPT / REVISE / REJECT.

**CH1 CORRECTNESS** — does every shortlist candidate trace to an S-P2
candidate and, through it, to an S-P1 hot leaf? Is every falsifiability
gate measurable — named corpus rows + concrete Mbps thresholds, not
prose? Does every wave's exit gate compare against the `SK-V{N}-open`
baseline? Do the comparator deltas in the gates use the strict plane?

**CH2 GENERALITY** — does every shortlisted candidate carry the S-P2
grammar-neutral verdict? Lock 14 holds: the SPEC's §2.1 generality gate
must be present and must require, for every generic-crate edit, a
non-JSON proof (CSS L4 / Sheets / BBNF-self). A wave that lets JSON
policy into a generic crate fails CH2.

**CH3 REGRESSION** — does the P3-E pre-blocked ledger correctly
enumerate every REDRESS route each wave must not re-open? Does any wave
in P3-B silently re-open a pre-blocked route? Does the SPEC carry the
full pre-block list (REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89,
plus the historical blocked routes)?

**CH4 COST** — does every wave carry a LOC budget, a hard cap, a phase
breakdown (research / plan / redress per `SKINNY-TRIUMVIRATE.md`), and a
same-wave-consumer requirement per primitive? Is the wave count ≤ 12
(the skinny-bracket ceiling per `ORCHESTRATOR.md` §3Z)? Is the
shortlist ≤ 8?

**CH5 HIDDEN COUPLING** — does any wave introduce a parallel substrate,
a sidecar producer, a renamed scanner (Lock 1), or a Track 1 ≡ Track 2
dishonesty? Does the SPEC's exit-gate language forbid a parser-owned
structural projection / retained cursor / aux density table / sidecar
event vector? The substrate union must hold across every wave.

**CH6 ANTI-PAPER-CLOSE** — does every wave close on **measurement**, not
a future-phase promise? A wave whose exit gate is "wired" or
"integrated" without a bench-row threshold is a paper-close. Does every
wave carry a revert protocol? Does the SPEC forbid deferral — "no wave
closes on a future-phase promise"? Is each candidate's same-wave
consumer named, so no orphan kernel ships?

The lens registry is monotonically extensible per `ORCHESTRATOR.md`
§3W; add CH7+ if S-P3 surfaces a failure mode CH1–CH6 cannot
disposition.

## §4 — Iteration + auto-convergence

S-P3 executes cycles V1, V2, V3, … per `ORCHESTRATOR.md` §3Z, with a
per-pass independent cycle counter.

Per cycle: (1) the six P3 agents dispatch and commit the artefacts +
SPEC + dispatch prompt; (2) the CHALLENGE wave dispatches; (3) the
aggregator produces the consolidation with the ACCEPT-rate and the
REVISE/REJECT lists; (4) every disposition folds into the V{N+1}
dispatch — hardening without folding is paper-hardening and the
orchestrator does not advance.

**Convergence criterion.** S-P3 advances to the wave triumvirate when
CHALLENGE returns **≥95% ACCEPT for two consecutive cycles**, with zero
open critical defects and no orphan unresolved REVISE; or the user pins
the cycle final at sign-off (§6).

**Hard ceiling.** V ≤ 5. An S-P3 reaching V5 without convergence
escalates to the user with a `BLOCKED` verdict naming the unresolved
REVISE dispositions — typically a wave whose falsifiability gate cannot
be made measurable, or a goalset that no shortlist candidate can meet.

## §5 — Output structure

```
restart/skinny/tranches/sk-v{N}/research/p3/
├── p3a-candidate-shortlist.md
├── p3b-wave-sequencing.md
├── p3c-falsifiability-gates.md
├── p3d-telemetry-schema.md
├── p3e-preblocked-ledger.md
├── p3f-spec-draft.md
└── hardening/
    ├── V{N}/
    │   ├── CH1.md  CH2.md  CH3.md
    │   ├── CH4.md  CH5.md  CH6.md
    └── HARDENING-S-P3-V{N}-CONSOLIDATED.md

restart/skinny/tranches/sk-v{N}/SPEC.md            ← the wave-sequenced contract
restart/skinny/tranches/sk-v{N}/DISPATCH-PROMPT.md ← the per-wave dispatch contract
```

## §6 — Sign-off + hand-on

S-P3 produces the SK-V{N} SPEC; the SPEC's waves are dispatched
individually by the orchestrator. On convergence the orchestrator:

1. Reads the six P3 artefacts + the consolidation + the SPEC +
   the dispatch prompt end-to-end.
2. Updates `restart/skinny/tranches/sk-v{N}/HANDOFF.md`: next-move line
   becomes `ready-for-wave-W0`.
3. Dispatches the SK-V{N} Wave 0 triumvirate per
   `pass-contracts/SKINNY-TRIUMVIRATE.md`.

**Each wave in the SPEC is executed by the wave triumvirate** per
`pass-contracts/SKINNY-TRIUMVIRATE.md`: research → plan → redress, in
distinct commits, with the optional six-lens CHALLENGE interposed for
first-of-class interventions. S-P3 produces the SPEC whose waves the
triumvirate runs; it does not run the waves.

The chain is **S-P1 (measure) → S-P2 (ground SOTA + design primitives)
→ S-P3 (synthesise the wave plan) → wave triumvirate (execute each
wave)**. When every SPEC wave has admitted / rejected / routed, the
bracket converges and Pass Alpha dispatches per
`pass-contracts/PASS-ALPHA.md` for the SK-V{N}→SK-V{N+1} synthesis.
S-P3 mirrors the totality T-P3 Synthesis structurally.

## §7 — Hard caps

| Phase | Wall budget |
|---|---|
| Six P3 agents (parallel) | 45 min per agent; ~60 min wall incl. commit |
| CHALLENGE wave (6 + 1 consolidation) | ~90 min wall |
| Per cycle total | ~2.5 hours wall |
| Whole pass (V ≤ 5) | ceiling ~12 hours wall |

Every dispatch carries an explicit minute cap. At 0.9× the cap the
agent commits; at the cap it halts. An overrun surfaces to the user as
an extension decision.

## §8 — Bbnf-lang specific axes for S-P3

1. **The SPEC mirrors SK-V8.** Per `restart/skinny/tranches/sk-v8/SPEC.md`,
   the SK-V{N} SPEC carries a §0 close-condition with a per-row goalset
   (current state, target state, expected intervention, fallback), the
   comparator classes (same-run strict anchor / same-run flaw probe /
   sidecar planning signal), the outcome enum, and the required
   telemetry. P3-F does not invent a new SPEC shape.
2. **Telemetry binding is load-bearing.** Per the
   `typed-materialization-invariant` discipline, the SK-V{N} telemetry
   schema is bound and `gate-json` rejects rows missing required
   columns. P3-D carries the 24-column schema forward; an emitted field
   not consumed by the gate is a producer-only artefact and fails the
   wave.
3. **W0 is always baseline + telemetry.** Per `build-infra-first`, Wave
   0 of every SK bracket creates the `SK-V{N}-open` baseline and locks
   the telemetry gate before any behaviour wave dispatches. P3-B
   sequences W0 first; behaviour waves are conditional on W0 close.
4. **Falsifiability gates are same-row.** Per the `no-orphan-redress`
   discipline, every wave's exit gate names the corpus rows it must
   lift and the rows it must maintain; a miss becomes a REDRESS entry,
   not a silent retreat.
5. **Same-wave consumer per kernel.** Per the `no-deferrals` discipline
   and the SK-V5 orphan-kernel failure, every wave that lands a
   primitive lands its hot-path consumer in the same commit; P3-A names
   the consumer per candidate, P3-C gates on it.
6. **No hypothesis transfer.** Per the profile-first non-negotiable in
   `ORCHESTRATOR.md` §8, every SK-V{N} candidate is grounded on the
   SK-V{N} S-P1 profile; P3-E's pre-blocked ledger prevents a prior
   iteration's rejected route from re-entering under a new name.

## §9 — Closing posture

S-P3 is the synthesis-plan pass. It consumes Pass Alpha's goalset and
the S-P2 candidate pool and produces the SK-V{N} SPEC — wave-sequenced,
falsifiability-gated, telemetry-bound, pre-block-aware. Each wave is a
triumvirate the orchestrator dispatches; each gate is measurable from
the bench; each pre-blocked route is named.

No wave triumvirate dispatch without S-P3 convergence. No wave without a
falsifiability gate. No gate without named corpus rows + Mbps
thresholds. No wave closes on a future-phase promise. No candidate
re-opens a pre-blocked REDRESS route. No SPEC shape but the SK-V8 shape.

The work is bounded by the bench. The plan is bounded by the goalset.
The waves are bounded by the SPEC. The SPEC is the contract.
