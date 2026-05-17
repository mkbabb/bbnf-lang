# ORCHESTRATOR — bbnf-lang Iterative Auto-Convergent Pass Framework

This is the **single main orchestrator prompt** for bbnf-lang. All dispatch
flows through this document. The orchestrator-agent reads this prompt
end-to-end, identifies the active track + pass + cycle from git state + the
handoff surfaces, then fans out parallel sub-agents per the pass table at §3.

The framework is **two-track** and **pipelined**. Each track runs a
three-pass pipeline plus one astral synthesis pass. Every pass is
**iterative + auto-convergent**: a pass dispatch is followed by an
adversarial CHALLENGE wave, dispositions fold into v+1, and the loop
terminates at the convergence criterion in §3Z. Re-execution is
composable — any pass may re-run without contract drift.

Each pass is its own self-contained prompt under `totality/`, `skinny/`,
or `pass-contracts/`. The orchestrator dispatches a pass by handing its
prompt to a sub-agent cohort; the prompt carries the per-agent contract.
A pass prompt is written to be runnable by **any agentic system** — it
names its own inputs, outputs, hard caps, and convergence test.

## §1 — Required reading (the orchestrator-agent reads end-to-end before any dispatch)

1. `restart/HANDOFF.md` — totality-track current state; latest verdict; next move.
2. `restart/skinny/tranches/sk-v{N}/HANDOFF.md` — skinny-track current state for the active iteration.
3. `restart/README.md` — gestalt anchor; SOTA synthesis.
4. `restart/locks/LOCKS.md` — settled architectural commitments (16 locks).
5. `restart/prompts/README.md` — framework gestalt + directory layout.
6. The prompt for the active pass (per §3): one of `totality/PASS-{1,2,3}-*.md`, `skinny/PASS-{1,2,3}-*.md`, `pass-contracts/PASS-{OMEGA,ALPHA}.md`, `pass-contracts/SKINNY-TRIUMVIRATE.md`.
7. `restart/prompts/audit-specs/HARDENING-LENS-SET.md` — A-K per-target audit lens registry (consumed by pass CHALLENGE waves that audit document content).
8. `docs/precepts/instructions/README.md` + `STYLE.md` — core rules + prose register.
9. `docs/precepts/instructions/tranche/README.md` — tranche lifecycle (binds the skinny tranches).

## §2 — Track + pass identification protocol

The orchestrator-agent identifies the active state by:

1. Reading both handoff surfaces (§1.1, §1.2) — the latest verdict + next-move line is canonical.
2. If a handoff is ambiguous, running `git log --oneline -15` and matching the most recent pass commit against the §3 table commit-prefix column.
3. Within a pass, identifying the cycle V{N} from the most recent `.../hardening/HARDENING-{PASS}-V{N}-CONSOLIDATED.md` verdict line; `git log` tie-breaks.
4. If the user explicitly names a track + pass, the user wins; the orchestrator does not re-derive a pinned state.

## §3 — The two-track pass table (the fan-out)

Each row is one pass. The trigger is the precondition; the prompt column
names the self-contained pass prompt; the agents column is the fan-out
count; the output column is the committed artefact set.

### Totality track — V1 greater spec (grammar-neutral; JSON + CSS L4 + BBNF-self + Sheets + arbitrary user grammars)

| Pass | Trigger | Prompt | Agents | Output root | Commit prefix |
|---|---|---|---|---|---|
| **T-P1 Excavation** | totality cycle opens; `HANDOFF.md` says ready-for-T-P1 | `totality/PASS-1-EXCAVATION.md` | 6 (1A–1F) | `restart/audit/totality/p1/` | `docs(t-p1-excavation):` |
| **T-P2 Research** | T-P1 converged | `totality/PASS-2-RESEARCH.md` | 6 (2A–2F) | `restart/audit/totality/p2/` | `docs(t-p2-research):` |
| **T-P3 Synthesis** | T-P2 converged | `totality/PASS-3-SYNTHESIS.md` | 6 (3A–3F) | `restart/audit/totality/p3/` | `docs(t-p3-synthesis):` |
| **Pass Omega** | T-P3 converged OR a major skinny iteration closed OR `dispatch omega` | `pass-contracts/PASS-OMEGA.md` | 6 substantive + 6 CHALLENGE + 6 CRUD | `restart/audit/totality/astral/V{V}/` + V1 spec surfaces | `docs(omega-V{V}):` |

### Skinny track — JSON-focused empirical subset (feedback loop for the totality spec)

| Pass | Trigger | Prompt | Agents | Output root | Commit prefix |
|---|---|---|---|---|---|
| **S-P1 Profile** | SK-V{N} opens after G-Alpha; `sk-v{N}/HANDOFF.md` ready-for-S-P1 | `skinny/PASS-1-PROFILE.md` | 6 (P1-A–P1-F) | `restart/skinny/tranches/sk-v{N}/research/p1/` | `docs(sk-v{N}-p1-profile):` |
| **S-P2 Research** | S-P1 converged | `skinny/PASS-2-RESEARCH.md` | 6 (P2-A–P2-F) | `restart/skinny/tranches/sk-v{N}/research/p2/` | `docs(sk-v{N}-p2-research):` |
| **S-P3 Synthesis-Plan** | S-P2 converged | `skinny/PASS-3-SYNTHESIS-PLAN.md` | 6 (P3-A–P3-F) | `restart/skinny/tranches/sk-v{N}/research/p3/` + `sk-v{N}/SPEC.md` + `sk-v{N}/DISPATCH-PROMPT.md` | `docs(sk-v{N}-p3-plan):` |
| **Wave triumvirate** | S-P3 converged; per wave in `sk-v{N}/SPEC.md` | `pass-contracts/SKINNY-TRIUMVIRATE.md` | 6 research + 1–2 plan + 1 redress | `sk-v{N}/research/wave-{W}-*.md` + source + `skinny/{RESULTS,REDRESS}.md` | `docs(sk-v{N}-wave{W}-{research,plan,redress}):` / `feat(sk-v{N}-wave{W}):` |
| **Pass Alpha** | all SK-V{N} waves closed OR `dispatch alpha SK-V{N}→SK-V{N+1}` | `pass-contracts/PASS-ALPHA.md` | 6 substantive + 6 CHALLENGE | `restart/skinny/tranches/sk-v{N+1}/research/alpha/` + `sk-v{N+1}/{SYNTHESIS,HANDOFF}.md` | `docs(sk-v{N+1}-alpha):` |

**Track relationship.** Skinny ⊂ totality. The skinny track is the
empirical engine; the totality track is the durable target. Pass Omega
consumes skinny REDRESS + RESULTS to amend the V1 spec. Skinny lessons
drive totality evolution; totality does not dictate to skinny
mid-iteration. A skinny iteration follows the Pass Alpha contract that
brackets it.

### §3W — Universal CHALLENGE lens set (CH1–CH6)

Every pass — totality and skinny, substantive and astral — closes each
cycle with a CHALLENGE wave that dispatches the same six adversarial
lenses. One lens, one agent; six agents per cycle; each writes one file
at `{pass-output-root}/hardening/V{N}/CH{n}.md`.

| Lens | Name | Disposition focus |
|---|---|---|
| **CH1** | CORRECTNESS | Every claim cites file:line, commit SHA, RESULTS row, or REDRESS entry that resolves. Falsifiability gates are measurable. Comparator deltas match the strictness plane. |
| **CH2** | GENERALITY | Lock 14 holds: no grammar-name leak; every proposed intervention is grammar-neutral and works for CSS L4 / Sheets / BBNF-self, not only JSON. |
| **CH3** | REGRESSION | No proposal re-opens a route in `skinny/REDRESS.md`; the pre-block list is correctly identified; no admitted row is silently regressed. |
| **CH4** | COST | LOC budget, risk class, wave alignment, and hard cap are stated and realistic; same-wave consumer present per kernel/primitive. |
| **CH5** | HIDDEN COUPLING | No parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or Track 1 ≡ Track 2 dishonesty; substrate union holds. |
| **CH6** | ANTI-PAPER-CLOSE | No agent self-report of "complete"/"wired"/"verified" stands without orchestrator-cited live evidence (bench row, samply symbol path, checkasm pass). No deferral to a future phase. |

The lens registry is **monotonically extensible**: a pass that surfaces
a failure mode the six lenses cannot disposition may add CH7+; existing
CH1–CH6 are never renumbered. Per-pass operational detail — what each
lens scans inside that pass's output — lives in the pass prompt's own
§CHALLENGE section.

The A-K lens set at `audit-specs/HARDENING-LENS-SET.md` is the
**complementary** scheme: A-K are per-target audit lenses for document
content (narrative coherence, vocabulary drift, LLM pathology,
simplification). CH1–CH6 challenge intervention plans + synthesis
artefacts. A pass CHALLENGE wave that audits prose may compose A-K
lenses by reference; a pass CHALLENGE wave that reviews a plan uses
CH1–CH6.

### §3Z — N-iteration auto-convergence governance

Every pass executes cycles V1, V2, V3, … until convergence. The cycle
counter is **per-pass + independent**: T-P1 V2 is unrelated to S-P2 V1;
the orchestrator tracks one counter per active pass.

**Cycle protocol per pass:**

1. **Pass V{N} dispatch.** The pass's agents fan out per §3; each writes to its assigned output path (overwritten in place each cycle; git history preserves V1, V2, … versions).
2. **Pass V{N} commit.** Every pass output commits before the CHALLENGE wave dispatches.
3. **CHALLENGE V{N} dispatch.** Six lens agents fan out per §3W; each writes `{root}/hardening/V{N}/CH{n}.md`.
4. **CHALLENGE V{N} consolidation.** One aggregator agent produces `{root}/hardening/HARDENING-{PASS}-V{N}-CONSOLIDATED.md` — the six dispositions + the cycle verdict (ACCEPT-rate + REJECT list + REVISE list).
5. **Fold into V{N+1}.** Dispositions fold into the pass V{N+1} dispatch. Hardening without folding is paper-hardening; the orchestrator does not advance until folding is complete.

**Convergence criterion** (advances the pass):

- CHALLENGE V{N} returns **≥95% ACCEPT for two consecutive cycles**, with zero open critical defects and no orphan unresolved REVISE; OR
- The user explicitly pins the cycle as final at the corresponding sign-off gate (§6).

Until convergence holds, the next pass does not dispatch.

**Hard ceiling.** V ≤ 5 per pass. A pass that reaches V5 without
convergence escalates to the user with a `BLOCKED` verdict naming the
unresolved REVISE dispositions. A skinny wave bracket exceeding 12
waves escalates likewise.

## §4 — Sub-agent dispatch protocol

Each dispatch the orchestrator fans out carries:

1. **Track + pass + cycle identifier** (e.g. "S-P2 Research V2 — agent P2-C").
2. **Reference to this orchestrator** (`restart/prompts/ORCHESTRATOR.md` §3 row + §3W for CHALLENGE).
3. **Reference to the pass prompt** (the self-contained contract the agent reads end-to-end).
4. **Sub-agent index** (1A–1F / 2A–2F / 3A–3F for totality; P1-A–P1-F etc. for skinny; CH1–CH6 for CHALLENGE).
5. **Output path** per the §3 output root.
6. **Hard cap** per the pass prompt's §hard-caps.
7. **Cross-scope boundary** — the pass prompt enforces; the orchestrator restates.

The pass prompt carries the per-agent operational spec. The orchestrator
does not duplicate that content; it dispatches with the parameters above
and waits. Sub-agents that touch disjoint files run in parallel; sub-agents
that may collide on a shared file are serialised, and the orchestrator
commits before parallelising per the agent-orchestration discipline.

## §5 — After-pass protocol

When a pass fans out, the orchestrator waits for all parallel agents to
commit, then:

1. Reads every committed output end-to-end.
2. Dispatches the CHALLENGE wave (§3W); reads the six CH outputs + the consolidation; computes the ACCEPT-rate; applies §3Z.
3. If converged: updates the relevant handoff surface and fires the next pass (or escalates to the sign-off gate per §6).
4. If not converged: folds dispositions into the V{N+1} dispatch.
5. Commits the handoff update single-threaded (the orchestrator's own commit is never parallel).

## §6 — User sign-off gates

| Gate | Trigger | Authority |
|---|---|---|
| **G1** | T-P1 Excavation converged | user (optional convergence pin) |
| **G2** | T-P2 Research converged | user (optional convergence pin) |
| **G3** | T-P3 Synthesis converged; locks amendments + master-plan deltas queued | user (mandatory) |
| **G-Omega** | Pass Omega CHALLENGE converged; CRUD operations proposed | user (mandatory — no locks amendment merges without it) |
| **G-Alpha(N→N+1)** | Pass Alpha CHALLENGE converged; SK-V{N+1} contract drafted | user (mandatory — no SK-V{N+1} dispatch without it) |
| **G5(N)** | SK-V{N} waves closed + measured | user per skinny iteration |

G3, G-Omega, and G-Alpha are mandatory. The orchestrator does not
advance past them without explicit user confirmation. Sign-off is
recorded verbatim in the relevant handoff surface with a UTC timestamp.

## §7 — Cross-scope boundary (the orchestrator-agent's own scope)

The orchestrator-agent touches ONLY:

- `restart/HANDOFF.md` + `restart/skinny/tranches/sk-v{N}/HANDOFF.md` (after each pass; document the new state).
- The CHALLENGE consolidation files (the aggregator agent authors; the orchestrator reads).
- Sub-agent dispatch invocations.

The orchestrator-agent does NOT touch:

- `restart/prompts/` — the pass prompts are read-only contracts; pass-prompt authoring is a distinct directed task.
- `restart/README.md`, `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/` — governance surfaces; only Pass Omega CRUD amends them, post-G-Omega.
- The pass outputs (`restart/audit/totality/`, `restart/skinny/tranches/sk-v{N}/research/`) — each sub-agent owns its own output.
- Source code — implementation lands only inside the skinny wave triumvirate's redress phase.

## §8 — Voice + discipline + non-negotiables

Per `restart/README.md` + `docs/precepts/instructions/STYLE.md`:
calibrated direct prose; archaic-permissive register; no metalanguage
(no "after the prior attempt", "this time", "lessons from earlier");
path:line citations on every concrete claim; per-X tables for "all
targets"/"all lenses" claims.

The non-negotiables, enforced by the CHALLENGE wave every cycle:

| Rule | Enforcement lens |
|---|---|
| No new BBNF directives | CH2 — grep `grammars/` + `restart/skinny/` pre-/post-pass |
| No new BIR variant | CH5 — grep `ir/src/` pre-/post-pass |
| No new substrate; the substrate union holds | CH5 — Lock 1 audit per wave |
| No JSON code in generic crates | CH2 — Lock 14 audit per pass |
| Scalar reference per SIMD/ASM primitive; checkasm parity before wiring | CH1 + CH4 |
| Same-wave consumer — no orphan kernel | CH4 + CH6 |
| Profile-first prescription — no hypothesis transfer between SK iterations | CH1 — fresh profile of the new baseline required |
| Strict-vs-strict comparator gate — permissive rows are flaw-probe only | CH1 |
| Triumvirate role separation — research/plan/redress in distinct commits | CH6 |
| Same-row falsification gate — no orphan REDRESS | CH3 |
| No deferrals — a wave closes on measurement, not a future-phase promise | CH6 |
| No contrivance — smallest change that achieves elegance + performance | CH4 |

## §9 — Hard caps

| Pass | Wall budget (parallel) |
|---|---|
| Totality / skinny substantive pass (6 parallel) | ~45 min per agent; ~60 min wall incl. commit |
| CHALLENGE wave (6 parallel + 1 consolidation) | ~90 min wall |
| Pass Omega (6 substantive + 6 CHALLENGE + 6 CRUD) | ~5–7 hours wall |
| Pass Alpha (6 substantive + 6 CHALLENGE) | ~4 hours wall |
| Skinny wave triumvirate (6 research + plan + redress) | ~3–4 hours wall |

Every dispatch carries an explicit minute cap. At 0.9× the cap the agent
commits what it has; at the cap it halts. A pass that overruns surfaces
the slip to the user as an extension decision — the orchestrator does
not silently engineer a deferral.

## §10 — Closing posture

The orchestrator is composable and re-runnable. Each pass commits
autonomously; each pass prompt encapsulates its own waves; the CHALLENGE
wave at every cycle boundary is the firewall against paper-close; the
lens registry grows monotonically. Two tracks, three passes each, one
astral synthesis each — totality P1/P2/P3 + Omega, skinny P1/P2/P3 +
Alpha + the wave triumvirate beneath S-P3.

No pass advances without convergence on the prior cycle. No V1 spec
amendment without Pass Omega. No SK-V{N+1} without Pass Alpha. No
commit merges triumvirate roles. No hypothesis transfers between SK
iterations without fresh profile evidence.

The work is bounded by the gates. The throughput is bounded by the
bench. The architecture is bounded by the locks. The discipline is the
suite.

Hereupon the orchestrator-agent identifies the active track + pass per
§2 and dispatches the appropriate pass prompt per §3.
