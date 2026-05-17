# bbnf-lang Prompt Suite — Iterative Auto-Convergent Two-Track Pass Framework

This directory carries the prompt contracts for bbnf-lang's pass
framework. The orchestrator-agent reads these to identify the active
track + pass + cycle and dispatch sub-agent cohorts. Every pass prompt
is a **self-contained dispatch contract** — an agentic system handed one
pass prompt plus `ORCHESTRATOR.md` can run that pass end-to-end.

## Directory layout

```
restart/prompts/
├── README.md                          ← this file
├── ORCHESTRATOR.md                    ← top-level dispatcher; the binding contract
├── totality/                          ← totality-track pass prompts (V1 greater spec)
│   ├── PASS-1-EXCAVATION.md            ← T-P1: current-state evidence excavation
│   ├── PASS-2-RESEARCH.md              ← T-P2: SOTA + architecture literature grounding
│   └── PASS-3-SYNTHESIS.md             ← T-P3: distil P1+P2 → spec amendments
├── skinny/                            ← skinny-track pass prompts (JSON empirical subset)
│   ├── PASS-1-PROFILE.md               ← S-P1: samply 3-way + PMU baseline profiling
│   ├── PASS-2-RESEARCH.md              ← S-P2: SOTA teardown + primitive design
│   └── PASS-3-SYNTHESIS-PLAN.md        ← S-P3: distil P1+P2 → SK-V{N} SPEC wave plan
├── pass-contracts/                    ← astral synthesis + per-wave contracts
│   ├── PASS-OMEGA.md                   ← totality astral: V1 spec cohesion + skinny fold-in
│   ├── PASS-ALPHA.md                   ← skinny astral: SK-V{N+1} contract creation
│   └── SKINNY-TRIUMVIRATE.md           ← per-wave research/plan/redress contract (beneath S-P3)
├── audit-specs/
│   └── HARDENING-LENS-SET.md           ← A-K per-target document-audit lens registry
└── sub-orchestrators/                 ← legacy operational detail; superseded by the pass prompts
    ├── HARDENING.md
    ├── RESEARCH-FOLD.md
    └── AMENDMENT-DISPATCH.md
```

The `sub-orchestrators/` directory is **legacy** — its hardening /
research-fold / amendment-dispatch cycles were the pre-framework totality
dispatch path. The three discrete `totality/` pass prompts supersede it.
The directory survives for historical reference and is queued for Pass
Omega CRUD archival; it is no longer in the `ORCHESTRATOR.md` §3 fan-out.

## Reading order

1. **This README** — framework gestalt.
2. **`ORCHESTRATOR.md`** — track + pass identification, the §3 pass table, §3W CHALLENGE lenses, §3Z convergence governance, §6 sign-off gates.
3. **The prompt for the active pass** — per the `ORCHESTRATOR.md` §3 table.

## Framework gestalt

bbnf-lang has two architectural tracks. The **totality track** drives
the V1 greater spec — grammar-neutral, targeting JSON + CSS L4 +
BBNF-self + Sheets + arbitrary user grammars. The **skinny track** drives
the SK-V{N} iterations — a JSON-focused empirical subset that serves as
the feedback loop proving the greater spec out. Skinny ⊂ totality:
skinny is the empirical engine; totality is the durable target; skinny
lessons fold up into the V1 spec, never the reverse mid-iteration.

Each track runs a **three-pass pipeline plus one astral synthesis pass**:

| Track | P1 | P2 | P3 | Astral |
|---|---|---|---|---|
| **Totality** | Excavation — current-state evidence | Research — SOTA + architecture grounding | Synthesis — spec amendments | **Pass Omega** — V1 spec cohesion + skinny fold-in + CRUD |
| **Skinny** | Profile — samply 3-way + PMU baseline | Research — SOTA teardown + primitive design | Synthesis-Plan — SK-V{N} SPEC wave plan | **Pass Alpha** — SK-V{N+1} contract creation |

Beneath skinny S-P3 sits the **wave triumvirate**
(`pass-contracts/SKINNY-TRIUMVIRATE.md`): each wave of the S-P3 wave
plan executes one research → plan → redress cycle in three distinct
commits. The triumvirate is the implementation layer; S-P3 is the
planning layer above it; Pass Alpha brackets the whole iteration.

## Iteration + auto-convergence

Every pass — totality and skinny, substantive and astral — is iterative.
A pass executes cycles V1, V2, V3, …; each cycle closes with a six-lens
CHALLENGE wave; dispositions fold into v+1. The cycle counter is
per-pass and independent.

**Convergence** (per `ORCHESTRATOR.md` §3Z): CHALLENGE returns ≥95%
ACCEPT for two consecutive cycles, with zero open critical defects and
no orphan unresolved REVISE — OR the user pins the cycle as final at the
pass's sign-off gate.

**Hard ceiling**: V ≤ 5 per pass; a skinny wave bracket ≤ 12 waves.
Overflow escalates to the user with a `BLOCKED` verdict.

## Two complementary lens schemes

**CH1–CH6** (`ORCHESTRATOR.md` §3W) — the universal CHALLENGE lens set.
Every pass cycle closes with these six adversarial lenses reviewing the
pass output: CH1 Correctness, CH2 Generality, CH3 Regression, CH4 Cost,
CH5 Hidden Coupling, CH6 Anti-Paper-Close. They challenge intervention
plans and synthesis artefacts for falsifiability + regression risk.

**A-K** (`audit-specs/HARDENING-LENS-SET.md`) — the per-target
document-audit lens registry: five carry-aware lenses (A-E), three
LLM-pathology lenses (F-H), three simplification lenses (I-K). They
audit document *content* for authorial pathologies + coverage gaps. A
CHALLENGE agent auditing prose may compose A-K by reference.

The two schemes are complementary: CH1–CH6 challenge plans; A-K audit
content.

## Sign-off gates

| Gate | Trigger | Authority |
|---|---|---|
| **G1** | T-P1 Excavation converged | user (optional pin) |
| **G2** | T-P2 Research converged | user (optional pin) |
| **G3** | T-P3 Synthesis converged; locks + master-plan deltas queued | user (mandatory) |
| **G-Omega** | Pass Omega CHALLENGE converged; CRUD proposed | user (mandatory) |
| **G-Alpha(N→N+1)** | Pass Alpha CHALLENGE converged; SK-V{N+1} contract drafted | user (mandatory) |
| **G5(N)** | SK-V{N} waves closed + measured | user per skinny iteration |

G3, G-Omega, G-Alpha are mandatory. No V1 spec amendment merges without
G-Omega; no SK-V{N+1} dispatches without G-Alpha.

## Non-negotiables (apply across all passes; enforced by the CHALLENGE wave)

| Rule | Lens |
|---|---|
| No new BBNF directives; no new BIR variant; no new substrate | CH2 / CH5 |
| The substrate union holds — structural projection IS the tape | CH5 |
| No JSON code in generic crates (Lock 14 grammar-neutrality) | CH2 |
| Scalar reference per SIMD/ASM primitive; checkasm parity before wiring | CH1 / CH4 |
| Same-wave consumer — no orphan kernel | CH4 / CH6 |
| Profile-first prescription — no hypothesis transfer between SK iterations | CH1 |
| Strict-vs-strict comparator gate — permissive rows are flaw-probe only | CH1 |
| Triumvirate role separation — research/plan/redress in distinct commits | CH6 |
| Same-row falsification gate — no orphan REDRESS | CH3 |
| No deferrals — a wave closes on measurement, not a future-phase promise | CH6 |
| No contrivance — the smallest change that achieves elegance + performance | CH4 |

## Dispatch invocation phrases

| Phrase | Action |
|---|---|
| `dispatch t-p1` / `t-p2` / `t-p3` | Dispatch the named totality pass per `totality/PASS-{1,2,3}-*.md` |
| `dispatch omega` | Dispatch Pass Omega per `pass-contracts/PASS-OMEGA.md` |
| `dispatch sk-v{N} p1` / `p2` / `p3` | Dispatch the named skinny pass per `skinny/PASS-{1,2,3}-*.md` |
| `dispatch sk-v{N} W{w}` | Dispatch one wave triumvirate per `sk-v{N}/SPEC.md` |
| `dispatch alpha SK-V{N}→SK-V{N+1}` | Dispatch Pass Alpha per `pass-contracts/PASS-ALPHA.md` |
| `status` | Orchestrator emits active track + pass + cycle + open dispositions |
| `pin {pass}` | User overrides the auto pass-identification |
| `abandon SK-V{N}` | Orchestrator nukes uncommitted SK-V{N} artefacts + restores prior state |

## Repository layout (the surfaces the passes read + write)

```
restart/
├── README.md            ARCHITECTURE.md       ← V1 spec gestalt + greater spec
├── MASTER-PLAN.md       MIGRATION.md          HANDOFF.md
├── locks/LOCKS.md                             ← the 16 architectural locks
├── prompts/                                   ← THIS DIRECTORY
├── audit/
│   └── totality/{p1,p2,p3,astral}/            ← totality pass + Pass Omega outputs
├── research/                                  ← totality research deep-dives
└── skinny/
    ├── {INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE,HARDENING}.md
    └── tranches/
        └── sk-v{N}/
            ├── SYNTHESIS.md  SPEC.md  HANDOFF.md  DISPATCH-PROMPT.md
            └── research/{p1,p2,p3,alpha}/      ← skinny pass + Pass Alpha outputs

skinny/RESULTS.md        ← the empirical bench gate (authority)
skinny/REDRESS.md        ← the rejected-route + admitted-win ledger
```

## Closing posture

The prompt suite is the durable orchestrator. The skinny + totality
content evolves through iteration; the framework gives the iteration a
shape — two tracks, three passes each, one astral synthesis each,
auto-convergent, challenge-hardened, telemetry-bound, gate-controlled.

No pass advances without convergence on the prior cycle. No V1 spec
amendment without Pass Omega. No SK-V{N+1} without Pass Alpha. No commit
merges triumvirate roles. No hypothesis transfers between SK iterations
without fresh profile evidence.

The work is bounded by the gates. The throughput is bounded by the
bench. The architecture is bounded by the locks. The discipline is the
suite.
