# bbnf-lang Prompt Suite — Iterative Auto-Convergent Multi-Pass Framework

This directory carries the prompt contracts for bbnf-lang's totality + skinny
pass framework. The orchestrator-agent reads these to identify the current
pass + cycle and dispatch sub-agents.

## Directory layout (post-SK-V7 restructure)

```
restart/prompts/
├── README.md                              ← this file
├── ORCHESTRATOR.md                        ← top-level dispatcher
├── sub-orchestrators/                     ← phase-type dispatchers
│   ├── HARDENING.md                       ← hardening-cycle sub-orchestrator
│   ├── RESEARCH-FOLD.md                   ← research-fold sub-orchestrator
│   └── AMENDMENT-DISPATCH.md              ← verify-then-patch sub-orchestrator
├── pass-contracts/                        ← pass-specific contracts
│   ├── PASS-ALPHA.md                      ← skinny astral synthesis (SK-V{N+1} cycle creation)
│   ├── PASS-OMEGA.md                      ← totality astral synthesis (V1 spec cohesion + skinny fold-in)
│   └── SKINNY-TRIUMVIRATE.md              ← per-iteration triumvirate (research/plan/redress)
└── audit-specs/                           ← per-target audit lens contracts
    └── HARDENING-LENS-SET.md              ← lens registry (A-K target audit + CH1-CH6 challenge)
```

## Reading order

1. **This README** (framework gestalt — 5 min read).
2. **`ORCHESTRATOR.md`** (phase identification + dispatch matrix + sub-orchestrator routing).
3. **The contract relevant to the active pass**:
   - Skinny iteration cycle: `pass-contracts/SKINNY-TRIUMVIRATE.md` (per-wave research/plan/redress contract).
   - Skinny astral (SK-V{N+1} creation): `pass-contracts/PASS-ALPHA.md`.
   - Totality astral (V1 spec cohesion + locks amendment): `pass-contracts/PASS-OMEGA.md`.
   - Hardening cycle: `sub-orchestrators/HARDENING.md` + `audit-specs/HARDENING-LENS-SET.md`.
   - Research-fold (deep-dive + topic absorption): `sub-orchestrators/RESEARCH-FOLD.md`.
   - Amendment-dispatch (narrow verify-then-patch): `sub-orchestrators/AMENDMENT-DISPATCH.md`.

## Framework gestalt

bbnf-lang has two architectural tracks: the **V1 totality spec** (greater
architecture; grammar-neutral; targets JSON + CSS L4 + BBNF-self + Sheets +
arbitrary user grammars) and the **skinny spec subset** (JSON-focused
implementation serving as feedback loop for the greater spec). Skinny
iterations (SK-V1 through SK-V{n}) are the empirical engine; the totality
spec is the durable target.

The prompt suite formalises this duality.

**Two parallel pass tracks**:

- **Totality passes** drive V1 spec evolution via the existing
  `sub-orchestrators/` (hardening + research-fold + amendment-dispatch).
  Each runs V1-V9+ cycles per ORCHESTRATOR.md §5 cycle-naming canon. The
  **Pass Omega** (`pass-contracts/PASS-OMEGA.md`) is the periodic cohesion
  + skinny-lessons-fold-in layer above the sub-orchestrators.
- **Skinny passes** drive SK-V{N} evolution via the **SKINNY-TRIUMVIRATE**
  (`pass-contracts/SKINNY-TRIUMVIRATE.md`) per-wave research/plan/redress
  pattern. Each SK-V{N} bracket runs 1-12 waves and closes when the
  measured close condition (no parse-G, no N-direct, strict-vs-strict
  comparator gate) holds OR a fixpoint is reached. **Pass Alpha**
  (`pass-contracts/PASS-ALPHA.md`) brackets each cycle by producing the
  next SK-V{N+1} contract with detailed goalset + precisely-defined
  telemetry binding.

## Lens registry — two complementary schemes

The audit + challenge layers use two distinct lens schemes:

**A-K lens scheme** (`audit-specs/HARDENING-LENS-SET.md`) — per-target
audit lenses for hardening cycles. Five carry-aware lenses A-E (inter-
document narrative coherence + vocabulary drift + worked-example scarcity
+ coverage gaps + architectural axiom consistency), three LLM-pathology
lenses F-H (bias + overfitting + hallucination), three simplification
lenses I-K (contrivance + host-language leverage + meta-grammar
discipline). Used by `sub-orchestrators/HARDENING.md` cycles.

**CH1-CH6 lens scheme** (defined inline in each pass contract) — universal
challenge-pass adversarial lenses. CH1 Correctness + CH2 Generality + CH3
Regression + CH4 Cost + CH5 Hidden Coupling + CH6 Next-Tranche-Impact.
Used by Pass Alpha + Pass Omega + Skinny Triumvirate CHALLENGE phases
for adversarial review of antecedent artefacts (not per-target audit).

The two schemes are complementary. A-K audits document content for
authorial pathologies + coverage gaps + architectural inconsistencies.
CH1-CH6 challenges intervention plans + synthesis artefacts for
falsifiability + regression risk + next-tranche impact.

## Sign-off gates

| Gate | Trigger | Authority |
|---|---|---|
| **G1** | Totality P1 (research-fold convergence) | user |
| **G2** | Totality P2 (hardening V{N}.1 convergence) | user |
| **G3** | Totality P3 (locks amendments proposed) | user |
| **G4** | MASTER-PLAN.md crystallised post-Omega | user |
| **G5(N)** | SK-V{N} implementation packet executed + measured | user per iteration |
| **G-Omega** | Pass Omega CRUD CONSOLIDATED | user |
| **G-Alpha(N→N+1)** | SK-V{N}'s Pass Alpha CHALLENGE CONSOLIDATED | user |

User sign-off is **mandatory** at G3, G-Omega, G-Alpha. The orchestrator
does not advance past these gates without explicit user confirmation.

## Iteration + auto-convergence governance

Each substantive pass iterates V1, V2, V3, … per `ORCHESTRATOR.md`
§5 cycle-naming + Pass Alpha §8 + Pass Omega §5 convergence rules.

**Convergence rule**: ≥95% ACCEPT on the most recent CHALLENGE pass +
zero open critical defects + no orphan unresolved REVISE.

**Hard ceiling**: V ≤ 5 per pass; > V5 without convergence escalates to
user with `BLOCKED` verdict.

**Hard ceiling**: 12 waves per SK-V{N} bracket; > 12 escalates.

## Non-negotiables (apply across all passes)

| Rule | Enforcement |
|---|---|
| No new BBNF directives | grep grammars/ + restart/skinny/ pre-/post-pass |
| No new BIR variant | grep ir/src/ pre-/post-pass |
| No new substrate | Lock 1 audit per CH5 per wave |
| No JSON code in generic crates | Lock 14 audit per CH2 per pass |
| Scalar reference per primitive | every SIMD/ASM primitive ships with scalar Rust ref + checkasm parity BEFORE wiring |
| Same-wave consumer | each redress commit lands hot-path caller; verified via samply symbol path |
| Profile-first prescription | no kernel intervention without fresh PC-level profile of NEW Track 1 baseline; hypothesis transfer between SK iterations forbidden |
| Strict-vs-strict comparisons | every comparator row matches strictness plane; permissive rows flaw-probe only |
| Triumvirate discipline | research → plan → redress in distinct commits |
| Hard cap per dispatch | every dispatch carries minute cap; at 0.9× commit, at cap halt |
| Same-row falsification gate | a kernel that does not lift a previously-named row is rejected; record in REDRESS with measurements |
| No deferrals | wave closes on measurement, not "future phase will fix it" |

## Phase glossary

- **CHALLENGE pass**: Adversarial review wave. CH1-CH6 lens agents take antecedent artefacts and produce dispositions (ACCEPT / REVISE / REJECT).
- **CRUD wave**: Document maintenance wave under Pass Omega + Alpha. Creates new sections, Reads for cohesion, Updates stale text, Deletes superseded artefacts.
- **v+1 fold**: After CHALLENGE closes, the original sub-agent author re-runs with the dispositions in hand and produces v+1 of the artefact.
- **Convergence criterion**: ≥95% ACCEPT on the most recent CHALLENGE + zero open critical defects + no orphan unresolved REVISE.
- **Triumvirate**: Research → Plan → Redress. Three distinct commits per wave. Research = read-only diagnosis. Plan = synthesis. Redress = implementation + measurement + REDRESS entry.
- **SK-V{N}**: Skinny iteration N. Audit dir at `restart/skinny/tranches/SK-V{N}-COHORT/`; master docs at `restart/skinny/tranches/{GRAND-SYNTHESIS,IMPLEMENTATION-PACKET,HANDOFF}-SK-V{N}.md`.
- **V{V}**: Pass iteration version within a single pass (V1, V2, V3, …). Auto-incremented per N-iteration governance.

## Dispatch invocation phrases

| Phrase | Action |
|---|---|
| `dispatch sk-v{N} W{w}` | Dispatch skinny wave W of iteration N per IMPLEMENTATION-PACKET §{wave-section} |
| `dispatch alpha SK-V{N}→SK-V{N+1}` | Dispatch Pass Alpha for SK-V{N+1} contract creation |
| `dispatch omega` | Dispatch Pass Omega for V1 spec cohesion + skinny fold-in |
| `dispatch hardening` | Dispatch hardening cycle (per sub-orchestrators/HARDENING.md) |
| `dispatch research-fold {topic}` | Dispatch research-fold (per sub-orchestrators/RESEARCH-FOLD.md) |
| `dispatch amendment {target}` | Dispatch amendment cycle (per sub-orchestrators/AMENDMENT-DISPATCH.md) |
| `status` | Orchestrator emits current pass + cycle + open dispositions |
| `pin {pass}` | User overrides current-pass identification |
| `abandon SK-V{N}` | Orchestrator nukes uncommitted SK-V{N} artefacts + restores SK-V{N-1} state |

## Repository layout (post-restructure)

```
restart/
├── README.md                              ← gestalt anchor (top-level)
├── ARCHITECTURE.md                        ← V1 spec
├── HANDOFF.md                             ← top-level state pointer
├── MASTER-PLAN.md                         ← H tranche + Pass Omega proposals
├── MIGRATION.md                           ← renames + abrogates
├── locks/
│   └── LOCKS.md                           ← Lock 1-16 (+ proposed Lock 17 per SK-V7)
├── prompts/                               ← THIS DIRECTORY
├── audit/                                 ← totality-track hardening cycles (V1-V9+)
├── research/                              ← totality-track research deep-dives
├── corpora/                               ← test corpora references
├── inheritance/                           ← pre-restart corpus (archive)
└── skinny/
    ├── BENCH.md
    ├── COMPILER.md
    ├── HARDENING.md                       ← skinny-scope hardening (Lenses L/M/N; composes by reference with audit-specs/HARDENING-LENS-SET.md)
    ├── INDEX.md
    ├── SUBSTRATE.md
    ├── WORKSPACE.md
    └── audit/
        ├── GRAND-SYNTHESIS-SK-V{N}.md     ← per-SK synthesis (current = SK-V7)
        ├── IMPLEMENTATION-PACKET-SK-V{N}.md
        ├── HANDOFF-SK-V{N}.md
        ├── SOTA-BEAT-DESIGN.md            ← cross-iteration design doc
        ├── SK-V{N}-COHORT/                ← per-SK cohort reports (current = SK-V7-COHORT/)
        └── V9.5-PSI-EXCAVATION/           ← historical excavation (proposed move to archive/sk-v3.5-psi/)
```

## The bbnf-lang specific axes

1. **Bench gate is empirical, not declarative.** Convergence requires `skinny/RESULTS.md` rows; "wired/complete" without bench citation is forbidden.
2. **Skinny ⊂ totality with feedback loop.** Pass Omega explicitly consumes skinny REDRESS to amend V1 locks. Skinny lessons drive totality evolution; not the reverse.
3. **Grammar generalisation is non-negotiable.** Lock 14 audit lives inside every CH2 lens; per-grammar variation lives in codegen-emitted .data + per-grammar wrapper dirs.

## Closing posture

The prompt suite is the durable orchestrator. The skinny + totality content evolves through iteration. The framework gives the iteration a shape — auto-convergent, challenge-hardened, telemetry-bound, gate-controlled.

No SK-V{N+1} dispatches without Pass Alpha goalset. No V1 spec amendment without Pass Omega cohesion verification. No new pass cycle without convergence on the prior. No commit merges triumvirate roles. No hypothesis transfer between SK iterations without fresh profile evidence.

The work is bounded by the gates. The throughput is bounded by the bench. The architecture is bounded by the locks. The discipline is the suite.
