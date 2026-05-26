# SK-V14 DISPATCH-PROMPT — Per-Wave Triumvirate Dispatch Contract

Date: 2026-05-23.

Status: S-P3 V1 dispatch contract, amended by Pass Omega V3 W2R on
2026-05-26. This file is the orchestrator's
per-wave invocation contract for SK-V14 W0..W11. Every SK-V14 wave is
dispatched as a research → plan → redress triumvirate per
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`. This contract
binds the per-wave dispatch envelope; the wave content is
`restart/skinny/tranches/sk-v14/SPEC.md §3-§14`.

## §0 — Authority + binding contracts

Read in order before dispatching any SK-V14 wave triumvirate:

1. `restart/skinny/tranches/sk-v14/SPEC.md` (the wave-sequenced contract this prompt dispatches; §0 close-condition + §1 non-negotiables + §2 wave manifest + §3-§14 per-wave sections + §15 pre-blocked routes + §16 G-Alpha)
2. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (Pass Alpha goalset; §0 close-condition + R1-R10 + P-1..P-7 + §4 S-P3 constraints)
3. `restart/skinny/tranches/sk-v14/HANDOFF.md` (tranche handoff)
4. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` (R-target acceptance criteria + the SK LOOP)
5. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (the wave-execution contract this prompt dispatches under)
6. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (the S-P3 contract that authored this dispatch prompt)
7. `restart/locks/LOCKS.md` (16 locks; Lock 1 v+1 + Lock 14 v+1 + Lock 16 v+1 load-bearing per SPEC §1)
8. `restart/skinny/tranches/sk-v14/research/p3/p3{a..f}-*.md` (S-P3 V1 outputs; CHALLENGE V1 input ledger)
9. `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (S-P2 §3Z LOCK + §6 carry-forward packets)
10. `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (74 findings; §2 sequencing constraints; §3 PRUNE-list)
11. `restart/audit/totality/astral/V3/G-OMEGA-PACKET.md` + `restart/audit/totality/astral/V3/hardening/CONSOLIDATED.md` (W2R amendment: W2 skinny-only; W6.0 root CSS L4).
12. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/ROLLING-SOTA-DELTA.md` (empirical floor)

## §1 — Per-wave triumvirate contract (research → plan → redress)

Every SK-V14 wave (W0..W11; W6 expanded into W6.0..W6.8 sub-waves)
follows the `SKINNY-TRIUMVIRATE.md §1` three-phase structure:

| Phase | Purpose | Agent count | Output | Source edits | Commit prefix |
|---|---|---|---|---|---|
| Research | Read-only profile-first diagnosis | up to 6 parallel | `research/skv14-{wave}{agent-id}-{topic}.md` per agent | NONE | `docs(sk-v14-wave{W}-research):` |
| Plan | Synthesis; select ONE intervention from research shortlist | 1-2 | `research/skv14-W{W}-plan.md` | NONE | `docs(sk-v14-wave{W}-plan):` |
| Redress | Implementation + measurement | 1 | source edits + bench rerun + REDRESS entry | YES (and only YES in this phase) | `feat(sk-v14-wave{W}):` on success / `docs(sk-v14-wave{W}-redress):` on failure |

Triumvirate role separation is load-bearing per
`SKINNY-TRIUMVIRATE.md §9`. The orchestrator MUST refuse to dispatch a
redress agent without an antecedent plan commit, and MUST refuse to
dispatch a plan agent without an antecedent research commit.
Same-commit role merger is REJECT.

### §1.1 Research phase

- Agents fan out per wave-specific scope rows (the wave plan from S-P3 + the SPEC §X "Tasks" section names the scope partition).
- Each agent reads: the current source + the most recent `RESULTS.md` + the most recent `REDRESS.md` + the SPEC §X for the wave + the SYNTHESIS §0-§4 binding + the dispatch context inheritance + the S-P2 §6 carry-forward packets.
- Each produces ONE artefact at `restart/skinny/tranches/sk-v14/research/skv14-{wave-letter}{agent-id}-{topic}.md` with frontmatter per `SKINNY-TRIUMVIRATE.md §1 Phase 1 Output schema`.
- Hard cap: 30 min per agent.

### §1.2 Plan phase

- 1-2 plan agents select ONE intervention from the research shortlist and author a single planning artefact at `restart/skinny/tranches/sk-v14/research/skv14-W{W}-plan.md`.
- Plan agents do NOT modify source code.
- Plan artefact carries: `Inputs:` (antecedent research with path:line), `Intervention:` (single sentence), `Owner paths:` (file paths the redress phase is authorised to touch — must MATCH SPEC §X owner paths or REVISE), `Falsifiability gate:` (named corpus rows + Mbps thresholds OR named correctness signals — MUST match SPEC §X exit gate or REVISE), `Hard cap:` (≤90 min), `Revert protocol:` (per SPEC §X revert protocol), `Same-wave consumer:` (the hot-path caller per S-P2 V3 §6.1 CF-3 3-gate cell: scalar-ref / checkasm-parity / same-wave-consumer NAMED), `Pre-blocked routes:` (REDRESS entries this wave MUST NOT re-open — must echo SPEC §X pre-blocked routes).
- Hard cap: 30 min.

### §1.3 Redress phase

- 1 redress agent (single implementation thread; avoids shared-file races).
- Implements the planned intervention per the plan's `Owner paths`.
- Measures against the falsifiability gate.
- Commits on success with bench rerun output + REDRESS entry.
- Reverts + records REDRESS entry on failure (with rejected patch saved at `/tmp/skv14-wave{W}-rejected.patch`).
- Hard cap: 60 min implementation + 15 min measurement = 75 min; 90 min hard ceiling.

## §2 — Phase caps and commit cadence

Per `SKINNY-TRIUMVIRATE.md §7` + `SPEC §2` phase caps:

| Phase | Hard cap | Commit type |
|---|---|---|
| Research (up to 6 parallel) | 30 min each (wall: 30 min) | `docs(sk-v14-wave{W}-research):` |
| CHALLENGE (optional, 6 parallel) | 60-90 min wall | `docs(sk-v14-wave{W}-challenge):` |
| Plan (1-2 agents) | 30 min | `docs(sk-v14-wave{W}-plan):` |
| Redress (1 agent) | 75 min (60 impl + 15 measure); 90 min ceiling | `feat(sk-v14-wave{W}):` or `docs(sk-v14-wave{W}-redress):` |
| **Wave total** | **~3-4 hours wall** | **3-4 commits per wave** |

Hard cap envelope per memory `[dispatch-hard-cap]`: every dispatch
carries "HARD CAP: N min. At 0.9N commit, at N halt"; defaults 30/30/75
for research/plan/redress. The orchestrator surfaces extension
decisions to the user.

W6 has 9 sub-waves W6.0..W6.8 × ~3 commits each = ~27 commits within
W6; aggregate wall-time ~24-32 hours. The SK-V14 bracket total wall-
time estimate: 12 waves × 3-4h + W6 × 9 sub-waves = ~50-70 hours
orchestrator-time per `SKINNY-TRIUMVIRATE.md §7`.

## §3 — Same-wave consumer mandate (load-bearing per `[no-deferrals]`)

Every redress commit that lands a primitive / kernel / new generated
path MUST include the hot-path caller that exercises it in the same
commit. The redress agent verifies:

1. Building the new primitive + scalar reference + checkasm parity (`BBNF_SIMD_STRICT=1`).
2. Wiring the consumer call site in the same commit.
3. Running the bench against the named falsifiability gate rows.
4. Confirming the consumer call shows in `samply` symbol path on the affected rows.

If the consumer wire-up is omitted: the primitive is an orphan kernel.
**REJECT** and record in REDRESS. No exception.

Per S-P2 V3 §6.1 CF-3 admission 3-gate cell: every shortlisted candidate's admission manifest carries:

- **scalar-ref status** (NAMED — path:line to scalar reference function body at HEAD; absence claims fail per CH7-V2 procedural addendum / `LOCKS.md` Lock 3 v+1 verification clause)
- **checkasm-parity expectation** (NAMED — `BBNF_SIMD_STRICT=1` command + expected parity disposition)
- **same-wave-consumer NAMED** (the exact hot-path call site in the same commit; per `SKINNY-TRIUMVIRATE.md §8`)

Per S-P2 V3 §6.3 F-V2-P1ABC-RERECORD Stage-0: any wave admitting any
of the 12 consumer-dependency primitives (P2-A C6 + P2-C C-P2C-3 +
C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13) MUST ship the
rerun in Stage 0 of the same wave: cargo build --release -p bbnf-bench
--features runtime/parse-attribution + interactive samply record
(NOT --save-only per `[samply-symbol-resolution]`) + cfg_attr flip
verification at `generated.rs:33-237` 8 sites (lines 33-34, 43-44,
58-59, 79-80, 86-87, 117-118, 138-139, 157-158; inline(always) →
inline(never)).

Per S-P2 V3 §6.2 §2.Y canonical-name binding: any wave admitting any
of the three convergent long-string-body SIMD scan identifiers
(`long_string_body_simd_scan` / `scan_string_special_block_sweep_64` /
quote-aware classifier composition) MUST commit to ONE canonical
primitive name + ONE canonical scalar-ref function — three orthogonal
SIMD bodies for one primitive is REJECT.

## §4 — Per-wave dispatch envelope

For each SK-V14 wave W ∈ {W0..W11}, the orchestrator's dispatch envelope is:

### §4.1 Pre-dispatch verification

Before dispatching the W{N} triumvirate:

1. Verify W{N-1} closed (admitted, rejected, or routed).
2. Verify W{N}'s entry gate per SPEC §X (W2 + W3 before W4; W5 + W6 before W7; W2+W3+W4+W5+W6+W7 before any new-admit W8/W9/W10 claim).
3. If W{N} is W3 or later, verify Pass Omega V3 W2R CRUD landed and W2 admitted under the amended skinny-only gate; otherwise stop with REVISE.
4. Verify W{N}'s SPEC §X owner paths, tasks, exit gate, revert protocol exist and are current.
5. Verify CHALLENGE acceptance if W{N} is first-of-class, substrate-touching, primitive, or high-risk (W5/W6/W7 mandatory; W1/W2/W4/W8/W9/W10 first-of-class recommendation).

### §4.2 Research dispatch

```
Dispatch: SK-V14 W{N} RESEARCH
Contract: SPEC §X tasks + SYNTHESIS §0-§4 + dispatch-context §1-§3
Agents: up to 6 parallel; scope rows per W{N} wave plan
Cap: 30 min per agent
Output: research/skv14-{wave-letter}{agent-id}-{topic}.md per agent
Commit: docs(sk-v14-wave{W}-research): archive {scope} cohort reports
```

### §4.3 CHALLENGE dispatch (optional but recommended; mandatory for substrate-touching waves W5/W6/W7)

```
Dispatch: SK-V14 W{N} CHALLENGE V{cycle}
Contract: CH1-CH6 + CH7 (Overfit-Prune) per ORCHESTRATOR.md §3W + PASS-0-OVERFIT-AUDIT.md §CH7
Agents: 7 parallel (one per lens) + 1 aggregator
Cap: 60-90 min wall
Output: research/skv14-wave{W}-challenge/V{cycle}/CH{n}.md + HARDENING-SKV14-W{W}-V{cycle}-CONSOLIDATED.md
Commit: docs(sk-v14-wave{W}-challenge): {disposition summary}
```

Per `SKINNY-TRIUMVIRATE.md §4` lens specialisations:

- CH1 Correctness: does the plan cite file:line for every claim? Is the falsifiability gate measurable?
- CH2 Generality: does the intervention respect Lock 14 v+1? Does it generalise to non-JSON grammars?
- CH3 Regression: does the plan re-open a REDRESS entry from the SPEC §15 watch-list?
- CH4 Cost: is the LOC budget realistic? Is the hard cap appropriate? Per S-P2 V3 §6.1 CF-3 3-gate cell?
- CH5 Hidden coupling: does the plan introduce parallel substrate, sidecar producer, Track 1 ≡ Track 2 dishonesty? Lock 1 v+1 triad declared?
- CH6 Anti-paper-close: does the plan specify revert protocol + same-wave consumer + present-future verb tense for NOT-PRESENT path:line per CH7-V2 procedural addendum?
- CH7 Overfit-Prune: does the plan re-open any P-1..P-7 pattern per SYNTHESIS §0.4? Does the wave plan admit a fixture-lookup, fake `@generated` header, or gate-relabel as admit?

Convergence: ≥95% ACCEPT × 2 cycles + zero orphan REVISE; V ≤ 5
ceiling per `PASS-3-SYNTHESIS-PLAN.md §4`.

### §4.4 Plan dispatch

```
Dispatch: SK-V14 W{N} PLAN
Contract: SPEC §X owner paths/tasks/exit gate + CHALLENGE V{final} dispositions
Agents: 1-2
Cap: 30 min
Output: research/skv14-W{W}-plan.md
Commit: docs(sk-v14-wave{W}-plan): select {intervention-name}
```

### §4.5 Redress dispatch

```
Dispatch: SK-V14 W{N} REDRESS
Contract: skv14-W{W}-plan.md + SPEC §X revert protocol + SPEC §1 non-negotiables + CF-3 3-gate cell + F-V2-P1ABC-RERECORD Stage-0 if applicable
Agent: 1 (single implementation thread)
Cap: 60 impl + 15 measure = 75 min; 90 min ceiling
Output: source edits + bench rerun output + REDRESS entry per SKINNY-TRIUMVIRATE.md §1 Phase 3
Commit: feat(sk-v14-wave{W}): admit {intervention-name}  [on success]
        docs(sk-v14-wave{W}-redress): reject {intervention-name}  [on failure]
```

### §4.6 Post-redress verification

After the redress commit:

1. Verify SPEC §X exit gate (per-row Mbps thresholds + correctness gates + Lock 14 baseline + Lock 1 triad if SIMD/union consumer + audit_overlay_verdict transition).
2. Verify `xtask gate-json` passes the full row schema per SPEC §0.4.
3. Verify the SPEC §X same-wave consumer is wired in the same commit (per §3 above).
4. Verify the F-V2-P1ABC-RERECORD packet shipped as Stage 0 if any consumer-dependency primitive admitted.
5. Update `RESULTS.md` + `ROLLING-SOTA-DELTA.md` + `REDRESS.md` + `HANDOFF.md`.

## §5 — CHALLENGE invocation discipline (when to invoke between phases)

CHALLENGE may be interposed:

- Between Research and Plan: when the research surfaces multiple parallel-safe interventions or a previously-unrecognised failure mode.
- Between Plan and Redress: when the intervention is first-of-class, substrate-touching, primitive, or high-risk.

Per `SKINNY-TRIUMVIRATE.md §4`:

- **Mandatory** for: W5 (PRUNE-3 Lock-14 refactor — substrate-touching), W6 (PRUNE-4 per-grammar runtime collapse — substrate-touching), W7 (PRUNE-5 W8+W9 wire-up — first-of-class CSP-shape consumer), any wave admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, any wave admitting the three convergent long-string-body SIMD scan identifiers per S-P2 V3 §6.2.
- **Recommended** for: W0 (telemetry schema first-of-class), W1 (comparator rebind + per-iter equality oracle — bench-harness substrate), W4 (PRUNE-2 — high-risk delete cluster), W8 (CSS L4 re-admit — first-of-class production-corpus admit), W10 (parse_only distinct path — first-of-class).
- **Optional** for: W2 (R4 regen-css; mechanical refactor with clear contract), W3 (R5 corpora; data-staging), W9 (JSON direct/typed re-admit; well-understood pattern post-W1), W11 (close; documentary).

CHALLENGE skip carries auditable orchestrator rationale in the wave's
plan commit.

## §6 — Failure modes and escalation

### §6.1 Round-trip rule trigger (SYNTHESIS §0.4 P-1 binding)

Any second-in-tranche reopen of W10.3 `nested_layout` requires user
re-pin with intrinsic-block evidence per SYNTHESIS §0.4 P-1. Any
future CSS feature whose claimed Mbps exceeds the same-plane SOTA
comparator by ≥50× inherits the same trigger. The orchestrator
escalates immediately to user.

### §6.2 Abrogate-before-patch (per `[abrogate-before-patch]`)

For intrinsic-failure subsystems, the orchestrator asks "can we delete?"
before "can we patch?". Any row family whose REDRESS history shows
two-or-more reopen attempts against the same fake-pattern DELETEs
rather than patches.

### §6.3 No-orphan-redress (per `[no-orphan-redress]`)

Every wave's exit gate carries named corpus rows + Mbps thresholds;
every primitive has same-wave consumer NAMED; W0 = baseline +
telemetry lock; ≤8 candidates per P3-A; ≤12 waves per P3-B (the SK-V14
SPEC manifest is exactly 12 waves W0..W11).

### §6.4 Bracket-exceeded escalation

If the SK-V14 bracket exceeds 12 waves without convergence, the
orchestrator escalates immediately per `SKINNY-TRIUMVIRATE.md §3`:
`BLOCKED: skinny bracket V14 exceeded 12 waves; user adjudicate scope
or abandon`.

### §6.5 Architectural-block proof acceptance

Per SYNTHESIS §0.1: any row whose intrinsic-block proof is recorded
counts as close-condition-satisfied. The orchestrator escalates to
user for any architectural-block-proof acceptance per
ORCHESTRATOR-PROMPT.md:189.

### §6.6 Comparator rebind discovers new misnaming pattern

If W1 (or any subsequent wave) discovers a new comparator misnaming
pattern beyond the four currently documented (sonic_rs::from_slice
single-lane fan-out; eager DOM vs strict struct deser; per-corpus
typed misbinding; lightningcss fact-stream vs full-AST asymmetry), the
orchestrator escalates immediately per ORCHESTRATOR-PROMPT.md:192.

### §6.7 New Lock 14 violation introduced by a wave

If a wave's redress introduces a new Lock 14 violation (grammar-name
branch in generic crate; per-grammar feature flag; hand-written
per-grammar runtime file post-W6), the orchestrator escalates
immediately per ORCHESTRATOR-PROMPT.md:193.

## §7 — Status tick cadence (per `[status-tick-cadence]`)

Emit one-line status tick every ~5 min of orchestrator-silent wait;
never make user ask status twice. Reconcile TaskList vs ps + JSONL
mtimes before every user-facing status reply per
`[reconcile-task-census]`; zombies are frequent.

Status tick format:

```
[sk-v14 W{N} {phase}] status: {agent-count} agents running; {wall-time}
elapsed of {cap} cap; {commit-count} commits landed since {anchor};
next decision: {decision}.
```

## §8 — Dispatch ledger output

After every wave triumvirate closes, the orchestrator records:

```
SK-V14 W{N} {wave-name}:
  Research commit: {sha} ({author}; {timestamp})
  Plan commit: {sha} ({author}; {timestamp})
  CHALLENGE commit: {sha} ({author}; {timestamp})  [if dispatched]
  Redress commit: {sha} ({author}; {timestamp})
  Disposition: {ADMITTED | REJECTED | ROUTED}
  Falsifiability gate: {met | missed | partially met}
  Same-wave consumer: {named consumer path:line at HEAD}
  Lock 1 triad: {substrate_target / retention_lifetime / policy_owner if SIMD/union/cost-shape}
  F-V2-P1ABC-RERECORD: {shipped Stage-0 | N/A for this wave}
  Rows admitted: {row-keys}
  Rows reverted: {row-keys}
  REDRESS entries: {ids}
  HANDOFF next-move: {ready-for-wave-W{N+1} | blocked-on-{condition} | bracket-closed}
```

The ledger writes to a wave-close artefact at
`restart/skinny/tranches/sk-v14/research/skv14-W{W}-close.md` and the
HANDOFF.md next-move line updates atomically.

## §9 — Post-bracket disposition

When all 12 waves W0..W11 close (admitted, rejected, or routed):

1. The orchestrator dispatches Pass Alpha per `restart/prompts/pass-contracts/PASS-ALPHA.md §1` to bracket SK-V14 → SK-V15.
2. Pass Alpha cohort + CHALLENGE produces the SK-V15 SYNTHESIS + HANDOFF + close-state row enumeration.
3. The indefatigability clause (SYNTHESIS §6) carries: if any goal remains unmet without architectural-block proof, SK-V15 brackets immediately under the same pinned bar.
4. G-Alpha(N → N+1) user sign-off gates the SK-V15 bracket; the orchestrator does not relinquish control between G-Omega closes EXCEPT at G-Alpha per `ORCHESTRATOR-PROMPT.md:204`.

## §10 — Closing posture

This dispatch prompt is the per-wave invocation contract. The SPEC is
the wave-sequenced implementation contract. The triumvirate is the
discipline. CHALLENGE is the integrity check. The bench is the truth
signal.

No primitive ships without scalar reference. No primitive ships without
checkasm parity. No primitive ships without same-wave consumer. No wave
ships without falsifiability gate. No wave ships without revert
protocol. No wave closes on a future-phase promise. No row admits
without per-iter equality oracle inside the timing region. No JSON row
admits without one of the three R1 plane-correct strict comparators.
No CSS L4 row admits without grammar-derived emission + production
corpora + lightningcss/cssparser work-equivalent comparator.

The campaign is indefatigable. SK-V14 is one bracket in a campaign
that closes only at full ADMIT or per-row architectural-block proof
across the 51 JSON cells + 24 CSS L4 features.
