# SK-V7 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V7. It binds to the full wave plan at `SPEC.md` and follows the
precepts/tranche formalism documented at
`docs/precepts/instructions/tranche/`.

Lifecycle per precepts:
`research → challenge → plan → wave spec → implementation → doc update/close`.
Each wave executes one full triumvirate cycle.

## Required reading (in order)

1. `docs/precepts/instructions/README.md` (core rules) + `STYLE.md` (prose register).
2. `docs/precepts/instructions/ORCHESTRATION.md` (dispatch, work isolation, verification).
3. `docs/precepts/instructions/tranche/README.md` + `SPEC.md` (tranche lifecycle).
4. `restart/skinny/tranches/sk-v7/HANDOFF.md` (entry state + pre-blocked routes).
5. `restart/skinny/tranches/sk-v7/SPEC.md` (the wave plan; sections §0-§14 carry all wave specs).
6. `restart/skinny/tranches/sk-v7/SYNTHESIS.md` (the corrected diagnosis + cohort findings).
7. `restart/prompts/README.md` + `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (per-wave triumvirate contract).
8. `skinny/RESULTS.md` (current gate authority) + `skinny/REDRESS.md` (rejected-route ledger).

## Wave manifest

Eleven sequential waves. Specifications live in `SPEC.md` at the cited
section. Do NOT re-copy wave content into commit messages; cite the
section.

| Wave | SPEC § | Title | Hard cap |
|---|---|---|---:|
| W0 | §2 | Comparator-plane repair (sonic-rs strict rebuild) | 75 min |
| W1 | §3 | TapeKind rename (Lock 14 lowest-risk) | 90 min |
| W2 | §4 | B5b Eisel-Lemire mantissa widen | 105 min |
| W3 | §5 | B5 mesh DirectBuild + DirectTypeRef::Vec specialisation | 165 min |
| W4 | §6 | B1 per-`\uXXXX` TBL classifier | 125 min |
| W5 | §7 | B2 NEON 16-byte plain-string scan widening | 165 min |
| W6 | §8 | B6 control/key compaction | 165 min |
| W7 | §9 | Lock 14 Phase A + B (parse-that-regex + passes) | 240 min |
| W8 | §10 | Lock 14 Phase C + D (codegen + ir) | 360 min |
| W9 | §11 | CostFacts substrate | 360 min |
| W10 | §12 | bbnf.asm body fills (PMULL + CSSC CTZ) + B6 hardening Stage 1 | 240 min |

Total wall-clock: ~38 hours across 11 waves; ~33 commits via triumvirate
(3 per wave: research + plan + redress).

## Per-wave dispatch protocol

For each wave, execute the triumvirate per `SKINNY-TRIUMVIRATE.md`:

**Phase 1 — Research** (read-only diagnosis):
- Read `SPEC.md §{N}` for owner paths + scope.
- Dispatch 1-6 parallel research sub-agents (count per `SPEC.md §{N}`).
- Each produces ONE artefact at `tranche/sk-v7/research/wave-{N}-{a}-{topic}.md`.
- Hard cap 30 min per agent.
- Commit: `docs(sk-v7-wave{N}-research): archive {scope} cohort reports`.

**Phase 2 — Plan** (synthesis):
- Read research outputs.
- Author ONE plan artefact at `tranche/sk-v7/research/wave-{N}-plan.md`.
- Plan carries: intervention name, owner paths, falsifiability gate (per
  `SPEC.md §{N}` exit gate), hard cap, revert protocol, same-wave consumer
  declaration, pre-blocked routes (cited from `HANDOFF.md §3`).
- Hard cap 30 min.
- Commit: `docs(sk-v7-wave{N}-plan): select {intervention-name}`.

**Phase 3 — Redress** (implementation + measurement):
- Implement the planned intervention against owner paths.
- Run `cargo test --workspace` (or per `SPEC.md §{N}` test directive).
- Run the relevant bench subset (`cargo bench` or `cargo run -p xtask --release -- gate-json`).
- Verify falsifiability gate per `SPEC.md §{N}`:
  - **PASS**: commit `feat(sk-v7-wave{N}): admit {intervention-name}` with measurement table in body + REDRESS entry.
  - **FAIL**: revert via `git stash` or patch save to `/tmp/skv7-wave-{N}-rejected.patch`, commit `docs(sk-v7-wave{N}-redress): reject {intervention-name}` with REDRESS entry naming the failure mode + the next candidate shape.
- Hard cap 75 min total (60 implement + 15 measure).

## Falsifiability gates (referenced; not re-stated here)

Per-wave falsifiability gates are specified in `SPEC.md §{N}` under each
wave's "Exit gate" or "Falsifiability gate" subsection. Do not advance
to W{N+1} until W{N}'s gate is met OR the wave is REJECTED with
measurement evidence in `skinny/REDRESS.md`.

## Pre-blocked routes (referenced; do not re-open)

See `HANDOFF.md §3 "Pre-Blocked Routes"`. The full REDRESS ledger
(entries 1-76 across SK-V1 through SK-V6) names every previously-
rejected intervention shape. Do NOT re-open any listed entry without
new measurement evidence that contradicts the original rejection.

Key clusters:
- REDRESS 28+33: Class A `match_tiny_plain_string` wiring as parse-G fix (twice-rejected).
- REDRESS 50-55: 5 SK-V5 UTF-8 fusion routes.
- REDRESS 60-72: 7 SK-V6 retained-parse + direct-materialization routes.

## Non-negotiables

Per `restart/prompts/README.md §Non-negotiables`. The orchestrator-agent
enforces:

- No new BBNF directives; no new BIR variant; no new substrate.
- No JSON code in generic crates (Lock 14 audit per CH2 lens).
- Scalar reference per primitive; checkasm parity BEFORE wiring.
- Same-wave consumer (no orphan kernels).
- Profile-first prescription (no hypothesis transfer from prior SK).
- Strict-vs-strict comparator gate (permissive rows are flaw-probe only).
- Triumvirate role separation (research/plan/redress = distinct commits).
- Hard cap per dispatch (at 0.9× commit, at cap halt).
- Same-row falsification gate (no orphan REDRESS).
- No deferrals (wave closes on measurement, not future-phase promise).

## Status discipline

Emit one-line status tick every ~5 min of orchestrator-silent wait
per `restart/prompts/ORCHESTRATOR.md §11`. Format:

```
[sk-v7-W{N}] {phase}: {N} agents in flight; {M} returned; ETA {time}
```

Reconcile TaskList vs ps + JSONL mtimes before every user-facing status
reply. Zombies are frequent; verify before reporting "still running".

## Convergence + escalation

A wave converges when its falsifiability gate is met (admit) OR the
intervention is rejected with measurement (reject). The cycle converges
when all 11 waves have closed.

Escalate to user with `BLOCKED` if:
- A wave has 3 successive rejection cycles without a viable next candidate.
- Total wave count exceeds 15 (3 sub-cycles past W10).
- An admit commit regresses a previously-passing row by >5% (per
  `SPEC.md §0.1` per-row close conditions + the maintain invariant).

## Sub-wave naming

If a wave splits into sub-cycles (W1, W1b, W1c per SK-V6 precedent), the
sub-cycle artefacts use `wave-{N}{letter}-*.md` (e.g.
`wave-1b-research-*.md`). Each sub-cycle is a fresh triumvirate; the
plan commit names the candidate revision and references the rejected
predecessor.

## After W10 closes (SK-V7 close)

Dispatch Pass Alpha for SK-V7 → SK-V8 per
`restart/prompts/pass-contracts/PASS-ALPHA.md`. Pass Alpha produces
`restart/skinny/tranches/sk-v8/{SYNTHESIS, SPEC, HANDOFF, DISPATCH-PROMPT}.md`
+ `research/` cohort. G-Alpha user sign-off required before SK-V8
dispatch.

## Pass Omega trigger candidates

Per `SYNTHESIS.md §9` + cohort R4 finding (Lock 17 recommendation),
the following are queued for Pass Omega CRUD after SK-V7 close:

- Lock 17 amendment: bench-honesty + comparator-plane strictness.
- ~220 cross-file path-swap cleanup (audit/ → tranche/ migration debt).
- Top-level CRUD (README + HANDOFF + ARCH + MASTER-PLAN + MIGRATION).
- Skinny canonical surface refresh (BENCH/COMPILER/INDEX/SUBSTRATE/WORKSPACE).
- 73-entry dead-link cleanup (per cohort R6).
- 5-section duplicate consolidation (BackendShape in 40 files; etc).

These are NOT SK-V7 wave work. Dispatch via `dispatch omega` after
SK-V7 closes.

## Commit conventions (referenced; per `SKINNY-TRIUMVIRATE.md §7`)

| Phase | Commit prefix | Body shape |
|---|---|---|
| Research | `docs(sk-v7-wave{N}-research):` | Inventory of scope + cohort reports archived + first findings |
| Plan | `docs(sk-v7-wave{N}-plan):` | Intervention name + owner paths + falsifiability gate + revert protocol |
| Redress admit | `feat(sk-v7-wave{N}):` | Source edits + before/after Mbps table + REDRESS entry # |
| Redress reject | `docs(sk-v7-wave{N}-redress):` | Failure mode + measurement + next candidate + REDRESS entry # |

## Entry condition (NOW)

The current state is:
- All SK-V7 master docs are at `restart/skinny/tranches/sk-v7/` per precepts/tranche layout.
- SK-V7 cohort (18 reports + 6 restructure reports) is at `tranche/sk-v7/research/`.
- SK-V5, SK-V6, SK-V3.5 are archived to sibling `tranche/sk-v{5,6,3.5}/` dirs.
- 16 architectural locks at `restart/locks/LOCKS.md` (formerly `14-LOCKS.md`).
- 4-subdir prompt suite at `restart/prompts/` (sub-orchestrators/, pass-contracts/, audit-specs/).
- ~220 cross-file path references are broken pending Pass Omega CRUD; non-blocking for W0.

The IMPLEMENTATION-AGENT, on receiving this prompt:

1. Confirms it has read all required-reading documents.
2. Declares the wave it will execute next (default: W0).
3. Dispatches the triumvirate for that wave per Phase 1/2/3 above.
4. Reports back at each commit landing.
5. Advances to W{N+1} after W{N} closes.

## Dispatch

**Begin SK-V7 W0 now**. SPEC §2 carries the full W0 specification:
- Edit `skinny/crates/bbnf-bench/Cargo.toml:21` to remove `, "utf8_lossy"`.
- Verify with `cargo tree -p bbnf-bench --edges=features | grep sonic-rs`.
- Run `cargo bench -p bbnf-bench --bench json_parity`.
- Run `cargo run -p bbnf-bench --bin gate --release` to refresh RESULTS.md.
- Author `tranche/sk-v7/research/wave-0-strict-baseline.md` documenting
  per-row Mbps delta + outcome reclassification + schema v3 column
  population per SPEC §2.4 falsifiability gate.
- Commit `feat(sk-v7-wave0): comparator-plane repair (sonic-rs strict)`.

After W0 commits and the falsifiability gate is verified, advance to W1
per SPEC §3. Repeat until W10 closes OR a wave is rejected with measurement
evidence + named successor candidate. The implementation agent does NOT
require user supervision between waves unless escalating per
§Convergence + escalation.

The work is bounded by the SPEC. The cadence is bounded by the hard caps.
The discipline is the triumvirate.

End of dispatch contract.
