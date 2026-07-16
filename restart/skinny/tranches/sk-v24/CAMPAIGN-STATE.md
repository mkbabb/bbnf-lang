# SK-V24 CAMPAIGN STATE — durable resurrection ledger

THE source of truth for campaign progress. Updated after EVERY stage transition and workflow
launch/completion. Any session — this one, a cron heartbeat, or a cold-started successor —
resurrects the campaign from this file alone.

## Resurrection protocol

1. Read this file, HANDOFF.md (§1.5 amendments bind), and ROUND-ZERO.md.
2. Reconcile: TaskList (#55 is the campaign task; metadata carries live runIds) vs the
   workflow table below. A runId listed RUNNING here but absent from live tasks = died at a
   wall (rate limit / session limit / crash).
3. Resurrect a dead workflow with Workflow({scriptPath, resumeFromRunId}) — completed agents
   replay free from the journal cache; agent prompts must stay byte-identical. Before
   resuming, TaskStop any zombie duplicate.
4. NEVER duplicate-launch: check live tasks first; one workflow per stage row below.
5. On session-limit walls: do not retry hot; the heartbeat cron re-fires hourly — resume then.
6. Stage complete + next not launched → launch next per the stage plan; update this file
   FIRST (launch intent), then launch, then update with the runId.

## Stage plan

| Stage | What | Status |
|-------|------|--------|
| 0 | Setup: charter §1.5, ROUND-ZERO, state file, heartbeat cron, memory | DONE 2026-07-16 |
| 1 | RECON: parallel cartography fleet (both repos, harnesses, reusable prototypes, SOTA sweep) → research/R1-R7 | DONE 2026-07-16 (wf_954e831e-a1c, 7/7, 731K tok) |
| 2 | PROVING: the PROVE-01..14 implementation fleet (worktree/scratch isolated, measured, cp-out) → prototypes/PROVE-XX/ | DONE 2026-07-16 (wf_386900ed-462, 14/14: 6 GO / 7 PARTIAL / 1 NO_GO; artifacts committed f6004995a) |
| 3 | VERIFY: adversarial re-measure of every claimed win; unproven dies | RUNNING wf_a4c300b0-b54 (12 verifiers; 02 NO_GO + 04 refutation accepted without re-litigation) |
| 4 | SPEC: the frozen codex (tranches/waves both repos, colocation edict normative, archival last, letter + checkboxes re-cut) + final report | BLOCKED ON 3 |

## Workflow ledger (append per launch; mark DONE/DEAD; keep scriptPaths exact)

| Stage | runId | scriptPath | Status |
|-------|-------|-----------|--------|
| 1-recon | wf_954e831e-a1c | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-recon-wf_954e831e-a1c.js | DONE 7/7 |

Recon deltas folded into the manifest: real workspace = repo-root skinny/ (restart/skinny is
docs); json_canon_bench.rs rescued from worktree wf_e940452a-431-7 into
skinny/crates/bbnf-bench/src/bin/ (was untracked, never committed); tape is SoA offset-style
at HEAD (not AoS); shufti/truffle NEON source already vendored in legacy root
crates/simd-scan/src/neon.rs — port, don't re-derive; M-series settled NEON-only (no
SVE/SVE2/SME for byte work); JSON grammar has ZERO -> projections (PROVE-08 adds them);
direct-to-struct emitter exists but schema is hand-authored (PROVE-08 derives it); all prior
prototype CODE is wiped — re-author from report inline diffs; PROVE-07 must be the fused
single-pass sink (fold-over-cursor refuted 2.3-4.9x).

| 2-proving | wf_386900ed-462 | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-proving-fleet-wf_386900ed-462.js | DONE 14/14 (36 min, 2.84M tok); prover worktrees preserved at .claude/worktrees/wf_386900ed-462-N |
| 3-verify | wf_a4c300b0-b54 | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-verify-fleet-wf_a4c300b0-b54.js | RUNNING (launched 2026-07-16; 12 adversarial verifiers, 45-min caps) |

Heartbeat cron live: job fc81e24f, hourly at :23, session-scoped.

## Heartbeat cron

Hourly, off-minute, session-scoped (7-day auto-expire; re-create on session restart as part
of resurrection). Duty: reconcile-and-resume per the protocol above; SILENT no-op when all
green — no spam. If this session is gone, a fresh session executing task #55 IS the heartbeat.

## Standing walls log (append incidents: what died, when, how resumed)

- (none yet)
