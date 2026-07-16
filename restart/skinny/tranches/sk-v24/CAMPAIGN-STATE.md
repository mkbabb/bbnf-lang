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
| 1 | RECON: parallel cartography fleet (both repos, harnesses, reusable prototypes, SOTA sweep) → research/R1-R7 | PENDING LAUNCH |
| 2 | PROVING: the PROVE-01..14 implementation fleet (worktree/scratch isolated, measured, cp-out) → prototypes/PROVE-XX/ | BLOCKED ON 1 |
| 3 | VERIFY: adversarial re-measure of every claimed win; unproven dies | BLOCKED ON 2 |
| 4 | SPEC: the frozen codex (tranches/waves both repos, colocation edict normative, archival last, letter + checkboxes re-cut) + final report | BLOCKED ON 3 |

## Workflow ledger (append per launch; mark DONE/DEAD; keep scriptPaths exact)

| Stage | runId | scriptPath | Status |
|-------|-------|-----------|--------|
| — | — | — | — |

## Heartbeat cron

Hourly, off-minute, session-scoped (7-day auto-expire; re-create on session restart as part
of resurrection). Duty: reconcile-and-resume per the protocol above; SILENT no-op when all
green — no spam. If this session is gone, a fresh session executing task #55 IS the heartbeat.

## Standing walls log (append incidents: what died, when, how resumed)

- (none yet)
