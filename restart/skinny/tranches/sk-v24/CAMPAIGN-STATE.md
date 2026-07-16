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
| 3 | VERIFY: adversarial re-measure of every claimed win; unproven dies | DONE 2026-07-16 (wf_a4c300b0-b54, 12/12: 7 CONFIRMED, 3 CONFIRMED_DELTA, 1 REFUTED [09 perf], 1 UNVERIFIABLE-perf [13, generality verified]) |
| 4 | SPEC: the frozen codex + final report | DONE 2026-07-16 — SPEC.md FROZEN (GN-0..6 + PT-N1-3 + BBNF-PT-G letter + 3 re-cut checkboxes + forward prompt) |

| 5 | TRANCHE CODEX AUTHORSHIP (owner order 2026-07-16): 10 implementation-depth tranche docs at docs/tranches/sk-v24/ (GN-0..6 + PT-1..3, parse-that elevated to FULL IMPLEMENTATION; totality framing — THE implementation, not a skinny pass; prototype residue = defect) + 3-auditor precept sweep + fix stage + INDEX.md | RUNNING wf_6ae8f4f7-16c (restarted once at T+1min to bake the owner totality edict into all briefs) |

| 6 | THE LOCKS (owner order 2026-07-16, multi-day): the convergent multiagent design loop applied per core lock — LOCK-1 solver spine (type inference/solving), LOCK-2 graph engine (egraph+CSP global/local), LOCK-3 IR transposition spine, LOCK-4 combinator core, LOCK-5 regex continuum (grammar-driven, bootstrapped, HIR-isomorphic), LOCK-6 rust/ts isomorphism plane, LOCK-7 grammar totality & bootstrap — ≥3 passes each, then THE GESTALT (both repos) → the perfected tranche set with DAG ordering. GOAL SET (owner): perfected tranches pursuant to precepts from perfected designs — novel, no x86, built for our use case, perfected idiomatic Rust, grammar generality, deep CSP/egraph optimization, sophisticated typing inference and solving, parse-that PRIMACY. Round zero: locks/PORTFOLIO.md. Pass workflows in the ledger below; registries at locks/LOCK-N/REGISTRY.md. Pass-transition duty: the ORCHESTRATOR reviews registries on each pass-round completion and launches the next round with refined directives (researchers never see registries — withholding law); the heartbeat only resurrects. | PASS 1 LAUNCHING 2026-07-16 |

## CAMPAIGN CLOSED 2026-07-16 (stages 0-4) — REOPENED: stage 5 (codex authorship, DONE pending final commit) + stage 6 (THE LOCKS, multi-day, RUNNING)

The swath: 7 CONFIRMED + 3 CONFIRMED_DELTA performance-proven features; 3 measured
refutations now law (02 latency inversion, 04 pipelining, 09 shim perf). The path: SPEC.md
GN-0→GN-6 + PT-N1-3. Heartbeat cron retired at close. Next acts are user-owned: §3
checkboxes + BBNF-PT-G placement (task #53), then execution dispatch (task #54, SPEC §5).

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
| 3-verify | wf_a4c300b0-b54 | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-verify-fleet-wf_a4c300b0-b54.js | DONE 12/12 (14 min, 1.06M tok) |

| 5-codex | wf_6ae8f4f7-16c | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-codex-authorship-wf_6ae8f4f7-16c.js | RUNNING (Author 10 → Audit 3 → Fix; 60/30/25-min caps) |

Heartbeat cron live: job 20e19341 (re-armed for stage 5), hourly at :23, session-scoped
(fc81e24f retired at stage-4 close).

## Heartbeat cron

Hourly, off-minute, session-scoped (7-day auto-expire; re-create on session restart as part
of resurrection). Duty: reconcile-and-resume per the protocol above; SILENT no-op when all
green — no spam. If this session is gone, a fresh session executing task #55 IS the heartbeat.

## Standing walls log (append incidents: what died, when, how resumed)

- (none yet)
