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

| 5-codex | wf_6ae8f4f7-16c | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-codex-authorship-wf_6ae8f4f7-16c.js | DONE 28/29 (32 min, 3.25M tok; GN-6 author died post-write — file complete on disk; 31 defects → all CRITICAL/MAJOR fixed; minors harmonization agent running; codex commit pending its return) |
| 6-locks-p1-bbnf | wf_63419230-36d | ~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/sk-v24-locks-pass-wf_19b300e6-c0a.js (args: group bbnf, locks [1,2,3,7], pass 1) | RESUMED (locks 1/3/7 FOLDED — registries on disk; LOCK-2 synth died at retry cap after writing FAMILY-SPECS; resume replays 1/3/7 cached, re-runs 2) |

| 6-locks-p2-pt | wf_a6d7a39f-623 | same script (args: group pt, locks [4,5,6,8,9], pass 2, baseCommit 7cdbff4ef) | DONE 5/5 (post-wall resume; 6.8M tok) — LOCK-5 AXIS VERDICT rendered (two domain-fit types + cheap bridge; 5B RETIRED); LOCK-4 TS runtime decoders refuted as fast face; LOCK-8 62-68 all four; LOCK-9 9B 62 |
| 6-locks-p1-bbnf-close | wf_63419230-36d (resume 3) | same script (args: group bbnf, locks [1,2,3,7], pass 1) | DONE — all four registries live (LOCK-3 re-fold died again at retry cap but run-1 registry stands; treated as folded) |
| 6-locks-p2-bbnf | wf_fce2ec2b-53a | same script (args: group bbnf, locks [1,2,3,7], pass 2, baseCommit ce51c36fc) | DEAD-AT-WALL 10:50 ET (15/33 done; all research + distills on disk; L1 synth survived; L2/3/7 synths + all L1 protos/crits/agg lost) — RESUME INTENT below |
| 6-locks-p3-pt | wf_acfc23d2-04b | same script (args: group pt, locks [4,5,6,8,9], pass 3, baseCommit ce51c36fc) | DEAD-AT-WALL 10:50 ET (27/35 done; all research + distills on disk; L9/L8 FAMILY-SPECS on disk; all 5 synth returns lost) — RESUME INTENT below |
| 6-locks-p2-bbnf (resume) | wf_fce2ec2b-53a (task wf4ls5wqq) | same script, resumeFromRunId wf_fce2ec2b-53a, args EXACTLY as ledgered | DEAD 14:05 ET at the process-exit wall (44 journal results banked) |
| 6-locks-p3-pt (resume) | wf_acfc23d2-04b (task w6d157m32) | same script, resumeFromRunId wf_acfc23d2-04b, args EXACTLY as ledgered | DEAD 14:05 ET at the process-exit wall (58 journal results banked) |
| 6-locks-p2-bbnf (resume 2) | wf_fce2ec2b-53a (task w2ac3wlqt) | same script, resumeFromRunId wf_fce2ec2b-53a, args EXACTLY as ledgered | RUNNING 14:22 ET (width-3; 44 cached replay; NOTE: TaskOutput keys on task ID, not runId — a runId miss is NOT proof of death; check task ID + agent-JSONL mtimes before declaring a wall) |
| 6-locks-p3-pt (resume 2) | wf_acfc23d2-04b (task wtjdi7mfm) | same script, resumeFromRunId wf_acfc23d2-04b, args EXACTLY as ledgered | RUNNING 14:22 ET (width-3; 58 cached replay) |

PASS-2 BINDING DIRECTIVES (from pass-1 agglomerators, orchestrator-ratified):
- BASE-PIN RULE: every bbnf-worktree prototype must FIRST verify/fast-forward its tree to
  the commit named in its brief (pass-1 protos ran d2b32f6e3, 51 commits stale — LOCK-3
  flagged all convergence numbers as wrong-artifact until re-run on live).
- Project.ty@204 is quadruple-homed across LOCK-3 families — single-owner born-RED gate.
- LOCK-1 redirect: 2-way equality-engine fork (1A vs 1B); 1C banked-as-discipline; 1D
  banked-as-LOCK-2-substrate; open the attribute-grammar cross-rule-seam research thread.
- LOCK-4 keystone: ONE combinator front onto bbnf_ir::IrNode exercised as tree-walk AND
  bytecode + ~200-LOC TS switch-loop decoder — collapses 4A/4C/4D grafts; produce the first
  TS number (constitution-iii is 0% built).
- LOCK-7: land the three bounded parser fixes (verified on disk) as pass-2 probe openers.
| 6-locks-p1-pt | wf_50d1252c-5ea | same script (args: group pt, locks [4,5,6], pass 1) | DONE 3/3 (resume run 51 min, 4.1M tok) — LOCK-4: 4B fusion mechanism SETTLED (0 vs 60 vtable blr, 1.16-1.27x cold-paired; isomorphism now the binding axis, 4B re-cast as Rust backend of an isomorphic terminal); LOCK-5: 5C automata-as-extraction 60 / 5A one-HIR 55 ADVANCED, two-tree reconciliation is a GESTALT precondition, all verdicts carry SPEC-ONLY(grammar-driven) until LOCK-7/7A lands; LOCK-6: 6A 62 / 6D 55 / 6B 48 ADVANCED, 6C BANKED-at-60 (NEON-crossing decider measured NEGATIVE — wasm-core removed as primary; folds into 6B/6D tape stratum) |
| 6-locks-p1-pt-ext | wf_0c50c1bb-81e | same script (args: group pt-ext, locks [8,9], pass 1) | DONE 2/2 (39 min, 2.33M tok) — LOCK-8: 8A 58 / 8B 50 / 8C 45 / 8D 50 all ADVANCED (+2 frontier routes pre-BANKED; 8C re-scoped to LOCK-6 consumer); LOCK-9: 9A/9B 55 ADVANCED (axis decider = LOCK-6 verdict, unrun), 9C BLOCKED 44, 9D BLOCKED 30; live refutation-law violation found in number_simd.rs double-vaddv → pass-2 banks the vshrn fix first |

AMENDMENT (owner, 2026-07-16): nine locks now — parse-that gets its own set of befitting
size: LOCK-8 TS backbone plane, LOCK-9 scanner & kernel plane; sets = bbnf {1,2,3,7}, pt
{4,5,8,9}, weave {6}. THE GESTALT is now three-tier, each itself a convergence loop:
GESTALT-B (bbnf locks + GN codex) ∥ GESTALT-P (parse-that locks + PT codex + native series)
→ GESTALT-Ω (both repos, all locks — the terminal artifact: the perfected dual-repo tranche
set with DAG ordering). Pass-transition procedure applies to all three groups; from pass 2
the groups are bbnf [1,2,3,7] and pt [4,5,6,8,9] (fold pt-ext into pt).

Heartbeat cron live: job db26fdc3 (resurrect + pass-transition duty), hourly at :23,
session-scoped, 7-day auto-expiry — re-arm as resurrection step one; a PROCESS EXIT wipes
crons silently, so every resurrection MUST CronList first (prior jobs fc81e24f, 20e19341,
2fd2e8d7 died with their processes). Current prompt text: reconcile ledger vs live tasks; resume dead
runs via Workflow({scriptPath, resumeFromRunId, args-as-ledgered}) after TaskStop-ing
zombies + walls-log entry; if both pass-K workflows DONE and pass-(K+1) unledgered, execute
the stage-6 pass-transition procedure; never duplicate-launch; silent single-line green
otherwise.

## Stage-6 pass-transition procedure (durable — executable by a cold session or the heartbeat)

When BOTH pass-K workflows (groups bbnf + pt) are DONE and no pass-(K+1) run is ledgered:
1. Read every locks/LOCK-N/REGISTRY.md. The agglomerators' pass-(K+1) directives ARE the
   between-pass synthesis (written by Fable-tier agents) — a mechanical relaunch is faithful.
2. Launch pass K+1 with the SAME script
   (~/.claude/.../workflows/scripts/sk-v24-locks-pass-wf_19b300e6-c0a.js), args
   {"group":"bbnf","locks":[1,2,3,7],"pass":K+1} and {"group":"pt","locks":[4,5,6],"pass":K+1}.
   Pass research-directives to researchers ONLY as refined questions (never registry content —
   the withholding law); the orchestrator may edit the script to inject per-lock directives
   via args {"directives":{...}} before relaunch.
3. Ledger both runIds here FIRST (launch intent), then launch, then update.
4. Convergence talk begins only at pass ≥3; then per-lock fresh non-author audits (agents who
   authored nothing for that lock), then THE GESTALT (both repos) → the perfected tranche set
   with DAG ordering at docs/tranches/sk-v24/ (v2, named supersessions of the v1 codex).
5. A lock at two consecutive clean passes + zero gaps + audit survival = CONVERGED; drop it
   from subsequent rounds and record the freeze in its REGISTRY.

COLD-SESSION RESURRECTION (if this session died entirely): task #57 carries the live runIds
in metadata; this file carries the stage plan; re-arm the heartbeat cron (prompt text below)
as step one; then reconcile per the protocol at the top of this file.

## Heartbeat cron

Hourly, off-minute, session-scoped (7-day auto-expire; re-create on session restart as part
of resurrection). Duty: reconcile-and-resume per the protocol above; SILENT no-op when all
green — no spam. If this session is gone, a fresh session executing task #55 IS the heartbeat.

## Standing walls log (append incidents: what died, when, how resumed)

- 2026-07-16 stage 5: GN-6 author hit StructuredOutput retry cap AFTER writing GN-6.md
  (36KB, complete) — file verified on disk, no redispatch needed (the standing lesson held).
- 2026-07-16 stage 6: wf_19b300e6-c0a died at launch — the Workflow args parameter reached
  the script as a JSON STRING, not an object; pipeline(args.locks) threw. Cure: scripts
  normalize with typeof args === 'string' ? JSON.parse(args) : args. Relaunched as
  wf_63419230-36d (bbnf) + wf_50d1252c-5ea (pt). RULE for all future parameterized scripts.
- 2026-07-16 stage 6: pt pass-1 partial — 6 StructuredOutput retry-cap deaths; LOCK-5/6
  synth stages threw, dropping both pipeline items; LOCK-4 unaffected (folded). Files
  survived on disk (FAMILY-SPECS.md both locks). Resumed wf_50d1252c-5ea from journal cache.
  WATCH: the synth schema (proto_briefs exactly-2) may be brittle — if the resume dies on
  the same agents, relax minItems/maxItems to 1-3 and accept the cache loss for lock 4.
- 2026-07-16 ~08:00-09:29 ET — THE SESSION-LIMIT WALL: pt pass-2 (wf_a6d7a39f-623) lost 30
  agents at the wall (5 distillers survived); bbnf pass-1 resume (wf_63419230-36d) completed
  new protos for locks 1/2/7 (RETIRE-1D parity proof; LOCK-2 §0 master gate PASS; LOCK-7
  N_host_construction=0) but lost all three agglomerators at the wall. Reset 8:40am ET;
  resumed BOTH at 09:29 per protocol (same runIds, args exactly as ledgered). Lesson: resume
  observed to re-run some previously-folded stages (prefix-cache sensitivity to interleaving)
  — treat re-folds as refinement, not waste; registries are cumulative.
- 2026-07-16 ~10:50 ET — SECOND SESSION-LIMIT WALL (bbnf p2 lost 18/33 incl. all L1
  protos/crits/agg + L2/3/7 synths; pt p3 lost 8/35 = all 5 synths + 3 retry-cap deaths;
  stated reset 2:20pm ET but OWNER ORDER 10:57: limit fully reset, re-deploy ALL immediately,
  batches of THREE agents in parallel). Cure baked into the pass script: gagent() width
  semaphore (args.width, default 3) gating every stage — at most 3 live agents per run;
  cached replays pass through free; prompts byte-identical so the journal cache holds. This
  WIDTH discipline is standing law for all future pass launches. If another wall lands
  despite width 3, next resume goes SEQUENTIAL (bbnf first, pt queued on its completion).
- 2026-07-16 ~14:05 ET — PROCESS-EXIT WALL: the Claude Code process exited (not a rate
  limit); both width-3 resumes killed mid-flight (bbnf p2 at 44 journal results, pt p3 at
  58 — strong progress, all cached) AND the session-scoped heartbeat cron was wiped with it.
  Detected at 14:20 via stopped-task notifications. Cure: cron re-armed FIRST (hourly :23),
  then both runs resumed with args exactly as ledgered. Lesson ruled: a process exit kills
  crons silently — every resurrection MUST CronList and re-arm before resuming workflows.
