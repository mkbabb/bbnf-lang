# Tranche AS — Retrospective

## 1. Scope reality vs plan

Plan: 5 phases, ~11 days estimated. Reality: 13 commits compressed
into ~1.5 hours on 2026-04-13 (plan rewrite `7f3d3d85` at 17:51 →
close `a6ec7b34` at 19:15). No `FINAL.md` was written; `PROGRESS.md`
is the sole exit artefact.

Landed cleanly: AS.1 CSS L4 activation (513-556 MB/s), AS.2.1+2.2
Span scalar admission, AS.3.1-3.4 scanner truth (sub-flag dispatch,
`scan_ident_with_escapes`, -263 LOC, cached `classify_regex`),
AS.5.6 generic number kernel. Scaffolded only: AS.2.3
`StructRegistry` (field added, never populated — AT Critical
Failure 5 marks it as dead code). Skipped: AS.4.3, AS.5.1, AS.5.2,
AS.5.4, AS.5.5.

## 2. Silent vs declared deferrals

Phase 4 is marked **PARTIAL** with one concrete commit (`415c85f4` —
a doc-only regression analysis). AS.4.1 (fresh samply profiles),
AS.4.2 (post-AS.json with throughput deltas), AS.4.3 (hot-path
optimisation) — all silently dropped. Hard gates #6 (post-AS.json)
and #7 (JSON twitter ≥ 2000 MB/s) were declared then skipped without
plan-document rationale.

Phase 5 declares 3/6 items "Not applicable" / "Not implemented" /
"Deferred" inside `PROGRESS.md` — retrofit rationalisations, not
plan-time deferrals. Hard gate #8 (five miners collapsed, ≥350 LOC
reduction) reconceded to 263 LOC and rerouted under AS.3.3.

## 3. Orchestration friction

No evidence of parallel agent dispatch. The commit cadence (single
author, strict temporal sequence, 1-10 min between commits) indicates
solo sequential execution, not a wave-structured tranche. No
worktree artefacts, no `research/` subdir, no audit/critique files —
the full apparatus declared in `docs/instructions/README.md` was
bypassed.

## 4. Agent-layer friction

Not observable from artefacts — the tranche was hand-executed. That
itself is the friction signal: a 5-phase plan with 9 hard gates
collapsed to solo execution, explaining both the speed (1.5h) and
the half-landing pattern (scaffold-without-use, partial-phase-no-
rationale).

## 5. Edict adherence

- Tranche directory: partial — `AS.md` and `PROGRESS.md` present;
  `FINAL.md` absent; no `research/` or audits.
- Commit-at-milestone: met (13 atomic commits, mostly conventional).
- `post-AS.json` capture: claimed in `a6ec7b34` but Hard gate #6's
  "measured throughput with deltas" not substantiated — AT.md
  reconstructs the regression table retroactively.
- No-deferral edict: violated — Phase 4 + 3 Phase 5 items dropped
  silently with post-hoc rationale.
- One codegen path: respected (scanner dispatch cleanly extends
  existing match).

## 6. Chronic deferrals IN + OUT

**IN (from AR):** 6 AR.6 scanner-consolidation items, sonic-rs gap,
Named struct ABI, 64-byte input padding, profile-directed hot-path.

**OUT (to AT):** meta_idx Vec regression (new; diagnosed not
fixed), StructRegistry population, `.map(|_| ())` value-discard
(AT Critical Failures 1-4), honest JSON bench, samply profiles,
hot-path opt, NEON 17-digit fractional scan. Named struct ABI and
ParsedGrammar elimination roll forward again.

## 7. Mid-tranche restructuring

AS was itself a mid-stream re-plan — `7f3d3d85` at 17:51 "rewrite
AS plan with post-audit ground truth" after the AR-audit modifier
fix (`0c6e011`) landed 10 minutes earlier. Plan re-scoped around
already-fixed work; no new tranche letter despite the scope-pivot
— a `new-tranche-new-doc` edict miss.

## 8. Lessons

1. **Scaffold-without-use is a silent deferral.** AS.2.3's
   `StructRegistry` hit its structural hard gate (field present,
   `compute_payload_layouts` handles `Named`) without ever being
   populated. AT's Critical Failure 5 documents it as dead code.
   Hard gates must require at least one real producer, not just a
   typed hole.
2. **A phase marked "PARTIAL" in PROGRESS is a declared-deferral
   masquerade.** Phase 4 skipped 3/3 items and still closed the
   tranche. Require `FINAL.md` with explicit gate-by-gate
   pass/defer/rationale accounting — PROGRESS prose is too soft a
   ledger.
3. **Solo sprint vs wave orchestration needs a plan-level
   choice.** AS ran in 1.5h hand-executed despite a plan calling
   for `~11 days` with 9 hard gates. Either the plan's scale was
   unjustified, or the wave/worktree apparatus should have been
   invoked — silently choosing solo mode hides both cases.
