# Tranche AK — Retrospective

## 1. Scope reality vs plan

AK predates the per-tranche directory edict. Only a single
`docs/tranches/AK.md` (now `AK/AK.md`) exists; no PROGRESS.md, no
FINAL.md, no audit/. The doc itself was authored inside commit
`c62ad389` at 05:15:02, **after** both implementation commits
(`0fc6bebe` AK.0, 05:04; `9658bb2e` AK.1+AK.2, 05:12). AK.md is
therefore a post-hoc close-out, not a forward plan.

Declared steps: AK.0 flat Vec substrate, AK.1 per-branch
`__branch_idx`, AK.2 variant-index correctness (absorbed by AK.1),
AK.3 "docs update" = the doc itself. All landed. Results (+6.8–10.1 %
vs AJ, surpassing simd-json on every JSON file) match the table
verbatim.

## 2. Silent vs declared deferrals

Neither. The tranche had no escape clause because there was no
plan-before-execution; scope was whatever the two commits delivered.
Zero undocumented deferrals, zero documented ones.

## 3. Orchestration friction

None recorded and none inferable. Three commits, single author, ~2.5 h
wall-clock (02:47 AJ close → 05:15 AK close), linear history, ≤ 165
insertions / 172 deletions. No cherry-picks, no worktree activity, no
sub-agents. This was a manual solo pass.

## 4. Agent-layer friction

N/A — no agent dispatch surface. The tranche is wholly pre-orchestrator
era.

## 5. Edict adherence

Most current edicts (per-tranche dirs, PROGRESS/FINAL cadence,
worktree isolation, `docs/instructions/README.md`) **did not yet
exist** — `docs/instructions/` was first created at `54f2c2bc` on
Apr 14, two days after AK. Measuring AK against today's edicts is
anachronistic. The one edict that did apply — commit-at-milestone —
was respected (three well-scoped conventional commits).

## 6. Chronic deferrals carried

**Inherited from AJ** (`AJ.md`): three-tier emission (Tape/Direct/Lazy)
was structurally inert on JSON because leaves were inlined; AK.md's
Context paragraph names this explicitly but does **not** dissolve it.

**Forwarded to AM**: `76085303` (AM.1) later *deleted* the entire
EmissionTier axis (~2000 LOC) and residual BumpSlab — the unused
scaffolding AK left in place. AM.0 (`4d1afeb0`) also caught four
pre-existing workspace regressions that AK's narrow scope skipped
(derive panic on `"->"`, `@recover` wrapper peeling, ParseDiagnostics
offset, stale LSP test). AM.3 per-branch push surgery (`cffcb6ba`)
finished the Alt-branch reform that AK.1's `__branch_idx` started.

## 7. Mid-tranche restructuring

None. No research/ directory, no audit, no mid-course re-plan. The
tranche was small enough to execute without iteration.

## 8. Lessons

1. **Post-hoc tranche docs are a retrospective anti-pattern.** AK.md
   reads cleanly because its author already knew the numbers; future
   retros cannot distinguish planned from emergent work. The current
   edict mandating `{LETTER}.md` *before* execution is the correct
   remediation — but retros of pre-edict tranches should treat their
   docs as commit-messages-in-prose, not plans.
2. **Tranches under ~200 LOC across ≤ 3 commits need no orchestration
   apparatus.** AK's two structural wins (flat Vec, per-branch
   discriminator) were single-file-locus changes. Forcing PROGRESS.md
   discipline onto work of this size is ceremony debt; the AW
   coalescer should reserve heavy orchestration for tranches whose
   diff exceeds a threshold, and permit lightweight close-outs for
   surgical passes.
3. **Foundation-laying vs foundation-demolishing tranches interleave
   productively.** AK landed the `__branch_idx` mechanism but left the
   obsolete EmissionTier axis standing; AM immediately demolished it.
   This rhythm — build, then prune the scaffolding the build exposed
   — is worth codifying as a planning idiom rather than treating the
   prune pass as "deferred debt".
