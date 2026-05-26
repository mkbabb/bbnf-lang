# Pass Omega V8 CH4 Cost

Date: 2026-05-26.
Lens: CH4 cost.
Disposition: ACCEPT.

## Findings

1. The V2 CH4 rejection is correctly preserved as the cost baseline. V2 failed
   because four informal 30-minute W5B-internal slices plus final verification
   did not fit the single W5B cap, W5B.0 mixed too many risk surfaces, LOC
   accounting omitted touched redress/report paths, and same-wave consumer
   wording weakened to a commit-set close
   (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:9-35`).
2. V8 fixes the authority shape rather than papering over the cap. Omega-A,
   Omega-B, Omega-D, Omega-E, Omega-F, the master-plan diff, and the corrective
   packet all converge on five formal W5B sub-waves: W5B.0 LOCK14-GATE, W5B.1
   IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4
   REQUEST-CONSUMER
   (`restart/audit/totality/astral/V8/ΩA-coherence-audit.md:69-72`,
   `restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:34-47`,
   `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:50-56`,
   `restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:22-32`,
   `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:30-36`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:47-67`,
   `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:84-90`).
3. The time cap is explicit and bounded. Each W5B.N sub-wave carries HARD CAP
   30 min, with safe evidence at 27 min and halt at 30 min; the aggregate
   W5B-FRONTEND implementation/redress cap is <=150 min across W5B.0 through
   W5B.4
   (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:60-63`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:37-39`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:51`,
   `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:92-95`).
4. LOC handling is sufficient for CH4. V8 preserves the existing <=1.0k C-1
   part-A source/test LOC envelope unless a later CHALLENGE-authorized partition
   narrows it, and it keeps generated output uncounted only when named,
   diff-audited, and included in the revert slice
   (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:64-67`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:34-36`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:52-53`). Omega-E also
   forbids changing LOC budgets outside W5B cap-accounting language
   (`restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:104-106`).
5. W5B.0 is now cap-valid scope. It is Lock14-only: owner-path roster,
   parent-diff routing, modified provider/template rejection tests, all-template
   guard, and generic owner-path leak census, with no grammar/codegen/xtask
   frontend source edits
   (`restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:57-62`,
   `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:52`,
   `restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:186-205`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:55-57`,
   `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:86`).
6. No borrowing remains open. V8 forbids borrowing time, source/test LOC,
   verification debt, or same-wave consumer evidence from W5C-GEN, W5D-DELETE,
   W6, W7, or new-admit waves, and keeps W5C-GEN blocked until W5B.4 closes
   aggregate W5B-FRONTEND
   (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:68-73`,
   `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:38-41`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:40-43`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:81`,
   `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:95-97`).

## Exact Folds Accepted

1. `restart/skinny/tranches/sk-v14/SPEC.md` Section 2: replace the one-slot
   W5B-FRONTEND cap with a formal aggregate sub-wave declaration naming W5B.0
   through W5B.4, <=30 min per W5B.N, <=150 min aggregate, and the preserved
   <=1.0k C-1 part-A source/test LOC envelope unless CHALLENGE narrows it.
2. `restart/skinny/tranches/sk-v14/SPEC.md` Section 8B: replace one-shot W5B
   tasks, exit gate, rerun/maintain wording, and revert protocol with W5B.0
   through W5B.4 entry/exit gates, W5B.0 Lock14-only scope, W5B.4 same-commit
   consumer proof, and final W5B close semantics.
3. `restart/MASTER-PLAN.md` Section 13.3: replace the one-shot W5B-FRONTEND row
   with aggregate W5B.0..W5B.4 and keep W5C-GEN blocked until aggregate
   W5B-FRONTEND close.
4. `restart/skinny/tranches/sk-v14/SPEC.md` Section 8C: change the W5C-GEN
   entry gate to aggregate W5B-FRONTEND close without changing W5C-GEN
   ownership.
5. `restart/skinny/tranches/sk-v14/SYNTHESIS.md`,
   `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`,
   `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`, and tranche handoff
   surfaces: dispatch W5B.0 first, apply the 30-minute per-sub-wave cap, forbid
   treating W5B.0 through W5B.3 as W5B close, and forbid W5C-GEN dispatch before
   W5B.4 closes aggregate W5B-FRONTEND.
6. `restart/HANDOFF.md`, `restart/MIGRATION.md`, and
   `restart/skinny/tranches/sk-v14/HANDOFF.md`: record REDRESS-212 /
   W5B-FRONTENDR, name W5B.0 LOCK14-GATE as next dispatch, and keep downstream
   W5C-GEN, W5D-DELETE, W6, W7, and W8/W9/W10 blocks intact.
7. `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md`: limited active
   authority alignment only where they still present W5B-FRONTEND as one capped
   wave. `restart/skinny/{BENCH,SUBSTRATE}.md`, `restart/ARCHITECTURE.md`, and
   `restart/locks/LOCKS.md` remain read/no-op for CH4 cost.

## Verdict

ACCEPT. V8 discharges the V2 CH4 cost defect with formal sub-wave authority,
per-sub-wave and aggregate caps, bounded LOC accounting, a narrowed W5B.0 scope,
and explicit no-borrowing semantics. No CH4 revision is required.
