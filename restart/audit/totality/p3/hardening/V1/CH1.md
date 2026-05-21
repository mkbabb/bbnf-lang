# T-P3 V1 CH1 Correctness

Verdict: ACCEPT.

## Scope

CH1 reviewed the T-P3 V1 synthesis packet against the PASS-3 correctness
contract: proposed deltas must cite real T-P1/T-P2 finding IDs or path:line
evidence; cited V1 target sections must resolve; 3C must disposition every real
LOCKS amendment candidate; and the proposed LOCKS v+1 diff must target current
`restart/locks/LOCKS.md` lines (`restart/prompts/totality/PASS-3-SYNTHESIS.md:103`-`107`).

## Evidence

T-P1 and T-P2 are valid inputs for this pass. T-P1 V5 records `G-T-P1-EXCAVATION-CONVERGED` as PASS and names the converged excavation packet (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:37`-`43`). T-P2 V5 records `G-T-P2-RESEARCH-CONVERGED` as PASS and authorizes 2A-2F plus the V2/V3/V4 addenda for T-P3 (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:10`-`16`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:30`-`45`).

The proposed delta tables are cited. 3A's ten architecture deltas carry concrete evidence and V1 architecture targets (`restart/audit/totality/p3/3A-architecture-synthesis.md:33`-`42`). 3B's nine MASTER deltas carry T-P1/T-P2 or skinny evidence plus MASTER target sections (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:128`-`136`). 3D's ten folds carry path:line evidence for each fold (`restart/audit/totality/p3/3D-skinny-fold.md:68`-`77`). 3E's eight generality deltas carry path:line evidence even where the source-id cell uses shorthand labels (`restart/audit/totality/p3/3E-grammar-generalisation.md:136`-`143`). 3F's eleven MIGRATION/HANDOFF deltas carry path:line evidence and current-surface targets (`restart/audit/totality/p3/3F-migration-handoff.md:77`-`87`). A mechanical path:line check over 3A-3F and the 3C diff found 1,127 local citations and zero missing files or out-of-range line targets.

The cited V1 targets resolve. The packet cites current `ARCHITECTURE.md` anchors for section 0, BIR, BackendShape, generated runtime, row-plane, and onboarding surfaces (`restart/audit/totality/p3/3A-architecture-synthesis.md:33`-`42`); current `MASTER-PLAN.md` anchors for sections 5 and 13 plus wave rows (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:23`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:128`-`136`); current `MIGRATION.md` and `HANDOFF.md` anchors in 3F (`restart/audit/totality/p3/3F-migration-handoff.md:66`-`87`); and current LOCKS anchors in 3C (`restart/audit/totality/p3/3C-locks-crystallisation.md:27`-`40`).

3C covers the real candidate set. The T-P1 1E table has eleven `LAC-1E-*` candidates (`restart/audit/totality/p1/1E-locks-evidence.md:100`-`110`). The T-P2 LOCKS candidate tables have five 2A candidates (`restart/audit/totality/p2/2A-sota-landscape.md:145`-`149`), seven 2B candidates (`restart/audit/totality/p2/2B-primitive-vocabulary.md:349`-`355`), five 2C candidates (`restart/audit/totality/p2/2C-grammar-neutrality.md:184`-`188`), five 2D candidates (`restart/audit/totality/p2/2D-cost-model.md:188`-`192`), four 2E candidates (`restart/audit/totality/p2/2E-host-arch-esoterica.md:268`-`271`), and four 2F candidates (`restart/audit/totality/p2/2F-parse-that-gaps.md:249`-`252`). That is 41 total. 3C reports the same total and dispositions as 30 ACCEPT, 11 MODIFY, 0 REJECT, and 0 DEFER (`restart/audit/totality/p3/3C-locks-crystallisation.md:42`-`50`), and its matrix enumerates all 41 with proposer, affected lock, disposition, evidence, and routing (`restart/audit/totality/p3/3C-locks-crystallisation.md:56`-`96`). No silent-drop candidate was found.

The proposed LOCKS diff targets current LOCKS lines. Hunk 1 replaces the current scoped SK-V9 allowance at `restart/locks/LOCKS.md:1`-`17` (`restart/audit/totality/p3/3C-locks-v+1-diff.md:16`-`52`). Subsequent hunks append to live Lock 1, 2, 3, 8, 9, 10, 11/12, 13, 14, 15, and 16 anchors at `restart/locks/LOCKS.md:52`, `:54`, `:56`, `:66`, `:68`, `:70`, `:72`-`:74`, `:76`, `:78`, `:80`-`:85`, and `:87`-`:112` (`restart/audit/totality/p3/3C-locks-v+1-diff.md:54`-`394`). The governance footer target before `## Lanes` resolves at `restart/locks/LOCKS.md:114` (`restart/audit/totality/p3/3C-locks-v+1-diff.md:396`-`415`). No lock renumbering or impossible target line was found.

## Required V2 Repairs

None for CH1 correctness.

Optional hygiene only: V2 may normalize shorthand source labels in 3B/3E/3F source-id cells into canonical finding IDs, but this is not required for CH1 because each affected proposed delta already carries resolving path:line evidence.
