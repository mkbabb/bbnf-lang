# Pass Omega V3 CH4 Cost

| Field | Value |
|---|---|
| Pass | Pass Omega |
| Cycle | V3 CHALLENGE |
| Date | 2026-05-21 |
| Lens | CH4 Cost |
| Output | `restart/audit/totality/astral/V1/hardening/V3/CH4.md` |

## Verdict

REVISE.

CRUD-1 through CRUD-5 now have enough local cost evidence for CH4: LOC
budget, propagation, risk class, hard cap, and implementation
exclusion/routing are stated in the folded packet. The remaining blocker is
CRUD-6. Pass Omega defines a six-agent CRUD pass, assigns CRUD-6 to audit and
cleanup, gives every CRUD agent an independent 30 minute cap, constrains all
CRUD to CHALLENGE authorization, and requires G-Omega to present CRUD-1 through
CRUD-6 (`restart/prompts/pass-contracts/PASS-OMEGA.md:57`-`74`,
`restart/prompts/pass-contracts/PASS-OMEGA.md:98`-`104`). The folded cost
packet does not provide a CRUD-6 LOC budget, propagation target list, risk
class, hard cap beyond the generic agent cap, or delete/archive routing.

This is not a source-code or implementation blocker. It is a destructive-doc
receiver blocker: CRUD-6 is the only receiver whose contract includes deletion
and archiving, so a no-op, delete, or archive disposition must be explicit
before G-Omega can present "the proposed CRUD operations (CRUD-1 through
CRUD-6)" without ambiguity.

## Evidence

| Receiver | Disposition | Evidence | CH4 finding |
|---|---|---|---|
| Governing CH4 scope | ACCEPT | Pass Omega CH4 asks for LOC budget and propagation cost per amendment (`restart/prompts/pass-contracts/PASS-OMEGA.md:49`); the common CH4 lens requires LOC budget, risk class, wave alignment, hard cap, and same-wave consumer for kernels/primitives (`restart/prompts/ORCHESTRATOR.md:81`-`87`). | V3 checks receiver completeness, not only local proposal-family plausibility. |
| Convergence boundary | ACCEPT | V2 consolidated accepted one challenge cycle, but still requires another accepted cycle before advancement (`restart/audit/totality/astral/V1/hardening/HARDENING-OMEGA-V2-CONSOLIDATED.md:12`-`23`); the orchestrator requires two consecutive accepted cycles or explicit user pin (`restart/prompts/ORCHESTRATOR.md:118`-`123`). | A V3 REVISE can still block convergence and must be folded before CRUD/G-Omega. |
| CRUD-1 ARCHITECTURE | ACCEPT | CRUD-1 authority is `restart/ARCHITECTURE.md` (`restart/prompts/pass-contracts/PASS-OMEGA.md:65`). Omega-A carries amendment-family budgets, propagation files, receivers, risk classes, hard caps, and implementation exclusions for architecture-affecting fixes (`restart/audit/totality/astral/V1/ΩA-coherence-audit.md:40`-`51`); Omega-B gives a receiver row for CRUD-1 with doc LOC, one-file propagation, future implementation split, risk, hard cap, and evidence-only routing (`restart/audit/totality/astral/V1/ΩB-skinny-lessons.md:69`-`78`). | Cost basis is sufficient. |
| CRUD-2 MASTER-PLAN | ACCEPT | CRUD-2 authority is `restart/MASTER-PLAN.md` (`restart/prompts/pass-contracts/PASS-OMEGA.md:66`). Omega-B gives the receiver-level CRUD-2 budget and implementation exclusion (`restart/audit/totality/astral/V1/ΩB-skinny-lessons.md:69`-`78`); Omega-D gives LOC/risk/receiver allocations for status changes and MP.NW0-MP.NW12 (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:50`-`82`); the companion diff says those costs are review allocations, not implementation authorization (`restart/audit/totality/astral/V1/master-plan-diff.md:69`). | Cost basis is sufficient. |
| CRUD-3 LOCKS | ACCEPT | CRUD-3 owns `LOCKS.md` and is G-Omega-gated (`restart/prompts/pass-contracts/PASS-OMEGA.md:67`). Omega-C folds all 13 hunks into a CH4 ledger with LOC budget, propagation, risk, wave alignment, same-wave gate, and hard cap/receiver split (`restart/audit/totality/astral/V1/ΩC-locks-amendments.md:80`-`98`), mirrored in `locks-diff.md` (`restart/audit/totality/astral/V1/locks-diff.md:12`-`30`). | Cost basis is sufficient, including implementation routing for union, decision-engine, provider, and SIMD/ASM work. |
| CRUD-4 HANDOFF + MIGRATION | ACCEPT | CRUD-4 owns `HANDOFF.md` and `MIGRATION.md` (`restart/prompts/pass-contracts/PASS-OMEGA.md:68`). Omega-F's MIGRATION rows include LOC budget, propagation, risk, hard cap, and blockers/routing (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:41`-`49`), and its HANDOFF rows do the same (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:58`-`65`). Its boundary excludes governance/source/generated/gate/RESULTS/REDRESS edits (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:11`-`22`). | Cost basis is sufficient. |
| CRUD-5 SKINNY CORPUS | ACCEPT | CRUD-5 owns the six skinny corpus files (`restart/prompts/pass-contracts/PASS-OMEGA.md:69`). Omega-E has a CRUD-5 cost ledger for `BENCH.md`, `COMPILER.md`, `HARDENING.md`, `INDEX.md`, `SUBSTRATE.md`, and `WORKSPACE.md`, with doc LOC, propagation, receiver, risk, hard cap, and S-P3 implementation routing (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:30`-`41`). Its blockers exclude governance/source/generated/gate/RESULTS/REDRESS/W0 edits (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:54`-`59`). | Cost basis is sufficient. |
| CRUD-6 AUDIT + CLEANUP | REVISE | Pass Omega assigns CRUD-6 to "Legacy doc nuke + cohort archive" and says it may delete superseded audit docs and archive old cohorts while keeping historical audits in `restart/skinny/tranches/` (`restart/prompts/pass-contracts/PASS-OMEGA.md:70`). Omega-F requires the G-Omega packet to include proposed CRUD operations "CRUD-1 through CRUD-6" (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:79`-`85`). V2 CH4 accepted Omega-B's CRUD-1 through CRUD-5 plus G-Omega row, Omega-E's CRUD-5 row, and Omega-F's CRUD-4 rows, but does not cite any CRUD-6 cost row (`restart/audit/totality/astral/V1/hardening/V2/CH4.md:28`-`35`). | Missing LOC budget, propagation files, risk class, hard cap, operation type, nuke/archive target inventory, and implementation/source/gate exclusion for the delete/archive receiver. |
| Kernel/primitive same-wave consumer | ACCEPT | Lock hunk 12 requires strict checkasm, scalar reference, first consumer, row movement/rejection, and zero-orphan disposition, with SIMD/ASM implementation routed to later primitive waves (`restart/audit/totality/astral/V1/ΩC-locks-amendments.md:97`, `restart/audit/totality/astral/V1/locks-diff.md:29`). Omega-F also blocks retained source-present primitives without same-wave consumer, scalar delegate, deletion, or architectural-block proof (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:48`). | No V3 CH4 blocker remains in the kernel/primitive cost path. |

## Required Fold Items

1. Add an explicit CRUD-6 cost/routing row in the folded Omega packet before
   convergence is claimed. The row must state operation type (`Read` no-op,
   `Delete`, `Update` archive, or mixed), LOC or deletion/archive budget,
   propagation file count and exact files/directories, risk class, hard cap,
   and implementation/source/gate/RESULTS/REDRESS exclusion.
2. If CRUD-6 is a no-op for this Omega cycle, say so directly: `0 doc LOC`,
   `0 files touched`, low risk, hard cap 0 or a bounded verification cap, and
   no legacy doc deletion or cohort archive without a cited nuke plan.
3. If CRUD-6 deletes or archives anything, cite the nuke plan and list the
   exact targets. The fold must preserve historical audits in
   `restart/skinny/tranches/` and must not delete source, generated runtime,
   gate output, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.
4. Update the proposed CRUD/G-Omega presentation basis so CRUD-6 appears beside
   CRUD-1 through CRUD-5 with blockers and cost, rather than only as a generic
   item in the G-Omega checklist.

No required CH4 fold item is raised for CRUD-1 through CRUD-5.

## Verification

`git diff --check -- restart/audit/totality/astral/V1/hardening/V3/CH4.md`
passed with no output.
