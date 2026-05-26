# Pass Omega V8 CHALLENGE V2 CH1 Correctness

Date: 2026-05-26.
Lens: CH1 correctness.
HEAD checked: `284e5683c`.
Disposition: ACCEPT.

## Findings

1. The cited authority chain resolves. Pass Omega scope/CRUD/G-Omega lines resolve
   at `restart/prompts/pass-contracts/PASS-OMEGA.md:24`-`35`,
   `:57`-`:74`, `:86`-`:104`, and `:168`-`:171`; SK-V14 SPEC's current
   one-shot W5B cap and split-before-dispatch rule resolve at
   `restart/skinny/tranches/sk-v14/SPEC.md:243`-`260`; SPEC Section 8B W5B
   owner/task/exit/same-wave/pre-block text resolves at `:712`-`:762`;
   MASTER-PLAN V7 one-shot W5B rows resolve at
   `restart/MASTER-PLAN.md:788`-`819`; Lock 10/14 and the 16-lock / five-shape
   BackendShape claims resolve at `restart/locks/LOCKS.md:44`-`61`,
   `:100`-`:109`, `:269`-`:280`, and `:349`-`:390`; the directive-canon `@ws`
   retirement claim resolves at `restart/ARCHITECTURE.md:1616`-`:1631`.
2. REDRESS-212 is correctly characterized. It rejects W5B-FRONTEND as a
   documentation-only rejection, retains no frontend/codegen/xtask source redress,
   and routes the correction through Pass Omega V8
   (`restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md:7`-`18`,
   `:68`-`:83`).
3. The W5B-FRONTEND V2 challenge evidence matches the V8 split. The V2
   consolidated packet records 2/7 ACCEPT with CH1, CH2, CH4, CH5, and CH6 still
   REVISE, and requires formal sub-waves or a narrowed non-closing slice
   (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:1`-`19`,
   `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:28`-`:58`). CH4 specifically rejects four internal 30-minute slices plus
   final verification under the single W5B cap
   (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:9`-`:13`,
   `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:31`-`:38`),
   and CH5 rejects the old W5B.0 because it coupled Lock 14 routing with import /
   `@ws` source work
   (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:18`-`:22`,
   `:30`-`:38`). V8's
   `W5B.0 LOCK14-GATE -> W5B.1 IMPORT-CLOSURE -> W5B.2 LAYOUT-DISCARD ->
   W5B.3 PRETTY-SPAN-PROJECTION -> W5B.4 REQUEST-CONSUMER` split directly
   answers that evidence (`restart/audit/totality/astral/V8/master-plan-diff.md:7`-`:26`,
   `restart/audit/totality/astral/V8/master-plan-diff.md:47`-`:67`;
   `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:15`-`:56`).
4. The prior V1 CH1/CONSOLIDATED exactness blockers are now folded. V1 CH1
   required owner file/type and exact test names per construct, exact W5B.0 Lock
   14 tests, per-test/per-log nonzero proof, maintain routing, and redress/REDRESS
   LOC accounting (`restart/audit/totality/astral/V8/hardening/CH1.md:21`-`:30`,
   `:34`-`:48`; `restart/audit/totality/astral/V8/hardening/CONSOLIDATED.md:27`-`:48`).
   The folded V8 authority now carries the exact construct table with owner
   file/type, target representation, exact positive tests, and exact fail-closed
   test names (`restart/audit/totality/astral/V8/master-plan-diff.md:68`-`:82`;
   `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:58`-`:75`).
5. The remaining CH1 exactness gates are present. Exact W5B.0 Lock 14 tests are
   named at `restart/audit/totality/astral/V8/master-plan-diff.md:83`-`:91`
   and mirrored in ΩD/ΩE/ΩF; per-test and per-log nonzero proof rejects wildcard
   aggregate greps at `restart/audit/totality/astral/V8/master-plan-diff.md:92`-`:96`,
   `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:87`-`:94`,
   and `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:81`-`:85`;
   redress report plus reject-only `skinny/REDRESS.md` LOC accounting is explicit
   at `restart/audit/totality/astral/V8/master-plan-diff.md:97`-`:100`,
   `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:94`-`:95`,
   and `restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:147`-`:150`.
6. Locks and architecture no-op claims are coherent.
   `restart/audit/totality/astral/V8/locks-diff.md:1`-`:26`
   proposes zero `LOCKS.md` delta, and ΩC explains that the V2 challenge found
   missing Lock 14 execution detail rather than missing lock text
   (`restart/audit/totality/astral/V8/ΩC-locks-amendments.md:51`-`:69`).

## Exact Fold Check

| Required CH1 fold | V2 result |
|---|---|
| Owner file/type per construct | ACCEPT: `restart/audit/totality/astral/V8/master-plan-diff.md:72`-`:82`; `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:65`-`:75`. |
| Exact fail-closed test names | ACCEPT: every construct row now names exact fail-closed tests; prose cells from the old W5B plan are superseded. |
| Exact W5B.0 Lock 14 tests | ACCEPT: eight tests named, including W5C/W5D rejection, provider/template modification rejection, all-template count 8, `grammar_provider.rs` exception, and leak census. |
| Per-test/per-log nonzero proof | ACCEPT: dedicated `/tmp/skv14-w5b-<test-name>.log` plus dedicated nonzero `rg` is required; wildcard aggregate greps are rejected. |
| Redress/REDRESS LOC accounting | ACCEPT: touched redress reports and reject-only `skinny/REDRESS.md` edits count in W5B LOC accounting. |

## Verdict

ACCEPT. Commit `284e5683c` folds the V1 CH1 exactness requirements into the V8
master/SPEC authority and mirrors them through ΩA, ΩD, ΩE, and ΩF. The old
W5B-FRONTEND plan remains historical V2 challenge evidence; the amended V8
authority is the W5B.0 through W5B.4 split with exact tests, nonzero proof, and
LOC accounting.
