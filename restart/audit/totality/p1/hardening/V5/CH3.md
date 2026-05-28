# CH3 REGRESSION - T-P1 V5 (SK-V15)

## Verdict

ACCEPT.

The SK-V15 V5 citation fold does not weaken the CH3 regression floor. The
folded inventory commit `af809cf27` touched `1A`, `1B`, and `1F` only; `1D`
and `1E` were not modified by the fold, and the `1F` change expands FNV
citations while keeping hash evidence quarantined as telemetry/UNKNOWN. The
delete/rebuild guard, REDRESS-183/184/209-213 pre-blocks, broader pre-block
ledger, and V4 CH3 accepted surfaces all remain load-bearing.

This is a CH3 lens verdict only. V5 hard-ceiling governance remains an
aggregator/orchestrator matter: the V5 context states that a clean V5 can close
known orphan REVISEs but does not by itself create two consecutive clean cycles
because V4 was REVISE (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:26-31`).

## Evidence

- CH3 authority is unchanged: the totality prompt requires no inventory to
  reopen a `skinny/REDRESS.md` route, requires 1D and 1E to identify the
  rejected-route pre-block list, and forbids mis-cataloguing admitted REDRESS
  rows as unimplemented (`restart/prompts/totality/PASS-1-EXCAVATION.md:116-119`).
  The orchestrator repeats the CH3 contract at
  `restart/prompts/ORCHESTRATOR.md:85` and requires hardening folds before
  advance at `restart/prompts/ORCHESTRATOR.md:112-120`.
- V5 context says the existing V5 CH files were stale SK-V14 material and must
  be superseded by SK-V15 V5 outputs
  (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:3-7`). It
  scopes CH3 V5 to verifying that the citation fold does not weaken
  delete/rebuild guards, REDRESS-183/184/209-213, or the broader pre-block
  ledger (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:72-73`).
- The V5 fold roster is citation/prose-only: F01 expands JSON scan/sink
  citations in 1A, F02 expands COH-016 FNV citations in 1F, and F03 repairs
  stale V3 self-description in 1A/1B
  (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:45-52`). The
  same context requires V4 ACCEPT surfaces to remain intact, including bounded
  receiver rows, non-admitting primitive rows, telemetry-only FNV/hash evidence,
  and generated-header-as-file-state treatment
  (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:54-61`).
- The actual fold diff matches that scope. `git diff --name-only
  0c79c2b43..af809cf27 -- restart/audit/totality/p1/1{A..F}*.md` reports only
  `1A-substrate-evidence.md`, `1B-codegen-evidence.md`, and
  `1F-coherence-scan.md`; `1D-skinny-lessons.md` and `1E-locks-evidence.md`
  are not in the V5 fold diff. The visible line-level receivers are the 1A
  stale-prose repair at `restart/audit/totality/p1/1A-substrate-evidence.md:56`,
  the 1A scan/sink root citation at
  `restart/audit/totality/p1/1A-substrate-evidence.md:83`, the 1B stale-prose
  repair at `restart/audit/totality/p1/1B-codegen-evidence.md:37`, and the 1F
  COH-016 citation expansion at
  `restart/audit/totality/p1/1F-coherence-scan.md:89-101`.
- The live delete/rebuild guard remains explicit in 1D: CSS parser retirement
  cannot outrun typed rebuild proof
  (`restart/audit/totality/p1/1D-skinny-lessons.md:158`), and REDRESS-183,
  REDRESS-184, and REDRESS-209 through REDRESS-213 remain the wave-graph-cycle
  precedent requiring rebuild-provider proof no later than the delete/retire
  wave (`restart/audit/totality/p1/1D-skinny-lessons.md:159`). The V2 fold
  ledger keeps `NEW-CH3-V5-01` bound to that pre-block row
  (`restart/audit/totality/p1/1D-skinny-lessons.md:191`).
- The lock receiver remains explicit in 1E: LAC-1E-V2-15 requires any
  deleting/retiring wave to prove the rebuild provider for that artifact has
  landed no later than the same wave
  (`restart/audit/totality/p1/1E-locks-evidence.md:144`), and its cost/wave
  carrier remains high-risk and routed to "S-P3 dependency table + T-P3 3C"
  (`restart/audit/totality/p1/1E-locks-evidence.md:183`).
- The independent 1F coherence carrier remains explicit: COH-003 names
  `NEW-CH3-V5-01` and blocks deletion unless rebuild-provider proof lands no
  later than the delete wave (`restart/audit/totality/p1/1F-coherence-scan.md:76`);
  COH-010 records the broader pre-block ledger as implemented
  (`restart/audit/totality/p1/1F-coherence-scan.md:83`), and the past-corpora
  ledger repeats that REDRESS-183/184/209..212/213 remain pre-blocked
  (`restart/audit/totality/p1/1F-coherence-scan.md:161`).
- Source REDRESS truth still resolves: REDRESS-183 rejects dual-tree W2 before
  root CSS runtime regeneration (`skinny/REDRESS.md:5092-5101`), REDRESS-184
  rejects provider/template deletion before replacement generation
  (`skinny/REDRESS.md:5105-5118`), REDRESS-209 rejects monolithic W5
  provider/template deletion before a valid generator
  (`skinny/REDRESS.md:5173-5193`), REDRESS-210 rejects W5B deletion before a
  provider-free generator (`skinny/REDRESS.md:5197-5217`), REDRESS-211 rejects
  the provider-free-generator body gap (`skinny/REDRESS.md:5221-5245`),
  REDRESS-212 rejects the overpacked W5B-FRONTEND authority shape
  (`skinny/REDRESS.md:5249-5272`), and REDRESS-213 rejects destructive root CSS
  runtime regeneration without root `regen-css` capability
  (`skinny/REDRESS.md:5276-5293`).
- SK-V15 source governance matches the inventory guard: CSS parser retirement is
  coupled to typed rebuild capability
  (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:91-96`), `NEW-CH3-V5-01`
  requires a dependency table proving rebuild capability no later than deletion
  (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:100-106`), and Alpha-F repeats
  the same dependency-table columns and same-wave proof requirement
  (`restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:64-69`).
- The broader pre-block ledger remains live. Alpha-C names the wave-graph
  cycles at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:26-33`,
  lists the route patterns SK-V15 must not reopen at
  `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:50-62`,
  and states that any deletion/retirement wave remains blocked until its rebuild
  provider is proven no later than that wave at
  `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:64-69`.
  1F carries the same list at `restart/audit/totality/p1/1F-coherence-scan.md:131`
  and expands it through the past-corpora ledger at
  `restart/audit/totality/p1/1F-coherence-scan.md:162-166`.
- Admitted rows are not silently regressed. JSON remains the validated guard
  baseline in 1D (`restart/audit/totality/p1/1D-skinny-lessons.md:101-105`),
  in 1F (`restart/audit/totality/p1/1F-coherence-scan.md:84`), in RESULTS
  (`skinny/RESULTS.md:139-149`), and in Alpha-D
  (`restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:10-21`).
  CSS is separately audit-demoted, not treated as a clean unimplemented reopen,
  in 1D (`restart/audit/totality/p1/1D-skinny-lessons.md:107-110`) and Alpha-D
  (`restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:25-31`).
- V4 CH3 accepted these same surfaces: the V4 consolidator records CH3 ACCEPT
  with `NEW-CH3-V5-01`, REDRESS-183/184/209..213, and delete/rebuild guards
  load-bearing (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:27-29`).
  V4 CH3's accepted findings cover the live `NEW-CH3-V5-01` carriers, the
  REDRESS-183/184 block, the REDRESS-209..213 block, bounded receiver rows, the
  primitive/kernel non-admission table, admitted-row preservation, and the
  broader pre-block list (`restart/audit/totality/p1/hardening/V4/CH3.md:84-92`).

Material checks run:

```sh
git diff --name-only 0c79c2b43..af809cf27 -- restart/audit/totality/p1/1A-substrate-evidence.md restart/audit/totality/p1/1B-codegen-evidence.md restart/audit/totality/p1/1C-runtime-evidence.md restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md

git diff --unified=0 0c79c2b43..af809cf27 -- restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md

rg -n "NEW-CH3|REDRESS-183|REDRESS-184|REDRESS-209|REDRESS-210|REDRESS-211|REDRESS-212|REDRESS-213|delete/rebuild|rebuild provider|provider proof|wave-graph|pre-block" restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md

nl -ba skinny/REDRESS.md | sed -n '5088,5296p'
```

Result: the V5 fold touched no 1D or 1E bytes, changed 1F only to expand
root-resolving FNV citations, and left the delete/rebuild guard hits live in
1D, 1E, and 1F. The REDRESS source rows still resolve to rejected or
pre-blocking delete-before-provider, provider-free-generator, sub-wave-authority,
and destructive-regeneration precedents.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH3-V5-001 | ACCEPT | The V5 citation fold did not edit the substantive CH3 guard carriers in 1D or 1E. | Fold roster at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:45-52`; diff scope excludes 1D/1E; live changed receivers are 1A citation/prose at `restart/audit/totality/p1/1A-substrate-evidence.md:56` and `restart/audit/totality/p1/1A-substrate-evidence.md:83`, 1B prose at `restart/audit/totality/p1/1B-codegen-evidence.md:37`, and 1F COH-016 citation expansion at `restart/audit/totality/p1/1F-coherence-scan.md:89-101`. | None. |
| CH3-V5-002 | ACCEPT | `NEW-CH3-V5-01` and the delete/rebuild dependency proof guard remain load-bearing. | 1D C-6/C-7 at `restart/audit/totality/p1/1D-skinny-lessons.md:158-159`; 1D fold ledger at `restart/audit/totality/p1/1D-skinny-lessons.md:191`; 1E LAC at `restart/audit/totality/p1/1E-locks-evidence.md:144` and cost carrier at `restart/audit/totality/p1/1E-locks-evidence.md:183`; 1F COH-003 at `restart/audit/totality/p1/1F-coherence-scan.md:76`; source rule at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:100-106`. | None. |
| CH3-V5-003 | ACCEPT | REDRESS-183, REDRESS-184, and REDRESS-209 through REDRESS-213 remain pre-blocked and are not reopened by V5. | Source rows at `skinny/REDRESS.md:5092-5118` and `skinny/REDRESS.md:5173-5293`; Alpha-C summary at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:26-33`; live carriers at `restart/audit/totality/p1/1D-skinny-lessons.md:159` and `restart/audit/totality/p1/1F-coherence-scan.md:161`. | None. |
| CH3-V5-004 | ACCEPT | The broader pre-block ledger remains intact: CSS broadcast, CSSOM mismatch, string-literal generation, silent gate exclusions, Pattern H header-only close, Decision Engine scaffold, and FNV production migration are still blocked. | Alpha-C route list at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:50-62`; Alpha-C receiver block at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:64-69`; 1F COH-010 at `restart/audit/totality/p1/1F-coherence-scan.md:83`; 1F divergence ledger at `restart/audit/totality/p1/1F-coherence-scan.md:131`; 1F past-corpora ledger at `restart/audit/totality/p1/1F-coherence-scan.md:162-166`. | None. |
| CH3-V5-005 | ACCEPT | The FNV citation fold strengthens source coverage without laundering hashes into admission evidence. | COH-016 remains `unknown` and says FNV hashes are telemetry-only unless W10 proves otherwise at `restart/audit/totality/p1/1F-coherence-scan.md:89`; the seven-profile transcript is root-resolving at `restart/audit/totality/p1/1F-coherence-scan.md:91-101`; the gap row still requires production FNV/hash-sidecar quarantine proof at `restart/audit/totality/p1/1F-coherence-scan.md:177`. | None. |
| CH3-V5-006 | ACCEPT | No admitted REDRESS row is silently demoted. JSON remains guard evidence; CSS remains audit-demoted as its own invalidated class, not recast as a clean implementation gap. | JSON guard evidence in 1D at `restart/audit/totality/p1/1D-skinny-lessons.md:101-105`, RESULTS at `skinny/RESULTS.md:139-149`, and Alpha-D at `restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:10-21`; CSS demotion is explicit in 1D at `restart/audit/totality/p1/1D-skinny-lessons.md:107-110` and Alpha-D at `restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:25-31`; 1E says JSON rows are credible while CSS close is over-stated at `restart/audit/totality/p1/1E-locks-evidence.md:97`. | None. |
| CH3-V5-007 | ACCEPT | V4 CH3 accepted surfaces are preserved: bounded receivers, non-admitting primitive/kernel rows, and the broader pre-block ledger remain unchanged by the V5 fold. | V4 consolidator accepted CH3 at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:27-29`; V5 context requires preserving the same surfaces at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:54-61`; live receiver rows remain at `restart/audit/totality/p1/1D-skinny-lessons.md:170-184`; live primitive/kernel rows remain at `restart/audit/totality/p1/1D-skinny-lessons.md:196-217`; V4 CH3 accepted the same classes at `restart/audit/totality/p1/hardening/V4/CH3.md:84-92`. | None. |

## Required Fold

None.
