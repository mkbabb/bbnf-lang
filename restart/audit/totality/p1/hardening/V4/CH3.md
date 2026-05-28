# CH3 REGRESSION - T-P1 V4 (SK-V15)

## Verdict

ACCEPT.

The SK-V15 V4 inventories preserve the CH3 regression floor. The V4 cost
carrier and primitive/kernel receiver edits in `1D-skinny-lessons.md` do not
reopen any REDRESS route: `NEW-CH3-V5-01` remains a live dependency-table
guard, REDRESS-183/184/209-213 remain pre-blocked, and admitted JSON rows are
not silently demoted to unimplemented work.

## Evidence

- CH3 authority: `restart/prompts/totality/PASS-1-EXCAVATION.md:116-119`
  requires no inventory to re-open a `skinny/REDRESS.md` route, requires the
  rejected-route pre-block list to be identified by 1D and 1E, and forbids
  mis-cataloguing admitted REDRESS rows as unimplemented. `restart/prompts/ORCHESTRATOR.md:85`
  carries the same CH3 contract, and `restart/prompts/ORCHESTRATOR.md:112-120`
  requires hardening folds before convergence.
- V4 dispatch focus: `restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:5-7`
  states the old V4 CH files are superseded by this SK-V15 cycle, and
  `restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:60-61`
  narrows CH3 V4 to delete/rebuild guards and REDRESS-183/184/209-213 after
  cost/primitive table edits.
- V3 baseline: `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:27`
  accepted CH3 because `NEW-CH3-V5-01` remained load-bearing and
  REDRESS-183/184/209..213 remained pre-blocked. The same consolidator's V4
  roster required cost/primitive edits in 1D at
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:38-39`,
  while preserving CH3 posture at
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:55`.
- Current 1D carries the live pre-block row: `restart/audit/totality/p1/1D-skinny-lessons.md:159`
  names REDRESS-183, REDRESS-184, and REDRESS-209 through REDRESS-213 as the
  wave-graph-cycle precedent and states that delete/retire waves cannot close
  unless rebuild-provider proof has already landed or lands in the same wave.
  The V2 fold ledger keeps this as CH3-V1-005 at
  `restart/audit/totality/p1/1D-skinny-lessons.md:191`.
- Current 1E keeps the lock receiver: `restart/audit/totality/p1/1E-locks-evidence.md:144`
  proposes LAC-1E-V2-15 for delete/rebuild dependency proof, citing SK-V15
  dependency-table language and REDRESS wave-cycle precedent. Its cost carrier
  remains high-risk and tied to "S-P3 dependency table + T-P3 3C" at
  `restart/audit/totality/p1/1E-locks-evidence.md:183`.
- Current 1F keeps an independent detector: `restart/audit/totality/p1/1F-coherence-scan.md:76`
  records `NEW-CH3-V5-01` as a live wave-graph-cycle miss, and
  `restart/audit/totality/p1/1F-coherence-scan.md:149` states no delete/retire
  wave may proceed without rebuild-provider proof.
- Source REDRESS truth still resolves: REDRESS-183 is rejected at
  `skinny/REDRESS.md:5092-5101`, REDRESS-184 at `skinny/REDRESS.md:5105-5118`,
  REDRESS-209 at `skinny/REDRESS.md:5173-5193`, REDRESS-210 at
  `skinny/REDRESS.md:5197-5217`, REDRESS-211 at `skinny/REDRESS.md:5221-5245`,
  REDRESS-212 at `skinny/REDRESS.md:5249-5272`, and REDRESS-213 at
  `skinny/REDRESS.md:5276-5293`.
- SK-V15 dependency-table language remains explicit in
  `restart/skinny/tranches/sk-v15/SYNTHESIS.md:102-106` and
  `restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:64-69`.
  Alpha-C also names the same pre-block cluster at
  `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:26-33`
  and blocks any deletion/retirement wave until its rebuild provider is proven
  at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:64-69`.

Material checks run:

```sh
rg -n "NEW-CH3|REDRESS-183|REDRESS-184|REDRESS-209|REDRESS-210|REDRESS-211|REDRESS-212|REDRESS-213|delete/rebuild|rebuild provider|provider proof|wave-graph|pre-block" \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

Result: current V4 hits in 1D, 1E, and 1F show the delete/rebuild rule, REDRESS
pre-blocks, and wave-graph-cycle detector are still live.

```sh
nl -ba skinny/REDRESS.md | sed -n '5088,5296p'
```

Result: REDRESS-183/184/209/210/211/212/213 still resolve to rejected
delete-before-provider, provider-free-generator, sub-wave-authority, or
destructive-regeneration routes.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH3-V4-001 | ACCEPT | `NEW-CH3-V5-01` is load-bearing in the current V4 packet, not only in V3 hardening prose. | `restart/audit/totality/p1/1D-skinny-lessons.md:159`, `restart/audit/totality/p1/1D-skinny-lessons.md:191`; `restart/audit/totality/p1/1E-locks-evidence.md:144`, `restart/audit/totality/p1/1E-locks-evidence.md:183`; `restart/audit/totality/p1/1F-coherence-scan.md:76`, `restart/audit/totality/p1/1F-coherence-scan.md:149`; SK-V15 source rule at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:102-106`. | None. |
| CH3-V4-002 | ACCEPT | REDRESS-183 and REDRESS-184 remain blocked as delete/rebuild precedents. | REDRESS-183 remains the historical W2 dual-tree rejection at `skinny/REDRESS.md:5092-5101`; REDRESS-184 remains the W4 provider/template deletion rejection at `skinny/REDRESS.md:5105-5118`; Alpha-C summarizes them at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:30-31`; current 1D carries them at `restart/audit/totality/p1/1D-skinny-lessons.md:159`. | None. |
| CH3-V4-003 | ACCEPT | REDRESS-209 through REDRESS-213 remain blocked for provider/template deletion before valid provider-free generation, missing sub-wave authority, and destructive root-runtime regeneration. | Source entries resolve at `skinny/REDRESS.md:5173-5193`, `skinny/REDRESS.md:5197-5217`, `skinny/REDRESS.md:5221-5245`, `skinny/REDRESS.md:5249-5272`, and `skinny/REDRESS.md:5276-5293`; Alpha-C summarizes the group at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:32-33`; current carriers are `restart/audit/totality/p1/1D-skinny-lessons.md:159` and `restart/audit/totality/p1/1F-coherence-scan.md:149`. | None. |
| CH3-V4-004 | ACCEPT | The V4 receiver cost carrier does not weaken the delete/rebuild guard. | `restart/audit/totality/p1/1D-skinny-lessons.md:174-184` splits bounded receiver rows. The risky delete-bearing Pattern H row rejects header-only/destructive close and requires same-wave replacement proof at `restart/audit/totality/p1/1D-skinny-lessons.md:178`; parse-that rows require a REDRESS pre-block check and generated consumer at `restart/audit/totality/p1/1D-skinny-lessons.md:183`; primitive same-wave consumers require explicit final disposition at `restart/audit/totality/p1/1D-skinny-lessons.md:184`. | None. |
| CH3-V4-005 | ACCEPT | The V4 primitive/kernel receiver table enumerates candidate routes without reopening blocked sidecar, hash, numeric, or direct-cursor paths. | `restart/audit/totality/p1/1D-skinny-lessons.md:200-217` enumerates concrete rows. PMULL/CSSC-linked bitmap rows remain architecture-blocked or scalar-delegate at `restart/audit/totality/p1/1D-skinny-lessons.md:202-203`; direct cursor/whitespace rejects retained cursor replay without schedule proof at `restart/audit/totality/p1/1D-skinny-lessons.md:209`; product-builder/harness hash rows are delete-or-bench-only and never production equality proof at `restart/audit/totality/p1/1D-skinny-lessons.md:211`; stale numeric/digit routes remain blocked at `restart/audit/totality/p1/1D-skinny-lessons.md:216`. | None. |
| CH3-V4-006 | ACCEPT | No admitted row is silently regressed. JSON remains admitted guard evidence, while CSS is explicitly audit-demoted rather than treated as a clean unimplemented reopen. | JSON guard rows are preserved at `restart/audit/totality/p1/1D-skinny-lessons.md:101-105` and `restart/audit/totality/p1/1F-coherence-scan.md:84`; CSS 24-row proof is flagged as audit-demoted at `restart/audit/totality/p1/1D-skinny-lessons.md:107-110`; 1E states JSON rows are credible but CSS close is over-stated at `restart/audit/totality/p1/1E-locks-evidence.md:97`. | None. |
| CH3-V4-007 | ACCEPT | The broader pre-block list remains a ledger constraint after the V4 edits. | Alpha-C says SK-V15 waves must not re-open broadcast CSS admits, mismatched CSSOM comparisons, moved string-literal parsers, silent exclusions, Pattern H without generated ownership, scaffold Decision Engine, or FNV closed-enum production migration at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:50-62`; 1F carries the same guard at `restart/audit/totality/p1/1F-coherence-scan.md:83`, `restart/audit/totality/p1/1F-coherence-scan.md:119`, and `restart/audit/totality/p1/1F-coherence-scan.md:150-154`. | None. |

## Required Fold

None. CH3 has no V4-required fold.
