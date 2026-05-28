# CH3 REGRESSION - T-P1 V3 (SK-V15)

## Verdict

ACCEPT.

Score: 7 / 7 ACCEPT, 0 REVISE, 0 REJECT.

The active SK-V15 V3 inventories preserve the V2 CH3 regression floor.
`NEW-CH3-V5-01` remains load-bearing in the live packet, REDRESS-183,
REDRESS-184, and REDRESS-209 through REDRESS-213 remain pre-blocked, and no
V3 fold reopens a closed REDRESS route or drops wave-graph-cycle detection.

## Evidence

- CH3 authority: `PASS-1-EXCAVATION.md` defines CH3 as the lens that checks no
  inventory reopens a `skinny/REDRESS.md` route, the rejected-route pre-block
  list is correctly identified, and admitted rows are not silently regressed.
  `ORCHESTRATOR.md` §3W carries the same CH3 contract, and §3Z requires folded
  dispositions before advancing.
- V2 baseline: `HARDENING-T-P1-V2-CONSOLIDATED.md:24` records CH3 ACCEPT
  because "`NEW-CH3-V5-01` delete/rebuild dependency rule is carried;
  REDRESS-183/184/209..213 remain pre-blocked."
- Active V3 1D carries the explicit pre-block row: `1D-skinny-lessons.md:157`
  says REDRESS-183, 184, and 209-213 are the wave-graph-cycle precedent and
  that deletion/retirement cannot close unless rebuild proof has landed no
  later than the same wave. `1D-skinny-lessons.md:173` records the V2 fold as
  `CH3-V1-005`.
- Active V3 1E carries the lock-amendment receiver: `1E-locks-evidence.md:143`
  adds delete/rebuild dependency proof for L08/L14, and
  `1E-locks-evidence.md:182` gives it a V3 wave/cost carrier.
- Active V3 1F carries the cross-corpus detector: `1F-coherence-scan.md:74`
  identifies the wave-graph-cycle miss, `1F-coherence-scan.md:109` keeps it as
  high-risk drift, and `1F-coherence-scan.md:145` states no delete/retire wave
  may proceed without the rebuild-provider proof table.
- Source REDRESS truth remains blocked: `skinny/REDRESS.md:5090-5118` records
  REDRESS-183 and REDRESS-184 as the W2/W4 delete-before-provider failures;
  `skinny/REDRESS.md:5171-5293` records REDRESS-209 through REDRESS-213 as the
  W5/W6 provider-free generator, deletion-gate, sub-wave authority, and
  destructive root-regeneration failures.
- SK-V15 S-P3 final surfaces reinforce, rather than weaken, the rule:
  `restart/skinny/tranches/sk-v15/SPEC.md:140` makes
  `NEW-CH3-V5-01` non-negotiable; `SPEC.md:192-204` defines dependency rows;
  `DISPATCH-PROMPT.md:74-91` mirrors the schema and active DEP rows.

Material command evidence used:

```sh
rg -n "NEW-CH3|REDRESS-18[34]|REDRESS-20[9]|REDRESS-21[0-3]|wave-graph|delete/rebuild|provider proof|pre-block" \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

Result: hits at `1D:44,156-157,173`, `1E:40,143,182`, and
`1F:74,109,116,145` show the active inventories still carry the
delete/rebuild rule, REDRESS pre-blocks, and wave-graph-cycle detector.

```sh
nl -ba skinny/REDRESS.md | sed -n '5088,5296p'
```

Result: REDRESS-183/184/209/210/211/212/213 are still recorded as rejected
delete-before-provider or destructive-regeneration routes.

```sh
rg -n "REDRESS-183|REDRESS-184|REDRESS-209|REDRESS-210|REDRESS-211|REDRESS-212|REDRESS-213|NEW-CH3-V5-01|provider_lands_no_later|DEP-W" \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md \
  restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md
```

Result: SPEC and DISPATCH include the named dependency schema and DEP rows,
while Alpha-C names the REDRESS 183/184/209-213 pre-block cluster.

## Findings

| ID | Disposition | Finding | Evidence | Required fold |
|---|---|---|---|---|
| CH3-V3-001 | ACCEPT | `NEW-CH3-V5-01` is still load-bearing in the active T-P1 V3 packet, not only in prior hardening prose. | `1D-skinny-lessons.md:157`, `1D-skinny-lessons.md:173`, `1E-locks-evidence.md:143`, `1F-coherence-scan.md:145`; SK-V15 `SPEC.md:140`, `SPEC.md:192-204`. | None. |
| CH3-V3-002 | ACCEPT | REDRESS-183 and REDRESS-184 remain blocked as the W2/W4 delete-before-provider precedent. | `skinny/REDRESS.md:5090-5118`; Alpha-C summary at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:30-31`; V3 carrier at `1D-skinny-lessons.md:157`. | None. |
| CH3-V3-003 | ACCEPT | REDRESS-209 through REDRESS-213 remain blocked for provider/template deletion before valid provider-free generation, sub-wave authority gaps, and destructive CSS root-runtime regeneration. | `skinny/REDRESS.md:5171-5293`; Alpha-C summary at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:32-33`; V3 carriers at `1D-skinny-lessons.md:157`, `1F-coherence-scan.md:145`. | None. |
| CH3-V3-004 | ACCEPT | The V3 folds do not drop wave-graph-cycle detection. 1D, 1E, and 1F all retain independent receivers for the same rule. | `1D-skinny-lessons.md:173`; `1E-locks-evidence.md:143`; `1F-coherence-scan.md:74`, `1F-coherence-scan.md:109`, `1F-coherence-scan.md:145`. | None. |
| CH3-V3-005 | ACCEPT | EventTape and typed-event cursor gaps remain fenced against EventCursor, retained structural-stream, class-lane, parser-owned cursor, or cross-call classifier-state reopens. | `1A-substrate-evidence.md:75-76`, `1A-substrate-evidence.md:94`; `1B-codegen-evidence.md:69`; `1C-runtime-evidence.md:64`, `1C-runtime-evidence.md:101`. | None. |
| CH3-V3-006 | ACCEPT | CSS broadcast/source-sidecar routes remain blocked rather than re-admitted as runtime substrate, CSS Value API proof, or independent CSS SOTA proof. | CSS demotion and provider coupling at `1D-skinny-lessons.md:150-157`; CSS source-sidecar fence at `1F-coherence-scan.md:86`; Alpha-C pre-blocks at `alpha-C-redress-digest.md:54-58`; SK-V15 close gates at `SYNTHESIS.md:38-49` and `SYNTHESIS.md:91-106`. | None. |
| CH3-V3-007 | ACCEPT | No admitted row is silently regressed. JSON remains admitted guard evidence; CSS is explicitly audit-demoted with proof and receiver routing rather than silently rewritten. | JSON/CSS ledger at `skinny/RESULTS.md:139-149`; SK-V15 split at `SYNTHESIS.md:57-68`; JSON guard and CSS demotion in `1D-skinny-lessons.md:99-105`; `1F-coherence-scan.md:81-82`. | None. |

## Required Fold

None. CH3 has no V3-required fold.
