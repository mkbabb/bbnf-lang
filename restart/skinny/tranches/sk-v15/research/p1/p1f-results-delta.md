# SK-V15 P1-F: RESULTS extraction and delta

Pass: S-P1 Profile. Cycle: V15.
Scope: `skinny/RESULTS.md` row extraction, SK-V14 close delta, and telemetry freshness classification.
Verdict: RESULTS-UNCHANGED; fresh S-P1 evidence is external profile/PMU evidence, not a RESULTS rewrite.

## Section 1 - Method
Commands executed:

```sh
git show 8e7378025:skinny/RESULTS.md | cmp -s - skinny/RESULTS.md
git show bae430dcf:skinny/RESULTS.md | cmp -s - skinny/RESULTS.md
rg -n '^\| .* \| (parse_only|direct_to_struct|real_typed_struct|css_l4)' skinny/RESULTS.md | wc -l
```

Both cmp commands returned 0. The broad row grep returned 92 because it includes schema/example/header-adjacent CSS lines as well as the 75 telemetry rows; the prior committed manifest classification remains 51 JSON rows plus 24 CSS L4 rows.

## Section 2 - Findings
| Surface | Current RESULTS state | SK-V15 S-P1 freshness | Delta vs SK-V14 close |
|---|---|---|---|
| JSON parse_only | 17 rows, A/GO, strict measured-row | fresh P1-A interactive profiles + PMU | RESULTS file delta 0; hot-leaf evidence now current |
| JSON direct_to_struct | 17 rows, A/GO, strict measured-row | fresh P1-B interactive profiles + PMU | RESULTS file delta 0; c/B misses on `mesh` and `unicode_escapes` |
| JSON real_typed_struct | 17 rows, A/GO, strict measured-row | fresh P1-B interactive profiles + PMU | RESULTS file delta 0; c/B miss on `unicode_escapes` |
| CSS L4 | 24 rows still present in RESULTS as SK-V14 full-parse admits | audit-demoted by PASS-IMPL V1; not rescued by S-P1 | unresolved SK-V15 prune input, not an admit |

## Section 3 - Delta
No row in `skinny/RESULTS.md` changed between SK-V14 close and SK-V15 S-P1. The correct delta is evidentiary: interactive profile artifacts and PMU rows exist for JSON; CSS remains demoted by the implementation-overfit audit.

## Section 4 - Anomalies
- `skinny/RESULTS.md` still says `hot-leaf=not-collected` in JSON row notes. S-P1 evidence resolves those for research consumption through `evidence/p1e-normalized-attribution.tsv`, but does not mutate the admission ledger because S-P1 is read-only against skinny source.
- The CSS L4 rows still encode the SK-V14 one-measurement broadcast values. They remain PRUNE-WAVE-A input.
- The row grep count of 92 is not used as a telemetry-row count because the file includes non-manifest table rows around the CSS section.

## Section 5 - Sources
- `skinny/RESULTS.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/*.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/artifact-manifest.tsv`.
