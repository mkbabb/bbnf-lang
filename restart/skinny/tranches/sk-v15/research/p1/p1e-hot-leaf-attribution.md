# SK-V15 P1-E: hot-leaf attribution synthesis

Pass: S-P1 Profile. Cycle: V15.
Scope: P1-A/P1-B/P1-C hot-leaf synthesis and classification for all 17 JSON corpora.
Verdict: CURRENT-SYNTHESIS with unresolved line caveat preserved in evidence TSVs.

## Section 1 - Method
Inputs: `evidence/p1ab-interactive-hotleaf-top20.tsv`, `evidence/p1c-interactive-hotleaf-top20.tsv`, `evidence/pmu-cpb-summary.tsv`, and the CH2-normalized row ledger `evidence/p1e-normalized-attribution.tsv`. Classification is rule-based over resolved symbol names and is not an implementation proposal.

## Section 2 - Findings
| Corpus | Parse hot-leaf class | Direct hot-leaf class | Typed hot-leaf class | Mode-III masking class | S-P2 implication |
|---|---|---|---|---|---|
| twitter | other | structural/dispatch | structural/dispatch | unicode/string/other | optimize measured parser/tape path |
| citm_catalog | allocation/tape | structural/dispatch | structural/dispatch | unicode/string/other | optimize measured parser/tape path |
| canada | allocation/tape | unicode/string | structural/dispatch | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| apache_builds | scan/string | harness-hash | harness-hash | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| github_events | other | structural/dispatch | structural/dispatch | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| update_center | allocation/tape | structural/dispatch | structural/dispatch | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| mesh | allocation/tape | structural/dispatch | structural/dispatch | unicode/string/other | direct_strict c/B miss; inspect structural numeric/tape path |
| random | allocation/tape | structural/dispatch | allocation/tape | unicode/string/other | optimize measured parser/tape path |
| gsoc-2018 | allocation/tape | structural/dispatch | structural/dispatch | allocation/tape/harness-hash | separate parser work from harness hashing before claiming wins |
| marine_ik | allocation/tape | structural/dispatch | structural/dispatch | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| instruments | allocation/tape | structural/dispatch | structural/dispatch | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| numbers | allocation/tape | allocation/tape | other | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| unicode_mixed | other | unicode/string | allocation/tape | unicode/string/other | optimize measured parser/tape path |
| unicode_escapes | allocation/tape | scan/string | scan/string | allocation/tape/other | product-plane c/B miss; treat unicode/string path as S-P2 debt |
| unicode_basic | allocation/tape | scan/string | allocation/tape | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| distinct_values | allocation/tape | allocation/tape | allocation/tape | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |
| y_string_unicode | allocation/tape | allocation/tape | other | unicode/string/harness-hash | separate parser work from harness hashing before claiming wins |

The synthesis resolves the stale `hot-leaf=not-collected` condition for S-P1 analysis, but it intentionally does not rewrite `skinny/RESULTS.md`. The row table remains an admission ledger; this pass is evidence for S-P2.

The normalized attribution ledger is the binding S-P2 antecedent surface. It contains 119 rows: 17 corpora x 7 profiled surfaces. Each row carries `raw_function`, `raw_pct`, `raw_file_line`, `primitive_boundary`, `attribution_status`, and `s_p2_antecedent_status`. Generated JSON wrappers, schema-specific `parse_type_*` products, comparator frames, checksum paths, and sidecar-symbolization drift are explicitly blocked or diagnostic unless the row maps to a grammar-neutral scanner, tape/allocation, unicode/string, memory, or direct-parser cursor boundary.

No row in this pass reopens REDRESS-50-55, REDRESS-60-72, REDRESS-80, REDRESS-82-84, REDRESS-88, or REDRESS-89. Those families remain historical; this pass only supplies current measurement evidence for S-P2.

## Section 3 - Delta
Compared with the stale SK-V14 P1 docs, SK-V15 now has current interactive profiles for parse, product-plane, masking-probe, and structural-scan surfaces. The main directional change is that mode-III profiles expose harness hashing and UTF-8 validation as masking costs, while product-plane PMU exposes `mesh` and `unicode_escapes` c/B misses. These are measurement deltas only; S-P1 does not prescribe or reopen a repair route.

## Section 4 - Anomalies
- Some sidecar symbols resolve to file paths without line numbers; the full TSV preserves raw rows and the reports cite first line-resolved rows where possible. The normalized ledger marks parser-file drift as `unknown/harness/sidecar-symbolization`, not parser proof.
- Top parse-only rows often name vector frame operations because the `profile_direct` checksum loop materializes parse frames. The normalized ledger marks those as tape/allocation or harness materialization, not a JSON-specific primitive.
- CSS L4 is not reclassified by P1-E; PASS-IMPL V1 demotion still controls CSS.

## Section 5 - Sources
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1ab-interactive-hotleaf-top20.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1c-interactive-hotleaf-top20.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/pmu-cpb-summary.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv`.
