# Alpha-A — Results Extraction — SK-V15 V1

Pass: Pass Alpha. Cycle: SK-V14 -> SK-V15.
Date: 2026-05-27.
Scope: SK-V14 close counts, audit-demoted CSS state, telemetry baseline.
Output: this file.
Baseline commits: SK-V14 close `8e7378025`; PASS-IMPL V1 `cbafeb566`; W11W source `bae430dcf`.

## Findings

SK-V14 closed its row ledger as full-admit, but SK-V15 brackets the close
through PASS-IMPL V1 rather than trusting the close ledger alone.

| Family | SK-V14 close ledger | SK-V15 Alpha state | Evidence |
|---|---:|---|---|
| JSON parse_only | 17 / 17 admitted | validated guard baseline | `restart/skinny/tranches/sk-v14/research/skv14-W11-close.md:126`, `skinny/RESULTS.md:139` |
| JSON direct_to_struct | 17 / 17 admitted | validated guard baseline | `restart/skinny/tranches/sk-v14/research/skv14-W11-close.md:127`, `skinny/RESULTS.md:139` |
| JSON real_typed_struct | 17 / 17 admitted | validated guard baseline | `restart/skinny/tranches/sk-v14/research/skv14-W11-close.md:128`, `skinny/RESULTS.md:139` |
| CSS L4 | 24 / 24 admitted | audit-demoted, PRUNE-required | `restart/skinny/tranches/sk-v14/research/skv14-W11-close.md:129`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21` |

The JSON close is measurement-valid for this bracket. `skinny/RESULTS.md`
states that direct rows are strict product rows, typed rows are strict typed
product rows, and absent C++ sidecars are not used as SK-V14 strict anchors
(`skinny/RESULTS.md:147`, `skinny/RESULTS.md:150-152`). W11W admits the
final six parse_only rows with the `memchr2` trusted-string split and same-run cold evidence
(`skinny/REDRESS.md:6254`).

The CSS close is not measurement-valid at the row granularity claimed. The
24 `css_l4_full_parse` rows share one repeated measurement
(`track1=2319.041`, `lightningcss=929.281`, `cssparser=2362.037`) across
24 conceptual row ids (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`).
The generator backing those rows is a hand-written `CSS_GENERATED_RS`
string literal rather than grammar-derived emission
(`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:31`).

## SK-V15 Baseline Telemetry

SK-V15 inherits the Pass Alpha row schema from
`restart/prompts/pass-contracts/PASS-ALPHA.md:77`: corpus, workload,
outcome, verdict, strictness, output plane, Track 1/2 Mbps, comparator
Mbps, deltas, hot leaf, and signal. It also carries the current
manifest-level fields already present in `skinny/RESULTS.md`: row id,
domain, wave/run id, Track 1 and Track 2 entry points, comparator plane,
per-iteration equality, audit overlay, sidecar freshness, substrate
target, validation, profile artifact, sample count, build flags, host
triple, redress, consumer, and comparator evidence.

SK-V15 adds CSS-specific anti-broadcast fields:

| Field | Required meaning |
|---|---|
| `measurement_row_id` | one row's timing identity; cannot be reused by sibling admits unless the row is explicitly aggregate |
| `measurement_origin` | TSV / bench command / corpus slice that produced the timing tuple |
| `value_plane` | typed value / CSSOM / fact-stream / summary; fact-stream and summary cannot count as CSS Value API |
| `css_comparator_workload` | cssparser / lightningcss workload plane; lightningcss only counts after CSSOM parity |
| `generator_source` | grammar-derived source path or explicit hand-written status |
| `lock14_scan_scope` | full-surface scan or named incomplete scan |
| `lock16_status` | scalar-only / SIMD-claimed / ASM-claimed status for the row |
| `checkasm_or_parity_status` | required parity/checkasm status when Lock 16 is applicable |
| `gate_exclusion_report` | explicit Lock 14 / Lock 16 exclusion list, empty-list proof, or self-exemption failure |
| `broadcast_group_id` | empty for independent rows; non-empty rows cannot all admit as separate conceptual wins |

## Disposition

JSON is a guard baseline. CSS L4 is reopened as invalidated/open. SK-V15
cannot claim the inflection point until CSS has a typed value API,
same-workload measurements, and grammar-derived emission.
