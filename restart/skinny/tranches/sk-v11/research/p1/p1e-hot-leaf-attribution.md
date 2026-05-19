# SK-V11 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: synthesize per-row hot leaves across parse, direct, typed, PMU, W0, and
P1-C/P1-F evidence without proposing behavior waves.
Output: this file.
Baseline: SK-V11-open (`3ce75df4`).
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: xctrace Time Profiler summary exports plus PMU TSVs; P1-C/P1-F
documents; W0 Criterion result surface.
Corpus coverage: 17/17 parse rows, 17/17 direct rows, 7/7 typed rows.

## Section 1 - Method

Read-only inputs:

```sh
sed -n '1,260p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md
sed -n '1,360p' skinny/RESULTS.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md
jq '.source, (.traces | length), (.traces[0] | keys)' /tmp/skv11-p1/parse-xctrace/exports/summary.json
jq '.source, (.traces | length), (.traces[0] | keys)' /tmp/skv11-p1/direct-xctrace/exports/summary.json
sed -n '1,120p' /tmp/skv11-p1/pmu/parse_pmu_rows.tsv
sed -n '1,160p' /tmp/skv11-p1/pmu/product_pmu_rows.tsv
sed -n '1,120p' /tmp/skv11-p1/pmu/capture_status.tsv
rg -n 'fn (skip_ascii_whitespace|scan_digit_run|match_string_at_quote_trusted_utf8|unescape_string|read_hex_unit_scalar|hex_nibble)' skinny/crates
rg -n 'match_tiny_plain_string|parse_array_element_at_direct|tiny_plain_string_end|skip_plain_string_end' skinny/crates
sed -n '2794,2940p' skinny/REDRESS.md
```

At final validation, `restart/skinny/tranches/sk-v11/research/p1/` contained
the full P1-A through P1-F cohort. P1-B confirms the direct/typed xctrace and
samply product-plane shape, and P1-A confirms the
parse-only xctrace/PMU rows and records 34/34 `samply-parse` artifacts with
`rc=0`; it also cautions that saved samply JSON profile metadata reports
`symbolicated=false` while companion `.json.syms.json` files carry symbol maps.
P1-D confirms 34 parse, 34 direct, and 14 typed PMU rows with `rc=0`, and it
supplies the aggregate c/B shape below. Therefore the self-time percentage
authority for P1-E V1 remains the xctrace Time Profiler summary export, and the
cost-shape authority is the PMU TSV pair plus P1-D's c/B ledger. P1-C
contributes masking, structural-scan, and lazy-tape cautions; P1-F contributes
row classification and SK-V10 delta.

The xctrace summary files contain:

| Export | Rows | Coverage |
|---|---:|---|
| `/tmp/skv11-p1/parse-xctrace/exports/summary.json` | 34 | 17 corpora x Track 1/Track 2 parse |
| `/tmp/skv11-p1/direct-xctrace/exports/summary.json` | 48 | 17 corpora x Track 1/Track 2 direct plus 7 typed x Track 1/Track 2 |

P1-D summarizes the PMU rows:

| Plane | Rows | PMU rc=0 | Aggregate c/B | Aggregate CPI | Aggregate IPC |
|---|---:|---:|---:|---:|---:|
| parse | 34 | 34 | 2.777033 | 0.211017 | 4.739 |
| direct | 34 | 34 | 4.428342 | 0.211681 | 4.724 |
| typed guards | 14 | 14 | 3.190644 | 0.190381 | 5.253 |

The `pct` values below are `pct_of_process_time` from each xctrace summary.
PMU `c/B` values come from `cycles_per_byte` in the TSV `PROBE_RESULT` rows.
Criterion Mbps and gate state remain the W0 authority; PMU throughput is a
diagnostic probe and is not used as admission evidence.

## Section 2 - Findings

### Hot-Leaf Vocabulary

| Cluster | Representative symbols | Source locus | Rows where it dominates |
|---|---|---|---|
| `string_tiny_scan` | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` / `::<8>` | `skinny/crates/runtime/src/grammars/json/generated.rs:171` | string-dense parse and direct rows: `twitter`, `github_events`, `update_center`, `distinct_values`, `random`, `apache_builds`, `unicode_basic` |
| `track2_tiny_scan` | `bbnf_bench::track2::json::match_tiny_plain_string` | `skinny/crates/bbnf-bench/src/track2/json.rs:314` | Track 2 parse string rows |
| `direct_hand_tiny_scan` | `<bbnf_bench::direct_struct::hand::HandParser>::tiny_plain_string` | `skinny/crates/bbnf-bench/src/direct_struct.rs:565` | Track 2 direct string rows |
| `typed_string_scan` | `<bbnf_bench::generated_real_typed::DirectParser>::tiny_plain_string_end`, `skip_plain_string_end` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811`, `:1825` | typed guards: `twitter`, `github_events`, `update_center`, `apache_builds` |
| `string_full_scan` | `parse_that_regex::match_string_at_quote_trusted_utf8`, `skip_string_plain_trusted` | `skinny/crates/parse-that-regex/src/lib.rs:162`, `:547` | `unicode_mixed`, `unicode_escapes`, `y_string_unicode`, `gsoc-2018` |
| `string_escape` | `parse_that_regex::validate_string_escape`, `unescape_string` | `skinny/crates/parse-that-regex/src/lib.rs:284`, `:718` | unicode direct and parse rows |
| `unicode_escape_hex` | `parse_that_regex::read_hex_unit_scalar`, `hex_nibble`, `validate_unicode_escape_run` | `skinny/crates/parse-that-regex/src/lib.rs:945`, `:959`, `:347` | `unicode_escapes`, `y_string_unicode` |
| `number_digit_scan` | `parse_that_regex::number::scan_digit_run`, `match_number_span_from_first` | `skinny/crates/parse-that-regex/src/number/mod.rs:106`, `:38` | `canada`, `mesh`, `marine_ik`, `numbers` |
| `number_materialize` | `parse_that_regex::number::materialize_u64`, `materialize_f64`, `parse_eight_digits` | `skinny/crates/parse-that-regex/src/number/mod.rs:247`, `:261`, `:214` | lower-ranked within number-heavy direct rows |
| `whitespace_skip` | `parse_that_regex::skip_ascii_whitespace`, `skip_ascii_spaces` | `skinny/crates/parse-that-regex/src/lib.rs:113`, `:128` | `citm_catalog`, `instruments`, `twitter`, `random`, typed `citm_catalog` |
| `dispatch_walk` | `runtime::generated_json::generated::dispatch_value`, `parse_value_at`; Track 2 `Parser::parse_value_at` | `skinny/crates/runtime/src/grammars/json/generated.rs:47`, `:37`; `skinny/crates/bbnf-bench/src/track2/json.rs:53` | `mesh`, `marine_ik`, `numbers`, `unicode_mixed`, escape parse rows |
| `structural_rediscovery` | `consume_container_next`, `consume_structural`, `parse_array_element_at_direct`, `parse_object_value_at_direct` | `skinny/crates/runtime/src/grammars/json/generated.rs:310`, `:292`, `:508`, `:468` | direct number arrays and mixed object/array rows |
| `simd_movemask` | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` | `gsoc-2018`, `github_events`, `update_center`, string scan support |
| `digest_hash` | `<bbnf_bench::direct_struct::JsonDirectDigest>::fold_string_scalar`, `hash_bytes`, `<u64>::wrapping_add` | `skinny/crates/bbnf-bench/src/direct_struct.rs:123`, `:717`, `:739` | direct digest rows, especially `apache_builds` and `distinct_values` |
| `typed_hash` | `bbnf_bench::real_typed_struct::hash_str`, `fold_opt_str`, `mix` | `skinny/crates/bbnf-bench/src/real_typed_struct.rs:734`, `:687`, `:742` | lower-ranked typed guard support |

The load-bearing pattern is not one leaf. The hot set splits into five
intervention-relevant families for later passes to reason about: tiny-string
matching, unicode/string escaping, number digit spans, whitespace/dispatch
walks, and digest/typed output hashing. This artifact names those families as
evidence only.

### Parse Diagnostic Rows

Parse rows are diagnostic-only. P1-F says the current parse surface is
16 `S / NO-GO` plus `canada` as `L / NO-GO`; P1-C and W0 say parse-only is
not a SOTA close target.

| Corpus | Outcome | Track 1 hot leaves | Track 2 hot leaves | PMU c/B T1/T2 | Attribution |
|---|---|---|---|---:|---|
| `twitter` | `S / NO-GO` | tiny16 47.2%, ws 11.6%, dispatch 7.3% | track2 tiny 38.3%, ws 12.2%, movemask 10.7% | 2.743 / 3.211 | string-tiny parse diagnostic |
| `citm_catalog` | `S / NO-GO` | ws 25.2%, tiny16 22.7%, memcpy 9.6% | ws 26.3%, track2 tiny 19.6%, container 12.2% | 1.332 / 1.841 | whitespace plus tiny strings |
| `canada` | `L / NO-GO` | digit-run 30.0%, memcpy 15.2%, dispatch 13.9% | digit-run 26.8%, Track 2 dispatch 20.2%, number span 13.9% | 1.968 / 2.061 | number scan; parse L anomaly |
| `apache_builds` | `S / NO-GO` | tiny16 33.3%, ws 21.6%, movemask 11.1% | track2 tiny 27.8%, ws 20.5%, movemask 12.9% | 3.265 / 3.320 | short-string and whitespace |
| `github_events` | `S / NO-GO` | tiny16 42.5%, movemask 10.5%, ws 9.8% | track2 tiny 41.8%, movemask 14.2%, ws 9.3% | 2.432 / 2.735 | tiny-string row |
| `update_center` | `S / NO-GO` | tiny16 48.1%, movemask 12.8%, dispatch 6.1% | track2 tiny 40.0%, movemask 14.3%, dispatch 7.6% | 2.996 / 3.879 | tiny-string row |
| `mesh` | `S / NO-GO` | digit-run 20.6%, dispatch 19.0%, ws 14.4% | Track 2 dispatch 23.1%, digit-run 22.7%, ws 11.7% | 3.242 / 3.304 | number plus dispatch |
| `random` | `S / NO-GO` | tiny16 37.6%, ws 15.1%, movemask 9.3% | track2 tiny 35.1%, ws 17.6%, key colon 8.7% | 4.087 / 4.827 | string row; P1-C cold-sensitive |
| `gsoc-2018` | `S / NO-GO` | movemask 33.1%, tiny16 19.2%, trailing-zeros 9.7% | movemask 33.3%, track2 tiny 17.9%, trailing-zeros 12.2% | 1.918 / 1.960 | SIMD-assisted string scan |
| `marine_ik` | `S / NO-GO` | dispatch 18.4%, digit-run 16.7%, memcpy 12.5% | Track 2 dispatch 25.6%, digit-run 17.3%, number span 12.0% | 3.140 / 3.248 | number and dispatch |
| `instruments` | `S / NO-GO` | tiny16 29.6%, ws 24.3%, dispatch 7.5% | ws 28.2%, track2 tiny 17.5%, movemask 6.8% | 2.483 / 3.130 | string plus whitespace |
| `numbers` | `S / NO-GO` | digit-run 34.5%, dispatch 19.6%, array next 8.5% | digit-run 36.0%, Track 2 dispatch 23.1%, container 7.7% | 2.267 / 2.318 | pure number row; P1-C cold-sensitive |
| `unicode_mixed` | `S / NO-GO` | dispatch 22.7%, validate escape 18.0%, full string 14.1% | Track 2 dispatch 20.5%, validate escape 19.5%, movemask 15.9% | 5.283 / 5.226 | unicode string/dispatch |
| `unicode_escapes` | `S / NO-GO` | read hex 22.9%, full string 19.4%, dispatch 18.1% | read hex 24.5%, dispatch 14.1%, full string 14.0% | 3.194 / 2.880 | unicode hex escape |
| `unicode_basic` | `S / NO-GO` | tiny16 34.8%, trailing-zeros 14.0%, dispatch 10.1% | track2 tiny 33.0%, trailing-zeros 16.1%, dispatch 11.0% | 2.911 / 3.313 | tiny string with UTF-8 payload |
| `distinct_values` | `S / NO-GO` | tiny16 58.7%, ws 13.1%, dispatch 7.2% | track2 tiny 65.1%, trailing-zeros 6.6%, ws 5.4% | 3.597 / 5.707 | tiny-string dominant |
| `y_string_unicode` | `S / NO-GO` | read hex 20.8%, hex nibble 20.7%, tiny16 7.1% | read hex 26.0%, hex nibble 17.8%, track2 tiny 7.6% | 6.022 / 5.964 | unicode escape hex dominant |

### Direct Residual Rows

This table separates the unclamped direct residual rows from direct guards and
W0-clamped non-admissions. Gaps are from P1-F and W0, using
`ceil(sonic-rs direct / 1.10)` as floor. Negative gaps mean below floor.

| Corpus | W0 gap T1/T2 Mbps | Direct Track 1 hot leaves | Direct Track 2 hot leaves | PMU c/B T1/T2 | Attribution |
|---|---:|---|---|---:|---|
| `twitter` | -2127 / -2924 | tiny8 20.0%, ws 17.6%, movemask 14.5% | hand tiny 18.7%, ws 14.7%, movemask 10.5% | 3.789 / 4.021 | string/whitespace residual |
| `canada` | -321 / -818 | digit-run 23.7%, direct array 14.2%, memcpy 11.8% | digit-run 21.6%, ws 10.8%, memcpy 10.4% | 3.313 / 3.495 | number array residual |
| `github_events` | -1485 / -2807 | tiny8 24.4%, movemask 15.2%, ws 13.6% | hand tiny 19.9%, movemask 14.3%, ws 9.7% | 2.903 / 3.112 | string-tiny residual |
| `update_center` | -1872 / -2585 | tiny8 26.3%, movemask 10.0%, wrapping-add 7.9% | hand tiny 22.3%, skip string 12.3%, movemask 10.9% | 4.809 / 5.741 | string/digest residual |
| `mesh` | -114 / -23 | direct array 21.9%, digit-run 14.4%, ws 12.7% | ws 20.8%, digit-run 18.3%, memcpy 8.3% | 5.409 / 5.354 | near-floor number/array residual |
| `random` | -185 / -929 | tiny8 23.8%, ws 17.9%, option copied 6.6% | hand tiny 20.2%, ws 16.9%, wrapping-add 8.5% | 5.564 / 5.980 | near-floor string/digest residual |
| `gsoc-2018` | -1072 / -1159 | movemask 22.9%, split-at 12.9%, tiny8 9.1% | movemask 21.6%, skip string 15.9%, split-at 12.7% | 3.112 / 3.226 | SIMD string-scan residual |
| `unicode_escapes` | -2096 / -2100 | unescape 25.1%, full string 22.1%, read hex 10.3% | unescape 23.4%, full string 22.0%, read hex 9.0% | 7.202 / 7.244 | unicode escape residual |
| `distinct_values` | -908 / -1033 | tiny8 22.1%, ws 15.8%, fold string 11.6% | hand tiny 19.4%, ws 16.5%, option copied 9.0% | 5.525 / 6.166 | tiny string plus digest residual |
| `y_string_unicode` | -1967 / -2921 | hex nibble 10.2%, read hex 10.0%, unescape 7.5% | read hex 16.1%, hex nibble 10.3%, unicode-run validate 8.4% | 9.912 / 11.492 | sparse unicode escape residual |

The direct residual surface groups cleanly:

- String/tiny rows: `twitter`, `github_events`, `update_center`, `random`,
  `distinct_values`.
- Number/array rows: `canada`, `mesh`.
- Unicode rows: `unicode_escapes`, `y_string_unicode`.
- SIMD-string support row: `gsoc-2018`, where `movemask_u8x16` is the top
  self-time leaf in both tracks.

No row here is a behavior-wave instruction. The table only says which leaves
later research must answer to if it chooses to reason about those rows.

### W0-Clamped Direct Rows

These rows are still `N-direct / NO-GO` in W0 even when Track 1 or both tracks
look above the computed floor. P1-F and W0 explicitly mark them as W0-clamped
non-admissions.

| Corpus | W0 gap T1/T2 Mbps | Direct Track 1 hot leaves | Direct Track 2 hot leaves | PMU c/B T1/T2 | Attribution |
|---|---:|---|---|---:|---|
| `instruments` | +2600 / +1767 | tiny8 15.7%, ws 15.7%, object direct 12.4% | ws 19.7%, hand tiny 13.1%, option copied 7.7% | 3.632 / 3.839 | guard-shaped but clamped non-admission |
| `numbers` | +2054 / -59 | digit-run 26.8%, direct array 13.4%, memcpy 10.1% | digit-run 27.5%, ws 11.5%, memcpy 9.8% | 3.794 / 3.952 | number row; Track 2 still short |
| `unicode_mixed` | +1165 / -161 | full string 28.0%, unescape 20.3%, validate escape 12.9% | full string 26.4%, unescape 18.4%, validate escape 13.8% | 9.039 / 9.222 | unicode row; Track 2 still short |

These are important cautions. Treating them as closed would be a paper close:
the row classifier did not admit them, and W0 says later passes must decide
whether they need measured admission, maintain gate, or demotion proof.

### Direct Guard Rows

Direct guards are existing `A / GO` rows. They should be preserved as guard
surfaces, not mined as permission to move behavior.

| Corpus | W0 state | Direct Track 1 hot leaves | Direct Track 2 hot leaves | PMU c/B T1/T2 | Guard attribution |
|---|---|---|---|---:|---|
| `citm_catalog` | `A / GO` | ws 25.9%, tiny8 14.3%, object direct 8.9% | ws 25.6%, hand tiny 13.6%, ascii spaces 8.1% | 2.054 / 2.155 | whitespace/string guard |
| `apache_builds` | `A / GO`, strict measured-row | wrapping-add 18.4%, tiny8 14.6%, ws 11.6% | wrapping-add 15.2%, hand tiny 14.1%, ws 12.7% | 3.851 / 4.224 | digest plus short-string guard |
| `marine_ik` | `A / GO` | digit-run 17.3%, direct array 16.3%, ws 11.2% | digit-run 17.8%, ws 15.1%, number span 7.9% | 4.910 / 4.907 | number/array guard |
| `unicode_basic` | `A / GO` | tiny8 15.7%, ws 11.0%, trailing-zeros 10.8% | hand tiny 15.7%, skip string 12.7%, hand string 11.4% | 3.844 / 4.302 | UTF-8 string guard |

### Typed Guard Rows

Typed rows are all `A / GO` in W0. For these rows, Track 1 is the generated
typed product path; Track 2 is the independent oracle and often profiles
`serde_json`. Track 2 typed symbols are therefore comparator/oracle evidence,
not generated-product hot leaves.

| Corpus | Typed Track 1 hot leaves | Typed Track 2 hot leaves | PMU c/B T1/T2 | Typed attribution |
|---|---|---|---:|---|
| `twitter` | typed skip plain 39.2%, ws 14.9%, NonNull eq 10.5% | serde whitespace 15.1%, serde skip escape 12.9%, serde peek 11.9% | 2.069 / 2.270 | typed string guard |
| `citm_catalog` | ws 35.7%, typed skip plain 18.0%, typed skip value 11.1% | serde peek 35.6%, serde whitespace 34.5%, serde discard 12.7% | 0.988 / 1.904 | typed whitespace/string guard |
| `apache_builds` | NonNull eq 26.0%, typed tiny 24.6%, ws 8.5% | NonNull eq 23.3%, UTF-8 validation 13.5%, wrapping-add 9.3% | 4.195 / 6.256 | typed tiny-string plus state compare |
| `github_events` | typed skip plain 28.5%, NonNull eq 20.5%, typed tiny 10.9% | NonNull eq 19.7%, serde skip escape 10.6%, UTF-8 validation 10.2% | 2.673 / 3.347 | typed string guard |
| `update_center` | typed skip plain 31.8%, typed tiny 15.7%, NonNull eq 12.8% | UTF-8 validation 17.4%, NonNull eq 13.4%, serde skip escape 10.8% | 3.047 / 3.859 | typed string/UTF-8 guard |
| `mesh` | digit-run 24.0%, ws 14.2%, number span 11.5% | serde peek 24.6%, serde discard 15.6%, serde decimal 13.0% | 4.840 / 5.830 | typed number guard |
| `marine_ik` | ws 17.3%, digit-run 17.1%, number span 13.5% | serde peek 23.5%, serde discard 16.2%, serde whitespace 13.8% | 3.599 / 3.936 | typed number/whitespace guard |

### Masking, Structural Scan, And Lazy-Tape Context

P1-C reports `host_call_eager_decode` as `MASKING` on all 17 corpora. The
largest eager/T1 ratios are `numbers` 8.37x, `unicode_escapes` 6.48x, and
`marine_ik` 4.68x. These rows identify eager decode/materialization cost
pressure; they do not admit a production path.

P1-C also reports:

| Diagnostic surface | Evidence | P1-E treatment |
|---|---|---|
| Structural scan only | no row reaches the 40000 Mbps aarch64 floor; `canada` is 14249 Mbps | nonproducer scan signal only |
| Cold first parse | `random` 2.20x T1 and `numbers` 4.27x T1 reported cold-sensitive | cold diagnostic, not direct close |
| Lazy tape | all 17 rows show zero payload bytes; allocation pressure peaks at `y_string_unicode` 0.75x input, `mesh` 0.72x, `marine_ik` 0.70x | substrate shape evidence only |
| Sparse escape flags | `unicode_mixed` 9795, `unicode_escapes` 9385, `y_string_unicode` 9000, `gsoc-2018` 8545 | string/unicode substrate signal |

The masking probes mostly reinforce the same hot families: string/unicode
decode, number scan/materialization, whitespace, and dispatch/structural
rediscovery. They are still diagnostic nonproducers.

### Pre-Blocked Substrate And W3 Routes

The dispatch and structural-rediscovery leaves are tempting because they point
at scalar delimiter rediscovery and event/tape consumption. That does not
reopen the W3 route family.

| Temptation from P1-E | Prior route status | Required caution |
|---|---|---|
| Replace scalar delimiter rediscovery with retained SIMD structural classes | REDRESS 96 rejected class-column plus move-consumed structural index; REDRESS 97 rejected streaming cursor; REDRESS 98 retired `G-W3-UNION-SUBSTRATE` | P1-E may name `consume_structural`/`parse_array_element_at_direct`, but may not prescribe a class column, sidecar vector, or streaming cursor |
| Add parser-owned cursor, event vector, whitespace bitmap, or aux projection | REDRESS 51 and 53 reject cursor/sidecar forms; REDRESS 98 says W3 is retired, not merely blocked | classify as substrate observation only |
| Use structural-scan-only throughput as a producer close | P1-C says structural scan is below floor and nonproducer | keep structural scan separate from direct and typed rows |
| Treat lazy-tape allocation or sparse flags as behavior admission | W0/P1-C say lazy-tape rows are substrate shape evidence with zero payload bytes | no direct row closes from lazy-tape facts alone |

Source: `skinny/REDRESS.md:2794` through `:2940`.

## Section 3 - Delta vs SK-V10

No SK-V10 machine-readable hot-leaf profile is available in this artifact, so
P1-E admits no symbol-level delta against SK-V10. The row-level delta comes
from P1-F:

| Row family | SK-V11 W0 state | Delta vs SK-V10 close | Hot-leaf consequence |
|---|---|---|---|
| Parse | 16 `S / NO-GO`, 1 `L / NO-GO` | `canada/parse_only` changed `S -> L`; all parse rows remain out of SOTA target | parse hot leaves are diagnostics only |
| Direct guards | 4 `A / GO` | two fewer direct admissions than SK-V10 close | guard leaves are preserve/monitor evidence |
| Direct residuals | 13 `N-direct / NO-GO` including W0-clamped rows | `instruments` and `numbers` no longer carry prior close admissions | direct residual leaves are research antecedents, not behavior permission |
| Typed guards | 7 `A / GO` | unchanged | typed leaves are guard-surface evidence |

The largest direct residual families after SK-V11 W0 are:

| Family | Rows | Evidence |
|---|---|---|
| Unicode/string escape | `unicode_escapes`, `unicode_mixed`, `y_string_unicode` | direct PMU c/B ranges from 7.202 to 11.492 and top leaves are `unescape_string`, `match_string_at_quote_trusted_utf8`, `read_hex_unit_scalar`, and `hex_nibble` |
| Tiny strings and digest | `twitter`, `github_events`, `update_center`, `random`, `distinct_values` | top leaves are tiny string matchers, whitespace, movemask, and digest hashing |
| Number arrays | `canada`, `mesh`, `numbers`, `marine_ik` | top leaves are digit-run scan, number span, array/object walks, and memcpy |
| Whitespace/dispatch | cross-cutting, especially `citm_catalog`, `instruments`, `mesh`, `numbers` | `skip_ascii_whitespace`, `dispatch_value`, and Track 2 `parse_value_at` recur across row families |

## Section 4 - Anomalies And No-Paper-Close Cautions

1. P1-A, P1-B, and P1-D appeared during validation and match the parse,
   direct, typed, xctrace, samply, and PMU evidence used here. P1-E still uses
   raw xctrace summaries plus PMU rows as numeric authority and treats the
   sibling files as corroboration.
2. P1-A records samply parse artifacts as present and `rc=0`, but also records
   `symbolicated=false` in the saved samply profile metadata. P1-E therefore
   cites samply artifacts as source files and xctrace summaries as the
   self-time percentage authority.
3. P1-D records all 82 PMU log captures as `rc=0`. The xctrace Time Profiler
   and CPU Counters trace entries mostly report `rc=54` because of the time
   limit path: 81 of 82 `xctrace-cpu-counters` rows and 81 of 82
   `xctrace-time-profiler` rows are `rc=54`. The exported summary JSONs still
   contain complete row coverage, so P1-E uses the exports but does not claim
   the xctrace return codes were clean exits.
4. PMU throughput does not replace Criterion. Example: PMU probe throughput
   for some parse/unicode rows is materially different from `skinny/RESULTS.md`
   Criterion Mbps. P1-E uses PMU only for c/B shape.
5. `y_string_unicode` direct xctrace process share is low relative to other
   direct rows: about 44% for Track 1 and 48% for Track 2 in the summary
   export. The top unicode leaves are still named, but the row has lower
   process-share confidence than the other direct rows.
6. Typed Track 2 hot leaves often name `serde_json` and core pointer/UTF-8
   helpers. Those are oracle/comparator hot leaves and must not be attributed
   to generated typed Track 1 behavior.
7. `NonNull` equality and `<u64>::wrapping_add` appear as top leaves in some
   typed/direct rows. P1-E classifies them as state/digest support overhead,
   not as grammar-neutral parser primitives.
8. `instruments`, `numbers`, and `unicode_mixed` are W0-clamped. Even where
   Track 1 is above the computed direct floor, the current row classifier is
   still `N-direct / NO-GO`.
9. P1-C's `host_call_eager_decode` masking signal fires on every corpus. It is
   a diagnostic cost probe, not an eager-decode behavior prescription.
10. Structural scan remains a nonproducer. The W3 union/class-column/streaming
   cursor family is pre-blocked or retired by REDRESS evidence and cannot be
   reintroduced from this hot-leaf attribution alone.

## Section 5 - Sources

- P1-A: `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md`.
- P1-B: `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`.
- P1-C: `restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md`.
- P1-D: `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md`.
- P1-F: `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md`.
- W0 baseline: `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`.
- Result authority: `skinny/RESULTS.md`.
- S-P1 prompt: `restart/prompts/skinny/PASS-1-PROFILE.md`.
- REDRESS W3 substrate record: `skinny/REDRESS.md:2794`.
- Parse xctrace summary: `/tmp/skv11-p1/parse-xctrace/exports/summary.json`.
- Direct xctrace summary: `/tmp/skv11-p1/direct-xctrace/exports/summary.json`.
- Parse PMU TSV: `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv`.
- Product PMU TSV: `/tmp/skv11-p1/pmu/product_pmu_rows.tsv`.
- Capture-status TSV: `/tmp/skv11-p1/pmu/capture_status.tsv`.
- Parse xctrace traces: `/tmp/skv11-p1/parse-xctrace/time-profiler/*.trace`.
- Direct xctrace traces: `/tmp/skv11-p1/direct-xctrace/time-profiler/*.trace`.
- PMU logs: `/tmp/skv11-p1/{parse-xctrace,direct-xctrace}/logs/*.pmu.log`.
- Samply raw artifacts listed in capture status:
  `/tmp/skv11-p1/samply/{parse,direct}/*.json.gz`.
- Runtime generated JSON source:
  `skinny/crates/runtime/src/grammars/json/generated.rs`.
- Track 2 JSON source:
  `skinny/crates/bbnf-bench/src/track2/json.rs`.
- Direct digest source:
  `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- Generated typed source:
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.
- Typed digest source:
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs`.
- Parse primitive source:
  `skinny/crates/parse-that-regex/src/lib.rs` and
  `skinny/crates/parse-that-regex/src/number/mod.rs`.
- SIMD movemask source:
  `skinny/crates/bbnf-simd/src/aarch64/movemask.rs`.
