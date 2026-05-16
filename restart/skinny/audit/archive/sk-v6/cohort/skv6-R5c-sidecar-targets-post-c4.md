# SK-V6 Wave 1c R5c - Sidecar Target Recalibration After Candidate4

Date: 2026-05-14.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: read-only. No repo files edited, staged, or committed.
Hard cap: 30 minutes. No full bench was run.

## Read Set

- `skinny/RESULTS.md` at current HEAD after Candidate4.
- `restart/skinny/audit/SK-V6-COHORT/skv6-R5-sidecar-refresh.md`.
- `restart/skinny/audit/SK-V6-COHORT/skv6-R5b-direct-bridge.md`.
- Existing Criterion output under `/tmp/skv6-wave2-candidate4-bench/criterion/`.
- Existing stale/default Criterion output under `skinny/target/criterion/` only for freshness checks.
- Existing native sidecar profile reports under `skinny/profile/{simdjson-expanded,yyjson,sonic-rs-expanded}/`.

## Method

I treated `skinny/RESULTS.md` as the current gate authority because it was refreshed by the Candidate4 advisory run. The matching Criterion artefacts exist under `/tmp/skv6-wave2-candidate4-bench/criterion/`; the default `skinny/target/criterion/` tree is older and should not be used as the post-Candidate4 authority.

I compared current `sonic-rs`, Rust `simd-json borrowed`, and Rust `simd-json owned` rows against the archived R5 sidecar snapshot. I used `>=5%` movement as material anchor drift. I did not rerun sidecars or full bench.

Thresholds below use the current BENCH outcome math:

- C substrate parity target: Track 2 Mbps >= `S / 1.05`.
- A beat target: Track 2 Mbps >= `S / 0.95`.
- Final strict SOTA-beat slack, for planning: Track 1 Mbps >= `1.10 * S`.

The table reports deltas against current post-Candidate4 rows. Positive deltas are additional Mbps needed.

## Finding

The remaining parse-G state is parser-owned, not explained by sidecar target churn.

Material sidecar movement exists in four rows, but it does not remove the parser diagnosis:

- `unicode_mixed`: current sonic-rs anchor fell from 15,681 to 11,981 Mbps (-23.6%). This made the row easier, yet Track 1 remains only 67.7% of S.
- `gsoc-2018`: Rust simd-json borrowed/owned fell -8.4%/-7.3%, but the binding S anchor is still sonic-rs and moved only +1.1%.
- `y_string_unicode`: Rust simd-json owned fell -8.1%, but the binding S anchor is still sonic-rs and moved only +0.1%.
- `mesh`: simd-json columns fell materially, but the row is already C/GO and is not a remaining parse-G blocker.

All other remaining parse-G rows have stable sidecar anchors. Their misses are bbnf parser throughput gaps.

## Remaining Parse-G Target Table

| Corpus | Class | Track1/S | Track2/S | Material anchor movement | T2 needed for C | Delta T2 to C | T2 needed for A | Delta T2 to A | T1 needed for +10% final | Delta T1 to +10% | Likely competitor hot shape |
|---|---|---:|---:|---|---:|---:|---:|---:|---:|---:|---|
| twitter | parser slow | 58.2% | 57.6% | none: sonic -0.1%, simd-b -0.0%, simd-o -0.4% | 20141 | +7958 | 22262 | +10079 | 23263 | +10945 | Current gate: sonic-rs fused Value parser. Native stale ceiling: yyjson `yyjson_read_opts` one-symbol fused parser; simdjson stage1 + UTF-8 checker. |
| citm_catalog | parser slow | 85.9% | 81.6% | none: sonic -0.1%, simd-b -0.6%, simd-o -1.5% | 24172 | +3458 | 26716 | +6002 | 27919 | +6108 | simdjson C++ stale ceiling is stage1-dominant structural classification; current gate still sonic-rs Value. |
| apache_builds | parser slow | 72.0% | 70.3% | none: sonic -0.5%, simd-b -3.2%, simd-o -4.9% | 16547 | +4341 | 18289 | +6083 | 19112 | +6601 | simdjson C++ stage1 structural classifier plus string visitor; current gate sonic-rs fused parser. |
| github_events | parser slow | 57.6% | 56.8% | none: sonic -1.4%, simd-b +0.9%, simd-o -1.0% | 21805 | +8803 | 24100 | +11098 | 25185 | +12001 | simdjson C++ stage1 structural classifier is stale native ceiling; current gate sonic-rs fused parser. |
| update_center | parser slow | 48.1% | 47.3% | none: sonic -3.0%, simd-b -2.0%, simd-o -3.3% | 18326 | +9223 | 20255 | +11152 | 21167 | +11908 | simdjson C++ stage1 near stage2; high string/object traversal pressure. |
| random | parser slow | 49.6% | 48.9% | none: sonic -0.3%, simd-b +1.0%, simd-o -1.1% | 14665 | +7133 | 16209 | +8677 | 16938 | +9299 | simdjson C++ stage1 plus UTF-8 checker; mixed object/string path. |
| gsoc-2018 | parser slow with non-binding simd drift | 44.4% | 44.2% | simd-b -8.4%, simd-o -7.3%; sonic +1.1% | 47016 | +25178 | 51965 | +30127 | 54303 | +32375 | Binding target is sonic-rs fused parser; no row-aligned native sidecar profile found. Gap too large to attribute to anchor drift. |
| instruments | parser slow | 63.3% | 60.6% | none: sonic +0.4%, simd-b -2.3%, simd-o -2.1% | 18847 | +6853 | 20831 | +8837 | 21768 | +9236 | Binding target is sonic-rs fused parser; object/string/number mixed row likely pays bbnf dispatch + string boundary overhead. |
| unicode_mixed | anchor easier but parser still slow | 67.7% | 72.5% | sonic -23.6%; simd stable | 11411 | +2719 | 12612 | +3920 | 13180 | +5073 | Native profiles point to string SIMD plus UTF-8 validation. The anchor moved down, but bbnf still misses C/A. |
| unicode_escapes | parser slow | 53.0% | 67.3% | none: sonic -2.1%, simd-b -0.1%, simd-o -0.6% | 17802 | +5227 | 19676 | +7101 | 20562 | +10654 | Sonic/simdjson both become stage2 string escape/codepoint dominated; bbnf is still slower despite same escape-complete plane. |
| unicode_basic | parser slow | 70.2% | 68.7% | none: sonic +0.3%, simd-b +1.6%, simd-o +0.5% | 15050 | +4191 | 16634 | +5775 | 17383 | +6291 | simdjson C++ stale ceiling is stage1 + UTF-8 checker; current gate sonic-rs Value. |
| distinct_values | parser slow | 34.7% | 34.4% | none: sonic -0.6%, simd-b -2.2%, simd-o -2.7% | 16884 | +10785 | 18662 | +12563 | 19501 | +13357 | simdjson C++ stage1 dominant plus `copy_and_find` string visitor; bbnf gap is not anchor movement. |
| y_string_unicode | parser slow with non-binding simd-owned drift | 46.0% | 43.2% | simd-o -8.1%; sonic +0.1%, simd-b +0.6% | 12995 | +7094 | 14363 | +8462 | 15009 | +8737 | Binding target is sonic-rs; simdjson profile shows stage2 string escape/codepoint dominance at roughly the same throughput as sonic. |

## Parser-Slow vs Anchor-Drift Classification

Parser-slow only:

- `twitter`
- `citm_catalog`
- `apache_builds`
- `github_events`
- `update_center`
- `random`
- `instruments`
- `unicode_escapes`
- `unicode_basic`
- `distinct_values`

Anchor moved materially but the row remains parser-slow:

- `unicode_mixed`: binding sonic-rs target got 23.6% easier; the row still misses C by +2719 Mbps on Track 2 and misses final +10% by +5073 Mbps on Track 1.
- `gsoc-2018`: only non-binding Rust simd-json anchors moved materially; sonic-rs remained stable and binding.
- `y_string_unicode`: only non-binding simd-json owned moved materially; sonic-rs remained stable and binding.

Not a remaining parse-G blocker but worth noting:

- `mesh`: Rust simd-json anchors moved down materially, but the row is already C/GO. Do not use mesh anchor drift to explain the parse-G set.

## Recalibrated Sidecar Interpretation

1. Current `RESULTS.md` S anchor is sonic-rs for every retained parse row. Against that gate, the remaining G rows need substrate/generator throughput recovery, not sidecar reinterpretation.
2. Stale native sidecars remain important for the eventual SOTA-beat claim. On `twitter`, `citm_catalog`, `apache_builds`, `github_events`, `update_center`, `random`, `unicode_basic`, and `distinct_values`, the stale simdjson C++/yyjson ceiling is above the current in-tree sonic-rs S anchor. Wave 4 must rerun those exact native rows before declaring strict SOTA-beat.
3. The likely competitor shape to beat is not one uniform kernel. The rows split into:
   - Stage1 structural/classifier dominance: `citm_catalog`, `apache_builds`, `github_events`, `update_center`, `distinct_values`.
   - Stage1 + UTF-8 validation pressure: `twitter`, `random`, `unicode_basic`, `unicode_mixed`, `y_string_unicode`.
   - Stage2 string escape/codepoint dominance: `unicode_escapes`, `y_string_unicode`.
   - Fused scalar/recursive parser dominance with no row-aligned native profile yet: `gsoc-2018`, `instruments`.
4. Candidate4 did not create a comparator-target anomaly. It improved bbnf container transition cost, but the sidecar anchors mostly stayed stable; the residual gaps are real parser gaps.

## Wave-2 Planning Implication

R5c does not recommend a new kernel by itself. It rules out a sidecar-anchor explanation for the current G rows.

The next Wave-2 candidate should therefore come from R1c/R2c/R3c PC attribution, not from sidecar recalibration. If those reports name a retained parser intervention, its falsifiability gate should target at least one of these rows where the required C delta is small enough to be plausible in one intervention:

- `unicode_mixed`: +2719 Mbps Track 2 to C, +5073 Mbps Track 1 to final +10%.
- `citm_catalog`: +3458 Mbps Track 2 to C, +6108 Mbps Track 1 to final +10%.
- `apache_builds`: +4341 Mbps Track 2 to C, +6601 Mbps Track 1 to final +10%.
- `unicode_basic`: +4191 Mbps Track 2 to C, +6291 Mbps Track 1 to final +10%.
- `unicode_escapes`: +5227 Mbps Track 2 to C, but +10654 Mbps Track 1 to final +10%; this likely needs string escape work, not container dispatch work.

Rows like `distinct_values`, `update_center`, `github_events`, `twitter`, and `gsoc-2018` require 7-25K Mbps of Track 2 recovery to reach C. They are diagnostic for broad architecture/fusion wins, not a realistic one-kernel close unless the PC attribution identifies a single overwhelming boundary.

## Provenance

- Current gate authority: `skinny/RESULTS.md`.
- Candidate4 Criterion artefacts: `/tmp/skv6-wave2-candidate4-bench/criterion/`.
- Previous R5 sidecar snapshot: `restart/skinny/audit/SK-V6-COHORT/skv6-R5-sidecar-refresh.md`.
- Direct/retained boundary note: `restart/skinny/audit/SK-V6-COHORT/skv6-R5b-direct-bridge.md`.
- simdjson C++ hot-shape profile: `skinny/profile/simdjson-expanded/PROFILE-REPORT.md`.
- yyjson hot-shape profile: `skinny/profile/yyjson/PROFILE-REPORT.md`.
- sonic-rs hot-shape profile subset: `skinny/profile/sonic-rs-expanded/PROFILE-REPORT.md`.

End of report.
