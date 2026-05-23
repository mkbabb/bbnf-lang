# SK-V14 P1-F: RESULTS Extraction + Δ vs SK-V13 Close
Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Baseline: SK-V14-open (audit-corrected SK-V13 close state; commit at HEAD; bench harness unchanged since SK-V13 close per A2 F9).
Host triple: aarch64-apple-darwin.
Build flags: profile=bench;rustflags=-C target-cpu=native;target_cpu=native (per `skinny/RESULTS.md:55-132` "Build flags" column).
Profile tool: extraction-only (P1-F is a documentary pass — no samply / cargo run; profile rows quoted from existing `skinny/RESULTS.md` SK-V9-open run id `sk-v9-open:criterion-fnv64-9d324a7ceab18d53`).
Corpus coverage: 51/51 JSON nominal cells × 3 planes + 31 CSS L4 rows (24 features + 7 covered-by aliases) = 82 row entries extracted; 51 JSON nominal − 6 MISSING real_typed_struct = 45 measured JSON rows; CSS L4 24 features fully covered.

## §1 — Method (verbatim, reproducible commands)

```bash
# (A) Inventory `skinny/RESULTS.md` rows
wc -l /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md          # 186
grep -c '^| ' /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md   # 123 pipe-prefixed lines
awk '/^\| / {n=gsub(/\|/,"|"); print NR" fields="n}' \
    /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md \
  | awk '$2=="fields=27"' | wc -l                                    # 47 parse-result rows (header @ L3/4 + L102 CSS)
awk '/^\| / {n=gsub(/\|/,"|"); print NR" fields="n}' \
    /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md \
  | awk '$2=="fields=23"' | wc -l                                    # 76 telemetry rows (header @ L53)

# (B) Inventory ROLLING-SOTA-DELTA SK-V13 close ledger
wc -l /Users/mkbabb/Programming/bbnf-lang/restart/skinny/ROLLING-SOTA-DELTA.md  # 99
# JSON rows L14-64 (51 cells), CSS L4 rows L70-93 (24 features)

# (C) SK-V14 schema-extension column probe
grep -c 'comparator_plane\|per_iter_equality\|audit_overlay_verdict\|track2_entry_point' \
        /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md         # 0 (all four NEW columns absent)

# (D) Audit prune-list source quote
sed -n '52,93p' /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
# PRUNE-1 reverts W14.1..5 (5 parse_only); PRUNE-2 reverts 24 CSS L4
# SK-V14 SYNTHESIS §0.2 + §1.2 widens direct 4→6 and typed 7→11 under the broader ledger.
```

The bench harness was not re-invoked for this pass. P1-F consumes the rows the SK-V13 close left in `skinny/RESULTS.md` + `restart/skinny/ROLLING-SOTA-DELTA.md` and overlays the SK-V14 audit verdict; per dispatch context §1, no SK-V14 implementation commits have landed so the Δ-vs-SK-V13-close on every numeric cell is ZERO (`A2 F9` negative-drift confirmation at `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:99-103`).

## §2 — Findings (per-row table with schema-v3 outcome + SK-V14 audit overlay)

### §2.1 JSON parse-result rows — 51-cell goalset

Schema-v3 outcome enum per `restart/skinny/tranches/sk-v8/SPEC.md §0.3 + §0.5`: `A` (admit) / `C` (close after redress) / `G` (gate pass without admit) / `K` (kept open, prior admit holds) / `L` (load-bearing failure under measured anchor) / `N-direct` (direct-digest miss) / `S` (substrate-guard non-admission). The SK-V14 audit overlay column `audit_overlay_verdict` is sourced from §1.2 of `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` cross-referenced with `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.2 + §1.2`.

| # | Row | Plane | Outcome | SK-V13 admit | SK-V14 verdict | Δ Mbps | Δ vs SK-V13 close | Audit cite |
|---:|---|---|---|---|---|---:|---|---|
| 1 | json/twitter/parse_only/main | parse_only | S | OPEN (`ROLLING:14`) | AUDIT-PENDING (parse_only path absent) | 0 (15561 / 21013) | none | n/a — was already OPEN |
| 2 | json/twitter/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:15`) | AUDIT-PENDING | 0 (11908 / 15110) | none | n/a |
| 3 | json/twitter/real_typed_struct/main | real_typed_struct | A | ADMITTED (`ROLLING:16`) | **AUDIT-FALSIFIED** (broader 11 typed ledger; `v6 §1 rows 3-4`) | 0 (17898 / 15502) | none | `sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1.2; sk-v14/SYNTHESIS.md §0.2 ¶3` |
| 4 | json/citm_catalog/parse_only/main | parse_only | A | ADMITTED via W14.2 (`ROLLING:17`) | **AUDIT-FALSIFIED** (PRUNE-1, `v2 §1`) | 0 (30150 / 25565) | none | `sk-v13/audit-overfit:39-40, 82-88; REDRESS.md:4802` |
| 5 | json/citm_catalog/direct_to_struct/main | direct_to_struct | A | ADMITTED (`ROLLING:18`) | **AUDIT-FALSIFIED** (4-narrow + 6-broad ledger; `v2 §3.2`) | 0 (21414 / 19938) | none | `sk-v13/audit-overfit §1.2; sk-v14/SYNTHESIS.md §1.2 ¶1` |
| 6 | json/citm_catalog/real_typed_struct/main | real_typed_struct | A | ADMITTED (`ROLLING:19`) | **AUDIT-FALSIFIED** (carry-over; `v2 §4.2`) | 0 (36719 / 22857) | none | `sk-v13/audit-overfit §1.2; sk-v14/SYNTHESIS.md §1.2 ¶2` |
| 7 | json/canada/parse_only/main | parse_only | A | ADMITTED via W14.3 (`ROLLING:20`) | **AUDIT-FALSIFIED** (PRUNE-1) | 0 (16977 / 14101) | none | `sk-v13/audit-overfit:82-88; REDRESS.md` (W14.3 close) |
| 8 | json/canada/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:21`) | AUDIT-PENDING | 0 (10962 / 12205) | none | n/a |
| 9 | json/canada/real_typed_struct/main | real_typed_struct | **MISSING** | MISSING (`ROLLING:22`) | AUDIT-PENDING (product-surface-not-generated) | n/a | none | row absent from RESULTS.md parse-result table — schema gap |
| 10 | json/apache_builds/parse_only/main | parse_only | S | OPEN (`ROLLING:23`) | AUDIT-PENDING | 0 (12767 / 17351) | none | n/a |
| 11 | json/apache_builds/direct_to_struct/main | direct_to_struct | A | ADMITTED W2 (`ROLLING:24`) | **AUDIT-FALSIFIED** (4-narrow ledger; `v2 §3.2`) | 0 (11428 / 11105) | none | `sk-v13/audit-overfit §1.2` |
| 12 | json/apache_builds/real_typed_struct/main | real_typed_struct | A | ADMITTED (`ROLLING:25`) | **AUDIT-FALSIFIED** (carry-over; `v2 §4.2`) | 0 (8127 / 8091) | none | `sk-v13/audit-overfit §1.2` |
| 13 | json/github_events/parse_only/main | parse_only | S | OPEN (`ROLLING:26`) | AUDIT-PENDING | 0 (14966 / 23009) | none | n/a |
| 14 | json/github_events/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:27`) | AUDIT-PENDING | 0 (12483 / 16197) | none | n/a |
| 15 | json/github_events/real_typed_struct/main | real_typed_struct | A | ADMITTED W6 (`ROLLING:28`) | **AUDIT-FALSIFIED** (typed-narrow ledger; `v2 §4.2`) | 0 (13040 / 12627) | none | `sk-v13/audit-overfit §1.2` |
| 16 | json/update_center/parse_only/main | parse_only | S | OPEN (`ROLLING:29`) | AUDIT-PENDING | 0 (11791 / 19661) | none | n/a |
| 17 | json/update_center/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:30`) | AUDIT-PENDING | 0 (8546 / 11183) | none | n/a |
| 18 | json/update_center/real_typed_struct/main | real_typed_struct | A | ADMITTED W15.1 (`ROLLING:31`) | **AUDIT-FALSIFIED** (W15.1 adjusted typed; broader 11-row ledger) | 0 (13191 / 12623) | none | `sk-v14/SYNTHESIS.md §0.2 ¶3, §1.2`; `REDRESS.md` W15.1 |
| 19 | json/mesh/parse_only/main | parse_only | A | ADMITTED W14.5 (`ROLLING:32`) | **AUDIT-FALSIFIED** (PRUNE-1) | 0 (12987 / 11758) | none | `sk-v13/audit-overfit:82-88` |
| 20 | json/mesh/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:33`) | AUDIT-PENDING | 0 (9661 / 9757) | none | n/a |
| 21 | json/mesh/real_typed_struct/main | real_typed_struct | A | ADMITTED (`ROLLING:34`) | **AUDIT-FALSIFIED** (typed-narrow ledger) | 0 (9686 / 8867) | none | `sk-v13/audit-overfit §1.2` |
| 22 | json/random/parse_only/main | parse_only | S | OPEN (`ROLLING:35`) | AUDIT-PENDING | 0 (9946 / 15665) | none | n/a |
| 23 | json/random/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:36`) | AUDIT-PENDING | 0 (7801 / 8944) | none | n/a |
| 24 | json/random/real_typed_struct/main | real_typed_struct | A | ADMITTED W13.3 (`ROLLING:37`) | **AUDIT-FALSIFIED** (typed-broader ledger; `sk-v14/SYNTHESIS §1.2`) | 0 (8151 / 7393) | none | `sk-v14/SYNTHESIS.md §1.2 ¶2; REDRESS.md:4542` |
| 25 | json/gsoc-2018/parse_only/main | parse_only | S | OPEN (`ROLLING:38`) | AUDIT-PENDING | 0 (23587 / 50363) | none | n/a |
| 26 | json/gsoc-2018/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:39`) | AUDIT-PENDING | 0 (15385 / 23880) | none | n/a |
| 27 | json/gsoc-2018/real_typed_struct/main | real_typed_struct | **MISSING** | MISSING (`ROLLING:40`) | AUDIT-PENDING (product-surface-not-generated) | n/a | none | row absent from RESULTS.md parse-result table |
| 28 | json/marine_ik/parse_only/main | parse_only | A | ADMITTED W14.4 (`ROLLING:41`) | **AUDIT-FALSIFIED** (PRUNE-1) | 0 (12357 / 9902) | none | `sk-v13/audit-overfit:82-88` |
| 29 | json/marine_ik/direct_to_struct/main | direct_to_struct | A | ADMITTED (`ROLLING:42`) | **AUDIT-FALSIFIED** (direct-broader ledger; `sk-v14/SYNTHESIS §1.2` +2 row) | 0 (10513 / 8454) | none | `sk-v14/SYNTHESIS.md §0.2 ¶3 ("+2 direct extension rows are marine_ik and instruments")` |
| 30 | json/marine_ik/real_typed_struct/main | real_typed_struct | A | ADMITTED (`ROLLING:43`) | **AUDIT-FALSIFIED** (typed carry-over) | 0 (12164 / 9198) | none | `sk-v13/audit-overfit §1.2` |
| 31 | json/instruments/parse_only/main | parse_only | S | OPEN (`ROLLING:44`) | AUDIT-PENDING | 0 (17468 / 19630) | none | n/a |
| 32 | json/instruments/direct_to_struct/main | direct_to_struct | A | ADMITTED W10 (`ROLLING:45`) | **AUDIT-FALSIFIED** (direct-broader +2 row) | 0 (12060 / 12731) | none | `sk-v14/SYNTHESIS.md §0.2 ¶3` (marine_ik + instruments are the +2 direct rows) |
| 33 | json/instruments/real_typed_struct/main | real_typed_struct | A | ADMITTED W13.4 (`ROLLING:46`) | **AUDIT-FALSIFIED** (typed-broader ledger; W13.4) | 0 (21464 / 16209) | none | `sk-v14/SYNTHESIS.md §1.2 ¶2; REDRESS.md:4580` |
| 34 | json/numbers/parse_only/main | parse_only | A | ADMITTED W14.1 (`ROLLING:47`) | **AUDIT-FALSIFIED** (PRUNE-1) | 0 (19267 / 13666) | none | `sk-v13/audit-overfit:82-88; REDRESS.md:4767` |
| 35 | json/numbers/direct_to_struct/main | direct_to_struct | A | ADMITTED W2 (`ROLLING:48`) | **AUDIT-FALSIFIED** (direct-narrow ledger; `v2 §3.2`) | 0 (14125 / 12747) | none | `sk-v13/audit-overfit §1.2` |
| 36 | json/numbers/real_typed_struct/main | real_typed_struct | A | ADMITTED W13.1 (`ROLLING:49`) | **AUDIT-FALSIFIED** (typed-broader ledger; W13.1) | 0 (13281 / 12249) | none | `sk-v14/SYNTHESIS.md §1.2 ¶2; REDRESS.md:4463` |
| 37 | json/unicode_mixed/parse_only/main | parse_only | S | OPEN (`ROLLING:50`) | AUDIT-PENDING | 0 (9294 / 18858) | none | n/a |
| 38 | json/unicode_mixed/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:51`) | AUDIT-PENDING | 0 (5062 / 10653) | none | n/a |
| 39 | json/unicode_mixed/real_typed_struct/main | real_typed_struct | **MISSING** | MISSING (`ROLLING:52`) | AUDIT-PENDING (product-surface-not-generated) | n/a | none | row absent from RESULTS.md parse-result table |
| 40 | json/unicode_escapes/parse_only/main | parse_only | S | OPEN (`ROLLING:53`) | AUDIT-PENDING | 0 (13550 / 19273) | none | n/a |
| 41 | json/unicode_escapes/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:54`) | AUDIT-PENDING | 0 (5523 / 14298) | none | n/a |
| 42 | json/unicode_escapes/real_typed_struct/main | real_typed_struct | **MISSING** | MISSING (`ROLLING:55`) | AUDIT-PENDING (product-surface-not-generated) | n/a | none | row absent from RESULTS.md parse-result table |
| 43 | json/unicode_basic/parse_only/main | parse_only | S | OPEN (`ROLLING:56`) | AUDIT-PENDING | 0 (12041 / 16125) | none | n/a |
| 44 | json/unicode_basic/direct_to_struct/main | direct_to_struct | A | ADMITTED (`ROLLING:57`) | **AUDIT-FALSIFIED** (direct-narrow ledger; `v2 §3.2`) | 0 (9317 / 8976) | none | `sk-v13/audit-overfit §1.2` |
| 45 | json/unicode_basic/real_typed_struct/main | real_typed_struct | A | ADMITTED W13.2 (`ROLLING:58`) | **AUDIT-FALSIFIED** (typed-broader ledger; W13.2) | 0 (6753 / 6044) | none | `sk-v14/SYNTHESIS.md §1.2 ¶2; REDRESS.md:4503` |
| 46 | json/distinct_values/parse_only/main | parse_only | S | OPEN (`ROLLING:59`) | AUDIT-PENDING | 0 (9920 / 18160) | none | n/a |
| 47 | json/distinct_values/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:60`) | AUDIT-PENDING | 0 (6540 / 11948) | none | n/a |
| 48 | json/distinct_values/real_typed_struct/main | real_typed_struct | **MISSING** | MISSING (`ROLLING:61`) | AUDIT-PENDING (product-surface-not-generated) | n/a | none | row absent from RESULTS.md parse-result table |
| 49 | json/y_string_unicode/parse_only/main | parse_only | S | OPEN (`ROLLING:62`) | AUDIT-PENDING | 0 (6590 / 13860) | none | n/a |
| 50 | json/y_string_unicode/direct_to_struct/main | direct_to_struct | N-direct | OPEN (`ROLLING:63`) | AUDIT-PENDING | 0 (5061 / 8998) | none | n/a |
| 51 | json/y_string_unicode/real_typed_struct/main | real_typed_struct | **MISSING** | MISSING (`ROLLING:64`) | AUDIT-PENDING (product-surface-not-generated) | n/a | none | row absent from RESULTS.md parse-result table |

**JSON tally:**
- 45 measured rows + 6 MISSING rows = 51 nominal cells × 3 planes ✔ matches `SK-V14 SYNTHESIS §0.2`.
- 17 AUDIT-FALSIFIED (5 parse_only W14.* + 6 direct + 6 typed under broader ledger). Outcome breakdown of the 45 measured: 5 S (substrate non-admit, all parse_only OPEN), 5 A parse_only (W14.1-5 → falsified), 6 A direct (W2/W10/W12 carry-overs → falsified), 11 A real_typed_struct (W6/W13.*/W15.1/carry-overs → falsified), 8 N-direct (digest miss).
- Δ Mbps and Δ vs SK-V13 close on every cell = 0 because no SK-V14 implementation has landed (the 17 doc commits between SK-V13 close `00181742e` and SK-V14 dispatch seed `12ff0744e` touched no parser/codegen/runtime/grammar bytes per `sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:70-74`).

### §2.2 CSS L4 rows — 24-feature goalset

CSS L4 in `skinny/RESULTS.md` lives almost entirely in the telemetry section (lines 102-132). Only `css_l4/declaration_values/direct_to_struct/main` (line 102) appears as a parse-result-shaped row; the remaining 30 CSS rows are telemetry-format only. The 24-feature goalset per `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.2` is fully populated as ADMITTED across the SK-V12 W1b through SK-V13 W10.3 wave family; every row is AUDIT-FALSIFIED per `sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §"CSS L4 — fake admissions"` (PRUNE-2).

| # | CSS L4 row | Source wave | SK-V13 admit | SK-V14 verdict | Mbps T1 / lightningcss | Δ vs SK-V13 close | Audit cite |
|---:|---|---|---|---|---:|---|---|
| 1 | css_l4/declaration_values/direct_to_struct/main | SK-V12-W1b-2b | ADMITTED (`ROLLING:70`) | **AUDIT-FALSIFIED** (PRUNE-2: hand-written template, 187 B fixture, P-1/P-3) | 434.13 / 168.23 | none | `sk-v13/audit-overfit:23-30, 99-113`; `v1 §1 + §5` |
| 2 | css_l4/declarations/direct_to_struct/main | SK-V13-W3 (covered-by extended) | ADMITTED (`ROLLING:71`) | **AUDIT-FALSIFIED** (covered-by alias, fixture-lookup) | 265.72 / 55.91 | none | `sk-v13/audit-overfit:23-30`; `v1 §1` |
| 3 | css_l4/stylesheet_root/direct_to_struct/main | SK-V13-W2 (covered-by stylesheet_and_selectors) | ADMITTED (`ROLLING:72`) | **AUDIT-FALSIFIED** (P-3 tiny 117 B fixture, P-7 covered-by alias) | 26894.88 / 595.05 | none | `sk-v13/audit-overfit:26 SUSPICIOUS W2`; `v1 §3` |
| 4 | css_l4/selectors/direct_to_struct/main | SK-V13-W2 (covered-by) | ADMITTED (`ROLLING:73`) | **AUDIT-FALSIFIED** (P-3 tiny fixture, P-7 alias) | 26894.88 / 595.05 | none | `sk-v13/audit-overfit:26`; `v1 §3` |
| 5 | css_l4/at_rules_keyframes/direct_to_struct/main | SK-V13-W10.1 (covered-by at_rules_and_media) | ADMITTED (`ROLLING:74`) | **AUDIT-FALSIFIED** (P-3 + fixture-lookup A4 NEW-2) | 21584.64 / 254.22 | none | `sk-v14/audit-overfit:120-134` (A4 NEW-2: 4 fixture-lookup scanners) |
| 6 | css_l4/nested_rules/direct_to_struct/main | SK-V13-W10.3 (covered-by nested_layout) | ADMITTED (`ROLLING:75`) | **AUDIT-FALSIFIED** (W10.3 = OVERFIT 124× lightningcss; `v1 §3 §6`) | 52233.54 / 422.16 | none | `sk-v13/audit-overfit:25 (W10.3 OVERFIT)`; `sk-v14/SYNTHESIS §0.4 P-1 round-trip rule trigger` |
| 7 | css_l4/css_variables/direct_to_struct/main | SK-V13-W3 (covered-by extended) | ADMITTED (`ROLLING:76`) | **AUDIT-FALSIFIED** (PRUNE-2; A4 NEW-2 fixture-lookup) | 265.72 / 55.91 | none | `sk-v14/audit-overfit:120-134` |
| 8 | css_l4/calc_expressions/direct_to_struct/main | SK-V13-W3 (covered-by) | ADMITTED (`ROLLING:77`) | **AUDIT-FALSIFIED** (PRUNE-2) | 265.72 / 55.91 | none | `sk-v13/audit-overfit:23-30` |
| 9 | css_l4/var_url_functions/direct_to_struct/main | SK-V13-W3 (covered-by) | ADMITTED (`ROLLING:78`) | **AUDIT-FALSIFIED** (PRUNE-2) | 265.72 / 55.91 | none | `sk-v13/audit-overfit:23-30` |
| 10 | css_l4/color_functions/direct_to_struct/main | SK-V13-W3 (covered-by) | ADMITTED (`ROLLING:79`) | **AUDIT-FALSIFIED** (PRUNE-2) | 265.72 / 55.91 | none | `sk-v13/audit-overfit:23-30` |
| 11 | css_l4/gradients/direct_to_struct/main | SK-V13-W4 (covered-by visual_functions) | ADMITTED (`ROLLING:80`) | **AUDIT-FALSIFIED** (PRUNE-2; 357 B fixture) | 225.89 / 115.53 | none | `sk-v13/audit-overfit:23-30` |
| 12 | css_l4/transforms/direct_to_struct/main | SK-V13-W4 (covered-by) | ADMITTED (`ROLLING:81`) | **AUDIT-FALSIFIED** (PRUNE-2) | 225.89 / 115.53 | none | `sk-v13/audit-overfit:23-30` |
| 13 | css_l4/filters/direct_to_struct/main | SK-V13-W4 (covered-by) | ADMITTED (`ROLLING:82`) | **AUDIT-FALSIFIED** (PRUNE-2) | 225.89 / 115.53 | none | `sk-v13/audit-overfit:23-30` |
| 14 | css_l4/easing_functions/direct_to_struct/main | SK-V13-W4 (covered-by) | ADMITTED (`ROLLING:83`) | **AUDIT-FALSIFIED** (PRUNE-2) | 225.89 / 115.53 | none | `sk-v13/audit-overfit:23-30` |
| 15 | css_l4/media_queries/direct_to_struct/main | SK-V13-W10.1 (covered-by at_rules_and_media) | ADMITTED (`ROLLING:84`) | **AUDIT-FALSIFIED** (P-3 + A4 NEW-2 fixture-lookup) | 21584.64 / 254.22 | none | `sk-v14/audit-overfit:120-134` |
| 16 | css_l4/vendor_prefixes/direct_to_struct/main | SK-V13-W10.2 (covered-by vendor_and_custom_atrules) | ADMITTED (`ROLLING:85`) | **AUDIT-FALSIFIED** (W10.2 SUSPICIOUS; A4 NEW-2) | 34635.22 / 278.74 | none | `sk-v13/audit-overfit:26`; `sk-v14/audit-overfit:120-134` |
| 17 | css_l4/custom_at_rules/direct_to_struct/main | SK-V13-W10.2 (covered-by) | ADMITTED (`ROLLING:86`) | **AUDIT-FALSIFIED** (P-3 + A4 NEW-2) | 34635.22 / 278.74 | none | `sk-v13/audit-overfit:26`; `sk-v14/audit-overfit:120-134` |
| 18 | css_l4/pseudo_classes/direct_to_struct/main | SK-V13-W2 (covered-by stylesheet_and_selectors) | ADMITTED (`ROLLING:87`) | **AUDIT-FALSIFIED** (P-3 117 B fixture; A4 NEW-2) | 26894.88 / 596.05 | none | `sk-v13/audit-overfit:26`; `sk-v14/audit-overfit:120-134` |
| 19 | css_l4/pseudo_elements/direct_to_struct/main | SK-V13-W2 (covered-by) | ADMITTED (`ROLLING:88`) | **AUDIT-FALSIFIED** (P-3; A4 NEW-2) | 26894.88 / 596.05 | none | `sk-v13/audit-overfit:26`; `sk-v14/audit-overfit:120-134` |
| 20 | css_l4/attribute_selectors/direct_to_struct/main | SK-V13-W2 (covered-by) | ADMITTED (`ROLLING:89`) | **AUDIT-FALSIFIED** (P-3; A4 NEW-2) | 26894.88 / 596.05 | none | `sk-v13/audit-overfit:26`; `sk-v14/audit-overfit:120-134` |
| 21 | css_l4/logical_properties/direct_to_struct/main | SK-V13-W10.3 (covered-by nested_layout) | ADMITTED (`ROLLING:90`) | **AUDIT-FALSIFIED** (W10.3 OVERFIT 124× per `v1 §3 §6`) | 52233.54 / 422.16 | none | `sk-v13/audit-overfit:25` |
| 22 | css_l4/grid/direct_to_struct/main | SK-V13-W10.3 (covered-by) | ADMITTED (`ROLLING:91`) | **AUDIT-FALSIFIED** (W10.3 OVERFIT) | 52233.54 / 422.16 | none | `sk-v13/audit-overfit:25` |
| 23 | css_l4/flexbox/direct_to_struct/main | SK-V13-W10.3 (covered-by) | ADMITTED (`ROLLING:92`) | **AUDIT-FALSIFIED** (W10.3 OVERFIT) | 52233.54 / 422.16 | none | `sk-v13/audit-overfit:25` |
| 24 | css_l4/typed_property_groups/direct_to_struct/main | SK-V13-W10.3 (covered-by) | ADMITTED (`ROLLING:93`) | **AUDIT-FALSIFIED** (W10.3 OVERFIT) | 52233.54 / 422.16 | none | `sk-v13/audit-overfit:25` |

**CSS tally:** 24/24 AUDIT-FALSIFIED. PRUNE-2 reverts the full population per `sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:99-113`. Seven additional "covered-by" telemetry-only rows beyond the 24 features (declaration_values_extended, stylesheet_and_selectors, at_rules_and_media, visual_functions, vendor_and_custom_atrules, nested_layout, declarations) also AUDIT-FALSIFIED under the same PRUNE-2 sweep; they are the seven hand-written `include_str!()` template heads themselves (per `sk-v13/audit-overfit:23`).

### §2.3 Outcome enum classification census

Per `restart/skinny/tranches/sk-v8/SPEC.md §0.3 + §0.5` enum (A / C / G / K / L / N-direct / S):

| Outcome | Count (post-audit) | Note |
|---|---:|---|
| A — admit | **0** (45 nominal "A" rows all reclassify per audit overlay) | per SK-V14 SYNTHESIS §1.3 honest baseline `0/17 / 0/17 / 0/17 / 0/24` |
| S — substrate-guard non-admission | 11 (all parse_only OPEN rows that were not gate-relabelled) | twitter, apache_builds, github_events, update_center, random, gsoc-2018, instruments, unicode_mixed, unicode_escapes, unicode_basic, distinct_values, y_string_unicode parse_only |
| N-direct — direct-digest miss | 8 (twitter, canada, github_events, update_center, mesh, random, gsoc-2018, unicode_mixed, unicode_escapes, distinct_values, y_string_unicode direct_to_struct) | listed 11 here per RESULTS.md L5-49; tally 11 |
| L — load-bearing failure | 0 | absent at SK-V13 close |
| C — close after redress | 0 | absent at SK-V13 close |
| G — gate pass without admit | 0 | absent at SK-V13 close |
| K — kept open / prior admit holds | 0 | absent at SK-V13 close |
| **MISSING** (non-enum) | 6 real_typed_struct rows | canada, gsoc-2018, unicode_mixed, unicode_escapes, distinct_values, y_string_unicode |

Note: SPEC §0.5 enumerates the SK-V8 opening as `16 S / 1 L parse_only · 3 A / 14 N-direct direct · 4 A typed`. SK-V13 close moved 5 S→A parse_only (W14.*) and added typed admits W13.* / W15.1; the audit-overlay column reverts every A → reopened OPEN-equivalent without altering the parse-result enum cell. Per the SK-V14 SYNTHESIS §0.2 honest baseline, all 45 measured rows + 24 CSS rows + 6 MISSING rows = 75 total population (matches dispatch instruction).

## §3 — Delta vs SK-V13 close (per row)

Tabular Δ is presented in §2.1 + §2.2 column "Δ vs SK-V13 close". The summary:

- **Per-row Δ Mbps = 0 on every cell.** No SK-V14 implementation commit has landed; the 17 commits between SK-V13 close (`00181742e`) and SK-V14 S-P1 dispatch (`ff653fbe6`, `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md:3`) are doc / synthesis / audit-pack commits only (`sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:99-103` A2 F9 negative-drift). The bench harness, comparator binding (`benches/json_parity.rs:87-102` single `sonic_rs_anchor` lane), and tape/runtime substrate are byte-identical to the SK-V13 close state.
- **Per-row Δ verdict ≠ 0 on every admitted cell.** The Δ that matters is the **audit-overlay verdict swap**: 5 parse_only A → AUDIT-FALSIFIED (PRUNE-1), 4 direct A (narrow ledger) or 6 direct A (broader ledger) → AUDIT-FALSIFIED, 7 typed A (narrow) or 11 typed A (broader) → AUDIT-FALSIFIED, 24 CSS L4 A → AUDIT-FALSIFIED (PRUNE-2). The remaining 51 JSON cells × 3 planes minus admits + 24 CSS cells reach AUDIT-PENDING.
- **Per-row Δ ROLLING-SOTA-DELTA reconciliation:** `restart/skinny/ROLLING-SOTA-DELTA.md:8` records `source_commit: 653cdf795+w15.1-redress` — the SK-V13 close pin. Every cell with `tranche_admitted=ADMITTED` (16 JSON + 24 CSS rows by line count) is the falsification surface. The dispatch context's narrow census (4 direct + 7 typed) corresponds to the SK-V14 SYNTHESIS §0.2 ¶3 reconciliation: the actual ROLLING-SOTA-DELTA ledger carries the broader 6 direct + 11 typed; PRUNE-1 must revert the wider population.

The "Δ vs SK-V13 close" in cell sense is therefore (a) numerically zero, (b) categorically a verdict downgrade on 40 of 75 rows in the narrow reading or 46 of 75 in the broader reading.

## §4 — Anomalies, masking signals, telemetry gaps (flagged for S-P2)

### §4.1 SK-V14 schema-extension columns absent

Per SK-V14 SYNTHESIS §2 telemetry binding, three NEW columns are mandatory on every row:

| Column | Origin | Present in current RESULTS.md? | Cells affected | Source |
|---|---|---|---|---|
| `comparator_plane` | R1 (per-plane strict comparator) | **No** — zero literal-string matches across 186 lines | 45 JSON measured + 31 CSS = 76 rows × column gap | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:241` |
| `per_iter_equality` | R2 (per-iter equality oracle inside timing region) | **No** — zero matches | 76 rows × column gap | `SYNTHESIS.md:242` |
| `audit_overlay_verdict` | audit overlay (the column this artefact populates) | **No** — zero matches | 75 rows (51 JSON cells + 24 CSS features) × column gap | `SYNTHESIS.md:255` |
| `track2_entry_point` | CH5 hidden-coupling guard | **No** — zero matches | 76 rows × column gap | `SYNTHESIS.md:240` |

The schema gap is total: `xtask gate-json` (per SYNTHESIS.md:230 "rejects any row missing required columns") would reject every row at SK-V14 open. R1 + R2 + the audit-overlay scribe land these columns in the C-2 wave; the SCHEMA itself must be rewritten in `skinny/RESULTS.md` (header line 3) before the bench harness emits them.

### §4.2 SPEC §0.4 (SK-V8 27-field manifest) compliance per row

The SK-V8 SPEC §0.4 enumerated 27 required telemetry fields. Cross-reference against the SK-V13 close telemetry manifest (`skinny/RESULTS.md:53-132`):

| §0.4 field | Present in telemetry manifest? | Coverage | Source |
|---|---|---|---|
| `row_id`/`Row id` | Yes (col 1) | all 76 rows | `RESULTS.md:55` |
| `grammar_id`/`Grammar` | Yes (col 2) | 76 rows | `RESULTS.md:55` |
| `domain` | Yes (col 3) | 76 rows | `RESULTS.md:55` |
| `comparator_id` / `wave id` / `Run id` | Partial — `Wave` (col 4) + `Run id` (col 5) present; `comparator_id` collapsed into `Comparator evidence` blob (col 22) | 76 rows; gate-json must parse blob — fragile | `RESULTS.md:55, 102-132` |
| `comparator_plane` | **Absent** (SK-V14 NEW; per §4.1) | 0/76 | gap |
| `comparator_strictness` | Encoded inside `Comparator evidence` blob (`strictness=strict`); not a column | 76 rows partial | `RESULTS.md:55` |
| `comparator_freshness` | Encoded inside blob (`freshness=same-run-native`/`historical:sk-v7-sidecar-profile`/`absent:not-collected`) | 76 rows partial | `RESULTS.md:55` |
| `measured_validation_path` / `Validation` (col 6) | Yes | 76 rows | `RESULTS.md:55` |
| `Profile artifact` (col 7) | Yes | 76 rows; CSS uses `n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion` for one row (line 103) — stale-artifact flag | `RESULTS.md:55, 103` |
| `Cycles per byte` / `Sample cost` (col 8) | Yes via `ns_per_byte=` substring; native column is `Sample cost` | 76 rows; cycles/byte NOT directly populated — JSON rows have `ns_per_byte`, CSS has `track1_mean_ns` mixed dimensions | `RESULTS.md:55, 102-132` |
| `Sample count` (col 9) | Yes; JSON=100 or 50, CSS=30 | 76 rows | `RESULTS.md:55-99, 102-132` |
| `Build flags` (col 10) | Yes; identical across all rows | 76 rows | `RESULTS.md:55` |
| `Host triple` (col 11) | Yes; `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` (JSON) vs `apple-m5-max` (CSS) — **schema inconsistency** between blocks | 76 rows; format drift | `RESULTS.md:55, 102` |
| `Feature mask` (col 12) | Yes; JSON `simd=Scalar`, CSS `simd=neon` or `simd=scalar-cssparser` — **drift** suggests SIMD path attribution not unified | 76 rows; drift | `RESULTS.md:55, 102, 116` |
| `CostFacts rule id` / `chosen shape` / `rejected alternative ids` | **Absent as discrete columns**; CSS rows encode `schema=sk-v12-css-l4-sota-v1;outcome=A;verdict=GO;...` blob (col 13) | 0/76 directly; 31 CSS partial via blob | gap on JSON, partial on CSS |
| `Redress entry` / `Redress` (col 14) | Yes | 76 rows | `RESULTS.md:55` |
| `Wave id` | Yes (col 4) | 76 rows | as above |
| `Run id` (col 5) | Yes | 76 rows | as above |
| `Sidecar freshness` | Encoded inside `Comparator evidence` blob (`sidecar=…`); not a column | 76 partial | `RESULTS.md:55` |
| `SK-V8-open delta` / `SK-V9-open delta` (col 15) | Yes | 45 JSON rows; CSS rows use `new-nonjson-row:…` instead — schema drift | `RESULTS.md:55, 102` |
| `substrate_surface` / `Substrate` (col 16) | Yes (`borrowed_view_over_offset_tape`/`sink_only_digest`/`typed_direct_projection`/`generated_css_l4_*`) | 76 rows | `RESULTS.md:55, 102` |
| `structural_projection_status` / `Structural projection` (col 17) | Yes | 45 JSON; CSS uses fact-stream names | `RESULTS.md:55, 102` |
| `substrate_cardinality` / `Cardinality` (col 18) | Yes | 76 rows | `RESULTS.md:55` |
| `same_wave_consumer_class` / `Consumer` (col 19) | Yes (`gate_only`/`generated_json_parse_only_contract`/`companion_gate_*`) | 76 rows | `RESULTS.md:55, 102` |
| `track2_independence_status` / `Track 2` (col 20) | Yes; JSON `independent_verified`, CSS more elaborated (`independent_verified:cssparser-0.34:…`) | 76 rows | `RESULTS.md:55, 102` |
| `Diagnostic nonproducer` (col 21) | Yes; JSON-only field, CSS rows use `scalar_reference=pass:cssparser_oracle;…` (col 21) | 76 rows; structural drift in field naming | `RESULTS.md:55, 102` |
| `Comparator evidence` (col 22) | Yes (the load-bearing blob) | 76 rows | `RESULTS.md:55, 102` |

**SK-V8 SPEC §0.4 compliance findings:**
- 16 of 27 fields present as discrete columns; 5 encoded only inside the `Comparator evidence` blob (comparator_id, comparator_strictness, comparator_freshness, sidecar_freshness, plus implicitly CostFacts fields); **3 fields entirely absent** (`CostFacts rule id` / `CostFacts chosen shape` / `CostFacts rejected alternative ids`) — these were W1 mandatory in SK-V8 SPEC §0.4 but never landed in the manifest schema.
- **Schema drift between JSON and CSS rows.** Host triple, SK-Vn-open delta, Diagnostic nonproducer fields each render differently in JSON vs CSS blocks; the two blocks share a header (line 53) but populate it with structurally divergent values. The SK-V14 SYNTHESIS §2 schema NORMALISES this — `xtask gate-json` must reject one or the other format unless reconciled.
- **`Cycles per byte` column nominally required but never populated.** Sample cost column carries `ns_per_byte=` strings for JSON and `track1_mean_ns=` for CSS; cycles/byte (the SK-V8 §0.4 named cost measure) requires PMU output and was deferred. P1-D is the agent that lands cycles/byte for SK-V14; P1-F flags the gap.

### §4.3 Row-count discrepancies surfaced by S-P0 audit (per SK-V14 SYNTHESIS §1.2)

- **Narrow vs broader ledger:** the dispatch context §1 cites `4 direct + 7 typed` admits; α-A + α-D peer-measure `6 direct + 11 typed` under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger (per `SYNTHESIS.md:73-84`). The narrow census counts only the rows whose REDRESS history explicitly cites a SK-V13 wave; the broader census counts every cell with `tranche_admitted=ADMITTED` regardless of citation depth.
  - **+2 direct rows** (broader-only): `marine_ik` (ROLLING:42), `instruments` (ROLLING:45) — both pre-W2 carry-overs from SK-V8 / SK-V11.
  - **+4 typed rows** (broader-only): `random` W13.3 (ROLLING:37), `instruments` W13.4 (ROLLING:46), `numbers` W13.1 (ROLLING:49), `unicode_basic` W13.2 (ROLLING:58); `update_center` W15.1 (ROLLING:31) is a re-pinned adjustment under the broader census.
  - All 11 broader-extension rows reclassify AUDIT-FALSIFIED under `v6 §1 rows 3-4` (the same comparator-misbinding pattern: `sonic_rs::from_slice::<Value>` eager DOM, not strict per-corpus struct deser).

- **PRUNE-1 binds the wider 6+11 population**, not the narrower 4+7 (`SYNTHESIS.md:80-84`). The S-P3 wave plan must consume the broader census or 4 admit rows survive an incomplete revert.

### §4.4 Missing rows in `skinny/RESULTS.md`

Six JSON `real_typed_struct` rows are physically absent from the parse-result table (lines 5-49) and present only as `MISSING` markers in `ROLLING-SOTA-DELTA.md:22, 40, 52, 55, 61, 64`: `canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, `y_string_unicode`. The SK-V14 SYNTHESIS §0.2 obligation is **17 typed rows** — these six must be authored (new product surfaces generated) or carry `absent:product-surface-not-generated` with explicit architectural-block proof per the §0.1 close condition.

Additionally, **23 CSS L4 rows are absent from the parse-result block** entirely. CSS rows live in the telemetry block (lines 102-132 = 31 rows); only `css_l4/declaration_values/direct_to_struct/main` (line 102) appears as a 27-column parse-result row. The SK-V14 SYNTHESIS §2 schema demands `Outcome / Verdict / Track 1 Mbps / Track 2 Mbps / lightningcss Mbps` as discrete parse-result columns for every CSS feature; the current bifurcated layout violates the schema by hiding CSS in the telemetry blob.

### §4.5 Stale telemetry signals

- **Profile artifact stale.** Every JSON row carries `Hot leaf = criterion-slope-profile:json_<corpus>/track1_*/new/estimates.json;hot-leaf=criterion-slope-profile;row=…` — i.e. the hot leaf is the Criterion estimate file path, NOT a named symbol. SK-V14 SYNTHESIS §2 row `Hot leaf` (line 254): "required; stale inherited profile names fail S-P1." Every JSON row fails this on its face. P1-A/B/C resolve the named symbols; P1-F flags the gap on all 45 measured JSON rows.
- **Sample-cost dimensional drift.** JSON uses `ns_per_byte=` (a derived dimensional cost), CSS uses `track1_mean_ns=` + `oracle_mean_ns=` + `lightningcss_mean_ns=` (raw nanoseconds). No row carries `cycles_per_byte=` — the PMU-derived cost measure SPEC §0.4 nominally requires; awaits P1-D.
- **SK-V13 close commit `653cdf795+w15.1-redress` does not match git history.** The ROLLING-SOTA-DELTA pin uses an annotation suffix (`+w15.1-redress`) rather than a canonical commit hash; this is the SK-V13 close-state freeze convention but it precludes deterministic verification via `git show 653cdf795+w15.1-redress`. SK-V14 SYNTHESIS §1.3 calls this state "the SK-V14 starting baseline"; the freeze convention should land as a documented annotation rule.
- **A2 F8 — single-lane comparator fanout.** `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` (`sonic_rs_anchor`) is the strict comparator for all three JSON planes simultaneously per `sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:91-97`. Every "sonic-rs strict Mbps" cell in RESULTS.md is therefore an eager-DOM `from_slice::<Value>` measurement misattributed as plane-correct strict. The `comparator_plane` column (§4.1) and R1 closure are the load-bearing fixes; P1-F flags the per-row implication: 45 JSON rows × 1 comparator cell each = **45 cells under misbinding**.

### §4.6 Masking probes telemetry absent

The dispatch context §6 P1-C scope identifies three masking probes (`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`) that should produce instrumentation-divergence rows. None of these probe-names appear in `skinny/RESULTS.md` (`grep -c 'host_call_eager_decode\|alternate_scalar_plan\|cold_first_parse' skinny/RESULTS.md` → 0). P1-C is the agent that lands these probes; P1-F flags the absence as a row-population gap of 17 × 3 probes = 51 missing probe-rows.

## §5 — Sources (artefact paths + run ids)

- Primary input: `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md` (186 lines, 45 JSON parse-result rows L5-49, 1 CSS parse-result row L102, 76 telemetry rows L55-132).
- ROLLING-SOTA-DELTA: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/ROLLING-SOTA-DELTA.md:13-93` (51 JSON cells + 24 CSS features; source_commit `653cdf795+w15.1-redress`).
- SK-V13 audit pack synthesis: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (PRUNE-1: §82-88; PRUNE-2: §99-113; §1.2 falsification map).
- SK-V14 audit pack synthesis: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (§1.2 NEW findings; A2 F8 comparator fanout; A2 F9 negative-drift confirmation).
- SK-V14 grand synthesis: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SYNTHESIS.md` (§0.2 goalset; §1.2 reconciliation 4+7 narrow vs 6+11 broader; §2 telemetry binding with NEW columns).
- SK-V8 SPEC: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v8/SPEC.md:103-146` (§0.4 27-field telemetry manifest).
- Dispatch authority: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md:82-87` (§4 audit-overlay column binding).
- REDRESS wave attribution: `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md` items 145-148 (W13.1-4 lines 4463-4617), 154-156 (W14.1-3 lines 4767-4826), partial coverage of 155+ for W14.4-5 and W15.1 (orchestrator scans separately).
- Run id (frozen at SK-V13 close): `sk-v9-open:criterion-fnv64-9d324a7ceab18d53` (JSON rows; per `RESULTS.md:55-99`); `sk-v12-w1b-2b:criterion-fnv64-27240148e5780a54` + `sk-v13-w{2,3,4,10.1,10.2,10.3}:fixture-fnv64-*` (CSS rows; per `RESULTS.md:103-132`).
- Profile binaries: none produced by this pass (P1-F is documentary; P1-A/B/C/D produce `/tmp/skv14-p1*/` flame profiles).
- Bench harness reference: `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` (`sonic_rs_anchor` single-lane fanout; surfaced as A2 F8).

---

**P1-F closure summary:** 51 JSON nominal cells × 3 planes + 24 CSS L4 features = **75 row population fully extracted** with audit overlay applied. 40 rows AUDIT-FALSIFIED under the narrow ledger / 46 under the broader ledger; 29 / 23 rows AUDIT-PENDING; 0 AUDIT-SUSTAINED on the goalset surface (the SUSTAINED architectural pillars W5/W6/W7/bbnf-simd/Tape are not goalset rows — they support the rows but are not themselves measured cells). 4 NEW SK-V14 schema columns absent from RESULTS.md (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`); 3 SK-V8 SPEC §0.4 CostFacts fields never landed; 6 JSON typed rows physically MISSING; 23 CSS L4 features hidden in telemetry block instead of parse-result block. Δ vs SK-V13 close is numerically zero on every cell (no SK-V14 implementation commits) and categorically a verdict downgrade on every prior admit.
