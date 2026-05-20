# SK-V12 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: extract the live `skinny/RESULTS.md` surface at capture source commit
`cf7848b227ebd2d4b7401d1441833a09c2e45c16`, compare the pin-aware
SK-V12-open surface to SK-V11 close, classify outcomes, and flag stale or
absent telemetry honestly.
Output: this file.
Baseline: SK-V12 pin-aware S-P1 extraction over unchanged SK-V11 close
(`db2c999b0b3e564b13cf2f5b8aa9858c8b16cb3a` close authority;
`3ce75df4e06b46eab8ca758c5ac32422aaad767c` measured W0 source anchor).
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: RESULTS extraction plus docs/source-read checks. Parent
orchestrator owns fresh profile capture under `/tmp/skv12-pin-p1`; final
companion profile status is PMU 82/82 PASS, samply 82/82 PASS, and xctrace
212/212 PASS.
Corpus coverage: 17/17 JSON corpora; 41/41 main RESULTS rows; 0 admitted CSS
L4 skinny rows.

Pin-aware extraction summary:

- `skinny/RESULTS.md` is unchanged from SK-V11 close for the JSON surface:
  17 `parse_only`, 17 `direct_to_struct`, and 7 `real_typed_struct` rows.
- Current files do not prove any admitted CSS L4 skinny row. The only current
  CSS L4 artifacts are non-admitting schema/report fixtures and historical
  REDRESS evidence; no `skinny/RESULTS.md` row and no generated runtime module
  admits CSS L4.
- The user-pin close bar is not populated by JSON rows. `lightningcss_mbps + 1`
  applies only to a generated CSS L4 row on the same corpus, same output plane,
  same host, and strict equality semantics. Existing JSON sonic/serde rows are
  guard or diagnostic evidence, not a CSS lightningcss comparator.
- The live RESULTS run id remains
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`; no SK-V12-specific RESULTS
  render is present.

## Section 1 - Method

Commands run:

```bash
git status --short
pgrep -fl 'cargo|rustc|xctrace|samply|criterion|bbnf'
git rev-parse HEAD
sed -n '1,240p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
sed -n '1,280p' restart/skinny/tranches/sk-v12/SYNTHESIS.md
sed -n '1,340p' restart/skinny/tranches/sk-v12/HANDOFF.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md
sed -n '1,280p' restart/skinny/tranches/sk-v11/research/close/close-redress.md
sed -n '3282,3356p' skinny/REDRESS.md
sed -n '3490,3565p' skinny/REDRESS.md
sed -n '1,260p' skinny/RESULTS.md
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py \
  skinny/RESULTS.md /tmp/skv12-p1f-current-results.csv
wc -l /tmp/skv12-p1f-current-results.csv
awk -F, 'NR>1{count[$3":"$4]++} END{for (k in count) print k,count[k]}' \
  /tmp/skv12-p1f-current-results.csv | sort
rg -n "lightningcss|css_l4|CSS L4" skinny/RESULTS.md /tmp/skv12-p1f-current-results.csv
git diff --name-status db2c999b..HEAD -- skinny/RESULTS.md skinny/REDRESS.md
find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
rg -n "css_l4|sheets_witness|lightningcss|emit_from_source|ensure_runtime_profile|runtime/src/grammars" \
  skinny/crates restart/skinny/tranches/sk-v12 -g '!target'
```

`git status --short` was clean before writing this artifact. The SK-V11 close
comparison produced no `skinny/RESULTS.md` or `skinny/REDRESS.md` diff from
`db2c999b` to HEAD. A parent-owned PMU replay process was running under
`/tmp/skv12-pin-p1`; this lane did not stop, consume, or mutate it.

## Section 2 - Current RESULTS Surface

The live row table is unchanged from SK-V11 close. Physical labels still say
`SK-V9-open` because the report was not re-rendered for SK-V12; S-P1 should
treat the SK-V12-open JSON surface as a freshness binding over close evidence,
not as new row telemetry.

| Family | Live extraction | SK-V12 role | Delta vs SK-V11 close |
|---|---:|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only | none |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | JSON guard rows plus REDRESS 119 routed ledger | none |
| `real_typed_struct` | 7 `A / GO` | JSON typed guard rows | none |
| generated CSS L4 | 0 admitted rows | authoritative first target under the user pin | absent |
| generated Sheets / BBNF-self | 0 admitted rows | fallback only after CSS L4 redress attempt | absent |
| Overall | `N-direct / NoGo` | seed outcome | none |

Observed main-table outcomes are `A`, `L`, `N-direct`, and `S`; no current row
renders `C`, `G`, or `K`. The JSON rows cannot fill the CSS L4
`lightningcss_mbps + 1` close bar because they use JSON corpora, JSON output
planes, and sonic/serde comparators rather than a CSS L4 fact stream and a
lightningcss comparator.

## Section 3 - Per-Row Extraction And Delta

Every SK-V12-open JSON delta below is unchanged because `skinny/RESULTS.md` is
unchanged from SK-V11 close. `Delta vs sonic` is the live Track 1 comparison
rendered in the JSON row and is guard/diagnostic evidence only.

| Row | Outcome | Verdict | T1 | T2 | sonic strict | Delta vs sonic | Delta vs SK-V11 close |
|---|---|---|---:|---:|---:|---:|---|
| `twitter/parse_only` | `S` | `NO-GO` | 10474 | 7757 | 16988 | -38.3% | unchanged |
| `twitter/direct_to_struct` | `N-direct` | `NO-GO` | 11613 | 10816 | 15113 | -23.2% | unchanged |
| `twitter/real_typed_struct` | `A` | `GO` | 17740 | 15912 | 15010 | +18.2% | unchanged |
| `citm_catalog/parse_only` | `S` | `NO-GO` | 26791 | 18271 | 21564 | +24.2% | unchanged |
| `citm_catalog/direct_to_struct` | `A` | `GO` | 18563 | 17787 | 15530 | +19.5% | unchanged |
| `citm_catalog/real_typed_struct` | `A` | `GO` | 30539 | 17675 | 20726 | +47.3% | unchanged |
| `canada/parse_only` | `L` | `NO-GO` | 15544 | 16215 | 13462 | +15.5% | unchanged |
| `canada/direct_to_struct` | `N-direct` | `NO-GO` | 10316 | 9819 | 11700 | -11.8% | unchanged |
| `apache_builds/parse_only` | `S` | `NO-GO` | 12733 | 12196 | 17291 | -26.4% | unchanged |
| `apache_builds/direct_to_struct` | `A` | `GO` | 11254 | 10189 | 10995 | +2.4% | unchanged |
| `apache_builds/real_typed_struct` | `A` | `GO` | 8478 | 6892 | 8106 | +4.6% | unchanged |
| `github_events/parse_only` | `S` | `NO-GO` | 14805 | 12791 | 22578 | -34.4% | unchanged |
| `github_events/direct_to_struct` | `N-direct` | `NO-GO` | 11918 | 10596 | 14743 | -19.2% | unchanged |
| `github_events/real_typed_struct` | `A` | `GO` | 11871 | 12275 | 12224 | -2.9% | unchanged |
| `update_center/parse_only` | `S` | `NO-GO` | 11493 | 9033 | 18962 | -39.4% | unchanged |
| `update_center/direct_to_struct` | `N-direct` | `NO-GO` | 8187 | 7474 | 11064 | -26.0% | unchanged |
| `update_center/real_typed_struct` | `A` | `GO` | 11851 | 10358 | 12467 | -4.9% | unchanged |
| `mesh/parse_only` | `S` | `NO-GO` | 13325 | 12128 | 11679 | +14.1% | unchanged |
| `mesh/direct_to_struct` | `N-direct` | `NO-GO` | 8561 | 8652 | 9542 | -10.3% | unchanged |
| `mesh/real_typed_struct` | `A` | `GO` | 9403 | 7897 | 8923 | +5.4% | unchanged |
| `random/parse_only` | `S` | `NO-GO` | 7747 | 7554 | 14172 | -45.3% | unchanged |
| `random/direct_to_struct` | `N-direct` | `NO-GO` | 7693 | 6949 | 8665 | -11.2% | unchanged |
| `gsoc-2018/parse_only` | `S` | `NO-GO` | 4887 | 4544 | 8472 | -42.3% | unchanged |
| `gsoc-2018/direct_to_struct` | `N-direct` | `NO-GO` | 2665 | 2578 | 4110 | -35.2% | unchanged |
| `marine_ik/parse_only` | `S` | `NO-GO` | 10675 | 11700 | 9376 | +13.9% | unchanged |
| `marine_ik/direct_to_struct` | `A` | `GO` | 8938 | 9437 | 8473 | +5.5% | unchanged |
| `marine_ik/real_typed_struct` | `A` | `GO` | 11788 | 10096 | 9010 | +30.8% | unchanged |
| `instruments/parse_only` | `S` | `NO-GO` | 16574 | 11587 | 19055 | -13.0% | unchanged |
| `instruments/direct_to_struct` | `N-direct` | `NO-GO` | 11569 | 10736 | 9865 | +17.3% | unchanged |
| `numbers/parse_only` | `S` | `NO-GO` | 17941 | 18328 | 13198 | +35.9% | unchanged |
| `numbers/direct_to_struct` | `N-direct` | `NO-GO` | 4479 | 2366 | 2667 | +67.9% | unchanged |
| `unicode_mixed/parse_only` | `S` | `NO-GO` | 1883 | 7326 | 15137 | -87.6% | unchanged |
| `unicode_mixed/direct_to_struct` | `N-direct` | `NO-GO` | 3753 | 2427 | 2846 | +31.9% | unchanged |
| `unicode_escapes/parse_only` | `S` | `NO-GO` | 3733 | 2421 | 7235 | -48.4% | unchanged |
| `unicode_escapes/direct_to_struct` | `N-direct` | `NO-GO` | 1345 | 1341 | 3785 | -64.5% | unchanged |
| `unicode_basic/parse_only` | `S` | `NO-GO` | 3217 | 2985 | 4354 | -26.1% | unchanged |
| `unicode_basic/direct_to_struct` | `A` | `GO` | 2299 | 2227 | 2353 | -2.3% | unchanged |
| `distinct_values/parse_only` | `S` | `NO-GO` | 2335 | 1675 | 4883 | -52.2% | unchanged |
| `distinct_values/direct_to_struct` | `N-direct` | `NO-GO` | 1750 | 1625 | 2923 | -40.1% | unchanged |
| `y_string_unicode/parse_only` | `S` | `NO-GO` | 1965 | 2695 | 6227 | -68.4% | unchanged |
| `y_string_unicode/direct_to_struct` | `N-direct` | `NO-GO` | 1983 | 1029 | 4344 | -54.4% | unchanged |

## Section 4 - REDRESS Fixpoint And Pin-Aware Deltas

REDRESS 119 remains the direct residual authority. It closes the 13 residual
`direct_to_struct` rows as measured fixpoint evidence, not as `GO`; W8 selected
no behavior source intervention, no W8a split, no gate semantic change, and no
`skinny/RESULTS.md` row movement.

REDRESS 120 closes SK-V11 as measured fixpoint and Alpha feedback:
`parse_only` stays 16 `S / NO-GO` plus 1 `L / NO-GO`,
`direct_to_struct` stays 4 `A / GO` plus 13 `N-direct / NO-GO`,
`real_typed_struct` stays 7 `A / GO`, and overall stays `N-direct / NoGo`.

REDRESS 111-113 are load-bearing for the non-JSON axis:

| REDRESS | Subject | Current P1-F extraction |
|---:|---|---|
| 111 | non-JSON gate/report schema lane | admits a non-admitting companion report lane only; it does not update `skinny/RESULTS.md`, create a generated baseline, or move a parser row |
| 112 | generated non-JSON baseline rejection | selected `css_l4/declaration_values/direct/main`; no CSS L4 baseline report was admitted |
| 113 | generated CSS L4 intervention entry block | no `W1b_css_baseline_mbps`; no source patch, generated parser, SIMD kernel, benchmark row, gate schema, or RESULTS row moved |

The user pin supersedes the old W1b/W2 target shape only as future authority:
CSS L4 is now mandatory first target and the floor is
`generated_track1_mbps > lightningcss_mbps + 1`. It does not retroactively
create a CSS row, a lightningcss comparator artifact, or a CSS fact-stream
oracle in the current files.

## Section 5 - Telemetry Freshness And Absence

- SK-V12-open vs SK-V11 close: no `skinny/RESULTS.md` or
  `skinny/REDRESS.md` diff from `db2c999b` to the capture source commit.
- Main-table extraction found 41 JSON rows: 11 `A / GO`, 16 `S / NO-GO`,
  13 `N-direct / NO-GO`, and 1 `L / NO-GO`.
- `rg` found no `lightningcss`, `css_l4`, or `CSS L4` entry in
  `skinny/RESULTS.md` or the extracted main-table CSV. Therefore the
  lightningcss close bar is absent from the current RESULTS surface.
- Runtime grammar inventory contains generated JSON plus `sheets_witness`;
  no generated `css_l4`, `css_l4_declaration_values`, `sheets`, or
  `bbnf_self` runtime module is present.
- Codegen still exposes the historical blocker named by REDRESS 112:
  direct and typed emission routes call `json_provider::ensure_runtime_profile`,
  and the provider is still the named JSON-runtime boundary.
- Main-table hot leaves remain Criterion slope artifact bindings. Separate
  pin-era xctrace hot-leaf tables now resolve `% self-time` and source
  file:line under `/tmp/skv12-pin-p1`; those profile symbols do not mutate
  `skinny/RESULTS.md`.
- Run identity is stale by SK-V12 name: every manifest row still carries
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`; no `SK-V12-open` run id is
  rendered.
- The manifest remains JSON-only for admitted rows. Non-JSON schema fixtures
  under `restart/skinny/tranches/sk-v12/research/` are not skinny RESULTS rows.

## Section 6 - Classification

| Classification | Rows | SK-V12 disposition |
|---|---:|---|
| `A / GO` direct guards | 4 | preserve or record measured demotion |
| `A / GO` typed guards | 7 | preserve or record measured demotion |
| `S / NO-GO` parse diagnostics | 16 | diagnostic only; not SOTA admission |
| `L / NO-GO` parse diagnostic | 1 | `canada/parse_only`; diagnostic only |
| `N-direct / NO-GO` residual direct | 13 | routed by REDRESS 119/120 unless fresh pin-relevant material evidence appears |
| generated CSS L4 baseline/admission | 0 | first material target; must beat `lightningcss_mbps + 1` |

Bottom line: the pin-aware SK-V12 S-P1 extraction is a freshness rebinding of
the SK-V11 measured JSON close plus a hard absence finding for CSS L4. Current
files do not contain an admitted generated CSS L4 row, and no JSON row
populates the lightningcss close bar.

## Section 7 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- `/tmp/skv12-p1f-current-results.csv`
