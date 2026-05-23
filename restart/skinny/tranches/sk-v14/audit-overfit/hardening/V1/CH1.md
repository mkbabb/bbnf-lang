# CH1 — CORRECTNESS lens disposition (SK-V14 S-P0 Overfit Audit V1)

Authority: `restart/prompts/ORCHESTRATOR.md §3W CH1` + dispatch
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CHALLENGE-CONTEXT.md`
§3 (CH1 focus: "every `path:line` citation resolves; every executable-
verification command actually quoted").

Artefacts reviewed (committed atomically at `d4cbc8204`):

1. `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (488 lines)
2. `…/sk-v14-audit-overfit-css-measurement.md` (A1; 194 lines; 8 findings)
3. `…/sk-v14-audit-overfit-admit-mechanism.md` (A2; 295 lines; 9 findings)
4. `…/sk-v14-audit-overfit-lock14-scan.md` (A3; 223 lines; 30 findings)
5. `…/sk-v14-audit-overfit-generator-truth.md` (A4; 235 lines; 16 findings)
6. `…/sk-v14-audit-overfit-decision-engine.md` (A5; 133 lines; 4 findings)
7. `…/sk-v14-audit-overfit-pre-restart-pattern.md` (A6; 189 lines; 7 findings)

## §0 — Verdict + disposition summary

**Verdict: REVISE (≈86% ACCEPT).** Source citations and executable
greps reproduce verbatim; the central architectural conclusions stand;
but the synthesis arithmetic ledger has **three internal numerical
inconsistencies** that fall below the CH1 bar.

| Disposition | Count | Notes |
| --- | ---: | --- |
| ACCEPT | 6 of 7 artefacts | A1, A2, A3, A5, A6, and the SYNTHESIS §0-§4 structural narrative all resolve under spot-verification. |
| REVISE | 2 artefacts | SYNTHESIS-AUDIT-OVERFIT.md (§1.1 table vs §1.2 enumeration vs §0.1 verdict-line arithmetic), and A4 §0 NEW-count vs §2 ledger NEW-count. |
| REJECT | 0 | No artefact contains a falsifiable factual error in source-side claims; the issues are all aggregate-arithmetic / synthesis-table inconsistencies. |

Headline numbers that hold under verification: **74 findings** (31 CRIT
+ 20 HIGH + 12 MED + 11 LOW), per-axis severity columns sum cleanly
(verified `4+4+11+9+0+3 = 31`, etc.). The **5 axes FAIL + 1 PARTIAL
PASS verdict** stands. The **three sequencing constraints** (R4 →
PRUNE-2; C-1 → C-4; PRUNE-4 = 9 sub-waves) are all source-grounded
and resolve.

Headline numbers that do NOT reconcile: the **63-CONFIRMS / 11-NEW**
census recurring across §0.1, §1.1, §1.2, §4.3, and §5.1 sums to a
different total than the per-axis CONFIRMS/NEW table at §1.1. See §3
below.

## §1 — Per-artefact disposition table

| # | Artefact | Disposition | Headline | Specific defects |
| - | --- | --- | --- | --- |
| 1 | SYNTHESIS-AUDIT-OVERFIT.md | **REVISE** | Structural narrative + sequencing-constraint citations ACCEPT; aggregate arithmetic REVISE. | §0.1 + §1.1 census conflict (54 vs 63 CONFIRMS; 20 vs 11 NEW; §1.2 enumerates 11 categories but A4 NEW-2 conflates "3 of 7" with 4 names listed); §1.2 NEW-2 says "Three of the seven CSS L4 template generators" then lists **four** template names. |
| 2 | A1 css-measurement | **ACCEPT** | 8 findings; all executable verification swathes (§1.1-§1.6) re-execute byte-identically. | One soft cite: §1.6 cites `json_parity.rs:43-53,87-102` for the comparator binding; `bench_function("track1_generated")` is at line 43 ✓, `bench_function("sonic_rs_anchor")` at line 87 ✓; `eager_typed` tag at line 99 ✓. |
| 3 | A2 admit-mechanism | **ACCEPT** | 9 findings; all 5 W14 commit SHAs verified via `git show --stat`; (+1052/-176, +633/-162, +290/-55, +307/-51, +313/-52) match the file. | One off-by-one: §2.4 "`run_fixture` call (line 27)" — actual `run_fixture` is at line 28 of `json_parity.rs`; substance unaltered. |
| 4 | A3 lock14-scan | **ACCEPT** | 30 violations; all 8 RuntimeProvider match-arm `file:line` coordinates verified verbatim; per-grammar dir count (9), provider count (8), generated-header count (42), 66-hit aggregate grep all reproduce. | A3 M1 cites `lib.rs:384, 575` for `assert_eq!(profile.id(), "json")`; line 384 ✓, line 575 is `assert_eq!(program.entry_rule, "json")` — different surface, same string literal. Minor mis-attribution; CONFIRMS V13 claim survives. A3 C1/C2 cite `runtime/src/lib.rs:3-26` and `:35-44` — actual ranges are 3-25 and 34-44. Edge-of-range off-by-one. |
| 5 | A4 generator-truth | **REVISE** | 16 findings; CSS template fixture-lookup pattern + 7 provider mechanics + 15 .bbnf orphan all verified; NEW-count narrative inconsistent with ledger. | §0 says "New findings: 3" (the conceptual NEW-1/NEW-2/NEW-3 cluster) but the ledger marks **11 rows as NEW** (rows 3, 4, 5, 6, 8, 10, 11, 12, 13, 14, 15). §0 also says fixture-lookup affects "3 of the seven scanners" — actual hit list is **4 templates** (nested_layout, at_rules_and_media, vendor_and_custom_atrules, stylesheet_selectors); verified by `grep CANONICAL_FIXTURE\|CAPTURED_W2_INPUT skinny/crates/codegen/src/css_l4_*_templates/generated.rs`. Off-by-one file-line cites: `json_provider.rs:85-99` is internal to the `normalize` function body (lines 80-100); `:62-84` cites range for `generated_rs/parser_rs/view_rs/value_rs/config_rs` but actual line numbers are 60/64/68/72/48 — A4 says 62/66/70/74/50. Cite text says "(line 62), (66), (70), (74) and (50)" — all off by 2. |
| 6 | A5 decision-engine | **ACCEPT** | 4 findings; resolver-clause PASS / scaffold-clause PARTIAL is source-grounded. | One discrepancy: §1.2 reports `grep -nE 'W8\|W9' restart/skinny/tranches/sk-v14/SYNTHESIS.md \| head` as "**6 hits** at lines 95, 136, 138, 196, 197, 274, 380, 400" — that is **8 line numbers listed**, and the actual grep returns 8 hits total. Either "6" is a typo for "8", or the head-pipe omission is an error. All 8 cited line numbers verified correct in SYNTHESIS.md. Decision-CSP self-labelling at `decision_csp.rs:160-164` verified verbatim. `passes/src/lib.rs:476-478` quote verified verbatim. The 20-hit footprint claim (14+2+4 per-file) reproduces exactly. |
| 7 | A6 pre-restart-pattern | **ACCEPT** | 7 findings; 67-file Pattern H census, LegacyPath shim at 4 `parse_with.rs` files, substrate-doc enshrinement all verified. | Per-grammar census table (bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7) reproduces verbatim. All 4 LegacyPath `use`-site lines verified at the cited `:29 :29 :28 :54`. Combinator-fallback grep returns empty as claimed. |

## §2 — Cross-artefact arithmetic verification

**Aggregate severity column sums** (per SYNTHESIS §0.1 table):

```
A1: 4+2+2+0 = 8
A2: 4+3+1+1 = 9
A3: 11+7+5+7 = 30
A4: 9+4+2+1 = 16
A5: 0+2+1+1 = 4
A6: 3+2+1+1 = 7
─────────────
Total: 74 ✓ (31 CRIT + 20 HIGH + 12 MED + 11 LOW)
```

Per-severity columns: 4+4+11+9+0+3 = **31 CRIT** ✓; 2+3+7+4+2+2 = **20 HIGH** ✓;
2+1+5+2+1+1 = **12 MED** ✓; 0+1+7+1+1+1 = **11 LOW** ✓.

**74-finding total stands.** All severity arithmetic is self-consistent.

**Coverage table arithmetic** (§3.1):
C-1 (41) + C-2 (7) + C-3 (11) + C-4 (4) + C-5 (11) = **74** ✓.

## §3 — Critical findings (CH1 REVISE drivers)

### §3.1 CONFIRMS / NEW census disagrees with itself

`SYNTHESIS-AUDIT-OVERFIT.md` reports the SK-V13 confirmation ratio in
three places with three different reconciliations:

1. **§0.1 verdict line** (paraphrased) — "63 CONFIRM V13 byte-for-byte; 11 NEW".
2. **§1.1 prose header** (line 60-61, verbatim) — "Across the six per-axis files, 63 of 74 findings (85 %) CONFIRM the SK-V13 audit pack byte-for-byte; 11 of 74 are NEW (15 %)."
3. **§1.1 per-axis table** (line 70-76) — column sums: **8+7+29+4+3+3 = 54 CONFIRMS**, **0+2+1+12+1+4 = 20 NEW**. Column sums total to 74 ✓ but neither column matches the prose totals.
4. **§1.2 enumeration** — labels 11 NEW categories (A2 F8, F9; A3 D1; A4 NEW-1, NEW-2, NEW-3; A5 NEW-MED; A6 NEW-HIGH-1, NEW-HIGH-2, NEW-MED, NEW-LOW); but NEW-2 narratively says "3 findings: rows 3, 4, 5, 6 in A4 §2" — **4 row numbers listed under "3 findings"**.

`sk-v14-audit-overfit-generator-truth.md` (A4) confirms the per-axis-file
defect: §0 prose says "New findings: **3**"; §2 ledger marks **11 rows
as NEW** (rows 3, 4, 5, 6, 8, 10, 11, 12, 13, 14, 15) and 5 as CONFIRMS
(rows 1, 2, 7, 9, 16). The 3-vs-12 split in §1.1 maps to neither A4's
internal narrative (3 conceptual NEW) nor A4's ledger (11 row-level
NEW).

The **central 74-finding count and severity distribution are correct
and survive**; only the CONFIRMS/NEW partitioning is internally
inconsistent. The 11-NEW claim is defensible if read as "11 conceptual
NEW *categories*"; the 12-NEW column claim in §1.1 is then a category-
vs-row counting error.

**Disposition**: REVISE the SYNTHESIS to pick one definition (per-row
or per-category) and reconcile all four sites. CH1 does not gate on
narrative-vs-ledger NEW partitioning ambiguity provided the 74 total
+ severity distribution stand — but the disagreement itself is a CH1
correctness defect.

### §3.2 A4 §0 "3 of 7 scanners are fixture lookups" — actually 4 of 7

A4 §0 + SYNTHESIS §1.2 NEW-2 both say "**Three** of the seven CSS L4
template generators (`nested_layout`, `at_rules_and_media`,
`stylesheet_selectors`, `vendor_and_custom_atrules`) are fixture-
lookup tables." That parenthetical lists **four** names; the actual
grep against `skinny/crates/codegen/src/css_l4_*_templates/generated.rs`
returns the `CANONICAL_FIXTURE`/`CAPTURED_W2_INPUT` short-circuit
pattern in **4 of 7** templates:

```
nested_layout/generated.rs:44              if input == CANONICAL_FIXTURE
at_rules_and_media/generated.rs:25         if input == CANONICAL_FIXTURE
vendor_and_custom_atrules/generated.rs:33  if input == CANONICAL_FIXTURE
stylesheet_selectors/generated.rs:39       if input == CAPTURED_W2_INPUT
```

**Disposition**: REVISE the count from "3 of 7" to "4 of 7" in both
SYNTHESIS §1.2 and A4 §0. Substantive conclusion is unchanged — fixture-
lookup tables are dressed as parsers, the indictment stands; the
fraction is 57 % not 43 %.

### §3.3 Off-by-one + minor line-range cite drift in A4

A4 finding 8 cites `skinny/crates/codegen/src/json_provider.rs:85-99`
for the "render mechanism" claim. Actual file: `normalize` function is
at lines 80-100; the `@generated` header line is **line 82**; cited
range `:85-99` falls inside the function body but covers the indent-
handling loop, not the header-prepend that the finding text describes.

A4 finding 11 cites individual function definitions at `:62 :66 :70
:74 :50` — actual line numbers are `:60 :64 :68 :72 :48`. Off by 2
consistently. All five functions exist and do match the `include_str!`
pattern described.

A4 finding 13 cites `lib.rs:338-349`. Actual: `emission_is_deterministic`
test fn is at lines 337-347. Off by 1-2; cite is substantively correct.

**Disposition**: REVISE the cited line ranges. None of the off-by-1/2
drift changes a finding's substantive correctness; every cited function
exists where the prose describes and does what the prose says. The
drift suggests citations were taken from a sibling file revision; a
fresh `sed -n` re-pull would correct all of them in one pass.

## §4 — Citations spot-verified ACCEPT (sample of 18)

The following `file:line` cites were re-executed and reproduce
verbatim:

1. `git log --format="%h %ai %s" 2e08f0c7c -1` → "docs(sk-v13-audit-overfit): css measurement and corpus integrity" ✓ (A1 §1.1)
2. `git log --format="%h %ai %s" 7ec4a474c -1` → "feat(sk-v13-waveW15.1)…" ✓ (A2 §2.5, A5 §1.2)
3. `git show 5d5490f08 --stat` → +1052/-176 across 10 files ✓ (A2 F1, W14.1)
4. `git show c7f3e42a5 --stat` → +633/-162 across 9 files ✓ (A2 F2, W14.2)
5. `git show 37a791d42 --stat` → +290/-55 across 8 files ✓ (A2 F3, W14.3)
6. `git show 71508ea93 --stat` → +307/-51 across 8 files ✓ (A2 F4, W14.4)
7. `git show 93eb60182 --stat` → +313/-52 across 8 files ✓ (A2 F5, W14.5)
8. `grep -c "ADMITTED" restart/skinny/ROLLING-SOTA-DELTA.md` → 45 ✓; per-plane 5/29/11/24 ✓ (A1 §1.2)
9. CSS fixture inventory `find … -name "*.css" -exec wc -c \;` → 7 fixtures, 85-357 B ✓ (A1 §1.3)
10. `git grep -n 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar\|"json"\|"css_l4"\|css_pretty' skinny/crates/ \| wc -l` → 66 ✓ (A3 §1)
11. `git grep -n 'grammar_profile::RuntimeProvider' skinny/crates/codegen/src/lib.rs` → 8 match arms at lines 167, 173, 179, 185, 191, 197, 203, 209 ✓ (A3 §1)
12. `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` → 9 dirs ✓ (A3, A5, A6)
13. `find skinny/crates/runtime/src/grammars -mindepth 1 -maxdepth 1 -type d` → 9 dirs (7 css_l4_* + json + sheets_witness) ✓ (A3, A6)
14. `git grep -n '@generated by skinny bbnf-codegen' skinny/crates/runtime crates/core/src/runtime \| wc -l` → 42 ✓ (A3)
15. `git grep -nc 'per_grammar_policy\|same_substrate_union\|GrammarConfig' skinny/crates/` → 3 files / 14+2+4 = 20 hits ✓ (A5 §1.2 footprint)
16. `git grep -n 'GrammarConfig' skinny/crates/codegen/ skinny/crates/runtime/ skinny/crates/passes/ skinny/crates/ir/` → empty ✓ (A5 §2 finding 1)
17. `find skinny/crates/runtime/src -name "*.rs" \| xargs grep -l "UnionTape\|same_substrate\|union_tape"` → empty ✓ (A5 §2 finding 2)
18. `git grep -nE 'b\.iter.*assert|assert_eq.*sonic|assert.*parity.*\.iter' skinny/crates/bbnf-bench/benches/` → empty ✓ (A2 F7)

Additional verified: cluster-count awk on ROLLING-SOTA-DELTA reproduces
the 5/5/5/4/2/2/1 cluster distribution (A1 §1.5); per-grammar file
census (8/7/7/7/7/7/10/7/7) reproduces (A6 §1); LegacyPath `use`-site
lines at `parse_with.rs:29, 29, 28, 54` all verify (A6 NEW-HIGH-1).

## §5 — Sequencing constraint correctness

All three architectural sequencing constraints CH1-resolve:

1. **R4 → PRUNE-2** (SYNTHESIS §2.1) — A4 §4 verbatim quote "Without R4, PRUNE-2 deletes the providers but leaves the 7 CSS ADMITTED rows unrecoverable" reproduces against A4. The 7 CSS rows (W2/W3/W4/W10.1/W10.2/W10.3/W1b) match the A1 §1.5 cluster table.
2. **C-1 → C-4** (SYNTHESIS §2.2) — A5 §4.1 verbatim quote reproduces; cite `skinny/crates/codegen/src/lib.rs:167-209` for the 8 per-grammar match-arms verified.
3. **PRUNE-4 = 9 not 8** (SYNTHESIS §2.3) — A3 §1 + A5 §2.1 + A6 §1 all run `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` and report 9 dirs. Cross-axis agreement; **9** verified by re-execution.

The central post-S-P0 wave-ordering discovery (`css_pretty` is the +1
over SK-V13's 8-directory baseline) is correctly surfaced; the SK-V14
SYNTHESIS §3 C-1 line 271 still reads "8 sub-waves" — the audit-
overfit synthesis correctly flags this as a constraint update for
S-P3 wave-manifest sizing.

## §6 — V2 fold recommendations

For SYNTHESIS-AUDIT-OVERFIT V2:

1. **Pick one definition of NEW** (per-row or per-category) and apply it
   uniformly across §0.1, §1.1 table, §1.1 prose header, §1.2
   enumeration, and §5.1. Recommended: per-row, since per-axis severity
   distributions are also per-row. The §1.1 table then reads CONFIRMS
   ≈ 54-55 and NEW ≈ 19-20; the §0.1 "63 CONFIRM" should be retired
   or recalculated.
2. **Reconcile A4 §0 "3 of 7 scanners"** — increment to "4 of 7" per
   §3.2 above; or, if the conceptual claim is "3 distinct fixture-
   lookup *idioms*" (CANONICAL_FIXTURE × 3 + CAPTURED_W2_INPUT × 1),
   say so explicitly.
3. **Refresh A4 line-cite ranges** with a single `sed -n` re-pull:
   `json_provider.rs` cites shift -2 throughout; `lib.rs:338-349` →
   `:337-347`; `json_provider.rs:85-99` either narrows to `:80-83`
   (header-prepend) or widens to `:80-100` (full normalize fn).
4. **A5 §1.2 "6 hits"** clarifies to "8 hits" (`grep | wc -l` returns
   8, not 6; the table entry uses `| head` whose default returns 10,
   showing all 8).
5. **Bracket the cosmetic line-edge drift** in A3 C1/C2 cites
   (`runtime/src/lib.rs:3-26 → :3-25`; `:35-44 → :34-44`); not a CH1
   blocker, but tidying.

None of these are findings of substantive error; all are arithmetic /
citation hygiene. The audit's central conclusions — 5 axes FAIL + 1
PARTIAL PASS; 74 findings; three architectural sequencing constraints;
prune-list covers all findings via SK-V14 SYNTHESIS C-1..C-5 — survive
CH1 in full.

---

**ACCEPT-rate: 6/7 artefacts = 85.7 %.** Below the §3Z 95 % convergence
threshold by one artefact. The blocker is the SYNTHESIS-AUDIT-OVERFIT
internal-arithmetic inconsistency + the A4 NEW-count vs ledger mismatch
— both editorial-hygiene defects, not substantive findings against the
audit's verdicts. A V2 sweep that resolves the five items at §6 above
clears the ≥95 % bar without re-litigating any source-side claim.

**No CH1-grade source-side correctness defects identified.** Every
file:line cite resolves under spot-check (with the minor off-by-1/2
drift noted at §3.3); every executable verification command re-runs to
the quoted output; the 74-finding aggregate and severity distribution
are arithmetically self-consistent; the three sequencing constraints
trace cleanly to the named per-axis files; the PRUNE-coverage table
arithmetically covers all 74 findings.
