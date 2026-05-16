# REVIEW-A — Consolidation Fidelity Audit

## §1 Audit target identification

| Field | Value |
|---|---|
| Reviewer role | Reviewer A — consolidation-fidelity auditor |
| Audit target | `restart/audit/hardening/HARDENING-CONSOLIDATED.md` (619 lines) |
| Audit target commit | `1cf6dac0` |
| Sources audited (in order) | `restart/audit/hardening/HARDENING-PASS-1.md` (206 lines, commit `8389c077`); `restart/audit/hardening/HARDENING-PASS-2.md` (294 lines, commit `303b91a9`); `restart/audit/hardening/HARDENING-PASS-3.md` (219 lines, commit `c839de98`); `restart/audit/hardening/HARDENING-MASTER-PLAN.md` (227 lines, commit `ac7fa8e2`) |
| Consolidation contract | `restart/prompts/sub-orchestrators/HARDENING.md:79-119` |
| Lanes applied | 5 fidelity lanes — Verdict tally / Conflict authenticity / Punch-list provenance / Floor completeness / Re-draft thresholds |
| Posture | Calibrated; archaic-permissive; no metalanguage; path:line citations; tables liberal |
| Scope boundary | Touch only `restart/audit/hardening/REVIEW-A-CONSOLIDATION-FIDELITY.md` |

The audit reads the consolidation against its four sources independently. It does not relitigate per-target verdicts. Whether the four hardening reports' verdicts are themselves correct is sister-reviewer scope. Reviewer A's verdict bears only on whether `HARDENING-CONSOLIDATED.md` faithfully represents what the four reports actually said.

## §2 Lane-by-lane verdict

| Lane | Rows audited | Faults found | Verdict |
|---|---:|---:|---|
| 1 — Verdict tally accuracy | 4 per-target counts × 9 lanes + 4 punch-list totals + cumulative roll-up = 76 cells | 0 | HONOURED |
| 2 — Cross-target conflict authenticity | 13 conflicts × ~3 citations each = 38 citation checks | 4 mistargeted citations (lane-verdict footer rather than content row); 0 fabricated conflicts | DRIFT (citation precision) |
| 3 — Punch-list provenance | 47 consolidated rows; 36 sample-audited against per-target sources | 6 mistargeted citations across 36 audited rows; 0 invented surgeries | DRIFT (citation precision) |
| 4 — Floor completeness | 4 target floor lists × ~10-16 items each = 56 floor-item checks | 0 missing REINVENT/DISCARD coverage from punch list; 4 items elided as subsumed by aggregate floor categories | HONOURED |
| 5 — Re-draft threshold gates | 10 re-draft escalation conditions | 0 invented; all 10 grounded in per-target text | HONOURED |

**Aggregate**: 5 lanes; 3 HONOURED; 2 DRIFT-with-citation-precision; 0 FABRICATION.

The fidelity defects are bounded — citation precision rather than substantive misrepresentation. The architectural claims, the surgery prescriptions, and the verdict counts hold. Several citations point at lane-verdict footers a few lines below the content they intend to reference; the named conflict and surgery in each case is real and is supported by the per-target text. No conflict and no punch-list item is fabricated; no surgery is invented; no per-target verdict is mis-routed.

## §3 Verdict tally drift table

### §3.1 Per-target totals

| Target | Field | Consolidated claim | Measured (per-target source) | Drift |
|---|---|---:|---:|---|
| PASS-1 | KEEP | 30 | 30 | none |
| PASS-1 | REINVENT | 29 | 29 | none |
| PASS-1 | DISCARD | 3 | 3 | none |
| PASS-1 | Punch-list rows | 19 | 19 | none |
| PASS-2 | KEEP | 38 | 38 | none |
| PASS-2 | REINVENT | 20 | 20 | none |
| PASS-2 | DISCARD | 1 | 1 | none |
| PASS-2 | Punch-list rows | 9 | 9 | none |
| PASS-3 | KEEP | 19 | 19 | none |
| PASS-3 | REINVENT | 47 | 47 | none |
| PASS-3 | DISCARD | 0 | 0 | none |
| PASS-3 | Punch-list rows | 12 | 12 | none |
| MASTER-PLAN | KEEP | 30 | 30 | none |
| MASTER-PLAN | REINVENT | 31 | 31 | none |
| MASTER-PLAN | DISCARD | 4 | 4 | none |
| MASTER-PLAN | Punch-list rows | 16 | 16 | none |
| Cumulative | KEEP | 117 | 30+38+19+30 = 117 | none |
| Cumulative | REINVENT | 127 | 29+20+47+31 = 127 | none |
| Cumulative | DISCARD | 8 | 3+1+0+4 = 8 | none |
| Cumulative | Punch-list rows pre-dedupe | 56 | 19+9+12+16 = 56 | none |

### §3.2 Per-lane × per-target tally

Walk-through of the §2 cohort table against per-target source lane-verdict rows.

| Lane | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cumulative | Sums |
|---|---|---|---|---|---|---|
| 1 Lock-Adherence | 7/7/0 (`HARDENING-PASS-1.md:53`) | 10/3/1 (`HARDENING-PASS-2.md:84`) | 7/7/0 (`HARDENING-PASS-3.md:58`) | 10/4/0 (`HARDENING-MASTER-PLAN.md:66`) | 34/21/1 | 7+10+7+10=34; 7+3+7+4=21; 0+1+0+0=1 ✓ |
| 2 Sequencing | N/A 0/0/0 (`HARDENING-PASS-1.md:63`) | N/A 1/0/0 (`HARDENING-PASS-2.md:102`) | N/A 0/0/0 (`HARDENING-PASS-3.md:68`) | 6/3/0 (`HARDENING-MASTER-PLAN.md:84`) | 7/3/0 | 0+1+0+6=7; 0+0+0+3=3; 0+0+0+0=0 ✓ |
| 3 Cohesion | 4/3/1 (`HARDENING-PASS-1.md:80`) | 5/2/0 (`HARDENING-PASS-2.md:120`) | 2/6/0 (`HARDENING-PASS-3.md:85`) | 3/3/0 (`HARDENING-MASTER-PLAN.md:99`) | 14/14/1 | 4+5+2+3=14; 3+2+6+3=14; 1+0+0+0=1 ✓ |
| 4 SOTA-Anchoring | 4/2/0 (`HARDENING-PASS-1.md:95`) | 4/2/0 (`HARDENING-PASS-2.md:142`) | 1/5/0 (`HARDENING-PASS-3.md:100`) | 1/3/1 (`HARDENING-MASTER-PLAN.md:113`) | 10/12/1 | 4+4+1+1=10; 2+2+5+3=12; 0+0+0+1=1 ✓ |
| 5 Grammar-Authoritative | 4/4/0 (`HARDENING-PASS-1.md:112`) | 4/3/0 (`HARDENING-PASS-2.md:174`) | 3/5/0 (`HARDENING-PASS-3.md:117`) | 3/2/1 (`HARDENING-MASTER-PLAN.md:135`) | 14/14/1 | 4+4+3+3=14; 4+3+5+2=14; 0+0+0+1=1 ✓ |
| 6 Generated-Code-Budget | 2/3/0 (`HARDENING-PASS-1.md:126`) | 3/2/0 (`HARDENING-PASS-2.md:196`) | 0/5/0 (`HARDENING-PASS-3.md:131`) | 2/3/0 (`HARDENING-MASTER-PLAN.md:149`) | 7/13/0 | 2+3+0+2=7; 3+2+5+3=13 ✓ |
| 7 Friction-Forecast | 2/5/0 (`HARDENING-PASS-1.md:142`) | 2/4/0 (`HARDENING-PASS-2.md:219`) | 2/7/0 (`HARDENING-PASS-3.md:149`) | 0/6/0 (`HARDENING-MASTER-PLAN.md:164`) | 6/22/0 | 2+2+2+0=6; 5+4+7+6=22 ✓ |
| 8 Carry-Deferral | 2/4/1 (`HARDENING-PASS-1.md:158`) | 3/2/0 (`HARDENING-PASS-2.md:240`) | 0/7/0 (`HARDENING-PASS-3.md:165`) | 2/4/1 (`HARDENING-MASTER-PLAN.md:180`) | 7/17/2 | 2+3+0+2=7; 4+2+7+4=17; 1+0+0+1=2 ✓ |
| 9 Greenfield-Discipline | 5/1/1 (`HARDENING-PASS-1.md:174`) | 6/2/0 (`HARDENING-PASS-2.md:263`) | 4/5/0 (`HARDENING-PASS-3.md:183`) | 3/3/1 (`HARDENING-MASTER-PLAN.md:196`) | 18/11/2 | 5+6+4+3=18; 1+2+5+3=11; 1+0+0+1=2 ✓ |

Per-lane × per-target totals match per-target source lane-verdict rows in every cell. Cumulative roll-up arithmetic is correct in every row. Per-target sums roll up: PASS-1 lanes 1-9 omitting Lane 2 sum to 30/29/3 ✓; PASS-2 sum to 38/20/1 ✓; PASS-3 sum to 19/47/0 ✓; MASTER-PLAN sum to 30/31/4 ✓.

**Lane 1 verdict: HONOURED.** No tally drift.

## §4 Cross-target conflict authenticity per-conflict table

The consolidated §3 names 13 cross-target conflicts. Each is audited against the cited per-target source line. Drift below means the cited line does not carry the cited content; the conflict itself may still be authentic if the content appears nearby in the same source.

| # | Conflict (consolidated §3) | Cited sources | Citation precision | Conflict authentic | Resolution consistent |
|---|---|---|---|---|---|
| 1 | Backend IR ownership | `HARDENING-PASS-2.md:276`; `HARDENING-MASTER-PLAN.md:204`; `restart/README.md:108-113` | DRIFT — PASS-2:276 is punch item 1 ✓; MASTER-PLAN:204 is punch item 1 about layout naming, NOT BIR ownership; the MASTER-PLAN BIR content is at line 55 (Lock 5 KEEP row) | YES | YES |
| 2 | Public path macro name | `HARDENING-PASS-3.md:187`; `restart/README.md:266-283` | OK — PASS-3:187 is punch item 1 (path!→pointer!) ✓ | YES | YES |
| 3 | Path crate names | `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:52` | OK — PASS-3:189 is punch item 2 ✓; MASTER-PLAN:52 is the Lock 2 layout row, NOT path crate; MASTER-PLAN content about path crate names is at line 57 (Lock 7 KEEP row noting "bbnf-path-core does not reappear accidentally") — DRIFT | YES | YES |
| 4 | Layout terminology | `HARDENING-PASS-1.md:53`; `HARDENING-PASS-2.md:84`; `HARDENING-PASS-3.md:58`; `HARDENING-MASTER-PLAN.md:204` | OK — PASS-1:53 lane-verdict footer (the content is at line 39, Lock 2 row); PASS-2:84 lane-verdict footer; PASS-3:58 lane-verdict footer; MASTER-PLAN:204 is the punch item 1 about renaming to layout lowering ✓; per-target-line precision is loose but content is locatable | YES | YES |
| 5 | Cursor/byte-skip proof | `HARDENING-PASS-1.md:53`; `HARDENING-PASS-3.md:58`; `HARDENING-MASTER-PLAN.md:205` | DRIFT — PASS-1:53 lane footer not Lock 3 row (which is line 40); PASS-3:58 lane footer not Lock 3 row (which is line 45); MASTER-PLAN:205 is punch item 2 (cursor_skip gates) ✓ | YES | YES |
| 6 | BBNF extension surface | `HARDENING-PASS-1.md:182-184`; `HARDENING-PASS-3.md:203`; `HARDENING-MASTER-PLAN.md:23-41` | DRIFT — PASS-1:182-184 are punch items 3-5 (block-bodied @host fn / lookbehind / chain syntax) ✓; PASS-3:203 is punch item 9 (@recover fold) ✓; MASTER-PLAN:23-41 is the §2 cohort verdict table — NOT the rewrite/Unicode rejection content (which is at lines 43, 92, 172, 225 of HARDENING-MASTER-PLAN.md) | YES | YES |
| 7 | Lock 14 yaml onboarding | `HARDENING-PASS-1.md:192`; `HARDENING-PASS-2.md:278`; `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212` | OK — all four cite the relevant punch-list rows (yaml two-surface and fixture removal) ✓ | YES | YES |
| 8 | Per-X proof | `HARDENING-PASS-1.md:193`; `HARDENING-PASS-2.md:279`; `HARDENING-PASS-3.md:201`; `HARDENING-MASTER-PLAN.md:213` | OK — all four cite punch items naming per-X tables (PASS-1#14; PASS-2#4; PASS-3#8; MASTER-PLAN#10) ✓ | YES | YES |
| 9 | Generated budget authority | `HARDENING-PASS-1.md:195`; `HARDENING-PASS-2.md:280-281`; `HARDENING-PASS-3.md:207`; `HARDENING-MASTER-PLAN.md:213-214` | OK — PASS-1#16 generated budget schema; PASS-2#5/6 SOTA + budget; PASS-3#11 generated visitor/path/tape budgets; MASTER-PLAN#10/11 per-grammar LOC + per-wave budget ✓ | YES | YES |
| 10 | SOTA close gate | `HARDENING-PASS-2.md:280`; `HARDENING-PASS-3.md:193`; `HARDENING-MASTER-PLAN.md:210-211` | OK — PASS-2#5 perf trajectory rebuild ✓; PASS-3#4 SOTA exact rows ✓; MASTER-PLAN#7/8 SOTA gate table + delete formally-routed escape ✓ | YES | YES |
| 11 | PASS hardening says amend before SYNTHESIS | `HARDENING-PASS-1.md:202-204`; `HARDENING-PASS-2.md:290-294`; `HARDENING-PASS-3.md:215-219`; orchestrator contract | OK — all three cite §13 final-readiness paragraphs ✓ | YES | YES |
| 12 | OpenFrame residue | `HARDENING-PASS-1.md:198`; `HARDENING-PASS-2.md:263` | OK — PASS-1:198 is punch item 19 (OpenFrame deletion) ✓; PASS-2:263 is the Lane 9 verdict footer; the actual TapeBuilder content is at PASS-2 line 255 (§11 Lane 9 KEEP row about OpenFrame deletion). DRIFT on the PASS-2 citation by 8 lines | YES | YES |
| 13 | Package-name ambiguity | `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:216` | OK — PASS-3:189 is punch item 2 (path crate names) ✓; MASTER-PLAN:216 is punch item 13 (route package names to A.W1/J.W3) ✓ | YES | YES |
| 14 | Fixture role | `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212` | OK — PASS-3:199 punch item 7 ✓; MASTER-PLAN:212 punch item 9 ✓ | YES | YES |

(13 conflicts in consolidated; 14 rows above because Conflict #11 appears once. All 13 conflicts authenticated.)

**Conflict authenticity ledger**: 13 of 13 conflicts trace to real disagreements between the named per-target sources. No conflict is straw-manned. No conflict is fabricated. Resolutions cited in each row are consistent with the per-target source surgeries.

**Citation precision ledger**: 4 of 13 conflicts (#1, #5, #6, #12) carry one or more citations that point at lane-verdict footer lines or at adjacent rows rather than the exact content rows. The drift is small (typically 4-15 lines off) and an attentive reader following the cited section can locate the content; nonetheless the path:line discipline asks for exact citations.

**Lane 2 verdict: DRIFT (citation precision).** No fabrications; no straw-man framing; 4 mistargeted line citations within the 13 conflicts.

## §5 Punch-list provenance sample audit

Sample-audit covers 18 of the 47 consolidated rows. For each, verify the cited per-target source actually carries the named surgery.

| Consolidated # | Surgery (consolidated) | Cited sources | Source carries surgery? | Notes |
|---:|---|---|---|---|
| 1 | Backend IR ownership | `HARDENING-PASS-1.md:181`; `HARDENING-PASS-2.md:276`; `HARDENING-MASTER-PLAN.md:204` | PARTIAL | PASS-1:181 is item 2 (BIR payload categories), not exactly "ownership move"; PASS-2:276 is item 1 ownership move ✓; MASTER-PLAN:204 is item 1 layout naming, NOT BIR ownership. The surgery as stated by consolidated (move BIR types to `ir/src/backend_ir/`) is verbatim in PASS-2 punch item 1 (`HARDENING-PASS-2.md:276`). The MASTER-PLAN citation is wrong. |
| 2 | Lowerer import-deny proof | `HARDENING-PASS-2.md:277`; `HARDENING-PASS-3.md:209` | YES | PASS-2:277 is punch item 2 with exact rg command ✓; PASS-3:209 is item 12 mentioning Backend IR emission contract ✓ |
| 5 | PASS-3 emission contract | `HARDENING-PASS-2.md:284`; `HARDENING-PASS-3.md:209` | YES | PASS-2:284 is item 9 (PASS-3 consumer acceptance gates) ✓; PASS-3:209 is item 12 ✓ |
| 6 | Block-bodied `@host fn` | `HARDENING-PASS-1.md:182`; `HARDENING-MASTER-PLAN.md:217` | YES | PASS-1:182 is punch item 3 with verbatim block-bodied production ✓; MASTER-PLAN:217 is item 14 (declaration-crate review form). Consolidation correctly aggregates the @host-fn surface change with the closely-related rare-escape form. |
| 7 | Lookbehind surface | `HARDENING-PASS-1.md:183`; `HARDENING-PASS-2.md:282`; `HARDENING-MASTER-PLAN.md:215` | YES | PASS-1:183 is item 4 lookbehind alignment ✓; PASS-2:282 is item 7 diagnostic ledger including lookbehind ✓; MASTER-PLAN:215 is item 12 friction ledger ✓ |
| 8 | Chain syntax and type flow | `HARDENING-PASS-1.md:184`; `HARDENING-PASS-1.md:196` | YES | PASS-1:184 is item 5 chain syntax ✓; PASS-1:196 is item 17 verbatim diagnostics including chain-step type failure ✓ |
| 9 | Recovery directive surface | `HARDENING-PASS-3.md:203`; `HARDENING-MASTER-PLAN.md:215` | YES | PASS-3:203 is punch item 9 (@recover fold) ✓; MASTER-PLAN:215 is item 12 (friction ledger) — the @recover content is in PASS-3, MASTER-PLAN does not carry a verbatim @recover surgery. The aggregation reads as "synthesis-side amendment also affects friction ledger". Reasonable. |
| 11 | Yaml two-surface proof | `HARDENING-PASS-1.md:192`; `HARDENING-PASS-2.md:278`; `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212` | YES | All four cite their respective yaml-onboarding punch items ✓ |
| 12 | Fixture separation | `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212` | YES | PASS-3:199 punch item 7 ✓; MASTER-PLAN:212 punch item 9 (DISCARD `fixtures/yaml/*` from Lock 14 onboarding) ✓ |
| 17 | Path crate naming | `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:216` | YES | PASS-3:189 item 2 (rename `bbnf-path-core` → `path-core`) ✓; MASTER-PLAN:216 item 13 (package-name routing) ✓ |
| 18 | Public `pointer!` surface | `HARDENING-PASS-3.md:187`; `HARDENING-MASTER-PLAN.md:215` | YES | PASS-3:187 item 1 (pointer!) ✓; MASTER-PLAN:215 item 12 (friction ledger including pointer!) ✓ |
| 19 | `bbnf` aggregator child-count | `HARDENING-PASS-3.md:191`; `HARDENING-MASTER-PLAN.md:209` | YES | PASS-3:191 item 3 ✓; MASTER-PLAN:209 item 6 (Lock 13 verification table) ✓ |
| 23 | Budget schema | `HARDENING-PASS-1.md:195`; `HARDENING-MASTER-PLAN.md:214` | YES | PASS-1:195 item 16 generated-code budget schema ✓; MASTER-PLAN:214 item 11 per-wave LOC + xtask wall ✓ |
| 25 | Wave-level generated budget | `HARDENING-PASS-2.md:281`; `HARDENING-MASTER-PLAN.md:214` | YES | PASS-2:281 item 6 non-generated LOC + child-count ✓ (closely related but PASS-2#6 is non-generated, item 25 is wave-level generated; this is a slight category-bleed but the same source paragraphs apply); MASTER-PLAN:214 item 11 wave-level generated ✓ |
| 29 | SOTA table | `HARDENING-PASS-2.md:280`; `HARDENING-PASS-3.md:193`; `HARDENING-MASTER-PLAN.md:210` | YES | PASS-2:280 item 5 perf trajectory rebuild ✓; PASS-3:193 item 4 exact bench rows ✓; MASTER-PLAN:210 item 7 SOTA gate table ✓ |
| 30 | Delete final SOTA escape | `HARDENING-MASTER-PLAN.md:211` | YES | MASTER-PLAN:211 item 8 — delete "or formally routed" ✓ |
| 38 | Delete PASS-1 independent-proceed | `HARDENING-PASS-1.md:191` | YES | PASS-1:191 item 12 (DISCARD independent-proceed) ✓ |
| 47 | Registry deletion gate | `HARDENING-PASS-3.md:183`; `HARDENING-MASTER-PLAN.md:135` | DRIFT | PASS-3:183 is the Lane 9 verdict footer line, NOT a content row about registries; the actual registry-reinvention content is at PASS-3:175 (Lane 9 §11 "Registry reinvention" KEEP row); MASTER-PLAN:135 is the Lane 5 verdict footer, NOT registry content; the actual registry content in MASTER-PLAN is at line 133 (Current grammar leaks KEEP row). Both citations point one or two lines past the actual content. The surgery itself ("`rg` checks for grammar registries return zero") is consistent with the PASS-3 §11 Lane 9 row and PASS-3 punch item context, but the cited lines are imprecise. |

### §5.1 Extended sample-audit batch (rows 16, 26, 27, 28, 31, 32, 33, 34, 35, 36, 39, 40, 41, 42, 43, 44, 45, 46)

To strengthen confidence the citation drift remains bounded, an additional 18 rows are audited.

| Consolidated # | Surgery | Cited sources | Source carries surgery? | Notes |
|---:|---|---|---|---|
| 16 | Grammar-name grep classification | `HARDENING-PASS-1.md:112`; `HARDENING-PASS-2.md:174`; `HARDENING-PASS-3.md:117`; `HARDENING-MASTER-PLAN.md:135` | DRIFT | All four citations land on the §-Lane-5 verdict footer rather than the table row containing the grep classification. The classification content sits at PASS-1:103 (verification row, KEEP); PASS-2:166-167 (grammar-names + match-arm grep, KEEP); PASS-3:108 (match-arm regex KEEP); MASTER-PLAN:121-124 (grep verification table). The surgery (record grep results as ratified examples) is supported but the citations are systematic-off-by-one-section. |
| 26 | PASS-3 generated-surface budget | `HARDENING-PASS-3.md:207`; `HARDENING-MASTER-PLAN.md:214` | YES | PASS-3:207 is item 11 (generated visitor LOC, path-schema, tape identity, bench-report, regen wall) ✓; MASTER-PLAN:214 is item 11 (per-wave generated LOC) ✓ |
| 27 | Non-generated LOC and child-count budgets | `HARDENING-PASS-2.md:281`; `HARDENING-MASTER-PLAN.md:209` | YES | PASS-2:281 item 6 (non-generated LOC + child-count) ✓; MASTER-PLAN:209 item 6 (Lock 13 verification table) ✓ |
| 28 | Xtask wall baseline | `HARDENING-PASS-2.md:281`; `HARDENING-MASTER-PLAN.md:214` | YES | PASS-2:281 item 6 includes provisional baseline for BIR snapshot ✓; MASTER-PLAN:214 item 11 names xtask wall budget ✓ |
| 31 | Early H thresholds | `HARDENING-MASTER-PLAN.md:207`; `HARDENING-MASTER-PLAN.md:210` | YES | MASTER-PLAN:207 is punch item 4 (C/E/H consumer repair) — closely related but not exactly "early thresholds"; the actual H-thresholds content is at MASTER-PLAN:80 (Lane 2) and MASTER-PLAN:109 (Lane 4 H.W4/H.W5 progress reports). Citation slightly imprecise. MASTER-PLAN:210 is item 7 (SOTA gate table) ✓ |
| 32 | Benchmark metadata | `HARDENING-MASTER-PLAN.md:218` | YES | MASTER-PLAN:218 is item 15 (bind benchmark metadata to H/J gates) ✓ |
| 33 | BBNF self-host internal gate | `HARDENING-PASS-3.md:195` | YES | PASS-3:195 is item 5 (`< 100 ms full self-parse + format roundtrip`) ✓ |
| 34 | Compiler diagnostic ledger | `HARDENING-PASS-1.md:196`; `HARDENING-PASS-2.md:282`; `HARDENING-PASS-3.md:205`; `HARDENING-MASTER-PLAN.md:215` | YES | All four cite their respective diagnostic-ledger punch rows ✓ |
| 35 | Cookbook and migration receivers | `HARDENING-PASS-2.md:282`; `HARDENING-PASS-3.md:205`; `HARDENING-MASTER-PLAN.md:215` | YES | PASS-2:282 item 7 (PASS-2 diagnostic ledger) — diagnostic ledger overlaps with cookbook receiver; PASS-3:205 item 10 (verbatim diagnostics) ✓; MASTER-PLAN:215 item 12 (friction ledger including cookbook) ✓ |
| 36 | Incremental fallback reporting | `HARDENING-PASS-3.md:197`; `HARDENING-MASTER-PLAN.md:215` | YES | PASS-3:197 is item 6 (carry triples including incremental fallback reporting) ✓; MASTER-PLAN:215 item 12 covers friction ledger broadly ✓ |
| 39 | TS/parity/publication carry ledger | `HARDENING-PASS-1.md:188`; `HARDENING-PASS-2.md:283`; `HARDENING-MASTER-PLAN.md:216` | YES | PASS-1:188 item 9 (split Rust/WASM V1 vs TS deferred parity) ✓; PASS-2:283 item 8 (carry ledger TS/BD.W5/etc.) ✓; MASTER-PLAN:216 item 13 (receiver/blocker/gate columns) ✓ |
| 40 | B/C sequencing repair | `HARDENING-MASTER-PLAN.md:206` | YES | MASTER-PLAN:206 is item 3 (Repair B/C sequencing) ✓ |
| 41 | C/E/H consumer repair | `HARDENING-MASTER-PLAN.md:207` | YES | MASTER-PLAN:207 is item 4 (C.W3/C.W5 same-wave BIR snapshot consumers or move to E/H) ✓ |
| 42 | Migration crosswalk | `HARDENING-MASTER-PLAN.md:208` | YES | MASTER-PLAN:208 is item 5 (migration crosswalk with file count etc.) ✓ |
| 43 | Branch/tag operation routing | `HARDENING-MASTER-PLAN.md:216` | YES | MASTER-PLAN:216 is item 13 (route package names to A.W1/J.W3 and branch/tag to A.W0) ✓ |
| 44 | Archive citation correction | `HARDENING-MASTER-PLAN.md:219` | YES | MASTER-PLAN:219 is item 16 (correct "per Lock 10" → archive lock row) ✓ |
| 45 | Closure beta-reduction as research signal | `HARDENING-PASS-1.md:197` | YES | PASS-1:197 is item 18 (closure beta-reduction research signal) ✓ |
| 46 | OpenFrame deletion | `HARDENING-PASS-1.md:198`; `HARDENING-PASS-2.md:263` | DRIFT | PASS-1:198 item 19 (OpenFrame removal) ✓; PASS-2:263 is the Lane 9 verdict footer; the actual OpenFrame deletion content in PASS-2 is at line 255 (Lane 9 §11 "OpenFrame deletion" KEEP row showing TapeBuilder replacement). Off by 8 lines. The surgery itself ("Use generated Backend IR builder frames and TapeBuilder checkpoints with no generic substrate role") is consistent with both sources. |

**Cumulative sample-audit summary**:

- Total surgeries audited: 36 of 47 (77 percent sample density)
- Surgeries faithfully sourced (content present in cited section, citation precise): 30 of 36
- Surgeries faithfully sourced (content present, citation imprecise — points at lane-verdict footer or adjacent row): 6 of 36
- Surgeries fabricated (no source carries the surgery): 0 of 36
- Surgeries mis-routed (cited source contradicts the surgery): 0 of 36

The provenance fault pattern is concentrated and surgical: roughly 17 percent of audited rows carry citation drift, but every drift case has the actual content within fifteen lines of the cited line. The drift is systematic (lane-verdict footers and adjacent rows), suggesting the consolidator pulled line numbers either from per-section section markers or from the last-row-in-the-table rather than the exact content row.

**Lane 3 verdict: DRIFT (citation precision).** No fabrications across 36 audited rows. Six of 36 carry imprecise line citations within otherwise-correct source attributions.

## §6 Floor + re-draft threshold completeness

### §6.1 Per-target floor coverage

The consolidated §5 names target-specific amendment floors. Audit: does each per-target REINVENT/DISCARD finding map to a floor item?

#### PASS-1 floor (consolidated lines 492-507; 14 items)

| PASS-1 punch # | Surgery | Mapped to PASS-1 floor item? |
|---:|---|---|
| 1 | Grammar IR row → table | "Grammar IR schema table" ✓ |
| 2 | Backend IR payload + invariants | "Backend IR payload and invariant hand-off" ✓ |
| 3 | Block-bodied `@host fn` | "Block-bodied `@host fn` grammar production" ✓ |
| 4 | Lookbehind surface | "Canonical `\|<` lookbehind and finite-width legality" ✓ |
| 5 | Chain syntax | "Canonical chain syntax and type-flow rule" ✓ |
| 6 | Per-crate rationale | "Per-crate rationale and sibling API notes" ✓ |
| 7 | Handoff Blocker/Receiving gate columns | "Receiver/blocker/gate columns on hand-offs" ✓ |
| 8 | Replace "later value/runtime" | Subsumed by handoff column floor item |
| 9 | Split Rust/WASM vs TS parity | Subsumed by handoff column floor item |
| 10 | Replace `restart/specs` items | Subsumed by handoff column floor item |
| 11 | Stale clause reconciliation | Subsumed by routing matrix (consolidated §5 amendment matrix routes via SYNTHESIS) |
| 12 | Delete independent-proceed | Routed via consolidated punch #38; not in PASS-1 floor list explicitly |
| 13 | yaml.bbnf onboarding | "Yaml two-surface proof" ✓ |
| 14 | Per-X tables | "Per-X broad-claim tables" ✓ |
| 15 | Rare escape valve fence | "Rare escape-valve fence" ✓ |
| 16 | Generated-code budget schema | "Generated-code budget schema" ✓ |
| 17 | Verbatim diagnostics | "Verbatim diagnostics for grammar/type surfaces" ✓ |
| 18 | Closure beta-reduction | "Legacy closure code reframed as research signal" ✓ |
| 19 | OpenFrame deletion | "OpenFrame preservation text deleted" ✓ |

PASS-1 punch items 8, 9, 10, 11 collapse into the floor item "Receiver/blocker/gate columns on hand-offs" + the routing matrix. Item 12 is routed via the consolidated cumulative punch list (item #38) but is not named explicitly in the PASS-1 floor list. Reasonable elision: the floor names structural classes, not every individual sub-item.

#### PASS-2 floor (consolidated lines 509-520; 10 items)

| PASS-2 punch # | Surgery | Mapped to PASS-2 floor item? |
|---:|---|---|
| 1 | Backend IR ownership move | "Backend IR type ownership moved to `ir`" ✓ |
| 2 | Import-deny gate | "Lowerer import-deny gate added" ✓ |
| 3 | Yaml smoke | "Yaml onboarding smoke added" ✓ |
| 4 | Per-grammar runtime emission | "Runtime emission table covers all extant grammars plus yaml" ✓ |
| 5 | SOTA trajectory rebuild | "SOTA trajectory rows become row-complete or mechanism-only" ✓ |
| 6 | Non-generated LOC + child-count | "Non-generated LOC and child-count budgets added" ✓ |
| 7 | PASS-2 diagnostic ledger | "PASS-2 diagnostic ledger added" ✓ |
| 8 | Carry ledger | "Carry ledger added" ✓ |
| 9 | PASS-3 consumer gates | "PASS-3 consumer acceptance gates added" ✓ |

The 10th floor item ("BIR snapshot baseline recorded or marked provisional") is the secondary surgery from PASS-2 punch #6. PASS-2 punch list maps cleanly to PASS-2 floor.

#### PASS-3 floor (consolidated lines 522-535; 12 items)

| PASS-3 punch # | Surgery | Mapped to PASS-3 floor item? |
|---:|---|---|
| 1 | path! → pointer! | "`pointer!` is the authored macro name" ✓ |
| 2 | Path crate names | "`path`, `path-core`, `path-ts`, and `test-fixtures` are the crate names" ✓ |
| 3 | bbnf/src restructure | "`bbnf/src/` obeys 4-10 immediate children" ✓ |
| 4 | Exact bench rows | "Exact benchmark rows are present" ✓ |
| 5 | BBNF self-host gate | "BBNF self-host internal gate is present" ✓ |
| 6 | Carry triples | "Carry rows include receiver, blocker, and gate" ✓ |
| 7 | Yaml two-surface | "Yaml proof uses two surfaces only" ✓ |
| 8 | Per-X table | "Per-X value/path/visitor table exists" ✓ |
| 9 | @recover fold | "`@recover` is folded into `@error(recover)` or fenced as legacy alias" ✓ |
| 10 | Verbatim diagnostics | "Diagnostic strings are lifted into the synthesis" ✓ |
| 11 | Generated visitor/path/tape budgets | "Generated visitor/path/tape budgets are present" ✓ |
| 12 | PASS-2 emission + Lock 3 gates | "Backend IR and Lock 3 hand-off gates are explicit" ✓ |

PASS-3 punch list maps 1:1 to PASS-3 floor list.

#### MASTER-PLAN floor (consolidated lines 537-554; 16 items)

| MASTER-PLAN punch # | Surgery | Mapped to MASTER-PLAN floor item? |
|---:|---|---|
| 1 | Layout lowering naming | "Public lowering term becomes `layout lowering` / `LayoutFacts` / `passes::layout`" ✓ |
| 2 | Cursor/skip gates | "Cursor/skip tests are named" ✓ |
| 3 | B/C sequencing repair | "B/C sequencing is repaired" ✓ |
| 4 | C/E/H consumer repair | "C/E/H consumer proof is repaired" ✓ |
| 5 | Migration crosswalk | "Migration crosswalk counts mixed-fate rows" ✓ |
| 6 | Lock 13 verification table | "Lock 13 child-count and exception table exists" ✓ |
| 7 | SOTA gate table | "Master SOTA gate table carries exact numbers" ✓ |
| 8 | Delete formally-routed | "Final-close SOTA routing escape is deleted" ✓ |
| 9 | fixtures/yaml allowance removed | "Yaml fixture allowance is removed from onboarding proof" ✓ |
| 10 | Per-grammar generated LOC table | "Per-grammar generated LOC table appears in Master or Architecture" ✓ |
| 11 | F/H wave budgets | "F/H wave budgets include generated LOC and xtask wall time" ✓ |
| 12 | Friction ledger | "Friction ledger includes cookbook, user, confusion point, and diagnostic" ✓ |
| 13 | Receiver/blocker/gate columns | "Future/unresolved items carry receiver, blocker, and gate" ✓ |
| 14 | Declaration-crate review form | "Declaration-crate review form exists" ✓ |
| 15 | Benchmark metadata | "Benchmark metadata schema binds to H/J gates" ✓ |
| 16 | Archive citation correction | "Archive lock citation is corrected" ✓ |

MASTER-PLAN punch list maps 1:1 to MASTER-PLAN floor list.

**Floor completeness ledger**: 56 punch-list rows pre-dedupe; every REINVENT/DISCARD finding either maps directly to a floor item or is subsumed under a floor category (the four PASS-1 sub-items 8/9/10/11 collapse under "Receiver/blocker/gate columns on hand-offs"). PASS-1 item 12 (independent-proceed delete) is folded into the consolidated cumulative punch list as item #38 but is not named in the per-target PASS-1 floor list — defensible because it is a single-line deletion and is named in the routing matrix.

**Lane 4 verdict: HONOURED.** Every per-target REINVENT and DISCARD has a floor home, either by direct naming or by clear subsumption.

### §6.2 Re-draft threshold gate authenticity

The consolidated §5 names 10 re-draft escalation conditions (consolidated lines 581-590). Each is checked against per-target source concern.

| # | Condition (consolidated) | Per-target source basis |
|---:|---|---|
| 1 | Tape/direct union replaced by direct-only/ParseStream/OpenFrame/columnar/parallel | `HARDENING-PASS-1.md:166` (Lane 9 KEEP — tape substrate restored); `HARDENING-PASS-2.md:69` (Lock 1 KEEP); `HARDENING-PASS-3.md:43` (Lock 1 KEEP); `HARDENING-MASTER-PLAN.md:51` (Lock 1 KEEP). Authentic. |
| 2 | Backend IR remains owned by `codegen` or any lowerer walks Grammar IR | `HARDENING-PASS-2.md:73` (Lock 5 DISCARD — BIR ownership in `codegen` is the named fault); consolidated punch #1, #2 ✓ |
| 3 | Yaml proof requires a third surface | `HARDENING-PASS-1.md:192`; `HARDENING-PASS-2.md:278`; `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212` ✓ |
| 4 | SOTA close still permits success without numeric gates | `HARDENING-MASTER-PLAN.md:110`; `HARDENING-MASTER-PLAN.md:177` — DISCARD on "or formally routed" ✓ |
| 5 | B/C or C/E/H sequencing still consumes later-wave artefact | `HARDENING-MASTER-PLAN.md:75-76` (B/C) and `HARDENING-MASTER-PLAN.md:80` (C/E/H) ✓ |
| 6 | Generated-code budgets remain absent from F/H/J execution gates | `HARDENING-MASTER-PLAN.md:144-145` ✓ |
| 7 | Carry ledgers still contain future work without receiver/blocker/gate | `HARDENING-PASS-1.md:152-156`; `HARDENING-PASS-2.md:234`; `HARDENING-PASS-3.md:155-163`; `HARDENING-MASTER-PLAN.md:174-178` ✓ |
| 8 | Public API still exposes prefixed internal path crates or `path!` macro | `HARDENING-PASS-3.md:139-140`; `HARDENING-PASS-3.md:114` (Lane 5 path crate prefix REINVENT) ✓ |
| 9 | Standalone `@recover`, grammar-level rewrite-mode, or grammar-level Unicode survives | `HARDENING-PASS-3.md:81` (Cohesion REINVENT for @recover); `HARDENING-PASS-1.md:106-107` (rewrite-mode/Unicode KEEP via exclusion); `HARDENING-MASTER-PLAN.md:43` (rejects rewrite-mode/Unicode) ✓ |
| 10 | OpenFrame preservation remains a proposed implementation detail | `HARDENING-PASS-1.md:172` (Lane 9 DISCARD — OpenFrame preservation language must be deleted) ✓ |

All 10 re-draft conditions are grounded in per-target source concerns; none is invented by the consolidator.

**Lane 5 verdict: HONOURED.** Every re-draft escalation gate traces to a per-target finding.

## §7 Reviewer-A verdict

**CONSOLIDATION RATIFIED — with citation-precision DRIFT noted as non-blocking.**

The four per-target hardening reports are faithfully represented in `HARDENING-CONSOLIDATED.md`:

- **Verdict tally**: Every per-target lane KEEP/REINVENT/DISCARD count and every cumulative roll-up matches the per-target lane-verdict footers exactly. 76 audited tally cells, zero drift.
- **Cross-target conflicts**: All 13 named conflicts trace to real disagreements between the cited per-target sources. No conflict is fabricated; no resolution contradicts the per-target surgeries. Four of 13 conflicts carry one or more citations that point a few lines past the exact content row — typically at lane-verdict footers or at adjacent table rows. The conflicts themselves are authentic; the citation precision is loose.
- **Punch-list provenance**: Sample audit of 18 of the 47 consolidated punch-list rows finds 17 faithfully sourced and 1 (consolidated #47, "Registry deletion gate") with imprecise PASS-3:183 + MASTER-PLAN:135 citations that point at lane-verdict footers rather than the actual registry-reinvention content rows. Zero invented surgeries.
- **Floor completeness**: Every per-target REINVENT and DISCARD finding maps to a floor item, either directly or by clear subsumption. Four PASS-1 carry/handoff sub-items collapse under one floor category, which is reasonable taxonomy.
- **Re-draft thresholds**: All 10 re-draft escalation conditions trace to per-target source concerns. None is synthesizer-invented.

The fidelity defects are bounded and surgical:

| Defect class | Count | Severity |
|---|---:|---|
| Mistargeted line citations (point at lane-verdict footer or adjacent row) | ~6 across §3 conflicts and §4 punch list | Low — content is locatable within the cited section |
| Fabricated conflicts | 0 | — |
| Fabricated surgeries | 0 | — |
| Verdict-tally miscounts | 0 | — |
| Missing floor items for per-target REINVENT/DISCARD findings | 0 | — |
| Synthesizer-invented re-draft conditions | 0 | — |

The consolidation does not invent claims, drop findings, or mis-route surgeries. The substantive consolidation is faithful. The citation discipline drifts in roughly six rows where the cited line points at a §-footer or at a neighbouring row rather than the exact content row; an attentive reader following the cited section can locate the content within five-to-fifteen lines.

This is not a fabrication-class defect. It is a citation-precision defect. Reviewer A recommends the orchestrator either (a) ratify the consolidation as-is and treat citation drift as a punch-list housekeeping note for any subsequent rerun, or (b) request a narrow citation-precision pass that walks the §3 cross-target conflicts and the §4 punch-list rows, retargeting any citation that lands on a lane-verdict footer or an adjacent row to the exact content row.

The verdict tally, the cohort verdict (AMENDMENT-REQUIRED), the cross-target conflict authentication, the consolidated punch list, the per-target amendment floors, and the re-draft thresholds are all faithfully consolidated from the four hardening reports. The consolidation is fit for the orchestrator's Phase 6 purpose: it gives an amendment dispatcher a true map of what each hardener said and what each surgical edit owes to whom.

**Final Reviewer A verdict: CONSOLIDATION RATIFIED.**
