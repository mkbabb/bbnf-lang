# SK-V9 S-P3 Hardening — CH1 CORRECTNESS — V2

Lens: CH1 CORRECTNESS. Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-18.
Cohort under review: `research/p3/skv9-p3-{A,B,C,D,E}-*.md` +
`skv9-p3-F-spec-draft.md` + `skv9-p3-F-dispatch-draft.md` (seven
artefacts).
Convergence rule: per `ORCHESTRATOR.md` §3W + §3Z, S-P3 must clear
≥95% × 2 consecutive cycles.

V1 verdict: 62.5% (30 ACCEPT / 13 REVISE / 5 REJECT) — did not converge.
V2 is the integration fold per `HARDENING-S-P3-V1-CONSOLIDATED.md`
(F-MAIN re-author of the two P3-F drafts + F-AUX surgical touch-ups).

---

## §1 — V1-REJECT + V1-REVISE resolution

CH1 V1 raised **5 REJECT** + **13 REVISE**. The fold resolution is
**asymmetric**: F-MAIN (the two P3-F drafts) is fully re-authored and
clears every V1 defect routed to it; F-AUX touched P3-A surgically and
P3-B/P3-C cosmetically; **P3-C, P3-D, and P3-E were NOT re-folded** and
each still carries the V1 wave-manifest defects in its own body.

### V1 REJECTs

| V1# | Defect | V2 status | Evidence |
|--:|---|---|---|
| **#23** | `canada` W10b floor cited fabricated/stale sonic 15871 | **FOLDED** in F-spec + P3-A; **NOT** in P3-C | F-spec §6 W3 clause 2: `canada ≥ 15866 (today 16190, sonic 12723 — today×0.98 binds, RESULTS.md:10)` — correct, live. F-spec dispatch "Load-bearing gate facts" repeats the corrected value with the explicit "no 15871 sonic floor; stale SK-V8 carryover" note. P3-A:283 `canada ≥ 15866`. **But P3-C §2 W3 maintain envelope still reads `canada ≥ 15866 (today × 0.98; sonic floor binds higher)`** — the parenthetical "sonic floor binds higher" is the exact false V1 #23 claim (canada sonic-strict 12723 does NOT bind higher than 15866). P3-C was not re-folded. |
| **#31** | Wave **count** — four irreconcilable manifests | **PARTIALLY FOLDED** | F-spec §2, F-dispatch "Wave Manifest", P3-A §3, P3-B §2 all now agree: **W0–W5, W4 sub-waved into W4a/W4b/W4c/W4d**. But **P3-C §1.4 still carries the 6-wave W0–W5 layout with W4=codec+string-block and W5=aarch64 ASM kernels** as separate waves; **P3-E still uses the W-AC/W-RG/W-UE/W-UC/W-AS letter scheme** (5 wave letters, no sub-waves). Two of seven artefacts contradict the unified manifest. |
| **#32** | Codec/string-block split contradicts P3-C §4.3 pairing | **FOLDED in F-spec; CONTRADICTION INVERTED in P3-C** | F-spec §7.2 + §2.2 now PAIR W4a (string-block) and W4b (codec) as strictly-adjacent sub-waves ("never separable"); the cascade-lock is disambiguated explicitly (§2.2 three named relations). This is the P3-C §4.3 pairing argument honoured. **However P3-C §4.3 itself still argues the codec+string-block are "one wave" with "the CSSC CTZ slice (P2-D §4.4) and the EOR3 / structural-bitmap kernels (P2-D §5) … separable into W5"** — i.e. P3-C's own text still describes the rejected 6-wave shape. F-spec is right; P3-C is stale. |
| **#33** | Wave names / SPEC-section binding — P3-B expectation unmet | **FOLDED in F-spec; STALE TEXT in P3-B** | F-spec §2 binds W1→§4, W2→§5, W3→§6, W4a-d→§7.1-7.4, W5→§8 — a clean, internally consistent section map. **But P3-B §2 note still reads: "P3-F resolves the SPEC-slot rename: the SPEC's W1 release slot is consumed by S-P3 convergence … behavior waves shift to occupy SPEC §6/§7/§8/§9."** F-spec V2 does NOT do this — it binds W1→§4, not §6. P3-B's stated expectation of P3-F is still unmet and still contradicts the F-spec section map. The V1 #33 defect survives in P3-B verbatim. |
| **#34** | P3-F §0.2 goalset table propagated the split | **FOLDED** | F-spec §0.2 goalset table now reads diagnosis #2→W4a, #3→W4b, #4→"W4a + W4b paired". Internally consistent with the paired sub-wave manifest. The V1 #34 defect (it was a consequence of #32) is cleared in F-spec. |
| **#35** | P3-F §0.4 added 3 forbidden telemetry columns | **FOLDED** | F-spec §0.y now states the schema is "the 36-identifier set … carried forward **unchanged** … **no SK-V9 behavior wave adds a 37th column**". The three forbidden fields (`checkasm_parity_status`, `union_class_column_status`, `codec_admission_basis`) are **absent** from the F-spec 36-field list (verified — the list is the exact P3-D §2.2 set). `codec_admission_basis` survives only as a per-wave *value* the W4b plan must state before redress (F-spec §7.2 entry gate), not as a column. P3-D §2.1's "no new column" ruling is honoured. |

**REJECT scorecard:** #34 and #35 fully folded. #23, #31, #32, #33
folded **in the F-MAIN layer** but the underlying defect **survives
unreconciled in a sibling artefact** (#23→P3-C, #31→P3-C+P3-E,
#32→P3-C, #33→P3-B). The cohort is not yet internally consistent.

### V1 REVISEs

| V1# | Defect | V2 status |
|--:|---|---|
| #4 | P3-F §0.3 lacked the explicit 10-identifier outcome enum | **FOLDED** — F-spec adds a dedicated §0.x "Outcome Enum" section enumerating `A C G I J K L M N-direct S` with one-line semantics each and the explicit "narrower enum would make gate-json reject a row the code produces" rationale. |
| #12 / #40 | P3-D 26/22/31/36 column-count never reconciled | **NOT FOLDED** — P3-D is untouched (still `Cycle: V1`). §2.1 still says "26 schema-v3 columns + 22 manifest columns, 31 distinct"; §2.2 lead-in still says "The 31 required identifiers" over a table numbered 1–36; §2.2 trailer says "the canonical … set is the 36-row table". The 31/36 confusion persists verbatim. F-spec §0.y sidesteps it by stating "36-identifier set" cleanly, but the cited authority (P3-D) still self-contradicts. |
| #13 | `validate_w0_outcome` cited `977-988`, actual `977-989` | **PARTIALLY FOLDED** — F-spec §0.x cites `report.rs:977-988`; live source spans `977-988` for the function body (the `Ok(())`+`}` close at 987-988). Re-verified: `fn validate_w0_outcome` at 977, the `matches!` at 981-986, `Ok(())` 987, `}` 988. **The V1 finding itself was wrong** — `977-988` is correct; V1 CH1 mis-counted. No defect. P3-D still cites `977-988` (correct). |
| #29 | P3-B W0 close commit `90609aee` unsubstantiated | **NOT FOLDED** — P3-B §1 still asserts "commit `90609aee`" with no cross-citation; no other artefact carries a W0 close SHA; the close artefact `skv9-W0-close.md` is the citable anchor. The V1 #29 REVISE survives verbatim in P3-B. |
| #30 | P3-A 8-candidate → P3-B 4-pool mapping not tabulated | **FOLDED** — P3-A §3 dependency graph now maps C1-C8 explicitly onto W1/W2/W3/W4a/W4b/W4c/W4d; F-spec §2 manifest "Shortlist candidate" column names C1..C7 per wave; F-dispatch manifest likewise. C6/C7 are recoverable (W4c/W4d). |
| #36 | P3-F §6 W3 exit gate dropped `distinct_values` | **FOLDED** — F-spec §6 clause 1 lists `twitter / apache_builds / update_center / distinct_values` (distinct_values ≥ 15731 restored). F-dispatch "W3 must-improve rows" also lists all four. |
| #37 | P3-C §5 W5 rows `github_events`/`random` un-sourced | **NOT FOLDED in P3-C; SIDESTEPPED in F-spec** — F-spec W4c (§7.3) and W4d (§7.4) are *producer/consumer accelerators* that "move no row of their own", so the F-spec carries NO `github_events`/`random` exit rows — the un-sourced figures are gone from the SPEC. **But P3-C §2 W5 still lists `github_events ≥ 19418` and `random ≥ 13788`** derived from un-cross-checked sonic figures. P3-C's stale W5 contradicts the F-spec W4c/W4d "no row of its own" gate. |
| #38 | P3-F §8 codec template "single file" vs P3-A `escape_codec/` sub-module | **FOLDED** — F-spec §7.2 owner-paths table names `codegen/src/escape_codec/` as a "(NEW sub-module)"; P3-A:398 likewise. The single-file form is gone. |
| #41 | P3-F dispatch cited superseded `alpha-C-redress-digest.md` | **FOLDED** — F-dispatch "Pre-Blocked Routes" cites `research/p3/skv9-p3-E-preblocked-ledger.md` as "the binding S-P3 ledger". The alpha digest is not cited. |
| #45 | P3-F §2 W5 LOC budget ≤600 under-provisioned the codec | **FOLDED** — F-spec §2 manifest W4b row budget is "~1,045 net incl. ~250 checkasm" — matches P2-E §7.4. The codec now owns its own sub-wave (W4b) with the correct envelope; W5 is docs-only. |
| #2,#5,#6,#7,#8,#9,#10,#11 (V1 ACCEPTs with trivial drift) | — | Carried; re-verified below. |

**REVISE scorecard:** #4, #30, #36, #38, #41, #45 folded; #13 was a
spurious V1 finding (no real defect); **#12/#40, #29, #37 NOT folded**
— each lives in an artefact (P3-D, P3-B, P3-C) the fold did not touch.

---

## §2 — V2 dispositions

Verified against the live tree: `gate.rs` `enum Outcome` (15 variants,
`gate.rs:4-20`), `gate.rs:56` slack constant, `report.rs:8-9`
`SCHEMA_V3_HEADER`/`_ALIGN` (26 `|`-fields counted), `report.rs:33-46`
`SkV8ComparatorEvidence`/`SkV8Telemetry`, `report.rs:685-695`
`SK_V9_OPEN_RUN_ID_PREFIX`/`is_skv9_open_run_id`, `report.rs:977-988`
`validate_w0_outcome`, `bin/gate.rs:1825-1832` regression test;
`RESULTS.md:5,8,10,12,16,19,24,26,29,31,33,35,39,41` parse_only Track 1
+ sonic-strict columns.

| # | Claim under review | Artefact | Verdict | Evidence |
|--:|---|---|---|---|
| 1 | `gate::Outcome` defines 15 variants | F-spec §0.x | ACCEPT | `gate.rs:4-20` — exactly 15 variants `A B C D E F-positive F-noise G I J K L M N-direct S`. F-spec §0.x correctly names `B D E F-positive F-noise` as the 5 dormant non-W0-admissible variants SK-V9 neither uses nor deletes. |
| 2 | `validate_w0_outcome` admits exactly 10: `A C G I J K L M N-direct S` | F-spec §0.x, P3-D §3 | ACCEPT | `report.rs:981-986` `matches!(outcome_id, "A"\|"C"\|"G"\|"I"\|"J"\|"K"\|"L"\|"M"\|"N-direct"\|"S")` — exactly 10. F-spec §0.x enumerates the same 10. |
| 3 | F-spec §0.x adds the explicit 10-identifier outcome enum (V1 #4 fold) | F-spec §0.x | ACCEPT | New §0.x section: the 10-identifier set, per-identifier semantics, the producer/consumer rationale, the 5-dormant-variant note. V1 #4 ("§0.3 has no target") is resolved. |
| 4 | F-spec §0.y schema is the 36-identifier set, no 37th column (V1 #35 fold) | F-spec §0.y | ACCEPT | §0.y enumerates 36 identifiers (the exact P3-D §2.2 set) and states "no SK-V9 behavior wave adds a 37th column". The three V1-#35 forbidden columns are absent. `codec_admission_basis` is a per-wave value (W4b entry gate), not a column. |
| 5 | F-spec §0.x cites `validate_w0_outcome` at `report.rs:977-988` | F-spec §0.x | ACCEPT | `fn validate_w0_outcome` body spans 977-988 in live source. V1 #13 (alleged `977-989`) was a spurious V1 mis-count — the citation is correct. |
| 6 | `DIRECT_PROJECTION_SONIC_SLACK = 1.10` | P3-C §1.2 | ACCEPT | `gate.rs:56` `pub const DIRECT_PROJECTION_SONIC_SLACK: f64 = 1.10;` — exact. |
| 7 | Regression test `w0_real_typed_metadata_expectation_…` asserts twitter/update_center true, apache/citm false | P3-A C1, F-spec §4 | ACCEPT | `bin/gate.rs:1825-1832` — `#[test]` at 1825; asserts `w0_real_typed_metadata_expected` true for twitter/update_center, `!`-false for apache_builds/citm_catalog. F-spec §4 owner table cites `gate.rs:1820-1831` (the test+attribute span) — correct. |
| 8 | `SK_V9_OPEN_RUN_ID_PREFIX` / `is_skv9_open_run_id` at `report.rs:685-695` | F-spec §0.y, P3-D §6.1 | ACCEPT | `report.rs:685` `SK_V9_OPEN_RUN_ID_PREFIX = "sk-v9-open:criterion-fnv64-"`; `is_skv9_open_run_id` at 687; 16-hex check at 690-695. Exact. |
| 9 | RESULTS run-id is `sk-v9-open:criterion-fnv64-cd1673844eeea12f` | all | ACCEPT | `RESULTS.md:48-50` carries that run-id verbatim; matches `SK_V9_OPEN_RUN_ID_PREFIX` + 16 hex. |
| 10 | `SCHEMA_V3_HEADER`/`SCHEMA_V3_ALIGN` at `report.rs:8-9`, 26 columns | P3-D §2.1, F-spec | ACCEPT | `report.rs:8` `SCHEMA_V3_HEADER`, `:9` `SCHEMA_V3_ALIGN`. Programmatic count of `|`-delimited field names = **26**. P3-D §2.1's "26 schema-v3 columns" is correct. |
| 11 | F-spec §0.y 36-field list = `RowMetadata` schema-v3 ∪ `SkV8Telemetry` ∪ `SkV8ComparatorEvidence` | F-spec §0.y, P3-D §2.2 | ACCEPT-WITH-NOTE | The 36-field list is the exact P3-D §2.2 table. NOTE: `SkV8ComparatorEvidence` has **7** struct fields in live source (`report.rs:33-40`: comparator_id, comparator_plane, comparator_strictness, comparator_freshness, sidecar_freshness, value_mbps, source_artifact); P3-D §1 says "6 fields". The 36-field count still holds (value_mbps/source_artifact fold into the comparator-string column), but P3-D's "6 fields" parenthetical is a stale citation. Carried as a P3-D defect (see #21). |
| 12 | W3 must-improve `twitter ≥ 17685` | F-spec §6, P3-C §2 | ACCEPT | twitter sonic-strict 19453 (`RESULTS.md:5`); `ceil(19453/1.10) = 17685`. Exact. |
| 13 | W3 must-improve `apache_builds ≥ 14124` | F-spec §6, P3-C §2 | ACCEPT | apache_builds sonic-strict 15536 (`:12`); `ceil(15536/1.10) = 14124`. Exact. |
| 14 | W3 must-improve `update_center ≥ 14369` | F-spec §6, P3-C §2, P3-A | **REVISE** | update_center sonic-strict 15806 (`:16`); `ceil(15806/1.10) = ceil(14369.09) = 14370`, **not 14369**. F-spec §6, F-dispatch, P3-C §2, P3-A:280 all state `14369`. Off-by-one against the artefact's own `ceil(sonic/1.10)` rule. The other three W3 rows (17685/14124/15731) are exact; only update_center is wrong. Either the rule is `floor` (then twitter `floor(17684.5)=17684` ≠ 17685, so no) or the figure is a one-unit error. Fold: `14369 → 14370`. |
| 15 | W3 must-improve `distinct_values ≥ 15731` (V1 #36 fold) | F-spec §6, P3-C §2 | ACCEPT | distinct_values sonic-strict 17304 (`:39`); `ceil(17304/1.10) = ceil(15730.9) = 15731`. Exact. Restored to the F-spec W3 exit gate — V1 #36 resolved. |
| 16 | W10b `canada ≥ 15866` — today×0.98 binds, sonic 12723 does NOT (V1 #23 fold) | F-spec §6, F-dispatch, P3-A | ACCEPT | canada parse_only Track 1 16190, sonic-strict 12723 (`RESULTS.md:10`). `int(16190×0.98)=15866`; `ceil(12723/1.10)=11567`. Max = 15866. F-spec §6 + F-dispatch state `15866` with the explicit "no 15871 sonic floor; stale SK-V8 carryover" correction. V1 #23 folded **in F-spec and P3-A**. |
| 17 | W10b `canada ≥ 15866 (… sonic floor binds higher)` | P3-C §2 W3 | **REJECT** | P3-C §2 W3 maintain envelope verbatim: `canada ≥ 15866 (today × 0.98; sonic floor binds higher)`. The parenthetical "sonic floor binds higher" is the exact false V1 #23 claim — canada sonic-strict is 12723; `ceil(12723/1.10)=11567 < 15866`, so the sonic floor binds **lower**, not higher. P3-C was not re-folded; the V1 #23 REJECT survives verbatim in P3-C. |
| 18 | W10b `citm_catalog ≥ 28631` | F-spec §6, P3-C §2, P3-A | **REVISE** | citm_catalog parse_only Track 1 29215 (`:8`); `29215 × 0.98 = 28630.7`. `int()` = 28630; `ceil()` = 28631; `round()` = 28631. F-spec/P3-C/P3-A all state `28631`. The `canada` floor in the *same block* uses `int(16190×0.98)=15866` (truncating). The block is internally inconsistent on the rounding convention: canada truncates, citm rounds up. Both rounding modes are defensible, but the cohort must pick ONE. Carried REVISE (V1 #19 ACCEPTed 28631 on `×0.98=28631`, which is `ceil`; canada at 15866 is `floor` — the inconsistency was latent in V1 too). |
| 19 | W10b `instruments ≥ 15865`, `marine_ik ≥ 11831`, `mesh ≥ 12186`, `numbers ≥ 17597` | F-spec §6, F-dispatch | ACCEPT | instruments 16189×0.98=15865.2→15865; marine_ik 12073×0.98=11831.5→11831; mesh 12435×0.98=12186.3→12186; numbers 17956×0.98=17596.9→17597. All four consistent with the stated today×0.98 rule (numbers rounds up, the others truncate — see #18 re convention). sonic-strict floors (`ceil/1.10`): instruments 15638, marine_ik 7652, mesh 10254, numbers 11793 — all below the today×0.98 leg, so today×0.98 binds for all four. Exact. |
| 20 | W4b codec floors: `unicode_escapes ≥ 16319`, `y_string_unicode ≥ 8270`, `unicode_mixed ≥ 12338` | F-spec §7.2, P3-C §4.1 | ACCEPT | `ceil(18132×0.90)=16319`; `ceil(11814×0.70)=8270`; `ceil(14515×0.85)=12338`. sonic-strict bases 18132/11814/14515 verified live (`RESULTS.md:35,41,33`). All three exact. |
| 21 | W4b `gsoc-2018 ≥ 21430` no-regression basis | F-spec §7.2, P3-C §4.1 | ACCEPT-WITH-NOTE | `ceil(21646×0.99)=21430`. NOTE: the cited baseline `21646` is P2-E-sourced; the **live `RESULTS.md:24` gsoc-2018 parse_only Track 1 is 22184**, not 21646. F-spec §7.1 itself cites "gsoc-2018 (today 22184, `:24`)". So the codec floor 21430 is computed from a *stale* baseline (21646) while the string-block clause uses the live 22184. Internal inconsistency: same row, two baselines in the same SPEC. The 21646 figure is a P2-E carryover. Fold: recompute `gsoc-2018` no-regression floor from the live 22184 (`ceil(22184×0.99)=21963`) or annotate why 21646 is retained. |
| 22 | F-spec §6 W3 exit gate `gsoc-2018` does not bind W3 | F-spec §6 | ACCEPT | F-spec §6 trailer: "`gsoc-2018` does NOT bind W3 … W3 falsifies only if the structural-rediscovery hot leaf does not drop to ≤ 5%." Consistent with P2-A §4.3 and P3-C §2 W3. |
| 23 | The unified wave manifest is internally consistent across A/B/C/D/E/F | all | **REJECT** | F-spec §2, F-dispatch manifest, P3-A §3, P3-B §2 agree on W0–W5 / W4a-d. But (a) **P3-C §1.4 candidate→wave map** still binds W4=codec+string-block, **W5=aarch64 ASM kernels** — the rejected 6-wave shape; P3-C §2 has a full W5 ASM-kernel gate row. (b) **P3-E** still uses the **W-AC/W-RG/W-UE/W-UC/W-AS** 5-letter scheme with W-UC=codec+string-block combined and W-AS=EOR3+CTZ combined — no sub-waves. Three of seven artefacts (P3-C, P3-E, and by stale-text P3-B §2) do not present the unified manifest. The cohort is not manifest-consistent. |
| 24 | F-spec cascade-lock disambiguation (V1 root-cause item 6) | F-spec §2.2 | ACCEPT | §2.2 names three distinct "same-wave" relations (cascade-lock / same-wave-consumer / codec-scanner-pairing) and gives the binding reading of P2-D §0 ("a kernel must not land *without the union substrate existing*; NOT one monolithic wave"). The CONSOLIDATED item-6 ambiguity is resolved. |
| 25 | F-spec W4 sub-wave structure resolves the 75-min redress ceiling (V1 CH4 item 7) | F-spec §2.2, §7 | ACCEPT | §2.2: monolithic W4 ≈1,595-1,860 LOC; W4a-d are four separate triumvirates each inside its own LOC budget + 75-min redress. F-dispatch Phase-3 confirms "each W4 sub-wave gets its own 75-min redress". Coherent. |
| 26 | F-spec §0.y PMU non-producer disposition | F-spec §0.y, P3-D §5 | ACCEPT | §0.y: `diagnostic_nonproducer_status` is the fixed constant `structural_scan+masking_probes+pmu+cycles:nonproducer`; `validate_sk_v8_w0` hard-rejects any other value. Matches `report.rs:341-347` live (the gate rejects a mismatched `diagnostic_nonproducer_status`). Exact. |
| 27 | F-spec §3 W0 closed with 38 rows, 36 fields, run-id frozen | F-spec §3 | ACCEPT | `RESULTS.md` carries the 38 main rows under `sk-v9-open:criterion-fnv64-cd1673844eeea12f`; §3 states "W0 populated all 36 schema fields for the 38-row baseline". Consistent with the live RESULTS manifest block. |
| 28 | F-dispatch "Pre-Blocked Routes" cites P3-E §2.x/§3.x per wave | F-dispatch | ACCEPT-WITH-NOTE | The F-dispatch §3.x citations resolve: W3→P3-E §3.2 (REDRESS 92), W4a→§3.5 (REDRESS 83), W4b→§3.4 (REDRESS 82), W4c→§3.3 (REDRESS 88), W4d→§3.6 (REDRESS 89) — each P3-E §3.x sub-section addresses the correct REDRESS entry. NOTE: P3-E §3.x sub-sections are labelled with the **old wave letters** ("§3.3 — W-AS vs REDRESS 88"), so a reader following the citation lands on a section headed `W-AS`, not `W4c`. The REDRESS-entry mapping is sound; the wave-label mismatch is the #23 manifest-drift symptom. |
| 29 | F-dispatch §0 footer + F-spec §0 footer claim all `[INTEGRATE]` markers resolved | F-spec, F-dispatch | ACCEPT | `grep -n "INTEGRATE" skv9-p3-*.md` returns only the two §0-footer self-references; **zero `[INTEGRATE P3-x]` markers survive** in any artefact body. V1's central root cause (unresolved markers) is cleared. |
| 30 | P3-A §3 dependency-graph reading text | P3-A §3 | **REVISE** | P3-A §2.2 (C6/C7 dispositions) correctly assigns C6→W4c, C7→W4d as sub-waves. But P3-A §3's prose reading still says: "P3-B should sequence C3 + C4 + C5 + C6 (+ C7) as **one cascade-locked behaviour wave** (or a tightly-coupled pair), not as independent W{n}" and quotes P2-D §0 "the wave may not be split" verbatim. P3-A is internally inconsistent: §2.2 sub-waves, §3 one-wave. The §2.2 disposition is the correct one; §3's reading text is stale-V1. |
| 31 | P3-B W0 close commit `90609aee` | P3-B §1 | **REVISE** | P3-B §1 still asserts the W0 close at "commit `90609aee`" with no cross-citation; the citable anchor is `skv9-W0-close.md`. V1 #29 unfolded. F-spec/F-dispatch correctly cite only the run-id + close artefact, never a SHA — so P3-B is the lone artefact carrying an unverifiable hash. |
| 32 | P3-B §2 SPEC-slot-rename note | P3-B §2 | **REJECT** | P3-B §2 note still states P3-F "shift[s] behavior waves to occupy SPEC §6/§7/§8/§9". F-spec V2 binds W1→§4 … W5→§8. The note's prediction is false against F-spec V2 and re-introduces the V1 #33 section-map contradiction. P3-B was cosmetically touched (the V2-fold stub at §0) but this load-bearing note was not corrected. |
| 33 | P3-D 31-vs-36 field count | P3-D §2.1, §2.2 | **REVISE** | P3-D §2.1 "31 distinct gate-consumed required-field identifiers"; §2.2 lead-in "The 31 required identifiers" over a 1–36 numbered table; §2.2 trailer "the canonical … set is the 36-row table". The 26∪22→36 overlap derivation is still not shown. V1 #12/#40 unfolded. F-spec §0.y states "36-identifier" cleanly, so the SPEC is right — but the cited authority self-contradicts. |
| 34 | P3-C §2 W5 un-sourced rows `github_events ≥ 19418`, `random ≥ 13788` | P3-C §2 | **REVISE** | P3-C §2 W5 still carries these two exit rows derived from un-cross-checked sonic figures (21360, 15166). The F-spec W4c/W4d are explicitly "no row of its own" accelerators with no such rows, so the SPEC sheds the un-sourced figures — but P3-C's stale W5 still asserts them and contradicts the F-spec W4c/W4d gate shape. V1 #37 unfolded in P3-C. |
| 35 | Citation spot-check — P3-D code anchors | P3-D §7 | ACCEPT | `report.rs:8-9`, `33-40`, `44-67`, `685-695`, `977-988`; `gate.rs:4-66`, `56`; `bin/gate.rs` regression test — all resolve (modulo the 7-vs-6 `SkV8ComparatorEvidence` field-count nit, #11/#21). |
| 36 | Citation spot-check — P3-A owner paths | P3-A §2.2 | ACCEPT | `report.rs:709` (`SK_V8_OPEN_BASELINE`), `bin/gate.rs:1820-1831`, `lib.rs:162` (`match_string_at_quote_trusted_utf8`), `lib.rs:402` (`unescape_uxxxx_x4_neon` call site), `lib.rs:718` (`unescape_string`), `unescape_uxxxx.rs`, `bbnf-simd/src/lib.rs:41` `class_table` — all consistent with the live tree and the V1-verified anchor set. |
| 37 | Citation spot-check — RESULTS sonic-strict bases | F-spec §6/§7, P3-C §4.1 | ACCEPT | twitter 19453, apache_builds 15536, update_center 15806, distinct_values 17304, canada 12723, citm_catalog 23590, instruments 17201, marine_ik 8417, mesh 11279, numbers 12972, gsoc-2018 45318, unicode_escapes 18132, y_string_unicode 11814, unicode_mixed 14515 — all 14 verified verbatim against `RESULTS.md` parse_only rows. The corrected sonic-strict floors derive from live RESULTS. |
| 38 | F-spec §6 W3 exit-gate floors derive live from RESULTS | F-spec §6 | ACCEPT-WITH-NOTE | Every §6 floor cites a `RESULTS.md:NN` line and the today/sonic figures match live — except `update_center ≥ 14369` (#14, off-by-one) and the `citm_catalog` rounding-convention nit (#18). The derivation method is sound and live-sourced; two arithmetic slips remain. |

---

## §3 — Aggregate verdict

**38 dispositions: 28 ACCEPT (incl. 4 ACCEPT-WITH-NOTE), 7 REVISE,
3 REJECT.**

ACCEPT rate = 28 / 38 = **73.7%.**

This is **below** the §3Z 95% threshold. **CH1 V2 does not converge.**

V2 is a genuine and substantial improvement over V1 (62.5% → 73.7%) and
the **F-MAIN re-author is sound**: the two P3-F drafts integrate
P3-A..E cleanly, resolve every `[INTEGRATE]` marker, carry the unified
W0–W5 / W4a-d manifest, the corrected `canada 15866` floor, the
10-outcome enum, the 36-field schema, the disambiguated cascade-lock,
and the sub-wave structure that resolves the 75-min ceiling. Were the
cohort *only* the two P3-F drafts, CH1 would pass.

The failure is **not** in F-MAIN. It is that **F-AUX was scoped too
narrowly**. The CONSOLIDATED F-AUX brief touched P3-A (C6/C7
disposition — done well) and made cosmetic edits to P3-B/P3-C (a
one-line §0 V2-fold stub), but **left the wave-manifest body of P3-C,
all of P3-D, and all of P3-E carrying their V1 content**. The
consequence:

- **P3-C** still presents the rejected 6-wave manifest (§1.4 map, §2 W5
  ASM-kernel gate, §4.3 "one wave" argument), still carries the false
  "sonic floor binds higher" canada claim (V1 #23 verbatim), and still
  carries the un-sourced `github_events`/`random` W5 rows (V1 #37).
- **P3-D** still self-contradicts on 31-vs-36 field count (V1 #12/#40).
- **P3-E** still uses the W-AC/W-RG/W-UE/W-UC/W-AS 5-letter scheme — not
  the unified W1-W5 manifest (V1 #31).
- **P3-B §2** still predicts a SPEC §6-§9 slot map that F-spec V2 does
  not perform (V1 #33 verbatim).

So three V1 REJECTs (#23, #31, #33) and three V1 REVISEs (#12/#40, #29,
#37) **survive — not in the F-MAIN layer, but in the un-refolded
siblings.** The cohort is internally inconsistent: a reader of the
F-spec sees the right plan; a reader of P3-C or P3-E sees the rejected
one. CH1 cannot certify a cohort whose own members contradict the SPEC
they are cited as authority for.

**The fix is bounded and mechanical** — re-fold P3-C, P3-D, P3-E, and
P3-B §2 to the F-spec's unified manifest. No re-research, no F-MAIN
change. A V3 fold that re-authors the four sibling sections to the
unified W0-W5/W4a-d manifest, plus the two arithmetic corrections
(#14, #21) and the rounding-convention pick (#18), should clear ≥95%.

---

## §4 — New defects (not in V1 CH1)

| # | Defect | Severity | Fix |
|--:|---|---|---|
| N1 | F-spec §6 / F-dispatch / P3-C / P3-A all state `update_center ≥ 14369`; the artefact's own `ceil(sonic_strict/1.10)` rule gives `ceil(15806/1.10)=14370`. Off-by-one. The other three W3 rows are exact. | REVISE | `14369 → 14370` in all four artefacts. |
| N2 | `gsoc-2018` no-regression floor `21430` is computed from a stale baseline `21646` (P2-E carryover); the live `RESULTS.md:24` gsoc-2018 parse_only Track 1 is `22184`. F-spec §7.1 itself cites the live `22184` for the same row — two baselines for one row in one SPEC. | REVISE | Recompute from live 22184 (`ceil(22184×0.99)=21963`) or annotate the retention of 21646. Also a latent V1 defect CH1 V1 did not catch (V1 #16 spot-checked gsoc sonic-strict 45318 but not the 21646 no-regression base). |
| N3 | The W10b maintain block mixes rounding conventions: `canada 15866` and `instruments 15865` and `mesh 12186` truncate `today×0.98`; `citm_catalog 28631` and `numbers 17597` round up. Same block, two conventions — neither F-spec nor P3-C states which is canonical. | REVISE | Pick one rounding mode for the whole W10b block and re-derive all six floors consistently. |
| N4 | P3-A is internally inconsistent: §2.2 disposes C6/C7 as sub-waves W4c/W4d; §3's prose reading still says "sequence C3+C4+C5+C6(+C7) as one cascade-locked behaviour wave … not independent W{n}" and quotes P2-D §0 "the wave may not be split". | REVISE | Re-fold P3-A §3's reading text to the sub-wave manifest; the §2.2 disposition is the correct one. |
| N5 | P3-D §1 cites `SkV8ComparatorEvidence` as "6 fields"; the live struct (`report.rs:33-40`) has **7** (comparator_id, comparator_plane, comparator_strictness, comparator_freshness, sidecar_freshness, value_mbps, source_artifact). The 36-field total is unaffected but the per-struct citation is wrong. | REVISE | `6 fields → 7 fields` in P3-D §1; re-check the 26∪22→36 overlap derivation against the correct struct cardinality. |
| N6 | The seven artefacts disagree on their own cycle label: F-spec and F-dispatch are stamped `Cycle: V2`; **P3-A, P3-B, P3-C, P3-D, P3-E are all still stamped `Cycle: V1`** (line 3 of each). P3-A/B/C carry a `§0 — V2 fold` trailer but the header is unchanged; P3-D and P3-E carry no V2-fold trailer at all. The cohort is not uniformly stamped as a V2 cohort. | REVISE | Stamp every re-folded artefact `Cycle: V2`; P3-D and P3-E need a V2-fold trailer documenting the manifest reconciliation. |

N1, N2, N3, N5 are arithmetic / citation slips the V1 CH1 review did
not surface (V1's spot-check set did not include the update_center
ceil, the gsoc no-regression base, the rounding convention, or the
`SkV8ComparatorEvidence` field count). N4 and N6 are integration-hygiene
defects produced by the partial fold itself. None is load-bearing on
its own; all six fold trivially alongside the §3 manifest-reconciliation
pass.
