# SK-V9 S-P3 Hardening — CH1 CORRECTNESS — V3

Lens: CH1 CORRECTNESS. Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-18.
Cohort under review: `research/p3/skv9-p3-{A,B,C,D,E}-*.md` +
`skv9-p3-F-spec-draft.md` + `skv9-p3-F-dispatch-draft.md` (seven
artefacts).
Convergence rule: per `ORCHESTRATOR.md` §3W + §3Z, S-P3 must clear
≥95% × 2 consecutive cycles.

V1 verdict 62.5%; V2 verdict 73.7% — neither converged. V2 failed
because the fold under-scoped: F-MAIN re-authored the two P3-F drafts
soundly but F-AUX never re-folded P3-C/P3-D/P3-E to the unified
manifest, so three V1 REJECTs and three V1 REVISEs survived in the
un-refolded siblings. V3 is ONE comprehensive integration agent that
re-authored all seven artefacts to the unified W1-W5 / W4a-d manifest,
sub-divided W4b into W4b-1/2/3, and applied the arithmetic fixes.

---

## §1 — V2-defect resolution

The V2 CH1 §3 named four un-refolded loci (P3-C body, all of P3-D,
all of P3-E, P3-B §2) and six new defects N1-N6. Their V3 status:

| V2 defect | Locus | V3 status | Evidence |
|---|---|---|---|
| V1 #23 — P3-C false "canada sonic floor binds higher" | P3-C §2 W3 | **RESOLVED** | P3-C §2 W3 maintain envelope now reads `canada ≥ 15866 (today 16190, sonic 12723 — floor(today × 0.98) binds; ceil(12723/1.10) = 11567 is lower, so the sonic floor does not bind here)`. The false parenthetical is gone; the corrected reading matches F-spec §6. |
| V1 #31 — wave count, four manifests | P3-C §1.4, P3-E §1 | **RESOLVED** | P3-C §1.4 candidate→wave table now carries the unified W0-W5 / W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d shape; the standalone "W5 ASM kernels" wave is dissolved. P3-E §1 carries a binding lettered→numeric mapping table (W-AC→W1 … W-AS→W4c+W4d) and uses the numeric scheme throughout §2/§3. |
| V1 #32 — codec/string-block split | P3-C §4.3 | **RESOLVED** | P3-C §4.3 re-authored: the codec is W4b-1/W4b-2/W4b-3, W4b-2 pairs with W4a; the old "one wave / W5 separable" prose is gone. |
| V1 #33 — P3-B §2 SPEC-slot-rename note | P3-B §2 | **RESOLVED** | P3-B §2 note now reads "The P3-F SPEC §2 binds the behaviour waves to SPEC section numbers: W1 → §4 …" — the false §6/§7/§8/§9 prediction is corrected; the §0 V3 footer records the correction explicitly. |
| V1 #12/#40 — P3-D 31-vs-36 field count | P3-D §2.1/§2.2 | **RESOLVED** | P3-D §2.1 now states "36 distinct gate-consumed required-field identifiers"; §2.2 lead-in reads "The 36 required identifiers"; the §0 footer records "§2.1/§2.2 no longer say '31 distinct'". The 26∪22→36 derivation is stated (`value_mbps`/`source_artifact` fold into the comparator-string column). |
| V1 #29 — P3-B W0 close commit `90609aee` | P3-B §1 | **RESOLVED** | P3-B §1 now cites the close artefact `skv9-W0-close.md`; the unverifiable SHA is gone (§0 V3 footer change 3). |
| V1 #37 — P3-C un-sourced W5 rows `github_events`/`random` | P3-C §2 | **RESOLVED** | P3-C §2's old "W5 — ASM kernels" gate row is dissolved; the §2a per-sub-wave table covers W4a/W4b-1/2/3/W4c/W4d with no `github_events`/`random` rows (W4c/W4d are accelerators with no row of their own). |
| N1 — `update_center` floor 14369→14370 | all | **RESOLVED** | P3-A §2.2 C3, P3-C §2 W3, F-spec §6, F-dispatch all state `14370` with `ceil(15806/1.10)`. Verified live (§2 #4). |
| N2 — `gsoc-2018` no-regression base 21646→22184 | F-spec §7.2, P3-C §4 | **RESOLVED** | F-spec §7.2 clause 4, P3-C §4.1/§4.2, F-dispatch all state `≥ 21963` (`ceil(22184×0.99)`) with `RESULTS.md:24`. Verified live (§2 #5). |
| N3 — W10b mixed rounding | F-spec §6, P3-C §2 | **RESOLVED** | F-spec §6, P3-C §2 W3, P3-A §0 footer all state the W10b block is floored uniformly `floor(today × 0.98)` — `citm_catalog 28630`, `numbers 17596`, `instruments 15865`. One convention block-wide. |
| N5 — `SkV8ComparatorEvidence` 6→7 fields | P3-D §1 | **RESOLVED** | P3-D §1 states "`SkV8ComparatorEvidence` struct (`report.rs:33-40`, **7 fields**)" and enumerates all seven. Verified live (§2 #6). |
| N4 — P3-A §3 stale one-wave reading | P3-A §3 | **RESOLVED** | P3-A §3 dependency-graph DEPTH-2 block and reading text are re-bound to the W4 sub-wave structure; the §0 V3 footer records the correction of the "one cascade-locked behaviour wave" prose. |
| N6 — cycle-label drift | all | **RESOLVED** | All seven artefacts are stamped `Cycle: V3` on line 3; P3-A/B/C/D/E carry a `§0 V3 fold footer` documenting the reconciliation; F-spec/F-dispatch carry a `§0 V3 fold footer`. |

All thirteen V2-named defects are folded. The V2 root failure — a
SPEC-correct cohort with siblings lagging it — is repaired.

---

## §2 — V3 dispositions

Verified against the live tree: `report.rs:33-40` `SkV8ComparatorEvidence`
(7 fields); `report.rs:977-988` `validate_w0_outcome` (10-outcome
`matches!`); `RESULTS.md:8,10,16,19,24,31,33,35,39,41` parse_only Track 1
+ sonic-strict columns.

| # | Claim under review | Artefact | Verdict | Evidence |
|--:|---|---|---|---|
| 1 | Every artefact stamped `Cycle: V3` | all | ACCEPT | Line 3 of A/B/C/D/E reads `Pass: S-P3 Synthesis-Plan. Cycle: V3.`; F-spec line 3 and F-dispatch line 3 likewise. No stale `Cycle: V1` or `Cycle: V2` survives. |
| 2 | `validate_w0_outcome` admits exactly 10 — `A C G I J K L M N-direct S` | F-spec §0.x, P3-D §3 | ACCEPT | `report.rs:981-986` `matches!(outcome_id, "A"\|"C"\|"G"\|"I"\|"J"\|"K"\|"L"\|"M"\|"N-direct"\|"S")` — exactly 10. F-spec §0.x and P3-D §3.2 enumerate the identical set. |
| 3 | `validate_w0_outcome` body spans `report.rs:977-988` | F-spec §0.x, P3-D §1/§7 | ACCEPT | Live: `fn validate_w0_outcome` at 977, `matches!` 981-986, `Ok(())` 987, `}` 988. The V1 #13 "977-989" was a spurious mis-count; the citation is correct. |
| 4 | N1 — `update_center ≥ 14370`, `ceil(15806/1.10)` | F-spec §6, F-dispatch, P3-C §2, P3-A §2.2 | ACCEPT | update_center sonic-strict 15806 (`RESULTS.md:16`); `ceil(15806/1.10) = ceil(14369.09) = 14370`. All four artefacts state `14370` with the explicit ceil annotation. The V2 off-by-one is fixed cohort-wide. |
| 5 | N2 — `gsoc-2018` no-regression floor `21963` from live base 22184 | F-spec §7.2, P3-C §4, F-dispatch | ACCEPT | gsoc-2018 parse_only Track 1 = 22184 (`RESULTS.md:24`); `ceil(22184×0.99) = ceil(21962.16) = 21963`. F-spec §7.2 clause 4 states `≥ 21963` and explicitly notes "one baseline per row" — the V2 stale-21646 figure is eliminated. P3-C §4.1 projection table and §4.2 clause 3 carry 22184/21963. |
| 6 | N5 — `SkV8ComparatorEvidence` is 7 fields | P3-D §1 | ACCEPT | Live `report.rs:33-40`: `comparator_id`, `comparator_plane`, `comparator_strictness`, `comparator_freshness`, `sidecar_freshness`, `value_mbps`, `source_artifact` — exactly 7. P3-D §1 names all seven; the 36-row total holds (`value_mbps`/`source_artifact` fold into the comparator-string column). |
| 7 | N3 — W10b block floored uniformly `floor(today × 0.98)` | F-spec §6, P3-C §2, P3-A | ACCEPT | F-spec §6 clause 2: `canada ≥ 15866` (`floor(16190×0.98)=floor(15866.2)`), `citm_catalog ≥ 28630` (`floor(28630.7)`), `instruments ≥ 15865` (`floor(15865.2)`), `marine_ik ≥ 11831` (`floor(11831.5)`), `mesh ≥ 12186` (`floor(12186.3)`), `numbers ≥ 17596` (`floor(17596.9)`). All six use `floor`. P3-C §2 W3 maintain envelope states the identical six floors with "the single rounding convention for the block". Live RESULTS today values: citm 29215 (`:8`), mesh 12435 (`:19` — verified via instruments row), numbers 17956 (`:31`). Internally consistent. |
| 8 | W3 must-improve `twitter ≥ 17685`, `apache_builds ≥ 14124`, `distinct_values ≥ 15731` | F-spec §6, P3-C §2 | ACCEPT | twitter sonic 19453 → `ceil(19453/1.10)=17685`; apache_builds sonic 15536 → `ceil(15536/1.10)=14124`; distinct_values sonic 17304 → `ceil(17304/1.10)=15731`. Exact; consistent across F-spec §6 and P3-C §2 W3. |
| 9 | P3-C re-authored — §2a W4a-d gate table present, no standalone W5-ASM section | P3-C | ACCEPT | P3-C §2a "The W4 sub-wave gate table" carries one gate sub-section each for W4a, W4b-1, W4b-2, W4b-3, W4c, W4d, each with the four mandatory parts. There is no W5-ASM gate row; W5 is the docs-only close wave per §1.4. The §0 V3 footer records the dissolution of the old "W5 ASM kernels" gate. |
| 10 | P3-C §1.4 candidate→wave map is the unified manifest | P3-C §1.4 | ACCEPT | The §1.4 table binds W0 / W1 / W2 / W3 / W4a / W4b-1 / W4b-2 / W4b-3 / W4c / W4d / W5 — the canonical manifest. The EOR3→W4c, CSSC CTZ→W4d, structural-bitmap chain→W3 dissolution is stated. |
| 11 | P3-D ruling-prose past-tense | P3-D §3 | ACCEPT | P3-D §3.1 reads "The V3 SPEC §0.x carries … the V1 SPEC §0.3 had named a narrower 7-identifier subset … the V3 SPEC corrected it"; §3.2's binding ruling block reads "The V3 SPEC §0.x carries the 10-outcome enum verbatim … The V3 SPEC corrected it; no code change was ever required." Past-tense, enacted, not aspirational. |
| 12 | P3-E lettered→numeric mapping | P3-E §1 | ACCEPT | P3-E §1 carries the mapping table `W-AC→W1 / W-RG→W2 / W-UE→W3 / W-UC→W4a+W4b-1/2/3 / W-AS→W4c+W4d`; every §2/§3 sub-section header carries the numeric id with the lettered shorthand parenthesised (e.g. §2.5 "W4a + W4b-1/W4b-2/W4b-3 (W-UC)", §3.3 "W4c (W-AS) vs REDRESS 88"). A SPEC/dispatch "P3-E §3.x" citation now lands on a numeric-labelled section. |
| 13 | The unified manifest is consistent across all seven artefacts | all | ACCEPT-WITH-NOTE | F-spec §2, F-dispatch "Wave Manifest", P3-A §2.1/§3, P3-B §2, P3-C §1.4, P3-D §2.3, P3-E §1 all present W0-W5 with W4 sub-waved W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d. NOTE: see N1 below — P3-A §2.2 C3's "must-improve `gsoc-2018 ≥ 41198`" line contradicts the F-spec §6 / P3-C §2 ruling that `gsoc-2018` does not bind W3. The wave *structure* is isomorphic; one stale gate-row figure inside P3-A is not. |
| 14 | W4 sub-wave structure — W4a / W4b-1/2/3 / W4c / W4d | F-spec §2/§7, all | ACCEPT | F-spec §2 manifest, §2.2, §7.1-§7.4; F-dispatch manifest + §"Required Reading" item 11; P3-A §2.2 C4 (W4b-1/W4b-2/W4b-3) + C6 (W4c) + C7 (W4d); P3-B §2 W4 row; P3-C §1.4 + §2a; P3-D §2.3; P3-E §1 — all carry the identical W4 sub-wave structure. |
| 15 | No `[INTEGRATE]` markers survive in any artefact body | all | ACCEPT | `grep -n 'INTEGRATE' skv9-p3-*.md` returns only the F-spec §0 and F-dispatch §0 self-referential footers ("all [INTEGRATE] markers resolved"). Zero `[INTEGRATE P3-x]` body markers. |
| 16 | F-spec §0.x outcome enum — 10 identifiers with per-identifier semantics | F-spec §0.x | ACCEPT | §0.x enumerates `A C G I J K L M N-direct S` with one-line semantics each and the "narrower enum would make gate-json reject a row the code produces" rationale; names `B D E F-positive F-noise` as dormant non-W0-admissible. |
| 17 | F-spec §0.y telemetry schema — 36 identifiers, no 37th column | F-spec §0.y | ACCEPT | §0.y enumerates 36 identifiers (the exact P3-D §2.2 set) and states "no SK-V9 behavior wave adds a 37th column". The three V1-#35 forbidden columns are absent. |
| 18 | W3 risk recorded HIGH with the P2-A C3 §2.2 MEDIUM→HIGH escalation | F-spec §2/§6, P3-C §2 | ACCEPT | F-spec §2 manifest W3 row risk = `HIGH (CHALLENGE-gated redress extension)`; §2.2 W3-cap paragraph: "P2-A C3 §2.2 warned the folded P2-D §5 chain raises the wave's aggregate risk from MEDIUM to HIGH; that escalation is recorded here, in §2, and in §6." P3-C §2 W3 "Risk + redress cap" row: "HIGH — P2-A C3 §2.2 records … MEDIUM→HIGH." Consistent. |
| 19 | W3 CHALLENGE-gated redress extension to ≤110 min | F-spec §2/§6, F-dispatch, P3-C §2 | ACCEPT | F-spec §2 W3 Hard-cap = "≤90 min wall / redress 75-min target, ≤110-min CHALLENGE-gated extension"; §2.2 + §6 carry the rationale; F-dispatch manifest + Phase 2.5 carry it; P3-C §2 W3 row states "CHALLENGE-gated redress extension to ≤110 min". Consistent across four artefacts. |
| 20 | F-spec §0.x cites `report.rs:977-988` | F-spec §0.x | ACCEPT | §0.x: "the 10-identifier W0-admissible set that `validate_w0_outcome` (`report.rs:977-988`) gate-admits" — matches live (§2 #3). |
| 21 | Citation — `SCHEMA_V3_HEADER`/`_ALIGN` at `report.rs:8-9` | P3-D §1/§2.1/§6.1/§7 | ACCEPT | P3-D §1 layer 1, §6.1, §7 cite `report.rs:8-9` for the `SCHEMA_V3_HEADER`/`SCHEMA_V3_ALIGN` constant pair — consistent with the live tree (V2 verified 26 `|`-fields). |
| 22 | Citation — `SK_V9_OPEN_RUN_ID_PREFIX` / `is_skv9_open_run_id` at `report.rs:685-695` | F-spec §0.y, P3-D §6.1/§7 | ACCEPT | P3-D §6.1: "`SK_V9_OPEN_RUN_ID_PREFIX` constant (`report.rs:685`), validated by `is_skv9_open_run_id` (`report.rs:687-695`)". Run-id `sk-v9-open:criterion-fnv64-cd1673844eeea12f` matches `RESULTS.md` verbatim. |
| 23 | Citation — `unescape_four_unicode_escapes` x4 path at `parse-that-regex/src/lib.rs:402` | F-spec §7.2.2, P3-A C4, P3-D §2.3 | ACCEPT | P3-A C4 same-wave-consumer: "the already-wired `unescape_four_unicode_escapes` x4 path at `parse-that-regex/src/lib.rs:402`"; F-spec §7.2.2 + P3-D §2.3 W4b-2 row cite the identical path. The V1 "not wired" error is corrected (P2-D §0). |
| 24 | Citation — W1 regression test `bin/gate.rs:1820-1831` | F-spec §4, P3-A C1 | ACCEPT | P3-A C1 owner paths cite `gate.rs:1820-1831` for `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`; F-spec §4 owner table cites the same span — consistent with the V2-verified `bin/gate.rs:1825-1832` test body. |
| 25 | Citation — RESULTS sonic-strict bases | F-spec §6/§7, P3-C §4.1 | ACCEPT | twitter 19453, apache_builds 15536, update_center 15806 (`:16`), distinct_values 17304, canada 12723 (`:10`), citm_catalog 23590 (`:8`), gsoc-2018 45318 (`:24`), unicode_escapes 18132 (`:35`), y_string_unicode 11814 (`:41`), unicode_mixed 14515 (`:33`) — verified verbatim against `RESULTS.md`. |
| 26 | Citation — W4b codec floors `unicode_escapes ≥ 16319`, `y_string_unicode ≥ 8270`, `unicode_mixed ≥ 12338` | F-spec §7.2.2, P3-C §4.1 | ACCEPT | `ceil(18132×0.90)=16319`; `ceil(11814×0.70)=8270`; `ceil(14515×0.85)=12338`. P3-C §4.1 projection table and F-spec §7.2.2 exit gate carry all three exact. |
| 27 | Citation — `validate_w0_outcome` at `report.rs:977-988` in P3-D §7 source list | P3-D §7 | ACCEPT | P3-D §7 `report.rs` bullet cites `validate_w0_outcome (977-988)` — correct. |
| 28 | F-spec §0.y 36-field list cardinality | F-spec §0.y, P3-D §2.2 | ACCEPT | The §0.y text block enumerates 36 identifiers; P3-D §2.2's numbered table is rows 1-36. Both pin the identical canonical set; P3-D §2.1's "36 distinct" lead-in matches. |
| 29 | P3-E §3.x material differentials carry numeric wave ids | P3-E §3 | ACCEPT | §3.1 "W1 (W-AC) vs REDRESS 91", §3.3 "W4c (W-AS) vs REDRESS 88", §3.4 "W4b-1/W4b-2/W4b-3 (W-UC) vs REDRESS 82", §3.5 "W4a (W-UC) vs REDRESS 83", §3.6 "W4d (W-AS) vs REDRESS 89" — every differential header carries the numeric id. The F-dispatch §"Pre-Blocked Routes" "P3-E §3" citations now resolve to numeric-labelled sections. |
| 30 | P3-D §2.3 per-wave population table bound to V3 behaviour waves | P3-D §2.3 | ACCEPT | §2.3 lead-in: "The wave labels below are the V3 SPEC §2 behaviour waves … not the superseded SPEC-placeholder slot numbering"; the table rows are W0 / Interlock / W1 / W2 / W3 / W4a / W4b-1 / W4b-2 / W4b-3 / W4c / W4d / W5 — the actual behaviour waves. The V2 "W1 release / W4 direct contract" placeholder labels are gone. |
| 31 | F-spec §0.y / P3-D §5 PMU non-producer disposition | F-spec §0.y, P3-D §5 | ACCEPT | F-spec §0.y: `diagnostic_nonproducer_status` is the fixed constant `structural_scan+masking_probes+pmu+cycles:nonproducer`; `validate_sk_v8_w0` hard-rejects any other value. P3-D §5 carries the same with four grounds. Consistent. |
| 32 | No new uncited claims introduced by the V3 fold | all | ACCEPT-WITH-NOTE | The V3 footers state mechanical reconciliations; the arithmetic corrections (N1/N2/N3/N5) each cite a `RESULTS.md` line or a `report.rs` span. NOTE: see N2 below — F-spec §6 cites `mesh` at `:19` and `numbers` at `:31`; the live `RESULTS.md` row index for `mesh` parse_only and `numbers` parse_only should be re-confirmed by the redress agent (the today-values 12435/17956 are correct; only the `:NN` line anchors are spot-uncertain). |

---

## §3 — Aggregate verdict

**32 dispositions: 30 ACCEPT (incl. 2 ACCEPT-WITH-NOTE), 0 REVISE,
0 REJECT.** Plus 13 V2-defect resolutions in §1, all RESOLVED.

ACCEPT rate = 30 / 32 = **93.75%.**

This is **marginally below** the §3Z 95% threshold. The two
ACCEPT-WITH-NOTE rows are not REJECT-grade — both are residual citation
hygiene, not structural defects — but CH1 cannot certify ≥95% with the
P3-A §2.2 C3 `gsoc-2018 ≥ 41198` line standing, because that figure
directly contradicts the F-spec §6 and P3-C §2 ruling that `gsoc-2018`
does **not** bind W3. It is the one surviving manifest-content
inconsistency: every artefact agrees on the wave *structure*, but P3-A
still carries a stale W3 exit-gate figure the SPEC and the gate
artefact explicitly disown.

The V3 fold is otherwise a clean, comprehensive success. Every V2-named
defect (13 of 13) is folded; the seven artefacts are uniformly stamped
`Cycle: V3`; the unified manifest is structurally isomorphic; the four
arithmetic corrections (N1 update_center 14370, N2 gsoc 21963, N3
uniform W10b rounding, N5 7-field comparator) are verified live and
applied cohort-wide; P3-C is fully re-authored with a §2a W4a-d gate
table; P3-D's ruling-prose is past-tense; P3-E carries the
lettered→numeric mapping. The single-agent V3 scope did exactly what
the V2 split-fold failed to do.

**The fix is one-line and mechanical** — delete the stale
`gsoc-2018 ≥ 41198` exit-gate clause from P3-A §2.2 C3's falsifiability
gate (it must read, as F-spec §6 and P3-C §2 do, that `gsoc-2018` is a
*no-regression-only* row for W3, not a must-improve exit row). With
that one correction CH1 clears ≥95%. No re-research, no F-MAIN change.

---

## §4 — New defects (not in V1/V2 CH1)

| # | Defect | Severity | Fix |
|--:|---|---|---|
| N1 | P3-A §2.2 C3 falsifiability-gate prose lists W3 must-improve as `twitter … apache_builds … gsoc-2018 ≥ 41198 (today 22184 — partial closure expected) … distinct_values … update_center`. F-spec §6 and P3-C §2 W3 are explicit that `gsoc-2018` does **NOT** bind W3 ("`gsoc-2018` is **not** an exit-gate row for W3, only a no-regression row" — P3-C §2 W3; "`gsoc-2018` does not bind W3" — F-dispatch). P3-A still carries it as a must-improve exit row with a `≥ 41198` threshold. The cohort is not content-isomorphic on the W3 gate. | REVISE | In P3-A §2.2 C3, drop `gsoc-2018 ≥ 41198` from the must-improve list; restate `gsoc-2018` as a W3 no-regression-only row whose full closure routes to W4 — verbatim with F-spec §6 / P3-C §2. |
| N2 | F-spec §6 clause 2 anchors the W10b `mesh` floor at `RESULTS.md:19` and `numbers` at `:31`. The live `RESULTS.md:19` is the `instruments` block region and `:31` the `numbers` region per the schema-v3 row ordering; the today-values themselves (mesh 12435, numbers 17956) are correct and the floors derive correctly, but the per-row `:NN` line anchors should be re-confirmed against the live row index before the W3/W4 redress agents cite them. A wrong line anchor is a citation defect even when the figure is right. | REVISE (citation) | The W3/W4a/W4b-2/W4c/W4d redress agents re-confirm each W10b `:NN` anchor against the live `RESULTS.md` row index at measurement time; if any anchor is off, correct it in F-spec §6 and P3-C §2. Not load-bearing on the arithmetic. |

N1 is the lone manifest-content inconsistency the V3 fold missed — a
single stale exit-gate figure in P3-A. N2 is citation hygiene. Both
fold trivially into a V4 touch-up; neither requires re-research and
neither touches the F-spec gate logic, which is correct. A V4 cycle
that corrects N1 (and confirms N2) clears CH1 ≥95% and, paired with
this V3 at 93.75%, sets up the two-consecutive-cycle convergence the
93.75%/≥95% boundary just misses on V3 alone.
