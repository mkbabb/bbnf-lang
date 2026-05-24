---
agent: CH6
pass: T-P2-research
cycle: V3
lens: ANTI-PAPER-CLOSE
disposition: CONFIRM (LOCK-TRIGGER)
generated_at: 2026-05-23T22:50:00-04:00
authority:
  - restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p2/hardening/V2/CH6.md
  - restart/prompts/totality/PASS-2-RESEARCH.md §3 (CH6)
  - restart/prompts/ORCHESTRATOR.md §3Z (cohort LOCK = ≥95% × 2 consecutive)
v2_prior:
  disposition: CONFIRM
  ACCEPT_rate: 1.00
  findings: 13 (13 ACCEPT / 0 REVISE / 0 REJECT)
  REVISE_set: []
head_pin: 5aaab91d1
v3_fold_packet_consumed:
  - F-V3-CH4-A (BBNF_SIMD_STRICT=1 cohort-wide propagation at 2A:192, 2C:305, 2D:142-149)
  - F-V3-CH4-B (numeric abrogate-gate binding at 2D:151-162 — closes V2 96% qualified residue)
  - F-CH5-V2-01 (substrate_target + retention_lifetime columns at 2B §A5 + §A6)
  - F-V3-CH1-A (README 235 reconciled at 2A 3 sites)
  - F-V3-CH1-B (counted_source_ids 24→26 at 2F frontmatter)
  - NEW-CH2-V3-02 (0 orphans verified)
inputs_audited:
  - restart/audit/totality/p2/2A-sota-landscape.md (V3; 228 lines; 15 counted source IDs)
  - restart/audit/totality/p2/2B-primitive-vocabulary.md (V3; 557 lines; 24 counted source IDs; 6 admitted + 3 SKELETON N/A DELETED)
  - restart/audit/totality/p2/2C-grammar-neutrality.md (V4 preserved; 464 lines; 7 counted source IDs)
  - restart/audit/totality/p2/2D-cost-model.md (V3; 282 lines; 11 counted source IDs)
  - restart/audit/totality/p2/2E-host-arch-esoterica.md (V7 preserved through V3; 493 lines; 11 counted source IDs)
  - restart/audit/totality/p2/2F-parse-that-gaps.md (V6 preserved; 615 lines; 21 counted source IDs)
  - restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md (V3 fold authority; 116 lines)
cohort_totals_v3:
  primary_citations: 105
  techniques_grounded: 69 (2A 9 + 2B 11 + 2C 14 + 2D 7 + 2E 14 + 2F 14)
  techniques_refuted: 32 (2A 5 + 2B 5 + 2C 5 + 2D 5 + 2E 6 + 2F 6)
  skeleton_only_contracts: 0 (V1=3 → V2 DELETE-recommended → V3 DELETE-preserved)
  refutation_density: 32/101 = 31.7% of grounded+refuted binary
disposition_summary:
  ACCEPT_findings: 13
  REVISE_findings: 0
  REJECT_findings: 0
  ACCEPT_rate: 1.00
cycle_disposition: SECOND_CONSECUTIVE_GTEQ_95
lock_status: §3Z_COHORT_LOCK_TRIGGERED_AT_V3_CLOSE
---

# T-P2 V3 CH6 — ANTI-PAPER-CLOSE (LOCK-TRIGGER cycle)

## Lens Contract

CH6 V3 binds per `PASS-2-RESEARCH.md §3` and the V3 dispatch focus in
`restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md §2` (CH6
row). The sole V3 obligations are: (a) verify F-V3-CH4-B numerically
closes the elastic-threshold residue that V2 carried as "96%
qualified"; (b) verify cohort refutation discipline is preserved (V2:
32 refuted vs 69 grounded ⇒ 31.7% density on the grounded+refuted
binary; V3 expected to preserve); (c) verify 2B SKELETON DELETE
posture is preserved at V3 with zero re-admission; (d) verify the V2
SKELETON-only-count collapse (3→0) is preserved; (e) under §3Z, V3 is
the **second consecutive ≥95% ACCEPT cycle** for CH6 — V2 closed at
13/13 = 100%, V3 confirms again at 13/13 = 100% — triggering the
2-cycle cohort §3Z LOCK at V3 close.

## Verdict

**CONFIRM. ACCEPT-rate = 13/13 = 1.00.**

V3 is a confirming cycle for CH6. No new findings open. Every V2 ACCEPT
finding (F1-F13) carries forward intact. The five V3 fold-packet items
consumed by the lens — F-V3-CH4-A, F-V3-CH4-B, F-CH5-V2-01, F-V3-CH1-A,
F-V3-CH1-B — each *strengthens* an existing CH6 discipline without
opening any seam: F-V3-CH4-B numerically binds the last two elastic
cells V2 CH4 had carried (e-graph node/iteration cap and generated-LOC
growth) at `2D-cost-model.md:151-162` and `T-P2-V3-FOLD-ADDENDUM.md:103-115`,
closing the "qualified" residue that kept V2 CH4 at 96% rather than
100%; F-V3-CH4-A binds `BBNF_SIMD_STRICT=1` cohort-wide inline at
`2A:192`, `2C:305`, and `2D:142-149` so the strict-mode admission
precondition is no longer carried by addendum alone; F-CH5-V2-01 adds
per-primitive `substrate_target` + `retention_lifetime` columns to
2B §A5 (audit at `:201-260`) and §A6 (cost ledger at `:285-333`) with
the **3 SKELETON DELETE rows preserved at `N/A` for both new columns**
— the strongest possible CH6 signal that no V3 fold smuggles the three
deleted contracts into the admitted count.

The cohort cross-cuts read clean at V3:

- Refutation count holds at **32** (V2: 32; V3: 32 — verified by
  frontmatter aggregation across the six dossiers: 2A 5 + 2B 5 + 2C 5
  + 2D 5 + 2E 6 + 2F 6 = 32).
- Grounded count holds at **69** (V2: 69; V3: 69 — 2A 9 + 2B 11 + 2C
  14 + 2D 7 + 2E 14 + 2F 14 = 69).
- SKELETON-only count holds at **0** (V1: 3; V2: 0 via binary DELETE;
  V3: 0 preserved — 2B §A5 rows at `:253-255` and §A6 rows at `:328-330`
  retain "DELETED per CH4-F1" disposition with `N/A` cells for the new
  F-CH5-V2-01 substrate/retention columns; OQ-discharge note at
  `:515-516` preserves V2 DELETE branch).
- Refutation density holds at **31.7%** of the grounded+refuted binary
  (32/101); the cohort 1:2 anti-paper-close pattern is preserved
  intact.

V3 is the second consecutive cohort-wide ≥0.95 ACCEPT cycle. Per §3Z
discipline, this triggers the cohort 2-cycle LOCK at V3 close.

## Findings — V2 ACCEPT Set Re-verified at V3 HEAD

| # | V2 disposition | V3 disposition | target (V3 HEAD) | V3 preservation evidence |
|---|---|---|---|---|
| F1 | ACCEPT | **ACCEPT (preserved)** | 2B `:54-66` + `:201-260` (§A5) + `:285-333` (§A6) | SKELETON DELETE posture preserved at V3 cohort-wide. Executive Summary at `2B:56-66` retains "V2 disposition (CH4-F1): DELETE the SKELETON triple from `bbnf.asm`" verbatim with executable zero-consumer evidence ("`grep -rn "frame_push\|frame_pop\|open_buf\|FRAME_PUSH\|FRAME_POP" skinny/crates/runtime/src/` returns zero hits beyond fixture strings" + "`grep -rn "FSM_DISPATCH_THREADED\|fsm_dispatch_threaded" skinny/crates/codegen/src/` returns zero hits"). Lock 16 v+1 close-state vocabulary citation at `LOCKS.md:335-342` preserved. **F-CH5-V2-01 strengthens, not weakens, the DELETE posture**: the new `substrate_target` + `retention_lifetime` columns at §A5 (`:253-255`) and §A6 (`:328-330`) carry `N/A (contract DELETED per CH4-F1)` for all three SKELETON rows with the parser-owned-stack Lock 1 v+1 substrate-union violation note — had the contracts survived, they would have failed the substrate-union invariant independently. Frontmatter `techniques_grounded: 11` preserved at `:16`. Three SKELETON contracts remain non-admissible; no re-admission seam exists in any V3 amendment. |
| F2 | ACCEPT | **ACCEPT (preserved)** | 2A `:101` + `:139` | 2A V3 amendments touch only the README line-count reconciliation (235 vs 236) and `BBNF_SIMD_STRICT=1` insertion at T2A-LAC-V1-03 (`:192`); the CH6-overlay row at `:101` ("'Grounded' never means 'validated design'. Microbench parity, citation density, and reference-stuffing without same-wave consumer / row movement remain non-admitting.") and T2A-REF-004 verbatim at `:139` ("Citation density + microbench parity admits a primitive. SK-V12 W2 escape mask added checkasm + corpus parity but no production scanner / row movement [...] SK-V12 W4 delimiter find achieved 4.718× microbench yet explicitly halted before production wiring") are unchanged. The five REFs (REF-001..REF-005) remain first-class load-bearing rows; refuted count holds at 5 per frontmatter `:11`. |
| F3 | ACCEPT | **ACCEPT (preserved)** | 2F V6 preserved (no V3 edit) | 2F V6 carries through V3 untouched per the V3 dispatch context (`2F V6 preserved`); the parse-that-base-not-in-workspace pin, Q1 absorption-decision frame at `:253`, V6 Architectural-Assertions-Refuted six-row register, F-CH5-V2-02 crate_target field, F-V3-CH1-B counted_source_ids 24→26 elevation, and LAC-2F-V5-02 Lock 1 substrate-union v+1 amendment all carry to V3 verbatim. Refuted count holds at 6 per frontmatter `:10`. |
| F4 | ACCEPT | **ACCEPT (preserved)** | 2D `:117` + `:124-126` + `:207-215` | 2D V3 amendments touch only the V3 CH4 fold sites (F-V3-CH4-A BBNF_SIMD_STRICT at `:142-149` and F-V3-CH4-B numeric abrogate bind at `:151-162`); the three load-bearing refutations preserved verbatim: P1-1B-D3 at `:117` ("the *finite-choice* idea transfers; the *fixed step order* does not"); P1-1B-D4 at `:124` ("Eligibility must include the spec's ≥4 byte-disjoint arms hub condition [...] PLUS a kernel emitter PLUS scalar oracle + checkasm differential per Lock 16 PLUS aarch64-admission refusal per F-CH5-V1-03"); P1-1B-D6 at `:125` ("`EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage` lowerers [...] emit `rule {name} -> <shape>` placeholders [...] four of its lowerers carry no real logic"). BackendShape Admission Ledger at `:164-170` preserved with 1 ADMITTED `SinkOnly` + 4 NOT-ADMITTED marker-string-lowerer rows. Refutation count holds at 5 per frontmatter `:10`. |
| F5 | ACCEPT (preserved + strengthened) | **ACCEPT (preserved)** | 2E V7 (preserved through V3; zero V3 edits) | 2E V7 carries through V3 untouched per dispatch context (`2E V2-LOCKED through V3; zero V3 edits`). Six refutations preserved per frontmatter `:40` (`techniques_refuted: 6`); SVE2-only `svmatch_u8` row preserved as `refuted`; V7 `architecture_pressure` audit_state values for PMULL prefix-XOR + CSSC CTZ rows preserved at `:239-240`; GFNI/VBMI2 cross-arch portability refutation preserved at `:127`. The lineage-vs-consumer discrimination (V1 F13 → V2 strengthened) holds at V3 unchanged. |
| F6 | ACCEPT | **ACCEPT (preserved)** | 2C V4 preserved + F-V3-CH4-A insertion at `:305` | 2C V4 carries through V3 with one F-V3-CH4-A insertion at §Closure Criteria For Live Grammar Leaks (`:303-305`) naming C3/C4/escape_mask/byte-window MAC under `BBNF_SIMD_STRICT=1`; the Feature/Witness Transfer Ledger at `:294-302` is unchanged. Selectors / declaration-values-extended / visual-functions / Sheets remain `NOT-VALIDATED` rows; CSS declaration values remains the lone `ADMITTED-EVIDENCE` row. The ledger is honest. F10 BBNF-self firm-binding preserved (see F10 V3 row). |
| F7 | ACCEPT (discharged) | **ACCEPT (preserved)** | 2E V7 preserved through V3 | 2E V7 fold-addition `CH6-F7-NOT-S-P3-ELIGIBLE-AT-V1-LABELS` at `:73-80` carries through V3 unchanged; per-entry `not_S-P3-eligible_at_V1` labels at `2E:238, :243, :245, :247, :248` preserved verbatim; "Other esoterica" preamble at `:303-309` preserved; BCAX paragraph qualification at `:243` preserved. Discharge mechanism remains the V2 mechanism — no V3 fold weakens any per-entry state cell. |
| F8 | ACCEPT (discharged) | **ACCEPT (preserved + strengthened by F-V3-CH4-B)** | 2D `:112-126` + new `:151-162` | 2D V3 preserves the V2 Technique Grounding Table same-wave-consumer fifth column at `:112-126` (4-of-5 `T2D-*` grounded rows still downgraded to `grounded-class-only`; `T2D-REGEX-NFA-DFA-PLAN` still `PENDING-via-2F`; `T2D-TAPE-STAGED-MATERIALIZATION` still `PENDING-via-marker-strings`; `T2D-SINKONLY-PROJECTION` still the lone `ADMITTED` row). **Strengthened by F-V3-CH4-B**: every candidate admitted via the BackendShape Admission Ledger now fails closed when any of the six numeric abrogate gates trips (e-graph ≤50000 nodes / ≤10000 classes / ≤30 iter; CSP ≤1 s/grammar; stale-cost ≤30%; generated LOC growth ≤candidate's `loc_budget`; row regression; parity/checkasm failure) per `2D:151-162`. The V2 same-wave-consumer discipline is now bracketed by numeric fail-closed gates; admission requires both consumer presence and gate satisfaction. |
| F9 | ACCEPT (discharged) | **ACCEPT (preserved)** | 2C V4 preserved | 2C V4 Technique Grounding Table two-cell split at `:111-118` carries through V3 unchanged: 2C-CSS-TOKEN-ALPHABET, 2C-CSS-SELECTOR-SCOPE, 2C-CSS-CALC-VAR each `grounded-with-generator-stub + pending-generator-emission` with explicit "Generator-stub gap:" callouts; 2C-RUNTIME-PROVIDER-REGISTRY `refuted (Lock-14-citation-grounded) + pending-generator-emission`. All four rows retain "Two-cell template inherited from `2C-BBNF-SELF-FALSIFIER`" trailers. |
| F10 | ACCEPT (discharged) | **ACCEPT (preserved)** | 2C V4 preserved at `:301` | 2C V4 BBNF-self witness row at `:301` carries through V3 unchanged: `ADMITTED-VIA-C4-W10` with same-commit binding per SK-V14 SPEC §13 W10 wave-anchor; "transitioning-if-C4-wave-lands" V1 phrasing remains struck. No V3 fold weakens the same-commit binding; W10 Stage-0 unconditionally remains the published wave per `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md:982`. |
| F11 | ACCEPT (discharged) | **ACCEPT (preserved + amplified by F-V3-CH4-B)** | 2A `:179-184` + 2D `:247-254` | 2A V3 preserves all six UNKNOWN wave/pass anchors at `:179-184` (UNKNOWN-1: "Discharge anchor: T-P3 §3C"; UNKNOWN-2: "Discharge anchor: S-P3 P3-B sidecar wave or T-P3 backlog (owner: T-P3 §3C P-2 follow-on)"; UNKNOWN-3/-4/-5/-6 each "Discharge anchor: T-P3 §3C"). 2D V3 preserves all six UNKNOWN wave/pass anchors at `:247-254` (UNKNOWN-2D-01 "discharged at T-P3 §3C amendment authoring"; UNKNOWN-2D-02/-03/-04 "deferred to S-P3 W{N=cost-axis-snapshot}"; UNKNOWN-2D-05 "deferred to S-P3 W{N=2E-source-backed-aarch64-candidate}"; UNKNOWN-2D-06 "deferred to S-P3 W{N=BackendShape-admission-ledger-wave}"). **Amplified by F-V3-CH4-B**: UNKNOWN-2D-03's bounded-scheduler caps now resolve to specific numeric values (`≤50000 nodes`, `≤10000 classes`, `≤30 iter`) per the V3 numeric bind, eliminating the last elastic phrasing in the OQ ledger. |
| F12 | ACCEPT (preserved + strengthened) | **ACCEPT (preserved + restrengthened)** | cross-dossier (V3 sites listed below) | The cross-dossier same-wave-consumer discipline is preserved cohort-wide at V3 and *further strengthened* by F-V3-CH4-A and F-V3-CH4-B: (a) 2A V3 binds "named same-wave consumer; orphan-kernel research is rejected" at `:99` + `BBNF_SIMD_STRICT=1` at T2A-LAC-V1-03 `:192`; (b) 2B V3 preserves Lock 16 same-wave consumer with executable `grep` evidence at `:54-66` + adds per-primitive substrate_target/retention_lifetime columns at §A5+§A6 binding admitted primitives to `local_temp_only`/`direct_sink`/`local_loop`/`generated_function` substrates (4 local_temp_only + 2 direct_sink at V3); (c) 2C V4 preserved binding + F-V3-CH4-A at `:305` naming the C3/C4/escape_mask/byte-window MAC consumers strict-mode; (d) 2D V3 preserves per-row same_wave_consumer cells + F-V3-CH4-A at `:142-149` + F-V3-CH4-B numeric fail-closed gates at `:151-162`; (e) 2E V7 preserved through V3; (f) 2F V6 preserved through V3. |
| F13 | ACCEPT (preserved + strengthened) | **ACCEPT (preserved)** | 2E V7 preserved through V3 | 2E V7 V6-fold-note verbatim at `:204-214` preserved through V3; PMULL prefix-XOR + CSSC CTZ `architecture_pressure` audit_state column values at `:239-240` preserved; lineage-vs-consumer rationale at `:87-89` preserved. The lineage-vs-consumer discrimination remains an explicit per-row column value at V3. |

## V3 Fold-Packet Cross-Verification (CH6 Lens)

| V3 fold item | discharge target | V3 verification outcome | CH6 anti-paper-close signal |
|---|---|---|---|
| **F-V3-CH4-B (numeric abrogate-gate binding)** | `2D-cost-model.md:151-162` + `T-P2-V3-FOLD-ADDENDUM.md:103-115` | **VERIFIED.** All six abrogate gates numerically bound: (a) e-graph saturation `≤50000 nodes / ≤10000 classes / ≤30 iter / ≤512 MiB resident memory per grammar` (V2 left node/iter cap deferred to addendum); (b) CSP `≤1 s per grammar on SK-V13 host`; (c) stale-cost `≤30% per grammar and output plane`; (d) generated LOC growth bound to candidate's ledger `loc_budget` upper bound, with explicit "stricter SPEC per-wave budget wins" — V2 left this elastic ("deferred to SPEC wave budget"); (e) row regression "any previously admitted JSON/CSS row below its prior admitted gate"; (f) parity/checkasm/equality "any scalar, checkasm, strict equality, or independent-oracle failure". | **Closes the V2 96%→100% qualified residue.** V2 CH4 named the e-graph cap in 2A but deferred in 2D; V3 hardens 2D inline with named SPEC reference for LOC growth and full numeric set propagated from T2A-LAC-V1-05 into 2D's per-row schema. The V2 elastic threshold is now a hard fail-closed wall. No "support-only landing" pathway survives the numeric bind. |
| **F-V3-CH4-A (BBNF_SIMD_STRICT=1 cohort-wide propagation)** | `2A:192` + `2C:305` + `2D:142-149` | **VERIFIED.** Inline at three cohort sites: 2A T2A-LAC-V1-03 manifest schema row names the strict-mode precondition as a mandatory ninth manifest field; 2C §Closure Criteria For Live Grammar Leaks names the four CSS-route primitives that must satisfy strict mode; 2D BackendShape Admission Ledger preamble names the cohort-wide precondition explicitly. | **Closes the V2 CH4-F5 "propagation incomplete" residue.** V2 CH4 carried strict-mode through the addendum bind only; V3 binds inline. No silent scalar fallback can pass as a SIMD admission row at V3. |
| **F-CH5-V2-01 (substrate_target + retention_lifetime columns)** | `2B:201-260` (§A5) + `2B:285-333` (§A6) | **VERIFIED.** §A5 and §A6 each extended with the two new columns; 6 admitted primitives populated (4 `local_temp_only` + 2 `direct_sink`; all transient-single-call `local_loop` or `generated_function` retention); 3 SKELETON rows carry `N/A (contract DELETED per CH4-F1)` for both columns with explicit substrate-union violation note ("had the contract survived, a parser-owned `open_buf` frame stack would itself be a Lock 1 substrate-union violation per LAC-2F-V5-02"). | **Strengthens the CH6 SKELETON-DELETE discipline.** The new columns are not a re-admission seam — they explicitly *re-affirm* the V2 DELETE branch with two additional bars (substrate-union violation + retained-across-call-boundary REJECT class). V3's hardening of substrate-union typing makes the SKELETON triple doubly inadmissible. |
| F-V3-CH1-A (README line-count reconciliation) | 2A `:32`, `:66`, `:213` (3 sites) | **VERIFIED.** Frontmatter + V2 Authority + Source Register all consistent at 235 lines (live `wc -l` = 235). NEW-CH2-V3-02 0 orphans verified. | Provenance discipline; not an admission claim. CH6 boundary preserved. |
| F-V3-CH1-B (counted_source_ids 24→26) | 2F frontmatter | **VERIFIED.** 2F V6 preserved through V3 with counted_source_ids elevation to 26. | Counted-source convention discipline; not an admission claim. CH6 boundary preserved. |

## Cohort ANTI-PAPER-CLOSE Census (V3 — LOCK-eligible)

| dossier | grounded | refuted | partial / NOT-VALIDATED | skeleton-only | V2 → V3 delta | CH6 honesty signal at V3 |
|---|---:|---:|---:|---:|---|---|
| 2A | 9 | 5 | 1 (architecture_pressure: asmjson) | 0 | preserved + F-V3-CH4-A insertion + F-V3-CH1-A reconciliation | REF-001..REF-005 preserved; T2A-REF-004 verbatim at `:139`; six OQs preserve wave/pass anchors; `BBNF_SIMD_STRICT=1` inline at T2A-LAC-V1-03 `:192`; T2A-LAC-V1-05 numeric abrogate caps elevated to cohort fold packet |
| 2B | 11 | 5 | 3 | **0 (DELETE preserved)** | F-CH5-V2-01 substrate/retention columns added (admitted: 6 populated; SKELETON: 3 `N/A`) | DELETE posture preserved + doubly enforced via Lock 1 v+1 substrate-union invariant; §A6 cost ledger 9-row schema extended (h)+(i); §R3 + §A5 + §A6 carry DELETE action verbatim from V2 |
| 2C | 14 | 5 | 3 (NOT-VALIDATED witness rows preserved) | 0 | preserved + F-V3-CH4-A insertion at §Closure Criteria `:303-305` | Technique Grounding Table 4-row two-cell split preserved (F9); BBNF-self witness firmed `ADMITTED-VIA-C4-W10` same-commit binding preserved (F10); `BBNF_SIMD_STRICT=1` now inline for C3/C4/escape_mask/byte-window MAC routes |
| 2D | 7 | 5 | 1 (`P1-1B-D2`) + 1 (`T2D-FIVE-SHAPE-FINITE-SET`) | 0 (4-of-5 marker-string lowerers refuted; BackendShape Admission Ledger preserved) | F-V3-CH4-A cohort-wide precondition at `:142-149` + F-V3-CH4-B numeric abrogate gate bind at `:151-162` | Same-wave-consumer column preserved per T2D-* row; 4 of 5 grounded rows preserve `grounded-class-only` downgrade; six OQs preserve wave anchors; **F-V3-CH4-B closes the V2 96% qualified residue numerically** |
| 2E | 14 | 6 | 7 (source_backed / conditional rows) | 0 | preserved (V2-LOCKED through V3; zero V3 edits) | V7 audit_state column normalisation preserved; per-entry `not_S-P3-eligible_at_V1` labels preserved (F7); PMULL/CSSC `architecture_pressure` cells preserved (F13) |
| 2F | 14 | 6 | 6 (5 PTG rows partial; 1 admission-decision-blocked) | 0 | preserved + F-V3-CH1-B counted_source_ids 24→26 + F-CH5-V2-02 crate_target preserved | parse-that-base-not-in-workspace pin preserved; Q1 carries W11 anchor + CH3 regression pre-flight; LAC-2F-V5-02 binds "no cross-call retained classifier state" cohort-wide |

**Cohort signal at V3:** 69 grounded / 32 refuted / 21 partial-or-NOT-VALIDATED /
**0 skeleton-only (preserved from V2 collapse)**. Refutation density holds at
**32/101 = 31.7%** of the grounded+refuted binary (V2: 31.7%; V3: 31.7% — exact
preservation). The SKELETON-only collapse to 0 holds at V3 with the
F-CH5-V2-01 substrate columns *re-affirming* the DELETE branch rather than
opening any re-admission seam. The cohort 1:2 anti-paper-close pattern is
locked at V3.

## CH6 V3 Dispatch Focus — Discharge Table

| dispatch focus item | V3 discharge in cohort | residual CH6 obligation |
|---|---|---|
| **F-V3-CH4-B numeric binding closes "qualified" elastic threshold residue** | All six abrogate gates numerically bound at `2D-cost-model.md:151-162` (cohort-wide precondition for BackendShape Admission Ledger) and at `T-P2-V3-FOLD-ADDENDUM.md:103-115` (shared cap table for 2B/2E/2F routes). V2's two elastic cells (e-graph node/iter cap deferred to addendum; generated LOC growth deferred to "SPEC wave budget") are now numerically bound inline. The V2 CH4 96% qualified residue closes to 100%. | None — discharge verbatim per CH4 V2 carry-forward. |
| **Cohort refutation discipline preserved (V2: 32 refuted vs 69 grounded; V3 expected to preserve)** | Frontmatter aggregation across the six dossiers yields exactly 32 refuted and 69 grounded at V3 (verified per `grep -nE "techniques_grounded\|techniques_refuted" 2*.md`). Refutation density preserved at 31.7% of grounded+refuted binary; cohort 1:2 anti-paper-close pattern preserved. | None — pattern preserved exactly. |
| **2B SKELETON DELETE preserved (zero re-admission)** | 2B §R3 at `:385-410`, §A5 at `:253-255`, §A6 at `:328-330`, OQ-discharge at `:515-516` all preserve V2 DELETE language verbatim. F-CH5-V2-01 substrate columns carry `N/A (contract DELETED per CH4-F1)` for all three SKELETON rows with explicit Lock 1 v+1 substrate-union violation note for the FRAME_PUSH/POP pair — V3's hardening of substrate typing makes the SKELETON triple doubly inadmissible. No V3 fold offers any re-admission pathway. | None — DELETE posture strengthened, not weakened. |
| **Collapse 3→0 preserved** | SKELETON-only count is 0 at V3 (V1: 3; V2: 0 via DELETE; V3: 0 preserved). Frontmatter `techniques_grounded: 11` at 2B `:16` carries forward the V2 honest count (does not smuggle the three deleted contracts back into the admitted total). | None — collapse preserved. |
| **§3Z: V3 = second consecutive ≥95% → 2-cycle LOCK** | V2 closed at 13/13 = 1.00 ACCEPT (≥0.95). V3 closes at 13/13 = 1.00 ACCEPT (≥0.95). Two consecutive cycles ≥0.95 trigger §3Z 2-cycle cohort LOCK at V3 close per `ORCHESTRATOR.md §3Z`. | **LOCK at V3 close.** No further CH6 cycle required; CH6 enters LOCK posture pending T-P3 dispatch. |

## Disposition

**CONFIRM at V3.** All 13 V2 ACCEPT findings (F1-F13) carry forward
intact at V3 HEAD. The V3 fold-packet items F-V3-CH4-B, F-V3-CH4-A,
F-CH5-V2-01, F-V3-CH1-A, and F-V3-CH1-B each *strengthen* an existing
CH6 discipline (numeric abrogate binding, cohort-wide strict-mode
propagation, substrate-union typing of SKELETON DELETE, provenance
reconciliation, counted-source elevation) without opening any new seam.
The cohort cross-cuts read clean: refutation count holds at 32 (V2:
32); grounded count holds at 69 (V2: 69); SKELETON-only count holds at
0 (V1: 3 → V2 DELETE → V3 DELETE preserved); refutation density holds
at 31.7%; cohort 1:2 anti-paper-close pattern preserved.

**ACCEPT-rate over the 13 findings: 13 ACCEPT / 0 REVISE / 0 REJECT =
1.00 ACCEPT.** This is the second consecutive ≥0.95 ACCEPT cycle for
CH6 (V2: 1.00; V3: 1.00). Per §3Z discipline (cohort LOCK = ≥0.95
ACCEPT × 2 consecutive cycles), **the 2-cycle cohort LOCK triggers at
V3 close for CH6**.

No new V3 REVISE findings emerge. The V2 dispatch correctly identified
the closure obligations; V3 confirms preservation across all 13
findings and strengthens 4 of them (F1, F8, F11, F12) via the V3 fold
mechanics without opening any anti-paper-close seam.

## LOCK Confirmation

**§3Z COHORT LOCK TRIGGERED FOR CH6 AT V3 CLOSE.**

- V2 ACCEPT-rate: 1.00 (≥0.95 threshold satisfied — first consecutive cycle)
- V3 ACCEPT-rate: 1.00 (≥0.95 threshold satisfied — second consecutive cycle)
- §3Z requirement: ≥0.95 ACCEPT × 2 consecutive cycles → **SATISFIED**
- V≤5 ceiling: V3 ≤ V5 → **SATISFIED**
- LOCK status: **LOCKED at V3 close**

CH6 enters LOCK posture pending T-P3 dispatch. No further T-P2 CH6
challenge cycle is required. The anti-paper-close discipline is bound
into the V3 cohort HEAD and is carried forward to T-P3 §3C as a fixed
boundary: future folds must preserve the 32-refuted / 69-grounded /
0-skeleton-only census, the 31.7% refutation density, the F1-F13
findings posture, and the numeric abrogate-gate bind from
`2D-cost-model.md:151-162` and `T-P2-V3-FOLD-ADDENDUM.md:103-115`.

## Fold Requirements For V4 (None)

V3 is the LOCK-close cycle. No further CH6 fold packet items emerge.
The CH6 anti-paper-close boundary is locked into the V3 cohort HEAD
and is preserved cohort-wide by:

- the numeric abrogate-gate bind at `2D-cost-model.md:151-162` and
  `T-P2-V3-FOLD-ADDENDUM.md:103-115`;
- the cohort-wide `BBNF_SIMD_STRICT=1` precondition inline at `2A:192`,
  `2C:305`, `2D:142-149`;
- the 2B SKELETON DELETE branch preserved at §R3 + §A5 + §A6 +
  OQ-discharge with Lock 1 v+1 substrate-union violation re-affirmation
  per F-CH5-V2-01;
- the cross-dossier same-wave-consumer discipline preserved cohort-wide
  per F12 carry-forward;
- the lineage-vs-consumer per-row column value preserved at 2E V7
  `:239-240` per F13 carry-forward;
- the OQ wave/pass anchor discipline preserved at 2A `:179-184` + 2D
  `:247-254` per F11 carry-forward, with UNKNOWN-2D-03's numeric
  bounded-scheduler caps now resolved per F-V3-CH4-B amplification.

Any subsequent T-P3 fold that touches a CH6-binding cell (refutation
row, SKELETON DELETE row, numeric abrogate gate, strict-mode
precondition, same-wave consumer cell, OQ wave anchor) must preserve
the V3 census exactly or trigger a re-opening of CH6 under §3Z
re-entry discipline.
