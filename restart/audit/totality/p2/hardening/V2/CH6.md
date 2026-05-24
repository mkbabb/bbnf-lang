---
agent: CH6
pass: T-P2-research
cycle: V2
lens: ANTI-PAPER-CLOSE
disposition: CONFIRM
generated_at: 2026-05-23T22:30:00-04:00
authority:
  - restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p2/hardening/V1/CH6.md
  - restart/prompts/totality/PASS-2-RESEARCH.md §3 (CH6)
v1_prior:
  disposition: REVISE
  ACCEPT_rate: 0.615
  findings: 13 (8 ACCEPT / 5 REVISE / 0 REJECT)
  REVISE_set: [F7, F8, F9, F10, F11]
head_pin: 4f17880d0
inputs_audited:
  - restart/audit/totality/p2/2A-sota-landscape.md (V2; 228 lines; 14 sources)
  - restart/audit/totality/p2/2B-primitive-vocabulary.md (V2; 528 lines; 18 sources; 6 admitted Layer-1 post-SKELETON-DELETE)
  - restart/audit/totality/p2/2C-grammar-neutrality.md (V4; 462 lines; 9 sources)
  - restart/audit/totality/p2/2D-cost-model.md (V2; 260 lines; 12 sources)
  - restart/audit/totality/p2/2E-host-arch-esoterica.md (V7; 493 lines; 28 sources)
  - restart/audit/totality/p2/2F-parse-that-gaps.md (V6; 586 lines; 24 sources)
cohort_totals_v2:
  primary_citations: 105
  techniques_grounded: 69
  techniques_refuted: 32
  skeleton_only_contracts: 0 (V1=3, V2 DELETE-RECOMMENDED per CH4-F1 / 2B §R3 / §A6)
disposition_summary:
  ACCEPT_findings: 13
  REVISE_findings: 0
  REJECT_findings: 0
  ACCEPT_rate: 1.00
---

# T-P2 V2 CH6 — ANTI-PAPER-CLOSE

## Lens Contract

CH6 binds per `PASS-2-RESEARCH.md §3` and the V2 dispatch focus in
`restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md §2`. V2's
sole CH6 obligation is to re-execute the V1 REVISE set against the
amended dossiers and confirm closure: F7 (2E esoterica
`not_S-P3-eligible_at_V1` labels), F8 (2D Technique Grounding Table
same-wave-consumer column), F9 (2C verdict two-cell split), F10 (2C
BBNF-self firmed ADMITTED-VIA-C4-W10), F11 (2A + 2D OQ wave/pass
anchors), plus preservation of the cohort refutation density (1:2
anti-paper-close pattern) and the V1 ACCEPT-set (F1-F6, F12-F13). Per
`§3Z` discipline, V2 targets first ≥0.95 ACCEPT cycle on V2; LOCK at
V3 close (two consecutive ≥0.95 ACCEPT cycles).

## Verdict

**CONFIRM. ACCEPT-rate = 13/13 = 1.00.** Every V1 REVISE finding is
discharged at V2 HEAD with the exact mechanism named in V1's "required
revision" column and with the closure verbiage CH6 binds. Every V1
ACCEPT finding carries forward intact. The cohort-wide anti-paper-close
posture strengthens at V2 — refutation count rises from 31 to 32 (2D
adds one explicit refutation row); the SKELETON-only count *drops* from
3 to 0 because 2B converts the V1 disclosure into a binary DELETE
disposition under Lock 16 v+1 close-state vocabulary (the most honest
possible CH6 disposition); and the cross-dossier same-wave-consumer
spine (the F12 V1 ACCEPT) now binds explicit-named-consumer cells per
row in 2D (the F8 V2 discharge), explicit-state-cells per esoterica
entry in 2E (the F7 V2 discharge), explicit two-cell verdict splits in
2C (the F9 V2 discharge), explicit ADMITTED-VIA-C4-W10 with same-commit
binding for BBNF-self (the F10 V2 discharge), and explicit wave/pass
anchors per UNKNOWN row in 2A + 2D (the F11 V2 discharge).

The V2 cohort meets the §3Z first ≥0.95 ACCEPT target with margin. V3
expected disposition: CONFIRMING (no new findings), enabling cohort
§3Z LOCK at V3 close per the V2 dispatch §3 prediction.

## Findings — V1 Discharge Verification

| # | V1 disposition | V2 disposition | target | V1 required revision | V2 discharge at HEAD |
|---|---|---|---|---|---|
| F1 | ACCEPT | **ACCEPT (preserved)** | `2B:43-48` + `:199` + `:222-231` (V2 equivalent) | "Preserve the SKELETON disclosure in V2; do NOT let any V2 fold smuggle the three contracts into the admitted count" | 2B V2 amplifies V1's posture: the `techniques_grounded: 11` frontmatter is preserved (`2B:14`), and the three SKELETON contracts are now bound to a binary DELETE disposition per CH4-F1 at `2B:54-66` ("V2 disposition (CH4-F1): DELETE the SKELETON triple from `bbnf.asm`") with executable `grep -rn` evidence ("zero hits beyond fixture strings") and Lock 16 v+1 close-state vocabulary citation at `LOCKS.md:335-342`. The V2 §A5 table at `2B:231-233` and §A6 at `2B:253` carry the per-contract DELETE action with line refs (`bbnf.asm:355-363`, `:404-418`, `:454-468`). This is the strongest possible CH6 honesty disposition — DELETE under Lock 16 v+1 is one of the four admissible close states; `skeleton-contract-only` is not. |
| F2 | ACCEPT | **ACCEPT (preserved)** | `2A:74` + `:111-113` (V2 equivalent `2A:101` + `2A:138-140`) | "Preserve the REF-004 row verbatim in V2." | 2A V2 preserves the CH6-overlay row verbatim at `2A:101` ("'Grounded' never means 'validated design'. Microbench parity, citation density, and reference-stuffing without same-wave consumer / row movement remain non-admitting.") and T2A-REF-004 verbatim at `2A:139` ("Citation density + microbench parity admits a primitive. SK-V12 W2 escape mask added checkasm + corpus parity but no production scanner / row movement [...] SK-V12 W4 delimiter find achieved 4.718× microbench yet explicitly halted before production wiring"). The five REFs (REF-001..REF-005) remain first-class load-bearing rows. |
| F3 | ACCEPT | **ACCEPT (preserved)** | `2F:32-34` + `:236-244` + `:472-473` (V2 equivalent) | "Q1 absorption-decision frame is exactly the anti-paper-close posture for the largest single architectural change in 2F." | 2F V6 preserves the parse-that-base-not-in-workspace pin and Q1 absorption-decision frame; the V6 Architectural-Assertions-Refuted retains its six load-bearing refutations (6 in V2 vs 6 in V1 per frontmatter `techniques_refuted: 6`); Q1 at `2F:253` carries the SK-V14 W11 wave anchor + the V6 F-CH3-2F-08 CH3 regression pre-flight. The V6 LAC-2F-V5-02 elevation to T-P3 §3C Lock 1 substrate-union v+1 amendment (`2F:490`) sharpens the prev-in-string refutation to "no cross-call retained classifier state" — broader binding than V1's per-shape REDRESS 96/97/98 fence. |
| F4 | ACCEPT | **ACCEPT (preserved)** | `2D:34-48` + `:80-87` (V2 equivalent `2D:44-50` + Architectural Assertions Refuted at `2D:207-215`) | "Preserve P1-1B-D3 / D4 / D6 refutations in V2." | 2D V2 preserves all three load-bearing refutations explicitly: P1-1B-D3 at `2D:117` ("the *finite-choice* idea transfers; the *fixed step order* does not"); P1-1B-D4 at `2D:124` ("Eligibility must include the spec's ≥4 byte-disjoint arms hub condition [...] PLUS a kernel emitter PLUS scalar oracle + checkasm differential per Lock 16 PLUS aarch64-admission refusal per F-CH5-V1-03"); P1-1B-D6 at `2D:125` ("`EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage` lowerers [...] emit `rule {name} -> <shape>` placeholders [...] four of its lowerers carry no real logic"). The V2 "BackendShape Admission Ledger" at `2D:142-148` materialises the per-shape binary disposition with eight-cell manifest schema per T2A-LAC-V1-03. Refutation count holds at 5. |
| F5 | ACCEPT | **ACCEPT (preserved + strengthened)** | `2E:82-89` + `:172` + `:378-384` (V2 equivalent `2E:122-128` + `2E:244`) | "None on the refutation register; see F7 for V6 nine-new-additions surface expansion." | 2E V7 preserves the six refutations (count holds at 6 per frontmatter `:40`) and the SVE2-only `svmatch_u8` row at `2E:244` (state `refuted`). Strengthened: the V7 fold adds `architecture_pressure` audit_state values to PMULL prefix-XOR + CSSC CTZ rows at `2E:239-240` explicitly distinguishing "abstract-primitive lineage intact" from "consumer measured-rejected per REDRESS-88/-89" — the canonical CH6 lineage-vs-consumer discrimination. The V6 GFNI/VBMI2 cross-arch portability refutation remains intact at `2E:127`. |
| F6 | ACCEPT | **ACCEPT (preserved)** | `2C:283-296` + `:446-457` (V2 equivalent `2C:294-301`) | "None on the ledger frame; see F10 for the soft-deferral phrasing on the BBNF-self row." | 2C V4 preserves the Feature/Witness Transfer Ledger at `2C:294-302` with explicit NOT-VALIDATED state column; selectors / declaration-values-extended / visual-functions / Sheets remain `NOT-VALIDATED` rows; CSS declaration values remains the lone `ADMITTED-EVIDENCE` row. The ledger is honest. F10 discharge (below) firms the BBNF-self row that V1 flagged. |
| F7 | **REVISE** | **ACCEPT (discharged)** | `2E:47-54` (v6_fold_additions) + `:231-235` ("Other esoterica") | "Add explicit `state = source_backed; not S-P3-eligible at V1; eligible only post-F-V2-P1ABC-RERECORD` labels to every 'Other esoterica 2E surfaces' entry. Tighten the BCAX paragraph at `:209` so the 'higher relevance than EOR3' claim is qualified as 'would be higher relevance *if* the AND-NOT-XOR algebra is named as a measured hot fan-in'." | 2E V7 fold-addition `CH6-F7-NOT-S-P3-ELIGIBLE-AT-V1-LABELS` at `:73-80` records the discharge; the per-entry labels are now in the dossier prose: BCAX at `2E:243` (state `source-backed / conditional / not_S-P3-eligible_at_V1`), LD4 at `2E:238` (state `partial / source-backed / not_S-P3-eligible_at_V1`), CRC32C at `2E:248` (state `source-backed / not in current candidate set / not_S-P3-eligible_at_V1`), cache hints at `2E:247` (`refuted as standalone admission / not_S-P3-eligible_at_V1`), ASCII run-skip at `2E:245` (`micro_proven / not admitted / not_S-P3-eligible_at_V1`). The "Other esoterica 2E surfaces for future evaluation" block at `2E:303-309` carries the V7 explicit preamble "Each entry below carries explicit `state = source_backed; not_S-P3-eligible_at_V1; eligible only post-F-V2-P1ABC-RERECORD` per CH6 F7 fold — the V6 lineage strengthening does not by itself promote shortlist eligibility." The BCAX paragraph at `2E:243` is tightened: "**Would be higher V6 relevance than EOR3 _if_ the AND-NOT-XOR algebra is named as a measured hot fan-in** — the existing scanner uses AND-NOT chains rather than XOR triples, but standalone shape-superiority over EOR3 is not by itself shortlist-promoting." Discharge verbatim per V1 required revision. |
| F8 | **REVISE** | **ACCEPT (discharged)** | `2D:54-66` (Technique Grounding Table grounded rows) | "For each `T2D-*` grounded row, add a `same_wave_consumer` cell naming the specific generated path or measured row that would demonstrate the grounded class beats the cascade in bbnf — or downgrade to `partial` until such cell is named." | 2D V2 fold packet item `CH6-F8` at `:16` records the discharge; the Technique Grounding Table at `2D:112-126` adds a fifth column `same_wave_consumer (per F8)` and downgrades 4 of the 5 `T2D-*` grounded rows to the new `grounded-class-only` verdict per CH6 anti-paper-close discipline: `T2D-EGRAPH-EXTRACTION` (`2D:114`, PENDING — "no production row currently selected by e-graph extraction beats a `PriorityStep`-derived plan on a measured row"); `T2D-EQSAT-ORIGIN` (`2D:115`, PENDING); `T2D-BURG-FINITE-ALTERNATIVES` (`2D:116`, PENDING); `T2D-CSP-FEASIBILITY-LAYER` (`2D:119`, PENDING). The remaining grounded rows carry explicit consumer cells: `T2D-REGEX-NFA-DFA-PLAN` at `:120` is `PENDING-via-2F` anchored to SK-V14 W11; `T2D-TAPE-STAGED-MATERIALIZATION` at `:121` is `PENDING-via-marker-strings` anchored to LAC-2D-04; `T2D-SINKONLY-PROJECTION` at `:122` is the lone `ADMITTED` row (CSS L4 declaration-values consumer). The dossier prose at `2D:108-110` binds the rule explicitly: "Citation grounds the *class*; the consumer cell grounds the *bbnf admission*. Rows without a namable consumer at V2 fold time downgrade to `grounded-class-only` and are themselves CH6 anti-paper-close discharge debts carried to T-P3 §3C." Discharge verbatim per V1 required revision. |
| F9 | **REVISE** | **ACCEPT (discharged)** | `2C:105-120` (Technique Grounding Table) + `:295` (BBNF-self witness state) | "For each Technique Grounding row whose bbnf-specific note identifies a generator gap (≥4 of the 14 rows), downgrade the verdict from `grounded` to `partial-grounded-pending-generator` or split into a `standards-citation-grounded` + `bbnf-discharge-pending` two-cell pair." | 2C V4 fold disposition at `:22` records the discharge ("V2-fold CH6 F9 Technique Grounding Table verdict two-cell split (grounded-with-generator-stub + pending-generator-emission) for 2C-CSS-TOKEN-ALPHABET, 2C-CSS-SELECTOR-SCOPE, 2C-CSS-CALC-VAR, 2C-RUNTIME-PROVIDER-REGISTRY"). The Technique Grounding Table at `2C:111-118` carries the two-cell split per V1 required revision: 2C-CSS-TOKEN-ALPHABET at `:111` is **`grounded-with-generator-stub` (standards-citation grounds the primitive class) + pending-generator-emission** with explicit "Generator-stub gap:" callout; 2C-CSS-SELECTOR-SCOPE at `:112` is the same two-cell split with "Generator-stub gap: the `CssL4StylesheetSelectors` provider variant exists at `grammar_profile.rs:21` but no generator emits selector facts"; 2C-CSS-CALC-VAR at `:113` is the same two-cell split with "Generator-stub gap: the per-grammar number/string policy emitter is not yet live"; 2C-RUNTIME-PROVIDER-REGISTRY at `:118` is **`refuted (Lock-14-citation-grounded)` + pending-generator-emission** ("the bbnf-discharge cell closes when `cargo xtask` regenerates `RuntimeProvider` from `Cargo.toml` workspace metadata"). All four rows carry the explicit "Two-cell template inherited from `2C-BBNF-SELF-FALSIFIER`" trailer per V1's required template-inheritance guidance. Discharge verbatim per V1 required revision. |
| F10 | **REVISE** | **ACCEPT (discharged)** | `2C:296` (BBNF-self witness row) | "Either firm the witness row to `ADMITTED-VIA-C4-SHAPE-IDENTICAL` (since 2C itself argues C4's BBNF-self consumer is shape-identical and discharges Lock 14 v+1 in the same wave that admits the SIMD body) or strike the transitioning clause and hold at `NOT-VALIDATED until C4 admission`. A V2 state that depends on a future S-P3 wave landing is exactly the deferral-to-later-research-pass CH6 forbids." | 2C V4 fold disposition at `:23` records the discharge ("V2-fold CH6 F10 BBNF-self witness firm-or-strike — firmed to ADMITTED-VIA-C4-W10 with same-commit binding per SK-V14 SPEC §13 W10 wave-anchor"). The BBNF-self witness row at `2C:301` reads: **`ADMITTED-VIA-C4-W10` — bound to SK-V14 SPEC §13 W10 wave (`restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md:982` ships W10 Stage-0 UNCONDITIONALLY; P3-C `:36` W10 wave-manifest cell + `:423` W10 exit-gate item 8 bind C4 admission); same-commit binding: the W10 commit that admits C4 SIMD body necessarily exercises the BBNF-self literal-escape consumer in the same commit because the consumer routes through `parse_that_regex::unescape_string` which is the C4 SIMD body's caller, so admission and exercise are atomic. No future-wave-landing dependency; verdict is firm at admission of C4-W10.** The "transitioning-if-C4-wave-lands" V1 phrasing is struck; the V2 firm anchors atomic same-commit binding via the published SK-V14 SPEC §13 W10 wave manifest. Discharge verbatim per V1 required revision (firm option chosen, not strike). |
| F11 | **REVISE** | **ACCEPT (discharged)** | `2A:124` + `2D:97` (Open Research Questions) | "Add an explicit wave or pass anchor to every OQ row: either 'discharged at T-P3 §3C amendment authoring' or 'deferred to S-P3 W{N}' or 'abrogated: NOT a T-P2 in-scope question, drop to T-P3 backlog with named owner.'" | 2A V2 fold disposition at `:15` records the discharge ("CH6-F11 (wave/pass anchors added to all six Open Research Questions)"). All six 2A UNKNOWNs at `2A:179-184` now carry explicit wave/pass anchors: UNKNOWN-1 "Discharge anchor: T-P3 §3C"; UNKNOWN-2 "Discharge anchor: S-P3 P3-B sidecar wave or T-P3 backlog (owner: T-P3 §3C P-2 follow-on)"; UNKNOWN-3, -4, -5, -6 each "Discharge anchor: T-P3 §3C". 2D V2 fold packet item `CH6-F11` at `:17` records the discharge; all six 2D UNKNOWNs at `2D:227-232` carry explicit anchors as a dedicated "wave / pass anchor" column: UNKNOWN-2D-01 "**discharged at T-P3 §3C amendment authoring**"; UNKNOWN-2D-02 "**deferred to S-P3 W{N=cost-axis-snapshot}**"; UNKNOWN-2D-03 / -04 same wave anchor; UNKNOWN-2D-05 "**deferred to S-P3 W{N=2E-source-backed-aarch64-candidate}**"; UNKNOWN-2D-06 "**deferred to S-P3 W{N=BackendShape-admission-ledger-wave}**". The dossier prose at `2D:219-223` binds the rule explicitly: "every Open Research Question row carries an explicit wave or pass anchor [...] so the OQ ledger never inherits the 'later research pass' deferral the §3 CH6 prohibition forbids." Discharge verbatim per V1 required revision. |
| F12 | ACCEPT | **ACCEPT (preserved + strengthened)** | `2A:60-61` + `2B:62` + `2C:116` + `2D:107` + `2E:295-315` + `2F:464` (V2 equivalents) | "Preserve the cross-dossier same-wave-consumer discipline verbatim." | The cross-dossier same-wave-consumer discipline is preserved cohort-wide and *strengthened* by the F8 discharge: 2D V2 now carries an explicit `same_wave_consumer` cell per `T2D-*` grounded row (cell name verbatim from F8 required revision); 2A V2 binds "named same-wave consumer; orphan-kernel research is rejected" at `2A:99`; 2B V2 binds Lock 16 same-wave consumer with executable `grep` evidence at `2B:54-66`; 2C V4 binds Lock 14 v+1 same-wave non-JSON consumer at `2C:104` + same-commit C4-W10 binding at `2C:301`; 2E V7 binds the per-entry abstract-primitive citation + hardware gate + abstract primitive name with V7 audit_state column normalisation at `2E:81-89`; 2F V6 binds the V5 admission ledger per-row consumer cell with F-V2-P1ABC-RERECORD Stage-0 dependency. |
| F13 | ACCEPT | **ACCEPT (preserved + strengthened)** | `2E:147-153` (V6 fold note re REDRESS pre-block) | "The lineage-vs-consumer distinction is exactly the CH6 anti-paper-close move. Preserve in V2." | 2E V7 preserves the V6 fold note verbatim at `2E:204-214` ("V6 fold note: per the dispatch instruction 'REDRESS pre-block 88 PMULL prefix-XOR hot body, 89 CSSC CTZ next-bit bulk (do NOT re-open)' — the V6 grounding of C-P2C-2 cites the published PMULL/PCLMUL lineage as the *abstract primitive justification*, not as a license to replay the REDRESS-88/-89 consumers"). Strengthened: V7 adds explicit `architecture_pressure` audit_state values to both rows at `2E:239-240` with comment-block lineage-vs-consumer rationale at `2E:87-89` ("`architecture_pressure` applied to PMULL prefix-XOR + CSSC CTZ (consumer-rejected per REDRESS-88/-89 with abstract-primitive lineage intact)"). The lineage-vs-consumer discrimination is now an explicit per-row column value, not only a paragraph-level discipline. |

## Cross-Dossier ANTI-PAPER-CLOSE Census (V2)

| dossier | grounded count | refuted count | partial / NOT-VALIDATED count | skeleton-only count | V1 → V2 delta | CH6 honesty signal at V2 |
|---|---|---|---|---|---|---|
| 2A | 9 | 5 | 1 (architecture_pressure: asmjson) | 0 | preserved | REF-001..REF-005 all preserved load-bearing; T2A-REF-004 verbatim at `:139`; six OQs now carry wave/pass anchors (F11 discharge) |
| 2B | 11 | 5 | 3 | **0 (V1=3 → V2 DELETE-RECOMMENDED)** | refuted unchanged; SKELETON triple converted to binary DELETE per CH4-F1 | `techniques_grounded: 11` honest; V2 §A6 cost ledger 9-row eight-cell manifest; §R3 + §A5 carry DELETE action with line-pinned removal sites |
| 2C | 14 | 5 | 3 (NOT-VALIDATED witness rows preserved) | 0 | preserved | Technique Grounding Table 4-row two-cell split (F9); BBNF-self witness firmed ADMITTED-VIA-C4-W10 same-commit binding (F10) |
| 2D | 7 | 5 | 1 (`P1-1B-D2`) + 1 (`T2D-FIVE-SHAPE-FINITE-SET`) | 0 (4-of-5 marker-string lowerers refuted; BackendShape Admission Ledger materialises per-shape disposition) | preserved + Per-Technique Transfer Coverage table added + BackendShape Admission Ledger materialised | Same-wave-consumer column added per T2D-* row (F8); 4 of 5 grounded rows downgraded to `grounded-class-only`; six OQs carry explicit wave anchors (F11) |
| 2E | 14 | 6 | 7 (source_backed / conditional rows) | 0 | +V7 audit_state column normalisation + per-entry `not_S-P3-eligible_at_V1` labels | V7 fold-addition `CH6-F7-NOT-S-P3-ELIGIBLE-AT-V1-LABELS`; "Other esoterica" block carries explicit per-entry state preamble (F7); PMULL/CSSC `architecture_pressure` cells (F13 strengthened) |
| 2F | 14 | 6 | 6 (5 PTG rows partial; 1 admission-decision-blocked) | 0 | preserved + LAC-2F-V5-02 elevated to T-P3 §3C Lock 1 v+1 amendment | parse-that-base-not-in-workspace pin preserved; Q1 carries W11 anchor + CH3 regression pre-flight; LAC-2F-V5-02 binds "no cross-call retained classifier state" cohort-wide |

**Cohort signal at V2:** 69 grounded / 32 refuted / 21 partial-or-NOT-VALIDATED /
**0 skeleton-only (V1=3)**. Refutation density rises to 32/101 = 31.7%
of the grounded+refuted binary. The SKELETON-only collapse to 0 is the
most honest CH6 disposition shift in the V2 cycle: V1 disclosed three
non-admissible contracts as honest non-admissions; V2 binds them to
binary DELETE under Lock 16 v+1 close-state vocabulary, removing the
ambiguity entirely. This is the inverse of paper-close — paper-close
would have smuggled the contracts into the admitted count; V2 deletes
them.

## CH6 Cross-Cuts Discharged (V2)

| dispatch focus | V2 discharge in cohort | residual CH6 obligation |
|---|---|---|
| V1 F7 — 2E esoterica `not_S-P3-eligible_at_V1` labels | 2E V7 fold-addition `CH6-F7-NOT-S-P3-ELIGIBLE-AT-V1-LABELS` at `:73-80`; per-entry labels at `2E:238, :243, :245, :247, :248`; "Other esoterica" preamble at `2E:305`; BCAX paragraph qualified at `2E:243` | None — discharge verbatim per V1 required revision |
| V1 F8 — 2D Technique Grounding Table same-wave-consumer column | 2D V2 Technique Grounding Table at `2D:112-126` adds 5th column; 4 of 5 grounded rows downgraded to `grounded-class-only`; dossier prose at `2D:104-110` binds the rule | None — discharge verbatim per V1 required revision |
| V1 F9 — 2C verdict two-cell split | 2C V4 Technique Grounding Table at `2C:111-118` carries the two-cell split for 4 generator-gap rows with explicit "Generator-stub gap:" callouts and "Two-cell template inherited from `2C-BBNF-SELF-FALSIFIER`" trailers | None — discharge verbatim per V1 required revision |
| V1 F10 — 2C BBNF-self firmed | 2C V4 row at `2C:301` reads `ADMITTED-VIA-C4-W10` with same-commit binding per SK-V14 SPEC §13 W10; "transitioning-if" V1 phrasing struck | None — discharge verbatim per V1 required revision (firm option) |
| V1 F11 — 2A + 2D OQ wave/pass anchors | 2A V2 UNKNOWNs at `2A:179-184` each carry "Discharge anchor: T-P3 §3C" or "S-P3 P3-B sidecar wave"; 2D V2 UNKNOWNs at `2D:227-232` carry dedicated "wave / pass anchor" column with explicit S-P3 W{N} names | None — discharge verbatim per V1 required revision |
| Cohort refutation density preservation (1:2 anti-paper-close pattern) | 32 refuted / 101 grounded+refuted = 31.7% (V1 was 31/100 = 31%); rises by one explicit refutation row in 2D | None — pattern strengthens at V2 |
| 2B SKELETON-only count → 0 binary DELETE | 2B V2 §R3 at `2B:54-66` + §A5 at `:231-233` + §A6 at `:253` carry the per-contract DELETE action with line-pinned removal sites and executable `grep` evidence; Lock 16 v+1 close-state vocabulary citation | None — most honest possible CH6 disposition |
| Same-wave consumer discipline (V1 F12) preservation | Cross-dossier discipline preserved + strengthened: 2D V2 adds the explicit per-row cell that V1 F12 named the cohort's "strongest CH6 defense"; 2A/2B/2C/2E/2F preserve their respective binding sites | None — discipline strengthens at V2 |
| Lineage-vs-consumer discrimination (V1 F13) preservation | 2E V7 preserves the V6 fold note verbatim + adds `architecture_pressure` audit_state column values to PMULL + CSSC rows with V7 fold-addition `CH7-AUDIT-STATE-COLUMN-NORMALISATION` at `2E:81-89` | None — discrimination strengthens at V2 |

## Disposition

**CONFIRM at V2.** All five V1 REVISE findings (F7, F8, F9, F10, F11)
are discharged at V2 HEAD with the exact mechanism named in V1's
required-revision column and with the closure verbiage CH6 binds. All
eight V1 ACCEPT findings (F1-F6, F12, F13) carry forward intact; F12
and F13 strengthen at V2. The cohort cross-cuts read clean: refutation
count rises by one (32 vs V1 31); SKELETON-only count drops by three
(0 vs V1 3, with binary DELETE disposition); per-row same-wave-consumer
discipline becomes explicit cell text in 2D; per-row state cells become
explicit text in 2E; verdict two-cell split becomes explicit text in
2C; BBNF-self witness firmed to same-commit-binding admission in 2C;
OQ wave/pass anchors become explicit cell text in 2A and 2D.

**ACCEPT-rate over the 13 findings: 13 ACCEPT / 0 REVISE / 0 REJECT =
1.00 ACCEPT.** This is the first ≥0.95 ACCEPT cycle on V2 per §3Z
discipline; V3 expected disposition CONFIRMING (no new findings), with
cohort §3Z LOCK at V3 close per the V2 dispatch §3 prediction.

No new V2 REVISE findings emerge: the V1 dispatch correctly identified
the five seams where T-P3 could turn citation density into
implementation authority; V2 closes those seams; no further seam opens
under the V2 amended dossiers.

## Fold Requirements For V3

None new from CH6. V2 → V3 is a confirming cycle. The V3 dispatch
should re-execute the V2 ACCEPT-set against V2 HEAD (or successor at V3
fold time) to validate the cohort §3Z LOCK condition (two consecutive
≥0.95 ACCEPT cycles). If V3 confirms 13/13 ACCEPT, CH6 LOCKS at V3
close per §3Z.
