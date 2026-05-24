---
agent: CH6
pass: T-P2-research
cycle: V1
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-05-23T22:00:00-04:00
authority:
  - restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md
  - restart/prompts/totality/PASS-2-RESEARCH.md §3 (CH6)
head_pin: 8d5e4e8f6
inputs_audited:
  - restart/audit/totality/p2/2A-sota-landscape.md (168 lines; 14 sources)
  - restart/audit/totality/p2/2B-primitive-vocabulary.md (287 lines; 18 sources; 6 admitted + 3 SKELETON)
  - restart/audit/totality/p2/2C-grammar-neutrality.md (457 lines; 9 sources)
  - restart/audit/totality/p2/2D-cost-model.md (125 lines; 12 sources)
  - restart/audit/totality/p2/2E-host-arch-esoterica.md (419 lines; 28 sources)
  - restart/audit/totality/p2/2F-parse-that-gaps.md (583 lines; 24 sources)
cohort_totals:
  primary_citations: 105
  techniques_grounded: 69
  techniques_refuted: 31
  skeleton_only_contracts: 3
disposition_summary:
  ACCEPT_findings: 8
  REVISE_findings: 5
  REJECT_findings: 0
  ACCEPT_rate: 0.615
---

# T-P2 V1 CH6 — ANTI-PAPER-CLOSE

## Lens Contract

CH6 binds per `PASS-2-RESEARCH.md §3` and the dispatch focus in
`CHALLENGE-CONTEXT.md §2`. CH6 audits whether each dossier converts its
citation density into bbnf-specific transfer claims, or whether it closes
on reference count alone. The dispatch nails five specific scans:

1. No "validated" / "proven" on citation density alone.
2. Reference-stuffing flagged (N sources cited, none integrated).
3. Every grounded technique states a bbnf-specific transfer reason, not
   merely "SOTA does it this way".
4. No deferral to "later research pass" / future-pass slippage.
5. The 2B `3/9` SKELETON-ONLY contracts must be flagged as non-admissions,
   not silently treated as part of the Layer 1 vocabulary count.

Per `§3Z` discipline, V1 expects ≥30% REVISE (an all-ACCEPT wave is itself
paper-close). The aggregator targets ≥95% ACCEPT only on a converged V≥2.

## Verdict

**REVISE.** The V1 cohort is substantively anti-paper-close at the
*structural* level — every dossier carries explicit refutations as
first-class output (5 in 2A, 4 in 2B, 5 in 2C, 5 in 2D, 6 in 2E, 6 in 2F);
2B explicitly flags 3/9 Layer-1 contracts as SKELETON-only and
non-admissible; 2D refutes the P1-P8 cascade as bbnf-local heuristic, not
literature-grounded optimizer; 2F refutes its own headline "extraction
mandatory" by pinning `parse-that` *not* in the workspace and downgrading
to absorption-decision-gated; 2E refutes "instruction availability implies
admission" and the cross-arch portability of GFNI/VBMI2.

What still lets V1 close as paper-architecture without revision: (a) 2D's
four-of-five lowerers carrying marker strings is correctly refuted, but
the dossier still uses "grounded" for the search/extraction class without
naming the same-wave consumer that would deflate marker-string lowerers;
(b) 2C's 14 grounded techniques include rows where the W3C/OASIS citation
density carries the "grounded" verdict before the generated-fact transfer
is named (the LAC-2C-02 grammar-SHAPE leak rule is right but several
Technique Grounding Table rows mark "grounded" *before* discharge); (c)
2E's V6 fold adds nine new abstract primitives (Per-Entry-Published-
Citation, S-P2-V3-Candidate-Crossref, PMULL-VPCLMUL-Lineage, LD4-
Interleaved-Classify, etc.) without explicit "candidate, not admission"
labels — V6 paragraph language treats the citation lineage as a
strengthening; CH6 reads it as expanded surface that still needs cell-by-
cell discharge; (d) 2F's Q1 (`bbnf-regex` absorption) and 2D's UNKNOWN-2D-01
through -06 are research questions, not deferrals — but their phrasing
("the natural locus is S-P3 V2 P3-C" / "the SK-V14 W11 wave admission")
binds *T-P3 timing*, which is acceptable, while 2C's
"NOT-VALIDATED (transitioning to ADMITTED-VIA-C4 if C4 wave lands)" row
for the BBNF-self witness is a soft deferral that must firm or downgrade.

## Findings

| # | disposition | target | finding | required revision |
|---|---|---|---|---|
| F1 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:43-48` + `:174-184` + `:222-231` | 2B is the load-bearing CH6 honesty test and passes it. The Executive Summary, the §A5 audit table, and §R3 all state that `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED` are **skeleton-contract only** at HEAD and explicitly non-admissible until scalar oracle + checkasm cell + same-wave consumer land. The `techniques_grounded: 11` frontmatter is honest precisely because the 3 skeleton contracts are not counted as admissions and are routed to OQ for `deleted`-vs-ship disposition. The Layer 0 = 138 macros number is a corpus count, not an admission claim. | None. Preserve the SKELETON disclosure in V2; do NOT let any V2 fold smuggle the three contracts into the admitted count without scalar+checkasm artefacts. |
| F2 | ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:74` + `:111-113` | 2A's CH6-overlay row is explicit: "Grounded never means validated design. Microbench parity, citation density, and reference-stuffing without same-wave consumer / row movement remain non-admitting." `T2A-REF-004` ("citation density + microbench parity admits a primitive") is itself a first-class refuted row, citing SK-V12 W2 + W4 as published-internal evidence of where citation+parity stopped short of admission. This is the anti-paper-close posture CH6 is built to find; 2A meets it. | None. Preserve the REF-004 row verbatim in V2. |
| F3 | ACCEPT | `restart/audit/totality/p2/2F-parse-that-gaps.md:32-34` + `:236-244` + `:472-473` | 2F refutes its own headline by pinning `parse-that` base crate **not** in `skinny/Cargo.toml:10` and downgrading the "extraction mandatory" V1-prior claim to an absorption-decision gate (Q1). The Architectural-Assertions-Refuted table lists six load-bearing refutations including (a) `regex-syntax` HIR is NOT admissible as runtime dep, (b) the simdjson `prev_in_string` cross-call retention is inadmissible under Lock 1, (c) `parse_that_regex::unescape_string` at `lib.rs:718` is NOT a SIMD body. The ledger marks `bbnf_regex_hir_engine` + `regex_lazy_dfa_fallback` as `source_backed; blocker = absorption decision` — honest non-admission. | None. The Q1 absorption-decision frame is exactly the anti-paper-close posture for the largest single architectural change in 2F. |
| F4 | ACCEPT | `restart/audit/totality/p2/2D-cost-model.md:34-48` + `:80-87` | 2D's executive summary states "the literature supports a search-and-extraction decision-engine class … not a hardcoded P1..P8 priority cascade." `P1-1B-D3` (the 8-step canonical-order claim) and `P1-1B-D6` (marker-string lowerers at 4 of 5 shapes) are themselves explicitly refuted as not defensible against the cited record. No published parser ships marker-string lowerers; the cite is internal-evidence-based, not paper-close. | None. Preserve P1-1B-D3 / D4 / D6 refutations in V2. |
| F5 | ACCEPT | `restart/audit/totality/p2/2E-host-arch-esoterica.md:82-89` + `:172` + `:378-384` | 2E's Executive Summary lists six refutations as first-class output including the V6-NEW refutation "x86 AVX-512 GFNI/VBMI2 are cross-arch portable primitives" — naming the *binding cross-arch primitive-vocabulary gap*. The `svmatch_u8`-as-NEON-primitive row is explicitly refuted (SVE2-only per ARM ARM §C2.2). Each esoterica entry carries `{published citation, abstract primitive name, hardware gate}` per the dispatch §2 mandate. | None on the refutation register; see F7 for the V6 nine-new-additions surface expansion that needs cell-level discharge. |
| F6 | ACCEPT | `restart/audit/totality/p2/2C-grammar-neutrality.md:283-296` + `:446-457` | 2C's "Feature / Witness Transfer Ledger" carries the CH6 anti-paper-close requirement explicitly: rows without an existing production consumer carry state `NOT-VALIDATED`. CSS declaration values is the single `ADMITTED-EVIDENCE` row; selectors / declaration-values-extended / visual-functions / Sheets / BBNF-self are all `NOT-VALIDATED`. The ledger is honest. | None on the ledger frame; see F10 for the soft-deferral phrasing on the BBNF-self row. |
| F7 | REVISE | `restart/audit/totality/p2/2E-host-arch-esoterica.md:47-54` (`v6_fold_additions`) + `:231-235` (Other esoterica 2E surfaces) | The V6 fold adds 6 fold-addition tags (`PMULL-VPCLMUL-LINEAGE`, `LD4-INTERLEAVED-CLASSIFY-CITATION`, `SVE2-MATCH-NEON-PORT-REFUTATION`, `X86-AVX512-SECONDARY-EXPANSION`, etc.) plus an "Other esoterica 2E surfaces for future evaluation" block naming three NEW candidates (`Interleave4Classify` LD4, `BicXor3Bcax`, `Crc32CHash`). These are correct surface expansions — but the block does not carry explicit `NOT-S-P3-ELIGIBLE-AT-V1` labels comparable to the C-P2C-6 EOR3 row. A T-P3 reader could promote one of the three on citation strength alone (BCAX has both ACLE citation and a "higher relevance than EOR3" narrative). The Lock 16 manifest at `:237-263` is the right gate, but the BCAX/LD4/CRC32C entries are introduced in narrative paragraphs *before* the manifest gate is named. | Add explicit `state = source_backed; not S-P3-eligible at V1; eligible only post-F-V2-P1ABC-RERECORD` labels to every "Other esoterica 2E surfaces" entry. Tighten the BCAX paragraph at `:209` so the "higher relevance than EOR3" claim is qualified as "would be higher relevance *if* the AND-NOT-XOR algebra is named as a measured hot fan-in" — not as a standalone superiority claim that could license shortlist promotion. |
| F8 | REVISE | `restart/audit/totality/p2/2D-cost-model.md:54-66` (Technique Grounding Table grounded rows) | The grounded rows for `T2D-EGRAPH-EXTRACTION` / `T2D-EQSAT-ORIGIN` / `T2D-BURG-FINITE-ALTERNATIVES` / `T2D-CSP-FEASIBILITY-LAYER` carry literature citation + structural-shape evidence but do not name a same-wave consumer that demonstrates the search/extraction class actually moves a row in bbnf. The architectural defense at `:72-78` repeats "live invocation at `passes/src/lib.rs:477` is correctly shaped" — *shape* is not *admission*. The four-of-five marker-string lowerer refutation at `P1-1B-D6` is correct, but its inverse is paper-close-shaped: "egg + BURG + Mison cited, therefore the search/extraction class is grounded" without naming the row consumer is the same shape that 2D refutes for `CollapsedStage`. | For each `T2D-*` grounded row, add a `same_wave_consumer` cell naming the specific generated path or measured row that would demonstrate the grounded class beats the cascade in bbnf — or downgrade to `partial` until such cell is named. The egg-as-shape-class claim is real; the egg-as-bbnf-admission claim needs the discharge column 2C added under V3 fold. |
| F9 | REVISE | `restart/audit/totality/p2/2C-grammar-neutrality.md:105-120` (Technique Grounding Table) + `:295` (BBNF-self witness state) | The Technique Grounding Table has 14 `grounded` rows; six of them (2C-CSS-TOKEN-ALPHABET, 2C-CSS-SELECTOR-SCOPE, 2C-CSS-CALC-VAR, 2C-BACKENDSHAPE-FIVE, 2C-RUNTIME-PROVIDER-REGISTRY, 2C-CSS-FACT-STREAM) carry W3C standards citations or local-spec citations as the source authority, but the bbnf-specific note in each row often names the *gap* (e.g. "alphabet must come from generated grammar config, not from `b"{}[],:\""` ") rather than the discharged transfer. CH6 reads this as honest gap-naming, not citation-density admission — but the verdict column should be `partial` for rows whose discharge requires generator work the dossier identifies as not yet landed. The "Closure Criteria For Live Grammar Leaks" at `:300-306` correctly names what is NOT closure; the Technique Grounding Table verdict column does not yet inherit that strictness. | For each Technique Grounding row whose bbnf-specific note identifies a generator gap (≥4 of the 14 rows), downgrade the verdict from `grounded` to `partial-grounded-pending-generator` or split into a `standards-citation-grounded` + `bbnf-discharge-pending` two-cell pair. The 2C-BBNF-SELF-FALSIFIER row at `:110` already carries the cleanest discharge frame (the BBNF-self literal escape is the C4 same-wave consumer, no carve-out needed) — extend that two-cell pattern to the other rows. |
| F10 | REVISE | `restart/audit/totality/p2/2C-grammar-neutrality.md:296` (BBNF-self witness row) | The BBNF-self witness state cell reads: `NOT-VALIDATED (transitioning to ADMITTED-VIA-C4 if C4 wave lands)`. The transitioning-if-wave-lands phrasing is the exact pattern CH6 flags as soft deferral — it relies on a future wave to resolve current dossier validity. The C4 cell in 2C's own `:215-243` rightly carries an explicit "ADMIT" verdict under Lock 14 v+1 strict read; the witness row should mirror that or stand firm at `NOT-VALIDATED`. | Either firm the witness row to `ADMITTED-VIA-C4-SHAPE-IDENTICAL` (since 2C itself argues C4's BBNF-self consumer is shape-identical and discharges Lock 14 v+1 in the same wave that admits the SIMD body) or strike the transitioning clause and hold at `NOT-VALIDATED until C4 admission`. A V2 state that depends on a future S-P3 wave landing is exactly the deferral-to-later-research-pass CH6 forbids. |
| F11 | REVISE | `restart/audit/totality/p2/2A-sota-landscape.md:124` + `restart/audit/totality/p2/2D-cost-model.md:97` (Open Research Questions) | 2A's OQ for the sonic-rs four-leaf generalisation to CSS L4 / Sheets and 2D's UNKNOWN-2D-05 (`CollapsedStage` aarch64 restatement) carry `verify_action` text but do not bind T-P3 timing or wave anchor. Compare 2F's Q1 which names "SK-V14 W11 the natural locus" and 2D's other UNKNOWNs which name "S-P3 V2 P3-C the natural locus" — those firm OQ closure to a wave. The unbound 2A and 2D OQ rows leave T-P3 free to inherit the OQs as "later research pass" — exactly the §3 CH6 prohibition. | Add an explicit wave or pass anchor to every OQ row: either "discharged at T-P3 §3C amendment authoring" or "deferred to S-P3 W{N}" or "abrogated: NOT a T-P2 in-scope question, drop to T-P3 backlog with named owner." The discharge anchor is the difference between an honest UNKNOWN and a paper-close future-pass slippage. |
| F12 | ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:60-61` + `restart/audit/totality/p2/2B-primitive-vocabulary.md:62` + `restart/audit/totality/p2/2C-grammar-neutrality.md:116` + `restart/audit/totality/p2/2D-cost-model.md:107` + `restart/audit/totality/p2/2E-host-arch-esoterica.md:295-315` + `restart/audit/totality/p2/2F-parse-that-gaps.md:464` | The cross-dossier same-wave-consumer discipline is consistent: 2A binds "named same-wave consumer; orphan-kernel research is rejected"; 2B binds Stage D of the 5-stage admission process; 2C binds Lock 14 v+1 same-wave non-JSON consumer or measured-deletion record; 2D binds `admits_sink_only` requiring generated consumer; 2E binds the material-differential gate with V6's `published_citation` + `abstract_primitive_name` additions; 2F binds the V5 admission ledger per-row consumer cell with F-V2-P1ABC-RERECORD Stage-0 dependency YES/NO. This is the cohort-wide anti-paper-close spine and it is consistent. | None. The cross-dossier same-wave-consumer discipline is the cohort's strongest CH6 defense; preserve verbatim. |
| F13 | ACCEPT | `restart/audit/totality/p2/2E-host-arch-esoterica.md:147-153` (V6 fold note re REDRESS pre-block) | The V6 fold note for C-P2C-2 PMULL+CSSC reopen explicitly distinguishes "the *abstract primitive justification* (Lemire 2016 algebra, simdjson VLDB 2019 §3.3)" from "license to replay the REDRESS-88/-89 consumers." This is the canonical anti-paper-close discrimination: citation grounds the primitive's existence as a published technique; REDRESS measured rejection of the *consumer shape* binds. The SK-V7 W10/W10b measured rejection remains binding. | None. The lineage-vs-consumer distinction is exactly the CH6 anti-paper-close move. Preserve in V2. |

## Cross-Dossier ANTI-PAPER-CLOSE Census

| dossier | grounded count | refuted count | partial / NOT-VALIDATED count | skeleton-only count | CH6 honesty signal |
|---|---|---|---|---|---|
| 2A | 9 | 5 | 1 (architecture_pressure: asmjson) | 0 | REF-001..REF-005 all load-bearing; T2A-REF-004 is the explicit CH6-shape refutation |
| 2B | 11 | 4 | 3 | **3 (FSM_DISPATCH_THREADED, FRAME_PUSH_BOUNDED, FRAME_POP_BOUNDED)** | §A5 audit table explicit; `techniques_grounded: 11` does NOT count the 3 skeletons |
| 2C | 14 | 5 | 3 (NOT-VALIDATED witness rows: selectors, declaration-values-extended, visual-functions) | 0 | "Feature / Witness Transfer Ledger" carries explicit NOT-VALIDATED state column |
| 2D | 7 | 5 | 1 (`P1-1B-D2` cost-facts) + 1 (`T2D-FIVE-SHAPE-FINITE-SET`) | 0 (but 4 of 5 lowerers carry marker strings — refuted) | P1-1B-D3 / D4 / D6 explicitly refuted as not-literature-grounded |
| 2E | 14 | 6 | 7 (source_backed / conditional rows in Hardware Gates table) | 0 | V6 REFUTATION block adds explicit GFNI/VBMI2 cross-arch portability refutation |
| 2F | 14 | 6 | 6 (5 PTG rows marked partial or refuted; 1 admission-decision-blocked) | 0 | parse-that-base-crate-not-in-workspace pin downgrades V1-prior "extraction mandatory" |

**Cohort signal:** 69 grounded / 31 refuted / 21 partial-or-NOT-VALIDATED /
3 skeleton-only. Refutations are 31/100 = 31% of the grounded+refuted
binary; partials + NOT-VALIDATEDs + skeletons add another 24 honest non-
admissions. This is well above the §3 ≥30% REVISE-shape signal that V1
expects; the dossiers are NOT paper-closing on citation density.

## CH6 Cross-Cuts Discharged

| dispatch focus | discharge in V1 cohort | residual CH6 obligation |
|---|---|---|
| No "validated" / "proven" on citation density alone | 2A:74 CH6-overlay row binds explicitly; no dossier uses bare "validated"/"proven"; "grounded" is the verdict and is shape-defined per §3 | F8 (2D) + F9 (2C) require Technique Grounding Table verdict tightening |
| Reference-stuffing flagged | No dossier hits the reference-stuffing pattern (N cited, none integrated) — every cite carries either a path:line discharge or a state column entry; 2E's 28 sources include explicit "cross-arch primitive vocabulary only" labels on x86 secondary rows | None |
| Every grounded technique states bbnf-specific transfer reason | 2A "bbnf-specific note" column per row; 2B "bbnf-specific note" column per row; 2D bbnf-specific note column; 2E "bbnf-specific note" column; 2F "bbnf-specific note" + per-gap dossier — uniformly present | F9 (2C) requires same-wave-consumer cell strengthening for ≥4 grounded rows whose bbnf-specific note identifies a generator gap |
| No deferral to "later research pass" | 2F Q1 binds W11 wave; 2D UNKNOWNs bind S-P3 P3-C and P3-A waves; 2E V6 fold defers C-P2C-2 to V2 material-differential checklist (binding, not slipping) | F10 (2C BBNF-self transitioning-if) + F11 (2A + 2D OQ rows without wave anchor) require explicit wave or pass anchoring |
| 2B 3/9 SKELETON-ONLY contracts flagged | 2B Executive Summary line 44-47; 2B §A5 table at `:174-184`; 2B §R3 at `:222-231`; 2B OQ at `:249-250` (each contract has explicit `deleted`-vs-ship disposition question); LAC-2B-02 binds same-commit scalar+checkasm requirement | None — discharged cleanly across four loci |

## Disposition

REVISE, not REJECT. The V1 cohort meets the CH6 anti-paper-close test on
*structural posture* (refutations as first-class output, same-wave
consumer discipline cross-dossier consistent, 2B SKELETON disclosure
honest, parse-that-not-in-workspace pinned, P1-P8 cascade refuted, GFNI/
VBMI2 cross-arch portability refuted, prev-in-string retention refuted).
The five REVISE findings are tightening obligations, not paper-close
rescues:

- **F7:** label 2E V6 new candidates (BCAX, LD4, CRC32C) as `not S-P3-
  eligible at V1` in the same paragraph that introduces them.
- **F8:** add same-wave-consumer cells to 2D's `T2D-*` grounded rows; the
  egg/BURG/Mison citation grounds the *class*, not the bbnf *admission*.
- **F9:** downgrade 2C's grounded-pending-generator rows from `grounded`
  to a two-cell `standards-citation-grounded` + `bbnf-discharge-pending`
  split.
- **F10:** firm or strike 2C's BBNF-self witness `transitioning-if-C4-
  wave-lands` phrasing — soft deferral.
- **F11:** add a wave or pass anchor to every OQ row in 2A and 2D.

ACCEPT-rate over the 13 findings: **8 ACCEPT / 5 REVISE / 0 REJECT =
0.615 ACCEPT, 0.385 REVISE.** This is the V1-shape signal (REVISE >30%);
V2 fold should land the five revisions and target ≥0.95 ACCEPT in V2 or
V3. No finding alone blocks convergence; the cluster blocks until
addressed because each is exactly the seam where T-P3 could turn a
citation density into an implementation authority.

## Fold Requirements For V2

1. **2E:** label every "Other esoterica" + V6 fold-addition entry with
   explicit `state = source_backed; not S-P3-eligible at V1` in the same
   paragraph that introduces the citation; the V6 lineage strengthening
   does not by itself promote shortlist eligibility.
2. **2D:** add a `same_wave_consumer` cell to each `T2D-*` grounded row,
   naming the generated path or measured row that would demonstrate the
   class beats the cascade; downgrade to `partial` if the cell is not
   namable at V2 fold time.
3. **2C:** split the Technique Grounding Table verdict column for
   generator-gap rows into `standards-citation-grounded` + `bbnf-
   discharge-pending`; the 2C-BBNF-SELF-FALSIFIER row is the two-cell
   template.
4. **2C:** firm the BBNF-self witness row to `ADMITTED-VIA-C4-SHAPE-
   IDENTICAL` per 2C's own §C4 discharge, or hold at `NOT-VALIDATED`
   without the transitioning clause.
5. **2A + 2D:** add a wave or pass anchor to every Open Research Question
   `verify_action` (T-P3 §3C, S-P3 W{N}, T-P3 backlog with owner).
6. **Cross-dossier:** preserve the SKELETON-only / parse-that-base-not-in-
   workspace / PMULL-consumer-rejection-vs-primitive-lineage discriminations
   verbatim — these are the cohort's strongest CH6 defenses.
