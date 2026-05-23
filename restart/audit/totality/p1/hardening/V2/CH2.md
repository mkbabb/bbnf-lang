---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V2
disposition: REVISE
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
v1_review_inputs:
  - restart/audit/totality/p1/hardening/V1/CH2.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md
lens_obligation: "PASS-1-EXCAVATION.md:110-114 — Lock 14 holds across inventories; no divergence catalogued as JSON-only when grammar-neutral substrate fact; 1C runtime census flags every grammar-named module in a generic crate; 1D separates JSON-empirical from grammar-neutral findings; no grammar-name leak passes uncited; pass-layer grammar-shape leaks (D8 recognizer-byte + D10 role-mining) must be carried as upstream Sheets/BBNF-self generalization blockers distinct from codegen-layer name leaks."
authority: restart/locks/LOCKS.md:220
head_commit_audited: 87816a2cd0d16ad0cdcf7b6483ef106efe363b52
verification_method: "live re-run of all Lock 14 verification commands at HEAD against V2 amended 1B/1C/1D/1F citations; mechanical Python extraction of grammar-named symbols from crates/core/src/runtime/mod.rs:25-71 (133 raw / 6 cited-neutrals present in the 25-71 window → 127 grammar-named, NOT 126); rg census of parser-name leak; 1B D8/D10 row distinctness across spec-claim table, generic-crate census, Sheets/BBNF-self implications, and divergences-catalogued tables; 1D row 106 split verification against 1B D8/D10; PC-008 + U-PC-002 cross-cite presence in 1D:131; 1A-DIV-008 two-cursor substrate-union nuance disposition presence; AP-020 CSS source-sidecar new row presence + AP-009 distinction; Lock 14 holds across all amended inventories"
v2_disposition_focus:
  - "1B D8/D10 split (V1-REVISE-9 fold) — three-table coverage at 50-51 (spec-claim), 63-64 (generic-crate census), 71-73 (Sheets/BBNF-Self Implications), 86-87 (divergences-catalogued)"
  - "1C undercount (V1-REVISE-7+8 fold) — 30/15 + 126/47 + ~190 LOC + 2.5× consumer-rewire band"
  - "1D row 106 split (V1-REVISE-9 mirror) — pass-layer leak row at 1D:124 distinct from codegen-layer leak row at 1D:123; Sheets/BBNF-self generalization blocker explicit"
  - "1D row 113 cross-cite (V1-REVISE-10 fold) — PC-008 + U-PC-002 verify-before-rederive obligation carried into bbnf-simd grammar-neutral row at 1D:131"
  - "1D row CSS L4 layout asymmetry (V1-REVISE-11 fold) — new row at 1D:134 with R4 regen-css canonical-layout determinant"
  - "AP-020 CSS source-sidecar comparator plane (V2 CH5-004 binding fold) — at 1F-anti-pattern.md:80,105"
  - "Lock 14 holds across all 8 amended inventories"
verdict_summary:
  accept: 11
  revise: 1
  reject: 0
  accept_rate: "11/12 = 91.7%"
hard_cap_minutes: 30
---

## Verdict

REVISE. The V2 micro-fold discharges all four V1 CH2 REVISE items and lands the AP-020 CSS source-sidecar new row, but the V2 1C/1F mechanical count for grammar-named reexports at `crates/core/src/runtime/mod.rs:25-71` is **off by one in the wrong direction**: V2 cites "126 grammar-named symbols" by subtracting "10 grammar-neutral exports" (`DtaError, ParseErr, CompoundHandle, StringHandle, RuntimeView, StructBuilder, IntoPathSegment, Path, PathSegment, GenericAtRule`), but only **6** of those 10 neutrals are present inside the cited 25-71 window — `IntoPathSegment`, `Path`, `PathSegment` are at `mod.rs:72`, and `RuntimeView` is at `mod.rs:76`, both **outside** the cited window. Live mechanical extraction at HEAD (commit `87816a2cd`) returns 133 raw `pub use` entries in 25-71, and only `CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle, StructBuilder` are neutral within the window — yielding **127 grammar-named symbols**, not 126. The 127-vs-126 delta is small but the cite logic is the same defect class V1 CH2 flagged ("60+" floor under-reporting): the underlying mechanical extraction was performed against the right window but the neutral-subtraction list reached outside the window. Every other V2 fold item ACCEPTs: D8/D10 row split is clean across all four 1B tables; 1D row 106 split mirrors 1B at 1D:123 (codegen-layer) + 1D:124 (pass-layer) with explicit Sheets/BBNF-self generalization blocker framing and the verify_action `rg -n 'b"\\{"|...' skinny/crates/passes/src/lib.rs ZERO post-redress`; PC-008 + U-PC-002 cross-cite landed at 1D:131; AP-020 properly distinct from AP-009 (1F-anti-pattern.md:80,105) with separate path:line anchors `nonjson_css_l4.rs:222,234,299,504` (AP-020) vs `:222-234,298-303` (AP-009); 1A-DIV-008 substrate-union nuance disposition at 1A:84 + 1A-LOCK1-AMEND-001:113 carries T-P3 §3C ratification-vs-unification choice; Lock 14 holds across all 8 amended inventories (zero CH7 hits in `restart/locks/LOCKS.md`, only generic-crate prose). V1 CH2 REVISE rows (a) parser-name 19+→30 + (c) PC-008 cross-cite + (d) CSS L4 layout asymmetry all discharge cleanly; only the 1C reexport-count cite needs a +1 micro-correction.

REJECT: none. No V2 inventory fails Lock 14 grammar-name fencing or mis-attributes JSON-only when grammar-neutral.

ACCEPT-rate: 11 / 12 = 91.7%. CH2 fires single REVISE on the 1C reexport-count off-by-one above.

## V1 → V2 Discharge Table

| V1 CH2 REVISE row | V1 finding | V2 fold | V2 disposition |
|---|---|---|---|
| V1-REVISE-7 (1C undercount, parser-name) | "19+ matches" floor undercounts by ~37%; live `rg` returns 30 across 15 files | 1C `:91,123,124,199` all read **30 matches across 15 files** verbatim; google_sheets/document/{mod,canonical}.rs:43,142 sites explicitly enumerated; LOC repair band rescaled from ~50 LOC to ~190 LOC | ACCEPT — live re-run at HEAD commit `87816a2cd` confirms `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` = 30; `rg -l … \| wc -l` = 15; full file list matches |
| V1-REVISE-8 (1C undercount, reexports) | "60+ grammar-named types" understates by ~50%; mechanical extraction returns 126 | 1C `:91,122,123,200` all cite "126 distinct grammar-named symbols" + 47-line window + ~2.5× consumer-rewire band; mod.rs:34-35 alias triple enumerated | REVISE — V2 cites 126 via "subtract 10 grammar-neutral exports"; live mechanical extraction at HEAD returns 133 raw `pub use` entries in 25-71 with only 6 of the 10 cited neutrals **inside** the window (4 of them — `IntoPathSegment, Path, PathSegment, RuntimeView` — sit at lines 72/76 OUTSIDE the cited 25-71 range). Correct count is **127**, not 126. The 1C cite should be either "127 distinct grammar-named symbols within `mod.rs:25-71` after subtracting the 6 neutral exports present inside the window" or "127 distinct grammar-named symbols within `mod.rs:25-77` after subtracting all 10 grammar-neutral exports (the surface that holds all 10 neutrals)". Either repair lands a consistent 127. Same defect propagates to 1F-coherence COH-011 + 1F-past-corpora PC-017 if those rows quote the 126 figure; both currently quote only file/dir counts (9 dirs, google_sheets=10, 67 hand-written), so the propagation is contained to 1C. |
| V1-REVISE-9 (1B D8/D10 → 1D matrix) | 1B carries `P1-1B-D8` recognizer-mining and `P1-1B-D10` role-mining grammar-shape leaks but 1D's matrix collapses them with codegen-layer leaks | 1B carries D8 and D10 as **distinct rows in all four tables**: spec-claim at `1B:50-51`; generic-crate census at `1B:63-64` with "CH2 binding: upstream Sheets / BBNF-self generalization blocker — distinct surface" framing; Sheets/BBNF-Self Implications at `1B:71` (Sheets) + `1B:72` (BBNF-self) with "TWO distinct pass-layer surfaces" + "NECESSARY-BUT-INSUFFICIENT" wording; divergences-catalogued at `1B:86-87` with verify_actions. 1D `:123` (codegen-layer) + `:124` (pass-layer) splits the V1 collapsed row; same-wave consumer obligation explicit: "pass-layer grammar-shape leak unblock requires a Sheets or BBNF-self grammar fixture whose role facts arise WITHOUT code change in passes"; verify_action `rg -n 'b"\\{"|b"\\}"|b"\\["|b"\\]"|b":"|b"true"|b"false"|b"null"' skinny/crates/passes/src/lib.rs` must return ZERO post-redress | ACCEPT — D8/D10 split is clean, three-tier framing (recognizer-byte plane vs role-inference plane), NECESSARY-BUT-INSUFFICIENT relative to PRUNE-4 stamped on both rows; live source at `skinny/crates/passes/src/lib.rs:331` and `:1300-1391` reproduces the cited byte-whitelist and role-literal predicates verbatim |
| V1-REVISE-10 (1D row 113 cross-cite) | `bbnf-simd` grammar-neutral verdict not falsifiability-gated against SK-V5 PC-008 + U-PC-002 verify-before-rederive obligation | 1D `:131` reads "proved (SK-V14 axis A3 v3 §4); SK-V5 PC-008 + U-PC-002 verify-before-rederive obligation open" + note column carries `1F-past-corpora.md:74` PC-008 cross-cite + `:158` U-PC-002 verify_action `rg -n 'JSON_STRUCTURAL\|scan_json\|JsonParseIndex' skinny/crates/bbnf-simd skinny/crates/runtime` | ACCEPT — cross-cite landed; verdict now explicitly carries its open-question pointer |
| V1-REVISE-11 (1C-D11 CSS L4 layout asymmetry) | Skinny CSS L4 7-cluster vs main monolithic css_l4/ + css_pretty/ asymmetry uncited in 1D + 1F-coherence | 1D `:134` adds new row "CSS L4 layout symmetry between skinny empirical floor and main workspace canonical shape" with V2-fold tag "CH2 required revision 4" + T-P3 §3C disposition obligation + SK-V14 R4 `regen-css` xtask canonical-layout determinant framing | ACCEPT — row lands in 1D as the lens preferred (SK-V14-binding obligations dispatch surface); 1F-coherence-scan does not duplicate, which matches the lens recommendation |

## NEW V2 Surfaces (CH2 binding fold)

| ID | New surface | Lock 14 / CH2 binding | Disposition |
|---|---|---|---|
| AP-020 (1F-anti-pattern.md:44,80,105) | CSS source-sidecar comparator plane at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504` lifted as separately classifiable anti-pattern row | CH2 binding: comparator-sidecar must be fenced as comparator-only evidence plane; never accepted as runtime substrate; pair with 1A CSS fact-stream fencing (`1A-SUB-014` at `1A:42,44,57`); cite-target for any future "runtime CSS substrate" admit. AP-009 retains classification-only entry (`:69,94`); AP-020 lifts the sidecar-as-anti-pattern row separately so V2 hardening can dispatch them on different waves: AP-009 is the row-plane evidence-accounting wave; AP-020 is the same wave but with a separate consumer (comparator-sidecar fence consumer) | ACCEPT — distinct path:line anchors (`:222,234,299,504` for AP-020 vs `:222-234,298-303` for AP-009); both rows carry V2 Planning Metadata (CH4 carrier) at `1F-anti-pattern.md:94,105` |
| 1A-DIV-008 (1A:84,113) | Substrate-union two-cursor nuance disposition pending T-P3 §3C: ratify two-cursor shape OR mandate unification | CH2 binding (CH5/CH6 cross-cut): 1D `:117` (row 100) records "Single substrate proved as substrate cardinality" (REDRESS attribution); 1A-DIV-008 records two structurally independent cursors at HEAD. Both can hold under different definitions — T-P3 §3C must choose. Renamed `StructuralIndex` scanner (CH5-002), Track 2 substrate-helper sharing (CH5-005), CSS source-sidecar (CH5-004 / AP-020), proof-witness generic-runtime exports (CH5-007) are all sub-cases of the same Lock 1 union-vs-split disposition. Until T-P3 closes, 1D `:117` carries an open `T-P3 §3C PENDING` flag | ACCEPT — nuance disposition correctly cross-folded between 1A `:84` (DIV) + 1A `:113` (LOCK-1-AMEND) + 1D `:117` (row 100); no paper-close at 1D `:117` |
| 1A-SUB-018 (1A:71) | S-P1 CH2 13/17 parse_only + 14/17 direct rank-1 envelope mis-attribution census carries forward | CH2 binding: envelope name dominance is a Lock-14 mis-attribution — the JSON-grammar names hide the grammar-neutral `dispatch` primitive. `DirectParser::skip_value` is "substrate + dispatch in equal parts"; per SK-V14 CH5 V3, the dispatch envelope dominance does not invalidate Lock 1 (`runtime/src/tape/` is still single) but invalidates any reading of Lock 14 that allows the JSON envelope name to stand as the primitive name in T-P2 dispatch | ACCEPT — 1A `:71` carries the census; 1D `:145` echoes with `research/p1/p1e-hot-leaf-attribution.md:105-148,232-234` anchor |

## Lock 14 Verification Re-run At HEAD (2026-05-23, commit `87816a2cd`)

All Lock 14 verification commands from `restart/locks/LOCKS.md:220` re-run cleanly at HEAD; one mechanical-extraction count diverges from the V2 cite by +1.

| Lock 14 verification command | Required result | Live result at HEAD `87816a2cd` | V2 citation | Verdict |
|---|---|---|---|---|
| `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d \| wc -l` | 0 | 9 (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) | 1C `:194,90`; 1F-coherence `:73`; 1F-past-corpora `:83` | ACCEPT — V2 cites 9 verbatim |
| `find crates/core/src/runtime/google_sheets -type f \| wc -l` | n/a (composition check) | 10 (V13 baseline 7 + 3 from `document/{mod,canonical,view,path_query}` sub-module) | 1C `:53,195`; 1F-anti-pattern `:40,76`; 1F-coherence `:73,92`; 1F-past-corpora `:50,83,120` | ACCEPT — V2 cites 10 across all five amended files |
| `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **30** | 1C `:92,124,199` | ACCEPT — V2 cites 30 verbatim; google_sheets/document/{mod,canonical}.rs:43,142 sites explicitly enumerated |
| `rg -l 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **15** | 1C `:92,124,199` | ACCEPT — V2 cites 15 verbatim; full file list reproduces |
| `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' crates/` | 0 | Lives in `skinny/crates/codegen/src/grammar_profile.rs:17-26` per 1B-D7/D13/AP-003/AP-012 — 8 `RuntimeProvider` variants + 7 CSS L4 match arms at `skinny/crates/codegen/src/lib.rs:167-209` | 1B `:48,60,90` enumerate verbatim | ACCEPT — 1B census matches live |
| Pattern H file census | 0 hand-written per-grammar runtime files | 67 hand-written (bbnf 8 + bnf 7 + css_l4 7 + css_pretty 7 + csv 7 + ebnf 7 + google_sheets 10 + json 7 + math 7); 0 generated markers (`rg -l '@generated\|AUTO-GENERATED\|THIS FILE IS GENERATED' crates/core/src/runtime/` returns empty) | 1C `:42-55,124,198`; 1F-anti-pattern `:40,76`; 1F-past-corpora `:50,83,120` | ACCEPT — V2 census matches live across all three amended files |
| Grammar-named module / type reexport census in generic-crate roots | 0 grammar-named modules / types in public APIs | 9 `pub mod <grammar>;` + **127 grammar-named reexports** at `crates/core/src/runtime/mod.rs:25-71` (mechanical Python extraction: 133 raw `pub use` entries inside the cited 25-71 window minus 6 cited neutrals **present inside the window** — `CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle, StructBuilder` — yields 127. The other 4 cited neutrals — `IntoPathSegment, Path, PathSegment, RuntimeView` — sit at `mod.rs:72,76`, OUTSIDE the 25-71 window) | 1C `:91,123` cite "126" via "subtracting the 10 grammar-neutral exports" | **REVISE — off by +1**: the 10-neutral subtraction list reaches outside the cited window. Correct count is 127 (subtract only the 6 present in 25-71) OR widen the window to 25-77 (which contains all 10 neutrals; 137 raw - 10 = 127). Either repair lands a consistent 127. The "47-line window" language is correct; only the neutral arithmetic needs the +1 fix. |

## Cross-Inventory Generality Coverage Matrix (V2 post-fold)

| Generality lens demand | 1A | 1B | 1C | 1D | 1E | 1F-anti | 1F-coh | 1F-past | Coverage |
|---|---|---|---|---|---|---|---|---|---|
| Lock 14 holds | DIV-008,LOCK1-AMEND-001 | D7,D8,D10,D13 | D1,D4,D6,D10 | rows 117,123,124,130 | LAC-08,12,15 | AP-003,012,016,020 | COH-005,008,011 | PC-006,008,017 | full |
| No JSON-only cataloguing of grammar-neutral facts | SUB-016,017,018 | D8,D10 | mod.rs cit | rows 100,113,131 | LAC-12 | AP-005 | COH-005 | PC-008 | full |
| 1C runtime census flags every grammar-named module | — | — | 1C-D1..D11 | row 130 | LAC-12 | AP-016 | COH-011 | PC-017 | full (1C primary) |
| 1D separates JSON-empirical from grammar-neutral | — | — | — | rows 117,118,123,124,131,134 | — | — | — | — | **full (V2-fold discharge of V1 partial)** — pass-layer leak row at 1D:124 explicitly distinct from codegen-layer leak row at 1D:123; CSS L4 layout-asymmetry row at 1D:134 |
| No grammar-name leak passes uncited | SUB-016,017,018,DIV-008 | D7,D8,D10,D13 | D1-D11 | rows 123,124,130,145 | LAC-08,12,15 | AP-012..016,020 | COH-005,011 | PC-008,017 | full |
| Pass-layer grammar-shape leaks (D8 + D10) carried as upstream Sheets/BBNF-self blockers distinct from codegen-layer name leaks | — | D8,D10 (4 tables) | — | row 124 | — | — | — | — | full (V2 micro-fold discharge) |

Coverage: full on all six lens demands. V2 micro-fold discharges the V1 partial on row 4 (1D matrix) and adds row 6 as a first-class lens demand.

## Required Revisions (single)

1. **1C reexport-count cite: 126 → 127 (off-by-one repair).** 1C rows at `restart/audit/totality/p1/1C-runtime-evidence.md:91,123` should be updated from "126 distinct grammar-named symbols" to "127 distinct grammar-named symbols" with a corrected neutral-subtraction phrasing. Two equivalent repairs:
   - **Repair A (preferred, preserves the 47-line window):** "127 distinct grammar-named symbols within `mod.rs:25-71` after subtracting the **6** grammar-neutral exports present inside the window (`CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle, StructBuilder`)."
   - **Repair B (widens the window):** "127 distinct grammar-named symbols within `mod.rs:25-77` (the full reexport block, which contains all **10** grammar-neutral exports including `IntoPathSegment, Path, PathSegment, RuntimeView` at `mod.rs:72,76`)."
   - Per-grammar breakdown becomes: bbnf 10, bnf 10, css_l4 **43**, css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10 (sum 127). The 1C cite's css_l4 estimate "~38 (+ 3 aliases)" is the source of the disagreement: live count of distinct css_l4 names (treating `Declaration as CssDeclaration`, `Selector as CssSelector`, plus the standalone `pub use css_l4::value::CssRule;` line as 3 named reexports beyond the multi-line block) is 43, not 41.
   - The repair-LOC band "47 lines hold 126 grammar-named symbols; ~190 LOC + 2.5× consumer-rewire band" rescales to "47 lines hold 127 grammar-named symbols; ~190 LOC + 2.5× consumer-rewire band" — no LOC-band change is required.
   - **Downstream propagation:** 1F-coherence COH-011 and 1F-past-corpora PC-017 do NOT quote the 126 figure (they cite file/dir counts only: 9 dirs, google_sheets=10, 67 hand-written), so the cite-correction is contained to 1C. No cross-file fold required.

## New Findings Surfaced

NEW-CH2-V2-01: The 1A-DIV-008 substrate-union two-cursor nuance disposition (1A `:84,113`) folds CH5-002 (`StructuralIndex` scanner), CH5-004 (CSS source-sidecar / AP-020), CH5-005 (Track 2 substrate-helper sharing), and CH5-007 (proof-witness generic-runtime exports) into a single T-P3 §3C ratification-vs-unification choice. This is correct CH2/CH5 cross-cutting consolidation: the four sub-cases are all Lock 1 union-vs-split sub-cases. The CH2 lens reads this as full CH2 coverage of the substrate-union question; T-P3 §3C must close before any CH2 row that depends on substrate-union shape can read "PROVED" rather than "PENDING".

NEW-CH2-V2-02: The 1B "Sheets / BBNF-Self Implications" section at `1B:69-73` is now the most precise statement of the upstream pass-layer blocker for non-JSON generality in the audit pack. The "TWO distinct pass-layer surfaces" + "NECESSARY-BUT-INSUFFICIENT relative to PRUNE-4" framing means PRUNE-4 (codegen-layer Lock 14 repair via the grammar-agnostic generator template) cannot, alone, deliver Sheets or BBNF-self admit rows. T-P3 §3C must order C-1 (PRUNE-3 + PRUNE-4) followed by a separate pass-layer wave that retires the D8 byte-whitelist + D10 role-literal predicates; the verify_action `rg -n 'b"\\{"|b"\\}"|b"\\["|b"\\]"|b":"|b"true"|b"false"|b"null"' skinny/crates/passes/src/lib.rs` must return ZERO post-redress, and a Sheets or BBNF-self role-mining fixture must round-trip clean.

NEW-CH2-V2-03: The mechanical-extraction defect class (V1 CH2 REVISE-7/8 underspecification; V2 CH2 REVISE-1 off-by-one) is a recurring CH2 risk: every count cite that takes the form "N grammar-named symbols extracted by subtracting K neutrals" must capture the neutral-list provenance (line-resolved against the cited window) before the subtraction. Recommend a CH6-style discipline rule for V3: any "N grammar-named X" cite that uses a subtraction-from-K must enumerate the K neutrals with line citations inside the cited window. This is a generalisation of the existing V1 CH6 captured-artefact discipline applied to mechanical-extraction arithmetic.

## V2 → V3 Trajectory

§3Z gate evaluation: V2 ACCEPT-rate 11/12 = 91.7%, single REVISE on the 1C reexport-count off-by-one (cite-only, no LOC-band change). Predicted V3 lands ACCEPT (≥95%) if the 1C 126→127 micro-correction is folded and the V3 dispatch re-runs the mechanical-extraction verification command against HEAD. No CH2 risk to LOCK trajectory; the count repair is local to 1C and has no propagation to other inventories.
