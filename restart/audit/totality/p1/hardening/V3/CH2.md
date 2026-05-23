---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V3
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
v2_review_inputs:
  - restart/audit/totality/p1/hardening/V2/CH2.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V2-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md
lens_obligation: "PASS-1-EXCAVATION.md:110-114 — Lock 14 holds across inventories; no divergence catalogued as JSON-only when grammar-neutral substrate fact; 1C runtime census flags every grammar-named module in a generic crate; 1D separates JSON-empirical from grammar-neutral findings; no grammar-name leak passes uncited; pass-layer grammar-shape leaks (D8 recognizer-byte + D10 role-mining) must be carried as upstream Sheets/BBNF-self generalization blockers distinct from codegen-layer name leaks."
authority: restart/locks/LOCKS.md:220
head_commit_audited: 0a9f1288c62ef9f507854e8ccfebcfc78ba0a322
verification_method: "live re-run of all Lock 14 verification commands at V3 HEAD against V3 amended 1B/1C/1D/1F citations; mechanical Python extraction of grammar-named symbols from crates/core/src/runtime/mod.rs:25-71 (133 raw pub-use entries inside the 25-71 window minus 6 in-window grammar-neutral exports → 127 grammar-named, sum 10+10+43+10+10+10+11+13+10=127); rg census of parser-name leak (30 sites across 15 files); 1B D8/D10 row distinctness across spec-claim table, generic-crate census, Sheets/BBNF-self implications, and divergences-catalogued tables; 1D row 117 split verification; PC-008 + U-PC-002 cross-cite presence in 1D:131; 1A-DIV-008 two-cursor substrate-union nuance disposition presence; AP-020 CSS source-sidecar new row presence + AP-009 distinction; NEW-CH2-V2-03 discipline rule (in-window vs out-window neutral enumeration) verification at 1C:201; Lock 14 holds across all amended inventories"
v3_disposition_focus:
  - "V2 REVISE-1 discharge (F-V3-CH2-1): 1C reexport-count cite 126→127 across all 1C rows (`:21,23,24,92,124,162,201`); css_l4 ~41→43; google_sheets 12→11; sum verification 10+10+43+10+10+10+11+13+10=127 explicit at 1C:201"
  - "NEW-CH2-V2-03 discipline rule (in-window vs out-window neutral enumeration with path:line) — load-bearing for V4 LOCK"
  - "1B D8/D10 split holds at V3 HEAD (V2-LOCKED axis; zero drift expected)"
  - "1D row 117 (substrate cardinality), row 123/124 (codegen-layer vs pass-layer leak split), row 131 (PC-008 + U-PC-002 cross-cite), row 134 (CSS L4 layout asymmetry) hold at V3 HEAD"
  - "AP-020 CSS source-sidecar comparator plane distinct from AP-009 at 1F-anti-pattern.md:55,80,105 (V3-rebound anchors)"
  - "Lock 14 holds across all 8 amended inventories"
verdict_summary:
  accept: 11
  revise: 1
  reject: 0
  accept_rate: "11/12 = 91.7%"
hard_cap_minutes: 25
---

## Verdict

REVISE. The V3 micro-fold discharges the V2 CH2 REVISE-1 off-by-one (126→127) across **seven** 1C citation sites (`:21,23,24,92,124,162,201`), institutionalises NEW-CH2-V2-03 (every "N grammar-named X minus K neutrals" cite must enumerate the K neutrals path:line inside the cited window), and the per-grammar breakdown rescaled correctly (css_l4 41→43; google_sheets 12→11). The sum-verification line `10+10+43+10+10+10+11+13+10 = 127` is now explicit at 1C:201 with both the in-window neutral enumeration (`StructBuilder:33`, `GenericAtRule:42`, `DtaError:58`, `ParseErr:58`, `CompoundHandle:63`, `StringHandle:63` — six entries) AND the out-window neutral enumeration (`IntoPathSegment:72`, `Path:72`, `PathSegment:72`, `RuntimeView:76` — four entries excluded). Live mechanical re-extraction at V3 HEAD (commit `0a9f1288c`) reproduces every count bit-for-bit. **One residual REVISE fires**: the 1C Executive Summary at `1C-runtime-evidence.md:40` still cites "**126 grammar-named type reexports**" — the V3 micro-fold updated the structural rows (folded sections, leak audit, divergences, verification) but did NOT propagate the 126→127 repair into the executive-summary prose. This is the same defect class as V2 CH2 REVISE-1 (incomplete arithmetic propagation) and is the third consecutive cycle in which a count cite at 1C requires a mechanical-extraction repair, but its scope is strictly orphan-cell containment (one sentence of executive-summary prose; zero downstream propagation; no other inventory quotes the 126/127 figure — only 1C does). Every other V3 fold item ACCEPTs: D8/D10 row split holds at HEAD across all four 1B tables (`1B:50-51, 63-64, 71-73, 86-87`); 1D row 123 (codegen-layer) + row 124 (pass-layer) split with explicit Sheets/BBNF-self generalization-blocker framing; PC-008 + U-PC-002 cross-cite at 1D:131 verbatim; AP-020 distinct from AP-009 at 1F-anti-pattern.md (V3-rebound anchors per V3 CHALLENGE-CONTEXT `:55,80,105`); 1A-DIV-008 substrate-union nuance disposition at 1A:84,113 holds; Lock 14 verifications all return the live counts the V3 amended inventories cite (9 dirs, 10 google_sheets files, 30 matches across 15 files, 67 hand-written, zero `@generated` markers, 127 reexports across 47 lines).

REJECT: none. No V3 inventory fails Lock 14 grammar-name fencing or mis-attributes JSON-only when grammar-neutral.

ACCEPT-rate: 11 / 12 = 91.7%. CH2 fires a single residual REVISE on the executive-summary orphan cell (1C:40 still cites "126" — local-orphan propagation defect, contained to one prose sentence of 1C).

## V2 → V3 Discharge Table

| V2 CH2 REVISE row | V2 finding | V3 fold | V3 disposition |
|---|---|---|---|
| V2-REVISE-1 (1C 126→127 off-by-one) | V2 cited "126 grammar-named symbols" via "subtract 10 grammar-neutral exports" but only 6 of those 10 sit INSIDE the cited 25-71 window; live mechanical extraction returns 127, not 126 | 1C `:21,23,24,92,124,162,201` all read "**127 distinct grammar-named symbols**" with neutral-arithmetic corrected to "133 raw `pub use` entries minus the 6 in-window neutrals (`StructBuilder:33`, `GenericAtRule:42`, `DtaError:58`, `ParseErr:58`, `CompoundHandle:63`, `StringHandle:63`)"; 4 out-window neutrals (`IntoPathSegment, Path, PathSegment` at `:72` + `RuntimeView` at `:76`) explicitly enumerated as excluded; per-grammar: bbnf 10, bnf 10, css_l4 **43**, css_pretty 10, csv 10, ebnf 10, google_sheets **11**, json 13, math 10; sum verification `10+10+43+10+10+10+11+13+10 = 127` present at 1C:201; NEW-CH2-V2-03 discipline rule applied | **REVISE-PARTIAL** — structural rows discharge cleanly at all seven cited sites; live re-extraction at V3 HEAD `0a9f1288c` reproduces 127 with the cited arithmetic. **Residual orphan cell**: 1C-runtime-evidence.md:40 (executive summary) still cites "**126 grammar-named type reexports**". The V3 fold updated the leak audit, the divergences table, the verification section, and the folded V1/V2 references but did NOT propagate the repair into the executive-summary prose. Repair: change 1C:40 from "126 grammar-named type reexports" to "127 grammar-named type reexports" (single-cell single-token edit; no LOC-band rescale; no downstream propagation — 1F-coherence COH-011 and 1F-past-corpora PC-017 do not quote the figure). |

## V3 Verification Re-run At HEAD (2026-05-23, commit `0a9f1288c`)

All Lock 14 verification commands from `restart/locks/LOCKS.md:220` re-run cleanly at V3 HEAD; mechanical extraction reproduces 127 bit-for-bit.

| Lock 14 verification command | Required result | Live result at V3 HEAD `0a9f1288c` | V3 citation | Verdict |
|---|---|---|---|---|
| `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d \| wc -l` | 0 | 9 (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) | 1C `:21,28,91,195`; 1F-coherence `:73`; 1F-past-corpora `:83` | ACCEPT — V3 cites 9 verbatim |
| `find crates/core/src/runtime/google_sheets -type f \| wc -l` | n/a (composition check) | 10 | 1C `:54,196`; 1F-anti-pattern `:40,76`; 1F-coherence `:73,92`; 1F-past-corpora `:50,83,120` | ACCEPT — V3 cites 10 across all five amended files |
| `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **30** | 1C `:23,93,125,200` | ACCEPT — V3 cites 30 verbatim; google_sheets/document/{mod,canonical}.rs:43,142 sites explicitly enumerated |
| `rg -l 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **15** | 1C `:93,125,200` | ACCEPT — V3 cites 15 verbatim |
| Pattern H file census | 0 hand-written | 67 hand-written; 0 generated markers | 1C `:42-57,126,196-199`; 1F-anti-pattern `:40,76`; 1F-past-corpora `:50,83,120` | ACCEPT — V3 census matches live |
| Grammar-named reexport census in runtime/mod.rs | 0 grammar-named | 9 `pub mod <g>;` + **127 grammar-named reexports** at `crates/core/src/runtime/mod.rs:25-71` (mechanical Python extraction at V3 HEAD: 133 raw `pub use` entries inside the cited 25-71 window minus 6 in-window neutrals — `CompoundHandle`, `DtaError`, `GenericAtRule`, `ParseErr`, `StringHandle`, `StructBuilder` — yields **127**. Per-grammar: bbnf 10, bnf 10, css_l4 43, css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10. Sum 10+10+43+10+10+10+11+13+10 = 127. Out-window neutrals at `:72` (`IntoPathSegment, Path, PathSegment`) and `:76` (`RuntimeView`) excluded.) | 1C `:23,24,92,124,162,201` cite "127" with full neutral-enumeration per NEW-CH2-V2-03 | ACCEPT for the seven structural rows — V3 fold discharges V2 REVISE-1 at every cited site; mechanical extraction reproduces 127 exactly. **REVISE for the orphan executive-summary cell at 1C:40** — still cites "126 grammar-named type reexports"; single-token repair contained to that prose sentence. |

### Mechanical extraction reproduction (V3 HEAD `0a9f1288c`)

```
Total pub use entries in lines 25-71: 133
In-window neutrals found: ['CompoundHandle', 'DtaError', 'GenericAtRule', 'ParseErr', 'StringHandle', 'StructBuilder']
In-window neutrals count: 6
Grammar-named (after neutral subtraction): 127
Per-grammar: {'bbnf': 10, 'bnf': 10, 'css_l4': 43, 'css_pretty': 10, 'csv': 10, 'ebnf': 10, 'google_sheets': 11, 'json': 13, 'math': 10}
Sum: 127
```

Out-window verification (lines 72-80):
```
pub use path::{IntoPathSegment, Path, PathSegment};
pub use view::RuntimeView;
```
Four out-window neutrals confirmed at the exact lines the V3 fold cites (`:72` × 3 + `:76`).

## Cross-Inventory Generality Coverage Matrix (V3 post-fold)

| Generality lens demand | 1A | 1B | 1C | 1D | 1E | 1F-anti | 1F-coh | 1F-past | Coverage |
|---|---|---|---|---|---|---|---|---|---|
| Lock 14 holds | DIV-008,LOCK1-AMEND-001 | D7,D8,D10,D13 | D1,D4,D6,D10 | rows 117,123,124,130 | LAC-08,12,15 | AP-003,012,016,020 | COH-005,008,011 | PC-006,008,017 | full |
| No JSON-only cataloguing of grammar-neutral facts | SUB-014,016,017,018 | D8,D10 | mod.rs cit | rows 100,113,131 | LAC-12 | AP-005 | COH-005 | PC-008 | full |
| 1C runtime census flags every grammar-named module | — | — | 1C-D1..D11 | row 130 | LAC-12 | AP-016 | COH-011 | PC-017 | full (1C primary) |
| 1D separates JSON-empirical from grammar-neutral | — | — | — | rows 117,118,123,124,131,134 | — | — | — | — | full |
| No grammar-name leak passes uncited | SUB-014,016,017,018,DIV-008 | D7,D8,D10,D13 | D1-D11 | rows 123,124,130,145 | LAC-08,12,15 | AP-009,011,012..016,020 | COH-005,011 | PC-008,017 | full |
| Pass-layer grammar-shape leaks (D8 + D10) carried as upstream Sheets/BBNF-self blockers distinct from codegen-layer name leaks | — | D8,D10 (4 tables) | — | row 124 | — | — | — | — | full |
| Subtract-from-K neutral arithmetic carries in-window enumeration per NEW-CH2-V2-03 | — | — | mod.rs cit `:23,201` | — | — | — | — | — | full (V3 micro-fold institutionalises) |

Coverage: full on all seven lens demands at V3. Row 7 added as a first-class lens demand by V3 (institutionalisation of NEW-CH2-V2-03).

## Required Revisions (single residual)

1. **1C executive-summary orphan cell: 126 → 127 (single-token propagation repair).** `restart/audit/totality/p1/1C-runtime-evidence.md:40` reads "Runtime root `mod.rs` hand-wires 9 `pub mod <g>;` declarations + **126 grammar-named type reexports** across 47 lines at `mod.rs:25-71`". Required edit: replace "**126 grammar-named type reexports**" with "**127 grammar-named type reexports**". The V3 fold updated the structural rows (V1-fold/V2-fold reference rows at `:21,23,24`; spec-claim table at `:92`; leak audit at `:124`; divergences at `:162`; verification section at `:201`) but did NOT touch the executive-summary prose. Single-cell single-token edit; no LOC-band rescale; no downstream propagation (1F-coherence COH-011 and 1F-past-corpora PC-017 do not quote the figure; other inventories carry only file/dir counts).

## New Findings Surfaced

NEW-CH2-V3-01: The V3 fold institutionalised NEW-CH2-V2-03 (in-window vs out-window neutral enumeration with path:line) at 1C:201 successfully — every "N grammar-named X minus K neutrals" cite in V3 1C carries the in-window neutral list with path:line (`:33,:42,:58,:63`) AND the out-window neutral list with path:line (`:72,:76`). This is load-bearing for V4 LOCK: future mechanical-extraction cite revisions must follow the same discipline. The 1C V3 sum-verification line `10+10+43+10+10+10+11+13+10 = 127` is the canonical exemplar of NEW-CH2-V2-03 compliance.

NEW-CH2-V3-02: A recurring failure mode is now visible across V1/V2/V3 CH2 dispositions: each V→V+1 cycle has surfaced a count-cite defect at 1C of decreasing magnitude (V1: 60+ → 126 floor-vs-floor; V2: 126 → 127 off-by-one; V3: 127 in structural rows but 126 orphan in executive summary). Recommend a V4 CH2 discipline check: any micro-fold that touches a count cite must include a `grep -n '<old-value>' <file>` verification step in the dispatch context, returning empty before commit. This generalises NEW-CH2-V2-03 from neutral-list provenance to count-cite propagation completeness — call this NEW-CH2-V3-02 ("orphan-cell propagation guard"): no count-cite micro-fold lands until `rg -n '<old-figure>'` returns empty across the amended file.

NEW-CH2-V3-03: V3's correction discipline (line-by-line enumeration of in-window vs out-window neutrals) has retroactive coverage: V2's "126" cite, V3's "127" cite, and the V4 repair (127 propagated to 1C:40) all map to the SAME live count from the SAME mechanical extraction. The repair history is now self-consistent — no further mechanical-extraction adjudication is required for the runtime/mod.rs:25-71 surface. V4 should LOCK this row once the orphan-cell repair lands.

## V3 → V4 Trajectory

§3Z gate evaluation: V3 ACCEPT-rate 11/12 = 91.7%, single residual REVISE on the 1C executive-summary orphan cell (single-token cite-only repair, zero LOC-band change, zero cross-inventory propagation). V3 is the **first cohort-wide ≥95% cycle** per the V3 CHALLENGE-CONTEXT framing, but CH2 itself remains at 91.7% — the same per-lens rate as V2. The two-consecutive-cycle LOCK rule (§3Z) requires CH2 to also clear ≥95% twice in a row; the V3 CH2 sub-axis rate (11/12) re-fires the V2 sub-axis rate. **Predicted V4 lands ACCEPT (≥95%) for CH2** IF the V4 dispatch packet includes the single 1C:40 "126 → 127" propagation repair AND adds the NEW-CH2-V3-02 orphan-cell propagation guard ("grep verify <old-figure> returns empty before commit") to the dispatch discipline. With both repairs landed, CH2 sub-axis converges 12/12 = 100% and the V5 LOCK cycle becomes the second-consecutive ≥95%. Cohort-wide trajectory is unchanged: V3 is first ≥95% cycle if other lenses clear; V4 must hold ≥95% across the full lens cohort for §3Z cohort LOCK.

No CH2 risk to LOCK trajectory beyond the single orphan-cell repair; cite-correction is local to one prose sentence in 1C and has zero propagation to other inventories.
