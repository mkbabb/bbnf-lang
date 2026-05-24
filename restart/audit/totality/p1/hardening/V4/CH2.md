---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V4
disposition: ACCEPT
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
v3_review_inputs:
  - restart/audit/totality/p1/hardening/V3/CH2.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md
lens_obligation: "PASS-1-EXCAVATION.md:110-114 — Lock 14 holds across inventories; no divergence catalogued as JSON-only when grammar-neutral substrate fact; 1C runtime census flags every grammar-named module in a generic crate; 1D separates JSON-empirical from grammar-neutral findings; no grammar-name leak passes uncited; pass-layer grammar-shape leaks (D8 recognizer-byte + D10 role-mining) must be carried as upstream Sheets/BBNF-self generalization blockers distinct from codegen-layer name leaks."
authority: restart/locks/LOCKS.md:220
head_commit_audited: 8f4756113a0332cc32414c9b0cbe95a3732d5e2c
verification_method: "live re-run of all Lock 14 verification commands at V4 HEAD against V4 amended 1A/1C/1D/1E/1F-anti-pattern citations; mechanical Python multi-line extraction of grammar-named symbols from crates/core/src/runtime/mod.rs:25-71 (133 raw pub-use entries inside the 25-71 window minus 6 in-window grammar-neutral exports — CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle, StructBuilder — yields 127 grammar-named; per-grammar bbnf=10, bnf=10, css_l4=43, css_pretty=10, csv=10, ebnf=10, google_sheets=11, json=13, math=10; sum 127); NEW-CH2-V3-02 orphan-cell propagation guard executed via `rg -n '\\b126\\b' restart/audit/totality/p1/1C-runtime-evidence.md` returning ONLY two preserved tokens — line 24 (historical V2-fold prose explicitly framing 'the V2 cycle cited 126 via subtract 10', required for V2-fold audit trail) and line 50 (css_l4 directory LOC sum '3,126', verified live by `find crates/core/src/runtime/css_l4 -type f -name '*.rs' -exec wc -l {} +` returning 3126 total); zero residual 126-as-grammar-named-reexport-count tokens remain; 1C executive summary at :40 now reads '127 grammar-named type reexports' verbatim discharging the V3 REVISE; 1B D8/D10 row distinctness across all 4 tables (`:50-51, :63-64, :71-73, :86-87`) holds at HEAD; 1D row 117 substrate-cardinality + row 123/124 codegen-vs-pass split + row 131 PC-008+U-PC-002 cross-cite + row 134 CSS L4 layout asymmetry hold at HEAD; AP-009 + AP-020 distinct rebound anchors at 1F-anti-pattern.md:55,80,105 hold; cross-inventory 126/127 scan over the 7 non-1C inventories returns zero grammar-named-reexport-count references — figure remains 1C-local"
v4_disposition_focus:
  - "V3 REVISE-1 discharge (F-V4-CH2-1): 1C:40 executive-summary single-token 126 → 127 propagation lands verbatim; live grep confirms zero residual grammar-named-reexport-count 126 tokens in 1C"
  - "NEW-CH2-V3-02 orphan-cell propagation guard (institutionalised in V4 CHALLENGE-CONTEXT §3): `rg -n '\\b126\\b' 1C-runtime-evidence.md` returns exactly 2 preserved tokens, both with documented justification (historical V2-fold framing at :24; unrelated css_l4 LOC sum at :50)"
  - "Live mechanical re-extraction at V4 HEAD reproduces 127 exactly with css_l4=43, google_sheets=11, sum 10+10+43+10+10+10+11+13+10=127"
  - "1B D8/D10 split holds at V4 HEAD (V3-LOCKED axis; zero drift expected and observed)"
  - "1D row 117 (substrate cardinality), row 123/124 (codegen-layer vs pass-layer leak split), row 131 (PC-008 + U-PC-002 cross-cite), row 134 (CSS L4 layout asymmetry) hold at V4 HEAD"
  - "AP-020 CSS source-sidecar comparator plane distinct from AP-009 at 1F-anti-pattern.md:55,80,105 (V3-rebound anchors carried forward)"
  - "Lock 14 verification cmds at V4 HEAD: 9 dirs, 10 google_sheets files, 30 matches across 15 files, 67 hand-written, zero @generated markers, 127 reexports across 47 lines"
verdict_summary:
  accept: 12
  revise: 0
  reject: 0
  accept_rate: "12/12 = 100%"
hard_cap_minutes: 25
---

## Verdict

ACCEPT. The V4 micro-fold (F-V4-CH2-1) discharges the V3 single residual REVISE cleanly: `1C-runtime-evidence.md:40` now reads "**127 grammar-named type reexports**" verbatim — the single-token executive-summary propagation lands at V4 HEAD `8f4756113` with zero collateral edits, zero LOC-band rescale, and zero downstream cross-inventory propagation needed (1F-coherence COH-011 and 1F-past-corpora PC-017 do not quote the figure; other inventories carry only file/dir counts). NEW-CH2-V3-02 orphan-cell propagation guard (institutionalised by V3 in the V4 dispatch context per CHALLENGE-CONTEXT §3) is satisfied with audit-grade rigor: `rg -n '\b126\b' restart/audit/totality/p1/1C-runtime-evidence.md` returns exactly **2** preserved tokens, both with on-the-record justification — (a) `1C:24` carries the historical V2-fold prose "the V2 cycle cited '126' via 'subtract 10 grammar-neutral exports'" which is required for the V2-fold audit trail of the off-by-one repair (a documentation cite of a prior cycle's defect, not a current count cite), and (b) `1C:50` carries the CSS L4 directory LOC sum "3,126" (verified live: `find crates/core/src/runtime/css_l4 -type f -name '*.rs' -exec wc -l {} +` returns `3126 total`), an unrelated structural integer that happens to contain the substring "126". Zero unjustified 126 tokens remain anywhere in 1C. Live mechanical re-extraction at V4 HEAD reproduces 127 bit-for-bit with the cited per-grammar arithmetic (`bbnf=10, bnf=10, css_l4=43, css_pretty=10, csv=10, ebnf=10, google_sheets=11, json=13, math=10; sum=127`) and the cited in-window neutral enumeration (`CompoundHandle :63, DtaError :58, GenericAtRule :42, ParseErr :58, StringHandle :63, StructBuilder :33` — 6 entries). The 4 out-window neutrals (`IntoPathSegment :72, Path :72, PathSegment :72, RuntimeView :76`) are correctly excluded per NEW-CH2-V2-03 discipline.

REJECT: none. No V4 inventory fails Lock 14 grammar-name fencing or mis-attributes JSON-only when grammar-neutral. All eight T-P1 inventories at V4 HEAD honor the V3-institutionalised in-window-vs-out-window neutral enumeration discipline.

ACCEPT-rate: **12 / 12 = 100%**. CH2 clears the LOCK-eligible cycle floor (≥95%) with first-ever per-lens 100% rate; the single V3 residual REVISE was the third consecutive count-cite defect class to be repaired at 1C and is now closed by the institutionalised orphan-cell propagation guard. V4 is the first cohort cycle where CH2 sub-axis converges to 12/12, making V5 the required second-consecutive ≥95% cycle for cohort §3Z LOCK.

## V3 → V4 Discharge Table

| V3 CH2 REVISE row | V3 finding | V4 fold (F-V4-CH2-1) | V4 disposition |
|---|---|---|---|
| V3-REVISE-1 (1C:40 executive-summary orphan 126 → 127) | V3 micro-fold updated all seven structural-row sites (`:21,23,24,92,124,162,201`) but did NOT propagate the 126→127 repair into the executive-summary prose at 1C:40; that one prose sentence still cited "126 grammar-named type reexports" | Single-token edit at `1C-runtime-evidence.md:40` replacing "126 grammar-named type reexports" with "127 grammar-named type reexports"; no LOC-band rescale; no downstream propagation needed (verified: no other inventory quotes the 126/127 figure as a grammar-named-reexport count) | **ACCEPT** — Live read at V4 HEAD `8f4756113`: 1C:40 reads "Runtime root `mod.rs` hand-wires 9 `pub mod <g>;` declarations + **127 grammar-named type reexports** across 47 lines at `mod.rs:25-71`". The exact V3-required edit landed verbatim. NEW-CH2-V3-02 orphan-cell propagation guard (`rg -n '\b126\b' 1C-runtime-evidence.md`) returns only 2 hits, both preserved-with-justification (V2-fold historical prose + unrelated css_l4 LOC sum). |

## V4 Verification Re-run At HEAD (2026-05-23, commit `8f4756113`)

### Lock 14 cmd matrix (live re-run at V4 HEAD)

| Lock 14 verification command | Required result | Live result at V4 HEAD `8f4756113` | V4 citation | Verdict |
|---|---|---|---|---|
| `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d \| wc -l` | 0 | 9 (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) | 1C `:21,28,91,195`; 1F-coherence `:73`; 1F-past-corpora `:83` | ACCEPT — V4 cites 9 verbatim; V3 cite-rebinds carry forward unchanged |
| `find crates/core/src/runtime/google_sheets -type f \| wc -l` | n/a (composition check) | 10 | 1C `:54,196`; 1F-anti-pattern `:40,76`; 1F-coherence `:73,92`; 1F-past-corpora `:50,83,120` | ACCEPT — V4 cites 10 across all five amended files; V4 micro-fold did not touch this surface |
| `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **30** | 1C `:23,93,125,200` | ACCEPT — V4 cites 30 verbatim; google_sheets/document/{mod,canonical}.rs:43,142 sites still explicitly enumerated |
| `rg -l 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **15** | 1C `:93,125,200` | ACCEPT — V4 cites 15 verbatim |
| Pattern H file census | 0 hand-written | 67 hand-written; 0 generated markers | 1C `:42-57,126,196-199`; 1F-anti-pattern `:40,76`; 1F-past-corpora `:50,83,120` | ACCEPT — V4 census matches live |
| Grammar-named reexport census in runtime/mod.rs | 0 grammar-named | 9 `pub mod <g>;` + **127 grammar-named reexports** at `crates/core/src/runtime/mod.rs:25-71` (mechanical multi-line Python extraction at V4 HEAD: 133 raw `pub use` entries inside the cited 25-71 window minus 6 in-window neutrals — `CompoundHandle`, `DtaError`, `GenericAtRule`, `ParseErr`, `StringHandle`, `StructBuilder` — yields **127**. Per-grammar: bbnf 10, bnf 10, css_l4 43, css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10. Sum 10+10+43+10+10+10+11+13+10 = 127. Out-window neutrals at `:72` (`IntoPathSegment, Path, PathSegment`) and `:76` (`RuntimeView`) excluded per NEW-CH2-V2-03.) | 1C `:23,24,40,92,124,162,201` (eight sites total at V4) cite "127" with full neutral-enumeration per NEW-CH2-V2-03 | ACCEPT — V4 fold discharges V3 REVISE-1 at the orphan executive-summary site; mechanical extraction reproduces 127 exactly across all eight cited sites including the freshly-folded `:40` |

### Mechanical extraction reproduction (V4 HEAD `8f4756113`)

```
Total pub use entries in lines 25-71: 133
In-window neutrals found: ['CompoundHandle', 'DtaError', 'GenericAtRule', 'ParseErr', 'StringHandle', 'StructBuilder']
In-window neutrals count: 6
Grammar-named (after neutral subtraction): 127
Per-grammar: {'bbnf': 10, 'bnf': 10, 'css_l4': 43, 'css_pretty': 10, 'csv': 10, 'ebnf': 10, 'google_sheets': 11, 'json': 13, 'math': 10}
Sum: 127
```

Out-window verification (lines 72-76 of `crates/core/src/runtime/mod.rs`):
```
pub use path::{IntoPathSegment, Path, PathSegment};

/// AZ-I.W2-act.close A.fix — re-export the grammar-agnostic
/// [`view::RuntimeView`] trait at the stable `crate::runtime` path.
pub use view::RuntimeView;
```
Four out-window neutrals confirmed at the exact lines the V4 inventory cites (`:72` × 3 + `:76`).

### NEW-CH2-V3-02 orphan-cell propagation guard (executable verification)

Command: `rg -n '\b126\b' restart/audit/totality/p1/1C-runtime-evidence.md`

Live output at V4 HEAD `8f4756113`:
```
24:    - V2-fold (F-V3-CH2-1 off-by-one repair): the V2 cycle cited "126" via "subtract 10 grammar-neutral exports", but 4 of those 10 (`IntoPathSegment, Path, PathSegment` at `mod.rs:72` + `RuntimeView` at `mod.rs:76`) sit OUTSIDE the 25-71 window. Correct count subtracting only the 6 in-window neutrals is **127**, not 126. […]
50:| `css_l4` | 7 | mod, arena, builder, document, parse_with, value, view | 3,126 | hand-written; builder.rs:1 "AZ-I.W2-act.B3 — CssStructBuilder" cutover-tagged | NO — same template miss |
```

Each preserved-token justification:

| Line | Token context | Justification class | Live verification |
|---|---|---|---|
| `1C:24` | "the V2 cycle cited '126' via 'subtract 10 grammar-neutral exports'" | **Historical V2-fold prose** — required for the V2-fold audit trail of the off-by-one repair; this is a quoted reference to a prior cycle's defect figure, not a current count cite. Deleting it would destroy the audit-history record of the F-V3-CH2-1 → F-V4-CH2-1 repair chain. | Read of 1C:24 confirms quote-mark framing around "126"; surrounding prose explicitly labels it as the V2 cycle's incorrect cite that V3 repaired. |
| `1C:50` | "css_l4 ... 3,126" (LOC sum for the css_l4 directory) | **Unrelated structural integer** — substring "126" happens to appear at the tail of the LOC sum 3126 for the css_l4 directory at HEAD. Wholly orthogonal to grammar-named-reexport counts. | `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/css_l4 -type f -name '*.rs' -exec wc -l {} +` → `3126 total` (parse_with 113 + mod 79 + arena 390 + value 852 + document 541 + builder 1014 + view 137 = 3126). |

Zero unjustified 126 tokens remain in 1C. NEW-CH2-V3-02 orphan-cell propagation guard SATISFIED for the F-V4-CH2-1 fold.

Cross-inventory 126/127 scan (`rg -n '\b12[67]\b' 1A 1B 1D 1E 1F-anti-pattern 1F-coherence 1F-past-corpora`): only two hits, both unrelated to grammar-named-reexport counts — `1F-anti-pattern.md:61` cites `restart/MASTER-PLAN.md:127` as a Lock 13 row authority (path:line reference, not a count); `1E-locks-evidence.md:35` cites `1E-locks-evidence.md:126-128` as the LAC-1E-12 promotion-candidacy line range. Neither is a grammar-named-reexport count. CH2 propagation is COMPLETE across the cohort.

## Cross-Inventory Generality Coverage Matrix (V4 post-fold)

| Generality lens demand | 1A | 1B | 1C | 1D | 1E | 1F-anti | 1F-coh | 1F-past | Coverage |
|---|---|---|---|---|---|---|---|---|---|
| Lock 14 holds | DIV-008,LOCK1-AMEND-001 | D7,D8,D10,D13 | D1,D4,D6,D10 | rows 117,123,124,130 | LAC-08,12,15 | AP-003,012,016,020 | COH-005,008,011 | PC-006,008,017 | full |
| No JSON-only cataloguing of grammar-neutral facts | SUB-014,016,017,018 | D8,D10 | mod.rs cit | rows 100,113,131 | LAC-12 | AP-005 | COH-005 | PC-008 | full |
| 1C runtime census flags every grammar-named module | — | — | 1C-D1..D11 | row 130 | LAC-12 | AP-016 | COH-011 | PC-017 | full (1C primary) |
| 1D separates JSON-empirical from grammar-neutral | — | — | — | rows 117,118,123,124,131,134 | — | — | — | — | full |
| No grammar-name leak passes uncited | SUB-014,016,017,018,DIV-008 | D7,D8,D10,D13 | D1-D11 | rows 123,124,130,145 | LAC-08,12,15 | AP-009,011,012..016,020 | COH-005,011 | PC-008,017 | full |
| Pass-layer grammar-shape leaks (D8 + D10) carried as upstream Sheets/BBNF-self blockers distinct from codegen-layer name leaks | — | D8,D10 (4 tables: `:50-51, :63-64, :71-73, :86-87`) | — | row 124 | — | — | — | — | full |
| Subtract-from-K neutral arithmetic carries in-window enumeration per NEW-CH2-V2-03 | — | — | mod.rs cit `:23,40,92,124,162,201` (eight sites total at V4) | — | — | — | — | — | full |
| Orphan-cell propagation guard per NEW-CH2-V3-02 (count-cite micro-folds must rg-verify justified-only before commit) | — | — | F-V4-CH2-1 fold satisfies | — | — | — | — | — | full (V4 micro-fold institutionalises) |

Coverage: full on all eight lens demands at V4. Row 8 (NEW-CH2-V3-02 orphan-cell propagation guard) added as a first-class lens demand by V4 (institutionalisation of V3's recommended discipline check via the V4 dispatch context per CHALLENGE-CONTEXT §3).

## Required Revisions

**None.** V4 CH2 clears with 12/12 = 100% ACCEPT-rate. The V3 single residual REVISE (1C:40 executive-summary orphan 126 → 127) is discharged verbatim; the NEW-CH2-V3-02 orphan-cell propagation guard is satisfied for the F-V4-CH2-1 fold with live grep evidence of zero unjustified 126 tokens; all V3-LOCKED rows hold at V4 HEAD with zero drift.

## New Findings Surfaced

NEW-CH2-V4-01: V4 institutionalises NEW-CH2-V3-02 (orphan-cell propagation guard) at the dispatch-context level: the V4 CHALLENGE-CONTEXT §3 mandates "every cite must be re-verified at V4 HEAD before ACCEPT" and embeds NEW-CH2-V3-02 as a load-bearing pre-commit discipline. The F-V4-CH2-1 fold-author captured the `rg -n '\b126\b' 1C` pre/post evidence per LAC-1E-12 procedural addendum, and the V4 ACCEPT verification chains that evidence into a justification table for each preserved token. Recommend a V5 CH2 discipline check (NEW-CH2-V4-01): any future count-cite repair across the cohort must produce a justification table for any preserved-old-value tokens (historical prose, unrelated structural integers) before the cycle closes; pure `rg → empty` is acceptable for surfaces with zero historical-prose or LOC-coincidence concerns, but `rg → non-empty with documented justification` becomes the new audit standard. The repair lineage `V1: 60+ → V2: 126 → V3: 127-in-structural-rows-but-126-orphan-in-summary → V4: 127-all-sites-with-justified-preserved-tokens` is the canonical exemplar.

NEW-CH2-V4-02: The cross-inventory 126/127 scan (`rg -n '\b12[67]\b'` across the 7 non-1C inventories) returns zero grammar-named-reexport-count references — the figure remains 1C-local. This confirms the V3-predicted "no downstream propagation needed" for the F-V4-CH2-1 fold and validates the §3Z-relevant claim that CH2's count-cite repair surface is contained. V5 may LOCK CH2 in confidence that the count-cite class of defects is fully closed at the cohort level.

NEW-CH2-V4-03: V4's CH2 sub-axis convergence (12/12 = 100%) is the first per-lens 100% ACCEPT-rate across the T-P1 hardening corpus (V1 60%, V2 91.7%, V3 91.7%, V4 100%). The trajectory matches the V3 CH2 prediction at V3:112 ("Predicted V4 lands ACCEPT (≥95%) for CH2 IF the V4 dispatch packet includes the single 1C:40 '126 → 127' propagation repair AND adds the NEW-CH2-V3-02 orphan-cell propagation guard"). Both V3-required conditions were met; the predicted convergence landed exactly. V5 confirming required for cohort §3Z LOCK (second consecutive ≥95% cycle).

## V4 → V5 Trajectory

§3Z gate evaluation: V4 ACCEPT-rate 12/12 = 100%, zero REVISE, zero REJECT. V4 is the **first cohort cycle where CH2 itself clears ≥95% as a per-lens floor** (V1: 60%, V2: 91.7%, V3: 91.7%, V4: 100%). The two-consecutive-cycle LOCK rule (§3Z) requires CH2 to clear ≥95% in both V4 AND V5. **Predicted V5 lands ACCEPT (≥95%) for CH2** assuming (a) no V4 → V5 micro-fold introduces a new count cite at 1C without pre-commit `rg → justified-only` verification per NEW-CH2-V3-02 + NEW-CH2-V4-01, (b) no other lens's V5 fold touches the runtime/mod.rs:25-71 surface in a way that triggers a CH2 cascade, and (c) all V3/V4-LOCKED rows (1B D8/D10, 1D rows 117/123/124/131/134, AP-009 + AP-020 rebound anchors) continue to hold at V5 HEAD. Cohort-wide trajectory: V5 is the required second-consecutive ≥95% cycle for cohort §3Z LOCK at the V5 ceiling.

No CH2 risk to V5 LOCK trajectory. The count-cite class of defects that drove the V1/V2/V3 CH2 REVISEs is now fully closed by the F-V4-CH2-1 repair + NEW-CH2-V3-02 + NEW-CH2-V4-01 discipline chain. V5 confirming run expected to maintain 12/12 = 100% in the absence of new fold-introduced count cites.
