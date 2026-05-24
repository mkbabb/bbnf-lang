---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V5
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
v4_review_inputs:
  - restart/audit/totality/p1/hardening/V4/CH2.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md
lens_obligation: "PASS-1-EXCAVATION.md:110-114 — Lock 14 holds across inventories; no divergence catalogued as JSON-only when grammar-neutral substrate fact; 1C runtime census flags every grammar-named module in a generic crate; 1D separates JSON-empirical from grammar-neutral findings; no grammar-name leak passes uncited; pass-layer grammar-shape leaks (D8 recognizer-byte + D10 role-mining) must be carried as upstream Sheets/BBNF-self generalization blockers distinct from codegen-layer name leaks."
authority: restart/locks/LOCKS.md:220
head_commit_audited: 9833295d5a295938019de54af2411c24e386530e
verification_method: "live re-run of every Lock 14 verification command + cite at V5 HEAD `9833295d5`; the V5 cycle is a verification-pass for the V4-LOCKED 7 inventories (1A/1B/1C/1D/1F-coherence/1F-anti-pattern/1F-past-corpora — all unchanged at V5) + a single-cell cosmetic anchor-refresh verification on 1E:35 (`:126-128` → `:128-130`); mechanical multi-line Python extraction over `crates/core/src/runtime/mod.rs:25-71` confirms 133 raw `pub use` entries minus 6 in-window neutrals (`CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle, StructBuilder`) yields **127** grammar-named with per-grammar bbnf=10, bnf=10, css_l4=43, css_pretty=10, csv=10, ebnf=10, google_sheets=11, json=13, math=10 (sum 127); NEW-CH2-V3-02 orphan-cell propagation guard executed via `rg -n '\\b126\\b' restart/audit/totality/p1/1C-runtime-evidence.md` returns ONLY 2 preserved tokens (1C:24 historical V2-fold prose + 1C:50 css_l4 LOC sum 3,126); css_l4 LOC sum reconfirmed live via `find crates/core/src/runtime/css_l4 -type f -name '*.rs' -exec wc -l {} +` = `3126 total` (parse_with 113 + mod 79 + arena 390 + value 852 + document 541 + builder 1014 + view 137 = 3126); cross-inventory `rg -n '\\b12[67]\\b'` over 7 non-1C inventories returns only `1F-anti-pattern.md:61` (MASTER-PLAN.md path-line ref, not a count cite) — figure remains 1C-local; 1B D8/D10 row distinctness across all 4 tables (`:50-51, :63-64, :71-73, :86-87`) holds at V5 HEAD; 1D row 117 substrate-cardinality + row 123/124 codegen-vs-pass split + row 131 PC-008+U-PC-002 cross-cite + row 134 CSS L4 layout asymmetry hold at V5 HEAD; AP-009 + AP-020 distinct rebound anchors at 1F-anti-pattern.md:55,80,105 hold; F-V5-CH6-1 1E:35 self-cite `:128-130` refresh verified live: 1E:128 reads `### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)` heading, 1E:130 reads the LAC-1E-12 promotion-candidacy body prose."
v5_disposition_focus:
  - "V4 fold (F-V4-CH2-1) V4-LOCKED at V5 with zero edits to 1C (the only file V4 amended for CH2); 1C:40 still reads `127 grammar-named type reexports` verbatim; 1C row sites `:21,23,24,92,124,162,201` carry 127 verbatim"
  - "NEW-CH2-V3-02 orphan-cell propagation guard (institutionalised V3, re-affirmed V4, MAINTAINED V5): `rg -n '\\b126\\b' 1C-runtime-evidence.md` returns exactly 2 preserved-with-justification tokens; zero unjustified 126 tokens remain at V5 HEAD"
  - "Live mechanical re-extraction at V5 HEAD `9833295d5` reproduces 127 exactly with css_l4=43, google_sheets=11, sum 10+10+43+10+10+10+11+13+10=127 (identical to V4)"
  - "1B D8/D10 split holds at V5 HEAD (V3-LOCKED axis carried through V4 → V5 with zero drift)"
  - "1D row 117 (substrate cardinality), row 123/124 (codegen-layer vs pass-layer leak split), row 131 (PC-008 + U-PC-002 cross-cite), row 134 (CSS L4 layout asymmetry) hold at V5 HEAD"
  - "AP-020 CSS source-sidecar comparator plane distinct from AP-009 at 1F-anti-pattern.md:55,80,105 (V3-rebound anchors carried forward through V4 → V5)"
  - "Lock 14 cmd matrix at V5 HEAD: 9 dirs, 10 google_sheets files, 30 matches across 15 files, 67 hand-written, zero @generated markers, 127 reexports across 47 lines (identical to V4)"
  - "F-V5-CH6-1 1E:35 anchor refresh confirmed: V4 paragraph insertion at 1E:35 itself (the sustained-UNKNOWN posture paragraph) shifted the LAC-1E-12 promotion-candidacy block by 2 lines (V4: heading at :126, body at :128 — V5: heading at :128, body at :130); 1E:35 self-cite now correctly reads `1E-locks-evidence.md:128-130` discharging the V4 non-blocking CH6 cosmetic"
  - "NEW-CH2-V4-01 (V4-institutionalised justification-table discipline for any preserved-old-value tokens after a count-cite repair): satisfied for the F-V5-CH6-1 fold under CH2 reading — the V5 fold is an anchor refresh (not a count-cite repair), so no new justification table is required; pre-existing NEW-CH2-V3-02 + NEW-CH2-V4-01 chain carries forward unbroken"
verdict_summary:
  accept: 12
  revise: 0
  reject: 0
  accept_rate: "12/12 = 100%"
hard_cap_minutes: 20
---

## Verdict

ACCEPT. V5 is the **LOCK-trigger verification-pass cycle** for T-P1, and CH2 sub-axis maintains the V4 per-lens 100% ACCEPT-rate verbatim. The V5 atomic cosmetic fold (commit `9833295d5`) touches exactly one cell — the 1E:35 sustained-UNKNOWN paragraph's tail self-cite `1E-locks-evidence.md:126-128` → `1E-locks-evidence.md:128-130` — discharging F-V5-CH6-1 with a 1-line diff (`+1/-1`). The fold's mechanism is structural: the V4 fold itself inserted/refactored the sustained-UNKNOWN paragraph at 1E:35 (the paragraph that now CARRIES the self-cite), and that paragraph insertion shifted the LAC-1E-12 promotion-candidacy block down by exactly 2 lines (V4: `### LAC-1E-12 promotion candidacy` heading at 1E:126 + body prose at 1E:128 → V5 confirms heading now at 1E:128 + body at 1E:130). The V5 cosmetic refresh re-anchors the 1E:35 self-cite to the post-shift location with zero collateral edits. **For CH2 GENERALITY's purposes this is a NO-OP**: the 1E:35 paragraph is the sustained-UNKNOWN anti-paper-close anchor (CH6 surface), not a Lock-14-bearing cell; the LAC-1E-12 promotion-candidacy block at the new 1E:128 + 1E:130 is governance prose (CH7 binding surface), not a grammar-named-leak count cite; and the cosmetic refresh introduces zero new count cites anywhere across the cohort. The CH2 verification surface (127 grammar-named reexports + Lock 14 cmd matrix + 1B D8/D10 split + 1D row 117/123/124/131/134 + AP-009 + AP-020 distinct anchors) holds bit-for-bit from V4.

NEW-CH2-V3-02 orphan-cell propagation guard (the V3-introduced + V4-institutionalised + V5-MAINTAINED pre-commit discipline): `rg -n '\b126\b' restart/audit/totality/p1/1C-runtime-evidence.md` at V5 HEAD `9833295d5` returns exactly **2** preserved-with-justification tokens, byte-identical to V4 — (a) 1C:24 carries the historical V2-fold prose "the V2 cycle cited '126' via 'subtract 10 grammar-neutral exports'" (audit-trail of the F-V3-CH2-1 → F-V4-CH2-1 → V5-LOCKED repair chain; not a current count cite), and (b) 1C:50 carries the css_l4 directory LOC sum "3,126" (verified live at V5 HEAD: `find crates/core/src/runtime/css_l4 -type f -name '*.rs' -exec wc -l {} +` returns `3126 total` summed from parse_with=113 + mod=79 + arena=390 + value=852 + document=541 + builder=1014 + view=137 = 3126 exactly). Zero unjustified 126 tokens remain in 1C. Cross-inventory `rg -n '\b12[67]\b'` over the 7 non-1C inventories at V5 HEAD returns only `1F-anti-pattern.md:61` (a `MASTER-PLAN.md:127` path:line authority reference, not a count) — CH2 count-cite repair surface remains 1C-local through V5 (validating the V4-predicted "no downstream propagation needed" claim across two consecutive cycles).

Live mechanical re-extraction at V5 HEAD `9833295d5` reproduces 127 bit-for-bit with the cited per-grammar arithmetic (`bbnf=10, bnf=10, css_l4=43, css_pretty=10, csv=10, ebnf=10, google_sheets=11, json=13, math=10; sum=127`) and the cited in-window neutral enumeration (`CompoundHandle :63, DtaError :58, GenericAtRule :42, ParseErr :58, StringHandle :63, StructBuilder :33` — 6 entries). The 4 out-window neutrals (`IntoPathSegment :72, Path :72, PathSegment :72, RuntimeView :76`) are correctly excluded per NEW-CH2-V2-03 discipline (re-verified at V5 HEAD: `sed -n '72,76p' crates/core/src/runtime/mod.rs` returns `pub use path::{IntoPathSegment, Path, PathSegment};` + `pub use view::RuntimeView;`).

REJECT: none. No V5 inventory fails Lock 14 grammar-name fencing or mis-attributes JSON-only when grammar-neutral. All eight T-P1 inventories at V5 HEAD honor the V3-institutionalised in-window-vs-out-window neutral enumeration discipline (NEW-CH2-V2-03) and the V4-institutionalised orphan-cell propagation guard (NEW-CH2-V3-02).

ACCEPT-rate: **12 / 12 = 100%**. CH2 clears the LOCK-trigger cycle floor (≥95%) with the **second consecutive per-lens 100% rate** — V4 was first per-lens 100% in the T-P1 corpus; V5 is the second consecutive confirming run. The CH2 trajectory across V1 → V5 (V1: 60% → V2: 91.7% → V3: 91.7% → V4: 100% → V5: 100%) satisfies the cohort §3Z two-consecutive-cycle LOCK rule for the CH2 sub-axis. **COHORT §3Z LOCK ENABLEMENT (CH2 contribution): CONFIRMED.**

## V4 → V5 Carry-Forward Table

| V4 CH2 ACCEPT row | V4 evidence | V5 state at HEAD `9833295d5` | V5 disposition |
|---|---|---|---|
| F-V4-CH2-1 (1C:40 executive-summary `126 → 127` propagation) | V4 single-token edit at 1C:40; live read returned `**127 grammar-named type reexports**` | V5 1C unchanged (V4-LOCKED axis); 1C:40 still reads `127 grammar-named type reexports` verbatim — `rg -n '127 grammar-named' 1C-runtime-evidence.md` returns `:21, :40, :124` (1C cite roster intact) | **ACCEPT-CARRY** — V4 fold holds at V5 HEAD without re-edit |
| NEW-CH2-V3-02 orphan-cell propagation guard | V4: `rg -n '\b126\b' 1C` returned 2 hits (justified: V2-fold prose at :24 + css_l4 LOC at :50) | V5: same command returns same 2 hits at same lines; byte-identical | **ACCEPT-CARRY** — guard discipline holds at V5; no V5 edit introduces new 126 tokens |
| Mechanical re-extraction reproduces 127 | V4: 133 raw entries − 6 in-window neutrals = 127; per-grammar 10/10/43/10/10/10/11/13/10 = 127 | V5: identical (133 − 6 = 127; same per-grammar distribution) | **ACCEPT-CARRY** — `crates/core/src/runtime/mod.rs:25-71` unchanged at V5 HEAD |
| 1B D8/D10 split (4 tables: :50-51, :63-64, :71-73, :86-87) | V4: V3-LOCKED axis carried through V4 with zero drift | V5: 1B unchanged at V5 HEAD (V3-LOCKED axis carried through V4 → V5) | **ACCEPT-CARRY** |
| 1D row 117 (substrate cardinality) | V4-LOCKED | V5: 1D unchanged at V5 HEAD | **ACCEPT-CARRY** |
| 1D row 123/124 (codegen-vs-pass leak split) | V4-LOCKED | V5: 1D unchanged at V5 HEAD | **ACCEPT-CARRY** |
| 1D row 131 (PC-008 + U-PC-002 cross-cite) | V4-LOCKED | V5: 1D unchanged at V5 HEAD | **ACCEPT-CARRY** |
| 1D row 134 (CSS L4 layout asymmetry) | V4-LOCKED | V5: 1D unchanged at V5 HEAD | **ACCEPT-CARRY** |
| AP-009 + AP-020 distinct anchors (1F-anti-pattern.md:55,80,105) | V3-rebound anchors carried through V4 | V5: 1F-anti-pattern unchanged at V5 HEAD | **ACCEPT-CARRY** |
| Lock 14 cmd matrix (9 dirs, 10 sheets files, 30/15, 67, 127/47) | V4: live re-run matched all cited integers | V5: live re-run matches all cited integers (see matrix below) | **ACCEPT-CARRY** |
| NEW-CH2-V4-01 justification-table discipline | V4: institutionalised post-V4; covers F-V4-CH2-1 with per-token justification table | V5: F-V5-CH6-1 is an anchor refresh (not a count-cite repair), so no new justification table is triggered; pre-existing discipline carries unbroken | **ACCEPT-CARRY** |
| Cross-inventory 126/127 propagation containment | V4: `rg -n '\b12[67]\b'` over 7 non-1C inventories returned `1F-anti-pattern.md:61` only (path-ref, not count) + `1E-locks-evidence.md:35` (self-cite `:126-128`) | V5: same command returns `1F-anti-pattern.md:61` only — the V5 fold rewrote the 1E:35 self-cite to `:128-130` (no longer contains `\b126\b` substring); cross-inventory propagation containment **TIGHTENED** at V5 | **ACCEPT-CARRY+** — V5 fold incidentally removed the last preserved 126 cross-inventory token outside 1C |

## V5 Verification Re-run At HEAD (2026-05-23, commit `9833295d5`)

### Lock 14 cmd matrix (live re-run at V5 HEAD)

| Lock 14 verification command | Required result | Live result at V5 HEAD `9833295d5` | V5 citation | Verdict |
|---|---|---|---|---|
| `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d \| wc -l` | 0 | 9 (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) | 1C `:21,28,91,195`; 1F-coherence `:73`; 1F-past-corpora `:83` | ACCEPT — 9 cited verbatim; V4 cite-rebinds carry forward unchanged |
| `find crates/core/src/runtime/google_sheets -type f \| wc -l` | n/a (composition check) | 10 | 1C `:54,196`; 1F-anti-pattern `:40,76`; 1F-coherence `:73,92`; 1F-past-corpora `:50,83,120` | ACCEPT — 10 cited across all five surfaces |
| `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **30** | 1C `:23,93,125,200` | ACCEPT — 30 cited verbatim |
| `rg -l 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` | 0 | **15** | 1C `:93,125,200` | ACCEPT — 15 cited verbatim |
| Pattern H file census | 0 hand-written | 67 hand-written; 0 generated markers | 1C `:42-57,126,196-199`; 1F-anti-pattern `:40,76`; 1F-past-corpora `:50,83,120` | ACCEPT — census matches live |
| Grammar-named reexport census in runtime/mod.rs | 0 grammar-named | 9 `pub mod <g>;` + **127 grammar-named reexports** at `crates/core/src/runtime/mod.rs:25-71` (mechanical multi-line Python extraction at V5 HEAD: 133 raw `pub use` entries inside the cited 25-71 window minus 6 in-window neutrals — `CompoundHandle`, `DtaError`, `GenericAtRule`, `ParseErr`, `StringHandle`, `StructBuilder` — yields **127**. Per-grammar: bbnf 10, bnf 10, css_l4 43, css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10. Sum 10+10+43+10+10+10+11+13+10 = 127. Out-window neutrals at `:72` (`IntoPathSegment, Path, PathSegment`) and `:76` (`RuntimeView`) excluded per NEW-CH2-V2-03.) | 1C `:23,24,40,92,124,162,201` (eight sites total at V5; identical to V4) cite "127" with full neutral-enumeration per NEW-CH2-V2-03 | ACCEPT — mechanical extraction reproduces 127 exactly across all eight cited sites; surface unchanged from V4 |

### Mechanical extraction reproduction (V5 HEAD `9833295d5`)

```
Raw pub use entries in lines 25-71: 133
In-window neutrals: ['CompoundHandle', 'DtaError', 'GenericAtRule', 'ParseErr', 'StringHandle', 'StructBuilder']  count=6
Grammar-named total: 127
Per-grammar:
  bbnf: 10
  bnf: 10
  css_l4: 43
  css_pretty: 10
  csv: 10
  ebnf: 10
  google_sheets: 11
  json: 13
  math: 10
Sum: 127
```

Out-window verification at V5 HEAD (lines 72-76 of `crates/core/src/runtime/mod.rs`):
```
pub use path::{IntoPathSegment, Path, PathSegment};

/// AZ-I.W2-act.close A.fix — re-export the grammar-agnostic
/// [`view::RuntimeView`] trait at the stable `crate::runtime` path.
pub use view::RuntimeView;
```
Four out-window neutrals confirmed at the exact lines the V5 inventory cites (`:72` × 3 + `:76`). Identical to V4 extraction.

### NEW-CH2-V3-02 orphan-cell propagation guard (executable verification at V5 HEAD)

Command: `rg -n '\b126\b' restart/audit/totality/p1/1C-runtime-evidence.md`

Live output at V5 HEAD `9833295d5`:
```
24:    - V2-fold (F-V3-CH2-1 off-by-one repair): the V2 cycle cited "126" via "subtract 10 grammar-neutral exports", but 4 of those 10 (`IntoPathSegment, Path, PathSegment` at `mod.rs:72` + `RuntimeView` at `mod.rs:76`) sit OUTSIDE the 25-71 window. Correct count subtracting only the 6 in-window neutrals is **127**, not 126. […]
50:| `css_l4` | 7 | mod, arena, builder, document, parse_with, value, view | 3,126 | hand-written; builder.rs:1 "AZ-I.W2-act.B3 — CssStructBuilder" cutover-tagged | NO — same template miss |
```

Each preserved-token justification (carried forward bit-for-bit from V4):

| Line | Token context | Justification class | Live verification at V5 HEAD |
|---|---|---|---|
| `1C:24` | "the V2 cycle cited '126' via 'subtract 10 grammar-neutral exports'" | **Historical V2-fold prose** — required for the audit trail of the F-V3-CH2-1 → F-V4-CH2-1 → V5-LOCKED repair chain; quoted reference to a prior cycle's defect figure, not a current count cite | Read of 1C:24 confirms quote-mark framing around "126"; surrounding prose explicitly labels it as the V2 cycle's incorrect cite that V3 repaired and V4 propagated to the orphan executive-summary site |
| `1C:50` | "css_l4 ... 3,126" (LOC sum for the css_l4 directory) | **Unrelated structural integer** — substring "126" happens to appear at the tail of the LOC sum 3126 for the css_l4 directory; orthogonal to grammar-named-reexport counts | `find crates/core/src/runtime/css_l4 -type f -name '*.rs' -exec wc -l {} +` → `3126 total` (parse_with 113 + mod 79 + arena 390 + value 852 + document 541 + builder 1014 + view 137 = 3126) — re-verified at V5 HEAD identical to V4 |

Zero unjustified 126 tokens remain in 1C. NEW-CH2-V3-02 orphan-cell propagation guard **SATISFIED for the V5 cycle**.

### Cross-inventory 126/127 propagation scan (V5 HEAD)

Command: `rg -n '\b12[67]\b' restart/audit/totality/p1/1A-substrate-evidence.md restart/audit/totality/p1/1B-codegen-evidence.md restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-anti-pattern.md restart/audit/totality/p1/1F-coherence-scan.md restart/audit/totality/p1/1F-past-corpora.md`

Live output at V5 HEAD `9833295d5`:
```
1F-anti-pattern.md:61:| AP-001 | Lock 13 forbids non-generated files >500 LOC at `restart/locks/LOCKS.md:220` block tail; MASTER `restart/MASTER-PLAN.md:127`. | […]
```

Exactly **1 hit** across the 7 non-1C inventories — `1F-anti-pattern.md:61` cites `MASTER-PLAN.md:127` as a Lock 13 row authority (path:line reference, not a count cite). **V5 fold-tightening observation**: V4 reported 2 hits (this `MASTER-PLAN.md:127` ref + `1E-locks-evidence.md:35` self-cite carrying `:126-128`); the V5 cosmetic fold rewrote 1E:35 to `:128-130`, dropping the `\b126\b` substring from that cell. The cross-inventory 126/127 propagation surface **tightens from 2 hits at V4 to 1 hit at V5** — a non-load-bearing but audit-clean side effect of F-V5-CH6-1. CH2 count-cite repair surface remains 1C-local through V5.

### F-V5-CH6-1 anchor-refresh verification (single V5 edit cell)

Live verification of the 1E:35 self-cite refresh:
```
diff (1E:35):
- […] Cross-reference to §1.5 LAC-1E-12 promotion candidacy block at `1E-locks-evidence.md:126-128`; […]
+ […] Cross-reference to §1.5 LAC-1E-12 promotion candidacy block at `1E-locks-evidence.md:128-130`; […]
```

Live read at V5 HEAD `9833295d5`:
- `1E:128`: `### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)` (heading present at refreshed anchor)
- `1E:130`: `T-P1 V2 promotes **LAC-1E-12 from candidate-addition to candidate-promoted-to-T-P3-§3C-priority** as the most substantive cross-lens governance signal surfaced by V1 hardening. […]` (body present at refreshed anchor)

The refresh is executable-verifiable in the LAC-1E-12 procedural addendum sense: the 1E:35 sustained-UNKNOWN paragraph (a CH6 anti-paper-close anchor) cites the LAC-1E-12 promotion-candidacy block (a CH7 binding surface), and at V5 HEAD that cite correctly resolves to the post-V4-shift location. **For CH2 GENERALITY**: this fold introduces zero new grammar-named-count cites, touches zero Lock-14-bearing cells, and removes one residual `\b126\b` token from the cross-inventory propagation surface — uniformly CH2-positive or CH2-neutral.

## Cross-Inventory Generality Coverage Matrix (V5 post-fold)

| Generality lens demand | 1A | 1B | 1C | 1D | 1E | 1F-anti | 1F-coh | 1F-past | Coverage |
|---|---|---|---|---|---|---|---|---|---|
| Lock 14 holds | DIV-008,LOCK1-AMEND-001 | D7,D8,D10,D13 | D1,D4,D6,D10 | rows 117,123,124,130 | LAC-08,12,15 | AP-003,012,016,020 | COH-005,008,011 | PC-006,008,017 | full |
| No JSON-only cataloguing of grammar-neutral facts | SUB-014,016,017,018 | D8,D10 | mod.rs cit | rows 100,113,131 | LAC-12 | AP-005 | COH-005 | PC-008 | full |
| 1C runtime census flags every grammar-named module | — | — | 1C-D1..D11 | row 130 | LAC-12 | AP-016 | COH-011 | PC-017 | full (1C primary) |
| 1D separates JSON-empirical from grammar-neutral | — | — | — | rows 117,118,123,124,131,134 | — | — | — | — | full |
| No grammar-name leak passes uncited | SUB-014,016,017,018,DIV-008 | D7,D8,D10,D13 | D1-D11 | rows 123,124,130,145 | LAC-08,12,15 | AP-009,011,012..016,020 | COH-005,011 | PC-008,017 | full |
| Pass-layer grammar-shape leaks (D8 + D10) carried as upstream Sheets/BBNF-self blockers distinct from codegen-layer name leaks | — | D8,D10 (4 tables: `:50-51, :63-64, :71-73, :86-87`) | — | row 124 | — | — | — | — | full |
| Subtract-from-K neutral arithmetic carries in-window enumeration per NEW-CH2-V2-03 | — | — | mod.rs cit `:23,40,92,124,162,201` (eight sites total at V5; identical to V4) | — | — | — | — | — | full |
| Orphan-cell propagation guard per NEW-CH2-V3-02 (count-cite micro-folds must rg-verify justified-only before commit) | — | — | V4 F-V4-CH2-1 fold satisfies; V5 verification re-affirms (no new count cite introduced; orphan-cell guard holds across V5) | — | — | — | — | — | full (V4 micro-fold institutionalised; V5 MAINTAINS) |
| Justification-table discipline per NEW-CH2-V4-01 (preserved-old-value tokens require per-token justification after a count-cite repair) | — | — | F-V4-CH2-1 carries justification table at V4 CH2.md; V5 carries the V4 table forward without re-edit (F-V5-CH6-1 is anchor refresh, not count repair, so no new justification table triggered) | — | — | — | — | — | full (V4 institutionalised; V5 inheritance correct) |

Coverage: **full on all nine lens demands at V5**. Row 9 (NEW-CH2-V4-01 justification-table discipline) added as a first-class lens demand by V4; V5 carries the discipline forward without trigger (F-V5-CH6-1 is not a count-cite repair surface, so the V4 discipline applies vacuously). Row 8 (NEW-CH2-V3-02 orphan-cell propagation guard) holds at V5 with zero edits to the 1C surface that V4 verified.

## Required Revisions

**None.** V5 CH2 clears with 12/12 = 100% ACCEPT-rate. The V4 single 1C fold (F-V4-CH2-1) is V4-LOCKED at V5 with the 1C surface unchanged; NEW-CH2-V3-02 orphan-cell propagation guard holds bit-for-bit (rg `\b126\b` in 1C returns same 2 justified hits); all V3/V4-LOCKED rows hold at V5 HEAD with zero drift; the F-V5-CH6-1 cosmetic anchor refresh introduces zero new count cites, zero Lock-14-bearing edits, and incidentally tightens the cross-inventory 126/127 propagation surface from 2 hits to 1.

## New Findings Surfaced

NEW-CH2-V5-01: V5's CH2 sub-axis convergence (12/12 = 100%) is the **second consecutive per-lens 100% ACCEPT-rate** across the T-P1 hardening corpus (V1 60%, V2 91.7%, V3 91.7%, V4 100%, V5 100%). Per cohort §3Z (`restart/prompts/ORCHESTRATOR.md` §3Z + V≤5 ceiling), CH2 satisfies the two-consecutive-cycle ≥95% LOCK gate at the V5 ceiling exactly. The trajectory matches the V4 prediction at V4:138 ("Predicted V5 lands ACCEPT (≥95%) for CH2 assuming (a) no V4 → V5 micro-fold introduces a new count cite at 1C without pre-commit `rg → justified-only` verification per NEW-CH2-V3-02 + NEW-CH2-V4-01, (b) no other lens's V5 fold touches the runtime/mod.rs:25-71 surface in a way that triggers a CH2 cascade, and (c) all V3/V4-LOCKED rows continue to hold at V5 HEAD"). All three V4-predicted conditions held: (a) V5 fold is an anchor refresh, not a count cite — NEW-CH2-V3-02/V4-01 do not trigger; (b) V5 fold touches only 1E:35 (a CH6 anchor cell), not runtime/mod.rs:25-71 — no CH2 cascade; (c) all V3/V4-LOCKED rows hold at V5 HEAD with zero drift. **CH2's contribution to cohort §3Z LOCK at V5: CONFIRMED.**

NEW-CH2-V5-02: F-V5-CH6-1's incidental cross-inventory tightening (V4: 2 hits on `\b12[67]\b` across 7 non-1C inventories → V5: 1 hit) is a non-load-bearing but audit-clean side effect that demonstrates the orchestrator's cosmetic-fold discipline is CH2-positive even on non-CH2-bearing edits. Recommend T-P3 §3C carry-forward packet note: cosmetic anchor refreshes (line-number self-cite updates following neighboring paragraph insertions) should be encouraged as a continuous-tightening mechanism, not just discharged as non-blocking. The F-V5-CH6-1 / V5 cycle is the canonical exemplar — the cosmetic fold restored anchor correctness AND incidentally reduced the cross-inventory 126/127 surface area. No new discipline required; existing NEW-CH2-V3-02 + NEW-CH2-V4-01 chain already captures the relevant invariants.

NEW-CH2-V5-03 (cohort §3Z LOCK observation, CH2-axis): The five-cycle T-P1 hardening trajectory for CH2 (60 → 91.7 → 91.7 → 100 → 100) demonstrates a clean monotonic-or-flat convergence curve across all five cycles with no regression and a terminal two-cycle plateau at 100%. This is the **cleanest convergence curve in the T-P1 CH-axis matrix** (per the V4 CH2 V4:138 trajectory and the V5 §4 close-prediction tables in the CONSOLIDATED reports). The repair lineage is fully captured: V1 (60+ symbol initial cite; baseline) → V2 (126 via incorrect "subtract-10" neutral arithmetic) → V3 (127 via correct "subtract-6-in-window" arithmetic + NEW-CH2-V2-03 in-window enumeration discipline + structural-row sites at 1C:21,23,24,92,124,162,201; orphan 1C:40 executive-summary 126 remains) → V4 (1C:40 propagation discharges + NEW-CH2-V3-02 orphan-cell propagation guard institutionalised + NEW-CH2-V4-01 justification-table discipline) → V5 (zero CH2-bearing edits; all V4 discipline carried forward bit-for-bit; cross-inventory 126/127 surface incidentally tightens from 2 to 1 hit). CH2 is the canonical exemplar of cohort §3Z LOCK convergence within T-P1.

## Cohort §3Z LOCK Enablement Confirmation (CH2 contribution)

§3Z gate (cohort-level, V≤5 ceiling): cohort LOCK fires when **all CH-axes clear ≥95% across two consecutive cycles**. CH2-axis evaluation at V5:

| Cycle | CH2 ACCEPT-rate | ≥95% gate |
|---|---|---|
| V1 | 60% | FAIL |
| V2 | 91.7% | FAIL |
| V3 | 91.7% | FAIL |
| V4 | 100% | **PASS** (first ≥95% cycle for CH2) |
| V5 | 100% | **PASS** (second consecutive ≥95% cycle for CH2 — LOCK-enabling) |

**CH2 contribution to cohort §3Z LOCK at V5: ENABLED.** CH2 has cleared the two-consecutive-cycle ≥95% LOCK rule at the V5 ceiling exactly. CH2 imposes no blocker on the cohort §3Z LOCK declaration; the cohort-wide LOCK trigger depends on CH1, CH3, CH4, CH5, CH6, CH7 also clearing the same gate (per their respective lens reports). No CH2 risk to V5 LOCK trajectory remains.
