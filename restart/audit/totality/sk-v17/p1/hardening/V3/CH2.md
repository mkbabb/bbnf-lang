---
lens: CH2-GENERALITY
pass: T-P1-SKV17-excavation
cycle: V3
reviewed_at: 2026-05-29T24:30:00Z
subject_artefacts:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
mandate: >
  Lock 14 grammar-neutrality firewall. No divergence catalogued as JSON-only
  when it is a grammar-neutral substrate fact. Excavation covers ALL grammars
  (JSON / CSS / Sheets / BBNF-self) for the value-API + NEON + tape surfaces
  (Lock 14). Not CSS-only. Every grammar-name leak in a generic crate flagged.
  JSON-empirical vs grammar-neutral separated.
live_truth_method: >
  grep -c scan_structural over crates/core/src/grammar/generated/*.rs (8 wired +
  math.rs=0); grep StructuralIndex over generated/*.rs; line-resolve css_l4.rs:15982,
  google_sheets.rs:3559, bbnf.rs:4843, ebnf.rs:1381, bnf.rs:848, csv.rs:566,
  css_pretty.rs:1905, json.rs:732; ValueRef at skinny tape/mod.rs:175, value_from_ref
  at skinny grammars/json/value.rs:143, empty in core json/value.rs; grammar-name
  leak grep over crates/simd-scan/src/*.rs + crates/core/src/runtime/tape/*.rs (both
  zero); sheets_witness 24-LOC stub (no .bbnf/BackendRule); substrate.rs:43,55
  data-binding; select_classifier(&[u8;64]) at dispatch.rs:42; StructuralAlphabet
  config at alphabet.rs:23-37. No build mutation. Master HEAD 445925167.
sections_dispositioned: 14
accept: 13
revise: 1
reject: 0
note: >
  All five missing-inventory orphans from V2 (1A, 1C, 1D — plus the 1F triad's
  three GENERALITY defects) have LANDED and FOLDED in V3. The V2 REJECT (the false
  "json/ebnf/bnf/csv only" Gaps row) and the two V2 REVISEs (Cross-Tree map, Exec
  Summary) are corrected in every V3 inventory: the all-8-grammar scan census is now
  carried uniformly. The two V2 pass-level orphans (1C runtime census, 1D
  JSON-vs-neutral split) are closed — both inventories exist in V3 and carry the
  generality coverage CH2 demands. One residual REVISE remains: a single
  scan-census over-count in 1D ("+math.rs" listed as scan-wired when math.rs has 0
  scan_structural calls).
---

## Executive Summary

CH2 GENERALITY firewall over the SK-V17 T-P1 cycle-V3 inventories. The V2 CH2
verdict (≈73% ACCEPT) carried one three-fold scan-generality REJECT/REVISE defect
and two pass-level orphan REVISEs (missing 1C and 1D). **All are folded in V3.**
The grammar-coverage census is now uniform and correct across 1A, 1C, 1D, and the
1F triad: `scan_structural` is catalogued as WIRED into all eight generated
grammars — JSON, EBNF, BNF, CSV, **css_l4, css_pretty, google_sheets (Sheets), and
bbnf (BBNF-self)** — verified live at `crates/core/src/grammar/generated/css_l4.rs:15982`,
`google_sheets.rs:3559`, `bbnf.rs:4843`, `ebnf.rs:1381`, `bnf.rs:848`, `csv.rs:566`,
`css_pretty.rs:1905`, `json.rs:732` (grep-verified, 1 call each). The phantom
"CSS-absent gap" the V2 1F Gaps row asserted is deleted and replaced everywhere
with the true Lock-1 gap (no tape consumes the grammar-general index) — see
1f-coherence Gaps row (`1f-coherence-scan.md:118`), 1f-past-corpora DO-NOT-CARRY
flag (`1f-past-corpora.md:85-95`), 1D SK17L-008, 1C RT17-003.

The lazy-`ValueRef<G>` generalization is correctly catalogued as grammar-NEUTRAL
across all inventories (1A SUB17-004, 1C RT17-002, 1D SK17L-002, 1F COH17-002): the
core value-API is verified per-grammar eager (`grep value_from_ref` over
`crates/core/src/runtime/json/value.rs` = EMPTY; `CssTypedValue` eager at
`css_l4/value.rs:414`), while the skinny `ValueRef<'doc,'input,K,G>` at
`skinny/crates/runtime/src/tape/mod.rs:175` is grammar-parametric. The NEON
`select_classifier(&[u8;64])` (`dispatch.rs:42`) and the richer core
`StructuralAlphabet` (`alphabet.rs:23-37`) are both correctly catalogued as
grammar-as-data (breadth-of-config, not breadth-of-proof), Lock 14 honoured.

Critically, **1D now carries the explicit JSON-empirical-vs-grammar-neutral split**
(`1d-skinny-lessons.md:95-107`) the V2 orphan demanded, and SK17L-009 makes the
load-bearing generality discrimination: projection generality is exercised
by-construction on JSON + CSS ONLY; `sheets_witness` is a verified 24-LOC
`EventGrammar` stub with NO `.bbnf`/`BackendRule` (live-verified: no BackendRule in
`skinny/crates/runtime/src/grammars/sheets_witness/`), and BBNF-self has no tape
witness — Sheets/BBNF generality is by-construction, an SK-V18 fold target. This is
exactly the JSON-vs-neutral separation Lock 14 protects.

ZERO grammar-name leaks in generic crates: `crates/simd-scan/src/*.rs` and
`crates/core/src/runtime/tape/*.rs` both return EMPTY on a grammar-name grep; the
only grammar names in tape are doc-comments (`tape/mod.rs:6,18,20`), and the emitter
data-binds `builder_path`/`document_path` from `EmitStrategy::StructDirect` as DATA
(`substrate.rs:43,55`), never a grammar-name branch. All per-grammar runtime
surfaces are correctly dispositioned Lock-14-ALLOWED.

ONE residual REVISE: 1D SK17L-008 (`1d-skinny-lessons.md:91`) appends "+math.rs" to
the scan-wired census, but `grep -c scan_structural math.rs` = **0**. math.rs holds
an `OnceCell<StructuralIndex>` field (doc-comment at `math.rs:281`) yet is NOT
scan-wired — it is the one generated grammar with the index field and no scan call.
The "+math.rs" over-counts scan breadth (9 vs the true 8). Minor census-accuracy
defect, not a Lock-14 firewall failure; the all-8 claim itself is correct.

## Section Dispositions

### 1a-substrate-evidence.md (NEW in V3 — closes CH5-S0 spine + V2 orphan)

| # | Section / row | Disposition | CH2 finding |
|---|---|---|---|
| 1 | SUB17-001/002/003 tape encoding + UNWIRED (table + Cross-Tree) | **ACCEPT** | Catalogued grammar-neutrally. The SoA `Tape`/AoS `TapeRec` bifurcation is a substrate fact across all grammars, not a per-grammar quirk. Verified `skinny/crates/runtime/src/tape/mod.rs:94` (SoA) vs `crates/core/src/runtime/tape/record.rs:103` (AoS). Correct altitude; the union is one substrate per tree. |
| 2 | SUB17-004 value-API per-grammar vs `ValueRef<G>` | **ACCEPT** | Correctly grammar-neutral and grammar-PARAMETRIC. Verified core `json/value.rs` has no `value_from_ref` (grep empty); skinny `ValueRef<'doc,'input,K,G:EventGrammar>` at `tape/mod.rs:175` is generic over G. The Cross-Tree row names the regenerated set as per-grammar value.rs/view.rs/document.rs × 8 (regen-gated) — generality breadth carried. |
| 3 | SUB17-005 CollapsedStage x86 / aarch64 UNKNOWN-2D-05 | **ACCEPT** | Grammar-neutral canon fact. The 5-shape canon and x86-pin are grammar-independent; aarch64-NEON absorbs into the 4 LLVM shapes. Correctly not scoped per-grammar. |
| 4 | SUB17-007 alphabet config-breadth | **ACCEPT** | Correctly grammar-neutral; "generality is config-breadth, not proof-breadth" (the `quote_classes` field is JSON/CSS-motivated per the doc comment, exercised-only). Both grammar-as-data. Honours Lock 14. |
| 5 | Cross-Tree "Structural scan" row (line 93) — all 8 grammars | **ACCEPT** | Now lists "WIRED into ALL 8 generated grammars incl json.rs:732, css_l4.rs:15982, css_pretty, ebnf.rs, bnf.rs, csv.rs, google_sheets, bbnf"; the skinny cell marks CSS scan as "W3 wiring-state, JSON-lane wired" — the wiring-state-not-design-property distinction is carried. Live-verified. This is the V2 REVISE row 84, fully corrected. |
| 6 | Substrate-Union Firewall sidecar row (line 101) — all-8 enumeration | **ACCEPT** | Enumerates the `OnceCell<StructuralIndex>` carriers across all 8 grammars (json.rs:686 … bbnf.rs); the "Verify at fold: scope to ALL 8 carriers, not json.rs alone" note is the correct generality scope. No JSON-only narrowing. |

### 1c-runtime-evidence.md (NEW in V3 — closes V2 pass-orphan)

| # | Section / row | Disposition | CH2 finding |
|---|---|---|---|
| 7 | RT17-004 per-grammar runtime census + Lock-14 leak audit | **ACCEPT** | The orphan-REVISE target from V2 §6 is now authored. Census lists nine per-grammar runtime dirs (json, css_l4, bbnf, bnf, csv, ebnf, css_pretty, google_sheets, math) and confirms grammar-named symbols (`CssStructBuilder`, `CssTypedValue`, `JsonChildrenIter`, `CssChildrenIter`) live ONLY under `runtime/<g>/` (Lock-14 ALLOWED). Generic `runtime/{tape,builder.rs,view.rs}` carry NO grammar names. Live-verified: grammar-name grep over `crates/core/src/runtime/tape/*.rs` = EMPTY. The Sheets (`google_sheets`) and BBNF-self (`bbnf`) runtime surfaces — the V2 CH2 explicitly could not confirm without 1C — are now catalogued generality-clean. Correct. |
| 8 | RT17-003 + RT17-005 scan-wired all 8 / OnceCell all 8 | **ACCEPT** | Lists exactly the 8 scan-wired grammars (json:732 … google_sheets:3559) and does NOT falsely add math to the scan-wired set (RT17-005 OnceCell list also enumerates the 8 doc-line carriers). Live-verified: 8 grammars at 1 scan call each, math.rs = 0. 1C's census is the accurate one. |
| 9 | RT17-007 BackendShape runtime-clean across grammars | **ACCEPT** | Verified grep-empty for all 5 shape names over `crates/core/src/runtime/` + `generated/` — the runtime carries no shape leak for ANY grammar. Grammar-neutral, correct. |

### 1d-skinny-lessons.md (NEW in V3 — closes V2 pass-orphan; JSON-vs-neutral split)

| # | Section / row | Disposition | CH2 finding |
|---|---|---|---|
| 10 | JSON-Empirical vs Grammar-Neutral Split table (lines 95-107) | **ACCEPT** | The exact deliverable the V2 §6 orphan demanded. Five lessons split into JSON-empirical witness vs grammar-neutral durable fact, each with a generality-status column ("tape: JSON-WITNESSED; CSS is SK-V17 first-mover; Sheets/BBNF by-construction"). The "a CSS-only generator that never re-emits JSON FAILS CH2" note (line 104) is itself a CH2-aware fence. Textbook JSON-vs-neutral discipline. ACCEPT. |
| 11 | SK17L-009 Sheets/BBNF-self by-construction-not-by-exercise | **ACCEPT** | The load-bearing generality discrimination. Live-verified: `sheets_witness` is a 24-LOC `EventGrammar` with NO `.bbnf`/`BackendRule` (`skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs`, 24 LOC; grammar-name + BackendRule grep returns none); BBNF-self has no tape witness. Correctly catalogued: Lock 14 generality is by-construction (alphabet-as-data), proven by-exercise on JSON (tape) + CSS (SK-V17). Sheets/BBNF proof is SK-V18. This is precisely the JSON-empirical-vs-grammar-neutral separation Lock 14 protects, and it does NOT over-claim Sheets/BBNF as proven. ACCEPT. |
| 12 | **SK17L-008 scan-wired census "+math.rs" (line 91)** | **REVISE** | CH2 census-accuracy DEFECT. The cell reads "WIRED into ALL 8 generated grammars: json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf each carry exactly one `scan_structural` call (`…/generated/*.rs`, grep-verified 1 per file; **+`math.rs`**)". LIVE TRUTH: `grep -c scan_structural crates/core/src/grammar/generated/math.rs` = **0**. math.rs is the ONE generated grammar that holds an `OnceCell<StructuralIndex>` field (doc-comment `math.rs:281`) but is NOT scan-wired. The "+math.rs" parenthetical over-counts the scan breadth to 9 and mislabels math as scan-wired — the inverse of the V2 under-count defect, in the same census. FIX: strike "+`math.rs`"; replace with "(math.rs is the lone generated grammar carrying the `OnceCell<StructuralIndex>` field but NO `scan_structural` call — 8 scan-wired, not 9; `grep -c scan_structural math.rs`=0, field doc-comment `math.rs:281`)". The all-8 claim itself is correct and stands; only the math parenthetical is wrong. Note: 1C (RT17-003/005) and the 1F triad correctly list only the 8 and do NOT add math — the defect is localized to this 1D cell. |

### 1f-coherence-scan.md / 1f-anti-pattern.md / 1f-past-corpora.md (V2 defects folded)

| # | Section / row | Disposition | CH2 finding |
|---|---|---|---|
| 13 | 1f-coherence Cross-Tree scan row (line 95) + Gaps row (line 118) — V2 REJECT/REVISE folded | **ACCEPT** | The V2 REJECT (false "json/ebnf/bnf/csv only … css_l4 grep absent") is GONE. Line 95 now reads "WIRED into ALL 8 generated grammars (json.rs:732, css_l4.rs:15982, …, google_sheets.rs:3559, bbnf.rs:4843)"; line 118 Gaps row reframes to the true Lock-1 gap ("the missing primitive is the TAPE CONSUMER, not the scan … All 8 generated grammars are scan-wired (not json/ebnf/bnf/csv only)"). COH17-008 carries the breadth-of-config-not-proof note. Every V2 generality defect corrected. 1f-anti-pattern AP17-002/004 correctly dispositions all grammar-named constructs as Lock-14-ALLOWED per-grammar surfaces and enumerates the OnceCell sidecar across all 8 (line 62). ACCEPT. |
| 14 | 1f-past-corpora DO-NOT-CARRY-UNDERCOUNT flag (lines 85-95) + all-8 enumeration (lines 97-105) | **ACCEPT** | An exemplary CH3/CH2 cross-guard: it catches that prior SK-V14 COH-014 itself enumerated JSON + Google Sheets carriers, which already contradicts the V2 "json/ebnf/bnf/csv only" claim, and asserts the V3 all-8 census with per-grammar line citations (json:732 … bbnf:4843). PC17-005 separates skinny-benched-surface (grammar-empirical) from totality-tree fold-target artefacts. No JSON-only mis-catalogue; no grammar-name leak. ACCEPT. |

## Grammar-Coverage Matrix (CH2 firewall verification)

| Grammar | scan_structural wired (core) | value-API in core | catalogued generality verdict | CH2 |
|---|---|---|---|---|
| JSON | YES (json.rs:732) | eager `JsonValue` enums, no `value_from_ref` | JSON-empirical WITNESS (tape proven); grammar-neutral substrate | clean |
| CSS (css_l4) | YES (css_l4.rs:15982) | eager `CssTypedValue` (value.rs:414) | SK-V17 first-mover; by-exercise proof | clean |
| CSS (css_pretty) | YES (css_pretty.rs:1905) | per-grammar | grammar-neutral scan, per-grammar runtime | clean |
| Sheets (google_sheets) | YES (google_sheets.rs:3559) | per-grammar runtime dir | by-construction (sheets_witness is 24-LOC stub, no BackendRule); SK-V18 proof | clean |
| BBNF-self (bbnf) | YES (bbnf.rs:4843) | per-grammar runtime dir | by-construction; no tape witness; SK-V18 proof | clean |
| EBNF | YES (ebnf.rs:1381) | per-grammar | grammar-neutral scan | clean |
| BNF | YES (bnf.rs:848) | per-grammar | grammar-neutral scan | clean |
| CSV | YES (csv.rs:566) | per-grammar | grammar-neutral scan | clean |
| math | **NO (math.rs scan=0)** | per-grammar runtime dir | holds OnceCell field, NOT scan-wired | 1D "+math.rs" REVISE |

CH2 verifies: no value-API/NEON/tape divergence is catalogued JSON-only when it is
grammar-neutral; Sheets and BBNF-self are explicitly covered (scan-wired + correctly
flagged by-construction-not-by-exercise for the value-API proof); the
generic-crate grammar-name-leak firewall is clean. The excavation is NOT CSS-only and
NOT JSON-only — it spans the full JSON/CSS/Sheets/BBNF-self matrix per Lock 14.

## V2-Orphan Closure Status

| V2 orphan / defect | V3 status |
|---|---|
| 1A absent (CH5-S0 spine) | CLOSED — `1a-substrate-evidence.md` landed, substrate spine grammar-neutral |
| 1C runtime census absent (V2 §6 orphan) | CLOSED — `1c-runtime-evidence.md` landed; Sheets+BBNF runtime surfaces catalogued clean (RT17-004) |
| 1D JSON-vs-neutral split absent (V2 §6 orphan) | CLOSED — `1d-skinny-lessons.md` landed; split table (lines 95-107) + SK17L-009 by-construction discrimination |
| V2 1F Gaps row 107 REJECT (false CSS-absent) | FOLDED — deleted/replaced with true tape-consumer gap in all inventories |
| V2 1F Cross-Tree row 84 REVISE | FOLDED — all-8 census carried |
| V2 1F Exec Summary 55/57 REVISE | FOLDED — Exec Summaries carry all-8 + wiring-state-not-design |

Zero open orphan REVISE on the pass for the GENERALITY axis. All V2 GENERALITY
charges are folded; one new minor census-accuracy REVISE (1D "+math.rs") is the
sole residual.

## CH2 Verdict

The V3 cycle is grammar-correct at the value-API / NEON / tape substrate altitude
and spans the full JSON / CSS / Sheets / BBNF-self matrix Lock 14 demands. The three
V2 GENERALITY defects (the false JSON/CSV-only scan gap, the Cross-Tree map, the
Exec Summary) are folded across every inventory; the two V2 pass-level orphans (1C
runtime census, 1D JSON-vs-neutral split) are closed by the newly-landed
inventories, both of which carry exactly the generality coverage CH2 demanded. The
sheets_witness-is-a-stub / BBNF-has-no-tape-witness discrimination (1D SK17L-009) is
the model JSON-empirical-vs-grammar-neutral separation. No grammar-name leak passes
uncited; the generic crates are leak-clean. One residual REVISE remains: a localized
scan-census over-count in 1D ("+math.rs" listed as scan-wired when math.rs has zero
scan_structural calls) — the inverse of the V2 under-count, in the same census cell.
The all-8 claim itself is correct; only the math parenthetical is wrong, and 1C/1F
already carry the accurate 8-not-9 census.

Counts: 14 sections — 13 ACCEPT, 1 REVISE, 0 REJECT (≈93% ACCEPT). Just below the
≥95% gate; one targeted V4 fold (strike 1D "+math.rs") closes it. Materially this is
a convergent cycle on the GENERALITY axis — the substantive V2 firewall breaches are
all resolved.
