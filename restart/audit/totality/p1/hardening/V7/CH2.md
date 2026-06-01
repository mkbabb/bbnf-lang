# SK-V18 T-P1 V7 (cycle V7) — CH2 GENERALITY

Adversarial review of the T-P1 evidence inventories
(`restart/audit/totality/p1/1A..1F`, including the two live 1F auxiliaries
`1F-anti-pattern.md` and `1F-past-corpora.md`) against the V1 spec surfaces
(`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`),
the SK-V18 plan (`restart/skinny/tranches/sk-v18/SPEC.md`), and the live code at
the dirty-tree HEAD.

Lens authority — Lock 14:

- no divergence catalogued JSON/CSS-only when it is a grammar-neutral substrate fact;
- 1C flags every grammar-named module in a generic crate;
- 1D separates JSON/CSS-empirical from grammar-neutral findings;
- no grammar-name leak passes uncited; every intervention works for CSS L4 /
  Sheets / BBNF-self, not only JSON.

This V7 cycle is the SECOND consecutive drive toward the two-clean fixed point.
V6 CH2 (`hardening/V6/CH2.md`) returned REVISE with ONE residual precision item
(CH2-V6-R01: the stale narrow-regex catch count "5" at `1F-anti-pattern.md:65`
contradicting its already-corrected companion `1F-coherence-scan.md`'s "4"). V7's
first duty is to confirm that fold landed; its second is to re-ground every
load-bearing Lock-14 leak row and catch any GENUINE reject. Per the corrected
REJECT convention, a REJECT is admissible ONLY when an inventory STATES SOMETHING
FALSE ON DISK with a live falsifying citation; a self-falsified suspicion is an
ACCEPT recorded reject=0.

## Verdict

ACCEPT (CLEAN). The sole V6 REVISE (CH2-V6-R01) is FOLDED: `1F-anti-pattern.md:65`
now reads "catches only **4** of the 9 idents rows (`:137,:143,:149,:155`; per
CH2-V3-008, superseding the CH2-V2-009 '5' wording); the other 5
(`:161,:167,:173,:179,:185`) escape." — verbatim the V6-required correction. The
two LIVE 1F files now AGREE on 4; no stale standalone "5"-as-catch-count survives
anywhere in `p1/*.md` (the only "5" tokens are the legitimate "the other **5**
escape" and the explicit historical "replacing the carried CH2-V2-009 'catches
only 5' wording" citation). Nine accept; zero revise; zero reject. The
grammar-neutrality skeleton is sound, every load-bearing Lock-14 leak row
re-verifies against live code to the line, and no grammar-neutral substrate fact
is catalogued JSON/CSS-only. No GENUINE reject: nothing in the inventories states
a load-bearing falsehood on disk.

This is the FIRST clean CH2 cycle; with one more clean confirming cycle the CH2
lens reaches the two-consecutive-clean §3Z fixed point.

## Spot-Verification of the Most Load-Bearing Cited Rows

Every primary CH2/Lock-14 citation re-grounded on the dirty-tree HEAD:

- `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE` — VERIFIED: exactly
  NINE grammar-named `idents` rows at `:137,:143,:149,:155,:161,:167,:173,:179,:185`
  (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty); the `:318` hit is the
  struct field declaration (`pub idents: &'static [&'static str],`), NOT a tenth row.
  Table opens `:134` `pub const PRODUCTION_MANIFEST_TABLE`; consumer
  `for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)` at
  `:216`. The 9-grammar-WIDE relocated-seam framing in `1F-coherence-scan.md:79`,
  `1F-anti-pattern.md:65`, `1C` D9 cross-ref, `1E:94,:118`, and `1F-past-corpora.md:82`
  is exact.
- Narrow-regex catch breadth — VERIFIED: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
  crates/ir/src/registry/strategy.rs | grep 'idents:'` matches EXACTLY the four rows
  `:137,:143,:149,:155`; the other five (`:161,:167,:173,:179,:185`) escape. The TRUE
  catch count is **4**. BOTH live 1F files now carry **4**: `1F-anti-pattern.md:65`
  (CH2-V6-R01 fold landed) and `1F-coherence-scan.md:79,:86`. The two files no longer
  contradict.
- `crates/core/src/css_types.rs:1` — VERIFIED verbatim: "//! Host shims for the CSS L4
  grammar's `-> parse_hex_color(...)` map.", 66 LOC, in `crates/core/src/` (NOT a
  `crates/<grammar>/` declaration crate). LOCKS.md:349 names "`crates/core/src/css_types.rs`"
  verbatim in its own "current overfitting mess" enumeration. 1C D9 / COH18-006 /
  D-1E-V5-14 Lock-14-(c)-does-not-apply disposition correct.
- LOCKS:349 self-gate falsification — VERIFIED: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
  crates/ir/src/ crates/analysis/src/` = **13** matching lines (11 ir + 2 analysis),
  while LOCKS.md:349's own verification command asserts it "returns ZERO." The self-gate
  is RED. Analysis-crate leaks live-confirmed: `crates/analysis/src/state/ast_utils/mod.rs:4`
  (`BbnfBootstrapNodeView`), `:11` (`BbnfBootstrapRuleKind`); ir siblings
  `crates/ir/src/passes/recognizers/grammar_facts.rs:799` (`BbnfBootstrap::parse`),
  `shape_dispatch/scalar.rs:17` (`BbnfBootstrap's`). `1E:94,:118` (D-1E-V5-14,
  CH2-V4-005/007), `1F-coherence COH18-012`, and `1C` C3 correct; reclassification OFF
  impl-exceeds-spec correct.
- Runtime-only Pattern-H leak (1C C3) — VERIFIED: `rg ... crates/core/src/runtime/` = 12
  sites in EXACTLY 4 `parse_with.rs` (json, google_sheets, bbnf, css_l4); the css_l4 leak
  symbols `__shape_support_CssL4Parser` / `parse_CssL4Parser_stylesheet` at
  `crates/core/src/runtime/css_l4/parse_with.rs:4,:33,:36`. `@generated` census = 67 (D4).
  Exact.
- Totality `OnceCell<StructuralIndex>` probe breadth — VERIFIED in the CORRECT tree
  (`crates/core/src/grammar/generated/`, NOT the skinny tree): `ensure_structural_index`
  present in exactly 8 of 9 generated grammars (json/bnf/ebnf/csv/google_sheets/css_pretty/
  bbnf/css_l4), `math.rs` = 0. `json.rs:701` `pub(crate) structural_index: ::core::cell::OnceCell<`,
  `:719` `pub(crate) fn ensure_structural_index<'a>(`, `:732` `::simd_scan::scan_structural(input, &alphabet)`;
  emitter diction `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`
  "The probe substrate (OnceCell + helper) emits". The "8 of 9, all but math" breadth
  (CH5-V3-003) carried at `1F-anti-pattern.md:44` and `1F-coherence COH18-015` is exact.
  (Note: the V6 CH2 verification command searched `skinny/crates/runtime/src/grammars`,
  which is the WRONG tree for this finding — skinny carries ZERO `OnceCell<StructuralIndex>`,
  as the inventory itself states; the finding is about the totality tree and is correct.)
- Scanner-crate asymmetry (COH18-015) — VERIFIED: `crates/simd-scan/src/lib.rs:68`
  `pub use index::{StructuralIndex, next_structural_at_or_after};`; skinny `bbnf-simd` has
  ZERO `next_structural_at_or_after` (`rg -c ... skinny/crates/bbnf-simd/src/lib.rs` = 0).
  The "FUNCTIONALLY PARALLEL, divergent APIs" framing (CH5-V4-011) is grammar-neutral and
  exact; the renamed-parallel-scanner risk is correctly ACTIVE-not-one-sided.
- 1D JSON/CSS-empirical vs grammar-neutral separation — VERIFIED: the `:177-179`
  separation header splits J/C rows (`:183-191`) from G rows (`:195-209`) per Lock 14;
  G-10 (`1D:206`) re-scopes the 94.1%/79.5% figures as CSS-EMPIRICAL (decision-RULE kept
  neutral, ratio kept CSS); the leaf census `rg find_component_delim
  skinny/crates/runtime/src | grep -v css` is EMPTY (leaf in 7 css_l4 replicas only, zero
  non-CSS callers). The grammar-NEUTRAL 5-shape `select_lowering` discriminator (G-3) and
  the Lock-1 substrate-union (G-1, `1F-past-corpora:29`) are NOT scoped JSON-only.
- Runtime witness + codegen module census — VERIFIED: `skinny/crates/runtime/src/lib.rs:34`
  `pub mod json_event_grammar_witness;` and `:38` `pub mod sheets_witness;` are BOTH
  `#[cfg(any(test, feature = "proof"))]`-gated (attrs at `:33`,`:36-37`). The UNGATED
  codegen modules `skinny/crates/codegen/src/lib.rs:4` `mod json_sink_direct;`, `:5`
  `mod json_typed_direct;` and the `json_templates/` directory (6 files: config/generated/
  parser/value/view/visitor) are census'd at `1F-anti-pattern.md:63,:64`. 1A-SUB-023
  phantom-`<G>` DELETE with the K-axis PRESERVED (`tape/mod.rs:175,:179`; K at `:178`) is
  exact.

Not one recalled LOC, fabricated symbol, or false path:line was found among the
load-bearing CH2 rows. Citation discipline remains near-exhaustive.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V7-A01 | ACCEPT (re-judges V6 CH2-V6-R01 from REVISE — FOLD LANDED) | The V6 residual precision REVISE is closed. `1F-anti-pattern.md:65` now carries the corrected narrow-regex catch count "**4** of the 9 idents rows (`:137,:143,:149,:155`; per CH2-V3-008, superseding the CH2-V2-009 '5' wording); the other 5 (`:161,:167,:173,:179,:185`) escape." — verbatim the V6-required correction. Both LIVE 1F files now agree on 4; no stale standalone "5"-as-catch-count survives anywhere in `p1/*.md`. A T-P2 reader cross-referencing the two 1F packets is no longer misled. | live `1F-anti-pattern.md:65` (fold text) ∧ `1F-coherence-scan.md:79,:86` (already-correct companion); falsifier `rg ... strategy.rs | grep 'idents:'` = `:137,:143,:149,:155` (4); stale-"5" grep over `p1/*.md` returns only the legitimate "other **5** escape" + the historical CH2-V2-009 supersession citation. |
| CH2-V7-A02 | ACCEPT | The relocated-seam analog is correctly catalogued as a 9-grammar-WIDE Lock-14 leak in the generic `ir` crate, with consumer anchor (`:216`) and the narrow-regex blind-spot named. The 9-row breadth is live-exact (9 `idents` rows `:137-185`; `:318` is the field decl, not a row). Grammar-name leak does NOT pass uncited. | `crates/ir/src/registry/strategy.rs:137-185` (9 idents rows, live-verified), `:134` table open, `:216` consumer, `:318` field decl; `1F-coherence-scan.md:79`; `1F-anti-pattern.md:65`; `1C-runtime-evidence.md:72`; `1E-locks-evidence.md:94,:118`. |
| CH2-V7-A03 | ACCEPT | The lock-NAMED `css_types.rs` host shim is flagged in 1C (D9), 1F-coherence (COH18-006), 1F-anti-pattern, and 1E (D-1E-V5-14) as a grammar-named MODULE in the generic core crate; Lock-14-(c) correctly held NOT to apply (it admits only a separate `crates/<grammar>/` declaration crate). LOCKS.md:349 names the file verbatim. Not catalogued JSON-only; the SK-V19 relocate-or-delete disposition is grammar-neutral. | `crates/core/src/css_types.rs:1` (66 LOC, live-verified); `1C-runtime-evidence.md:72`; `1F-coherence-scan.md:80`; `1F-anti-pattern.md` css_types row; `restart/locks/LOCKS.md:349` (names the file verbatim). |
| CH2-V7-A04 | ACCEPT | The LOCKS:349 self-gate is correctly catalogued as FALSIFIED/RED (13 live sites vs asserted ZERO; 11 ir + 2 analysis), reclassified off impl-exceeds-spec. The analysis-crate leaks (`ast_utils/mod.rs:4,:11`) and ir siblings (`grammar_facts.rs:799`, `shape_dispatch/scalar.rs:17`) are all live-grounded. A generality gate that lies about its own coverage — exactly a CH2 leak that must not pass uncited. | live `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = 13; `crates/analysis/src/state/ast_utils/mod.rs:4,:11`; `restart/locks/LOCKS.md:349` (command asserts ZERO); `1F-coherence-scan.md:86,:103` (COH18-012); `1E-locks-evidence.md:94,:118`. |
| CH2-V7-A05 | ACCEPT | 1D separates JSON/CSS-empirical (J/C rows) from grammar-neutral (G rows) per Lock 14; the G-10 94.1%/79.5% figures are correctly re-scoped CSS-EMPIRICAL (decision-rule neutral, ratio CSS), with the `grep -v css = empty` leaf census verified live. No grammar-neutral substrate fact catalogued JSON-only; no CSS-empirical ratio laundered as fleet-neutral. | `1D-skinny-lessons.md:177-179` (separation header), `:206` (G-10, CH2-V3-009); live `find_component_delim | grep -v css` = empty (leaf in 7 css_l4 replicas only). |
| CH2-V7-A06 | ACCEPT | The grammar-NEUTRAL 5-shape `select_lowering`/`BackendShape` discriminator (G-3) and the Lock-1 substrate-union (G-1) are correctly NOT scoped JSON-only: the substrate-union (`Tape`/`ValueRef`/`PayloadArena`) is the shared foundation CSS borrows (1A-SUB-024 confirms CSS reuses the same `Tape`, no second substrate). The `<G:EventGrammar>` phantom is correctly excluded from the union claim and routed to G4-DELETE while the real `K`=Kind axis is PRESERVED. | `1D-skinny-lessons.md:197,:199` (G-1/G-3 neutral); `1F-past-corpora.md:29` (substrate union CLEAN); `1A-substrate-evidence.md:95` (1A-SUB-023 phantom-`<G>` DELETE, K-axis preserved), `1A-SUB-024` (CSS reuses same Tape); SK-V18 `SPEC.md:1202-1207`. |
| CH2-V7-A07 | ACCEPT | The totality-tree `OnceCell<StructuralIndex>` probe is flagged across 1F-anti-pattern (`:44`), 1F-coherence (COH18-015), and 1E as a grammar-fleet-WIDE carry (8 of 9 generated grammars, math excepted), with crate attribution and the skinny-asymmetry "functionally parallel" correction; classified per Lock 1, not catalogued JSON-only. The breadth is live-exact in the totality tree (math=0). | live `ensure_structural_index` present 8/9 in `crates/core/src/grammar/generated/` (math 0); `json.rs:701,:719,:732`; `support.rs:67`; `1F-anti-pattern.md:44`; `1F-coherence-scan.md` COH18-015. |
| CH2-V7-A08 | ACCEPT | Skinny generic-codegen grammar-named modules (`json_sink_direct`, `json_typed_direct`) and the `json_templates/` directory are flagged in 1F-anti-pattern (`:63,:64`) as Lock-14-(a) module/directory leaks and cross-cited in 1B/1C. These are UNGATED always-compiled modules; the `json_templates/` directory exists with all 6 files. Grammar-name leak does not pass uncited. | live `skinny/crates/codegen/src/lib.rs:4` `mod json_sink_direct;`, `:5` `mod json_typed_direct;` (both ungated); `json_templates/` = {config,generated,parser,value,view,visitor}.rs; `1F-anti-pattern.md:63,64`. |
| CH2-V7-A09 | ACCEPT | The runtime witness grammar-named modules `runtime/src/lib.rs:34` (`json_event_grammar_witness`), `:38` (`sheets_witness`) are `#[cfg(any(test, feature = "proof"))]`-gated test/proof witnesses, cited at FILE level across the 1A/1C/1D rows and fully DELETE-dispositioned (SK-V18 G4 DELETEs the `EventGrammar` axis + the `*EventGrammar` witnesses). Carried ACCEPT from V6 CH2-V6-A08 under the proportionality mandate: a T-P2 reader gets the complete leak picture and its disposition from the existing rows. | live `skinny/crates/runtime/src/lib.rs:34,:38` (both cfg(test,proof)-gated, attrs `:33,:36-37`); file-level cites at `1A-substrate-evidence.md:95` (1A-SUB-023), `1C-runtime-evidence.md`, `1D-skinny-lessons.md`; SK-V18 `SPEC.md:1202-1207` (G4 DELETE). |

## Proportionality Note (sub-threshold items examined, judged ACCEPT)

One precision nit was examined and judged BELOW the misleading threshold (ACCEPT,
not REVISE), consistent with the V6 proportionality mandate:

- `1F-past-corpora.md:82` cites the relocated-seam table as
  `crates/ir/src/registry/strategy.rs:137-155`, an abbreviated span covering only the
  FIRST 4 of the 9 rows (the same 4 the narrow regex catches). This is the
  non-authoritative do-not-re-derive ledger (its own header `:7,:13-14` names
  `1F-coherence-scan.md` as the "authoritative live coherence packet"), and the cell
  explicitly cross-references `(COH18-005/006)` — where the FULL `:137-185` 9-row
  enumeration with "9-grammar-WIDE" is stated. The cell's own prose carries the
  generality ("structural-collapse obligation" over ALL rows, "totality mirror of the
  skinny `RuntimeTarget` R16 collapse"). It is a pointer-span to the table head, not a
  claim the table ends at `:155`; a T-P2 reader following the cross-ref reaches the full
  9-grammar breadth. This is the same abbreviated-span pattern V6 ACCEPTED in COH18-005's
  divergence-table summary cell (`1F-coherence:98`). Below threshold — ACCEPT.

## REJECT Gate

No GENUINE reject. Per the corrected V7 convention, a REJECT requires an inventory to
state something FALSE ON DISK that a load-bearing finding rests on, plus a live
falsifying citation. The only on-disk-false token flagged across the entire CH2 history
— the stale "5" at `1F-anti-pattern.md:65` (CH2-V6-R01) — is now FOLDED to the correct
"4"; no false token remains. I checked whether any cited LOC / symbol / path:line is
fabricated or recalled across the load-bearing CH2 rows and found NONE — every figure
(the 9 idents rows, the 4-catch / 5-escape split, the 13-site self-gate, the 8-of-9
OnceCell breadth, the css_types.rs 66 LOC, the analysis-crate doc-comment leaks, the
empty `grep -v css` leaf census) matches disk. That is an ACCEPT of the inventories'
correctness, recorded reject=0. Recording reject=N here to mean "I rejected my own
hypothesis" would invert the gate; the honest tally is reject=0.

## Required Fold

NONE. CH2 V7 is CLEAN — zero REVISE, zero REJECT. The single V6 orphan (CH2-V6-R01) is
confirmed FOLDED into `1F-anti-pattern.md:65`.

Preserve all V7 ACCEPT surfaces: the 9-row idents-table relocated-seam (COH18-005 /
D-1E-V5-14) with the corrected 4-catch/5-escape count now consistent across BOTH live 1F
files, the lock-NAMED `css_types.rs` mess (COH18-006), the RED LOCKS:349 self-gate
(COH18-012, CH2-V4-005/007) with the 13-site / analysis-crate grounding, the 1D
JSON/CSS-empirical vs grammar-neutral separation with the G-10 CSS-scoped ratio, the
grammar-neutral substrate-union and 5-shape `BackendShape` discriminator, the 8-of-9
totality-tree OnceCell probe breadth (CH5-V3-003), the functionally-parallel scanner
correction (CH5-V4-011), the UNGATED codegen json-module + `json_templates/` directory
leaks, and the cfg-gated DELETE-bound runtime witness modules (CH2-V6-A08 / CH2-V7-A09).
Do not broaden any CSS-scoped or JSON-scoped finding into fleet-wide generality, and do
not narrow the grammar-neutral substrate or decision-discriminator into a JSON-only
lesson.

## Governance Note

This is the FIRST clean CH2 cycle (V6 was REVISE). It does NOT by itself satisfy the
§3Z two-consecutive-clean lock; one further clean confirming CH2 cycle is required to
reach the fixed point. No new evidence defect was found; the inventories are sound under
the GENERALITY lens.

TALLY accept=9 revise=0 reject=0
