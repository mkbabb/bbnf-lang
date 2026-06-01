# SK-V18 T-P1 V8 (cycle V8) — CH2 GENERALITY

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

This V8 cycle is the SECOND consecutive drive toward the two-clean fixed point.
V7 CH2 (`hardening/V7/CH2.md`) was the FIRST clean CH2 cycle (accept=9 revise=0
reject=0), confirming the V6 REVISE (CH2-V6-R01: the stale narrow-regex catch
count "5" at `1F-anti-pattern.md:65`) had been FOLDED to "4". V8's duties: (1)
confirm that fold still persists at HEAD (the dirty tree shows `1F-anti-pattern.md`
modified in `git status`, so re-grounding the fold is mandatory, not assumable);
(2) re-ground every load-bearing Lock-14/generality citation against live code;
(3) sweep for any NEW grammar-name leak in a generic crate that no inventory
cites; (4) catch any GENUINE reject. Per the corrected REJECT convention, a
REJECT is admissible ONLY when an inventory STATES SOMETHING FALSE ON DISK with a
live falsifying citation; a self-falsified suspicion is an ACCEPT recorded
reject=0.

## Verdict

ACCEPT (CLEAN). The V6→V7 fold (CH2-V6-R01) PERSISTS verbatim at
`1F-anti-pattern.md:65`: "catches only **4** of the 9 idents rows
(`:137,:143,:149,:155`; per CH2-V3-008, superseding the CH2-V2-009 '5' wording);
the other 5 (`:161,:167,:173,:179,:185`) escape." Both LIVE 1F files agree on 4.
Nine accept; zero revise; zero reject. The grammar-neutrality skeleton is sound;
every load-bearing Lock-14 leak row re-verifies against live code to the line; no
grammar-neutral substrate fact is catalogued JSON/CSS-only; no CSS-empirical
ratio is laundered fleet-neutral; and an exhaustive match-arm sweep over the
generic crates finds ZERO uncited grammar-name arms — every grammar-name surface
in a generic crate is accounted for. No GENUINE reject: nothing in the
inventories states a load-bearing falsehood on disk.

This is the SECOND consecutive clean CH2 cycle (V7 was the first). With V7 it
satisfies the §3Z two-consecutive-clean fixed point for the CH2 GENERALITY lens.

## Spot-Verification of the Most Load-Bearing Cited Rows

Every primary CH2/Lock-14 citation re-grounded on the dirty-tree HEAD:

- `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE` — VERIFIED:
  exactly NINE grammar-named `idents` rows at `:137,:143,:149,:155,:161,:167,
  :173,:179,:185` (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty);
  `rg -n 'idents:' strategy.rs` shows the 9 rows plus the `:318` struct-field
  declaration (`pub idents: &'static [&'static str],`), NOT a tenth row. Table
  opens `:134` `pub const PRODUCTION_MANIFEST_TABLE: &[ManifestStrategyEntry]`;
  consumer `Self::for_grammar_with_manifest(grammar_ident, registry,
  PRODUCTION_MANIFEST_TABLE)` at `:216`. The 9-grammar-WIDE relocated-seam framing
  in `1F-coherence-scan.md:79`, `1F-anti-pattern.md:65`, `1C` D9 cross-ref,
  `1E:94,:118`, COH18-005/012, and the `1F-past-corpora.md:82` ledger pointer is
  exact.
- Narrow-regex catch breadth — VERIFIED: `rg 'JsonParser|CssL4Parser|
  BbnfBootstrap|GoogleSheetsParser' crates/ir/src/registry/strategy.rs | grep
  'idents:'` matches EXACTLY the four rows `:137,:143,:149,:155`; the other five
  (`:161,:167,:173,:179,:185`) escape. The TRUE catch count is **4**. BOTH live
  1F files carry **4**: `1F-anti-pattern.md:65` (CH2-V6-R01 fold HOLDS at the
  dirty HEAD) and `1F-coherence-scan.md:79,:86`. The two files do not contradict;
  no stale standalone "5"-as-catch-count survives in `p1/*.md`.
- `crates/core/src/css_types.rs:1` — VERIFIED verbatim: "//! Host shims for the
  CSS L4 grammar's `-> parse_hex_color(...)` map.", 66 LOC, in `crates/core/src/`
  (NOT a `crates/<grammar>/` declaration crate). `restart/locks/LOCKS.md:349`
  names "`crates/core/src/css_types.rs`" VERBATIM in its own "current overfitting
  mess" enumeration. 1C D9 / COH18-006 / `1F-anti-pattern.md:66` /
  D-1E-V5-14 Lock-14-(c)-does-not-apply disposition correct (the lock admits only
  a separate `crates/<grammar>/` declaration crate; `css_types.rs` is in the
  generic core crate).
- LOCKS:349 self-gate falsification — VERIFIED: `rg 'JsonParser|CssL4Parser|
  BbnfBootstrap|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = **13**
  matching lines (11 ir + 2 analysis), while `LOCKS.md:349`'s own verification
  command asserts it "returns ZERO". The self-gate is RED. Analysis-crate leaks
  live-confirmed: `crates/analysis/src/state/ast_utils/mod.rs:4`
  (`BbnfBootstrapNodeView`), `:11` (`BbnfBootstrapRuleKind`). `1E:94,:118`,
  `1F-coherence COH18-012`, and `1C` C3 correct; reclassification OFF
  impl-exceeds-spec correct. A generality gate that lies about its own coverage —
  exactly a CH2 leak that must not pass uncited.
- Totality `OnceCell<StructuralIndex>` probe breadth — VERIFIED in the CORRECT
  tree (`crates/core/src/grammar/generated/`, NOT the skinny tree): per-grammar
  `rg -c 'fn ensure_structural_index'` = 1 for json/bnf/ebnf/csv/google_sheets/
  css_pretty/bbnf/css_l4, and **0 for math** — exactly **8 of 9**. `json.rs:701`
  `pub(crate) structural_index: ::core::cell::OnceCell<`, `:719` `pub(crate) fn
  ensure_structural_index<'a>(`, `:732` `::simd_scan::scan_structural(input,
  &alphabet)`; emitter diction `crates/core/src/backend/rust/emitter/shapes/
  dispatcher/support.rs:67` "The probe substrate (OnceCell + helper) emits". The
  "8 of 9, all but math" breadth (CH5-V3-003) carried at `1F-anti-pattern.md:44`
  and `1F-coherence COH18-015` is exact; classified per Lock 1, not catalogued
  JSON-only.
- Scanner-crate asymmetry (COH18-015) — VERIFIED: `crates/simd-scan/src/lib.rs:68`
  `pub use index::{StructuralIndex, next_structural_at_or_after};`; skinny
  `bbnf-simd` has ZERO `next_structural_at_or_after` (`rg -c ...
  skinny/crates/bbnf-simd/src/lib.rs` = 0) yet exposes a full `StructuralIndex`
  (`:72` struct, `:78` `from_positions`, `:82` `positions`, `:94` `parity_hash`,
  `:106` `scan_dispatch`, `:126` `scan_scalar`). The "FUNCTIONALLY PARALLEL,
  divergent APIs" framing (CH5-V4-011) is grammar-neutral and exact; the
  renamed-parallel-scanner risk is correctly ACTIVE-not-one-sided.
- 1D JSON/CSS-empirical vs grammar-neutral separation — VERIFIED: the `:177-179`
  separation header splits J/C rows (`:185-191`) from G rows (`:197-209`) per
  Lock 14; G-10 (`1D:206`) re-scopes the 94.1%/79.5% figures as CSS-EMPIRICAL
  (decision-RULE kept neutral, ratio kept CSS); the leaf census `rg
  find_component_delim skinny/crates/runtime/src | grep -v css` is EMPTY (leaf in
  the 7 css_l4 replicas only, zero non-CSS callers). The grammar-NEUTRAL Lock-1
  substrate-union (G-1, `1D:197`) and the 5-shape `BackendShape`/`select_lowering`
  discriminator (G-3, `1D:199`) are NOT scoped JSON-only.
- Phantom `<G:EventGrammar>` vs real `K`=Kind axis — VERIFIED:
  `skinny/crates/runtime/src/tape/mod.rs:175` `pub struct ValueRef<'doc, 'input:
  'doc, K = AnyKind, G: EventGrammar = AnyGrammar>`; `:178` `_kind: PhantomData<fn()
  -> K>` (REAL axis, PRESERVED); `:179` `_grammar: PhantomData<fn() -> G>`
  (decorative). SK-V18 SPEC G4 DELETE anchor `sk-v18/SPEC.md:99-102` DELETEs the
  `<G>` axis "(preserving the REAL `K=Kind` axis untouched)". 1A-SUB-023 / D5 /
  COH18-008 / `1F-anti-pattern.md:43` / 1D G-8 exact; the phantom is correctly
  excluded from the Lock-1 substrate-union claim.
- Runtime witness + codegen module census — VERIFIED: `skinny/crates/runtime/
  src/lib.rs:34` `pub mod json_event_grammar_witness;` and `:38` `pub mod
  sheets_witness;` are BOTH `#[cfg(any(test, feature = "proof"))]`-gated (attrs at
  `:32`,`:36`). The UNGATED codegen modules `skinny/crates/codegen/src/lib.rs:4`
  `mod json_sink_direct;`, `:5` `mod json_typed_direct;` and the `json_templates/`
  directory (6 files: config/generated/parser/value/view/visitor) are census'd at
  `1F-anti-pattern.md:63,:64` as Lock-14-(a) module/directory leaks. The
  gated-vs-ungated distinction is exact.
- 7 byte-identical css_l4 replicas (R4) — VERIFIED: `md5 -q` over the 7
  `css_l4_*/generated.rs` files = 7× `b654562ccff46ed62dd48e9ace325830` (single
  uniq class). The replica-overfit framing in 1C C10/D3, `1F-anti-pattern.md:45`,
  1D C-1 is exact. The 94.1% CSS-empirical leaf is scoped to these replicas only.

Not one recalled LOC, fabricated symbol, or false path:line was found among the
load-bearing CH2 rows. Citation discipline remains near-exhaustive.

## Generality Sweep (Lock-14 leak completeness — the one duty my lens must not skip)

To certify "no grammar-name leak passes uncited," I ran the LOCKS:349 match-arm
gate independently over the generic crates:

- `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*
  \s*=>' crates/{ir,parse,codegen,analysis,path,egraph}/src` = **0 matches**, and
  `rg -nE 'CssL4\s*=>|GoogleSheets\w*\s*=>|Bbnf\w*\s*=>|Json\s*=>'
  crates/{ir,codegen,analysis}/src` = **0**. There is NO uncited grammar-name
  `match` arm in any generic crate. The ONLY grammar-name surfaces in generic
  crates are the already-catalogued `PRODUCTION_MANIFEST_TABLE` `idents`
  data-table rows (caught ONLY structurally — the relocated-seam, exactly as
  COH18-005/012 and `1F-anti-pattern.md:65` state) and the doc-comment leaks
  (`grammar_facts.rs:799`, `shape_dispatch/scalar.rs:17`, `ast_utils/mod.rs:4,:11`),
  all enumerated. The inventories' claim that the leak is grammar-WIDE and
  arm-grep-invisible is confirmed: no new arm exists for the regex to miss either.

This closes the only residual risk for a GENERALITY lens — that some grammar-name
leak in a generic crate sits outside the catalogued surface. None does.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V8-A01 | ACCEPT (re-confirms V7 CH2-V7-A01 — FOLD PERSISTS) | The V6→V7 narrow-regex catch-count fold HOLDS at the dirty HEAD despite `1F-anti-pattern.md` being modified in `git status`. `:65` carries "catches only **4** of the 9 idents rows (`:137,:143,:149,:155`; per CH2-V3-008, superseding the CH2-V2-009 '5' wording); the other 5 (`:161,:167,:173,:179,:185`) escape." Both LIVE 1F files agree on 4; no stale "5"-as-catch-count survives in `p1/*.md`. A T-P2 reader cross-referencing the two 1F packets is not misled. | live `1F-anti-pattern.md:65` (fold text intact) ∧ `1F-coherence-scan.md:79,:86`; falsifier `rg ... strategy.rs \| grep 'idents:'` = `:137,:143,:149,:155` (4). |
| CH2-V8-A02 | ACCEPT | The relocated-seam analog is correctly catalogued as a 9-grammar-WIDE Lock-14 leak in the generic `ir` crate, with table open (`:134`), consumer anchor (`:216`), and the narrow-regex blind-spot named. The 9-row breadth is live-exact (9 `idents` rows `:137-185`; `:318` is the field decl). The independent match-arm sweep confirms NO other generic-crate grammar-name arm exists, so the arm-grep-invisible characterization is sound. Grammar-name leak does NOT pass uncited. | `crates/ir/src/registry/strategy.rs:134,:137-185,:216,:318` (live-verified); `1F-coherence-scan.md:79`; `1F-anti-pattern.md:65`; `1C-runtime-evidence.md:72`; `1E-locks-evidence.md:94,:118`; my match-arm sweep = 0. |
| CH2-V8-A03 | ACCEPT | The lock-NAMED `css_types.rs` host shim is flagged in 1C (D9), 1F-coherence (COH18-006), `1F-anti-pattern.md:66`, and 1E (D-1E-V5-14) as a grammar-named MODULE in the generic core crate; Lock-14-(c) correctly held NOT to apply (it admits only a separate `crates/<grammar>/` declaration crate). LOCKS.md:349 names the file verbatim. Not catalogued JSON-only; the SK-V19 relocate-or-delete disposition is grammar-neutral. | `crates/core/src/css_types.rs:1` (66 LOC, live-verified); `1C-runtime-evidence.md:72`; `1F-coherence-scan.md:80`; `1F-anti-pattern.md:66`; `restart/locks/LOCKS.md:349` (names the file verbatim). |
| CH2-V8-A04 | ACCEPT | The LOCKS:349 self-gate is correctly catalogued as FALSIFIED/RED (13 live sites vs asserted ZERO; 11 ir + 2 analysis), reclassified off impl-exceeds-spec. The analysis-crate leaks (`ast_utils/mod.rs:4,:11`) and ir siblings are all live-grounded. A generality gate that lies about its own coverage — exactly a CH2 leak that must not pass uncited. | live `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = 13; `crates/analysis/src/state/ast_utils/mod.rs:4,:11`; `restart/locks/LOCKS.md:349` (command asserts ZERO); `1F-coherence-scan.md:86,:103` (COH18-012); `1E-locks-evidence.md:94,:118`. |
| CH2-V8-A05 | ACCEPT | 1D separates JSON/CSS-empirical (J/C rows `:185-191`) from grammar-neutral (G rows `:197-209`) per Lock 14; the G-10 94.1%/79.5% figures are correctly re-scoped CSS-EMPIRICAL (decision-rule neutral, ratio CSS), with `grep -v css = empty` leaf census verified live. No grammar-neutral substrate fact catalogued JSON-only; no CSS-empirical ratio laundered as fleet-neutral. | `1D-skinny-lessons.md:177-179` (separation header), `:206` (G-10); live `find_component_delim \| grep -v css` = empty (leaf in 7 css_l4 replicas only). |
| CH2-V8-A06 | ACCEPT | The grammar-NEUTRAL Lock-1 substrate-union (G-1) and the 5-shape `BackendShape`/`select_lowering` discriminator (G-3) are correctly NOT scoped JSON-only: the substrate-union (`Tape`/`ValueRef`/`PayloadArena`) is the shared foundation CSS borrows. The `<G:EventGrammar>` phantom is correctly EXCLUDED from the union claim and routed to G4-DELETE while the real `K`=Kind axis is PRESERVED (`tape/mod.rs:178` `_kind` vs `:179` `_grammar`). | `1D-skinny-lessons.md:197,:199`; `1F-past-corpora.md:29` (substrate union CLEAN); `skinny/crates/runtime/src/tape/mod.rs:175,:178,:179`; SK-V18 `SPEC.md:99-102` (G4 DELETE, K preserved). |
| CH2-V8-A07 | ACCEPT | The totality-tree `OnceCell<StructuralIndex>` probe is flagged across 1F-anti-pattern (`:44`), 1F-coherence (COH18-015), and 1E as a grammar-fleet-WIDE carry (8 of 9 generated grammars, math excepted), with crate attribution (`OnceCell` in the `crates/core` consumer, not `simd-scan`) and the skinny-asymmetry "functionally parallel" correction; classified per Lock 1, not catalogued JSON-only. The breadth is live-exact in the totality tree (`ensure_structural_index` = 8/9, math=0). | live per-grammar `ensure_structural_index` count = 8/9 (math 0) in `crates/core/src/grammar/generated/`; `json.rs:701,:719,:732`; `support.rs:67`; `crates/simd-scan/src/lib.rs:68`; `1F-anti-pattern.md:44`; `1F-coherence-scan.md` COH18-015. |
| CH2-V8-A08 | ACCEPT | Skinny generic-codegen grammar-named modules (`json_sink_direct`, `json_typed_direct`) and the `json_templates/` directory are flagged in 1F-anti-pattern (`:63,:64`) as Lock-14-(a) module/directory leaks and cross-cited in 1B/1C. These are UNGATED always-compiled modules; the `json_templates/` directory exists with all 6 files (config/generated/parser/value/view/visitor). Grammar-name leak does not pass uncited. | live `skinny/crates/codegen/src/lib.rs:4` `mod json_sink_direct;`, `:5` `mod json_typed_direct;` (both ungated, no cfg attr); `json_templates/` = 6 files; `1F-anti-pattern.md:63,64`. |
| CH2-V8-A09 | ACCEPT | The runtime witness grammar-named modules `runtime/src/lib.rs:34` (`json_event_grammar_witness`), `:38` (`sheets_witness`) are `#[cfg(any(test, feature = "proof"))]`-gated test/proof witnesses (attrs `:32`,`:36`), cited at FILE level across the 1A/1C/1D rows and fully DELETE-dispositioned (SK-V18 G4 DELETEs the `EventGrammar` axis + the `*EventGrammar` witnesses). Carried ACCEPT from V7 CH2-V7-A09 under the proportionality mandate: a T-P2 reader gets the complete leak picture and its disposition from the existing rows. | live `skinny/crates/runtime/src/lib.rs:34,:38` (both cfg(test,proof)-gated, attrs `:32,:36`); file-level cites at `1A-substrate-evidence.md:95`, `1C-runtime-evidence.md`, `1D-skinny-lessons.md`; SK-V18 `SPEC.md:99-102` (G4 DELETE). |

## Proportionality Note (sub-threshold item examined, judged ACCEPT)

One precision nit was re-examined and judged BELOW the misleading threshold
(ACCEPT, not REVISE), consistent with the V6/V7 proportionality mandate:

- `1F-past-corpora.md:82` cites the relocated-seam table as
  `crates/ir/src/registry/strategy.rs:137-155`, an abbreviated span covering only
  the FIRST 4 of the 9 rows (the same 4 the narrow regex catches). This is the
  non-authoritative do-not-re-derive ledger (its own header `:7,:13-14` names
  `1F-coherence-scan.md` as the "authoritative live coherence packet"), and the
  cell explicitly cross-references `(COH18-005/006)` — where the FULL `:137-185`
  9-row enumeration with "9-grammar-WIDE" is stated. The cell's own prose carries
  the generality ("structural-collapse obligation" over ALL rows, "totality mirror
  of the skinny `RuntimeTarget` R16 collapse"). It is a pointer-span to the table
  head, not a claim the table ends at `:155`; a T-P2 reader following the cross-ref
  reaches the full 9-grammar breadth. This is the same abbreviated-span pattern V7
  ACCEPTED (V7 Proportionality Note). My independent judgment confirms V7's
  reasoning. Below threshold — ACCEPT.

## REJECT Gate

No GENUINE reject. Per the corrected V8 convention, a REJECT requires an inventory
to state something FALSE ON DISK that a load-bearing finding rests on, plus a live
falsifying citation. The only on-disk-false token ever flagged across the entire
CH2 history — the stale "5" at `1F-anti-pattern.md:65` (CH2-V6-R01) — was folded to
"4" in V7 and PERSISTS folded at the dirty HEAD; no false token remains. I checked
whether any cited LOC / symbol / path:line is fabricated or recalled across the
load-bearing CH2 rows and found NONE — every figure (the 9 idents rows, the 4-catch
/ 5-escape split, the 13-site self-gate, the 8-of-9 OnceCell breadth, the
css_types.rs 66 LOC, the analysis-crate doc-comment leaks, the empty `grep -v css`
leaf census, the 7× `b654562c` md5, the phantom-`<G>`/real-`K` axis split) matches
disk. Additionally, my independent match-arm sweep found ZERO uncited grammar-name
arms in the generic crates — there is no leak the inventories miss. That is an
ACCEPT of the inventories' correctness, recorded reject=0. Recording reject=N here
to mean "I rejected my own hypothesis" would invert the gate; the honest tally is
reject=0.

## Required Fold

NONE. CH2 V8 is CLEAN — zero REVISE, zero REJECT. The single historical orphan
(CH2-V6-R01) remains FOLDED into `1F-anti-pattern.md:65`.

Preserve all V8 ACCEPT surfaces: the 9-row idents-table relocated-seam
(COH18-005 / D-1E-V5-14) with the 4-catch/5-escape count consistent across BOTH
live 1F files, the lock-NAMED `css_types.rs` mess (COH18-006), the RED LOCKS:349
self-gate (COH18-012, CH2-V4-005/007) with the 13-site / analysis-crate grounding,
the 1D JSON/CSS-empirical vs grammar-neutral separation with the G-10 CSS-scoped
ratio, the grammar-neutral substrate-union (G-1) and 5-shape `BackendShape`
discriminator (G-3), the 8-of-9 totality-tree OnceCell probe breadth (CH5-V3-003),
the functionally-parallel scanner correction (CH5-V4-011), the UNGATED codegen
json-module + `json_templates/` directory leaks, and the cfg-gated DELETE-bound
runtime witness modules (CH2-V7-A09 / CH2-V8-A09). Do not broaden any CSS-scoped
or JSON-scoped finding into fleet-wide generality, and do not narrow the
grammar-neutral substrate or decision-discriminator into a JSON-only lesson.

## Governance Note

This is the SECOND consecutive clean CH2 cycle (V7 was the first; V6 was REVISE).
Together with V7 it satisfies the §3Z two-consecutive-clean fixed point for the
CH2 GENERALITY lens. No new evidence defect was found; the inventories are sound
under the GENERALITY lens, and the match-arm completeness sweep confirms no
grammar-name leak escapes the catalogue.

TALLY accept=9 revise=0 reject=0
