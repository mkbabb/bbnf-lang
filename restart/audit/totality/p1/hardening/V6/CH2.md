# SK-V18 T-P1 V6 (cycle V6) — CH2 GENERALITY

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

This V6 cycle drives out the RESIDUAL precision REVISEs toward a two-consecutive-clean
fixed point. The inventories were re-baselined under SK-V18 in commit `097c4dd90`
("totality evidence inventories 1A-1F (V1-V5 hardened, near-converged)"); the V5 CH2
verdict (`hardening/V5/CH2.md`) recorded two REVISE folds (CH2-V5-R01, CH2-V5-R02).
V6's first duty is to confirm whether those folds landed; its second is to catch any
GENUINE reject. Per the corrected REJECT convention, a REJECT is admissible ONLY when
an inventory STATES SOMETHING FALSE ON DISK with a live falsifying citation; a
self-falsified suspicion is an ACCEPT.

## Verdict

REVISE. One residual precision REVISE survives from V5 unfolded (CH2-V6-R01: the
1F-anti-pattern narrow-regex catch count is still the stale **5**, contradicting its
own authoritative companion's **4**); eight accept. The grammar-neutrality skeleton is
sound, every load-bearing Lock-14 leak row re-verifies against live code to the line,
and the V5 CH2-V5-R02 census-completeness item is re-judged ACCEPT under the
proportionality mandate (substance exhaustively cited and DELETE-dispositioned; below
the misleading threshold). No GENUINE reject: nothing in the inventories states a
load-bearing falsehood on disk.

## Spot-Verification of the Most Load-Bearing Cited Rows

Every primary CH2/Lock-14 citation re-grounded on the dirty-tree HEAD:

- `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE` — VERIFIED: exactly
  NINE grammar-named `idents` rows at `:137,:143,:149,:155,:161,:167,:173,:179,:185`
  (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty); the `:318` `idents:`
  hit is the struct field declaration, NOT a tenth row. Consumer
  `for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)` at
  `:216`. The "relocated-seam analog … 9-grammar-WIDE" framing in
  `1F-coherence-scan.md:79`,`1F-anti-pattern.md:65`, `1C` D9 cross-ref, and `1E`
  D-1E-V5-14 is exact.
- Narrow-regex catch breadth — VERIFIED: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
  crates/ir/src/registry/strategy.rs | grep 'idents:'` matches EXACTLY the four rows
  `:137,:143,:149,:155`; the other five (`:161,:167,:173,:179,:185` —
  Csv/Math/Bnf/Ebnf/CssPretty) escape. The TRUE catch count is **4**. The authoritative
  `1F-coherence-scan.md:79,:86` carry **4** with the explicit CH2-V3-008 note. The
  companion `1F-anti-pattern.md:65` still carries **5** — see CH2-V6-R01.
- `crates/core/src/css_types.rs:1` — VERIFIED verbatim: "Host shims for the CSS L4
  grammar's `-> parse_hex_color(...)` map.", 66 LOC, in `crates/core/src/` (NOT a
  `crates/<grammar>/` declaration crate). LOCKS.md:349 names "`crates/core/src/css_types.rs`"
  in its own "current overfitting mess" enumeration. 1C D9 / COH18-006 / D-1E-V5-14
  Lock-14-(c)-does-not-apply disposition correct.
- LOCKS:349 self-gate falsification — VERIFIED: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
  crates/ir/src/ crates/analysis/src/` = **13** sites (11 ir + 2 analysis), while
  LOCKS.md:349's own verification command asserts it "returns ZERO." The self-gate is
  RED. `1E:94,:118` (D-1E-V5-14, CH2-V4-005/007) and `1F-coherence COH18-012` correct;
  reclassification OFF impl-exceeds-spec correct.
- Totality `OnceCell<StructuralIndex>` probe breadth — VERIFIED: `fn ensure_structural_index`
  present in exactly 8 of 9 generated grammars (json/css_l4/css_pretty/csv/ebnf/bnf/
  google_sheets/bbnf), `math.rs` = 0. The "8 of 9, all but math" breadth (CH5-V3-003) is
  exact. `1F-anti-pattern.md:44` carries it correctly.
- 1D JSON/CSS-empirical vs grammar-neutral separation — VERIFIED: G-10 (`1D:206`)
  re-scopes the 94.1%/79.5% figures as CSS-EMPIRICAL (decision-RULE kept neutral, ratio
  kept CSS); the leaf census `rg find_component_delim skinny/crates/runtime/src | grep -v css`
  is EMPTY (leaf appears only in the 7 css_l4 replicas, zero non-CSS callers). The
  grammar-NEUTRAL 5-shape `select_lowering` discriminator (`1D:64`) and the Lock-1
  substrate-union (`1F-past-corpora:29`) are NOT scoped JSON-only.
- Runtime witness grammar-named modules — VERIFIED: `skinny/crates/runtime/src/lib.rs:33-34`
  `#[cfg(any(test, feature = "proof"))] pub mod json_event_grammar_witness;` and `:37-38`
  `#[cfg(any(test, feature = "proof"))] pub mod sheets_witness;` — BOTH cfg(test/proof)-gated.
  Contrast the codegen modules `1F-anti-pattern.md:63,64` DOES census
  (`codegen/src/lib.rs:4,5` `mod json_sink_direct;`/`mod json_typed_direct;`), which are
  UNGATED. See CH2-V6-A08.

Not one recalled LOC, fabricated symbol, or false path:line was found among the
load-bearing CH2 rows. Citation discipline remains near-exhaustive.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V6-A01 | ACCEPT | The relocated-seam analog is correctly catalogued as a 9-grammar-WIDE Lock-14 leak in the generic `ir` crate, with consumer anchor (`:216`) and the narrow-regex blind-spot named. The 9-row breadth is live-exact (9 `idents` rows; `:318` is the field decl, not a row). Grammar-name leak does NOT pass uncited. | `crates/ir/src/registry/strategy.rs:137-185` (9 idents rows, live-verified), `:216` consumer; `1F-coherence-scan.md:79`; `1F-anti-pattern.md:65`; `1C-runtime-evidence.md:72`; `1E-locks-evidence.md:118`. |
| CH2-V6-A02 | ACCEPT | The lock-NAMED `css_types.rs` host shim is flagged in 1C (D9), 1F-coherence (COH18-006), 1F-anti-pattern, and 1E (D-1E-V5-14) as a grammar-named MODULE in the generic core crate; Lock-14-(c) correctly held NOT to apply (it admits only a separate `crates/<grammar>/` declaration crate). LOCKS.md:349 names the file verbatim. Not catalogued JSON-only; the SK-V19 relocate-or-delete disposition is grammar-neutral. | `crates/core/src/css_types.rs:1` (66 LOC, live-verified); `1C-runtime-evidence.md:72`; `1F-coherence-scan.md:80`; `1F-anti-pattern.md` css_types row; `restart/locks/LOCKS.md:349` (names the file verbatim). |
| CH2-V6-A03 | ACCEPT | The LOCKS:349 self-gate is correctly catalogued as FALSIFIED/RED (13 live sites vs asserted ZERO), reclassified off impl-exceeds-spec. A generality gate that lies about its own coverage — exactly a CH2 leak that must not pass uncited. | live `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = 13; `restart/locks/LOCKS.md:349` (command asserts ZERO); `1F-coherence-scan.md:86` (COH18-012); `1E-locks-evidence.md:94,:118`. |
| CH2-V6-A04 | ACCEPT | 1D separates JSON/CSS-empirical (J/C rows) from grammar-neutral (G rows) per Lock 14; the G-10 94.1%/79.5% figures are correctly re-scoped CSS-EMPIRICAL (decision-rule neutral, ratio CSS), with the `grep -v css = empty` leaf census verified live. No grammar-neutral substrate fact catalogued JSON-only; no CSS-empirical ratio laundered as fleet-neutral. | `1D-skinny-lessons.md:177` (separation header), `:206` (G-10, CH2-V3-009); live `find_component_delim \| grep -v css` = empty (leaf in 7 css_l4 replicas only). |
| CH2-V6-A05 | ACCEPT | The grammar-NEUTRAL 5-shape `select_lowering` discriminator and the Lock-1 substrate-union are correctly NOT scoped JSON-only: the discriminator matches over the 5 `BackendShape` variants with zero grammar names; the substrate-union (`Tape`/`ValueRef`/`PayloadArena`) is the shared foundation CSS borrows. The `<G:EventGrammar>` phantom is correctly excluded from the union claim and routed to G4-DELETE while the real `K`=Kind axis is PRESERVED. | `1D-skinny-lessons.md:64` (5-shape discriminator); `1F-past-corpora.md:29` (substrate union CLEAN); `1A-substrate-evidence.md:95` (1A-SUB-023, phantom-`<G>` DELETE, K-axis preserved); SK-V18 `SPEC.md:1203-1207,:1232-1236`. |
| CH2-V6-A06 | ACCEPT | The totality-tree `OnceCell<StructuralIndex>` probe is flagged across 1F-anti-pattern (`:44`), 1F-coherence (COH18-015), and 1E as a grammar-fleet-WIDE carry (8 of 9 generated grammars, math excepted), with crate attribution and the skinny-asymmetry "functionally parallel" correction; classified per Lock 1, not catalogued JSON-only. | live: `ensure_structural_index` present 8/9 (math 0); `1F-anti-pattern.md:44`; `1F-coherence-scan.md` COH18-015; `1E-locks-evidence.md`. |
| CH2-V6-A07 | ACCEPT | Skinny generic-codegen grammar-named modules (`json_sink_direct`, `json_typed_direct`) and the `json_templates/` directory are flagged in 1F-anti-pattern (`:63,:64`) as Lock-14-(a) module/directory leaks and cross-cited in 1B/1C. These are UNGATED always-compiled modules. Grammar-name leak does not pass uncited. | live `codegen/src/lib.rs:4` `mod json_sink_direct;`, `:5` `mod json_typed_direct;` (both ungated); `1F-anti-pattern.md:63,64`; `1C-runtime-evidence.md` C8. |
| CH2-V6-A08 | ACCEPT (re-judges V5 CH2-V5-R02 from REVISE) | The runtime witness grammar-named modules `runtime/src/lib.rs:33-34,:37-38` (`json_event_grammar_witness`, `sheets_witness`) are NOT enumerated as a distinct Lock-14-(a) module-declaration census row — but they are `#[cfg(any(test, feature = "proof"))]`-gated test/proof witnesses, already cited at FILE level across six inventory rows (1A-SUB-012/`:84`, 1A-SUB-015/`:87`, 1A-SUB-023/`:95`, 1A-DIV-007/`:110`, 1C D8/`:68-69`, 1D `:70`), and fully DELETE-dispositioned (SK-V18 G4 DELETEs the `EventGrammar` axis + the `*EventGrammar` witnesses; P4 fences `FORBIDDEN_GENERIC_TOKENS ⊇ {EventGrammar,*EventGrammar}`). A T-P2 reader gets the complete leak picture and its disposition from the existing rows; the missing module-declaration row would not mislead. Under the V6 proportionality mandate ("a nit is a REVISE only if it would mislead a T-P2 reader"), this is below threshold — ACCEPT, not REVISE. | live `skinny/crates/runtime/src/lib.rs:33-34,:37-38` (both cfg(test,proof)-gated); file-level cites at `1A-substrate-evidence.md:84,:87,:95,:110`, `1C-runtime-evidence.md:68-69`, `1D-skinny-lessons.md:70`; SK-V18 `SPEC.md:1203-1207,:1232-1236` (G4 DELETE), `:474,:711` (P4 FORBIDDEN_GENERIC_TOKENS ⊇ {EventGrammar,*EventGrammar}). |
| CH2-V6-R01 | REVISE | `1F-anti-pattern.md:65` STILL carries the FALSIFIED narrow-regex count flagged in V5 as CH2-V5-R01: it states the strict 4-name leak regex "catches only **5** ident sites (CH2-V2-009 — the leak is 9-grammar-wide, NOT 4)." Live verification proves the regex catches exactly **4** idents rows (`:137,:143,:149,:155`). The authoritative companion `1F-coherence-scan.md:79,:86` already carries the corrected **4** with the explicit CH2-V3-008 note "replacing the carried CH2-V2-009 'catches only 5 ident sites' wording." The SK-V18 re-baseline (`097c4dd90`) folded the V1-V5 hardening into 1F-coherence but did NOT propagate the CH2-V3-008 / CH2-V5-R01 correction into the non-superseded companion 1F-anti-pattern, leaving the two LIVE 1F files mutually contradictory on a CH2 generality count (4 vs 5). A T-P2 reader cross-referencing the two 1F packets is misled about the regex catch count — above threshold. CORRECTION: change `1F-anti-pattern.md:65` "catches only 5 ident sites (CH2-V2-009 — the leak is 9-grammar-wide, NOT 4)" to "catches only **4** of the 9 idents rows (`:137,:143,:149,:155`; per CH2-V3-008, superseding the CH2-V2-009 '5' wording); the other 5 (`:161,:167,:173,:179,:185`) escape." | falsifying evidence: live `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/registry/strategy.rs \| grep 'idents:'` — the only `idents:` rows matched are `:137,:143,:149,:155` (4); `:161,:167,:173,:179,:185` escape. Cross-ref the already-correct `1F-coherence-scan.md:79,:86`. |

## REJECT Gate

No GENUINE reject. Per the corrected V6 convention, a REJECT requires an inventory to
state something FALSE ON DISK that a load-bearing finding rests on, plus a live
falsifying citation. The only on-disk-false token is the stale "5" at
`1F-anti-pattern.md:65` (CH2-V6-R01) — but the leak itself (9-grammar-WIDE) is stated
CORRECTLY in the same cell, the 9-row breadth and consumer anchor are exact, and the
authoritative companion already carries the corrected "4." The defect is a single
stale parenthetical regex-catch count, not a false structural finding the inventory
hangs on; it is correctly a single-locus precision REVISE, not a reject. I checked
whether any cited LOC / symbol / path:line is fabricated or recalled across the
load-bearing CH2 rows and found NONE — every figure matches disk, which is an ACCEPT of
the inventories' correctness, recorded as reject=0.

## Required Fold

One REVISE fold, bounded to a single locus on the 1F-anti-pattern surface:

1. **CH2-V6-R01 (`1F-anti-pattern.md:65`)** — propagate the CH2-V3-008 / CH2-V5-R01
   "4 not 5" narrow-regex correction (already landed in `1F-coherence-scan.md:79,:86`)
   into the companion `1F-anti-pattern.md` row, so the two LIVE 1F files stop
   contradicting each other on the idents-leak catch count. The TRUE live count is 4.
   This is the same orphan CH2-V5-R01 carried unfolded through the SK-V18 re-baseline.

Preserve all V6 ACCEPT surfaces: the 9-row idents-table relocated-seam (COH18-005 /
D-1E-V5-14), the lock-NAMED `css_types.rs` mess (COH18-006), the RED LOCKS:349
self-gate (COH18-012, CH2-V4-005/007), the 1D JSON/CSS-empirical vs grammar-neutral
separation with the G-10 CSS-scoped ratio, the grammar-neutral substrate-union and
5-shape `select_lowering` discriminator, the 8-of-9 OnceCell probe breadth
(CH5-V3-003), the skinny-scanner functional-parallel correction (CH5-V4-011), the
UNGATED codegen json-module leaks, and the cfg-gated DELETE-bound runtime witness
modules (CH2-V6-A08). Do not broaden any CSS-scoped or JSON-scoped finding into
fleet-wide generality, and do not narrow the grammar-neutral substrate or
decision-discriminator into a JSON-only lesson.

TALLY accept=8 revise=1 reject=0
