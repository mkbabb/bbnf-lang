# SK-V18 T-P1 V5 (cycle V5) — CH2 GENERALITY

Adversarial review of the T-P1 evidence inventories
(`restart/audit/totality/p1/1A..1F`) against the V1 spec surfaces
(`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`)
and the live code. Lens authority: `restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`
and `restart/prompts/ORCHESTRATOR.md:83-84` —

- Lock 14 holds: no divergence catalogued JSON/CSS-only when it is a
  grammar-neutral substrate fact;
- 1C flags every grammar-named module in a generic crate;
- 1D separates JSON/CSS-empirical from grammar-neutral findings;
- no grammar-name leak passes uncited; every intervention works for CSS L4 /
  Sheets / BBNF-self, not only JSON.

This V5 directory previously held a superseded SK-V15 campaign verdict; it is
overwritten in place per the §3Z in-place supersession protocol. This file is the
SK-V18 T-P1 cycle-V5 CH2 verdict.

## Verdict

REVISE. Two findings revise (one falsified internal count, one census-completeness
gap), seven accept. The grammar-neutrality skeleton is sound and the load-bearing
Lock-14 leak rows verify against live code to the line; the revises are
discipline-tightenings, not structural failures.

## Spot-Verification of the Most Load-Bearing Cited Rows

Every primary CH2/Lock-14 citation was re-grounded on the dirty-tree HEAD:

- `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE` — VERIFIED:
  exactly NINE `idents` rows at `:137,:143,:149,:155,:161,:167,:173,:179,:185`
  (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty), consumer
  `for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)`
  at `:216`. The table's own doc-comment (`:199-216`) self-describes the prior
  "9-arm match expression listing every production grammar by literal
  parser-name" replaced by this lookup — i.e. the relocated-seam is real and
  self-documented (COH18-005 / 1F-anti-pattern row / D-1E-V5-14 correct).
- `crates/core/src/css_types.rs:1` — VERIFIED verbatim: "Host shims for the CSS L4
  grammar's `-> parse_hex_color(...)` map," 66 LOC, in `crates/core/src/` (NOT a
  `crates/<grammar>/` declaration crate). 1C D9 / COH18-006 / D-1E-V5-14 / U-COH18-002
  Lock-14-(c)-does-not-apply disposition correct.
- Lock-14 self-gate falsification — VERIFIED: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
  crates/ir/src/ crates/analysis/src/` returns exactly **13 sites** (11 `crates/ir/src/`
  + 2 `crates/analysis/src/`), while `LOCKS.md:349`'s own verification command asserts
  it "returns ZERO." The LOCKS:349 self-gate is RED. COH18-012 / D-1E-V5-14 (CH2-V4-007)
  correct; reclassification OFF impl-exceeds-spec is correct.
- The narrow-regex catch breadth — VERIFIED: the 4-name regex catches exactly the
  4 idents rows at `:137,:143,:149,:155` (plus doc-comment siblings); the other 5
  idents rows (`:161..:185`) escape. So the COH18-005 / COH18-012 "catches only 4 of
  9" wording is the TRUE count. (This falsifies the stale "5" in 1F-anti-pattern —
  see CH2-V5-R01.)
- `crates/analysis/src/state/ast_utils/mod.rs:4,11` (`BbnfBootstrapNodeView`,
  `BbnfBootstrapRuleKind`) + `crates/ir/src/passes/recognizers/grammar_facts.rs:799`
  (`BbnfBootstrap::parse`) + `shape_dispatch/scalar.rs:17` (`BbnfBootstrap's`) — all
  VERIFIED as doc-comment hits; `scalar.rs` confirmed to exist at exactly three disk
  locations (`crates/simd-scan/src/scalar.rs`, `crates/ir/src/passes/recognizers/shape_dispatch/scalar.rs`,
  `crates/core/src/backend/rust/emitter/shapes/scalar.rs`), the leak being the `ir`
  recognizers copy (CH1-V4-F10 disambiguation correct).
- Totality `OnceCell<StructuralIndex>` probe breadth — VERIFIED: `fn ensure_structural_index`
  is present in 8 of 9 generated grammars (json/css_l4/css_pretty/csv/ebnf/bnf/
  google_sheets/bbnf = 1 each), `math.rs` = 0 (only inert doc-comment shell at `:281,:285`).
  The "8 of 9, all but math" breadth (CH5-V3-003 correction) is exact. `json.rs:701`
  field, `:719` helper, `:732` `scan_structural` all confirmed.
- simd-scan crate attribution — VERIFIED: `OnceCell` count in `crates/simd-scan/src/`
  = 0 (the OnceCell lives in the `crates/core` consumer, CH5-V4-010 correct);
  `crates/simd-scan/src/lib.rs:68` exports exactly `{StructuralIndex, next_structural_at_or_after}`.
- Skinny scanner asymmetry — VERIFIED: skinny `bbnf-simd` carries 0
  `next_structural_at_or_after`, but DOES expose `StructuralIndex`/`from_positions`/
  `parity_hash`/`scan_dispatch`/`scan_scalar` (`lib.rs:72,78,94,106,126`). The
  "functionally parallel, not one-sided" correction (CH5-V4-011) is correct.
- `select_lowering` Lock-14-clean discriminator — VERIFIED: `lower/mod.rs:18-26`
  matches on `cost.chosen` over the 5 `BackendShape` variants with ZERO grammar
  names. 1B's "zero grammar names — Lock-14-clean discriminator" row correct.
- CSS substrate-union not catalogued JSON-only — VERIFIED:
  `css_l4_declaration_values/generated.rs:257` = "Holds exactly the existing `Tape`
  — no second substrate." 1A's grammar-neutral-kernel claim (the inverse-direction
  CH2 risk) is correctly NOT scoped JSON-only.
- G-10 CSS-only-leaf census — VERIFIED: `rg find_component_delim
  skinny/crates/runtime/src | grep -v css` is EMPTY; the leaf appears only in the 7
  css_l4 replicas (6 hits each). 1D's CH2-V3-009 re-scoping of the 94.1%/79.5%
  figures as CSS-EMPIRICAL (decision-RULE kept neutral, ratio kept CSS) is exactly
  the discipline this lens demands.
- G-5 production FNV telemetry — VERIFIED: `input_fnv64` at
  `css_l4_declaration_values/generated.rs:393`, `fn fnv64` at `:899`; harness
  `css_cold_harness.rs:131 fn track1_full` (`:130` is the comment). CH3-V4-007 /
  CH6-V4-005 harness repair correct.

Not one recalled LOC, fabricated symbol, or false path:line was found among the
load-bearing CH2 rows. The citation discipline in these inventories is, by my
sampling, near-exhaustive.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V5-A01 | ACCEPT | The relocated-seam analog is correctly catalogued as a 9-grammar-WIDE Lock-14 leak in the generic `ir` crate, with consumer anchor and the narrow-regex blind-spot named. Grammar-name leak does NOT pass uncited. | `crates/ir/src/registry/strategy.rs:137-185` (9 idents rows, live-verified), `:216` consumer; `1F-coherence-scan.md:79`; `1F-anti-pattern.md:65`; `1C-runtime-evidence.md:72` (D9 cross-ref); `1E-locks-evidence.md:118` (D-1E-V5-14). |
| CH2-V5-A02 | ACCEPT | The lock-NAMED `css_types.rs` host shim is flagged in 1C, 1F-coherence, 1F-anti-pattern, and 1E as a grammar-named module in the generic core crate; Lock-14-(c) correctly held NOT to apply (it admits only a separate `crates/<grammar>/` declaration crate). Not catalogued JSON-only; the SK-V19 relocate-or-delete disposition is grammar-neutral. | `crates/core/src/css_types.rs:1` (66 LOC, live-verified); `1C-runtime-evidence.md:72`; `1F-coherence-scan.md:80` (COH18-006); `1F-anti-pattern.md:66`; `restart/locks/LOCKS.md:349` (names the file verbatim). |
| CH2-V5-A03 | ACCEPT | The LOCKS:349 self-gate is correctly catalogued as FALSIFIED/RED (13 live sites vs asserted ZERO), reclassified off impl-exceeds-spec. This is a generality gate that lies about its own coverage — exactly a CH2 leak that must not pass uncited. | live `rg ... crates/ir/src/ crates/analysis/src/` = 13; `1F-coherence-scan.md:86` (COH18-012, CH2-V4-007); `1E-locks-evidence.md:94,:118`. |
| CH2-V5-A04 | ACCEPT | 1D separates JSON/CSS-empirical (J-1..J-3, C-1..C-4) from grammar-neutral (G-1..G-13) findings per Lock 14; the G-10 94.1%/79.5% figures are correctly re-scoped CSS-EMPIRICAL (decision-rule neutral, ratio CSS), with the `grep -v css = empty` leaf census verified live. No grammar-neutral substrate fact catalogued JSON-only; no CSS-empirical ratio laundered as fleet-neutral. | `1D-skinny-lessons.md:181-209`; G-10 at `:206` (CH2-V3-009); live `find_component_delim | grep -v css` = empty. |
| CH2-V5-A05 | ACCEPT | The grammar-neutral substrate-union (1A) is correctly NOT scoped JSON-only: CSS borrows the same `Tape`/sparse-flag pair, no second substrate; the `<G:EventGrammar>` phantom is correctly excluded from the union claim (it is decoration the union never touches) and routed to DELETE while preserving the real K-axis. | `1A-substrate-evidence.md:73-74,:96,:126-138,:148-160`; live `css_l4_declaration_values/generated.rs:257` "no second substrate"; `select_lowering` zero-grammar-name discriminator at `lower/mod.rs:18-26`. |
| CH2-V5-A06 | ACCEPT | The totality-tree `OnceCell<StructuralIndex>` probe is flagged across 1C-adjacent surfaces (1F-coherence COH18-015, 1F-anti-pattern, 1E CH5-V1-03) as a grammar-fleet-WIDE carry (8 of 9 generated grammars, math excepted), with crate attribution and skinny-asymmetry corrected; classified per Lock 1, not catalogued JSON-only. | live: `ensure_structural_index` present 8/9 (math 0); `crates/simd-scan` OnceCell=0; `1F-coherence-scan.md:104` (COH18-015), `1F-anti-pattern.md:44`, `1E-locks-evidence.md:159`. |
| CH2-V5-A07 | ACCEPT | Skinny generic-codegen grammar-named modules (`json_sink_direct`, `json_typed_direct`) and the `json_templates/` directory are flagged (1F-anti-pattern, CH2-004) as Lock-14-(a) module/directory leaks and cross-cited functionally in 1B/1C. Grammar-name leak does not pass uncited. | live `codegen/src/lib.rs:4,5` + `json_templates/` (6 files) verified; `1F-anti-pattern.md:63,64`; `1B-codegen-evidence.md:38,59,94`; `1C-runtime-evidence.md:39`. |
| CH2-V5-R01 | REVISE | `1F-anti-pattern.md:65` carries a FALSIFIED narrow-regex count: it states the 4-name leak regex "catches only **5** ident sites (CH2-V2-009 — the leak is 9-grammar-wide, NOT 4)." Live verification proves the regex catches exactly **4** idents rows (`:137,:143,:149,:155`); the authoritative `1F-coherence-scan.md:79` already CORRECTED this to "4" with the explicit note "untransposed per CH2-V3-008, replacing the carried CH2-V2-009 'catches only 5 ident sites' wording." The CH2-V3-008 correction folded into 1F-coherence but NOT into its companion 1F-anti-pattern, leaving the two 1F files mutually contradictory on a CH2 generality count. CORRECTION: change `1F-anti-pattern.md:65` "catches only 5 ident sites (CH2-V2-009 — the leak is 9-grammar-wide, NOT 4)" to "catches only **4** of the 9 idents rows (`:137,:143,:149,:155`; per CH2-V3-008, superseding the CH2-V2-009 '5' wording)". | falsifying evidence: live `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/registry/strategy.rs` — the only `idents:` rows matched are `:137,:143,:149,:155` (4); `:161,:167,:173,:179,:185` (Csv/Math/Bnf/Ebnf/CssPretty) escape. Cross-ref the already-correct `1F-coherence-scan.md:79`,`:86`. |
| CH2-V5-R02 | REVISE | Census-completeness gap against the lens clause "1C flags EVERY grammar-named module in a generic crate." The skinny `runtime` crate (`bbnf-runtime`, named generic at `LOCKS.md:349`) declares grammar-named modules at its lib.rs ROOT: `pub mod json_event_grammar_witness` (`runtime/src/lib.rs:34`) and `pub mod sheets_witness` (`:38`), both `#[cfg(any(test, feature = "proof"))]`-gated hand-written witnesses (the 7 `generated_css_l4_*` + `generated_json` siblings ARE the template-emitted product and correctly not leaks). The two witness MODULES at the lib.rs-declaration site are not enumerated as Lock-14-(a) module-name rows in any inventory — only the underlying witness FILES are cited (1A-SUB-012/`:84`, 1A-DIV-007/`:110`, the phantom-`<G>` G4-DELETE cluster). The disposition (PRUNE-4 adds `EventGrammar`/`*EventGrammar` to `FORBIDDEN_GENERIC_TOKENS`, G4 DELETEs the phantom) covers the substance, so this is a citation-completeness tightening, not a missed leak. CORRECTION: add to `1F-anti-pattern.md` "Grammar-Name Leaks" table (or 1C census) a row: `runtime/src/lib.rs:34,38` (`json_event_grammar_witness`, `sheets_witness`) — cfg(test)/proof-gated grammar-named module declarations in the generic `bbnf-runtime` crate, receiver G4-DELETE ∧ PRUNE-4 `FORBIDDEN_GENERIC_TOKENS ⊇ {EventGrammar,*EventGrammar}`. | live `skinny/crates/runtime/src/lib.rs:34` `pub mod json_event_grammar_witness;`, `:38` `pub mod sheets_witness;` (both behind `#[cfg(any(test, feature = "proof"))]`); cited-only-at-file-level in `1A-substrate-evidence.md:84,:110`. |

## Required Fold

Two REVISE folds, both bounded to the 1F surface:

1. **CH2-V5-R01 (1F-anti-pattern.md:65)** — propagate the CH2-V3-008 "4 not 5"
   narrow-regex correction (already landed in `1F-coherence-scan.md:79,:86`) into the
   companion `1F-anti-pattern.md` row, so the two 1F files stop contradicting each
   other on the idents-leak catch count. The TRUE live count is 4.
2. **CH2-V5-R02 (1F-anti-pattern.md grammar-name-leak table or 1C census)** — add the
   `runtime/src/lib.rs:34,38` cfg-gated grammar-named witness-module declarations as an
   explicit Lock-14-(a) module-name row, closing the "EVERY grammar-named module in a
   generic crate" census-completeness clause. Substance already dispositioned (G4-DELETE
   ∧ PRUNE-4); the row is a citation-completeness pin.

Preserve all V5 ACCEPT surfaces: the 9-row idents-table relocated-seam (COH18-005 /
D-1E-V5-14), the `css_types.rs` lock-NAMED mess (COH18-006), the RED LOCKS:349
self-gate (COH18-012, CH2-V4-007), the 1D JSON/CSS-empirical vs grammar-neutral
separation with the G-10 CSS-scoped ratio, the grammar-neutral substrate-union (1A),
the 8-of-9 OnceCell probe breadth (CH5-V3-003), and the skinny-scanner
functional-parallel correction (CH5-V4-011). Do not broaden any CSS-scoped or
JSON-scoped finding into fleet-wide generality, and do not narrow the grammar-neutral
substrate or decision-discriminator into a JSON-only lesson.

TALLY accept=7 revise=2 reject=0
