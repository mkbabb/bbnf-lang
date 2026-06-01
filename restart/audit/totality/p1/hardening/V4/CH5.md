---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V4
campaign: SK-V18
disposition: REVISE
generated_at: 2026-06-01T00:00:00Z
files_audited:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/audit/totality/p1/hardening/V1/CH5.md
  - restart/audit/totality/p1/hardening/V3/CH5.md
spot_verified_paths:
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:393,394,899
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs:42
  - skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:130,131
  - skinny/crates/codegen/src/runtime_generator.rs:1093,1094,1599
  - crates/core/src/grammar/generated/json.rs:701,711,719,732
  - crates/core/src/grammar/generated/math.rs (0 ensure_structural_index)
  - crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67
  - crates/simd-scan/src/lib.rs:68; crates/simd-scan/src/index.rs:74,101
  - skinny/crates/bbnf-simd/src/lib.rs:72,77,78,82,94,106,126
  - skinny/crates/runtime/src/grammars/json/scan.rs:22,38,43,47
  - skinny/crates/runtime/src/grammars/json/generated.rs:12-15,760-768
  - skinny/crates/runtime/src/grammars/json/parser.rs:7-12,18,49
  - skinny/crates/codegen/src/json_typed_direct.rs:361,668,671
score: "9 ACCEPT / 3 REVISE / 0 REJECT"
---

# CH5 Hidden Coupling — SK-V18 T-P1 V4 (cycle V4 directory; live inventories carry cycle V5-SKV18-totality)

## Verdict

REVISE.

The hidden-coupling catalogue is overwhelmingly sound. Every load-bearing
coupling row I spot-verified against current source resolves — the CSS FNV
production-path telemetry, the totality `OnceCell<StructuralIndex>` probe, the
three cursor carriers, the `RuntimeEmitterKind` relocated-seam fork, the
`PRODUCTION_MANIFEST_TABLE` grammar-name leak, the phantom `<G>` axis, and the
skinny Lock-1 union all land at the cited path:line. The prior-cycle V1 REVISE
batch (CH5-V1-02 OnceCell classification, CH5-V1-03 1E scoping, CH5-V1-04
scanner asymmetry, CH5-V1-09 1A-SUB-017 NO-OP correction) folded correctly and
verifies on disk. No recalled, false, or uncited coupling claim survives — zero
REJECT.

Three REVISES remain, all on the scanner-asymmetry / hash-sidecar axis the CH5
lens specifically guards:

1. **COH18-015 mis-locates the `OnceCell<StructuralIndex>` into `crates/simd-scan`.**
   The OnceCell retention lives in the *consuming* totality generated grammars
   (`crates/core/src/grammar/generated/json.rs:701`), NOT in the `simd-scan`
   crate; only `next_structural_at_or_after` lives in `simd-scan`. The evidence
   column splits them correctly, but the prose conflates two crates.
2. **The scanner-asymmetry framing understates skinny's live `StructuralIndex`.**
   COH18-015 / 1F-anti-pattern row 43 say skinny `bbnf-simd` "lacks both" /
   "carries ZERO" — true only for the two named functions. Skinny `bbnf-simd`
   DOES expose a full `StructuralIndex` (`from_positions`/`positions`/
   `scan_dispatch`/`scan_scalar`/`parity_hash`) consumed in `json/scan.rs`. The
   renamed/parallel-scanner risk the lens demands is therefore STRONGER than
   framed, not weaker.
3. **`parity_hash` is an uncatalogued production-crate hash producer the
   FNV/sidecar guard does not catch.** `bbnf_simd::StructuralIndex::parity_hash`
   (`skinny/crates/bbnf-simd/src/lib.rs:94`) is consumed at
   `skinny/crates/runtime/src/grammars/json/scan.rs:43` (`hash:
   index.parity_hash(input)`). No p1 inventory cites it, and the guard
   `input_fnv64|stream_fnv64|fn fnv64|fnv64(` cannot see it.

## Lens Authority

CH5 = Lock-1 / hidden-coupling review: the substrate inventory (1A) must honour
the Lock-1 union with no catalogued state implying a parallel substrate, sidecar
producer, or renamed scanner; 1F must catch the live couplings; Track-1≡Track-2
dishonesty must surface if present (`restart/prompts/totality/PASS-1-EXCAVATION.md`
CH5 overlay; `restart/locks/LOCKS.md:75` substrate-union;
`restart/locks/LOCKS.md:137-158` retained-sidecar REJECT classes).

## Spot-Verification Ledger (current source, dirty-tree HEAD)

| coupling | inventory claim | live disk | result |
|---|---|---|---|
| CSS FNV production path | 1F-anti-pattern:41 `generated.rs:393,394,899`; `parser.rs:42 emit_full_parse`; `css_cold_harness.rs:130 track1_full` | `:393` `push_str("source\tinput_fnv64=")`, `:394` `push_hex64(&mut out, fnv64(input.as_bytes()))`, `:899` `fn fnv64`; `parser.rs:42` `generated::emit_full_parse(input)` inside `parse_full`; `css_cold_harness.rs:131 track1_full` calls `parse_full` | MATCH |
| CSS FNV generator template | 1F-anti-pattern:41 `runtime_generator.rs:1093,1599` | `:1093` `input_fnv64=`, `:1094` `push_hex64(.. fnv64(..))`, `:1599` `fn fnv64` | MATCH (current-source re-anchor; prior V4 CH5 `:737/:783/:1331` now stale) |
| 7 css_l4 replicas | 1F-anti-pattern:44 md5 `b654562c` ×7 | `md5 css_l4_*/generated.rs` = 7× `b654562ccff46ed62dd48e9ace325830` | MATCH |
| totality OnceCell probe, 8/9 | 1F-anti-pattern:43 "8 of 9 (all but `math`)" | `ensure_structural_index` count: bbnf/bnf/css_l4/css_pretty/csv/ebnf/json=2, google_sheets=3, **math=0** | MATCH (8/9 exact) |
| emitter "probe substrate" diction | 1F-anti-pattern:43 `support.rs:67` | `support.rs:67` "The probe substrate (OnceCell + helper)"; gate `ctns_probe_admits` `:74-95` | MATCH |
| `next_structural_at_or_after` in simd-scan | COH18-015 evidence `simd-scan/src/lib.rs:68`, `index.rs:74,101` | `lib.rs:68` `pub use index::{StructuralIndex, next_structural_at_or_after}`; `index.rs:74` method, `:101` free fn | MATCH (location correct in evidence column) |
| skinny bbnf-simd lacks OnceCell | 1F-anti-pattern:43 "ZERO `OnceCell<StructuralIndex>` (verified empty)" | `rg OnceCell<StructuralIndex> skinny/crates/bbnf-simd/src` = 0 | MATCH |
| skinny bbnf-simd lacks named fns | COH18-015 "lacks both" | `rg next_structural_at_or_after\|scan_structural skinny/.../bbnf-simd/src` = 0 | LITERALLY MATCH but understated (REVISE-2) |
| 1A-SUB-017 NO-OP | `attach_structural_index` NO-OP, no retained sidecar | `generated.rs:12-15` `debug_assert_eq!(config::STRUCTURAL_BYTES, …); let _ = state;`; `parser.rs:49` calls it; `ParserState` retains no `StructuralIndex` | MATCH |
| three cursor carriers | 1A-DIV-006 retained `parser.rs:7-12`, direct `generated.rs:766`, codegen `json_typed_direct.rs:668,671,361` | `parser.rs:9` `cursor: usize` + `tape: TapeBuilder`; `generated.rs:766` `let mut cursor = 0`; `json_typed_direct.rs:668` `struct DirectParser`, `:671` `cursor: usize`, `:361` `let checkpoint = parser.cursor` | MATCH |
| phantom `<G>` census empty | 1A-SUB-023 / 1F-anti-pattern:42 zero non-test instantiation | confirmed against prior-cycle census + def/witness/`#[cfg(test)]` scope | MATCH |
| 1E retained-sidecar scoping | CH5-V1-03 fold: "NONE in the SKINNY benched tree" + totality carry | `1E:158` carries the skinny-scoped NONE + the 8/9 totality OnceCell carry | MATCH (landed) |

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH5-V4-001 | ACCEPT | 1A honours the Lock-1 union for the skinny benched tree: one grammar-neutral `Tape`, one `ValueRef` cursor, CSS holds exactly the existing tape — no second substrate is claimed or implied. | `1A-substrate-evidence.md:73-75` (SUB-001/002/003); live `skinny/crates/runtime/src/tape/mod.rs:94`,`:175`; `css_l4_declaration_values/generated.rs:257` "Holds exactly the existing `Tape` — no second substrate"; 1A explicitly scopes its closure to "One retained tape … for JSON+CSS" (`1A:171`) and routes the totality probe to 1F, not itself. |
| CH5-V4-002 | ACCEPT | The CSS FNV production-path telemetry coupling is catalogued at current source with the production call chain proven (NOT bench-quarantined). | `1F-anti-pattern.md:41` anchors `generated.rs:393,394,899` + template `runtime_generator.rs:1093,1599`; verified `parse_full→emit_full_parse→fnv64` and `css_cold_harness.rs:131 track1_full`. Classified telemetry/hash-sidecar, non-equality / non-substrate / non-document-identity, fence obligation — correct CH5 posture; no laundering into CSS Value API or equality proof. |
| CH5-V4-003 | ACCEPT | The totality `OnceCell<StructuralIndex>` probe is correctly Lock-1-classified as per-parse `generated_function` lifetime (admissible class), NOT the cross-call REJECT class, with the 8/9 breadth exact. | `1F-anti-pattern.md:43` + `1E:158`; live `json.rs:701` OnceCell field, `:711` `ScanState::new(){ OnceCell::new() }`, `:719` `ensure_structural_index`; per-grammar `ensure_structural_index` count confirms math=0, all others ≥2; `LOCKS.md:139-149` REJECT class is cross-call-retained, which this is not. |
| CH5-V4-004 | ACCEPT | The three cursor carriers are catalogued, not collapsed into a false single-cursor union; the §9 shared-event-cursor gap stays open. | `1A-DIV-006` + `1A-SUB-020`; live `parser.rs:9`, `generated.rs:766`, `json_typed_direct.rs:671`/`:361`. No inventory claims one unified typed cursor exists. |
| CH5-V4-005 | ACCEPT | The phantom `<G:EventGrammar>` axis is correctly held as decoration, NOT a second substrate, with an empty non-test instantiation census. | `1A-SUB-023`, `1F-anti-pattern.md:42`, `1C-C5/D5`; the union is `&'i Tape<'i>` + cursor, which `<G>` never touches; DELETE is the SK-V18 G4 disposition, not a sidecar add. |
| CH5-V4-006 | ACCEPT | The `RuntimeEmitterKind` config-carried grammar-family fork (the relocated-seam / Track-analog hidden coupling) is caught across 1B/1C/1E/1F and gated by structural row-collapse, not md5-distinctness alone. | `1F-coherence COH18-003`, `1F-anti-pattern.md:59`, `1C-D2`, `1E LAC-1E-V5-02`; live `grammar_provider.rs:40-42`, dispatch `runtime_generator.rs:17,25`. |
| CH5-V4-007 | ACCEPT | The `PRODUCTION_MANIFEST_TABLE` grammar-named ident table in the generic `ir` crate (totality relocated-seam analog) is caught, with the breadth corrected to 9-grammar-wide and the §1643 gate-scope gap flagged. | `1F-coherence COH18-005/012`, `1F-anti-pattern.md:64`; live `crates/ir/src/registry/strategy.rs:137-185` nine `idents` rows, consumer `:216`. |
| CH5-V4-008 | ACCEPT | Track-1≡Track-2 dishonesty is not reopened: JSON cold Track-1 beats sonic-rs strict same-plane; CSS >SOTA rides hand-written content and is held directional, not re-locked; the FNV stays a non-equality probe. | `1F-coherence COH18-013`, `1F-past-corpora A` (JSON SETTLED / CSS DIRECTIONAL split), `1D U-4`; no Track-1≡Track-2 substrate-independence claim is made; CSS preservation-through-the-generator stays an open burden. |
| CH5-V4-009 | ACCEPT | 1A-SUB-017's NO-OP correction (CH5-V1-09 fold) landed and is honest: skinny carries NO retained structural sidecar; `attach_structural_index` is a debug-assert stub, the scan plane is `local_temp_only`. | Live `generated.rs:12-15` `let _ = state;`; `parser.rs:49` call site; capacity scan via `scan.rs:22 scan_structurals` / `:47 structural_capacity_for`; `ParserState` retains no index. |
| CH5-V4-010 | REVISE | COH18-015 PROSE mis-attributes the `OnceCell<StructuralIndex>` retention to `crates/simd-scan`. The OnceCell lives in the consuming totality generated grammars (`crates/core/src/grammar/generated/json.rs:701`), NOT in the `simd-scan` crate; only `next_structural_at_or_after` lives in `simd-scan`. The evidence column splits them correctly, so this is a prose locational error, not a fabricated claim. | Live `crates/simd-scan/src/lib.rs:68` exports ONLY `{StructuralIndex, next_structural_at_or_after}` (no OnceCell anywhere in `simd-scan`); the `OnceCell<::simd_scan::StructuralIndex>` is the `ScanState` field at `crates/core/src/grammar/generated/json.rs:701` — a `crates/core` consumer. 1E-L08 (`1E:158`) keeps the two correctly separated ("the totality OnceCell + the `crates/simd-scan` `next_structural_at_or_after` probe API"), so the inventories are internally inconsistent on attribution. **CORRECTION:** in `restart/audit/totality/p1/1F-coherence-scan.md:100`, change the COH18-015 row text "the totality `crates/simd-scan` carries a retained-index probe API (`next_structural_at_or_after` + `OnceCell<StructuralIndex>`)" to "the totality `crates/simd-scan` carries a random-access probe API (`next_structural_at_or_after`) and the totality generated grammars retain it across the parse via `OnceCell<StructuralIndex>` on `ScanState` (`crates/core/src/grammar/generated/json.rs:701`)". |
| CH5-V4-011 | REVISE | The scanner-asymmetry framing ("skinny `bbnf-simd` … does NOT have" / "carries ZERO" / "lacks both") understates skinny's live structural scanner and so UNDERSTATES the renamed/parallel-scanner risk the CH5 lens exists to police. Skinny `bbnf-simd` exposes a full `StructuralIndex` with `from_positions`/`positions`/`into_positions`/`backend`/`parity_hash` and `scan_dispatch`/`scan_scalar`, consumed in `json/scan.rs`. The two scanners are FUNCTIONALLY PARALLEL (same `StructuralIndex` concept, different function names + a skinny-only `parity_hash`), which makes the SK-V19 "unify vs renamed-parallel-scanner" decision MORE load-bearing, not "absent from skinny". | Live `skinny/crates/bbnf-simd/src/lib.rs:72` `pub struct StructuralIndex`, `:78 from_positions`, `:82 positions`, `:94 parity_hash`, `:106 scan_dispatch`, `:126 scan_scalar`; consumed `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,35`. COH18-015 (`1F-coherence-scan.md:100`) and 1F-anti-pattern row 43 (`1F-anti-pattern.md:43`) both assert the absence as a clean "ZERO/lacks both". **CORRECTION:** in `restart/audit/totality/p1/1F-coherence-scan.md:100` (COH18-015) and `restart/audit/totality/p1/1F-anti-pattern.md:43`, qualify the asymmetry: skinny `bbnf-simd` has a `StructuralIndex` + `scan_dispatch`/`scan_scalar` (`lib.rs:72,106,126`) but lacks the `next_structural_at_or_after` random-access API and the cross-parse `OnceCell` retention; the renamed-parallel-scanner risk is therefore ACTIVE (two real scanners with divergent APIs), not a one-sided "totality-only" feature. |
| CH5-V4-012 | REVISE | An uncatalogued production-crate hash producer escapes the sidecar/hash grep guard. `bbnf_simd::StructuralIndex::parity_hash` (`skinny/crates/bbnf-simd/src/lib.rs:94`) is consumed in the runtime crate at `skinny/crates/runtime/src/grammars/json/scan.rs:43` (`hash: index.parity_hash(input)`), building a `ScalarParityReport`. No p1 inventory cites `parity_hash`/`ScalarParityReport`, and the guard `input_fnv64|stream_fnv64|fn fnv64|fnv64(` cannot see it. It is reached only via `scalar_parity_report` (a scan-parity diagnostic, NOT the `parse_json`/`parse_direct` hot path), so severity is LOW — but the CH5 lens requires every hash-sidecar producer in `skinny/crates` to be catalogued and fenced, and the guard claims hash-surface completeness it does not have. | Live `skinny/crates/bbnf-simd/src/lib.rs:94` `pub fn parity_hash(&self, input: &[u8]) -> [u8; 32]`; `json/scan.rs:38 scalar_parity_report` → `:43 hash: index.parity_hash(input)`; production callers are `parser.rs:18 structural_capacity_for` and `parser.rs:49 attach_structural_index` (neither touches `scalar_parity_report`); `rg parity_hash restart/audit/totality/p1/*.md` = 0. **CORRECTION:** in `restart/audit/totality/p1/1F-anti-pattern.md:41` (and the companion guard in `1F-coherence-scan.md`), extend the close-gate grep to `input_fnv64|stream_fnv64|fn fnv64|fnv64\(|parity_hash|ScalarParityReport` and add a row classifying `json/scan.rs:43 parity_hash` as a scan-parity diagnostic hash (NOT on the `parse_json`/`parse_direct` hot path, NOT an equality arbiter, NOT a retained sidecar) — fenced, owner SK-V19 scanner-unification. |

## Required Fold

Three REVISE corrections, all routed to 1F (the live coherence packet that owns
the scanner/hash-sidecar census):

1. `restart/audit/totality/p1/1F-coherence-scan.md:100` (COH18-015) — fix the
   `OnceCell` crate attribution (CH5-V4-010) and qualify the skinny scanner
   asymmetry (CH5-V4-011).
2. `restart/audit/totality/p1/1F-anti-pattern.md:43` — qualify "ZERO/lacks both"
   to name skinny's live `StructuralIndex`/`scan_dispatch`/`scan_scalar`
   (CH5-V4-011).
3. `restart/audit/totality/p1/1F-anti-pattern.md:41` (+ companion guard in
   `1F-coherence-scan.md`) — extend the hash-sidecar close-gate grep to
   `parity_hash|ScalarParityReport` and add the classified diagnostic-hash row
   (CH5-V4-012).

The skinny Lock-1 union closure, the CSS FNV census, the OnceCell 8/9
classification, the three-cursor catalogue, the relocated-seam fork, the
manifest-table leak, and the Track-1≡Track-2 posture all SURVIVE — the
corrections sharpen the scanner/hash-sidecar census, they do not unwind any
substrate-union or coupling conclusion.

No source edit, inventory edit, staging, or commit was performed for this CH5 report.

TALLY accept=9 revise=3 reject=0
