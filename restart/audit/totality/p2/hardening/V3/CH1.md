# SK-V18 T-P2 V3 — CH1 CORRECTNESS (provenance / citation-resolution lens)

Lens: `CH1 CORRECTNESS`. Every cited paper must EXIST and carry the claimed
finding; every library-source citation must resolve to the claimed path:line;
every benchmark number must trace to a named corpus+platform. A confabulated or
unverifiable citation is a REJECT. A refuted-technique row must match the
literature's actual position.

Cycle: SK-V18-T-P2 V3 (CHALLENGE). Target packet (regenerated 2026-06-01):
`2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`,
`2D-cost-model.md`, `2E-host-arch-esoterica.md`, `2F-parse-that-gaps.md`.

This is a fresh SK-V18 review of the regenerated packet — it does NOT inherit
the stale SK-V15-lineage V3/CH1 verdict (commit `d11a9eec0`) that previously
occupied this path; that verdict reviewed a different, smaller packet.

## Disposition (per-dossier enumeration)

| dossier | disposition | basis |
|---|---|---|
| 2A-sota-landscape | **REVISE** | All published-paper and external-URL citations resolve and carry the claimed finding (verified below). One internal-source over-count: the "14 differential harnesses (12 single-kernel + 2 aggregate)" sub-classification (2A `:190`, SK-V18 assertion 1) is inaccurate. |
| 2B-primitive-vocabulary | **ACCEPT** | Every load-bearing local source and every cited paper/URL resolved (eq-set NEON body, scalar oracles, checkasm harness, Lemire/Validark/Langdale-Lemire, ACLE intrinsics). No confabulation; refuted rows (LD4-by-citation, PMULL/CSSC/SVE2-by-ISA, FSM/frame-stack source-only) match the literature's position. |
| 2C-grammar-neutrality | **ACCEPT** | The load-bearing falsifiable refutations all resolve EXACTLY: the 9-ident `PRODUCTION_MANIFEST_TABLE` at `strategy.rs:137-185`, the Lock-14 self-gate falsification (`rg` returns exactly 13 sites), and `css_types.rs` (generic core, 66 LOC, line-1 host-shim comment). Pratt POPL 1973 (DOI 10.1145/512927.512931) verified. |
| 2D-cost-model | **ACCEPT** | The decision-engine "no longer scaffold" re-grounding resolves to source verbatim (`NormalizeDirectSinkCost` instantiated `backend_egraph.rs:75`, struct+impl `:191`/`:193`, `BackoffScheduler` `:73`, `Extractor`/`DecisionCostModel` `:84-87`). egg POPL 2021, Mison PVLDB 2017, iburg LOPLAS 1992 DOIs all verified. |
| 2E-host-arch-esoterica | **ACCEPT** | Movemask divergence (`movemask.rs:5` SHRN vs `byte_class_from_eq_set_64.rs:79` vaddv) resolves; Kutenin, Validark, Lemire-2026, ACLE, SVE2-absence-on-host all verified. svmatch refutation matches Lemire-2026's actual position (SVE2 `match` "fastest" but host-absent → deployable NEON eq-fan). |
| 2F-parse-that-gaps | **REVISE** | The central V3 refutation (upstream parse-that scan substrate exists) is TRUE and resolves; `scan_balanced` at `balanced.rs:26`, the `<=8` assert at `:44-45`, `classify_16` at `structural_bitmap.rs:37`. But SRC-PARSE-THAT-SCAN mislocates `find_first_of_nibble_lut`/`build_nibble_luts` into the `scan/` directory — they live in `src/scanners.rs:262`/`:235` (imported by `balanced.rs:27`). Plus the same "14 checkasm-gated kernels" over-count as 2A. |

Net: 4 ACCEPT, 2 REVISE, 0 REJECT.

## Critical Findings

| id | dossier | severity | finding | falsifying / confirming evidence | convergence impact |
|---|---|---|---|---|---|
| CH1-V3-SK18-01 | 2A | REVISE (minor) | SK-V18 Architectural Assertion 1 (`2A:190`) states the in-tree replica "carries 14 differential harnesses (12 single-kernel + 2 aggregate, SPEC §3.1)". `ls skinny/crates/bbnf-simd/tests/checkasm_*.rs` returns 14 *files*, but `checkasm_common.rs` is a shared helper module (opens `#![allow(dead_code)]`, defines `Xorshift64`), NOT a harness, and only `checkasm_parity.rs` is the aggregate. The true split is 12 single-kernel + 1 aggregate + 1 helper. The "+2 aggregate" sub-count and the implication of 14 *kernel* harnesses are wrong. | `ls .../tests/checkasm_*.rs` = 14 files; `checkasm_common.rs` head = `#![allow(dead_code)] / pub struct Xorshift64`; `checkasm_parity.rs` = the sole aggregate (8 `#[test]`/`check_` fns). | Does not block. The dav1d/checkasm PROCESS grounding (the load-bearing claim) is correct; only the harness count is over-stated. Fix: "13 differential harnesses (12 single-kernel + 1 aggregate `checkasm_parity.rs`) plus the `checkasm_common.rs` helper module." |
| CH1-V3-SK18-02 | 2F | REVISE (minor) | SRC-PARSE-THAT-SCAN (`2F:69`) lists `find_first_of_nibble_lut` and `build_nibble_luts` among `.../src/parsers/scan/{...}` files, and LAC-2F-V3-01's close-test greps `bbnf-simd` for these exact symbol names. The symbols are LIVE but live in the parent module `src/scanners.rs` (`find_first_of_nibble_lut` `:262`, `build_nibble_luts` `:235`), not in the `scan/` directory; `balanced.rs:27` imports them. The substantive claim (substrate exists, `scan_balanced` rides the nibble-LUT classifier) holds; only the provenance path of the two named symbols is mislocated. | `grep -rn find_first_of_nibble_lut src/` → `scanners.rs:262` (def), `balanced.rs:27,48,56` (use); `build_nibble_luts` → `scanners.rs:235`. The cited `scan/structural_bitmap.rs:94` mention is a *comment* ("Replaces ... `find_first_of_nibble_lut`"), not the definition. | Does not block. Correct SRC-PARSE-THAT-SCAN to add `src/scanners.rs:235,262` as the actual home of the two named kernels (the `scan/` files consume them). |
| CH1-V3-SK18-03 | all | none | Every published-paper citation EXISTS and carries the claimed finding. | simdjson/VLDBJ arXiv:1902.08318; Mison PVLDB 10(10) DOI 10.14778/3115404.3115416; egg PACMPL POPL 2021 DOI 10.1145/3434304; iburg LOPLAS 1(3) 1992 DOI 10.1145/151640.151642 (pp 213-226); Pratt "Top Down Operator Precedence" POPL 1973 DOI 10.1145/512927.512931 — all confirmed via web with matching authors/venue/finding. | No block. |
| CH1-V3-SK18-04 | 2E, 2B | none | Every load-bearing external technique URL resolves and matches its dossier characterization. | Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" (SVE2 `match` fastest, replaces eq-cmp+OR-reduce) confirmed live; Kutenin "Bit twiddling with Arm Neon: beating SSE movemasks" (`vshrn` imm=4 SHRN-movemask) confirmed; Validark "Use interleaved vectors for parsing on ARM" 2024 (`ld4`) confirmed. The svmatch/SVE2 refutation matches Lemire-2026's actual position. | No block. |
| CH1-V3-SK18-05 | 2A, 2D, 2E, 2F | none | The load-bearing LOCAL SIMD-source citations resolve to the claimed path:line. | `find_css_significant` `runtime_simd.rs:169`; two-fan OR-reduce `:199`; dead-caller R7 = only `lib.rs:574` under the single `#[cfg(test)] mod tests` (`:51-52`); `movemask.rs:5` `vshrn_n_u16::<4>`; `byte_class_from_eq_set_64.rs:79` slow `vaddv_u8` pack; `checkasm_parity.rs:3` verbatim "Modelled on FFmpeg's `tests/checkasm/checkasm.h`"; `NormalizeDirectSinkCost` `backend_egraph.rs:75/191/193`. | No block. |
| CH1-V3-SK18-06 | 2C | none | 2C's load-bearing falsifiable REFUTED rows resolve EXACTLY, including a verbatim-numeric self-gate falsification. | `strategy.rs:137-185` = 9-ident `PRODUCTION_MANIFEST_TABLE` (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty); `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` returns exactly **13** sites (the dossier's claimed Lock-14-self-gate-RED count); `crates/core/src/css_types.rs` = 66 LOC, line-1 `//! Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map`. | No block. |
| CH1-V3-SK18-07 | all | none | Every benchmark/profile number traces to its named source + platform; the fleet-wide-cited S-P1 numbers resolve. | `SYNTHESIS-PROFILE.md:90-100`: `find_component_delim` 79.5%, `consume_balanced_at` 14.6%, scalar-scan 4121/4379=94.1%, JSON leaves 91.52%, JSON product scan-free. Host gating (M5 Max, FEAT_PMULL/DotProd/I8MM/CSSC/SHA3=1, FEAT_SVE2 absent) is a local `sysctl` probe, disclosed as such. sonic-rs README @ pinned commit confirms targeted-SIMD + direct-to-struct (no tape) claims. | No block. |
| CH1-V3-SK18-08 | all | none | No refuted-technique row contradicts the literature. | dav1d *pixel kernels* not-transferable (only the PROCESS) — correct; checkasm PASS ≠ speedup — matches FFmpeg's correctness/--bench split; x86/AVX-512 cannot close an M5 Max row — consistent with the aarch64-only host; svmatch-on-NEON refuted — matches SVE2 host-absence + Lemire-2026. | No block. |

## Evidence Inspected

- All six target dossiers (2A 277 lines, 2B 436, 2C 347, 2D 144, 2E 240, 2F 192).
- Local SIMD/runtime sources: `skinny/crates/runtime/src/runtime_simd.rs:6-7,29,112,169,199`,
  `skinny/crates/runtime/src/lib.rs:51-52,500,548-578`,
  `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:5`,
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:79-87`,
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-5`, `checkasm_common.rs:1-8`,
  the 14 `checkasm_*.rs` file census.
- Decision-engine source: `skinny/crates/passes/src/backend_egraph.rs:73-87,191-193`,
  `skinny/crates/codegen/src/lower/{mod,collapsed_stage,sink_only}.rs` (existence + sizes).
- Totality-tree generic-crate sources: `crates/ir/src/registry/strategy.rs:131-185`,
  `crates/core/src/css_types.rs:1-3`, and the live `rg` self-gate count (13 sites).
- Upstream parse-that substrate: `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/scan/`
  (balanced.rs:7,26,44-45; structural_bitmap.rs:37,94; quote_parity.rs:23,26,41,54,122,185),
  `src/scanners.rs:235,262` (the actual home of `build_nibble_luts`/`find_first_of_nibble_lut`).
- Profile/bench: `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md:63-146`.
- Web verification (paper/URL existence + claimed finding): Lemire 2026-04-19 blog; Pratt POPL 1973
  DOI 10.1145/512927.512931; Kutenin Arm-community NEON-movemask post; iburg LOPLAS 1992
  DOI 10.1145/151640.151642; Mison PVLDB 2017 DOI 10.14778/3115404.3115416; egg POPL 2021
  DOI 10.1145/3434304; Validark "Use interleaved vectors for parsing on ARM" 2024;
  sonic-rs README @ commit `03545a9...` (targeted-SIMD + direct-to-struct).

## Fold Requirements (REVISE dossiers)

- **2A** — In SK-V18 Architectural Assertion 1 (`2A:190`) and any harness-count mention, replace
  "14 differential harnesses (12 single-kernel + 2 aggregate)" with the accurate split: 12
  single-kernel `checkasm_<primitive>.rs` harnesses + 1 aggregate (`checkasm_parity.rs`) + the
  `checkasm_common.rs` shared-helper module (not a harness). The dav1d/checkasm process grounding
  itself is correct and stands.
- **2F** — In SRC-PARSE-THAT-SCAN (`2F:69`), relocate `find_first_of_nibble_lut` and
  `build_nibble_luts` to their real home `src/scanners.rs:262`/`:235` (the `scan/` files import
  them); the `scan/structural_bitmap.rs:94` reference is a comment, not the definition. The same
  "14 checkasm-gated kernels" phrasing in the 2F frontmatter and SRC-BBNF-SIMD-EQSET should be
  corrected to "12 single-kernel + 1 aggregate checkasm harnesses (+ a `checkasm_common` helper)".
  The central V3 refutation (upstream substrate exists, scan_balanced is the reference shell) is
  fully grounded and stands.

## Convergence Impact

CH1 does NOT block T-P2 V3 convergence. Both REVISE items are minor citation-precision
corrections (a harness sub-count over-statement and a symbol-home path mislocation); neither is a
confabulated or unverifiable source, and neither falsifies a grounding or a refutation. No REJECT.
Every paper, every external technique URL, every load-bearing library path:line, and the
fleet-cited S-P1 benchmark numbers resolved to their claimed source and finding. The two REVISE
folds are orphan-free and can be applied without touching any grounded technique row or
refuted-route position.

TALLY accept=4 revise=2 reject=0
