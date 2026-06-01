# SK-V18 T-P2 V4 — CH1 CORRECTNESS (provenance / citation-resolution lens)

Lens: `CH1 CORRECTNESS`. Every cited paper must EXIST and carry the claimed
finding; every library-source citation must resolve to the claimed path:line;
every benchmark number must trace to a named corpus+platform. A confabulated or
unverifiable citation is a REJECT. A refuted-technique row must match the
literature's actual position. A REJECT requires citing the specific
unverifiable source, not a blanket suspicion.

Cycle: SK-V18-T-P2 V4 (CHALLENGE). Target packet (regenerated 2026-06-01):
`2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`,
`2D-cost-model.md`, `2E-host-arch-esoterica.md`, `2F-parse-that-gaps.md`.

This is a fresh, independent SK-V18 re-verification of the regenerated packet —
it does NOT inherit the V3/CH1 verdict; every load-bearing citation was
re-resolved against HEAD (and, for papers/URLs, re-confirmed on the web) this
cycle. The V3 REVISE folds (2A harness-count, 2F `scanners.rs` symbol-home) were
audited for correct application.

## Disposition (per-dossier enumeration)

| dossier | disposition | basis |
|---|---|---|
| 2A-sota-landscape | **ACCEPT** | The V3 REVISE (harness over-count) is correctly folded: frontmatter `:17`, Assertion 1 `:192-194`, and `SRC-BBNF-SIMD-CHECKASM:275` now read "13 differential harnesses (12 single-kernel + 1 aggregate `checkasm_parity.rs`) + the `checkasm_common.rs` helper" (the "14 `checkasm_*.rs` files" phrasing at `:275` is the accurate FILE count). Every published paper, external URL, and load-bearing local source resolves and carries the claimed finding (verified below). |
| 2B-primitive-vocabulary | **ACCEPT** | Every load-bearing SK-V18 local source resolves EXACTLY: `find_css_significant` two-fan OR-reduce (`runtime_simd.rs:169`/`:199`), the ZERO-live-caller proof (`lib.rs:574` `#[cfg(test)]`), the eq-set NEON movemask (`byte_class_from_eq_set_64.rs:79-89`), and the REDRESS benchmark provenance (144 +109.87%/444.208 vs 434.1316 Mbps `:4434-4436`; 126 microbench 4.718279341× `ROUTE-PRODUCTION-SPLIT` `:3768`). The `runtime_simd.rs:6-7` inaccurate-comment is real and correctly flagged as a source-fix obligation. |
| 2C-grammar-neutrality | **REVISE** | The SK-V18 V3 section is exact (9-ident `strategy.rs:137-185`, 13-site self-gate leak, `css_types.rs` 66 LOC host-shim line-1, `strategy.rs:216` consumer, Sheets `:137`/`:163`, Pratt POPL 1973 DOI verified). But three SK-V15-HISTORICAL rows (`:81`, `:138`, `:164`) cite `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379` as the "scan-root list that excludes the leak roots" — at HEAD that line range is a generic `is_allowed_path` helper + the start of `validate_backend_shape_surface`, and the file DOES now contain `runtime_generator.rs` (8×) and `grammar_provider`/`per_grammar_provider` (15×). The cited path:line no longer resolves to the claimed content and the claim is falsified at that location at HEAD. |
| 2D-cost-model | **ACCEPT** | The decision-engine re-grounding resolves verbatim: `NormalizeDirectSinkCost` (instantiated `backend_egraph.rs:75`, struct+impl `:191`/`:193`), `BackoffScheduler` (`:73`), `Extractor::new(&graph, &DecisionCostModel)` (`:84-87`), `select_lowering(cost.chosen)` 5-shape match (`lower/mod.rs:18-24`), `collapsed_stage.rs:16` delegating to `tape_plan::render_rule(.., Collapsed)`, the `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork (`grammar_provider.rs:39-43`). iburg LOPLAS 1992, egg POPL 2021, Mison PVLDB 2017, simdjson arXiv:1902.08318 all web-verified. |
| 2E-host-arch-esoterica | **ACCEPT** | The movemask divergence resolves (`movemask.rs:5` canonical `vshrn_n_u16::<4>` SHRN vs `byte_class_from_eq_set_64.rs:79-87` shift-add `vaddv_u8`). The host probe is verified ON THE ACTUAL HOST: `Apple M5 Max`, `FEAT_SVE2` ABSENT (unknown oid), `FEAT_PMULL/DotProd/I8MM/CSSC/SHA3=1` — the exact crux of the svmatch refutation. The CH4-V3-03 Lemire-2026 attribution fold (eq-fan/shuffle in COMMENTS, body endorses TBL classifier + SVE2 `match`) is web-verified ACCURATE against the live post. |
| 2F-parse-that-gaps | **ACCEPT** | The V3 REVISE is correctly folded: `SRC-PARSE-THAT-SCAN:73` and `SRC-BBNF-SIMD-EQSET:74` now home `find_first_of_nibble_lut`/`build_nibble_luts` at `scanners.rs:262`/`:235` (the `scan/` files import them; `structural_bitmap.rs:94` is a comment), and the harness count reads "12 single-kernel + 1 aggregate (+ helper) = 13". The upstream substrate resolves EXACTLY (`scan_balanced` `balanced.rs:26`, `<=8` assert `:44-45`, import `:27`, CSS hot leaf `find_component_delim`/`consume_balanced_at` `generated.rs:657`/`:693`). |

Net: 5 ACCEPT, 1 REVISE, 0 REJECT.

## Critical Findings

| id | dossier | severity | finding | falsifying / confirming evidence | convergence impact |
|---|---|---|---|---|---|
| CH1-V4-SK18-01 | 2C | REVISE (minor) | Three SK-V15-HISTORICAL rows (`2C:81` SK-V15-2C-GATE-EXCLUSION, `:138` refuted-assertions table, `:164` LAC-2C-SK15-05) cite `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379` as the Lock-14 scan-root list that "excludes `runtime_generator.rs`, `grammar_provider.rs`, JSON direct generators, json_templates". At HEAD the file is 5095 LOC and `:2367-2379` is the generic `fn is_allowed_path(...)` helper plus the start of `fn validate_backend_shape_surface(...)` — NOT a scan-root list. The actual allowed/scan roots live at `:345-435`, and the file DOES contain `runtime_generator.rs` (`grep -c` = 8) and `grammar_provider`/`per_grammar_provider` (= 15). The cited path:line is a stale SK-V15-era number carried verbatim into a materially-changed regenerated file, and the claim it anchors (leak roots excluded) is falsified at that location at HEAD. | `awk 'NR>=2367,NR<=2379'` = `is_allowed_path` body + `validate_backend_shape_surface` header (no leak-root list); `grep -c runtime_generator.rs lock14_baseline.rs` = 8; `grep -c grammar_provider` = 15; the allowed-root list with `json_sink_direct.rs`/`json_typed_direct.rs`/`json_templates/*`/`grammar_profile.rs`/`lower/mod.rs` is at `:345-435`. | Does NOT block. The rows are explicitly fenced `SK-V15 HISTORICAL (non-SK-V18-cost)` (`2C:59`) and "retained verbatim"; the LIVE SK-V18 generalisation finding rests on the *current* `strategy.rs:137-185` 9-ident table and the 13-site self-gate count, both of which resolve EXACTLY. Fix: re-anchor the three citations to the live root list (`lock14_baseline.rs:345-435`) and note that at HEAD the leak roots are now PRESENT in the scan file (the SK-V15-era exclusion is superseded), OR mark the row's path:line as a frozen SK-V15-era pointer no longer valid at HEAD. The `CONSOLIDATED-AUDIT.md:45-47` companion citation is unverified-but-internal; the binding defect is the falsified `lock14_baseline.rs:2370-2379` resolution. |
| CH1-V4-SK18-02 | all | none | Every published-paper citation EXISTS and carries the claimed finding (re-confirmed on the web this cycle). | Pratt "Top Down Operator Precedence" POPL 1973 DOI 10.1145/512927.512931 (binding-power / precedence-climbing — matches 2C's Sheets negative-control); iburg (Fraser/Hanson/Proebsting) LOPLAS 1(3) 1992 pp 213-226 DOI 10.1145/151640.151642 (tree-pattern-match + DP dispatch-on-selected-pattern — matches 2D R-A); Mison PVLDB 10(10) 2017 DOI 10.14778/3115404.3115416 (projection/filter pushdown into the parser — matches 2D SinkOnly); egg PACMPL 5(POPL) 2021 DOI 10.1145/3434304 (equality saturation pipeline — matches 2D); simdjson/Langdale-Lemire VLDBJ 28(6) arXiv:1902.08318 (two-stage structural-index/tape). All authors/venue/finding confirmed. | No block. |
| CH1-V4-SK18-03 | 2E | none | The svmatch refutation's load-bearing external citation matches the literature's ACTUAL position, including a subtle body-vs-comments attribution. | Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" — web-verified LIVE: post body argues SVE2 `match` is fastest and references the Langdale/Lemire table-driven NEON classifier; the `vceqq_u8` eq-fan and `vqtbl1q_u8` shuffle appear in the COMMENT thread (commenter "-.-"), NOT the author's benchmark. 2E's CH4-V3-03 fold (eq-fan = commenter route; binding grounding = simdjson/Langdale-Lemire lineage + on-disk kernel; SVE2 `match` host-absent) is exactly the source's position. | No block. |
| CH1-V4-SK18-04 | 2E, 2B | none | The most falsifiable HARDWARE claim — the M5 Max feature gate — is verified ON THE LIVE HOST, not merely cited. | `sysctl machdep.cpu.brand_string` = `Apple M5 Max`; `hw.optional.arm.FEAT_SVE2` = unknown oid (ABSENT); `FEAT_PMULL/DotProd/I8MM/CSSC/SHA3` all = 1 — exactly `SRC-HOST-PROBE` (`2E:88`). The movemask divergence is real: `movemask.rs:5` = `vshrn_n_u16::<4>`; `byte_class_from_eq_set_64.rs:79-87` = shift-add `vaddv_u8` pack. | No block. |
| CH1-V4-SK18-05 | 2B, 2F | none | Every load-bearing SK-V18 LOCAL SIMD/runtime citation resolves to the claimed path:line, and the V3 `scanners.rs` symbol-home fold (2F) landed. | `find_css_significant` `runtime_simd.rs:169`, two-fan `byte_class_from_eq_set_64(block,set_a) | byte_class_from_eq_set_64(block,set_b)` `:199`, ZERO live callers (only `lib.rs:574` `#[cfg(test)]`, `:500` a comment); `count_top_level_commas` rides the eq-set kernel `:44,:56`; eq-set movemask `byte_class_from_eq_set_64.rs:79-89` (`movemask_u8x16` opens `:79`, dossier's `:80` is ±1); upstream `scan_balanced` `balanced.rs:26`, `<=8` assert `:44-45`, import of `{build_nibble_luts,find_first_of_nibble_lut}` from `crate::scanners` `:27`, both kernels at `scanners.rs:235`/`:262`, `structural_bitmap.rs:94` a comment; CSS hot leaf `find_component_delim` `generated.rs:657`, `consume_balanced_at` `:693`. | No block. |
| CH1-V4-SK18-06 | 2C | none | The LIVE SK-V18 falsifiable refutations resolve EXACTLY, including a verbatim-numeric self-gate falsification. | `strategy.rs:137-185` = 9 grammar `idents` rows (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty); `for_grammar_with_manifest(...PRODUCTION_MANIFEST_TABLE)` `:216`; `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = exactly **13** sites (the dossier's Lock-14-self-gate RED count); `css_types.rs` = 66 LOC, line-1 `//! Host shims for the CSS L4 grammar's -> parse_hex_color(...) map`; Pattern-H census `find ... | wc -l` = 71 (2C correctly cites 71, supersedes SK-V15-era 67). | No block. |
| CH1-V4-SK18-07 | 2D | none | The decision-engine load-bearing local citations all resolve verbatim. | `NormalizeDirectSinkCost` instantiation `backend_egraph.rs:75`, `struct ...:191` / `impl Rewrite ...:193`; `BackoffScheduler::default()` `:73`; `Extractor::new(&graph,&DecisionCostModel)` `:84-87`; `select_lowering(cost.chosen)` over exactly five `BackendShape` arms `lower/mod.rs:18-24`; `collapsed_stage.rs:16` = `tape_plan::render_rule(rule, TapeFlavor::Collapsed)`; `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:39-43`. | No block. |
| CH1-V4-SK18-08 | 2A, 2B, 2E, 2F | none | No refuted-technique row contradicts the literature; the REDRESS benchmark numbers trace to named corpus+platform. | dav1d PIXEL kernels not-transferable / only PROCESS (correct); checkasm PASS != speedup (matches FFmpeg correctness/--bench split); x86/AVX-512 cannot close an M5 Max row (consistent with the verified SVE2-absent aarch64 host + P1 x86 delete); svmatch-on-NEON refuted (matches SVE2 host-absence + Lemire-2026). REDRESS 144 `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT (+109.87%, Track-1 444.208 vs 434.1316 Mbps, `nonjson_css_l4/track1_generated_css_l4_decl_values` Criterion, strict cssparser/lightningcss green); REDRESS 126 `ROUTE-PRODUCTION-SPLIT` microbench 4.718279341× WITHOUT production wiring — both trace to named REDRESS ledger entries + Criterion harness + the M5 Max host. | No block. |

## Evidence Inspected

- All six target dossiers (2A 282 lines, 2B 437, 2C 377, 2D 144, 2E 252, 2F 197).
- checkasm census: `ls skinny/crates/bbnf-simd/tests/checkasm_*.rs` = 14 files;
  `checkasm_common.rs` head = `#![allow(dead_code)] / pub struct Xorshift64`
  (helper, not a harness); `checkasm_parity.rs:1-6` "Modelled on FFmpeg's
  `tests/checkasm/checkasm.h`", `:129-130` `classify_reference`/`scan_scalar`,
  `:206` (the sole aggregate) → 13 differential harnesses.
- Decision-engine: `backend_egraph.rs:73-87,191-193` (`NormalizeDirectSinkCost`/
  `BackoffScheduler`/`Extractor`/`DecisionCostModel`), `lower/mod.rs:18-24`,
  `collapsed_stage.rs:16`, `grammar_provider.rs:39-43`.
- SIMD/runtime: `runtime_simd.rs:6-8` (inaccurate comment), `:44,:56,:169,:199`;
  `lib.rs:500,574` (dead-caller census); `byte_class_from_eq_set_64.rs:79-89`;
  `movemask.rs:1-8` (canonical SHRN).
- Totality-tree generic-crate sources: `crates/ir/src/registry/strategy.rs:137-216`
  (9 idents + `for_grammar_with_manifest`), `crates/core/src/css_types.rs:1`
  (66 LOC host-shim), the live `rg` self-gate count (13 sites), Pattern-H census = 71.
- Lock-14 gate file: `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (5095 LOC) —
  `:2367-2379` (the stale-cited `is_allowed_path`/`validate_backend_shape_surface`,
  NOT a scan-root list), `:345-435` (the live allowed-root list, leak roots PRESENT),
  `grep -c` runtime_generator.rs = 8, grammar_provider = 15.
- Upstream parse-that substrate: `parse-that/rust/parse_that/src/parsers/scan/`
  (full file census; `balanced.rs:26,27,44-45`, `structural_bitmap.rs:94` comment),
  `parse_that/src/scanners.rs:235,262` (the real home of the two nibble-LUT kernels).
- CSS hot leaf: `css_l4_declaration_values/generated.rs:657,693`.
- REDRESS provenance: `skinny/REDRESS.md:3768` (126 `ROUTE-PRODUCTION-SPLIT`,
  4.718279341×), `:4420-4438` (144 `G-W12-SIMD-ASM-PRODUCTION`, +109.87%,
  444.208/434.1316 Mbps).
- Host probe (live): `sysctl machdep.cpu.brand_string` = `Apple M5 Max`;
  `FEAT_SVE2` ABSENT; `FEAT_PMULL/DotProd/I8MM/CSSC/SHA3` = 1.
- Web (paper/URL existence + claimed finding): Pratt POPL 1973
  DOI 10.1145/512927.512931; iburg LOPLAS 1992 DOI 10.1145/151640.151642 (pp 213-226);
  Mison PVLDB 2017 DOI 10.14778/3115404.3115416; egg POPL 2021 DOI 10.1145/3434304;
  Lemire 2026-04-19 ARM-match post (body = SVE2 `match` + TBL classifier; eq-fan/
  shuffle in COMMENTS — attribution fold verified).

## Fold Requirements (REVISE dossier)

- **2C** — Re-anchor the stale `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379`
  citation in the three SK-V15-HISTORICAL rows (`:81` SK-V15-2C-GATE-EXCLUSION
  evidence cell, `:138` refuted-assertions table, `:164` LAC-2C-SK15-05 evidence cell)
  to the live allowed-root list (`lock14_baseline.rs:345-435`), and add a note that at
  HEAD the JSON leak roots (`runtime_generator.rs`, `grammar_provider`/`per_grammar_provider`,
  `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates/*`) are now PRESENT in
  the scan file — i.e. the SK-V15-era "leak roots excluded" finding is superseded at HEAD.
  Alternatively, mark the `:2370-2379` pointer as a frozen SK-V15-era line no longer
  valid against the regenerated file. The LIVE SK-V18 generalisation findings (the
  9-ident `strategy.rs:137-185` table, the 13-site self-gate RED, `css_types.rs`) are
  UNAFFECTED — they resolve exactly and stand; only the historical-row's path:line is stale.

## Convergence Impact

CH1 does NOT block T-P2 V4 convergence. The single REVISE is a citation-precision
correction to a frozen SK-V15-HISTORICAL row whose path:line drifted under file
regeneration; it is neither a confabulated/unverifiable source nor a falsification of
a LIVE SK-V18 grounding or refutation. Every published paper (Pratt, iburg, Mison,
egg, simdjson), every external technique URL (Lemire-2026, Kutenin, Validark, ACLE),
every load-bearing LIVE SK-V18 library path:line (the decision engine, the eq-set/
two-fan kernels, the 9-ident leak, the upstream parse-that substrate, the CSS hot
leaf), the REDRESS benchmark provenance, and the M5-Max/SVE2-absent host gate all
resolved to their claimed source and finding — several re-verified directly on the
web and one (the host probe) on the live machine. The 2A and 2F V3 REVISE folds are
correctly applied. The one REVISE fold is orphan-free and can be applied without
touching any LIVE grounded technique row or refuted-route position.

TALLY accept=5 revise=1 reject=0
