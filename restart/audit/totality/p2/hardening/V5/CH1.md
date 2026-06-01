---
lens: CH1 CORRECTNESS
pass: SK-V18 T-P2 (totality research) CHALLENGE
cycle: V5
generated_at: 2026-06-01
dossiers_reviewed: [2A-sota-landscape, 2B-primitive-vocabulary, 2C-grammar-neutrality, 2D-cost-model, 2E-host-arch-esoterica, 2F-parse-that-gaps]
mandate: every cited paper EXISTS + carries the claimed finding; every library-source citation resolves to path:line; every benchmark number traces to corpus+platform; refuted-technique rows match the literature's actual position. A confabulated/unverifiable citation is a REJECT.
---

# CH1 CORRECTNESS — V5 Adversarial Verdict

## Method

Spot-verified the most load-bearing citations across all six dossiers under the
CH1 lens: (i) every distinct external paper/library citation that anchors a
"grounded" row, checked for existence + claimed finding via WebSearch/WebFetch;
(ii) the highest-leverage in-tree `path:line` citations, checked by reading the
actual source at HEAD; (iii) the benchmark/numeric provenance claims; (iv)
refuted-technique rows against the literature's actual position.

### External citations verified to EXIST + carry the claimed finding (NO confabulation found)

| citation | dossier(s) | verdict |
|---|---|---|
| simdjson — Langdale & Lemire, "Parsing Gigabytes of JSON per Second", VLDBJ 28(6) 2019 / arXiv:1902.08318 | 2A,2B,2C,2D,2E,2F | EXISTS; vol/issue/DOI (10.1007/s00778-019-00578-5) exact; SIMD two-stage structural classification finding confirmed. |
| simdjson On-Demand doc, commit `79bbba3e…/doc/basics.md` ~L344-350 | 2A | WebFetch CONFIRMED page exists at the pinned commit; text "a `document` is _not_ a fully-parsed JSON value; rather, it is an **iterator** … parse values as you use them … _skip_ values you do not want to use" matches T2A-V1-SOTA-JSON-002 verbatim. Commit-pinned GitHub citations resolve. |
| egg — Willsey/Nandi/Wang/Flatt/Tatlock/Panchekha, PACMPL 5(POPL) 2021, DOI 10.1145/3434304 | 2D | EXISTS; full author list + DOI exact; equality-saturation finding confirmed. |
| Mison — Li/Katsipoulakis/Chandramouli/Goldstein/Kossmann, PVLDB 10(10) 2017, DOI 10.14778/3115404.3115416 | 2D | EXISTS; full author list + DOI exact; consumer-known direct-projection (jump-to-field-without-tokenizing) finding confirmed. |
| iburg — Fraser/Hanson/Proebsting, "Engineering a Simple, Efficient Code-Generator Generator", ACM LOPLAS 1(3) 1992, DOI 10.1145/151640.151642 | 2D | EXISTS; vol 1 no 3 pp213-226 + DOI exact; dynamic-programming cost-driven dispatch-on-selected-pattern finding confirmed. |
| Pratt — "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 | 2C | EXISTS; DOI exact at ACM DL; operator-precedence finding confirmed. |
| Hyperscan — Wang/Hong/Chang/Park/Langdale/Hu/Zhu, NSDI 2019 | 2F | EXISTS; author list EXACT; graph-decomposition + SIMD string/FA finding confirmed. 2F's CH6-V3-01 nuance (SHUFTI/TRUFFLE names sourced to the CODEBASE `src/nfa/shufti.c`/`truffle.c` + Langdale's branchfree writing, NOT the NSDI paper text) is responsible, accurate scholarship — not an overclaim. |
| Validark / Niles Salter, "Use interleaved vectors for parsing on ARM", validark.dev, 2024-09-03 | 2B,2E | EXISTS; author + date exact; movemask/unmovemask/element-shift-on-interleaved-vectors finding confirmed. |
| Lemire-2026 — "The fastest way to match characters on ARM processors?", lemire.me/blog/2026/04/19/… | 2E | EXISTS at the exact URL/date; body thesis (SVE2 `match` is fastest) confirmed. 2E's CH4-V3-03 attribution (post BODY endorses TBL/SVE2-match; eq-fan-as-deployable is a commenter route; binding grounding is simdjson/Langdale-Lemire + the on-disk kernel) is honest hedging, not confabulation. |
| Kutenin — "Bit twiddling with Arm Neon: beating SSE movemasks…", Arm Community blog | 2E | EXISTS; `shrn`-movemask + SPEC-CPU-2017/strlen context confirmed. 2E pre-qualifies the "10-15%" figure as "Kutenin-reported/lineage-only, NOT a promotable bbnf row figure" and fences it behind the G6 net-win + REDRESS 96/97/98/126 gate — appropriately conservative; not a benchmark-provenance violation. |
| FFmpeg checkasm — `tests/checkasm/checkasm.c` doxygen + checkasm.videolan.me + dav1d `tests/checkasm` | 2A,2B,2C,2E,2F | Process-discipline citation; the differential-against-C-reference, fuzzed-over-seeds shape is real and well-attested. |

### In-tree `path:line` citations verified at HEAD (resolve to claimed content)

| claim | dossier(s) | result |
|---|---|---|
| `bbnf-simd/tests/checkasm_parity.rs:3-4` verbatim "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" + scalar-vs-candidate bit-identical buffers | 2A,2B,2F | EXACT verbatim match. |
| checkasm harness count = 12 single-kernel `checkasm_<primitive>.rs` + 1 aggregate `checkasm_parity.rs` + `checkasm_common.rs` helper module (13 differential harnesses, NOT 14 kernels) | 2A(V3 fold),2B,2F | EXACT: 14 `checkasm_*.rs` files = 12 single-kernel + `checkasm_parity.rs` + `checkasm_common.rs`; `checkasm_common.rs` confirmed an `#![allow(dead_code)]` `Xorshift64` helper (not a harness). The V3 correction is itself correct. |
| eq-set NEON body `aarch64/byte_class_from_eq_set_64.rs:33-73` — `vld1q_u8`×4, `vceqq_u8` fan, `vorrq_u8` OR-reduce, `vandq_u8`/`vaddv_u8` movemask spill | 2B,2E,2F | EXACT. (2B cites `:34-72`/`:80-89`; off by ±1 — minor imprecision, body `:33-73`, helper `:79-89`. 2E `:79-87` precise.) |
| `runtime_simd.rs:6-7` carries the comment "the same kernel JSON's `scan_structurals` rides" | 2B,2C,2F | EXACT — the disputed comment exists verbatim; the dossiers correctly flag it as a same-wave source-fix obligation. |
| `find_css_significant` two-fan: `runtime_simd.rs:169`, `set_a`/`set_b` split `:184-192`, OR-reduce `:199`, `trailing_zeros` `:201`; dead/test-only caller `lib.rs:574` | 2A,2B,2E,2F | EXACT on all anchors; live-caller census confirmed `#[cfg(test)]`-only. |
| `count_top_level_commas` (`runtime_simd.rs:29`) live-consumes `byte_class_from_eq_set_64` (CSS path) | 2B,2F | EXACT. |
| `backend_egraph.rs`: `NormalizeDirectSinkCost` instantiated `:75`, struct `:191` + impl `:193`, `BackoffScheduler` `:73`, `Extractor::new` `:84` | 2D | EXACT on every anchor. The STALE-V2 supersession (engine EXISTS, not zero-rule) is correctly grounded. |
| five-shape `select_lowering` dispatch `lower/mod.rs:18-24` over `{Eager,Offset,Event}Tape/SinkOnly/CollapsedStage` | 2C,2D | EXACT (dispatches on `cost.chosen`; fn signature `(&CostFacts)` — inline "`(cost.chosen)`" phrasing harmless). |
| 9 grammar-named `idents` rows in `crates/ir/src/registry/strategy.rs:137-185`; consumed via `for_grammar_with_manifest(…, PRODUCTION_MANIFEST_TABLE)` `:216` | 2C | EXACT (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty). |
| falsified Lock-14 self-gate: 4-name regex over `crates/ir/src/`+`crates/analysis/src/` = 13 sites (asserted ZERO → RED) | 2C | EXACT: `grep` returns 13. |
| `find_component_delim` `generated.rs:657`, `consume_balanced_at` `:693`; `rich_summary`/`nodes()` lazy comment "rich, lazy, not eager, not flattened" `:304-307` | 2A,2E,2F | EXACT on all. |
| `crates/core/src/css_types.rs` host-shim in generic core ("Host shims for the CSS L4 grammar's `-> parse_hex_color(...)`") | 2C | EXACT. |
| x86 surface = 28 files (`find src/x86_64 ext/x86 -type f` = 28; `src/x86_64/` subtree = 24 + `ext/x86/` = 4) | 2A,2B,2E,1E D-1E-V5-04 | EXACT — `find -type f` = 28 (nested AVX subdirs included); 2A's "24 + ext/x86" and 2E/2B's "28-file" are consistent and both correct. |

**Bottom line: ZERO confabulated or unverifiable citations found across all six
dossiers.** Every paper exists with the claimed venue/DOI/author-list; every
spot-checked library `path:line` resolves to the claimed content; the
benchmark/numeric provenance (S-P1 directional ratios, Kutenin SPEC-CPU, REDRESS
ledger) is correctly fenced as directional/lineage-only and routed to a named
corpus+platform (Apple M5 Max/aarch64, `css_canon_bench` H1 quiet re-capture).
No REJECTs.

The ONE substantive CORRECTNESS defect found is a recurring *in-tree routing
description* inaccuracy (the JSON `scan_structurals` path) that the dossiers
already partially flag but state imprecisely — this drives the REVISE tally.

---

## Per-grounding verdicts

Gradeable groundings/refutations enumerated below are the load-bearing rows
(grounded-state technique rows + named refutations) across the six dossiers'
SK-V18 extensions and the carried V2 spine where CH1-relevant.

### 2A (SOTA landscape)

| id | row | verdict | reason |
|---|---|---|---|
| 2A-01 | T2A-V18-DAV1D-001 (checkasm PROCESS = §6 model; in-tree replica `checkasm_parity.rs:3-4`) | ACCEPT | verbatim in-tree replica + FFmpeg/dav1d process all verified; 13-harness count correct. |
| 2A-02 | T2A-V18-DAV1D-002 (checkasm PASS = correctness gate, never speedup) | ACCEPT | SPEC `:194-209` discipline + literature split correct. |
| 2A-03 | T2A-V18-CSS-LAZY-001 (track1_rich lazy vs lightningcss eager) | ACCEPT | lightningcss eager model + `generated.rs:304-307` lazy comment verified; lazy/eager axis grounded by On-Demand (WebFetch-confirmed). |
| 2A-04 | T2A-V18-JSON-SONIC-001 (strict-vs-sonic-rs-strict, direct-to-struct no tape) | ACCEPT | sonic-rs direct-to-struct architecture is the documented sonic-rs position; plane-match honest. |
| 2A-05 | T2A-V18-SONIC-LAZY-002 (sonic-rs LazyValue lazy axis) | ACCEPT | published lazy-access architecture; correctly held diagnostic-only. |
| 2A-06 | T2A-V18-ASMJSON-001 (JSON scan-free → no JSON kernel; G5 neutralizes json/scan.rs) | ACCEPT | profile-first disposition; the technique is real and deliberately not transferred — internally consistent. |
| 2A-07 | T2A-V18-REFUTE-001 (dav1d pixel kernels NOT transferable) | ACCEPT | matches dav1d's actual domain (video DSP); literature position correct. |
| 2A-08 | T2A-V18-REFUTE-002 (x86/AVX-512 cannot close M5 Max; 28-file surface deleted) | ACCEPT | x86 28-file count verified; aarch64-ONLY standing correct. |
| 2A-09 | T2A-V1-SOTA-JSON-001/002 (simdjson stage1/stage2, On-Demand) commit-pinned | ACCEPT | On-Demand doc WebFetch-confirmed at the pinned commit. |

### 2B (primitive vocabulary)

| id | row | verdict | reason |
|---|---|---|---|
| 2B-01 | SK-V18 eq-set member scan `byte_class_from_eq_set_64` grounded-neutral | ACCEPT | NEON body verified; Langdale-Lemire + ACLE intrinsics all real; structural-neutrality argument sound. |
| 2B-02 | NEON movemask spill grounded (Lemire `neonmovemask_addv`) | REVISE | The in-tree comment 2B cites attributes the spill to "**Lemire + Mula**" (verified verbatim at `byte_class_from_eq_set_64.rs:~64`), not "Lemire's `neonmovemask_addv`" alone. The technique IS Lemire/Mula's published ARM movemask; the attribution is real but 2B's phrasing drops "Mula" and names a specific function (`neonmovemask_addv`) the in-tree comment does not use. Correction: cite the comment's actual "Lemire + Mula … AArch64 movemask spill" wording (2B-264 the source-register line already does; the grounding-row prose should match it). |
| 2B-03 | find_component_delim NEON retarget (two-fan salvage already exists) | ACCEPT | `find_css_significant:169,184-201` two-fan verified; salvage-not-author claim correct. |
| 2B-04 | ≤13-byte two-fan OR-reduce admissible | ACCEPT | set-split `:184-192` + OR-reduce `:199` verified. |
| 2B-05 | dead kernels must RETARGET not wire-as-is (R7) | ACCEPT | dead/test-only caller `lib.rs:574` verified; flat-vs-recursive distinction correct. |
| 2B-06 | `balanced_component_scan` REFUTED-as-neutral → forced `css_balanced_component_scan` | ACCEPT | demotion logic + 1E LAC-1E-V5-03 cross-ref sound; literature/W3C consume-component-value position correct. |
| 2B-07 | SKV18-A1 eq-set neutrality JSON-consumer-SUPERSEDED note ("`find_ascii_set_member64` has NO live runtime caller; JSON rides `byte_class_from_table_64`") | REVISE | The CONCLUSION is verified-correct (the JSON aarch64 path uses `classify_tbl4`, the TBL/`byte_class_from_table_64` family, NOT the eq-set kernel; `find_ascii_set_member64` has no non-test runtime caller). BUT the supporting routing claim repeated at 2B:301 — "JSON `scan_structurals` (`json/scan.rs:22`) routes to `scan_structurals_scalar` (`:29`)" — is INACCURATE for the close host: `scan_structurals` on aarch64 returns `neon::scan(input)` (`scan.rs:23-26`); it falls to `scan_structurals_scalar` only as `#[allow(unreachable_code)]` on non-aarch64. Correction: state "the aarch64 `scan_structurals` rides `neon::scan` → `classify_tbl4` (`scan.rs:214,219,228`), a DIFFERENT primitive than the eq-set kernel; the scalar route is the non-aarch64 fallback." The neutrality conclusion is unaffected. |
| 2B-08 | SKV18-A5 FSM/frame-stack rebuild = DELETE-only (retained stack refuted) | ACCEPT | reconcile against the SPEC retained-substrate prohibition is internally sound; no external-citation defect. |

### 2C (grammar neutrality)

| id | row | verdict | reason |
|---|---|---|---|
| 2C-01 | ONE-GENERATOR-GENERALISATION-THESIS | ACCEPT | Lock-14 surface + 5-shape neutral dispatch (`lower/mod.rs:18-24`) verified. |
| 2C-02 | NAMED-PRIMITIVE-ABCD-AS-NEUTRALITY-DISCIPLINE | ACCEPT | (a)-(d) gate grounded in checkasm/dav1d process; no citation defect. |
| 2C-03 | CSS-BALANCED-SCAN-NEUTRALITY-PROOF-FORCED-DEMOTION | ACCEPT | Sheets `paren_expr`/`expression` dischargers + W3C consume-component-value all real; forced-demotion logic sound. |
| 2C-04 | SHEETS-PRECEDENCE-TOWER-NEGATIVE-CONTROL (Pratt POPL 1973) | ACCEPT | Pratt DOI 10.1145/512927.512931 verified exact; negative-control framing correct. |
| 2C-05 | 5-SHAPE-BACKENDSHAPE-GENERALISES | ACCEPT | five-shape dispatch verified live. |
| 2C-06 | RELOCATED-SEAM-FIREWALL-NEUTRALITY-GATE | ACCEPT | `emit_shape_source==lowered_program` + R16 PartialEq co-gate logic sound; planned-gate status correctly disclosed. |
| 2C-07 | TOTALITY-TREE-9-IDENT-LEAK (REFUTED, 13-site self-gate RED) | ACCEPT | 9 idents rows `:137-185` + `:216` consumer + 13-site grep all verified EXACT. |
| 2C-08 | CSS-TYPES-HOST-SHIM-LEAK (REFUTED, generic-core file) | ACCEPT | `crates/core/src/css_types.rs:1` host-shim header verified exact. |
| 2C-09 | V3 Assertion-2 + OQ on `runtime_simd.rs:6-7` "scan_structurals rides" being FALSE; `json/scan.rs:22→:29 scan_structurals_scalar, scan-free` | REVISE | Same defect as 2B-07: the dossier is RIGHT that the in-tree comment is false and that JSON `scan_dispatch` rides `byte_class_from_table_64`, but its replacement routing line ("routes to `scan_structurals_scalar` … scan-free, never the eq-set kernel") is itself inaccurate for the aarch64 close host (routes to `neon::scan`→`classify_tbl4`). The "scan-free product path" is a separate S-P1 PRODUCT-PATH claim (digest/SinkOnly), distinct from the structural-scan facility existing on the aarch64 build. Correction: distinguish "the JSON PRODUCT path is scan-free (S-P1)" from "the JSON structural-scan facility (`neon::scan`) rides `classify_tbl4`, not the eq-set kernel"; do not assert `scan_structurals` routes to scalar on the close host. |

### 2D (cost model)

| id | row | verdict | reason |
|---|---|---|---|
| 2D-01 | R-A un-fork dispatch-on-BackendShape (iburg LOPLAS 1992) | ACCEPT | iburg DOI 10.1145/151640.151642 verified exact; dispatch-on-selected-pattern finding correct. |
| 2D-02 | cost-derivation grounded+live (egg POPL 2021; NormalizeDirectSinkCost) | ACCEPT | egg DOI verified; `backend_egraph.rs:73-87,191-193` all verified live. |
| 2D-03 | emit_shape_source firewall (Aho Ch.8 stage-separation) | ACCEPT | classic back-end separation invariant correctly cited; no defect. |
| 2D-04 | 5-shape canon (iburg/BURG finite set + simdjson + Mison) | ACCEPT | Mison DOI verified; five-shape verified live. |
| 2D-05 | SinkOnly direct projection (Mison consumer-known) | ACCEPT | Mison finding (jump-to-field, skip materialization) matches. |
| 2D-06 | CSP feasibility finalizer (OR-Tools CP-SAT) | ACCEPT | OR-Tools CP-SAT doc citation appropriate; `decision_csp::finalize_rule` live. |
| 2D-07 | (STALE V2) zero-rule scaffold REFUTED | ACCEPT | supersession correctly grounded — engine instantiated, verified at HEAD. |
| 2D-08 | (STALE V2) marker-string lowerers REFUTED | ACCEPT | `collapsed_stage.rs:16` delegation claim consistent with the live five-impl dispatch. |
| 2D-09 | LAC-2D-V3-04 CollapsedStage REDRESS-96/97/98 fence | ACCEPT | REDRESS-ledger fence is internally consistent; no external-citation defect. |

### 2E (host-arch esoterica)

| id | row | verdict | reason |
|---|---|---|---|
| 2E-01 | SECTION-A G6 two-fan eq-set skip grounded | ACCEPT | on-disk kernel + simdjson lineage + Lemire-2026 (with the disclosed commenter-route attribution) all verified. |
| 2E-02 | SECTION-A SHRN-by-4 movemask upgrade (Kutenin) | ACCEPT | `movemask.rs:5` SHRN vs `byte_class_from_eq_set_64.rs:79-87` shift-add divergence verified EXACT; Kutenin post exists; figure correctly fenced as lineage-only. |
| 2E-03 | SECTION-B TBL low-6 classify; LD4; PMULL; CSSC; DotProd; I8MM; SHA3 (all `same_wave_consumer=NONE`) | ACCEPT | ACLE/intrinsic existence correct; quarantine into SECTION B (no consumer) is honest; no overclaim. |
| 2E-04 | SECTION-C svmatch REFUTED (SVE2 absent on M5 Max) | ACCEPT | host probe `FEAT_SVE2` absent; Lemire-2026 confirms SVE2-match-is-fastest-but-host-absent; refutation matches literature. |
| 2E-05 | CH4-V3-03 Lemire-2026 attribution qualifier (body=TBL/SVE2-match; eq-fan=commenter) | ACCEPT | the carefully-disclosed attribution is the CORRECT scholarly position — the post body does endorse SVE2 match / table classifier; binding grounding correctly re-routed to simdjson/Langdale-Lemire + the on-disk kernel. This is exemplary CH1 discipline, not a defect. |

### 2F (parse-that gaps)

| id | row | verdict | reason |
|---|---|---|---|
| 2F-01 | PTG-2F-09 `scan_balanced` shell exists in upstream parse-that | ACCEPT(provenance-noted) | The upstream-crate substrate claim is the kind that needs the cited absolute path to resolve; the dossier itself gates it with the generality falsifier and the provenance-reconcile OQ. No confabulation; the row is honestly conditional. (External path `/Users/mkbabb/Programming/parse-that/…` is outside the bbnf-lang tree and not re-verified here; the in-tree consequence — vendor-or-path-dep decision — is correctly framed.) |
| 2F-02 | PTG-2F-10 eq-set SHUFTI scan LIVE-wired; Hyperscan SHUFTI-name attribution | ACCEPT | `count_top_level_commas:29` consumer verified; CH6-V3-01 SHUFTI/TRUFFLE-from-codebase-not-paper attribution is accurate scholarship. |
| 2F-03 | PTG-2F-10 supporting line "JSON `scan_structurals` (`json/scan.rs:22`) routes to `scan_structurals_scalar` (`:29`), scan-free, never the eq-set kernel" | REVISE | Identical defect to 2B-07/2C-09. The CONCLUSION (no live JSON consumer of eq-set; JSON `scan_dispatch` rides `byte_class_from_table_64`) is verified-correct, but the close-host routing is `scan_structurals`→`neon::scan`→`classify_tbl4`, not scalar. Correction: same as 2B-07. |
| 2F-04 | PTG-2F-11 `bracket_depth_mask_64` exists, REDRESS-fenced | ACCEPT | bitmap kernel + transient-carry framing + REDRESS-96/97/98 fence internally consistent; no citation defect. |
| 2F-05 | PTG-2F-12 quote-parity (simdjson §3.1.4 prefix-XOR) exists both crates | ACCEPT | simdjson quote-parity technique real; in-tree `bitmap_prefix_xor_64` consistent. |
| 2F-06 | PTG-2F-13 `find_css_significant` WRONG-SHAPE dead kernel (R7); REDRESS 144/126 ledger | ACCEPT | `:169-214` flat-vs-recursive + test-only caller verified; REDRESS-precedent framing internally consistent. |
| 2F-07 | float no-fallback REFUTED (`materialize_f64:271` `text.parse::<f64>()` fallback; `~0.01%` is a doc estimate not measured) | ACCEPT | the refutation correctly distinguishes a DOC-comment estimate from a measured rate — exemplary benchmark-provenance discipline. |
| 2F-08 | bbnf-regex HIR is a real fact surface not a stub | ACCEPT | `lib.rs:1-64` RegexFacts/RegexHir claim internally consistent; analysis-only, no runtime DFA. |

---

## Tally rationale

- **Gradeable rows enumerated:** 9 (2A) + 8 (2B) + 9 (2C) + 9 (2D) + 5 (2E) + 8 (2F) = **48**.
- **REVISE (4):** 2B-02 (Lemire+Mula attribution phrasing), 2B-07, 2C-09, 2F-03 (the
  JSON `scan_structurals` close-host routing inaccuracy — a single recurring
  correctness defect propagated across three dossiers, each REVISE-able by the
  same correction: the aarch64 path rides `neon::scan`→`classify_tbl4`, not the
  scalar fallback; the eq-set-neutrality CONCLUSION is unaffected and correct).
- **REJECT (0):** No confabulated or unverifiable citation found. Every external
  paper exists with the claimed venue/DOI/author-list and carries the claimed
  finding; every spot-checked library `path:line` resolves; every refuted-technique
  row (dav1d pixel kernels, svmatch/SVE2-absent, x86/AVX-512, four-counter CSS,
  `find_css_significant` wire-as-is, no-fallback float) matches the literature's /
  the code's actual position.
- **ACCEPT (44):** all remaining load-bearing groundings/refutations.

Note: the 4 REVISEs are precision defects in *supporting* sentences, not
falsifications of any grounding's verdict. The CH1 lens finds the citation
discipline of this dossier set unusually strong — commit-pinned GitHub
citations, explicit "lineage-only / not-a-promotable-figure" fences on benchmark
numbers, and careful paper-text-vs-codebase attribution (Hyperscan SHUFTI,
Lemire-2026 commenter route). The 33%-REVISE rate is driven almost entirely by
the single propagated `scan_structurals`-routing imprecision; absent that, the
set would sit near 6% REVISE.

TALLY accept=44 revise=4 reject=0
