---
agent: 1D
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-28T04:19:00Z
spec_surfaces_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - skinny/REDRESS.md
  - skinny/RESULTS.md
  - restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md
  - restart/skinny/tranches/sk-v15/HANDOFF.md
  - restart/skinny/tranches/sk-v15/SYNTHESIS.md
  - restart/skinny/tranches/sk-v15/research/alpha/alpha-A-results-extraction.md
  - restart/skinny/tranches/sk-v15/research/alpha/alpha-B-competitor-deltas.md
  - restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md
  - restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md
  - restart/skinny/tranches/sk-v15/research/alpha/alpha-E-candidate-shortlist.md
  - restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md
  - restart/skinny/tranches/sk-v15/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
  - restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A1-measurement-integrity.md
  - restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md
  - restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md
  - restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md
  - restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md
  - restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md
  - restart/skinny/tranches/sk-v15/research/p1/p1a-samply-mode-1.md
  - restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md
  - restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md
  - restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md
  - restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md
  - restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md
  - restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md
  - restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md
  - restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md
  - restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md
  - restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md
files_audited_count: 30
live_truth_method: "nl -ba on PASS-1-EXCAVATION, REDRESS, RESULTS, PASS-IMPL V1 consolidated audit, SK-V15 handoff/synthesis/alpha/S-P0/S-P1/S-P2 docs; rg for SK-V15/P0/P1/P2 discovery and REDRESS anchors; git status --short preflight to avoid unrelated dirty work"
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - CH2-FOLD-004 added pass-layer JSON-shape leak citations P1-1B-D9/P1-1B-D10
    - CH3-V1-005 added explicit REDRESS-183/184/209..213 wave-graph pre-block row
    - CH4-V1-002 added LOC/risk fields for divergence buckets
    - CH5-V1-04 and CH5-V1-05 added CSS sidecar and broadcast-admission handling
  first_cycle_additions:
    - SKV15-J-JSON-51-GUARD
    - SKV15-J-PARSEONLY-MEMCHR
    - SKV15-J-DIRECT-TYPED-STRICT-PRODUCTS
    - SKV15-J-FNV-BENCH-QUARANTINE
    - SKV15-J-PMU-CB-MISSES
    - SKV15-C-CSS-BROADCAST-DEMOTION
    - SKV15-C-CSS-GENERATED-RS-DEMOTION
    - SKV15-C-CSS-VALUE-API-GAP
    - SKV15-G-LOCK14-LOCK16-GATE-HOLES
    - SKV15-G-PATTERN-H-OWNERSHIP-GAP
    - SKV15-G-DECISION-ENGINE-SCAFFOLD
    - SKV15-G-SAME-TAPE-SUBSTRATE
    - SKV15-G-AARCH64-ONLY-ADMISSION
    - SKV15-G-PRIMITIVE-CANDIDATE-GATES
divergence_count:
  spec_claims_implemented: 7
  spec_claims_unimplemented: 10
  impl_exceeds_spec: 3
  unknown: 8
locks_amendment_candidates: 0
---

## Executive Summary

SK-V15 does not reopen the old row ledger as a clean generalisation proof. It
starts from a split truth: JSON is a validated 51/51 guard baseline, while CSS
L4 is audit-demoted and must be pruned/rebuilt before the V1 spec can cite it
as a second worked grammar. The SK-V15 handoff states the current
classification directly: JSON honest, CSS contrived, generic infrastructure
mixed, Pattern H not collapsed, and Decision Engine scaffold-only
(`restart/skinny/tranches/sk-v15/HANDOFF.md:8-18`). The close condition
requires JSON guard preservation, CSS anti-broadcast repair, CSS typed Value
API, Lock 14/16 gate restoration, codegen neutrality, Pattern H generated
ownership, Decision Engine activation, FNV quarantine, and executable close
evidence (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:34-50`).

For totality V1, the durable lessons separate into three buckets. JSON-empirical
lessons prove strict same-plane measurement and product equality can work, with
bench-only FNV and fresh PMU c/B misses pending. Grammar-neutral lessons prove
the single tape/substrate direction and native aarch64 admission discipline, but
disprove current Lock 14/16, Pattern H, codegen-neutrality, and Decision Engine
claims. CSS lessons are not admits: they are audit-demoted facts that must drive
PRUNE-WAVE-A and REBUILD-WAVE-E before any CSS >SOTA claim returns.

## Spec-Claim ↔ Implementation Table

| totality V1 claim that must reflect skinny evidence | skinny evidence | verdict | note |
|---|---|---|---|
| JSON can remain the guard proof-of-concept for strict same-plane measurement. | `skinny/RESULTS.md` records JSON parse_only 17/17, direct_to_struct 17/17, and real_typed_struct 17/17 admitted, plus CSS 24/24 separately (`skinny/RESULTS.md:139-141`). SK-V15 Alpha explicitly marks the three JSON families as validated guard baseline (`restart/skinny/tranches/sk-v15/research/alpha/alpha-A-results-extraction.md:14-19`). | proved / JSON-empirical | V1 should cite JSON as a guard baseline, not as proof that CSS or arbitrary grammars are clean. |
| JSON parse_only can beat the strict parser comparator with a distinct parse_only contract. | W11W admits the final six parse_only rows with `memchr2` trusted-string split (`skinny/REDRESS.md:6254-6265`), correctness gates (`skinny/REDRESS.md:6266-6271`), positive cold margins for all six residual rows (`skinny/REDRESS.md:6272-6280`), and 17/17 parse_only close (`skinny/REDRESS.md:6281-6284`). | proved / JSON-empirical | This is JSON parse_only evidence only; S-P1 later keeps it as guard evidence and does not mutate RESULTS (`restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md:19-27`). |
| JSON direct and typed rows can be strict product rows rather than digest-plane rows. | RESULTS notes direct rows are strict product rows, typed rows are strict typed product rows, and CSS rows are a separate full-parse plane (`skinny/RESULTS.md:147-149`). W11A closes strict-product direct rows (`skinny/REDRESS.md:5853-5871`), and W11U admits `unicode_escapes` direct/typed through raw JSON string lexeme products (`skinny/REDRESS.md:6213-6220`; `skinny/RESULTS.md:45-46`). | proved / JSON-empirical | Keep strict product terminology precise; do not collapse it back to digest-plane proof. |
| JSON guard evidence is clean enough to preserve, but W11L/W11N/W11O FNV closed-enum products need quarantine. | PASS-IMPL V1 flags W11L/W11N/W11O FNV closed-enum strict-product weakness as bench-only (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60-65`). SK-V15 requires FNV products to stay bench-only and comparator hardening to catch closed-enum sidecar coupling (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:48`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:47`). | pending / JSON-empirical | V1 must prevent this technique from becoming a production equality arbiter. |
| Fresh P1 evidence does not reverse JSON admits, but exposes c/B debt for S-P2. | P1-D reports parse_only Track 1 beats best strict comparator on 17/17 rows, while direct_strict misses `mesh` and `unicode_escapes`, and real_typed misses `unicode_escapes` (`restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:70-73`). P1-B states those misses are measurement-only S-P2 inputs, not admission reversals (`restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:37-40`). | pending / JSON-empirical | V1 should distinguish admission ledger state from current research debt. |
| Harness/checksum/profile hot leaves are not parser primitives. | P1-E makes the normalized attribution ledger binding and blocks generated wrappers, schema-specific products, comparators, checksums, and sidecar drift unless mapped to grammar-neutral boundaries (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31-35`). P1-C warns structural-scan profiles often show hash/checksum work and must not be mistaken for parser wins (`restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md:37-45`). | proved / JSON-empirical | V1 research gates should require hot-leaf classification before candidate selection. |
| CSS L4 24/24 rows are not admitted V1 evidence. | PASS-IMPL V1 headline says all 24 CSS rows are one measurement broadcast 24 times (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-27`). SK-V15 starting state brackets CSS as audit-demoted/reopened despite the SK-V14 ledger (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:57-68`). P1-F says CSS rows still appear in RESULTS but remain PRUNE-WAVE-A input (`restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md:24-31`). | disproved / CSS audit-demoted | V1 must not cite CSS L4 rows as 24 independent admits. |
| CSS L4 live generated code is not grammar-derived. | PASS-IMPL V1 identifies the generator as a 646-line hand-written `CSS_GENERATED_RS` string literal copied into the seven CSS modules (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:31-33`). S-P0 A4 confirms `emit_frontend_facts` writes `normalize(CSS_GENERATED_RS)` and all seven generated files are byte-identical (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:24`). | disproved / CSS audit-demoted | V1 must require grammar-derived emission before CSS returns as proof. |
| CSS admission comparator/equality plane is not same-workload typed output. | PASS-IMPL V1 says `CssFullParseSummary` is four counters and lightningcss builds a full CSSOM; cssparser beats Track 1 in the same measurement (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29`). A2 adds that CSS equality is marker/status checking, not equivalent value equality (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:45-46`). | disproved / CSS audit-demoted | V1 must bind CSS to cssparser near-term and lightningcss only after CSSOM/value parity. |
| CSS Value API is absent. | PASS-IMPL V1 says JSON has `JsonValue` + view/visitor, but CSS parse returns `Result<String, CssFactError>` fact-stream output (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56-58`). SK-V15 close requires typed CSS value/document/view/visitor surfaces (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:40-42`). | spec_claims_unimplemented / CSS audit-demoted | This is REBUILD-WAVE-E, not a documentation clarification. |
| The retained tape/structural projection direction is the correct substrate boundary. | REDRESS records direct view as a typed projection over sealed tape offsets, not a parallel struct tree (`skinny/REDRESS.md:126-132`). P2-D confirms live `Tape` owns source, offsets, sparse flags, payload arena and id, while `ValueRef` is `&Tape + cursor` (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:12-15`). | proved / grammar-neutral | V1 should preserve "structural projection is the tape" while rejecting retained sidecars. |
| Retained structural sidecars, streaming cursors, and union-substrate imports remain blocked. | P2-A says simdjson/sonic retained structural indexes and parser-owned cursors cannot be imported; REDRESS 51/53/97/98 already rejected cursor/sidecar routes (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:36-37`). P2-D repeats that retained class columns, streaming cursors, and class-lane-only union substrate were retired or blocked (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:31-32`). | disproved / grammar-neutral | Candidate primitives must consume local masks into the existing tape/event loop. |
| Native Apple M5 Max/aarch64 is the admission platform for this tranche. | Handoff says x86 and AVX-512 rows are diagnostic signals, not SK-V15 anchors (`restart/skinny/tranches/sk-v15/HANDOFF.md:16-18`). P2-C repeats x86 cannot rescue or anchor any candidate (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:14`, `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:59`). | proved / grammar-neutral | V1 should not let x86-only comparator pressure close SK-V15 claims. |
| Lock 14 / Lock 16 gates are currently insufficient. | PASS-IMPL V1 says Lock 14 scan roots exclude `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, and `json_templates/` (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45-47`). A3 adds the scan token universe is JSON-only and Lock 16 report coverage is incomplete (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:26-31`). | disproved / grammar-neutral | PRUNE-WAVE-B is required before generic cleanliness is credible. |
| Pattern H is not collapsed. | PASS-IMPL V1 reports 67 runtime files, 0/67 generated headers, four runtime styles, and CSS L4 `OpenFrame` as a canonical violation (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:37-43`). A4/A6 reconfirm 67 files and zero line-1 generated provenance (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:17-18`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:23-28`). | disproved / grammar-neutral | PRUNE-WAVE-D must prove generator ownership, not header-only paper close. |
| Decision Engine is scaffold, not load-bearing. | PASS-IMPL V1 says e-graph has zero rewrite rules, CSP is tautological, and four lowerers are 17-LOC stubs (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:49-55`). A5 confirms emitted runtime code ignores rule plans, e-graph has zero rewrites, CSP preserves selected index, grammar-named facts remain, and four lowerers are label-string stubs (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md:10-17`). | disproved / grammar-neutral | REBUILD-WAVE-F must produce runtime-relevant generated diffs. |
| Candidate primitives are research gaps, not admits. | P2-A lists candidate primitives such as byte-class masks, byte-set skip, string events, escape decode, number spans, local container skip, and tape reserve, each requiring scalar reference and Lock 1/14 discipline (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:44-55`). P2-B defines the five-stage SIMD/ASM admission process: scalar oracle, target path, checkasm parity, same-wave consumer, and manifest/locks (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:22-32`). | pending / grammar-neutral | V1 should encode scalar/checkasm/same-wave consumer as mandatory, not optional quality bars. |
| parse-that Layer-1 vocabulary is incomplete. | P2-E says `parse-that-regex` exposes JSON-shaped string/number helpers but lacks grammar-neutral byte-set skip, local structural dispatch, bounded literal spans, UTF-8 run validation, digit-run accumulation, and escaped segment streaming (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:10-25`). | pending / grammar-neutral | This is a research input for S-P3; no implementation claim yet. |

## Proved / Disproved / Pending Digest

### JSON-Empirical Findings

| id | status | lesson V1 must reflect | citations |
|---|---|---|---|
| J-1 | proved | JSON has a validated 51-row guard baseline across parse_only, direct_to_struct, and real_typed_struct. | `skinny/RESULTS.md:139-141`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:8-21` |
| J-2 | proved | W11W parse_only memchr trusted-string split is the accepted JSON parse_only close route. | `skinny/REDRESS.md:6254-6284`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:8-16` |
| J-3 | proved | Direct and typed JSON admits are strict product/typed product rows, not digest-plane rows. | `skinny/RESULTS.md:147-149`; `skinny/RESULTS.md:101-102` |
| J-4 | pending | W11L/W11N/W11O closed-enum/FNV products are bench-only and require quarantine/comparator hardening. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60-65`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:47` |
| J-5 | pending | Current P1 PMU exposes direct/typed c/B misses on `mesh` and `unicode_escapes`; S-P1 does not reverse admits. | `restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:70-73`; `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:37-40` |
| J-6 | disproved as candidate source | Harness hashes, checksums, generated wrappers, and sidecar symbolization drift are not parser primitive evidence. | `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31-35`; `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:57-58` |
| J-7 | disproved as reopen route | Rejected JSON W10/W11 parse_only/product routes remain pre-blocked without fresh material differential. | `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:35-38`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:61-72` |

### Grammar-Neutral Findings

| id | status | lesson V1 must reflect | citations |
|---|---|---|---|
| G-1 | proved | Structural projection and tape are one substrate; `ValueRef` is a cursor into the tape. | `skinny/REDRESS.md:126-132`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:12-15` |
| G-2 | proved | Native Apple M5 Max/aarch64 anchors this tranche; x86/AVX-512 is diagnostic only. | `restart/skinny/tranches/sk-v15/HANDOFF.md:16-18`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:14-15` |
| G-3 | disproved | Current Lock 14/16 gate coverage is not clean; scan roots and report/checkasm coverage hide known gaps. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45-47`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:26-31` |
| G-4 | disproved | Pattern H is not collapsed: 67 files remain and 0/67 root runtime files carry line-1 generated provenance. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:37-43`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:23-35` |
| G-5 | disproved | Decision Engine is not load-bearing; e-graph/CSP/lowerers are scaffolded or non-driving. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:49-55`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md:10-17` |
| G-6 | disproved | Codegen neutrality is not achieved; grammar-family modes, static CSS rosters, JSON templates, and grammar/backend bindings remain. | `restart/skinny/tranches/sk-v15/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:68-74`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:37-39` |
| G-7 | disproved | Retained structural indexes, streaming cursors, class columns, and second substrates remain blocked. | `restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:36-37`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50-58` |
| G-8 | pending | SIMD/ASM candidates require scalar oracle, strict checkasm, same-wave consumer, and Lock 16 manifest. | `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:22-32`; `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:34-41` |
| G-9 | pending | parse-that needs grammar-neutral Layer-1 vocabulary before S-P3 can shortlist parser primitives. | `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:10-25`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:223-237` |

### CSS Audit-Demoted Findings

| id | status | lesson V1 must reflect | citations |
|---|---|---|---|
| C-1 | disproved | CSS L4 24-row admit is one broadcast aggregate, not 24 independent feature measurements. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-27`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A1-measurement-integrity.md:11-13` |
| C-2 | disproved | CSS runtime generation is relocated hand-written `CSS_GENERATED_RS`, not grammar-derived parser emission. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:31-33`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:24-26` |
| C-3 | disproved | CSS comparison is workload-mismatched: Track 1 summary/fact stream is not lightningcss CSSOM, and cssparser is faster in the same row. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:45` |
| C-4 | disproved | CSS equality oracle is marker/status checking, not equivalent CSS value equality. | `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:46`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:74-78` |
| C-5 | spec_claims_unimplemented | CSS typed value/document/view/visitor API is missing and blocks CSS >SOTA admission. | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56-58`; `restart/skinny/tranches/sk-v15/SYNTHESIS.md:40-42` |
| C-6 | pending | CSS parser retirement is coupled to typed CSS rebuild proof; deletion/retirement cannot outrun provider proof. | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:91-96`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:64-69` |
| C-7 | pre-blocked | REDRESS-183, REDRESS-184, REDRESS-209, REDRESS-210, REDRESS-211, REDRESS-212, and REDRESS-213 are the SK-V14 wave-graph-cycle precedent: a delete or retirement wave cannot close unless the rebuild provider for that artefact has already landed or lands in the same wave. | `skinny/REDRESS.md:5090-5118`; `skinny/REDRESS.md:5171-5293`; `restart/skinny/tranches/sk-v15/SYNTHESIS.md:102-106`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:64-69` |

## Divergences Catalogued

| divergence | count bucket | loc_delta_estimate | risk | evidence | V1 impact |
|---|---:|---:|---|---|---|
| Proved/currently implemented skinny lessons that V1 may cite with scope qualifiers: JSON 51-row guard, W11W parse_only, strict product rows, hot-leaf attribution discipline, same tape substrate, aarch64 admission pin, root candidate process discipline. | 7 implemented | 0-120 doc LOC | medium if overgeneralized | `skinny/RESULTS.md:139-149`; `skinny/REDRESS.md:6254-6284`; `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31-35`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:12-15`; `restart/skinny/tranches/sk-v15/HANDOFF.md:16-18`; `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:22-32` | Scope these as JSON-empirical or process/substrate, not proof of CSS/generalized codegen. |
| Disproved/unimplemented claims: CSS broadcast, CSS generated string, CSS comparator/equality mismatch, CSS Value API absence, Lock 14/16 gate holes, Pattern H ownership gap, Decision Engine scaffold, codegen grammar-family leaks, retained sidecars, harness/hash primitive misuse. | 10 unimplemented | 1,500-8,000 implementation LOC plus gate work | high | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-58`; `restart/skinny/tranches/sk-v15/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:16-24`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50-58`; `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31-35` | V1 must not describe SK-V15 as at generalisation inflection until these close. |
| Implementation/evidence exceeds old totality schema: CSS anti-broadcast telemetry fields, new CH3/CH5/CH7 addenda, and P1/P2 normalized evidence surfaces. | 3 impl_exceeds_spec | 200-600 doc/gate LOC | medium | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98-127`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-A-results-extraction.md:48-62`; `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31-35` | V1 schema should absorb anti-broadcast fields, gate-exclusion reporting, and normalized hot-leaf evidence. |
| Unknown/pending close surfaces: FNV quarantine, JSON c/B misses, CSS typed API/re-timing, Lock 14/16 gate restoration, Pattern H round-trip, Decision Engine emission diffs, grammar-neutral parse-that vocabulary, primitive same-wave consumers. | 8 unknown | 400-3,000 implementation LOC per receiver | high | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:70-80`; `restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:70-73`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:27-29`; `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:24-32` | These are S-P3 / T-P3 routing inputs, not close evidence. |

## V2 Hardening Fold

| fold | disposition |
|---|---|
| CH2-FOLD-004 | 1B pass-layer leaks `P1-1B-D9` and `P1-1B-D10` are now carried as grammar-neutral Lock 14 failures: recognizer mining and materialization role mining are JSON-shaped generic pass logic, not JSON-only empirical lessons. |
| CH3-V1-005 | Added `C-7` wave-graph-cycle pre-block naming REDRESS-183/184/209..213 and binding `NEW-CH3-V5-01` dependency-table logic. |
| CH4-V1-002 | Added LOC and risk fields to the divergence table. |
| CH5-V1-05 | Broadcast CSS admits are treated as admission-plane dishonesty unless rows are explicitly aggregate or carry distinct `measurement_row_id` / `broadcast_group_id`. |
| CH5-V1-04 | CSS source-sidecar comparator evidence is comparator-only and cannot be used as runtime substrate or CSS Value API proof. |

## Gaps / Missing Primitives

| gap | status | evidence | verify_action |
|---|---|---|---|
| CSS typed value/document/view/visitor surface. | UNKNOWN / required | CSS Value API is a close gate (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:40-42`), and A4 shows current CSS parse/full-parse outputs are strings/counters (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:25`). | Build REBUILD-WAVE-E; verify a CSS row reports typed value/CSSOM-equivalent output and value equality against cssparser before any CSS SOTA admit. |
| Anti-broadcast RESULTS/gate telemetry. | UNKNOWN / required | SK-V15 adds `measurement_row_id`, `measurement_origin`, `value_plane`, comparator workload, generator source, scan scope, exclusion report, and `broadcast_group_id` (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:119-127`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-A-results-extraction.md:48-62`). | Gate must reject one-to-N measurement stamps unless explicitly aggregate; verify CSS 24 rows collapse or carry 24 distinct measurement ids. |
| Lock 14 / Lock 16 full-surface gate restoration. | UNKNOWN / required | A3 says generic scan roots omit leak-bearing codegen roots and Lock 16 lacks manifest/strict command enforcement (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:26-31`). | PRUNE-WAVE-B should scan all generic roots, print exclusions as findings, require strict checkasm text, and produce a source-present primitive manifest. |
| Pattern H generated ownership and round-trip proof. | UNKNOWN / required | Root runtime count remains 67 with zero generated headers and no non-writing root runtime round-trip gate (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:17-18`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:47`). | PRUNE-WAVE-D must preserve count 67, add line-1 provenance, and provide delete+regen or check proof from one grammar-neutral generator. |
| Decision Engine runtime-relevant emission. | UNKNOWN / required | A5 requires rewrite count >=1, CSP that can alter/reject selection, no grammar-named facts, and five real lowerers (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md:44-50`). | REBUILD-WAVE-F must produce a generated runtime diff when selected BackendShape changes; verify `rg` finds no JSON/CSS decision facts. |
| Grammar-neutral parse-that Layer-1 vocabulary. | UNKNOWN / research input | P2-E identifies missing `skip_byte_set_run`, `classify_local_block_64`, `bounded_plain_literal_span`, `validate_utf8_run`, `digit_run_span_accumulate`, and `escaped_literal_segments` (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:27-55`, `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:57-221`). | S-P3 should only shortlist rows with scalar oracle, non-JSON witness or scoped claim, REDRESS pre-block review, and same-wave consumer. |
| SIMD/ASM primitive manifest and same-wave consumer. | UNKNOWN / required for SIMD admits | P2-B defines scalar oracle, target path, checkasm, same-wave consumer, and manifest gates (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:22-32`). | For each source-present primitive, close as `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`; no orphan kernel may close. |
| FNV closed-enum production guard. | UNKNOWN / required | SK-V15 close requires W11L/W11N/W11O FNV products to remain bench-only and comparator hardening to catch sidecar coupling (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:48`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-E-candidate-shortlist.md:20`). | REBUILD-WAVE-G should assert no FNV-keyed arbiter in production runtime and add adversarial strict-product fixtures. |

## Open Questions

| question | status | verify_action |
|---|---|---|
| Does totality V1 already distinguish JSON-empirical proof from grammar-neutral proof? | UNKNOWN | Check `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, and `restart/locks/LOCKS.md` for language equivalent to T-P1 CH2's required separation (`restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`) and SK-V15's JSON-guard/CSS-demoted split (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:55-68`). |
| Does totality V1 mark CSS L4 as audit-demoted rather than admitted? | UNKNOWN | Verify no V1 surface cites `skinny/RESULTS.md:112-135` CSS rows as independent admits without the PASS-IMPL demotion evidence (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-31`). |
| Does V1 require anti-broadcast telemetry in RESULTS/gate schema? | UNKNOWN | Verify schema includes `measurement_row_id`, `measurement_origin`, `value_plane`, `gate_exclusion_report`, and `broadcast_group_id` or equivalent fields (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:119-127`). |
| Does V1 require CSS typed Value API before CSS SOTA claims? | UNKNOWN | Confirm V1 blocks `CssFullParseSummary`/fact-stream output as admission evidence until typed CSS value/document/view output exists (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:40-42`; `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56-58`). |
| Does V1 encode Lock 14 / Lock 16 gates that scan exclusions as findings? | UNKNOWN | Compare lock wording and gate specs against SK-V15 `NEW-CH7-V5-03` and PRUNE-WAVE-B (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:107-110`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:100-110`). |
| Does V1 forbid retained structural sidecars while permitting same-call masks consumed into the tape/event loop? | UNKNOWN | Verify Lock 1/V1 substrate text matches P2-D's allowed candidates and explicit non-candidate class (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:34-42`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50-58`). |
| Does V1 require scalar oracle + strict checkasm + same-wave consumer for SIMD/ASM admits? | UNKNOWN | Verify Lock 16 or totality research gates encode the five-stage P2-B process (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:22-41`). |
| Does V1 route Decision Engine as scaffold until runtime diffs prove shape selection drives emission? | UNKNOWN | Verify totality codegen claims do not credit zero-rewrite egraph/CSP/lowerer stubs as load-bearing; require A5 acceptance tests (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md:44-52`). |
