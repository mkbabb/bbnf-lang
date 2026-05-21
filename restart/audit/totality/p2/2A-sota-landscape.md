---
agent: 2A
pass: T-P2-research
cycle: V3
generated_at: 2026-05-21T04:38:47-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 15
counted_source_ids: [T2A-SRC-V2-FOLD, T2A-SRC-SIMDJSON-PAPER, T2A-SRC-SIMDJSON-SRC, T2A-SRC-SONIC, T2A-SRC-YYJSON, T2A-SRC-ASMJSON-README, T2A-SRC-ASMJSON-SAXDOM, T2A-SRC-ASMJSON-CONFORMANCE, T2A-SRC-FFMPEG-CHECKASM, T2A-SRC-DAV1D-CHECKASM, T2A-SRC-REDRESS, T2A-SRC-RESULTS, T2A-SRC-T-P1-1A, T2A-SRC-T-P1-1E, T2A-SRC-S-P1-LEDGER]
techniques_grounded: 10
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: [CH1, CH2, CH3, CH4, CH5, CH6]
  first_cycle_additions: [T2A-SOTA-001, T2A-SOTA-002, T2A-SOTA-003, T2A-SOTA-004, T2A-SOTA-005, T2A-SOTA-006, T2A-SOTA-007, T2A-SOTA-008]
  v2_shared_contract: restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
  v3_shared_contract: restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
  v2_fold_additions: [T2A-V2-FOLD-001, T2A-V2-FOLD-002, T2A-V2-FOLD-003, T2A-V2-FOLD-004]
  v3_fold_additions: [T2A-V3-COUNTED-SOURCE-IDS]
locks_amendment_candidates: 5
---

## Executive Summary

SOTA JSON parsing does not defend a blanket "retain every SIMD structural
index" thesis. simdjson proves the two-stage structural-index architecture and
On-Demand proves forward-only, use-specific parsing, but bbnf has already
measured two faithful retained-union attempts as regressions on the M5 Max
(`skinny/REDRESS.md:2795-2940`). sonic-rs is the binding JSON comparator because
the skinny matrix has same-run strict sonic rows, while simdjson, yyjson, and
asmjson remain architecture pressure until same-plane sidecars are wired
(`skinny/RESULTS.md:3`, `skinny/RESULTS.md:145-149`). yyjson grounds the scalar
discipline lesson: high ILP, branch prediction, and compact layouts can beat
poorly integrated SIMD. asmjson grounds a useful 64-byte classifier/SAX-vs-DOM
shape, but its AVX-512-only assembly and permissive conformance note make it
non-admissive for SK-V13. FFmpeg/dav1d checkasm grounds the process, not pixel
kernels: scalar reference, randomized differential harness, CPU-feature gating,
callee-saved-register safety, and benchmark reporting before production wiring.
For totality, the defended rule is narrower: every primitive must be
grammar-neutral data/policy, scalar-referenced, checkasm/parity tested, and
consumed by a row-moving same-wave parser or sink.

V2 folds CH1-CH6 through the shared mechanical contract in
`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`. Source citations below
ground candidates only; they are not admissions. Moving upstream source trees
are pinned by the addendum's provenance register, external performance claims
are architecture pressure unless the same corpus, platform, strictness, and
output plane are named, and every row reopen inherits the REDRESS-119 matrix
plus the REDRESS 121-127 taxonomy. A SOTA route reaches T-P3 only after the
addendum's ledger state advances from `source_backed` through scalar,
checkasm/parity, micro-proof, production consumer, and measured row admission
or architectural-block evidence.

## V2 Fold Authority

| CH lens | 2A fold |
|---|---|
| CH1 correctness / provenance | Upstream JSON/checkasm source claims are tied to the V2 provenance register rather than moving `main` / `master` authority. Benchmark and speed claims without strict corpus/platform context are downgraded to architecture pressure. |
| CH2 generality / Lock 14 | SOTA techniques transfer only through generated grammar facts or caller-supplied policy. One CSS declaration-values row is non-JSON evidence, not fleet-wide grammar closure; fleet-wide claims require the addendum's CSS plus Sheets/BBNF-self transfer contract. |
| CH3 REDRESS | REDRESS-119 is reopened only row-by-row under the addendum matrix. Union, PMULL, CSSC, and SIMD labels are not material differentials unless the consumer-level checklist distinguishes them from REDRESS 88/89/96/97/98. |
| CH4 cost | 2A classifies SOTA sources as candidate evidence only. The per-technique admission ledger, state machine, LOC/risk, rollback, and abrogate gates live in the addendum and must be instantiated by owner dossiers or S-P3. |
| CH5 Lock 1 | Structural masks remain `local_temp_only` unless consumed into `existing_tape`, `direct_sink`, or `admitted_fact_output`. 2A's transient-mask thesis now uses the addendum's `substrate_target`, `retention_lifetime`, and `policy_owner` vocabulary. |
| CH6 anti-paper-close | "Grounded" never means "validated design." Citation density, parity-only proofs, and microbench-only wins remain non-admitting until strict equality and row movement are gate-consumed. |

## Technique Grounding Table

| spec claim or T-P1 divergence id | source authority | V2 state | bbnf-specific note |
|---|---|---|---|
| `SOTA-simdjson-stage1-stage2` | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 28(6), 2019 / arXiv:1902.08318; simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765` per V2 addendum. | source_backed / architecture_pressure | Stage 1 structural indexes + UTF-8 validation and stage 2 tape construction are real SOTA architecture. bbnf may transfer transient mask production only if consumed into the existing tape/sink in the same loop; retained class-column replay is refuted by REDRESS 96/97/98 and fenced by the V2 Lock 1 substrate-kind contract. |
| `SOTA-simdjson-On-Demand` | simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765`, `doc/basics.md` and `doc/ondemand_design.md`, per V2 addendum. | source_backed / architecture_pressure | On-Demand supports forward-only, use-specific parsing and skipping unused values. This defends product/direct sinks and generated shape selection more than a universal DOM/tape materialization route, but fleet-wide totality requires CSS plus Sheets/BBNF-self transfer proof before this becomes grammar-neutral closure. |
| `SOTA-simdjson-runtime-dispatch` | simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765`, `include/simdjson/implementation.h`, per V2 addendum. | process_grounded | Runtime CPU feature selection is admissible as a process pattern. For bbnf it must stay behind Lock 16 allowlist + scalar fallback, never select unsupported x86 paths on Apple Silicon, and advance through the addendum primitive ledger before admission. |
| `SOTA-sonic-targeted-SIMD` | sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5`, `README.md`, per V2 addendum. | source_backed / row_target_pressure | sonic-rs explicitly rejects simdjson-style two-stage copying as its central tactic and uses SIMD for long strings, float fractions, field lookup, and whitespace. bbnf should target these row leaves only with row-local material differentials; source prestige alone cannot reopen REDRESS-119 rows. |
| `SOTA-sonic-direct-typed-anchor` | sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5`; local `skinny/RESULTS.md` same-run strict sonic rows. | comparator_grounded_for_local_rows | The M1 Pro twitter/citm struct rows are published anchors, but bbnf admission uses the same-host strict sonic rows in `RESULTS.md`. External sonic benchmarks remain context, not the numeric gate, unless corpus/platform/output-plane parity is reproduced locally. |
| `SOTA-sonic-lazy-field-lookup` | sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5`, `docs/benchmark_aarch64.md`, per V2 addendum. | architecture_pressure | Sonic's unchecked field lookup uses SIMD to skip unnecessary fields. bbnf's legal transfer is generated FIRST/follow probe or same-loop string/field skip with strict comparator parity; unchecked/lossy modes are not admission anchors. |
| `SOTA-yyjson-scalar-fast` | yyjson HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97`, `README.md`, per V2 addendum. | architecture_pressure | yyjson's primary lesson is scalar: ANSI C, no explicit SIMD, strict default, high ILP and branch predictor preference. This supports bbnf's measured finding that wide-core scalar envelopes can beat poorly integrated retained SIMD, but it admits no row without bbnf-local strict comparator evidence. |
| `SOTA-yyjson-strict-default-plus-flags` | yyjson HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97`, `src/yyjson.h`, per V2 addendum. | process_grounded | yyjson strict default plus explicit non-standard flags reinforces bbnf's strict-vs-strict gate. Any permissive yyjson/sonic/asmjson number is a flaw probe, not admission. |
| `SOTA-asmjson-64-byte-classifier` | asmjson crate `0.2.5` README on docs.rs. | architecture_pressure / not_admitting | The 64-byte classifier and SAX sink are useful shape evidence, but AVX-512BW is x86-only and SK-V13 is Apple Silicon/aarch64. Treat as architecture pressure, not a close route. |
| `SOTA-asmjson-strictness` | asmjson crate `README.md:209-222`, https://docs.rs/crate/asmjson/0.2.5/source/README.md#209 | refuted | asmjson is explicitly permissive on control characters and does not scan string contents for unescaped controls. It cannot be a strict SK-V13 JSON admission comparator. |
| `PROCESS-ffmpeg-checkasm` | FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929`, `tests/checkasm`, per V2 addendum. | process_grounded | FFmpeg grounds reference-vs-new function selection, checked calls, timing loops, overhead correction, and benchmark printing. bbnf Lock 16 should require the same shape before primitive admission. |
| `PROCESS-dav1d-checkasm` | dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33`, `tests/checkasm` and `src/arm/cpu.c`, per V2 addendum. | process_grounded | dav1d grounds CPU-flag matrices, call-ref/call-new differential checks, and benchmark-after-equality. Pixel kernels do not transfer; the process does. |
| `1A-SUB-014 scanner/sidecar plane fence` | T-P1 1A states JSON `StructuralIndex` is transient and CSS sidecar is comparator evidence (`restart/audit/totality/p1/1A-substrate-evidence.md:43-46`, `restart/audit/totality/p1/1A-substrate-evidence.md:58`). | grounded | SOTA structural indexes are not a license for a retained sidecar. Totality should amend Lock 1 to name transient scanner and comparator-sidecar fences. |
| `1E-L16 traceability UNKNOWN` | T-P1 1E marks Lock 16 partial until every intrinsic/`asm!` maps to allowlist + parity + consumer (`restart/audit/totality/p1/1E-locks-evidence.md:87`, `restart/audit/totality/p1/1E-locks-evidence.md:112-119`). | grounded | Checkasm process sources above defend this amendment. |
| `SK-V13 P2-A comparator sidecars` | SK-V13 P2-A says only sonic-rs strict is a JSON admission comparator today and C++/DOM sidecars are pressure unless same-plane sidecars are produced (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42-52`, `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:140-143`). | grounded | Totality must not claim simdjson/yyjson/asmjson rows are current bbnf SOTA gates unless new sidecar infrastructure lands. |

## Architectural Assertions Defended

| assertion | defense | bbnf consequence |
|---|---|---|
| Structural SIMD is useful as a transient producer. | simdjson stage 1 extracts structural indexes and validates UTF-8 before stage 2 consumes them into tape (`doc/parse_many.md:54-57`). SK-V13 S-P1 also measured structural scanner micro-results but kept them non-admitting (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:78-92`). | Totality should keep a transient-mask primitive family, but require same-loop consumption into the one existing substrate or row sink. |
| Product/direct planes are legitimate SOTA surfaces. | simdjson On-Demand and sonic-rs direct struct/lazy surfaces both show SOTA parsers win by parsing only used fields or direct product targets. sonic-rs README says direct struct avoids a temporary tape for struct deserialization (`README.md:88-90`). | bbnf's typed and direct planes are not second-class if strict equality and same-plane comparators hold. The gate must encode output plane, not a single parse-only scoreboard. |
| Scalar envelopes are not fallback noise. | yyjson is explicitly "no explicit SIMD" and prefers high ILP/branch prediction (`README.md:14`, `README.md:73-78`); REDRESS 98 found scalar delimiter rediscovery cheaper than retained structural cursor on the M5 Max (`skinny/REDRESS.md:2928-2933`). | T-P3 should add a scalar-first amendment: a SIMD route must beat an isolated scalar reference and the integrated row, not just win a microbench. |
| Checkasm is process discipline, not video-domain cargo cult. | FFmpeg and dav1d both structure tests around registered CPU implementations, reference/new calls, randomized inputs, equality checks, and bench reporting. dav1d loopfilter calls `call_ref`, `call_new`, checks pixels, then `bench_new` (`tests/checkasm/loopfilter.c:177-188`). | Lock 16 should mandate scalar oracle + checkasm/parity + corpus equality + same-wave consumer for every primitive; no orphan support-only primitives at close. |
| Runtime CPU dispatch can be clean if feature-gated. | simdjson's implementation layer reports required instruction sets and runtime support (`implementation.h:68-75`); dav1d's Apple aarch64 path gates DotProd/I8MM via `sysctlbyname` (`src/arm/cpu.c:87-95`). | bbnf can select aarch64 kernels at runtime, but the manifest must name hardware gate, scalar fallback, checkasm cell, and row consumer. |

## Architectural Assertions Refuted

| refuted assumption | source evidence | bbnf consequence |
|---|---|---|
| "simdjson stage 1 implies bbnf should retain a union structural class column." | REDRESS 96 and 97 both landed correctness-green union variants and missed every W3 target and W10b guard; REDRESS 98 attributes the loss to parse-loop memory traffic/cursor indirection (`skinny/REDRESS.md:2823-2848`, `skinny/REDRESS.md:2881-2906`, `skinny/REDRESS.md:2910-2940`). | A fresh union attempt must name a material differential. SOTA citation alone cannot reopen the old class-column or streaming-cursor route. |
| "asmjson can be a strict comparator or portable close route." | asmjson declares its AVX-512BW assembly x86-only and unsafe for unsupported CPUs (`README.md:100-103`, `README.md:206-207`) and states permissive control-character behavior (`README.md:211-222`). | Use asmjson as shape pressure for classifier/sink design only. It cannot close SK-V13 strict JSON rows or Apple Silicon ASM obligations. |
| "C++ DOM sidecars in RESULTS are current SOTA gates." | `skinny/RESULTS.md` marks many simdjson/yyjson/asmjson columns historical, absent, or `n/a`, and warns native Rust comparators are same-run while C++ sidecars are historical/absent (`skinny/RESULTS.md:3`, `skinny/RESULTS.md:145-149`). | Totality must require same-run same-plane sidecars before citing non-sonic competitors as admission comparators. |
| "SIMD parity or microbench alone admits a primitive." | SK-V12 W2 escape mask added checkasm/corpus parity but no production scanner or row movement (`skinny/REDRESS.md:3603-3632`); W4 delimiter find had 4.718x microbench and explicitly halted before production wiring (`skinny/REDRESS.md:3766-3820`). | Lock 16 must say parity is prerequisite, not admission. Admission requires row-moving consumer or measured deletion/rejection. |
| "Direct residual REDRESS-119 is permanent architecture block." | REDRESS-119 closed SK-V11 as measured fixpoint, but SK-V13 lifted it as history-only and S-P2 V4 confirms full-SOTA addendum reopens every row with fresh material differentials (`skinny/REDRESS.md:3495-3527`; `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:21-26`, `:56-58`). | Direct rows remain eligible only through the addendum's row-specific REDRESS-119 reopen matrix. "Decision engine", "union", or "ASM" is not itself a material differential until the row-local consumer and changed dataflow are named. |
| "CSS declaration-values proves full grammar generality." | CSS L4 declaration-values admitted one strict fact-stream row, but T-P1 1A calls it a substrate/telemetry category gap and SK-V13 P2-F keeps CSS rows conditional row-production scopes (`skinny/REDRESS.md:3824-3840`; `restart/audit/totality/p1/1A-substrate-evidence.md:45-46`; `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:46-51`). | Lock 14 can cite this as non-JSON evidence, not universal CSS/SHEETS/BBNF-self proof. |

## V2 REDRESS And Admission Carry-Forward

| evidence | V2 SOTA-landscape meaning |
|---|---|
| REDRESS-119 | History-only direct-row fixpoint. Each JSON direct reopen must cite the addendum's row-specific differential, consumer, strict comparator/oracle, and old-route block condition. |
| REDRESS 121 | GrammarConfig legality prerequisite. It repairs part of Lock 14 but admits no CSS/SOTA row and no SIMD primitive. |
| REDRESS 122 | `escape_mask_64` correctness prerequisite. It supplies checkasm/parity evidence, not a production scanner or row admission. |
| REDRESS 123 | CSS L4 generated scaffold. It creates the non-JSON baseline/oracle surface but is not lightningcss SOTA admission. |
| REDRESS 124 | lightningcss comparator infrastructure. It proves strict same-plane equality machinery, not a SOTA row by itself. |
| REDRESS 125 | measured CSS SOTA candidate. It becomes campaign admission only after REDRESS 127 close reconciliation. |
| REDRESS 126 | ASCII run-skip microbench route-production split. The 4.718x result is micro-proven architecture pressure until production CSS wiring and row movement land. |
| REDRESS 127 | SK-V12 CSS declaration-values close. It admits one CSS row, leaves full CSS parity open, leaves union category unattempted in SK-V12, and treats W4 ASM-gen as routed production split rather than production admission. |

## V2 SOTA Candidate Admission State

| candidate family | V2 state | required next evidence |
|---|---|---|
| simdjson-style structural masks | source_backed / architecture_pressure | `substrate_target=local_temp_only` or same-loop `existing_tape` / `direct_sink` / `admitted_fact_output`, scalar reference, row consumer, and material differential from REDRESS 96/97/98. |
| simdjson On-Demand shape selection | source_backed / architecture_pressure | generated grammar facts, Lock 14 transfer proof over CSS plus Sheets/BBNF-self, and strict row movement without parser-owned cursor sidecars. |
| sonic-rs targeted SIMD leaves | row_target_pressure | bbnf-local strict sonic comparator, row-specific material differential from REDRESS-119 where reopening direct rows, and no unchecked/lossy mode. |
| yyjson scalar envelope | source_backed / scalar_pressure | isolated scalar baseline plus integrated-row proof; used to reject SIMD/substrate candidates that lose against wide-core scalar cost. |
| asmjson classifier/SAX shape | architecture_pressure / not_admitting | aarch64-native primitive and strict equality proof; AVX-512 and permissive conformance cannot close SK-V13. |
| FFmpeg/dav1d checkasm process | process_grounded | bbnf scalar reference, checkasm/parity cell, CPU-feature gate, same-wave consumer, and corpus equality before production admission. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Can simdjson On-Demand's forward-only use-specific parsing be represented as a generated bbnf direct/typed shape without creating parser-owned cursor sidecars? | T-P3 should map On-Demand principles to existing `BackendShape::SinkOnly` / typed product rows and require CSS plus Sheets/BBNF-self transfer proof before fleet-wide closure; reject any public substrate API expansion. |
| Are non-sonic JSON sidecars worth wiring under SK-V13? | If S-P3 wants simdjson/yyjson/asmjson as real gates, add a wave that builds same-run, same-plane, strict sidecars and rejects historical/DOM-plane comparisons. |
| Which bbnf SIMD use-sites have complete Lock 16 lineage? | Generate a manifest over `core::arch`/`asm!` use-sites mapping source path, scalar reference, checkasm test, corpus parity, hardware gate, and production consumer. |
| Can a fresh union variant be a codegen-time shape selection rather than a retained parser sidecar? | Require a micro-proof plus row-moving consumer and satisfy the addendum's PMULL/CSSC/union material-differential checklist before any implementation wave. |
| Does yyjson-style scalar layout explain any JSON parse_only rows better than SIMD masks? | Benchmark scalar envelope reductions on `dispatch_value` and direct envelopes before adding new SIMD bodies; compare against S-P1 hot-leaf rows (`evidence-ledger-v3.md:32-62`). |
| Can CSS parity rows expose parser hot leaves rather than fact-sink/timer overhead? | Before scoping CSS scanner ASM, collect narrow CSS parser profiles for each selected feature row; current declaration-values profile is nonparser overhead (`evidence-ledger-v3.md:100-104`). |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | lock(s) | proposed amendment candidate | supporting evidence | disposition for T-P3 |
|---|---|---|---|---|
| T2A-LAC-01 | Lock 1 | Add a transient-mask clause: SIMD structural/class masks may exist only as ephemeral producers consumed into the single tape/direct/fact sink in the same loop; retained sidecars/class columns require fresh material-differential proof and the addendum's `substrate_target` / `retention_lifetime` / `policy_owner` fields. | simdjson stage1/tape docs; REDRESS 96/97/98 measured union failures; T-P1 1A scanner/sidecar fence; V2 fold addendum Lock 1 contract. | Add to locks diff; mark old union routes historical unless materially differentiated. |
| T2A-LAC-02 | Lock 8 / BENCH | Add comparator-plane provenance: same-run Rust sonic strict is current JSON gate; simdjson/yyjson/asmjson cannot be gate anchors while historical, absent, permissive, x86-only, or different-plane. | `skinny/RESULTS.md:3`, `skinny/RESULTS.md:145-149`; asmjson conformance note; SK-V13 P2-A comparator strictness fold. | Add BENCH.md row-plane matrix requirement and "architecture pressure" status for unwired competitors. |
| T2A-LAC-03 | Lock 16 | Require a primitive manifest for every SIMD/ASM intrinsic: abstract primitive name, primary source, hardware gate, scalar reference, checkasm/parity test, corpus parity, same-wave production consumer, row movement or measured rejection, substrate target, retention lifetime, policy owner, rollback path, and abrogate threshold. | FFmpeg/dav1d checkasm sources; T-P1 1E Lock 16 partial/UNKNOWN; SK-V12 W2/W4 prerequisite-only outcomes; V2 fold addendum per-technique ledger. | Add to Lock 16 and make manifest gate-consumed by skinny waves. |
| T2A-LAC-04 | Lock 14 | Competitor techniques transfer only through generated grammar data or policy traits, not grammar-name branches in generic crates; CSS fact streams are admitted evidence but not generic substrate closure. | sonic/yyjson/simdjson techniques are grammar-agnostic only at primitive level; T-P1 1A CSS category gap; S-P2 V4 CSS row-scope confirmation; V2 fold addendum transfer contract. | Add grammar-neutral transfer rule with CSS plus Sheets/BBNF-self proof consumer. |
| T2A-LAC-05 | Lock 10 / decision engine | Add scalar-first / micro-prove-first cost precondition: no SIMD/substrate shape reaches S-P3 wave scope unless isolated scalar reference and integrated row cost both predict a win, with fail-closed abrogate gates for e-graph saturation, CSP timeout, stale cost, generated LOC growth, row regression, and parity/checkasm failure. | yyjson scalar high-ILP evidence; REDRESS 98 M5 Max scalar delimiter finding; SK-V12 W4 microbench-only split; V2 fold addendum abrogate gates. | Route to decision-engine fold so cost resolver can reject citation-only SIMD. |

## Source Register

- T-P2 V2 shared mechanical contract:
  `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- Langdale, Geoff and Daniel Lemire. "Parsing Gigabytes of JSON per Second." VLDB Journal 28(6), 2019. https://arxiv.org/abs/1902.08318
- simdjson verified upstream HEAD `168ef580757d75270475b379e83c2b39787a6765` on 2026-05-21 per V2 addendum:
  `doc/parse_many.md:54-57` stage 1 / stage 2:
  https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/doc/parse_many.md#L54-L57;
  `doc/basics.md:343-350` On-Demand iterator:
  https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/doc/basics.md#L343-L350;
  `doc/ondemand_design.md:71-89` skip/use-specific parsing:
  https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/doc/ondemand_design.md#L71-L89;
  `include/simdjson/implementation.h:40-75` runtime implementation:
  https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/include/simdjson/implementation.h#L40-L75
- sonic-rs verified upstream HEAD `03545a9530346fe279b674dd496e037d94204bc5` on 2026-05-21 per V2 addendum:
  `README.md:60-66` targeted SIMD:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L60-L66;
  `README.md:78-90` direct struct:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L78-L90;
  `docs/benchmark_aarch64.md:1-15` M1 Pro anchors:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/docs/benchmark_aarch64.md#L1-L15;
  `docs/benchmark_aarch64.md:140-151` field lookup:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/docs/benchmark_aarch64.md#L140-L151
- yyjson verified upstream HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97` on 2026-05-21 per V2 addendum:
  `README.md:10-19` strict/no explicit SIMD:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/README.md#L10-L19;
  `README.md:73-78` ILP/branch predictor:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/README.md#L73-L78;
  `src/yyjson.h:736-744` strict default flags:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L736-L744;
  `src/yyjson.h:759-837` non-standard flags:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L759-L837
- asmjson crate README `README.md:7-12`, 64-byte AVX-512BW/SWAR classifier: https://docs.rs/crate/asmjson/0.2.5/source/README.md#7
- asmjson crate README `README.md:100-113`, x86-only assembly/SAX-vs-DOM notes: https://docs.rs/crate/asmjson/0.2.5/source/README.md#100
- asmjson crate README `README.md:209-222`, conformance caveats: https://docs.rs/crate/asmjson/0.2.5/source/README.md#209
- FFmpeg verified upstream HEAD `085714182302333dd83dcb9c36cf828dc4eba929` on 2026-05-21 per V2 addendum:
  `tests/checkasm/checkasm.h:214-240` reference/new call macros:
  https://github.com/FFmpeg/FFmpeg/blob/085714182302333dd83dcb9c36cf828dc4eba929/tests/checkasm/checkasm.h#L214-L240;
  `tests/checkasm/checkasm.h:396-430` benchmark macro:
  https://github.com/FFmpeg/FFmpeg/blob/085714182302333dd83dcb9c36cf828dc4eba929/tests/checkasm/checkasm.h#L396-L430;
  `tests/checkasm/checkasm.c:679-737` overhead/reporting:
  https://github.com/FFmpeg/FFmpeg/blob/085714182302333dd83dcb9c36cf828dc4eba929/tests/checkasm/checkasm.c#L679-L737
- dav1d verified upstream HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` on 2026-05-21 per V2 addendum:
  `tests/checkasm/checkasm.c:37-88` registry/CPU flags:
  https://github.com/videolan/dav1d/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/tests/checkasm/checkasm.c#L37-L88;
  `tests/checkasm/loopfilter.c:177-188` call-ref/call-new/check/bench:
  https://github.com/videolan/dav1d/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/tests/checkasm/loopfilter.c#L177-L188;
  `src/arm/cpu.c:87-95` Apple aarch64 feature detection:
  https://github.com/videolan/dav1d/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/src/arm/cpu.c#L87-L95
- `skinny/REDRESS.md` REDRESS 88/89, 96/97/98, 119, and 121-127.
- `skinny/RESULTS.md` current row-plane comparator provenance.
- `restart/audit/totality/p1/1A-substrate-evidence.md` scanner/sidecar fence.
- `restart/audit/totality/p1/1E-locks-evidence.md` Lock 16 traceability gap.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
  local S-P1 hot-leaf and microbench evidence.
- `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md`
  JSON comparator sidecar classification.
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
  full-SOTA addendum and CSS row-scope confirmation.
