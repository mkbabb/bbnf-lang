# SK-V15 S-P2 V2 CH6 - Anti-Paper-Close

Verdict: ACCEPT.

## Evidence

CH6 rejects self-reported research/design closure unless the claim has citable evidence, scalar/checkasm/source claims resolve, and candidate grounding is not deferred to a future phase (`restart/prompts/ORCHESTRATOR.md:74`-`88`; `restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`138`). S-P2 also requires every candidate primitive to carry shape, scalar-reference status, architecture/checkasm discipline, and P1 antecedent, and S-P3 may consume only candidates that survived S-P2 CHALLENGE (`restart/prompts/skinny/PASS-2-RESEARCH.md:77`-`85`, `:119`-`:124`, `:192`-`:195`). SK-V15 adds exclusion-report pressure for Lock 14 / Lock 16 gates and telemetry fields including `checkasm_or_parity_status` and `gate_exclusion_report` (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:98`-`127`).

Current S-P2 files reviewed: `p2a` through `p2f`, V1 CH6, and cited evidence/source surfaces needed to resolve CH6 claims. The local resolution checks found no `UNKNOWN`, `TBD`, `TODO`, or unresolved marker in the six P2 artifacts. Representative local source/evidence references resolve, including the P1 TSV/PMU evidence, `restart/locks/LOCKS.md`, `skinny/REDRESS.md`, bbnf-simd checkasm files, tape/runtime files, grammar witness files, and the local sonic-rs registry sources cited by P2-A.

## Findings

### ACCEPT - Comparator and process claims are source-backed, not self-reported

P2-A cites comparator source for every comparator family before extracting primitive shapes: asmjson README/source/assembly for AVX-512/SWAR, SAX/DOM callbacks, string/digit/tape behavior; sonic-rs docs/source for non-space bitmaps, string masks, container skip, allocation, and strict UTF-8 behavior; simdjson stage1/stage2/On Demand source for structural indexing/string/tape allocation; yyjson README/source/header for strict DOM shape, scalar skip/number/string paths, and max-memory calculation (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:16`-`42`, `:101`-`:120`). It also quarantines asmjson strictness, x86/AVX-512, numeric comparator rows, and harness-hash rows instead of treating them as SOTA closure (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:20`, `:78`-`:80`).

P2-B's upstream process claims are grounded in FFmpeg, dav1d, VLC, local bbnf-simd checkasm, and Lock 16 citations (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:12`-`18`, `:81`-`:111`). Its admission gate requires scalar oracle, arch dispatch, checkasm parity, same-wave consumer, and manifest/lock evidence, then states strict checkasm commands are necessary but insufficient without a measured consumer or explicit rejection (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:24`-`41`).

### ACCEPT - ISA and host claims resolve without turning feature presence into admission

P2-C cites the committed host probe for Apple M5 Max/aarch64 feature presence and lists Arm primary sources for CSSC/CTZ, PMULL, DotProd/UDOT, TBL/TBX, CMEQ, and shift families (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:14`, `:76`-`:82`; `restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt:1`-`23`). The candidate table rejects or blocks UDOT, CSSC, PMULL, and x86 routes when P1 antecedent or same-wave consumer evidence is absent, so instruction availability is inventory only (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`-`35`, `:53`-`:59`).

### ACCEPT - Primitive grounding is current, scalar/checkasm-aware, and not deferred

P2-D grounds tape/substrate claims in Lock 1, live `Tape`/`TapeBuilder`/JSON view code, materialisation rows, and REDRESS 96/97/98 before naming same-substrate candidates (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:12`-`32`). Its candidate table supplies scalar/equality references, checkasm/parity expectations, P1 antecedents, grammar-neutrality boundaries, and REDRESS verdicts for every tape candidate (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:34`-`41`). It explicitly rejects ratio-only and checkasm-bypass closure (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50`-`58`).

P2-E states the rows are candidate gaps only and S-P3 must drop any row missing same-wave consumer, scalar oracle, REDRESS pre-block, and strict checkasm/parity gate (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:27`-`29`). Each live gap includes a scalar sketch and checkasm expectation, while the numeric/digit row is rejected for this S-P2 cycle because current P1 evidence is diagnostic rather than a surviving BBNF-side numeric hot leaf (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:41`-`49`, `:72`-`:82`, `:107`-`:115`, `:138`-`:146`, `:169`-`:185`, `:207`-`:215`).

### ACCEPT - Deferred-looking language is guarded by rejection or admission preconditions

Future-facing phrases are not used as closure. `raw_number_span_classify`, A64 UDOT digit4, and `digit_run_span_accumulate` are rejected or diagnostic until fresh P1 evidence exists (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:52`, `:64`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:173`-`:185`). Optional SIMD support for escape/string/UTF-8 candidates is conditioned on scalar references, dedicated checkasm, same-wave consumers, and REDRESS avoidance, not accepted as done (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:52`-`55`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:209`-`:213`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71`-`:81`).

### ACCEPT - P2-F folds every candidate surface into explicit disposition

P2-F closes the CH6 orphan risk by assigning ACCEPT/REVISE/REJECT dispositions to P2-B/C/D/E candidates and explicitly folding P2-A aliases (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:24`-`58`, `:59`-`:69`). It separates accepted grammar-neutral abstractions from revised template/host-function surfaces and rejected REDRESS/platform/harness routes (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:83`-`:117`). The CH4 fold then preserves scalar references, parity gates, same-wave consumers, LOC/risk/wave constraints, and rollback/deletion pressure for non-REJECT survivors (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71`-`:81`).

## Orphan V1 Disposition Check

V1 CH6 accepted P2-A through P2-F and found no candidate closing on self-report alone (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH6.md:45`-`49`). V2 rechecked the same surfaces against the current files plus the SK-V15 addendum and found no orphan V1 CH6 disposition:

- P2-A comparator-context rows now have explicit P2-F alias dispositions (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59`-`:69`).
- P2-B/C scalar/checkasm/ISA claims remain attached to source, host, checkasm, REDRESS, or rejection evidence (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:45`-`:58`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:26`-`:35`).
- P2-D/E design candidates retain scalar/equality references and same-wave consumer constraints rather than paper-closing on prose (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:36`-`:41`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:227`-`:236`).

No unresolved `UNKNOWN`, unverified scalar/checkasm/source claim, or deferred candidate grounding remains for CH6 to revise.
