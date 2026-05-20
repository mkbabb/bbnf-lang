# SK-V11 S-P2 CH4 Cost
Pass: S-P2 CHALLENGE. Cycle: V3.
Date: 2026-05-20.
Scope: scalar-reference, parity/checkasm, micro-proof, same-wave consumer,
feature/fallback, output-plane, and reject-boundary review for the V3 S-P2
research cohort.
Output: this file.
Disposition: ACCEPT.
Accept rate contribution: 1.

## Findings

1. Accepted - The CH4 contract is unchanged and V3 preserves the V2 accepting
   tuple rather than reopening the candidate pool. PASS-2 makes scalar-reference
   status, checkasm/parity expectation, and same-wave-consumer notes mandatory
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`124`), while the
   orchestrator non-negotiables require scalar references, checkasm before
   wiring, same-wave consumers, and no contrivance
   (`restart/prompts/ORCHESTRATOR.md:197`-`212`). V2 consolidated hardening
   already accepted that retained candidates carry scalar reference, parity,
   micro-proof, consumer, fallback, output plane, and reject boundary
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:19`-`24`)
   and required V3 only to preserve those facts
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:41`-`56`).

2. Accepted - P2-B remains the primary admission tuple source. Its V3 fold note
   says the V2 candidate pool is unchanged and proof/oracle rows stay outside
   parser row-mover admission
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:9`-`12`).
   Its common process requires scalar oracle first, differential cell, AArch64
   feature gate, micro-prove-first, same-wave consumer, and proof-only demotion
   when no consumer exists
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:247`-`264`).
   The candidate table explicitly carries columns for scalar-ref status, strict
   parity/checkasm, micro-proof, same-wave consumer, feature/fallback, and reject
   boundary, covering `HEX_QUARTET_X4_PROOF`,
   `STRING_SPECIAL_BLOCK_CALLER_MICROPROOF`, `BYTE_CLASS_TBL_CLASSIFIER`,
   `DIGIT_SPAN_UDOT`, `WHITESPACE_BYTE_SET_SKIP`, `MOVEMASK_EXHAUSTIVE_GATE`,
   `CONTAINER_DISPATCH_CLASSIFIER`, and `OUTPUT_DIGEST_HASH_ORACLE`
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:266`-`275`).

3. Accepted - P2-A preserves the comparator-derived candidates with cost gates.
   Its V3 fold states the parser candidate pool is unchanged, x4/hex stays
   proof-only until scalar x4 oracle, strict parity, source delta, and same-wave
   consumer exist, and digest/hash remains output-plane only
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:20`-`30`).
   C1-C5 each name scalar-reference sketch, same-wave consumer/proof, output
   plane, feature/fallback, and reject boundary
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:59`-`184`).
   C8 is expressly non-parser output-plane surface with scalar digest source,
   consumer/proof requirement, fallback, and reject boundary
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:186`-`212`).
   Support rows are not standalone candidates
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:214`-`224`).

4. Accepted - P2-C's AArch64 inventory does not overclaim cost admission. The
   V3 fold carries forward the same-wave consumer, scalar reference, strict
   parity/checkasm or product parity, feature/fallback, output-plane, and reject
   constraints
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:9`).
   TBL/classifier, UDOT digit-span, widened string-special block, x4 hex proof,
   and whitespace skip each name scalar status, same-wave consumer, fallback,
   micro-proof, and reject boundary
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:27`-`72`).
   Movemask, `EXT`, PMULL/CTZ, SHA3 ternary, and PRFM/STNP/cache hints stay
   support or inventory rows with no standalone row movement
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:73`-`81`).

5. Accepted - P2-D satisfies CH4 for scalar consumer-shape and tape/accounting
   candidates. D1-D5 state scalar references and candidate shape
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:28`-`36`).
   The V2 proof-gate table then binds every D row to output-plane declaration,
   scalar-output parity plan, micro-proof/reject boundary, and fallback/no-op
   plan
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:38`-`48`).
   Checkasm is N/A for these scalar/code-shape packets unless an optional SIMD
   mask body is later routed; product-output parity is the required cost gate.

6. Accepted - P2-E carries the complete parse-that tuple for the retained
   primitive gaps. It states the CH4 tuple requirement, exact parity commands,
   strict no-regression, useful movement floor, and strict bbnf-simd checkasm
   requirement for any AArch64 body
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:33`-`43`).
   The four retained gaps, `pt_byte_set_run_skip`,
   `pt_bounded_plain_string_end`, `pt_digit_run_span_accumulate`, and
   `pt_escaped_string_segments`, each carry scalar reference, output plane,
   same-wave product consumer, row/proof set, strict parity/checkasm, fallback,
   and reject boundary
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:45`-`50`).
   `container_dispatch`, `simd_movemask`, and `output_digest_hash` remain
   support/oracle-only for P2-E
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:21`-`31`).

7. Accepted - P2-F is CH4-clean as the grammar-neutral crosswalk. It declares
   C1-C7 as the parser primitive pool and keeps C8/C9 outside parser-primitive
   admission
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:37`-`56`).
   C1-C7 carry scalar/parity gates plus same-wave consumer and reject boundaries
   at the abstraction level
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:41`-`49`).
   C8 is benchmark/oracle or host-sink only, and C9 is Lock-1/output-plane
   accounting only
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:51`-`56`).
   The mandatory non-JSON benchmark still requires generated Track 1
   before/after throughput, independent Track 2 or oracle, strict equality,
   primitive self-time, PMU c/B where available, strict checkasm/parity for
   SIMD/ASM, scalar/no-op fallback, no sidecar allocation, no generic-crate
   grammar names, and a same-wave generated non-JSON consumer
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86`-`91`).

8. Accepted - The local SIMD/checkasm evidence supports the cost posture. The
   `bbnf-simd` crate exposes the `checkasm_parity` test target
   (`skinny/crates/bbnf-simd/Cargo.toml:25`-`27`). Dedicated byte-class
   checkasm states scalar reference as the executable specification and requires
   every backend to agree bit-for-bit
   (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:14`-`17`).
   AArch64 string-block tests compare NEON output directly to
   `scan_string_special_block_scalar`
   (`skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:139`-`164`). P2-E
   requires `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --tests` when any
   AArch64 body is routed
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:39`-`40`).

## Accepted Facts For S-P3

1. C1-C7 are the parser primitive pool. C8 is benchmark/oracle or per-product
   host sink only; C9 is Lock-1/output-plane accounting only.
2. `HEX_QUARTET_X4_PROOF`, `MOVEMASK_EXHAUSTIVE_GATE`,
   `OUTPUT_DIGEST_HASH_ORACLE`, support/inventory ISA rows, and C9 cannot close
   behavior gates as standalone parser row movers.
3. Any AArch64 SIMD/ASM candidate shortlisted by S-P3 must carry scalar oracle,
   strict candidate-specific checkasm/parity, same-host micro-proof, feature
   gate, scalar/no-op fallback, same-wave consumer, output-plane declaration, and
   reject boundary before product measurements count.
4. Scalar-only code-shape packets satisfy checkasm as N/A only when they carry
   product-output parity against generated Track 1 plus independent Track 2 or
   oracle, micro-proof-first plan, no-op fallback, and row movement or reject
   floors.

## Required Folds

None. CH4 accepts V3 for S-P2 convergence. The remaining cost work belongs to
S-P3 wave sequencing: concrete LOC budgets, risk class, owner paths, hard caps,
and per-wave falsifiability gates.
