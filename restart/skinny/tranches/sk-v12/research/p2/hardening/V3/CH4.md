# SK-V12 S-P2 CHALLENGE V3 - CH4 Cost

Pass: S-P2 Research CHALLENGE. Cycle: V3.
Date: 2026-05-20.
Lens: CH4 COST.
Commit: 6b8be238.
Disposition: ACCEPT.

## Section 1 - Scope

ACCEPT. This is a convergence check over the V1-folded S-P2 research
cohort. CH4 asks whether every current candidate carries the accounting
triad: scalar-reference status, checkasm/parity expectation, and same-wave
consumer note (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`:124`).
The orchestrator makes same-wave consumers and scalar/checkasm parity
non-negotiable (`restart/prompts/ORCHESTRATOR.md:86`,
`restart/prompts/ORCHESTRATOR.md:205`-`:206`). S-P2 advances only after two
consecutive >=95% ACCEPT cycles with zero open critical defects or orphan
REVISE (`restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`:158`;
`restart/prompts/ORCHESTRATOR.md:118`-`:121`).

V2 already accepted the CH4 fold and directed V3 against the same folded
cohort (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:15`,
`:21`, `:44`). V3 re-checks the current packet rather than adding new
candidate authority.

## Section 2 - Candidate Accounting

| Artifact | CH4 result | Evidence |
| --- | --- | --- |
| P2-A SOTA teardown | ACCEPT | C1-C7 now have explicit `Scalar-reference status`, `Checkasm/parity expectation`, and `Same-wave consumer note` columns (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`-`:37`). The scalar-sketch floor says S-P3 must replace sketches with executable scalar references before native wiring (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:39`-`:49`). |
| P2-B DAV1D/FFmpeg process | ACCEPT | The common process orders scalar oracle, differential checkasm, feature/fallback, caller micro-proof, and same-wave consumer before admission (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:27`-`:34`). Its 12 admission gates carry scalar-ref status, strict parity/checkasm, same-wave consumer rule, and admission boundary columns (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`-`:51`). |
| P2-C architecture/esoterica | ACCEPT | The artifact declares six current candidates and two inventory/non-selectable support entries (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:38`-`:42`). Every current candidate has scalar-ref status, checkasm expectation, and same-wave consumer text: C1 (`:44`-`:53`), C3 (`:67`-`:76`), C4 (`:78`-`:87`), C5 (`:89`-`:98`), C6 (`:100`-`:109`), and C7 (`:111`-`:120`). |
| P2-D substrate/tape | ACCEPT | Current selectable candidate count is zero; same-tape diagnostics are three; rejected parallel-substrate routes are one (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`-`:72`). The table carries scalar-reference, checkasm/parity, same-wave proof/consumer, P1-grounding, and disposition columns (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:74`-`:79`). |
| P2-E parse-that primitive gaps | ACCEPT | All five gaps carry scalar sketches plus checkasm/parity and same-wave consumer notes: byte-set run skip (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:60`-`:89`), bounded plain string end (`:110`-`:147`), digit run span/accumulate (`:175`-`:216`), hex quad decode (`:237`-`:278`), and escaped string segments (`:307`-`:354`). |
| P2-F grammar-neutral map | ACCEPT | F1-F8 now carry scalar-ref status, checkasm/parity status, same-wave consumer/proof note, P1 antecedent, and eligibility columns (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:31`-`:40`). F7 and F8 are not parser candidates: F7 is oracle-only and F8 is accounting-only (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`-`:40`). |

The bbnf-simd process evidence still supports this accounting without
weakening the consumer rule. The local harness maps reference-vs-candidate
calls, source mutation checks, alignment sweeps, stack canaries, signal
guards, and outlier filtering into the Rust checkasm framework
(`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41`-`:51`). Strict mode
promotes random/misaligned divergences, while corpus parity always asserts
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`-`:20`,
`:345`-`:370`). The no-orphan rule remains binding for primitive bodies that
lack real codegen/runtime consumers (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:251`-`:254`).

## Section 3 - Ineligible Surface Check

Non-candidates remain ineligible:

| Surface | V3 CH4 status | Evidence |
| --- | --- | --- |
| P2-C `a64_ld4_interleaved_classifier64x4` | Inventory only | It has no sufficient SK-V12 P1 antecedent and is non-selectable until a fresh profile names an existing interleaved stream plus scalar deinterleave oracle and generated consumer (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:55`-`:65`). |
| P2-C `a64_sha3_ternary_bool_fold` | Inventory only | It has no sufficient SK-V12 P1 antecedent and is non-selectable until a fresh profile names the exact three-input expression plus scalar boolean formula, checkasm parity, and generated/runtime consumer (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:122`-`:133`). |
| P2-D same-tape diagnostics | Diagnostic/ineligible | Capacity policy, sparse flag lookup, and retained cursor skip each require fresh hot-leaf evidence and same-wave proof/consumer before they can become selectable (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:76`-`:78`). |
| P2-D structural class-lane union | Rejected | The route has no admissible scalar reference or legal same-wave consumer under Lock 1 and remains rejected under REDRESS 96/97/98 (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:79`). |
| P2-F F7/F8 | Parser-candidate-ineligible | Output digest/hash is oracle-only; tape/direct output-plane operations are accounting-only (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`-`:40`). |

The live result surface remains `N-direct / NoGo`, and Track 2 remains the
independent hand-coded tape parser rather than a generated Track 1 alias
(`skinny/RESULTS.md:143`-`:145`). REDRESS 119/120 keep SK-V11 residual direct
rows at measured fixpoint and route SK-V12 toward generated non-JSON baseline
work first (`skinny/REDRESS.md:3495`-`:3527`, `:3531`-`:3553`). CH4 therefore
finds no row-movement claim arising from primitive parity alone.

## Section 4 - Disposition

ACCEPT. The V3 S-P2 packet remains CH4-complete. Current candidates carry
scalar-reference/sketch status, checkasm or parity expectation, and same-wave
consumer/proof notes. Entries lacking current S-P1 movement authority remain
inventory-only, diagnostic/ineligible, oracle-only, accounting-only, or
rejected. No CH4 REVISE or REJECT remains open.

No source, RESULTS, REDRESS, lock, or sibling CH file change is requested by
this lens.
