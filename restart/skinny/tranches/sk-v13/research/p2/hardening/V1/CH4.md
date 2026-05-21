# SK-V13 S-P2 V1 CH4: Cost / Scalar Reference / Checkasm / Same-Wave Consumer

Verdict: ACCEPT.

## Evidence

- The CH4 contract is explicit: every candidate must carry scalar-reference status, checkasm/parity expectation, and same-wave-consumer note; a candidate missing any one of the three fails CH4 (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`124`). S-P1 is converged and authorizes S-P2, but keeps every profile fact non-admissive until later S-P2/S-P3/redress selection (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:10`-`15`, `:53`-`:61`).

- P2-A carries the required CH4 fields for all eight SOTA-derived primitives: its candidate table has dedicated columns for scalar-ref status, arch/checkasm expectation, and same-wave consumer/reject boundary (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:70`-`87`). It also fences support-only items until attached to a row-moving primitive with scalar reference, checkasm/parity, and same-wave row consumer (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:83`-`87`).

- P2-B defines the process gate S-P3 should enforce before shortlisting any SIMD/ASM wave: scalar reference, differential checkasm, same-wave consumer, grammar policy, and row gate are all mandatory stages (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:24`-`35`). Its B1-B5 candidates each name scalar-ref status, checkasm expectation, and same-wave consumer/reject boundary (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:37`-`103`).

- P2-C's AArch64 table is CH4-complete: C-P2C-1 through C-P2C-7 include scalar reference, checkasm/parity expectation, same-wave consumer, and S-P3 disposition columns (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:28`-`38`). Inventory-only entries are not papered over: EOR3 is non-shortlistable without a measured string-mask consumer, and `byte_context` is close hygiene unless wired through C-P2C-5 or deleted/demoted (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:24`, `:37`-`:38`).

- P2-D's substrate/tape candidates D1-D5 each carry the required non-ISA parity form or checkasm escalation rule plus a same-wave consumer. D1 names tape materialization parity and retained parser/CSS constructor consumers; D2 names offset/token/ValueRef/direct/CSS fact parity and CSS/JSON consumers; D3 names scalar-vs-SIMD mask parity and retained parse/CSS consumers; D4 names direct/CSS fact equality and direct/CSS row consumers; D5 names flag-byte/string-output parity and CSS/JSON unicode consumers (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:146`-`181`, `:183`-`:223`, `:225`-`:272`, `:274`-`:310`, `:312`-`:343`).

- P2-E's parse-that gaps P2E-1 through P2E-8 all include scalar reference sketch, checkasm expectation, architecture/status, and same-wave consumer note. Non-ISA analysis/policy entries correctly say checkasm is not applicable unless a SIMD recognizer is selected, then require unit/property or downstream SIMD parity before routing (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:30`-`116`).

- P2-F cross-folds the sibling candidates into a single Lock-14 matrix with a combined "Scalar ref / checkasm / same-wave consumer" column for CSS, dispatch, sink, SIMD, regex, decision-engine, and union candidates (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:60`-`101`). It also prevents CH4 overclaim by marking no-consumer or no-antecedent surfaces as `INVENTORY-ONLY` and non-shortlistable without fresh material differential (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:35`-`42`, `:132`-`:145`).

## Blockers

None.

## Fold Requirements

No V2 fold is required for CH4. S-P3 must preserve the explicit reject boundaries already present here: inventory-only SIMD is not shortlistable without a fresh same-wave consumer, policy/refactor-only work is not a behavior admit without row movement, and SIMD/ASM admission commands must retain strict scalar/checkasm parity before production wiring.
