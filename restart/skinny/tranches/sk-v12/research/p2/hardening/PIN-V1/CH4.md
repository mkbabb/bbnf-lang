# SK-V12 S-P2 PIN-V1 CH4 Cost / Scalar-Reference / Checkasm Review

Verdict: REVISE

Score: 78%

## Blocking Findings

1. P2-A is not CH4-complete on the current on-disk packet. PASS-2 CH4 requires each candidate to carry scalar-reference status, checkasm/parity expectation, and same-wave-consumer note (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`:124`), while the current P2-A table exposes only `Candidate`, `Shape`, `Scalar-ref status`, `Arch`, and `P1 antecedent` (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`-`:37`). That leaves no per-candidate checkasm/parity expectation, no same-wave consumer class, no micro-prove-first line, and no zero-orphan disposition for C1-C7. This directly contradicts the prior V3 CH4 claim that P2-A has explicit `Checkasm/parity expectation` and `Same-wave consumer note` columns (`restart/skinny/tranches/sk-v12/research/p2/hardening/V3/CH4.md:31`).

2. The convergence marker and V3 CH4 accounting are stale against the current six P2 artifacts. `HARDENING-S-P2-CONVERGED.md` says P2-C has six current AArch64 candidates and P2-E has five parse-that primitive gaps (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:28`-`:34`), but current P2-C enumerates C1-C12 (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:40`-`:146`) and current P2-E enumerates nine rows, including output-plane/fact-stream rows (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:31`-`:42`). S-P2 cannot stay CH4-converged while the accepted boundary describes a different candidate surface.

3. Support-only and oracle/accounting rows are still mixed into candidate tables without a uniform row-mover disclaimer at the source row. P2-F correctly labels bitmap next-bit/prefix-XOR as support-only and output digest as parser-candidate-ineligible (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`-`:43`, `:51`), but P2-A still presents C6 `grammar_output_event_sink` and C7 `generated_dispatch_template` in the same candidate table as primitive shapes (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:36`-`:37`). P2-E similarly lists `pt_fact_event_emit` and `pt_fact_stream_digest` as candidate primitives (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:41`-`:42`) and only later narrows them to output-plane bridge/digest discipline (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:54`). The fold must mark these as legality/oracle/accounting surfaces, not row movers.

4. Zero-orphan handling is present globally but not attached to every SIMD-backed candidate. The user pin requires zero orphan kernels by SK-V12 close (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71`-`:78`), and the coverage audit names the five carried orphans plus their support-only status (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:36`-`:61`). P2-B and P2-F carry this discipline (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:27`-`:37`; `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:26`-`:28`), but P2-A and P2-E do not attach orphan admission/removal/demotion handling per candidate. CH4 should fail closed until every SIMD-backed row says whether it consumes, demotes, removes, or does not touch the orphan set.

## Nonblocking Notes

- P2-B is the strongest CH4 source. It explicitly orders scalar oracle, differential checkasm, Lock 16 provenance, micro-prove-first, same-wave consumer, strict comparator, and zero-orphan close (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:29`-`:37`), and its table includes scalar-ref, checkasm, and same-wave-consumer columns (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:41`-`:56`).
- The `escape_mask_64` prerequisite is correctly treated as a blocker in P2-B, P2-C, P2-E, P2-F, HANDOFF, and USER-PIN (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:20`; `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:20`; `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:66`; `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:26`; `restart/skinny/tranches/sk-v12/HANDOFF.md:125`-`:138`).
- The bbnf-simd checkasm substrate is real enough to support the fold once the accounting is fixed: the harness covers reference-vs-candidate calls, alignment sweeps, stack canaries, signal guards, strict mode, bug injection, and primitive checkasm routes (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41`-`:63`, `:211`-`:244`; `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`-`:20`).
- P2-D is appropriately conservative: no P2-D primitive is shortlist-ready, and same-tape ideas are diagnostic or conditional until CSS Track 1 and same-plane lightningcss evidence exist (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:102`-`:112`, `:144`-`:156`).

## Exact Fold Edits If REVISE/REJECT

1. In `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`, replace the §2 table with per-row columns: `Candidate`, `Class`, `Scalar reference`, `Checkasm/parity expectation`, `Micro-proof`, `Same-wave consumer`, `escape_mask_64 / Lock 16 prerequisite`, `Orphan disposition`, and `P1 antecedent`. Mark C6 as output-plane/oracle contract and C7 as generated-template legality surface; neither is a parser row mover.

2. In `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`, split §2 into `Selectable candidates` and `Inventory/support/nonselectable`. Keep C1/C3/C4/C5/C6 conditional; mark C2/C9 inventory-drop unless a fresh profile names the stream/formula; mark C7/C8 as narrow support only after W2 `escape_mask_64` is green; mark C10/C11/C12 support/inventory with explicit consume-or-demote zero-orphan language.

3. In `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`, add a `Checkasm/parity expectation` and `Candidate class` column. Split parser primitives from output-plane gates: `pt_fact_event_emit` and `pt_fact_stream_digest` must be labeled output-plane/oracle/accounting, not parse-that scanner row movers. Add per-row `escape_mask_64` blocker text for string/escape/SIMD-backed rows and orphan-disposition text for Layer-0-backed rows.

4. In `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md` and the next consolidated hardening pass, replace stale candidate counts with the current folded surface. Do not claim P2-A has checkasm/same-wave columns until the P2-A table actually has them; do not claim P2-C has six current candidates or P2-E has five gaps unless those artifacts are folded back to that shape.

5. Redispatch PIN-V2 CH4 after the folds. Acceptance criteria: every current candidate/support/oracle/accounting row has scalar-reference status, checkasm/parity or explicit N/A, micro-proof or explicit N/A, same-wave consumer/proof or explicit ineligible status, `escape_mask_64` prerequisite where SIMD/string-region admission is implicated, and orphan consume/remove/demote handling where aarch64 production support is touched.

## Commands Used

- `git status --short`
- `rg --files restart/skinny/tranches/sk-v12/research`
- `rg --files | rg 'checkasm|simd|coverage-audit|totality-fold-scout|scalar|PIN|pin|S-P1|convergence|p2|P2|escape_mask_64'`
- `sed -n '1,220p' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `sed -n '1,260p' restart/prompts/ORCHESTRATOR.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `sed -n '1,230p' restart/skinny/tranches/sk-v12/HANDOFF.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md`
- `sed -n '1,240p' restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`
- `rg -n 'CH4 COST|scalar-reference|checkasm-parity|same-wave-consumer|Convergence criterion' restart/prompts/skinny/PASS-2-RESEARCH.md restart/prompts/ORCHESTRATOR.md`
- `rg -n 'escape_mask_64|orphan|same-wave|checkasm|micro-prove' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md`
- `rg -n 'scalar reference|checkasm|escape_mask_64|same-wave|primitive-checkasm|orphan' skinny/crates/bbnf-simd/CHECKASM-REPORT.md skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '25,44p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '29,44p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '31,53p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/V3/CH4.md | sed -n '17,39p'`
