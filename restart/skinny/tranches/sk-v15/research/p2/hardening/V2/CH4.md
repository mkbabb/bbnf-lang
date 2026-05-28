# SK-V15 S-P2 Research V2 - CH4 COST

Pass: S-P2 Research. Cycle: V2.
Lens: CH4 COST.
Output: `restart/skinny/tranches/sk-v15/research/p2/hardening/V2/CH4.md`.

## Overall Verdict

**ACCEPT.**

V2 closes the V1 CH4 cost defect for the surviving implementation surfaces. The binding CH4 test requires realistic LOC budget, risk class, wave alignment, hard cap, and same-wave consumer per primitive (`restart/prompts/ORCHESTRATOR.md:86`), while S-P2 adds scalar-reference status, checkasm/parity expectation, and same-wave-consumer notes as mandatory per-candidate fields (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`124`). Current P2-F now supplies a CH4 cost fold for non-REJECT survivors with scalar reference, parity gate, same-wave consumer, LOC budget, risk class, and wave/hard-cap column (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71`-`81`).

This is not an implementation approval. S-P3 may consume these rows only if it preserves the same-wave scalar/parity/consumer/measurement/rollback bundle and carries the hard cap into any split wave. The orchestrator still requires V-cycle folding before advance (`restart/prompts/ORCHESTRATOR.md:112`-`123`), and SK-V15 addenda still require wave dependency proof, broadcast-admission detection, and gate-exclusion reporting (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:98`-`110`).

## Evidence

1. **Authority and cycle gate.** ORCHESTRATOR CH4 is the cost authority: LOC budget, risk class, wave alignment, hard cap, and same-wave consumer must be stated and realistic (`restart/prompts/ORCHESTRATOR.md:81`-`87`). ORCHESTRATOR §3Z requires hardening to fold into the next cycle; paper-hardening does not advance (`restart/prompts/ORCHESTRATOR.md:112`-`123`). PASS-2-RESEARCH CH4 adds the scalar-reference, checkasm/parity, and same-wave-consumer minimum, with failure if any of the three is absent (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`124`).

2. **V1 defect is explicitly folded.** V1 found that P2-A through P2-F had candidate gates but lacked per-candidate LOC budgets, implementation risk classes, wave alignment, and hard caps (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:13`-`21`). V1 required every non-REJECT candidate to carry scalar reference, parity gate, same-wave consumer, cost budget, risk class, wave alignment, hard cap, and orphan risk before S-P3 shortlisting (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:109`-`125`). Current P2-F adds exactly that fold for non-REJECT survivor groups (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71`-`81`).

3. **Survivor universe is closed.** P2-F declares that P2-B/C/D/E are the implementation candidate universe, that P2-A is comparator context only, and that no new primitive is introduced in P2-F (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:10`-`12`). P2-A aliases are mapped to fold targets or REJECT in §2.1, so the comparator names do not escape without disposition (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59`-`69`).

4. **Scalar/checkasm/same-wave discipline is present before cost.** P2-B defines the admission process as scalar oracle, SIMD/ASM path, strict checkasm parity, same-wave consumer, and manifest/lock closure (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:24`-`32`). It also states source-present primitives must end as wired, deleted, scalar-delegate-non-ASM, or architectural-block-with-REDRESS; orphan intrinsic/ASM files do not close Lock 16 (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:41`).

5. **Rejected diagnostics are excluded from implementation cost obligations.** P2-F rejects numeric/digit surfaces, `EOB_PAD_CLAMP` as S-P2 implementation candidate, PMULL hot-body promotion, CSSC CTZ bulk consumer, retained sidecars, schema/harness rows, and x86 diagnostic routes (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:104`-`115`). These rows do not appear in the non-REJECT CH4 cost fold except where a surviving scalar/local abstraction remains, such as local prefix algebra or local mask-to-tape operation (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:30`-`31`, `:75`-`:81`).

## Findings

1. **Survivor CH4 fields are present at the grouped implementation-surface level.** The classifier family has existing/new scalar reference expectations, existing/new checkasm parity, same-wave scanner consumers, <=220 LOC code and <=180 LOC tests, medium risk, and same-wave rollback/demotion language (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:75`). The eq-set / byte-set-run surface has scalar loop and eq-set reference, checkasm plus caller equality, JSON and non-JSON consumers, <=180 LOC code and <=160 LOC tests, medium risk, and delete-on-fail cap (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:76`).

2. **String, UTF-8, escape, direct-cursor, and same-tape survivors now have cost caps.** P2-F assigns scalar references, parity gates, consumers, LOC/test budgets, risk classes, and wave caps to string/literal spans, UTF-8 run validation, escape/unicode segment work, direct cursor/FIRST-set templates, and same-tape capacity/flag/fact/mask-writer work (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:77`-`81`). The high-risk rows also name the relevant REDRESS block families and require rollback, scalar-only demotion, deletion, or no-sidecar limits.

3. **Same-tape parity is correctly treated as equality/materialisation rather than mandatory checkasm.** P2-D says capacity, sparse flags, same-tape fact projection, and mask-to-tape writer use offset/flag/view/fact equality and materialisation evidence, with checkasm only if a SIMD producer is introduced (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:36`-`41`). P2-F carries that rule into the cost fold for same-tape operations (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:81`).

4. **Hard-cap realism is acceptable for research handoff, with a required S-P3 carry-forward constraint.** The V2 caps are not open-ended: every non-REJECT surface has a maximum code/test LOC envelope, same-wave implementation/test/consumer/measurement/rollback language, and a delete/demote/forbid condition (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:75`-`81`). If S-P3 splits a grouped surface into smaller waves, it must preserve these caps per split rather than treating the group table as a pooled budget.

5. **Diagnostic rows are not cost debt.** P2-E marks digit-run accumulate REJECTED for this S-P2 cycle and names no same-wave consumer (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154`-`185`). P2-C rejects UDOT digit work, CSSC CTZ bulk emit, PMULL prefix-XOR production promotion, and x86 admission (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`-`35`). P2-F consolidates those as REJECT surfaces (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:108`-`115`), so they impose no implementation LOC, test, wave, or hard-cap obligation in S-P3 unless reopened by fresh P1 evidence.

## Orphan V1 Disposition Check

| V1 issue | V2 disposition |
|---|---|
| P2-A comparator rows lacked checkasm/consumer/cost and could escape as raw candidates (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:31`-`41`). | Closed. P2-F maps all seven P2-A aliases to fold targets, REVISE reframings, or REJECT (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59`-`69`). |
| Non-REJECT survivors needed scalar reference, parity gate, same-wave consumer, cost budget, risk class, wave alignment, hard cap, and orphan risk (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:109`-`123`). | Closed at grouped surface level. P2-F §2.2 supplies those fields for all non-REJECT survivor groups (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71`-`81`). |
| Existing/checkasm-green primitives risked becoming orphan kernels without consumers (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:19`-`23`). | Closed. P2-B requires same-wave consumer and final wired/deleted/delegated/blocked status (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:24`-`32`, `:41`); P2-F rejects support inventory or production promotions without surviving consumer proof (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`, `:45`-`:47`, `:108`-`:115`). |
| Tape/allocation pressure was incorrectly tempting as a standalone SIMD primitive (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:57`, `:79`-`:82`). | Closed. P2-B rejects it as SIMD/ASM primitive (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:57`), P2-D reframes same-substrate tape candidates with equality/materialisation parity (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:36`-`41`), and P2-F assigns same-tape cost caps (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:81`). |
| REDRESS-blocked PMULL, CSSC, retained sidecars, x86, schema/harness, and numeric diagnostic rows needed to remain outside implementation obligations (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md:99`-`107`). | Closed. P2-F's REJECT table excludes them from the survivor cost fold (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:104`-`115`). |

## Disposition

CH4 V2 is **ACCEPT**. The only carry-forward is mechanical: S-P3 must preserve the P2-F §2.2 cost block on every shortlisted split and must not assign implementation budgets to rows P2-F marks REJECT or diagnostic.
