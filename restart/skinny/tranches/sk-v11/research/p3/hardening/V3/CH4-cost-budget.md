# SK-V11 S-P3 CHALLENGE V3 CH4 Cost / Wave Budget

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V3.
Date: 2026-05-20.
Scope: cost, wave count, redress caps, W8 split/spare bracket, LOC caps, micro-prove-first cost, and W1a/W1b/W2 feasibility review for SK-V11.
Output: this file.
Disposition: ACCEPT.
Accept rate contribution: 1.

## Verdict

ACCEPT. V3 preserves the V2 CH4 cost fixes and does not introduce a new budget
defect. The bracket is explicitly W0, W1a, W1b, W2-W9: 11 waves with one spare
split before the skinny `> 12` escalation rule. Every dispatchable behavior
wave carries a handwritten LOC budget, a redress cap, a phase cap, and a
reject-before-redress rule when the plan cannot fit. W8 source work is isolated
behind W8a and consumes the only spare split.

The only tight budget remains W1b. A source spot-check confirms why it must stay
narrow: skinny codegen still rejects non-`json` runtime emission
(`skinny/crates/codegen/src/json_provider.rs:4-12`,
`skinny/crates/codegen/src/lib.rs:102-108`, `:139-147`), and the skinny runtime
exports `generated_json` plus proof-only sheets witness plumbing rather than a
CSS generated runtime (`skinny/crates/runtime/src/lib.rs:3-16`). V3 handles that
cost correctly by splitting W1a/W1b/W2, limiting W1b to exactly one generated
non-JSON baseline plus oracle, forbidding intervention/admission in W1b, and
requiring W2 to consume the W1b baseline rather than create it.

## Challenge Checks

| Check | Disposition | Evidence |
|---|---|---|
| Wave count and spare bracket | ACCEPT | P3-B states the bracket is 11 waves, W0/W1a/W1b/W2-W9, with exactly one spare split before the `> 12` escalation rule (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:43-48`). SPEC repeats the same count and reserves the spare for either W8a or one behavior-wave rescue (`restart/skinny/tranches/sk-v11/SPEC.md:206-211`). DISPATCH repeats the 11-wave budget and spare split (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-72`). The underlying skinny rule escalates after `> 12` waves (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:102-110`). |
| Shortlist count | ACCEPT | PASS-3 CH4 requires wave count <=12 and shortlist <=8 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132`). P3-B says the packet consumes at most eight candidate surfaces, C1-C7 plus C8, while C9 is accounting rather than a row-moving candidate (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:47-48`). P3-F's wave table keeps that split: W2-W6 consume C1-C7 support, W7 is C8 only, and W1a/W1b carry C9 accounting/baseline harness rather than row-moving shortlist items (`restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md:56-68`). |
| 90-minute caps and phase breakdown | ACCEPT | P3-B inherits research 30 min, plan 30 min, redress 75 min, and CHALLENGE 60-90 min for first-of-class/high-risk plans (`p3b-wave-sequencing.md:57-62`). SPEC binds redress to a 75-minute target / 90-minute hard cap unless a SPEC-recorded split or extension is accepted, and requires REVISE before source work when a plan cannot fit (`SPEC.md:201-204`). DISPATCH gives research, plan, challenge, and redress phase rules and caps (`DISPATCH-PROMPT.md:79-131`) and halts/escalates at the redress cap (`DISPATCH-PROMPT.md:210-212`). |
| LOC caps | ACCEPT | SPEC gives W1a-W9 explicit handwritten LOC caps and separates regenerated-output caps from handwritten source/test/gate LOC (`SPEC.md:187-199`). P3-B gives the same per-wave budgets, including W1a <=260, W1b <=360, W2 <=430, W3/W5/W6 <=360, W4 <=430, W7 <=350, W8 <=250 docs/gate/result, and W9 source-free close (`p3b-wave-sequencing.md:64-76`). DISPATCH mirrors the same budget table (`DISPATCH-PROMPT.md:51-63`). Generated output is allowed only from named generator/schema inputs (`SPEC.md:178-179`, `:243-244`). |
| W8 split/spare bracket | ACCEPT | P3-B makes W8 docs/gate/result accounting by default and requires source work to become CHALLENGE-accepted W8a consuming the only spare bracket slot (`p3b-wave-sequencing.md:75`). SPEC says source work is not part of default W8, requires W8a first with exactly one candidate and one row subset, and leaves W8 as accounting (`SPEC.md:692-721`). DISPATCH makes W8 conditional on all behavior dispositions and states source work requires W8a plus the spare slot (`DISPATCH-PROMPT.md:69-72`, `:100-103`). |
| Micro-prove-first cost | ACCEPT | SPEC makes micro-prove-first a close condition for every kernel, substrate-adjacent, SIMD/ASM, or generic parser intervention: scalar reference/oracle, strict differential/checkasm or product parity, same-host microbench, fallback, same-wave consumer, and row gate (`SPEC.md:47-50`). It repeats that no primitive, SIMD kernel, generated path, codegen shape, or host sink ships without scalar/oracle, parity/checkasm where applicable, same-host microbench, same-wave consumer, and measured gate (`SPEC.md:172-175`). W2-W7 cannot reach redress until the plan records these proof artifacts and rejects primitive-only speed as production evidence (`SPEC.md:213-227`). DISPATCH requires the plan to name scalar/oracle, parity/checkasm, micro-proof, thresholds, LOC budget, revert protocol, and same-wave consumer (`DISPATCH-PROMPT.md:88-94`) and requires SIMD/ASM differential/checkasm before row measurements count (`DISPATCH-PROMPT.md:117-125`). |
| Same-wave consumer cost | ACCEPT | The load-bearing triumvirate rule requires every primitive/kernel/generated path to land its hot-path caller in the same redress commit and rejects omitted consumers as orphan kernels (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177-186`). P3-B makes a missing consumer a REJECT, not a deferral (`p3b-wave-sequencing.md:115-119`). SPEC repeats that telemetry is consumed by the relevant gate in the same wave and producer-only proof cannot close a wave (`SPEC.md:56-57`). |
| W1a feasibility | ACCEPT | W1a is not asked to create a baseline or move rows. Its tasks are gate/report consumption and fixtures for non-JSON evidence, while JSON `gate-json --with-cost-facts --check-results` stays green and no JSON rows move (`SPEC.md:283-324`). P3-B sizes that lane at <=260 handwritten LOC / <=90 min and says it blocks W1b (`p3b-wave-sequencing.md:67`). Current report code already centralizes the required telemetry fields but rejects non-JSON grammar/domain values, making W1a a bounded gate/report extension rather than parser work (`skinny/crates/bbnf-bench/src/report.rs:276-332`, `:620-653`). |
| W1b feasibility | ACCEPT | W1b is correctly sized only because it is one baseline, one grammar/workload, one oracle/Track 2 path, no intervention, and no row admission. SPEC requires CHALLENGE to select exactly one non-JSON target and names the independent oracle/Track 2 path (`SPEC.md:345-347`), then limits tasks to one generated baseline row, oracle, strict equality, and JSON-policy leak proof (`SPEC.md:349-367`). P3-B gives <=360 handwritten LOC and generated output capped to selected inputs (`p3b-wave-sequencing.md:68`). If this first-of-class lane cannot fit, SPEC's budget rule forces REVISE before source work (`SPEC.md:201-204`). |
| W2 feasibility | ACCEPT | W2 is feasible as a separate wave because it consumes W1b instead of creating baseline authority. SPEC requires W1b closed at entry, exactly one generated non-JSON intervention, baseline Mbps, target threshold, and Lock 14 proof (`SPEC.md:397-400`), and explicitly says W2 consumes the W1b baseline and may not create the first measurable non-JSON row (`SPEC.md:404-409`). P3-B makes W1b block W2 and says W2 is REVISE before redress if W1b cannot produce the baseline (`p3b-wave-sequencing.md:80-86`, `:106`). The <=430 handwritten LOC cap is therefore scoped to one intervention consumer, not baseline creation plus intervention (`p3b-wave-sequencing.md:69`). |

## Residual Risk

The residual risk is executional, not an S-P3 packet defect. W1b is close to the
edge because the current skinny codegen/runtime surface is JSON-profile-gated,
and W2 remains viable only if W1b actually produces the selected baseline and
oracle. V3 gives CH4 the necessary enforcement: CHALLENGE gates W1a, W1b, and
W2; W1b is single-target and no-admission; W2 cannot invent the first baseline;
and any plan that cannot fit the cap returns REVISE before redress.

## Required Action

None for CH4. Proceed only if the other V3 CHALLENGE lenses also accept.
