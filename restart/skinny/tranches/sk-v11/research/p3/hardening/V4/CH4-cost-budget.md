# SK-V11 S-P3 CHALLENGE V4 CH4 Cost / Wave Budget

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V4.
Date: 2026-05-20.
Scope: stability review for cost, wave count, spare bracket slot, hard caps,
LOC budgets, W8 split discipline, and W1a/W1b/W2 feasibility.
Output: this file.
Disposition: ACCEPT.
Accept rate contribution: 1.

## Verdict

ACCEPT. V4 preserves the V3 CH4 cost/budget contract. The packet still uses an
11-wave bracket, W0, W1a, W1b, W2-W9, inside the <=12 skinny ceiling and with
exactly one spare split. It still assigns per-wave LOC budgets, phase hard caps,
redress caps, micro-prove-first requirements, and same-wave-consumer gates. W8
source work remains outside default W8 and must become W8a, consuming the only
spare bracket slot.

The V3 consolidation asked V4 to be a stability cycle that preserves V3
semantics (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`).
This CH4 pass finds no V4 cost regression. The residual risk remains the same
executional W1b risk named in V3 CH4: current codegen/runtime are still
JSON-profile-gated, so W1b is feasible only because V4 keeps it to exactly one
generated non-JSON baseline plus independent oracle, no intervention, and no row
admission.

## Challenge Checks

| Check | Disposition | Evidence |
|---|---|---|
| Wave count <=12 and spare slot | ACCEPT | ORCHESTRATOR escalates a skinny bracket exceeding 12 waves (`restart/prompts/ORCHESTRATOR.md:125-128`), and PASS-3 CH4 asks this lens to check wave count <=12 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132`). V4 P3-B states 11 waves with exactly one spare split before escalation (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:43-46`). SPEC repeats 11 waves and one spare split (`restart/skinny/tranches/sk-v11/SPEC.md:206-211`). DISPATCH repeats the same bracket (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-72`). |
| Shortlist / candidate budget | ACCEPT | PASS-3 CH4 also checks shortlist <=8 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132`). P3-B keeps the row-moving candidate budget to C1-C7 plus C8, while C9 is accounting only (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:47-48`). P3-F's V4 wave table preserves that allocation: W2-W6 use C1-C7 support, W7 is C8 only, and W1a/W1b carry C9 accounting/baseline work (`restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md:56-68`). |
| Hard caps and phase breakdown | ACCEPT | P3-B inherits research 30 min, plan 30 min, redress 75 min, and CHALLENGE 60-90 min for first-of-class/high-risk plans (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:57-62`). SPEC binds redress to a 75-minute target and 90-minute hard cap, and forces REVISE before source work when a plan cannot fit (`restart/skinny/tranches/sk-v11/SPEC.md:201-204`). DISPATCH preserves research, plan, CHALLENGE, and redress phase rules, single redress thread, gate thresholds, and measured failure handling (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:79-131`). |
| LOC budgets | ACCEPT | SPEC gives every wave an explicit LOC budget: W1a <=260, W1b <=360, W2 <=430, W3 <=360, W4 <=430, W5 <=360, W6 <=360, W7 <=350, W8 <=250 docs/gate/result, and W9 80-180 docs/gate LOC (`restart/skinny/tranches/sk-v11/SPEC.md:187-199`). P3-B and DISPATCH mirror those budgets (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:64-76`, `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:51-63`). Generated output remains capped to named inputs, not handwritten scope expansion (`restart/skinny/tranches/sk-v11/SPEC.md:231-244`). |
| W8 split discipline | ACCEPT | V4 keeps W8 as docs/gate/result accounting by default. P3-B requires any W8 source work to become CHALLENGE-accepted W8a and consume the only spare bracket slot (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:75`). SPEC says source work is not part of W8 unless split as W8a, and if source work is required it must route to W8a first with exactly one candidate and one row subset (`restart/skinny/tranches/sk-v11/SPEC.md:692-721`). DISPATCH makes W8 CHALLENGE mandatory if it touches behavior source or generic/codegen/runtime-outside-JSON paths (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:100-103`). |
| Micro-prove-first and same-wave consumer cost | ACCEPT | SPEC blocks W2-W7 redress until scalar/oracle, differential/checkasm when applicable, same-host microbench, observed threshold facts, same-wave consumer path, fallback, and REDRESS boundary are recorded (`restart/skinny/tranches/sk-v11/SPEC.md:213-227`). P3-B says missing consumer means REJECT, not deferral (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:115-119`). DISPATCH requires plans to name scalar/oracle, parity/checkasm, micro-proof, thresholds, LOC budget, risk class, revert protocol, same-wave consumer, and pre-blocked routes before redress (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:88-95`). |
| W1a feasibility | ACCEPT | W1a remains gate/report work, not parser row movement. SPEC says no parser rows move, tasks are non-JSON evidence consumption plus fixtures, JSON gate remains green, no JSON rows move, and no generated baseline authority is claimed (`restart/skinny/tranches/sk-v11/SPEC.md:283-324`). P3-B sizes W1a at <=260 handwritten LOC and <=90 min (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:67`). Current report code already centralizes required telemetry fields and rejects unsupported non-JSON grammar/domain values, so this is a bounded gate/report extension (`skinny/crates/bbnf-bench/src/report.rs:276-332`, `skinny/crates/bbnf-bench/src/report.rs:620-653`). |
| W1b feasibility | ACCEPT | W1b is feasible only under its narrow V4 scope: exactly one target selected by CHALLENGE, one generated non-JSON baseline, one independent oracle/Track 2, strict equality, gate consumption, no intervention, and no JSON row movement (`restart/skinny/tranches/sk-v11/SPEC.md:326-377`). P3-B caps it at <=360 handwritten LOC and regenerated output only for selected inputs (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:68`). The source spot-check confirms why the cap must stay this narrow: codegen still calls `json_provider::ensure_runtime_profile` for both direct and typed emission (`skinny/crates/codegen/src/lib.rs:102-108`, `skinny/crates/codegen/src/lib.rs:139-147`), and that guard rejects non-`json` runtime emission (`skinny/crates/codegen/src/json_provider.rs:4-12`). |
| W2 feasibility | ACCEPT | W2 remains separate from W1b and consumes the W1b baseline. SPEC requires W1b closed at entry, exactly one selected generated non-JSON intervention, baseline Mbps, target threshold, and Lock 14 proof (`restart/skinny/tranches/sk-v11/SPEC.md:397-400`), then says W2 wires exactly one primitive family and may not create the first measurable non-JSON row (`restart/skinny/tranches/sk-v11/SPEC.md:402-409`). P3-B makes W1b block W2 and says W2 is REVISE before redress if W1b cannot produce the baseline (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:80-86`, `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:106`). The <=430 handwritten LOC budget is therefore scoped to one intervention consumer, not baseline plus intervention. |
| V4 preservation of V3 CH4 | ACCEPT | V3 CH4 accepted the same 11-wave bracket, one spare split, per-wave LOC and 90-minute caps, W8/W8a split discipline, micro-prove-first cost, same-wave consumer rule, and W1a/W1b/W2 feasibility (`restart/skinny/tranches/sk-v11/research/p3/hardening/V3/CH4-cost-budget.md:34-43`). V4 P3-B, P3-F, SPEC, and DISPATCH preserve those load-bearing facts rather than reopening the cost surface. |

## Residual Risk

The risk is executional, not a V4 packet defect. W1b is tight because the current
runtime exports generated JSON plus proof-only witness plumbing rather than a
CSS generated runtime (`skinny/crates/runtime/src/lib.rs:3-16`). V4 keeps that
risk bounded: W1a only prepares the gate/report lane, W1b creates one baseline
plus oracle, W2 consumes that baseline for one intervention, and any plan that
cannot fit its cap returns REVISE before source work.

## Required Action

None for CH4. No source edits were made.
