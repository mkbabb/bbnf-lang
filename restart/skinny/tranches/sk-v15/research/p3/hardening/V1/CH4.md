# SK-V15 S-P3 V1 CH4 COST

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V1. Lens: CH4 COST.
Date: 2026-05-28.
Input packet: commit `4fe37c0429c2023a6e76aec1b0aa9d20532400c5`.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/CH4.md`.

## Verdict

REVISE.

The V1 packet passes the two count checks: P3-A declares eight candidates
and the table has exactly eight rows (`p3a-candidate-shortlist.md:5`,
`:20`-`:29`); P3-B/SPEC declare ten waves, W0 through W9
(`p3b-wave-sequencing.md:17`-`:19`, `SPEC.md:157`-`:166`), below the
12-wave skinny-bracket escalation ceiling (`ORCHESTRATOR.md:104`-`:116`,
`SKINNY-TRIUMVIRATE.md:100`-`:110`).

The cost contract is not yet acceptable. CH4 requires LOC budget, risk
class, wave alignment, realistic hard caps, phase breakdown, and
same-wave consumers (`ORCHESTRATOR.md:81`-`:87`;
`PASS-3-SYNTHESIS-PLAN.md:128`-`:132`). V1 carries phase time caps and
same-wave-consumer rules, but it does not carry per-wave LOC budgets or
risk classes, and W5/W7 are too large for a 30 minute redress cap unless
split before dispatch.

## Findings

| ID | Disposition | Evidence | Finding | Required fold |
|---|---|---|---|---|
| CH4-V1-01 | ACCEPT | P3-B lists `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9` and calls it 10 waves (`p3b-wave-sequencing.md:17`-`:19`); SPEC has W0-W9 rows (`SPEC.md:157`-`:166`). | Wave count is <=12. | None. Preserve <=12 after splits by reindexing to W0-W11 at most. |
| CH4-V1-02 | ACCEPT | P3-A scope is `<=8 candidate interventions` (`p3a-candidate-shortlist.md:5`), says the shortlist contains eight survivor families (`p3a-candidate-shortlist.md:12`), and enumerates candidates 1-8 (`p3a-candidate-shortlist.md:20`-`:29`). | Shortlist count is <=8. | None. Do not add a ninth candidate during V2 folds. |
| CH4-V1-03 | REVISE | SPEC and P3-B provide time caps (`SPEC.md:146`-`:153`; `p3b-wave-sequencing.md:23`-`:31`) but the wave manifest has no LOC budget or risk-class column (`SPEC.md:155`-`:166`; `p3b-wave-sequencing.md:45`-`:56`). | The packet fails the LOC/risk part of CH4. Without manual/generated/test LOC envelopes, the 30 minute redress cap is not auditable. | Add per-wave `Risk class`, `Manual source/test LOC budget`, `Generated LOC handling`, and `Docs/ledger LOC budget` columns to P3-B and SPEC Section 2, mirrored in DISPATCH-PROMPT envelopes. Generated output may be named separately, but cannot hide manual source or test scope. |
| CH4-V1-04 | ACCEPT WITH FOLD | SPEC and DISPATCH carry research/plan/redress caps (`SPEC.md:146`-`:153`; `DISPATCH-PROMPT.md:32`-`:39`). P3-B explicitly selects the SK-V15 Alpha cap rather than the broader default (`p3b-wave-sequencing.md:23`-`:29`). | Phase breakdown exists and is internally stated: research <=20m, plan <=15m, redress <=30m, halt/split at cap. | Keep the override explicit in V2 and add a note that SKINNY-TRIUMVIRATE default caps are superseded for SK-V15 by the Alpha cap. |
| CH4-V1-05 | ACCEPT WITH FOLD | P3-A names same-wave consumers for every candidate (`p3a-candidate-shortlist.md:20`-`:29`); P3-C rejects producer-only telemetry and unwired primitives (`p3c-falsifiability-gates.md:19`-`:21`); DISPATCH-PROMPT Section 5 requires same-wave consumption for primitives, generated paths, parser helpers, telemetry fields, and gate reports (`DISPATCH-PROMPT.md:173`-`:178`). | Same-wave-consumer requirements are present at the packet level. | After any wave split, move each consumer to the wave that lands the primitive/path/field. No split wave may rely on the following wave as its consumer. |
| CH4-V1-06 | REVISE | W5 must build typed CSS value, document, view, and visitor output, retime against cssparser, maintain JSON 51/51, and possibly retire old CSS proof (`SPEC.md:281`-`:305`; `p3c-falsifiability-gates.md:133`-`:154`). The owner path spans grammar, codegen, runtime, bench, gate, RESULTS, and rolling delta (`p3b-wave-sequencing.md:52`). | W5 is not hard-cap practical as one 30 minute redress wave. It combines new CSS API construction, comparator retiming, JSON guard maintenance, and provider retirement. | Split W5 into two waves: W5A CSS typed value/document/view/visitor provider plus one gate-consumed aggregate diagnostic row; W5B same-workload cssparser retiming, old-proof retirement, RESULTS/REDRESS update, and optional independent feature rows. W5A must not retire live CSS proof unless it also completes W5B-grade proof. |
| CH4-V1-07 | REVISE | W7 must implement real paths for EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage and add scaffold-failing tests (`SPEC.md:327`-`:346`; `DISPATCH-PROMPT.md:142`-`:151`). P3-C's older combined gate also says all five lowerers are invoked and emit non-placeholder output (`p3c-falsifiability-gates.md:156`-`:174`). | W7 is not hard-cap practical as one 30 minute redress wave. Five lowerers plus fixtures and generated diffs is a multi-surface implementation, not one cap-valid intervention. | Split W7 into two lowerer waves. Recommended fold: W7A fixture harness plus EagerTape/OffsetTape lowerers; W7B EventTape/SinkOnly/CollapsedStage lowerers plus the all-five gate. Each split wave must have generated fixtures that would fail against the previous scaffold. |
| CH4-V1-08 | REVISE | P3-C says it was authored before P3-B and must be revised if P3-B changes the wave set (`p3c-falsifiability-gates.md:12`). P3-B and SPEC split Decision Engine into W6 spine and W7 lowerers (`p3b-wave-sequencing.md:21`, `:53`-`:55`; `SPEC.md:163`-`:165`), while P3-C still labels W6 as combined Decision Engine plus lowerers and W7 as FNV (`p3c-falsifiability-gates.md:156`-`:199`). P3-F states a mapping but does not rewrite the stale gate table (`p3f-spec-draft.md:16`-`:20`). | Wave alignment is cost-ambiguous. A redress agent reading P3-C literally can overload W6 and misdispatch FNV as W7. | Rewrite P3-C to match the final wave index after V2 splits. If W5 and W7 split, use an integer W0-W11 map: W0 baseline; W1 CSS honesty; W2 locks; W3 codegen leak; W4 Pattern H; W5 CSS provider; W6 CSS retime/retire; W7 Decision spine; W8 lowerers A; W9 lowerers B; W10 FNV; W11 close. |

## Required Folds

1. Add a cost table to P3-B and SPEC Section 2. Required columns:
   `Wave`, `Risk class`, `Manual source/test LOC budget`,
   `Generated output budget/status`, `Docs/ledger LOC budget`,
   `Phase caps`, `Split trigger`, and `Same-wave consumer`.

2. Split W5. The minimum cap-valid shape is:
   - W5A: CSS typed value/document/view/visitor provider and same-wave
     diagnostic aggregate consumer.
   - W5B: same-workload cssparser retiming, old CSS proof retirement,
     RESULTS/REDRESS/gate update, and optional independent feature rows.

3. Split W7. The minimum cap-valid shape is:
   - W7A: lowerer fixture harness plus EagerTape and OffsetTape.
   - W7B: EventTape, SinkOnly, CollapsedStage, and the all-five lowerer
     gate.

4. Reindex after splits without exceeding 12 waves. The packet has room
   for exactly two added waves: current 10 plus W5 split plus W7 split
   equals 12. Do not add another top-level wave without removing or
   folding an existing one.

5. Rewrite P3-C, P3-F, SPEC, and DISPATCH-PROMPT so the wave numbers,
   same-wave consumers, dependency-table rows, and close gates use one
   final W0-W11 map. Hardening without this fold is paper-hardening under
   `ORCHESTRATOR.md:112`-`:116`.

6. Add an explicit redress-cap practicality rule: if plan research
   estimates exceed either LOC budget or 30 minutes redress, the wave
   must split before redress or record an intrinsic block. Challenge time
   cannot be used as implementation overflow.

## Cost Matrix

| Wave | V1 cost posture | CH4 action |
|---|---|---|
| W0 Baseline/telemetry | Cap-plausible if limited to telemetry/gate/report and no behavior change. | Add LOC/risk budget. |
| W1 CSS admission honesty | Cap-plausible as ledger/gate demotion; not cap-plausible if it retimes 24 feature rows. | Add LOC/risk budget and forbid retiming unless split or routed to W5B. |
| W2 Lock 14/16 restoration | High risk but acceptable if it only restores scan roots, reports exclusions, and classifies primitives. | Add LOC/risk budget and one coherent scan/report intervention. |
| W3 codegen leak abrogation | High risk but acceptable only if plan chooses one coherent leak family. | Add LOC/risk budget and preserve the "one coherent leak family" limit from DISPATCH-PROMPT. |
| W4 Pattern H discipline | High risk but acceptable if primarily gate/provenance/check work. | Add LOC/risk budget and block destructive deletion unless proof lands in-wave. |
| W5 CSS typed Value API | Too broad. | Split W5A/W5B. |
| W6 Decision spine | Cap-plausible after lowerers are moved out. | Keep as W6 with LOC/risk budget. |
| W7 BackendShape lowerers | Too broad. | Split W7A/W7B. |
| W8 FNV quarantine | Cap-plausible as bench/xtask quarantine plus strict-product negative fixtures. | Reindex after splits and add LOC/risk budget. |
| W9 Close reconciliation | Cap-plausible if no implementation residue remains. | Reindex after splits and add docs/ledger LOC budget. |

## Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128`-`:132`
- `restart/prompts/ORCHESTRATOR.md:81`-`:87`, `:104`-`:116`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:41`-`:75`, `:100`-`:125`
- `restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md:5`, `:20`-`:29`
- `restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:17`-`:35`, `:45`-`:56`
- `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:12`-`:21`, `:133`-`:174`, `:176`-`:199`
- `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:21`-`:65`
- `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:37`-`:60`, `:80`-`:106`, `:166`-`:175`
- `restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md:16`-`:20`
- `restart/skinny/tranches/sk-v15/SPEC.md:146`-`:166`, `:281`-`:346`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:32`-`:39`, `:56`-`:66`, `:123`-`:151`, `:173`-`:188`
