# S-P2 V3 CH1 Correctness Review

Role: CH1 (Correctness)

Verdict: REVISE

Score: 84/100

## Blocking Findings

1. **The V2 RESULTS-column correction is only partially folded; core SC docs still shift simdjson-sidecar deltas into sonic-strict evidence.**

   The governing CH1 rule is that comparator deltas must match the strictness plane (`restart/prompts/ORCHESTRATOR.md:83`), and S-P2 requires SOTA claims against the correct comparator source and strictness plane (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`restart/prompts/skinny/PASS-2-RESEARCH.md:100`, `restart/prompts/skinny/PASS-2-RESEARCH.md:214`-`restart/prompts/skinny/PASS-2-RESEARCH.md:219`). `skinny/RESULTS.md:3` defines the delta order as `Delta vs SK-V6`, `Delta vs sonic-strict`, `Delta vs simdjson DOM`, then `Delta vs yyjson`. Under that order, canada is `+27.9%` vs sonic-strict and `+54.6%` vs simdjson DOM (`skinny/RESULTS.md:10`); mesh is `+21.4%` vs sonic-strict and `+51.5%` vs simdjson DOM (`skinny/RESULTS.md:19`); citm_catalog is `+24.6%` vs sonic-strict and `-11.3%` vs simdjson DOM (`skinny/RESULTS.md:8`); apache_builds is `-28.2%` vs sonic-strict and `-65.3%` vs simdjson DOM (`skinny/RESULTS.md:12`).

   V3 fixes this in SC-4 and SC-5: SC-4 explicitly names canada `+27.9%` and mesh `+21.4%` vs sonic-strict while marking `+54.6%`/`+51.5%` as simdjson sidecar values (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:93`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:105`), and SC-5 states the same corrected taxonomy (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:39`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:61`). But the older shifted readings remain in load-bearing places:

   - SC-1 labels canada `+54.6%`, mesh `+51.5%`, and citm `-11.3%` as `same-run sonic strict` evidence (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:101`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:116`) and later repeats "same-run strict `parse_only` wins" as canada `+54.6%`, mesh `+51.5%` (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:194`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:199`).
   - SC-2 says canada is `+54.6% vs sonic strict`, mesh is `+51.5%`, and twitter is `-35.8%` in the same string-density evidence paragraph, but those are simdjson DOM deltas for canada, mesh, and twitter, not sonic-strict deltas (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:248`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:253`; compare `skinny/RESULTS.md:5`, `skinny/RESULTS.md:10`, `skinny/RESULTS.md:19`).
   - SC-3 repeats the shifted canada/mesh numbers as the throughput basis for the union-substrate diagnosis (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:75`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:82`).
   - SYNTHESIS says the highest-ratio canada/mesh/marine_ik corpora are "exactly the corpora bbnf wins by roughly +50%" (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:97`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:101`), which preserves the sidecar magnitude instead of the stricter canada/mesh sonic-strict magnitudes.

   This is blocking because the V3 fold is internally inconsistent: the corrected SC-4/SC-5/SPEC posture is right, but SC-1/SC-2/SC-3/SYNTHESIS still use shifted values in the central "number-heavy wins / string-heavy loses" argument. That reopens the V2 CH1 defect and lets non-admission `parse_only` guard rows wear stricter evidence than the authoritative table supports.

## Non-Blocking Notes

- SC-4's sonic/simdjson architecture separation is folded. It now cites simdjson as the retained document-wide stage-1 index precedent and limits sonic-rs to same-run strict performance plus local skip-scan/single-pass comparison absent exact upstream source proof (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:123`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:151`).

- `parse_only` non-admission, `gate-json` refusal rules, and `tape_vs_tape` telemetry are correctly folded at the packet level. SPEC adds `S`, bars `K`/`S` from strict SOTA admission, and requires plane/strictness/freshness/measured-validation refusal (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`, `restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`); SC-5 says `parse_only` residuals stay visible but not admission evidence (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:179`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:193`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:240`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:259`).

- The Tier A/B split is substantially folded. SYNTHESIS and SPEC define Tier A as structural-class cursor migration and Tier B as string-boundary / quote-backslash-parity / CostFacts-template work; `tape_vs_tape` is not a W3 production consumer (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:123`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:145`, `restart/skinny/tranches/sk-v8/SPEC.md:423`-`restart/skinny/tranches/sk-v8/SPEC.md:444`).

- The S-P2 two-ACCEPT-cycle guard is folded into HANDOFF. V1 and V2 did not converge, so a future V3 ACCEPT would be only the first ACCEPT cycle after REVISE unless the user pins final (`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:83`; governance source `restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123` and `restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:158`).

- Lock 14 and no-new-directive/BIR/substrate posture are not CH1 blockers in this V3 read. SC-6 removes the `UnionTape` node option and constrains generic code to generated byte sets plus opaque ordinals (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:263`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:287`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:508`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).

## Required Fold Actions

1. Recompute and rewrite every remaining comparator-delta claim in SYNTHESIS, SC-1, SC-2, and SC-3 against the `skinny/RESULTS.md:3` column order. For the examples above: canada is `+27.9%` vs sonic-strict and `+54.6%` vs simdjson DOM; mesh is `+21.4%` vs sonic-strict and `+51.5%` vs simdjson DOM; citm is `+24.6%` vs sonic-strict and `-11.3%` vs simdjson DOM; twitter is `-25.1%` vs sonic-strict and `-35.8%` vs simdjson DOM.

2. Rewrite SC-1's win/loss table and later "same-run strict wins" prose so `parse_only` rows are substrate-guard telemetry, not strict SOTA admission evidence. Positive sonic-strict deltas may remain visible, but the rows remain `K`/future-`S` non-admission while `Strictness=deferred`, `parse_utf8=view-boundary`, and output-plane mismatch persist.

3. Rebuild the SC-2/SC-3/SYNTHESIS substrate-correlation summaries from the corrected strict-sonic values. If the diagnosis still relies on simdjson DOM sidecar values, label those values as sidecar planning signals and keep them out of strict-vs-strict evidence.

4. Preserve the already-correct V3 folds while making these edits: SC-4's sonic/simdjson separation, SC-5's `parse_only` demotion, SPEC's executable gate refusal rules, the Tier A/B split, the Lock 14 opaque-ordinal/fact-id boundary, and the two-ACCEPT-cycle convergence guard.
