# SK-V8 W1-E: REDRESS/pre-block and rollback ledger

Date: 2026-05-18.
Scope: W1 CostFacts/strict-comparator gate binding only; identify routes W1 must not reopen, evidence W1 may record without making a performance claim, and the rollback/REDRESS protocol for missing or non-neutral evidence.
Output: this file.

## §1 - Findings (concrete, file:line cited)

1. W1 is a triumvirate research/plan/redress wave, not an implementation free-for-all. Research is read-only; the plan must name the falsifiability gate, revert protocol, same-wave consumer, and pre-blocked routes; redress either commits source plus evidence or reverts and records REDRESS (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:41`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:65`). The contract also says no wave ships without a falsifiability gate or revert protocol (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:202`).

2. W0 is closed and W1 is dispatchable, but only under the W1 entry gate. V12 states "W0 is closed" and dispatches W1 under SPEC Section 4 (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:19`, `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:82`). The W0 residual routed to W1 is replacing `none:pre-W1` CostFacts sentinels before behavior waves may cite route quality (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:67`). The handoff repeats that W1 must keep generated JSON output and parser behavior unchanged, and must make `gate-json --with-cost-facts` the same-wave consumer that rejects missing evidence after W1 (`restart/skinny/tranches/sk-v8/HANDOFF.md:174`).

3. SPEC Section 4 defines W1's allowed work as gate/report binding: CostFacts rule id, chosen shape, rejected alternative ids, evidence source, wave id, REDRESS reference, comparator id, plane, strictness, freshness, and measured-validation path (`restart/skinny/tranches/sk-v8/SPEC.md:396`). The exit gate requires missing CostFacts evidence to reject, strict admission to fail closed on comparator mismatch, generic CostFacts paths to stay JSON-policy-free, non-JSON proof to pass, and the full table to maintain within +/-1.0% of `SK-V8-open` (`restart/skinny/tranches/sk-v8/SPEC.md:407`). The same-wave consumers are `gate-json --with-cost-facts` and the strict-admission gate, not a parser hot path (`restart/skinny/tranches/sk-v8/SPEC.md:418`).

4. SPEC Section 4 explicitly pre-blocks W1 from behavior changes, CostFacts-as-performance claims, global route-fact policies that ignore rejected alternatives, generic JSON policy, generated output drift, and producer-only CostFacts/telemetry (`restart/skinny/tranches/sk-v8/SPEC.md:421`). Its rollback rule is already narrow: revert CostFacts/report/gate changes together, keep read-only audit evidence in the research artifact, and add REDRESS naming the missing or non-neutral fact class (`restart/skinny/tranches/sk-v8/SPEC.md:425`). A W1 rejection blocks W2-W6 behavior waves (`restart/skinny/tranches/sk-v8/SPEC.md:429`).

5. The inherited route ledger says a route may reopen only with fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance (`restart/skinny/tranches/sk-v8/SPEC.md:756`). W1 therefore cannot reopen the global blocks: new directive/BIR/substrate/API/`BackendShape`/`UnionTape`, generic JSON policy, sidecar/permissive/stale comparator strict admission, telemetry-only consumers, orphan primitives, Track 1/Track 2 coupling, benchmark-private parsers, or automatic implementation dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:762`). Section 10 also says REDRESS 74-79, 81, and 87 may be cited only under their admitted boundaries and do not authorize behavior by analogy (`restart/skinny/tranches/sk-v8/SPEC.md:791`).

6. P3-E gives W1 only route-fact authority. The W1 ownership row allows CostFacts binding, rejected-alternative accounting, and zero behavior drift; it forbids using CostFacts as a performance result or reopening REDRESS 50-72/28+33/83/84 by bookkeeping alone (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:26`). P3-E's no-deferral rule says missing owning evidence excludes the route, and a tried route that misses its gate must be reverted with REDRESS in the same wave (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:51`). Its W1 checklist names the same explicit pre-blocks: behavior changes, CostFacts-as-performance, global policies ignoring rejected alternatives, and generated-output drift unless the wave is split (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:133`).

7. REDRESS 72 is a plane-specific cap-16 admit, not a global string policy. It admits 16-byte tiny probes only for generated retained `OffsetTape` parsing, while hand Track 2 and generated direct `SinkOnly` cap-16 attempts regressed and were restored to cap 8 (`skinny/REDRESS.md:1996`, `skinny/REDRESS.md:2045`). W1 may record this split as CostFacts evidence, but must not generalize it across planes.

8. REDRESS 74-76 are research/spec/comparator lessons, not behavior admits. asmjson is a reference architecture, not a new directive or permissive strict anchor; transferable shapes require CostFacts, emitted tables, admitted primitives, same-plane strictness, and same-wave consumers (`skinny/REDRESS.md:2092`). The same pass marks lossy/permissive comparator rows as flaw probes unless same-hardware same-output strict rows exist, and keeps architecture-feature work unadmitted until exact profiles point there (`skinny/REDRESS.md:2107`, `skinny/REDRESS.md:2116`).

9. REDRESS 77-79 admit reporting/neutralization only. The sonic-rs strict feature repair fixed a comparator flaw but did not reclassify parse rows or open W1 from throughput (`skinny/REDRESS.md:2130`). Schema-v3 telemetry is explicit provenance, and its missing SK-V6 delta is an honest reporting limitation, not a performance result (`skinny/REDRESS.md:2152`). TapeKind rename produced no `RESULTS.md` or generated-output diff and did not reopen REDRESS 28+33, 50-55, or 60-72 (`skinny/REDRESS.md:2187`, `skinny/REDRESS.md:2208`).

10. REDRESS 81 admits generated real typed Vec expansion only for explicit host/API schema consumers. It does not reopen retained-parse materializers, benchmark-private hand typed sinks, or capacity prescan routes (`skinny/REDRESS.md:2252`, `skinny/REDRESS.md:2281`).

11. REDRESS 87 is the direct W1 precedent: CostFacts is an evidence substrate. It records chosen lowerers, rejected alternatives, evidence sources, and diagnostics, but generated JSON outputs and `RESULTS.md` stayed unchanged, and it did not reopen REDRESS 50-72, REDRESS 28+33, W5 StringBlock16, W6 value-byte compaction, or any pre-blocked route (`skinny/REDRESS.md:2468`, `skinny/REDRESS.md:2487`, `skinny/REDRESS.md:2502`). Alpha-C states the same admitted boundary: CostFacts records choices and rejected alternatives, but does not authorize retrying rejected hot-path routes without fresh evidence (`restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md:75`).

12. REDRESS 88-90 block primitive/body-fill shortcuts. PMULL default prefix-XOR and CTZ/bulk production consumption were rejected despite correctness, checkasm, and asm proof because JSON rows regressed (`skinny/REDRESS.md:2510`, `skinny/REDRESS.md:2544`). B6 Stage 1 admits only checkasm hardening with zero production/runtime/generated/RESULTS diff; PMULL and CTZ remain rejected (`skinny/REDRESS.md:2589`). Alpha-C repeats that primitive body admission still needs scalar reference, checkasm, same-wave production consumer, asm proof, and row-level gate (`restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md:107`).

## §2 - Recommendations (named falsifiability gates)

1. Gate: `W1-costfacts-completeness`. `gate-json --with-cost-facts` must fail closed unless every materialized JSON rule has a CostFacts rule id, chosen shape, rejected alternative ids, evidence source, REDRESS reference, and W1 wave id. Acceptable evidence is provenance/accounting: W0 row/run/profile identifiers, REDRESS-backed prior measurements, explicit rejected alternatives, and comparator metadata. It must not create or cite a throughput win.

2. Gate: `W1-no-behavior-drift`. W1 must prove generated JSON output, parser behavior, product behavior, and `RESULTS.md` throughput cells remain unchanged except for W1-approved report/gate metadata. Use the SPEC Section 4 full-table +/-1.0% maintain rule as a report/gate guard, not as an improvement claim.

3. Gate: `W1-plane-neutrality`. Every CostFacts decision must be plane-specific. REDRESS 72 can support generated retained `OffsetTape` cap 16 and record rejected direct/Track 2 alternatives; it cannot become a global cap-16 policy. REDRESS 81 can support host/API typed product-plane evidence; it cannot justify digest, retained parse, hand typed, or capacity-prescan routes.

4. Gate: `W1-strict-admission-fail-closed`. Comparator evidence must record id, plane, strictness, freshness, and measured-validation path. Stale sidecars, lossy sonic rows, permissive asmjson rows, plane mismatches, deferred validation, or missing measured path are planning evidence only and must reject strict admission.

5. Gate: `W1-redress-boundary-audit`. The W1 plan should list every inherited block it is not reopening: behavior changes; CostFacts-as-performance; global route policy; generic JSON policy; generated drift; producer-only telemetry; REDRESS 28+33; REDRESS 50-72; REDRESS 74-79; REDRESS 81; REDRESS 87; REDRESS 88-90; Alpha-E bitmap/body-fill reserve research. This is a gate input for CH3/CH6 review.

## §3 - Risks (REDRESS entries to pre-block)

| Risk / route W1 must not reopen | Pre-blocked treatment |
|---|---|
| Behavior changes in parser/runtime/codegen/product planes | Out of W1 unless split into a separately challenged behavior consumer; W1 same-wave consumer is gate/report only. |
| CostFacts-as-performance | CostFacts may record evidence, choice, alternatives, REDRESS provenance, and diagnostics; it may not classify a row as faster or reopen a rejected route. |
| REDRESS 72 cap-16 globalization | Record generated retained cap 16 plus direct/Track 2 rejections; do not make cap 16 a generic or cross-plane default. |
| REDRESS 74 asmjson/DAV1D analogy | Cite only as architecture/process evidence; no new directive, permissive anchor, CollapsedStage shortcut, `UnionTape`, or substrate expansion. |
| REDRESS 75/77 comparator shortcuts | Strict admission requires same-run, same-output-plane, strict evidence; lossy/permissive/stale sidecars remain flaw probes. |
| REDRESS 76 architecture features | PMULL/CSSC/DotProd/SVE/SME/AVX-512 remain unadmitted absent exact profiles and later primitive gates. |
| REDRESS 78/79 telemetry/rename admits | Schema-v3 and TapeKind rename are provenance/neutrality evidence only; no parser/runtime performance claim. |
| REDRESS 81 typed Vec admit | Host/API typed product-plane evidence only; no hand typed sinks, digest proof, capacity prescan, or retained/direct route reopen. |
| REDRESS 87 CostFacts precedent | Evidence substrate only; no automatic reopen of REDRESS 28+33, 50-72, 83, 84, or any behavior route. |
| REDRESS 88-90 primitive/canary family | PMULL and CTZ production body fills remain rejected; B6 canary is test-harness hardening only. |
| Generic JSON policy leakage | No JSON policy in generic CostFacts paths, including renamed helpers. |
| Producer-only CostFacts/telemetry | Reject if the report emits fields that `gate-json --with-cost-facts` and strict admission do not consume. |

Rollback/REDRESS protocol for W1:

1. If required evidence is missing before implementation, W1 plan should mark the candidate not dispatchable and keep the route blocked; do not synthesize CostFacts defaults.
2. If redress starts and any evidence is missing, stale, plane-mismatched, JSON-policy-bearing, behavior-changing, generated-output-drifting, or non-neutral, revert the CostFacts/report/gate slice together.
3. Preserve the failed patch at `/tmp/skv8-wave1-rejected.patch` when source changes were attempted; use an empty patch only if no source edit was attempted after evidence disappeared.
4. Keep read-only audit evidence in this research file and the W1 plan; add a new `skinny/REDRESS.md` entry that states `SK-V8 W1 CostFacts Gate Binding is REJECTED`, names the missing or non-neutral fact class, lists the failed command/gate output, records generated/RESULTS/behavior diff status, and states that W2-W6 remain blocked.
5. Do not route a failed W1 into a partial admit. If CostFacts evidence is incomplete but comparator binding passes, the wave still rejects unless the plan was explicitly split before redress.

## §4 - Sources (every external citation)

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`
- `skinny/REDRESS.md`
