# SK-V8 S-P3 Hardening V1 CH3: Regression

Date: 2026-05-18.
Pass: S-P3 Synthesis-Plan.
Cycle: V1.
Lens: CH3 REGRESSION.

## Scope

This review audits the S-P3 packet for regression against historical REDRESS and pre-blocked routes. The focus is P3-E and the folded live SPEC, with a secondary check that P3-B/P3-F/DISPATCH do not silently reopen a blocked route.

Inputs reviewed: ORCHESTRATOR, PASS-3-SYNTHESIS-PLAN, PASS-ALPHA, SKINNY-TRIUMVIRATE, P3-A through P3-F, live `SPEC.md`, `DISPATCH-PROMPT.md`, `HANDOFF.md`, S-P2 SC-1 through SC-6 plus V7 consolidation, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.

Regression targets: REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, plus historical function-pointer, pair-token, token-width, separator, generic SWAR, capacity prescan, EventCursor/sidecar, raw f64, and orphan primitive routes.

## Verdict

ACCEPT.

Confidence: 95%.

Blockers: none.

Required folds if REVISE: none.

## Findings

No blocking regression found.

The S-P3 challenge contract requires CH3 to verify that P3-E enumerates every REDRESS route that waves must not reopen and that SPEC carries the full pre-block list, including REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, and historical blocked routes (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:122`, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:126`). P3-E does that explicitly: its minimum list names every required REDRESS cluster and historical route (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:12`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:18`), its route ledger expands them route by route (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:55`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:99`), and its per-wave checklist binds W3/W4/W6 to the relevant preblocks (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:127`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:139`).

The live SPEC carries the same ledger as an inherited global block: every wave may reopen a route only with fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance (`restart/skinny/tranches/sk-v8/SPEC.md:727`, `restart/skinny/tranches/sk-v8/SPEC.md:731`). SPEC Section 10 then names the full list: historical REDRESS 16/17/18/25 routes, REDRESS 28+33, 49-55, 59-65 plus 72/83, 66-72 plus 80, 82-84, 88-90, Alpha-E bitmap reserve, and Tier B blocked from W3 Tier A (`restart/skinny/tranches/sk-v8/SPEC.md:746`, `restart/skinny/tranches/sk-v8/SPEC.md:772`). DISPATCH repeats the same authority and blocks the requested route clusters before any conditional wave may run (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:153`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:173`).

## Regression Matrix

| Route family | Historical rejection | P3-E disposition | SPEC/DISPATCH disposition | CH3 result |
|---|---|---|---|---|
| REDRESS 28+33 Class A tiny-string / NEON-TBL parse close | REDRESS records active 16-byte tiny-string dispatch regressing `twitter` about 25%, and later Class A wiring as the wrong parse-G boundary (`skinny/REDRESS.md:324`, `skinny/REDRESS.md:337`, `skinny/REDRESS.md:394`, `skinny/REDRESS.md:410`). | P3-E keeps Class A NEON/TBL tiny-string wiring blocked unless changed framing proves a different current hot boundary with scalar/checkasm and same-wave consumer (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:62`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:136`). | SPEC Section 10 names REDRESS 28+33 as blocked, and W3 Tier A preblocks Tier B/local string families (`restart/skinny/tranches/sk-v8/SPEC.md:750`, `restart/skinny/tranches/sk-v8/SPEC.md:548`, `restart/skinny/tranches/sk-v8/SPEC.md:554`). | PASS. No wave silently revives the Class A close route. |
| REDRESS 50-55 side tables, EventCursor, parser-local cursor, decoded stats, fused materializer | REDRESS rejects parse-time aux side tables, `JsonEventCursor`, parser-local structural cursor, decoded-string stats, and quote-source fused materializer (`skinny/REDRESS.md:715`, `skinny/REDRESS.md:882`). | P3-E globally blocks sidecar producers, parser-owned projection/cursor, retained cursor, aux density table, and sidecar event vector; W3 and W4 carry the split preblocks (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:40`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:44`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:66`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:70`). | W3 entry challenge must prove it is not a renamed REDRESS 50-55 route; W3 exit requires exactly one tape, no old append API, no parser-owned cursor/fact slot, and generated JSON retained parser as consumer (`restart/skinny/tranches/sk-v8/SPEC.md:500`, `restart/skinny/tranches/sk-v8/SPEC.md:546`). W4 blocks the direct stats/materialization half (`restart/skinny/tranches/sk-v8/SPEC.md:611`, `restart/skinny/tranches/sk-v8/SPEC.md:615`). | PASS. No sidecar, EventCursor, aux, parser-owned cursor, or materializer retry is reopened. |
| REDRESS 60-72 retained string/direct materialization families | REDRESS rejects trusted-string boundary collapse, wide retained scans, Unicode validator/classifier retries, object carry, direct source hooks, parser-owned scratch, byte-output unescape, semantic string facts, and hand typed sinks as proof (`skinny/REDRESS.md:1344`, `skinny/REDRESS.md:2059`). | P3-E separates these families into W3 retained-string blocks and W4 direct/product blocks, and bars cap-16 spillover from reopening direct/Track 2 routes (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:71`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:85`). | SPEC Section 10 names REDRESS 59-65/72/83 and 66-72 as blocked, while W4 separately blocks parser-owned scratch, byte-output unescape, semantic string facts, direct source-hook folding, raw f64 shortcut, and digest-as-product proof (`restart/skinny/tranches/sk-v8/SPEC.md:756`, `restart/skinny/tranches/sk-v8/SPEC.md:761`, `restart/skinny/tranches/sk-v8/SPEC.md:611`, `restart/skinny/tranches/sk-v8/SPEC.md:615`). | PASS. W3 stays Tier A; W4 does not disguise direct/materialization repeats as product proof. |
| REDRESS 80 raw f64 / stale mantissa widen | REDRESS records zero f64 fallback pool on `canada`, so the same-wave consumer disappeared and no source patch was attempted (`skinny/REDRESS.md:2215`, `skinny/REDRESS.md:2248`). | P3-E blocks raw `parse::<f64>()`, stale f64 fallback elimination, and table-only mantissa widen unless fresh attribution finds a nonzero fallback pool and strict numeric direct rows pass (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:63`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:123`). | SPEC Section 10 groups REDRESS 80 with the direct/materialization blocks and W4 preblocks raw f64 shortcut plus stale `canada` mantissa widening (`restart/skinny/tranches/sk-v8/SPEC.md:759`, `restart/skinny/tranches/sk-v8/SPEC.md:761`, `restart/skinny/tranches/sk-v8/SPEC.md:611`, `restart/skinny/tranches/sk-v8/SPEC.md:615`). | PASS. Numeric stale-source routes remain blocked. |
| REDRESS 82-84 single-quartet Unicode, StringBlock16, object-pair value-byte carry | REDRESS rejects single-quartet Unicode classifier, generated-retained `StringBlock16` wrapper, and object-pair value-byte control compaction (`skinny/REDRESS.md:2285`, `skinny/REDRESS.md:2395`). | P3-E blocks Unicode escape validator/classifier retries, object key/value-byte carry, and StringBlock16/cap-16 spillover in W3, with no renamed object-control route allowed (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:76`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:84`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:136`). | W3 entry challenge names REDRESS 82-84, and W3 preblocks object-pair value-byte carry, StringBlock16 tiny probe, and single-quartet Unicode classifier (`restart/skinny/tranches/sk-v8/SPEC.md:506`, `restart/skinny/tranches/sk-v8/SPEC.md:553`). | PASS. W3 does not smuggle these local parser kernels. |
| REDRESS 88/89 PMULL and CTZ/bulk production consumers | REDRESS rejects PMULL prefix-XOR as default hot production body and CTZ/bulk production consumer after JSON rows regressed (`skinny/REDRESS.md:2508`, `skinny/REDRESS.md:2585`). | P3-E keeps bitmap/primitive work as reserve research outside W0-W6 by P3-F, blocks default PMULL/CTZ body fills, and requires full primitive plus JSON row gates before any future narrowed route (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:32`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:93`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:99`). | W3 preblocks unconditional PMULL/CTZ, W6 blocks PMULL/CTZ/B6 canary as performance evidence, and SPEC Section 10 names REDRESS 88-90 plus Alpha-E bitmap reserve as not in W0-W6 (`restart/skinny/tranches/sk-v8/SPEC.md:552`, `restart/skinny/tranches/sk-v8/SPEC.md:718`, `restart/skinny/tranches/sk-v8/SPEC.md:770`). | PASS. No PMULL/CTZ production rewire is reopened by W3 or W6. |
| Historical blocked routes | REDRESS history blocks pair-token fusion, function-pointer dispatch, token-width churn, structural-index prepass, separator elision, generic SWAR, capacity prescan, EventCursor/parallel prepasses, raw f64, and orphan/checkasm-only primitives. | P3-E ledger names these at route level and adds global bans on orphan primitives, checkasm-only body fills, and automatic dispatch (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:55`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:61`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:45`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:47`). | SPEC Section 10 preserves REDRESS 16/17/18/25 historical blocks and global bans on orphan primitives, telemetry consumers, Track 1/Track 2 coupling, and automatic implementation dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:733`, `restart/skinny/tranches/sk-v8/SPEC.md:749`). DISPATCH repeats function-pointer, pair-token, 12-byte churn, separator, generic SWAR, capacity prescan, raw f64, EventCursor/parallel prepasses, and orphan primitive admission as always blocked (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:169`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171`). | PASS. No historical route is silently reopened. |

## W3 Smuggling Check

W3 does not smuggle Tier B. P3-A removes Tier B from the implementation shortlist and keeps `tape_vs_tape` plus PMULL/CTZ default production rewires out of W3 (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:30`, `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:35`). P3-B says the W3 lead is Tier A only, not Tier B, not `tape_vs_tape`, and not automatic implementation dispatch (`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:18`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:26`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:42`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:48`). P3-F folds the same boundary into SPEC and DISPATCH: Tier A is structural-class cursor migration inside one retained `Tape`; Tier B string-boundary, quote/backslash/parity, CostFacts-template, `tape_vs_tape`, default PMULL/CTZ, and sidecar/parser-owned cursor routes remain blocked (`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:31`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:36`).

The live SPEC preserves this exactly. W3's lead hypothesis is structural-class cursor migration only and explicitly does not claim string-boundary, quote/backslash/parity, CostFacts-template, non-JSON production, direct/SinkOnly, or path closure (`restart/skinny/tranches/sk-v8/SPEC.md:511`, `restart/skinny/tranches/sk-v8/SPEC.md:523`). W3's same-wave consumer is generated JSON retained Track 1 parsing plus touched/proven-untouched retained view/`ValueRef`; `tape_vs_tape`, direct/SinkOnly rows, `path!`, Track 2, and telemetry-only rows are audit or residual surfaces, not production consumers (`restart/skinny/tranches/sk-v8/SPEC.md:527`, `restart/skinny/tranches/sk-v8/SPEC.md:546`). W3 preblocks Tier B, `tape_vs_tape`, unconditional PMULL/CTZ, sidecar event vectors, aux tables, density caches, retained cursors, parser-owned class/fact slots, second source scans, old offset append, and local string/materialization families (`restart/skinny/tranches/sk-v8/SPEC.md:548`, `restart/skinny/tranches/sk-v8/SPEC.md:554`).

This matches the S-P2 and V7 boundaries. SC-2 defines Tier A as a retained `Tape` positions/classes cursor with no post-build `StructuralIndex` API, sidecar, aux table, density cache, parser-owned cursor, or parallel offset append; it explicitly does not close string-boundary or quote/backslash/parity work (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:287`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:316`). SC-3 requires exactly one retained `Tape`, move-only scan product, no independent generated-parser cursor, and names `tape_vs_tape` as non-consumer telemetry for Tier A (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:407`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:423`). V7 consolidated the same limits: Tier A structural only, Tier B owns string/parity/CostFacts-template, no new directive/BIR/`BackendShape`/`UnionTape`/public substrate/parser-owned cursor/parallel substrate, and `tape_vs_tape` is residual telemetry, not W3 consumer (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:44`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:64`).

## Disposition Of V6/V7 Governance

The packet handles V6/V7 governance correctly. V7 consolidation says V6 was a qualifying ACCEPT cycle, V7 is the second consecutive qualifying ACCEPT cycle, and S-P2 convergence authorizes S-P3 Synthesis-Plan dispatch only (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:13`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:20`). P3-B and P3-F preserve that boundary: S-P3 is planning, no implementation wave or G-Alpha close follows from S-P2, and W0 is the only initial dispatch after G-Alpha (`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:12`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:14`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:48`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:63`). The live SPEC and DISPATCH repeat that no SK-V8 implementation wave dispatches from S-P3 itself and no W3 implementation dispatches from S-P2 or S-P3 alone (`restart/skinny/tranches/sk-v8/SPEC.md:29`, `restart/skinny/tranches/sk-v8/SPEC.md:36`, `restart/skinny/tranches/sk-v8/SPEC.md:774`, `restart/skinny/tranches/sk-v8/SPEC.md:785`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:9`).

## Residual Non-Blocking Risks

1. W3's local entry-gate challenge line names REDRESS 50-55, 60-72, 82-84, 88, and 89, but not REDRESS 28+33 or the historical routes (`restart/skinny/tranches/sk-v8/SPEC.md:500`, `restart/skinny/tranches/sk-v8/SPEC.md:507`). This is not blocking because SPEC Section 10 is inherited by every wave and explicitly names REDRESS 28+33 plus the historical route set (`restart/skinny/tranches/sk-v8/SPEC.md:727`, `restart/skinny/tranches/sk-v8/SPEC.md:772`), while P3-E's W3 checklist separately blocks Class A tiny-string and historical W3 routes (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:131`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:139`). A future W3 plan should still quote Section 10 rather than relying only on the shorter W3 entry line.

2. P3-E contains source citations to the pre-fold SPEC line range for the route list, while the live SPEC now carries that list in Section 10 at later line numbers (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:14`, `restart/skinny/tranches/sk-v8/SPEC.md:727`, `restart/skinny/tranches/sk-v8/SPEC.md:772`). This is citation drift, not a regression, because the folded SPEC and DISPATCH carry the correct content.

3. Bitmap reserve language remains intentionally future-facing. P3-E and SPEC leave a future challenged density-gated primitive route possible, but they keep PMULL/CTZ default production rewires outside W0-W6 and require fresh W0 evidence, scalar/checkasm, same-wave production consumer, full row maintain, REDRESS citation, and challenge acceptance before any future reopen (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:32`, `restart/skinny/tranches/sk-v8/SPEC.md:767`, `restart/skinny/tranches/sk-v8/SPEC.md:770`).

## Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/alpha/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Self-Verdict

CH3 disposition: ACCEPT.

Confidence: 95%.

No required folds.
