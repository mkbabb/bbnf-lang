# CH1 Correctness Challenge - SK-V8 Alpha V2

Date: 2026-05-17.
Lens: CH1 Correctness.

Scope:

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha/`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/`

Overall disposition: ACCEPT.

The V1 CH1 REVISE findings are resolved for the packet as scoped. The final
documents are suitable for G-Alpha review with one correctness boundary:
`G-Alpha closed` authorizes SK-V8 W0 only. W1-W6 remain non-dispatchable until
W0 closes and a post-W0 plan augmentation names exact rows, owner paths, gates,
pre-block citations, and challenge acceptance where required.

## Resolution Matrix

| V1 correctness issue | V2 disposition | Evidence |
|---|---|---|
| Citation adequacy | ACCEPT | `SYNTHESIS.md` binds the packet to `skinny/RESULTS.md`, REDRESS 77-90, the SK-V7 commit chain through `56e66ef5`, alpha A-F, and V1 hardening. Numeric opening-state claims are tied to current RESULTS rows by corpus/workload in `SPEC.md` Section 0.5. Route claims in the synthesis cite REDRESS items and commit SHAs. Pre-block reopening requires fresh W0 evidence, REDRESS citation, and challenge acceptance before any conditional behavior wave can dispatch. |
| Measurable W0 gate | ACCEPT | `SPEC.md` makes W0 create `SK-V8-open`, populate required telemetry for all 38 current main rows, reject placeholder hot leaves and missing artifacts through `gate-json`, reject a malformed sidecar manifest, hold throughput within +/-1.0 percent, and land no parser/scanner/SIMD/asm/codegen/product behavior. `DISPATCH-PROMPT.md` carries the W0 research, plan, redress, test, and gate commands. |
| Outcome enum | ACCEPT | `SPEC.md` Section 0.3 extends the SK-V8 outcome enum to `A`, `C`, `G`, `K`, `L`, and `N-direct`, and requires `gate-json` to reject other outcomes after W0 unless REDRESS and SPEC deliberately amend the enum. |
| Comparator plane wording | ACCEPT | `SPEC.md` Section 0.2 defines same-run strict anchors, same-run flaw probes, and sidecar planning signals. It also states direct and typed rows compare only to sonic-rs strict and serde_json unless W0 or a later telemetry wave adds same-run C++ product-plane evidence. `SYNTHESIS.md` repeats that sidecar comparators are planning signals unless refreshed under same-run rules. |
| Profile-conditional behavior waves | ACCEPT | `SYNTHESIS.md` says parse or direct behavior work is allowed only after W0 evidence names exact owner paths, hot leaves, thresholds, and pre-block differences. `SPEC.md` makes W1 CostFacts binding precede behavior quality claims, then makes W2-W4 conditional on W0/W1 closure plus exact plan updates and, for parse work, challenge proof that the route is not a renamed REDRESS 82, 83, 84, 88, or 89 path. |
| No stale current-hot-leaf claims | ACCEPT | Final docs do not carry Alpha-E's stale "bbnf remains split" current-hot-path claim. They state current hot leaves are placeholders, explicitly unprofiled in W0b, and that twitter fusion is post-W0 planning only, not a W0 prescription. |
| G-Alpha W0-only posture | ACCEPT | `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, and `DISPATCH-PROMPT.md` all state no SK-V8 implementation dispatch before G-Alpha and only W0 dispatch after `G-Alpha closed`. `DISPATCH-PROMPT.md` also says not to dispatch W1-W6 from the prompt alone. |

## Remaining Blockers

None for CH1 correctness under the W0-only G-Alpha scope.

Post-W0 conditional waves still carry required work, but it is correctly routed
out of this packet's dispatch authority: W1-W6 need W0 closure, fresh plans,
exact owner paths, measurable row gates, cited pre-block handling, and challenge
acceptance where required.
