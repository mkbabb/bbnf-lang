# CH4 Cost Challenge - SK-V8 Alpha V1

Date: 2026-05-17.
Lens: CH4 Cost.
Scope: PASS-ALPHA plus alpha-A through alpha-F for the SK-V7 -> SK-V8
Pass Alpha packet.

## Overall Disposition

REVISE.

The alpha packet is cost-aware enough to continue hardening, but it is not
ready for G-Alpha as written. Alpha-A through alpha-D are acceptable as evidence
inputs. Alpha-E and alpha-F need cost fixes before the final SK-V8 contract is
dispatchable:

- LOC budgets exist for alpha-E candidates, but alpha-F waves do not carry
  per-wave source LOC caps.
- The wave hard caps are ambiguous because alpha-F lists both per-wave caps and
  phase caps without saying whether verification is inside the cap.
- W4 CostFacts gate integration lands too late in alpha-F to govern earlier
  W1-W3 implementation decisions.
- W2 and W3 are placeholders with broad owner-path envelopes. They are too
  broad to dispatch until W0 profiles select exact rows, owners, and consumers.
- The bitmap asm candidate is acceptable only as a conditional reserve after
  fresh density and hot-leaf evidence. It is rejected as a default SK-V8
  implementation candidate.

## Artifact Dispositions

| Artifact | Disposition | Cost finding | Required fix |
|---|---|---|---|
| PASS-ALPHA | ACCEPT | The cost challenge criteria are explicit: LOC budget, risk, wave alignment, same-wave consumer, hard caps, and pre-blocked routes. | No fix. Use PASS-ALPHA section 4.4 as the final packet checklist. |
| alpha-A results extraction | ACCEPT | It does not prescribe source work and correctly records missing c/B, hot-leaf, and previous-SK delta data. | No fix. Keep the missing telemetry caveats as W0 entry material. |
| alpha-B competitor deltas | ACCEPT | It avoids converting stale or sparse sidecar values into same-run strict anchors. | No fix. Keep sidecar completion as telemetry work, not performance work. |
| alpha-C redress digest | ACCEPT | The pre-block list prevents expensive retry loops on rejected SK-V7 routes. | No fix. Carry the reopen conditions verbatim into final HANDOFF and SPEC. |
| alpha-D validated/invalidated ledger | ACCEPT | It demotes throughput-neutral wins and rejected primitive bodies, which keeps implementation cost from drifting toward invalidated routes. | No fix. Use the demotions to block false cost savings from "already admitted" claims. |
| alpha-E candidate shortlist | REVISE | Candidate budgets are present, but candidate 1 is too broad at 900 source LOC, candidate 2 overlaps alpha-F W0/W4, candidate 3 can create low-value churn, and candidate 4 is too costly/risky as a default implementation wave. | Apply the candidate fixes below. |
| alpha-F contract draft | REVISE | The wave structure is directionally right but not cost-closed: W0 is broad, W1 lacks an alpha-E candidate binding, W2/W3 are unselected placeholders, W4 comes too late, and W5/W6 can be merged unless a source fix is found. | Apply the wave and hard-cap fixes below. |

## Alpha-E Candidate Dispositions

| Candidate | Disposition | Cost challenge | Concrete fix |
|---|---|---|---|
| 1. Twitter yyjson residual fusion-quality retained parser refactor | REVISE | The same-wave consumer is present, but the owner paths span runtime parser, scanner, codegen templates, bench gate, and RESULTS. A 900 source LOC cap permits a broad parser rewrite before W0 proves the hot owner. | After W0, select one exact dominant caller or driver. Cap the first implementation attempt at 450 source LOC, or 650 source LOC only if template parity is required. Do not touch scanner, parser, templates, and gate in one wave unless W0 names the exact crossing point and the final SPEC lists those paths. |
| 2. RESULTS schema completion and sidecar freshness gate | REVISE | The concept is necessary, but it overlaps alpha-F W0 telemetry lock and W4 CostFacts gate integration. Verification cost is not capped, especially for sidecar manifests and hot-leaf coverage. | Fold schema/hot-leaf/delta requirements into W0. Move CostFacts evidence binding before implementation waves. Cap telemetry source changes at 350 LOC in W0 and 300 LOC in the CostFacts gate wave. Sidecar freshness may cover populated cells first; missing sidecars must be explicit non-admission reasons, not blockers for all SK-V8 work. |
| 3. Remaining Lock 14 template-residue boundary audit and relocation | REVISE | The audit is valuable, but the 500 LOC/file-move allowance invites broad churn for a boundary that alpha-C/alpha-D say is mostly neutralized. | Make this docs/audit-only by default. Source LOC cap is 0 unless a grep gate finds a production generic JSON-policy hit. If a hit exists, cap the fix at 150 source LOC and require byte-identical generated JSON output. Larger relocation routes to Pass Omega or a separate challenge-approved wave. |
| 4. Bitmap asm bodies under changed density-gated measurement framing | REJECT as default; REVISE as reserve | W10 and W10b already proved correctness can lose whole-report throughput. The proposed 500 LOC implementation plus checkasm, simd_scan, full RESULTS, density predicate, and falsifier guards is a high verification-cost path. | Remove from the default SK-V8 wave manifest. It may return only if W0 hot-leaf data names bitmap prefix/next-bit or bulk emit as at least 10 percent self-time on a target row and a no-runtime simd_scan density probe shows one selected-density win >= 5 percent plus one rejected-density fallback. Runtime wiring still needs same-wave consumer and full no-regression gates. |

## Alpha-F Wave Structure Challenge

| Wave | Disposition | Cost challenge | Concrete fix |
|---|---|---|---|
| W0 Baseline Profile And Telemetry Lock | REVISE | It combines all-row hot leaves, profile paths, c/B, SK-V7 deltas, sidecar provenance, schema changes, and gate rejection in 180 min. That is too broad unless sidecar freshness is scoped. | Set total hard cap to 240 min including verification. Source LOC cap: 350. Required coverage: all current RESULTS rows get hot leaf, profile artifact, run id, cycles-per-byte or equivalent sample cost, and delta vs SK-V7-open. Sidecar provenance covers populated cells first; absent comparators become explicit "missing sidecar" reasons. No runtime/parser behavior changes. |
| W1 Typed Product Plane Expansion | REVISE | Alpha-F makes this an implementation wave, but alpha-E did not shortlist a typed product candidate with selected rows, LOC budget, or exact same-wave consumer details. | Either add a costed alpha-E candidate before G-Alpha or remove W1 from the default manifest. If retained: choose exactly two new typed rows, cap source LOC at 650 including templates and benches, require generated Track 1 plus independent Track 2/oracle, and keep the four existing real_typed_struct GO rows green. |
| W2 Parse Candidate From Fresh Profiles | REVISE | The wave is intentionally selected after W0, but final SPEC cannot dispatch a wave whose owner paths are "likely under" several crates. | Keep W2 as a reserved slot only until W0 selects one exact parse intervention. Final SPEC must name owner paths, target rows, hot leaf, same-wave consumer, source LOC cap, and verification commands. Default cap: 450 source LOC, or 650 if codegen template parity is required. Candidate 4 bitmap work is not eligible unless the reserve trigger above fires. |
| W3 Direct Guard Triage | REVISE | It is neither clearly telemetry-only nor clearly implementation. Direct digest rows are guards, not product-plane SOTA, so implementation cost can sprawl without changing the product close. | Make W3 profile/triage-only by default with 0 runtime LOC and a 180 min cap, or select at most two digest rows after W0 with a 300 source LOC cap and explicit non-product wording. Any output semantics change must route to typed product work, not digest-guard closure. |
| W4 CostFacts Gate Integration | REVISE | Correct work, wrong position. Landing it after W1-W3 means the gate cannot control the earlier cost decisions it is meant to bind. | Move CostFacts gate integration immediately after W0 and before any implementation wave. Source LOC cap: 300. Hard cap: 180 min. Gate must consume CostFacts evidence and rejected alternatives for selected interventions before source redress begins. |
| W5 Grammar-Neutral Audit And Lock 14 Preservation | ACCEPT with narrowing | Useful as a final audit, but it should not be the only place Lock 14 is checked. | Run the grep gate at every implementation wave exit. Keep W5 docs/audit-only unless it finds a production generic JSON-policy hit. Source LOC cap: 0 by default, 150 if a concrete hit is fixed. |
| W6 Close, Redress Reconciliation, And Alpha Feedback | ACCEPT with merge option | The close wave is cheap and necessary, but it can be folded into W5 if W5 remains docs/audit-only. | Merge W5 and W6 into one close wave unless W5 discovers source drift. Hard cap: 180 min. Source LOC cap: 0 except REDRESS/HANDOFF/SPEC updates. |

## Required Revised Wave Order

The final packet should not dispatch alpha-F's current order unchanged. Use this
order unless CHALLENGE consolidated gives a stronger reason:

| Order | Wave | Cost reason |
|---:|---|---|
| 0 | Opening profile and telemetry lock | Makes row diagnosis executable before implementation. |
| 1 | CostFacts gate and candidate selection | CostFacts must govern route choice, not document it afterward. |
| 2 | Typed product expansion, if alpha-E adds selected rows | Product-plane wins are validated, but only if rows and schemas are named. |
| 3 | Single parse intervention from W0 evidence | Prevents a broad fusion rewrite from becoming a catch-all parser wave. |
| 4 | Direct guard triage or two-row direct experiment | Keeps digest work from masquerading as product SOTA. |
| 5 | Lock 14 audit, REDRESS reconciliation, and close | Cheap close work; split only if source drift is found. |

## Hard Cap And Verification Fixes

The final SPEC must define hard caps as total wave budgets, including research,
plan, implementation, verification, RESULTS refresh, REDRESS, and docs. If the
packet keeps separate phase caps, the sum must not exceed the wave hard cap.

Minimum final budget table:

| Wave class | Source LOC cap | Total hard cap | Verification allowance |
|---|---:|---:|---|
| Telemetry/gate-only | 300-350 | 180-240 min | one schema test suite, one gate-json run, one RESULTS refresh, one malformed-manifest negative test |
| Typed product implementation | 650 | 300 min | focused correctness, generated-output diff, Track 1/Track 2 parity, one full json_parity/gate refresh |
| Parse implementation | 450 default, 650 with template parity | 300 min | focused correctness, one profile before/after, one full json_parity/gate refresh, guard-row comparison |
| Direct digest experiment | 300 | 240 min | focused direct rows, Track 2/oracle parity, one full gate refresh |
| Docs/audit/close | 0 source LOC by default | 120-180 min | grep gates, generated-output byte identity if relevant, git diff --check |

Extra reruns are allowed only to diagnose a failed gate. They must be recorded
as verification cost. If a wave needs a second full bench rerun after a failed
implementation attempt, the default disposition should become REJECT with
REDRESS unless the SPEC pre-authorizes the extra run.

## Same-Wave Consumer Findings

ACCEPT:

- Alpha-E candidate 2 has a valid consumer: `gate-json` consumes the schema it
  emits.
- Alpha-E candidate 3 is audit-only when no source fix is found; same-wave
  consumer is not applicable.
- Alpha-E candidate 4 names a density-gated scan consumer, but only for reserve
  status after W0 evidence.

REVISE:

- Candidate 1 must name the exact retained parse caller after W0. "Retained JSON
  parse for twitter" is a direction, not enough for a final source wave.
- Alpha-F W1 must name the generated typed consumers and independent Track 2 or
  oracle for the selected rows.
- Alpha-F W2 must not create helper code before the consuming hot path is
  listed in the SPEC.
- Alpha-F W3 must state whether the consumer is digest guard only or real typed
  output. It cannot use digest closure as product-plane proof.
- Any primitive or asm body without a same-wave production consumer is rejected
  by default, even if scalar reference and checkasm pass.

## Final Required Fix List

1. Add per-wave source LOC caps to alpha-F's final SPEC.
2. Define each hard cap as total wall-clock/task budget including verification.
3. Move CostFacts gate integration before implementation waves.
4. Remove bitmap asm from the default SK-V8 implementation manifest; keep only
   as a reserve candidate with the W0 trigger above.
5. Add a costed typed product alpha-E candidate or remove alpha-F W1.
6. Convert W2 and W3 from broad placeholders into exact post-W0 selected waves
   before G-Alpha, or label them reserved and non-dispatchable.
7. Make Lock 14 audit cheap by default and run its grep gates at every
   implementation wave exit.
8. Merge W5/W6 close work unless source drift requires a separate fix wave.
9. Record verification commands and maximum full-bench reruns per wave.
10. Keep generated output and RESULTS refreshes outside source LOC caps, but
    require byte-diff audits and explicit accounting for their review cost.

With those fixes, the cost lens would move alpha-E and alpha-F from REVISE to
ACCEPT. Without them, G-Alpha should not close because the current packet can
still authorize broad, high-verification-cost waves whose consumers and caps are
not final.
