# SK-V8 W4 Hardening V4 CH3

Verdict: ACCEPT.

Confidence: 95%.

## Findings

1. The unchanged V4 input is a valid CH3 target. Section 3Z requires the V4
   challenge before consolidation, and V3 consolidated explicitly asks for an
   unchanged V4 challenge. Current HANDOFF still says W4 is a proposed
   rejection/routing disposition pending hardening convergence, so the absence
   of a pre-existing V4 closure artifact is not a regression defect.
2. The W4 disposition remains fail-closed. REDRESS 93 rejects the hand Track 2
   scalar-parent fold, admits no source patch, adds no Lock 14 allowance, and
   leaves `skinny/RESULTS.md` unchanged. Apache's local pass is not partial
   admission because `random` missed sonic/1.10 and `numbers` regressed by
   +6.3287% Track 2 time.
3. No REDRESS route is reopened. REDRESS 93 explicitly excludes runtime,
   codegen, BIR, directive, substrate, generic crate, generated Track 1,
   string materializer, cap-16, value-byte carry, source-hook, semantic string
   fact, raw-f64, and mantissa movement. That keeps REDRESS 66-69, 72, 80, and
   84 closed; REDRESS 70-71 also remain untouched because W4 does not recast
   digest rows as typed-product proof.
4. Source state matches the rejection. `git status --short` is clean,
   `git diff --exit-code -- skinny/RESULTS.md
   skinny/crates/bbnf-bench/src/direct_struct.rs` is clean, and the rejected
   patch at `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` touches only
   `skinny/crates/bbnf-bench/src/direct_struct.rs`. Current hand Track 2
   object/array paths still fold `self.value()?` child digests; scalar
   parent-fold helpers exist on the generated Track 1 sink path, not as an
   active hand Track 2 W4 admission.
5. No admitted row is silently regressed. `skinny/RESULTS.md` remains W0
   authority: the selected W4 rows stay `N-direct / NO-GO`, existing direct GO
   rows (`citm_catalog`, `marine_ik`, `unicode_basic`) remain recorded as GO,
   and current `real_typed_struct` GO rows (`twitter`, `update_center`,
   `mesh`, `marine_ik`) remain unchanged. With no admitted behavior slice, CH3
   requires clean state and honest routing, not a fresh row-table refresh.
6. Digest evidence remains guard-plane only. SPEC Section 7 requires residual
   direct rows to route without product-plane proof, `RESULTS.md` records
   `direct_to_struct` as digest/gate-only evidence, and REDRESS 93 routes
   remaining direct misses to a later direct-output-contract or control-path
   tranche. Typed product proof remains owned by `real_typed_struct`, not W4
   digest rows.

## Required Folds

None.

Carry-forward: CH3 acceptance does not by itself close W4 or activate W5; V4
consolidation must record the second consecutive qualifying cycle before
HANDOFF can move W4 from pending to closed/routed.
