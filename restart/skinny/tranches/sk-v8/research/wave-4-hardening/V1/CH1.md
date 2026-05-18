# SK-V8 W4 Hardening V1 CH1

Verdict: REVISE.

Confidence: 86%.

## Findings

1. The selected rows are valid under current `SK-V8-open` direct floors.
   `apache_builds`, `numbers`, and `random` are `N-direct`, use same-run
   `sonic_rs_direct_to_struct` strict anchors, and compute to
   `ceil(sonic / 1.10)` floors of 8048, 7230, and 7401 Mbps. Track 1 clears
   each; Track 2 misses by about 3.2%, 3.8%, and 6.5%.
2. The verification plan under-proves the SPEC Section 7 maintain gate. SPEC
   requires all non-target rows no worse than -2.0% plus existing direct and
   real-typed GO preservation, but `skv8-W4-plan.md` narrows this to
   non-target direct rows and benches only targets plus existing direct GO
   rows.
3. Track 2 parser independence is mostly preserved, but digest-arithmetic
   independence weakens. If hand Track 2 starts using the same
   `JsonDirectDigest::fold_*_scalar` helpers already used by generated Track 1
   sink callbacks, exact Track1 == Track2 parity no longer independently
   validates parent-scalar fingerprint folding.
4. Preblocked-route leakage is avoidable but needs tighter wording. Parent
   scalar folding must be distinguished from REDRESS 66 source-hook receiver
   folding and REDRESS 84 value-byte/control compaction: no generated
   `JsonSink` source hooks, no generated helper shape copied into Track 2, no
   value-byte carry, no cap-16, no string fact/materializer retry, no
   raw-f64/mantissa route.
5. LOC/time looks viable for the source slice. The hand parser change should
   fit well under 300 LOC. The verification slice is the risk, not
   implementation size.

## Required Folds

- Restore SPEC Section 7 maintain wording: all non-target rows, not only
  non-target direct rows, remain no worse than -2.0%; existing direct GO and
  real-typed GO rows maintain GO.
- Add a verification path that can actually prove that gate: either a full
  checked table refresh, or an explicit all-direct plus real-typed GO bench
  matrix with a checked comparison against `SK-V8-open`.
- Add a Track 2 independence backstop: keep a test-only child-digest
  construction path for scalar object/array values, or add focused tests
  proving every scalar parent-fold helper matches
  `fold_child(JsonDirectDigest::{string,number,bool,null})`.
- Add a preblocked-route sentence naming the exact negative surfaces above so
  this cannot be read as REDRESS 66/84 under a smaller name.
