# SK-V12 W1b-2 CH5 V2 - Hidden Coupling

Verdict: REVISE.

PLAN-V2 has the correct fixture-limited posture, but CH5 requires the redress
contract to make implementation obligations explicit.

Blockers:

- No lightningcss comparator exists yet.
- No independent source-sidecar scanner exists yet.
- Fixture limits are prose, not fail-closed code.
- The current report validator records checksum/bytes but not fixture-limit
  enforcement.

Required revision:

- Make fixture-shape enforcement, source-sidecar implementation, and
  lightningcss fact artifact consumption first-class redress tasks. Do not use
  broader CSS normalization as evidence for strict fact-stream equality.
