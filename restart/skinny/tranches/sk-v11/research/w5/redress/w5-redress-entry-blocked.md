# SK-V11 W5 Redress Entry Block

Date: 2026-05-20.

Scope: W5 bounded string span and special-byte scan.

Disposition: BLOCKED before implementation dispatch.

## Evidence

W5 completed research, Plan V1, CHALLENGE V1, Plan V2, and CHALLENGE V2. The
entry gate in SPEC Section 9 requires CHALLENGE to select a scalar span shape,
one string/key caller, cap, and at most two target rows before behavior source
redress.

CHALLENGE V2 did not accept the plan:

- CH1 accepted the release-mode opening-quote guard, but kept malformed-input
  parity at REVISE because Plan V2 did not require the CH1 malformed fixture set
  to reject across generated Track 1, independent hand Track 2, `serde_json`,
  and `sonic-rs`.
- CH4 accepted the floor-level probe trigger, but kept cost at REVISE because
  Plan V2 still had no concrete independent Track 2 cost mechanism to close
  `random/direct_to_struct` from 6949 Mbps to the 7878 Mbps floor.

The second point is load-bearing: without a plausible Track 2 mechanism, a
source patch would only manufacture a probe rejection. That is exactly what the
CHALLENGE gate exists to prevent.

## Disposition

No behavior source, generated runtime, SIMD kernel, benchmark body,
`skinny/RESULTS.md`, gate schema, or report schema moved. The rejected-patch
marker is `/tmp/skv11-waveW5-rejected.patch`; it is empty because no source
patch was attempted.

W5 does not admit a span API and does not produce a rejected-but-reusable scalar
proof. W6 may dispatch only through SPEC Section 10's independent segment-plan
entry route: CHALLENGE must name a new source delta beyond the already-consuming
`unescape_string` path.

## Evidence Commands

- `git diff --exit-code -- skinny/RESULTS.md`
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`
