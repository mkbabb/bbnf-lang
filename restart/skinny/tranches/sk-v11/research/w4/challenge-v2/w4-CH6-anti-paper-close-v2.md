# SK-V11 W4 CH6 Challenge V2: Anti-Paper-Close

Date: 2026-05-20.
Scope: CH6 re-check against W4 Plan V2 after CHALLENGE V1 revisions.
Owned artifact: `restart/skinny/tranches/sk-v11/research/w4/challenge-v2/w4-CH6-anti-paper-close-v2.md`.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 8.
- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`.
- `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH6-anti-paper-close.md`.

## Verdict

ACCEPT.

Plan V2 still has a strict measured row closure path and a real measured reject
path. It does not create a paper close.

Positive W4 closure remains bounded to exactly
`random/direct_to_struct/main`. Admission requires fresh native measurement
showing generated Track 1 and independent Track 2 both at or above the
`random/direct_to_struct` floor of 7878 Mbps, exact Track 1 vs Track 2 digest
equality, strict same-row serde_json and sonic-rs comparator rejection of
malformed tail fixtures, direct and typed guard compliance, and same-wave
`gate-json`/`report.rs` consumption of W4 provenance.

## V2 Check

### 1. Measured Row Closure Remains Mandatory

SPEC Section 8 requires every selected direct row to meet its Section 0.4 floor
on Track 1 and Track 2, with Track 2 independence, same-output proof, no hidden
carry/sidecar/substrate policy, and Section 0.5 guards. Plan V2 preserves this
by selecting only `random/direct_to_struct/main` and making admission depend on
both tracks meeting 7878 Mbps in a fresh native Criterion capture.

The pre-admission probes in Plan V2 are not close evidence by themselves. They
are a row-admission guard before `RESULTS.md` movement. The close predicate is
still the full measured product row plus gate/report consumption.

### 2. Reject Path Is Explicit

Plan V2 keeps a fail-closed revert protocol. The source, generated runtime,
direct parser, gate/report, `RESULTS.md`, and `REDRESS.md` slice reverts on any
helper-contract violation, malformed-tail parity miss, `random` probe miss,
Criterion row-floor miss, Track 1/Track 2 output mismatch, serde_json or
sonic-rs malformed-tail acceptance, direct or typed guard regression, Track 2
coupling, owner-path or Lock 14 violation, or missing same-wave W4 gate/report
consumption.

On reject, Plan V2 requires the patch to be saved to
`/tmp/skv11-waveW4-rejected.patch` and REDRESS 115 to record measured evidence.
That is a measured rejection route, not deferred success.

### 3. Paper Close Is Still Blocked

Plan V2 does not allow closure by prose, helper shape, probe-only result,
metadata-only production, stale W2/W10 provenance, comparator drift, one-track
floor pass, or future-wave promise. It also carries forward REDRESS 113's
non-JSON block and does not convert a JSON-local `container_tail_next` helper
into Lock 14 grammar-generalization proof.

Same-wave telemetry consumption remains a close predicate: W4 provenance must
use `same_wave_consumer_class=gate_json_direct_contract`,
`wave_id=SK-V11-W4`, `redress_entry=REDRESS-115`, and the W4 direct delta
`direct-dispatch-byteset`, with the selected-row floor consumed by both gate
and report validation.

## Required Predicates

V2 remains CH6-acceptable only if these predicates hold:

1. Selected shape stays P2-D D1 `container_tail_next`.
2. Selected target set stays exactly `random/direct_to_struct/main`.
3. Generated Track 1 and independent Track 2 both clear 7878 Mbps in fresh
   native measurement before any `RESULTS.md` row movement.
4. Malformed container-tail fixtures reject in generated Track 1, hand Track 2,
   serde_json, and sonic-rs.
5. Track 2 does not call generated Track 1, `runtime::generated_json`,
   generated SinkOnly helpers, `container_tail_next_direct`, or any generated
   Track 1 tail symbol.
6. Direct and typed guards required by Plan V2 and SPEC Section 0.5 hold.
7. Gate and report consume the W4 provenance in the same wave.
8. REDRESS 113's non-JSON block is carried forward.

DISPOSITION: ACCEPT
