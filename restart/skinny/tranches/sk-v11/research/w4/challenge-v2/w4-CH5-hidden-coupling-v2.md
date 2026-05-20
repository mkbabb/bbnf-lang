# SK-V11 W4 CH5 Hidden Coupling V2

Date: 2026-05-20.

Scope: CH5 challenge-v2 review of
`restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`
against the CH5 v1 blockers in
`restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH5-hidden-coupling.md`.

Output: this file only.

Disposition: **ACCEPT for redress**.

## Verdict

W4 plan V2 resolves the CH5 hidden-coupling blockers sufficiently to enter
redress. It now binds a selected-row W4 floor authority, requires stale
direct-provenance rejection, closes the 7734-7877 Mbps false-accept band,
keeps unselected W4 candidates clamped, requires direct guard negative tests,
and makes malformed-input rejection proof separate from valid-row digest parity.

This is redress authorization, not closure. Redress must still implement the
source, gate/report, and test changes and must revert under the V2 protocol if
any required negative test, floor, comparator, provenance, or guard check fails.

## Materials Read

- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`
- `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH5-hidden-coupling.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`

## CH5 V1 Blocker Resolution

| Blocker | V2 resolution | Disposition |
|---|---|---|
| Shared floor authority | V2 requires one selected-row floor authority for `random/direct_to_struct` at 7878 Mbps, consumed by both producer and validator, and requires the 7734-7877 Mbps band to fail both gate and report validation. | ACCEPT |
| Generated helper leakage | V2 keeps Track 1 and Track 2 helpers as separate source implementations and forbids Track 2 from calling `runtime::generated_json`, generated SinkOnly helpers, `container_tail_next_direct`, or any generated Track 1 tail symbol. It also requires source-level leakage assertions in the W4 test suite. | ACCEPT |
| Malformed fixture proof | V2 adds generated Track 1 and hand Track 2 malformed-tail fixtures for object and array tails, requires bad-byte and EOF offset/kind assertions, and requires generated Track 1, hand Track 2, serde_json, and sonic-rs to reject malformed container-tail fixtures. | ACCEPT |
| Stale provenance | V2 requires rejection of W4 candidate rows carrying `SK-V10-W2`, `SK-V10-W10`, `REDRESS-101`, `REDRESS-109`, `direct-reclaimed`, or `direct-residual`, even with passing Mbps. | ACCEPT |
| False-accept band | V2 explicitly tests rejection of `random` in the stale-floor band above 7734 Mbps and below the SK-V11 W4 floor of 7878 Mbps. | ACCEPT |
| Unselected clamp | V2 requires tests that reject unselected W4 candidates above their floor. This preserves W0 clamp behavior outside the selected `random/direct_to_struct/main` row. | ACCEPT |
| Guard negative tests | V2 requires direct guard floor-miss tests for `citm_catalog`, `apache_builds`, `marine_ik`, and `unicode_basic`, and requires typed guards to be measured and hold. | ACCEPT |

## Redress Notes

Two CH5 v1 details should be carried into implementation without requiring
another plan revision:

1. The source-level generated-helper leakage assertion should also prove the
   generated tail helper is not exported or re-exported. V2's helper contract
   uses a private `fn`, so this is an implementation detail of the required
   leakage test, not a blocker.
2. V2 lists `{}`, `{ }`, `[]`, and `[ ]` beside malformed-tail fixtures. Those
   are valid empty-container boundary fixtures and must be accepted by all four
   parsers; they should not be placed in the malformed rejection table.

## Redress Gate

CH5 authorizes W4 redress only if the implementation preserves these V2
constraints:

- exactly `random/direct_to_struct/main` is the selected target;
- gate and report use the same W4 selected-row floor authority;
- Track 2 remains source-independent from generated Track 1 and generated tail
  helpers;
- malformed rejection tests are separate from valid digest parity;
- stale direct-contract provenance, false-accept-band rows, unselected
  candidates, and direct guard floor misses fail closed;
- comparator evidence remains same-run strict direct digest evidence from
  serde_json and sonic-rs.

DISPOSITION: ACCEPT for redress.
