# SK-V11 W5 CH1 Correctness Challenge V2 Re-check

Date: 2026-05-20.

Scope: this re-check covers only whether Plan V2 resolves CH1's
opening-quote guard and malformed-string parity concerns.

Disposition: REVISE.

## Evidence Read

- CH1 V1 required a release-mode opening-delimiter guard before the generated
  direct bounded helper and before the trusted fallback, with the same
  observable guard mirrored independently in hand Track 2.
- CH1 V1 also required malformed string fixtures to prove rejection, not just
  valid digest equality. For invalid strings, serde_json and sonic-rs were
  required as rejection oracles so a shared generated/hand acceptance bug could
  not pass as parity.
- Plan V2 now makes release-mode opening-quote guards mandatory before both the
  generated Track 1 and hand Track 2 helper/fallback paths.
- Plan V2 requires non-quote string positions to reject with stable errors, and
  its reject protocol says opening-quote guard or malformed-string parity
  failure rejects W5.

## Assessment

Opening-quote guard: ACCEPT. Plan V2 directly closes the CH1 guard gap. It
requires `bytes.get(*cursor) == Some(&b'"')` in release mode, returns
`ExpectedValue` at the current cursor on failure, prevents entry into both the
bounded helper and trusted fallback on non-quote input, and requires hand Track
2 to implement its own guard without generated Track 1 symbols.

Malformed-string parity: REVISE. Plan V2 improves the previous plan by naming
malformed-string parity as a reject condition, but the pre-measurement test
obligations are still weaker than CH1 required. The V2 test list requires
malformed object key/value and array string positions to avoid release-mode
panic, but it does not explicitly require those malformed inputs to reject
across generated Track 1, independent hand Track 2, serde_json, and sonic-rs.
It also does not carry forward CH1's named malformed fixture classes as oracle
tests, including non-quote object keys, invalid escapes, invalid unicode
escapes, invalid surrogate pairs, control-before-close, unterminated strings,
and short-tail cases.

The same-run sonic-rs and serde_json comparator rows in Plan V2 are useful for
valid-output comparison, but they are not an explicit invalid-input rejection
oracle. CH1 required product parity tests to distinguish valid-output equality
from malformed-input rejection.

## Required Revision

Plan V2 can return to CH1 ACCEPT after it adds a pre-measurement malformed
fixture requirement stating that generated Track 1, independent hand Track 2,
serde_json, and sonic-rs all reject the CH1 malformed string/key/value/array
cases, with generated Track 1 preserving the intended stable error for
non-quote string positions.

The opening-quote guard language should remain as written.

DISPOSITION: REVISE
