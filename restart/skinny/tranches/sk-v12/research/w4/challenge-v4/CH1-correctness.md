# SK-V12 W4 CHALLENGE V4 - CH1 Correctness

Verdict: ACCEPT.

No semantic blocker remains. PLAN-V4 selects the `{`, `}`, `;` delimiter set
that matches the current CSS `scan_block` branch surface. The scalar reference
and `byte_class_from_eq_set_64` mask contract make `mask.trailing_zeros()` the
correct first-member cursor advance for full 64-byte windows, with scalar tail
fallback preserving cursor/end semantics.

The required caller parity surface is sufficient for the microbench-only branch:
cursor/end/tail cases, no-hit windows, first-hit lanes, duplicate delimiter set
entries, high-bit bytes, frozen CSS fixture bytes, adversarial seeds, source
immutability, and fallback behavior are all named. Production separation is
also clean: default W4 cannot claim CSS ADMIT, strict equality, production
consumer, report/gate movement, or `RESULTS.md`; a microbench pass routes to a
separately challenged production split.
