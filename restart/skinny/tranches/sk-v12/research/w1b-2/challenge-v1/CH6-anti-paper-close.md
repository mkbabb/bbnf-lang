# SK-V12 W1b-2 CH6 - Anti-Paper-Close

Verdict: ACCEPT.

No CH6 blocker was found.

Accepted facts:

- Admission math is correctly derived as `track1_mbps > lightningcss_mbps + 1`.
- Equality at exactly `lightningcss_mbps + 1` remains a miss.
- REDRESS 124 evidence requirements include fixture checksum/input bytes, run
  id, host/build fields, Criterion command/root, Track 1/cssparser/lightningcss
  Mbps, threshold, margin, equality status, fact paths, lightningcss version /
  build hash, report path, and gate result.
- RESULTS/REDRESS discipline is honest: RESULTS is untouched for a measured CSS
  miss, and `PASS-MEASURED-BASELINE` routes W3/W4/fixpoint evidence without
  claiming CSS ADMIT.

Non-blocking cleanup:

- Tighten any prose that says "does not beat lightningcss" to the exact bar
  "does not beat `lightningcss_mbps + 1`."
