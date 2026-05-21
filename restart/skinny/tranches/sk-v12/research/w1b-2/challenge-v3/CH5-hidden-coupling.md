# SK-V12 W1b-2a CH5 V3 - Hidden Coupling

Verdict: ACCEPT WITH REDRESS CHECKLIST.

The CH5 lens correctly observed that the current source tree does not yet have
the lightningcss comparator, independent source-sidecar scanner, fail-closed
fixture checks, or lightningcss artifacts. Those are the W1b-2a redress tasks,
not a remaining plan defect.

Binding redress checklist:

- add the lightningcss dependency and comparator;
- implement source-sidecar facts from original input bytes, not cssparser token
  state;
- enforce the frozen fixture limits fail-closed;
- write `lightningcss-facts.txt` and `lightningcss-strict-equality.txt`;
- add the Criterion row `lightningcss_same_plane_fact_stream`;
- do not use broader CSS normalization evidence for strict fact-stream
  equality.
