# SK-V16 S-P0 V1 CH1 - Correctness

Disposition: ACCEPT.

CH1 verifies that the folded S-P0 packet correctly records:

- `PRUNE-BLOCKED`, with no behavior-wave admission.
- 16 locks, 67 Pattern H files, 51 JSON `ADMITTED` rows, and 24 CSS `OPEN`
  rows.
- The exact dirty generated manifest.
- S-P3 report flags as future consumer obligations, not implemented proof.

Required edits after fold: none.
