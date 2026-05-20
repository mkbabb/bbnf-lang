# SK-V12 S-P1 V1 Hardening Fold Revisions

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: revision ledger for V1 CH1/CH2/CH4/CH6 dispositions.
Output: this file.

## Folded Changes

1. `CH1` / `CH6` self-time evidence:
   - Exported all 34 parse Time Profiler bundles.
   - Recaptured all 48 product Time Profiler rows under
     `/tmp/skv12-p1/direct-xctrace/time-profiler-v2` because the original
     product exports were shallow launch/setup captures.
   - Parsed the exports into
     `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` and
     `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`.
   - Folded exact target-sample coverage and top-family distributions into
     `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`, and
     `p1e-hot-leaf-attribution.md`.

2. `CH2` grammar-neutral naming:
   - Replaced residual JSON-role labels in P1-A with the canonical family set:
     `bounded_plain_string_scan`, `ascii_whitespace_skip`,
     `container_dispatch`, `number_digit_span`, `string_escape_decode`,
     `unicode_escape_hex_decode`, `simd_movemask`, `output_digest_hash`,
     `typed_direct_projection`, `serde_json_oracle_read_parse`, plus
     `memory_copy` / `runtime_support` for generic leaves.
   - Rewrote the P1-B product-family prose from sequence/object language to
     sequence/value and digest/typed projection language.

3. `CH4` replayability:
   - Added `skv12-p1-capture-manifest.md` with host/tool versions, build flags,
     CWD policy, `rc=54` interpretation, PMU aggregate commands, xctrace export
     policy, product-v2 recapture command shape, and the `update_center` /
     `update-center` alias correction.
   - Corrected P1-E PMU aggregate c/B values to the weighted TSV values:
     parse `2.920217`, direct `4.290305`, typed `3.123172`.

4. Mode III boundary:
   - Preserved P1-C's honest `0/17` fresh Mode III call-stack status.
   - Recorded that W0 Criterion Mode III facts are throughput-only diagnostic
     nonproducer evidence; S-P2/S-P3 may not use Mode III symbols as fresh
     SK-V12 hot-leaf authority without a later explicit capture.
