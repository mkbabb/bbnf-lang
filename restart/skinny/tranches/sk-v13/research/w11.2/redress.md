# SK-V13 W11.2 Redress - Object-Loop Scalar Direct Dispatch

Date: 2026-05-21.
Disposition: REJECTED-MEASURED.
Rejected patch: `/tmp/skv13-waveW11.2-rejected.patch`.

## Outcome

The object-loop scalar dispatch patch was correct on targeted parity tests and
measurably improved `github_events/direct_to_struct`, but it did not admit any
primary row over same-run sonic strict + 1. Per CHALLENGE, the behavior patch
was reverted before commit.

## Measurements

Criterion command:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|github_events)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/update_center/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

| row | Track 1 Mbps | sonic strict Mbps | threshold | result |
|---|---:|---:|---:|---|
| `json/twitter/direct_to_struct/main` | 11842.746 | 15068.981 | 15069.981 | miss |
| `json/github_events/direct_to_struct/main` | 12536.922 | 16296.054 | 16297.054 | miss |
| `json/update_center/direct_to_struct/main` | 8587.486 | 11243.365 | 11244.365 | miss |

Criterion detected a significant `github_events` Track 1 improvement
(`+2.4403%` throughput), but the absolute row remained below the strict
same-plane sonic threshold. Twitter was unchanged; update_center improved only
within the noise threshold.

## Verification

- `cargo test -p bbnf-bench direct_object_scalar_dispatch -- --nocapture`
  passed before revert.
- No `RESULTS.md` or `ROLLING-SOTA-DELTA.md` update was made because no row
  admitted.

## Routed Remainder

Object-loop scalar wrapper removal is not enough to close the object-heavy
direct residuals. Future W11 direct work must route around the object value
dispatch envelope more substantially, such as generated per-shape object member
handling, sink stack specialization, or a different row-family material
differential accepted by CHALLENGE.
