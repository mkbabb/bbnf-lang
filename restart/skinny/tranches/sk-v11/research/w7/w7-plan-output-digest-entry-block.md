# SK-V11 W7 Plan - Output Digest/Hash Host-Sink Entry Block

Date: 2026-05-20.

Phase: W7 plan synthesis.

Disposition: PROPOSED BLOCK before source redress.

## Selected Intervention

No behavior intervention is selected. `G-W7-DIGEST-SINK` cannot be made
measurable under SPEC Section 11 with the W7 research packet as written.

SPEC Section 11 permits only C8 output digest/hash oracle or per-product host
sink work. The admissible source seam is narrower after W6:

- `JsonDigestSink::*_source` decoded-byte folding is pre-blocked by REDRESS 117
  as a REDRESS 54 replay with REDRESS 55/66/69 adjacency.
- Generic parser digest/hash semantics remain pre-blocked.
- Cache-hint or prefetch-only changes cannot admit W7.
- The non-JSON host-sink route has no generated non-JSON Track 1 baseline after
  REDRESS 113 / W1b.

The remaining JSON route would have to specialize only the existing output
digest sink below the decoded `&str` boundary, centered on `hash_bytes` /
`mix`, and would still need a fresh post-W6 profile naming
`output_digest_hash` as limiting for a bounded selected product-row subset.
The available W7 evidence does not pass that entry gate.

## Entry-Gate Evidence

W3-W6 all have dispositions: REDRESS 114, 115, 116, and 117. W3 and W4
rejected measured source patches; W5 and W6 blocked before source redress. No
hot-path behavior source changed after the SK-V11 S-P1 profile, so that profile
is behavior-equivalent for research triage, but it is not the fresh post-W6
profile named by SPEC Section 11.

Even granting the S-P1 profile as triage evidence, the candidate row set is not
admissible:

| Row | W0 Track 1 | W0 Track 2 | Floor | Visible digest bucket | W7 eligibility |
|---|---:|---:|---:|---:|---|
| `apache_builds` | 9305 | 9477 | 9373 | 25.960% T1 / 15.180% T2 | guard row already direct A/GO; not a residual admission target |
| `distinct_values` | 1750 | 1625 | 2658 | 16.400% T1 / 6.091% T2 | floor gap too large; perfect visible-bucket removal still misses |
| `update_center` | 8187 | 7474 | 10059 | 12.236% T1 / 9.398% T2 | floor gap too large; perfect visible-bucket removal still misses |
| `random` | 7693 | 6949 | 7878 | 11.684% T1 / 8.452% T2 | Track 1 is near, Track 2 still misses under perfect visible-bucket removal |
| `github_events` | 11918 | 10596 | 13403 | 8.180% T1 / 6.526% T2 | floor gap too large; perfect visible-bucket removal still misses |

The unicode residual rows are string/escape limited, not digest-hash limited
under the post-W6 legal seam. W6 already blocked the decoded source-method
digest fold; W7 cannot recover that route by renaming it host-sink work.

## Owner Paths

No W7 source owner path is opened. A successful W7 redress would have been
limited to SPEC Section 11 owner paths:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- selected non-JSON oracle/report files if W1b uses digest output
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

This plan opens only the W7 docs packet and routes the wave to CHALLENGE for
adversarial review of the block decision.

## Falsifiability Gate

`G-W7-DIGEST-SINK` is unmeasurable for a row-moving source patch because no
bounded selected row subset satisfies the entry condition:

- observed limiting `output_digest_hash` hot leaf,
- exact scalar fold/mix source below the pre-blocked decoded-string seam,
- output plane,
- independent Track 2 / oracle proof,
- plausible §0.4 row-floor closure on both tracks, or a valid non-JSON
  host-sink baseline.

If CHALLENGE finds a legal candidate, it must name the exact row, scalar source
function, consumer, independent oracle, fresh-profile command, and floor
closure math. Otherwise W7 proceeds to a REDRESS block with no source patch.

## Pre-Blocked Routes

W7 carries forward REDRESS 34/35/48, 54/55/66/69, 64, 82, 93, 100/101/109,
113, 116, and 117. In this plan they block:

- direct digest evidence as typed proof,
- Track 1-only digest admission,
- sink-local decoded stats/hash/source hooks,
- semantic facts entering parser output,
- parser-owned digest/hash semantics,
- hidden non-JSON close claims without generated non-JSON Track 1,
- cache hints or prefetch as a standalone admit,
- reusing W6's decoded-source fold as W7 host-sink work.

## Revert Protocol

No behavior patch is produced. If CHALLENGE accepts this block, W7 redress
records REDRESS 118, confirms `skinny/RESULTS.md` did not move, and writes
`/tmp/skv11-waveW7-rejected.patch` as an empty marker.
