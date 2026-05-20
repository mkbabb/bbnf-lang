# SK-V11 W7 CH4 - Cost/Floor Challenge

Date: 2026-05-20.

Challenge: CH4 cost/floor plausibility for the W7 output digest/hash host-sink
entry block.

Disposition: ACCEPT BLOCK.

## Scope Read

SPEC Section 11 admits W7 only for C8 output digest/hash oracle or per-product
host-sink work. The entry gate requires CHALLENGE acceptance that
`output_digest_hash` is an observed limiting hot leaf for a bounded selected
product-row subset, with exact scalar fold/mix source, output plane, and
independent oracle. The exit gate then requires selected direct rows whose fresh
post-W6 profile still names `output_digest_hash` as limiting to clear the
Section 0.4 floors on both Track 1 and Track 2/oracle.

The requested `/tmp/skv11-p1` profile exports are not present in this workspace
at challenge time. I therefore checked the committed W7 R2/R6 summaries,
`skinny/RESULTS.md`, and the plan's visible digest bucket table. That is enough
to challenge the floor plausibility claim because the plan uses explicit bucket
percentages.

## Cost Math

For a visible self-time bucket `p`, the most favorable possible throughput after
removing that bucket is:

```text
optimistic_mbps = current_mbps / (1 - p)
```

This is an upper bound. It assumes perfect removal of all visible digest cost,
zero replacement cost, no instruction-cache or dependency cost, no lost
parallelism, no measurement noise penalty, and no hidden non-digest limiter.

| Row | Track 1 current | Track 1 bucket | Track 1 optimistic | Track 2 current | Track 2 bucket | Track 2 optimistic | Floor | Result |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| `distinct_values/direct_to_struct` | 1750 | 16.400% | 2093 | 1625 | 6.091% | 1730 | 2658 | misses both |
| `update_center/direct_to_struct` | 8187 | 12.236% | 9329 | 7474 | 9.398% | 8249 | 10059 | misses both |
| `random/direct_to_struct` | 7693 | 11.684% | 8711 | 6949 | 8.452% | 7590 | 7878 | Track 1 clears, Track 2 misses |
| `github_events/direct_to_struct` | 11918 | 8.180% | 12980 | 10596 | 6.526% | 11336 | 13403 | misses both |

The plan's visible-bucket/floor conclusion is therefore correct for all residual
rows it lists. `random/direct_to_struct` is the only row where a perfect
`output_digest_hash` removal can plausibly clear one track, but it still misses
Track 2 by `7878 - 7590 = 288 Mbps`. Because W7 admission requires both Track 1
and independent Track 2/oracle to clear the floor, `random` is not a valid REVISE
row.

`apache_builds/direct_to_struct` is not a residual admission target. Using the
current `skinny/RESULTS.md` guard row values, perfect visible digest removal
would be ample:

```text
Track 1: 11254 / (1 - 0.25960) = 15199 Mbps, above guard 11028
Track 2: 10189 / (1 - 0.15180) = 12013 Mbps, above guard 9996
```

That only makes it a regression sentinel. It cannot justify W7 source redress
because the row is already direct `A / GO`.

## Digest Bucket Verification

The visible bucket numbers used by the plan are internally plausible with W7 R2:

- `apache_builds` is the strongest digest/hash profile signal but is already a
  guard row.
- `distinct_values` is the strongest residual digest candidate, but the visible
  bucket is smaller than the required floor delta on both tracks.
- `random` is a near-floor Track 1 scout, but its Track 2 visible bucket cannot
  cover the Track 2 gap.
- `update_center` and `github_events` have visible digest support, but both
  remain far below floor even under perfect removal.

The committed evidence also leaves a separate freshness failure: no fresh
post-W6 profile export is available under `/tmp`, and W7 R2 says the S-P1
profile is only behavior-equivalent triage evidence, not the literal Section 11
fresh profile. Even if freshness were waived, the optimistic floor math still
does not produce a residual row that can clear both tracks.

## Challenge Decision

ACCEPT the W7 plan's BLOCK disposition.

There is no REVISE row. No listed residual row has a plausible
`output_digest_hash`-only route that clears both Track 1 and Track 2 floors:
`random/direct_to_struct` is the closest, and its best-case Track 2 estimate is
`6949 / (1 - 0.08452) = 7590 Mbps`, below the `7878 Mbps` floor.
