# SK-V8 W4 Hardening V1 Consolidated

Date: 2026-05-18.

Verdict: REVISE.

Panel:

| Reviewer | Verdict | Confidence |
|---|---|---:|
| CH1 | REVISE | 86% |
| CH2 | ACCEPT | 97% |
| CH3 | ACCEPT | 90% |
| CH4 | REVISE | 94% |
| CH5 | ACCEPT | 93% |
| CH6 | ACCEPT | 95% |

Result: 4/6 ACCEPT, 2/6 REVISE. W4 V1 does not converge.

## Blocking Findings

1. The selected W4 rows are valid Track2-only direct misses, but the plan's
   verification proof is insufficient for SPEC Section 7. A global hand Track 2
   parser change needs full-table maintain evidence, not only target rows plus
   existing direct GO rows.
2. The checked report path is still W0-shaped. Current
   `gate-json --check-results` rejects row status, run id, outcome, and delta
   movement by design. Any W4 row-table admission would need a W4-aware checked
   gate before updating `skinny/RESULTS.md`.
3. Lock 14 has no W4 parent-diff allowance for
   `crates/bbnf-bench/src/direct_struct.rs`; W4 source admission would need an
   explicit allowance and tests.
4. Track 2 parser independence is preserved by the proposed source shape, but
   digest arithmetic needs an executable backstop if scalar folding is admitted.
5. Preblocked route leakage must stay explicitly closed: no source hooks, no
   generated helper mirroring, no value-byte carry, no cap-16, no string
   materializer/fact retry, and no raw-f64 or mantissa route.

## Post-Challenge Measurement

A W4 scalar-parent-fold candidate was implemented locally after the plan
commit and saved as rejected patch
`/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`. Correctness passed:

```text
cargo test -p bbnf-bench direct_struct -- --nocapture
```

Targeted native Criterion falsified the three-row plan:

| Row | Track 2 result | sonic-rs result | W4 result |
|---|---:|---:|---|
| `apache_builds/direct_to_struct` | 95.347 us | 92.643 us | passes sonic/1.10 |
| `random/direct_to_struct` | 569.57 us | 463.26 us | fails sonic/1.10 |
| `numbers/direct_to_struct` | 106.43 us | 93.211 us | fails sonic/1.10 and regresses |

The `numbers` Track 2 lane regressed by +6.3287% time in Criterion. That alone
falsifies the V1 plan, independent of the report-gate issue.

## Fold Direction

Do not admit W4 source on V1. Revert the source patch, leave
`skinny/RESULTS.md` unchanged, add REDRESS for the failed scalar-parent fold,
route W4's remaining direct digest misses, and challenge the rejection/routing
disposition in V2.
