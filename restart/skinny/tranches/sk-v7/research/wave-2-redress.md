# SK-V7 Wave 2 Redress: Zero-Fallback Mantissa-Widen Rejection

Date: 2026-05-16.

Outcome: **REJECTED**.

Intervention: zero-fallback mantissa-widen redress.

## Measurement

Fresh fallback attribution on the current W1 baseline counted `111126` canada
numbers: `46` integers and `111080` f64 candidates. All `111080` f64
candidates were handled by the current Eisel-Lemire path. The measured canada
fallback pool was:

| Counter | Count |
|---|---:|
| f64 candidates | 111080 |
| mantissa overflow | 0 |
| ambiguous EL `None` | 0 |
| `str::parse::<f64>()` fallback | 0 |
| fallback rate | 0.0000% |

The scoped direct Criterion subset was run for canada, numbers, mesh, and
marine_ik. The refreshed gate rows are:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Outcome |
|---|---:|---:|---:|---|
| canada | 10773 | 10296 | 12421 | N-direct / NO-GO |
| numbers | 12615 | 12362 | 12838 | A / GO |
| mesh | 8798 | 8699 | 9902 | N-direct / NO-GO |
| marine_ik | 9391 | 9349 | 8465 | A / GO |

## Verification

Commands:

```bash
cargo test --workspace
cargo run -p xtask --release -- primitive-checkasm
cargo bench -p bbnf-bench --bench json_parity -- 'json/(canada|numbers|mesh|marine_ik)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

Results:

- `cargo test --workspace` passed.
- `primitive-checkasm` passed.
- The scoped Criterion subset completed.
- `gate --advisory` refreshed `skinny/RESULTS.md` and exited 5 because the
  current overall gate remains `N-direct / NoGo`.

## Rejection Reason

The planned mantissa-widen route has no same-wave consumer on canada in the
current tree. canada does not reach `materialize_f64`'s fallback branch: there
is no mantissa overflow, no ambiguous EL return, and no `str::parse::<f64>()`
fallback to eliminate. Editing `POWER_OF_FIVE_128` or widening the mantissa
table would be non-causal for the measured failure.

No source patch was attempted after attribution falsified the route. The
rejected patch file is present at `/tmp/skv7-wave-2-rejected.patch` and is
empty by construction.

## Next Candidate

Route the canada direct residual to a separately profiled numeric-array
scan/dispatch candidate. W2 evidence names `match_number_span_from_first` and
array-number direct dispatch as the likely residual surface, not Eisel-Lemire
fallback elimination.
