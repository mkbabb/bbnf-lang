# SK-V13 W11.4 Redress - Direct Cursor Byte Fetch

Date: 2026-05-22.
Disposition: REJECT.
Gate: `G-W11.4-JSON-DIRECT-CURSOR-BYTE`.

## Patch

W11.4 replaced the four hot generated direct-dispatch byte fetches in
`json_sink_direct.rs` and the checked-in generated JSON output:

- `parse_value_direct`
- `parse_object_value_at_direct`
- `parse_array_element_at_direct`
- `parse_array_direct`

The attempted shape was an explicit `*cursor >= bytes.len()` guard followed by
`unsafe { *bytes.get_unchecked(*cursor) }`. The source patch is reverted and
retained at `/tmp/skv13-waveW11.4-rejected.patch`.

Rejected patch SHA-256:

```text
7ce243dc25e321d8e370670c9939055db5627d962a51d91ce404a55abf550cd7
```

## Measurement

Native direct-lane Criterion was run with:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(instruments|mesh|random|canada|github_events)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

Same-run Mbps from the Criterion estimate files:

| row | Track 1 Mbps | sonic strict Mbps | margin vs sonic+1 | time delta |
|---|---:|---:|---:|---:|
| instruments | 12025.558 | 12721.724 | -697.166 | +2.249% |
| mesh | 9665.378 | 9744.222 | -79.844 | +2.380% |
| random | 7815.510 | 8944.573 | -1130.064 | +1.697% |
| canada | 10983.047 | 12201.442 | -1219.395 | -1.101% |
| github_events | 12474.864 | 16161.279 | -3687.414 | +0.148% |

The primary target `json/instruments/direct_to_struct/main` regressed and
missed the strict sonic+1 gate by `697.166` Mbps. Guard rows did not rescue the
wave: `mesh` and `random` regressed, `github_events` was flat, and the small
`canada` speedup remained far below its sonic floor.

## Verification

Passed before measurement:

```text
cargo test -p bbnf-bench direct_struct::tests -- --nocapture
cargo test -p bbnf-bench direct_contract -- --nocapture
cargo test -p runtime json -- --nocapture
```

`cargo test -p codegen json -- --nocapture` failed in
`tests::json_config_policy_fields_are_consumed` with
`STRING_NEEDS_DECODE has no generated consumer`. That failure is outside the
W11.4 owner slice and is not caused by the byte-fetch patch: the current
generated code consumes the policy through `config::needs_decode_flags()` and
the view layer consumes `config::string_needs_decode`, while the test searches
for the exact uppercase constant spelling. W11.4 records it as routed
verification debt rather than widening the redress owner paths.

## Disposition

The patch is rejected and reverted. W11.4 records a measured material
differential from REDRESS 119/120/143: direct cursor byte-fetch specialization
does not close `instruments`, does not move any guard row over strict SOTA, and
regresses multiple direct guard lanes on this host. No `RESULTS.md` or rolling
delta row is updated.
