# SK-V14 W9 Plan - JSON Direct + Typed Re-Admit

Wave: W9 JSON Direct + Typed Re-Admit (R7).
Authority: `restart/skinny/tranches/sk-v14/SPEC.md` Section 12 and
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`
Section 1.3.

## Selection

W9 selects all 17 JSON `direct_to_struct` rows and all 17 JSON
`real_typed_struct` rows for disposition. A row admits only if the executable
evidence satisfies the rebound strict plane, per-iteration equality, and the
per-row Mbps floor. Rows that lack the required product surface or comparator
plane are routed to REDRESS instead of being relabeled.

Selected corpora:

`twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`,
`update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`,
`numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
`distinct_values`, `y_string_unicode`.

## Comparator And Track Binding

Direct rows:

- Track 1: `bbnf_bench::direct_struct::track1_digest`
- Track 2: `bbnf_bench::direct_struct::track2_digest`
- Current comparator executable: `bbnf_bench::direct_struct::sonic_digest`
- Required W9 comparator plane: `<corpus>::strict_struct_deser`

Typed rows:

- Track 1: `bbnf_bench::real_typed_struct::track1_typed`
- Track 2: `bbnf_bench::real_typed_struct::track2_typed`
- Sonic comparator: `bbnf_bench::real_typed_struct::sonic_typed`
- Serde comparator: `bbnf_bench::real_typed_struct::serde_typed`
- Required W9 comparator plane: `<corpus>::typed_strict_struct_deser`

The direct plane currently remains a generic digest surface. It may prove
correctness parity for all 17 rows, but it is not a per-corpus strict struct
deserialization product and therefore cannot admit direct rows in this wave
without a real per-corpus product implementation.

The typed plane has product surfaces for 11 corpora:

`twitter`, `apache_builds`, `citm_catalog`, `github_events`, `update_center`,
`mesh`, `marine_ik`, `instruments`, `numbers`, `unicode_basic`, `random`.

The remaining typed rows have no product surface at HEAD:

`canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`,
`distinct_values`, `y_string_unicode`.

## Measurement

The wave uses the existing cold per-parse `profile_direct` binary rather than a
warm Criterion-only relabel. The run builds a fresh release binary with native
target CPU and then measures selected direct and typed modes through the binary:

```sh
cd skinny
CARGO_TARGET_DIR=/tmp/skv14-w9-target \
RUSTFLAGS="-C target-cpu=native" \
cargo build --release -p bbnf-bench --bin profile_direct
```

For each selected typed product row, measure:

- `real_typed_track1`
- `real_typed_track2`
- `real_typed_sonic`
- `real_typed_serde`

For direct rows, run parity tests and the W9 probe; do not admit from digest
comparators.

## Exit Routing

Admit typed rows only when all of the following hold:

- `track1_typed`, `track2_typed`, `sonic_typed`, and `serde_typed` checksums
  match in the timed-path implementation.
- Cold per-parse Track 1 Mbps is greater than the typed strict sonic comparator
  by at least 1 Mbps.
- Track 2 is executable and structurally independent.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` move only the
  admitted typed rows to an admitted W9 posture.

Reject and route:

- all direct rows whose evidence remains `digest` rather than per-corpus strict
  struct product;
- typed rows with missing product surfaces;
- any typed product row whose measured Track 1 does not clear the sonic
  strict floor.

W9 does not carry Stage-0 F-V2-P1ABC-RERECORD. SPEC Section 12 pins that
obligation outside this wave because the direct and typed planes consume full
tape parses, not the dispatch-envelope parse-only scan.
