# SK-V11 W4 R3 - Direct Oracle Map

Date: 2026-05-20.
Lane: W4 research R3.
Scope: research only; no source, gate, result, or redress edits.

## Question

Map the direct Track 1, Track 2, serde, and sonic oracle paths used by the W4
candidate rows:

```text
canada mesh random update_center github_events twitter
```

W4 is `G-W4-DISPATCH-BYTESET-DIRECT`: selected direct rows must clear the
SPEC Section 0.4 floor on both generated Track 1 and independent Track 2, keep
the same output plane, preserve strict same-run sonic digest comparator
evidence, and measure direct/typed guards.

## Source Map

### Direct digest core

`skinny/crates/bbnf-bench/src/direct_struct.rs`

- `JsonDirectDigest` is the direct output plane. It records object/array/member/
  element counts, string count/bytes, number/bool/null counts, max depth, and a
  rolling fingerprint.
- `track1_digest(input)` builds a `JsonDigestSink` and calls
  `runtime::generated_json::parse_direct(input, &mut sink)`. This is the
  generated Track 1 direct-to-struct path.
- `track2_digest(input)` calls `hand::sink_digest(input)`. This is the direct
  Track 2/oracle path for `track2_direct_to_struct`; it is a local hand parser,
  not `track2::json::parse`.
- `serde_digest(bytes)` calls `serde_json::from_slice::<JsonDirectDigest>`.
- `sonic_digest(bytes)` calls `sonic_rs::from_slice::<JsonDirectDigest>`.
- `assert_direct_struct_parity(input, bytes)` requires exact `track1 == track2`
  and shape equality against serde/sonic via `same_shape_as`. Current serde/sonic
  proof is therefore shape/count/depth/string-byte parity on the digest plane,
  not fingerprint equality.
- `hand::sink_digest` is structurally independent from generated Track 1. It has
  its own object/array/string/number/literal parser and only shares the low-level
  regex helpers for string, number, and whitespace scanning.
- The serde/sonic paths both enter through the `Deserialize` impl and
  `DigestVisitor`, so competitor direct rows measure deserialization straight
  into the same digest output type.

### Track 2 tape parser

`skinny/crates/bbnf-bench/src/track2/json.rs`

- `parse(input)` is the handcoded Track 2 tape/DOM path for
  `track2_handcoded`, not the direct digest benchmark body.
- Its parser owns object/array/value dispatch, container tail handling,
  key/colon handling, tiny string fast path, number span emission, and literal
  emission into a `TapeBuilder`.
- `json_parity.rs` uses this path for `track2_payload_counters(input)`, so
  Track 2 direct metadata inherits payload counters from the handcoded tape
  parser even though `track2_direct_to_struct` executes `direct_struct::track2_digest`.
- Its tests assert valid JSON parsing without payload writes, invalid JSON
  rejection, and offset compatibility with Track 1 without calling the Track 1
  parser from the Track 2 implementation.

### Bench harness and metadata

`skinny/crates/bbnf-bench/benches/json_parity.rs`

- For every loaded fixture, the bench first runs generic parse parity and then
  `direct_struct::assert_direct_struct_parity(input, bytes)` before measuring.
- Candidate fixtures come from `test_fixtures::load_available_bench_fixtures()`;
  if corpus fixtures are present, the canonical manifest rows include all six
  W4 candidates.
- `canada` uses sample size 50 and 8 seconds measurement time. Other candidate
  rows use sample size 100 and 5 seconds.
- Direct Track 1 benchmark:
  `json_<corpus>/track1_direct_to_struct` ->
  `bbnf_bench::direct_struct::track1_digest(input)`.
- Direct Track 2 benchmark:
  `json_<corpus>/track2_direct_to_struct` ->
  `bbnf_bench::direct_struct::track2_digest(input)`.
- Sonic direct comparator:
  `json_<corpus>/sonic_rs_direct_to_struct` ->
  `bbnf_bench::direct_struct::sonic_digest(bytes)`.
- Serde direct comparator:
  `json_<corpus>/serde_json_direct_to_struct` ->
  `bbnf_bench::direct_struct::serde_digest(bytes)`.
- Metadata rows are written under `CRITERION_HOME/json_<corpus>/<bench>/metadata.toml`
  when `CRITERION_HOME` is set.

## Candidate Row Matrix

All six candidates share the same four direct oracle paths above. The per-row
distinction is fixture size, W0 throughput, and the W4 floor.

| Row | Fixture bytes | Sample/seconds | Track 1 | Track 2 | Sonic direct | Floor | Binding Criterion ids |
|---|---:|---:|---:|---:|---:|---:|---|
| `canada` | 2,251,051 | 50 / 8s | 10316 | 9819 | 11700 | 10637 | `json_canada/{track1_direct_to_struct,track2_direct_to_struct,sonic_rs_direct_to_struct,serde_json_direct_to_struct}` |
| `mesh` | 723,597 | 100 / 5s | 8561 | 8652 | 9542 | 8675 | `json_mesh/{track1_direct_to_struct,track2_direct_to_struct,sonic_rs_direct_to_struct,serde_json_direct_to_struct}` |
| `random` | 510,476 | 100 / 5s | 7693 | 6949 | 8665 | 7878 | `json_random/{track1_direct_to_struct,track2_direct_to_struct,sonic_rs_direct_to_struct,serde_json_direct_to_struct}` |
| `update_center` | 533,178 | 100 / 5s | 8187 | 7474 | 11064 | 10059 | `json_update_center/{track1_direct_to_struct,track2_direct_to_struct,sonic_rs_direct_to_struct,serde_json_direct_to_struct}` |
| `github_events` | 65,132 | 100 / 5s | 11918 | 10596 | 14743 | 13403 | `json_github_events/{track1_direct_to_struct,track2_direct_to_struct,sonic_rs_direct_to_struct,serde_json_direct_to_struct}` |
| `twitter` | 631,515 | 100 / 5s | 11613 | 10816 | 15113 | 13740 | `json_twitter/{track1_direct_to_struct,track2_direct_to_struct,sonic_rs_direct_to_struct,serde_json_direct_to_struct}` |

Near-floor order by smallest combined Track 1/Track 2 gap is `mesh`, `canada`,
`random`, `github_events`, `twitter`, `update_center`. W4 still caps the
selected redress target set at three rows unless CHALLENGE splits the wave.

## Oracle Boundaries

- Generated Track 1 direct proof is `parse_direct` plus `JsonDigestSink`; W4
  dispatch changes must be differentialed against this sink path.
- The direct Track 2 oracle is `direct_struct::hand::sink_digest`, not the
  Track 2 tape parser. It must remain free of calls into generated Track 1,
  generated SinkOnly helpers, generated typed helpers, or benchmark-private
  generated parser code.
- `track2/json.rs` remains relevant as the independent handcoded tape parser and
  payload-counter source for metadata, but it does not prove direct digest
  equality by itself.
- Sonic is the strict same-run direct comparator consumed by gate/report logic on
  the `digest` plane. Serde is an additional strict digest-plane comparator.
- Current direct parity proves exact generated-vs-hand digest equality and
  serde/sonic shape parity. A W4 admission should not describe serde/sonic as
  exact fingerprint or key-order proof unless the code changes to enforce that.

## Commands Needed For W4

Run from repository root with `cd skinny` in each command.

1. Direct and Track 2 oracle unit coverage:

```sh
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_struct -- --nocapture
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench track2::json -- --nocapture
```

2. Gate/report contract tests. Existing direct contract filters should stay
   green; W4 source work also needs a W4-specific gate test/filter once the
   W4 provenance marker or route is implemented.

```sh
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate direct -- --nocapture
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench report::tests::direct_contract -- --nocapture
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w4 -- --nocapture
```

3. Compile owner surface after any W4 source patch:

```sh
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench
```

4. Fast same-host preselection sweep across all six candidate rows and all four
   direct oracle bodies:

```sh
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
cd skinny && for corpus in canada mesh random update_center github_events twitter; do for mode in track1 track2 sonic serde; do RUSTFLAGS="-C target-cpu=native" ./target/release/profile_direct 2000 "$corpus" "$mode"; done; done
```

5. Binding Criterion direct run for selected W4 rows. Replace the six-row regex
   with the CHALLENGE-selected set of at most three rows for the admission run;
   use the six-row version only for research/preselection.

```sh
cd skinny && CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(canada|mesh|random|update_center|github_events|twitter)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

6. Guard measurements required by SPEC Section 8:

```sh
cd skinny && CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(citm_catalog|apache_builds|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
cd skinny && CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

7. Gate consumption of the same Criterion home:

```sh
cd skinny && CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## R3 Conclusion

W4 direct admission evidence must be built from the `json_parity` direct rows:
generated Track 1 digest, independent hand direct Track 2 digest, sonic strict
digest comparator, and serde digest comparator. The handcoded Track 2 tape parser
is an independence and metadata support path, but the row-moving direct oracle is
`direct_struct::hand::sink_digest`.
