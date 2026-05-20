# SK-V11 W5 Plan: Scalar Bounded String Span Direct

Date: 2026-05-20.

Disposition: PLAN by W5 Phase 2 plan agent A.

Inputs:

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 9.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R1-parse-that-string-span.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R2-generated-consumers.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R3-simd-string-block.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R4-row-gates-measurement.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R5-grammar-neutral.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R6-preblocked-risk.md`.

## Selected Intervention

W5 selects a scalar-only generated JSON direct bounded string span consumed by
generated direct `parse_string_direct`.

The selected caller is the generated direct string/key caller emitted from
`skinny/crates/codegen/src/sink_direct.rs` into
`skinny/crates/runtime/src/grammars/json/generated.rs`. The named consumer is
`parse_string_direct` as used by object keys through `sink.key_source`; root,
object, and array string-value call sites are affected because they share that
same helper, but they are parity and guard surfaces rather than additional W5
admission consumers.

The selected cap is the current generated direct cap 8. The cap includes the
closing quote in the examined bytes after the opening quote, so content lengths
0 through 7 may take the bounded plain fast path and a close at the first byte
outside the cap must fall back to the existing full matcher.

The selected row gate is exactly:

- `random/direct_to_struct/main`, floor `7878` Mbps on both Track 1 and
  independent Track 2.

No second target row is selected. Scout rows may be probed, but they do not
become admission rows without a new CHALLENGE.

This plan is admissible only under the JSON direct-plane closure envelope. It
does not close REDRESS 113's non-JSON block, does not reopen W3 numeric or W4
container-tail claims, and does not claim grammar-neutral generated-parser
proof.

## Owner Paths

Behavior/source owner paths:

- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md` only if the row admits
- `skinny/REDRESS.md` in either admit or reject redress

Read-only reference paths for this selected packet:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/tests/`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`

The selected implementation must not edit `parse-that-regex`, `bbnf-simd`, or
real-typed generated sources. If CHALLENGE requires a public parse-that span API
or any generic/codegen/runtime-outside-JSON behavior change, this plan returns
to REVISE/BLOCKED because R5 says the required same-wave non-JSON
string/literal proof is unavailable under REDRESS 113.

## Source Paths And Current Hooks

- `skinny/crates/parse-that-regex/src/lib.rs` owns `StringMatch`,
  `match_string_at_quote_trusted_utf8`, `match_string_at_quote`,
  `skip_string_plain`, and `unescape_string`; W5 uses these as semantic
  references and preserves their fallback behavior.
- `skinny/crates/codegen/src/sink_direct.rs` renders the generated direct
  `parse_string_direct` helper and all generated direct string/key call sites.
- `skinny/crates/runtime/src/grammars/json/generated.rs` is regenerated output.
  Its current direct string path uses `match_tiny_plain_string_direct` with cap
  8, falls back to `match_string_at_quote_trusted_utf8`, and emits raw slices
  plus `needs_unescape`.
- `skinny/crates/bbnf-bench/src/direct_struct.rs` owns Track 1
  `runtime::generated_json::parse_direct`, independent hand Track 2, and
  `assert_direct_struct_parity`.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs` and
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs` remain typed guard and
  W5 probe references, not selected implementation owners.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs` must consume W5 provenance and
  reject stale or false-accept rows.

## Implementation Sketch

1. In `sink_direct.rs`, replace the generated direct raw-end-only tiny loop
   shape with a generated JSON-local scalar span shape equivalent to:
   `{ content_start, content_end, raw_end, needs_decode }`.
2. Keep the helper JSON-local and direct-plane-specific. It uses delimiter
   `"`, escape `\`, control cutoff `0x20`, and cap 8 only because this selected
   caller is generated JSON direct SinkOnly. Do not transfer retained cap-16
   evidence or typed cap-32 evidence into this caller.
3. The bounded helper returns `Some` only when the closing quote appears inside
   the cap before any escape or control byte. It returns `None` on escape,
   control, missing quote, cap miss, short tail, overflow risk, or any
   unsupported precondition. The `None` path must leave `cursor` unchanged.
4. `parse_string_direct` consumes the span directly: borrow
   `&bytes[content_start..content_end]`, set `cursor = raw_end`, and emit
   `needs_unescape = false` for the bounded plain path.
5. The fallback path remains the existing
   `parse_that_regex::match_string_at_quote_trusted_utf8` mapping and must keep
   current `InvalidString`, `ExpectedValue`, offset, escape, surrogate, UTF-8
   trusted-input, and `needs_decode` behavior.
6. Preserve Track 2 independence. The hand parser in `direct_struct.rs` may add
   tests, but it must not call generated Track 1 helpers or generated span
   symbols.
7. Add W5 gate/report validation for only `random/direct_to_struct/main` with
   `wave_id=SK-V11-W5`, `same_wave_consumer_class=gate_json_direct_contract`,
   digest output, independent Track 2 status, and the expected W5 REDRESS entry
   (`REDRESS-116` if the ledger has not advanced).
8. Do not add SIMD, decoded scratch, string side tables, retained wrappers,
   semantic string facts, source-hook hashes, byte-output materializers, or
   parser-owned decoded state.

## Generated Regeneration Requirements

- Edit `sink_direct.rs`; do not hand-patch `generated.rs`.
- Regenerate JSON output from `skinny/`:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-json
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
```

- `regen-real-typed` is not part of this selected packet. If any typed source
  or schema path changes, return to CHALLENGE before implementation, then run:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-real-typed
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-real-typed
```

- Any generated-output diff without the corresponding renderer diff is
  inadmissible.

## Required Tests Before Measurement

Scalar/product correctness:

- Add W5 generated direct tests under `bbnf-bench` covering empty strings,
  non-zero offsets, cap-boundary closes, close just outside cap, escape before
  close, control before close, unterminated strings, raw non-ASCII trusted UTF-8,
  and exact `needs_unescape` propagation.
- Exercise object keys through `sink.key_source` and string values through the
  shared generated `parse_string_direct` caller.
- Assert generated Track 1, hand Track 2, serde_json, and sonic-rs agree on
  valid digest output and reject malformed string fixtures.
- Add source-level or symbol-level checks that Track 2 does not call generated
  Track 1 span helpers.

Gate/report correctness:

- Add W5 gate/report tests proving `random/direct_to_struct` rejects below
  `7878` Mbps, admits only with W5 measured-row provenance, and rejects stale
  `SK-V9-open`, `SK-V10-W2`, `SK-V10-W10`, `SK-V11-W4`, `REDRESS-113`,
  `REDRESS-114`, `REDRESS-115`, `gate_only`, deferred validation, wrong output
  plane, coupled Track 2, missing REDRESS, or wrong same-wave consumer class.
- Prove unselected W5 rows cannot admit under W5 provenance.
- Prove Unicode residual monitors remain residual unless explicitly selected by
  a later CHALLENGE.

Expected command set:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-json
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w5_string_span -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w5 -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench
```

Run `cargo test -p parse-that-regex` as a guard if any parse-that source is
even touched for tests. Touching parse-that behavior returns this packet to the
non-JSON proof rule above.

## Measurement Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Build the probe binary:

```sh
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Run selected row, direct guards, and Unicode residual monitors before and after
the W5 source patch:

```sh
for row in random citm_catalog apache_builds marine_ik unicode_basic unicode_escapes unicode_mixed y_string_unicode; do
  ./target/release/profile_direct 20000 "$row" track1
  ./target/release/profile_direct 20000 "$row" track2
done
```

Run typed guard probes before Criterion:

```sh
for row in twitter citm_catalog apache_builds github_events update_center mesh marine_ik; do
  ./target/release/profile_direct 5000 "$row" real_typed_track1
  ./target/release/profile_direct 5000 "$row" real_typed_track2
done
```

Probe sub-gate:

- At least one `random` track must improve by `>= 1.0%` Mbps.
- The other `random` track must not regress by more than `0.5%`.
- Direct guards, typed guards, and Unicode residual monitors must not show a
  guard-threatening regression.
- Probe movement permits Criterion; it does not admit the row.

Criterion for selected row, direct guards, and Unicode residual monitors:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(random|citm_catalog|apache_builds|marine_ik|unicode_basic|unicode_escapes|unicode_mixed|y_string_unicode)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

Criterion for typed guards:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Final gate/report check:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

Optional scout probes for future CHALLENGE only, not W5 admission:

```sh
for row in update_center github_events distinct_values gsoc-2018 twitter; do
  ./target/release/profile_direct 20000 "$row" track1
  ./target/release/profile_direct 20000 "$row" track2
done
```

## Exit Gate

Exit gate: `G-W5-STRING-SPAN-DIRECT`.

Admission requires:

- `random/direct_to_struct` Track 1 and independent Track 2 both meet
  `7878` Mbps in the same native Criterion root.
- Generated Track 1 and hand Track 2 digest output are exactly equal, and
  serde_json plus sonic-rs strict comparator rows are same-run and same-shape.
- Direct guards hold: `citm_catalog`, `apache_builds`, `marine_ik`, and
  `unicode_basic`.
- Typed guards hold: `twitter`, `citm_catalog`, `apache_builds`,
  `github_events`, `update_center`, `mesh`, and `marine_ik`.
- Unicode residual monitors `unicode_escapes`, `unicode_mixed`, and
  `y_string_unicode` are recorded honestly and are not admitted W5 guards.
- `gate-json --with-cost-facts --check-results` consumes W5 provenance in the
  same wave with strict measured-row evidence and no stale W2/W3/W4 close.
- REDRESS 113's non-JSON block is carried forward.

## Reject Protocol

Reject W5, save the implementation patch at
`/tmp/skv11-waveW5-rejected.patch`, restore only the W5 touched slice, and
record the W5 REDRESS entry if any of these occur:

- `random/direct_to_struct` misses `7878` Mbps on either Track 1 or Track 2.
- Probe movement is below the useful threshold or appears to come from
  unrelated container-tail, whitespace, digest, cold-code, or noise effects.
- Generated Track 1 diverges from hand Track 2, serde_json, or sonic-rs.
- Cap semantics are ambiguous, transfer retained cap-16 evidence, or differ
  between generated direct and hand Track 2 fixtures.
- Direct or typed guard floors fail.
- Unicode residual rows regress materially, are silently admitted, or are used
  as W5 guards without a new CHALLENGE.
- Track 2 calls generated Track 1 helpers or otherwise loses independence.
- `regen-json` or `check-json` fails, or generated output is patched without
  renderer source.
- The patch edits parse-that generic behavior, bbnf-simd, typed generated
  behavior, or runtime-outside-JSON without the same-wave non-JSON
  string/literal proof.
- The patch adds decoded scratch, retained string side tables, semantic string
  facts, output hash shortcuts, `StringBlock16` retained wrappers, 64-byte
  retained scans, SIMD production wiring, primitive-only production, or REDRESS
  106 full-string proof replay.
- The redress text treats REDRESS 113, REDRESS 114, or REDRESS 115 as closed by
  W5.

The revert slice is limited to W5 edits in `sink_direct.rs`, regenerated
`generated.rs`, `direct_struct.rs`, `json_parity.rs`, gate/report files,
`RESULTS.md`, and `REDRESS.md`. Preserve unrelated user or agent edits.
