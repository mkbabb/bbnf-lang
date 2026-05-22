# SK-V13 W15.1 Plan - UpdateCenter Typed Plugin Fast Path

Date: 2026-05-22.
Gate: `G-W15.1-JSON-TYPED-UPDATE-CENTER-PLUGIN`.

## Selected Intervention

Emit a generated typed-direct fast path for `Plugin` objects in the
`update_center` typed product parser. The fast path recognizes the six observed
ordered plugin field families without allocating `Cow` keys for ignored fields.
It must use the existing validating value parsers/skippers and fall back to the
current generic parser on any mismatch.

The generic parser remains the semantic authority. The fast path is an
optimistic specialization:

```text
checkpoint = parser.cursor
if parse_type_plugin_ordered(parser) succeeds:
    return value
parser.cursor = checkpoint
run current parse_type_plugin loop
```

## Implementation Shape

1. Add `DirectParser::take_string_literal` and `DirectParser::take_field` to
   consume exact JSON string field names without materializing a `Cow`.
2. Add `parse_type_plugin_ordered` to the generated typed module.
3. Patch the renderer to emit the same specialization for `type_id == "Plugin"`
   so checked-in generated output and generator remain synchronized.
4. Keep all retained values parsed through `parse_option_scalar_string`.
5. Keep ignored values skipped through `skip_string_raw`, `skip_array`, or
   `skip_value` as appropriate. No permissive balanced-skip shortcut is in
   W15.1 scope.

The first measurement may use the checked-in generated module and renderer
edit together. If the source patch regresses or misses, save
`/tmp/skv13-waveW15.1-rejected.patch`, revert the source patch, and record a
measured REDRESS rejection.

## Falsifiability Gate

W15.1 admits only if:

- `json/update_center/real_typed_struct/main` Track 1 exceeds same-run
  sonic-rs strict real_typed_struct by at least 1 Mbps;
- strict real typed fixture parity passes against serde/sidecars;
- prior typed A/GO rows do not silently demote in `RESULTS.md` or rolling
  delta;
- generated output and renderer stay synchronized;
- REDRESS records exact same-host Criterion measurements and report/gate
  consumption.

Guard rows:

- `json/twitter/real_typed_struct/main`
- `json/github_events/real_typed_struct/main`
- `json/mesh/real_typed_struct/main`
- `json/marine_ik/real_typed_struct/main`

## Verification

Run:

```text
cargo test -p bbnf-bench generated_update_center_typed_parser_matches_sidecars -- --nocapture
cargo test -p bbnf-bench w2_full_real_typed_fixtures_match_sidecars -- --nocapture
cargo test -p bbnf-bench real_typed_struct -- --nocapture
cargo xtask regen-real-typed
cargo xtask check-real-typed
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(update_center|twitter|github_events|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

If admitted, add the W15.1 gate/report lane and refresh `RESULTS.md` plus the
rolling delta. If rejected, no status row changes.

## Revert Protocol

Revert only:

- `skinny/crates/codegen/src/json_typed_direct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- any W15.1 report/gate/status files added during redress

Unrelated dirty CSS sidecar JSON files stay unstaged.
