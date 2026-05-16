# SK-V6 C2 Direct Profile

Date: 2026-05-15
Workspace: `/Users/mkbabb/Programming/bbnf-lang`

Read-only discipline: no repository files were edited. The only retained output
from this dispatch is this file. Fresh samples were captured through
`sample -file /dev/stdout` so no `.profile.json.gz`, `.syms.json`, or
`.sample.txt` artifacts were retained.

## Authority Read

- SK-V6 synthesis and packet:
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md`,
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md`, and
  `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md`.
- Current result/redress authority: `skinny/RESULTS.md` and
  `skinny/REDRESS.md`.
- Cohort references:
  `restart/skinny/audit/SK-V6-COHORT/skv6-B4-profile-direct-three-way.md`,
  `skv6-R3-direct-attribution.md`, and
  `skv6-R3g-typed-directbuild-implementation.md`.

Binding context from those files: Track 1 direct is now generated runtime
`runtime::generated_json::parse_direct`; Track 2 direct is the independent
hand parser. The old bench-private `SinkParser` attribution is obsolete.
The existing `direct_to_struct` row is a `semantic_full_digest_stressor`, while
`real_typed_struct` is the representative host/API typed-output plane.

## Method

- Binary reused: existing `skinny/target/release/profile_direct`. I did not
  rebuild because the live workspace is dirty and the dispatch requested
  write-only final output.
- Throughput scout: unsampled `profile_direct <iters> <corpus> track1|track2`
  stderr Mbps.
- Hot-symbol scout: macOS `sample <pid> 1 1 -mayDie -file /dev/stdout`,
  demangled with `rustfilt`, parsed from "Sort by top of stack, same
  collapsed". Each sampled row has about 760 samples, so percentages are a
  scouting profile, not a Criterion gate.
- Existing B4/R3 samply tables corroborate the main symbols on
  `unicode_escapes`, `numbers`, `distinct_values`, `twitter`,
  `update_center`, and `y_string_unicode`.

## Direct Hot Symbols

`c/B` assumes the existing cohort convention: `28000 / Mbps`.

| Corpus | Track 1 generated direct scout | Track 2 hand direct scout | Readout |
|---|---:|---:|---|
| `unicode_escapes` | 4638 Mbps, 6.04 c/B. Hot: `unescape_json_string` 46.9%; `generated::parse_object_value_at_direct::<JsonDigestSink>` 42.5%; `JsonDigestSink::array_string` closure 2.6%. | 4648 Mbps, 6.02 c/B. Hot: `unescape_json_string` 45.9%; `HandParser::string` 44.7%; `HandParser::value` 2.5%. | Both tracks are escape decode/materialization bound. Generated routing is real, but field layout is not involved on this digest row. |
| `distinct_values` | 5991 Mbps, 4.67 c/B. Hot: `generated::parse_array_element_at_direct::<JsonDigestSink>` 47.1%; `generated::parse_object_value_at_direct::<JsonDigestSink>` 30.3%; `JsonDigestSink::array_string` closure 22.4%. | 5352 Mbps, 5.23 c/B. Hot: `HandParser::string` 40.7%; `HandParser::object` 32.1%; `HandParser::value` 27.2%. | Generated Track 1 is dominated by generated container/value loops plus same-loop string folding. Track 2 pays hand string/object recursion. |
| `y_string_unicode` | 3393 Mbps, 8.25 c/B. Hot: `mach_absolute_time` 30.8%; `unescape_json_string` 20.8%; `generated::parse_array_element_at_direct::<JsonDigestSink>` 16.3%; malloc/free 21.1%; `JsonDigestSink::array_string` closure 3.1%. | 2901 Mbps, 9.65 c/B. Hot: `mach_absolute_time` 29.1%; `unescape_json_string` 21.0%; `HandParser::string` 17.1%; malloc/free 16.5%; `HandParser::value` 7.0%. | Small-corpus noisy; still escape/allocation dominated rather than a clean generated emitter row. |
| `numbers` | 11424 Mbps, 2.45 c/B. Hot: `generated::parse_array_element_at_direct::<JsonDigestSink>` 78.9%; `number::materialize_f64` 11.3%; `profile_direct::run_once` 9.5%. | 11877 Mbps, 2.36 c/B. Hot: `HandParser::value` 87.1%; `number::materialize_f64` 12.9%. | Number materialization is wired; the row is mostly array/value loop plus `materialize_f64`, not old `serde_json::parse_number`. |
| `twitter` | 11144 Mbps, 2.51 c/B. Hot: `generated::parse_object_value_at_direct::<JsonDigestSink>` 72.9%; `generated::parse_array_element_at_direct::<JsonDigestSink>` 11.3%; `JsonDigestSink::array_string` closure 8.5%; `unescape_json_string` 1.6%. | 10367 Mbps, 2.70 c/B. Hot: `HandParser::string` 53.3%; `HandParser::value` 21.7%; `HandParser::object` 17.5%. | Digest Track 1 remains generic generated JSON + sink fold. The separate `real_typed_struct` product plane exists for this corpus and passes in `RESULTS.md`, but this digest profile does not use it. |
| `update_center` | 7967 Mbps, 3.51 c/B. Hot: `generated::parse_object_value_at_direct::<JsonDigestSink>` 70.1%; `JsonDigestSink::array_string` closure 19.1%; `generated::parse_array_element_at_direct::<JsonDigestSink>` 6.8%. | 7134 Mbps, 3.93 c/B. Hot: `HandParser::string` 57.2%; `HandParser::value` 28.2%; `HandParser::object` 12.0%. | Same shape as R3: receiver/string fold is visible on generated Track 1. The separate generated typed DirectBuild row exists and passes, but the digest stressor remains distinct. |
| `mesh` | 8139 Mbps, 3.44 c/B. Hot: `generated::parse_array_element_at_direct::<JsonDigestSink>` 77.6%; `generated::parse_object_value_at_direct::<JsonDigestSink>` 14.8%; `number::materialize_f64` 4.8%; `number::materialize_u64` 2.6%. | 8314 Mbps, 3.37 c/B. Hot: `HandParser::value` 93.7%; `number::materialize_f64` 6.3%. | Numeric/container row. Digest Track 1 is generated, but no field-layout materializer is present for a mesh typed product row yet. |

## Output Plane And Path Split

All seven profiled rows above are the `direct_to_struct` /
`semantic_full_digest_stressor` plane: generated `JsonDigestSink` digest vs
independent hand `HandParser` digest vs sidecar typed serde comparators in the
gate. `skinny/RESULTS.md` marks this plane as deferred / view-boundary / escape
complete, with correctness passing but many throughput misses still visible.

Generated Track 1 path:

- `bbnf_bench::direct_struct::track1_digest` calls
  `runtime::generated_json::parse_direct(input, &mut JsonDigestSink)`.
- `parse_direct` dispatches through generated
  `parse_object_value_at_direct`, `parse_array_element_at_direct`,
  `parse_number_object_direct`, and `parse_number_array_direct`.

Track 2 direct path:

- `bbnf_bench::direct_struct::track2_digest` calls `hand::sink_digest`.
- `HandParser` recursively returns `JsonDirectDigest` children and folds them
  in `object` / `array`.
- It does not call `runtime::generated_json::parse_direct`.

## Scalar Folding And Field Layout

Same-loop scalar-parent folding exists on the generated direct digest path.
`JsonSink` exposes parent-aware methods (`array_string`, `array_f64`,
`object_string`, `object_f64`, etc.), generated direct calls the corresponding
`*_source` and number-object/number-array paths, and `JsonDigestSink` overrides
those methods to fold scalars directly into the current parent frame. This is
why `JsonDigestSink::array_string` shows up on `distinct_values`,
`twitter`, and `update_center`, and why `numbers` / `mesh` sit in generated
array-element and number materialization code.

The hand Track 2 parser does not have the same generated same-loop form. It
materializes a child digest from `value()` and then calls `fold_child`.

Generated field-layout materialization also exists, but not on the digest
profiles above. Current code has `DirectSchemaSet`,
`lower::schema_direct::lower_program`, `json_typed_direct`, and generated
`parse_twitter_search` / `parse_update_center` typed DirectBuild output. That
path is available for the separate `real_typed_struct` rows for `twitter` and
`update_center`. It is not currently available for `unicode_escapes`,
`distinct_values`, `y_string_unicode`, `numbers`, or `mesh`, and the digest
stressor rows intentionally continue to report their misses.

## C2 Conclusion

The generated-vs-hand split is clean:

- Escape rows (`unicode_escapes`, `y_string_unicode`) are decode/allocation
  dominated in both tracks. The generated path is real, but scalar folding does
  not remove the need to decode the selected semantic strings.
- String/object digest rows (`distinct_values`, `twitter`, `update_center`)
  show generated object/array dispatch plus `JsonDigestSink` receiver folding.
- Numeric rows (`numbers`, `mesh`) are generated array/value loop plus
  parse-that number materialization; the old number-parser diagnosis remains
  obsolete.
- The product-plane DirectBuild close must be typed-output schema work, not
  another sink-local full-digest rewrite.

## Falsifiable DirectBuild Candidate

Candidate C2-DB1: add a generated `real_typed_struct` / DirectBuild schema for
`mesh`.

Shape: host/API `DirectSchemaSet` for a `MeshDirect` product type with typed
numeric vectors for the large numeric fields (`positions`, and any admitted
normal/index/vector fields), typed `Batch` entries, explicit ignored-field skip
facts for non-output maps, and capacity hints for the large arrays. Generate it
through the existing `DirectSchemaSet -> schema_direct -> json_typed_direct`
path; do not add a BBNF directive, BIR variant, bench-private Track 1 parser,
or checksum-only parse result.

Pass condition:

- Correctness parity across generated Track 1 typed output, structurally
  independent Track 2 typed oracle, sonic-rs typed serde, and serde_json typed
  serde.
- Same-plane `mesh real_typed_struct` generated Track 1 is within `1.10x`
  sonic-rs time.
- A profile shows typed generated symbols (`parse_type_mesh`,
  `parse_vec_f64`/typed vector materializers, skip helpers) replacing
  `JsonDigestSink` symbols. `number::materialize_f64` may remain, but
  `JsonDigestSink::array_string` and digest parent folding must be absent from
  the typed row.
- Existing `semantic_full_digest_stressor` rows remain correctness-green and
  are still reported separately; no guard direct digest row regresses by more
  than 5 percent in the same build.

Fail condition:

- The typed row still profiles as generic `JsonDigestSink` / digest folding,
  misses sonic-rs `1.10x` slack, or requires a hidden JSON-only generic-crate
  rule-name path.
