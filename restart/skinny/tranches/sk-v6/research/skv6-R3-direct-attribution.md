# SK-V6 R3 Direct PC Attribution - Track 1 Generated SinkOnly

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Report: `/tmp/skv6-R3-direct-attribution.md`
Profiles: `/tmp/skv6-R3-profiles/*.track1.profile.json.gz`

## Method

Authority read first: SK-V6 implementation prompt sections 1-5/Wave 1+3, `skinny/RESULTS.md` `direct_to_struct`, `skinny/REDRESS.md` entries 54-59/tail, SK-V5 B2 direct attribution, and SK-V5 D5 SinkOnly novelty audit.

Build used:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv6-cargo/R3
cargo build --release -p bbnf-bench --bin profile_direct --features runtime/parse-attribution
```

Profile command shape used for each row:

```sh
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open \
  -o /tmp/skv6-R3-profiles/<corpus>.track1.profile.json.gz \
  /tmp/skv6-cargo/R3/release/profile_direct <iters> <corpus-or-path> track1
```

`update_center` was passed as `test_data/update-center.json` because the fixture file uses a hyphen; its output artifact keeps the requested `update_center` row name. Percentages below are leaf/self samples from the presymbolicated Firefox profile plus `.syms.json` sidecar. Mbps are the `profile_direct` loop Mbps under `samply` and `runtime/parse-attribution`, not Criterion gate Mbps.

## Per-Row Attribution

| Row | Profile | Mbps | Samples | Track 1 path reaches `parse_direct`? | Top direct self symbols | Named generated/sink emission >10% self? |
|---|---|---:|---:|---|---|---|
| twitter | `/tmp/skv6-R3-profiles/twitter.track1.profile.json.gz` | 9878 | 16378 | Yes, 100.0% stack share | `match_tiny_plain_string` 31.4%; `parse_string_direct` 21.1%; `parse_object_direct` 20.1%; `fold_string_scalar` 6.6%; `parse_object_value_at_direct` 5.0% | No. String scan/parser leaves dominate; receiver fold is 6.6%. |
| canada | `/tmp/skv6-R3-profiles/canada.track1.profile.json.gz` | 9112 | 17482 | Yes, 100.0% | `parse_number_array_direct` 49.1%; `parse_array_direct` 20.0%; `materialize_f64` 12.3%; `emit_number_array_direct` 11.2%; `parse_array_element_at_direct` 7.3% | Yes: `emit_number_array_direct` 11.2%. Number materialization is also 12.3%. |
| apache_builds | `/tmp/skv6-R3-profiles/apache_builds.track1.profile.json.gz` | 9739 | 15033 | Yes, 99.9% | `JsonDigestSink::array_string` closure 33.1%; `parse_string_direct` 22.7%; `match_tiny_plain_string` 19.9%; `parse_object_direct` 18.9%; `parse_object_value_at_direct` 3.3% | Yes: receiver/string fold path 33.1%. |
| github_events | `/tmp/skv6-R3-profiles/github_events.track1.profile.json.gz` | 10383 | 14418 | Yes, 99.9% | `match_tiny_plain_string` 36.0%; `parse_string_direct` 21.7%; `fold_string_scalar` 16.7%; `parse_object_direct` 16.4%; `parse_object_value_at_direct` 3.3% | Yes: `fold_string_scalar` 16.7%. |
| update_center | `/tmp/skv6-R3-profiles/update_center.track1.profile.json.gz` | 7079 | 16901 | Yes, 100.0% | `parse_string_direct` 28.5%; `match_tiny_plain_string` 27.6%; `JsonDigestSink::array_string` closure 19.4%; `parse_object_direct` 14.1%; `parse_object_value_at_direct` 3.8% | Yes: receiver/string fold path 19.4%. |
| random | `/tmp/skv6-R3-profiles/random.track1.profile.json.gz` | 6668 | 17128 | Yes, 99.9% | `parse_object_direct` 24.6%; `fold_string_scalar` 22.1%; `match_tiny_plain_string` 18.6%; `parse_string_direct` 16.1%; `parse_object_value_at_direct` 6.4% | Yes: `fold_string_scalar` 22.1%. |
| gsoc-2018 | `/tmp/skv6-R3-profiles/gsoc-2018.track1.profile.json.gz` | 13460 | 17543 | Yes, 100.0% | `parse_string_direct` 41.9%; `fold_string_scalar` 20.7%; `unescape_json_string` 10.9%; `match_tiny_plain_string` 9.8%; `parse_object_direct` 7.7% | Yes: `fold_string_scalar` 20.7%. |
| instruments | `/tmp/skv6-R3-profiles/instruments.track1.profile.json.gz` | 10413 | 16199 | Yes, 99.9% | `parse_object_direct` 32.2%; `match_tiny_plain_string` 30.7%; `parse_string_direct` 11.4%; `parse_number_object_direct` 9.1%; `emit_number_object_direct` 7.0% | No >10%. Number emit is visible but below threshold at 7.0%. |
| unicode_mixed | `/tmp/skv6-R3-profiles/unicode_mixed.track1.profile.json.gz` | 4190 | 17829 | Yes, 100.0% | `parse_string_direct` 51.1%; `unescape_json_string` 22.8%; `match_tiny_plain_string` 4.0%; `parse_object_direct` 3.9%; `fold_string_scalar` 3.5% | No named generated/sink emission >10%. Escaped-string materialization dominates. |
| unicode_escapes | `/tmp/skv6-R3-profiles/unicode_escapes.track1.profile.json.gz` | 4691 | 17260 | Yes, 100.0% | `unescape_json_string` 46.9%; `parse_string_direct` 43.4%; `<u32 as Ord>::max` 2.2%; `parse_object_direct` 0.6%; `parse_number_object_direct` 0.2% | No. The row is overwhelmingly decode/materialization, not a named generated emitter. |
| unicode_basic | `/tmp/skv6-R3-profiles/unicode_basic.track1.profile.json.gz` | 7850 | 17153 | Yes, 100.0% | `parse_string_direct` 26.7%; `match_tiny_plain_string` 26.1%; `fold_string_scalar` 16.6%; `parse_object_direct` 13.2%; `parse_array_direct` 5.0% | Yes: `fold_string_scalar` 16.6%. |
| distinct_values | `/tmp/skv6-R3-profiles/distinct_values.track1.profile.json.gz` | 5423 | 16270 | Yes, 100.0% | `parse_object_direct` 36.0%; `parse_string_direct` 22.1%; `JsonDigestSink::array_string` closure 19.9%; `match_tiny_plain_string` 14.7%; `parse_object_value_at_direct` 5.1% | Yes: receiver/string fold path 19.9%. |
| y_string_unicode | `/tmp/skv6-R3-profiles/y_string_unicode.track1.profile.json.gz` | 3290 | 69099 | Yes, 100.0% | Application leaves: `unescape_json_string` 22.8%; `parse_string_direct` 14.6%; `JsonDigestSink::array_string` closure 3.6%; `parse_array_element_at_direct` 2.4%. System/malloc leaves are large: `mach_absolute_time` 29.3%, unresolved `libsystem_malloc` leaf bucket 25.3%. | No named generated/sink emission >10%. The active cost is escaped-string decode plus allocation/system overhead; the row remains small-corpus noisy even after a longer 200k-iter rerun. |

## Cross-Row Findings

Every requested row now reaches `runtime::generated_json::parse_direct` in the sample stack. This directly verifies the new Track 1 baseline and invalidates any current diagnosis that still assumes the old bench-private `SinkParser` path.

Named generated/sink emission appears above 10% self-time on 8 of 13 rows:

- Number emission: `canada` has `emit_number_array_direct` at 11.2%, with `materialize_f64` another 12.3%.
- String/field receiver folding: `apache_builds` 33.1%, `github_events` 16.7%, `update_center` 19.4%, `random` 22.1%, `gsoc-2018` 20.7%, `unicode_basic` 16.6%, `distinct_values` 19.9%.

Rows without >10% named generated/sink emission are still not clean parser rows:

- `twitter` and `instruments` are generated string/object parse-scanner dominated.
- `unicode_mixed`, `unicode_escapes`, and `y_string_unicode` are escaped-string decode/materialization dominated. `y_string_unicode` additionally exposes heavy allocator/system time, consistent with the default allocate-then-contiguous-hash baseline rather than a named generated emit leaf.

## Comparison To SK-V5 B2

Survived from B2:

- The authority diagnosis survived: old direct Track 1 attribution was non-canonical because it measured a bench-private parser. The current profiles prove the replacement path is generated `runtime::generated_json::parse_direct`.
- The high-level string/Unicode blocker survived. The current leaf names changed, but string-heavy rows still spend their time in `parse_string_direct`, `match_tiny_plain_string`, and `unescape_json_string`; escaped rows are especially dominated by `unescape_json_string`.
- A number/materialization residual survived for `canada`, but it is no longer `serde_json::parse_number`; it is generated `parse_number_array_direct` plus parse-that number materialization and `emit_number_array_direct`.
- The field-layout materializer class is now visible as receiver/digest folding above 10% on many direct rows. This is evidence for the class named in Wave 3 framing, not evidence to reopen REDRESS 54/55 sink-local decoded hash shapes.

Invalid after generated runtime wiring:

- Specific B2 leaves `SinkParser::string`, `SinkParser::value`, and `SinkParser::object` are invalid for the current baseline. They do not describe Track 1 anymore.
- B2's `Track 1 == Track 2 == sink_only_digest` path is invalid for current HEAD. Track 1 calls generated `parse_direct`; Track 2 is the hand-coded parser.
- The old `serde_json::parse_number` / `serde_number_digest` direct-number attribution is invalid for generated Track 1. Eisel-Lemire/parse-that materialization is wired; remaining number cost is in generated number-array parsing/materialization/emission.
- The SK-V5 Wave 3 direct-string sink-local routes remain rejected. Current profiles show escaped-string decode/allocation cost, but they do not overturn REDRESS 54 or 55; no direct row here justifies re-proposing exact decoded stats or quote-source streaming hash.

## No-Workaround Notes

No tracked files were modified, staged, or committed. The workspace already had unrelated dirty tracked files before this dispatch; they were not touched.
