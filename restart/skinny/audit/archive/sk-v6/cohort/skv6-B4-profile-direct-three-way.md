# SK-V6 B4 Profile Direct Three-Way

- Baseline: committed `HEAD` `7cac3971666c2d7c1cce628a5bc651463262d23f` from `/Users/mkbabb/Programming/bbnf-lang`. The live checkout had pre-existing dirty `skinny/` files, so this run used an archived committed snapshot at `/tmp/skv6-B4-skinny-committed` and ignored those changes.
- Binary: `cargo build --release -p bbnf-bench --bin profile_direct` in the standalone skinny snapshot. Release profile is `opt-level=3`, fat LTO, `codegen-units=1`, `debug=true`, `strip=false`.
- Profiler: `samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open`. Mbps values are from `profile_direct` timed loops under samply. c/B assumes a 3.5 GHz Apple performance core (`c/B = 28000 / Mbps`).
- Scope: direct-to-struct digest modes for `unicode_escapes`, `numbers`, and `distinct_values`: generated Track1, hand Track2, sonic-rs sidecar, serde_json sidecar.

## Throughput And Hot Leaves

| Corpus | Mode | Bytes | Iters | Wall s | Mbps | c/B | Samples | Hot leaves (self >=1%) |
|---|---|---:|---:|---:|---:|---:|---:|---|
| `unicode_escapes` | Track1 generated direct | 1050797 | 1800 | 3.37 | 4492 | 6.23 | 13492 | 48.0% `parse_that_regex::unescape_json_string`<br>42.8% `generated::parse_object_value_at_direct::<JsonDigestSink>`<br>2.5% `mach_absolute_time`<br>2.0% `JsonDigestSink::array_string::{closure#0}`<br>1.5% `_platform_memmove` |
| `unicode_escapes` | Track2 hand direct | 1050797 | 1800 | 3.41 | 4433 | 6.32 | 13664 | 46.4% `parse_that_regex::unescape_json_string`<br>43.0% `HandParser::string`<br>2.9% `HandParser::value`<br>2.4% `mach_absolute_time`<br>2.1% `_platform_memmove` |
| `unicode_escapes` | sonic-rs sidecar | 1050797 | 3000 | 2.13 | 11848 | 2.36 | 8497 | 71.4% `<sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_escaped_char`<br>13.4% `sonic Deserialize::deserialize_any::<DigestVisitor>`<br>3.4% `<sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_string_escaped`<br>1.4% `Cow<str> DeserializeSeed::deserialize`<br>1.2% `simdutf8::implementation::aarch64::validate_utf8_basic_neon` |
| `unicode_escapes` | serde_json sidecar | 1050797 | 1600 | 2.48 | 5431 | 5.16 | 9951 | 37.0% `serde_json::SliceRead::parse_str`<br>20.5% `serde_json::read::parse_unicode_escape::<serde_json::read::SliceRead>`<br>18.7% `<serde_json::read::SliceRead>::skip_to_escape`<br>9.5% `core::str::converts::from_utf8`<br>4.2% `JsonDirectDigest::deserialize::<serde_json>`<br>3.7% `_platform_memmove` |
| `numbers` | Track1 generated direct | 150124 | 30000 | 3.17 | 11372 | 2.46 | 12623 | 78.6% `generated::parse_array_element_at_direct::<JsonDigestSink>`<br>11.1% `parse_that_regex::number::materialize_f64`<br>10.2% `profile_direct::run_once` |
| `numbers` | Track2 hand direct | 150124 | 12000 | 1.28 | 11266 | 2.49 | 5092 | 89.8% `HandParser::value`<br>10.2% `parse_that_regex::number::materialize_f64` |
| `numbers` | sonic-rs sidecar | 150124 | 30000 | 3.17 | 11377 | 2.46 | 12595 | 99.0% `sonic Deserialize::deserialize_any::<DigestVisitor>`<br>1.0% `simdutf8::implementation::aarch64::validate_utf8_basic_neon` |
| `numbers` | serde_json sidecar | 150124 | 20000 | 3.28 | 7312 | 3.83 | 13049 | 48.9% `<serde_json::de::Deserializer<serde_json::read::SliceRead>>::parse_decimal`<br>25.3% `JsonDirectDigest::deserialize::<serde_json>`<br>12.9% `serde_json::SeqAccess::has_next_element`<br>7.9% `<serde_json::de::Deserializer<serde_json::read::SliceRead>>::parse_integer`<br>5.1% `<serde_json::de::Deserializer<serde_json::read::SliceRead>>::f64_from_parts` |
| `distinct_values` | Track1 generated direct | 153630 | 15000 | 3.22 | 5719 | 4.90 | 12857 | 52.9% `generated::parse_array_element_at_direct::<JsonDigestSink>`<br>26.7% `generated::parse_object_value_at_direct::<JsonDigestSink>`<br>19.9% `JsonDigestSink::array_string::{closure#0}` |
| `distinct_values` | Track2 hand direct | 153630 | 30000 | 7.34 | 5024 | 5.57 | 28878 | 37.7% `HandParser::string`<br>31.9% `HandParser::value`<br>30.5% `HandParser::object` |
| `distinct_values` | sonic-rs sidecar | 153630 | 30000 | 5.58 | 6602 | 4.24 | 22124 | 31.0% `mach_absolute_time`<br>29.8% `sonic Deserialize::deserialize_any::<DigestVisitor>`<br>4.0% `Cow<str> DeserializeSeed::deserialize`<br>3.6% `_platform_memmove`<br>3.6% `libsystem_malloc.dylib!0x2b088`<br>1.7% `libsystem_malloc.dylib!0x2b06c` |
| `distinct_values` | serde_json sidecar | 153630 | 18000 | 4.36 | 5076 | 5.52 | 17333 | 24.6% `mach_absolute_time`<br>15.8% `core::str::converts::from_utf8`<br>14.4% `JsonDirectDigest::deserialize::<serde_json>`<br>7.4% `<serde_json::read::SliceRead>::skip_to_escape`<br>4.3% `serde_json::SliceRead::parse_str`<br>2.7% `serde_json::MapAccess::next_key_seed`<br>2.3% `_platform_memmove`<br>2.1% `libsystem_malloc.dylib!0x2b088`<br>1.3% `<serde_json::de::MapAccess<_> as serde::de::MapAccess>::next_key_seed::has_next_key::<serde_json::read::SliceRead>`<br>1.1% `libsystem_malloc.dylib!0x2a0d4` |

## Profile Paths

| Corpus | Mode | Profile | Symbols | Run log |
|---|---|---|---|---|
| `unicode_escapes` | Track1 generated direct | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.track1.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.track1.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.track1.stderr` |
| `unicode_escapes` | Track2 hand direct | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.track2.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.track2.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.track2.stderr` |
| `unicode_escapes` | sonic-rs sidecar | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.sonic.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.sonic.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.sonic.stderr` |
| `unicode_escapes` | serde_json sidecar | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.serde.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.serde.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/unicode_escapes.serde.stderr` |
| `numbers` | Track1 generated direct | `/tmp/skv6-B4-profile-direct-three-way/numbers.track1.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/numbers.track1.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/numbers.track1.stderr` |
| `numbers` | Track2 hand direct | `/tmp/skv6-B4-profile-direct-three-way/numbers.track2.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/numbers.track2.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/numbers.track2.stderr` |
| `numbers` | sonic-rs sidecar | `/tmp/skv6-B4-profile-direct-three-way/numbers.sonic.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/numbers.sonic.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/numbers.sonic.stderr` |
| `numbers` | serde_json sidecar | `/tmp/skv6-B4-profile-direct-three-way/numbers.serde.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/numbers.serde.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/numbers.serde.stderr` |
| `distinct_values` | Track1 generated direct | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.track1.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.track1.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.track1.stderr` |
| `distinct_values` | Track2 hand direct | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.track2.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.track2.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.track2.stderr` |
| `distinct_values` | sonic-rs sidecar | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.sonic.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.sonic.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.sonic.stderr` |
| `distinct_values` | serde_json sidecar | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.serde.profile.json.gz` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.serde.profile.json.syms.json` | `/tmp/skv6-B4-profile-direct-three-way/distinct_values.serde.stderr` |

## Readout

- `unicode_escapes`: both BBNF direct tracks are dominated by `parse_that_regex::unescape_json_string` plus the generated or hand string/value dispatch path. The sonic sidecar is dominated by `parse_escaped_char`; serde_json splits the same problem across `parse_str`, `parse_unicode_escape`, `skip_to_escape`, and UTF-8 conversion. This is still an escape-decode row, not a structural dispatch row.
- `numbers`: Track1, Track2, and sonic are all around 11.3-11.4 Gbps in this profiled pass. Track1/Track2 hot leaves are the array/value loop plus `materialize_f64`; serde_json spends most self-time in `parse_decimal`, `parse_integer`, `f64_from_parts`, and digest visitor sequencing.
- `distinct_values`: Track1 spends most self-time in generated array/object value loops and the direct `array_string` fold; Track2 spreads across hand `string`, `value`, and `object`; sonic/serde sidecars show meaningful harness/allocator clock noise on this small corpus, but still expose string/deserializer and allocation leaves.

## Support Matrix

| Path | Same-loop scalar-parent folding | Field-layout materializer | Evidence |
|---|---|---|---|
| Track1 generated direct | Supported for scalar values under object/array parents. | Not used for these rows. | `crates/runtime/src/grammars/json/sink.rs` exposes `array_*`/`object_*`; generated direct code calls `object_string_source`, `array_string_source`, `parse_number_object_direct`, `parse_number_array_direct`, and parent scalar methods. `JsonDigestSink` overrides those methods to fold into the current parent frame. |
| Track2 hand direct | Not supported in the same-loop form. | Not supported. | `HandParser::object` and `HandParser::array` call `self.value()?` and then `fold_child`, so scalar children are materialized as child digests before parent folding. |
| sonic-rs sidecar | Not a BBNF sink path. | Not a BBNF field-layout materializer. | `profile_direct` calls `sonic_rs::from_slice::<JsonDirectDigest>`. |
| serde_json sidecar | Not a BBNF sink path. | Not a BBNF field-layout materializer. | `profile_direct` calls `serde_json::from_slice::<JsonDirectDigest>`. |
| Generated typed DirectBuild | N/A to this digest run. | Supported in codegen, but unavailable for `unicode_escapes`, `numbers`, and `distinct_values`. | `emit_json_typed_from_source` lowers through `schema_direct::lower_program` and `json_typed_direct::render`; `real_typed_struct::fixture_for_name` currently supports only `twitter` and `update_center`. |

## Bottom Line

Same-loop scalar-parent folding is real only on generated Track1 direct; it is already active on these three rows and does not close the unicode escape or distinct string rows by itself. Field-layout materialization is a separate generated typed DirectBuild path and is not available for the requested three corpora, so B4 has no evidence that field-layout materialization can be applied to `unicode_escapes`, `numbers`, or `distinct_values` without adding a typed schema for those workloads.
