# SK-V4 Re-Assay Profile Report

Date: 2026-05-13.

Scope: current-binary profile triage after the SK-V3 direct-sink redress and
before the SK-V4 implementation packet. This file consolidates the existing
repository profiles plus the fresh A5 short Samply pass captured under
`/tmp/bbnf-a5-profiles`.

The current gate authority remains `skinny/RESULTS.md`: parse-G rows are
`twitter`, `random`, `unicode_mixed`, and `unicode_basic`; direct-to-struct is
correctness-green but `N-direct / NoGo` because direct rows pass 6 of 17 and
fail 11 of 17 against sonic-rs direct.

## Profile Planes

Three profile planes matter for the next implementation pass.

1. `parse_only`: `profile-lazy` over `runtime::generated_json::parse`.
2. `direct_to_struct`: `profile_direct` over the BBNF `SinkOnly` digest path.
3. `native_sidecars`: yyjson, simdjson C++, sonic-rs, and asmjson sidecars
   recorded under `skinny/profile/native-sidecars/`.

The fresh A5 pass did not replace the full sidecar authority. It answers only
"what is hot in the current binary right now?"

## Fresh A5 Hot Leaves

Direct sink, Track 1:

| Corpus | Top self-time leaves | Reading |
|---|---|---|
| `twitter` | `SinkParser::string` 62.3%, `SinkParser::value` 19.8%, `SinkParser::object` 16.3% | Current direct miss is string-bound. |
| `numbers` | `SinkParser::value` 62.1%, `serde_json::parse_number` 23.4%, `serde_number_digest` 12.6% | Exact number materialization remains the blocker. |
| `unicode_mixed` | `SinkParser::string` 81.4%, `SinkParser::value` 12.7%, `SinkParser::object` 3.0% | Unicode/string decode quality dominates. |
| `random` | `SinkParser::string` 65.1%, `SinkParser::value` 19.1%, `SinkParser::object` 15.8% | Direct miss is again string-bound. |

Parse-only lazy path:

| Corpus | Top self-time leaves | Reading |
|---|---|---|
| `twitter` | `runtime::generated_json::generated::parse_value_at` 99.7% | Symbol-level Samply is too fused; no-inline or PC-level attribution is needed. |
| `numbers` | `parse_value_at` 97.2%, `_platform_memmove` 1.3%, `TapeBuilder::new` 0.7% | Number-heavy parse no longer shows allocation as top cause. |
| `unicode_mixed` | `parse_value_at` 99.5% | The source-byte parse hub still hides string leaf detail. |
| `random` | `parse_value_at` 99.6% | The next parse profile must break the hot hub apart. |

## Profile Interpretation

The direct path has moved past the old retained-view penalty and duplicate
UTF-8 validation. It is now two concrete primitives:

- string/Unicode materialization for `twitter`, `unicode_mixed`, and `random`;
- exact number materialization for `numbers`, `canada`, `mesh`, and other
  numeric rows.

The parse path is too fused to diagnose by symbol names alone. The SK-V4 pass
therefore must add a no-inline diagnostic build or address-map report around
`parse_value_at` before prescribing another kernel. A new primitive that is
checkasm-green but leaves `parse_value_at` at ~100% self-time has not proven
anything about the current parse-G rows.

## Reproduction Commands

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
mkdir -p /tmp/bbnf-a5-next/direct /tmp/bbnf-a5-next/parse

cargo build --release -p bbnf-bench --bin profile_direct
cargo build --release -p xtask --bin profile-lazy

samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/direct/twitter.track1.profile.json.gz ./target/release/profile_direct 10000 twitter track1
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/direct/numbers.track1.profile.json.gz ./target/release/profile_direct 50000 numbers track1
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/direct/unicode_mixed.track1.profile.json.gz ./target/release/profile_direct 4000 unicode_mixed track1
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/direct/random.track1.profile.json.gz ./target/release/profile_direct 15000 random track1

samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/parse/twitter.profile.json.gz ./target/release/profile-lazy 10000 twitter
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/parse/numbers.profile.json.gz ./target/release/profile-lazy 50000 test_data/numbers.json
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/parse/unicode_mixed.profile.json.gz ./target/release/profile-lazy 4000 test_data/unicode_mixed.json
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open -o /tmp/bbnf-a5-next/parse/random.profile.json.gz ./target/release/profile-lazy 15000 test_data/random.json
```

## Binding Next Work

- Direct `SinkOnly`: generated runtime/codegen path, exact float materializer,
  and string/Unicode decode primitives. Bench-owned sink parsers are no longer
  sufficient as Track 1.
- Parse `OffsetTape`: no-inline/PC-level `parse_value_at` attribution, then
  event-cursor consumption or primitive routing based on the attributed leaf.
- Sidecars: keep yyjson/simdjson/asmjson as comparators and flaw probes, but do
  not accept asmjson's permissive strictness plane as a strict JSON win.
