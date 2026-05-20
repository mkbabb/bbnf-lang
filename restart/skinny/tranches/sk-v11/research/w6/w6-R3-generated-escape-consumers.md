# SK-V11 W6 R3 - Generated Escape Consumers

Scope: generated JSON direct and generated real-typed escaped-string consumers
for W6. This is read-only diagnosis; no source or other docs were edited.

## Disposition

W6 has a real target surface, but the generated JSON/direct consumer route is
not currently a clean one-file/codegen source delta. The generated direct parser
centralizes escaped strings as `ParsedString { raw, needs_unescape }`, then
hands decoding to `JsonSink::*_source`. Those default decode methods are in
`skinny/crates/runtime/src/grammars/json/sink.rs`, which is not in the W6 owner
path list in SPEC §10 (`restart/skinny/tranches/sk-v11/SPEC.md:600-609`).

Therefore a W6 plan that adds trait-level escaped-segment consumer methods must
either get CHALLENGE approval to expand the owner set to include
`runtime/src/grammars/json/sink.rs`, or avoid trait changes and accept that it
is still reusing the existing `unescape_string` production path. SPEC §10
pre-blocks that reuse as an admit (`restart/skinny/tranches/sk-v11/SPEC.md:645-646`).

## Binding W6 Frame

- W6 owner paths are parse-that, aarch64 `unescape_uxxxx`, SIMD tests, codegen,
  generated JSON, direct struct, generated real-typed, `json_parity`, RESULTS,
  and REDRESS (`restart/skinny/tranches/sk-v11/SPEC.md:600-611`).
- Entry requires W5 disposition or CHALLENGE acceptance of an independent
  segment plan, and the plan must name a source delta beyond the
  already-consuming `unescape_string` path
  (`restart/skinny/tranches/sk-v11/SPEC.md:613-615`).
- Tasks are scalar escaped-segment visitor or hex-run oracle, optional x4 scalar
  oracle/checkasm, and a new direct/typed/non-JSON escaped-segment consumer
  (`restart/skinny/tranches/sk-v11/SPEC.md:617-623`).
- Exit rows are selected direct rows from `unicode_escapes`, `unicode_mixed`,
  and `y_string_unicode`; x4 proof alone cannot admit production
  (`restart/skinny/tranches/sk-v11/SPEC.md:625-636`).

## Generated JSON Direct Callers

Generated Track 1 direct enters through
`runtime::generated_json::parse_direct` from `direct_struct::track1_digest`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:401-405`). The generated entry
dispatches root strings to `parse_string_direct`, then calls
`sink.string_source(value.raw, value.needs_unescape)`
(`skinny/crates/runtime/src/grammars/json/generated.rs:409-443`;
template: `skinny/crates/codegen/src/sink_direct.rs:96-140`).

Object string values use the same helper and call `sink.object_string_source`
(`skinny/crates/runtime/src/grammars/json/generated.rs:468-483`; template:
`skinny/crates/codegen/src/sink_direct.rs:165-180`). Array string values use
`sink.array_string_source` (`skinny/crates/runtime/src/grammars/json/generated.rs:508-523`;
template: `skinny/crates/codegen/src/sink_direct.rs:205-220`). Object keys use
`sink.key_source` (`skinny/crates/runtime/src/grammars/json/generated.rs:548-564`;
template: `skinny/crates/codegen/src/sink_direct.rs:251-267`).

The central generated helper first tries an 8-byte plain-string shortcut, then
falls back to `match_string_at_quote_trusted_utf8`. It returns the raw content
slice plus `span.needs_decode()` (`skinny/crates/runtime/src/grammars/json/generated.rs:610-640`;
template: `skinny/crates/codegen/src/sink_direct.rs:315-350`). The decode itself
does not happen in generated.rs.

The decode currently happens in the `JsonSink` trait defaults:
`key_source`, `string_source`, `array_string_source`, and
`object_string_source` call `unescape_string(raw)` when `needs_unescape` is true
(`skinny/crates/runtime/src/grammars/json/sink.rs:17-31`,
`:44-47`, `:85-88`). That file is an observed owner requirement for any
trait-level W6 consumer, but it is absent from SPEC §10's owner list.

## Independent Direct Track 2

Track 2 is hand-coded in `direct_struct.rs` and independently parses both
string values and object keys through `HandParser::string`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:472-474`, `:497-500`).
Its string path mirrors Track 1: tiny plain scan, trusted full matcher, then
`unescape_string(raw)` on `span.needs_decode()`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:541-560`). Its tiny cap is 8
bytes (`skinny/crates/bbnf-bench/src/direct_struct.rs:564-576`).

Any direct row admission must move both Track 1 and independent Track 2. A
Track 1-only generated change will not close rows whose Track 2 floor remains
below gate.

## Generated Real-Typed Consumers

Typed codegen imports `match_string_at_quote_trusted_utf8` and
`unescape_string` (`skinny/crates/codegen/src/typed_direct.rs:21-29`). Struct
field dispatch, scalar string values, map keys, and map-entry keys all funnel to
`parser.parse_string()` (`skinny/crates/codegen/src/typed_direct.rs:93-95`,
`:198-202`, `:332-340`, `:352-356`).

The generated typed `DirectParser::parse_string` has a release-mode quote guard,
a 32-byte tiny plain-string shortcut, and then calls
`match_string_at_quote_trusted_utf8`; if `span.needs_decode()` it materializes
with `unescape_string(raw)` (`skinny/crates/codegen/src/typed_direct.rs:479-500`).
The checked-in generated output matches that shape
(`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1648-1670`), with 32-byte
tiny scan and 96-byte skip scan (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811-1835`).

The current typed fixture set is only `twitter`, `apache_builds`,
`citm_catalog`, `github_events`, `update_center`, `mesh`, and `marine_ik`
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:311-320`). `json_parity`
only emits typed benchmarks when `fixture_for_name` returns one of those rows
(`skinny/crates/bbnf-bench/benches/json_parity.rs:261-345`). There is no typed
fixture for `unicode_escapes`, `unicode_mixed`, or `y_string_unicode`, so typed
escape work is currently a guard/probe surface, not a W6 direct-row admission
surface. The existing W6-named typed test is full `github_events` parity
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:858-863`), which has only
155 backslashes and zero `\uXXXX` escapes in the fixture by local count.

## Current Escape Engine

`match_string_at_quote_trusted_utf8` sets `needs_unescape` when it sees a
backslash and validates escapes before returning the raw span
(`skinny/crates/parse-that-regex/src/lib.rs:162-190`). JSON escape validation
routes `\u` through `validate_unicode_escape_run`
(`skinny/crates/parse-that-regex/src/lib.rs:283-293`, `:347-365`).

`unescape_string` is the current materializer. It returns a borrowed string on
the no-backslash path, otherwise builds an owned `String`
(`skinny/crates/parse-that-regex/src/lib.rs:718-728`). On aarch64 it already
tries `unescape_four_unicode_escapes` before falling back to scalar
`decode_unicode_escape` (`skinny/crates/parse-that-regex/src/lib.rs:775-785`).
That helper packs four consecutive `\uXXXX` quartets and calls
`unescape_uxxxx_x4_neon` (`skinny/crates/parse-that-regex/src/lib.rs:386-403`).
The SIMD primitive itself decodes four quartets with TBL/range checks
(`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:123-166`).

This is the core W6 trap: x4 Unicode decode is already in the production
`unescape_string` route. W6 cannot honestly admit by only proving or re-gating
that same route; P3 pre-blocks x4 proof-to-production promotion through
`unescape_string` (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:90-94`,
`:190-194`).

## Unicode Rows And Target Ranking

Current direct row facts:

| Row | Track 1 | Track 2 | Sonic strict | W6 floor | Gap that matters |
|---|---:|---:|---:|---:|---:|
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Track 2 short by 161 |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | both tracks short by about 2100 |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | Track 1 short by 1967; Track 2 short by 2921 |

Sources: RESULTS rows (`skinny/RESULTS.md:37-39`, `:44-45`) and SPEC §0.4
floors (`restart/skinny/tranches/sk-v11/SPEC.md:132-135`).

Local fixture counts confirm the shape:

| Fixture | Bytes | Backslashes | `\uXXXX` escapes | Escaped quotes |
|---|---:|---:|---:|---:|
| `unicode_escapes.json` | 1050797 | 230134 | 136682 | 7249 |
| `unicode_mixed.json` | 1053086 | 65489 | 0 | 11917 |
| `y_string_unicode.json` | 35601 | 4600 | 4400 | 200 |

P1 attribution agrees: `unicode_escapes` direct is dominated by `unescape`,
full string, and read-hex leaves; `y_string_unicode` by hex nibble/read-hex and
unescape; `unicode_mixed` by full string, unescape, and escape validation
(`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:162-164`,
`:188`, `:281`).

Recommended target order:

1. `unicode_mixed/direct_to_struct` is the best closure candidate. Track 1 is
   already above floor; Track 2 is short by only 161 Mbps. It is not a clean
   hex-decode row because it has no `\uXXXX` escapes, so the intervention would
   need to be an escaped-segment/no-allocation path, not x4 hex.
2. `unicode_escapes/direct_to_struct` is the best dense `\uXXXX` evidence row,
   but it needs roughly 2.56x Track 1 and Track 2 improvement. Because x4 is
   already in `unescape_string`, a pure hex decoder proof is likely to record an
   uncloseable measurement unless it removes materialization or a second pass.
3. `y_string_unicode/direct_to_struct` is the cleanest one-caller structural
   shape because the fixture is a root array of strings, hitting the array
   string consumer. It is also the least plausible closure row because Track 2
   needs about 3.84x improvement. Use it as an honest hex-run proof/negative
   monitor, not the primary admission bet.

Typed rows should be guard rows only unless W6 adds a new typed Unicode fixture
and benchmark authority. The current typed rows do not cover the W6 Unicode
direct residuals.

## Possible Same-Wave Consumer Shapes

### A. Direct escaped-segment sink, with owner expansion

Add a JSON escaped-segment visitor that preserves current surrogate and error
offset semantics, then add exactly one generated direct consumer helper that
routes `needs_unescape` strings to the sink without materializing an owned
`String`. This is the only shape that plausibly attacks `unicode_mixed` and
`unicode_escapes` beyond the already-wired x4 path.

Required owner adjustment: `runtime/src/grammars/json/sink.rs` must be in the
redress owner set, because current decoding is hidden in `JsonSink::*_source`.
Without this file, the generated parser can only continue to call existing
`*_source` methods or bypass them with ad hoc generated code. The first is
pre-blocked as `unescape_string` reuse; the second risks JSON-specific sink
policy hidden inside generated dispatch.

### B. Array-string-only proof on `y_string_unicode`

Select the generated array string caller
(`skinny/crates/runtime/src/grammars/json/generated.rs:520-523`) and the hand
Track 2 string path (`skinny/crates/bbnf-bench/src/direct_struct.rs:541-560`).
This is a narrow one-caller proof surface and keeps object keys out of scope.
It should be planned as a likely uncloseable measurement because the Track 2
floor gap is very large.

### C. Typed escaped string probe

Use `generated_real_typed::DirectParser::parse_string` as a typed guard
consumer (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1648-1670`) and
mirror the codegen template (`skinny/crates/codegen/src/typed_direct.rs:479-500`).
This is not a direct-row admission route today. A real typed W6 admit would
need a new typed Unicode fixture/root and benchmark authority, which is outside
the current SPEC §10 target rows and likely outside the owner list because
`real_typed_struct.rs` owns fixture mapping and parity.

## Research Recommendation

W6 should not be scoped as "wire x4 into production"; it is already wired under
`unescape_string`. The only non-paper JSON path is an escaped-segment consumer
that removes owned decoded materialization or a second pass, measured through a
direct product row and mirrored in independent Track 2. That path currently
requires either owner-set expansion for `runtime/src/grammars/json/sink.rs` or a
CHALLENGE-approved generated-only bypass that proves it is not a hidden digest
side channel.

For row selection, plan `unicode_mixed/direct_to_struct` first for closure,
measure `unicode_escapes/direct_to_struct` as dense hex evidence, and treat
`y_string_unicode/direct_to_struct` as a narrow array-string proof/likely
uncloseable monitor. Keep typed generated escape changes as guards unless a
separate typed Unicode fixture is explicitly added by the plan.
