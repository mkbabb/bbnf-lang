# SK-V6 R3b Competitor Shape: Strict String/Object Hot Paths

Date: 2026-05-14
Workspace: /Users/mkbabb/Programming/bbnf-lang
Output: /tmp/skv6-R3b-competitor-shape.md

No network was used. No repository files were edited, staged, or committed.

## Scope and comparator plane

This pass compares strict-vs-strict local source shapes only. I did not re-open permissive asmjson as a close target. The strict local comparator sources available in the Cargo registry were:

- sonic-rs 0.5.8: /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8
- simd-json 0.13.11: /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11

The current retained parse G rows in /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md show sonic-rs as the strict speed anchor for the requested retained rows:

| row | class | retained Track 1 Mbps | sonic-rs Mbps | Track 1 / sonic |
| --- | --- | ---: | ---: | ---: |
| twitter | G | 12303 | 21176 | 58.1% |
| citm_catalog | G | 20775 | 25413 | 81.8% |
| apache_builds | G | 12341 | 17453 | 70.7% |
| github_events | G | 13161 | 23219 | 56.7% |
| update_center | G | 9430 | 19835 | 47.5% |
| random | G | 7794 | 15451 | 50.4% |
| gsoc-2018 | G | 21907 | 48816 | 44.9% |
| instruments | G | 11887 | 19714 | 60.3% |
| unicode_mixed | G | 8720 | 15681 | 55.6% |
| unicode_escapes | G | 12848 | 19090 | 67.3% |
| unicode_basic | G | 10898 | 15753 | 69.2% |
| distinct_values | G | 6097 | 17828 | 34.2% |
| y_string_unicode | G | 6084 | 13633 | 44.6% |

Candidate 1/2 string routes failed gates, so this report treats string materializer shape as evidence but does not re-propose REDRESS 54/55 forms.

## Our generated retained parser shape

Relevant local files:

- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/parser.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/scan.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/tape/assembler.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/aarch64/string_block.rs

Current generated retained JSON parsing is recursive-descent over source bytes. It repeatedly performs local whitespace and delimiter discovery at each syntactic edge:

- `parse_json` -> `parse_value` -> `parse_value_at` -> `dispatch_value`.
- Objects loop through `parse_pair`, `parse_key_colon`, value dispatch, then `consume_container_next`.
- Arrays loop through value dispatch, then `consume_container_next`.
- String values and keys use `match_tiny_plain_string` first, then `match_string_at_quote`, which calls `match_json_string_at_quote_trusted_utf8`.
- Structural and primitive tape events are emitted one by one through `ParserState::emit_plain_offset`, which calls `TapeBuilder::push_plain_offset`.

The retained scanner in `scan.rs` can produce a structural index and SIMD quote/string masks, but the default generated retained parser does not consume that structural stream as its parse substrate. In the common GrowOnly plan it mostly benefits capacity planning, not control flow. The hot parser still discovers whitespace, colons, commas, closes, and quote boundaries again from the source bytes.

For strings, our trusted UTF-8 matcher already has a useful strict fast path: on aarch64 it scans 16-byte blocks for quote, backslash, and control bytes, then uses 8-byte SWAR and scalar tails. This is close in spirit to competitor string-special scans. The missing part is not simply more string scanning; it is that object/list control flow still pays many byte-local checks around strings and retained tape emission.

## sonic-rs shape

Primary files inspected:

- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/parser.rs
- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/util/string.rs

Observed hot strategy:

1. String body classification is vectorized and branch-light.
   - `StringBlock` classifies quote, backslash, and control bytes in a SIMD block.
   - The parser asks cheap questions such as quote-before-backslash/control and only falls into escaped handling on an actual backslash.

2. Plain strings are first-class borrowed/inplace spans.
   - `parse_string_raw` can return a borrowed slice for the common quote-first path.
   - `parse_string_inplace` advances through plain strings without copying, then mutates/copies only for escaped strings.

3. Object parsing is visitor-directed and typed.
   - Object loops parse key strings, consume compact colon fast paths, dispatch typed values, and call the visitor directly.
   - For serde direct parse, sonic-rs is not building our retained offset tape first; it can turn successful parse discoveries directly into the target visitor shape.

4. It has a structural string mask utility, but its serde shape is not a retained structural-index machine.
   - The important strict lesson for our retained path is not to mimic its exact API, because our Track 1 retained parser must produce a tape.
   - The useful lesson is that string/object hot paths avoid rediscovering the same boundaries and keep copied/escaped materialization out of the plain-string path.

Implication for BBNF retained parser: sonic-rs explains part of the gap on string-heavy rows, but it also benefits from direct typed visitation and no retained tape emission. A retained baseline should not expect to match sonic-rs by string decoding alone. It must make object/list event discovery and tape emission cheaper too.

## simd-json shape

Primary files inspected:

- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11/src/lib.rs
- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11/src/impls/neon/stage1.rs
- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11/src/stage2.rs
- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11/src/stringparse.rs
- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11/src/serde.rs
- /Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.13.11/src/value.rs

Observed hot strategy:

1. Stage 1 builds the control-flow substrate.
   - `find_quote_mask_and_bits` computes quote masks, quote bits, and control-byte error masks.
   - `find_whitespace_and_structurals` uses SIMD classification for whitespace and structural bytes.
   - `finalize_structurals` masks structural bytes inside strings and adds pseudo-structurals for atom starts.
   - `flatten_bits` writes structural positions into a compact `Vec<u32>` stream.

2. Stage 2 parses from the structural stream.
   - `build_tape` reserves tape and stack from structural index length.
   - Object and array state machines advance by structural index, not by repeated source-byte whitespace scans.
   - Key/value transitions expect colon/comma/close events from the stream and only touch source bytes for token validation/materialization at event positions.

3. Strings are parsed at structural quote events.
   - Stage 1 has already identified quote positions and protected structural characters inside strings.
   - Stage 2 calls string parsing at those event boundaries, then inserts string nodes into the tape.

4. API caveat: simd-json's borrowed/serde paths can rewrite the input buffer for de-escaping. That differs from our immutable `&str` retained parser contract. The shape comparison is still useful, but it is not a drop-in close-target claim.

Implication for BBNF retained parser: simd-json's large advantage is not just SIMD string validation; it makes the structural stream the parser's actual input. That is the strict comparator shape most relevant to retained object-heavy rows such as `distinct_values`, `gsoc-2018`, `github_events`, and `update_center`.

## Diagnosis: what survived from old SinkParser work

Survived:

- String-heavy rows still expose a materialization class. Plain strings need a borrowed/span-first route and escaped paths must remain rare and explicit.
- Object/key-heavy rows still expose field-layout and delimiter cadence costs. Repeated colon/comma/close/whitespace discovery is expensive when the document is dominated by shallow records and keys.
- Per-event retained emission remains visible as an architectural cost even when each push is cheap. Tape construction needs reserve/batch opportunities tied to discovered events.

Invalidated or weakened after generated runtime wiring:

- The old bench-private SinkParser diagnosis cannot be treated as direct evidence for Track 1 anymore. The new generated retained parser reaches `runtime::generated_json::parse_direct` and has different call boundaries, string matchers, tape builders, and capacity planning.
- A sink-only novelty conclusion that blamed callback/receiver hooks does not transfer directly. Track 1 retained parse emits an offset tape, not a bench-private sink API, so hook names are not the right explanatory target unless profiles show them in the generated runtime path.
- Re-proposing Candidate 1/2 string routes is not justified. The competitors confirm that string shape matters, but the retained G gap is broadest on object/list structural cadence and tape substrate, not only on escaped string materialization.

## Grammar-general lessons

1. Make structural discovery the parser substrate, not a sidecar.
   For grammars with delimiters, quotes, comments, or nested containers, a strict scanner can classify event positions once. The parser should consume those events directly instead of using them only for capacity planning or occasional lookup.

2. Keep semantic materialization separate from event discovery.
   Structural events should drive object/list/control flow. String, number, literal, identifier, and escape validators should run only at event boundaries.

3. Give plain quoted spans a zero-copy contract.
   For any grammar with quoted tokens or escaped identifiers, the common route should be quote-first/plain-span detection. Escape decoding should be an uncommon branch triggered by an actual escape marker.

4. Optimize delimiter cadence, not only payload tokens.
   Object/map/list grammars spend substantial time on colon/comma/close/whitespace cadence. A parser that consumes delimiter events has fewer branchy local byte probes than recursive descent that rediscovers separators after every child.

5. Batch or pre-reserve retained emission from event counts.
   Any retained-token grammar can reserve and write token offsets from structural/event cardinality. Per-event `push` checks should not be on the dominant path when the event stream length is known or cheaply estimated.

## Concrete candidate: single-substrate structural-event retained parser

Candidate shape: consume a strict structural-event stream as the retained parser's control-flow substrate, while preserving the existing offset tape and generated runtime API.

Constraints satisfied:

- No directives.
- No new BIR variants.
- No parallel substrate.
- No public retained tape format change.
- No REDRESS 54/55 string materializer re-proposal.

Implementation sketch:

- Reuse the existing JSON scanner machinery in `scan.rs`, but change its role for retained JSON from capacity helper to parser substrate.
- The scanner should feed real structural events and pseudo-structural starts to a private event cursor/callback consumed by generated parsing code.
- The generated parser should advance through this event cursor for object/array/key/value transitions. Source bytes should be touched only when validating strings, numbers, literals, and exact token bytes at event positions.
- Tape output remains the existing `OffsetTape`. If needed, add only writer helpers that batch or extend plain offsets from event positions; do not add a second retained side table.

Likely file paths touched if implemented:

- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/scan.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/parser.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/tape/assembler.rs
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/json_templates/generated.rs, or the equivalent generator source that emits the retained JSON parser
- /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/profile_direct.rs only if instrumentation needs to name the candidate path; not required for the algorithm

Falsifiability rows/gate:

| row | current retained Track 1 Mbps | candidate gate |
| --- | ---: | --- |
| distinct_values | 6097 | >= 8000 Mbps and >= +25% vs current |
| gsoc-2018 | 21907 | >= 30000 Mbps and >= +25% vs current |
| github_events | 13161 | >= +15% vs current |
| update_center | 9430 | >= +15% vs current |
| unicode_mixed | 8720 | no regression >5%; not a primary success row |
| unicode_escapes | 12848 | no regression >5%; not a primary success row |
| canada / mesh / marine_ik / numbers | retained non-G rows | no regression >5% |

Correctness gate:

- Existing strict JSON conformance must stay green.
- The generated retained parser must still reject trailing non-whitespace, unterminated strings, invalid controls, invalid escapes, invalid numbers, and container delimiter mismatches.
- Profiles must show the Track 1 path reaching `runtime::generated_json::parse_direct` or equivalent generated retained entry. Success is invalid if the improvement comes from a bench-private SinkParser path.

Expected profile signature if the candidate is real:

- Less self-time in repeated `skip_json_whitespace`, `consume_container_next`, `consume_structural`, and key-colon byte probing.
- More time concentrated in event scanning, string/number validation, and offset tape writes.
- No new dominant named receiver hook or generated SinkOnly emission symbol above 10% self-time.
- String-heavy rows may improve modestly, but object/list structural rows should move first.

## Bottom line

The strict competitors point to one retained-parser issue that is broader than JSON strings: source-byte recursive descent is rediscovering a structural event stream that the runtime already knows how to classify. Sonic-rs proves the value of plain-span string routes and direct typed visitation, but its serde shape is not retained. Simd-json proves the value of making structural positions the parse substrate, but its two-stage retained index and input-rewrite API should not be copied wholesale.

The concrete SK-V6 Wave 1b candidate is therefore a single-substrate structural-event retained parser over the existing offset tape: use strict event discovery once, consume those events for generated object/list control flow, and keep materialization validators at event boundaries. It is falsifiable on `distinct_values`, `gsoc-2018`, `github_events`, and `update_center`; it should be dropped if those rows do not move without harming retained non-G rows.
