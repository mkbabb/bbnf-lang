# SK-V6 R2f Direct Digest Pathology

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only repository/source inspection plus existing SK-V6 reports. No repository files edited.

## Required Sources Read

- `skinny/REDRESS.md` entries 66-68.
- `skinny/RESULTS.md` retained/direct matrices and materialization notes.
- `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`.
- `skinny/crates/runtime/src/grammars/json/generated.rs`.
- `skinny/crates/runtime/src/grammars/json/sink.rs`.
- Supporting prior read-only reports: `restart/skinny/tranches/sk-v6/research/skv6-R3-direct-attribution.md`, `skv6-R1d-direct-escaped-materialization.md`, `skv6-R2e-unescape-distribution.md`, and `skv6-R3d-direct-generality.md`.

## Current Direct Digest Semantics

Track 1 direct is genuinely generated now. `bbnf_bench::direct_struct::track1_digest` constructs `JsonDigestSink` and calls `runtime::generated_json::parse_direct(input, &mut sink)`. Track 2 is the independent hand parser. `profile_direct` dispatches directly to `track1_digest`, `track2_digest`, `sonic_digest`, or `serde_digest`.

The timed direct workload is not only structural. It computes a full synthetic semantic digest:

- every string value increments string counts and byte counts and hashes semantic decoded UTF-8 bytes with `hash_bytes(value.as_bytes())`;
- every object key contributes semantic decoded byte length and `hash_bytes` to the object fingerprint;
- numbers are materialized into i64/u64/f64 classes, including `-0.0` handling;
- containers fold child digests and member/element counts.

Generated `parse_direct` returns `ParsedString { raw, needs_unescape }`. The default `JsonSink::*_source` methods then call `unescape_json_string(raw)` only when `needs_unescape` is true, and finally deliver a semantic `&str` to the digest sink. Plain strings are borrowed. Therefore the current direct representation boundary is semantic `&str`, not a lazy string span, string atom, or consumer-specific field fact.

## Focused Rows

Current `skinny/RESULTS.md` direct rows:

| row | Track 1 | Track 2 | sonic-rs | Track 1 / sonic |
|---|---:|---:|---:|---:|
| `unicode_escapes` | 5143 | 5030 | 14485 | 35.5% |
| `unicode_mixed` | 3881 | 4137 | 10142 | 38.3% |
| `y_string_unicode` | 3674 | 3679 | 8676 | 42.3% |
| `distinct_values` | 6072 | 5563 | 13185 | 46.1% |
| `gsoc-2018` | 15013 | 14458 | 24163 | 62.1% |

Existing attribution explains the split:

- `unicode_escapes`: generated direct profile was 46.9% `unescape_json_string` and 43.4% `parse_string_direct`. The fixture has 5,636 strings, 1,877 escaped strings, 230,134 backslashes, and 136,682 `\u` units. This is strict escaped-string semantic materialization plus the string scanner.
- `unicode_mixed`: 51.1% `parse_string_direct`, 22.8% `unescape_json_string`. The fixture has 25,121 strings but only 1,959 escaped strings, all simple escapes and no `\u`. This is mostly string recognition/control plus a smaller materialization component.
- `y_string_unicode`: direct attribution is noisy because the file is only 35,601 bytes, but the active application leaves are `unescape_json_string` and `parse_string_direct`; malloc/system time is also large. It is materialization/allocation exposed by tiny-corpus repetition.
- `distinct_values`: attribution shows `parse_object_direct` 36.0%, `parse_string_direct` 22.1%, `JsonDigestSink::array_string` closure 19.9%, and `match_tiny_plain_string` 14.7%. This row is not escaped decode dominated. It is high-cardinality plain string recognition plus digest/fold work.
- `gsoc-2018`: attribution shows `parse_string_direct` 41.9%, `fold_string_scalar` 20.7%, `unescape_json_string` 10.9%, and `match_tiny_plain_string` 9.8%. This is large string scanning plus digest folding; escaped materialization exists but is not the whole miss.

## Rejected Explanations

Receiver/hook overhead is not the main cause. REDRESS 66 added direct source hooks and folded strings/keys directly into digest frames while keeping `unescape_json_string`; it moved `unicode_escapes` only +0.99%, `unicode_mixed` +0.11%, `y_string_unicode` +1.75%, `distinct_values` +1.54%, and `gsoc-2018` -0.01%.

Allocator reuse / parser-owned scratch is not the close. REDRESS 67 threaded one reusable `String` scratch through generated direct parsing and regressed `unicode_escapes` by 44.03%, `unicode_mixed` by 4.91%, and `y_string_unicode` by 16.76% in the stopped smoke.

Byte-output inside the existing `Cow<str>` API is not the close. REDRESS 68 changed escaped-string writes to a byte buffer and regressed `unicode_escapes` by 4.00%.

A pure digest-hash explanation is incomplete. `hash_bytes` and `fold_string_scalar` are visible on `distinct_values`, `gsoc-2018`, and other plain-string rows, but the Unicode rows are dominated by `parse_string_direct` and `unescape_json_string`. Also, sonic/serde anchors deserialize into the same `JsonDirectDigest`, so the digest operation is part of the common semantic workload, though bbnf reaches it through a less favorable representation boundary.

Generated parser control is real but secondary. It is visible in `parse_object_direct`, `parse_string_direct`, and context-specific value functions, especially on `distinct_values` and `gsoc-2018`. However, attempts that only changed receiver/control shape did not move the rows enough, and parser-owned decode made escaped rows worse. The control cost matters because it is coupled to the current semantic-string boundary, not because dispatch alone is the root cause.

## Diagnosis

The remaining focused N-direct gap is best classified as a field-layout / representation mismatch that manifests as strict string materialization on escaped rows and digest/fold pressure on plain-string rows.

The current direct field contract is too late and too semantic: generated direct parsing emits raw string spans plus a `needs_unescape` flag, but the sink contract immediately collapses that into `&str`. The digest then consumes all string/key bytes as if the target typed layout required fully materialized string payloads. For `unicode_escapes` and `y_string_unicode`, that means strict escaped-string materialization dominates. For `distinct_values` and `gsoc-2018`, the same contract turns high-cardinality strings into scanner plus hash/fold work even when the timed workload is only proving a digest representation.

So the gap is not explained by one local component. It is not merely strict string materialization, not merely digest hashing, and not merely generated parser control. The common cause is the current DirectBuild/SinkOnly field representation: strings are represented as semantic `&str` events instead of as consumer-typed direct fields.

## Recommended Intervention

Admit exactly one representation-level candidate: a DirectBuild string-field representation fact that lets generated SinkOnly lower string consumers as typed semantic string facts, not as generic `&str` sink events.

Concrete shape:

- Extend the direct field-fact layer, not BIR variants or user directives, so a string field can declare the materializer it actually needs: `BorrowedStr`, `OwnedDecodedStr`, or `SemanticStringFact { decoded_len, fingerprint, equality_oracle }`.
- For the direct digest workload, generated Track 1 may consume `SemanticStringFact` fields for keys and string scalar values, because the timed representation only needs semantic byte length and semantic fingerprint. The untimed parity oracle must still be able to materialize and compare full semantic strings against serde/sonic shape where needed.
- The implementation must be generated from DirectBuild field facts and consumed in the same generated SinkOnly loop. Do not add `JsonDigestSink::*_source` overrides, parser-owned reusable scratch, byte-output `unescape_json_string`, sink-local decoded stats helpers, or quote-source streaming hash under another name.
- The same field-fact mechanism must be data-driven enough to support non-digest consumers; otherwise it is just the rejected digest-specific sink helper again.

This is admissible because it changes the representation contract at DirectBuild field selection, which REDRESS 68 explicitly leaves as the remaining route, instead of retesting the exhausted allocation / receiver / byte-writer family.

## Exact Measurement Gate

Correctness gate:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv6-R2f-direct-fieldfacts-correctness cargo test -p runtime --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-R2f-direct-fieldfacts-correctness cargo test -p bbnf-bench --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-R2f-direct-fieldfacts-correctness cargo run -p xtask --release -- check-json
CARGO_TARGET_DIR=/tmp/skv6-R2f-direct-fieldfacts-correctness cargo run -p xtask --release -- check-conformance
```

Throughput gate: same-tree baseline/candidate production `profile_direct` Track 1 medians, five samples per row, no `runtime/parse-attribution` feature.

Focus rows and required median lift:

- `unicode_escapes`: at least +20%.
- `unicode_mixed`: at least +15%.
- at least two of `y_string_unicode`, `distinct_values`, and `gsoc-2018`: at least +8% each.
- no guard row among `twitter`, `apache_builds`, `github_events`, `unicode_basic`, `canada`, and `numbers` may regress more than 3%.

Profile gate: parse-attribution samply on `unicode_escapes`, `unicode_mixed`, `distinct_values`, and `gsoc-2018` must show one of these two outcomes:

- `parse_string_direct + unescape_json_string + fold_string_scalar/hash_bytes` combined self-time drops at least 20% relative on `unicode_escapes` and 15% relative on `unicode_mixed`; or
- the new generated field-fact materializer accounts for the removed self-time while row throughput clears the throughput gate.

Reject immediately if the diff is primarily a `JsonDigestSink` source-hook override, an `unescape_json_string` writer rewrite, parser-owned scratch, or a raw-source streaming hash helper without DirectBuild field-fact authority.
