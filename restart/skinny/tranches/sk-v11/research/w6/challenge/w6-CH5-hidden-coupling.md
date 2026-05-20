# SK-V11 W6 CH5 - Hidden Coupling

Pass: W6 CHALLENGE lens CH5.
Date: 2026-05-20.
Scope: hidden coupling, Track 1 / Track 2 independence, trait-default
interaction, generated parser coupling, gate/report schema coupling, and direct
path coverage for `w6-plan-escaped-segment-digest-fold.md`.
Source edits: none.

## Verdict

REVISE.

The W6 plan has a plausible narrow route: override the four `JsonDigestSink`
source methods in `skinny/crates/bbnf-bench/src/direct_struct.rs` so generated
Track 1 direct parsing can fold escaped raw string slices without the
`JsonSink` trait defaults allocating through `unescape_string`. That route can
stay inside the Section 10 owner set and can cover root strings, object values,
array values, and keys because generated JSON already calls
`string_source`, `object_string_source`, `array_string_source`, and
`key_source`.

It is not redress-ready as written. The plan leaves three CH5-load-bearing
questions undecided: whether Track 2 may share the same escaped-byte helper as
Track 1, how `gate-json` will consume a new `SK-V11-W6` direct row instead of
the existing SK-V10 hardcoded contracts, and how implementation will prove no
generated direct string path silently falls back to the current trait defaults.
Those are hidden-coupling risks, not implementation details.

## Findings

1. Track 1 source-method coverage is viable but must be made fail-closed.

Generated JSON direct calls the source methods at all four string positions:
root string to `sink.string_source`, object string to
`sink.object_string_source`, array string to `sink.array_string_source`, and
object key to `sink.key_source`
(`skinny/crates/runtime/src/grammars/json/generated.rs:440`,
`:480`, `:520`, `:562`). The current `JsonSink` trait defaults decode escaped
strings with `unescape_string` in each corresponding method
(`skinny/crates/runtime/src/grammars/json/sink.rs:17`, `:28`, `:44`, `:85`).
`JsonDigestSink` currently overrides only decoded-value methods such as
`key`, `string`, `array_string`, and `object_string`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:259`, `:301`, `:311`, `:341`,
`:371`).

This means the proposed implementation can avoid editing
`runtime/src/grammars/json/sink.rs`, but only if it overrides all four
`*_source` methods in `JsonDigestSink`. Omitting any one path silently reuses
the default `unescape_string` materializer for that context and replays the
SPEC Section 10 / REDRESS 108 pre-block.

Required revision: the plan must require a direct fixture matrix with escaped
root string, escaped object key, escaped object string, and escaped array
string, and the redress tests must fail if any `JsonDigestSink::*_source`
override is removed or delegates to the trait default on `needs_unescape=true`.

2. Track 2 independence is underspecified.

The plan says Track 2 will use "an independent local string-digest/key-fold
path" but then asks CHALLENGE to decide whether a single local helper in
`direct_struct.rs` may be shared by both tracks. CH5 cannot accept that
ambiguity. If generated Track 1 and hand Track 2 both call the same new
escaped-byte fold helper, the equality check can become Track 1 equivalent to
Track 2 by construction. That is exactly the dishonest shape CH5 exists to
block.

The current hand Track 2 path is independent of generated parsing: it enters
`hand::sink_digest`, parses strings through `HandParser::string`, and currently
materializes escaped strings with `unescape_string`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:408`, `:440`, `:541`, `:557`).
W6 may change that, but it must preserve independence from generated parser and
sink machinery.

Required revision: pick one of these two shapes before redress:

- Preferred: two local implementations. Track 1 source-method overrides use one
  escaped digest fold body; Track 2 uses a separately named hand-parser body
  that does not call the Track 1 body, `JsonDigestSink`, `JsonSink`,
  `track1_digest`, `runtime::generated_json`, generated typed helpers, or a
  generated sink helper.
- Alternative: one shared scalar escaped-segment oracle may exist only as a
  correctness oracle, not as the sole production fold for both measured tracks.
  The plan must then add an explicit independence test proving the measured
  Track 2 hot path does not call the Track 1 production helper.

In either shape, malformed and edge-case fixtures must compare generated Track
1, hand Track 2, `serde_json`, and `sonic-rs`; exact Track 1 / Track 2 digest
equality alone is insufficient if both tracks share the new fold code.

3. The plan must keep the trait-default boundary narrow.

The plan correctly rejects editing
`skinny/crates/runtime/src/grammars/json/sink.rs`; that file is absent from
SPEC Section 10 owner paths. This is important because changing the trait
defaults would alter every `JsonSink` implementation, including consumers not
selected by W6, and would hide the W6 behavior in a generic runtime contract.

Required revision: state that any redress patch touching
`runtime/src/grammars/json/sink.rs` returns REVISE unless the owner set is
explicitly reopened by a new CHALLENGE disposition. The only accepted default
interaction for the current plan is local override in
`JsonDigestSink`.

4. The generated parser coupling is acceptable only as a caller, not as proof of
a new generic parser path.

The generated JSON parser already produces `ParsedString { raw,
needs_unescape }` from `parse_string_direct`
(`skinny/crates/runtime/src/grammars/json/generated.rs:610`). W6 can consume
that existing producer in the direct output sink. But because the plan rejects
codegen and generated runtime edits, it must not claim a new generated parser,
new parse-that kernel, or new SIMD production path. The behavior delta is a
bench direct-output consumer delta.

Required revision: phrase the source delta as "direct digest escaped-segment
consumer in `JsonDigestSink` plus independent hand Track 2 consumer" unless the
plan actually adds a scalar escaped-segment visitor or hex-run oracle in
`parse-that-regex`. Do not claim `unescape_uxxxx_x4_neon`,
`unescape_four_unicode_escapes`, or current `unescape_string` as W6 production.

5. Gate/report consumption is currently too vague and risks stale SK-V10
coupling.

The plan requires `wave_id=SK-V11-W6`, `redress_entry=REDRESS-117`, and a
consumer class naming source-method overrides, but current gate/report code is
hardcoded around earlier contracts. The existing typed W6 validator is for
`json/github_events/real_typed_struct/main`, `REDRESS-105`, and
`SK-V10-W6` (`skinny/crates/bbnf-bench/src/report.rs:1165`, `:1212`).
The generic direct contract rejects gate-only rows and checks REDRESS/wave
provenance, but its helper/test fixtures still use SK-V10 direct admissions
(`skinny/crates/bbnf-bench/src/report.rs:1142`, `:2229`). `gate.rs` also has
SK-V10 direct marker functions for W2/W10, not an SK-V11 W6 unicode-mixed
marker (`skinny/crates/bbnf-bench/src/bin/gate.rs:1010`, `:1023`).

Required revision: the plan must name same-wave gate/report edits that add a
row-specific SK-V11 W6 direct contract for exactly
`json/unicode_mixed/direct_to_struct/main` with:

- `wave_id = SK-V11-W6`;
- `redress_entry = REDRESS-117`;
- output plane `digest`;
- strict direct comparators present;
- `track2_independence_status = independent_verified`;
- a validator-known same-wave consumer class for the direct digest
  source-method overrides;
- both Track 1 and Track 2 floor `2588`;
- rejection tests for `gate_only`, stale `SK-V10-W6`, stale `REDRESS-105`,
  wrong row id, missing comparator, missing Track 2, and floor miss.

Do not reuse the existing SK-V10 W6 typed validator or the SK-V10 direct floor
table for W6. A `unicode_mixed` W6 floor of 2588 is not the same contract as
the older SK-V10 direct floor machinery.

6. The direct path can miss object/array accounting unless Track 2 exposes
separate value and key folds.

In Track 1, source-method overrides can call parent-specific helpers:
`key_source` must update object members and key fingerprint, root
`string_source` must push a root string, `array_string_source` must update array
element state, and `object_string_source` must update object-value state. In
Track 2, the current hand parser uses `string()` for both values and keys, then
separately calls `JsonDirectDigest::string` or `fold_key`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:472`, `:497`). A single
"string digest" helper can easily fold a key as a value or a value as a key.

Required revision: Track 2 must name separate hand-parser operations for
escaped value digest and escaped key fold, or a context-typed helper that cannot
mix those two uses. The fixture matrix must include escaped key versus escaped
value cases with identical decoded text so a context swap is observable in the
digest.

## Required Plan Changes Before ACCEPT

1. Decide the Track 2 independence shape. Do not leave shared-helper acceptance
   to redress.
2. Require all four `JsonDigestSink` source-method overrides and fixture tests
   for root string, object key, object string, and array string.
3. Make `runtime/src/grammars/json/sink.rs` a fail-closed non-owner path for
   this plan.
4. Narrow the claim to a direct digest output consumer unless a real scalar
   escaped-segment or hex-run oracle is added.
5. Specify exact same-wave `gate.rs` / `report.rs` validator changes for the
   SK-V11 W6 `unicode_mixed/direct_to_struct` row and add negative tests for
   stale SK-V10 provenance and Track 2 coupling.
6. Require context-sensitive Track 2 folding so keys and values cannot share a
   hidden digest path incorrectly.

With those revisions, CH5 can re-review for ACCEPT. Without them, W6 risks a
Track 1 equivalent Track 2 artifact, silent fallback to `unescape_string`, or
unconsumed `RESULTS.md` provenance.
