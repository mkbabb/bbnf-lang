# SK-V8 W4 Hardening V1 CH5

Verdict: ACCEPT.

Confidence: 93%.

## Findings

1. The current scalar-parent folding source shape is feasible under the W4
   caps: current diff is 108 insertions / 5 deletions in one bench-private
   file, with no generated/runtime/codegen/generic crate changes.
2. No blocking scalar-parent correctness defect found. Object values now
   dispatch through `value_into_object`, array values through
   `value_into_array`; scalar fingerprints are still
   `mix(parent, mix(tag, value))`, matching the previous
   `fold_child(JsonDirectDigest::scalar(...))` route and generated Track 1
   sink scalar callbacks.
3. Element/member counts look preserved. Object member count remains owned by
   the key path before value parsing; array element count remains owned by the
   array element path before folding, matching the old
   `elements += 1; fold_child(value())` order.
4. `max_depth` looks preserved. Direct scalar folds cap parent depth at least
   `2`, while nested arrays/objects still use `fold_child(child)` and therefore
   apply `child.max_depth + 1`.
5. String, number, bool, and null scalar paths preserve digest semantics.
   Strings still use the existing `string()` path and `unescape_string`;
   numbers use a new `fold_number_raw_known_scalar` mirroring
   `number_raw_known`; literals use `consume_literal` plus the same bool/null
   fold math.
6. Structural independence is preserved. Track 2 still uses the hand parser
   only; it does not call generated SinkOnly, generated typed helpers,
   generated Track 1, or a shared generated parser.

## Required Folds

None for source correctness. Recommended but not blocking: add one focused unit
test covering object-owned scalar values for string, signed/unsigned/f64
numbers, `true`, `false`, and `null`, because current tests cover array scalar
folding more directly than object scalar folding.
