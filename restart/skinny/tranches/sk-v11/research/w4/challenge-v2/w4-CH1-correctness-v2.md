# SK-V11 W4 CH1 Correctness Challenge V2

Date: 2026-05-20.

Disposition: ACCEPT.

V2 resolves the CH1 correctness blockers from V1 for the D1
`container_tail_next` route. This is an acceptance of the V2 plan contract, not
a source-redress acceptance: implementation still has to prove the contract with
the named generated/hand tests and malformed-input oracle coverage before any
measurement or row admission.

## Evidence Read

- V2 selects exactly one JSON-local scalar helper over the current post-value
  byte cursor, consumed by generated Track 1 and independently mirrored in
  direct Track 2
  (`restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md:12`-`14`).
- SPEC Section 8 authorizes one scalar generated dispatch/container-tail helper
  and requires independent Track 2, same-output proof, and guard floors before
  admission (`restart/skinny/tranches/sk-v11/SPEC.md:489`-`537`).
- V1's CH1 blockers were: underspecified direct error offsets, overstated
  serde/sonic digest equality, unnamed separator fixtures, and plan-level
  gate/report floor ambiguity
  (`restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH1-correctness.md:63`-`106`).
- Current generated direct loops handle empty containers before non-empty loops,
  then after each child skip whitespace, accept comma as `Next`, or require the
  configured close byte as `Done`
  (`skinny/crates/codegen/src/sink_direct.rs:257`-`279`,
  `skinny/crates/codegen/src/sink_direct.rs:291`-`307`;
  regenerated at `skinny/crates/runtime/src/grammars/json/generated.rs:554`-`604`).
- Current hand Track 2 mirrors that separator shape, with empty object/array
  handling before the loop and comma advancing to the next key/value position
  (`skinny/crates/bbnf-bench/src/direct_struct.rs:483`-`538`).

## Assessment

Helper semantics: resolved. V2 names the exact generated helper signature,
restricts classification to `,` or the supplied close byte at the post-value
tail offset, advances `Next` to `skip_ascii_whitespace(bytes, comma + 1)`, and
advances `Done` to `close + 1`
(`w4-plan-container-tail-direct-v2.md:26`-`46`). It also states the helper is
sink-free and cannot retain cursor/sidecar state or carry object key/value bytes
across the boundary (`w4-plan-container-tail-direct-v2.md:47`-`49`). That
matches the current generated direct loop's observable boundary while avoiding
the older tape helper's cursor/error-state ambiguity.

Error offsets: resolved at plan level. V1 required direct errors at the skipped
tail offset because current direct `consume_direct` reports at `*cursor`, while
the old tree/tape `consume_container_next` computed a skipped offset but errored
against the old parser state
(`w4-CH1-correctness.md:65`-`75`;
`skinny/crates/runtime/src/grammars/json/generated.rs:310`-`338`,
`skinny/crates/runtime/src/grammars/json/generated.rs:800`-`810`). V2 now
requires the helper to report errors at the skipped tail offset and requires
generated Track 1 tests asserting `ParseErrorKind` and byte offsets for
whitespace-before-bad-byte and whitespace-before-EOF in object and array tails
(`w4-plan-container-tail-direct-v2.md:43`-`46`,
`w4-plan-container-tail-direct-v2.md:112`-`114`).

Empty and trailing-comma cases: resolved at plan level. V2 explicitly says the
helper does not handle empty containers and does not treat close-after-comma as
success (`w4-plan-container-tail-direct-v2.md:47`-`49`). It also requires
generated Track 1 and hand Track 2 malformed-tail tests for `{}`, `{ }`, `[]`,
`[ ]`, `{"a":1,}`, `{"a":1,,"b":2}`, `[1,]`, `[1,,2]`, and nested
close-after-child cases (`w4-plan-container-tail-direct-v2.md:108`-`116`).
Those fixtures cover the V1 risk that comma-plus-close or a second comma could
be accidentally classified as another successful container tail.

Oracle language: resolved. V1 objected that the old plan claimed exact
serde/sonic digest equality even though `assert_direct_struct_parity` only
requires exact `track1 == track2` and shape equality against serde_json and
sonic-rs (`w4-CH1-correctness.md:77`-`84`;
`skinny/crates/bbnf-bench/src/direct_struct.rs:420`-`425`). V2 now states the
accepted contract precisely: exact generated Track 1 versus independent Track 2
digest equality, with serde_json and sonic-rs as strict same-row direct
comparators and valid-shape oracles under the existing parity contract
(`w4-plan-container-tail-direct-v2.md:50`-`54`,
`w4-plan-container-tail-direct-v2.md:141`-`146`). Malformed-tail rejection is
kept separate and must pass in all four parsers.

## Remaining Blockers

No CH1 correctness plan blocker remains.

Source redress must still revert or return to CHALLENGE if any of these fail:

1. The generated helper implementation deviates from the exact V2 signature or
   cursor/error-offset semantics.
2. Empty-container handling moves into the helper or changes the current
   begin/end behavior.
3. `{"a":1,}`, `[1,]`, `{"a":1,,"b":2}`, `[1,,2]`, or nested
   close-after-child fixtures are accepted by generated Track 1 or hand Track 2.
4. Generated Track 1 does not assert the required `ParseErrorKind` and byte
   offset for whitespace-before-bad-byte and whitespace-before-EOF in both
   object and array tails.
5. Valid-row exact digest equality fails between generated Track 1 and
   independent Track 2, or malformed-tail rejection fails in generated Track 1,
   hand Track 2, serde_json, or sonic-rs.
6. Direct Track 2 calls `runtime::generated_json`, generated SinkOnly helpers,
   `container_tail_next_direct`, or any generated Track 1 tail symbol.

CH4/CH5 cost, floor-authority, guard-floor, provenance, and gate/report
consumption requirements remain separate W4 exit-gate blockers, but they no
longer block CH1 correctness acceptance of the V2 plan.
