# SK-V11 W4 CH1 - D1 Container-Tail Correctness Challenge

Date: 2026-05-20.

Disposition: REVISE.

The D1 `container_tail_next` route is not rejected on semantic grounds: a
sink-free scalar helper over the current cursor can preserve the current JSON
object/array tail semantics. The plan is not acceptable as written because it
does not lock the direct error-offset contract, overstates serde/sonic digest
equality, and leaves the W4 gate/report floor authority underspecified.

## Evidence Read

- SPEC Section 8 admits one scalar generated dispatch/container-tail helper,
  requires Track 1/current-output, independent Track 2, and serde/sonic oracle
  differential evidence, and requires both tracks, same-output proof, and guard
  floors before row admission (`restart/skinny/tranches/sk-v11/SPEC.md:489`-`537`).
- REDRESS 113 keeps W2's non-JSON axis blocked; W4 may continue only as direct
  closure/fixpoint work (`skinny/REDRESS.md:3340`-`3355`).
- REDRESS 114 rejects the W3 numeric route and leaves W4 dispatchable, with the
  W2 block carried forward (`skinny/REDRESS.md:3357`-`3380`).
- The W4 plan selects D1 for `random/direct_to_struct/main`, adds generated
  direct and independent hand Track 2 helpers, and claims same-output digest
  equality across generated Track 1, Track 2, serde_json, and sonic-rs
  (`restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md:9`-`23`,
  `:70`-`:84`, `:91`-`:119`).

## Semantic Assessment

JSON semantics: conditionally sound. The current generated direct loops parse
object keys, colons, and child values before tail handling, then accept only
`,` or the configured close byte (`skinny/crates/codegen/src/sink_direct.rs:264`-`279`,
`:298`-`:307`; regenerated at
`skinny/crates/runtime/src/grammars/json/generated.rs:561`-`576`,
`:595`-`:604`). A D1 helper preserves JSON semantics only if it is called after
the child value and whitespace point already used today, returns `Next` only for
the comma at that tail position, and returns `Done` only for the close byte at
that same tail position.

Empty containers: conditionally sound. Empty `{}` and `[]` are handled before
the non-empty loops in both generated direct and hand Track 2
(`sink_direct.rs:259`-`263`, `:293`-`:297`;
`direct_struct.rs:492`-`:495`, `:523`-`:526`). D1 must not replace this with a
single generic tail call before the first member/element unless it proves the
same begin/end behavior and cursor result.

Object and array separators: conditionally sound with a required trailing-comma
guard. Current behavior after a comma is to skip whitespace and then parse the
next key/value; `[1,]`, `{"a":1,}`, and `[1,,2]` therefore fail as
`ExpectedValue` at the second delimiter/close, not as a successful close. The
D1 helper must not classify comma-plus-close as `Done`, and it must not consume
a second comma as another tail while the parser is expecting a value.

Sink events: conditionally sound. Current generated direct sink events are
outside tail classification: `begin_object`/`begin_array` happen on open,
scalar child events happen in the value dispatch, and `end_object`/`end_array`
happen after close acceptance (`generated.rs:554`-`575`, `:588`-`:603`). The
existing runtime event test fixes the observable event order for nested objects
and arrays (`skinny/crates/runtime/src/lib.rs:220`-`267`). D1 must remain
sink-free; it may only classify the tail and update the cursor.

## Blockers

1. Direct error-offset contract is not specified tightly enough. The existing
   direct loops skip whitespace before calling `consume_direct`, so delimiter
   errors report the first non-whitespace byte or EOF
   (`generated.rs:569`-`:574`, `:597`-`:602`, `:800`-`:810`). The existing
   tree/tape `consume_container_next` shape computes a skipped `offset` but
   reports `error(state, ...)` at the old cursor on error
   (`generated.rs:320`-`:338`). The D1 direct helper must not copy that behavior.
   Required contract: compute the tail offset, and on error call
   `direct_error(input, offset, kind)` or set `*cursor = offset` before using a
   cursor-based error. Add invalid-tail tests for whitespace-before-bad-byte and
   whitespace-before-EOF in both object and array tails.

2. The plan claims exact serde/sonic digest equality, but the current oracle
   does not enforce it. `assert_direct_struct_parity` requires exact
   `track1 == track2`, then only `same_shape_as` for serde_json and sonic-rs
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:421`-`425`). SPEC Section 8
   and the plan both use same-output language. Before ACCEPT, either strengthen
   the W4 oracle to exact digest equality across all four paths for the selected
   row, or revise the plan to state the current shape-only serde/sonic proof and
   get explicit CHALLENGE approval for that weaker contract.

3. Separator error cases are not named as required parity fixtures. Add generated
   Track 1 and hand Track 2 tests covering at least:
   `{"a":1 x}`, `{"a":1   `, `{"a":1,}`, `{"a":1,,"b":2}`,
   `[1 2]`, `[1   `, `[1,]`, `[1,,2]`, `{}`, `{ }`, `[]`, `[ ]`, and nested
   object/array close-after-child cases. For generated Track 1, assert
   `ParseErrorKind` and byte offset, not only that parsing fails.

4. W4 gate/report admission remains under-specified. The producer currently has
   only W2 and W10 direct admission branches before the W0 clamp
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:911`-`954`), and report validation
   currently uses `sk_v10_direct_floor`, where `random` is `7734` rather than
   the SPEC §0.4 W4 floor `7878`
   (`skinny/crates/bbnf-bench/src/report.rs:1102`-`1114`,
   `:1301`-`:1310`). The W4 plan must require one shared W4 selected-row floor
   helper consumed by both producer and validator, with negative tests proving a
   row between 7734 and 7878 remains rejected.

5. Direct guard floors are claimed but not shown as gate-consumed. The plan
   measures `citm_catalog`, `apache_builds`, `marine_ik`, and `unicode_basic`,
   but it does not specify the report/gate assertion that unchanged guard rows
   hold their §0.5 direct floors. Add that consumer or narrow the claim.

## Required Revision

Revise the plan before source redress with an exact helper contract:

```rust
enum ContainerTail {
    Next,
    Done,
}

fn container_tail_next_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    close: u8,
    kind: ParseErrorKind,
) -> Result<ContainerTail, ParseError<'i>>;
```

The helper must classify only `,` and `close` at the post-value tail offset,
advance to `skip_ascii_whitespace(bytes, comma + 1)` for `Next`, advance to
`close + 1` for `Done`, and report errors at the skipped tail offset. It must
not call sink methods, must not handle empty containers, must not treat a close
after comma as successful, must not retain a cursor/sidecar, and must have an
independent hand Track 2 implementation in `direct_struct.rs`.

With those revisions, D1 can return to CHALLENGE as an ACCEPT candidate. As
written, it is REVISE, not REJECT.
