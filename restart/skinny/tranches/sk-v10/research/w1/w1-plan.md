# SK-V10 W1 Plan - Direct Output Contract

Pass: Wave Plan.
Cycle: W1.
Date: 2026-05-19.
Scope: contract-only implementation plan for `G-W1-DIRECT-CONTRACT`.

## Entry Gate

PASS.

- W0 is closed under REDRESS 99.
- The direct output evidence surface is named: generated Track 1
  `track1_direct_to_struct`, independent hand Track 2
  `track2_direct_to_struct`, sonic strict direct comparator
  `sonic_rs_direct_to_struct`, and digest output plane.
- W1 moves no `RESULTS.md` row.

## Selected Intervention

Add an executable direct row movement contract to report validation.

Owner path:

- `skinny/crates/bbnf-bench/src/report.rs`

Out of scope:

- `skinny/RESULTS.md`
- parser/runtime behavior
- generated output
- benchmark bodies
- row table movement

## Contract

When a row matches the SK-V10 opening baseline, validation remains unchanged.

When a direct row whose opening baseline is `N-direct / NO-GO` changes outcome
or verdict, `Report::validate_sk_v8_w0` may accept it only if all of these
predicates hold:

- row id is `json/<corpus>/direct_to_struct/main`;
- new outcome/verdict is `A / GO`;
- row output plane is `digest`;
- row strictness is `strict`;
- `parse_utf8` and `measured_validation_path` are `measured-row`;
- `escape_complete` is `yes`;
- Track 2 independence is `independent_verified`;
- same-wave consumer is not `gate_only`;
- REDRESS entry is present and not `none`;
- sonic-rs and serde_json native comparator evidence is same-run, strict,
  digest-plane, sidecar-free, and sourced from the direct Criterion benches.

Every missing predicate produces a validation error before the row can move.

## Exit Gate

`G-W1-DIRECT-CONTRACT` from SPEC Section 4.

Required evidence:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests::direct_contract
```

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --advisory
```

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

## LOC Budget And Risk

Budget: 180-320 docs/gate LOC. Expected redress is under 140 LOC including
tests.

Risk: HIGH by SPEC because this unblocks direct row movement. The local code
risk is moderate: the new path is dormant for unchanged W0 rows but must be
strict enough that W2 cannot move a row by editing only outcome/verdict cells.

## Revert Protocol

Revert the `report.rs` contract validator and tests as one slice. Leave
`RESULTS.md` unchanged. Record REDRESS with the missing predicate if the
contract cannot be made executable.

## Same-Wave Consumer

`Report::validate_sk_v8_w0`, called by `gate-json`, consumes the new predicate
in the same wave. Unit tests exercise both acceptance and rejection paths.

## Pre-Blocked Routes

- Direct digest rows cannot admit typed product rows.
- REDRESS 93 scalar-parent folding and REDRESS 73 helper-shape transfer remain
  blocked.
- REDRESS 50-55 and 66-69 sidecar, scratch, source-hook receiver, decoded
  scratch, byte-output unescape, and semantic-fact routes remain blocked.
- No generic crate or codegen behavior changes are planned, so SPEC Section 2.1
  generic-proof requirements are not triggered.
