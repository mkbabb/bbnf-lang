# SK-V10 W2 Plan - Direct Row-Table Reclamation

Pass: Wave Plan.
Cycle: W2.
Date: 2026-05-19.
Scope: measured direct row movement for `G-W2-DIRECT-RECLAMATION`.

## Entry Gate

PASS.

- W1 closed under REDRESS 100 and its executable direct row movement contract
  is live in `Report::validate_sk_v8_w0`.
- The frozen same-run direct Criterion capture under
  `CRITERION_HOME=target/skv9-w1/criterion` has a uniform run id:
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
- The W2 research scan found exactly two `N-direct / NO-GO` rows that satisfy
  the Section 0.2 Track 1 and Track 2 direct floors: `apache_builds` and
  `numbers`.

## Selected Intervention

Admit only `apache_builds` and `numbers` direct rows by replacing the W0
no-admission clamp with a W2 direct reclamation predicate.

Owner paths:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

No edits are planned for parser/runtime behavior, benchmark bodies,
`direct_struct.rs`, or `xtask/src/main.rs`.

## Implementation

- Add a small Section 0.2 floor table for W2 direct reclamation candidates.
- At direct row rendering time, admit a row only when:
  - `classify_direct_projection` already returned pass;
  - the row opened as `N-direct / NO-GO`;
  - generated Track 1 and independent Track 2 Mbps are both present;
  - both Track 1 and Track 2 meet the row's Section 0.2 floor;
  - the row is one of `apache_builds` or `numbers`.
- Render admitted rows as `A / GO` with W1 contract fields:
  - `strictness=strict`;
  - `parse_utf8=measured-row`;
  - `measured_validation_path=measured-row`;
  - `same_wave_consumer_class=gate_json_direct_contract`;
  - `redress_entry=REDRESS-101`;
  - `wave_id=SK-V10-W2`.
- Leave all floor-missing direct rows as `N-direct / NO-GO`.
- Leave existing direct guard rows unchanged and verify their maintain floors.
- Leave typed rows unchanged and verify their maintain floors.

## CHALLENGE Disposition

CHALLENGE is skipped under the dispatch contract's optional W2 clause: the plan
stays inside the accepted W1 gate semantics, moves no behavior source, touches
no parser/runtime owner path, and adds no new outcome or telemetry field.

## Exit Gate

`G-W2-DIRECT-RECLAMATION` from SPEC Section 5.

Required evidence:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w2_direct -- --nocapture
```

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests -- --nocapture
```

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --update-results --advisory
```

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

## LOC Budget And Risk

Budget: 120-240 gate/report LOC. Expected redress is under 160 source LOC plus
the generated `RESULTS.md` row-table update and REDRESS entry.

Risk: MEDIUM. The behavior surface is unchanged, but row movement is
scoreboard-sensitive and must remain impossible by a manual `RESULTS.md` flip.

## Revert Protocol

Revert `gate.rs`, `report.rs`, `RESULTS.md`, and the W2 REDRESS entry as one
slice. Preserve the W2 research floor table and failed measurements in REDRESS
if the gate cannot be made executable.

## Same-Wave Consumer

`gate-json` consumes the W2 reclamation predicate while rendering the moved
rows, then `Report::validate_sk_v8_w0` consumes the W1 contract fields from
the same rendered report. No field is emitted without a same-wave consumer.

## Pre-Blocked Routes

- No parser/runtime source change.
- No parse-only evidence.
- No typed row movement.
- No direct digest row is relabeled as typed product proof.
- Apache/numbers direct rows move only because the fresh W1 contract fields
  and Section 0.2 floors pass on both generated and independent tracks.
