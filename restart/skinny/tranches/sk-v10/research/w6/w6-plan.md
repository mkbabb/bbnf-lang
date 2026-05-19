# SK-V10 W6 Plan - Root Typed Row Admission

Status: Phase 2 plan for SPEC Section 9.

## Selected Intervention

Select `github_events/real_typed_struct` as the W6 root-unblocked typed row.
This follows the SPEC order: `github_events` before `gsoc-2018` unless
CHALLENGE reverses it. The row consumes W5's `Vec<T>` root proof and does not
reopen W3 or any union substrate route.

## Owner Paths

Initial owner paths were exactly SPEC Section 9:

- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

CHALLENGE accepted one required revision before redress. Add this
gate-validation owner path:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`

The Lock 14 edit may only add an exact SK-V10 W6 allowance for the W6 frozen
typed owner paths. It may not authorize generic runtime, grammar, parser
substrate, W3, or non-W6 source paths.

## Implementation Shape

Add one generated typed root:

```rust
DirectRootSchema::typed_root(
    "parse_github_events",
    "Vec<crate::real_typed_struct::GithubEvent<'i>>",
    vec_with_capacity(ty("GithubEvent"), 30),
)
```

Add Rust product types and checksums for the selected projection:

- `GithubEvent<'a>` with event type, created timestamp, event id, public flag,
  actor, repo, optional org, and payload.
- `GithubActor<'a>` with id/login/url/avatar URL.
- `GithubRepo<'a>` with id/name/url.
- `GithubPayload<'a>` with common scalar payload fields.

The schema skips variant-heavy payload members through the existing unknown
field policy. The full fixture is still parsed by generated Track 1,
independent Track 2, serde_json, and sonic-rs sidecars; all four outputs must
share the checksum.

## Falsifiability Gate

Exit gate: `G-W6-ROOT-TYPED-ROW` from SPEC Section 9.

Operational measurement rules:

- Criterion capture uses `RUSTFLAGS="-C target-cpu=native"`.
- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` must exist
  for `json/github_events` under one coherent run id.
- Generated Track 1 and independent Track 2/oracle must both meet
  `ceil(same-run sonic_typed / 1.10)`.
- Existing typed maintain rows remain admitted and above Section 0.2 floors.
- `gate-json` consumes the new row in the same wave; no emit-now-consume-later.

## Same-Wave Consumer

The generated typed parser is consumed by:

- `json_parity` fixture parity at bench start.
- `track1_real_typed_struct` benchmark row.
- `gate-json` row construction and SK-V10 W6 typed-row contract.
- `RESULTS.md` only if the gate admits.

## Budget And Risk

- LOC budget: 160-260 source/generated plus 40-80 gate/report LOC per corpus.
- Risk: MEDIUM-HIGH, because report validation must add one non-baseline W6 row
  while preserving W0 row-count discipline for all other rows.
- Redress cap: <=90 minutes.

## Revert Protocol

On parity failure, missing comparator evidence, floor miss, or typed maintain
regression:

1. Save the full source patch to `/tmp/skv10-waveW6-rejected.patch`.
2. Revert schema, generated typed code, typed structs/checksums, bench/gate
   changes, and any `RESULTS.md` movement for `github_events`.
3. Record a REDRESS reject with measured Track 1, Track 2, sonic, serde_json,
   and the floor.

## Pre-Blocked Routes

- Canada typed remains blocked.
- Direct digest evidence cannot admit typed rows.
- No root row moves without the W5 proof.
- No W3, union substrate, parser-owned class column, or parse-only SOTA claim.
- No generic JSON policy leak into codegen/runtime outside the W5 root model.

## Plan Revision - CHALLENGE W6

Accepted revision: include `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
as a gate-validation owner path. Redress must add an exact W6 parent-diff
allowance covering the selected `github_events` typed row source slice and must
keep all other frozen roots locked.
