# SK-V10 W6 Research - Root Typed Row Admission

Status: read-only research for SPEC Section 9.

## Entry Gate

W5 is closed under REDRESS 104. `DirectRootSchema` now stores a
`DirectTypeRef`, and the generated proof roots cover both `Vec<T>` and
map-entry roots without moving `RESULTS.md`. W6 is therefore dispatchable.

## Target Order

SPEC Section 9 requires one root-unblocked corpus, `github_events` before
`gsoc-2018` unless CHALLENGE reverses the order.

`github_events` is the correct first target:

- Fixture path: `skinny/test_data/github_events.json`.
- Size: 65,132 bytes.
- Root shape: top-level array.
- Element count: 30.
- Required top-level keys on every element: `type`, `created_at`, `actor`,
  `repo`, `public`, `payload`, `id`.
- Optional top-level key: `org` appears on 6 of 30 events.

This exactly consumes the W5 `Vec<T>` root model. `gsoc-2018` remains the
second target because its fixture is a 3,327,831-byte numeric-string-keyed map
root with 1,264 entries. That shape is W5-covered, but it is a larger
measurement and implementation slice than the first W6 row should carry.

## Proposed Typed Product Shape

Use a generated typed root:

```rust
Vec<crate::real_typed_struct::GithubEvent<'i>>
```

The typed product should be a real consumer projection over the whole fixture,
not a one-field smoke test. The first slice should keep the schema bounded and
stable:

- `GithubEvent`: `type`, `created_at`, event `id`, `public`, `actor`, `repo`,
  optional `org`, and `payload`.
- `GithubActor`: `id`, `login`, `url`, `avatar_url`.
- `GithubRepo`: `id`, `name`, `url`.
- `GithubPayload`: common scalar payload fields (`action`, `ref`,
  `ref_type`, `push_id`, `size`, `distinct_size`, `head`, `before`,
  `description`, `master_branch`).

The current schema model already supports these field classes: borrowed string,
`u64`, `bool`, `Option<T>`, nested structs, and `Vec<T>`. Heavy variant payload
members such as `commits`, `forkee`, `issue`, `comment`, and `pages` can remain
unknown skipped fields for this typed product row, as existing typed rows also
validate selected typed projections over the full fixture while skipping
unknown JSON members.

## Gate Work Required

W6 is row-moving, so the redress slice must update both producer and consumer:

- Add the `github_events` fixture to `RealTypedFixture`.
- Add serde_json and sonic-rs typed sidecars for the same Rust output type.
- Add checksum coverage for the full fixture output.
- Regenerate `generated_real_typed.rs` from `xtask/src/real_typed_schema.rs`.
- Extend `gate-json` metadata expectations so `github_events` has typed
  Criterion rows.
- Extend `Report::validate_sk_v8_w0` with a W6 typed-row contract; otherwise a
  new row is rejected as unknown against the W0 baseline row count.
- Preserve existing typed maintain floors.
- Add a Lock 14 exact owner-path allowance for the W6 generated typed source
  slice.

## Expected Failure Mode

The hard gate is not only generated Track 1. The independent Track 2/oracle must
also clear `ceil(same-run sonic_typed / 1.10)`. W4 failed exactly here. If the
generated parser passes and the serde_json/Track 2 oracle misses the floor, the
source slice must be reverted, `/tmp/skv10-waveW6-rejected.patch` must be saved,
and REDRESS must record the measured rejection with no `RESULTS.md` row
movement.
