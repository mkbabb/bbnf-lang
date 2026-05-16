# SK-V7 W0b R2: sonic-rs strict/lossy provenance

Scope: read-only inspection of `bbnf-bench` comparator wiring, Cargo feature
mechanics, and schema-v3 provenance requirements.

## Current State

`bbnf-bench` now depends on `sonic-rs` with only `sort_keys`. The live feature
tree confirms no `utf8_lossy`.

Current timed sonic rows are strict-default `from_slice` rows:

- `sonic_rs_anchor` and `sonic_rs_checked` both call
  `sonic_rs::from_slice::<sonic_rs::Value>`.
- `sonic_rs_direct_to_struct` routes through `direct_struct::sonic_digest`.
- `sonic_rs_real_typed_struct` routes through
  `real_typed_struct::sonic_typed`.

`sonic_rs_checked` is a duplicate of `sonic_rs_anchor`; it does not represent a
separate strictness plane.

## Cargo Feature Mechanics

Dual Cargo-feature builds of `sonic-rs = 0.5.8` are not feasible inside one
ordinary `bbnf-bench` crate graph. Cargo features are additive for the same
package ID, so dependency aliases for strict and lossy would compile one
package with the union feature set and taint both aliases.

A true dual feature build would require separate package identities, a vendored
copy, a fork/path copy, dynamic sidecars, or separate Cargo invocations. Those
are poor W0b routes because they either change comparator identity or break
same-run provenance.

## Same-Run Route

Dual strict/lossy semantic rows are feasible without enabling the Cargo
feature. `sonic-rs` exposes `Deserializer::utf8_lossy()` as an explicit public
API independent of the feature. The feature only makes `from_slice` enter that
mode by default.

W0b can keep the feature mask strict and add an explicit lossy flaw-probe row:

```rust
let mut de = sonic_rs::Deserializer::from_slice(bytes).utf8_lossy();
let value = de.deserialize::<sonic_rs::Value>()?;
de.end()?;
```

The honest provenance is not `feature_mask=sort_keys+utf8_lossy`; it is
`feature_mask=sort_keys`, `api_symbol=Deserializer::utf8_lossy`, and
`strictness=permissive`.

## Schema-v3 Provenance

Strict sonic row:

- comparator: `sonic-rs`
- version: `0.5.8`
- feature_mask: `sort_keys`
- api_symbol: `sonic_rs::from_slice::<T>`
- strictness: `strict`
- parse_utf8: `scan-boundary`
- escape_complete: `yes`
- s-anchor eligible: yes

Lossy sonic row:

- comparator: `sonic-rs`
- version: `0.5.8`
- feature_mask: `sort_keys`
- api_symbol: `sonic_rs::Deserializer::from_slice(...).utf8_lossy().deserialize::<T>()`
- strictness: `permissive`
- parse_utf8: `none`
- escape_complete: `no` for S-anchor purposes
- flaw_probe: `lossy_utf8 substitution; not S-anchor eligible`
- s-anchor eligible: no

Only the strict column may feed `Delta vs sonic-strict` or GO/NO-GO
classification.

## Recommended Plan Input

Do not attempt Cargo dependency aliasing. Repurpose the duplicate
`sonic_rs_checked` lane into an explicit lossy API flaw-probe row, add matching
direct/real-typed lossy rows only if needed for schema completeness, and make
strictness/anchor eligibility data-driven through metadata/report/gate.
