# SK-V10 W5 Root-Typed Proof

Date: 2026-05-19.
Gate: `G-W5-ROOT-TYPED-PROOF`.

## Result

W5 proves that typed DirectBuild roots can be represented as `DirectTypeRef`
values, not only as named struct roots.

Root shapes proved:

- Array root:
  `Vec<crate::real_typed_struct::W5ArrayEvent<'i>>`.
- Map-entry root:
  `Vec<crate::real_typed_struct::W5MapMetricEntry<'i>>`.

## Generic Model

`DirectRootSchema` now carries a `DirectTypeRef`. Existing named struct roots
use `DirectRootSchema::struct_root`; collection roots use
`DirectRootSchema::typed_root`.

The typed renderer collects helpers for root-level `DirectTypeRef` values
before public root functions are emitted. Public roots call the same
`Renderer::parse_expr` path as field-level values, so vectors and map-entry
vectors use the existing typed DirectBuild helpers.

No generic codegen JSON policy was added. The codegen proof asserts that
emitted typed roots do not contain `JsonSink` or `serde_json::Value`.

## Generated Proof

The existing `generated_real_typed` module now includes:

- `parse_w5_array_root_probe`
- `parse_w5_map_entry_root_probe`

The proof probes are synthetic and do not register a bench row. They exist only
to prove the two root shapes needed before W6 can attempt corpus row movement.

## Sidecar Parity

`real_typed_struct.rs` contains W5 tests comparing generated output against
serde_json and sonic-rs typed sidecars:

- `w5_generated_array_root_probe_matches_sidecars`
- `w5_generated_map_entry_root_probe_matches_sidecars`

Both tests compare structural checksums over the generated and sidecar outputs.

## Row Accounting

No `RESULTS.md` row moves in W5. `github_events` and `gsoc-2018` remain
blocked until W6 supplies full-fixture generated Track 1, independent Track 2,
serde_json typed, sonic-rs typed, checksum parity, and typed floor evidence.
