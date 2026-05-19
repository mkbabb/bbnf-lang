# SK-V10 W5 Redress - Root-Type Typed Generalization Proof

Pass: Wave Redress.
Cycle: W5.
Date: 2026-05-19.
Gate: `G-W5-ROOT-TYPED-PROOF`.
Disposition: PASS.

## Patch

W5 changes the typed DirectBuild root model from struct-only roots to
`DirectTypeRef` roots:

- `DirectRootSchema` now stores a `DirectTypeRef`.
- `DirectRootSchema::struct_root` preserves existing named struct roots.
- `DirectRootSchema::typed_root` represents collection roots directly.
- The typed renderer collects root-level helpers and renders public root
  functions through the same `Renderer::parse_expr` path used for fields.
- `real_typed_schema.rs` adds synthetic W5 array and map-entry proof roots.
- `generated_real_typed.rs` is regenerated with `parse_w5_array_root_probe`
  and `parse_w5_map_entry_root_probe`.
- `real_typed_struct.rs` adds generated/serde_json/sonic checksum parity tests
  for both proof roots.
- Lock 14 parent-diff authorization is exact to the W5 root-typed owner paths.

## Proof

Root shapes proved:

- `Vec<crate::real_typed_struct::W5ArrayEvent<'i>>`
- `Vec<crate::real_typed_struct::W5MapMetricEntry<'i>>`

The proof is recorded in
`restart/skinny/tranches/sk-v10/research/p3/root-typed-proof/ROOT-TYPED-PROOF.md`.

## Evidence

Pre-commit evidence passed:

```text
cargo test --manifest-path skinny/Cargo.toml -p codegen typed_direct -- --nocapture
cargo xtask regen-real-typed
cargo xtask check-real-typed
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w5_ -- --nocapture
```

Dirty-root Lock 14 intentionally rejects the uncommitted source state, so the
full Lock 14 and frozen report checks were run after the redress commit:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14 -- --nocapture
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

Result: PASS.

## Gate Accounting

- No `RESULTS.md` row moved.
- No `json_parity` bench row was registered.
- No new telemetry field or outcome variant was added.
- `github_events` and `gsoc-2018` remain blocked until W6 supplies same-wave
  typed comparator rows, independent Track 2/oracle proof, checksum parity, and
  typed floor evidence.
