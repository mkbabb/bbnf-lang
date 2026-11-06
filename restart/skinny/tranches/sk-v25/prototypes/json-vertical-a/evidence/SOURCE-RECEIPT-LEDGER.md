# Family A incomplete-assay source and receipt ledger

## Authority

```json
{
  "scope": "prototype-evidence-only",
  "status": "INCOMPLETE_ASSAY",
  "prototype_complete": false,
  "architecture_selected": false,
  "movement_authorized": false,
  "production_transpose_authorized": false,
  "review_passes_credited": 0
}
```

This is an implementation-session receipt, not a hostile review or closure.

## Bound inputs

- Governing wave document SHA-256:
  `36f88bf72043c95a33b9e0f814b08634664fa018ac2a6396d18a319e3c7bfc64`.
- Locked JSON module: 537 bytes, SHA-256
  `ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604`.
- Standalone Cargo lock SHA-256:
  `2d2efd5806186b28df89065e52639ec04b267d6059bd6e0c45d7eb26d28b3618`.
- Full relative implementation/test/fixture ledger:
  `evidence/source-checksums.sha256`, SHA-256
  `8eadb2e0d8475e52520095ae74092f0c47d6d7d195c0a0d060cb7e628b6744d1`.
- Receipt hashes before this self-excluding ledger:
  - `README.md`: `0cdf73e132ab28f4e18aeece679cdb2a45186205022409ce0b615105af9a1767`;
  - `STATUS.json`: `ad2b2d27791de9e5d4e75b2cdb8cbff8929624d6062e4a3e4a2632d38dcf4d64`;
  - `FRICTION-AND-ROUTING.md`: `1671bb230aeb3ad8d3a831c02f125c2fc3d3499726b06ddfc2605d2a7d05331c`.
- The crate has an empty `[workspace]`; no root manifest or lock is changed.

## Host/toolchain observed

```text
arm64
macOS 26.4.1 build 25E253
rustc 1.96.0-nightly f5eca4fcf (2026-04-09)
LLVM 22.1.2
cargo 1.96.0-nightly eb94155a9 (2026-04-09)
```

The dated toolchain resolved to `f5eca4fcf`, not the earlier default-toolchain
observation `02c7f9bec`; no equivalence claim is made. No PMU or performance
measurement was run.

## Implemented and tested

- exact source lock and typed source budget;
- module-derived rule names, bodies, references, and last-rule entry;
- immutable program/source identities;
- bijective zero-rewrite normalization;
- deterministic dependency edges and iterative SCCs;
- exact JSON recursive SCC `{array,pair,object,value}`;
- second tiny module with different names/literals/delimiters and no sidecar;
- closed-row permutation stability and conflict rejection;
- hard stop before type publication and complete product compilation;
- receipts permanently deny movement, transpose, selection, and completion.

Stable assay result identities:

```text
source     ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604
program    908129a7e097b1070ac1b7ac587cd242999ca8672057c13faf22abf41051ca71
normalized 650f68b18ef9a566ed0857feead3afc4c232fec9c606dec3ca26ffbeab47ccaa
graph      f0e97cd27cb8d67d1b86e310c823bb62a976d6c4fbdb092f24eca4c24e6cd8ac
```

## Gates

```text
cargo +nightly-2026-04-10 fmt --manifest-path <family>/Cargo.toml -- --check
PASS

cargo +nightly-2026-04-10 clippy --manifest-path <family>/Cargo.toml --locked --all-targets --all-features -- -D warnings
PASS (Cargo reports inherited, unused root patch entries; no compiler/clippy warning)

cargo +nightly-2026-04-10 test --manifest-path <family>/Cargo.toml --locked --all-features
PASS: 13 external tests, 0 failed, 0 ignored
```

Clean replay used an external target directory. Repository status was unchanged:

```text
before_status_sha256 306f7914cfc050cf6d7e7c9ea1c05a33833f70bc9d9d48c1f3f5ae9c706ad2c5
after_status_sha256  306f7914cfc050cf6d7e7c9ea1c05a33833f70bc9d9d48c1f3f5ae9c706ad2c5
family-local target  absent
```

## RED boundary

W1 is partial and W2 is RED. No type solution, total facts, lowering plan,
checked code, value document, artifact set, Rust/TS/WASM backend, frontend
protocol, or performance result is published. `compile` and `TypeSolution::solve`
return `VerticalError::Incomplete` until those products can be inferred solely
from complete BBNF module semantics without schema/type/fact/lowering sidecars.

## Required independent review packet

Two fresh hostile gestalt passes must inspect this exact ledger and separately
cover tranche optimality, wave/spec adherence, all implemented behavior,
overfitting, missing locks, and the successor routing in
`FRICTION-AND-ROUTING.md`. This implementation session claims neither pass.
