# SK-V10 W5 Plan - Root-Type Typed Generalization Proof

Pass: Wave Plan.
Cycle: W5.
Date: 2026-05-19.
Scope: `G-W5-ROOT-TYPED-PROOF`.

## Entry Gate

PASS.

W4 closed under REDRESS 103. SPEC Section 8 authorizes proof-only root model
work for `github_events` top-level arrays and `gsoc-2018`
numeric-string-keyed map roots.

## Selected Intervention

Replace struct-only typed roots with `DirectTypeRef` roots and prove two root
shapes in the existing typed DirectBuild path:

- Array root: `Vec<crate::real_typed_struct::W5ArrayEvent<'i>>`.
- Map-entry root:
  `Vec<crate::real_typed_struct::W5MapMetricEntry<'i>>`.

This is a root-model and proof wave only. No `RESULTS.md` row moves, and no
`github_events` or `gsoc-2018` production typed row is admitted in W5.

## Owner Paths

- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `restart/skinny/tranches/sk-v10/research/p3/root-typed-proof/`
- `skinny/REDRESS.md`
- SK-V10 close documents for wave status only.

## Implementation

- Change `DirectRootSchema` so roots carry a `DirectTypeRef` rather than only a
  named struct `type_id`.
- Add `DirectRootSchema::struct_root` and `DirectRootSchema::typed_root`
  constructors so existing struct roots stay explicit and root-shaped proofs
  are easy to audit.
- Update validation to validate the root `DirectTypeRef`.
- Update typed rendering to collect helpers from root-level `DirectTypeRef`
  values and render public functions through `Renderer::parse_expr`.
- Add codegen tests proving a generated module can expose both array and
  map-entry roots without `JsonSink`, `serde_json::Value`, or runtime policy
  leakage.
- Add synthetic generated roots to `real_typed_schema.rs` and regenerate
  `generated_real_typed.rs`.
- Add `real_typed_struct.rs` tests that parse the synthetic array and map roots
  with generated Track 1, serde_json, and sonic-rs typed sidecars and compare
  checksums.
- Extend the Lock 14 parent-diff authorizer for exactly the W5 frozen owner
  paths. Without this, `gate-json` cannot validate the committed W5 proof
  state even though the frozen-root diff is authorized by SPEC Section 8.
- Add `research/p3/root-typed-proof/ROOT-TYPED-PROOF.md` with the proof facts.

## Section 2.1 / Lock 14

W5 edits generic codegen schema and typed renderer code, but does not encode
JSON-specific root policy. The generic representation is `DirectTypeRef`, which
already models vectors and map-entry vectors for typed DirectBuild fields. The
proof names non-row synthetic roots rather than corpus-specific shortcuts.

The generic proof is:

- Codegen unit test asserts no `JsonSink` or `serde_json::Value` in emitted
  typed roots.
- Bench proof tests compare generated output to serde_json and sonic-rs typed
  sidecars.
- No parser/runtime behavior outside typed DirectBuild codegen changes.

## Exit Gate

`G-W5-ROOT-TYPED-PROOF` from SPEC Section 8.

Required evidence:

```text
cargo test --manifest-path skinny/Cargo.toml -p codegen typed_direct -- --nocapture
```

```text
cargo xtask regen-real-typed
cargo xtask check-real-typed
```

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w5_ -- --nocapture
```

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

## Revert Protocol

Revert the root-model codegen changes, synthetic proof schema, regenerated
typed module, proof tests, and W5 docs as one slice. Record the failed root
shape in REDRESS. If a row moves, reject the wave; W5 is proof-only.

## Pre-Blocked Routes

- No handwritten per-corpus shortcut for `github_events` or `gsoc-2018`.
- No direct digest evidence as typed product proof.
- No `RESULTS.md` movement.
- No JSON-specific policy in generic codegen or runtime code.
