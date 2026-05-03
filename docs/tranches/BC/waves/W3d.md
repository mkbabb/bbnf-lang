# BC.W3d — Umbrella `core` Slim-Down

**Name**: W3d — Umbrella core Slim-Down
**Opens after**: BC.W3c close (three sub-crates extracted)
**Hard gate**: `crates/core/src/lib.rs` reduces to re-export shell only; `cargo check -p bbnf` (umbrella) green; downstream tests resolve via re-exports
**Status**: planned

## §1 Deliverable

Slim `crates/core/` to a re-export shell. The crate's only source file post-split is `lib.rs`. Re-exports preserve the public surface for backwards-compat per `audit/W3-crate-dependency-dag.md:§3`. The `backend/` namespace is renamed to `codegen/` (consumer-facing rename); the `runtime/<g>` paths sunset and consumers migrate to `generated/<g>/<G>Value`-style paths.

## §2 Milestones

### §2.1 Slim lib.rs
Mechanism: `crates/core/src/lib.rs` becomes the re-export shell:
```rust
pub use bbnf_parse::*;
pub use bbnf_codegen::*;
pub mod runtime { pub use bbnf_runtime::*; }
pub mod path { pub use ::path::*; pub use ::path_core::*; }
pub use bbnf_runtime::Visitor;     // permanent convenience re-export
pub use ::path::pointer;           // permanent convenience re-export
```
Sub-gate: `wc -l crates/core/src/lib.rs` < 50 lines (re-exports only); no other source files in `crates/core/src/` post-W3d.

### §2.2 backend → codegen rename (consumer-facing)
Mechanism: per migration cookbook §2.2, `bbnf::backend::*` consumers rewrite to `bbnf::codegen::*`. The umbrella's re-export does NOT include `backend` namespace — only `codegen`.
Sub-gate: `rg -n 'pub mod backend' crates/core/src/lib.rs` returns zero.

### §2.3 Update `crates/core/Cargo.toml`
Mechanism: deps become `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`, `path`, `path-core`. Drop `bbnf-ir` direct dep (consumed transitively via bbnf-parse).
Sub-gate: `cargo metadata` resolves clean.

### §2.4 Re-export sunset markers (per `audit/W3-crate-dependency-dag.md:§3`)
Mechanism: doc comments mark which re-exports are permanent vs sunset-at-BC.W6.
Sub-gate: `crates/core/src/lib.rs` includes `//! Re-export sunset rules: see docs/migration/bc-core-split.md §3`.

### §2.5 Compile-time verification
Mechanism: every previously-importable path through `bbnf::*` resolves through the re-exports OR is documented as sunsetted in the migration cookbook.
Sub-gate: `cargo check -p bbnf --tests` green; `cargo nextest run -p bbnf` 100% pass.

### §2.6 LOC accounting
Mechanism: capture LOC at W3d close.
Sub-gate: `crates/core/src/` total LOC < 100 (lib.rs only); pre-W3 was ~25,000 source LOC; reduction ~99.6%.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W3d-G1 | `crates/core/src/lib.rs` is the only source file | `find crates/core/src -name '*.rs' \| wc -l` returns 1 |
| W3d-G2 | lib.rs is a re-export shell | `wc -l crates/core/src/lib.rs` < 50 |
| W3d-G3 | Umbrella compiles | `cargo check -p bbnf` green |
| W3d-G4 | Workspace tests pass | `cargo nextest run --workspace` 100% pass |
| W3d-G5 | `backend` namespace retired | `rg -n 'bbnf::backend' docs/ crates/` returns zero (or only archived) |
| W3d-G6 | Re-export sunset markers present | `grep -c 'sunset' crates/core/src/lib.rs` ≥ 1 |

## §4 Invariants

§I1. **Lock 13**: umbrella is purely re-export shell.

§I2. **Backwards compat**: `bbnf::*` paths resolve via re-exports through BC.W6.

§I3. **Sunset transparency**: migration cookbook §3 records which re-exports retire when.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| `bbnf::backend::*` consumer breaks | Expected | Migration cookbook records the rename; downstream rewrites at first build error |
| Re-export ambiguity (two crates export same name) | Medium | Selective re-exports if conflict; cookbook records resolution |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BC.W3c→BC.W3d | BC.W3c | Three sub-crates exist |

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC.W3d→BC.W3e | BC.W3e | Umbrella ready; xtask path can update |

## §7 Iter-time check

| Activity | Pre-W3d wall | Post-W3d wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf` | ~24 s | ~24 s | re-export resolution overhead |

## §8 Dependencies

- **Depends on**: BC.W3c close.
- **Blocks**: BC.W3e.

## §9 Closing posture

`crates/core/` is the umbrella shell. Three sub-crates own three concerns. Re-exports preserve compat through BC.W6.
