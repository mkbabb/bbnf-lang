# BC.W3c — `bbnf-codegen` Extraction

**Name**: W3c — bbnf-codegen Extraction
**Opens after**: BC.W3b close (`bbnf-parse` exists)
**Hard gate**: `cargo check -p bbnf-codegen` green; dep arrow `bbnf-codegen → bbnf-parse → bbnf-runtime`; `cargo nextest run -p bbnf-codegen` 100% pass
**Status**: planned

## §1 Deliverable

Extract `bbnf-codegen`. Includes `codegen/` directory (Rust lowerer + TS scaffold + WASM scaffold + optimiser). The Rust lowerer at `crates/core/src/codegen/rust/lower.rs` (post-W1a rename) moves to `crates/bbnf-codegen/src/rust/lower.rs`; same for `ts/`, `wasm/`, `optimiser/`.

## §2 Milestones

### §2.1 Crate creation
Sub-gate: workspace member count = 15.

### §2.2 Move codegen sub-modules
Mechanism: relocate `crates/core/src/codegen/rust/` → `crates/bbnf-codegen/src/rust/`; same for `ts/`, `wasm/`, `optimiser/`.
Sub-gate: `find crates/bbnf-codegen/src -name '*.rs' \| wc -l` matches pre-relocation count.

### §2.3 Configure deps
Mechanism: `bbnf-codegen/Cargo.toml` declares `bbnf-parse`, `bbnf-ir`, `egraph`, `csp-solver`, `simd-scan`, `parse-that` (for combinator imports), `bbnf-runtime` (transitively via bbnf-parse re-exports; not direct).
Sub-gate: `cargo tree -p bbnf-codegen \| grep -c bbnf-runtime` may return matches via transitive but not direct.

### §2.4 Per-shape consumer relocation
Mechanism: `crates/core/src/codegen/rust/shapes/` → `crates/bbnf-codegen/src/rust/shapes/`. Per W1a, the shapes consume typed IR.
Sub-gate: shape consumers compile in new location.

### §2.5 Optimiser relocation
Mechanism: `crates/core/src/codegen/optimiser/` → `crates/bbnf-codegen/src/optimiser/`. Includes `pratt_detect`, `simd_detect`, `alt_dispatch::classify_*`, `regex_synthesise`, `scanner_classify`.
Sub-gate: optimiser compiles in new location.

### §2.6 Scaffold relocation
Mechanism: BC.W2's TS + WASM scaffolds at `crates/core/src/codegen/{ts,wasm}/` → `crates/bbnf-codegen/src/{ts,wasm}/`. Smoke tests still pass post-relocation.
Sub-gate: smoke tests pass at new location.

### §2.7 Define public surface
Mechanism: `bbnf-codegen/src/lib.rs` exports `Emitter` trait, `RustLowerer`, `TsEmitter`, `WasmEmitter`, `TypedIRNode` (re-exported from `bbnf-ir::typed_ir`).
Sub-gate: `cargo doc -p bbnf-codegen --no-deps` clean.

### §2.8 Workspace test surface
Mechanism: per-crate tests pass.
Sub-gate: `cargo nextest run -p bbnf-codegen` 100% pass.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W3c-G1 | `bbnf-codegen` exists | workspace member count = 15 |
| W3c-G2 | Compiles independently | `cargo check -p bbnf-codegen` green |
| W3c-G3 | Dep arrow correct | `cargo tree -p bbnf-codegen` shows `bbnf-parse` direct, `bbnf-runtime` only via transitive |
| W3c-G4 | Codegen sub-modules relocated | source files match pre-W3c count at new path |
| W3c-G5 | Smoke tests pass | scaffolds still produce non-empty output post-relocation |
| W3c-G6 | Tests pass | `cargo nextest run -p bbnf-codegen` 100% pass |

## §4 Invariants

§I1. **Lock 5**: per-backend lowerers live in bbnf-codegen.

§I2. **Lock 13**: cohesive crate (codegen concern).

§I3. **No grammar names in codegen** per G05-4.

§I4. **bbnf-codegen → bbnf-parse → bbnf-runtime** strict acyclic chain.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Smoke tests break due to path migration | Medium | W3c §2.6 verifies smoke tests pass at new path |
| Optimiser references via stale `crate::` paths | Medium | Update imports per migration cookbook |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BC.W3b→BC.W3c | BC.W3b | bbnf-parse exists |
| BC.W2→BC.W3c | BC.W2 | TS + WASM scaffolds |
| BC.W1a→BC.W3c | BC.W1a | Refactored Rust lowerer |

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC.W3c→BC.W3d | BC.W3d | Three new sub-crates exist; umbrella can slim down |

## §7 Iter-time check

| Activity | Pre-W3c wall | Post-W3c wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf-codegen` | n/a | ~12 s | new iter-loop |

## §8 Dependencies

- **Depends on**: BC.W3b close.
- **Blocks**: BC.W3d.

## §9 Closing posture

The three target sub-crates exist. The dep arrow `bbnf-codegen → bbnf-parse → bbnf-runtime` is the strict acyclic chain.
