# BC.W3a — `bbnf-runtime` Extraction

**Name**: W3a — bbnf-runtime Extraction
**Opens after**: BC.W2 close (TS + WASM scaffolds compile against typed IR)
**Hard gate**: `crates/bbnf-runtime/` exists as workspace member; `cargo check -p bbnf-runtime` green; `cargo nextest run -p bbnf-runtime` 100% pass for runtime-owned tests
**Status**: planned

## §1 Deliverable

Per `audit/W3-crate-dependency-dag.md:§6`, W3 splits into five disjoint sub-waves: W3a (`bbnf-runtime`), W3b (`bbnf-parse`), W3c (`bbnf-codegen`), W3d (umbrella `core` slim-down), W3e (xtask path update + migration cookbook). W3a is the first extraction; it lands `bbnf-runtime` as a workspace member.

Per surgery 8 (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:60`), `bbnf-runtime` **depends on** `crates/path/`; it does NOT absorb `path/`. The path crate triplet from BA.W3 (path, path-core, path-ts) is unchanged.

## §2 Milestones

### §2.1 `bbnf-runtime` directory creation

Mechanism: create `crates/bbnf-runtime/` with `Cargo.toml`, `src/lib.rs`, `src/{visitor,handle,arena}.rs`. Add to workspace `[members]`.

Files: `crates/bbnf-runtime/Cargo.toml`, `crates/bbnf-runtime/src/lib.rs` (create).

Sub-gate: `cargo check -p bbnf-runtime` compiles a minimal lib.rs (trivial green).

### §2.2 Move per-grammar runtime modules

Mechanism: move `crates/core/src/runtime/<g>/` to `crates/bbnf-runtime/src/runtime/<g>/` for each of nine grammars.

Files: per-grammar runtime modules — move from `crates/core/src/runtime/` to `crates/bbnf-runtime/src/runtime/`.

Sub-gate: `find crates/bbnf-runtime/src/runtime/ -maxdepth 1 -type d \| wc -l` returns 9; per-grammar module structure preserved.

### §2.3 Move handle + arena

Mechanism: move `crates/core/src/handle.rs` (or analogous) to `crates/bbnf-runtime/src/handle.rs`; move arena types similarly.

Files: handle.rs, arena types — relocate.

Sub-gate: `cargo check -p bbnf-runtime` green; tests pass.

### §2.4 Move visitor trait

Mechanism: per BC.W4 §2.1 (which lands in W4 post-W3), the visitor trait will live at `crates/bbnf-runtime/src/visitor.rs`. W3a creates the file as a stub; W4 populates.

Files: `crates/bbnf-runtime/src/visitor.rs` (create stub).

Sub-gate: stub compiles; W4 populates the trait surface.

### §2.5 Move path runtime (per surgery 8)

Mechanism: per surgery 8 + BA.W3 surgery 7, the typed-path executor at `crates/core/src/path/` was moved to `crates/path/src/runtime/` at BA. `bbnf-runtime` *depends on* `crates/path/`, never absorbing it. Verify BA.W3's relocation lands; if `crates/core/src/path/` is empty post-BA, no W3a action needed.

Files: verify `crates/core/src/path/` empty or deleted post-BA.W3.

Sub-gate: `find crates/core/src/path -name '*.rs' \| wc -l` returns zero post-BA.W3.

### §2.6 Define `bbnf-runtime` public surface

Mechanism: per `audit/W3-crate-dependency-dag.md:§2 bbnf-runtime`, the public surface includes `Visitor<'i, T>`, `Visit<'i, T>`, `VisitTypes`, `<G>Document`, `<G>Value`, `<G>Arena`, `LazyValue<'a>`, `PathQuery`, `pointer!` re-export.

Files: `crates/bbnf-runtime/src/lib.rs` (modify-carve to export).

Sub-gate: `cargo doc -p bbnf-runtime --no-deps` produces clean output naming all public surface.

### §2.7 Workspace topology pre-flight

Mechanism: per `audit/MODULES-2026-05-03.md:1149-1156`, verify zero circular dependencies. `cargo metadata --format-version 1 \| jq '.workspace_members' \| length` returns 13 (12 + 1 for new bbnf-runtime).

Files: `docs/tranches/BC/audit/W3a-topology.md` (create).

Sub-gate: workspace member count = 13; no circular deps.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W3a-G1 | `bbnf-runtime` exists as workspace member | `cargo metadata --format-version 1 \| jq '.workspace_members \| length'` returns 13 |
| W3a-G2 | Compiles independently | `cargo check -p bbnf-runtime` green |
| W3a-G3 | Tests pass | `cargo nextest run -p bbnf-runtime` 100% pass |
| W3a-G4 | Path crate unmodified | `cargo check -p path -p path-core -p path-ts` green; W3a does not touch path crate |
| W3a-G5 | Public surface documented | `cargo doc -p bbnf-runtime --no-deps` clean |

## §4 Invariants

§I1. **Lock 13 (cohesive crate)**: `bbnf-runtime` owns one concern (runtime).

§I2. **Lock 7 (`crates/path/` consolidation)**: `bbnf-runtime` depends on `crates/path/`; does not absorb per surgery 8.

§I3. **No regression**: behaviour unchanged; per-grammar parity tests still green via the umbrella.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Per-grammar runtime modules reference path types via `crate::path::*` paths that no longer resolve | Medium | Update imports to `path::*` or `bbnf_runtime::path::*` (re-export from path crate); migration cookbook records |
| Test fixtures at `crates/core/tests/runtime_<g>.rs` reference old paths | Medium | Test relocates with the runtime module; workspace test surface re-verifies at W3 close |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BC.W2→BC.W3a | BC.W2 | Scaffolds at `crates/core/src/codegen/{ts,wasm}/` will move at W3c |

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W3a→BC.W3b | BC.W3b | bbnf-runtime exists; bbnf-parse can declare path-dep |

## §7 Iter-time check

| Activity | Pre-W3a wall | Post-W3a wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf` | ~22 s | ~24 s | umbrella adds re-export resolution |
| `cargo check -p bbnf-runtime` | n/a | ~8 s | new sub-crate iter-loop (smallest) |

## §8 Dependencies

- **Depends on**: BC.W2 close (TS + WASM scaffolds compiled against IR contract).
- **Blocks**: BC.W3b (`bbnf-parse` depends on `bbnf-runtime`).

## §9 Closing posture

W3a is the first of five W3 sub-waves. `bbnf-runtime` lands; path crate untouched per surgery 8. The runtime-only iter loop emerges (~8 s).
