# BC.W3b — `bbnf-parse` Extraction

**Name**: W3b — bbnf-parse Extraction
**Opens after**: BC.W3a close (`bbnf-runtime` exists)
**Hard gate**: `cargo check -p bbnf-parse` green; `bbnf-parse` does NOT depend on `bbnf-codegen`; `cargo nextest run -p bbnf-parse` 100% pass
**Status**: planned

## §1 Deliverable

Extract `bbnf-parse` from `crates/core/`. Per `audit/W3-crate-dependency-dag.md:§2 bbnf-parse`, includes `source/`, `parse/`, `lower/`, `host/`, `pipeline/`, and `parse/generated/` (post-W3e relocation).

`bbnf-parse` depends on `bbnf-runtime` (lower/ reads `bbnf-runtime::<g>::<G>View` for self-host BBNF reflection); on `bbnf-ir` for typed-IR construction; on `parse-that` for the BBNF self-host parser combinator surface. **MUST NOT depend on `bbnf-codegen`.**

## §2 Milestones

### §2.1 Crate creation
Mechanism: `crates/bbnf-parse/Cargo.toml` + `src/lib.rs`. Add to workspace `[members]`.
Sub-gate: `cargo metadata` shows 14 members.

### §2.2 Move source/parse/lower/host/pipeline
Mechanism: relocate source modules from `crates/core/src/` to `crates/bbnf-parse/src/`.
Sub-gate: per-module compilation green.

### §2.3 Move generated/
Mechanism: per `audit/W3-generated-output-relocation.md`, `crates/core/src/grammar/generated/` moves to `crates/bbnf-parse/src/parse/generated/`. Bytes unchanged.
Sub-gate: nine generated grammar files relocated; byte-identical to BB close.

### §2.4 Configure deps
Mechanism: `bbnf-parse/Cargo.toml` declares `bbnf-runtime`, `bbnf-ir`, `parse-that` as deps; explicitly does NOT include `bbnf-codegen`.
Sub-gate: `cargo tree -p bbnf-parse \| grep -c bbnf-codegen` returns 0.

### §2.5 Define public surface
Mechanism: `bbnf-parse/src/lib.rs` exports `compile_grammar`, `Layout`, `LayoutSink`, `GrammarIR`, `<G>Parser`, `parse_<g>_source`, etc.
Sub-gate: `cargo doc -p bbnf-parse --no-deps` clean.

### §2.6 Per-grammar host namespaces (G05-1)
Mechanism: per G05-1, host fns live at `bbnf-parse/src/host/<g>/` (e.g., `bbnf-parse/src/host/css_l4/`, `bbnf-parse/src/host/google_sheets/`); not at flat `host/` root.
Sub-gate: `find crates/bbnf-parse/src/host -maxdepth 2 -type d` shows per-grammar subdirs; `rg -nE 'pub fn (parse_hex_color\|parse_url)' crates/bbnf-parse/src/host/css_l4/` returns expected sites.

### §2.7 Workspace test surface
Mechanism: per-crate tests pass.
Sub-gate: `cargo nextest run -p bbnf-parse` 100% pass.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W3b-G1 | `bbnf-parse` exists | `cargo metadata --format-version 1 \| jq '.workspace_members \| length'` returns 14 |
| W3b-G2 | Compiles independently | `cargo check -p bbnf-parse` green |
| W3b-G3 | No `bbnf-codegen` dep | `cargo tree -p bbnf-parse \| grep -c bbnf-codegen` returns 0 |
| W3b-G4 | Generated files relocated | `find crates/bbnf-parse/src/parse/generated -name '*.rs' \| wc -l` returns 9 |
| W3b-G5 | Per-grammar host namespaces | `find crates/bbnf-parse/src/host -maxdepth 2 -type d \| wc -l` returns ≥ 4 (one per host-bearing grammar) |
| W3b-G6 | Tests pass | `cargo nextest run -p bbnf-parse` 100% pass |

## §4 Invariants

§I1. **Lock 5**: bbnf-parse produces grammar IR; `bbnf-codegen` consumes it.

§I2. **Lock 13**: cohesive crate (parse concern only).

§I3. **No `bbnf-codegen` dep** (W3-G3 enforcer).

§I4. **G05-1 per-grammar host namespaces** (no `host/<g>.rs` flat).

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Lower module references runtime types via stale paths | Medium | Update `use crate::runtime::<g>::<G>View` to `use bbnf_runtime::<g>::<G>View` |
| Test fixtures at old paths | Medium | Relocate with crate; test surface verified |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BC.W3a→BC.W3b | BC.W3a | bbnf-runtime exists |

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W3b→BC.W3c | BC.W3c | bbnf-parse exists; bbnf-codegen can declare path-dep |

## §7 Iter-time check

| Activity | Pre-W3b wall | Post-W3b wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf-parse` | n/a | ~14 s | new sub-crate iter-loop |

## §8 Dependencies

- **Depends on**: BC.W3a close.
- **Blocks**: BC.W3c.

## §9 Closing posture

bbnf-parse extracted. The dep arrow `bbnf-parse → bbnf-runtime` lands; the no-bbnf-codegen invariant enforced. Per-grammar host namespaces honour G05-1.
