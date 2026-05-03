# BC.W3e — xtask Regen Path Update + Migration Cookbook

**Name**: W3e — xtask Regen Path Update + Migration Cookbook
**Opens after**: BC.W3d close (umbrella `core` slim-down)
**Hard gate**: `xtask/src/regen.rs` writes to `crates/bbnf-parse/src/parse/generated/`; migration cookbook at `docs/migration/bc-core-split.md` complete; `cargo xtask regen --check` byte-identical to BB close at the new path
**Status**: planned

## §1 Deliverable

Per `audit/W3-generated-output-relocation.md` and surgery 22, W3e updates the xtask regen path to write to `crates/bbnf-parse/src/parse/generated/`. The migration cookbook at `docs/migration/bc-core-split.md` (drafted per surgery 34 + F07-7) lands here with consumer-facing import migration tables.

## §2 Milestones

### §2.1 xtask path update
Mechanism: edit `xtask/src/regen.rs` to write to `crates/bbnf-parse/src/parse/generated/<g>.rs` for each grammar.
Sub-gate: `cargo xtask regen --check` produces byte-identical output to BB close.

### §2.2 Stale-path scrub
Mechanism: per `audit/W3-generated-output-relocation.md:§3`, `rg -n "crates/core/src/grammar/generated" docs/ crates/ xtask/ tests/` returns zero post-W3e (or only archived references).
Sub-gate: rg returns zero for non-archive locations.

### §2.3 Migration cookbook landed
Mechanism: `docs/migration/bc-core-split.md` exists with §1 crate ownership / §2 import migration / §3 re-export sunset / §4 build-time impact / §5 troubleshooting. (Cookbook is drafted at re-draft time; this milestone verifies presence.)
Sub-gate: `test -f docs/migration/bc-core-split.md && wc -l docs/migration/bc-core-split.md` > 200.

### §2.4 Workspace test surface verification
Mechanism: per surgery 26 / D08-3, the worktree fixture closure receiver is BC.W5d (not W3e); W3e ensures the workspace tests pass at the post-relocation paths.
Sub-gate: `cargo nextest run --workspace` 100% pass.

### §2.5 Generated-LOC budget verification
Mechanism: per `audit/W3-generated-output-relocation.md:§4`, net delta = 0%.
Sub-gate: `wc -l crates/bbnf-parse/src/parse/generated/*.rs` matches pre-W3e total exactly.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W3e-G1 | xtask writes to new path | `cargo xtask regen --check` byte-identical at new path |
| W3e-G2 | Stale paths scrubbed | `rg -n "crates/core/src/grammar/generated" docs/ crates/ xtask/ tests/ \| grep -v archive` returns zero |
| W3e-G3 | Migration cookbook exists | `test -f docs/migration/bc-core-split.md && wc -l docs/migration/bc-core-split.md` > 200 |
| W3e-G4 | Workspace tests pass | `cargo nextest run --workspace` 100% pass |
| W3e-G5 | LOC delta zero | per-grammar LOC at W3e close == pre-relocation |

## §4 Invariants

§I1. **Lock 6 (xtask emits committed source)**: regen writes to canonical path post-relocation.

§I2. **Lock 13**: `bbnf-parse/src/parse/generated/` is a cohesive sub-tree.

§I3. **No behavioural change**: bytes unchanged; LOC unchanged.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Downstream consumer references stale path | Expected | Migration cookbook records canonical path; downstream rewrites |
| xtask path update misses a sub-path | Low | Diff against BB close exhaustive |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BC.W3d→BC.W3e | BC.W3d | Umbrella shell; sub-crates exist |

### BC-G gates closed
| Gate | Closure |
|---|---|
| BC-G5 | Core crate splits into bbnf-parse / bbnf-codegen / bbnf-runtime; each compiles independently (W3a-G2, W3b-G2, W3c-G2); `bbnf-parse` does not depend on `bbnf-codegen` (W3b-G3) |

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC.W3e→BC.W4 | BC.W4 | Visitor surface formalisation lands at `bbnf-runtime/src/visitor.rs` post-split |
| BC.W3e→BC.W5 | BC.W5 | Sister crate API freeze references the post-split workspace topology |

## §7 Iter-time check

| Activity | Pre-W3e wall | Post-W3e wall | Notes |
|---|---:|---:|---|
| `cargo xtask regen --check` | ≤ 23 s | ≤ 22 s | improved (smaller compile graph) |

## §8 Dependencies

- **Depends on**: BC.W3d close.
- **Blocks**: BC.W4 (visitor lands at post-split paths), BC.W5 (sister freeze references post-split).

## §9 Closing posture

xtask regen writes to canonical path. Migration cookbook ratifies the consumer-facing rename surface. The W3 split chain (W3a → W3b → W3c → W3d → W3e) closes here. Lock 13 honoured at the crate level.
