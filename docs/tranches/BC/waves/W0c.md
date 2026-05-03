# BC.W0c — Sibling Baseline + AscentStrategy Disposition

**Name**: W0c — Sibling Baseline + AscentStrategy Disposition
**Opens after**: BC.W0b close (smoke lowerer landed)
**Hard gate**: sibling baseline captured at `docs/tranches/BC/audit/W0-sibling-baseline.txt`; AscentStrategy disposition recorded; AscentStrategy enum + emit sites deleted; `rg -n 'AscentStrategy' crates/ tests/ docs/codegen-IR-CONTRACT.md` returns zero
**Status**: planned

## §1 Deliverable

W0c executes two disjoint actions per surgery 29 (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:69`) and D08-5 (`audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:14`):

1. Capture sibling repository state for the five tracked siblings (`parse-that`, `pprint`, `csc411/csp-solver`, `bbnf-buddy`, `ffuzzy`) before BC edits cross-repo. The capture grounds BC.W5d's reconciliation.
2. Delete `AscentStrategy` per `docs/tranches/BC/audit/W0-ascent-strategy-disposition.md`. The PrattSpine variant from `audit/W0-typed-ir-variant-table.md:§2` subsumes left-recursion handling; AscentStrategy as a separate substrate is an orthogonal codepath per `feedback_no_orthogonal_codepaths`.

## §2 Milestones

### §2.1 Sibling baseline captured

Mechanism: per `audit/HARDENING-SYNTHESIS-2026-05-03.md:158-164`, run `git rev-parse --short HEAD` and `git status --short` for each sibling. Record one row per sibling.

Files: `docs/tranches/BC/audit/W0-sibling-baseline.txt` (populate; the placeholder created at re-draft time is replaced with concrete SHAs).

Sub-gate: file lists all five siblings with HEAD SHA + status; W5d references this file.

### §2.2 AscentStrategy enum deletion

Mechanism: walk the IR types module; locate `AscentStrategy` enum; delete the enum and all variants. Per `feedback_no_workarounds`, zero tolerance for legacy code.

Files: `crates/ir/src/passes/types/mod.rs` (or post-BA.W2 `crates/ir/src/passes/layout/mod.rs`) — delete the enum.

Sub-gate: `rg -n 'enum AscentStrategy' crates/` returns zero.

### §2.3 AscentStrategy emit-site deletion

Mechanism: walk `crates/core/src/codegen/rust/`; locate `ascent.rs`, `shapes/ascent_dispatcher.rs`, `lowering/ascent_lowering.rs` (or analogous per BA.W2 split); delete the files. Any unique left-recursion logic merges into `bbnf-codegen::optimiser::pratt_detect` per `feedback_no_orthogonal_codepaths`.

Files: codegen sites referencing AscentStrategy — delete or merge.

Sub-gate: `rg -n 'AscentStrategy' crates/core/src/codegen/` returns zero.

### §2.4 Test deletion

Mechanism: any tests referencing AscentStrategy must delete or migrate to PrattSpine equivalent. The PrattSpine round-trip test from BC.W0b §2.4 covers the same surface for left-recursion grammars (BBNF's `binary_factor`).

Files: `crates/core/tests/ascent_strategy_*.rs` (delete if exists).

Sub-gate: `rg -n 'AscentStrategy' tests/` returns zero.

### §2.5 Documentation scrub

Mechanism: ensure no documentation references `AscentStrategy` post-deletion. The contract spec at `docs/codegen-IR-CONTRACT.md` does not mention AscentStrategy; `audit/W0-typed-ir-variant-table.md:§2` references PrattSpine instead.

Files: scrub `docs/codegen-IR-CONTRACT.md`, `docs/GESTALT.md`, BC waves.

Sub-gate: `rg -n 'AscentStrategy' docs/` returns zero (or only archived references with date stamp).

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W0c-G1 | Sibling baseline captured | `test -s docs/tranches/BC/audit/W0-sibling-baseline.txt && grep -c '^[a-z-]\+:\|HEAD\s' docs/tranches/BC/audit/W0-sibling-baseline.txt` ≥ 5 |
| W0c-G2 | AscentStrategy enum deleted | `rg -n 'enum AscentStrategy' crates/` returns zero |
| W0c-G3 | AscentStrategy emit sites deleted | `rg -n 'AscentStrategy' crates/core/src/codegen/` returns zero |
| W0c-G4 | AscentStrategy tests deleted | `rg -n 'AscentStrategy' tests/ crates/core/tests/` returns zero |
| W0c-G5 | AscentStrategy docs scrubbed | `rg -n 'AscentStrategy' docs/codegen-IR-CONTRACT.md docs/GESTALT.md` returns zero |
| W0c-G6 | Workspace builds clean | `cargo check --workspace` green; `cargo nextest run --workspace` 100% pass |

## §4 Invariants

§I1. **No orthogonal codepaths**: PrattSpine is the singular dispatch path for left-recursion per `feedback_no_orthogonal_codepaths`.

§I2. **No workarounds**: `feedback_no_workarounds` zero tolerance; AscentStrategy is excised, not deprecated.

§I3. **Sibling baseline real**: the file contains concrete SHAs, not placeholders, post-W0c close.

§I4. **No regression**: deletion does not regress workspace test suite; the PrattSpine path covers all left-recursion test cases.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| AscentStrategy emit site has unique logic not covered by PrattSpine | Low | BB.W3's PrattSpine mining covers all observed left-recursion patterns; audit BBNF's binary_factor + math grammar's expression rule pre-W0c |
| Sibling baseline drift between W0c capture and W5d reconciliation | Low | The baseline is a snapshot; drift due to upstream pulls is acceptable and recorded in W5d's reconciliation document |
| Deletion breaks an external doc reference | Low | W0c §2.5 scrubs docs; cross-reference verified |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BC.W0a→BC.W0c | BC.W0a | Typed IR has PrattSpine; AscentStrategy is now redundant |

### BC-G gates closed

(none directly — W0c is a disjoint cleanup wave)

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W0c→BC.W5d | BC.W5d | The sibling baseline grounds the W5d reconciliation |

## §7 Iter-time check

| Activity | Pre-W0c wall | Post-W0c wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf-ir` | ~10 s | ~9 s | minor reduction from AscentStrategy deletion |
| `cargo check -p bbnf` | ~22 s | ~22 s | unchanged |

## §8 Dependencies

- **Depends on**: BC.W0a close (PrattSpine variant defined; the substrate AscentStrategy is replaced by lives in W0a).
- **Blocks**: BC.W5d (consumes the sibling baseline for reconciliation).

## §9 Closing posture

W0c is a janitorial wave: capture the sibling state, delete the orphan enum. Two distinct concerns sharing a sub-wave because both are short and both are non-substrate (no IR contract change). Era V structurally precluded because both items have known consumers (W5d for sibling baseline; PrattSpine for AscentStrategy's role).
