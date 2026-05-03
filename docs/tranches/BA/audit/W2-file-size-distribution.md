# BA.W2 File-Size Distribution Artefact — Lock 13 Honour Proof

Date: 2026-05-03
Source: `find /Users/mkbabb/Programming/bbnf-lang/crates -name '*.rs' ! -path '*/generated/*' -exec wc -l {} +`.
Target: post-BA.W2, every bucket within Lock 13 limits.

## §1 — Pre-BA baseline (current state)

Total source files (excluding `generated/`): **824**.

| Bucket | Pre-BA count | % |
|---|---:|---:|
| <50 LOC | 122 | 14.8% |
| 50-99 LOC | 169 | 20.5% |
| 100-249 LOC | 332 | 40.3% |
| 250-499 LOC | 152 | 18.4% |
| ≥500 LOC | 49 | 5.9% |

Files ≥500 LOC: 49. Of these, 23 are production source under `crates/*/src/` (the kill-list at `audit/CENSUS-2026-05-03.md:319-353`); the remaining 26 are test files (per `audit/CENSUS-2026-05-03.md:329-330` only two test files explicitly split — `crates/ir/tests/shape_dispatch.rs:1438` and `crates/ir/tests/structural_alphabet_extended.rs:1410`); per `audit/CENSUS-2026-05-03.md:122` test surfaces may carry concrete grammars (DEFER) and the file-count discipline does not apply uniformly.

## §2 — Post-BA.W2 target (production source `crates/*/src/`)

Per BA.W2's god-module split surface (23 → 0 files >500 LOC outside `generated/`):

| Bucket | Post-W2 count target | % |
|---|---:|---:|
| <50 LOC | ≤ 5% of total | bound |
| 50-99 LOC | ~25% | natural |
| 100-249 LOC | ≥ 50% (dominant) | cohesive |
| 250-499 LOC | ~20% | natural |
| ≥500 LOC | **0** | Lock 13 |

The Lock 13 close: zero files ≥500 LOC under `crates/*/src/` (excluding `generated/`). The lower bound: <50 LOC ≤ 5% (avoids micro-fragmentation per `feedback_no_god_modules` "every level... separates concerns; 'utils'/'helpers'/'common' kitchen sinks are god modules in gestation" — the inverse rule prevents 5-LOC stub files that fragment cohesive concerns).

## §3 — Per-directory child-count discipline

Per Lock 13 ("cohesive encapsulation at every level"), every post-W2 directory has 4-10 children:

| Directory | Pre-W2 children | Post-W2 children | Status |
|---|---:|---:|---|
| `crates/core/src/` (top-level) | 9 | 9 | within bounds |
| `crates/core/src/backend/rust/emitter/shapes/` | 8 | 8-10 | within bounds |
| `crates/core/src/backend/rust/emitter/shapes/flat/` | 1 file | 4-5 (post W2.M2 split) | within bounds |
| `crates/core/src/runtime/css_l4/` (post-split builder) | 7 | 8 | within bounds |
| `crates/ir/src/passes/` | 8 | 8-10 | within bounds |
| `crates/ir/src/passes/recognizers/grammar_facts/` | 1 file | 12-13 | **>10 — flag** |

The `grammar_facts/` post-split (per `audit/MODULES-2026-05-03.md:1213-1219` recogniser families) projects to 12-13 children. The split discipline at W2.M1 collapses related miners into sub-sub-modules: `grammar_facts/{alt_classifier,chain_facts,branch_uniqueness,context,quoted,balanced,comment,identifier,separator,tokens,punct,delim_scan,key_dispatch}.rs` — 13 children mixing concerns. The mitigation: group into 3-4 cohesive sub-directories (`grammar_facts/{classifier,context,probes,scanner}/`); per-directory child count drops to 4-7. This is in-W2 surgery; the resulting tree obeys Lock 13.

## §4 — Verification gate

The W2.M4 closer gate (per `docs/tranches/BA/waves/W2.md`):

```
find crates -name '*.rs' ! -path '*/generated/*' \
  | xargs wc -l | awk '$1>=500 && $2!="total"' | wc -l
```

Expected: `0` (excluding test fixtures DEFER per `audit/CENSUS-2026-05-03.md:122`; production source under `crates/*/src/` is the gated surface).

The histogram artefact updates at W2 close to record the actual post-split bucket distribution; W6.M3 verifies against the §2 target table.

## §5 — Test-fixture exception (DEFER per CENSUS:122)

Test files >500 LOC at BA close (informational; not Lock 13 gated):

| Path | LOC | Justification |
|---|---:|---|
| `crates/csp-solver/tests/solver.rs` | 1,667 | CSP solver fixture — instantiates concrete constraints |
| `crates/core/tests/common/css_normalize.rs` | 1,590 | CSS normalisation parity fixture |
| `crates/lsp/tests/integration.rs` | 1,468 | LSP integration; multi-grammar fixture |
| `crates/ir/tests/shape_dispatch.rs` | 1,438 | **W2.M5 splits** per shape family |
| `crates/ir/tests/structural_alphabet_extended.rs` | 1,410 | **W2.M5 splits** per alphabet probe |
| `crates/ir/tests/vm/interpreter.rs` | 1,029 | VM interpreter fixture |
| `crates/csp-solver/tests/lattice.rs` | 985 | CSP lattice fixture |
| `crates/ir/tests/lattices/types.rs` | 907 | Type-lattice fixture |
| `crates/core/tests/typed_accessor_surface.rs` | 795 | Typed accessor surface fixture |
| `crates/core/tests/pipeline.rs` | 763 | Pipeline integration fixture |
| `crates/core/tests/sheets_expr_parity.rs` | 722 | Sheets parity fixture |
| `crates/core/tests/sheets_parity.rs` | 711 | Sheets parity fixture |
| `crates/ir/tests/passes/passes_dispatch.rs` | 699 | Passes dispatch fixture |
| `crates/core/tests/named_pipeline_probe.rs` | 698 | Named-pipeline probe fixture |
| `crates/ir/tests/substrate_audit.rs` | 636 | Substrate audit fixture (consumed at W6.M5) |
| `crates/ir/tests/vm/cost_weights_unified.rs` | 602 | Cost-weights fixture |
| `crates/ir/tests/passes/passes_prefix.rs` | 592 | Passes prefix fixture |

Two test files (`shape_dispatch.rs`, `structural_alphabet_extended.rs`) are explicitly W2.M5-split per `audit/CENSUS-2026-05-03.md:329-330`. The remaining test files are KEEP (test fixtures permitted concrete grammars per CENSUS:122 DEFER); the file-count gate applies to `crates/*/src/` only.

## §6 — Closer disposition

Post-W2:

- `find crates -name '*.rs' ! -path '*/generated/*' -path '*/src/*' | xargs wc -l | awk '$1>=500'` returns empty.
- `find crates -name '*.rs' ! -path '*/generated/*' | xargs wc -l | awk '$1<50' | wc -l` returns ≤ 5% of total file count.
- Every directory under `crates/*/src/` has 4-10 children.

The post-W2 actuals are recorded at W2 close; W6.M3 cross-references this artefact to verify Lock 13 honoured.
