# BB.W1c — Sheets Direct-To-Struct + Host-Fn Relocation

**Thesis** Hereupon Sheets's `OpenFrame` retires from `crates/core/src/runtime/google_sheets/builder.rs`; the specialised leaf-deposit logic (cell_ref normalisation, identifier resolution, sheet_prefix lookup, error formatting) relocates to per-grammar host namespace `crates/core/src/grammar/host/google_sheets.rs` per surgery 15 + G05-9. **Closer-gate** `rg -n 'enum OpenFrame' crates/core/src/runtime/google_sheets/` returns zero; `tests/parse_with_google_sheets.rs::cell_ref_normalisation` passes; `find crates/core/src/host -name 'sheets.rs' \| wc -l` returns 0; `find crates/core/src/grammar/host -name 'google_sheets.rs' \| wc -l` returns 1.

## §1 Deliverable

W1c is the third of three specialised-grammar sub-waves. Sheets carries 6 OpenFrame variants per `audit/CENSUS-2026-05-03.md:485-490` and specialised leaf-deposit logic for cell_ref, identifier, sheet_prefix, and error. The W1c deliverable retires the OpenFrame substrate AND relocates the specialised leaf-deposit logic to per-grammar host namespace per G05-9 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:32`.

The 6 Sheets OpenFrame variants:

| Variant | Pre-W1c LOC | Migration milestone |
|---|---:|---|
| `CellRef` | ~70 (with normalisation logic) | M1 |
| `Identifier` | ~50 | M2 |
| `SheetPrefix` | ~55 | M2 |
| `RangeRef` | ~60 | M3 |
| `FunctionCall` | ~80 | M3 |
| `ErrorValue` | ~40 | M3 |
| **Frame plumbing** | ~22 | retired with last variant |
| **Total** | **~377** | net post-W1c ~180 LOC |

The host-fn relocation per G05-9 is a critical part of W1c. The pre-W1c CSS host fns at `crates/core/src/host/sheets.rs` (and similar per-grammar files at the generic root) violate G05-9: per-grammar code lives at the grammar-host generic root, not at per-grammar namespace. W1c relocates Sheets's host fns:

```text
   crates/core/src/host/sheets.rs                (PRE-W1c — generic-root pattern; G05-9 violation)
       |
       v MOVE
   crates/core/src/grammar/host/google_sheets.rs (POST-W1c — per-grammar namespace)
```

The relocated host fns:

- `normalise_cell_ref(input_slice) -> NormalisedRef` — collapses `A1`, `$A$1`, `$A1`, `A$1` to canonical form.
- `resolve_identifier(input_slice) -> Identifier` — handles bare identifiers, function-name idents, named-range idents.
- `lookup_sheet_prefix(input_slice) -> SheetPrefix` — separates `Sheet1!A1` into `(SheetPrefix("Sheet1"), CellRef("A1"))`.
- `format_error(input_slice) -> SheetsError` — handles `#REF!`, `#VALUE!`, `#NAME?`, `#DIV/0!`, etc.

Each per-rule parse fn calls the relocated host fn directly. The SheetsStructBuilder consumer reads the normalised value from the parse-fn return value.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W1c verification | Verify W1a + W1b closer-gates passed | `rg -n 'enum OpenFrame' crates/core/src/runtime/{css_l4,bbnf}/` returns 0; `cargo nextest run --workspace --profile ax-iter` 100% pass | W1a + W1b baseline holds. |
| M1 | CellRef variant + normalisation host-fn relocation | Migrate `CellRef` `OpenFrame` variant to direct-projection; relocate `normalise_cell_ref` from `crates/core/src/host/sheets.rs` to `crates/core/src/grammar/host/google_sheets.rs::normalise_cell_ref`; the per-rule parse fn calls the relocated host fn directly | `tests/parse_with_google_sheets.rs::cell_ref_normalisation` passes; `find crates/core/src/host -name 'sheets.rs' \| wc -l` returns 0 | CellRef migrated; G05-9 grammar-authoritative discipline honoured for normalisation. |
| M2 | Identifier + SheetPrefix variants | Migrate `Identifier` and `SheetPrefix` (2 variants) to direct-projection; relocate `resolve_identifier`, `lookup_sheet_prefix` to per-grammar host namespace | `tests/parse_with_google_sheets.rs::identifier && ::sheet_prefix` pass; `find crates/core/src/host -name '*sheets*'` returns nothing | 2 variants migrated; per-grammar host namespace holds 3 host fns. |
| M3 | RangeRef + FunctionCall + ErrorValue | Migrate the remaining 3 variants to direct-projection; relocate `format_error` to per-grammar host namespace | `tests/parse_with_google_sheets.rs::full` passes; `enum OpenFrame` retired entirely from Sheets | All 6 variants migrated; `runtime/google_sheets/builder.rs` net delta ~357 → ~180 LOC. |
| M4 | Per-grammar host namespace verification | Verify all relocated host fns live at `crates/core/src/grammar/host/google_sheets.rs`; verify generic root has no Sheets-specific code | `find crates/core/src/host -type f -name '*.rs' \| xargs rg -l 'cell_ref\|sheet_prefix\|google_sheets'` returns 0 | G05-9 closure verified across all 4 host fns. |
| M5 | Generated-LOC budget close | Per-grammar LOC table; `google_sheets.rs ≤ 13,500` per surgery 21 window | `wc -l crates/core/src/grammar/generated/google_sheets.rs` ≤ 13,500 | The W1c close commit body includes the LOC table. |
| M6 | Sheets host-fns artefact | Land `docs/tranches/BB/audit/W1c-sheets-host-fns.md` recording the inventory of relocated host fns + the per-grammar host namespace verification | `test -f docs/tranches/BB/audit/W1c-sheets-host-fns.md` | Per-grammar host artefact lands. |

## §3 Closer gate

```sh
rg -n 'enum OpenFrame' crates/core/src/runtime/google_sheets/                       # zero hits
cargo nextest run -p bbnf --test parse_with_google_sheets --profile ax-iter         # 100% pass
find crates/core/src/host -name 'sheets.rs' | wc -l                                # 0 (G05-9 honoured)
find crates/core/src/grammar/host -name 'google_sheets.rs' | wc -l                  # 1 (per-grammar namespace)
find crates/core/src/host -type f -name '*.rs' | xargs rg -l 'cell_ref' \| wc -l    # 0 (no Sheets refs at generic root)
wc -l crates/core/src/grammar/generated/google_sheets.rs | awk '$1 < 13500'        # google_sheets.rs ≤ 13,500
test -f docs/tranches/BB/audit/W1c-sheets-host-fns.md                              # host-fns artefact lands
```

All seven conditions must pass; any failure halts W2a dispatch.

## §4 Invariants

§I1. **Lock 1** — Sheets tape and columnar dead.

§I2. **Lock 5** — IR + per-backend lower demonstrates pattern across Sheets's grammar position.

§I3. **Lock 9** — slice-borrow primary.

§I4. **Lock 6** — xtask emits committed source artefacts.

§I5. **G05-9 grammar-authoritative discipline** per surgery 15 + `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:32` — Sheets host fns live at `crates/core/src/grammar/host/google_sheets.rs`, NOT at `crates/core/src/host/sheets.rs`. The generic-root pattern is extinct.

## §5 Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Cell_ref normalisation drops behaviour during migration (e.g., `$A$1` and `A1` no longer collapse to canonical form) | Medium | `tests/parse_with_google_sheets.rs::cell_ref_normalisation` fails | Specialised leaf-deposit becomes per-leaf host fn; the normalisation logic is preserved verbatim in the relocated `crates/core/src/grammar/host/google_sheets.rs::normalise_cell_ref`; per-rule parse fn calls the host fn directly. |
| Function-name lookup PHF table drift during migration (the 200+ Sheets function names) | Medium | `tests/parse_with_google_sheets.rs::function_call` fails on a specific function name | The PHF table is a generated artefact at `crates/core/src/grammar/generated/google_sheets.rs` (xtask regen output); the PHF generation is part of the codegen, not part of the migration. |
| Some imports at the generic root reference `sheets.rs` (the relocated module name) | Low | `cargo check` errors with "no `sheets` in `crate::host`" | Before deletion of `crates/core/src/host/sheets.rs`, verify no callers reference the path; replace all imports with `crate::grammar::host::google_sheets::*`. |

## §6 Cross-references

- **BB-G gates this wave is on the path to closing**: BB-G11 (generated-LOC delta bound; `google_sheets.rs ≤ 13,500`).
- **Carry-tags this wave consumes**: BA→BB.C1 (direct-to-struct emitter scaffolding); BA→BB.C2 (Layout canon); BA→BB.C5 (grammar-agnostic bbnf-ir); BA→BB.C4 (path-core for Sheets path queries).
- **Carry-tags this wave produces**: BB→BC.C2 precursor (direct-to-struct emit generalises to Sheets).
- **Preceding wave dependency**: BB.W1b — BBNF migration; per-variant pattern + bounds-preservation precedent.
- **Following wave consumer**: BB.W2a — cohort template emission consumes the W1{a,b,c} per-variant migration pattern + the host-fn relocation discipline.

## §7 Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target |
|---|---|---|
| `cargo check -p bbnf --profile ax-iter` | ≤ 11 s | n/a |
| `cargo nextest run -p bbnf --test parse_with_google_sheets --profile ax-iter` | ≤ 22 s | 100% |
| `xtask regen --check --grammar google_sheets` | ≤ 7 s | n/a |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W1c-sheets-openframe-retiral.md` | `docs/tranches/BB/audit/` | Per-variant inventory |
| `W1c-sheets-host-fns.md` | same | Inventory of relocated host fns at per-grammar namespace; G05-9 closure verification |
| `W1c-sheets-cell-ref-normalisation.md` | same | Pre/post-W1c cell_ref normalisation behaviour verification |

## §9 Audit lane forecast

| Lane | Anticipated challenge | W1c response |
|---|---|---|
| Lane 1 | "L1 honoured for Sheets?" | M3 verifies `enum OpenFrame` extinct from Sheets |
| Lane 2 | "Same-wave consumer for host-fn relocation?" | M1, M2, M3 each move a host fn AND consume it from a per-rule parse fn in the same commit |
| Lane 4 | W1c has no SOTA gate (Sheets has no external SOTA per surgery 14) | Sheets perf rows removed from per-grammar trajectory table |
| Lane 5 | "G05-9 honoured?" | M4 verifies `find crates/core/src/host -type f -name '*.rs' \| xargs rg -l 'cell_ref\|sheet_prefix\|google_sheets'` returns 0; per-grammar host namespace is the single residence |
| Lane 6 | "Per-wave LOC budget?" | M5 verifies `google_sheets.rs ≤ 13,500` per surgery 21 |
| Lane 7 | Migration is internal | Transparent to grammar authors; existing API unchanged |
| Lane 8 | "Does W1c close any carry?" | W1c extends BA→BB.C1 to Sheets; the BA→BB.C1 carry now covers all 4 specialised grammars (JSON at BA.W5; CSS L4, BBNF, Sheets at W1{a,b,c}) |
