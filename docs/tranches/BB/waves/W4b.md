# BB.W4b — Per-Grammar Test + Cookbook + Lifetime-Surfaces.md Gate

**Thesis** Hereupon trybuild test fixtures verify the verbatim error message at lifetime mismatch; `docs/cookbook/lifetime-surfaces.md` lands per F07-2 of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:36`; per-grammar docstrings cite the cookbook by path:line. **Closer-gate** `test -f docs/cookbook/lifetime-surfaces.md`; `cargo doc -p bbnf 2>&1 \| grep -c 'cookbook/lifetime-surfaces'` ≥ 27; `cargo nextest run -p bbnf --test error_messages` 100% pass with verbatim text.

## §1 Deliverable

W4b is the second of two W4 sub-waves. W4a lands the three-surface API; W4b lands the friction-mitigation: cookbook + trybuild test fixtures + per-grammar docstrings.

Per Lane 7 friction-forecast, the three-surface API confuses grammar authors choosing between bumpalo and owned. The cookbook entry at `docs/cookbook/lifetime-surfaces.md` carries:
- §1 Model (slice-borrow primary; bumpalo opt-in; owned opt-in).
- §2 Signatures (per-grammar uniform shape).
- §3 Decision flowchart (where will the parsed value be used?).
- §4 Errors at lifetime mismatch (verbatim error messages).
- §5 Troubleshooting (common pitfalls).

The trybuild fixtures verify the verbatim error text at `crates/core/tests/error_messages/`. When a user writes `let v: JsonValue<'static> = parse(input)?;`, the compiler emits the standard E0597; the bbnf-lang docstring at the parse fn site adds help text linking to the right escape hatch. The trybuild fixture verifies exact stderr output character-for-character.

The per-grammar docstrings: each generated `parse` / `parse_in` / `parse_owned` fn carries a `///` docstring linking to the cookbook by path:line:

```rust
/// Parse JSON input into a slice-borrow JsonValue.
///
/// Default surface — slice-borrow primary; lifetime tied to input.
/// See [`docs/cookbook/lifetime-surfaces.md`](../../docs/cookbook/lifetime-surfaces.md)
/// for the decision flowchart between this, [`parse_in`], and [`parse_owned`].
pub fn parse<'i>(input: &'i str) -> Result<JsonValue<'i>, ParseErr> { ... }
```

The cookbook citation is mechanical (path:line); xtask regen emits the docstrings at the same time as the surface signatures.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W4b verification | Verify W4a three-surface API stable | `cargo nextest run -p bbnf --test surface_composition` 100% pass | W4a baseline holds. |
| M1 | Cookbook lifetime-surfaces.md | Land `docs/cookbook/lifetime-surfaces.md` (≥ 200 LOC; sections §1-§5 per the BB.W4b deliverable spec) | `test -f docs/cookbook/lifetime-surfaces.md && wc -l docs/cookbook/lifetime-surfaces.md \| awk '$1 >= 200'` | Cookbook lands. |
| M2 | Per-grammar docstrings | Each grammar's `parse` / `parse_in` / `parse_owned` carries a docstring citing the cookbook | `cargo doc -p bbnf 2>&1 \| grep -c 'cookbook/lifetime-surfaces'` ≥ 27 (3 surfaces × 9 grammars) | Docstrings cite cookbook. |
| M3 | trybuild fixtures | Land `crates/core/tests/error_messages/lifetime_mismatch.stderr` etc.; verify verbatim text matches the cookbook §4 examples | `cargo nextest run -p bbnf --test error_messages --profile ax-iter` 100% pass | Verbatim error message commitment honoured. |
| M4 | Per-grammar verbatim error | Per Lane 7 (`docs/HARDENING-PLAN-PROMPT.md:139`), the friction error message at lifetime mismatch is committed verbatim; test fixtures verify exact text | `cargo nextest run -p bbnf --test error_messages` reports the verbatim text per F07-E3 of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:26` | Lane 7 friction-forecast committed; error message is a test fixture, not a hope. |

## §3 Closer gate

```sh
test -f docs/cookbook/lifetime-surfaces.md                                          # cookbook lands
wc -l docs/cookbook/lifetime-surfaces.md | awk '$1 >= 200'                         # ≥ 200 LOC
cargo doc -p bbnf 2>&1 | grep -c 'cookbook/lifetime-surfaces'                       # ≥ 27 (3 × 9)
cargo nextest run -p bbnf --test error_messages --profile ax-iter                   # 100% pass with verbatim text
test -f crates/core/tests/error_messages/lifetime_mismatch.stderr                   # trybuild fixture lands
```

## §4 Invariants

§I1. **Lane 7 friction-forecast** — cookbook + trybuild fixtures + per-grammar docstrings mitigate the three-surface friction.
§I2. **F07-2** of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:36` — gate is the BB.W4b tranche-level commitment.
§I3. **F07-E3** of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:26` — verbatim error text is committed.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Cookbook content drifts from trybuild fixtures | Low | M3 + M4 verify the verbatim text matches; the cookbook is the documentation source-of-truth. |
| Per-grammar docstrings reference cookbook paths that move | Low | The cookbook path is stable (`docs/cookbook/lifetime-surfaces.md`); the citation mechanism is mechanical via xtask regen. |

## §6 Cross-references

- **Carry-tags consumed**: (W4a outputs only)
- **Preceding wave**: BB.W4a.
- **Following wave**: BB.W5a.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo doc -p bbnf --profile ax-iter` | ≤ 30 s |
| `cargo nextest run -p bbnf --test error_messages --profile ax-iter` | ≤ 18 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W4b-error-message-verbatim.md` | `docs/tranches/BB/audit/` | Verbatim error messages for lifetime-mismatch + bumpalo-vs-owned |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 7 | F07-2, F07-E3 honoured by cookbook + trybuild |
| Lane 1 | L9 honoured (continuation from W4a) |
