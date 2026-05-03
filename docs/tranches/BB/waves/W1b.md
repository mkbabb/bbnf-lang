# BB.W1b — BBNF Direct-To-Struct

**Thesis** Hereupon BBNF's `OpenFrame` retires from `crates/core/src/runtime/bbnf/builder.rs`; the bounds-recording extension survives as per-rule emission concern; the LSP consumer at `crates/analysis/` continues to read source-position bounds. **Closer-gate** `rg -n 'enum OpenFrame' crates/core/src/runtime/bbnf/` returns zero; `tests/parse_with_bbnf.rs` 100% pass; `cargo nextest run -p bbnf-analysis` 100% pass.

## §1 Deliverable

W1b is the second of three specialised-grammar sub-waves. BBNF carries 8 OpenFrame variants per `audit/CENSUS-2026-05-03.md:441` and a bounds-recording extension that the LSP consumer at `crates/analysis/` reads for source-position diagnostics. The W1b deliverable retires the OpenFrame substrate while preserving bounds-recording as a per-rule emission concern.

The 8 BBNF OpenFrame variants:

| Variant | Pre-W1b LOC | Migration milestone |
|---|---:|---|
| `RuleDef` | ~30 | M1 |
| `Alt` | ~25 | M2 |
| `Seq` | ~25 | M2 |
| `RepeatGroup` | ~30 | M3 |
| `Quantified` | ~25 | M3 |
| `RegexAtom` | ~35 | M3 |
| `KeywordAtom` | ~25 | M3 |
| `RootProgram` | ~30 | M3 |
| **Frame plumbing** | ~18 | retired with last variant |
| **Total** | **~243** | net post-W1b ~120 LOC |

The bounds-recording mechanism deserves explicit handling. The pre-W1b `BbnfStructBuilder` records compound bounds (start/end byte positions for each rule's parsed range); the BBNF self-host LSP at `crates/analysis/` consumes these for source-position diagnostics. The W1b migration preserves bounds-recording as a per-rule emission concern: the layout-lowering pass (BA→BB.C2 carry) emits a `record_bounds: bool` parameter on each per-rule parse fn; the BbnfStructBuilder consumer reads bounds from the parse-fn return value, not from a per-frame slot.

The bounds-recording test at `tests/parse_with_bbnf.rs::bounds_recording` is the regression detector. The pre-W1b test fixture records bounds for each typed-record's start/end byte position; the post-W1b version verifies the same bounds emerge from the per-rule parse fn return. The W1b discipline is mechanical: bounds preservation is a TEST gate; if bounds disappear, the wave halts.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W1b verification | Verify W1a closer-gate passed; CSS L4 `enum OpenFrame` is extinct; BBNF migration starts from a known-stable baseline | `rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns 0; `cargo nextest run --workspace --profile ax-iter` 100% pass | W1a baseline holds; BBNF-specific work proceeds. |
| M1 | RuleDef variant migration | Migrate `RuleDef` `OpenFrame` variant to direct-projection; emit `parse_rule_def(input, &mut p, &mut arena, &mut state, &mut cursor, record_bounds: bool) -> Result<BbnfTypedValue<'p>, ParseErr>`; the `record_bounds` parameter preserves the LSP-consumer contract | `tests/parse_with_bbnf.rs::rule_def` passes; bounds preservation verified via `cargo nextest run -p bbnf-analysis` | RuleDef variant disappears; bounds-recording survives as fn parameter. |
| M2 | Alt + Seq variants | Migrate `Alt` and `Seq` (2 variants) to direct-projection; each emits its own per-rule parse fn with `record_bounds` parameter | `tests/parse_with_bbnf.rs::alt && ::seq` pass | 2 variants gone; `enum OpenFrame` retains 5 variants. |
| M3 | RepeatGroup + Quantified + RegexAtom + KeywordAtom + RootProgram | Migrate the remaining 5 variants to direct-projection; per-rule parse fns carry `record_bounds`; the BbnfStructBuilder consumer reads bounds from parse-fn returns | `tests/parse_with_bbnf.rs::full` passes; `enum OpenFrame` retired entirely from BBNF | All 8 variants gone; `runtime/bbnf/builder.rs` net delta ~243 → ~120 LOC. |
| M4 | LSP consumer verification | Run `cargo nextest run -p bbnf-analysis --profile ax-iter` 100% pass; verify the LSP-consumer's `goto_definition`, `find_references`, `hover` features still resolve via post-W1b bounds emission | All bbnf-analysis tests pass | LSP consumer contract preserved; bounds-recording mechanism transparent across the migration. |
| M5 | Generated-LOC budget close | Per-grammar LOC table; `bbnf.rs ≤ 20,500` per surgery 21 window | `wc -l crates/core/src/grammar/generated/bbnf.rs` ≤ 20,500 | The W1b close commit body includes the LOC table. |
| M6 | Bounds-recording artefact | Land `docs/tranches/BB/audit/W1b-bbnf-bounds-recording.md` recording the pre/post-W1b bounds-emission mechanism + the LSP-consumer integration verification | `test -f docs/tranches/BB/audit/W1b-bbnf-bounds-recording.md` | LSP consumer documentation source-of-truth lands. |

## §3 Closer gate

```sh
rg -n 'enum OpenFrame' crates/core/src/runtime/bbnf/                          # zero hits
cargo nextest run -p bbnf --test parse_with_bbnf --profile ax-iter            # 100% pass
cargo nextest run -p bbnf-analysis --profile ax-iter                          # 100% pass (LSP consumer)
cargo bench -p bbnf -- bbnf_self_host --profile ax-iter                       # bench (no perf regression)
wc -l crates/core/src/grammar/generated/bbnf.rs | awk '$1 < 20500'           # bbnf.rs ≤ 20,500
test -f docs/tranches/BB/audit/W1b-bbnf-bounds-recording.md                   # bounds artefact lands
```

All six conditions must pass; any failure halts W1c dispatch.

## §4 Invariants

§I1. **Lock 1** — BBNF tape and columnar dead; direct-to-struct visible-and-internal across BBNF.

§I2. **Lock 5** — IR + per-backend lower demonstrates pattern; the BBNF self-host grammar is itself a 9-grammar fleet member.

§I3. **Lock 9** — slice-borrow primary; default `parse(input: &'i str) -> Result<BbnfTypedValue<'i>, ParseErr>`.

§I4. **Lock 6** — xtask emits committed source artefacts; every regenerated `crates/core/src/grammar/generated/bbnf.rs` lands as committed source.

§I5. **Bounds-recording preservation** — the LSP consumer at `crates/analysis/` continues to read bounds; the migration is transparent to the LSP layer.

## §5 Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Bounds-recording extension does not survive the migration (recorded bounds disappear) | Medium | `tests/parse_with_bbnf.rs::bounds_recording` test fails; LSP `goto_definition` regresses | Bounds-recording becomes per-rule emission concern at the layout-lowering pass; per-rule parse fns carry an explicit `record_bounds: bool` parameter; the BbnfStructBuilder consumer reads bounds from the parse-fn return value, not from a per-frame slot. M4 explicitly verifies. |
| The W1b commit cascades into BB.W2a cohort migration before W2a is ready | Low | BB.W2a entry preflight finds W1b changes the cohort template generator is not aware of | W1b explicitly TARGETS BBNF; the cohort uses `SimpleStructBuilder` template (`crates/core/src/runtime/builder_template.rs:286`) which W1b does not touch. |
| OpenFrame retiral surfaces hidden checkpoint() / rollback() coupling in BBNF's left-recursive operator chain | Low | `tests/parse_with_bbnf.rs::binary_factor` fails | The `binary_factor` rule (BBNF's Pratt candidate per BB.W3c) does NOT use OpenFrame at the speculative-branch level; the rule is left-recursive recursive descent at W1b; BB.W3c migrates to Pratt as a separate concern. |

## §6 Cross-references

- **BB-G gates this wave is on the path to closing**: BB-G11 (generated-LOC delta bound; `bbnf.rs ≤ 20,500`).
- **Carry-tags this wave consumes**: BA→BB.C1 (direct-to-struct emitter scaffolding); BA→BB.C2 (Layout/LayoutSink canon); BA→BB.C5 (grammar-agnostic bbnf-ir).
- **Carry-tags this wave produces**: BB→BC.C2 precursor (direct-to-struct emit shape generalises to BBNF).
- **Preceding wave dependency**: BB.W1a — CSS L4 OpenFrame retired; per-variant migration pattern established.
- **Following wave consumer**: BB.W1c — Sheets migration consumes the W1{a,b} per-variant migration pattern + the bounds-preservation discipline.

## §7 Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target |
|---|---|---|
| `cargo check -p bbnf --profile ax-iter` | ≤ 11 s | n/a |
| `cargo nextest run -p bbnf --test parse_with_bbnf --profile ax-iter` | ≤ 18 s | 100% |
| `cargo nextest run -p bbnf-analysis --profile ax-iter` | ≤ 30 s | 100% |
| `xtask regen --check --grammar bbnf` | ≤ 6 s | n/a |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W1b-bbnf-openframe-retiral.md` | `docs/tranches/BB/audit/` | Per-variant inventory; gate that asserts `enum OpenFrame` extinct from BBNF |
| `W1b-bbnf-bounds-recording.md` | same | The pre/post-W1b bounds-emission mechanism; LSP-consumer integration verification |
| `W1b-bbnf-self-host-bench.md` | same | BBNF self-host bench measurements pre/post-W1b; verifies no perf regression |

## §9 Audit lane forecast

| Lane | Anticipated challenge | W1b response |
|---|---|---|
| Lane 1 | "L1 honoured for BBNF?" | M3 + M4 verify `enum OpenFrame` extinct + LSP consumer 100% pass |
| Lane 2 | "Does W1b land substrate without consumer?" | Each variant migration has same-commit consumer (per-rule parse fn); LSP test M4 is same-wave consumer for bounds-recording mechanism |
| Lane 4 | W1b has no SOTA gate (BBNF has no external SOTA per surgery 14) | The BBNF perf rows are removed from the per-grammar trajectory table per surgery 14; M4 + M5 are non-SOTA gates |
| Lane 5 | "Per-grammar leaks?" | BBNF host fns (if any) would relocate to `crates/core/src/grammar/host/bbnf.rs` per G05-9; the migration is grammar-authoritative |
| Lane 6 | "Per-wave LOC budget?" | M5 verifies `bbnf.rs ≤ 20,500` per surgery 21 |
| Lane 8 | "Does W1b close any carry?" | W1b extends BA→BB.C1 to BBNF; combined with W1a + W1c + W2a, the carry closes at W2a aggregate |
