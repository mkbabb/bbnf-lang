# BB.W5c — Cookbook + Diagnostic Gates

**Thesis** Hereupon `docs/cookbook/path-macro.md`, `docs/cookbook/visitors.md`, `docs/optimizer/pratt-simd-detection.md` land per surgery 34 + F07-1, F07-4, F07-6 of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:35-41`; verbatim `pointer!` ambiguity error message committed as trybuild fixture per surgery 35. **Closer-gate** BB-G12 met; `test -f docs/cookbook/path-macro.md && test -f docs/cookbook/visitors.md && test -f docs/optimizer/pratt-simd-detection.md`; trybuild test fixtures verify verbatim text.

## §1 Deliverable

W5c is the third of three W5 sub-waves. The cookbook + diagnostic surfaces land here.

Per surgery 34 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:71` + Lane 7 friction-forecast, three cookbook pages land at BB.W5c (the lifetime-surfaces.md cookbook landed at BB.W4b):

| Cookbook | Path | Anchor surgery | Friction-forecast row |
|---|---|---|---|
| `docs/cookbook/path-macro.md` | new | 34 | F07-1 (`audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:35`) |
| `docs/cookbook/visitors.md` | new | 34 | F07-6 (`audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:41`) |
| `docs/optimizer/pratt-simd-detection.md` | new | 34 | F07-4 (`audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:39`) |

Each cookbook ≥ 200 LOC content; sections §1 Model / §2 Syntax / §3 Examples / §4 Errors / §5 Troubleshooting per the BB.W5c deliverable spec. Real content; not placeholders.

The verbatim error messages per surgery 35 + G05-8: `pointer!` produces typed terminal paths without turbofish on unambiguous paths; wildcard returns typed iterators; invalid paths include grammar-aware diagnostics. The trybuild fixtures at `crates/path/tests/error_messages/` verify the verbatim text matches the cookbook §4 examples character-for-character.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W5c verification | Verify W5b visitor lands; the verbatim error messages from W5a are in trybuild fixtures | `cargo nextest run -p path --test error_messages --profile ax-iter` 100% pass | W5a + W5b baselines hold. |
| M1 | path-macro cookbook | Land `docs/cookbook/path-macro.md` (≥ 200 LOC; sections §1-§5 per the BB.W5c deliverable spec) per F07-1 | `test -f docs/cookbook/path-macro.md && wc -l docs/cookbook/path-macro.md \| awk '$1 >= 200'` | path-macro cookbook lands. |
| M2 | visitors cookbook | Land `docs/cookbook/visitors.md` (≥ 200 LOC; sections §1-§5) per F07-6 | `test -f docs/cookbook/visitors.md && wc -l docs/cookbook/visitors.md \| awk '$1 >= 200'` | visitors cookbook lands. |
| M3 | pratt-simd-detection optimizer page | Land `docs/optimizer/pratt-simd-detection.md` (≥ 200 LOC; sections §1-§8) per F07-4 + F07-E5 + F07-E6 | `test -f docs/optimizer/pratt-simd-detection.md && wc -l docs/optimizer/pratt-simd-detection.md \| awk '$1 >= 200'` | pratt-simd-detection lands. |
| M4 | trybuild fixtures verify cookbook examples | The cookbook §4 verbatim error messages match the trybuild fixture text character-for-character | `diff <(cat crates/path/tests/error_messages/ambiguity.stderr) <(grep -A 10 'Ambiguity error' docs/cookbook/path-macro.md)` returns matching content | Cookbook + trybuild parity. |
| M5 | Per-grammar docstring citation | Each grammar's `pointer!` callsite docstring + visitor trait docstring cites the cookbook by path:line | `cargo doc -p bbnf -p path 2>&1 \| grep -c 'cookbook/path-macro\|cookbook/visitors'` ≥ 18 (per-grammar usage count) | Docstrings cite cookbook. |

## §3 Closer gate

```sh
test -f docs/cookbook/path-macro.md                                                  # path-macro lands
test -f docs/cookbook/visitors.md                                                    # visitors lands
test -f docs/optimizer/pratt-simd-detection.md                                       # pratt-simd-detection lands
wc -l docs/cookbook/path-macro.md | awk '$1 >= 200'                                # ≥ 200 LOC
wc -l docs/cookbook/visitors.md | awk '$1 >= 200'                                  # ≥ 200 LOC
wc -l docs/optimizer/pratt-simd-detection.md | awk '$1 >= 200'                     # ≥ 200 LOC
cargo nextest run -p path --test error_messages --profile ax-iter                    # 100% pass
cargo doc -p bbnf -p path 2>&1 | grep -c 'cookbook/path-macro\|cookbook/visitors'    # ≥ 18
```

## §4 Invariants

§I1. **Surgery 34** of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:71` — three cookbook pages land at BB.W5c (lifetime-surfaces.md at W4b).
§I2. **Surgery 35** + **G05-8** — `pointer!` produces typed terminal paths; verbatim error messages committed as trybuild fixtures.
§I3. **Friction-forecast Lane 7** — F07-1, F07-4, F07-6 mitigations land via the three cookbook pages.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Cookbook content drifts from trybuild fixtures over time | Low | M4 verifies parity; future cookbook updates require trybuild fixture updates atomically. |
| Cookbook page LOC budget breached (e.g., a cookbook grows beyond a sustainable maintenance burden) | Low | The 200 LOC minimum is the floor; the §1-§5 structure is the cap; verbose narrative is rejected per voice lock §V3. |

## §6 Cross-references

- **BB-G gates closing**: BB-G12.
- **Carry-tags consumed**: (W5a + W5b outputs only)
- **Preceding wave**: BB.W5b.
- **Following wave**: BB.W6.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo doc -p bbnf -p path --profile ax-iter` | ≤ 35 s |
| `cargo nextest run -p path --test error_messages --profile ax-iter` | ≤ 18 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W5c-cookbook-path-macro.md` | `docs/tranches/BB/audit/` | Cookbook content snapshot + trybuild fixture parity verification |
| `W5c-cookbook-visitors.md` | same | Cookbook content snapshot + visitor pruning examples |
| `W5c-optimizer-pratt-simd.md` | same | Optimizer page content snapshot + verbatim warning examples |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 7 | F07-1, F07-4, F07-6 honoured by cookbook + trybuild |
| Lane 5 | G05-8 typed pointer terminal honoured |
