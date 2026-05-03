# BB.W1a — CSS L4 Direct-To-Struct

**Thesis** Hereupon CSS L4's 14-variant `OpenFrame` builder retires from `crates/core/src/runtime/css_l4/builder.rs`; per-variant migration with `tests/css_l4_parity.rs` lightningcss canonical-form parity at each step. **Closer-gate** `rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns zero; `tests/css_l4_parity.rs::full_canonical_form` passes; `cargo bench -p bbnf -- css_l4_bootstrap` ≤ 4.0 ms intermediate; `css_l4.rs ≤ 98,000` LOC per surgery 21 window.

## §1 Deliverable

W1a is the first of three specialised-grammar sub-waves per surgery 25 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:53` and the per-grammar table at `docs/tranches/BB/audit/W1-substrate-migration-per-grammar.md`. CSS L4 is the dominant migration: 14 OpenFrame variants, 1,014 LOC builder, lightningcss parity discipline, 4-MB tailwind stress test as the hardest perf target (BB-G2).

CSS L4's `OpenFrame` builder per `audit/CENSUS-2026-05-03.md:464` carries 14 variants:

| Variant | Pre-W1a LOC contribution | Migration milestone |
|---|---:|---|
| `Declaration` | ~85 | M1 |
| `Color` | ~70 | M2 |
| `ColorFunction` | ~80 | M2 |
| `ColorMix` | ~65 | M2 |
| `HexColor` | ~55 | M2 |
| `Length` | ~55 | M3 |
| `Dimension` | ~75 | M3 |
| `Function` | ~95 | M3 |
| `Calc` | ~85 | M3 |
| `AtRule` | ~110 | M3 |
| `Ratio` | ~50 | M3 |
| `Time` | ~50 | M3 |
| `CustomIdent` | ~50 | M3 |
| `SelectorList` | ~120 | M3 |
| **Frame plumbing** | ~50 | retired with last variant |
| **Total** | **~1,095** | net post-W1a ~340 LOC |

Per `audit/CENSUS-2026-05-03.md:328` the file split (BA.W2 god-module sweep) lands at `runtime/css_l4/builder/{frame, declarations, selectors, color, dimensions, finalize}.rs`. W1a retires the 14 OpenFrame variants from these split files; the per-rule parse fn emission lands at `crates/core/src/grammar/generated/css_l4.rs` directly.

The post-W1a emit pattern follows the JSON direct-to-struct shape demonstrated at BA.W5. Per-rule parse fns directly return typed values; the speculative-branch checkpoint becomes a single `arena.checkpoint()` u32 offset rather than a `Vec<OpenFrame>::clone`. The 86.07% hot-path waste from `audit/RESTART-SKETCH-2026-05-03.md:101-108` is structurally retired.

The CSS L4 parity discipline is critical. `tests/css_l4_parity.rs` exists today and tests against the lightningcss canonical-form emit; W1a milestones extend this test with per-variant byte-equality verification. The test fixtures for color, length, dimension, function, calc, at_rule, ratio, time, custom_ident, selector_list each have dedicated parity tests; M2 + M3 verify each variant migration against its parity test before retiring the OpenFrame variant.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W1a substrate audit | Verify the JSON direct-to-struct path (BA.W5 carry BA→BB.C1) compiles cleanly with the W0a/W0b path-dep relocations; sample a samply trace to confirm `Vec<OpenFrame>::clone` remains absent from JSON profile | `cargo bench -p bbnf -- json_twitter --profile ax-iter` ≤ 400 µs (BA-G1 envelope) | JSON regression-free post-W0; BA close performance preserved. |
| M1 | CSS L4 declaration variant | Migrate `Declaration` `OpenFrame` variant to direct-projection; emit `parse_declaration(input, &mut p, &mut arena, &mut state, &mut cursor) -> Result<CssTypedValue<'p>, ParseErr>`; retire the per-variant frame from `runtime/css_l4/builder/declarations.rs` | `tests/css_l4_parity.rs::declaration_byte_equality` passes against lightningcss canonical-form | The Declaration variant disappears from `enum OpenFrame`; the parse fn emits typed-enum directly. |
| M2 | CSS L4 color variants | Migrate `Color`, `ColorFunction`, `ColorMix`, `HexColor` (4 variants) to direct-projection; each emits its own per-rule parse fn; the host fn `parse_hex_color` (relocated to per-grammar host namespace per surgery 15 + G05-1: `crates/core/src/grammar/host/css_l4.rs`) is consumed by the `HexColor` variant directly | `tests/css_l4_parity.rs::color_byte_equality` passes; 4 variants gone from `enum OpenFrame`; `find crates/core/src/host/ -name 'css_*.rs'` returns 0 (CSS L4 host code lives at per-grammar namespace) | `runtime/css_l4/builder/color.rs` shrinks by ~280 LOC; `enum OpenFrame` retains 9 variants; G05-1 grammar-authoritative discipline honoured. |
| M3 | CSS L4 dimension + selector + function variants | Migrate `Length`, `Dimension`, `Function`, `Calc`, `AtRule`, `Ratio`, `Time`, `CustomIdent`, `SelectorList` (9 variants) to direct-projection; this completes the 14-variant migration | `tests/css_l4_parity.rs::full_canonical_form` passes; `enum OpenFrame` retired entirely from CSS L4 | `runtime/css_l4/builder.rs` net delta: ~1,014 → ~340 LOC. |
| M4 | Generated-LOC budget close | Per-grammar LOC table for `crates/core/src/grammar/generated/css_l4.rs`; `wc -l crates/core/src/grammar/generated/css_l4.rs` ≤ 98,000 per surgery 21 window | LOC measurement matches forecast within ±2% of 98,000 | The W1a close commit body includes the `## Generated-LOC Budget` table per Lane 06. |
| M5 | Samply trace verification | Run `samply record -- cargo bench -p bbnf -- css_l4_bootstrap` post-W1a; verify `OpenFrame::clone` symbol is absent from inclusive-samples list | samply flame-graph shows zero hits on `OpenFrame::clone` | The 86.07% hot-path waste is structurally retired across CSS L4. |
| M6 | Workspace nextest at green | Run `cargo nextest run --workspace --profile ax-iter` 100% pass; verify no regression from BB.W0b baseline | Pass-rate matches BB.W0b baseline | Workspace remains green at W1a close; W1b entry preflight inherits a stable baseline. |
| M7 | CSS L4 14-variant verification | The artefact `docs/tranches/BB/audit/W1a-css-l4-parity.md` records per-variant byte-equality verification (Declaration, Color × 4, Length, Dimension, Function, Calc, AtRule, Ratio, Time, CustomIdent, SelectorList = 14 rows) | Each row records its parity test result + the post-W1a per-rule parse fn signature | The W1a close commit body cites the artefact; BC.W0 IR contract reads the post-W1a emit shape. |

## §3 Closer gate

```sh
rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/                                 # zero hits
cargo nextest run -p bbnf --test css_l4_parity --profile ax-iter                       # 100% pass
cargo bench -p bbnf -- css_l4_bootstrap                                                  # ≤ 4.0 ms intermediate
cargo bench -p bbnf -- json_twitter --profile ax-iter                                  # ≤ 400 µs (no regression)
test -f crates/core/src/runtime/css_l4/builder/frame.rs                                # BA.W2 god-module split persists
test -f crates/core/src/grammar/host/css_l4.rs                                         # G05-1 host fn relocation per surgery 15
find crates/core/src/host/ -name 'css_*.rs' | wc -l                                    # 0 (no CSS host fns at generic namespace)
wc -l crates/core/src/grammar/generated/css_l4.rs | awk '$1 < 98000'                  # CSS L4 ≤ 98,000 per surgery 21
test -f docs/tranches/BB/audit/W1a-css-l4-parity.md                                    # parity artefact lands
```

All nine conditions must pass; any failure halts W1b dispatch.

## §4 Invariants

§I1. **Lock 1** — tape and columnar dead; direct-to-struct visible-and-internal across CSS L4. The substrate IS the typed-enum + slice-borrow.

§I2. **Lock 5** — IR + per-backend lower demonstrates pattern across CSS L4's 9-grammar fleet position. The Rust emitter consumes IR-shape uniformly.

§I3. **Lock 9** — slice-borrow primary; default `parse(input: &'i str) -> Result<CssTypedValue<'i>, ParseErr>`. Every per-rule parse fn emits typed values borrowing slices from input; no eager bumpalo allocation.

§I4. **Lock 6** — xtask emits committed source artefacts. Every regenerated `crates/core/src/grammar/generated/css_l4.rs` lands as committed source; no proc-macro façade.

§I5. **Lock 13** — files >500 LOC outside `generated/` forbidden. `runtime/css_l4/builder.rs` shrinkage from 1,014 → ~340 LOC closes the BA.W2 god-module debt.

§I6. **G05-1 grammar-authoritative discipline** per `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:24` + surgery 15 — CSS host fns live at per-grammar namespace `crates/core/src/grammar/host/css_l4.rs`, NOT at generic root `crates/core/src/host/css_*.rs`.

## §5 Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| 14-variant retiral changes byte-output relative to lightningcss canonical-form, breaking parity | High | `tests/css_l4_parity.rs` byte-equality fails on any variant | Per-variant migration; one variant per commit; parity test runs at each commit; rollback granularity is variant-scoped. |
| The OpenFrame retiral surfaces a hidden checkpoint() / rollback() coupling in non-byte-disjoint Alt that direct-projection cannot model | Medium | Speculative branch in CSS L4 selector_list fails to roll back without OpenFrame snapshot | Direct-projection still supports speculative branches via `attempt_p = *p; attempt_arena = arena.checkpoint();` — the `arena.checkpoint()` is a single u32 offset, NOT a Vec clone. The 86.07% pathology was the `Vec<OpenFrame>::clone`; the arena offset checkpoint is constant-cost. |
| Bench regression beyond +20% from BA close | Medium | `cargo bench -p bbnf -- css_l4_bootstrap` reports > 4.5 ms (vs BA-close baseline) | Profile-driven inquiry via samply at M5; if regression is mechanism-induced, rollback to per-variant granularity until the cause is isolated. |
| LOC window breach: `css_l4.rs` exceeds 98,000 | Low | M4 `wc -l` exceeds 98,000 | Per-grammar generator monitor at xtask regen time; if a single variant's emit exceeds expected LOC, the cost-model integration at BB.W3c re-evaluates the variant's strategy. |

## §6 Cross-references

- **BB-G gates this wave is on the path to closing**: BB-G1 precursor (lightningcss bootstrap parity; closure at BB.W3c with optimiser pipeline); BB-G2 precursor (lightningcss tailwind parity); BB-G11 (generated-LOC delta bound).
- **Carry-tags this wave consumes**: BA→BB.C1 (direct-to-struct emitter scaffolding from JSON demonstration); BA→BB.C2 (Layout/LayoutSink canon); BA→BB.C5 (grammar-agnostic bbnf-ir).
- **Carry-tags this wave produces**: BB→BC.C2 precursor (direct-to-struct emit shape grammar-agnostic; BC.W0 reads post-W1{a,b,c} shape).
- **Preceding wave dependency**: BB.W0b — sister-crate path-deps verified by smoke pass.
- **Following wave consumer**: BB.W1b — BBNF migration consumes the W1a per-variant migration pattern + the host-fn relocation discipline.

## §7 Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target |
|---|---|---|
| `cargo check -p bbnf --profile ax-iter` | ≤ 12 s on M1 Pro | n/a |
| `cargo nextest run -p bbnf --test css_l4_parity --profile ax-iter` | ≤ 25 s | 100% |
| `xtask regen --check --grammar css_l4` | ≤ 8 s | n/a |
| `cargo bench -p bbnf -- css_l4_bootstrap --profile ax-iter` | n/a | ≤ 4.0 ms intermediate |
| `samply record -- cargo bench -p bbnf -- css_l4_bootstrap` | ≤ 60 s | n/a |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W1a-css-l4-parity.md` | `docs/tranches/BB/audit/` | Per-variant byte-equality verification (14 variants) |
| `W1a-openframe-retiral.md` | same | The pre/post-W1a OpenFrame variant inventory; gate that asserts `enum OpenFrame` extinct from CSS L4 |
| `W1a-samply-trace.svg` | same | Flame graph showing `OpenFrame::clone` retired from CSS L4 profile |
| `W1a-css-host-relocation.md` | same | Inventory of CSS L4 host fns relocated to `crates/core/src/grammar/host/css_l4.rs` per G05-1 |

## §9 Audit lane forecast

| Lane | Anticipated challenge | W1a response |
|---|---|---|
| Lane 1 | "Is L1 honoured?" | M3 verifies `rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns zero |
| Lane 2 | "Does W1a land substrate without consumer?" | Each per-variant migration in M1, M2, M3 has a same-commit consumer (the per-rule parse fn replacing the OpenFrame variant); verdict OK |
| Lane 4 | "BB-G1 (lightningcss bootstrap parity) named at W1a" | M0 verifies JSON regression-free; the CSS L4 bench at intermediate ≤ 4.0 ms; BB-G1 final closure at W3c |
| Lane 5 | "Does W1a introduce per-grammar leaks in supposedly-generic crates?" | M2 explicitly relocates CSS host fns to per-grammar namespace per G05-1; `find crates/core/src/host/ -name 'css_*.rs' \| wc -l` returns 0 |
| Lane 6 | "Per-wave LOC budget check?" | M4 emits the per-grammar table; CSS L4 ≤ 98,000 per surgery 21 window |
| Lane 7 | W1a's API surface is internal codegen change; no user friction | Migration is transparent to grammar authors; existing `parse(input)` callers continue to work |
| Lane 8 | "Does W1a close BA→BB.C1 (direct-to-struct generalisation)?" | W1a generalises to CSS L4; W1b/W1c continue to BBNF/Sheets; W2a continues to cohort; BA→BB.C1 closes at W2a in full |
