# BA.W5b — CSS L4 Direct-To-Struct Migration

**Thesis** (the CSS L4 14-variant `OpenFrame` substrate retires; per-variant typed-enum emission lands across declaration / color / color_function / color_mix / selector_list / hex_color / etc. frames; per-rule generated `parse_<rule>(...)` functions return typed `CssTypedValue<'p>` directly; lightning-css parity tests pass; CSS L4's BA close internal-progress gate BA-G1b records bootstrap.css parse regression-bound). **Closer-gate** (post-W5b `rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns 0; lightning-css parity tests at `tests/parse_with_css_l4.rs` 100% pass; `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)' --profile ax-iter` 100% pass; CSS L4 typed-enum lands per all 14 `OpenFrame` variants).

## §1 — Deliverable

Hereupon CSS L4's 14-variant `OpenFrame` taxonomy at `crates/core/src/runtime/css_l4/builder.rs:61-200` (1,014 LOC per `audit/MODULES-2026-05-03.md:951` — second-largest core file) retires by mechanism. The 14 variants — declaration, color, color_function, color_mix, selector_list, hex_color, function_call, at_rule, length_unit, dimension, hue, alpha, percent, at_block — each map to a typed `CssTypedValue<'p>` enum constructor at codegen. Per `audit/CENSUS-2026-05-03.md:328` the W2 split distributes the 14-variant builder into per-variant frame modules under `builder/<variant>.rs` (declaration, color, color_function, color_mix, selector_list, hex_color, etc.); W5b consumes the post-W2 split as the canonical surface for direct-projection emit.

Per `docs/tranches/BA/audit/W5-generated-parser-shape.md` §1.1-§1.10 the per-construct emission shapes are JSON-side canonical; CSS L4's deviations:

- **Alt (speculative case)**: per W5-generated-parser-shape §1.2, CSS L4's `value` rule has overlapping FIRST sets (color names overlap with function names; `rgb(...)` and `rgba(...)` share `r` prefix). The classifier emits speculative `attempt_p` for these cases per the canonical `audit/RESTART-SKETCH-2026-05-03.md:582-590` shape — but with O(1) tuple Checkpoint, not `Vec<OpenFrame>::clone`.
- **Keyword (≥ 4 case)**: CSS L4 declares many color names (`red`, `green`, `blue`, `aqua`, ...) so `phf::Map` lookup emits per W5-generated-parser-shape §1.7 ≥ 4-keyword case.
- **HostCall**: CSS L4 has multiple host fns (e.g. `parse_hex_color` per `audit/CENSUS-2026-05-03.md:109`); under surgery #15, host fns relocate to `crates/core/src/grammar/host/css_l4/` per-grammar namespace. W5b's emit references `crate::grammar::host::css_l4::parse_hex_color(...)` per W5-generated-parser-shape §1.10.
- **CharClass (medium-density)**: CSS L4's identifier and length-unit classes are mid-density; SIMD shuffle emits per `simd_eligibility = "auto"` metadata.

Per `audit/CENSUS-2026-05-03.md:951` the post-W2 split is the precondition; W5b begins with the per-variant frame modules already in place. The direct-projection emission per variant frame:

```rust
fn parse_declaration<'p>(
    input: &'p [u8], p: &mut usize, arena: &mut CssArena<'p>,
    state: &mut ScanState, cursor: &mut PathCursor<'_>,
) -> Result<CssDeclaration<'p>, ParseErr> {
    let property = parse_property(input, p)?;       // CowArcStr<'p>
    skip_space(input, p, state);
    expect_byte(input, p, b':')?;
    skip_space(input, p, state);
    let value = parse_css_value(input, p, arena, state, cursor)?;  // CssTypedValue<'p>
    let important = parse_important_flag(input, p);
    Ok(CssDeclaration { property, value, important })
}
```

Each of the 14 variants follows: typed children bind to typed struct fields; the MapExpr (`-> CssDeclaration { property, value, important }`) compiles to direct struct constructor; no `OpenFrame::Declaration` push, no `commit_declaration` indirection. The `arena.intern_*` surfaces remain for slab-allocated arrays/objects per `crates/core/src/runtime/css_l4/arena.rs:390` (CssArena per `audit/MODULES-2026-05-03.md:950`).

The performance trajectory: lightning-css's bootstrap.css parse is 4.16 ms per `audit/SOTA-2026-05-03.md:130` (vs cssnano's 544 ms). BA's W5b internal-progress gate BA-G1b records post-W5b bootstrap.css parse measurement; no SOTA claim at BA close (lightning-css's 4.16 ms remains the BB-tranche SOTA target per BB-G1). Per `feedback_no_workarounds_arch` the architectural transposition (14-variant retiral) is the deliverable; the BB-tranche SOTA pursuit consumes it.

The Era V failure mode is closed for CSS L4 at W5b; the same-wave consumer is the lightning-css parity test cohort + `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)'` at 100% pass.

## §2 — Milestones

> **M0 — Per-variant typed-enum lands for 14 OpenFrame variants**
>
> *Surface*: `crates/core/src/runtime/css_l4/value.rs` (852 LOC pre-W2 per `audit/MODULES-2026-05-03.md:333`; post-W2 split per `audit/CENSUS-2026-05-03.md:333`); `crates/core/src/runtime/css_l4/builder/<variant>.rs` (post-W2.M? split per `audit/CENSUS-2026-05-03.md:328`).
> *Action*: For each of the 14 OpenFrame variants, emit a typed-enum constructor in `CssTypedValue<'p>`: `Declaration(CssDeclaration<'p>)`, `Color(CssColor)`, `ColorFunction(CssColorFunction<'p>)`, `ColorMix(CssColorMix<'p>)`, `SelectorList(CssSelectorList<'p>)`, `HexColor(CssHex)`, `FunctionCall(CssFunctionCall<'p>)`, `AtRule(CssAtRule<'p>)`, `LengthUnit(CssLengthUnit)`, `Dimension(CssDimension)`, `Hue(CssHue)`, `Alpha(CssAlpha)`, `Percent(CssPercent)`, `AtBlock(CssAtBlock<'p>)`. Each variant has its own struct; the typed-enum is the rich AST per `feedback_preserve_rich_ast`.
> *Gate*: 14 variants present in `CssTypedValue<'p>`; the AST shape preserved.
> *Exit-criteria*: `rg -n 'CssTypedValue::' crates/core/src/runtime/css_l4/value.rs | wc -l | tr -d '\n'` returns ≥ 14; the typed-enum compiles.

> **M1 — Per-rule `parse_<rule>` direct-projection emit for CSS L4**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct/{header,body,fields,finalize}.rs`; the CSS L4 grammar at `grammar/css_l4/css_l4.bbnf`; the regen target `crates/core/src/grammar/generated/css_l4.rs` (107,138 LOC pre-W5b per MODULES:622).
> *Action*: Implement direct-projection emit for every CSS L4 rule. Per W5-generated-parser-shape §1.1-§1.10, byte-disjoint Alt emits direct `match`; speculative Alt emits O(1) tuple Checkpoint; PHF Keyword emits for color names; bespoke regex DFA emits for length-unit / number; HostCall emits via `crate::grammar::host::css_l4::*` namespace.
> *Gate*: post-regen `crates/core/src/grammar/generated/css_l4.rs` contains `pub fn parse_declaration<'p>(...) -> Result<CssDeclaration<'p>, ParseErr>` etc.; no `OpenFrame` references.
> *Exit-criteria*: `cargo xtask regen --grammar css_l4 && rg -n 'OpenFrame' crates/core/src/grammar/generated/css_l4.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'pub fn parse_(declaration|color|color_function|color_mix|selector_list)' crates/core/src/grammar/generated/css_l4.rs | wc -l | tr -d '\n'` returns ≥ 5.

> **M2 — `OpenFrame` deletes from CSS L4 path**
>
> *Surface*: `crates/core/src/runtime/css_l4/builder.rs` (1,014 LOC pre-W5b per MODULES:951); the post-W2 per-variant split modules.
> *Action*: Delete `enum OpenFrame` declaration; delete `commit_<variant>` methods that pushed/popped frames; reduce `CssStructBuilder` to arena-management surface (parallel to JsonStructBuilder's W5a reduction). The post-W2 per-variant frame modules collapse — direct-projection eliminates the variant frame as runtime concept; only the typed-enum survives. Per the BA.md §Generated-LOC budget table the post-W5b `css_l4.rs` ≤ 100,000 LOC (down from 107,138; ~7% shrinkage from speculative-checkpoint and `__layout` literal retiral).
> *Gate*: `enum OpenFrame` is gone from `runtime/css_l4/`; per-variant builder modules retain typed-construction helpers but lose frame-push/pop logic.
> *Exit-criteria*: `rg -n 'enum OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/css_l4/ 2>&1 | wc -l | tr -d '\n'` returns `0`.

> **M3 — Lightning-css parity tests pass**
>
> *Surface*: `crates/core/tests/parse_with_css_l4.rs`; lightning-css parity fixture set (bootstrap.css + animate.css + tailwind.css per `audit/SOTA-2026-05-03.md:130`).
> *Action*: Run `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)' --profile ax-iter`; verify 100% pass. Byte-equality vs lightning-css output for bootstrap.css; typed-equality for every variant in the parity test cohort.
> *Gate*: 100% pass on CSS L4 parity tests.
> *Exit-criteria*: `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M4 — BA-G1b: bootstrap.css parse internal-progress measurement**
>
> *Surface*: bench harness for CSS L4; fixture `data/css_l4/bootstrap.css`.
> *Action*: Run `cargo bench --bench bench_css_l4 -- bootstrap` post-regen; record measurement. BA-G1b is internal-progress only (not SOTA); lightning-css's 4.16 ms remains the BB-G1 SOTA target. The W5b measurement records direction-of-travel: post-W5b bootstrap.css parse should be regression-bound vs pre-W5b (no slowdown introduced by the migration).
> *Gate*: post-W5b bench output recorded; no regression vs pre-W5b baseline.
> *Exit-criteria*: bench output JSON's `bbnf_parse_bootstrap.mean` recorded; comparison vs pre-W5b baseline shows no regression (variance within 5%).

> **M5 — CSS L4 generated-LOC budget verification**
>
> *Surface*: pre-W5b `crates/core/src/grammar/generated/css_l4.rs` (107,138 LOC); post-W5b same file.
> *Action*: Per the BA.md §Generated-LOC budget table the post-W5b target is ≤ 100,000 LOC (down from 107,138; ~7% shrinkage). Verify `wc -l crates/core/src/grammar/generated/css_l4.rs` returns ≤ 100,000.
> *Gate*: `css_l4.rs` post-W5b LOC ≤ 100,000.
> *Exit-criteria*: `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/css_l4.rs | awk '{print ($1 <= 100000)}' | tr -d '\n'` returns `1`.

> **M6 — W5b artefact emission**
>
> *Surface*: `docs/tranches/BA/audit/W5b-css-l4-direct-to-struct.md` (new artefact).
> *Action*: Emit a per-grammar disposition artefact recording (a) CSS L4 pre-/post-W5b generated-LOC, (b) sample post-regen `parse_declaration` body, (c) bootstrap.css internal-progress measurement, (d) lightning-css parity test pass-rate, (e) per-variant typed-enum mapping table (14 rows: variant → typed-enum constructor + struct).
> *Gate*: artefact exists; records the five evidence items.
> *Exit-criteria*: `test -f /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5b-css-l4-direct-to-struct.md && wc -l /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5b-css-l4-direct-to-struct.md | awk '{print ($1 > 80)}' | tr -d '\n'` returns `1`.

## §3 — Closer gate

```
# CSS L4 OpenFrame deleted
rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/   ; expect: 0 matches

# css_l4.rs LOC reduction
wc -l crates/core/src/grammar/generated/css_l4.rs        ; expect: ≤ 100,000

# 14 typed-enum variants present
rg -n 'CssTypedValue::' crates/core/src/runtime/css_l4/value.rs
                                                         ; expect: ≥ 14

# CSS L4 nextest filter 100% pass
cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)' --profile ax-iter
                                                         ; expect: 100% pass

# BA-G1b bootstrap.css internal-progress (no regression vs pre-W5b)
cargo bench --bench bench_css_l4 -- bootstrap            ; expect: ≤ pre-W5b mean × 1.05
```

## §4 — Invariants

§I1. **Lock 1 partial honour** (CSS L4-side). Per the canonical W5a invariant, W5b lands the discipline for CSS L4. The remaining grammars BBNF / Sheets / cohort follow at W5c / W5d / W5e. Lock 1's BA close honour completes at W5e.

§I2. **Lock 3** (one parse impl). The W4-introduced `parse_with(input, &path)` is the single parse implementation; W5b's direct-projection rewrites the body but preserves the unified surface.

§I3. **Lock 9** (slice-borrow primary). CSS L4's `CssTypedValue<'p>` is slice-borrowed; per `audit/SOTA-2026-05-03.md:122-123` `CowArcStr<'i>` for case-folded names.

§I4. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The dual-strategy resolver (DirectToStruct for JSON+CSS L4 post-W5b; OpenFrame for BBNF+Sheets+cohort transient) collapses progressively at W5c..W5e.

§I5. **No combinators monolithic** (per `feedback_no_combinators_monolithic`). CSS L4's emit extends alt-classifier (M1) — speculative Alt with O(1) tuple Checkpoint — no combinator/SpanParser usage.

§I6. **Preserve rich AST** (per `feedback_preserve_rich_ast`). The 14-variant typed-enum is the rich AST; lightning-css parity is non-negotiable. The typed-enum carries every variant; no flattening for speed.

§I7. **No regression vs pre-W5b** (per `feedback_no_workarounds_arch`). The architectural transposition is the deliverable; BB-tranche SOTA pursuit (lightning-css 4.16 ms parity) consumes the post-W5b foundation.

§I8. **Inspect generated output** (per `feedback_inspect_generated_output`). M1 + M2 verification requires reading post-regen `generated/css_l4.rs`; the 14-variant typed-enum mapping is the inspection artefact at M6.

## §5 — Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| 14-variant migration breaks lightning-css parity tests at M3 | High | M3 `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)'` | Per-variant migration: each of the 14 variants migrates independently; nextest run between each variant; rollback isolates the offending variant |
| `css_l4.rs` LOC reduction (≤ 100,000) is missed because the 14-variant typed-enum produces verbose `match` arms | Medium | M5 LOC verification | Per `audit/RESTART-SKETCH-2026-05-03.md:592-595` the order-of-magnitude reduction holds for byte-disjoint cases; speculative cases retain modest emission shrinkage; if missed, root-cause via emitter inspection |
| HostCall emit for `parse_hex_color` etc. drifts from the workspace metadata path | Low | M1 verification (`rg -n 'crate::grammar::host::css_l4::' generated/css_l4.rs`) | Per surgery #15 the host fns live under `grammar/host/css_l4/`; the codegen reads from workspace metadata; the BA.W0.M1 host-fn relocation is the precondition |
| The post-W5b bootstrap.css internal-progress measurement regresses vs pre-W5b baseline | Low | M4 bench output | The 86.07% pathology was JSON-specific; CSS L4's pre-W5b baseline is already different but the speculative-checkpoint retiral and `__layout` literal retiral apply uniformly; if regressed, profile-driven via samply |
| 14-variant `CssTypedValue` produces an enum size that pessimises `match` codegen | Low | M3 + M4 measurement | Box large variants per `feedback_preserve_rich_ast`; the typed-enum need not be Copy — CSS L4's value enum carries Box<...> per variant if the size > threshold |

## §6 — Cross-references

- **Closes BA-G1b** (per BA.md §Hard gates internal-progress row): post-W5b bootstrap.css parse no regression.
- **Continues Lock 1 honour** (per BA.md §13-Lock honoured): CSS L4 OpenFrame retires; BBNF / Sheets / cohort follow at W5c/d/e.
- **Honours Lock 9** (per BA.md): CSS L4 parse returns slice-borrowed `CssTypedValue<'p>` + `CowArcStr<'p>`.
- **Preceding wave**: BA.W5a (JSON direct-to-struct migration; the canonical pattern).
- **Following wave**: BA.W5c (BBNF direct-to-struct migration).
- **Producing**: 14-variant typed-enum lands; W5c/d/e consume the per-variant emission pattern at smaller scale.
- **Consuming**: BA.W2 14-variant builder split (per `audit/CENSUS-2026-05-03.md:328`); BA.W0.M1 host-fn relocation (per surgery #15).

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo xtask regen --grammar css_l4` | ≤ 30 s | exit 0 | Per-grammar regen for CSS L4 14-variant direct-projection |
| `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)' --profile ax-iter` | ≤ 30 s | 100% | Lightning-css parity gate (M3) |
| `cargo bench --bench bench_css_l4 -- bootstrap` | ≤ 60 s | regression-bound | BA-G1b internal-progress |
| `wc -l crates/core/src/grammar/generated/css_l4.rs` | < 1 s | ≤ 100,000 | Post-W5b generated-LOC budget gate |
| `rg -n 'CssTypedValue::' crates/core/src/runtime/css_l4/value.rs` | < 1 s | ≥ 14 | 14-variant typed-enum |

## §8 — Verification artefacts

- `docs/tranches/BA/audit/W5b-css-l4-direct-to-struct.md` — per-grammar disposition artefact (M6); records 14-variant typed-enum mapping table + lightning-css parity evidence.
- `docs/tranches/BA/audit/W5-generated-parser-shape.md` — per-construct emission shapes; W5b deviations (speculative Alt with O(1) tuple Checkpoint; PHF Keyword; HostCall under `grammar/host/css_l4/` namespace).

## §9 — Audit lane forecast

The W5b audit lane forecast: post-W5b, the following lanes progress:

- Lane 04 (sota anchoring) — CSS L4 BB-side SOTA pursuit (lightning-css 4.16 ms) inherits the W5b foundation; BA's BA-G1b is internal-progress not SOTA per surgery #11.
- Lane 06 (generated code budget) — `css_l4.rs` ≤ 100,000 progresses the BA-tranche aggregate.
- Lane 05 (substrate audit) — CSS L4 OpenFrame retires; substrate_audit at BA.W6.M5 zero-caller count drops.

Lanes still open: Lane 03 (cross-tranche carry) closes at W6; the remaining grammars BBNF / Sheets / cohort retire OpenFrame at W5c/d/e respectively.

## §10 — Phase-4 surgery ledger

| Surgery # | Description | Landed at | Verification |
|---|---|---|---|
| 1 | Delete CSS L4 `OpenFrame` preservation | W5b (no longer deferred to BB.W1a) | M2 closer-gate |
| 2 | Lock 1 substrate identity flipped from option (b) to option (a) per user override; W5 split into W5a..W5e per-grammar | W5b | per-sub-wave closer-gates |
| 11 | Per-grammar engineering gate (BA-G1b) marked internal-progress (not SOTA); lightning-css 4.16 ms remains BB-tranche SOTA target | BA.md §Hard gates rewrite | BA.md §Hard gates row BA-G1b |
| 15 | CSS host fns to per-grammar host namespace (`grammar/host/css_l4/`) | BA.W0.M1 (precondition for W5b emit) | W5b.M1 host-fn path verification |
| 17 | Inverse-layout-audit gate — every compound-typed CSS L4 rule has Layout reaching emitted fields | BA.W2.M5 (precondition) + W5b.M1 verification | W5b.M1 typed-enum mapping table |
