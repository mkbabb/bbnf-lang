# W1 Substrate Migration Decision Companion

Date: 2026-05-03
Scope: Per-grammar migration plan for the OpenFrame retiral, conditional on BA's option (b) choice (JSON-only at BA close, all-grammar migration at BB) per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:80-88`. If BA picks option (a), this document retires; the synthesis pass surfaces the redundancy.

## Per-grammar migration table

The "all eight remaining grammars" overclaim of `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:28` (fault C03-14) decomposes into per-grammar plans. Five grammars are cohort-template (BB.W2a); three are specialised (BB.W1{a,b,c}); JSON inherits from BA.W5.

| Grammar | Sub-wave | Current OpenFrame variant count | Post-migration typed enum | LOC delta (`runtime/<g>/builder.rs`) | Regression test surface | Iter-time impact |
|---|---|---:|---|---:|---|---|
| CSS L4 | BB.W1a | 14 (Declaration, Color, ColorFunction, ColorMix, HexColor, Length, Dimension, Function, Calc, AtRule, Ratio, Time, CustomIdent, SelectorList per `audit/CENSUS-2026-05-03.md:464`) | `CssTypedValue<'p>` enum with 14 variants, each carrying its layout-derived payload | 1,014 → ~340 (−66%) | `tests/css_l4_parity.rs::full_canonical_form` against lightningcss canonical-form emit | `cargo check -p bbnf` ≤ +0.5 s; `xtask regen --grammar css_l4` ≤ +0.3 s |
| BBNF | BB.W1b | 8 (RuleDef, Alt, Seq, RepeatGroup, Quantified, RegexAtom, KeywordAtom, RootProgram per `audit/CENSUS-2026-05-03.md:441`) | `BbnfTypedValue<'p>` enum with 8 variants; bounds-recording survives as per-rule emission concern (each parse fn carries `record_bounds: bool` parameter) | 243 → ~120 (−51%) | `tests/parse_with_bbnf.rs::bounds_recording`; `cargo nextest run -p bbnf-analysis` (LSP bounds-reading consumer 100% pass) | `cargo check -p bbnf` ≤ +0.2 s |
| Sheets | BB.W1c | 6 (CellRef, Identifier, SheetPrefix, RangeRef, FunctionCall, ErrorValue per `audit/CENSUS-2026-05-03.md:485-490`) | `SheetsTypedValue<'p>` enum with 6 variants; specialised leaf-deposit logic (cell_ref normalisation, identifier resolution, sheet_prefix lookup, error formatting) relocates to per-grammar host fns at `crates/core/src/grammar/host/google_sheets.rs` per G05-9 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:32` | 357 → ~180 (−50%) | `tests/parse_with_google_sheets.rs::cell_ref_normalisation` + per-fn host tests | `cargo check -p bbnf` ≤ +0.2 s |
| BNF | BB.W2a | n/a (cohort uses `SimpleStructBuilder`) | `BnfValue<'p>` enum templated from `crates/core/src/codegen/runtime_template.rs` | hand-written ~440 → templated ≤ 50 (−89%) | byte-equality + `tests/parse_with_bnf.rs` | `xtask regen --grammar bnf` ≤ 1.5 s |
| CSV | BB.W2a | n/a | `CsvValue<'p>` enum templated | hand-written ~458 → templated ≤ 50 (−89%) | `tests/parse_with_csv.rs` | same |
| EBNF | BB.W2a | n/a | `EbnfValue<'p>` enum templated | hand-written ~445 → templated ≤ 50 (−89%) | `tests/parse_with_ebnf.rs` | same |
| CSS Pretty | BB.W2a | n/a | `CssPrettyValue<'p>` enum templated | hand-written ~455 → templated ≤ 50 (−89%) | `tests/parse_with_css_pretty.rs` | same |
| Math | BB.W2a | n/a | `MathValue<'p>` enum templated | hand-written ~467 → templated ≤ 50 (−89%) | `tests/parse_with_math.rs` | same |
| JSON | (BA.W5 close) | 0 (already retired) | `JsonValue<'i>` enum (BA.W5 deliverable) | (BA close = direct-to-struct) | `tests/parse_with_json.rs`; regression-free post-W0a path-dep relocation | inherits from BA.W5 baseline |

## Sequencing argument: CSS L4 first vs BBNF first vs Sheets first

The three specialised-grammar sub-waves (W1a, W1b, W1c) sequence as: **CSS L4 → BBNF → Sheets**. The argument:

1. **CSS L4 is the highest-risk, highest-value migration.** 14 OpenFrame variants, 1,014 LOC builder, lightningcss parity discipline, 4-MB tailwind stress test as the hardest perf target (BB-G2). Frontloading CSS L4 means the parity discipline is established early and the perf-gate trajectory has the most runway. If CSS L4 misfires at W1a, BB.W1b/W1c carry less risk because their migration mechanics are now proven on the hardest grammar.

2. **BBNF is the second-highest risk.** 8 OpenFrame variants, 243 LOC builder, BUT the bounds-recording extension at `audit/CENSUS-2026-05-03.md:441` requires explicit per-rule emission preservation (the LSP consumer at `crates/analysis/` reads bounds for source-position diagnostics). The bounds preservation is a cross-cutting concern that the W1a CSS L4 migration does not exercise. Placing BBNF at W1b means the W1a pattern is established when the bounds-preservation concern lands.

3. **Sheets is the lowest risk + highest cohesion benefit.** 6 OpenFrame variants, 357 LOC builder, with specialised leaf-deposit logic. The host-fn relocation per G05-9 (Sheets host fns move to per-grammar namespace `crates/core/src/grammar/host/google_sheets.rs`) is its own discipline; placing Sheets at W1c means the host-relocation pattern lands after the simpler OpenFrame retirals, and W1c's grammar-authoritative gate is the cleanest demonstration that cross-cutting refactoring lands per-grammar without leaking into supposedly-generic crates.

The alternative orderings:
- **BBNF first** would prematurely couple bounds-preservation discipline with the OpenFrame retiral mechanics; risk-management argument fails.
- **Sheets first** would couple host-fn relocation with the OpenFrame retiral mechanics; the simpler structural transposition would be entangled with a separate G05-9 concern.

The chosen sequence isolates each cross-cutting concern into its own sub-wave: W1a (parity discipline), W1b (bounds preservation), W1c (host-fn relocation).

## Conditional retirement (option (a))

If BA picks option (a) — all 9 grammars migrate at BA close in W5/W5b/W5c/W5d sub-waves — this document retires. The decision-companion notes:

- BA's option (a) does not affect the cohort template emission at BB.W2a (cohort grammars never had OpenFrame; BB.W2a's deliverable is the cohort template substrate).
- BA's option (a) does not affect BB.W2b (cursor unification across 9 grammars).
- BA's option (a) does not affect BB.W3{a,b,c} or BB.W4{a,b} or BB.W5{a,b,c}.

The only BB sub-waves option (a) retires: BB.W1a, BB.W1b, BB.W1c. They become deferred-substrate-now-redundant notes in BB.md's wave summary.

If BA's `docs/tranches/BA/audit/W5-substrate-identity-decision.md` ratifies option (b) (the directive's preferred path per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:80-88`), this document is the executable plan. The synthesis pass reconciles.

## Receiving-side anchors

Each row's regression test surface receives explicit citation:

- `tests/css_l4_parity.rs`: per-variant byte-equality verification against lightningcss canonical-form emit; the parity discipline at `audit/SOTA-2026-05-03.md:131-136`.
- `tests/parse_with_bbnf.rs::bounds_recording`: per-rule bounds-recording verification per the LSP consumer's contract at `crates/analysis/`.
- `tests/parse_with_google_sheets.rs::cell_ref_normalisation`: per-leaf normalisation verification (e.g., `A1` ↔ `$A$1` canonicalisation).
- `tests/parse_with_<cohort>.rs`: per-grammar cohort regression coverage; emitted from the W2a template alongside the runtime templates.

The cohort grammars do NOT have OpenFrame variants; their migration is the W2a template substitution. The "Pre-W1 LOC" column for cohort rows reflects the hand-written cohort runtime modules, not OpenFrame builder mass.
