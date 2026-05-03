# BA.W5 Substrate Identity Decision — Lock 1 Disposition

Date: 2026-05-03
Decision: **(a) BA migrates all 9 grammars to direct-to-struct; Lock 1 honoured at BA close.**

## §1 — Choice

Hereupon BA owns the all-grammar direct-to-struct migration. Each of nine grammars (JSON, CSS L4, BBNF, Google Sheets, BNF, CSV, EBNF, CSS Pretty, Math) retires its `OpenFrame` substrate WITHIN BA, across five sub-waves W5a..W5e. Lock 1 is **honoured at BA close**, not deferred. The `enum OpenFrame` declarations at `crates/core/src/runtime/{json,css_l4,bbnf,google_sheets,bnf,csv,ebnf,css_pretty,math}/builder.rs` are extinct at BA close. BA.md §13-Lock honoured table marks Lock 1 as **honoured at BA.W5a..W5e**, citing each sub-wave's closer-gate.

Per directive §10 honesty discipline ("No claim of lock-honoured if substrate is preserved"), the only path to honoured-Lock-1 is full migration. The substrate-without-receiver concern is mitigated wave-by-wave because each sub-wave delivers a same-wave consumer: JSON, CSS L4, BBNF, Sheets, and the cohort each reach a consumer (parser benchmark, parity test, self-host roundtrip, cssparser-class fixture, cohort fixture run) at their respective sub-wave close.

## §2 — Defence (i): Iter-time impact

Per `audit/HARDENING-PLAN-2026-05-03-04-sota-anchoring.md` S04-2 the pre-BA `cargo xtask regen --check` baseline is 59.98 s. Per BA-G3 the post-BA close requirement is ≤ 30 s. The five sub-waves apportion regen + nextest cycle cost per grammar.

| Sub-wave | Grammar | Pre-W LOC | Builder LOC | Per-grammar regen | Per-grammar nextest | Cumulative wave wall |
|---|---|---:|---:|---:|---:|---:|
| W5a | JSON | 3,500 | 382 | ~25 s (post-W2 split) | ~12 s (`-E 'test(json)'`) | ~50 s |
| W5b | CSS L4 | 107,138 | 1,014 | ~30 s | ~30 s (`-E 'test(css_l4)'` lightning-css parity) | ~75 s |
| W5c | BBNF | 21,503 | 243 | ~22 s | ~18 s (`-E 'test(bbnf)'` self-host roundtrip) | ~50 s |
| W5d | Sheets | 14,088 | 357 | ~18 s | ~15 s (`-E 'test(sheets)'` cssparser-class) | ~40 s |
| W5e | Cohort (5 grammars) | ~22,521 | 5×54 | ~25 s parallel regen | ~30 s parallel nextest | ~70 s |

Aggregate sub-wave wall budget across W5a..W5e: ~285 s (~5 min). Distributed across five separately committed sub-waves, each independently executable, the per-wave xtask regen is bounded ≤ 30 s — BA-G3 holds wave-by-wave because no single sub-wave regenerates the full nine-grammar matrix. The post-BA full-regen baseline at W6.M2 measures `cargo xtask regen --check` ≤ 30 s under steady-state cache (every grammar's emitter source is the post-W5e shape; no fallback OpenFrame path exists to dual-regenerate).

The migration order's iter-time discipline: W5a (smallest, ~50 s wave wall) is first to anchor the emitter pattern; W5b (largest, ~75 s wave wall) consumes the established pattern as scale-test; W5c (self-host stress, ~50 s) verifies the pattern under correctness pressure; W5d (cssparser-class arena complexity, ~40 s) verifies under non-JSON value-space pressure; W5e (cohort, ~70 s) consumes the pattern as repetition-test. The cumulative tranche calendar grows from 7-wave estimate to 13-wave; at ~50-75 s per W5x sub-wave the cumulative W5 phase contributes ~5 min wall, against W2's 23 god-module split ~15 min wall and W4's cursor unification ~10 min wall. Phase-4 calendar impact: 1.5-2x prior 7-wave estimate — accepted as the cost of honouring Lock 1.

## §3 — Defence (ii): Regression-risk mitigation

Each grammar carries a distinct regression vector; the migration order mitigates each in turn.

| Sub-wave | Grammar | Regression vector | Mitigation strategy | Equality discipline |
|---|---|---|---|---|
| W5a | JSON | Smallest surface; primary perf target | Direct-projection emit replaces 86.07% pathology; parity test reads typed `JsonValue<'p>` | Byte-equality fixture roundtrip; BA-G1 ≤ 400 µs |
| W5b | CSS L4 | 14-variant `OpenFrame` taxonomy; lightning-css parity tests extensive | Per-variant emission; `color-function` typed-enum lands per variant frame; bootstrap.css fixture parity | Byte-equality vs lightning-css `bootstrap.css`; typed-equality on every variant in `tests/css_l4_*.rs` |
| W5c | BBNF | Self-host stresses correctness — the bootstrap of the language | Pratt operator chain emission for grammar's own operators; `tests/bbnf_self_host_roundtrip.rs` parses BBNF into IR, emits, re-parses to byte-identical output | Self-host roundtrip; typed-equality on AST shape |
| W5d | Sheets | Arena-fallback complexity (cell_ref, sheet_prefix, error variants) | Per-leaf-deposit direct emission; cssparser-class parity asserted | cssparser-class fixture parity; typed-equality on `tests/sheets_*.rs` |
| W5e | Cohort (BNF, CSV, EBNF, CSS Pretty, Math) | Templated/repetitive | Hand-written direct-to-struct emission per grammar — five mechanical files; templating consolidation deferred to BB.W2 | Per-grammar fixture parse; byte-equality roundtrip per cohort grammar |

The cascade-risk mitigation. Per BA.md §Risks row 1 ("BA.W2 god-module splits cascade into BA.W5 codegen breakage (renames pull through emitter)"), W2 god-module splits land first; W4 cursor unification lands second; W5a..W5e regen cycles land last per per-sub-wave gates. `cargo xtask regen --check` between every sub-wave verifies no inter-sub-wave cascade. Per `feedback_no_orthogonal_codepaths` ("Arena allocation must be singular collection strategy; no conditional Vec-vs-scratch branching"), the post-W5 emitter has ONE codegen path — direct-to-struct — for every grammar. No `EmitStrategy::OpenFrame` fallback survives BA close; the strategy resolver collapses to `EmitStrategy::DirectToStruct` for all grammars at W5e close.

## §4 — Defence (iii): Per-grammar test coverage

Each sub-wave's closer-gate is a 100% nextest pass on the migrated grammar's filter.

| Sub-wave | Grammar | Test crate / nextest filter | Pre-BA pass-rate baseline | Post-W close gate |
|---|---|---|---|---|
| W5a | JSON | `cargo nextest run -p bbnf -E 'test(json)' --profile ax-iter` | 100% (`tests/parse_with_json.rs`) | 100% post-direct-to-struct emit |
| W5b | CSS L4 | `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)' --profile ax-iter` | 100% (`tests/parse_with_css_l4.rs` + lightning-css parity tests) | 100% post-14-variant typed-enum |
| W5c | BBNF | `cargo nextest run -p bbnf -p bbnf-bootstrap -E 'test(bbnf) + test(self_host)' --profile ax-iter` | 100% (`tests/parse_with_bbnf.rs` + bootstrap roundtrip) | 100% post-Pratt + self-host roundtrip |
| W5d | Sheets | `cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)' --profile ax-iter` | 100% (`tests/parse_with_google_sheets.rs` + cssparser-class) | 100% post-arena-fallback retiral |
| W5e | Cohort | `cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)' --profile ax-iter` | 100% (`tests/cohort_*.rs`) | 100% post-five-grammar hand-written direct-to-struct |

The closer-gate at each sub-wave is the post-migration nextest filter passes 100%. Decision: at each wave's close, the migrated grammar's nextest filter is 100%, gated as the wave's exit-criterion. No grammar leaks regression to the next sub-wave; the W5a→W5b→W5c→W5d→W5e ordering is sequential, with `cargo nextest run` between each as the gate.

## §5 — Migration order rationale

The order JSON → CSS L4 → BBNF → Sheets → Cohort is decided in-plan by complexity ascent:

1. **JSON first (W5a)** — smallest surface (382 LOC builder; 4-variant `OpenFrame`); the demonstration of mechanism. Per `audit/RESTART-SKETCH-2026-05-03.md:154-220` the 86.07% pathology IS the JSON `Vec<OpenFrame>::clone`; retiring it on JSON proves the direct-projection mechanism. Consumer same-wave: BA-G1 (twitter ≤ 400 µs vs sonic-rs's 436 µs).
2. **CSS L4 second (W5b)** — largest surface (1,014 LOC builder; 14-variant `OpenFrame`); the highest-stakes parity test. Per `audit/CENSUS-2026-05-03.md:328` ("`runtime/css_l4/builder.rs` 1014 LOC ... 14-variant OpenFrame builder ... split per variant frame modules"), the 14-variant explosion is mined wave-by-wave: declaration, color, color_function, color_mix, selector_list, hex_color, etc. Consumer same-wave: BA-G1b CSS L4 bootstrap parse regression-bound (lightning-css 4.16 ms remains the SOTA target for BB; BA's BA-G1b is internal-progress).
3. **BBNF third (W5c)** — self-host stresses correctness (243 LOC builder; smallest of remaining four). Pratt operator chain emission for grammar operators is the critical path; if the migration regresses the bootstrap, every downstream grammar regen breaks. Consumer same-wave: BA-G1c BBNF self-host parse-and-format roundtrip passes (regression-only, no SOTA).
4. **Sheets fourth (W5d)** — cssparser-class parity (357 LOC builder; arena-fallback complexity per `audit/CENSUS-2026-05-03.md:193`). The cell_ref / sheet_prefix / error leaf-deposit pattern is the per-leaf direct-projection demonstration. Consumer same-wave: BA-G1d Sheets parse_simple regression-bound (no SOTA).
5. **Cohort last (W5e)** — five mechanical grammars (BNF, CSV, EBNF, CSS Pretty, Math), each ~54 LOC builder shim onto `SimpleStructBuilder` per `audit/MODULES-2026-05-03.md:993`. BA writes five hand-written direct-to-struct cohort modules; BB.W2 consolidates to one template per `feedback_no_deferrals`. Consumer same-wave: BA-G1e cohort grammars: each parses fixture without regression.

## §6 — BA close Lock 1 disposition

Per BA.md §13-Lock honoured table:

| Lock | Wave | Disposition |
|---|---|---|
| L1. Tape + columnar dead | W0 (residue scrub); W5a..W5e (per-grammar OpenFrame retiral) | **Honoured at BA close**: all nine grammars retire `OpenFrame`. Closer-gate `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 at BA.W6.M5. Each sub-wave (W5a JSON; W5b CSS L4; W5c BBNF; W5d Sheets; W5e cohort) closes its grammar's `enum OpenFrame` retiral. |

The synthesis agent at BA close verifies Lock 1 honoured by `rg -n 'enum OpenFrame' crates/core/src/runtime/` returning 0 — not by deferral receiver. The 13-Lock honoured table cell L1 says "honoured at W5a..W5e", not "deferred to BB". Per directive §10 honesty discipline, the carry is closed within BA.

## §7 — BB.W1 implications

Under option (a), BB.W1 retires from per-grammar OpenFrame migration scope. The BB tranche reorients: BB.W1 becomes the consumer of the post-BA all-grammar direct-to-struct foundation, focusing on (a) emitter generalisation patterns, (b) cohort template consolidation (BB.W2 absorbs W5e's hand-written modules into one parameterised template per gap D), (c) Pratt + SIMD auto-detection (Lock 10), (d) e-graph + CSP + cost-model (Lock 4). BB.W1's per-grammar receiver gates retire; the synthesis pass reconciles cross-tranche.

The BB.W2 cohort-template work consumes W5e's five hand-written modules. Per gap D (`docs/tranches/BB/audit/W2-cohort-template-spec.md`), BB.W2's parameter table is populated from W5e's per-grammar instantiations (BNF, CSV, EBNF, CSS Pretty, Math each carry their direct-to-struct emission shape; BB.W2 templates them).

## §8 — Closer condition

The decision is settled in-plan; option (a) is the BA close posture. BA.md §13-Lock honoured row L1 marks "honoured at BA.W5a..W5e (full all-grammar direct-to-struct migration)". The carry-tag table BA→BB no longer carries per-grammar OpenFrame migration; the renumbered C1' carries cohort hand-written → BB.W2 template-consolidation only.
