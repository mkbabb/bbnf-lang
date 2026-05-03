# BA.W5d — Google Sheets Direct-To-Struct Migration

**Thesis** (the Google Sheets `OpenFrame` substrate retires; per-rule generated `parse_<rule>(...)` functions return typed `SheetsValue<'p>` directly via specialised leaf-deposit (cell_ref, identifier, sheet_prefix, error); the cssparser-class parity is asserted; arena-fallback complexity at `runtime/google_sheets/arena.rs:38, 40, 103, 153` resolves; CSV-style input passes; Lock 1 honour continues from W5a/b/c). **Closer-gate** (post-W5d `rg -n 'enum OpenFrame' crates/core/src/runtime/google_sheets/` returns 0; cssparser-class parity tests at `tests/parse_with_google_sheets.rs` 100% pass; `cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)' --profile ax-iter` 100% pass; arena-fallback comments resolved).

## §1 — Deliverable

Hereupon Google Sheets's `OpenFrame` taxonomy at `crates/core/src/runtime/google_sheets/builder.rs:1-357` (per `audit/MODULES-2026-05-03.md:963`) retires by mechanism. Sheets' specialised leaf-deposit pattern (cell_ref, identifier, sheet_prefix, error) per `audit/MODULES-2026-05-03.md:963` ("`SheetsStructBuilder` — specialised leaf-deposit (cell_ref, identifier, sheet_prefix, error)") becomes per-leaf direct-projection emission. The `arena.rs` fallback comments per `audit/CENSUS-2026-05-03.md:193` ("`crates/core/src/runtime/google_sheets/arena.rs:38, 40, 103, 153` repeated 'fallback' comments on default arena arms — INVESTIGATE — confirm each arm is structurally reachable. FAIL-EXPLICIT on unreachable arms") resolve at W5d.M3.

The Sheets grammar exercises:

- **Alt (byte-disjoint)** — top-level cell_ref vs identifier vs literal.
- **Seq** — formula composition; range expressions.
- **Repeat** — argument lists; CSV-style input rows.
- **CharClass** — column letters (`A`-`Z`, `a`-`z`); digit class for row indices.
- **Keyword** — sheet-name prefixes; reserved function names (`SUM`, `AVERAGE`, `IF`, ...).
- **Scanner** — cell_ref regex DFA (`[A-Z]+[0-9]+`); range regex; error literal regex (`#REF!`, `#DIV/0!`, `#NAME?`, etc.).
- **MapExpr** — typed-enum constructors per leaf type.
- **HostCall** — none at W5d (no host fns declared per `audit/CENSUS-2026-05-03.md`).

The W5d emit shape for Sheets' `cell_ref` rule (representative):

```rust
fn parse_cell_ref<'p>(
    input: &'p [u8], p: &mut usize, arena: &mut SheetsArena<'p>,
    state: &mut ScanState, cursor: &mut PathCursor<'_>,
) -> Result<SheetsCellRef<'p>, ParseErr> {
    let start = *p;
    // Column letters: [A-Z]+
    while *p < input.len() && input[*p].is_ascii_uppercase() { *p += 1; }
    if *p == start { return Err(ParseErr::syntax(*p)); }
    let column = std::str::from_utf8(&input[start..*p]).unwrap();
    let row_start = *p;
    // Row digits: [0-9]+
    while *p < input.len() && input[*p].is_ascii_digit() { *p += 1; }
    if *p == row_start { return Err(ParseErr::syntax(*p)); }
    let row: u32 = std::str::from_utf8(&input[row_start..*p]).unwrap()
        .parse().map_err(|_| ParseErr::Number(row_start))?;
    Ok(SheetsCellRef { column, row })
}
```

Each Sheets rule compiles to direct `SheetsValue<'p>` constructor; the typed-enum is `CellRef(SheetsCellRef<'p>) | Identifier(&'p str) | SheetPrefix(&'p str) | Error(SheetsError) | Range(SheetsRange<'p>) | Function(SheetsFunctionCall<'p>) | Number(SheetsNumber) | String(&'p str) | ...` per `feedback_preserve_rich_ast`.

The arena-fallback complexity per `audit/CENSUS-2026-05-03.md:193` resolves: per `feedback_no_workarounds` the fallback comments are FAIL-EXPLICIT — each arm at `arena.rs:38, 40, 103, 153` is examined; reachable arms retain with explicit panic-on-violation comments; unreachable arms delete. The post-W5d `arena.rs` carries no "fallback" comment.

The `runtime/google_sheets/value.rs:76, 92` "grammar's TODO note" + "AU.6.7 TODO note; today the leaf carries the borrowed span" per `audit/CENSUS-2026-05-03.md:189` resolves: the TODO closes by the direct-projection emit (the borrowed-span is carried via `&'p str` in the typed-enum; AU.6.7's deferred work is the post-W5d shape).

The performance trajectory: Sheets has no external SOTA competitor (cssparser-class is the parity reference, not perf). BA-G1d is regression-bound (post-W5d Sheets parse_simple measurement ≤ pre-W5d × 1.05). The BB tranche's BB-G3 (or equivalent) records the SOTA pursuit if any external competitor surfaces; W5d's deliverable is the architectural transposition (OpenFrame retiral + arena-fallback resolution).

The Era V failure mode is closed for Sheets at W5d; the same-wave consumer is the cssparser-class parity test cohort + `cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)'` at 100% pass.

## §2 — Milestones

> **M0 — SheetsValue typed-enum lands; per-leaf direct emission**
>
> *Surface*: `crates/core/src/runtime/google_sheets/value.rs` (per `audit/MODULES-2026-05-03.md:961`); the per-leaf typed-enum.
> *Action*: Land typed-enum `SheetsValue<'p>` per `feedback_preserve_rich_ast`. Each leaf type (cell_ref, identifier, sheet_prefix, error, range, function, number, string) maps to a typed-enum constructor; the rich AST carries every variant.
> *Gate*: `SheetsValue<'p>` enum lands; 8+ variants present.
> *Exit-criteria*: `rg -n 'SheetsValue::' crates/core/src/runtime/google_sheets/value.rs | wc -l | tr -d '\n'` returns ≥ 8.

> **M1 — Per-rule `parse_<rule>` direct-projection emit for Sheets**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct/{header,body,fields,finalize}.rs`; the Sheets grammar at `grammar/google_sheets/google_sheets.bbnf`; the regen target `crates/core/src/grammar/generated/google_sheets.rs` (14,088 LOC pre-W5d per MODULES:624).
> *Action*: Implement direct-projection emit. Per W5-generated-parser-shape §1.1-§1.10: byte-disjoint Alt → direct `match`; CharClass for column letters and digits; bespoke regex DFA for cell_ref + range + error literals; PHF Keyword for function names + sheet-prefixes (≥ 4 case).
> *Gate*: post-regen `crates/core/src/grammar/generated/google_sheets.rs` contains `pub fn parse_cell_ref<'p>(...) -> Result<SheetsCellRef<'p>, ParseErr>` etc.; no `OpenFrame` references.
> *Exit-criteria*: `cargo xtask regen --grammar google_sheets && rg -n 'OpenFrame' crates/core/src/grammar/generated/google_sheets.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'pub fn parse_(cell_ref|range|function|sheet_prefix)' crates/core/src/grammar/generated/google_sheets.rs | wc -l | tr -d '\n'` returns ≥ 4.

> **M2 — `OpenFrame` deletes from Sheets path**
>
> *Surface*: `crates/core/src/runtime/google_sheets/builder.rs` (357 LOC pre-W5d per MODULES:963).
> *Action*: Delete `enum OpenFrame`; reduce `SheetsStructBuilder` to arena-management surface. Per the BA.md §Generated-LOC budget table the post-W5d `google_sheets.rs` ≤ 12,000 LOC (down from 14,088; ~15% shrinkage from speculative-checkpoint and `__layout` literal retiral).
> *Gate*: `enum OpenFrame` is gone from `runtime/google_sheets/`.
> *Exit-criteria*: `rg -n 'enum OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/google_sheets/ 2>&1 | wc -l | tr -d '\n'` returns `0`.

> **M3 — Arena-fallback complexity resolves**
>
> *Surface*: `crates/core/src/runtime/google_sheets/arena.rs:38, 40, 103, 153` (per `audit/CENSUS-2026-05-03.md:193`).
> *Action*: Per surgery #18 (fail-explicit table) and `feedback_no_workarounds`, examine each "fallback" comment arm. Reachable arms: rewrite the comment to explicit panic-on-violation rationale; unreachable arms: delete. The post-W5d `arena.rs` carries no `fallback` comment.
> *Gate*: arena.rs has zero `fallback` comments; reachable arms have explicit panic comments; unreachable arms deleted.
> *Exit-criteria*: `rg -n 'fallback' crates/core/src/runtime/google_sheets/arena.rs 2>&1 | wc -l | tr -d '\n'` returns `0`.

> **M4 — cssparser-class parity tests pass**
>
> *Surface*: `crates/core/tests/parse_with_google_sheets.rs`; cssparser-class parity fixture set.
> *Action*: Run `cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)' --profile ax-iter`; verify 100% pass. Byte-equality vs cssparser-class output for a CSV-style fixture; typed-equality on every variant in `SheetsValue<'p>`.
> *Gate*: 100% pass on Sheets parity tests.
> *Exit-criteria*: `cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M5 — BA-G1d: Sheets parse_simple regression-bound**
>
> *Surface*: bench harness for Sheets; fixture `data/google_sheets/simple.gsheets` (or equivalent).
> *Action*: Run `cargo bench --bench bench_google_sheets -- parse_simple` post-regen; record measurement. BA-G1d is regression-bound (no SOTA target — Sheets has no external competitor at this scale); post-W5d measurement ≤ pre-W5d × 1.05.
> *Gate*: post-W5d bench output recorded; no regression vs pre-W5d.
> *Exit-criteria*: `sheets_parse_simple.mean ≤ pre-W5d × 1.05`.

> **M6 — Sheets generated-LOC budget verification**
>
> *Surface*: pre-W5d `crates/core/src/grammar/generated/google_sheets.rs` (14,088 LOC); post-W5d same file.
> *Action*: Per the BA.md §Generated-LOC budget table the post-W5d target is ≤ 12,000 LOC. Verify `wc -l crates/core/src/grammar/generated/google_sheets.rs` returns ≤ 12,000.
> *Gate*: `google_sheets.rs` post-W5d LOC ≤ 12,000.
> *Exit-criteria*: `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/google_sheets.rs | awk '{print ($1 <= 12000)}' | tr -d '\n'` returns `1`.

> **M7 — W5d artefact emission**
>
> *Surface*: `docs/tranches/BA/audit/W5d-google-sheets-direct-to-struct.md` (new artefact).
> *Action*: Emit a per-grammar disposition artefact recording (a) Sheets pre-/post-W5d generated-LOC, (b) sample post-regen `parse_cell_ref` body, (c) cssparser-class parity test pass-rate, (d) per-leaf typed-enum mapping table, (e) arena-fallback resolution evidence (per arm: reachable + panic comment, OR unreachable + deleted).
> *Gate*: artefact exists; records the five evidence items.
> *Exit-criteria*: `test -f /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5d-google-sheets-direct-to-struct.md && wc -l /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5d-google-sheets-direct-to-struct.md | awk '{print ($1 > 80)}' | tr -d '\n'` returns `1`.

## §3 — Closer gate

```
# Sheets OpenFrame deleted
rg -n 'enum OpenFrame' crates/core/src/runtime/google_sheets/
                                                         ; expect: 0 matches

# google_sheets.rs LOC reduction
wc -l crates/core/src/grammar/generated/google_sheets.rs ; expect: ≤ 12,000

# Arena-fallback comments resolved
rg -n 'fallback' crates/core/src/runtime/google_sheets/arena.rs
                                                         ; expect: 0 matches

# Sheets nextest filter 100% pass
cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)' --profile ax-iter
                                                         ; expect: 100% pass

# BA-G1d Sheets parse_simple regression-bound
cargo bench --bench bench_google_sheets -- parse_simple  ; expect: ≤ pre-W5d × 1.05

# 8 typed-enum variants present
rg -n 'SheetsValue::' crates/core/src/runtime/google_sheets/value.rs
                                                         ; expect: ≥ 8
```

## §4 — Invariants

§I1. **Lock 1 partial honour** (Sheets-side). Per the canonical W5a invariant, W5d lands the discipline for Sheets. Cohort follows at W5e. Lock 1's BA close honour completes at W5e.

§I2. **Lock 3** (one parse impl). The W4-introduced `parse_with(input, &path)` is the single parse implementation; W5d's direct-projection rewrites the body but preserves the unified surface.

§I3. **Lock 9** (slice-borrow primary). Sheets' `SheetsValue<'p>` is slice-borrowed; `&'p str` over input.

§I4. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The dual-strategy resolver (DirectToStruct for JSON+CSS L4+BBNF+Sheets post-W5d; OpenFrame for cohort transient) collapses at W5e.

§I5. **No combinators monolithic** (per `feedback_no_combinators_monolithic`). Sheets' emit extends alt-classifier (M1) — no combinator/SpanParser usage.

§I6. **Preserve rich AST** (per `feedback_preserve_rich_ast`). The `SheetsValue<'p>` typed-enum preserves shape: 8+ variants for Sheets AST.

§I7. **Fail-explicit** (per `feedback_no_workarounds` + surgery #18). Per M3 the arena-fallback comments resolve to explicit panic-on-violation comments OR delete unreachable arms; no silent fallback.

§I8. **Inspect generated output** (per `feedback_inspect_generated_output`). M1 + M2 verification requires reading post-regen `generated/google_sheets.rs`; the per-leaf typed-enum mapping is the inspection artefact at M7.

## §5 — Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Per-leaf direct emission misclassifies a Sheets variant (e.g. error literal) | Medium | M4 cssparser-class parity test | Conservative classifier: each leaf-deposit's typed-enum constructor is verified at M0; the parity test exercises every leaf |
| `google_sheets.rs` LOC reduction (≤ 12,000) is missed | Low | M6 LOC verification | The 15% shrinkage budget matches the 12% applied to BBNF; if missed, root-cause via emitter inspection |
| Arena-fallback resolution at M3 deletes a load-bearing arm | Medium | `cargo nextest run -p bbnf -E 'test(google_sheets)'` post-M3 | Per `audit/CENSUS-2026-05-03.md:193` the comment instructs INVESTIGATE; M3's verification reads each arm's reachability via codegen + runtime trace; `cargo expand`-equivalent inspection |
| Post-W5d Sheets parse_simple bench regresses vs pre-W5d | Low | M5 bench output | Per `feedback_no_warm_benches` cold per-parse only |
| The cssparser-class parity test exposes a typed-equality gap (e.g. error literal variant differs) | Medium | M4 nextest output | The typed-enum carries every variant; if gap, the typed-enum is updated to match cssparser-class output |

## §6 — Cross-references

- **Closes BA-G1d** (per BA.md §Hard gates internal-progress row): post-W5d Sheets parse_simple regression-bound.
- **Continues Lock 1 honour** (per BA.md §13-Lock honoured): Sheets OpenFrame retires; cohort follows at W5e.
- **Honours Lock 9** (per BA.md): Sheets parse returns slice-borrowed `SheetsValue<'p>`.
- **Closes surgery #18 (Sheets-side)**: arena-fallback resolution per `audit/CENSUS-2026-05-03.md:193`.
- **Preceding wave**: BA.W5c (BBNF direct-to-struct migration).
- **Following wave**: BA.W5e (cohort direct-to-struct migration).
- **Producing**: per-leaf direct-projection emission pattern; W5e consumes the pattern at smaller scale (5 cohort grammars, each ~54 LOC builder).
- **Consuming**: BA.W2 fail-explicit table (per surgery #18).

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo xtask regen --grammar google_sheets` | ≤ 18 s | exit 0 | Per-grammar regen for Sheets direct-projection |
| `cargo nextest run -p bbnf -E 'test(google_sheets) + test(sheets)' --profile ax-iter` | ≤ 15 s | 100% | cssparser-class parity gate (M4) |
| `cargo bench --bench bench_google_sheets -- parse_simple` | ≤ 60 s | regression-bound | BA-G1d internal-progress |
| `wc -l crates/core/src/grammar/generated/google_sheets.rs` | < 1 s | ≤ 12,000 | Post-W5d generated-LOC budget gate |
| `rg -n 'fallback' crates/core/src/runtime/google_sheets/arena.rs` | < 1 s | 0 | Arena-fallback resolution gate |

## §8 — Verification artefacts

- `docs/tranches/BA/audit/W5d-google-sheets-direct-to-struct.md` — per-grammar disposition artefact (M7); records SheetsValue typed-enum mapping + arena-fallback resolution evidence + cssparser-class parity test pass-rate.
- `docs/tranches/BA/audit/W2-fail-explicit-table.md` (consumed) — surgery #18 fail-explicit row for Sheets arena.
- `docs/tranches/BA/audit/W5-generated-parser-shape.md` — per-construct emission shapes; W5d deviations (per-leaf direct emission for cell_ref/range/error literals).

## §9 — Audit lane forecast

The W5d audit lane forecast: post-W5d, the following lanes progress:

- Lane 04 (sota anchoring) — Sheets has no external SOTA competitor; BA-G1d is regression-bound only; the BB tranche may surface a competitor.
- Lane 06 (generated code budget) — `google_sheets.rs` ≤ 12,000 progresses the BA-tranche aggregate.
- Lane 05 (substrate audit) — Sheets OpenFrame retires; substrate_audit zero-caller count drops.
- Surgery #18 (fail-explicit) closes for Sheets-side.

Lanes still open: cohort retires OpenFrame at W5e; Lock 4 + Lock 10 close at BB.W3.

## §10 — Phase-4 surgery ledger

| Surgery # | Description | Landed at | Verification |
|---|---|---|---|
| 1 | Delete Sheets `OpenFrame` preservation | W5d (no longer deferred to BB.W1c) | M2 closer-gate |
| 2 | Lock 1 substrate identity flipped from option (b) to option (a) per user override; W5 split into W5a..W5e per-grammar | W5d | per-sub-wave closer-gates |
| 11 | Per-grammar engineering gate (BA-G1d) marked regression-bound (no SOTA — no external competitor at this scale) | BA.md §Hard gates rewrite | BA.md §Hard gates row BA-G1d |
| 18 | Fail-explicit table — Sheets arena-fallback rows resolved | W5d.M3 | M3 closer-gate |
