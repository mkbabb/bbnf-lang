# SK-V12 W1 A7 - Sheets Execution Scout For Redress

Date: 2026-05-20. Scope: tactical research to arm W1 redress with exact paths, smallest corpus, oracle candidate, leak disposition, and challenge pre-flags.

## 1. Exact File Paths And Artifacts

Generated Track 1 source (compiler input + output):
- Grammar source: `grammars/sheets.bbnf` (to be created or sourced from gorgeous/google-sheets.bbnf)
- Generated runtime module: `skinny/crates/runtime/src/grammars/sheets/generated.rs` (emitted by codegen)
- Generated module wrapper: `skinny/crates/runtime/src/grammars/sheets/mod.rs` (hand ~15 LOC)
- Runtime lib integration: `skinny/crates/runtime/src/lib.rs` (add `pub mod sheets` ~2 LOC)

Codegen path:
- Main entry: `skinny/crates/codegen/src/lib.rs` (route sheets to new branch, ~5 LOC)
- Sheets branch: `skinny/crates/codegen/src/sheets_direct.rs` (new file, ~95 LOC)
- JSON provider guard: `skinny/crates/codegen/src/json_provider.rs` (add sheets variant, ~8 LOC)

Fixture and oracle:
- Fixture corpus: `restart/skinny/tranches/sk-v12/research/w1/fixtures/sheets-formulas.txt` (40–60 formulae)
- Oracle/Track 2 module: `skinny/crates/bbnf-bench/src/nonjson_sheets.rs` (new file, ~110 LOC)
- Oracle source: public crate `bbnf::grammar::generated::google_sheets::GoogleSheetsParser` (gorgeous crate)

Benchmark:
- Bench harness: `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs` (new file, ~50 LOC)
- Bench registration: `skinny/crates/bbnf-bench/Cargo.toml` (add `nonjson_baseline` bench, ~3 LOC)

Gate and report:
- Report schema extension: `skinny/crates/bbnf-bench/src/report.rs` (add Section 0.4 fields, ~70 LOC)
- Lock 14 authorization: `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (add W1 entry, ~15 LOC)
- Gate command: `skinny/crates/bbnf-bench/src/bin/gate.rs` (no change if existing gate dispatch works)

Artifacts:
- Equality output: `restart/skinny/tranches/sk-v12/research/w1/skv12-W1-sheets-equality.txt`
- Report output: `restart/skinny/tranches/sk-v12/research/w1/skv12-W1-nonjson-baseline.json`
- Criterion root: `/tmp/skv12-w1-nonjson-criterion`
- Rejected patch: `/tmp/skv12-waveW1-rejected.patch` (if revert needed)

## 2. Smallest Sheets Corpus

**Fixture format:** one formula per line, no blank lines, UTF-8.

**Sample set target:** ≥30 samples, ≥1 Mbps Track 1, ≥1 Mbps oracle.

**Smallest viable corpus:** 40–50 diverse formulas totaling ~4–6 KB:
- Cell refs: `A1`, `$B$2`, `Sheet1!C3` (5 formulae)
- Ranges: `A1:B2`, `A:A`, `1:5`, `$A$1:$B$2` (4 formulae)
- Operators: `=1+2`, `=A1*B1`, `="x"&"y"`, `=A1^2`, `=A1/B1` (5 formulae)
- Functions: `=SUM(A1:A10)`, `=IF(A1>0,B1,C1)`, `=VLOOKUP(x,A:B,2)`, `=INDEX(A:A,1)` (4 formulae)
- Complex: `=IF(SUM(A:A)>100, CONCAT(B1:B3), ERROR())` (1 formula)
- Strings & bools: `="hello"`, `=TRUE()`, `=#VALUE!`, `=""` (4 formulae)
- Unary/postfix: `=-A1`, `=A1%`, `=+B1`, `=(A1+B1)` (4 formulae)
- Nested: `=SUM(IF(A1:A10>5, B1:B10, 0))`, `=LET(x, A1, y, B1, x+y)`, `=LAMBDA(a, a*2)` (3 formulae)
- Edge cases: empty input, whitespace handling, lowercase identifiers (5 formulae)

**Total:** ~42 formulae, ~5.2 KB. Criterion will sample each 30+ times at native CPU speed.

## 3. Independent Oracle Candidate

**Candidate:** `bbnf::grammar::generated::google_sheets::GoogleSheetsParser` from `crates/gorgeous/src/google_sheets.rs` and `crates/core/src/grammar/generated/google_sheets.rs`.

**Rationale:**
- Structurally independent: not a sibling of the generated Track 1; a full BNF-compiled parser from a stable mature grammar in the gorgeous crate.
- Same output plane: direct sink parse-tree match for the formula expression result.
- Public host call: `GoogleSheetsParser::parse(input)` returns `Result<T, ParseError>` on success only; strict binary equality (OK vs Err).
- Not stale: gorgeous crate is active, sheets grammar at `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf` is production-grade.
- Section 2.1 clean: no JSON profiling, no shared codegen path with Track 1.

**Location in redress:** `skinny/crates/bbnf-bench/src/nonjson_sheets.rs` (110 LOC) will:
1. Load fixture from file.
2. Call `GoogleSheetsParser::parse()` for each formula (oracle).
3. Call generated Track 1 `sheets::parse_direct()`.
4. Assert binary equality (both OK or both Err, same parse shape on Err code).
5. Collect Mbps and sample count for report.

## 4. JSON-Provider Leak Risk Map

**Existing generic helpers in codegen/runtime that redress must NOT clone or reuse under a neutral name:**

| File | Function/Item | Disposition | Reason |
|---|---|---|---|
| `codegen/src/json_provider.rs` | `ensure_runtime_profile()` | must-extract | Checks only `backend.grammar_name == "json"`. For sheets, delete the guard entirely; do not rename or reuse. |
| `codegen/src/json_provider.rs` | `mod_rs()`, `host_rs()`, `parser_rs()`, `scan_rs()`, `sink_rs()`, `value_rs()`, `view_rs()`, `visitor_rs()` | must-NOT-reuse | All JSON-specific templates. Sheets uses direct sink only; skip all. |
| `codegen/src/json_provider.rs` | `normalize()` | safe | Generic string formatter; can be extracted as `src/util.rs` and reused. |
| `codegen/src/sink_direct.rs` | entire file | must-rewrite | `JsonSink` dispatch is hard-coded. Sheets direct sink does not emit JSON; must rewrite render to emit formula-specific output (e.g., event log or AST tuple). |
| `codegen/src/direct_schema.rs` | entire file | safe | Schema-agnostic type definitions for typed output; not touched by sheets direct/sink. |
| `runtime/src/grammars/json/` | all files | safe | JSON module is separate namespace; sheets gets its own `grammars/sheets/` directory. |
| `codegen/src/lower.rs` | lowering pipeline | safe | Grammar-agnostic; both JSON and sheets use same lowering to sink-only IR. |

**Summary:** Redress must not patch `json_provider.rs` to be polymorphic. Instead: new function `sheets_direct::new_runtime_profile()` that skips the JSON guard entirely, and new render function `sheets_direct::render()` that emits non-JSON output.

## 5. Smallest-LOC Path to Admit (≤480 non-generated LOC)

**Component breakdown (hand + codegen surface, excluding generated.rs and artifacts):**

| Component | Files | Estimated LOC | Notes |
|---|---|---|---|
| Sheets codegen route | `codegen/src/lib.rs` (+5), `codegen/src/sheets_direct.rs` (+95) | 100 | Route choice in emit_from_source; direct sink render. |
| Sheets provider | `codegen/src/json_provider.rs` (+8) | 8 | Add sheets case to guard, no template clone. |
| Runtime module | `runtime/src/lib.rs` (+2), `runtime/src/grammars/sheets/mod.rs` (+15) | 17 | Module stub; re-export generated. |
| Fixture + oracle + equality | `bbnf-bench/src/nonjson_sheets.rs` (+110) | 110 | Load, oracle call, equality, Mbps reporting. |
| Bench harness | `bbnf-bench/benches/nonjson_baseline.rs` (+50), `bbnf-bench/Cargo.toml` (+3) | 53 | Criterion bench for sheets/formula group. |
| Report schema + validation | `bbnf-bench/src/report.rs` (+70) | 70 | Section 0.4 fields: strictness, measured_validation_path, profile_artifact, scalar_reference_status, checkasm_or_parity_status, comparator_set. |
| Lock 14 W1 auth | `bbnf-bench/src/lock14_baseline.rs` (+15) | 15 | Add W1 sheets entry with class `generated_non_json:sheets`. |
| Fixture file | `w1/fixtures/sheets-formulas.txt` | ~50 (bytes, not LOC) | Plain text formula list. |
| **Total non-generated** | | **373 LOC** | Well under 480 cap. |

**Generated output (outside LOC cap):**
- `runtime/src/grammars/sheets/generated.rs` (~200–300 LOC, compiler-emitted).
- Report/equality/Criterion artifacts (file output only).

**Time estimate:** 65 minutes (plan budget 75 min; ~10 min safety margin).

## 6. CHALLENGE Pre-Flag List

**Lines in plan/execution likely to be rejected by CH1–CH6:**

### CH1 Correctness
- **Flag 1:** Oracle `GoogleSheetsParser` must be imported from `bbnf::grammar::generated::google_sheets`. If gorgeous crate is unavailable or not in workspace, oracle is blocked.
- **Flag 2:** Fixture corpus must be committed under `restart/skinny/tranches/sk-v12/research/w1/fixtures/sheets-formulas.txt` BEFORE equality/bench run. If missing, gate fails.
- **Flag 3:** Generated `sheets/generated.rs` must be placed under `runtime/src/grammars/sheets/`, not under codegen or bbnf-bench temp directories.
- **Flag 4:** Lock 14 authorization must name the exact W1 entry scoped to `sheets/formula/direct_to_struct/main`; generic "W1" entry will be rejected.

### CH4 Cost
- **Flag 5:** If `sheets_direct.rs` exceeds 95 LOC before first compile, cost is over-budget; must split.
- **Flag 6:** If report schema extension requires >70 LOC (e.g., nested struct for oracle metadata), redress records BLOCKED.
- **Flag 7:** If oracle fixture load or equality test in `nonjson_sheets.rs` exceeds 110 LOC, cost over-budget.

### CH2 / Section 2.1 Generality
- **Flag 8:** `sheets_direct.rs` must not import or clone `json_provider` helpers except `normalize()`. Any `JsonSink` reference is a JSON provider clone blocker.
- **Flag 9:** `codegen/src/json_provider.rs` must NOT be edited to be polymorphic (e.g., `if grammar == "json" || grammar == "sheets"`). Instead, add separate `sheets_direct::new_profile()` function.
- **Flag 10:** No new `directive`, `BIR`, or `BackendShape` types; use existing lowering and cost facts.

### CH3 / Regression
- **Flag 11:** JSON guard floors must hold: run `cargo run -p xtask -- check-json` and verify citm_catalog/apache_builds/marine_ik/unicode_basic all maintain Mbps >= baseline.
- **Flag 12:** `RESULTS.md` must not change if JSON is not refreshed; `git diff --exit-code -- RESULTS.md` must pass.
- **Flag 13:** No stale `sheets_witness` in Track 1 or oracle; only use gorgeous parser.

### CH5 Hidden Coupling
- **Flag 14:** Report row must point to real Criterion artifact under `/tmp/skv12-w1-nonjson-criterion/`, not mock/placeholder.
- **Flag 15:** Equality artifact must be written by the `nonjson_sheets.rs` test harness and consumed by `gate-json --skv12-non-json-report`, not produced separately.
- **Flag 16:** Oracle Mbps must be measured in the same Criterion run as Track 1; if oracle Mbps is estimated/theoretical, gate rejects.

### CH6 / Paper-Close
- **Flag 17:** W1 redress must not touch CSS or BBNF-self paths; only sheets.
- **Flag 18:** If W1 is rejected/blocked, revert must be atomic: codegen/runtime/bench/report/gate changes all revert together to commit e24a7e01.
- **Flag 19:** Rejection must update REDRESS.md with explicit preflight/measure failure reason and save patch to `/tmp/skv12-waveW1-rejected.patch`.

---

## Summary

**Redress will:**
1. Create `sheets.bbnf` (or source from gorgeous) and route codegen via new `sheets_direct.rs` branch (100 LOC).
2. Emit `runtime/src/grammars/sheets/generated.rs` and wire into `lib.rs` (17 LOC).
3. Load 42-formula fixture under `w1/fixtures/sheets-formulas.txt` (~5 KB).
4. Call `GoogleSheetsParser::parse()` as oracle in `nonjson_sheets.rs` (110 LOC) and assert binary equality.
5. Bench sheets/formula group with Criterion (sample ≥30, target ≥1 Mbps Track 1 & oracle, ~50 LOC).
6. Extend report schema with Section 0.4 fields (70 LOC) and add Lock 14 W1 entry (15 LOC).
7. Measure, gate, and if all pass, admit one SK-V12 non-JSON row: `sheets/formula/direct_to_struct/main`.
8. On fail/block, revert atomically and record REDRESS entry.

**Total hand LOC: 373 (under 480 cap). Time: ~65 min (under 75 cap). Gate: G-W1-GENERATED-NONJSON-BASELINE.**
