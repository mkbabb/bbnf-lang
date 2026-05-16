# SK-V7 W2 R2 - canada f64 fallback attribution and integer fast-path coverage

Date: 2026-05-16

Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`

Scope: read-only research for SK-V7 Wave 2 Phase 1. Source was not edited. This artifact is the only new file created by this pass.

## Findings

1. The current W2 premise is false on the checked tree: canada direct does not have an observable f64 fallback pool to eliminate. A one-off attribution binary linked against the current `parse_that_regex` scanner counted `111126` JSON numbers in `canada.json`: `46` integers and `111080` floats. All `111080` floats returned through `eisel_lemire::compute_f64`; `f64_fallback=0`, `f64_mantissa_overflow=0`, and `f64_ambiguous=0`. The measured fallback rate is therefore `0.0000%` of floats, not the handoff/spec hypothesis of about 25%.

2. The source shape explains the zero fallback result. `materialize_f64` calls Eisel-Lemire only when `span.mantissa_overflow` is false, and falls back to `str::parse::<f64>()` only after mantissa overflow or `compute_f64(...) == None` (`skinny/crates/parse-that-regex/src/number/mod.rs:260`). The canada attribution found `digit_gt19=0`, `max_digits=17`, and exponent range `[-15,0]`, so the scanner's 19-digit mantissa budget is not exceeded. The scanner sets `mantissa_overflow` only after more than 19 significant digits or non-zero trailing overflow digits (`skinny/crates/parse-that-regex/src/number/mod.rs:304`).

3. Integer fast-path coverage is complete for the numeric rows checked. canada has `46/46` integers on the span-native fast path (`40` signed, `6` unsigned, zero slow integer calls). `numbers` has no integers. `mesh` has `40613/40613` unsigned integers on the fast path. `marine_ik` has `130225/130225` integers on fast paths (`6091` signed, `124134` unsigned, including `6085` `-0` spans classified before f64 fallback). `instruments` has `4935/4935` unsigned integers on the fast path. The integer slow path in `integer.rs` is therefore correctness coverage for overflow/long-width edges, not a current canada/numbers hot route (`skinny/crates/parse-that-regex/src/number/integer.rs:40`).

4. Current direct gate evidence still says canada is red, but not because f64 fallback remains. `skinny/RESULTS.md` reports canada direct as `10464` Track 1 Mbps and `10119` Track 2 Mbps versus `12509` sonic-rs strict Mbps (`skinny/RESULTS.md:11`). The generated hot branch for canada's numeric arrays is the array number arm (`skinny/crates/runtime/src/grammars/json/generated.rs:507`, `skinny/crates/runtime/src/grammars/json/generated.rs:525`), which then scans a number span and emits through `emit_number_array_direct` (`skinny/crates/runtime/src/grammars/json/generated.rs:674`, `skinny/crates/runtime/src/grammars/json/generated.rs:749`).

5. Prior cohort C2 already points away from fallback elimination and toward number scan / typed numeric-array shape. It attributes canada Track 1 to the numeric array branch, with `materialize_f64` at only `14.2%`, and says residual cost is `match_number_span_from_first` scan plus dispatch rather than an EL fallback pool (`restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:117`). The same report says mesh's `materialize_f64` is only `2.4%` and Eisel-Lemire compute is `5.2%` total, again naming digit/exponent scan as residual (`restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:119`).

## Attribution table

Command source: `cargo build -p parse-that-regex`, then a temporary stdin-compiled Rust counter linked against `target/debug/deps/libparse_that_regex-*.rlib` and run over the fixture files. The counter used `match_number_span_from_first`, `materialize_i64`, `materialize_u64`, and `eisel_lemire::compute_f64` from the current crate.

| Fixture | Total numbers | Integers | Floats | f64 EL ok | f64 fallback | Fallback rate of floats | Mantissa overflow | Ambiguous EL | Integer fast ok | Integer slow ok | Max digits | Exp range |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| canada | 111126 | 46 | 111080 | 111080 | 0 | 0.0000% | 0 | 0 | 46 | 0 | 17 | [-15, 0] |
| numbers | 10001 | 0 | 10001 | 10001 | 0 | 0.0000% | 0 | 0 | 0 | 0 | 16 | [-16, -8] |
| mesh | 73013 | 40613 | 32400 | 32400 | 0 | 0.0000% | 0 | 0 | 40613 | 0 | 16 | [-17, 0] |
| marine_ik | 245175 | 130225 | 114950 | 114950 | 0 | 0.0000% | 0 | 0 | 130225 | 0 | 7 | [-6, 0] |
| instruments | 4935 | 4935 | 0 | 0 | 0 | n/a | 0 | 0 | 4935 | 0 | 7 | [0, 0] |

Short profile sanity commands were also run:

| Command | Result |
|---|---|
| `cargo run -p bbnf-bench --bin profile_direct --release -- 200 canada track1` | `10155 Mbps` profile loop |
| `cargo run -p bbnf-bench --bin profile_direct --release -- 200 canada track2` | `10114 Mbps` profile loop |

These are short sanity loops, not gate benches; the gate authority remains `RESULTS.md`.

## Recommended gate

Do not admit a Wave 2 mantissa-widen implementation under the current evidence. The gate should be:

- **Pre-block / redress the f64 fallback-elimination route for canada on this tree**, because measured fallback rate is `0.0000%`.
- Preserve the existing W2 guard that `numbers` direct must stay PASS; `RESULTS.md` currently records `numbers` direct as PASS at `12566` Track 1 Mbps and `12073` Track 2 Mbps versus `12919` sonic-rs strict Mbps (`skinny/RESULTS.md:30`).
- If W2 continues, redefine implementation scope to a separately planned numeric-array scan/dispatch route only after a fresh PC-level profile names a concrete hot leaf. The current likely leaf is `match_number_span_from_first` / array-number dispatch, not `str::parse::<f64>()` fallback.

## Risks and pre-blocked routes

- A mantissa widen would add correctness risk without a measured canada consumer. The Eisel-Lemire table already spans 651 powers over the f64 exponent range (`skinny/crates/parse-that-regex/src/number/eisel_lemire/table.rs:1`), and `compute_f64` only reports ambiguous rounding through `power2 == -1` (`skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs:12`).
- Raw f64 shortcutting is already explicitly blocked by REDRESS: the accepted SK-V5 EL materializer fixed the exact-number gap "without taking the rejected `raw.parse::<f64>()` shortcut" (`skinny/REDRESS.md:517`, `skinny/REDRESS.md:525`).
- Another local digit-prefix probe is pre-blocked by measurement: a 16-byte digit-prefix probe regressed `canada`, `numbers`, and `mesh` (`skinny/REDRESS.md:633`, `skinny/REDRESS.md:641`).
- Function-pointer / dispatch-table replacement is pre-blocked: a real 256-entry dispatch table was implemented in Track 1 and Track 2, measured, reverted, and left invalid in the gate report (`skinny/REDRESS.md:216`, `skinny/REDRESS.md:220`).
- Pair-token fusion is pre-blocked for canada: the pair-token-free projection regressed Track 1 on twitter and canada (`skinny/REDRESS.md:209`, `skinny/REDRESS.md:211`).

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md:149` through `restart/skinny/tranches/sk-v7/SPEC.md:165` - W2 requested fallback confirmation, mantissa widen, canada/numbers/mesh/marine gates.
- `restart/skinny/tranches/sk-v7/HANDOFF.md:37` through `restart/skinny/tranches/sk-v7/HANDOFF.md:38` - handoff hypothesis that about 25% of canada f64 overflows EL fast path.
- `skinny/RESULTS.md:11` and `skinny/RESULTS.md:30` - current canada and numbers direct gate rows.
- `skinny/RESULTS.md:193` through `skinny/RESULTS.md:195` - numbers materialization census.
- `restart/skinny/tranches/sk-v7/research/skv7-A4-parse-that-gaps.md:55` through `restart/skinny/tranches/sk-v7/research/skv7-A4-parse-that-gaps.md:69` - parse-that number module inventory.
- `restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:117` through `restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:124` - canada/mesh/marine/numbers direct hot-leaf attribution.
- `skinny/crates/parse-that-regex/src/number/mod.rs:32` through `skinny/crates/parse-that-regex/src/number/mod.rs:103` - number span scanner.
- `skinny/crates/parse-that-regex/src/number/mod.rs:225` through `skinny/crates/parse-that-regex/src/number/mod.rs:272` - integer and f64 materializers.
- `skinny/crates/parse-that-regex/src/number/mod.rs:304` through `skinny/crates/parse-that-regex/src/number/mod.rs:319` - mantissa accumulation and overflow rule.
- `skinny/crates/parse-that-regex/src/number/integer.rs:9` through `skinny/crates/parse-that-regex/src/number/integer.rs:54` - integer fallback parser.
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:131` through `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:177` - f64 compute path and ambiguous fallback signal.
- `skinny/crates/runtime/src/grammars/json/generated.rs:507` through `skinny/crates/runtime/src/grammars/json/generated.rs:525` - array direct value dispatch.
- `skinny/crates/runtime/src/grammars/json/generated.rs:674` through `skinny/crates/runtime/src/grammars/json/generated.rs:684` - direct array number span scanning.
- `skinny/crates/runtime/src/grammars/json/generated.rs:749` through `skinny/crates/runtime/src/grammars/json/generated.rs:773` - direct array number materialization and f64 sink.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:89` through `skinny/crates/bbnf-bench/src/direct_struct.rs:104` - hand/direct digest number classification.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:579` through `skinny/crates/bbnf-bench/src/direct_struct.rs:583` - hand parser number path.
