# AZ-IV W0 Pre-Wave Baseline

**Recorded**: 2026-05-01 by orchestrator before W0 dispatch
**Base commit**: `2678ed4410399454e89dec8bd6ccda579594097a` (master, clean)
**Branch**: `master`

## Generated LOC budget anchor (per W0 hard gate 9, ±5 % per file)

| Generated artefact | Pre-W0 LOC | Post-W0 ceiling (+5 %) | Post-W0 floor (-5 %) |
|---|---:|---:|---:|
| `crates/core/src/grammar/generated/bbnf.rs`          | 17260  | 18123  | 16397  |
| `crates/core/src/grammar/generated/bnf.rs`           | 2500   | 2625   | 2375   |
| `crates/core/src/grammar/generated/css_l4.rs`        | 88213  | 92624  | 83802  |
| `crates/core/src/grammar/generated/css_pretty.rs`    | 4927   | 5174   | 4680   |
| `crates/core/src/grammar/generated/csv.rs`           | 1227   | 1289   | 1165   |
| `crates/core/src/grammar/generated/ebnf.rs`          | 6047   | 6350   | 5744   |
| `crates/core/src/grammar/generated/google_sheets.rs` | 11623  | 12205  | 11041  |
| `crates/core/src/grammar/generated/json.rs`          | 2173   | 2282   | 2064   |
| `crates/core/src/grammar/generated/math.rs`          | 624    | 656    | 592    |
| `crates/core/src/grammar/generated/mod.rs`           | 35     | 36     | 33     |
| **total**                                            | **134629** | **141360** | **127898** |

Overflow blocks W0 close until the regression is traced, deliberately accepted with a recorded ceiling raise, or rolled back.
