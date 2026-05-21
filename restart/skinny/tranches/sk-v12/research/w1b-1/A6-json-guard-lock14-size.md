# SK-V12 W1b-1 A6 - JSON Guard, Lock 14, And Size Budget

Scope: read-only W1b-1 research. Current head is `da0c2481`; no source edits.

## Findings

W1a’s Lock 14 gate is consumed by `bbnf-bench --bin gate` before either JSON
report rendering or SK-V12 non-JSON companion report validation. The active
scan is `lock14_baseline::validate`, covering frozen roots, `BackendShape`
cardinality, and generic-crate neutrality.

Current generic scan roots include `crates/codegen/src/lib.rs`,
`crates/codegen/src/grammar_profile.rs`, `crates/passes/src/lib.rs`,
`crates/runtime/src/lib.rs`, `crates/runtime/src/tape`, and `crates/ir/src`.

W1b-1 must keep CSS policy out of those generic roots. CSS-specific alphabet,
fact stream, sink/oracle, and generated runtime policy belong in CSS-owned
provider/runtime/bench files, not generic codegen/runtime/tape/IR.

## No-Touch Proof Commands

Use only if W1b-1 does not move JSON-producing behavior paths:

```sh
git diff --exit-code -- skinny/RESULTS.md

RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- check-real-typed
RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- check-conformance
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --with-cost-facts --advisory --check-results
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

Record `json_guard_state=not_refreshed:no_behavior_drift` only if these pass.

## Guard Rerun Commands

Required if W1b-1 moves generic runtime/codegen, generated JSON, JSON
parser/scanner, SIMD/string helpers consumed by JSON, benchmark/report/gate
code that renders or validates JSON, or `skinny/RESULTS.md`:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1b-1-json-guard-target CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --with-cost-facts --advisory --check-results
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

## Size Accounting Baseline

W1a generated JSON roster, excluding JSON-owned `scan.rs` / `sink.rs`, is 1614
LOC / 52323 bytes. JSON-owned non-generated `scan.rs + sink.rs` is 457 LOC /
14031 bytes. `generated_real_typed.rs` is 1846 LOC / 62599 bytes. The selected
CSS L4 grammar inputs already present (`tokens`, `values`, `value-unit`,
`properties`) total 405 LOC / 18114 bytes. The CSS runtime directory does not
exist yet.

W1b-1 must record generated CSS runtime LOC, module bytes, grammar bytes, and
O(N) status. Current `SkV12NonJsonRow` does not include
`generated_loc`, `generated_module_bytes`, or `grammar_size_guard`; adding gate
consumption for those fields will touch report/gate code and should be treated
as JSON guard-sensitive.

## Preblocked Routes

W1b-1 may only scaffold `css_l4/declaration_values/direct_to_struct/main` with
output plane `css_l4_declaration_value_fact_stream`.

Blocked:

- Sheets / BBNF-self before measured CSS redress.
- hand-only, parser-only, schema-only, report-only, or stale REDRESS 111
  fixture row.
- CSS ADMIT or lightningcss claim in W1b-1; lightningcss belongs to W1b-2.
- new directive, BIR variant, `BackendShape`, public substrate API, hidden
  host schema, x86.
- generic JSON/CSS/Sheets policy branches.
- SIMD proof-only/orphan primitives.
