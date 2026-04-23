# divan migration — concrete procedure

**Scope**: replace every `bencher`-harness bench target in the workspace with
`divan`. Remove `bencher = "0.1"` dependency. Add `divan = "0.1"` under
`[dev-dependencies]`. Add `iai-callgrind` as a CI-only secondary surface.

## Current state (enumerated)

`crates/core/Cargo.toml` declares **19 `[[bench]]` entries**, each with
`harness = false`:

| # | name | path |
|---|---|---|
| 1  | `json_monolithic`          | `benches/json/monolithic.rs` |
| 2  | `json_parse_that`          | `benches/json/parse_that.rs` |
| 3  | `json_vm`                  | `benches/json/vm.rs` |
| 4  | `json_competitors`         | `benches/json/competitors.rs` |
| 5  | `json_stress`              | `benches/json/stress.rs` |
| 6  | `css_l4`                   | `benches/css/l4.rs` |
| 7  | `css_vm`                   | `benches/css/vm.rs` |
| 8  | `css_competitors`          | `benches/css/competitors.rs` |
| 9  | `css_stress`               | `benches/css/stress.rs` |
| 10 | `google_sheets_monolithic` | `benches/google_sheets/monolithic.rs` |
| 11 | `google_sheets_vm`         | `benches/google_sheets/vm.rs` |
| 12 | `json_ts`                  | `benches/json/ts.rs` |
| 13 | `json_wasm`                | `benches/json/wasm.rs` |
| 14 | `css_ts`                   | `benches/css/ts.rs` |
| 15 | `css_wasm`                 | `benches/css/wasm.rs` |
| 16 | `compile_pipeline`         | `benches/compile_pipeline.rs` |
| 17 | `json_value`               | `benches/json/value.rs` |
| 18 | `bbnf_monolithic`          | `benches/bbnf/monolithic.rs` |

Every file imports `use bencher::{Bencher, benchmark_group, benchmark_main}`
and wraps its entries in `benchmark_group!(...); benchmark_main!(...);`.

Additionally, `crates/core/benches/common/timeout.rs` provides
`bench_with_timeout(b, limits::*, || body())` — a wall-clock guard wrapping
each call. Divan's migration retains this guard via a `divan::Bencher`-compatible
shim (details in §Shim below).

## Target dependency matrix

### Remove

- `bencher = "0.1"` from `crates/core/Cargo.toml:40`.

### Add

- `divan = "0.1"` under `[dev-dependencies]` in `crates/core/Cargo.toml`.
- `iai-callgrind = "0.12"` under `[dev-dependencies]`, feature-gated (`iai`),
  in `crates/core/Cargo.toml` (CI-only; local dev never compiles it).

### Keep

- Every `[[bench]]` entry keeps its `harness = false` (divan registers its
  own `main`; libtest harness stays off).
- `benches/common/timeout.rs` — the wall-clock guard. Adapt the function
  signature to accept `&mut divan::Bencher` instead of `&mut Bencher`.

## Exemplar: BEFORE vs AFTER

Chosen exemplar: `crates/core/benches/compile_pipeline.rs` (94 lines — the
smallest "real" bench file with multiple entries). This is the full before/
after for the smallest bench; the pattern applies mechanically to the rest.

### BEFORE

```rust
use bencher::{Bencher, benchmark_group, benchmark_main};

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
    compile_paths_request,
};

#[path = "common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

fn vm_request() -> CompileRequest { /* ... */ }
fn grammar_path(name: &str) -> std::path::PathBuf { /* ... */ }
fn load_grammar(name: &str) -> String { /* ... */ }

fn compile_json(b: &mut Bencher) {
    let source = load_grammar("json/json.bbnf");
    bench_with_timeout(b, limits::COMPILE_JSON, || {
        compile_grammar_request(&source, &vm_request()).unwrap()
    });
}

// ... compile_ebnf, compile_bbnf, compile_sheets, compile_css_l4 ...

benchmark_group!(
    compile,
    compile_json,
    compile_ebnf,
    compile_bbnf,
    compile_sheets,
    compile_css_l4,
);
benchmark_main!(compile);
```

### AFTER

```rust
use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
    compile_paths_request,
};

#[path = "common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

fn vm_request() -> CompileRequest { /* ... unchanged ... */ }
fn grammar_path(name: &str) -> std::path::PathBuf { /* ... unchanged ... */ }
fn load_grammar(name: &str) -> String { /* ... unchanged ... */ }

#[divan::bench]
fn compile_json(b: divan::Bencher) {
    let source = load_grammar("json/json.bbnf");
    bench_with_timeout(b, limits::COMPILE_JSON, |source| {
        compile_grammar_request(source, &vm_request()).unwrap()
    }, &source);
}

#[divan::bench]
fn compile_ebnf(b: divan::Bencher) { /* same shape */ }

#[divan::bench]
fn compile_bbnf(b: divan::Bencher) { /* path variant */ }

#[divan::bench]
fn compile_sheets(b: divan::Bencher) { /* path variant */ }

#[divan::bench]
fn compile_css_l4(b: divan::Bencher) { /* path variant */ }

fn main() {
    divan::main();
}
```

### Shim: `benches/common/timeout.rs`

The current shim signature is:

```rust
pub fn bench_with_timeout<F: FnMut() -> R, R>(b: &mut Bencher, limit: Duration, f: F);
```

Rewritten for divan:

```rust
use std::time::{Duration, Instant};

/// Run `body` under a per-iteration wall-clock guard. If any single
/// iteration exceeds `limit`, panics — surfacing the regression as a bench
/// failure rather than an indefinite hang (feedback: bench-sequential-regression).
///
/// Divan's Bencher is by-value (not &mut). `setup` runs once per sample-group;
/// `body` runs per sample. The guard checks `body`, not `setup`.
pub fn bench_with_timeout<I: Clone, R>(
    b: divan::Bencher,
    limit: Duration,
    body: impl Fn(I) -> R,
    setup_input: &I,
) {
    b.with_inputs(|| setup_input.clone())
        .bench_values(|input| {
            let start = Instant::now();
            let result = body(input);
            let elapsed = start.elapsed();
            if elapsed > limit {
                panic!(
                    "bench iteration exceeded wall-clock guard: {}ms > {}ms",
                    elapsed.as_millis(),
                    limit.as_millis()
                );
            }
            result
        });
}
```

## Divan configuration (per-workspace invariants)

Divan configuration lives in each bench binary's `main()`. The workspace
invariants (feedback: no-warm-benches, bench-single-run) resolve to:

```rust
fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)         // cold-per-parse; feedback: no-warm-benches
        .skip_ext_time(true)    // exclude setup time from reported per-sample wall
        .max_time(Duration::from_secs(30))
        .run_benches();
}
```

This is centralised in `benches/common/harness.rs` (new) and called as
`common::divan_main()` from every bench binary.

## Parametric benches (generic over grammar)

Agent 2's research notes divan's key advantage: **generic bench functions
across type parameters in one declaration**. Example replacing the current
5-way JSON bench sweep:

```rust
#[divan::bench(
    types = [Json4KB, JsonLarge, JsonStress, JsonCompetitor, JsonValue],
    sample_size = 1,
)]
fn parse_json<F: JsonFixture>(b: divan::Bencher) {
    b.bench_local(|| parse::<F>());
}
```

This collapses `json_monolithic.rs` + `json_stress.rs` + `json_value.rs` into
one bench binary with one function. **Do not do this migration in B1** — it
is a second-order restructuring that should follow the mechanical bencher →
divan port. B1 ports one-to-one; a successor tranche collapses duplicates.

## iai-callgrind CI (one example)

New file: `crates/core/benches/json_callgrind.rs`. Linux-only (valgrind
requirement). Feature-gated `iai` in `Cargo.toml`.

```rust
#![cfg(feature = "iai")]

use bbnf::pipeline::{compile_grammar_request, CompileRequest, CompileTarget, PipelineOptions};
use iai_callgrind::{library_benchmark, library_benchmark_group, main};

fn vm_request() -> CompileRequest {
    CompileRequest { options: PipelineOptions::default(), target: CompileTarget::Vm }
}

#[library_benchmark]
fn compile_json_iai() -> bbnf::pipeline::CompiledProgram {
    let source = std::fs::read_to_string(
        concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/json/json.bbnf")
    ).unwrap();
    compile_grammar_request(&source, &vm_request()).unwrap()
}

library_benchmark_group!(name = compile; benchmarks = compile_json_iai);
main!(library_benchmark_groups = compile);
```

Add a matching `[[bench]]` entry in `crates/core/Cargo.toml`:

```toml
[[bench]]
name = "json_callgrind"
path = "benches/json_callgrind.rs"
harness = false
required-features = ["iai"]
```

The `bench-ci` profile (in `.cargo/config.toml.draft`) runs this on every PR
via `.github/workflows/bench-iai.yml` (deferred to B1.W0.c CI wave).

## Baseline storage

Divan writes its structured output to stdout as a table plus — when
`DIVAN_BENCH_FORMAT=json` is set — one JSON document per bench binary on
stdout. The ceremony artefact pipeline captures these into
`docs/benchmarks/post-B1-W0-divan-{json,css,bbnf,sheets,compile}.json`.

**Do not adopt divan's built-in baseline-comparison feature** (`--save-baseline`).
It writes `target/divan/<name>/` state that is (a) worktree-local and (b)
implicit. The workspace's truth-track convention is plain text under
`docs/benchmarks/`; the comparator is the `bench_regression.sh` script
(already in-repo), adapted to parse divan JSON instead of bencher stdout.

## Migration order (mechanical)

1. Land `divan = "0.1"` in `crates/core/Cargo.toml`; keep `bencher` alongside.
2. Port `compile_pipeline.rs` (exemplar). Run `cargo bench --bench compile_pipeline`;
   verify output matches the pre-migration wall numbers within ±5%.
3. Port all 5 JSON benches. Commit.
4. Port all 4 CSS benches. Commit.
5. Port remaining (sheets, ts, wasm, bbnf, value). Commit.
6. Delete `bencher = "0.1"` from `crates/core/Cargo.toml`. `cargo check -p bbnf` must pass.
7. Update `benches/common/timeout.rs` signature (the shim). Re-run all benches.
8. Add iai-callgrind target + workflow. CI gate on instruction-count regression >1%.

Total: **~20 files touched, ~2 agent-days**. Fits inside B1.W0.c if the divan
port is bounded to mechanical one-to-one (skip parametric collapsing).
