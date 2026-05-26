# SK-V14 W5B.1 Plan: Request-Local Import Closure

Inputs:

- `restart/skinny/tranches/sk-v14/SPEC.md` §8B W5B.1 requires request-local
  import DAG resolution, source-map closure, missing-import fail-closed test,
  and import-cycle fail-closed test.
- W5B.0 is admitted at `c52e624c6`; W5B.1 may now touch
  `skinny/crates/grammar/src/lib.rs`.
- `skv14-W5B1-A-import-closure-surface.md` finds import fact scanning but no
  import graph.
- `skv14-W5B1-B-request-source-map.md` finds codegen already supplies a
  request-local source map but W5B.4 owns consumption.

Intervention: add a grammar crate frontend closure API that resolves `@import`
edges entirely from the provided `RuntimeSource<'_>` slice and exposes a
coherent frontend closure surface: per-source hashes plus import edges carrying
specifier, importer hash, resolved path, and resolved source hash for later
W5B.4 consumption.

Owner paths:

- Redress may edit `skinny/crates/grammar/src/lib.rs`.
- Redress may edit only the W5A runtime request test fixture in
  `skinny/crates/codegen/src/lib.rs` to provide the imported request-local
  source already required by the new fail-closed import closure. No codegen
  generation behavior may change in W5B.1.
- Redress may write dedicated proof logs under `/tmp/skv14-w5b-<test-name>.log`.
- Redress must not edit grammar-provider behavior, xtask, provider/template,
  generated runtime, or results/rolling-delta files in W5B.1.

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_import_graph_resolves_request_sources --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_missing_import_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_import_cycle_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_runtime_contract_consumes_source_and_metadata --profile ax-iter -- --exact`

Each command tees to its matching `/tmp/skv14-w5b-<test-name>.log`, and each log
is paired with a dedicated
`rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-<test-name>.log`.

Same-wave consumer: `parse_runtime_source_facts()` consumes the closure builder
in the grammar crate by resolving import edges and failing closed before
returning runtime source facts. W5B.4 remains responsible for codegen generation
behavior that uses those closure facts.

Revert protocol: if any exact W5B.1 test fails inside the 30-minute cap, revert
the `skinny/crates/grammar/src/lib.rs` slice, save the failed patch to
`/tmp/skv14-waveW5B1-rejected.patch`, and record REDRESS rather than weakening
request-local import closure.

Pre-blocked routes:

- No filesystem import lookup.
- No provider/template change.
- No codegen consumer claim before W5B.4; the only codegen edit allowed here is
  request-fixture closure preservation for existing W5A proof.
- No public syntax expansion beyond existing `@import` directive acceptance.
- No Rust API that requires a newer compiler than the skinny workspace MSRV.
