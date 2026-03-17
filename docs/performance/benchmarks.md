# Benchmarks

## Overview

BBNF uses [bencher](https://crates.io/crates/bencher) for benchmarking with throughput measurement (MB/s via `b.bytes`). Benchmarks cover three grammar families across two execution modes:

- **AOT** — ahead-of-time compiled parsers via `#[derive(Parser)]` (Rust codegen)
- **VM** — bytecode-compiled grammars run by the `bbnf-ir` interpreter

## Running Benchmarks

```bash
# All benchmarks
cd rust && cargo bench -p bbnf

# Individual benchmark suites
cargo bench -p bbnf --bench json_parse
cargo bench -p bbnf --bench css_parse
cargo bench -p bbnf --bench google_sheets

# LSP benchmarks (manual timing, not bencher)
cargo test -p bbnf-lsp --test bench_lsp -- --nocapture
```

## Benchmark Suites

### JSON (`json_parse.rs`)

| Dataset | Size | Description |
|---------|------|-------------|
| `data.json` | 35 KB | Small mixed-type JSON |
| `twitter.json` | 617 KB | Twitter API response (strings, nested objects) |
| `citm_catalog.json` | 1.6 MB | Event catalog (arrays, numbers) |
| `canada.json` | 2.1 MB | GeoJSON coordinates (number-heavy) |

Groups: `aot_json`, `vm_json`

### CSS (`css_parse.rs`)

| Dataset | Size | Description |
|---------|------|-------------|
| `normalize.css` | 6 KB | Reset stylesheet |
| `bootstrap.css` | 274 KB | Full Bootstrap framework |
| `tailwind.css` | 3.6 MB | Full Tailwind v2 utility build |

**Caveat**: The prettify grammar (`css-stylesheet-pretty.bbnf`) only consumes a fraction of real-world stylesheets. Throughput is reported over the full file size for consistency, but absolute numbers should be interpreted carefully.

Groups: `aot_css`, `vm_css`

### Google Sheets (`google_sheets.rs`)

| Benchmark | Description |
|-----------|-------------|
| `vm_compile` | Grammar compilation + bytecode generation |
| `vm_parse_*` / `aot_parse_*` | Pathological, 1KB, 10KB formulas |
| `*_format_*` | End-to-end parse + pretty-print |
| `*_format_*_cached` | Render only (pre-parsed AST) |
| `*_to_doc_only` | AST to Doc conversion (no rendering) |
| `*_render_only` | Doc to string rendering (no parsing) |

Groups: `vm_benches`, `aot_benches`, `aot_phase_benches`

### LSP (`bench_lsp.rs`)

Separate manual-timing suite. Measures 8 LSP actions across 5 grammar sizes.

```bash
cargo test -p bbnf-lsp --test bench_lsp -- --nocapture
```

## Fairness Notes

- All benchmarks set `b.bytes` for throughput (MB/s) reporting
- Input data is loaded once before benchmarking (not included in timing)
- VM benchmarks create a new `Interpreter` per iteration (includes allocation)
- AOT benchmarks reuse the parser across iterations (parser construction is free)
- Benchmark data files are in `data/json/` and `data/css/`
- Bench profile uses `lto = "fat"` and `codegen-units = 1` for maximum optimization
