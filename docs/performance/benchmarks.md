---
title: Benchmarks
order: 46
section: Performance
---

# Benchmarks

## Overview

BBNF uses [bencher](https://crates.io/crates/bencher) for benchmarking with throughput measurement (MB/s via `b.bytes`). All benchmarks use mimalloc as the global allocator and validate parse success before the hot loop.

bbnf-lang benchmarks all competitors (combinator-based, hand-written, SIMD-accelerated) for both JSON and CSS. parse-that keeps only its own combinator micro-benches. gorgeous keeps only formatting benches.

## Running Benchmarks

```bash
# All benchmarks
cd rust && cargo bench -p bbnf

# Individual benchmark suites (run sequentially for stable numbers)
cargo bench -p bbnf --bench json_bbnf
cargo bench -p bbnf --bench json_competitors
cargo bench -p bbnf --bench css_bbnf
cargo bench -p bbnf --bench css_competitors
cargo bench -p bbnf --bench google_sheets

# LSP benchmarks (manual timing, not bencher)
cargo test -p bbnf-lsp --test bench_lsp -- --nocapture
```

## Benchmark Suites

### JSON — BBNF (`json_bbnf.rs`)

Four tiers of BBNF JSON parsing on the same 4 datasets:

| Tier | What | Work Level |
|------|------|------------|
| **span** | Raw BBNF parse — opaque AST spans | Structural validation only |
| **borrow** | Borrowed `JsonValue` — numbers parsed, strings stripped | Zero-copy, no escape decode |
| **owned** | Owned `JsonValue` — full escape decode, `Cow<str>` | Full deserialization |
| **vm** | Bytecode interpreter | Runtime interpretation |

Groups: `span`, `borrow`, `owned`, `vm` — 4 groups × 4 datasets = 16 bench fns

### JSON — Competitors (`json_competitors.rs`)

8 external JSON parsers on the same 4 datasets:

| Parser | Category | Notes |
|--------|----------|-------|
| serde_json | Hand-written | Full owned parse to `Value` |
| serde_json_borrow | Hand-written | Zero-copy borrowed parse |
| sonic-rs | SIMD | Arena-allocated, full unescape |
| simd-json | SIMD | Requires mutable input (`.to_vec()`) |
| jiter | Hand-written | Pydantic's iterable parser |
| nom | Combinator | Borrowed strings, zero-copy |
| winnow | Combinator | nom successor, `dispatch!` macro |
| pest | PEG | Grammar-generated parser |

Groups: `bench_serde`, `bench_serde_borrow`, `bench_sonic`, `bench_simd`, `bench_jiter`, `bench_nom`, `bench_winnow`, `bench_pest` — 8 groups × 4 datasets = 32 bench fns

### CSS — BBNF (`css_bbnf.rs`)

Two tiers of BBNF CSS parsing on 3 datasets:

| Tier | Grammar | What |
|------|---------|------|
| **fast** | `css-fast.bbnf` | Opaque spans, maximum throughput (L0) |
| **pretty** | `css-stylesheet-pretty.bbnf` | Structural AST with `@pretty` directives (L1.5) |

Groups: `fast`, `pretty` — 2 groups × 3 datasets = 6 bench fns

### CSS — Competitors (`css_competitors.rs`)

| Parser | Category | Notes |
|--------|----------|-------|
| cssparser | Hand-written | Mozilla/Servo CSS tokenizer, callback/visitor (L0–L1) |
| lightningcss | Hand-written | Parcel CSS parser, full L2 semantic parse |

Groups: `bench_cssparser`, `bench_lightningcss` — 2 groups × 3 datasets = 6 bench fns

lightningcss may skip tailwind on parse error.

### Google Sheets (`google_sheets.rs`)

| Benchmark | Description |
|-----------|-------------|
| `vm_compile` | Grammar compilation + bytecode generation |
| `vm_parse_*` / `aot_parse_*` | Pathological, 1KB, 10KB formulas |
| `*_format_*` | End-to-end parse + pretty-print |

Groups: `vm_benches`, `aot_benches`

### LSP (`bench_lsp.rs`)

Separate manual-timing suite. Measures 8 LSP actions across 5 grammar sizes.

```bash
cargo test -p bbnf-lsp --test bench_lsp -- --nocapture
```

## Datasets

### JSON (4 files)

| Dataset | Size | Description |
|---------|------|-------------|
| `data.json` | 35 KB | Small mixed-type JSON |
| `twitter.json` | 617 KB | Twitter API response (strings, nested objects) |
| `citm_catalog.json` | 1.6 MB | Event catalog (arrays, numbers) |
| `canada.json` | 2.1 MB | GeoJSON coordinates (number-heavy) |

### CSS (3 files)

| Dataset | Size | Description |
|---------|------|-------------|
| `normalize.css` | 6 KB | Reset stylesheet |
| `bootstrap.css` | 274 KB | Full Bootstrap framework |
| `tailwind.css` | 3.6 MB | Full Tailwind v2 utility build |

## Validation

Every bench fn validates parse success ONCE before the hot loop. The bench binary panics if any parser can't handle the input. CSS benches also assert ≥95% consumption.

## Fairness Notes

- All benchmarks set `b.bytes` for throughput (MB/s) reporting
- All benchmarks use mimalloc as the global allocator
- Input data is loaded once before benchmarking (not included in timing)
- VM benchmarks create a new `Interpreter` per iteration (includes allocation)
- AOT benchmarks reuse the parser across iterations (parser construction is free)
- simd-json requires `.to_vec()` per iteration — inherent library cost
- Benchmark data files are in `data/json/` and `data/css/`
- Bench profile uses `lto = "fat"` and `codegen-units = 1` for maximum optimization

## Cross-Repo Layout

| Repo | Benches | Purpose |
|------|---------|---------|
| **bbnf-lang** | `json_bbnf`, `json_competitors`, `css_bbnf`, `css_competitors`, `google_sheets` | All competitors |
| **parse-that** | `parse_that_combinator`, `parse_that_css`, `micro_parse_that` | parse-that's own combinator perf only |
| **gorgeous** | `gorgeous` | Formatting benches only (gorgeous vs Biome) |
