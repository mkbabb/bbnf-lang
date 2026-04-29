---
title: Benchmarks
order: 46
section: Performance
---

# Benchmarks

## Overview

BBNF uses divan for benchmarking. Legacy `bencher` / libtest
benchmark harnesses are retired from the live Rust bench surface. All
runtime benchmark claims should cite divan output or samply artifacts;
Linux instruction-count claims should cite iai-callgrind output.

bbnf-lang benchmarks all competitors (combinator-based, hand-written,
SIMD-accelerated) for both JSON and CSS. parse-that keeps only its own
combinator micro-benches. gorgeous keeps only formatting benches.

Post-AZ-II partial-close numbers are stale for planning. Refresh the
17-entry matrix after `cutover.O` closes EBNF, deletes tape, and
recodes parity.

## Running Benchmarks

```bash
# All workspace divan benchmarks
cargo bench --profile ay-final --workspace

# Individual benchmark suites, run sequentially for stable numbers.
cargo bench --profile ay-final -p bbnf --bench json_monolithic
cargo bench --profile ay-final -p bbnf --features competitor --bench json_competitors
cargo bench --profile ay-final -p bbnf --bench css_l4
cargo bench --profile ay-final -p bbnf --features competitor --bench css_competitors
cargo bench --profile ay-final -p bbnf --bench google_sheets_monolithic

# LSP divan benchmarks
cargo bench --profile ay-final -p bbnf-lsp --bench bench_lsp
```

## Benchmark Suites

### JSON — BBNF (`json_monolithic.rs`)

Four tiers of BBNF JSON parsing on the same datasets, all using `BumpSlab` (cold per-parse: fresh slab + parser per iteration):

| Tier | What | Work Level |
|------|------|------------|
| **span** | Opaque AST spans | Structural validation |
| **borrow** | Borrowed `JsonValue` — numbers parsed, strings stripped | Zero-copy, no escape decode |
| **copy** | Owned `JsonValue` — full escape decode, `Cow<str>` | Full deserialization |
| **vm** | Bytecode interpreter | Runtime interpretation |

Groups: `span`, `borrow`, `copy`, `vm` — 4 groups × 5 datasets = 20 bench fns

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

Groups: `bench_serde`, `bench_serde_borrow`, `bench_sonic`, `bench_simd`, `bench_jiter`, `bench_nom`, `bench_winnow`, `bench_pest` — 8 groups × 6 datasets = 48 bench fns

### CSS — BBNF (`css_l4.rs`)

Four tiers of BBNF CSS parsing on 3 datasets:

| Tier | Grammar | What |
|------|---------|------|
| **span** | `css/pretty.bbnf` | Opaque spans, `@ws` SIMD whitespace, `@token` lexical fusion (L0) |
| **slab** | `css/pretty.bbnf` | Typed enum tree with opaque values (L0.5) |
| **l4** | `css/l4/stylesheet.bbnf` | Full CSS L4 via `@import` composition — property-aware dispatch, typed selectors, typed values |
| **vm** | `css/pretty.bbnf` | Bytecode interpreter |

Groups: `slab`, `span`, `l4`, `vm` — 4 groups x 3 datasets = 12 bench fns. Cold per-parse with `BumpSlab`.

### CSS — Competitors (`css_competitors.rs`)

| Parser | Category | Notes |
|--------|----------|-------|
| cssparser | Hand-written | Mozilla/Servo CSS tokenizer, callback/visitor (L0–L1) |
| lightningcss | Hand-written | Parcel CSS parser, full L2 semantic parse |

Groups: `bench_cssparser`, `bench_lightningcss` — 2 groups × 3 datasets = 6 bench fns

lightningcss may skip tailwind on parse error.

### Google Sheets (`google_sheets_monolithic.rs`)

| Benchmark | Description |
|-----------|-------------|
| `vm_compile` | Grammar compilation + bytecode generation |
| `vm_parse_*` / `aot_parse_*` | Pathological, 1KB, 10KB formulas |
| `*_format_*` | End-to-end parse + pretty-print |

Groups: `vm_benches`, `aot_benches`

### LSP (`bench_lsp.rs`)

Divan suite for LSP actions across representative grammar sizes.

## Datasets

### JSON (6 files)

| Dataset | Size | Description |
|---------|------|-------------|
| `data.json` | 35 KB | Small mixed-type JSON |
| `twitter.json` | 617 KB | Twitter API response (strings, nested objects) |
| `citm_catalog.json` | 1.6 MB | Event catalog (arrays, numbers) |
| `canada.json` | 2.1 MB | GeoJSON coordinates (number-heavy) |
| `data_xl.json` | 20 MB | 18K synthetic records (mixed types, 7 levels deep) |
| `data_supermaxx.json` | 1.0 GB | 350K synthetic records (escape-heavy strings, deep nesting, large arrays) |

### CSS (3 files)

| Dataset | Size | Description |
|---------|------|-------------|
| `normalize.css` | 6 KB | Reset stylesheet |
| `bootstrap.css` | 274 KB | Full Bootstrap framework |
| `tailwind.css` | 3.6 MB | Full Tailwind v2 utility build |

## Validation

Every bench fn validates parse success once before the hot loop. The
bench binary panics if any parser cannot handle the input. CSS benches
also assert >=95% consumption unless the suite documents a stricter
semantic-parity gate.

## Fairness Notes

- Divan benchmark groups report throughput in MB/s for input-sized parse
  workloads.
- All benchmarks use mimalloc as the global allocator
- Input data is loaded once before benchmarking (not included in timing)
- VM benchmarks create a new `Interpreter` per iteration (includes allocation)
- Slab benchmarks create a fresh `BumpSlab` + `Parser` per iteration (cold per-parse)
- simd-json requires `.to_vec()` per iteration — inherent library cost
- Benchmark data files are in `data/json/` and `data/css/`
- Bench profile uses `lto = "fat"` and `codegen-units = 1` for maximum optimization
- Codegen-level optimizations (inline optional Span, generalized regex strength reduction, direct Span construction in delim_scan, `@token` fusion) are applied automatically by the IR pipeline—no hand-written Rust in any BBNF benchmark

### Work-Equivalence Tiers (JSON)

Charts group parsers by the actual work performed, not just by whether they return borrowed references:

| Tier | String Work | Tree Structure | Parsers |
|------|------------|----------------|---------|
| **Span** | None (opaque byte ranges) | Structural validation | BBNF span, tree-sitter |
| **Borrow** | Strip quotes, no escape decode | Zero-copy tree | BBNF borrow, nom, winnow, pest |
| **Copy** | Full or selective escape decode, owned/Cow allocation | Deserialized tree | BBNF copy, sonic-rs, serde_json_borrow, simd-json, jiter, serde_json |

Comparing across tiers is misleading—a no-decode parser skipping escape handling will always outperform a full-decode parser on the same input. Within each tier, the work performed is comparable and differences reflect genuine parser/codegen efficiency.

## Cross-Repo Layout

| Repo | Benches | Purpose |
|------|---------|---------|
| **bbnf-lang** | `json_monolithic`, `json_competitors`, `json_value`, `css_l4`, `css_competitors`, `google_sheets_monolithic`, plus VM/stress/compile variants | All competitors |
| **parse-that** | `parse_that_combinator`, `parse_that_css`, `micro_parse_that` | parse-that's own combinator perf only |
| **gorgeous** | `gorgeous` | Formatting benches only (gorgeous vs Biome) |
