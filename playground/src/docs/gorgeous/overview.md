---
title: Overview
order: 30
section: gorgeous
---

# gorgeous

gorgeous is a grammar-derived code formatter built in Rust. Instead of hand-writing layout logic for each language, gorgeous generates formatters directly from BBNF grammar files using `@pretty` directives. The pipeline is: **parse** (parse_that) **&rarr; AST &rarr; Doc tree** (pprint) **&rarr; formatted output**. All formatters ship as native Rust code and compile to WebAssembly for browser use.

## Built-in Formatters

gorgeous includes five pre-built formatters, each generated from a BBNF grammar:

| Language | Entry point | Notes |
|----------|------------|-------|
| **JSON** | `prettify_json()` | Full JSON spec with surrogate pair validation |
| **CSS** | `prettify_css()` | L1.75 — at-rules, media queries, selectors, declarations |
| **BNF** | `prettify_bnf()` | Standard Backus-Naur Form |
| **EBNF** | `prettify_ebnf()` | Extended BNF (ISO 14977) |
| **BBNF** | `prettify_bbnf()` | BBNF grammar files themselves |

Every formatter is purely grammar-driven with zero manual overrides. The `@pretty` directives in the grammar control all layout decisions: `group`, `indent`, `sep("...")`, `split("...")`.

## Performance

gorgeous is fast. The combination of parse_that's high-throughput parser combinators and pprint's stack-based renderer produces output at hundreds of megabytes per second.

**CSS formatting (gorgeous vs Biome):**

| File | Size | gorgeous | Biome | Speedup |
|------|------|----------|-------|---------|
| app.css | 6 KB | 50 MB/s | 11 MB/s | 4.5x |
| bootstrap.css | 281 KB | 342 MB/s | 17 MB/s | **20x** |
| tailwind.css | 3.8 MB | 36 MB/s | 8 MB/s | 4.5x |

**JSON formatting:** 115 MB/s (cached), competitive with serde-based formatters.

**Internal pipeline throughput** on bootstrap.css: `to_doc` at 1,450 MB/s, `render` at 1,428 MB/s.

## WASM API

gorgeous compiles to WebAssembly via the `bbnf-wasm` crate. The WASM module exports formatting functions alongside BBNF analysis features (diagnostics, hover, completions, semantic tokens, and more).

### Formatter Functions

Each formatter takes the input text and three configuration parameters:

```typescript
function format_json(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;
function format_css(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;
function format_bnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;
function format_ebnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;
function format_bbnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;
```

Functions return `undefined` if parsing fails (e.g., malformed input).

### Browser Integration

The playground loads the WASM module lazily with a singleton loader:

```typescript
import { ensureWasmLoaded, getWasmModule } from "./composables/wasm/loader";

// Initialize once (async, cached)
await ensureWasmLoaded();

// Then call formatters synchronously
const mod = getWasmModule();
const formatted = mod.format_css(input, 80, 2, false);
```

The `useWasm()` composable wraps this pattern for Vue components, providing reactive `isLoaded` / `isLoading` state and async methods:

```typescript
const { formatWithGorgeous, isLoaded } = useWasm();

const result = await formatWithGorgeous("css", input, 80, 2, false);
```

### Node.js Integration

The same WASM module works in Node.js. Import the generated package and call `default()` to initialize:

```typescript
import init, { format_json, format_css } from "bbnf-wasm";

await init();

const formatted = format_json('{"key":"value"}', 80, 2, false);
```

## Configuration

All formatters accept the same three parameters, mirroring pprint's `Printer` struct:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `max_width` | `number` | 80 | Target line width in columns |
| `indent` | `number` | 2 | Spaces per indentation level |
| `use_tabs` | `boolean` | false | Use tab characters instead of spaces |

## How It Works

The gorgeous pipeline for each language:

1. **Parse** — parse_that's parser combinators consume the input and produce a typed AST
2. **to_doc** — BBNF-derived codegen transforms each AST node into a pprint `Doc` tree, applying `@pretty` directives (`group`, `indent`, `sep`, `split`)
3. **Render** — pprint's `pprint()` function traverses the `Doc` tree and produces the formatted string, breaking lines when Groups exceed `max_width`

The `@pretty split(",")` directive deserves special mention: it enables grammar-driven format-time splitting of opaque spans (like CSS selector lists) using `split_balanced()` from parse_that, which respects parenthesis/bracket nesting and string quoting. This eliminated the last manual formatting override in the CSS formatter.
