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
| **CSS** | `prettify_css()` | L1.5—at-rules, media queries, selectors, declarations |
| **BNF** | `prettify_bnf()` | Standard Backus-Naur Form |
| **EBNF** | `prettify_ebnf()` | Extended BNF (ISO 14977) |
| **BBNF** | `prettify_bbnf()` | BBNF grammar files themselves |

All formatters are generated from BBNF grammars. The `@pretty` directives in the grammar control all layout decisions: `group`, `indent`, `sep("...")`, `split("...")`.

## Performance

See [Formatting Performance](/docs/performance/formatting) for full benchmark data. gorgeous achieves 205 MB/s end-to-end on bootstrap.css (13x faster than Biome), with cached parse results reaching 409 MB/s.

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

The `@pretty split(",")` directive enables grammar-driven format-time splitting of opaque spans (like CSS selector lists) using `split_balanced()` from parse_that, which respects parenthesis/bracket nesting and string quoting.
