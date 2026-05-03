# W1 — TS Source Generator Specification

Date: 2026-05-03
Scope: Specification of which typed-IR variants emit which TypeScript constructs at BD.W1 activation. Documents the per-shape emit decomposition, host-fn resolution, BorrowedSpan lifetime surface, generated-LOC budget per grammar.

## §1 Per-Shape Emit Decomposition

The BC.W0 typed IR contract (`docs/tranches/BC/BC.md:139-152`) names the alphabet. W1's TS emitter lowers each variant to canonical TypeScript per the table:

| TypedIR variant | TS emit shape | Sample |
|---|---|---|
| `TypedRule { rule_id, body, layout }` | `function parse<Rule>(ctx: ParseCtx): <G>Value { ... }` | `function parseObject(ctx: ParseCtx): JsonValue { ... }` |
| `TypedAlt { branches, dispatch: ByteDisjoint }` | `switch (ctx.bytes[ctx.pos]) { case 0x7b: return parseObject(ctx); ... }` | per `audit/RESTART-SKETCH-2026-05-03.md:559-577` |
| `TypedAlt { branches, dispatch: Speculative }` | ordered try with `const checkpoint = ctx.pos; try { ... } catch { ctx.pos = checkpoint; ... }` | speculative dispatch |
| `TypedSeq { children, layout }` | object-literal field writes: `return { kind: 'pair', key, value };` | typed-record construction |
| `TypedRepeat { body, kind: Star }` | `[]` + push loop with break on stop-class | `while (notStopClass(ctx.bytes[ctx.pos])) { items.push(parseItem(ctx)); }` |
| `TypedRepeat { body, kind: Plus }` | first-mandatory + push loop | first parse + while loop |
| `TypedCharClass { class }` | inline byte-test or regex-equivalent: `if (b >= 0x30 && b <= 0x39) ...` | digit class |
| `TypedKeyword { keyword }` | byte-comparison: `if (bytes.subarray(pos, pos+len).every((b, i) => b === KW_BYTES[i])) ...` | constant-byte keyword |
| `TypedRef { rule_id }` | recursive call: `parse<Rule>(ctx)` | `parseValue(ctx)` |
| `TypedRegex { regex_id }` | bbnf-regex DFA compiled to TS regex literal: `if (REGEX_<id>.test(bytes.subarray(pos, end))) ...` | regex match |
| `TypedMap { inner, fn_id }` | typed-enum constructor with bound field positions: `return { kind: 'mapped', value: <inner> };` | post-parse map |
| `TypedHost { inner, fn_id }` | host-fn resolution: `return runtime.<host_fn>(ctx.bytes.subarray(start, end));` | host-fn call |

Each emit pattern is sample-realised in `crates/bbnf-codegen/src/ts/parse_fn.rs`; the emit function takes a `&TypedIRNode` and a `&mut TsEmitContext` and writes to `ctx.output_buf`.

## §2 Per-Rule Module Structure

For each grammar, the emitted TS module structure:

```
crates/bbnf-codegen/src/ts/generated/<grammar>.ts
├── // imports
├── // typed value sum
├── export type <G>Value = ...
├── // record interfaces
├── export interface <G>Pair { ... }
├── // parse fns
├── export function parse<G>(input: Uint8Array): <G>Value
├── export function parse<RuleA>(ctx: ParseCtx): <RuleAValue>
├── ...
├── // visitor surface
├── export interface <G>Visitor { ... }
└── // helper utilities
```

Each grammar emits one TS module; the module size is bounded by the per-grammar generated/* LOC × ~2 (TS verbosity factor).

## §3 Generated-LOC Budget per Grammar

Per `docs/tranches/BD/BD.md` Generated-LOC Budget, the TS budget is ≤ 280K LOC across 9 grammars (≤ 35K per grammar max). Per-grammar estimate:

| Grammar | Rust LOC (BC close) | TS LOC (BD.W1 estimate) | Multiplier |
|---|---:|---:|---:|
| json.rs | 2,250 | ~4,500 | 2.0× |
| bbnf.rs | 20,200 | ~33,000 | 1.6× |
| css_l4.rs | 94,800 | ~120,000 | 1.3× |
| google_sheets.rs | 13,460 | ~25,000 | 1.9× |
| css_pretty.rs | 1,820 | ~3,500 | 1.9× |
| ebnf.rs | 1,520 | ~3,000 | 2.0× |
| bnf.rs | 610 | ~1,200 | 2.0× |
| csv.rs | 335 | ~650 | 1.9× |
| math.rs | 172 | ~340 | 2.0× |
| **TOTAL** | **135,167** | **~191,190** | **1.4×** (effective) |

The TS multiplier is < 2× because TS's expressive type system reduces the per-record LOC vs Rust; CSS L4 has the lowest multiplier because its many record types have shorter TS forms via union types.

The aggregate TS LOC at BD.W1 close is ~191K (under the ≤ 280K budget). At BD.W5 (full activation), the LOC is the same (W5 is verification, not generation).

## §4 Host-Fn Resolution Table

Per BD.W1 §2.3, the host-fn resolution table is workspace metadata at `Cargo.toml`'s `[workspace.metadata.bbnf-host-fns]` block. The TS emitter consults the `ts` column at codegen time:

```toml
[workspace.metadata.bbnf-host-fns]
parse_hex_color = { rust = "crate::host::parse_hex_color", ts = "runtime.parseHexColor", wasm = "extern_idx_0" }
parse_color_name = { rust = "crate::host::parse_color_name", ts = "runtime.parseColorName", wasm = "extern_idx_1" }
parse_url = { rust = "crate::host::parse_url", ts = "runtime.parseUrl", wasm = "extern_idx_2" }
parse_dimension = { rust = "crate::host::parse_dimension", ts = "runtime.parseDimension", wasm = "extern_idx_3" }
```

The TS emitter generates calls like:

```typescript
// emitted for `-> parse_hex_color` annotation
const value = runtime.parseHexColor(ctx.bytes.subarray(start, end));
```

The runtime lib at `npm/runtime/src/host-fns.ts` provides JS implementations:

```typescript
// npm/runtime/src/host-fns.ts
export function parseHexColor(bytes: Uint8Array): CssColor {
  // implementation
}
```

## §5 BorrowedSpan Lifetime Surface

Per Lock 9 (`docs/HARDENING-PLAN-PROMPT.md:50`), the slice-borrow primary surface uses `&'i str` slices over input. TS analogue: `BorrowedSpan` wrapper over `Uint8Array.subarray()`:

```typescript
// npm/runtime/src/types.ts
export interface BorrowedSpan {
  input: Uint8Array;
  start: number;
  end: number;
}

export function spanAsString(span: BorrowedSpan): string {
  return new TextDecoder('utf-8').decode(span.input.subarray(span.start, span.end));
}
```

The TS emitter emits parse fns that return `BorrowedSpan` for "string"-typed payloads:

```typescript
// JSON: parseString returns BorrowedSpan, not string
function parseString(ctx: ParseCtx): BorrowedSpan {
  if (ctx.bytes[ctx.pos] !== 0x22) throw syntaxErr(ctx.pos);
  const start = ctx.pos + 1;
  // ... scan to closing quote ...
  return { input: ctx.bytes, start, end };
}
```

The materialisation to `string` is opt-in via `spanAsString(span)`. Bench harness measures the zero-copy benefit at BD.W1 §2.5.

## §6 ParseCtx Shape

The TS emitter's `ParseCtx` is the parse state:

```typescript
// npm/runtime/src/types.ts
export interface ParseCtx {
  bytes: Uint8Array;
  pos: number;        // current byte offset
  // optional fields for cursor / path support
  cursor?: PathCursor;
  arena?: <G>Arena;
}
```

The `cursor` field is only present when path-based queries are active (Lock 3); the eager fast path's `__EAGER_EMPTY_PATH` lazy is `cursor === undefined`, eliding cursor calls per the BA.W4 mitigation.

## §7 Error Handling

Errors throw a `SyntaxErr` instance:

```typescript
// npm/runtime/src/types.ts
export class SyntaxErr extends Error {
  constructor(public pos: number, public expected?: string) {
    super(`syntax error at ${pos}${expected ? `: expected ${expected}` : ''}`);
  }
}

export function syntaxErr(pos: number, expected?: string): never {
  throw new SyntaxErr(pos, expected);
}
```

The throw-on-error model matches the Rust emitter's `Result<T, ParseErr>` semantics; JS exception unwind is the analogue of Rust's `?` operator.

## §8 npm Package Layout

`@bbnf-lang/runtime` package layout:

```
@bbnf-lang/runtime
├── package.json
├── index.js              (NAPI loader for native bindings)
├── index.d.ts            (TS types)
├── parsers/
│   ├── json.ts           (xtask-emitted)
│   ├── csv.ts            (xtask-emitted)
│   ├── ... (other grammars at BD.W5)
├── src/
│   ├── types.ts          (BorrowedSpan, SyntaxErr, ParseCtx)
│   ├── host-fns.ts       (JS host-fn implementations)
│   └── parity/
│       ├── canonical-json.ts
│       └── comparator.ts
└── __bench__/
    └── bench-twitter.js  (BD-G1 verification)
```

The per-platform sub-packages (`@bbnf-lang/runtime-darwin-arm64`, etc.) ship only the `.node` binary + minimal `package.json`.

## §9 Bench Harness Methodology

`npm/runtime/__bench__/bench-twitter.js`:

| Step | Detail |
|---|---|
| Warmup | 10 samples discarded (V8 JIT stabilisation) |
| Sample count | 100 |
| Measurement | `performance.now()` deltas per parse |
| Aggregation | median (50th percentile); p99 reported |
| Gate | median ≤ 8 ms (BD-G1) |
| Comparison | sequential vs `JSON.parse` baseline; record ratio in audit doc |

The harness does NOT run other workloads (canada.json, citm-catalog.json) at W1; those land at W5 as parity-matrix members.

## §10 Closing Posture

The TS source generator at BD.W1 emits canonical TypeScript per the BC.W0 IR contract. Per-shape emit decomposition is a 12-row table; per-grammar generated LOC is ~191K (under the ≤ 280K budget); host-fn resolution consumes workspace metadata; BorrowedSpan preserves Lock 9's slice-borrow primary surface; ParseCtx + SyntaxErr provide the parse state + error model; npm package layout ships per-platform NAPI binaries. The bench harness ratifies BD-G1 against sonic-rs baseline + `JSON.parse` floor.
