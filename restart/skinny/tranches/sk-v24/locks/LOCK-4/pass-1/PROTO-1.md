# PROTO-1 — VALUE-PLANE-SURVIVAL (LOCK-4, pass 1)

**Probe of families 4A / 4C / 4D — shared runtime-analyzable crux.**

## Question

Does keeping an analyzable parser representation alive at runtime survive the
**value plane** at parity with the inlined-closure core — or does adding typed
value construction push the interpreter below it?

The single number decides whether 4A/4C/4D can be **single-tier** cores
(one analyzable graph doing packrat + diagnostics + iso *and* value assembly),
or must narrow to the two-tier compromise ("analyzable graph for structure,
closures retained for hot value assembly").

- **PASS (SURVIVES):** graph-with-values ≥ 0.98× the closure core, cold paired → single-tier VIABLE.
- **FAIL:** < 0.9× → value plane forces the two-tier split; families narrow.

## Method

Scratch-copy of `parse-that` per R3 §8 (rsync to
`…/scratchpad/pt-scratch`, 7 external sibling patch paths repointed absolute,
isolated `CARGO_TARGET_DIR`, root `rust-toolchain.toml` pin **nightly-2026-04-11**,
`data/` corpus carried). **`/Users/mkbabb/Programming/parse-that` untouched
(READ-ONLY).**

The existing `span_parser` tree-walker produces only `Span` (byte ranges) — no
value plane. The probe adds the value plane as the **smallest** faithful graph:

- **New node `Map { inner: SpanParser, f: Box<dyn Fn(Span)->JsonValue> }`** plus
  its dispatch arm — the one construct that lifts a matched span into a typed
  value through an indirect call. (`JsonNode::Map`, `src/parsers/json_graph.rs`.)
- The whole JSON value parser re-expressed as **one runtime-analyzable arena**
  (`JsonGraph { nodes: Vec<JsonNode> }`, `nodes[0]` = first-byte value dispatch;
  `Array`/`Object` recurse back by index). Node kinds: `Dispatch · Object ·
  Array · Map(string/true/false/null) · Number`. **The closure tier is retired**
  — no `lazy`, no `Parser<JsonValue>`, no `dispatch_byte_multi` in the value path.

**Arms compared** (both build `JsonValue<'a>`, same corpora, cold per-parse):
- **closure** — `parse_that::json::json_parser()` (unchanged): `Parser<JsonValue>`,
  `lazy` recursion, `SmallBox<dyn ParserFn>`, `dispatch_byte_multi`.
- **graph** — `parse_that::json_graph::json_graph()`: the enum arena above.

Bench: divan `value_plane` (new target), mimalloc global (matches the canonical
`parse_that_combinator` harness), `BytesCount`, `bench_local`, cold per-parse
(buffer read once, re-parsed), 100 samples/bench, two arms **adjacent per corpus**
so both see the same machine state. Two windows, opposite temporal position, on
the six JSON corpora. `objdump` on `JsonGraph::call` to locate the Map indirect call.

## What ran

- **Hard gate — parity** (`tests/json_graph_parity_test.rs`): `json_graph().parse`
  vs `json_parser().parse`, asserted **`JsonValue` structural equality**
  (`PartialEq`) on all six corpora **and** 19 edge inputs mirroring the shapes the
  37 `json_test` cases exercise (escapes, unicode, empties, deep nesting, number
  forms, literals, leading/trailing whitespace). **`test result: ok. 2 passed`.**
  (First run overflowed the 2 MB test-thread stack on canada's deeply-nested
  coordinate arrays in the *debug* build — a debug-frame artifact shared by both
  arms, not a value-plane effect; passes cleanly with adequate stack. Release/bench
  frames are tiny.)
- **Bench** — `cargo bench -p parse_that --bench value_plane`, bench profile
  (fat-LTO, codegen-units=1, opt-level=3), two windows. loadavg (1-min) 7.5–8.0
  throughout both windows; identical machine state across arms.
- **asm** — `objdump -d` of `<JsonGraph as …>::call`.

## The numbers

Median cold per-parse. **Ratio = graph throughput ÷ closure throughput**
(= closure_time ÷ graph_time); **> 1.00 ⇒ graph faster**.

| corpus        | closure med | graph med | W1 ratio | W2 ratio | mean |
|---------------|-------------|-----------|----------|----------|------|
| apache-builds | 96.74 µs    | 90.83 µs  | 1.065    | 1.074    | **1.069** |
| canada        | 2.273 ms    | 2.288 ms  | 0.993    | 0.979    | **0.986** |
| citm_catalog  | 1.036 ms    | 960.7 µs  | 1.078    | 1.110    | **1.094** |
| data          | 25.58 µs    | 23.14 µs  | 1.105    | 1.067    | **1.086** |
| data_xl       | 26.71 ms    | 24.61 ms  | 1.085    | 1.032    | **1.059** |
| twitter       | 436.6 µs    | 406.3 µs  | 1.075    | 1.078    | **1.076** |

(closure/graph columns are window 1; W2 medians agree within noise — apache 97.39/90.72,
canada 2.301/2.350, citm 1.051 ms/946.8 µs, data 25.04/23.47, data_xl 25.89/25.08 ms,
twitter 448.9/416.4 µs.)

- **Graph FASTER on 5/6 corpora** by ~6–9%.
- **canada** (coordinate-dense; runtime dominated by `scan_number_strict_f64`,
  which is *byte-identical* between arms) is the natural parity floor: **0.986×
  mean**, worst single window 0.979×. Every corpus ≥ **0.98×**.
- **Aggregate ≈ 1.06×** (graph is at parity-or-better everywhere).

**asm** — `JsonGraph::call` compiles to **647 direct `bl` vs 45 indirect `blr`**;
the Map's `Box<dyn Fn>` call is the fat-pointer load `ldp x0,x8,[x21,#0x38]` /
`ldr x9,[x8,#0x28]` / `blr x9`, and it appears **only on the scalar-leaf path**.
The structural spine — first-byte dispatch, array/object loops, number scan,
whitespace trim — is fully inlined and direct. The value-plane indirection is
confined to leaf construction, which is why it never touches the hot path.

## Why the graph wins rather than merely ties

The "closure core" is not pure monomorphized inlining: it pays `lazy`
(`UnsafeCell` + `Box<dyn LazyParserFn>` + cached-`Option` check) on every
array/object recursion, `SmallBox<dyn ParserFn>` per combinator, and a
`Vec<Parser>` indirection in `dispatch_byte_multi`. The analyzable graph replaces
all of that with a flat `Vec<JsonNode>` arena + enum jump-table dispatch +
by-index recursion. It **adds** exactly one indirection — the Map `blr` — and
confines it to scalar leaves. Net: fewer indirect calls on the spine than the
closure tier it retires. Keeping the representation analyzable *removed* cost
rather than adding it.

## Verdict

> **RAN · SURVIVES.** The runtime-analyzable value graph is at parity-or-better
> with the inlined closure core across all six JSON corpora (worst 0.986× mean /
> 0.979× worst-window on the number-bound canada floor; aggregate ≈ 1.06×), on
> byte-identical typed output (37-test-shape parity gate green). The value plane
> does **not** push the interpreter below the closure core — it is confined to a
> single leaf indirection off the structural spine. **4A/4C/4D single-tier is
> VIABLE**; the two-tier "closures for hot value assembly" compromise is **not
> forced**.

### Files (in scratch copy `…/scratchpad/pt-scratch`)

- `rust/parse_that/src/parsers/json_graph.rs` — value graph (new)
- `rust/parse_that/src/parsers/mod.rs` — `pub mod json_graph;` (+1 line)
- `rust/parse_that/benches/parse_that/value_plane.rs` — paired bench (new)
- `rust/parse_that/tests/json_graph_parity_test.rs` — hard-gate parity (new)
- `rust/parse_that/Cargo.toml` — `[[bench]] value_plane` (+5 lines)

Unified diff: `PROTO-1.patch` (this directory).
