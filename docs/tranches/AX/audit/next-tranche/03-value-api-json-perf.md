# W1a.A3 — Value API Characterisation + JSON Apples-to-Apples

Baseline commit: `ededfc7c`. Binaries `json_monolithic-683fcdeaeb1021e7`
+ `json_competitors-3be45b3f89974af2` (main shared target). Profiles
`.profiles/samply/json_monolithic/{canada,twitter}/profile.json.{gz,syms.json}`
were already fresh from this HEAD (2026-04-17); consumed verbatim per the
no-rerun contract in `PROFILING.md`.

## 1. Value API surface

BBNF exposes a **walker-style, zero-copy view** model — no materialised tree.
Per-grammar parser entry:

```rust
JsonParser::parse(src) -> Result<Parsed<'_, JsonParser>, ParseError>
```

`Parsed<'p, R>` (`crates/core/src/runtime/parsed.rs:76`) owns the `Tape`
(SoA six-column primary + AoS sidecar per W1.D), borrows `&'p str` input,
and stores the root `TapeOffset`. Consumer surface:

- `parsed.view() -> R::View<'_>` — constant-cost, resolves the root rule's
  generated `<Root>View<'p>` via the `Root` GAT (`view/mod.rs:344-358`).
- `parsed.tape()`, `parsed.input()`, `parsed.root_offset()`,
  `parsed.into_tape()` — raw substrate access.

`<Grammar>NodeView<'p>` + per-rule `<Rule>View<'p>` structs wrap a
`TapeCursor<'p>` + `&'p str` (see `view/mod.rs:97-374`). **Universal
accessors** on every view (emitted by `emit_common_accessors`):
`.kind()`, `.span()`, `.span_text()`, `.input()`, `.variant_idx()`,
`.rule_kind()`, `.children()`, `.child(i)`, `.is_recovered()`,
`.identifier_span()`.

**Typed per-kind accessors** (dispatched in `emit_typed_accessors`,
lines 392-447):
- Leaves (`leaves.rs`): `.text()`, `.as_f64()`, `.as_u32()`,
  `.byte_range()`, and for aggregate-payload rules the packed-bytes
  readers (`payload_f64`, `payload_string_with_source`, colour shims).
- Seq (`seq.rs`): `.child_N()` positional + named accessors from `Ref`
  targets. KV-pair shape `[Span, scalar]` gets `.key()`/`.value()`.
- Alt (`alt.rs`): `.as_<variant>()`, `.is_<variant>()`, `.chosen()`.
- Repeat (`repeat.rs`): `.iter()`, `.len()`, `.is_empty()`, `.get(i)`.

**Serialize + prettify.** `serialize_compact<S>(view) -> String`
(`generate/serialize/mod.rs:76`) dispatches on `variant_idx` to per-rule
`serialize_<rule>` functions that currently emit `span_text()` verbatim
— round-trip byte-equal in the canonical-form parity harness
(`tests/json_canonical_parity.rs`). `<rule>_prettify()` combinators
(`emitter/prettify/`) compose pprint groups from IR shape; `@ws` threads
through OptionalWhitespace so comments + trim are idempotent.

**Vs sonic-rs.** Sonic-rs ships (a) `from_str::<Value>` — materialised
typed tree with unescape; (b) `get_by_path` — lazy lookup over a scanned
tape; (c) `to_object` — on-demand projection. BBNF is closest to (b):
parse produces only the tape, per-field decoding is lazy at view-read.
BBNF has **no materialised-tree mode** — `NodeView` is a cursor, not an
`enum Value { ... }`. `serialize_compact` is the sole materialisation
surface, emitting text not a structured value. This is invariant AX.21:
grammar-derived view surface with canonical-serialization parity, no
hand-coded `bbnf::json::Value`.

## 2. Bench matrix — JSON parse, cold per-parse

File: `/tmp/a3-bench-json-{monolithic,competitors}.txt`. Sizes in bytes
(via ns/iter × MB/s). All numbers ns/iter, cold timer iter.

| fixture  | size (B) | bbnf        | sonic-rs   | simd-json  | serde_json | jiter      | serde_jb   | bbnf/sonic | bbnf/simd |
|----------|---------:|------------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|----------:|
| data     |   35 455 |      84 135 |     14 898 |     27 085 |     37 158 |     23 320 |     22 018 |   **5.65×** | 3.11× |
| twitter  |  631 514 |   1 444 314 |    242 448 |    467 403 |    707 395 |    511 876 |    362 351 |   **5.96×** | 3.09× |
| citm     | 1 727 204 |   3 947 874 |    571 068 |  1 072 896 |  1 475 016 |  1 291 088 |  1 111 787 |   **6.91×** | 3.68× |
| canada   | 2 251 051 |  12 062 762 |  1 463 570 |  3 153 020 |  4 025 532 |  3 115 374 |  3 245 529 |   **8.24×** | 3.83× |
| data_xl  |21 258 988 |  79 518 033 | 14 495 441 | 21 961 020 | 35 310 795 | 26 005 874 | 22 928 637 |   **5.49×** | 3.62× |

All 30 cells populated (5 fixtures × 6 parsers). Throughput floor: **bbnf
loses 5.5–8.2× vs sonic-rs on every fixture**; canada (heavy numeric
payload) is the worst. bbnf is ~2× slower than serde_json's `Value` on
average — bbnf is behind even the hand-written owned-tree comparator.

## 3. Hot-spot attribution — samply self-time

### canada (`.profiles/samply/json_monolithic/canada/`, 7 086 samples)

| # | self % | symbol |
|---|------:|--------|
| 1 | 50.93 | `json_monolithic::__jsonparser_emit_impl::__dta_walker_inline::run` |
| 2 | 16.53 | `bbnf_tape::driver::advance_or_pop_with` |
| 3 | 13.56 | `bbnf_tape::finaliser::finalise` |
| 4 |  4.81 | `<f64 as core::str::traits::FromStr>::from_str` |
| 5 |  4.04 | `core::str::converts::from_utf8` |
| 6 |  3.10 | `<json_monolithic::JsonParser>::parse` |
| 7 |  2.38 | `<bbnf_tape::driver::FrameStack>::nearest_variant_frame` |
| 8 |  1.37 | `_platform_memset` |
| 9 |  1.28 | `core::num::imp::dec2flt::lemire::compute_float::<f64>` |
|10 |  0.78 | `bbnf_tape::psi::write_decoded` |

### twitter (`.profiles/samply/json_monolithic/twitter/`, 3 138 samples)

| # | self % | symbol |
|---|------:|--------|
| 1 | 61.50 | `json_monolithic::__jsonparser_emit_impl::__dta_walker_inline::run` |
| 2 | 11.28 | `bbnf_tape::finaliser::finalise` |
| 3 | 10.42 | `bbnf_tape::driver::advance_or_pop_with` |
| 4 |  5.67 | `bbnf_tape::psi::write_decoded` |
| 5 |  4.11 | `<json_monolithic::JsonParser>::parse` |
| 6 |  1.94 | `_platform_memset` |
| 7 |  1.66 | `<bbnf_tape::driver::FrameStack>::nearest_variant_frame` |
| 8 |  1.08 | `core::str::converts::from_utf8` |
| 9 |  0.86 | `_platform_memmove` |
|10 |  0.48 | `read` (file I/O in timer window) |

## 4. Gap analysis

**Top-3 bbnf union (both fixtures):** `__dta_walker_inline::run`,
`advance_or_pop_with`, `finaliser::finalise`.

No sonic-rs profile is retained under `.profiles/`; attribution cites
sonic-rs's public design rather than a local profile (sonic-rs's hot path
is `sonic_rs::parser::Parser::parse_from_slice` → SIMD scan +
arena-allocated DOM with unified decode). The bbnf gap attributes as:

1. **Dispatch overhead (~72 % of canada self-time).** `__dta_walker_inline::run`
   IS the emitted per-grammar rule kernel — a giant state machine over the
   DTA table with `advance_or_pop_with` as its frame-stack helper.
   Violates invariants 17 + 18 of the AX→AY handoff (no DTA symbols in the
   shape dispatcher path). This is the single dominant cost, not the
   scanner: sonic-rs's SIMD scan amortises structural bookkeeping into
   vector loads; bbnf's walker is one rule-frame dispatch per byte
   class. Cite: `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/json_monolithic/canada/profile.json.syms.json:3609 samples`.

2. **Tape finalisation (~13 % canada, 11 % twitter).**
   `bbnf_tape::finaliser::finalise` post-processes the tape after the
   walker runs — the flamegraph frames include `write_decoded` + an
   entire `max_by::fold` over `&[u8]` for the PSI bookkeeping and a
   pass over `PayloadJob` slots. This is a second pass over data the
   walker already traversed, unconditionally paid even when the
   consumer never touches decoded payloads. Cite:
   `profile.json.syms.json:961 samples` (canada), symbols 76-91 cover
   finalise + psi::write_decoded + dec2flt::lemire.

3. **Scanner/UTF-8 scan is MINOR.** `core::str::converts::from_utf8` is
   4 % canada / 1 % twitter — the input is UTF-8 validated once per parse
   in `JsonParser::parse` (3.10 % canada self-time). The rest of the
   character-class decisions are folded inside `__dta_walker_inline::run`
   — so the scanner cost is absorbed in (1), not a distinct hot spot.

4. **Allocation is minor.** `_platform_memset` at 1.4–1.9 % (zero-fill on
   growth), mimalloc internals invisible (< 0.5 %). Arena allocation
   works. The loss is not in `malloc`.

Synthesis: the bbnf / sonic-rs gap is **dispatch overhead** (walker
runtime state machine) + **double-pass tape finalisation**, not scanner
throughput or allocator overhead. The `NodeView` surface itself is zero-cost
at the API boundary — the cost is upstream in the tape construction pipeline
before the view is ever handed to the consumer.

## 5. Levers — 3 proposals

**L1 — Retire `__dta_walker_inline::run` via per-rule shape emission
(W0b debt).** The single 50–61 % symbol IS the gate-predicate-era DTA
walker AX.W0a.2.h pivoted around; W0b declared its retirement on the
shape-emission-authoritative contract (invariant 20) — W1r has not
touched it. Inlining per-rule emission collapses (1) + (7) = **~53 %
canada / ~63 % twitter self-time** into frames the compiler can
specialise. Cite: canada lines 1 + 2 + 7
(`__dta_walker_inline::run` 3609, `advance_or_pop_with` 1171,
`nearest_variant_frame` 169 samples). Expected bbnf/sonic ratio post-
lever: **~2.5–3×**.

**L2 — Fuse `bbnf_tape::finaliser::finalise` into the emit loop.**
The finaliser sweeps PSI payload jobs, recomputes max-depth, decodes
strings — data the walker already visits. IR's `PayloadLayout` knows
at codegen time which kinds need decoding; hoist into per-record emit
arms, empty the post-pass. Predicted: (3) + (10) = **~14 % canada /
~17 % twitter** + eliminates the second memory sweep (cache wins).
Cite: canada lines 3 + 10
(`finaliser::finalise` 961, `psi::write_decoded` 55 samples).

**L3 — Add a materialised-tree `Value` projection as opt-in surface.**
Lazy `NodeView` is the right default but forces consumers to re-walk
per field (W1r.7 twitter bench added AoS `packed_cache` precisely
because SoA random-access hits 6-column-hops/record). One-shot
`parsed.to_value::<T>() -> T` codegen — grammar-emitted, `T =
bbnf::Value` (enum Object/Array/Number/…) — gives downstream consumers
apples-to-apples vs `sonic_rs::Value`. Current bench compares bbnf
*parse only* to sonic-rs *parse + materialise*; matched-work needs
bbnf to also materialise. Cite: absence of any `<T: FromTape>::materialize`
in `crates/core/src/runtime/parsed.rs:138` (file ends at `view()`).
Predicted: opens a fair comparator lane and reveals whether post-L1
the residual loss is dispatch or materialisation.

## Artefacts

- `/tmp/a3-bench-json-monolithic.txt` — bbnf 5 fixtures.
- `/tmp/a3-bench-json-competitors.txt` — 45 comparator runs (9 × 5).
- `.profiles/samply/json_monolithic/canada/profile.json.{gz,syms.json}`.
- `.profiles/samply/json_monolithic/twitter/profile.json.{gz,syms.json}`.
- Self-time extraction script inlined in this doc's generation (addr → rva
  bisect over syms-proof string-table via firefox-profiler format).
