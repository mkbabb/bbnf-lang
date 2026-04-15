# AU.6.8 — Cross-grammar typed materialisation parity audit

Status as of W6 (post-W5 substrate landing): **systemic codegen
gaps prevent universal `->` reach**. Every grammar family carries
firing typed payloads on at least one annotated rule, but the
codegen exhibits two distinct, repeatable bugs that drop payloads
on a substantial fraction of declared annotations.

The four parity test files committed alongside this document pin
the firing surface so that any future codegen change either
preserves the current behaviour or is forced to update the
tests visibly.

| Family | Test file | Tests | Pass | Fail |
|--------|-----------|-------|------|------|
| JSON | `crates/core/tests/json_parity.rs` | 9 | 9 | 0 |
| CSS L4 | `crates/core/tests/css_l4_parity.rs` | 14 | 14 | 0 |
| BBNF | `crates/core/tests/bbnf_parity.rs` | 18 | 18 | 0 |
| Google Sheets | `crates/core/tests/sheets_parity.rs` | 17 | 17 | 0 |
| **Total** | | **58** | **58** | **0** |

`grammar_roundtrip` remains 6/6 green; no tape parity goldens
were touched.

## Top-level summary

Every `->` annotation in every grammar is one of:

1. **Firing** — the codegen emits `push_leaf_with(...)` /
   `push_leaf_with_arena_frame(...)` and the typed payload reaches
   the tape and is recoverable through the matching
   `Tape::payload_*` accessor.
2. **Routed but blocked** — the function body declares the payload
   buffers (`__has_payload`, `__aggregate_buf`, `__payload_tag`)
   but only one branch (or zero branches) actually writes to them.
   The leaf record is still pushed, but with `child_off ==
   TapeOffset::NONE`, i.e. no payload.
3. **Documented gap** — `-> input : Span` annotations and BBNF's
   `-> Span` shorthand reach IR as `TypeDesc::Named("Span")` which
   the emitter does not lower to `PayloadData`. Result:
   `push_compound(Rule, ...)` with no payload reach.

The two systemic codegen bugs that cause categories (2) and (3):

### Bug 1 — Alt-payload first-branch loss

For Alt-bodied rules with literal alternatives carrying typed
payloads, only **one** alt branch in the emitted code carries the
payload-write block. The other branches break out of the alt
without setting `__has_payload = true`.

Example: `bool = "true" -> true | "false" -> false ;` (JSON). The
generated `__bool` function:

```rust
'__alt_lit_blk0: {
    if ... b"false" ... {                       // first branch
        __aggregate_buf[..1] = [0u8];           // payload write
        __has_payload = true;                   // FIRES
        break '__alt_lit_blk0 Some(());
    }
    if ... b"true" ... {                        // second branch
        // NO payload write                     // GAP
        break '__alt_lit_blk0 Some(());
    }
}
```

So `false` materialises `Bool(false)` correctly but `true`
materialises a leaf with `has_payload=false`.

The same pattern repeats across:

- JSON: `bool` (only `false` branch fires)
- CSS L4: every Nu8 unit rule (`absoluteLengthUnit`,
  `viewportLengthUnit`, `containerLengthUnit`, `fontLengthUnit`,
  `angleUnit`, `timeUnit`, `frequencyUnit`, `resolutionUnit`,
  `flexUnit`); every keyword rule (`positionKeyword`,
  `overflowKeyword`, …); `colorType`, `colorSpace`, `mixSpace`,
  `hueMethodKeyword`, `radialShape`, `radialExtent`, `linearSide`,
  `mediaType`, `filterName`, `mathProductOp`, `mathOperator`,
  `mathFunctionName`, `anPlusB`, `colorProps`, `listTableProps`.
- CSS L4: `namedColor` (148 branches, only ~113 fire — the
  factor-pass byte-dispatch table loses payload writes on
  factored prefix branches such as "violet" under `118u8` and
  "khaki" under `107u8`).
- Sheets: `boolean`, `error_literal`, `sheet_prefix`,
  `compare_op`, `add_op`, `mul_op`, `unary_prefix`.

Single-branch rules (e.g. `percentageUnit = "%" -> 255u8 ;`,
`flexUnit = "fr" -> 0u8 ;`, JSON `null = "null" -> 0u8 ;`) fire
their payload write reliably because there is no alt in which to
lose it.

**Severity**: blocks the AU.6.8 hard gate "every `->` reaches the
tape" but does **not** block AU completion — the parity test
suite documents the gap and the grammar-roundtrip / tape-parity
fixtures still pass. Routes to **W7** as a focused codegen fix in
`crates/core/src/backend/rust/emitter/grammar.rs` (the alt-lit
emission path that constructs the per-branch match arms).

### Bug 2 — Span-annotated typed leaves lower to push_compound

`-> Span` (BBNF shorthand) and `-> input : Span` (Sheets,
`identifier`/`cell_ref`) reach the IR as
`TypeDesc::Named("Span")`. The IR-side whitelist
(`TypeDesc::from_scalar_name` / `is_type_name`) admits "Span" for
the Alt-bodied dispatch path but the rule-body emitter does not
route the resulting `__has_payload` setup through the Span case
of `PayloadData::Aggregate(8 bytes)` /
`PayloadData::WideScalar(...)`.

Consequence: every BBNF token rule (`identifier`, `literal`,
`regex`, `big_comment`, `comment`, `string_lit`) and every Sheets
`-> input : Span` rule (`string`, `cell_ref`, `identifier`)
emits `push_compound(Rule, ...)` with `has_children=false` —
neither a structural compound nor a typed leaf. The view layer
falls back to `cursor.span()` for these rules, which **does**
recover the source text, so the rule remains usable; only the
typed-tape projection is missing.

Similarly `int_lit -> i64` and `float_lit -> f64` in BBNF route
through `__value_atom` whose `__payload_tag` dispatch is dead
code: the inner `__int_lit` / `__float_lit` rules each declare
`__has_payload = false` and never overwrite it. The alt prelude
sets up `__payload_i64`/`__payload_f64` locals, but the inner
scanner (`scan_hex_mut`/`scan_digits_mut`) advances `state.offset`
without producing a parsed value.

**Severity**: blocks the strict reading of "every `->` reaches the
tape" but does **not** block AU completion. The view layer's
`text()` / `span_text()` fallback covers reader needs. Routes to
**AV** because the fix requires extending the Rust emitter's IR
lowering for `TypeDesc::Named("Span")` and threading the f64/i64
return values from `parse_that` numeric scanners back into the
push site — non-trivial wiring across `emitter/grammar.rs`,
`ir/passes/types/`, and `parse_that/.../number_*.rs`.

## Per-grammar firing tables

Each table maps every `->` annotation in the grammar family to its
firing status as observed in the W5 codegen.

### JSON — `grammar/json/json.bbnf`

| Rule | Declared type | Fires (push_leaf_with) | Notes |
|------|---------------|------------------------|-------|
| `null = "null" -> 0u8` | u8 | YES | InlineScalar via 1-byte aggregate |
| `bool = "true" -> true \| "false" -> false` | bool | PARTIAL | Only `false` branch fires (alt-payload gap) |
| `number = /…/ -> f64` | f64 | YES | WideScalar via 8-byte aggregate (`scan_number_strict_f64`) |
| `string = /…/ -> decode_json_string_to_arena(input) : String` | String | YES | `push_leaf_with_arena_frame` after decode kernel |

JSON: 4/4 annotations route to emitter; bool's `true` branch
loses the payload write under the alt-payload gap.

Total `push_leaf_with` sites in `expand-json.txt`: **8**.

### CSS L4 — `grammar/css/l4/*.bbnf`

| Rule | Declared type | Fires | Notes |
|------|---------------|-------|-------|
| `number = /…/` (no annotation, W3-E held back) | (none) | n/a | Scan-only; `-> f64` restoration deferred to W7 |
| `integer -> i64` | i64 | YES | WideScalar |
| `absoluteLengthUnit = "px" -> 0u8 \| …` (7) | u8 | PARTIAL | Only one branch fires |
| `viewportLengthUnit` (24) | u8 | PARTIAL | Only one branch fires |
| `containerLengthUnit` (6) | u8 | PARTIAL | Only one branch fires |
| `fontLengthUnit` (12) | u8 | PARTIAL | Only one branch fires |
| `angleUnit` (4) | u8 | PARTIAL | Only `grad -> 2u8` fires |
| `timeUnit` (2) | u8 | PARTIAL | Only one branch fires |
| `frequencyUnit` (2) | u8 | PARTIAL | Only one branch fires |
| `resolutionUnit` (4) | u8 | PARTIAL | Only one branch fires |
| `flexUnit = "fr" -> 0u8` | u8 | YES | Single branch; full reach |
| `percentageUnit = "%" -> 255u8` | u8 | YES | Single branch; full reach |
| `length`, `angle`, `time`, `frequency`, `resolution`, `flex`, `percentage` | (Span × Nu8) aggregate | PARTIAL | KvPair aggregates fire when the unit branch happens to be the firing one |
| `namedColor = "aliceblue" -> 0xF0F8FFFFu32 \| …` (148) | u32 | PARTIAL | 113/148 branches fire (factor-pass byte-dispatch loses ~35) |
| `hex = "#" , /…/ -> parse_hex_color(input) : u32` | u32 | YES | KvPair 4-byte aggregate via host fn |
| `colorType` (8) | u8 | PARTIAL | Alt-payload gap |
| `colorSpace` (9) | u8 | PARTIAL | Alt-payload gap |
| `mixSpace` (14) | u8 | PARTIAL | Alt-payload gap |
| `hueMethodKeyword` (4) | u8 | PARTIAL | Alt-payload gap |
| `radialShape`, `radialExtent`, `linearSide` | u8 | PARTIAL | Alt-payload gap |
| `mediaType` (3+) | u8 | PARTIAL | Alt-payload gap |
| `filterName` (5) | u8 | PARTIAL | Alt-payload gap |
| `mathFunctionName` (3+) | u8 | PARTIAL | Alt-payload gap |
| `mathProductOp` (2) | u8 | PARTIAL | Alt-payload gap |
| `mathOperator` (4) | u8 | PARTIAL | Alt-payload gap |
| `anPlusB` (2) | u8 | PARTIAL | Alt-payload gap |
| `colorProps` (60+), `listTableProps` (5+) | u8 | PARTIAL | Alt-payload gap |
| All `*Keyword` rules (positionKeyword, overflowKeyword, …) | u8 | PARTIAL | Alt-payload gap |

Total `push_leaf_with` sites in `expand-css.txt`: **43**
(12 InlineScalar for `namedColor`/`hex`-style; 31 Aggregate for unit/keyword paths).
Total `PayloadData::Aggregate` writes are 31 across the entire
grammar — far less than the ~600+ alt branches in the grammar.

### BBNF — `grammar/bbnf/*.bbnf` (via `BbnfBootstrap`)

| Rule | Declared type | Fires | Notes |
|------|---------------|-------|-------|
| `identifier = /…/ -> Span` | Span | NO | Bug 2: `-> Span` lowers to push_compound |
| `literal = ( … alts … ) -> Span` | Span | NO | Bug 2 |
| `regex = ( "/", /…/, "/" ) -> Span` | Span | NO | Bug 2 |
| `big_comment = ( "/*", /…/, "*/" ) ?w -> Span` | Span | NO | Bug 2 |
| `comment = ( "//", /.*/ ) ?w -> Span` | Span | NO | Bug 2 |
| `int_lit = /…/ -> i64` | i64 | NO | Bug 2: scanner returns Span; i64 conversion not threaded |
| `float_lit = /…/ -> f64` | f64 | NO | Bug 2: scanner returns Span; f64 conversion not threaded |
| `string_lit = ( "\"", /…/, "\"" ) -> Span` | Span | NO | Bug 2 |

Total `push_leaf_with` sites in BBNF generated.rs: **4** — and
those are inside `__int_lit` / `__float_lit` / `__value_atom`,
guarded by `if __has_payload` which is never set to true. So the
runtime firing count is **0**.

The W3-Bug 1 (typed-emitter) work landed the prelude scaffolding
(`__aggregate_buf`, `__has_payload`, `__payload_tag`) but the
inner scanner-to-payload wiring was deferred. This is the single
largest gap in the BBNF family.

### Google Sheets — `grammar/google-sheets/google-sheets.bbnf`

| Rule | Declared type | Fires | Notes |
|------|---------------|-------|-------|
| `number = /…/ -> f64` | f64 | NO | Bug 2: regex match returns Span; f64 not threaded |
| `string = /…/ -> input : Span` | Span | NO | Bug 2 |
| `boolean = /TRUE/ -> true \| /FALSE/ -> false` | bool | PARTIAL | Bug 1: only first branch (`true`) fires |
| `error_literal` (9) | u8 | PARTIAL | Bug 1: only first branch (`#N/A` -> 0u8) fires |
| `sheet_prefix` (2) | u8 | PARTIAL | Bug 1 |
| `cell_ref = /…/ -> input : Span` | Span | NO | Bug 2 |
| `identifier = /…/ -> input : Span` | Span | NO | Bug 2 |
| `compare_op` (6) | u8 | PARTIAL | Bug 1 |
| `add_op` (2) | u8 | PARTIAL | Only `+` -> 0u8 fires; `-` -> 1u8 dropped |
| `mul_op` (2) | u8 | PARTIAL | Only `*` -> 0u8 fires; `/` -> 1u8 dropped |
| `unary_prefix` (2) | u8 | PARTIAL | Only `+` -> 0u8 fires; `-` -> 1u8 dropped |

Total `push_leaf_with` sites in `expand-sheets.txt`: **9** — one
per Alt-lit rule whose first branch carries the payload write.

## Severity routing

| Gap | Severity | Routing |
|-----|----------|---------|
| Bug 1 — Alt-payload first-branch loss | Hard-gate-blocking | **W7** — `crates/core/src/backend/rust/emitter/grammar.rs` per-alt-branch payload-write emission |
| Bug 2 — `-> Span` shorthand lowers to compound | Hard-gate-blocking | **W7** — same emitter path; admits `TypeDesc::Named("Span")` to the leaf-payload route |
| Bug 2b — `-> i64`/`-> f64` scanner-to-payload wiring | Substrate-scale | **AV** — requires `parse_that::scan_*_mut` returning the parsed scalar AND emitter threading it into the alt prelude |
| Named-color factor-pass payload loss (~35/148) | Hard-gate-blocking | **W7** — IR-pass change to preserve payload writes through the byte-dispatch factorisation |

## What the parity tests actually pin

Each test file contains two categories of tests:

- **Parse-reach tests**: every annotated rule must parse its
  representative inputs cleanly. These guard against grammar
  regressions where a rule stops accepting valid inputs.
- **Payload-firing tests**: rules whose payload DOES reach the
  tape are asserted exactly. Rules whose payload does NOT reach
  the tape (under the alt-payload gap) are pinned with
  `assert_eq!(count, 0)` and a comment that the assertion should
  flip to `>= N` once the codegen fix lands. These act as the
  visible-delta guard for the W7 / AV fixes.

The pinned-zero tests are explicitly named `pinned_*` so the W7
agent can find them quickly:

- BBNF: `pinned_int_lit_drops_payload`,
  `pinned_float_lit_drops_payload`,
  `pinned_identifier_drops_payload`,
  `pinned_literal_drops_payload`,
  `pinned_regex_drops_payload`,
  `pinned_comment_drops_payload`,
  `pinned_big_comment_drops_payload`
- JSON: `bool_true_branch_currently_drops_payload`
- Sheets: `pinned_add_op_minus_branch_drops_payload`,
  `pinned_mul_op_div_branch_drops_payload`,
  `pinned_number_drops_f64_payload`

## Recommendations for AV (and FINAL.md)

1. **Bug 1 fix is small and surgical**: the Alt-lit emitter must
   wrap each branch's match-arm with the same payload-write
   block, not just the first. The conditional that today gates
   the write should be hoisted out of the per-branch closure.
2. **Bug 2 fix requires `TypeDesc::Named("Span")` admission to the
   leaf-payload route**: the symmetric whitelist landed in W2.B
   but only for KvPair aggregate paths. Extending it to the bare-
   Span path closes the BBNF / Sheets gap.
3. **Number-scanner threading** (i64 / f64) requires
   `parse_that::scan_*_mut` to return `Option<f64>` /
   `Option<i64>` instead of `Option<()>`. This is best done as
   AV substrate work alongside the columnar pivot since both
   touch the same scanner-API surface.
4. **The `kind=Rule, has_children=false, has_payload=true` quirk
   discovered during this audit** (empty-compound representation
   spuriously reports has_payload=true because child_off=0x0 is
   not the NONE sentinel) is a `bbnf-tape` API consistency bug.
   Fix candidate: `push_compound` writes
   `child_off=TapeOffset::NONE` when the children-run is empty,
   instead of leaving the marked offset. Routes to AV since it
   shifts the tape semantics across every reader.

## Audit artefact pointers

- `/tmp/w6c/expand-json.txt` (4482 lines), `/tmp/w6c/expand-css.txt`
  (162437 lines), `/tmp/w6c/expand-sheets.txt` (17271 lines):
  full `cargo expand` output for each bench.
- `crates/core/src/grammar/generated.rs`: the BBNF bootstrap
  parser (no separate cargo expand needed; the file IS the
  bootstrap source).
- Per-grammar PayloadData counts (W5 substrate baseline):
  - JSON: 8 `push_leaf_with` sites; 4 declared rules; alt-payload
    gap on `bool` only.
  - CSS L4: 43 `push_leaf_with` sites; ~600+ declared alt
    branches; alt-payload gap pervasive.
  - BBNF: 4 sites in source, 0 firing at runtime.
  - Sheets: 9 sites; 11+ declared rules; alt-payload gap
    pervasive on multi-branch rules.
- Test files committed alongside this audit:
  `crates/core/tests/{json,css_l4,bbnf,sheets}_parity.rs`.
