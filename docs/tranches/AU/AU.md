# Tranche AU — Projection Activation, Regression Redress, Scanner Truth

## Current state (post-AT audit)

AT landed 12 commits across 7 phases. Multi-type payload projection
implemented, SIMD guard fixed, meta_idx folded into TapeRec, CSS spec
gaps closed, structural tests added. However:

### AT landed correctly

| Feature | Status | Evidence |
|---------|--------|----------|
| `resolve_branch_type` | **WORKING** | Walks inlined Map/Constant/FnDescriptor nodes |
| Multi-type `__payload_tag` | **WORKING** | JSON value declares f64+bool+u8, match dispatch codegen |
| SIMD guard (integer digits) | **WORKING** | parse-that commit 44ae43b |
| meta_idx → kind_meta packing | **WORKING** | 5-bit meta, meta Vec eliminated, 28 tape tests |
| CSS `\|=` attr selector | **WORKING** | `attrName` rule with 4-branch disambiguation |
| CSS Unicode identifiers | **WORKING** | `\x80-\xff` byte ranges in ident regexes |
| `\xHH` hex escape in bbnf-regex | **WORKING** | character class byte-range support |
| Structural tests | **WORKING** | 17 deep-walk tests, bench validation helpers |
| Dead code cleanup | **DONE** | -166 LOC, 10 warnings eliminated |
| Tape parity fixtures | **DONE** | 22/22 pass |

### What AT broke or failed to activate

#### Critical bug 1 — `branch_pushes_children()` misclassifies leaf branches

**File**: `crates/core/src/backend/driver/alt.rs:67-69`

```rust
// Seq, Alt, Repeat, Skip, Next, Minus, TokenDispatch — all
// structurally push children.
_ => true,
```

After rule fusing, JSON's `bool` rule becomes a nested
`Alt([Map(Literal("true"), ...), Map(Literal("false"), ...)])` inside
`value`'s body. The `_ => true` catch-all classifies this nested Alt
as compound, forcing `mark_children + push_compound`. Since tape
surgery propagates compound classification to ALL branches of the
outer Alt, every branch (including number, string, null) gets
`__has_children = true`.

**Consequence**: ALL payload captures in `__value` are dead stores.
The epilogue always takes `push_compound`. `push_leaf_with_f64`,
`push_leaf_with_bool`, `push_leaf_with_u8` are never called. Typed
payloads (f64 for numbers, bool for true/false, u8 for null) are
computed but never stored — a **correctness bug**.

**Performance impact**: 18% regression on canada.json (1483 vs 1796).
Sources: 7 extra stack locals per `__value` call (28 bytes including
f64), dead epilogue code bloating icache, push_compound overhead vs
push_leaf for leaf branches, payload Vec pre-alloc (2.1MB) allocated
and freed unused.

#### Critical failure 2 — KvPair is dead infrastructure

`TapeKind::KvPair`, `is_kv_pair_shape`, `emit_tape_span_only_aggregate_epilogue`
with `kv_pair: bool`, view accessors — all exist. None fire for any
grammar. JSON `pair` emits `push_compound` with `TapeKind::Rule`.
The type projection doesn't produce the `Tuple([Span, scalar])` shape
that triggers KvPair detection for the `pair` rule.

#### Critical failure 3 — CSS has zero fused scanners

Despite the architecture supporting fused scanners (`css_ident_fast`,
`css_number_scan_f64`, `css_ws_comment_fast`, `css_string_fast`),
the expanded CSS L4 parser uses NONE of them. All 319 whitespace
scans use the generic `scan_ws_block_comments`. All 8 ident scans
use `DEFAULT_IDENT_CONFIG` instead of `CSS_IDENT_CONFIG` (7 of 8).
All 20 number scans compute f64 via Eisel-Lemire and discard it.

#### Critical failure 4 — CSS HexConvert mapping is broken

`hex = "#" , /[0-9a-fA-F]{3,8}/ -> crate::css_types::parse_hex_color(input) : u32`
declares a HexConvert mapping, but the expanded parser emits
`push_compound` with no payload. The `parse_hex_color` function
(defined in the bench) is never called. HexConvert codegen
does not reach the tape-first emitter path.

#### Failure 5 — Payload Vec pre-alloc wastes 2.1MB per JSON parse

`builder.rs:69-74` pre-allocates `expected / 4 * 8` bytes for payloads.
For canada.json this is 2.1MB. Since all payload captures are dead
stores (bug #1), zero bytes are written. The 2.1MB is malloc'd,
zeroed, and freed unused — ~0.10-0.15ms per parse (~10% of baseline).

### Deferred items accumulated across AR+AS+AT (must not defer again)

| Item | Origin | Tranches deferred |
|------|--------|-------------------|
| ParsedGrammar elimination | AR.7.2 | **11 tranches** |
| StructRegistry population | AS.2.3 / AT.6.1 | 2 tranches |
| Named struct view codegen | AT.6.2 | 1 tranche |
| String decode codegen wiring | AT.3.3-3.5 | 1 tranche |
| 64-byte input padding | AR.5.2 | 3 tranches |
| NEON fractional scan | AR.8.1 | 3 tranches |
| Fresh samply profiles | AT.4.1 | 1 tranche |

### Pre-AU profiling data (all 4 grammars)

**JSON parse (MB/s)**:

| Dataset | AQ | AT | Delta | vs sonic-rs |
|---------|-----|------|-------|-------------|
| canada | 1796 | 1483 | **-17%** | 99.9% |
| citm | 2698 | 2661 | **-1%** | 86% |
| twitter | 2086 | 2193 | **+5%** | 83% |
| data | 1939 | 1944 | **0%** | 80% |
| data_xl | 1348 | 1228 | **-9%** | 83% |

**CSS L4 parse (MB/s)**:

| Dataset | AT | vs cssparser | vs lightningcss |
|---------|----|-------------|-----------------|
| normalize | 999 | 1.4x | 3.6x |
| bootstrap | 513 | 1.1x | 4.0x |
| tailwind | 579 | 1.3x | 6.0x |

**Google Sheets parse (MB/s)**: simple 90, nested 123, stress 115

**BBNF self-hosting parse (MB/s)**: ebnf 213, json 262, css_pretty 583,
sheets 552, bbnf_self 379, css_l4_grammar 425

**Compile pipeline (ms)**: json 0.13, ebnf 0.39, bbnf 1.61, sheets 2.14,
css_l4 9.98

**Codegen fingerprint (push counts)**:

| Grammar | compound | leaf | leaf_with_* | Compound% | .map(\|_\| ()) |
|---------|----------|------|-------------|-----------|-----------------|
| JSON | 8 | 1 | 3 (dead) | 67% | 9 |
| CSS L4 | 234 | 22 | 7 | 89% | 206 (20 f64 discards) |
| Sheets | 37 | 0 | 0 | 100% | 50 |
| BBNF | 90 | 15 | 0 | 86% | 106 |

## AU plan — 5 phases

### Phase 1 — Fix projection activation (the tape surgery conflict)

The payload system and the tape surgery system conflict. Tape surgery
forces `mark_children + push_compound` on every branch. The payload
system expects `push_leaf_with_<T>` for scalar branches. Both are
active on the same Alt-bodied rule, but tape surgery wins because
`branch_pushes_children` over-classifies.

#### AU.1.1 Fix `branch_pushes_children` for nested leaf structures

In `crates/core/src/backend/driver/alt.rs:39-70`, the `_ => true`
catch-all misclassifies nested Alts whose branches are all leaves.
Fix: recurse into `Alt`, `Seq` (all-leaf Seq is also leaf-like),
and handle the inlined shapes correctly:

```rust
IrNode::Alt(branches, _) => branches.iter().any(|b| branch_pushes_children(ir, &b.node)),
IrNode::Seq(children) => children.iter().any(|c| branch_pushes_children(ir, c)),
```

Hard gate: JSON `__value` number/bool/null branches emit
`push_leaf_with_f64`/`push_leaf_with_bool`/`push_leaf_with_u8`
respectively. String branch emits `push_leaf`. Object/array branches
emit `push_compound`. Verified via `cargo expand`.

#### AU.1.2 Verify payload correctness end-to-end

After fixing branch classification, write a test that:
1. Parses `{"n": 42.5, "b": true, "z": null}` with the JSON parser
2. Walks the tape to find the number record
3. Reads `payload_f64()` from the record and asserts it equals `42.5`
4. Reads `payload_bool()` from the bool record and asserts `true`
5. Reads `payload_u8()` from the null record and asserts `0`

This proves the direct-to-struct projection path works end-to-end.

Hard gate: the test passes. Typed payloads are stored in the tape
and readable from the view layer.

#### AU.1.3 Revert payload Vec pre-alloc to lazy growth

Change `builder.rs:69-74` from `Vec::with_capacity(expected / 4 * 8)`
to `Vec::new()`. The payloads Vec grows lazily only when a rule
actually writes a payload. For JSON, leaf branches (number/bool/null)
now use `push_leaf_with_*` which allocates payload slots on demand.
Compound branches (object/array) never touch the payloads Vec.

Hard gate: `Vec::with_capacity` does not appear for payloads in
TapeBuilder.

#### AU.1.4 Bench: JSON must exceed AQ baseline

| Dataset | AQ | AU target |
|---------|-----|-----------|
| canada | 1796 | **≥ 1800** |
| citm | 2698 | **≥ 2700** |
| twitter | 2086 | **≥ 2100** |

Rationale: with leaf branches using `push_leaf_with_*` instead of
`push_compound`, we save 4 ops per leaf push AND capture the payload.
The f64 computation was always running (Eisel-Lemire); we now KEEP
the result instead of discarding it. This should be net-positive
because `push_leaf` is cheaper than `push_compound`.

### Phase 2 — CSS scanner activation

The CSS L4 parser uses zero fused scanners despite the architecture
supporting them. 319 whitespace scans, 20 f64 discards, 7/8 wrong
ident config. The CSS bench is 4-6x over lightningcss but should be
higher with proper scanner routing.

#### AU.2.1 Activate `css_ws_comment_fast` for `@ws` directive

The CSS grammar uses `@ws /(?s)(?:\s|\/\*.*?\*\/)*/ ;` which should
route to `css_ws_comment_fast` (SIMD-accelerated). Verify the
`scanner_plan.rs` routing for `WhitespaceWithBlockComment` and fix
if broken. 319 call sites will benefit.

#### AU.2.2 Fix ident config routing

7 of 8 `scan_ident` calls use `DEFAULT_IDENT_CONFIG` instead of
`CSS_IDENT_CONFIG`. The CSS ident supports leading dash and double-dash
prefix. The scanner_plan.rs routing must detect the CSS ident regex
shape and select the CSS config.

#### AU.2.3 Add `-> f64` to CSS number rule

The CSS `number` rule in `value-unit.bbnf` lacks a `-> f64` mapping.
Adding it activates payload capture for the 20 `scan_number_f64` call
sites. The f64 is already computed by the scanner — the mapping just
keeps the result.

#### AU.2.4 Fix HexConvert codegen for tape-first path

The `hex` rule's `-> crate::css_types::parse_hex_color(input) : u32`
HexConvert mapping does not reach the tape-first emitter. The
`emit_hex_convert` method in map_value.rs likely only handles the
slab/combinator path. Wire it through the tape emitter.

Hard gate: CSS L4 bootstrap ≥ 600 MB/s (vs 513 currently).

### Phase 3 — String decode + honest JSON bench

#### AU.3.1 Wire `decode_json_string_to_arena` through codegen

The decode function exists in parse-that. Wire it through
`scanner_plan.rs` as a new `SharedScanner::JsonStringDecode` variant.
The kernel calls `decode_json_string_to_arena` and stores the
`StringPayload` in the tape via `push_leaf_with_string` (new method).

#### AU.3.2 Build `json_monolithic_value` bench

Bench target that parses with string decode enabled, walks the tape
to verify values, and compares directly against `sonic_rs::from_str`.

Hard gate: `json_monolithic_value` bench produces numbers directly
comparable to sonic-rs. twitter.json ≥ 1800 MB/s on the value path.

### Phase 4 — Accumulated debt elimination

#### AU.4.1 ParsedGrammar elimination

11-tranche deferral. The bootstrap loop is closed. `host.rs` extracts
`ParsedGrammar` from the tape-first bootstrap parser, then the pipeline
lowers it to IR. Eliminate the intermediate: produce IR directly from
the bootstrap tape. Delete `ParsedGrammar` from `types.rs` and all
consumers. ~600 LOC removal.

Hard gate: `ParsedGrammar` does not exist in the codebase.

#### AU.4.2 StructRegistry: populate or delete

Either populate `ir.struct_registry` from `project_types` for struct-
shaped rules, or delete the scaffold entirely. Dead infrastructure is
worse than absent infrastructure.

#### AU.4.3 Module-level `#[allow]` for generated.rs

Replace 280 per-item `#[allow]` attributes with 1 module-level
attribute. 5-minute fix.

#### AU.4.4 Delete schema emitter stubs

`schema/emit/ts.rs` (returns `String::new()`) and
`schema/emit/runtime.rs` (returns `Vec::new()`) have been stubs for
10+ tranches. Delete them.

### Phase 5 — Profile-driven optimization + bench parity

#### AU.5.1 Fresh samply profiles (all grammars)

Profile JSON (canada, citm), CSS L4 (bootstrap, tailwind), and
Sheets (stress) with samply. Compare against AU.1 bench results.
Identify any remaining hot spots.

#### AU.5.2 Full bench suite with regression checks

Run all 6 bench suites. Record results in `PROGRESS.md`. Verify
no regression vs AQ on any grammar.

## Hard gates summary

1. JSON `__value` number branch emits `push_leaf_with_f64` (Phase 1)
2. JSON `__value` bool branch emits `push_leaf_with_bool` (Phase 1)
3. Payload correctness test: `payload_f64()` returns exact value (Phase 1)
4. JSON canada ≥ 1800 MB/s (Phase 1)
5. JSON citm ≥ 2700 MB/s (Phase 1)
6. CSS L4 bootstrap ≥ 600 MB/s (Phase 2)
7. `css_ws_comment_fast` in expanded CSS parser (Phase 2)
8. `json_monolithic_value` bench directly comparable to sonic-rs (Phase 3)
9. `ParsedGrammar` does not exist in codebase (Phase 4)
10. Fresh samply profiles with delta vs AQ baseline (Phase 5)
11. `cargo test --workspace` no new failures (all phases)

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/backend/driver/alt.rs` | 1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1 |
| `crates/core/src/backend/rust/emitter/map_value.rs` | 1, 2 |
| `crates/bbnf-tape/src/builder.rs` | 1 |
| `crates/core/src/generate/regex/emit/scanner_plan.rs` | 2 |
| `crates/core/src/backend/kernels/comment_ws.rs` | 2 |
| `crates/core/src/backend/kernels/identifier.rs` | 2 |
| `grammar/css/l4/value-unit.bbnf` | 2 |
| `parse-that/rust/parse_that/src/parsers/scan/decode.rs` | 3 |
| `crates/core/src/types.rs` | 4 |
| `crates/core/src/grammar/host.rs` | 4 |
| `crates/ir/src/types/grammar.rs` | 4 |

## Operational directives

See `/INSTRUCTIONS.md` at the repo root. Progress tracked in
`PROGRESS.md` alongside this document.
