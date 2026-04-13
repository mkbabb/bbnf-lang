# Tranche AT — Projection Truth, Regression Redress, Bench Parity

## Current state (post-AS audit)

AS landed 13 commits across 5 phases. CSS L4 parse activated,
TypeDesc::Span admitted, hand-rolled regex parsers replaced, escape
identifier scanner built, generic number kernel added.

### What AS landed correctly

| Feature | Status | Evidence |
|---------|--------|----------|
| CSS L4 parse | **WORKING** | normalize+bootstrap+tailwind parse, 525-974 MB/s |
| Identifier sub-flag dispatch | **WIRED** | 3 arms in scanner_plan.rs (plain, CSS, escape) |
| scan_ident_with_escapes | **WIRED** | CSS_IDENT_ESCAPE_CONFIG + kernel emit |
| Hand-rolled parser removal | **DONE** | -263 LOC in generate/regex/emit/ |
| Cached classify_regex | **WIRED** | leaves.rs + cost_model.rs use EmitOpts |
| Generic number kernel | **WIRED** | CSS Numeric { reject_leading_zero: false } routes correctly |
| StructRegistry scaffold | **STRUCTURAL** | HashMap field on GrammarIR + layout.rs handles Named |
| TypeDesc::Span admitted | **PARTIAL** | is_scalar_payload includes Span, builder+tape methods exist |

### What AS did NOT land or landed incorrectly

#### Critical failure 1 — f64 payload path does not fire for JSON

JSON `number = /.../ -> f64` runs `scan_number_strict_f64` via
Eisel-Lemire but the result is discarded with `.map(|_| ())`.
The number is stored as a compound Rule record, not a scalar leaf
with f64 payload. The `push_leaf_with_f64` method exists but the
emitter's payload_type routing never surfaces `TypeDesc::F64` for
the inlined number rule.

Root cause: `has_scalar_payload_type` / the `needs_payload_slot()`
gating in `emitter/mod.rs:426` looks up the rule's projected type,
but the `number` rule is inlined into `value`'s Alt body. After
inlining, the `Map(Regex, NumberConvert)` node sits inside the
`value` rule's body — not as a standalone rule with its own
`TypeDesc::F64` entry in `ir.types`.

Expected impact of fix: citm +300-500 MB/s, canada +200-400 MB/s.
The Eisel-Lemire computation already runs; wiring the result into
the payload slot is the ONLY change needed.

#### Critical failure 2 — bool payload path does not fire

Same issue as f64. `bool = "true" -> true | "false" -> false`
should produce `TypeDesc::Bool` but the bool rule is also inlined.
No `push_leaf_with_bool` appears in the expanded JSON parser.

#### Critical failure 3 — KvPair never emits for JSON

KvPair detection (`is_kv_pair_shape`) exists in layout.rs. The
`pair` rule is `string, colon >> value`. After type projection,
pair should be `Tuple([Span, <value_type>])`. When value resolves
to a scalar, this matches `[Span, scalar]` — the KvPair shape.
But the expanded parser shows `push_compound` for pair, not
`push_leaf_with_aggregate` + KvPair. The KvPair path isn't wired
into the emitter for JSON.

Expected impact: twitter +200-400 MB/s (13K pairs, most scalar).

#### Critical failure 4 — Span payload is admitted but never used

`push_leaf_with_Span` and `payload_Span` exist in builder/tape.
`is_scalar_payload()` includes Span. But no grammar produces a
rule where Span appears as an aggregate field that triggers the
payload path. The Span admission is dead infrastructure until
a grammar has `Tuple([Span, scalar])` where Span is a field.

#### Critical failure 5 — StructRegistry is always empty

`struct_registry: HashMap<StringId, Vec<TypeDesc>>` exists on
GrammarIR. But no pass populates it. `project_types` doesn't emit
Named types. The Named struct path in layout.rs is dead code.

#### Regression 1 — JSON throughput AQ→AR (-14% to -39%)

| Dataset | post-AQ | post-AS | Delta |
|---------|---------|---------|-------|
| canada | 1796 | 1089 | **-39%** |
| citm | 2698 | 2331 | **-14%** |
| twitter | 2086 | 2003 | **-4%** |
| data | 1939 | 1805 | **-7%** |
| data_xl | 1348 | 1046 | **-22%** |

The modifier fix did NOT change JSON's IR. The regression is from
AR codegen changes, primarily the `meta: Vec<u8>` side-channel
(commit `d9a760a`). Every push_leaf/push_compound writes an extra
byte to a separate cache line region. For canada (~500K records),
this adds measurable cache pressure.

#### Regression 2 — JSON bench is not apples-to-apples with sonic-rs

JSON bench produces `Parsed<JsonParser>` (tape of spans + some
payloads). sonic-rs produces `sonic_rs::Value` (materialized tree
with decoded strings). Strings are never decoded in our parser.
For fair comparison, JSON strings need direct projection during
parse: scan + decode escape sequences into an arena, store arena
offset in the payload slot. One pass, zero re-traversal.

### Deferred items accumulated across AR+AS (must not defer again)

1. 64-byte input padding (AR.5.2) — requires parse-that ownership
2. NEON 17-digit fractional scan (AR audit proposal 5)
3. Named struct ABI finalization
4. ParsedGrammar elimination (bootstrap loop is closed)
5. String decode kernel for JSON parity

## AT plan — 5 phases

### Phase 1 — Projection truth: wire all scalar paths (~2 days)

The payload infrastructure exists. The wiring doesn't fire. Fix
the codegen routing so every type annotation produces the
corresponding payload-bearing push call.

#### AT.1.1 Fix f64 payload for inlined Map(Regex, NumberConvert)

The `value` rule's Alt body contains inlined `Map(Regex(number_pat),
NumberConvert)` nodes. The emitter sees this as a branch of `value`,
not as a standalone `number` rule. The per-branch scalar detection
in `emitter/mod.rs:436-448` iterates Alt branches looking for
`Ref(rid)` with scalar types — but the `number` rule is inlined,
so there's no `Ref` to find.

Fix: extend the per-branch detection to recognize `Map(_, NumberConvert)`
directly, not just `Ref(scalar_rule)`. When the Map's FnDescriptor is
NumberConvert, the branch type is F64 regardless of whether the inner
node is a Ref or an inlined Regex.

Hard gate: `push_leaf_with_f64` appears in the expanded JSON parser.
`__has_payload = true` fires for number branches. `__payload_f64`
carries the scanned value.

#### AT.1.2 Fix bool payload for inlined Constant maps

Same pattern. `"true" -> true | "false" -> false` produces
`Map(Literal, Constant { value: "true", type: Bool })`. Extend
detection to recognize `Constant` → `TypeDesc::Bool`.

Hard gate: `push_leaf_with_bool` appears in expanded JSON parser.

#### AT.1.3 Wire KvPair for JSON pair rule

`pair = string, colon >> value` projects as `Tuple([Span, <value_type>])`.
When value resolves to a scalar, `is_kv_pair_shape` should match.
Verify the layout planner produces a KvPair layout for `pair`.
If not, trace the type projection to find where the pair type
is computed and why it doesn't match.

Hard gate: `TapeKind::KvPair` appears in expanded JSON parser for
`pair` branches where value is scalar.

#### AT.1.4 Verify all payload paths fire

Expand all 6 grammar parsers (JSON, CSS L4, BBNF, EBNF, CSS pretty,
Google Sheets). For each, count `push_leaf_with_*` calls and verify
they match the expected typed rules:
- JSON: f64 (number), bool (bool), u8 (null), KvPair (pair)
- CSS L4: u8 (unit enums), f64 (dimension numbers)
- Google Sheets: typed leaf payloads

### Phase 2 — Regression redress: meta_idx + capacity (~1 day)

#### AT.2.1 Fold meta_idx into TapeRec

Eliminate the parallel `meta: Vec<u8>` by packing meta_idx into
the TapeRec. Two options:

**Option A** (preferred): Split `kind: u8` into `kind: u4` +
`meta_idx: u4`. TapeKind uses values 0-15 (4 bits). meta_idx up
to 15 covers all JSON and most CSS L4 rules. For the CSS L4
`declaration` Alt (27 branches), store meta_idx >= 16 in the
`payload_idx` field (with a flag bit indicating "meta_idx overflow").

**Option B**: Use the `flags` byte. Currently uses 6 bits for
variant_idx + 2 flags. Reduce variant_idx to 5 bits (32 max),
freeing 1 bit. Combined with 4 bits from kind, that's 5 bits
for meta_idx (0-31). Covers CSS L4's 27-branch declaration Alt.

After folding: `Tape::meta: Vec<u8>` is deleted. `TapeCursor::meta_idx()`
reads from the packed TapeRec field. All `tape.meta.push(meta_idx)`
calls are replaced with writes to the packed field.

Hard gate: `meta: Vec<u8>` does not exist. `meta_idx()` reads from
TapeRec. `cargo test -p bbnf-tape` passes. JSON canada throughput
recovers to >= 1500 MB/s.

#### AT.2.2 Verify capacity heuristic is optimal

The current `len/2+2` should be correct for most inputs. Profile
with samply to confirm no reallocation on the critical path. Check
with `Vec::capacity()` instrumentation if desired.

### Phase 3 — String decode kernel + JSON bench parity (~3 days)

#### AT.3.1 Implement decode_json_string_to_arena in parse-that

New function in `parse-that/rust/parse_that/src/parsers/scan/`:
```
decode_json_string_to_arena(
    bytes: &[u8],       // input bytes (including quotes)
    arena: &mut Vec<u8>, // destination
) -> StringPayload
```

Where `StringPayload` is either:
- `Borrowed { start: u32, end: u32 }` — no escapes, reference into source
- `Owned { arena_offset: u32, len: u32 }` — decoded bytes in arena

Fast path (common case): `memchr(b'\\', content)` returns None →
`Borrowed`. No copying, no allocation.

Slow path: walk the string, decode `\"`, `\\`, `\/`, `\b`, `\f`,
`\n`, `\r`, `\t`, `\uXXXX` (with surrogate pairs) into UTF-8.
Append to arena. Return `Owned`.

Test with JSON test suite (valid.jsonl, invalid.jsonl in
`grammar/tests/json/`).

#### AT.3.2 Add push_leaf_with_string to TapeBuilder

New method that stores a `StringPayload` into the tape:
- Borrowed: pack (start, end) into one 8-byte payload slot
  (same as push_leaf_with_Span)
- Owned: append arena bytes to payloads, store (offset, len)
  into one 8-byte slot, set a flag bit to distinguish from Borrowed

The `payload_idx` field (u16) identifies the slot. A flag bit in
`flags` (or the high bit of the payload data) distinguishes
borrowed vs owned.

#### AT.3.3 Wire string decode through JSON codegen

New `SharedScanner::JsonStringDecode` variant in scanner_plan.rs.
Kernel: `kernels::quoted_string::emit_call_strict_decode()`.
Generated code calls `decode_json_string_to_arena(state, &mut tape.payloads)`
and stores the result via `push_leaf_with_string`.

Grammar change: `string` in json.bbnf gains a `-> String` mapping
or a `@decode` directive (TBD: which is more natural for BBNF).

#### AT.3.4 Add payload_string view accessor

`Tape::payload_string<'input>(&self, rec, input: &'input str) -> &'input str`

Reads the flag bit. For Borrowed, indexes into `input`. For Owned,
reads from `self.payloads` and returns a `&str` (the payloads Vec
outlives the view because both are owned by `Parsed`).

#### AT.3.5 Build honest JSON bench

New bench target `json_monolithic_value` that:
1. Parses with string decoding enabled
2. Walks the tape to verify all values are accessible
3. Compares against `sonic_rs::from_str::<sonic_rs::Value>(&input)`

The existing `json_monolithic` bench stays as-is (tape-only, for
comparison with cssparser-style span-only parsers).

Hard gate: `json_monolithic_value` produces throughput numbers that
are directly comparable to sonic-rs. String values are fully decoded.
twitter.json >= 1800 MB/s on the value path.

### Phase 4 — Profile-driven optimization (~2 days)

#### AT.4.1 Fresh samply profiles

Profile JSON (5 datasets) + CSS L4 (3 datasets) + Google Sheets.
Compare to AR-baseline profiles in `docs/benchmarks/profiles/`.

#### AT.4.2 Hot-path optimization pass

Profile-directed, not speculative. Likely targets:
- Whitespace scan overhead in `?w` paths (3 trims per JSON pair)
- Dispatch table branch misprediction on large Alts
- SIMD scan integration gaps (verify NEON/SSE paths fire)
- 64-byte input padding (eliminate SIMD tail bounds checks)

#### AT.4.3 NEON 17-digit fractional scan

The AR audit proposal 5. SWAR 8-digit chunks cover integer
mantissa; fractional digits go byte-by-byte. NEON vector scan
for the fractional part gives us an edge on Apple Silicon.

#### AT.4.4 Post-AT.json with complete results

Delta vs post-AS. Include both tape-only and value-materialization
numbers for JSON.

### Phase 5 — Named struct ABI + ParsedGrammar cleanup (~2 days)

#### AT.5.1 Populate StructRegistry from project_types

When project_types resolves a rule's type to a concrete set of
named fields, register it in `ir.struct_registry`. This bridges
the scaffold from AS.2.3 to actual use.

#### AT.5.2 Named struct view codegen

Extend the view layer to generate typed struct accessors for
Named types: field-by-name access, type-safe getters, the full
direct-to-struct chain.

#### AT.5.3 Dead code cleanup (AS audit)

Delete dead code identified in the AS code quality audit:
- `has_scalar_payload_type` in grammar.rs:80 (never called)
- `META_IDX_ZERO` in repeat.rs:26 (unused copy)
- Span arms in `emit_alt_mustape_prelude_epilogue` and
  `emit_alt_span_only_prelude_epilogue` — unreachable because
  `needs_payload_slot()` excludes Span upstream
- Stale doc comment in emitter_types.rs:79

#### AT.5.4 Regenerate tape_parity golden fixtures

11 of 22 tape_parity tests fail: `root_variant_idx` changed due to
AS enum reordering. The fixtures in `tests/fixtures/tape_golden/`
need regeneration. Not a functional regression — record counts
are correct.

#### AT.5.5 Commit parse-that changes

The AS tranche added `allow_escapes: bool` to `IdentConfig` and
`CSS_IDENT_ESCAPE_CONFIG` in parse-that's working tree. These
changes must be committed in the parse-that repo.

#### AT.5.6 ParsedGrammar elimination

The bootstrap loop is closed. `host.rs` extracts the grammar from
the tape-first bootstrap parser. `ParsedGrammar` (the old AST-based
representation) is now redundant. Delete it and route all consumers
through the IR path.

## Hard gates summary

1. `push_leaf_with_f64` appears in expanded JSON parser (Phase 1)
2. `push_leaf_with_bool` appears in expanded JSON parser (Phase 1)
3. `TapeKind::KvPair` appears in expanded JSON pair (Phase 1)
4. `meta: Vec<u8>` eliminated from Tape/TapeBuilder (Phase 2)
5. JSON canada >= 1500 MB/s (Phase 2)
6. `decode_json_string_to_arena` with test coverage (Phase 3)
7. `json_monolithic_value` bench directly comparable to sonic-rs (Phase 3)
8. Fresh samply profiles with delta vs AR-baseline (Phase 4)
9. StructRegistry populated by at least one grammar (Phase 5)
10. tape_parity: 22/22 pass (fixtures regenerated) (Phase 5)
11. Zero dead code in AS-changed files (Phase 5)
12. parse-that changes committed (Phase 5)
13. `cargo test --workspace` no new failures (all phases)

## Items already landed (from AS)

- CSS L4 parse activated: normalize 974, bootstrap 525, tailwind 569 MB/s
- Beats lightningcss 4-6x, cssparser 1.1-1.4x
- Identifier sub-flag dispatch: 3 scanner variants
- scan_ident_with_escapes: CSS selector escape sequences
- Hand-rolled regex parsers: -263 LOC
- Cached classify_regex: EmitOpts path
- TypeDesc::Span admitted with builder+tape+view
- StructRegistry field on GrammarIR
- Generic number kernel for CSS
- Bootstrap idempotent: gen1 == gen2, 25,008 lines

## CSS L4 audit findings (post-AS)

47/49 CSS edge cases pass. Two spec-compliance gaps:

1. **`|=` attribute matcher**: conflicts with namespace `|` prefix
   in `selectors.bbnf`. `wqName` consumes `lang|` as namespace
   before `attrMatcher` can match `|=`. Fix: factor `|=` ahead
   of bare `|` in `attrSelector`.

2. **ASCII-only identifiers**: `selectorIdent` and `ident` regexes
   only match ASCII letters. CSS Syntax L3 §4.3.10 allows non-ASCII.
   Fix: extend regex or use `\p{L}`.

### Benchmark fairness correction

The cssparser comparison is **misleading**: its bench has
`parse_declarations() -> false` — it tokenizes and counts top-level
rules, not building a typed AST. BBNF builds full typed declarations,
selectors, and values. The real comparison is BBNF vs lightningcss
(both build typed ASTs): bootstrap 525 vs 124 MB/s (**4.2x**).

## What is NOT in scope

- **Global CSP solve**: per-component sufficient.
- **WASM/TS backend updates**: Rust backend only for this tranche.
- **CSS pretty format quality**: gorgeous formatting is separate.
- **Language server features**: LSP is not on the critical path.
- **CSS spec gap fixes** (|= and Unicode idents): separate tranche.

## Operational directives

See `/INSTRUCTIONS.md` at the repo root. Progress tracked in
`PROGRESS.md` alongside this document.

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/backend/rust/emitter/mod.rs` | 1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1, 2 |
| `crates/core/src/backend/rust/emitter/map_value.rs` | 1 |
| `crates/core/src/backend/rust/emitter/tape_prelude.rs` | 1 |
| `crates/core/src/backend/rust/view/{alt,leaves,seq}.rs` | 1 |
| `crates/bbnf-tape/src/{tape,builder,cursor,kind}.rs` | 2 |
| `crates/ir/src/types/type_desc.rs` | 1 |
| `parse-that/rust/parse_that/src/parsers/scan/quoted.rs` | 3 |
| `parse-that/rust/parse_that/src/parsers/scan/mod.rs` | 3 |
| `crates/core/src/generate/regex/emit/scanner_plan.rs` | 3 |
| `crates/core/src/backend/kernels/quoted_string.rs` | 3 |
| `crates/ir/src/passes/types/mod.rs` | 5 |
| `crates/core/src/grammar/host.rs` | 5 |
| `docs/benchmarks/post-AT.json` | 4 (NEW) |
