# Tranche AQ — Generalization, TypeDesc-Driven Projection, Self-Hosting Closure

## Ground Truth (Post-AP Deep Audit, 6 parallel agents)

This tranche is informed by a 6-agent parallel audit documenting:
- Complete prior-tranche status (AA→AP): `docs/tranches/AQ-audit.md`
- JSON/CSS/BBNF/Sheets parse + compile profiling: `docs/benchmarks/profiles/post-AP/`
- Structural dispatch root-cause analysis
- sonic-rs architectural comparison
- Generalization debt inventory (5 nominal RegexClass variants, 7 sp_*/sp_*, 6 scan_json_*, hardcoded byte sets)

All findings are grounded in measured profile data and source reads.

---

## Measured Baseline (post-AP, aarch64-apple-darwin M4 Max)

### JSON Monolithic
| Dataset | MB/s | sonic-rs | Gap |
|---------|------|----------|-----|
| canada | 1,797 | 1,540 | **+17% BEAT** |
| citm | 2,712 | 3,000 | -10% |
| twitter | 2,173 | 2,643 | -18% |
| data | 1,900 | 2,346 | -19% |
| data_xl | 1,341 | 1,460 | -8% |

### CSS L4 Typed
| Dataset | MB/s |
|---------|------|
| normalize | 978 |
| bootstrap | 505 |
| tailwind | 534 |

### Hot Paths
- JSON citm: `__value` 56.4%, `__pair` 28.2%, WS scan 11.9%
- CSS L4 bootstrap: `__declaration` 33.2%, `__compoundSelector` 32.2%
- CSS L4 normalize: `__compoundSelector` 40.2% (does hand-rolled CSS ident parsing)

---

## Four Architectural Findings (Post-AP)

### 1. Self-hosting must close. Hand-patching must end.
`generated.rs` has been hand-patched across AE/AF/AG/AI/AM/AN/AP. Every tranche defers it. **AQ MUST end the deferral chain.** Two specific blockers must be resolved, not worked around:
- Schema emitter's `cst_directives` layout check under structural mode
- Heterogeneous Alt sub-variant coercion under structural mode

### 2. AL-prototype-2's "direct projection" fix is already implemented
AL proposed: unified ABI `(state, tape) -> Option<TapeOffset>` where a "Direct" rule emits `push_leaf` instead of `push_compound`. AL itself noted: *"This is exactly what `MaterializationClass::TapeSpanOnly` already generates!"* Nothing new to ship from AL. The OPEN question (typed Rust return values from parser) is addressed in Phase 4 below via TypeDesc-driven payload layout.

### 3. Structural pre-scan is no longer viable — DELETE, don't activate
Honest math post-AP (citm, 1.7MB):
- Parse time: 920µs
- WS self-time (post AP.3.1 SIMD bitmap): **11.9% = 110µs**
- Simdjson-class fused pre-scan cost: **~300µs** (SIMD scan 200µs + fused filter_quote_parity 100µs)
- Max savings from WS elision: 110µs
- **Net: -190µs regression (+20% slower)**

Pre-scan WAS viable pre-AP when WS was 50% of citm (~460µs > 300µs). AP.3.1's SIMD bitmap killed that savings opportunity by solving WS directly. Pre-scan infrastructure is now dead weight.

### 4. Current "direct projection" is NOT generalized — consume TypeDesc directly
`PayloadKind::F64/Bool/U8` is **redundant** with `TypeDesc::F64/Bool/U8`. The `project_types` CSP pass (op 16) already infers `TypeDesc` for every rule. The payload system should consume `TypeDesc` directly, not maintain a parallel nominal enum.

---

## Design Principles

1. **Type inference drives everything.** `TypeDesc` is the single source of truth for what a rule produces. Payload layout, view accessors, backend emission — all consume `TypeDesc`.
2. **No legacy cruft.** Every deferred item either ships or is deleted with rationale. Every nominal name replaced by structural. Every duplicate collapsed.
3. **Structural before nominal.** Regex classification parameterizes on shape. Scanner names describe what they scan.
4. **Delete what doesn't earn its keep.** Pre-scan, `PayloadKind`, `classify_known_pattern`, `scan_json_*` functions, CSS-nominal helpers.
5. **Self-hosting closure is non-negotiable.** Bootstrap regen or nothing.

---

## AQ Plan — 9 phases

Phases are ordered by priority within constraint dependencies. All are mandatory unless explicitly marked optional.

## Phase 1: Self-Hosting Closure (HIGHEST PRIORITY)

End the deferral chain from AE/AF/AG/AI/AM/AN/AP. No more hand-patching.

### AQ.1.1 Resolve the two structural-mode codegen deficits
1. **Schema emitter `cst_directives` layout check** — must handle wrapper preservation under structural mode. Fix in `crates/core/src/grammar/schema/` (the check currently assumes optimizer-produced Tuple layout).
2. **Heterogeneous Alt sub-variant coercion** — must run correctly under structural mode. Fix in sub-variant coercion logic.

### AQ.1.2 Regenerate `generated.rs` from scratch
Run bootstrap regen. `diff` against hand-patched version. All differences must be explainable as correct structural-mode output. No hand patches remain.

### AQ.1.3 Freeze `grammar_roundtrip` constants
`crates/core/tests/grammar_roundtrip.rs:47-52` — replace `usize::MAX` sentinels with exact rule counts. Remove `#[ignore]` attributes. Tests must pass.

### AQ.1.4 Delete legacy bridges
- Span-text fallback in `core/src/grammar/host.rs:206-423`
- All "AE-era" / "AF-era" compatibility shims
- All `// temporary` / `// TODO: remove after regen` comments and their associated code

**Files:**
- REGEN: `crates/core/src/grammar/generated.rs`
- MODIFY: schema emitter (`crates/core/src/grammar/schema/`)
- MODIFY: sub-variant coercion logic
- MODIFY: `crates/core/tests/grammar_roundtrip.rs`
- DELETE: `crates/core/src/grammar/host.rs` legacy bridge

**Hard gate:** `cargo test --test grammar_roundtrip` passes without `#[ignore]`.

## Phase 2: IR Inspection Module (Foundation)

Deduplication of 5 IR-walking primitives that are copy-pasted across recognizer miners and key_dispatch.

### AQ.2.1 Create `bbnf_ir::passes::inspect`
```
crates/ir/src/passes/inspect/
    mod.rs
    walk.rs      // visit_children_alt (moved from recognizers::mod)
    unwrap.rs    // unwrap_wrap, unwrap_map_ow
    resolve.rs   // resolve_to_seq, unwrap_to_alt, unwrap_to_repeat
    literal.rs   // single_byte_literal (unified from 3 duplicated copies)
    leading.rs   // extract_leading_literals, extract_leading_regex_pattern
```

### AQ.2.2 Migrate all consumers
- Recognizer miners (`delim_scan`, `balanced_wrap`, `separator_list`, `punct_ws_region`, `key_dispatch`) import from `passes::inspect`
- Delete in-file duplicates
- `csp_strategy/mod.rs` continues to work via re-export

**Files:**
- NEW: `crates/ir/src/passes/inspect/` (6 files)
- MODIFY: all files in `crates/ir/src/passes/recognizers/`

## Phase 3: Deoverfit RegexClass

5 of 13 `RegexClass` variants are nominal (language-named). Collapse to structural variants with parameters.

### AQ.3.1 Expand structural variants
```rust
pub enum RegexClass {
    Numeric {
        allows_sign: bool,
        allows_fraction: bool,
        allows_exponent: bool,
        reject_leading_zero: bool,       // NEW
        allow_leading_dot: bool,          // NEW
    },
    QuotedString {
        quote_char: u8,
        allows_escapes: bool,
        allows_u_escapes: bool,           // NEW — JSON strings with surrogate handling
    },
    Identifier {
        allows_leading_dash: bool,        // NEW — CSS -foo
        allows_double_dash_prefix: bool,  // NEW — CSS --var
    },
    HexDigits,
    WhitespaceWithBlockComment,           // RENAMED from WsBlockComment
    CharClassQuantified(ClassRangeInfo),
    PrefixThenClass { prefix, tail },
    AccelDriven(u8),
    Unknown,
}
```

### AQ.3.2 Delete nominal variants
Delete `JsonString`, `JsonNumber`, `CssIdent`, `CssQuotedString`.

### AQ.3.3 Delete `classify_known_pattern` dictionary
Delete `classify_known_pattern` function and all `*_PATTERNS` constants (`JSON_STRING_PATTERNS`, `JSON_NUMBER_PATTERNS`, `WS_BLOCK_COMMENT_PATTERNS`, `IDENT_PATTERNS`, `QUOTED_STRING_PATTERNS`).

### AQ.3.4 Update structural classifiers
`try_classify_numeric` / `try_classify_quoted_string` / `try_classify_identifier` populate new fields from HIR structure.

### AQ.3.5 Add `RegexClass::canonical_pattern(&self)` helper
Returns the canonical regex string for a parameterized variant. Eliminates IR types hardcoding pattern strings (e.g., `key_class_regex_pattern()` in recognizer_configs.rs).

### AQ.3.6 Migrate all ~60 consumers
Every `match RegexClass::JsonString` becomes `match RegexClass::QuotedString { allows_u_escapes: true, .. }` etc.

**Files:**
- MODIFY: `parse-that/rust/regex/src/classify/mod.rs`
- MODIFY: `parse-that/rust/regex/src/classify/structural.rs`
- MODIFY: `parse-that/rust/regex/src/info/mod.rs`
- MODIFY: ~20 consumer files in bbnf-ir and bbnf-core

## Phase 4: Deoverfit Scanners and Kernels

### AQ.4.1 Rename `scan_json_number_*` → `scan_number_strict_*`
Rename `JSON_NUMBER_CONFIG` → `STRICT_NUMBER_CONFIG`. "JSON-ness" is a NumberConfig value, not a function family.

### AQ.4.2 Move `quoted_string_scan_full` out of `parsers/json.rs`
Move to `scan/quoted.rs`, rename → `scan_quoted_string_strict`. Move `validate_json_escapes` → `validate_strict_escapes`.

### AQ.4.3 Delete language-prefixed SpanParser constructors
Delete from `parse-that/src/span_parser/constructors.rs:88-124`:
- `sp_json_number`, `sp_json_string`, `sp_json_string_quoted`
- `sp_css_ident`, `sp_css_ws_comment`, `sp_css_string`, `sp_css_block_comment`

Provide structural `sp_number(config)`, `sp_quoted_string(config)`, `sp_ident(config)`, `sp_ws_comment`, `sp_block_comment`.

### AQ.4.4 Rename `SpanScanner` enum variants
- `JsonNumber` → `NumberStrict`
- `JsonString` / `JsonStringQuoted` → `QuotedStringStrict{Content?}`
- `CssIdent` / `CssWsComment` / `CssString` / `CssBlockComment` → generic forms

### AQ.4.5 Rename `is_css_ws` → `is_ascii_ws_no_vtab`
In `parse-that/src/parsers/scan/ws_comment.rs:28`.

### AQ.4.6 Delete `scan_balanced_end` wrapper
In `parse-that/src/parsers/scan/balanced.rs:114`. Hardcodes CSS config. Callers build their own `BalancedScanConfig`.

### AQ.4.7 Parameterize `scan_ident`
Accept `IdentConfig { allow_leading_dash, allow_double_dash_prefix }`.

### AQ.4.8 Kernel renames + relaxed-config overloads
- `kernels::quoted_string::emit_json_call` → `emit_call_strict`
- `kernels::number::emit_call_span` / `emit_call_fused` — add relaxed-config overloads
- `kernels::identifier::emit_call` — add variants per config
- Drop "JSON" from `punct_ws_region.rs:1` module doc

### AQ.4.9 Parameterize `STRUCTURAL_PUNCTS`
`ir/src/passes/recognizers/punct_ws_region.rs:30` hardcodes `b",:{}[]"`. Derive from grammar-driven byte set.

**Files:**
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/{number,number_f64,ws_comment,ident,balanced,quoted}.rs`
- MODIFY: `parse-that/rust/parse_that/src/parsers/json.rs` (split quoted helpers out)
- MODIFY: `parse-that/rust/parse_that/src/span_parser/{constructors,span_scanner}.rs`
- MODIFY: `crates/core/src/backend/kernels/{quoted_string,number,identifier,punct_ws_region}.rs`
- MODIFY: `crates/ir/src/passes/recognizers/punct_ws_region.rs`

## Phase 5: DELETE Structural Dispatch Infrastructure

Per finding #3: structural pre-scan is no longer viable post-AP. AP.3.1's SIMD WS bitmap captured the savings directly. Pre-scan is now dead weight. **Delete entirely.**

### AQ.5.1 Delete parse-that structural scanners
- DELETE: `parse-that/rust/parse_that/src/parsers/scan/structural.rs`
- DELETE: `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs`
- Remove `scan_structural`, `filter_quote_parity`, `StructuralIter`, `advance_to_structural`, `sync_structural_cursor_to_offset`, `current_structural_byte` from `parse-that` public API

### AQ.5.2 Delete structural fields on ParserState
In `parse-that/rust/parse_that/src/state.rs`:
- DELETE: `structural_index: *const u32` field
- DELETE: `structural_len: u32` field
- DELETE: `structural_cursor: u32` field
- DELETE: all methods that reference them

### AQ.5.3 Delete IR pass
- DELETE: `crates/ir/src/passes/structural_bytes.rs`
- Remove `compute_structural_bytes` from pipeline call in `crates/core/src/pipeline/compile.rs`
- DELETE: `GrammarIR::structural_bytes` field

### AQ.5.4 Delete structural codegen
- Remove `structural_mode: bool` from `RustEmitter` (in `emitter_types.rs`)
- DELETE: structural pre-scan emission in `grammar.rs:668-683`
- DELETE: hybrid dispatch branch in `alt.rs:121-150`
- DELETE: structural-WS-elision branches in `ws.rs`
- Remove the `emitter.structural_mode = false;` line in `generate/mod.rs:61`

### AQ.5.5 Delete test fixtures
- Remove any structural-mode test fixtures or assertions
- Remove `structural_bytes` references from tests

**Rationale:** ~400 LOC removed. All savings were already captured by AP.3.1 (SIMD WS bitmap) and AP.3.3 (SIMD string scanner). Pre-scan added ~190µs regression on citm with no corresponding win.

**If future work demonstrates a viable use case (e.g., `LazyValue`-style on-demand parsing, large-input >10MB amortization), re-introduce with a clean design. Don't keep dead infrastructure "just in case."**

**Files:**
- DELETE: `parse-that/rust/parse_that/src/parsers/scan/structural.rs`
- DELETE: `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs`
- DELETE: `crates/ir/src/passes/structural_bytes.rs`
- MODIFY: `parse-that/rust/parse_that/src/state.rs` (delete 3 fields + methods)
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/mod.rs` (remove pub use)
- MODIFY: `parse-that/rust/parse_that/src/lib.rs` (remove re-exports)
- MODIFY: `crates/ir/src/types/grammar.rs` (remove structural_bytes field)
- MODIFY: `crates/core/src/pipeline/compile.rs` (remove pass call)
- MODIFY: `crates/core/src/generate/mod.rs` (remove flag)
- MODIFY: `crates/core/src/backend/rust/emitter_types.rs` (remove structural_mode)
- MODIFY: `crates/core/src/backend/rust/emitter/{grammar,alt,ws}.rs` (remove branches)

## Phase 6: TypeDesc-Driven Payload Projection (MANDATORY — all sub-phases)

Finding #4: `PayloadKind` is redundant with `TypeDesc`. Delete `PayloadKind`. Consume `TypeDesc` directly. Generalize to full integer suite. Aggregate payloads via layout planner. Alt-return typed enums.

### AQ.6.A — Full scalar TypeDesc expansion + PayloadKind deletion (MANDATORY)

#### AQ.6.A.1 Add full integer suite to TypeDesc
In `crates/ir/src/types/type_desc.rs`, add:
```rust
pub enum TypeDesc {
    // existing: Span, F64, Bool, U8, U32, Option, Vec, Tuple, BoxedEnum, Enum, Named
    I8,   // NEW
    I16,  // NEW
    U16,  // NEW
    I32,  // NEW
    I64,  // NEW
    U64,  // NEW
    // Existing F64 stays. F32 not added unless profile shows use.
}
```

Update `project_types` CSP (op 16) to infer these from:
- Map(Regex, NumberConvert) with explicit sign/range info → signed or unsigned integer
- Hex conversion → U32 (already)
- Map(Literal, Constant(i32/i64)) → signed int

#### AQ.6.A.2 Delete `PayloadKind` enum entirely
```
DELETE: crates/core/src/backend/rust/emitter_types.rs PayloadKind enum
DELETE: all `ctx.payload_kind: Option<PayloadKind>` references
```

Replace with: `ctx.payload_type: Option<&TypeDesc>`, read directly from `ir.types[rule_id]` at emission time.

#### AQ.6.A.3 Add TypeDesc payload-eligibility predicate
```rust
impl TypeDesc {
    /// Is this type storable inline in a fixed-size payload slot?
    pub fn is_scalar_payload(&self) -> bool {
        matches!(self,
            | TypeDesc::F64
            | TypeDesc::Bool
            | TypeDesc::I8  | TypeDesc::U8
            | TypeDesc::I16 | TypeDesc::U16
            | TypeDesc::I32 | TypeDesc::U32
            | TypeDesc::I64 | TypeDesc::U64)
    }

    /// Size in bytes; None for non-scalar types.
    pub fn payload_size_bytes(&self) -> Option<u8> {
        match self {
            F64 | I64 | U64 => Some(8),
            I32 | U32 => Some(4),
            I16 | U16 => Some(2),
            Bool | I8 | U8 => Some(1),
            _ => None,
        }
    }

    /// Alignment requirement.
    pub fn payload_align_bytes(&self) -> Option<u8> {
        self.payload_size_bytes()  // for primitive types, align == size
    }
}
```

#### AQ.6.A.4 Generalize `TapeBuilder` payload API
In `crates/bbnf-tape/src/builder.rs`:
- Keep generic `push_leaf_with_scalar<T: Copy>(tape, span, kind, flags, value: T) -> TapeOffset`
- Provide type-specialized wrappers: `push_leaf_with_f64`, `push_leaf_with_i8`, `push_leaf_with_u8`, etc. for ALL scalar types in TypeDesc
- DELETE old single-type methods that are not generalized (`push_leaf_with_bool`, `push_leaf_with_u8` — these become wrappers)

In `crates/bbnf-tape/src/tape.rs`:
- Generic `payload_scalar<T: Copy>(rec) -> Option<T>`
- Type-specialized readers for every scalar TypeDesc type

#### AQ.6.A.5 Type-driven emission
In `crates/core/src/backend/rust/emitter/{tape_prelude,grammar,map_value}.rs`:
- Emission code reads `ctx.payload_type.map(TypeDesc::payload_size_bytes)` to decide emission shape
- No `match PayloadKind::F64 { .. }` anywhere
- Prelude/epilogue dispatches on `TypeDesc` variant

#### AQ.6.A.6 Type-driven view accessors
In `crates/core/src/backend/rust/view/leaves.rs`:
- For each scalar TypeDesc, emit `.value() -> T` that calls `tape.payload_<T>(rec)` with span-text fallback
- No hardcoded `F64` / `Bool` / `U8` match — iterate over TypeDesc variants generically

### AQ.6.B — Aggregate payloads via compile-time layout planner (MANDATORY)

#### AQ.6.B.1 Layout planner pass
New IR pass: `crates/ir/src/passes/payload/layout.rs`
- Input: `GrammarIR`
- For each rule whose `TypeDesc` is `Tuple(scalar_fields...)` where all fields pass `is_scalar_payload`:
  - Compute field offsets respecting alignment
  - Store as `GrammarIR::payload_layouts: HashMap<RuleId, PayloadLayout>`
  - `PayloadLayout { fields: Vec<(TypeDesc, u8 offset)>, total_bytes: u8 }`
- Fixed max size = 16 bytes (fits in `TapeRec.payload_idx` slot range); larger aggregates don't qualify

#### AQ.6.B.2 Aggregate builder API
In `crates/bbnf-tape/src/builder.rs`:
- `push_leaf_with_aggregate(tape, span, kind, flags, bytes: &[u8]) -> TapeOffset`
- Stores up-to-16-byte payload slice

#### AQ.6.B.3 Parser emits multi-field payload writes
When a rule's TypeDesc is a payload-eligible Tuple:
- For each field in the rule body (typically a `Seq` of typed children):
- Emit write of that field's scalar into the correct offset
- After body completes, single `push_leaf_with_aggregate`

#### AQ.6.B.4 View accessor returns typed Rust struct
For a rule with Tuple type `(f64, u8)`, view emits:
```rust
pub fn value(&self) -> (f64, u8) {
    let payload = tape.payload_bytes(rec).unwrap();
    (
        f64::from_le_bytes(payload[0..8].try_into().unwrap()),
        payload[8],
    )
}
```

For rules with `TypeDesc::Named(sid)` wrapping a user struct, emit construction:
```rust
pub fn value(&self) -> Length {
    let payload = tape.payload_bytes(rec).unwrap();
    Length {
        value: f64::from_le_bytes(payload[0..8].try_into().unwrap()),
        unit: LengthUnit::from_u8(payload[8]),
    }
}
```

**Field names come from the grammar's named Seq children or from the Named TypeDesc resolution.**

### AQ.6.C — Alt → typed Rust enum (MANDATORY)

#### AQ.6.C.1 Detect Alt-of-payload-eligible-branches
When a rule's TypeDesc is `Enum` (produced for heterogeneous Alt), and EVERY branch is payload-eligible (scalar or aggregate Tuple):
- The variant_idx in `TapeRec.flags` already encodes which branch
- Each branch stores its payload via AQ.6.A (scalar) or AQ.6.B (aggregate)

#### AQ.6.C.2 View accessor returns typed enum
```rust
// For: value = string | number | "true" -> true | "false" -> false | "null" -> 0u8
// With variants inferred as: Span | F64 | Bool | Bool | U8
pub fn value(&self) -> JsonValue<'p> {
    let rec = self.cursor.record();
    match rec.variant_idx() {
        0 => JsonValue::String(self.span_text()),
        1 => JsonValue::Number(self.tape.payload_f64(rec).unwrap()),
        2 => JsonValue::Bool(true),
        3 => JsonValue::Bool(false),
        4 => JsonValue::Null,
    }
}
```

#### AQ.6.C.3 Enum type generation
Where TypeDesc::Named wraps a user enum, generate the enum type:
```rust
pub enum JsonValue<'p> {
    String(&'p str),
    Number(f64),
    Bool(bool),
    Null,
    // array/object variants via child iteration fallback
}
```

Where branches aren't all payload-eligible, mix payload branches with cursor-wrapped branches:
```rust
pub fn value(&self) -> JsonValue<'p> {
    match rec.variant_idx() {
        0 => JsonValue::String(self.span_text()),       // scalar payload
        1 => JsonValue::Number(tape.payload_f64(rec)),   // scalar payload
        2 => JsonValue::Array(ArrayView::from(self)),    // cursor-wrapped (non-payload)
        ...
    }
}
```

### AQ.6.D — Rename honestly, delete legacy terminology (MANDATORY)

#### AQ.6.D.1 Doc renames
Replace every "direct projection" / "tier B" / "direct-to-struct" mention in code comments, docs, tranche files with:
- "Typed payload projection" or
- "TypeDesc-driven value materialization"

The mechanism is enhanced tape + TypeDesc-driven payload layout, not a separate ABI.

#### AQ.6.D.2 Delete all PayloadKind references
- `PayloadKind` enum (`emitter_types.rs`)
- `ctx.payload_kind` field on RustEmitCtx
- Every `match payload_kind` site
- All doc comments referring to `PayloadKind`

#### AQ.6.D.3 Audit `is_f64_payload_eligible`
Rename → `has_scalar_payload_type`. Return `Option<&TypeDesc>`. Generalize from "is this F64?" to "is this any scalar?".

**Files:**
- MODIFY: `crates/ir/src/types/type_desc.rs` (add I8/I16/U16/I32/I64/U64, helpers)
- MODIFY: `crates/ir/src/passes/types/` (CSP inference for new types)
- NEW: `crates/ir/src/passes/payload/layout.rs` (layout planner)
- MODIFY: `crates/ir/src/types/grammar.rs` (payload_layouts field)
- MODIFY: `crates/bbnf-tape/src/builder.rs` (generic scalar + aggregate pushes)
- MODIFY: `crates/bbnf-tape/src/tape.rs` (generic scalar + aggregate reads)
- MODIFY: `crates/core/src/backend/rust/emitter_types.rs` (delete PayloadKind)
- MODIFY: `crates/core/src/backend/rust/emitter/{grammar,map_value,tape_prelude,mod,leaves}.rs`
- MODIFY: `crates/core/src/backend/rust/view/{leaves,alt,seq}.rs`

**Hard gate:** `grep -rn "PayloadKind" crates/` returns zero matches.

## Phase 7: CSS L4 `__compoundSelector` — Kernel Routing

AP.4 landed key dispatch for `__declaration`. The remaining hottest function is `__compoundSelector` (40.2% normalize, 36.6% tailwind). It does hand-rolled byte-by-byte CSS identifier parsing inline instead of calling the `scan_ident` kernel.

### AQ.7.1 Classify CSS identifier regex as `RegexClass::Identifier`
With `allows_leading_dash: true, allows_double_dash_prefix: true`. Update `try_classify_identifier` in bbnf-regex (builds on AQ.3.1 parameter additions).

### AQ.7.2 Emit `scan_ident` kernel call instead of inline char-class loops
In `core/src/generate/regex/emit/` — when emission detects `RegexClass::Identifier`, emit `::parse_that::scan_ident(state, &config)` instead of unrolling char-class loops.

### AQ.7.3 Length-bucketed perfect hash for key dispatch
AP.4 emits a linear array-equality ladder inside `'kd_blk`. Replace with:
- Group by byte length
- Within each length, perfect-hash on first/last byte
- Single SIMD compare for 16-byte-aligned names

**Expected impact: CSS bootstrap 505 → 800+, normalize 978 → 1,400+.**

**Files:**
- MODIFY: `parse-that/rust/regex/src/classify/structural.rs` (expand Identifier detection)
- MODIFY: `crates/core/src/generate/regex/emit/` (emit scan_ident for Identifier variant)
- MODIFY: `crates/ir/src/passes/recognizers/key_dispatch.rs` (length-bucketed hash)
- MODIFY: `crates/core/src/backend/driver/alt.rs` (hash-based dispatch codegen)

## Phase 8: Port sonic-rs Techniques (Portable Only)

### AQ.8.1 `skip_space` bitmap caching
Add `nospace_bits: u64, nospace_start: isize` to ParserState. After first SIMD WS scan, cache non-space bitmap. Next `trim_leading_whitespace` within 64 bytes of previous scan reuses via `trailing_zeros()`. sonic-rs's fast path 2 — HUGE win on pretty JSON.

### AQ.8.2 Pre-size output Vec with grammar-derived constant
Compile-time analysis: maximum nodes-per-byte bound from grammar structure. Parser emits `TapeBuilder::with_capacity(input.len() * MAX_NODE_PER_BYTE)` with grammar-specific constant (replace current `/4` heuristic).

### AQ.8.3 TLS-recycled scratch for TapeBuilder (codegen flag)
Optional per-grammar flag: `#[parser(tls_scratch)]`. Thread-local recycled Vec<TapeRec>. LSP and gorgeous repeat-parse scenarios benefit.

### AQ.8.4 Port `simd_str2int` to NEON
sonic-rs has x86_64 SIMD digit-parse but no NEON. Implement NEON version: `vsubq_u8` by `'0'`, range check via `vmaxq_u8/vminq_u8`, bitmask via `vandq_u8 + vshrn_n_u16`, pairwise adds via `vmull_u8 + vpaddlq_u16`. Used for float fractions.

**Expected impact: canada 1,797 → 2,100+, data_xl 1,341 → 1,700+.**

**Files:**
- MODIFY: `parse-that/rust/parse_that/src/state.rs` (bitmap cache fields)
- MODIFY: `parse-that/rust/parse_that/src/scanners.rs` (skip_space cache)
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/number.rs` (NEON simd_str2int)
- MODIFY: `crates/bbnf-tape/src/builder.rs` (TLS scratch option)
- MODIFY: `crates/core/src/backend/rust/emitter/grammar.rs` (Vec sizing + TLS flag)

## Phase 9: Validation + Instrumentation + Cost Calibration

### AQ.9.1 Full bench sweep
Single-invocation sweep for JSON (5) + CSS L4 (3) + sheets (3) + compile pipeline (5). Write to `docs/benchmarks/post-AQ.json`.

### AQ.9.2 samply diff against post-AP
Save post-AQ profiles to `docs/benchmarks/profiles/post-AQ/`. Every +X% claim cites symbol + self-time delta.

### AQ.9.3 Release-build instrumentation
`BBNF_EGRAPH_REPORT=1` and `BBNF_CSP_REPORT=1`. AN.6, AO.4.3, AP.6.4 all deferred this — must ship.

### AQ.9.4 Cost model grid sweep
Sweep `dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`, `tape_push` across JSON + CSS L4. Select defaults maximizing geomean. No individual bench regresses >1%.

### AQ.9.5 Global CSP solve
Currently per-component. CSS L4 still has multi-component grammars that could benefit. Implement `solve_grammar_global` with 10M node budget. Accept compile-time cost if parse throughput wins OR document as architecturally unnecessary with evidence.

### AQ.9.6 Grammar rule-count freeze
After AQ.1 bootstrap regen, `crates/core/tests/grammar_roundtrip.rs` has exact counts. Verify across all grammars (json, ebnf, bbnf, sheets, css_l4).

---

## Performance Targets

### JSON (target: BEAT sonic-rs on citm/twitter even without pre-scan)
| Dataset | Current | AQ Target | sonic-rs | Goal |
|---------|---------|-----------|----------|------|
| canada | 1,797 | **2,100+** | 1,540 | **BEAT (extend)** |
| citm | 2,712 | **3,100+** | 3,000 | **BEAT** |
| twitter | 2,173 | **2,700+** | 2,643 | **BEAT** |
| data | 1,900 | 2,300+ | 2,346 | PARITY |
| data_xl | 1,341 | **1,700+** | 1,460 | BEAT |

Note: citm target is 3,100 (not 3,400 in old plan) because pre-scan is deleted. Gains come from skip_space cache + SIMD integer parsing + aggregate payloads reducing tape-construction overhead.

### CSS L4 (key dispatch + scan_ident)
| Dataset | Current | AQ Target |
|---------|---------|-----------|
| normalize | 978 | **1,400+** |
| bootstrap | 505 | **800+** |
| tailwind | 534 | **800+** |

### Hard Gates
- `cargo test --workspace` passes (minus 2 known-failing google_sheets tests)
- **`grammar_roundtrip` tests un-ignored and passing** (Phase 1 completion)
- **`generated.rs` reproducible from bootstrap script with ZERO hand patches** (Phase 1)
- `grep -rn "PayloadKind" crates/` returns zero matches (Phase 6 completion)
- `grep -rn "structural_bytes\|scan_structural\|filter_quote_parity" crates/ parse-that/` returns zero matches (Phase 5 completion)
- Zero `JsonString`/`JsonNumber`/`CssIdent`/`CssQuotedString` in `RegexClass` (Phase 3)
- Zero `scan_json_*` / `sp_json_*` / `sp_css_*` function names in parse-that (Phase 4)
- `docs/benchmarks/post-AQ.json` exists

### Soft Gates
- JSON citm ≥ 3,100 MB/s (BEAT sonic-rs)
- JSON twitter ≥ 2,700 MB/s (BEAT sonic-rs)
- CSS L4 bootstrap ≥ 800 MB/s
- NEON `simd_str2int` shipped
- `skip_space` bitmap cache active
- Global CSP solve shipped or documented as unnecessary

---

## Execution Waves (6 agents per wave)

### Wave 1: Self-Hosting Closure + Foundation (AQ.1 + AQ.2)
- Agent A: Schema emitter `cst_directives` layout fix (AQ.1.1 blocker 1)
- Agent B: Heterogeneous Alt sub-variant coercion fix (AQ.1.1 blocker 2)
- Agent C: IR inspect module (AQ.2)
- Agent D: Delete legacy bridges in host.rs (AQ.1.4)
- Agent E: Bootstrap regen orchestration + roundtrip test freeze (AQ.1.2, AQ.1.3)
- Agent F: DELETE structural dispatch infrastructure (AQ.5)

### Wave 2: Deoverfit (AQ.3 + AQ.4)
- Agent A: RegexClass variant expansion + classifier updates (AQ.3.1, AQ.3.4)
- Agent B: RegexClass consumer migration (AQ.3.6)
- Agent C: Delete classify_known_pattern + nominal variants (AQ.3.2, AQ.3.3)
- Agent D: parse-that scanner renames (AQ.4.1-4.6)
- Agent E: parse-that SpanParser + SpanScanner renames (AQ.4.3-4.4, 4.7)
- Agent F: Kernel renames + STRUCTURAL_PUNCTS parameterization (AQ.4.8-4.9)

### Wave 3: TypeDesc-Driven Projection (AQ.6)
- Agent A: TypeDesc expansion (I8/I16/U16/I32/I64/U64) + helpers (AQ.6.A.1, A.3)
- Agent B: Delete PayloadKind + migrate emitter to TypeDesc consumption (AQ.6.A.2, A.5, A.6)
- Agent C: Generalize TapeBuilder scalar API (AQ.6.A.4)
- Agent D: Layout planner pass + aggregate payloads (AQ.6.B)
- Agent E: Alt→typed enum view codegen (AQ.6.C)
- Agent F: Rename cleanup + delete legacy terminology (AQ.6.D)

### Wave 4: CSS L4 + sonic-rs + Validation (AQ.7 + AQ.8 + AQ.9)
- Agent A: CSS L4 __compoundSelector → scan_ident routing (AQ.7.1-7.2)
- Agent B: Length-bucketed perfect hash (AQ.7.3)
- Agent C: skip_space bitmap caching (AQ.8.1) + Vec pre-sizing (AQ.8.2)
- Agent D: NEON simd_str2int (AQ.8.4)
- Agent E: Full bench sweep + samply profiles + post-AQ.json (AQ.9.1, 9.2, 9.6)
- Agent F: Cost model grid sweep + global CSP + release instrumentation (AQ.9.3-9.5)

---

## Critical Files

| File | Phase |
|------|-------|
| `crates/core/src/grammar/generated.rs` | 1 (REGEN) |
| `crates/core/src/grammar/schema/**` | 1 |
| `crates/core/src/grammar/host.rs` | 1 (DELETE bridge) |
| `crates/core/tests/grammar_roundtrip.rs` | 1 |
| `crates/ir/src/passes/inspect/` | 2 (NEW) |
| `crates/ir/src/passes/recognizers/*.rs` | 2 |
| `parse-that/rust/regex/src/classify/mod.rs` | 3 |
| `parse-that/rust/regex/src/classify/structural.rs` | 3, 7 |
| `parse-that/rust/parse_that/src/parsers/scan/{number,number_f64,ident,ws_comment,balanced,quoted}.rs` | 4 |
| `parse-that/rust/parse_that/src/parsers/json.rs` | 4 |
| `parse-that/rust/parse_that/src/span_parser/{constructors,span_scanner}.rs` | 4 |
| `parse-that/rust/parse_that/src/parsers/scan/structural.rs` | 5 (DELETE) |
| `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs` | 5 (DELETE) |
| `parse-that/rust/parse_that/src/state.rs` | 5, 8 |
| `crates/ir/src/passes/structural_bytes.rs` | 5 (DELETE) |
| `crates/ir/src/types/type_desc.rs` | 6 |
| `crates/ir/src/passes/payload/layout.rs` | 6 (NEW) |
| `crates/ir/src/passes/types/` | 6 |
| `crates/bbnf-tape/src/{builder,tape}.rs` | 6 |
| `crates/core/src/backend/rust/emitter_types.rs` | 5, 6 |
| `crates/core/src/backend/rust/emitter/{alt,ws,grammar,leaves,map_value,tape_prelude,mod}.rs` | 5, 6 |
| `crates/core/src/backend/rust/view/{leaves,alt,seq}.rs` | 6 |
| `crates/core/src/backend/kernels/*.rs` | 4 |
| `crates/ir/src/passes/recognizers/key_dispatch.rs` | 7 |
| `crates/core/src/backend/driver/alt.rs` | 7 |
| `parse-that/rust/parse_that/src/scanners.rs` | 8 |
| `parse-that/rust/parse_that/src/parsers/scan/number.rs` | 8 |
| `docs/benchmarks/post-AQ.json` | 9 (NEW) |
| `docs/benchmarks/profiles/post-AQ/` | 9 (NEW) |

---

---

## Operational Directives for Executing Agents

### Worktree Isolation Strategy

Each agent runs in its OWN worktree. This tranche has ~30 sub-phases across 4 waves × 6 agents = 24 parallel sessions max. Worktrees prevent file-level contention.

```bash
# Orchestrator launches agents with isolation: "worktree" parameter
# Agents MUST NOT git add -A or git add . (would pick up other worktree paths)
# Agents commit to their isolated branch; orchestrator cherry-picks onto master

# Example agent context injection:
isolation: "worktree"
# Agent receives: worktreePath, worktreeBranch, agentId
# Agent works entirely within their worktree
```

**Inter-agent file coordination:**
- Each agent gets EXCLUSIVE WRITE permission for named files in its brief
- Agents with OVERLAPPING write needs must be sequenced into different waves
- Example conflicts to avoid in the same wave:
  - `emitter_types.rs` (PayloadKind delete) vs `emitter/mod.rs` (payload detection)
  - `type_desc.rs` (variants) vs `types/` CSP (inference updates)
  - `alt.rs` (structural delete) vs `alt.rs` (key dispatch hash) — must be different waves

**Stale lockfile avoidance:**
- Worktrees share `Cargo.lock` with main repo — this is FINE for `cargo check` but can cause friction
- Agents running `cargo bench` must clear proc-macro cache AND touch derive crate:
  ```bash
  rm -rf /Users/mkbabb/Programming/bbnf-lang/crates/target/.bbnf-cache
  touch /Users/mkbabb/Programming/bbnf-lang/crates/derive/src/lib.rs
  ```
  OR the build will use cached generated code and changes won't be tested.

### Orchestrator Responsibilities

1. **Launch Wave 1 (6 agents in parallel).** Collect all results before launching Wave 2.
2. **Cherry-pick each worktree branch onto master** after agent completion:
   ```bash
   for b in $(git branch | grep worktree-agent | tr -d ' +'); do
       echo "=== $b ==="
       git log master..$b --oneline
   done
   git cherry-pick worktree-agent-<id>
   ```
3. **If cherry-pick conflicts,** stash the conflicting worktree changes, resolve manually, commit, then pop stash. Record the conflict pattern for future wave planning.
4. **Run validation between waves:**
   ```bash
   cargo check --workspace 2>&1 > /tmp/wave_check.txt
   grep "^error" /tmp/wave_check.txt  # must be empty
   cargo test -p bbnf-ir -p bbnf-tape 2>&1 > /tmp/wave_test.txt
   grep "test result:" /tmp/wave_test.txt
   ```
5. **Delete stale worktree branches** after successful cherry-pick:
   ```bash
   for b in $(git branch | grep worktree-agent | tr -d ' +'); do git branch -D "$b"; done
   ```
6. **Commit with `/commit` frequently.** After each successful wave, after each cherry-pick group.

### samply Profiling Methodology

After each wave that affects parser hot paths (Wave 3 payload, Wave 4 CSS L4), capture profiles:

```bash
# Standard capture procedure
rm -rf /Users/mkbabb/Programming/bbnf-lang/crates/target/.bbnf-cache
touch /Users/mkbabb/Programming/bbnf-lang/crates/derive/src/lib.rs

cargo bench -p bbnf --bench json_monolithic --bench css_l4 --bench compile_pipeline --no-run 2>&1 > /tmp/build.txt
grep "Finished" /tmp/build.txt  # confirm bbnf_derive + bbnf recompiled

JSON_BIN=$(find /Users/mkbabb/Programming/bbnf-lang/target/release/deps -maxdepth 1 -type f -perm -111 -name 'json_monolithic-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
CSS_BIN=$(find /Users/mkbabb/Programming/bbnf-lang/target/release/deps -maxdepth 1 -type f -perm -111 -name 'css_l4-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
COMPILE_BIN=$(find /Users/mkbabb/Programming/bbnf-lang/target/release/deps -maxdepth 1 -type f -perm -111 -name 'compile_pipeline-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)

mkdir -p docs/benchmarks/profiles/post-AQ

# Per-dataset JSON (5 datasets — all must be captured)
for ds in canada citm_catalog data data_xl twitter; do
  (cd /Users/mkbabb/Programming/bbnf-lang/crates/core && \
   samply record --save-only --unstable-presymbolicate \
     -o ../../docs/benchmarks/profiles/post-AQ/json_${ds}.samply \
     -- "$JSON_BIN" $ds --bench 2>&1) > /tmp/samply_json_${ds}.txt
done

# Per-dataset CSS L4
for ds in bootstrap normalize tailwind; do
  (cd /Users/mkbabb/Programming/bbnf-lang/crates/core && \
   samply record --save-only --unstable-presymbolicate \
     -o ../../docs/benchmarks/profiles/post-AQ/css_l4_${ds}.samply \
     -- "$CSS_BIN" $ds --bench 2>&1) > /tmp/samply_css_${ds}.txt
done

# Compile pipeline
for g in compile_json compile_bbnf compile_ebnf compile_sheets compile_css_l4; do
  samply record --save-only --unstable-presymbolicate \
    -o docs/benchmarks/profiles/post-AQ/${g}.samply \
    -- "$COMPILE_BIN" ${g} --bench 2>&1 > /tmp/samply_${g}.txt
done
```

**Key directives:**
- `--unstable-presymbolicate` is mandatory (produces `.syms.json` sidecar so profiles load standalone)
- `-o` absolute OR relative-from-crates/core path (bench binaries use relative paths internally)
- Change to `crates/core` before running bench binaries (fixture paths are relative)
- Single-invocation sweep per bench binary (Tranche Z invariant — cold-start contaminates individual runs)
- Post-AQ profiles must be stored at `docs/benchmarks/profiles/post-AQ/` matching the post-AP layout

### Cargo Expand Usage

After codegen changes, validate emission with `cargo expand`:

```bash
# Never pipe long output to stdout — always redirect
cargo expand -p bbnf --test json_slab 2>&1 > /tmp/json_expand.txt
wc -l /tmp/json_expand.txt  # sanity-check size

# Key counts to verify
grep -c "push_leaf_with_f64\|push_leaf_with_bool\|push_leaf_with_u8\|push_leaf_with_u32" /tmp/json_expand.txt
grep -c "PayloadKind" /tmp/json_expand.txt  # must be 0 after Phase 6
grep -c "scan_structural\|sync_structural_cursor" /tmp/json_expand.txt  # must be 0 after Phase 5
grep -c "RegexClass::JsonNumber\|RegexClass::JsonString" /tmp/json_expand.txt  # must be 0 after Phase 3

# Inspect hot functions specifically
awk '/^    fn __value/,/^    fn __[a-zA-Z]/' /tmp/json_expand.txt | head -200 > /tmp/value_fn.txt
awk '/^    fn __pair/,/^    fn __[a-zA-Z]/' /tmp/json_expand.txt | head -100 > /tmp/pair_fn.txt
```

### Running Tests — File-First Discipline

Long cargo commands MUST redirect output to a file, grep/tail over it, never re-invoke:

```bash
cargo test --workspace 2>&1 > /tmp/workspace_tests.txt
echo "EXIT: $?" >> /tmp/workspace_tests.txt

# Then grep for results
grep "test result:" /tmp/workspace_tests.txt
grep "FAILED\|panicked" /tmp/workspace_tests.txt | head -20
grep "^error\[E[0-9]*\]:" /tmp/workspace_tests.txt | head -10
```

Known pre-existing failures (do not flag as regressions):
- `google_sheets_slab::parse_arithmetic` — pre-existing on master
- `google_sheets_slab::parse_let_nested` — pre-existing on master

### Writing Analysis Reports

When an agent writes an analysis report:
- Use relative paths like `docs/benchmarks/profiles/post-AQ/analysis-X.md`
- Cite line numbers precisely: `file.rs:42`
- Every "+X%" claim must cite a samply symbol + self-time delta
- Every "slow path" claim must cite an actual profile sample count
- Every "generalized" claim must list the consumer sites migrated

### Anti-Patterns to Avoid (Codebase Edicts)

From memory + user directives across tranches:

1. **NO language-specific code** in shared infrastructure — no `json_*`, `css_*` names where structural names suffice
2. **NO workarounds, hacks, or temporary fixes** — fix root causes. Never add `// TODO: remove` comments
3. **NO `#[allow(...)]`** to suppress warnings related to your changes
4. **NO hand-patching `generated.rs`** — AQ.1 MUST end this
5. **NO inline `#[cfg(test)]` modules** — tests live in `tests/`
6. **NO editorializing in commits** — utilitarian, pragmatic
7. **NO Claude or AI authorship** in commits
8. **Benchmark invariant:** single `--bench` invocation per binary. Per-test subprocess invocations add ~5-12% noise
9. **Proc-macro cache is sneaky** — ALWAYS clear `crates/target/.bbnf-cache` + touch `crates/derive/src/lib.rs` before any bench after code changes to codegen

### Pre-Wave Sanity Check

Before each wave, orchestrator runs:

```bash
# Verify we're on master with clean tree
git status --short  # should show at most untracked /tmp-style files
git log --oneline -5  # confirm expected base commit
cargo check --workspace 2>&1 > /tmp/pre_wave.txt
grep "^error" /tmp/pre_wave.txt  # must be empty
echo "Pre-wave OK: $(date)"
```

### Post-Wave Validation

After each wave's cherry-picks:

```bash
# Clear caches and force rebuild
rm -rf /Users/mkbabb/Programming/bbnf-lang/crates/target/.bbnf-cache
touch /Users/mkbabb/Programming/bbnf-lang/crates/derive/src/lib.rs

# Compile check
cargo check --workspace 2>&1 > /tmp/post_wave_check.txt
grep "^error" /tmp/post_wave_check.txt  # must be empty

# Test check
cargo test --workspace 2>&1 > /tmp/post_wave_tests.txt
grep "test result:" /tmp/post_wave_tests.txt | grep -v "0 passed; 0 failed"
grep "FAILED\|panicked" /tmp/post_wave_tests.txt

# If Wave 3 or 4 (hot-path changes), capture post-wave benches
cargo bench -p bbnf --bench json_monolithic --bench css_l4 --no-run 2>&1 > /tmp/post_wave_build.txt
JSON_BIN=$(find /Users/mkbabb/Programming/bbnf-lang/target/release/deps -maxdepth 1 -type f -perm -111 -name 'json_monolithic-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
CSS_BIN=$(find /Users/mkbabb/Programming/bbnf-lang/target/release/deps -maxdepth 1 -type f -perm -111 -name 'css_l4-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
(cd /Users/mkbabb/Programming/bbnf-lang/crates/core && "$JSON_BIN" --bench 2>&1) > /tmp/post_wave_json.txt
(cd /Users/mkbabb/Programming/bbnf-lang/crates/core && "$CSS_BIN" --bench 2>&1) > /tmp/post_wave_css.txt
```

### Wave Dependencies (Hard Ordering)

| Wave | Depends On | Rationale |
|------|-----------|-----------|
| 1 | Master (clean) | Foundation: self-hosting, inspect module, structural delete |
| 2 | Wave 1 complete | Deoverfit builds on cleaner foundation (inspect helpers in use) |
| 3 | Wave 2 complete | TypeDesc expansion needs RegexClass parameterization for integer inference |
| 4 | Wave 3 complete | CSS scan_ident routing needs Wave 3's RegexClass::Identifier flags. Validation needs all changes landed. |

### Agent Briefing Template

Every agent brief MUST include:
- **Task title + phase reference** (e.g., "AQ.6.A.4: Generalize TapeBuilder scalar API")
- **Context on what was already done** (link to audit doc, prior tranches if relevant)
- **Explicit file bounds** (MODIFY / READ / DELETE with absolute paths)
- **Hard gates** (compile + test + specific grep must-fails)
- **Worktree isolation flag**
- **Commit directive** at completion
- **Anti-pattern reminder** (no workarounds, no hacks, no legacy preservation)

Example skeleton:

```
## Task: AQ.X.Y — <title>
Location: /Users/mkbabb/Programming/bbnf-lang (worktree will be auto-created)

## Context
<what prior tranches did, what AP.md says, what the audit found>

## Specific Change
<exact code change required with file:line references>

## File Bounds
- MODIFY: <list with absolute paths>
- READ: <list>
- DELETE: <list>
- DO NOT MODIFY: <list for isolation>

## Validation
<bash block that verifies correctness>

## CRITICAL RULES
- No workarounds, hacks, temporary fixes
- No language-specific naming
- Write all output to files (never pipe long output to stdout)
- Commit with clear message before finishing
- Clear proc-macro cache before bench operations
```

---

## Summary of Changes vs Initial AQ Plan

Changes from the first draft of AQ.md based on deeper analysis:

| Area | Initial Plan | Revised Plan |
|------|-------------|--------------|
| **Phase 1** | Was Phase 7 (low priority) | **Moved to Phase 1 (HIGHEST PRIORITY)** — Self-hosting closure cannot be deferred further |
| **Structural dispatch** | Phase 3: activate with fused filter + WS elision | **Phase 5: DELETE entirely** — no longer viable post-AP |
| **TypeDesc expansion** | I8/I16/I32/I64/U16 mentioned as extension | **I8/I16/U16/I32/U32/I64/U64 MANDATORY** — full integer suite |
| **Direct-to-struct** | 3 phases with Alt→enum marked optional | **All 4 sub-phases A/B/C/D MANDATORY** — full TypeDesc consumption, aggregate layouts, Alt→enum, rename cleanup |
| **PayloadKind** | "Rename for honesty" | **DELETE entirely** — consume TypeDesc directly |
| **AL's direct fix** | Phase 4.1 | **Acknowledged already shipped via MaterializationClass=TapeSpanOnly** — no separate ABI |
| **Pre-scan viability** | Gated behind WS elision | **Flatly not viable post-AP.3.1** — delete |
