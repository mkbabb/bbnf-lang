# SK-V12 Value/ValueRef/Tape Lock-14 Audit

Audit date: 2026-05-20. Scope: Lock-14 generality surface in runtime Value/ValueRef/Tape and codegen projection emitters. Outputs: surface map, shape contracts, leak inventory, ValueRef status, Sheets-readiness verdict, W2 contact points.

---

## §1 Surface Map

### runtime crate (`skinny/crates/runtime/src/`)

| Name | Path | Role | Generic? | JSON-Policied? |
|------|------|------|----------|---------|
| `Tape<'input>` | `tape/mod.rs:94` | Substrate; offset/flag/payload arena for all parsed trees | Yes | No |
| `ValueRef<'doc, 'input, K, G>` | `tape/mod.rs:175` | Cursor + tape reference (nominal genericity over kind + grammar) | Yes | No |
| `TapeId` | `tape/mod.rs:92` | Identity tag for tape instances | Yes | No |
| `OffsetFlags` | `tape/mod.rs:18` | Bit flags: HAS_ESC (0x01), HAS_CONTROL (0x02) | Yes | No (generic flag semantics) |
| `TapeBuilder<'input>` | `tape/assembler.rs:42` | Tape assembly: offset/flag allocation, payload write | Yes | No |
| `PayloadArena` | `tape/mod.rs:38` | Byte buffer for string/payload materialization | Yes | No |
| `JsonValue<'doc, 'input>` | `grammars/json/value.rs:69` | Enum: Object/Array/String/Number/Bool/Null | No (JSON-specific) | Yes |
| `JsonNodeKind` | `grammars/json/value.rs:12` | Enum: Root/ObjectOpen/ArrayOpen/Pair/String/Number/True/False/Null/separators | No (JSON-specific) | Yes |
| `JsonSink` | `grammars/json/sink.rs:3` | Trait for direct-parse output (sink pattern) | No (JSON-specific) | Yes |
| `JsonObject<'doc, 'input>` | `grammars/json/view.rs:84` | Wrapper over ValueRef for object navigation | No (JSON-specific) | Yes |
| `JsonArray<'doc, 'input>` | `grammars/json/view.rs` | Wrapper over ValueRef for array navigation | No (JSON-specific) | Yes |
| `JsonString<'doc, 'input>` | `grammars/json/view.rs` | Wrapper over ValueRef for string span/decode | No (JSON-specific) | Yes |
| `JsonRoot<'input>` | `grammars/json/view.rs:11` | Entry point; wraps JsonDocument | No (JSON-specific) | Yes |
| `EventGrammar` | `tape/event_grammar.rs` | Trait: STRUCTURAL_CLASS_COUNT, FactId, admits_fact | Yes | No |
| `AnyGrammar` | `tape/event_grammar.rs` | Marker struct for untyped ValueRef | Yes | No |
| `DocumentView<'a>` | `tape/mod.rs:227` | Trait: root_value(), tape_id(), source() | Yes | No |

### codegen crate (`skinny/crates/codegen/src/`)

| Name | Path | Role | Generic? | JSON-Policied? |
|------|------|------|----------|---------|
| `DirectSchemaSet` | `direct_schema.rs:4` | Output schema metadata: module_name, roots, types, schema_hash | Yes | No |
| `DirectRootSchema` | `direct_schema.rs:12` | Root projection descriptor (function_name, rust_type, ty) | Yes | No |
| `DirectTypeSchema` | `direct_schema.rs:45` | Type definition: type_id, rust_type, kind (Struct with fields) | Yes | No |
| `DirectFieldSchema` | `direct_schema.rs:61` | Field descriptor: key_literal, rust_field, ty, presence, duplicate policies | Yes | No (policies are generic) |
| `DirectScalar` | `direct_schema.rs:106` | Enum: String/Bool/I64/U64/U32/F64 | Yes | No |
| `json_provider` module | `json_provider.rs` | Provider funcs: ensure_runtime_profile, mod_rs, generated_rs, etc. | No | Yes |

### IR crate (`skinny/crates/ir/src/`)

| Name | Path | Role | Generic? | JSON-Policied? |
|------|------|------|----------|---------|
| `GrammarIr` | `lib.rs:31` | Grammar IR: name, source_hash, rules, exprs | Yes | No |
| `Rule` | `lib.rs` | Rule: id, name, body, span | Yes | No |
| `ExprKind` | `lib.rs` | Expr variant: Seq/Alt/Repeat/Literal/Ref/etc. | Yes | No |

---

## §2 Shape Contracts

Three output planes, each with runtime path, IR lowering, and codegen function:

| Plane | Runtime Path | IR Lowering | Codegen Emitter |
|-------|--------------|-------------|-----------------|
| **parse_only** | `parse()` → `JsonRoot` + `JsonValue` enums + tape-backed views (JsonObject/Array/String wrapping ValueRef) | Grammar rule → Seq/Alt/Repeat exprs; JSON value rule dispatches to container/scalar branch | `json_provider::generated_rs()` emits `parse_json<'i>()` + dispatch tree to tape builder. Output: read-only JsonValue enum + tape offset navigation. |
| **direct_to_struct** (direct sink) | `parse_direct<'i, S: JsonSink>()` with context-sensitive sink methods (root_string, array_i64, object_string, etc.) | Grammar rule → Seq/Alt for structure; values dispatch to sink context per container role (root/object/array) | `json_provider::generated_rs()` emits `parse_direct<'i, S: JsonSink>()` + role-tagged sink calls. Schema from DirectSchemaSet (unimplemented in current codegen; JSON-only output). |
| **real_typed_struct** (typed direct) | Not yet emitted. Would be: generated `parse_typed<'i>() → T` where T is Rust struct from schema. | DirectTypeSchema → each Struct type becomes a parsing rule; fields map to sink/direct field assignments. | Would be: codegen function `emit_typed_direct()` that generates typed parsers per DirectTypeSchema root. Currently blocked by `ensure_runtime_profile()` check. |

---

## §3 Lock-14 Leaks (JSON Policy Embedded in Generic Code)

**Leak count: 5 major + 2 embedded**

### Leak #1: Structural alphabet hardcoding (CRITICAL)
- **File:line** `skinny/crates/codegen/src/json_templates/generated.rs:10`; also `skinny/crates/runtime/src/grammars/json/generated.rs:10`
- **Policy bled in**: JSON structural character set `b"{}[],:\""` is compiled as a const rather than injected from grammar metadata.
- **Should be provided by**: Generated grammar metadata module (per-grammar `STRUCTURAL_ALPHABET` constant or enum, not hardcoded in shared template).
- **SK-V12 wave**: W1. Must be decoupled before non-JSON baseline can compile. Lock 14 violation: generic generated.rs template contains JSON-specific literal.

### Leak #2: Value dispatch hardcoding
- **File:line** `skinny/crates/codegen/src/json_templates/generated.rs:47` (dispatch_value); `skinny/crates/runtime/src/grammars/json/generated.rs:47`
- **Policy bled in**: dispatch_value() matches on JSON-specific byte patterns (b'{', b'[', b'"', b'-'|b'0'..9, b't', b'f', b'n') and maps to JsonNodeKind variants. Non-JSON grammars have different primaries (Sheets: formulas start with =, CSS selectors with . or #, etc.).
- **Should be provided by**: Generated dispatch table from FIRST sets + byte class metadata; generic template iterates table, not hardcoded match arms.
- **SK-V12 wave**: W1. p2f-grammar-neutral.md §3 cites this as F6 defect: "no generic branch selects behavior by JSON grammar name".

### Leak #3: String escape/quote policy (JSON backslash model)
- **File:line** `skinny/crates/codegen/src/json_templates/generated.rs:99-100` (match_tiny_plain_string, match_string_at_quote calls with JSON escape flags); `skinny/crates/runtime/src/grammars/json/generated.rs:99-100`
- **Policy bled in**: parse_key_colon() and string parsing use OffsetFlags::HAS_ESC (0x01) which implies JSON backslash escape model. Sheets uses doubled-quote escape, CSS uses backslash + context-specific sequences.
- **Should be provided by**: Generated escape_policy metadata (escape_char, terminators, control_set); host function for per-grammar decode.
- **SK-V12 wave**: W1. p2f cites as F2 conditional + F3 conditional: "decoded materialization must stay per grammar/host."

### Leak #4: Number policy (JSON-only matching)
- **File:line** `skinny/crates/codegen/src/json_templates/generated.rs:215` (parse_number delegates to match_number_span_from_first); reused in `skinny/crates/runtime/src/grammars/json/generated.rs:215`
- **Policy bled in**: JSON numbers are matched with leading-minus/0..9 patterns; JSON does not allow leading-dot (Sheets does, CSS does). Current parse_number_span is JSON-tuned.
- **Should be provided by**: Generated number_config from grammar metadata (digit_alphabet, leading_dot, exponent_style, etc.); template calls configured span matcher, not JSON-specific.
- **SK-V12 wave**: W1. p2f F4: "config covers sign, leading-dot, leading-zero, fraction, exponent, suffix/unit"; currently JSON-hardcoded.

### Leak #5: Key quoting assumption (JSON object member model)
- **File:line** `skinny/crates/codegen/src/json_templates/generated.rs:83-93` (parse_key_colon expects quoted string, colon separator). Runtime version: `skinny/crates/runtime/src/grammars/json/generated.rs:83-93`
- **Policy bled in**: parse_pair() calls parse_key_colon() which assumes key is a quoted JSON string followed by colon. CSS properties and Sheets cell properties have different syntax.
- **Should be provided by**: Generated pair/object rule from grammar, not hardcoded in shared template. Object iteration rule must be per-grammar.
- **SK-V12 wave**: W1. Major blocker for non-JSON grammar emission.

### Leak #6 (embedded): OffsetFlags semantics tied to JSON escape model
- **File:line** `skinny/crates/runtime/src/tape/mod.rs:22-23` (HAS_ESC, HAS_CONTROL bit constants)
- **Policy bled in**: These bit constants encode JSON string-escape semantics. Non-JSON grammars may need different flags (e.g., doubled-quote escape has no backslash bit, CSS escape has context).
- **Should be provided by**: Grammar-generated flag scheme; generic OffsetFlags could define slots but not bit meanings.
- **SK-V12 wave**: W2/W3. Lower priority; tape API likely stable for W1.

### Leak #7 (embedded): JsonSink trait methods hardcoded to JSON contexts
- **File:line** `skinny/crates/runtime/src/grammars/json/sink.rs:3-119` (JsonSink trait definition); also `skinny/crates/codegen/src/json_templates/generated.rs` (sink call emission)
- **Policy bled in**: JsonSink defines begin_object, end_array, key(), string(), i64(), u64(), f64(), bool(), null() — all JSON-specific. Direct parse for Sheets would need different callbacks (formula_start, cell_range, error_literal, etc.).
- **Should be provided by**: Grammar-generated sink trait per grammar (SheetsDirectSink, CssDirectSink); shared template accepts &mut dyn SinkTrait parameterized by grammar metadata.
- **SK-V12 wave**: W1 (required for Sheets emission; see W1 plan preflight W1-A2).

---

## §4 ValueRef / Value Status

### ValueRef<'doc, 'input, K = AnyKind, G: EventGrammar = AnyGrammar>

**Lifetimes:**
- `'input`: borrowed from parse input bytes
- `'doc`: document scope (borrowed from Tape reference)
- Constraint: `'input: 'doc` ensures tape outlives document views

**Generic parameters:**
- `K`: Kind phantom (AnyKind erases, specific kinds like RootKind/ObjectKind scoped to JSON value wrappers)
- `G: EventGrammar`: Grammar marker (currently AnyGrammar only; SheetsEventGrammar exists as witness but unused in runtime)

**Variants:** None — ValueRef is a struct, not an enum. Holds (tape, cursor) + phantoms.

**Scoping:**
- `ValueRef<'doc, 'input, AnyKind, AnyGrammar>` is the generic erased form.
- JSON wrappers (JsonObject/Array/String) preserve specific K but still use AnyGrammar in practice.
- **Defect**: No per-grammar type-level scoping. SheetsEventGrammar exists but is never used to specialize ValueRef at runtime. W1 plan must add grammar parametrization to output views.

**Allocation strategy:**
- Zero allocation: ValueRef is (tape ref + u32 cursor) only; Copy + Clone.
- Tape holds all allocations (offset/flag vecs + payload arena).
- Safe for any grammar as long as tape structure is invariant.

**Cross-shape contamination:** Yes, defect identified.
- **Issue**: JsonObject/Array/String wrappers over ValueRef assume JSON-specific value dispatch (JsonNodeKind::at_cursor checks JSON byte patterns). Non-JSON grammars cannot reuse these wrappers without decoding byte checks.
- **Evidence**: `grammars/json/value.rs:29-46` (JsonNodeKind::at_cursor hardcodes JSON dispatch); `grammars/json/view.rs:105-112` (JsonObject::get assumes JSON string key spans).
- **Required fix (W1)**: Generate grammar-specific view wrappers (SheetsObject, CssValue, etc.) from templates, not shared JSON types.

---

## §5 Sheets-Ready Check

**Question**: Can Sheets grammar emission use the existing Value/Tape surface as-is without modifications?

**Verdict**: **NO. Minimal generic surface additions required.**

### Current blockers (non-minimal):

1. **JsonSink trait is JSON-only** (Leak #7). Sheets direct parse needs different callbacks (formula_start(), cell(), range_ref(), error()). Cannot override JsonSink to Sheets without changing generated parser.
   - **Fix**: Add grammar-parameter to sink emitter. Generate `SheetsDirectSink` trait per grammar in `sheets/sink.rs` template. Tape substrate unchanged.

2. **Value/ValueRef dispatch assumes JSON** (Leak #2, contamination in §4). JsonNodeKind::at_cursor uses JSON byte checks. Sheets has different primaries (=, cell refs, errors).
   - **Fix**: Generate grammar-specific NodeKind enum per grammar (SheetsNodeKind) and at_cursor dispatch table from FIRST sets. Tape/ValueRef unchanged; views generated per grammar.

3. **String/Number/Escape policies hardcoded** (Leaks #3, #4). Sheets doubled-quote escape and exponent-only numbers differ from JSON.
   - **Fix**: Generate escape/number/layout config structs from grammar metadata. Pass to generic span matcher functions (currently inline in JSON generated.rs). Tape/ValueRef/TapeBuilder unchanged.

### Required minimal additions to generic surface:

**Addition A: Generic escape/number config trait** (supports W1 Sheets preflight)

File: New `skinny/crates/runtime/src/tape/grammar_config.rs`

```rust
pub trait GrammarConfig {
    type EscapePolicy: Copy + Clone;
    type NumberPolicy: Copy + Clone;
    
    fn escape_policy() -> Self::EscapePolicy;
    fn number_policy() -> Self::NumberPolicy;
}
```

Modify `TapeBuilder::new()` signature:
```rust
impl<'input, C: GrammarConfig> TapeBuilder<'input, C> { ... }
```

**Addition B: Generated grammar metadata modules** (one per grammar)

Example: `skinny/crates/runtime/src/grammars/sheets/config.rs`

```rust
pub struct SheetsConfig;
impl GrammarConfig for SheetsConfig {
    type EscapePolicy = DoubledQuoteEscape;
    type NumberPolicy = SheetsNumber; // allows leading dot, exponent
    // ...
}
```

No runtime.rs or tape/mod.rs changes needed.

**Addition C: Parametrize view code generation** (emitter template change, no API addition)

Change `emit_with_layout()` in codegen to pass grammar name to `view_rs()` + `value_rs()` template functions instead of hardcoding JSON names. Generated modules still use same ValueRef<>, Tape<>, TapeBuilder<> types.

---

### Sheets-Ready Status

**Minimal additions needed**: YES (A + B + C above).
**New public API**: Only GrammarConfig trait + per-grammar config structs (internal to generated modules).
**No changes to**: Tape, ValueRef, TapeBuilder, OffsetFlags, ValueRef lifetime/borrow model, PayloadArena.
**Blocker**: Codegen must emit non-JSON baselines before Sheets runtime can compile (Leak #1 + #5: structural alphabet + key-value pair rule hardcoded in generated.rs). Lock 14 gate requires REDRESS 112 satisfied (generated non-JSON baseline exists) before Sheets preflight closes.

---

## §6 W2-Intervention API Risk (Scalar+SIMD Kernel Contact Points)

When W2 lands its selected scalar+SIMD kernel (bounded string span, escape decode, number matching, or byte-set classifier), where will it touch Value/Tape API?

**Contact Point 1: TapeBuilder for new flag schemes** (MEDIUM RISK)
- **Touch**: W2 SIMD string span kernel will call `TapeBuilder::patch_flags(cursor, flags)` to mark escape/control/encoding states in the tape.
- **Risk**: If W2 adds new flag bits (e.g., for SIMD-detected encoding classes), OffsetFlags bit semantics must be grammar-agnostic. Currently tied to JSON HAS_ESC/HAS_CONTROL.
- **Lock-14 safety**: Only if flag meanings are generated per-grammar metadata, not hardcoded in substrate. Generic OffsetFlags(u8) stays stable; grammar-generated SheetsFlags or CssFlags can interpret bit slots differently.
- **Mitigation (W2)**: Define `OffsetFlags` as generic bit slots; generate interpretation per grammar. Document flag bit allocation scheme per grammar.

**Contact Point 2: Tape.offset_at() and tape.source() reads** (LOW RISK)
- **Touch**: SIMD kernel reads tape offsets and source bytes to validate/scan spans.
- **Risk**: None if kernel remains backend-agnostic. Tape API is grammar-neutral (offsets are byte positions, flags are opaque bits).
- **Lock-14 safety**: Already safe. No change needed.

**Contact Point 3: PayloadArena for materialized values** (MEDIUM RISK)
- **Touch**: W2 escape-decode kernel may call `PayloadArena::write_bytes()` to stage decoded strings for host sink.
- **Risk**: If W2 adds eager materialization (REDRESS 54/55/60-69 preblocked per p2f §4.2), it re-opens rejected route. Payload writes must remain grammar-owned.
- **Lock-14 safety**: Safe only if payload writes are called from generated grammar modules, not from generic kernel. Generic kernel yields raw bytes; grammar module decides sink target.
- **Mitigation (W2)**: Kernel is source-only (raw span + flags return); materialization stays per-grammar host function.

**Contact Point 4: ValueRef lifetime / borrowing semantics** (LOW RISK)
- **Touch**: W2 kernel does not directly consume ValueRef (SIMD operates on source bytes). If W2 builds a new view type (SheetsSpanRef, etc.) it would use same 'doc/'input lifetime model.
- **Risk**: None if new types follow same pattern.
- **Lock-14 safety**: Already safe. Lifetime model is generic.

**Contact Point 5: Value enum (JSON-only type)** (HIGH RISK — but out of W2 scope)
- **Touch**: W2 scalar/SIMD kernel does NOT produce Value enums (those are JSON-specific). W2 output is raw spans + flags or sink calls.
- **Risk**: If W2 tries to unify value representation across grammars (e.g., a generic Value enum), it violates Lock 14 (grammar-specific types must be generated, not generic).
- **Lock-14 safety**: Safe only if Value type remains per-grammar generated code. Generic substrate has NO value enum.
- **Mitigation**: Ensure W2 plan document states that W2 kernel outputs are spans/flags/sink calls, not typed Value. Generated value types stay per grammar.

---

**Summary: W2 contact risk is MEDIUM if W2 adds new OffsetFlags semantics without grammar-generated interpretation.** Lock-14 requires mitigation documented in W2 plan: new flag bits must be grammar-interpreted, not substrate-hardcoded.

