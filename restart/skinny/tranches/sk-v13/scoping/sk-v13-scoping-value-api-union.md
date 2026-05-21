# SK-V13 Scoping: Value API + GrammarConfig + Union Substrate Post-W1a

Date: 2026-05-21.
Author: Read-only research; no source edits.
Status: W1a closed; union category (USER PIN D3) and SIMD category (USER PIN D4) unblocked.

---

## §1 GrammarConfig Surface As-Shipped

### Trait Definition

**Status**: No public trait. W1a implemented a `pub(crate)` generated metadata surface instead.

**Shape** (JSON example, `skinny/crates/runtime/src/grammars/json/config.rs`):

```rust
pub(crate) const STRUCTURAL_BYTES: &[u8] = b"{}[],:\"";
pub(crate) const STRING_NEEDS_DECODE: u8 = OffsetFlags::HAS_ESC;
pub(crate) const TINY_STRING_CAP: usize = 16;
pub(crate) const DIRECT_TINY_STRING_CAP: usize = 8;
pub(crate) const TRUE_LITERAL: &[u8] = b"true";
pub(crate) const FALSE_LITERAL: &[u8] = b"false";
pub(crate) const NULL_LITERAL: &[u8] = b"null";

#[inline(always)]
pub(crate) fn needs_decode_flags() -> OffsetFlags {
    OffsetFlags::NONE.with(STRING_NEEDS_DECODE)
}
```

**Per-grammar config structs**: Located in `grammars/{json,css_l4_declaration_values}/config.rs`.
- Each generated module owns its structural bytes, literal patterns, flag semantics, and decode policy.
- No public API exported; config is `pub(crate)` and consumed only by generated grammar modules.

**W1a contract**: "Generated metadata moves JSON policy behind per-grammar config modules rather than exposing a public generic `GrammarConfig` trait" (CONSOLIDATED.md §2.1, A2-runtime-grammar-config.md §3).

### Generated Parser Consumption

**File**: `skinny/crates/codegen/src/json_templates/generated.rs:1-15`

```rust
use super::config;
// recognizer: SimdScan Exact PreEntry alphabet="{}[],:\""
pub(crate) fn attach_structural_index(state: &mut ParserState<'_>) {
    debug_assert_eq!(config::STRUCTURAL_BYTES, b"{}[],:\"");
    let _ = state;
}
```

Parser calls `config::STRUCTURAL_BYTES`, `config::needs_decode_flags()`, `config::TRUE_LITERAL`, etc.
- Generated parser remains generic; policy injected via module-private config import.
- No parser-internal hardcoded alphabets; all dispatch/literal/flag semantics are config-driven.

---

## §2 Lock-14 Leak Status Delta (Post-W1a)

### Leak Resolution Inventory

Seven leaks named in `skv12-value-api-audit.md §3` (Lines 63-107):

| # | Leak | Category | Status | Evidence | File:Line |
|---|------|----------|--------|----------|-----------|
| 1 | Structural alphabet hardcoding | CRITICAL | **RESOLVED** | JSON: `config::STRUCTURAL_BYTES = b"{}[],:\""`; generated parser calls config, not hardcoded. CSS: same pattern. | `json/config.rs:4`, `generated.rs:13` |
| 2 | Value dispatch hardcoding | MAJOR | **PARTIAL** | Dispatch remains hardcoded in `generated.rs:45-56` as `match byte { b'{' => ..., b'[' => ...`. Config provides LITERALS but dispatch table is still inline. | `generated.rs:45-56` |
| 3 | String escape/quote policy | MAJOR | **PARTIAL** | `needs_decode_flags()` defers to config, but string matching (`match_tiny_plain_string`, `match_string_at_quote_trusted_utf8`) hardcodes JSON quote=`b'"'`, escape=`b'\\'`. Only flag placement changed. | `json/config.rs:5`, `generated.rs:93-100` |
| 4 | Number policy | MAJOR | **PARTIAL** | `match_number_span_from_first` is called generically, but implementation in `parse-that-regex` crate hardcodes JSON sign/dot/zero rules. No per-grammar number config in runtime. | `generated.rs:213-215` |
| 5 | Key quoting assumption | MAJOR | **PARTIAL** | `parse_key_colon()` remains hardcoded as `parse_quote_at_cursor() + match_string_at_quote()`. No grammar-specific pair rule; JSON object/pair structure baked in. | `generated.rs:88-115` |
| 6 | OffsetFlags semantics | EMBEDDED | **UNRESOLVED** | `HAS_ESC` and `HAS_CONTROL` remain in `tape/mod.rs:22-23` with JSON meanings. No per-grammar flag scheme; CSS config.rs contains only metadata constants, not flag definitions. | `tape/mod.rs:18-36`, `json/config.rs:5` |
| 7 | JsonSink trait hardcoding | EMBEDDED | **UNRESOLVED** | Sink is still JSON-specific (`grammars/json/sink.rs:3-119`). No grammar-parametrized sink trait; CSS L4 config.rs has NO sink binding. CSS direct parse is unimplemented. | `grammars/json/sink.rs:4-15` |

### Residual JSON Policy in Nominally-Generic Code

**Leak #2 residual**: `dispatch_value()` function in `generated.rs:45-56` lists JSON FIRST bytes inline:

```rust
fn dispatch_value<'i>(state: &mut ParserState<'i>, byte: u8) -> Result<(), ParseError<'i>> {
    match byte {
        b'{' => parse_object(state),
        b'[' => parse_array(state),
        b'"' => parse_string(state),
        b'-' | b'0'..=b'9' => parse_number(state, byte),
        b't' => parse_literal(state, config::TRUE_LITERAL, JsonNodeKind::True),
        b'f' => parse_literal(state, config::FALSE_LITERAL, JsonNodeKind::False),
        b'n' => parse_literal(state, config::NULL_LITERAL, JsonNodeKind::Null),
        _ => return Err(error(state, ParseErrorKind::ExpectedValue)),
    }
}
```

CSS L4 requires different FIRST dispatch (e.g., whitespace/comment/semicolon primaries). Current template cannot emit non-JSON dispatch without hand-specialization.

**Leak #3 residual**: String matching is tied to JSON semantics. `match_tiny_plain_string()` hardcodes:
- Terminator: `b'"'` (JSON)
- Escape: `b'\\'` (JSON backslash model)
- Control threshold: `0x00..=0x1f` (JSON forbidden bytes)

See `generated.rs:159-183`. No per-grammar string config struct; Sheets and CSS would need separate emitters.

**Leak #5 residual**: `parse_key_colon()` is JSON-specific. Assumes key is quoted string + colon separator. CSS properties have unquoted names; Sheets has cell references.

**Leak #6 residual**: `OffsetFlags` in `tape/mod.rs:22-24` names bits with JSON semantics:
```rust
pub const HAS_ESC: u8 = 0x01;
pub const HAS_CONTROL: u8 = 0x02;
```

No grammar-generated flag interpretation layer. CSS escape model (context-dependent; no backslash in strings) cannot reuse these named constants without semantic collision.

**Leak #7 residual**: `JsonSink` trait is hardcoded to JSON value callbacks. CSS requires `begin_declaration`, `property_name`, `property_value`, `end_declaration`. No generated sink trait per grammar; CSS config.rs does not define a sink shape.

### Resolution Count

- **RESOLVED**: 1/7 (structural alphabet only)
- **PARTIAL**: 4/7 (literals, policy constants, generic function calls present but underlying implementations hardcoded)
- **UNRESOLVED**: 2/7 (OffsetFlags semantics, JsonSink trait)

---

## §3 Value / ValueRef / Tape Surface Post-W1a

### Live API Shape

**Tape** (`skinny/crates/runtime/src/tape/mod.rs:94-101`):

```rust
pub struct Tape<'input> {
    source: &'input [u8],
    offsets: Vec<u32>,
    flag_cursors: Vec<u32>,
    flag_values: Vec<u8>,
    payloads: PayloadArena,
    id: TapeId,
}
```

**ValueRef** (`skinny/crates/runtime/src/tape/mod.rs:175-181`):

```rust
pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar> {
    tape: &'doc Tape<'input>,
    cursor: u32,
    _kind: PhantomData<fn() -> K>,
    _grammar: PhantomData<fn() -> G>,
    _input: PhantomData<&'input [u8]>,
}
```

**OffsetFlags** (`skinny/crates/runtime/src/tape/mod.rs:16-36`):

```rust
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
#[repr(transparent)]
pub struct OffsetFlags(pub u8);

impl OffsetFlags {
    pub const NONE: Self = Self(0);
    pub const HAS_ESC: u8 = 0x01;
    pub const HAS_CONTROL: u8 = 0x02;
    // ...
}
```

### Grammar-Neutrality Verdict

**Status**: **Not grammar-neutral; still JSON-presupposing.**

**Evidence**:
1. `ValueRef` has a `G: EventGrammar` phantom, but JSON wrappers (`JsonRoot`, `JsonObject`, `JsonArray`, `JsonString` in `grammars/json/view.rs`) do not specialize it. They use `ValueRef<..., AnyGrammar>` (line 67-80 of `view.rs`).
2. No grammar-specific `ValueRef<..., CssEventGrammar>` wrappers exist in runtime yet.
3. `OffsetFlags` bit names (`HAS_ESC`, `HAS_CONTROL`) carry JSON semantics hardcoded in the type definition, not per-grammar config.
4. No grammar-generated flag interpretation layer (e.g., `CssFlags` struct that maps bit 0x01 to a different meaning).
5. Direct parse for CSS remains unimplemented; `JsonSink` is the only sink trait.

**Consequence**: CSS L4 grammar can use `Tape<'input>` and `ValueRef` for storage, but value dispatch, string escape, and sink semantics are still JSON-owned. W1b-1 uses CSS only as a parsing target for throughput measurement; it does not reuse JSON value wrappers or dispatch.

---

## §4 CSS L4 Sufficiency Check

### Question

Does the post-W1a surface let SK-V13 add CSS L4 productions (selectors, at-rules, full stylesheet) WITHOUT touching GrammarConfig, or are extensions needed?

### Answer

**Status**: **Extensions required. GrammarConfig (or config module expansion) is insufficient alone.**

### Missing Dimensions

1. **Dispatch Table**: CSS L4 selectors start with `.`, `#`, `*`, `[` (attribute), `@` (at-rule), or whitespace. Current `dispatch_value()` hardcodes JSON FIRST set. A grammar-driven dispatch table is needed.

   **Required**: `config::CSS_DISPATCH_TABLE` or generated `dispatch_css_selector()` function.

2. **String/Escape Policy**: CSS properties use context-specific escapes (e.g., unquoted names can have backslash-sequences, but not JSON-style `\"` escapes). Current `match_tiny_plain_string` assumes JSON quote/backslash.

   **Required**: Generated `css_match_property_name()` and `css_match_string_value()` functions per-grammar.

3. **Whitespace/Comment Policy**: CSS requires comment-aware scanning and complex whitespace rules (not just `skip_ascii_whitespace`). Layout policy is hardcoded as `skip_ascii_whitespace(state.bytes, cursor)`.

   **Required**: `config::skip_whitespace_and_comments: fn(&[u8], usize) -> usize` callback.

4. **Number Policy**: CSS numbers allow leading dot (`.5em`), which JSON forbids. `match_number_span_from_first` is JSON-tuned.

   **Required**: Generated `match_css_number()` or a `config::NUMBER_POLICY` struct with dot/exponent/unit rules.

5. **Literal/Keyword Policy**: CSS keywords (e.g., `auto`, `inherit`, `none`) differ from JSON literals. Current `parse_literal()` is generic in name but paired with JSON literal names in config.

   **Required**: `config::CSS_KEYWORDS` table and a `dispatch_css_keyword()` function.

6. **Sink Trait**: CSS direct parse needs `begin_declaration()`, `property_name()`, `property_value()`, `end_declaration()` — not JSON's `begin_object`, `key`, `string`, `i64`.

   **Required**: Generated `CssDirectSink` trait in `css_l4_declaration_values/sink.rs`.

### Concrete New Trait Methods / Config Fields

**Expansion to runtime GrammarConfig trait (if public):**

```rust
pub trait GrammarConfig {
    // Already exist implicitly in per-grammar config module:
    const STRUCTURAL_BYTES: &'static [u8];
    
    // NEW required for CSS L4 and future grammars:
    fn dispatch_value(byte: u8) -> Option<ParseAction>;  // Maps byte → parse rule
    fn skip_whitespace_and_comments(input: &[u8], cursor: usize) -> usize;
    fn match_string(input: &[u8], quote: u8, escape_char: Option<u8>) -> Option<StringSpan>;
    fn match_number(input: &[u8], cursor: usize) -> Option<NumberSpan>;
    type DirectSink: (?Trait);  // Trait associated type for grammar sink
}
```

**Practical SK-V13 approach** (avoid expanding public API per W1a exit gate):

- Emit per-grammar modules with these functions as `pub(crate)` code generation artifacts.
- No public trait; code generation drives specialization.
- Examples: `css_l4_declaration_values/config.rs` + `css_l4_declaration_values/generated.rs` with CSS-specific dispatch/whitespace/string/number/sink functions.

---

## §5 Rust-Union Substrate Legality Under GrammarConfig

### Question

Can the current GrammarConfig surface host a union substrate variant (e.g., EventTape ⊕ OffsetTape selected per rule) without a new directive / BIR variant / `BackendShape` enum addition?

### Answer

**Verdict**: **YES, with strong caveats.**

### Legal Route

1. **No new directive**: Union variant selection must be driven by grammar-internal rules or a per-grammar `config::UNION_POLICY` constant, not a new `@union` directive in the IR.

   **SPEC §1**: "W1a exit forbids a new directive" (SPEC.md:341-347). Union selection at code-gen time from existing IR (rule name, arity, nesting depth) is legal.

2. **No new BIR variant**: Union shape selection (e.g., "rule `declaration_value` uses EventTape, rule `selector` uses OffsetTape") must be recorded in a generated config module or codegen-private data structure, not in the BIR enum.

   **SPEC §1**: "W1a exit forbids ... BIR variant" (SPEC.md:341-347). The BIR remains a single-substrate semantic IR; multi-substrate routing is a codegen concern.

3. **No `BackendShape` variant**: The output `BackendShape` enum (which selects `Json / CssL4 / Sheets / BBNF`) is not expanded. Union is a per-grammar implementation detail, not a new shape category.

   **SPEC §5 / §8**: Union substrate is a "same-tape projection" (SPEC.md:614), not a new output plane.

4. **Runtime structure stability**: `Tape<'input>`, `ValueRef<'doc, 'input, K, G>`, `OffsetFlags`, `PayloadArena`, `TapeBuilder` have no breaking changes. Union variants share the same tape structure; they just interpret tape contents differently at codegen time.

   **A2-runtime-grammar-config.md**: "Keep `Tape`, `TapeBuilder`, `ValueRef`, `OffsetFlags`, and `PayloadArena` storage unchanged" (lines 229-230).

### Binding Clauses

**SPEC §1 (Lock 14 / Generic Crate Neutrality)**:
```
W1a exit forbids:
- A new directive (e.g., @union_strategy)
- A new BIR variant (e.g., BirStatement::Union)
- A new BackendShape variant
- A public tape/substrate API change (e.g., pub trait GrammarConfig)
```

Union selection must remain **codegen-private** (e.g., inside `codegen/src/lower/` or `codegen/src/css_l4_declaration_values_templates/`), and substrate choice must be driven by grammar metadata or per-grammar config, not a directive.

**SPEC §8 (W3 Pre-Blocks)**:
```
"Plan proves no sidecar substrate, no parser-owned cursor/list, 
no parallel UnionTape, and no retained decoded-byte/class side vector."
```

A legal union substrate variant must:
- Share the single `Tape<'input>` structure (no `UnionTape` type).
- Avoid parser-owned unions (routing is compile-time via codegen, not runtime type selection).
- Have no sidecar vectors (all state is in the shared tape).

### Legality Conclusion

**YES**: A union substrate can be materialized under current `GrammarConfig` (post-W1a) as long as:
1. Union selection (which rule uses which tape variant) is recorded in per-grammar `config` modules or codegen-private tables, not a new directive.
2. No new `BackendShape` enum variant.
3. No new BIR variant.
4. `Tape`, `ValueRef`, and `TapeBuilder` APIs remain unchanged.
5. Runtime union decision is **compile-time per rule** (driven by codegen, not runtime dispatch).

---

## §6 Three Candidate Union-Substrate W3 Attempts

### Material Differential Requirement

**USER PIN D3** (lines 39-56): "New implementations attempting the same architectural goal MAY dispatch under the standard scalar reference + parity/checkasm + same-wave consumer gate. A new implementation must cite the prior REDRESS, name the material differential, and pass CHALLENGE."

**REDRESS 96/97/98** (SPEC.md:754): Prior measured-rejected attempts:
- REDRESS 96: Class-column union (V1)
- REDRESS 97: Streaming cursor union (V2)
- REDRESS 98: Class-lane union (V3)

---

### Candidate C1: GrammarConfig-Driven Per-Rule Shape Selection

**Concept**:
At codegen time, the runtime `TapeBuilder` selects EventTape vs OffsetTape for a rule based on rule metadata in grammar `config`:

```rust
// In css_l4_declaration_values/config.rs
pub(crate) struct RuleSubstratePolicy {
    pub rule_id: u32,
    pub use_event_tape: bool,  // if true, emit EventTape variant; else OffsetTape
}

pub(crate) const RULE_POLICIES: &[RuleSubstratePolicy] = &[
    RuleSubstratePolicy { rule_id: 1, use_event_tape: true },   // declaration_value
    RuleSubstratePolicy { rule_id: 2, use_event_tape: false },  // selector
];
```

The generated parser calls `config::should_use_event_tape(rule_id)` and configures the tape builder accordingly.

**Differential vs REDRESS 96/97/98**:
- REDRESS 96 was class-column (side vector per structural class).
- REDRESS 97 was streaming cursor (runtime routing to two types).
- **C1 is codegen-time routing** (no side vector, compile-time monomorphism per rule).

**Expected LOC**: ~150-200 lines (config struct, per-rule codegen dispatch logic, conditional TapeBuilder construction).

**CHALLENGE Risk**: *MEDIUM*. Must prove that per-rule substrate selection does not leak across rule boundaries and that tape merging (output of one rule fed to another) preserves semantics.

**Target Row**: `css_l4/declaration_values/direct_to_struct/main` (CSS L4 declared value parsing).

---

### Candidate C2: E-graph-Rewritten Union (Resolver Fold)

**Concept**:
The decision-engine fold (from "agent 5's audit") performs e-graph rewriting on the IR to identify equivalence classes of rules. Rules in the same equivalence class use the same tape variant; rules in different classes may use different variants. The resolver assigns tape variant per equivalence class, and codegen emits specialized parsers.

Example: If `declaration_value` and `dimension` rules parse to equivalent structural patterns (both have `[key=value, separator, payload]`), they are one equivalence class → same tape variant. If `selector` is structurally distinct, it's a different class → optionally different variant.

**Differential vs REDRESS 96/97/98**:
- REDRESS 96/97/98 were **ad-hoc per-rule choices** (no structural equivalence analysis).
- **C2 uses structural e-graph equivalence** to make data-driven assignments (provably optimal subset of rules per variant).

**Expected LOC**: ~400-600 lines (e-graph construction, equivalence-class resolution, codegen emission of per-class tape specialization).

**CHALLENGE Risk**: *HIGH*. E-graph rewriting is a new solver; must prove convergence, soundness (equivalence classes preserve semantics), and competitive advantage over C1's simpler model.

**Target Row**: Same row `css_l4/declaration_values/direct_to_struct/main`, but with measured throughput Delta to show e-graph routing beats per-rule C1.

---

### Candidate C3: ARMv9.2 SIMD-First Union (Vector Lane Index)

**Concept**:
SIMD primitives (`PMULL` prefix-XOR, `CSSC` count-trailing-zeros, `EOR3` triple-XOR) build a structural index in vector lanes during the scan phase. The scalar `consume_structural` function consults this vector index to decide: per-structural position, select EventTape mode (dense structural updates) vs OffsetTape mode (sparse offsets). The vector lane index is computed once per 64-byte SIMD block and reused across rule invocations within that block.

**Differential vs REDRESS 96/97/98**:
- REDRESS 88/89/90 touched primitive rewrite (PMULL/CSSC/EOR3) but not union routing.
- **C3 couples SIMD index construction (W4-eligible primitives) with union selection** (vector lanes directly drive tape mode per structural position, not per rule).
- Deletes scalar `consume_structural` only after vector index validation passes parity.

**Expected LOC**: ~300-400 lines (vector index computation in scan phase, lane-based mode selection in consume_structural, validation harness).

**CHALLENGE Risk**: *VERY HIGH*. Couples W4 ASM-gen primitives (PMULL/CSSC/EOR3) to union routing. Requires W2/W4 to complete first, and a fresh parity proof that vector lane decisions match scalar equivalence-class decisions.

**Target Row**: Same row, but demonstrating SIMD index reduces `consume_structural` overhead and throughput Delta vs C1/C2.

---

### Candidate Summary Table

| Candidate | Mechanism | Diff vs 96/97/98 | Expected LOC | CHALLENGE Risk | Target Evidence |
|-----------|-----------|---|---|---|---|
| **C1** | Codegen-time per-rule config | Compile-time, monomorphic routing | 150-200 | MEDIUM | Parity across rule boundary, no semantic loss |
| **C2** | E-graph equivalence classes | Structural e-graph analysis + resolver fold | 400-600 | HIGH | E-graph convergence, equivalence preservation, Delta to C1 |
| **C3** | SIMD lane index (W4 coupling) | Vector lane-driven routing, scalar deleted post-validation | 300-400 | VERY HIGH | Parity of vector lanes vs scalar, W2/W4 prerequisite, SIMD Delta |

---

## §7 W2 escape_mask_64 Fix

### Xorshift Falsifier Resolution

**Historical bug**: `escape_mask_64` function in `skinny/crates/bbnf-simd/src/lib.rs:175-195` had a carry-propagation defect triggered by the xorshift seed `0xCAFEF00DBAADF00D` (used in adversarial testing).

**Current status** (W2 CONSOLIDATED.md):
> "The historical `escape_mask_64` bug is no longer reproducing in the existing strict scanner parity harness at HEAD."

### Source Diff Location

**File**: `skinny/crates/bbnf-simd/src/lib.rs:175-195`

**Implementation** (current, post-W2):
```rust
pub fn escape_mask_64(bs_mask: u64, bs_carry_in: bool) -> (u64, bool) {
    const EVEN_BITS: u64 = 0x5555_5555_5555_5555;
    const ODD_BITS: u64 = 0xAAAA_AAAA_AAAA_AAAA;

    let starts = bs_mask & !(bs_mask << 1);
    let carry_continues = bs_carry_in && (bs_mask & 1) == 1;
    let starts_eff = if carry_continues { starts & !1 } else { starts };
    let (even_starts, odd_starts) = if carry_continues {
        (starts_eff & EVEN_BITS, (starts_eff & ODD_BITS) | 1)
    } else {
        (starts_eff & EVEN_BITS, starts_eff & ODD_BITS)
    };

    let (even_carries, _) = bs_mask.overflowing_add(even_starts);
    let even_escape = (even_carries & !bs_mask) & ODD_BITS;
    let (odd_carries, _) = bs_mask.overflowing_add(odd_starts);
    let odd_escape = (odd_carries & !bs_mask) & EVEN_BITS;
    let mut escape = even_escape | odd_escape;

    if bs_carry_in && (bs_mask & 1) == 0 {
        escape |= 1;
    }
    // ... carry_out logic follows
}
```

### Checkasm Parity Status

**Test**: `skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs`

**Result** (W2 CONSOLIDATED.md, line 14-20):
```
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_parity -- --nocapture
passed, including `classifier_corpus_parity`.
```

**Verdict**: **Parity PASS** at current HEAD. Xorshift falsifier `0xCAFEF00DBAADF00D` no longer triggers a divergence.

### W2 Evidence Status

**Gap** (W2 CONSOLIDATED.md, lines 24-29):
> "Current coverage has two gaps:
> 1. There is no direct `escape_mask_64(bs_mask, carry_in)` differential test.
> 2. The caller-level JSON scanner adversarial windows are not explicitly pinned."

**Recommendation** (lines 32-45):
- Add a dedicated `checkasm_escape_mask_64` test with direct coverage for carry-in/out, bit-0 continuation, bit-63 trailing runs.
- Add caller-level JSON scanner parity for the xorshift falsifier seed, long backslash runs, residual tails, alignments.
- Run strict checkasm, check-json, check-conformance.

**Status**: W2 does NOT require fresh source edits to the `escape_mask_64` implementation; the bug is resolved. W2 requires **proof artifacts** (dedicated tests, parity reports) before SIMD admission (W4).

---

## §8 SK-V13 Value-API Wave Shortlist

### Rationale

The post-W1a surface has:
1. Per-grammar config modules (JSON, CSS L4) to hold policy constants.
2. No public `GrammarConfig` trait (W1a exit gate compliance).
3. 4/7 leaks partially resolved (literals + policy deferred); 3/7 remain (dispatch, escape, sink).
4. Union substrate legal (C1/C2/C3 candidates viable) but codegen infrastructure incomplete.

SK-V13 must choose between:
- **Option A**: Extend per-grammar config modules to cover dispatch/escape/sink, support non-CSS grammars (Sheets, BBNF).
- **Option B**: Commit to single-substrate (offset-only) for all grammars, defer union to SK-V14+.
- **Option C**: Add public `GrammarConfig` trait (violates W1a exit gate but unblocks API reuse).

### Recommended Wave Shortlist (Assuming Option A + Union C1)

#### Wave 1: Per-Grammar Config Module Expansion (GrammarConfig Phase 2)

**Scope**: Move dispatch, string matching, number parsing, and literal lookup from generated.rs into per-grammar config modules.

**Owners**: codegen (emitter templates), runtime (config.rs per grammar).

**Artifacts**:
- `json/config.rs`: Add `dispatch_value()`, `match_json_string()`, `match_json_number()`, `JSON_KEYWORDS`.
- `css_l4_declaration_values/config.rs`: Add `dispatch_declaration_value()`, `match_css_string()`, `match_css_number()`, `CSS_KEYWORDS`.
- `sheets/config.rs` (new): Add `dispatch_formula()`, `match_formula_string()`, `match_sheet_number()`, `SHEET_KEYWORDS`.
- Generated templates: Call config dispatch functions instead of inline matches.

**Target Rows**: 
- JSON parse (existing guard floor).
- CSS L4 (existing measured row).
- Sheets parse (new).

**Estimated LOC**: 400-600 (config modules) + 200-300 (template changes).

---

#### Wave 2: Grammar-Specific View / Sink Emission (Value API Phase 2)

**Scope**: Generate grammar-specific `NodeKind` enums and `DirectSink` traits per grammar, replacing JSON-only wrappers.

**Owners**: codegen (new emitter: `view_templates.rs`, `sink_templates.rs`), runtime (generated view/sink per grammar).

**Artifacts**:
- `json/value.rs` (update): Generated from template, not hand-written.
- `json/sink.rs` (update): Generated from template.
- `css_l4_declaration_values/value.rs` (new): `CssNodeKind` enum, `CssValue` wrappers.
- `css_l4_declaration_values/sink.rs` (new): `CssDirectSink` trait.
- `sheets/value.rs` (new), `sheets/sink.rs` (new): Grammar-specific views.

**Target Rows**: CSS L4, Sheets (new).

**Estimated LOC**: 600-800 (emitter templates) + 300-400 (generated modules per grammar).

---

#### Wave 3: Union Substrate C1 Attempt (Candidate Selection + Codegen)

**Scope**: Implement C1 (GrammarConfig-driven per-rule shape selection) for CSS L4 as a measured W3 attempt.

**Owners**: codegen (union routing logic), runtime (dual-tape support), benchmark (throughput Delta).

**Artifacts**:
- `css_l4_declaration_values/config.rs`: Add `RuleSubstratePolicy` table.
- Generated CSS parser: Conditional `TapeBuilder::new_event()` vs `TapeBuilder::new_offset()` per rule.
- Benchmark: Measure CSS L4 with C1 union vs. post-W1b-2b OffsetTape baseline.

**Gate**: Throughput Delta > 0, parity hold, JSON guards unchanged.

**Target Row**: CSS L4 (existing).

**Estimated LOC**: 150-250 (config + codegen logic).

---

#### Wave 4: Escape / Number / OffsetFlags Grammar-Neutrality (Remaining Leaks 3,4,6)

**Scope**: Remove JSON hardcoding from string/number/flag semantics. Requires public or `pub(crate)` grammar configuration trait.

**Decision Point**: Accept the W1a exit-gate violation (public `GrammarConfig` trait) or defer this wave.

**Artifacts** (if proceeding):
- `tape/grammar_config.rs` (new, public): `pub trait GrammarConfig { type EscapePolicy; type NumberPolicy; type FlagPolicy; }`.
- `json/grammar_config_impl.rs` (new): JSON impl of GrammarConfig.
- `css_l4_declaration_values/grammar_config_impl.rs` (new): CSS impl.
- `parse-that-regex/src/` (update): Accept generic escape/number policies.

**Gate**: SPEC revision (W1a exit gate must be re-negotiated).

**Estimated LOC**: 200-400 (trait + impls).

---

#### Wave 5: Union Substrate C2 Attempt (E-graph Analysis)

**Scope** (conditional on C1 success): Implement C2 (e-graph equivalence-class driven union) for CSS L4 as a W3+ attempt.

**Gate**: C1 provides a baseline; C2 must show structural e-graph analysis and prove equivalence + measured Delta.

**Estimated LOC**: 400-600 (e-graph resolution).

---

### Summary Table

| Wave | Title | Scope | Owners | LOC | Gate | Prerequisite |
|------|-------|-------|--------|-----|------|---|
| **W1** | Per-Grammar Config Expansion | Dispatch, string, number, keywords | codegen, runtime | 600-900 | JSON guard + CSS parity | W0 (W1a) |
| **W2** | Grammar-Specific Views & Sinks | NodeKind + DirectSink per grammar | codegen, runtime | 900-1200 | CSS parity, Sheets measure | W1 |
| **W3** | Union Substrate C1 (Per-Rule) | GrammarConfig-driven shape selection | codegen, runtime, bench | 150-250 | CSS Delta > 0, parity | W2 |
| **W4** | Escape / Number / OffsetFlags (Leak 3,4,6) | Grammar-neutral policies | codegen, runtime, parse-that-regex | 200-400 | SPEC revision (W1a waiver) | W3 |
| **W5** | Union Substrate C2 (E-graph) | Equivalence-class driven selection | codegen, solver, bench | 400-600 | C1 baseline + equivalence proof | W3/W4 |

---

## Appendix: Scoping Decision

### Option Summary

**Option A (Recommended)**: Pursue Waves 1–3 (per-grammar config expansion, grammar-specific views/sinks, C1 union). Defer Waves 4–5 (escape/number/flags, C2 union) to SK-V14 or contingent on SPEC revision.

**Rationale**:
- Unblocks CSS L4 + future Sheets/BBNF grammars without W1a exit-gate violation.
- C1 union is simplest and requires no solver or e-graph infrastructure.
- Leaves 3/7 leaks (escape, number, flags, sink) for future waves, acceptable since they are partially mitigated by per-grammar config.

**Option B (Conservative)**: Pursue Wave 1–2 only; skip union substrate (C1/C2/C3) entirely for SK-V13. Defer to SK-V14+ pending CHALLENGE feedback on union legality and C1 feasibility.

**Rationale**:
- Lowest risk; guarantees no W1a gate violation.
- Focuses on grammar-neutrality of core Value/Tape surface.
- Union deferred; leaves USER PIN D3 unexercised.

**Option C (Aggressive)**: Extend W1a by adding a **public `GrammarConfig` trait** and move ALL 7 leaks (dispatch, escape, number, flags, sink) into trait methods. Pursue C2 or C3 union.

**Rationale**:
- Complete grammar-neutrality; single, unified API for all future grammars.
- Highest upside (C2/C3 union) but requires SPEC waiver for W1a exit gate.
- Risk: Introduces public surface that must be stable.

---

**End of scoping document. Recommend Option A for SK-V13 dispatch.**

