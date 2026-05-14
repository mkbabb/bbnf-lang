# Skinny Spec — Compiler Slice

The compiler slice owns the compile-time path from a single grammar source
(`grammars/json.bbnf`) through to emitted Rust under
`runtime/src/grammars/json/`. The skinny exists to validate the V1 SOTA-beat
premise on JSON before tranches A-J commit, so this slice deletes every V1
compiler crate that JSON does not exercise and states the per-skip impact.

This slice composes with three siblings:

- SUBSTRATE (`SUBSTRATE.md`) — `Tape<'input>`, `ValueRef<'i>`, `JsonRoot<'i>`,
  payload arena, SIMD integration contract.
- BENCH (`BENCH.md`) — dual-track measurement (generated parser vs hand-coded
  JSON parallel), reproducibility schema, go/no-go thresholds.
- WORKSPACE (`WORKSPACE.md`) — `Cargo.toml`, member list, per-crate LOC
  budgets for the skinny, build/test commands.

Anything those slices own is referenced here by **contract only**. The compiler
emits Rust that names `JsonRoot<'i>`, `Tape<'input>`, `ValueRef<'i>`, the
structural-alphabet kernel, and the payload arena; it does not specify their
internal layout. SUBSTRATE owns the layout.

The compiler slice is for **one grammar (JSON)** and the **minimal HM** that
JSON requires. The full V1 stack adds DK13 higher-rank, GADT branch-local
equality, finite CSP, e-graph rewrites, recognizer mining, cost-model
extraction, VM replay, language server, and `path-core`. None of those are
reachable from a JSON parse and none are part of this slice.

---

## 1. `json.bbnf` Source Sketch

The skinny ships a host-fn-free JSON grammar as a deliberate deviation from the
V1 JSON row. ARCH §12.2 gives full V1 JSON metadata plus numeric/string host
fns from `host::primitives`; the committed `grammar/json/json.bbnf` decodes
string escapes through `decode_json_string_to_arena(input) -> String`. The
skinny removes that `@host fn` surface and keeps string/number materialisation
lazy in SUBSTRATE-owned accessors so the SOTA hot path measures structure parse,
not scalar extraction.

### 1.1 Skinny grammar text

```bbnf
null   = "null" ;
bool   = "true" | "false" ;

number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;

string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;

ws     = /[ \t\n\r]*/ ;

comma  = "," ws ;
colon  = ":" ws ;

value  = ws (object | array | string | number | bool | null) ws ;

pair   = string ws colon value ;
member = pair (comma pair)* ;
members = member? ;

elements = (value (comma value)*)? ;

array  = "[" ws elements "]" ;
object = "{" ws members "}" ;

json   = ws value ws ;
```

The grammar keeps `value = ws (...) ws` as the semantic source shape. Codegen is stricter: emitted `parse_value` consumes leading whitespace only, and callers consume trailing whitespace at root, separator, closer, and EOF boundaries. This preserves the grammar language while avoiding repeated trailing layout scans inside nested JSON values. A whitespace-bearing parse-index variant was measured and rejected for the skinny because it materially regressed throughput.

Differences from the committed `grammar/json/json.bbnf` and why:

| Difference | Reason |
|---|---|
| No `-> 0u8`, `-> true`, `-> false`, `-> f64` map tails. | `MapTail` lowers to `Call(kind: Map)` → `ValueProject`. The skinny does not need typed scalar projections to measure SOTA throughput; the typed root keeps raw spans and lazy accessor methods. ShapeFacts mining is replaced by hand-curated shapes. |
| No `decode_json_string_to_arena(input) : String` host call. | Strings emit `RegexProgram` against the validator regex; the tape records the raw string span plus `STRING_NEEDS_UNESCAPE` when needed. `JsonString::as_str()` lazily returns `Cow<'input, str>` through SUBSTRATE, outside the parse-time SOTA measurement. |
| `?w` collapsed to explicit `ws`. | The `?w` whitespace marker is sugar for an `@layout(ws = ...)` policy. The skinny does not lower `LayoutDirective` and does not own a `passes::layout` HM-internal subroutine. ARCH §8.2 reads HM-equality as a layout-lowering subroutine; in the skinny, layout is a no-op pass-through and HM runs as a top-level pass. To keep the grammar legal without `@layout`, whitespace is desugared to an explicit `ws` rule that lowers to a `RegexProgram` discarded before `TapeEmit`. |
| `>>` / `<<` suppression operators replaced with explicit `ws`. | Same reason as `?w`. The suppression operators are layout-policy sugar; the skinny grammar treats whitespace as ordinary rule body. |
| `@pretty object group ;` etc. removed. | Pretty-print directive is a PASS-3 / runtime concern; no BIR variant. Removing keeps the directive surface to zero. |

### 1.2 Regex literals JSON exercises

| Literal | Engine route | Verifier shape |
|---|---|---|
| `/-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/` | `parse-that-regex` lazy DFA. | `RegexProgram { plan: LazyDfa, span: NumberSpan }`. The parser validates the matched span and emits the span; numeric conversion happens in `JsonNumber::as_f64()` / `as_i64()`. |
| `/"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/` | `parse-that-regex` lazy DFA. | `RegexProgram { plan: LazyDfa, span: StringSpan }`. The parser validates UTF-8/escape structure, records `STRING_NEEDS_UNESCAPE`, and does not write decoded bytes to the arena during parse. |
| `/[ \t\n\r]*/` | Trivial DFA, in practice scalar `take_while`. | `RegexProgram { plan: TakeWhileClass, span: WhitespaceSpan }`. The skinny may inline this as a tight loop to avoid the regex VM hop; it is byte-class equivalent. |

The structural-alphabet `SimdScan` is **not** spelled in the grammar source —
it is mined by the recognizer pass (which the skinny skips). The skinny
substitutes a hand-curated structural-alphabet recognizer (see §5).

### 1.3 `@host fn` decision

The skinny is host-fn-free. JSON in V1 declares one host call to decode escape
sequences; the skinny moves that decode into the SUBSTRATE `decode_string`
path and removes the `@host fn` surface entirely. Rationale:

1. The SOTA test measures structure-parse throughput. The cost of routing
   decode through `CallHost` is not assumed away — BENCH §7.8 carries
   **two probes** that bound it separately: (a) a per-call dispatch overhead
   microbench (`host_call_dispatch_overhead`) measuring `CallHost` indirection
   in isolation against direct calls (target: ≤ 50 ns/call), and (b) a
   gross-time eager-decode JSON variant (`host_call_eager_decode`) that
   forces parse-time string decoding through the V1-shaped registry path.
   The two probes test two different masking modes; neither uses a single
   2% threshold.
   **Empirical finding (per `skinny/RESULTS.md`):** the two probes split the
   disposition cleanly. Probe (a) returns 0.82 / 0.80 / 0.72 ns/call on
   twitter / citm_catalog / canada — PASS at the ≤ 50 ns target; dispatch
   overhead is not masking. Probe (b) returns 20.9% / 18.5% / 23.6% of Track 1
   Mbps on twitter / citm_catalog / canada, registering as a MASKING signal on
   every corpus. The empirical disposition is therefore: the host-fn-free
   cut is FAITHFUL **only** if V1 JSON keeps string decoding lazy in the
   substrate/view path (SUBSTRATE §2 + Lock 9 `Cow<'input, str>` model
   commit this). A V1 grammar that decodes every string at parse time must
   treat this as a measured MASKING signal and lower the JSON SOTA
   probability accordingly. Earlier predictive language anticipating a
   5–15% delta is preserved here only as history; the measurement is
   load-bearing.
2. Removing host functions removes the entire `host::types`, host overload
   selection CSP path, and `CallHost` lowering. Three crates (`host`,
   `csp-solver`, the `host-overload` improvement bridge) become unreachable.
3. ARCH §12.1 yaml onboarding admits a metadata-only host route. JSON in the
   skinny is a stricter case — no host stanza at all.

The cost: the V1 grammar's `-> f64` and `-> decode(...)` map tails do not
typecheck against the skinny compiler. That is acceptable only because the
SOTA test parses structural shape and validates regex spans; scalar values are
materialised lazily by SUBSTRATE accessors. The bench-counters feature must
show zero payload-arena writes for Track 1 and Track 2.

The skinny's meta-grammar surface is therefore a strict subset of the V1
BBNF directive vocabulary: the skinny recognises only the six directives
its `grammar` partial parser needs to round-trip `grammars/json.bbnf` (none
of which are present in the skinny JSON grammar text), and the parser is
required to reject any other directive with the diagnostic
`BBNF-DIRECTIVE-NOT-IN-SKINNY` at §5.2. Closure to the full V1 BBNF
meta-grammar is mechanical: the V1 graduation reinstates `@host`, `@layout`,
`@error`, `@pretty`, and the lookbehind/predicate constructors on the same
Grammar IR backbone; no skinny surface needs to be unbuilt.

---

## 2. Grammar IR Subset

The full V1 Grammar IR has 14 variants (ARCH §7.1). JSON exercises 9.

### 2.1 Exercised variants

| Variant | JSON site | Skinny coverage |
|---|---|---|
| `Rule` | every rule (`null`, `bool`, `number`, `string`, `ws`, `comma`, `colon`, `value`, `pair`, `member`, `members`, `elements`, `array`, `object`, `json`). | Required. Carries name, body ID, and (skinny) trivial monomorphic signature. |
| `Seq` | `pair = string ws colon value`; `array = "[" ws elements "]"`; etc. | Required. Empty-Seq normalisation runs in `passes::normalize` (legality rewrite). |
| `Alt` | `value = object \| array \| string \| number \| bool \| null`; `bool = "true" \| "false"`. | Required. JSON is byte-disjoint at every alt site (every alt's first byte is unique: `{`, `[`, `"`, digit/`-`, `t`, `f`, `n`). The skinny lowers every JSON alt as `Alt { mode: Dispatch }`; **`Alt { mode: Speculative }` is unreachable** for JSON. |
| `Repeat` | `(comma pair)*`; `(comma value)*`. | Required. Uses min=0 and no separator metadata — the comma is part of the body. The skinny rejects nullable bodies at `passes::normalize` (legality rewrite: nullable-body Repeat detection). |
| `Optional` | `member?`, `(member (comma member)*)?` (the `members` and `elements` rules). | Required. Empty branch keeps shape (the empty array/object case). |
| `Literal` | `"null"`, `"true"`, `"false"`, `"["`, `"]"`, `"{"`, `"}"`, `","`, `":"`. | Required. Byte literal with `case = Sensitive`. |
| `Regex` | `number`, `string`, `ws`. | Required. Three regex programs total. |
| `Ref` | `value` references `object`, `array`, `string`, `number`, `bool`, `null`; `pair` references `string`, `colon`, `value`; etc. | Required. JSON has no generics, so every `Ref` instantiates the empty type-arg list. |
| `Annotation` | None in the skinny grammar. | Retained as a Grammar IR variant for the BBNF AST round-trip; the skinny grammar carries zero `Annotation` payloads. |

### 2.2 Skipped variants and per-skip impact

| Variant | Skipped because | Impact on SOTA test |
|---|---|---|
| `Predicate` | JSON has no `&` / `!` lookahead. | None. SOTA throughput is unaffected by predicate machinery. |
| `Lookbehind` | JSON has no `\|<` / `\|<!` lookbehind. | None. |
| `Call` (`kind: Map`) | Skinny grammar drops `-> f64`, `-> true`, etc. | Slight: the typed `Json` root exposes raw spans and lazy accessors instead of pre-decoded scalars. Scalar decode runs at access time, not at parse time. SOTA latency is the parse phase, so the move from parse-time to access-time scalar decode is recorded as a favorable skinny deviation. |
| `Call` (`kind: Host`) | Skinny is host-fn-free. | Empirically split per `skinny/RESULTS.md`: BENCH §7.8 carries two probes — `host_call_dispatch_overhead` (returns 0.82/0.80/0.72 ns/call on twitter/citm/canada, PASS) and `host_call_eager_decode` (returns 20.9/18.5/23.6% of Track 1 Mbps on twitter/citm/canada, MASKING). The dispatch-overhead cut is FAITHFUL; the eager-decode cut is MASKING and constrains V1 JSON to keep string decoding lazy in the substrate/view path (Lock 9 `Cow<'input, str>`). |
| `LayoutDirective` | Whitespace is desugared to an explicit `ws` rule. | Slight increase in BIR size (every whitespace site becomes a `CallRule(ws)`). The whitespace rule itself lowers to a tight scalar loop, so the runtime cost is the same as `@layout(ws = ...)`. SOTA neutral. |
| `ErrorDirective` | JSON has no `@error` recovery. | None. SOTA is measured on valid input; recovery is irrelevant. |

### 2.3 Validation pass surface

`passes::validate` in the skinny runs three checks and rejects everything else:

1. **No backend node names** — Grammar IR contains no variant whose name
   matches a Backend IR variant (per ARCH §7.1 invariant
   `ir::validate::grammar_ir_has_no_backend_nodes`). Static enum check.
2. **No nullable Repeat body** — every `Repeat` body must be non-nullable.
   The skinny computes a one-pass nullability fixpoint over Grammar IR. If
   `members`'s body were `pair?` (instead of `pair (comma pair)*` followed by
   the outer `?`), nullability would be flagged. JSON's `(comma pair)*` body
   is non-nullable because `comma` is `","` then `ws`; the leading `,` is
   non-nullable.
3. **All `Ref` targets resolve** — every `Ref { target: RuleId }` resolves to
   a defined `Rule`. The skinny rejects forward references that never bind.
   JSON has one cyclic reference (`value` → `object` → `pair` → `value`); the
   resolver tolerates cycles because cyclic resolution is not the same as
   nullable cycle.

Skipped validation in the skinny:

| V1 check | Skipped because | Impact |
|---|---|---|
| Lookbehind width proof | JSON has no lookbehind. | None. |
| Recovery code registration | JSON has no `@error`. | None. |
| Layout policy scoping | JSON has no `@layout`. | None. |
| Pretty-print directive vocabulary | JSON has no `@pretty`. | None. |
| `directive-canon` lint | Skinny grammar uses zero directives. | None. |

---

## 3. BIR Subset

The full V1 Backend IR has 20 variants (ARCH §7.2). JSON exercises 14.

### 3.1 Exercised variants

| BIR variant | JSON site | Notes |
|---|---|---|
| `Entry` | the `json` root rule. | Single public entry: `pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>`. |
| `Seq` | every `Seq` Grammar IR node lowers to BIR `Seq`. | Straight-line control flow. |
| `Alt { mode: Dispatch }` | every `Alt` Grammar IR node. | All JSON alts are byte-disjoint, so `Dispatch` is the only mode used. The Dispatch discriminator is a 256-entry byte table built at codegen time from the alt's first-byte set. |
| `RepeatLoop` | every `Repeat`. | Min=0, no separator metadata; the body carries the comma. Progress guard required (the body must consume at least one byte). |
| `OptionalBranch` | `members`, `elements` empty cases. | Empty branch keeps shape (the empty `Vec`). |
| `ByteLiteral` | every `Literal`. | Byte compare; the codegen folds the long-prefix literals (`null`, `true`, `false`) into a `u32`/`u64` aligned compare where possible. |
| `RegexProgram` | `number`, `string`, `ws`. | Three programs. The skinny lowers each through `parse-that-regex` lazy DFA (no full DFA, no VM). |
| `SimdScan` | structural-alphabet pre-scan over the whole input. | One `SimdScan { mode: Exact, needle: StructuralAlphabet { '{', '}', '[', ']', ',', ':', '"' }, fallback: scalar }`. The hand-curated recognizer (§5) places this as a single `SimdScan` BIR node before the `Entry` body and feeds the `Alt`/Dispatch table with byte offsets. |
| `CallRule` | every `Ref`. | Regular function call. JSON has 14 rule defs and ~35 `CallRule` sites. |
| `SpanMark` | every captured rule (compiler-generated). | Start + end span pairs for `value`, `string`, `number`, `array`, `object`, `pair`. Used by `JsonRoot` view to expose source slices. |
| `TapeEmit` | every node + token event (compiler-generated). | The tape carries `(NodeKind, span, payload_slot?)`. JSON node kinds: `Object`, `Array`, `Pair`, `String`, `Number`, `Bool(true)`, `Bool(false)`, `Null`, `Member`, `Element`. |
| `DirectBuild` | every typed-view rule (compiler-generated). | Builds the typed projection surface: `JsonValue<'i>` enum + `JsonObject<'i>` / `JsonArray<'i>` + `JsonString<'i>` views that borrow the sealed tape. It is not an eager parallel owned struct tree. Co-scheduled with `TapeEmit` per ARCH §7.2 invariant 2. |
| `ValueProject` | the `Json::value(self) -> JsonValue<'i>` projection. | Single projection from `JsonRoot<'i>` to its top-level value; called by user code, not by the parser body. |
| `Return` | end of every rule body. | Compiler-generated. |

### 3.2 Skipped BIR variants and per-skip impact

| Variant | Skipped because | Impact on SOTA test |
|---|---|---|
| `Alt { mode: Speculative }` | JSON has zero non-disjoint alts. | None. The full V1 `Alt` payload still carries a `mode: Dispatch \| Speculative` discriminator; the skinny extractor always picks `Dispatch`. |
| `PrattSpine` | JSON has no operator precedence. | None. |
| `CallHost` | Skinny is host-fn-free. | Not emitted in the main skinny parser. BENCH emits **two** measurement probes (`host_call_dispatch_overhead` and `host_call_eager_decode`) per §1.3 + BENCH §7.8.1; the empirical disposition per `skinny/RESULTS.md` is dispatch-overhead FAITHFUL (0.82/0.80/0.72 ns/call), eager-decode MASKING (20.9/18.5/23.6% of Track 1 Mbps across twitter/citm/canada). The cut is FAITHFUL only under the V1 lazy-decode constraint (Lock 9). |
| `LayoutScope` | Whitespace desugared to a `ws` rule. | None for throughput. The desugar has the same emitted code shape as a layout policy push/pop because `LayoutScope` lowers to identical scanner state. |
| `ErrorRecover` | JSON has no `@error`. | None. SOTA inputs are valid; recovery is unmeasured. |
| `PathEval` | Skinny does not link `path-core`. | None for SOTA. Path queries are a PASS-3 surface. |
| `DebugMark` | Skinny disables the debug profile. | None. |

### 3.3 Lowering matrix per `LayoutFacts.backend_shape` (normative; lowering-only)

Generated parser bodies lower per the five values of `LayoutFacts.backend_shape[rule_id]` (ARCH §7.3) derived by the cost model (per Lock 10 auto-detect; from existing Grammar IR facts: first-set disjointness, `@error(recover)` presence, `@host fn` parse-time-decoded presence, `@layout` scope presence). No new BIR variant; no user-visible directive; the lowering pattern lives entirely in `crates/codegen/src/lower/rust.rs`'s emission of the existing `Alt { mode: Dispatch }` / `Alt { mode: Speculative }` variants and the surrounding `Seq` / `Repeat` / `Optional` bodies. The contract is normative because the codegen template inversion is load-bearing for SOTA-BEAT (`SUBSTRATE.md` §1.6; cycle-budget evidence at `skinny/profile/simdjson-v2/PROFILE-REPORT.md` showing stage2 visit functions never re-scan source; yyjson at 0.91 c/B twitter beats simdjson 1.142 without SIMD by fusing scan + dispatch via `always_inline`). Generated bodies that re-scan source bytes for whitespace or value boundaries when `backend_shape ∈ {OffsetTape, EventTape, CollapsedStage}` are faults regardless of throughput outcome — the audit gate at `BENCH.md` §6 outcome class `G-fusion-quality` fires when comparator-anchored hot-leaf-count exceeds the structural-shape threshold.

| `backend_shape` | Cursor over | Source-byte access | Selected when |
|---|---|---|---|
| `EagerTape` | `pos: usize` (eager byte position) | every dispatch + boundary | rule body or transitive uses include `@error(recover)`, `@host fn` decoded-at-parse, `@layout` scope, OR first-set has overlap (forces `Alt { Speculative }`) |
| `OffsetTape` | `cursor: u32` indexing `Tape::offsets` | only inside grammar-neutral primitives (string body, number span) | byte-finite disjoint first-set + lazy scalar spans (JSON skinny default) |
| `EventTape` | `cursor: u32` indexing event cells with stored payload class | per-event payload class drives primitive choice | payload/recovery/layout side facts must be retained per cursor |
| `SinkOnly` | none retained; direct emit to typed sink | only inside grammar-neutral primitives | API shape requires no post-parse path/value traversal (typed extraction / ETL / validation-only) |
| `CollapsedStage` | mask-held parser state (k-mask on AVX-512; vreg-held on NEON) | none in scan stage; primitives consume from mask | target features admit AND rule is a hub with ≥ 4 byte-disjoint arms |

JSON in the skinny derives `OffsetTape` for every rule per the algorithm at ARCH §7.3 (no `@error`, no `@host fn` decoded-at-parse, no `@layout`, byte-disjoint first-sets). The lowering primitives below are the `OffsetTape` emission templates; equivalent templates for `EventTape`, `SinkOnly`, and `CollapsedStage` follow the same structural shape (cursor-indexed dispatch; no source-byte rescans outside primitives) with their respective cursor type and emission target. `EagerTape` retains the pre-2026-05-12 source-byte cursor shape verbatim.

**Primitive 1 — `parse_value` shape**. The typed dispatch hub reads exactly one byte per dispatch via `source[offsets[*cursor as usize] as usize]`. No `skip_ws`, no raw `peek`, no `pos` advancement against source. Cursor advances through the offset array via `*cursor += 1` per consumed structural unit.

```rust
fn parse_value<'i>(
    source: &'i [u8],
    offsets: &[u32],
    flags: &[u8],
    cursor: &mut u32,
    arena: &Arena,
) -> Result<JsonValue<'i>, ParseError> {
    let b = source[offsets[*cursor as usize] as usize];
    match b {
        b'{' => parse_object(source, offsets, flags, cursor, arena),
        b'[' => parse_array(source, offsets, flags, cursor, arena),
        b'"' => parse_string(source, offsets, flags, cursor, arena),
        b'-' | b'0'..=b'9' => parse_number(source, offsets, cursor, arena),
        b't' | b'f' | b'n' => parse_literal(source, offsets, cursor),
        _ => Err(ParseError::Unexpected(b)),
    }
}
```

**Primitive 2 — Container body shape**. `parse_object` and `parse_array` consume the open via `*cursor += 1`, then loop. The loop terminator checks `source[offsets[*cursor]]` against the close byte; the separator between elements is consumed via `*cursor += 1` (no re-validation; the scan already verified the separator).

```rust
fn parse_object<'i>(source: &'i [u8], offsets: &[u32], flags: &[u8],
                   cursor: &mut u32, arena: &Arena) -> Result<JsonObject<'i>, ParseError> {
    *cursor += 1;  // consume '{'
    let start = *cursor;
    loop {
        let b = source[offsets[*cursor as usize] as usize];
        if b == b'}' { *cursor += 1; break; }
        parse_pair(source, offsets, flags, cursor, arena)?;
        let next = source[offsets[*cursor as usize] as usize];
        if next == b',' { *cursor += 1; continue; }
        if next == b'}' { *cursor += 1; break; }
        return Err(ParseError::Unexpected(next));
    }
    Ok(JsonObject { /* ... start..*cursor span ... */ })
}
```

**Primitive 3 — String primitive shape with `HasEsc` flag**. The scan emits a per-string `HasEsc` bit in the `flags` array; the generated parser borrows the string body directly when the flag is clear.

```rust
fn parse_string<'i>(source: &'i [u8], offsets: &[u32], flags: &[u8],
                   cursor: &mut u32, arena: &Arena) -> Result<JsonString<'i>, ParseError> {
    let start_off = offsets[*cursor as usize];     // position of opening quote
    let has_esc = flags[*cursor as usize] & FLAG_HAS_ESC != 0;
    *cursor += 1;
    let end_off = offsets[*cursor as usize];        // position of closing quote
    *cursor += 1;
    let body = &source[(start_off as usize + 1)..(end_off as usize)];
    if !has_esc {
        Ok(JsonString::Borrowed(unsafe { std::str::from_utf8_unchecked(body) }))
    } else {
        Ok(JsonString::Decoded(decode_escapes(body, arena)?))
    }
}
```

**Primitive 4 — Number primitive shape**. The number primitive uses the offset delta to bound the digit span; no per-byte cursor walk during structural parse.

```rust
fn parse_number<'i>(source: &'i [u8], offsets: &[u32],
                   cursor: &mut u32, arena: &Arena) -> Result<JsonNumber<'i>, ParseError> {
    let start = offsets[*cursor as usize] as usize;
    *cursor += 1;
    let end = offsets[*cursor as usize] as usize;
    Ok(JsonNumber::lazy_borrow(&source[start..end]))
}
```

**Primitive 5 — Dispatch density / jump-table emission**. Each `Alt { mode: Dispatch }` lowering emits arm-density facts to encourage LLVM to emit a jump table where the target/backend shape makes that profitable. The stable-Rust shape uses ordinary `match` plus cost-model-owned density; the nightly/ASM path is reserved for admitted `CollapsedStage` NASM and does not introduce a new BIR node. The function-pointer dispatch table previously rejected at `REDRESS-17` is *not* the same primitive; that was call-site indirection (every dispatch invokes through a function pointer), while this is inlined dispatch lowering.

**Primitive 6 — Owned decode materialization policy**. The hot path never
allocates for borrowed offsets or raw strings. `SinkOnly` direct lowering now
passes raw string spans plus `NEEDS_DECODE` to `JsonSink::*_source` hooks; the
default hooks allocate only on `HAS_ESC` / `NEEDS_DECODE` paths, and decoded
payload storage carries an explicit drop policy. The no-allocation decoded
visitor route was measured and rejected, and the later exact decoded-stats
sink was also measured and rejected because two-pass decoded length/hash work
regressed escape-heavy direct rows. A quote-source one-pass streaming hasher
was also measured and rejected because it lost to the default
allocate-then-contiguous-hash baseline. The admissible direct close is a
field-layout decode+sink materializer or same-loop SinkOnly/CollapsedStage
primitive rather than parser-side eager decode, a generic visitor layered on
`unescape_json_string`, or a sink-local decoded hash helper.
There is no offset-vector `set_len(0)` SOTA primitive: offsets are `u32` and
have no per-element destructor to bypass.

**Audit invariants** (apply when `backend_shape ∈ {OffsetTape, EventTape, SinkOnly, CollapsedStage}`; `EagerTape` is exempt because it retains the source-byte cursor by design):
- No `skip_ws` call site survives in generated `parse_*` bodies.
- No `peek` against source bytes survives in dispatch positions; dispatch reads through the typed cursor (`OffsetTape` / `EventTape`), through the typed-sink builder (`SinkOnly`), or through the mask-held state register (`CollapsedStage`). Source-byte reads remain inside grammar-neutral primitives such as `parse-that/string`, `parse-that/number`, and exact literal verification.
- The cursor consumes the single tape/event substrate, not a byte-class
  whitespace wrapper and not a second parser-local scanner. SK-V5 redress item
  51 measured and rejected the wrapper route; item 53 measured and rejected a
  parser-local structural-mask cursor over source bytes. A conforming lowering
  consumes the scanner/tape event stream itself and never materializes a
  retained structural-event sidecar.
- Direct escaped-string materialization consumes the same source-hook seam but
  must beat the default allocation baseline. SK-V5 redress item 54 measured
  and rejected an exact sink-local decoded-stats helper; item 55 measured and
  rejected a quote-source one-pass streaming hasher. Conforming `SinkOnly`
  lowerings route escaped strings through a field-layout materializer or a
  same-loop grammar event, not through a decoded hash helper at the sink.
- The cycle-per-byte gate (`BENCH.md` §7.9) is comparator-anchored: skinny twitter c/B ≤ 1.5 × simdjson twitter c/B (the simdjson floor at its algorithm is ~1.142 c/B per `simdjson-v2/PROFILE-REPORT.md`).
- Hot-leaf count gate (`BENCH.md` §6 outcome class `G-fusion-quality`): comparator-anchored count ≤ 3 leaves at ≥10% self-time (comparators: sonic-rs = 1, simdjson = 2).

### 3.4 BIR construction discipline

The skinny ratifies ARCH §7.2 invariants:

| Invariant | Skinny enforcement |
|---|---|
| Lowerers never inspect Grammar IR. | `codegen::lower::rust` imports `ir::backend_ir::*` only. The skinny does not need an import-deny lint at this size, but the rule holds. |
| Tape and direct-to-struct are one materialization strategy. | `TapeEmit` and `DirectBuild` are scheduled together by `passes::extract` (skinny version: a fixed-shape extraction with no choices). |
| OpenFrame clone stacks are absent. | Skinny uses a single arena; speculative alts are absent for JSON, so checkpoint/rollback is dead code. |
| SIMD is mined, not syntax-directed. | Skinny replaces the miner with a hand-curated recognizer (§5) that nominates the structural-alphabet `SimdScan` site. |
| VM can replay all BIR variants. | **Not enforced in the skinny.** The `vm` crate is stubbed; no replay invariant. V1 receiver: Tranche I (VM replay + golden trace gate restored when the `vm` crate lands). |

---

## 4. HM-Only Type Checker

The V1 type system is HM-equality + Pierce-Turner bidirectional + DK13 +
finite CSP + GADT branch-local equality (ARCH §8.2). The skinny ships **only
HM-equality** — Algorithm-W, first-order unification, scheme generalization,
scheme instantiation. Nothing else.

### 4.1 What the skinny HM checker does

| Component | Skinny shape |
|---|---|
| Algorithm-W constraint generation | One pass over Grammar IR rules. Every rule body produces a fresh type variable; `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref` each have one inference rule. |
| First-order unifier | Robinson-style `unify(t1, t2) -> Result<Substitution, TypeError>`. Occurs-check on. ~150 LOC. |
| Scheme generalization | At rule definition: `generalize(t, env) -> Scheme` over free type variables not bound in env. JSON rules are monomorphic so generalization always returns a closed scheme. |
| Scheme instantiation | At every `Ref` site: `instantiate(scheme) -> Type` with fresh type vars per quantifier. JSON schemes have zero quantifiers, so instantiation is identity. |
| `TypeFacts` output | `HashMap<RuleId, Type>` plus `HashMap<NodeId, Type>` for body expressions. Internal to `passes::layout::types`. |
| `TypeObligationLog` | `Vec<TypeObligation>` for diagnostics; carries source span, expected-from, actual-from, solver-stage. Skinny uses this only to format errors; no obligation discharge logic (no coercions in skinny). |

### 4.2 What the skinny HM checker does **not** do, and why JSON does not need it

| Mechanism | Why skipped | Why JSON does not need it |
|---|---|---|
| **DK13 higher-rank algorithmic completeness** | Adds ordered existential contexts, principality tracking, decidability/soundness/completeness proofs, explicit annotation rules for non-principal programs. | JSON has zero higher-rank types. Every JSON rule infers a closed monomorphic type. No `forall` quantifier survives generalization. This cut is JSON-FAITHFUL; CSS L4, Sheets, and BBNF-self remain V1 caveats where generic/host-chain shape can load-bear. |
| **Pierce-Turner bidirectional check/synth** | Requires the synth/check distinction at every node, expected-type propagation through annotations and chain steps. | JSON has zero explicit annotations and zero chain steps. Every node synthesises. The check direction has no callers in JSON. The skinny's HM is pure synth. This cut is JSON-FAITHFUL; CSS L4 and Sheets carry explicit type annotations and host-chain steps where bidirectional check direction load-bears, so the V1 grammar set restores Pierce-Turner before those grammars dispatch. |
| **Bounded coercion obligations** | Numeric widening, lifetime-owned escalation, generated-record shape narrowing, host-improvement rules. | JSON in the skinny exposes raw spans + arena handles; no scalar widening at parse time. No record narrowing because no `@host fn` returns a narrowed shape. The skinny's `TypeObligationLog` carries only equality failures. |
| **Finite-choice CSP** | Resolves host overload selection, layout representation, materialization mode, recognizer eligibility, recovery strategy, backend erasure, extraction legality. | JSON is host-fn-free (no host overload). Layout is pass-through (no layout choice). Materialization is fixed at tape-direct (no choice). Recognizer is hand-curated (no eligibility CSP). Recovery is absent. Backend is `RustBackend` only. Extraction is single-plan. **Every CSP axis has zero choice for JSON.** |
| **GADT branch-local equality** | Match-arm refinements, OutsideIn(X) implication constraints, `Implication { givens, wanted }` propagation. | JSON has zero match arms. The skinny grammar uses no `Pattern @ where T = U -> Block` form. This cut is JSON-FAITHFUL; BBNF-self and future typed host-chain grammars remain the V1 caveat. |
| **CHR-style improvement** | Closes host-overload ambiguity at the bridge boundary. | No host overload, no ambiguity. |
| **Schema-mining miner** | Telemetry-driven shape inference; corpus-fed candidate proposal; HM/CSP/DK13 solver chain. | Skinny ships **skinny-only hand-curated shapes** for the JSON typed root (§5). The miner is replaced by a small hand-written `ShapeFacts` table that carries a deletion gate at V1 graduation. |
| **Record narrowing** | Finite generated-shape coercion for source/target shapes both known at compile time. | Skinny's `JsonObject<'i>` and `JsonArray<'i>` are open shapes (read-only views); no narrowing is required. |

### 4.3 `TypeFacts` shape

The skinny `TypeFacts` is internal to `passes::layout::types` so the V1
graduation can add DK13/GADT/CSP siblings without moving Algorithm-W:

```rust
// crates/passes/src/layout/types/facts.rs (skinny)
pub(crate) struct TypeFacts {
    /// Inferred type per rule definition.
    pub rule_types: HashMap<RuleId, Type>,
    /// Inferred type per body expression node.
    pub node_types: HashMap<NodeId, Type>,
    /// Free type-variable substitution accumulated during unification.
    pub subst: Substitution,
    /// Diagnostic obligations for any unification failure.
    pub obligations: Vec<TypeObligation>,
}
```

`Type` is a sum:

```rust
pub(crate) enum Type {
    /// Type variable (post-instantiation; free during inference).
    Var(TypeVarId),
    /// Concrete builtin: Bytes, Str, F64, U8, Bool, Span.
    Builtin(BuiltinTy),
    /// Sequence; carries member shape ordering.
    Seq(Vec<Type>),
    /// Alternative (sum); carries member shape set.
    Alt(Vec<Type>),
    /// Repetition: list of body type.
    List(Box<Type>),
    /// Optional: nullable body type.
    Option(Box<Type>),
    /// Reference to a named rule's scheme.
    Rule(RuleId),
}
```

JSON's inferred `Type::Rule(value)` resolves (after one round of substitution)
to:

```text
Alt[
  Rule(object),
  Rule(array),
  Rule(string),
  Rule(number),
  Rule(bool),
  Rule(null),
]
```

Each branch resolves further; the recursion terminates because JSON's
recursive cycle (`value -> object -> pair -> value`) is well-typed under HM
(rule schemes are first-class members of the type lattice, so cyclic
references unify by name, not by structural unfold).

### 4.4 `LayoutFacts` in the skinny

ARCH §8.2 + Lock 2 make `passes::layout` the public boundary; HM is its
internal subroutine. The skinny preserves the boundary but makes
`passes::layout` a **trivial pass-through**:

```rust
// crates/passes/src/layout/mod.rs (skinny)
pub fn run(grammar: &GrammarIr, type_facts: TypeFacts) -> LayoutFacts {
    LayoutFacts {
        rule_types: type_facts.rule_types,
        node_types: type_facts.node_types,
        layout_policies: HashMap::new(), // no @layout in JSON skinny
    }
}
```

`TypeFacts` is consumed by `passes::layout`; `LayoutFacts` is the public
side-table consumed by `passes::extract` (§5). The skinny preserves the
**name and surface** of the boundary so a future tranche can drop in real
layout lowering without renaming the public artefact, but the **content** is
trivial.

JSON does not need any layout policy because:

1. Whitespace is desugared to an explicit `ws` rule.
2. `?w`, `>>`, `<<` operators are absent from the skinny grammar.
3. No `@layout` directive.
4. No layout-derived type narrowing.

### 4.5 HM checker LOC budget

| Module | Skinny LOC budget |
|---|---|
| `passes/src/layout/types/algorithm_w.rs` | ~250 |
| `passes/src/layout/types/unify.rs` | ~150 |
| `passes/src/layout/types/scheme.rs` | ~80 |
| `passes/src/layout/types/facts.rs` | ~60 |
| `passes/src/layout/types/diagnostic.rs` | ~120 |
| **Total `passes/layout/types/` skinny** | **~660** |

Compare to V1 estimate (~3,500 LOC for HM + bidirectional + DK13 + GADT). The
~80% cut is the entire SOTA-validation point: the skinny tests whether SOTA
falls out of the substrate + extraction shape, **independently** of whether
DK13 is in or out.

---

## 5. Pipeline Subset

The full V1 pipeline (ARCH §6) is 13 phases. The skinny runs 9 (the source-load step plus the eight transitions enumerated in §5.1).

### 5.1 Skinny pipeline

```text
source load
  -> BBNF parse
  -> semantic validation
  -> HM inference
  -> minimal shape mining (hand-curated)
  -> BIR construction (single-plan extraction)
  -> Rust lowerer
  -> template emit
  -> regen equality
```

### 5.2 Per-phase shape

| Phase | Skinny implementation | Output |
|---|---|---|
| **source load** | Read `grammars/json.bbnf` from disk; record source hash. | `Source { path, bytes, hash }`. |
| **BBNF parse** | Use the skinny `grammar` crate's partial parser for the §1.1 JSON subset. It parses the six-directive vocabulary enough to reject non-skinny directives with the verbatim diagnostic `BBNF-DIRECTIVE-NOT-IN-SKINNY: '<directive>' is reserved for V1 closure and is not parsed by the skinny grammar surface`, but it does not depend on the skipped `parse-that` crate or the full self-host path. The diagnostic carries the offending directive name, the source span, and a fixed error code (`E-SKINNY-DIRECTIVE`) so the regen-equality gate at §6.4 can distinguish "directive rejection" from "syntax error" in CI. | `GrammarIr`. |
| **semantic validation** | The 3 checks at §2.3. | `ValidationReport`; halts compile on failure. |
| **HM inference** | Algorithm-W as in §4. Runs as a top-level pass (not a `passes::layout` subroutine in the skinny — the layout pass is trivial pass-through). | `TypeFacts`. |
| **minimal shape mining (hand-curated)** | A 80-line hand-written `ShapeFacts` table for JSON. Names every typed view: `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`, `JsonPair`. Replaces the V1 schema-mining miner (telemetry-driven; no telemetry in the skinny). | `ShapeFacts`. |
| **BIR construction (single-plan extraction)** | One-pass tree walk: Grammar IR + `LayoutFacts` + `ShapeFacts` → BIR. Single plan — no extraction CSP, no cost frontier, no e-graph. Hand-curated recognizer nominates the one `SimdScan` site (structural alphabet over the input). | `BackendIr`. |
| **Rust lowerer** | `codegen::lower::rust` walks BIR and produces `proc_macro2::TokenStream` for each emitted file. Detail at §6. | `EmittedSource { generated, parser, host (empty), view, value, visitor }`. |
| **template emit** | `codegen::runtime_template` writes the emitted source to `runtime/src/grammars/json/` as committed source (Lock 6). Skinny: write straight to disk via `cargo xtask regen-json`. | Files on disk. |
| **regen equality** | `cargo xtask check-json` re-runs the pipeline and diffs the output against the committed bytes. | Pass / fail diagnostic. |

### 5.3 Skipped phases and per-skip impact

| Phase | Skipped because | Impact on SOTA test |
|---|---|---|
| **recognizer mining** | Replaced by a hand-curated structural-alphabet recognizer for JSON. | Empirically FAITHFUL only for the original triad's structural scan and scalar alternate envelope: the canonical hand-curated plan wins against `alternate_scalar_plan`, and the real dispatch-table probe regressed per REDRESS item 17. The current `N-direct / NoGo` report proves this is not enough for SOTA-BEAT: recognizer mining becomes load-bearing once the cost model must choose event-cursor consumption, generated `SinkOnly`, and parse-that primitive use by rule shape. |
| **egraph rewrite** | No rewrites in the skinny — pick canonical plan. | FAITHFUL for legality on JSON, but MASKING for SOTA-BEAT once materialization and direct-sink alternatives are in scope. The scalar/dispatch-table probes bound only one JSON plan family; they do not prove that cost-driven materialization rewrites are aspirational. Current receiver: H.W1/H.W4 must measure generated `OffsetTape` / `EventTape` / `SinkOnly` alternatives before V1 can call the cut faithful. |
| **CSP extraction** | Trivial single-plan choice. No host overload, no layout choice, no materialisation choice, no recognizer eligibility, no recovery, no backend erasure, no extraction legality. | None for JSON. Every CSP axis has zero choice (§4.2). |
| **cost extraction** | Constant-cost extraction. | JSON has one production plan in the skinny, but BENCH still bounds the missing cost axis with the alternate-plan stub before claiming the cut is FAITHFUL. |
| **VM replay** | No `vm` crate. | None for SOTA. VM is a debug/replay artefact, not a perf path. |

### 5.4 Hand-curated recognizer

The skinny ships a tiny `passes/src/recognizers/skinny_json.rs` (~40 LOC).
This is a skinny-only fixture, not a generic recognizer miner. V1 graduation
deletes this module when `passes::recognizers` can nominate the same site from
grammar shape.

```rust
// crates/passes/src/recognizers/skinny_json.rs (skinny-only)
pub fn nominate(grammar: &GrammarIr) -> Vec<RecognizerNomination> {
    // For JSON, exactly one structural-alphabet SimdScan over the entire
    // input feeds the Alt/Dispatch table at every `value` site.
    vec![RecognizerNomination::SimdStructuralAlphabet {
        alphabet: StructuralAlphabet::new(b"{}[],:\""),
        verifier: VerifierRoute::Scalar,
        site: SimdSite::PreEntry, // emitted before the json rule body
    }]
}
```

`passes::extract` consumes the nomination and emits one `SimdScan { mode:
Exact, ... }` BIR node before the `Entry`. The runtime uses the structural
indices to skip whitespace and dispatch alts in constant time.

The full V1 miner (`passes::recognizers`) is much larger and runs detection
over the entire grammar corpus. The skinny replaces it with a single
hand-written nomination function for JSON. **For grammars beyond JSON the
skinny does not run** — the recognizer is JSON-specific and carries a deletion
gate.

This is an explicit Lock 14 waiver for the skinny: Lock 14 requires recognizer
nominations to be grammar-shape derived (no per-grammar hand-curated
fixtures); the skinny's `recognizers/skinny_json.rs` violates that contract
under the skinny-only fixture rubric. The waiver is bounded: the V1 graduation
gate is the deletion of this module the moment `passes::recognizers` can
nominate the same site from grammar shape, and the test for "same site" is the
regen-equality + BIR-snapshot pair at §6.4.

### 5.5 Hand-curated shapes

```rust
// crates/passes/src/shapes/skinny_json.rs (skinny-only)
pub fn shapes_for_json() -> ShapeFacts {
    let mut facts = ShapeFacts::new();
    facts.add_struct("JsonRoot", &[("value", "JsonValue<'i>")]);
    facts.add_enum("JsonValue", &[
        "Object(JsonObject<'i>)",
        "Array(JsonArray<'i>)",
        "String(JsonString<'i>)",
        "Number(JsonNumber<'i>)",
        "Bool(bool)",
        "Null",
    ]);
    facts.add_struct("JsonObject", &[("members", "TapeSlice<'i, JsonPair<'i>>")]);
    facts.add_struct("JsonArray", &[("elements", "TapeSlice<'i, JsonValue<'i>>")]);
    facts.add_struct("JsonPair", &[("key", "JsonString<'i>"), ("value", "JsonValue<'i>")]);
    facts.add_struct("JsonString", &[("span", "Span<'i>"), ("needs_unescape", "bool")]);
    facts.add_struct("JsonNumber", &[("span", "Span<'i>")]);
    facts
}
```

`TapeSlice<'i, T>` and `Span<'i>` are SUBSTRATE-owned contracts. String and
number decoded values are accessor results, not parse-time fields.

The hand-curated `shapes_for_json()` table is the second explicit Lock 14
waiver in the skinny: Lock 14 requires shape facts to come from the
schema-mining miner, not per-grammar fixtures. The skinny waives this on the
same bounded terms as §5.4 — the fixture is JSON-only, carries a deletion
gate at V1 graduation, and is replaced by the telemetry-driven miner the
moment the miner can reproduce the table from grammar + corpus signal.

---

## 6. `codegen::rust` Path

The Rust lowerer walks `BackendIr` and produces a `proc_macro2::TokenStream`
per emitted file. The lowerer is BIR-only (Lock 5; ARCH §10).

### 6.1 Per-BIR-variant lowering (skinny scope)

| BIR variant | Emitted Rust (sketch) | Notes |
|---|---|---|
| `Entry { symbol, body }` | `pub fn parse_<name><'i>(input: &'i str) -> Result<<Root><'i>, ParseError> { let mut state = ParserState::new(input); <body>; Ok(state.finish()) }` | Public entry; one per grammar. |
| `Seq { children }` | `<child_1>; <child_2>; ...` | Sequential lowered statements. |
| `Alt { mode: Dispatch, branches }` | `match state.peek_byte() { b'{' => <branch_object>, b'[' => <branch_array>, b'"' => <branch_string>, b'-' \| b'0'..=b'9' => <branch_number>, b't' \| b'f' => <branch_bool>, b'n' => <branch_null>, _ => return Err(ParseError::ExpectedValue), }` | Predictive byte dispatch through Rust `match`. LLVM owns branch-table/jump-table lowering. A function-pointer 256-entry dispatch table is not canonical; the prototype measured it as a regression and marks the old duplicate probe invalid. |
| `RepeatLoop { body, min: 0, max: None }` | `loop { let cp = state.checkpoint(); match <body> { Ok(()) => continue, Err(_) => { state.restore(cp); break; } } }` | Progress guard via checkpoint compare. |
| `OptionalBranch { body }` | `let cp = state.checkpoint(); if let Err(_) = <body> { state.restore(cp); }` | Empty branch keeps shape (typed as `Option<T>` in the view). |
| `ByteLiteral { bytes }` | For short literals: `state.expect_bytes(b"<literal>")?;`. For 4-byte literals (`null`, `true`): `state.expect_u32_le(<u32_packed>)?;`. For 5-byte (`false`): `state.expect_bytes_5(b"false")?;`. | Aligned compares for the keyword literals. |
| `RegexProgram { plan: LazyDfa, span_kind }` | `let span = state.match_regex_lazy_dfa(&REGEX_<id>)?;` | The compiled regex is a `static REGEX_<id>: LazyRegex = LazyRegex::new(<pattern>);` initialised once. |
| `SimdScan { mode: Exact, alphabet, verifier }` | `let scan = runtime::tape::scan_parse_index(input.as_bytes()); state.attach_index(scan);` | Emitted once, before the `Entry` body. For parser code this is `JsonParseIndex`: structural offsets plus string escape/control candidate arrays. The structural-only `StructuralIndex` is still the scan-floor bench product. SUBSTRATE owns both products; the lowerer emits the dispatch site only. |
| `CallRule { callee, result_slot }` | `let <slot> = parse_<callee>(state)?;` | Generated rule functions are colocated in `parser.rs`. |
| `SpanMark { kind: Start }` / `End` | `let __start = state.position();` / `let __span = Span::new(__start, state.position());` | Compiler-generated. |
| `TapeEmit { kind, span, payload? }` | `state.tape.emit(NodeKind::<kind>, __span, <payload_slot>);` | Append-only tape write. SUBSTRATE owns `Tape::emit`. |
| `DirectBuild { shape, fields }` | `Json<Shape> { <field_1>: <slot_1>, ... }` | Builds the typed projection view; cursors point into the sealed tape. Scalar accessors parse or unescape lazily. |
| `ValueProject { from, projection }` | `JsonValue::project(<from>, <projection>)` | Single projection helper for the typed root. |
| `Return { value }` | `Ok(<value>)` | Compiler-generated. |

### 6.2 Emitted file shape

The skinny emits `runtime/src/grammars/json/`:

| File | Skinny content | LOC budget |
|---|---|---|
| `mod.rs` | Re-exports of `Json`, `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonString`, `JsonNumber`. | ~30 |
| `generated.rs` | All BIR-derived parser bodies: `parse_json`, `parse_value`, `parse_object`, `parse_array`, `parse_pair`, `parse_string`, `parse_number`, `parse_bool`, `parse_null`, `parse_ws`. Includes the regex-literal `static`s and the structural-alphabet `static`. | ~600 |
| `parser.rs` | Public entry: `pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>` plus `ParserState`. Calls `generated::parse_json`. | ~120 |
| `host.rs` | **Empty file** (one `// no host fns` comment). JSON is host-fn-free in the skinny. | ~5 |
| `view.rs` | Typed view structs: `JsonRoot<'i>`, `JsonValue<'i>`, `JsonObject<'i>`, `JsonArray<'i>`, `JsonString<'i>`, `JsonNumber<'i>`, `JsonPair<'i>` with span-backed lazy accessor methods. Borrows from `Tape<'i>`. | ~250 |
| `value.rs` | `JsonValue` projection helpers; `Display` impl for the value enum. | ~80 |
| `visitor.rs` | `JsonVisitor` trait + default impls. Skinny ships only the dispatch-by-kind shape; no path crate integration. | ~100 |
| **Total emitted skinny LOC** | | **~1,185** |

V1 baseline is 3,500 → 3,570 (ARCH §12.2). The skinny cuts emitted LOC by
~66% because it omits the `host.rs` host-shim body, the path-schema
sidecar, the visitor's path integration, and the layout-derived view
narrowing.

### 6.3 Emitted parser entry sketch

```rust
// runtime/src/grammars/json/parser.rs (skinny, hand-sketched)
use crate::tape::{Tape, NodeKind, Span};
use super::generated::parse_json;
use super::view::JsonRoot;

pub struct Json;

impl Json {
    /// Parse a `&str` into a `JsonRoot<'i>`.
    ///
    /// The returned root borrows from both the input slice and a tape owned by
    /// the parser state. The cold owned wrapper lives in SUBSTRATE and is not
    /// part of the SOTA hot path.
    pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError> {
        let mut state = ParserState::new(input);
        // Pre-scan: structural-alphabet SIMD index over the whole input.
        // The index is a Vec<u32> of byte offsets matching any of
        // { '{', '}', '[', ']', ',', ':', '"' }.
        let scan = bbnf_simd::scan_json_structurals(
            input.as_bytes(),
            &STRUCTURAL_ALPHABET_JSON,
        );
        state.attach_index(scan);
        // Body.
        parse_json(&mut state)?;
        // Materialise the typed root from the finished tape.
        Ok(state.finish::<JsonRoot<'_>>())
    }
}

pub struct ParserState<'i> {
    input: &'i [u8],
    cursor: usize,
    tape: Tape<'i>,
    scan: Option<JsonParseIndex>,
}
```

`Tape<'i>`, `JsonParseIndex`, `runtime::tape::scan_parse_index`, and
`JsonRoot<'i>` are SUBSTRATE / external contracts. The compiler emits the
calling shape; SUBSTRATE provides the implementations.

### 6.4 Snapshot regen check

`codegen::verify` runs the regen-equality gate at every build:

```sh
cargo xtask check-json
```

Implementation: re-run the pipeline, compare the emitted token streams to the
committed bytes byte-for-byte. Any drift fails the gate.

The skinny also commits a BIR snapshot under
`crates/ir/tests/snapshots/json.bir.snap` so a BIR-shape change is detected
even when emitted Rust differs only in formatting.

The regen-equality + BIR-snapshot pair is the binary falsifier surface the
skinny inherits from ARCH §7.4 (diagnostic vocabulary + golden-trace
discipline); ARCH §7.4 names regen-equality as the codegen falsifier and
BIR-snapshot as the pre-codegen shape falsifier. The skinny implements both
as committed-byte gates so any compiler-side drift fails CI before
substrate or BENCH artefacts run.

### 6.5 `parse-attribution` feature flag (SK-V5 Wave 0)

The codegen template emits `#[inline(always)]` on every generated kernel
helper by default — this is the Lock 15 force-inline discipline required for
the i-cache-resident hot function the yyjson profile evidenced. Force-inline
is correct for production builds but obliterates symbol-level samply
attribution: every kernel boundary collapses into one outer parse symbol, and
cohort-B-style profile reports cannot name the hot leaf. SK-V5 cohort B1
(`restart/skinny/audit/SK-V5-COHORT/skv5-B1-parse-attribution.md`) named this
dishonesty: the gate authority's profile rows attribute the whole hot path to
`parse_value_at` without PC-level decomposition.

The remediation is a `parse-attribution` Cargo feature on
`skinny/crates/runtime/Cargo.toml`. When off (the production / SOTA bench
default) every kernel helper keeps `#[inline(always)]`. When on (the
attribution-profile build) the same helpers carry `#[inline(never)]` so the
boundary survives codegen and samply can name it. The codegen template emits
the attribute via paired `#[cfg_attr]`:

```rust
#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn dispatch_value(...) -> ... { /* ... */ }
```

Seven named kernel boundaries in
`skinny/crates/runtime/src/grammars/json/generated.rs` carry this gating:

1. `dispatch_value` — the source-byte → handler match (the `parse_value`
   typed dispatch hub from §3.3 Primitive 1).
2. `skip_whitespace_boundary` — whitespace consumption on cursor advance,
   distinct from the structural-scan whitespace skip that lives in
   `bbnf-simd`.
3. `open_object` / `close_object` / `open_array` / `close_array` — the four
   container-boundary helpers (`*cursor += 1` plus invariant checks; the
   close helpers also seal the typed view span).
4. `match_string_at_quote` entry — the string primitive entry from §3.3
   Primitive 3; the `HasEsc` flag branch and the escape decoder are interior
   to this boundary and remain force-inlined.
5. `match_number_at_digit` entry — the number primitive entry from §3.3
   Primitive 4; the digit-span / float-decode interior remains force-inlined.
6. `verify_literal_true` / `verify_literal_false` / `verify_literal_null` —
   the three keyword-literal verifiers (4-byte / 5-byte / 4-byte aligned
   compares per §6.1 `ByteLiteral` lowering).
7. `tape_emit_token` / `tape_advance_cursor` — the two tape-write boundaries
   that compose every `TapeEmit` BIR lowering; PC-level attribution against
   tape writes is the only way to separate scanner-emitted offsets from
   codegen-emitted offsets per the tape-union audit.

Build the attribution profile via `cargo build --release -p xtask --bin
profile-lazy --features runtime/parse-attribution`. The feature lives on the
runtime crate (not on bench or codegen) so any consumer — the bench harness,
the xtask profiler, an external profile target — can opt into named hot
leaves without changing source. The Lock 15 i-cache budget at `ARCH §7.4`
re-binds on the default build (`parse-attribution` off); the budget is
intentionally not enforced on the attribution build because `#[inline(never)]`
explodes hot-function size by design.

Falsifier: cohort-B attribution runs with the feature on must yield named
hot leaves at ≥5% self-time matching the seven boundaries above. A run that
still collapses to one fused symbol means the codegen template did not emit
the paired `cfg_attr` and the feature is a no-op; that is a regen-equality
fail at §6.4.

Authority: `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md` §2.3.

---

## 7. What's Stubbed In The Skinny

| V1 crate | Skinny status | Per-skip impact on SOTA measurement |
|---|---|---|
| `cost-model` | **Stubbed in the current runnable skinny.** The skinny treats every BIR construction as constant-cost. No `CostFacts`, no `CostDecision`, no scalar score, no Pareto frontier. | The expanded corpus refutes the prior "cost-model is not a recovery lever" reading. `skinny/RESULTS.md` is overall `N-direct / NoGo`: retained parse has 13 G rows plus one Canada L row, while direct-to-struct correctness is green but only `numbers` passes the direct slack. SOTA-BEAT therefore requires a grammar-neutral cost model over materialization plan (`OffsetTape` / `EventTape` / `SinkOnly` / `CollapsedStage`), hot-rule inline selection, byte-class primitive choice, generated source-hook SinkOnly emission, structural-scan floor restoration, and fused exact float/string/Unicode materialization. Lens L verdict: **MASKING until those choices are measured as alternatives, not constants**. |
| `egraph` | **Stubbed.** No e-class, no rewrite, no saturation, no fixpoint. ARCH §10.1 `legality-rewrites` and `normalization-rewrites` (LOAD-BEARING for V1 correctness) are inlined as pre-extraction passes in `passes::normalize`; `cost-driven-rewrites` is omitted from the runnable skinny. | JSON's grammar can be extracted without rewrite search, but SOTA-BEAT cannot claim the omitted rewrite/cost axis is orthogonal. The refined skinny keeps e-graph saturation out of the prototype, but BENCH must carry explicit plan probes for materialization mode, dispatch form, primitive kernel, and capacity strategy before the cut can be FAITHFUL. |
| `csp-solver` | **Stubbed.** No constraint store, no propagation, no improvement, no Implication discharge. | None for JSON. Every CSP axis has zero choice on JSON (§4.2). |
| `vm` | **Stubbed.** No interpreter, no replay, no debug trace. | None for SOTA. VM is a debug/test artefact. The skinny does not have the `vm::replay` golden gate. |
| `bbnf-language-server` | **Stubbed.** No LSP, no editor integration. | None for SOTA. LSP is a developer-experience artefact. |
| `path` / `path-core` | **Stubbed.** No `path!` macro, no path schema, no typed selector. | None for SOTA. Path queries are user-facing; SOTA measures parse-only throughput. The emitted runtime exposes `JsonRoot<'i>` directly without path glue. |

Aggregate cut: the V1 24-crate spec drops to ~10 crates in the skinny
(SUBSTRATE owns ~4, COMPILER owns ~5, BENCH owns ~1). The skinny compiler
slice's crate footprint inside that 10:

| Crate | Skinny LOC budget |
|---|---|
| `bbnf` (CLI + driver) | ~400 |
| `grammar` (BBNF surface IR + partial parser for `json.bbnf`) | ~800 |
| `ir` (Grammar IR + BIR types) | ~500 |
| `passes` (validate + layout/types + layout-passthrough + skinny-only shapes/recognizer fixtures + extract) | ~1,500 |
| `codegen` (lower::rust + runtime_template + verify) | ~1,200 |
| **Compiler skinny crate budget** | **~4,400** |

WORKSPACE sets the binding numbers; this row is the COMPILER slice's input.

---

## 8. The Compile-And-Test Loop

The developer runs four commands end-to-end on the skinny:

| Command | What it does | Expected outcome |
|---|---|---|
| `cargo build -p bbnf` | Build the CLI driver. Compiles `grammar`, `ir`, `passes`, `codegen`, `bbnf`. | Clean build; warnings allowed but no errors. |
| `cargo xtask regen-json` | Run the full pipeline: load `grammars/json.bbnf` → parse → validate → infer → mine shapes → extract BIR → lower to Rust → write `runtime/src/grammars/json/`. | Six files written (`generated.rs`, `parser.rs`, `host.rs`, `view.rs`, `value.rs`, `visitor.rs`). Total emitted LOC ≤ skinny budget (~1,185). |
| `cargo xtask check-json` | Re-run the pipeline and compare bytes to committed runtime files. Fail on any drift. | Exit 0 = unchanged. Exit 1 with diff = drift. |
| `cargo test -p runtime --test json_parity` | BENCH-owned parity test: parse a fixture corpus through the generated parser and through a hand-coded JSON parser; compare outputs. Confirms generated-parser correctness before SOTA bench runs. | All fixture cases pass. |

### 8.1 Bench handoff

After the four commands above pass, the BENCH slice runs:

```sh
cargo bench -p bbnf-bench --bench json_parity -- twitter
cargo bench -p bbnf-bench --bench json_parity -- citm
cargo bench -p bbnf-bench --bench json_parity -- canada
cargo bench -p bbnf-bench --bench simd_scan -- twitter
```

Those commands are owned by BENCH.md; the COMPILER slice's only
responsibility is that the four commands above produce a generated parser
that parses the BENCH fixtures correctly.

### 8.2 Compile-and-test loop end-to-end

```text
edit grammars/json.bbnf
  -> cargo build -p bbnf
  -> cargo xtask regen-json
  -> cargo xtask check-json                            (catches drift if commit forgotten)
  -> cargo test -p runtime --test json_parity         (correctness)
  -> cargo bench -p bbnf-bench --bench json_parity    (BENCH-owned; SOTA gate)
  -> cargo bench -p bbnf-bench --bench simd_scan      (BENCH-owned; SIMD floor)
```

A full clean loop (cold cargo cache) is targeted at ≤ 4s wall time for the
build step (matches PASS-2 §6 row for json: ≤ 4s wall) and ≤ 30s including
the parity test. Bench is separately budgeted by BENCH.

---

## 9. Open Questions And Source-Authority Conflicts

Per the brief's instruction to flag contradictions, two source-authority
points surfaced during exploration:

### 9.1 Layout subroutine ownership in the skinny

ARCH §8.2 + Lock 2 require HM/CSP type checking to run as an internal
subroutine of `passes::layout`; `LayoutFacts` is the public side-table.
ARCH §6 invariant table reads `passes::layout (HM + bidirectional + CSP run as
a subroutine inside layout lowering per Lock 2)`. The skinny keeps Algorithm-W
under `passes::layout::types` but still **inverts the call hierarchy**: the
skinny pipeline calls HM first, then `passes::layout` pass-through wraps the
resulting `TypeFacts` as `LayoutFacts`. This is a deliberate scope cut (JSON has
zero layout policy and zero CSP), and the file placement makes V1 closure
mechanical: real layout lowering later calls the same Algorithm-W module as an
internal subroutine, then adds DK13/GADT/CSP siblings without rewriting it.

### 9.2 Host-fn-free claim for JSON

The brief states "JSON is host-fn-free in the skinny per ARCH §12.1." ARCH
§12.1 is the **YAML onboarding walkthrough**, not a JSON declaration. The
authoritative JSON row at ARCH §12.2 reads `Host route: metadata + numeric/
string host fns from host::primitives`. The committed
`grammar/json/json.bbnf` declares `decode_json_string_to_arena(input)`. The
skinny's host-fn-free decision is a deliberate skinny scope cut (§1.3) but
is **not** sourced from ARCH §12.1 verbatim. The skinny's interpretation:
ARCH §12.1 documents the YAML onboarding pattern as host-fn-optional, and
the skinny adapts that pattern to JSON to delete the `host` and
`csp-solver` crates from the slice. The trade-off is documented at §1.3.

---

## 10. Summary

| Dimension | V1 spec | Skinny |
|---|---|---|
| Grammars supported | 9 + yaml probe | 1 (JSON only) |
| Grammar IR variants reachable | 14 | 9 |
| BIR variants reachable | 20 | 14 |
| Type system mechanisms | 5 (HM + bidirectional + DK13 + CSP + GADT) | 1 (HM) |
| Pipeline phases | 13 | 9 |
| Compiler crates | ~24 (full) | ~5 (compiler slice) |
| Emitted Rust LOC for JSON | 3,500–3,570 | ~1,185 |
| HM checker LOC | ~3,500 | ~660 |
| `host.rs` | host-fn body | empty |
| `path-schema.toml` | emitted | not emitted |
| Recognizer mining | telemetry-driven miner | hand-curated nominator (~40 LOC) |
| Shape mining | telemetry-driven miner | hand-curated table (~80 LOC) |
| Cost model | Pareto frontier + scalar score | constant cost + alternate-plan stub bound |
| E-graph rewrites | legality + normalization + cost-driven | none (legality + normalization inlined) |

The skinny's architectural premise: **if JSON SOTA falls out of the skinny
substrate + extraction shape (single plan, no rewrites, hand-curated
recognizer, host-fn-free) after the host-call and alternate-plan probes produce
non-masking results,
the V1 spec's elaborate machinery — DK13, GADT, CSP, e-graph, cost-model,
miner — is evidence-backed as tail-of-distribution correctness/coverage
machinery rather than load-bearing for JSON throughput. If JSON SOTA misses,
the measurement diagnostic identifies whether the miss is substrate
(SUBSTRATE owns), extraction shape or cost-plan masking (COMPILER owns), or
codegen (COMPILER owns) — and tranches A-J commit only with a calibrated prior
on which V1 axes the SOTA budget actually depends on.**

The compiler slice's job is to produce a generated parser whose performance
ceiling is set by the SUBSTRATE, not by the compiler. Every cut in this
spec is a cut to compiler-side machinery whose absence cannot lower the
ceiling — only correctness coverage. The skinny tests the ceiling.
