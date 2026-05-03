# BA.W5 Generated Parser Shape Specification — JSON Direct-To-Struct

Date: 2026-05-03
Scope: BA.W5's JSON-side generated-parser-shape contract. Per directive §3 gap G ("Generated parser shape"), this artefact specifies the per-construct emission shape for the JSON grammar; BB.W1a/W1b/W1c/W2 generalise to other grammars.

## §1 — Per-construct emission shapes (JSON)

For each grammar construct, the post-W5 emit shape, sample emission (Rust pseudocode), invariants, and cost-model decision points.

### §1.1 — Alt (byte-disjoint case)

When every alt-branch's FIRST set is a singleton byte (or non-overlapping byte set), emit direct `match first { ... }` without speculative checkpoint.

**Sample emission (JSON `value` rule):**

```rust
fn parse_value<'p>(
    input: &'p [u8],
    p: &mut usize,
    arena: &mut JsonArena<'p>,
    state: &mut ScanState,
    cursor: &mut PathCursor<'_>,
) -> Result<JsonValue<'p>, ParseErr> {
    skip_space(input, p, state);
    let first = input.get(*p).copied().ok_or(ParseErr::Eof)?;
    match first {
        b'{' => parse_object(input, p, arena, state, cursor).map(JsonValue::Object),
        b'[' => parse_array(input, p, arena, state, cursor).map(JsonValue::Array),
        b'"' => parse_string(input, p).map(JsonValue::String),
        b'-' | b'0'..=b'9' => parse_number(input, p).map(JsonValue::Number),
        b't' | b'f' => parse_bool(input, p).map(JsonValue::Bool),
        b'n' => parse_null(input, p).map(|_| JsonValue::Null),
        _ => Err(ParseErr::syntax(*p)),
    }
}
```

**Invariants:**
- No `attempt_p = *p; attempt_builder = builder.checkpoint()` (per `audit/RESTART-SKETCH-2026-05-03.md:101-117`).
- No `Result::Err → rollback` arm; the byte-dispatch ladder is the only dispatch.
- Returns by value; `JsonValue<'p>` is Copy 16-byte tag-and-payload.

**Cost-model decision point:** the alt-classifier (post-W2.M1 split at `crates/ir/src/passes/recognizers/grammar_facts/alt_classifier.rs`) computes byte-disjoint vs overlapping. Byte-disjoint cases emit direct `match first`; overlapping cases retain the speculative emit until BB.W3 introduces a generalised cost model. Per W5.M2 gate: `parse_value` body in `generated/json.rs` post-regen contains direct `match first {...}` — verified by grep.

### §1.2 — Alt (speculative case)

When alt-branches overlap on FIRST sets, emit `attempt_p` save + per-branch try + rollback. JSON's `value` rule is byte-disjoint so this case does not appear in the JSON post-W5 emit; the shape is reserved for future grammars where Alt is genuinely speculative.

```rust
let attempt_p = *p;
match try_branch_a(input, p, ...) {
    Ok(v) => return Ok(v),
    Err(_) => *p = attempt_p,
}
match try_branch_b(input, p, ...) {
    Ok(v) => return Ok(v),
    Err(_) => *p = attempt_p,
}
```

**Cost-model decision point:** the per-branch overlap is mined at IR stage [9]; speculative emission is the fallback, not the default. Direct emission is preferred when classifiable.

### §1.3 — Seq

Linear push; each child's typed return value binds to the parent's typed-enum variant field.

**Sample emission (JSON `pair` rule):**

```rust
fn parse_pair<'p>(
    input: &'p [u8],
    p: &mut usize,
    arena: &mut JsonArena<'p>,
    state: &mut ScanState,
    cursor: &mut PathCursor<'_>,
) -> Result<JsonPair<'p>, ParseErr> {
    let key = parse_string(input, p)?;
    skip_space(input, p, state);
    if input.get(*p).copied() != Some(b':') {
        return Err(ParseErr::syntax(*p));
    }
    *p += 1;
    skip_space(input, p, state);
    let value = parse_value(input, p, arena, state, cursor)?;
    Ok(JsonPair { key, value })
}
```

**Invariants:**
- Each child's typed return binds to a named struct field.
- No `begin_compound`/`end_compound` calls.
- The struct constructor is direct: `Ok(JsonPair { key, value })`.

**Cost-model decision point:** none; Seq is mechanical.

### §1.4 — Repeat

Loop with break condition; CharClass-driven SIMD scan when alphabet is structural; cursor consultation when path-driven.

**Sample emission (JSON `object` rule's pair-list):**

```rust
fn parse_object<'p>(
    input: &'p [u8],
    p: &mut usize,
    arena: &mut JsonArena<'p>,
    state: &mut ScanState,
    cursor: &mut PathCursor<'_>,
) -> Result<JsonObjectId, ParseErr> {
    if input.get(*p).copied() != Some(b'{') {
        return Err(ParseErr::syntax(*p));
    }
    *p += 1;
    skip_space(input, p, state);
    if input.get(*p).copied() == Some(b'}') {
        *p += 1;
        return Ok(JsonObjectId::EMPTY);
    }
    let mut pairs: SmallVec<[JsonPair<'p>; 8]> = SmallVec::new();
    loop {
        let pair = parse_pair(input, p, arena, state, cursor)?;
        pairs.push(pair);
        skip_space(input, p, state);
        match input.get(*p).copied() {
            Some(b',') => { *p += 1; skip_space(input, p, state); continue; }
            Some(b'}') => { *p += 1; return Ok(arena.intern_object(pairs.into_vec())); }
            _ => return Err(ParseErr::syntax(*p)),
        }
    }
}
```

**Invariants:**
- The pair-vec is stack-allocated `SmallVec<[_; 8]>` (per `audit/RESTART-SKETCH-2026-05-03.md:215`); spills to heap only when N > 8.
- Loop break is byte-driven (`b',' | b'}'`); no cursor consultation on the eager path (per BA.W4.M0 constant-fold).
- `arena.intern_object(pairs.into_vec())` returns an opaque `JsonObjectId`; the pair-vec heap-allocates only at intern time.

**Cost-model decision point:** the SmallVec inline capacity (8) is mined per-grammar from the structural-alphabet analysis at IR stage [6]; rules with mineable upper-bound get larger `[_; N]`. Default is 8.

### §1.5 — Optional

Peek byte; commit-or-skip.

**Sample emission (a hypothetical optional comma-terminator):**

```rust
let trailing_comma = input.get(*p).copied() == Some(b',');
if trailing_comma {
    *p += 1;
    skip_space(input, p, state);
}
```

**Invariants:**
- Single byte peek; no recursion, no checkpoint.
- The boolean carries forward to the parent's MapExpr binding if needed.

**Cost-model decision point:** none; Optional is mechanical.

### §1.6 — CharClass

Lookup table for small (≤ 16 byte) classes; SIMD shuffle for medium (16-64 byte) classes; scalar fallback for large or partition-rejected classes.

**Sample emission (JSON `string` body — printable ASCII non-quote-non-backslash):**

```rust
// Lookup: 256-bit table compiled at codegen time.
static STRING_BODY_OK: [bool; 256] = build_string_body_class();

fn parse_string_body<'p>(input: &'p [u8], p: &mut usize) -> Result<&'p str, ParseErr> {
    let start = *p;
    while *p < input.len() {
        let b = input[*p];
        if b == b'"' { break; }
        if b == b'\\' { return parse_string_escape(input, p, start); }
        if !STRING_BODY_OK[b as usize] {
            return Err(ParseErr::syntax(*p));
        }
        *p += 1;
    }
    let body = std::str::from_utf8(&input[start..*p]).map_err(|_| ParseErr::Utf8(*p))?;
    Ok(body)
}
```

**Invariants:**
- Lookup table is `static` (compile-time constant); no runtime initialisation cost.
- SIMD scan path emits when SIMD eligibility is `force` or `auto`-determined high-density (per `bbnf-strategy.grammars.json.simd_eligibility`).

**Cost-model decision point:** the SIMD/scalar boundary is per-grammar `simd_eligibility` (per `docs/tranches/BA/audit/W1-workspace-metadata-schema.md` §1).

### §1.7 — Keyword

PHF for ≥ 4 keywords; small-string compare for < 4; suffix elide when keywords share prefixes.

**Sample emission (JSON `bool` rule — two keywords):**

```rust
fn parse_bool(input: &[u8], p: &mut usize) -> Result<bool, ParseErr> {
    if input.get(*p..*p + 4) == Some(b"true") {
        *p += 4;
        return Ok(true);
    }
    if input.get(*p..*p + 5) == Some(b"false") {
        *p += 5;
        return Ok(false);
    }
    Err(ParseErr::syntax(*p))
}
```

**Invariants:**
- Two-keyword case uses sequential byte-slice compare; no PHF.
- ≥ 4-keyword case (e.g. CSS L4 colour names) uses `phf::Map` lookup at codegen.

**Cost-model decision point:** keyword count threshold (default 4); per-grammar override via metadata (future BB extension).

### §1.8 — Scanner

Regex DFA when grammar declares a regex; bespoke NFA when the regex compiles to small NFA (≤ 64 states); inline byte-test when single-character class.

**Sample emission (JSON `number` rule — regex `-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?`):**

```rust
fn parse_number(input: &[u8], p: &mut usize) -> Result<f64, ParseErr> {
    let start = *p;
    // DFA-emitted state machine: each byte transitions to the next state.
    let mut state = NUMBER_DFA_INITIAL;
    while *p < input.len() {
        let b = input[*p];
        let next = NUMBER_DFA_TRANSITIONS[state as usize][b as usize];
        if next == NUMBER_DFA_REJECT { break; }
        state = next;
        *p += 1;
    }
    if !NUMBER_DFA_ACCEPT[state as usize] {
        return Err(ParseErr::syntax(start));
    }
    let body = std::str::from_utf8(&input[start..*p]).map_err(|_| ParseErr::Utf8(start))?;
    body.parse::<f64>().map_err(|_| ParseErr::Number(start))
}
```

**Invariants:**
- DFA tables (`NUMBER_DFA_TRANSITIONS`, `NUMBER_DFA_ACCEPT`) are `static` arrays compiled at codegen; no runtime allocation.
- The float-parse step (`body.parse::<f64>()`) is the post-DFA conversion; per `audit/CENSUS-2026-05-03.md:227-231` the existing `parse_number_lexical_overflow` fallback (renamed from `parse_number_fallback`) is KEEP for edge-case numbers that `lexical-core` rejects.

**Cost-model decision point:** DFA size threshold; ≥ 64 states routes to NFA-with-cache emit. Per the bespoke regex HIR per `feedback_regex_generalized` and `project_bespoke_regex`.

### §1.9 — MapExpr

Typed-enum constructor with bound field positions.

**Sample emission (a JSON-grammar mapped rule, hypothetical):**

```rust
// Grammar: `pair = string, ":", value -> JsonPair { key, value };`
// Bindings: `key` → string-result, `value` → value-result.

fn parse_pair<'p>(
    input: &'p [u8], p: &mut usize, arena: &mut JsonArena<'p>,
    state: &mut ScanState, cursor: &mut PathCursor<'_>,
) -> Result<JsonPair<'p>, ParseErr> {
    let key = parse_string(input, p)?;            // bound to `key`
    skip_space(input, p, state);
    expect_byte(input, p, b':')?;
    skip_space(input, p, state);
    let value = parse_value(input, p, arena, state, cursor)?;  // bound to `value`
    Ok(JsonPair { key, value })                   // MapExpr applied
}
```

**Invariants:**
- The MapExpr (`-> JsonPair { key, value }`) compiles to a direct struct constructor at codegen.
- Binding sites are typed: `key: &'p str`, `value: JsonValue<'p>`.
- No `begin_compound` indirection; the MapExpr IS the compound construction.

**Cost-model decision point:** none; MapExpr is mechanical (per `feedback_typed_materialization_invariant`).

### §1.10 — HostCall

Resolved at codegen via workspace metadata; backend-specific dispatch.

**Sample emission (a CSS L4 host fn, hypothetical — JSON has no host fns at BA.W5):**

```rust
// Grammar: `hex = "#", hex_digits -> parse_hex_color(input);`
// host_fns metadata: { name = "parse_hex_color", crate = "bbnf",
//                      path = "grammar::host::css_l4::parse_hex_color" }

fn parse_hex<'p>(input: &'p [u8], p: &mut usize) -> Result<CssColor, ParseErr> {
    expect_byte(input, p, b'#')?;
    let digits = parse_hex_digits(input, p)?;
    crate::grammar::host::css_l4::parse_hex_color(digits)
        .map_err(|e| ParseErr::Host(e))
}
```

**Invariants:**
- The host-fn path comes from the workspace metadata's `host_fns[].path`; no `match grammar { "css_l4" => ... }` arm.
- Per surgery #15, the host-fn lives under `grammar/host/<g>/` (per-grammar namespace).
- Backend-specific dispatch: Rust emits `crate::...`; TS emits `runtime.parseHexColor(...)`; WASM emits indexed extern import.

**Cost-model decision point:** none; host-fn dispatch is mechanical.

## §2 — All-grammar commit (per option (a))

The shapes above are the BA.W5a JSON-side canonical exemplar. Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` option (a), BA owns the all-grammar migration; per-grammar emission patterns extend the JSON shape construct-by-construct across W5a..W5e. Per-grammar deviations are noted in the respective sub-wave §1 deliverables; the canonical shape table here is the master contract.

Hereupon BA owns all-grammar migration; BB.W1 retires. The construct-by-construct emission patterns for W5b..W5e:

### §2.1 — CSS L4 (W5b) deviations

- **Alt (speculative case)** per §1.2 — CSS L4's `value` rule has overlapping FIRST sets (color names overlap with function names); speculative `attempt_p` with O(1) tuple Checkpoint, not `Vec<OpenFrame>::clone`.
- **Keyword (≥ 4 case)** per §1.7 — CSS L4 declares many color names (`red`, `green`, `blue`, `aqua`, ...); `phf::Map` lookup emits.
- **CharClass (medium-density)** per §1.6 — CSS L4's identifier and length-unit classes are mid-density; SIMD shuffle emits per `simd_eligibility = "auto"` metadata.
- **HostCall** per §1.10 — CSS L4 has multiple host fns (`parse_hex_color`, etc.); per surgery #15 host fns relocate to `crates/core/src/grammar/host/css_l4/` per-grammar namespace.

CSS L4's full per-construct deviation table is at `docs/tranches/BA/waves/W5b.md` §1.

### §2.2 — BBNF (W5c) deviations

- **Pratt operator chain** — BBNF carries grammar operators (`|`, `,`, `&`, `=`); operator-token lookup table emits with recursive descent for operands. Per `feedback_grammar_closures` BBNF's operators carry first-class closures and beta-reduction at compile time.
- **HostCall** per §1.10 — BBNF has host fns for grammar-introspection (e.g. `@debug` markers per `project_debug_infra`); under surgery #15 extension, host fns relocate to `crates/core/src/grammar/host/bbnf/`.

BBNF's full per-construct deviation table is at `docs/tranches/BA/waves/W5c.md` §1.

### §2.3 — Sheets (W5d) deviations

- **Per-leaf direct emission** — Sheets' specialised leaf-deposit pattern (cell_ref, identifier, sheet_prefix, error) per `audit/MODULES-2026-05-03.md:963` becomes per-leaf direct-projection.
- **Scanner (regex DFA)** per §1.8 — bespoke regex DFA for cell_ref (`[A-Z]+[0-9]+`), range, error literals (`#REF!`, `#DIV/0!`, `#NAME?`).
- **Arena-fallback resolution** per surgery #18 — `arena.rs:38, 40, 103, 153` "fallback" comments resolve to explicit panic-on-violation OR delete unreachable arms.

Sheets' full per-construct deviation table is at `docs/tranches/BA/waves/W5d.md` §1.

### §2.4 — Cohort (W5e) — five hand-written grammars

The cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) each exercise a strict subset of constructs from §1; W5e produces five hand-written direct-to-struct modules. Per-cohort exercises:

| Cohort grammar | Constructs | Notable deviations |
|---|---|---|
| BNF | Alt (byte-disjoint), Seq, Repeat, Optional, CharClass, Keyword | none — canonical exemplar |
| CSV | Alt (byte-disjoint), Seq, Repeat, CharClass, Scanner | scanner regex for quoted-field |
| EBNF | Alt (byte-disjoint, speculative), Seq, Repeat, Optional, CharClass, Keyword | speculative Alt for choice operators |
| CSS Pretty | Alt (byte-disjoint, speculative), Seq, Repeat, Optional, CharClass, Keyword, MapExpr | typed-enum constructors per declaration |
| Math | Alt (byte-disjoint), Seq, Repeat, CharClass, Keyword, Pratt operator chain | Pratt for arithmetic operators |

Cohort full per-construct deviation tables are at `docs/tranches/BA/waves/W5e.md` §1. BB.W2 consolidates the five hand-written modules into one parameterised template per gap D (`docs/tranches/BB/audit/W2-cohort-template-spec.md`).

## §3 — Sample-emission compilation note

Each sample emission above must compile mentally: the types resolve (`JsonValue<'p>`, `JsonPair<'p>`, `JsonObjectId`, `JsonArena<'p>`, `ScanState`, `PathCursor`, `ParseErr` are all extant per `audit/MODULES-2026-05-03.md:938-942`); the `arena.intern_object(...)` surface mirrors the existing `crates/core/src/runtime/json/arena.rs:139` interner; the `SmallVec<[_; 8]>` is the per `audit/RESTART-SKETCH-2026-05-03.md:215` recommendation. Post-W5 regen produces these shapes; the closer-gate's `rg -n 'match first {'` and `rg -n 'OpenFrame' crates/core/src/grammar/generated/json.rs` returns 0 verifies.

## §4 — Closer reference

The W5 close (W5e) confirms the per-construct shapes for all nine grammars by:

1. Reading post-regen `crates/core/src/grammar/generated/json.rs` (W5a) and verifying the `parse_value` body matches §1.1's emission.
2. Reading post-regen `crates/core/src/grammar/generated/{css_l4,bbnf,google_sheets,bnf,csv,ebnf,css_pretty,math}.rs` (W5b..W5e) and verifying per-grammar deviations match §2.1-§2.4.
3. Bench-confirming BA-G1 (`twitter ≤ 400 µs`) — JSON-side performance proof.
4. Allocation-confirming BA-G2 (`≤ 2 heap allocations per parse-call`) — JSON-side allocation discipline proof.
5. Verifying `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 across all nine grammars (Lock 1 honoured at BA close).
