# DEEPX-3 — Google Sheets Audit (Sonic-Class Parity + Profile)

**Agent**: DEEPX-3 / SHEETS-WITNESS
**Base**: master `40e1835d` (Phase-1 plan-surgery commit)
**Mandate**: richer profiling for full Google Sheets; semantic gaps; sonic-class API generalised; bind the Flat-shape lazy carry to a wave.

This audit closes the Sheets thread the synthesis left open. Three of
the four-grammar dedup outliers (JSON / CSS L4 / BBNF) are recurrently
observed in DEEP-A/B/C. Sheets is the one whose shape (Flat-of-Pratt,
positional compounds, no Object/Array) is structurally distinct enough
that the W3 lazy mechanism does not reach it; this document names the
mechanism the carry must close.

---

## I — Sheets Surface Inventory

Files (read-only): `grammar/google-sheets/google-sheets.bbnf` (185),
`crates/core/src/grammar/generated/google_sheets.rs` (14,088 generated),
`crates/core/src/grammar/generated/google_sheets.registry.json` (1,250),
`crates/core/src/runtime/google_sheets/{value,builder,arena,parse_with}.rs`
(189 / 357 / 332 / 114), `runtime/google_sheets/document/{mod,
path_query,canonical,view}.rs` (split per AUDIT-B F2),
`crates/core/benches/google_sheets/{monolithic,vm}.rs` (166 / 128),
`crates/core/tests/parse_with_google_sheets.rs` (58),
`crates/core/tests/google_sheets_slab.rs`.

### Fixtures (3 cold tiers, line-per-formula)

| Fixture | Path | Bytes | Shape |
|---|---|---:|---|
| simple | `data/sheets/simple.txt` | 505 | 34 single-call formulas (`=A1+B1`, `=SUM(A1:A100)`, scalar funcs) |
| nested | `data/sheets/nested.txt` | 1,456 | LET / LAMBDA / FILTER / MAP / IFERROR composition (~32 lines) |
| stress | `data/sheets/stress.txt` | 1,838 | Deep nesting; LET-of-LAMBDA-of-FILTER-of-INDEX, REDUCE-of-LAMBDA, deepest fixture |

---

## II — Profiling Truth (post-AZ-IV bench rows)

`docs/benchmarks/post-AZ-IV.json` carries five Sheets parse rows + two
format rows + the bbnf-monolithic Sheets cross-grammar row.

| Row | ns/iter | MB/s | AU floor | Δ |
|---|---:|---:|---:|---:|
| `google_sheets_monolithic.parse_simple` | 106,700 | 4.73 | 5,271 | **−20×** BELOW |
| `google_sheets_monolithic.parse_nested` | 429,300 | — | 11,333 | **−38×** BELOW |
| `google_sheets_monolithic.parse_stress` | 977,000 | — | 15,121 | **−65×** BELOW |
| `google_sheets_monolithic.format_simple` | 165.7 | — | 140 | −1.18× (within noise) |
| `google_sheets_monolithic.format_stress` | 4,332 | — | 3,813 | −1.13× (near noise) |
| `bbnf_monolithic.google_sheets` (cross-grammar) | 335,800 | — | 8,731 | **−38×** BELOW |
| `bbnf_monolithic.bbnf_self` (Pratt-tower analog) | 488,000 | — | 13,003 | **−37×** BELOW |

### Why parse-* is 20-65× regressed; format-* hits noise

The W6.1 close note in `post-AZ-IV.json:123` and DEEP-B §"Why is the
AU floor 18/19 BELOW?" name the mechanism: AZ-IV.W5's per-grammar
arena/builder template indirection costs **K constant per compound
emission** — one heap allocation for the layout + one heap allocation
for the frame's Vec. Sheets's grammar exposes this maximally because
**the Pratt operator-precedence tower allocates a new compound per
nesting level even on `=A1+B1`**: the cheapest 7-byte formula descends
through Formula → Expression → ComparisonExpr → ConcatExpr → AddExpr →
MulExpr → ExpExpr → UnaryExpr → PostfixExpr → Primary → Cell →
CellRef = **11 nested compounds for 7 bytes of input**. When fixture
compound density is high (parse_stress; LET/LAMBDA-of-FILTER), K
dominates absolute wall — observed 65×. When density is low
(format_*; one descent over an in-memory tree without re-parse), K is
small enough rows hit AT_OR_ABOVE / noise.

DEEP-B's deeper attribution (samply, 25,963 samples on
`bbnf_value_twitter`) lands the canonical hotspot at **86.07%
inclusive** on `<JsonStructBuilder as StructBuilder>::checkpoint` —
which deep-clones `Vec<OpenFrame>` per speculative branch. The Sheets
analog is `<SheetsStructBuilder as StructBuilder>::checkpoint`
(`builder.rs:182-189`):

```rust
fn checkpoint(&self) -> Self::Checkpoint {
    SheetsStructCheckpoint {
        compounds: self.arena.compound_count(),
        stack: self.stack.clone(),       // ← Vec<Frame> deep-clone
        root: self.root,
        next_handle: self.next_handle,
    }
}
```

Where `Frame { kind, children: Vec<SheetsValue<'p>>, handle_token }` —
each clone walks the entire open-frame stack and clones its child
vectors. Sheets fires this at every speculative branch in the Pratt
tower (every `add_op` / `mul_op` / `compare_op` / `unary_prefix`
candidate). Sheets is **the more expensive case** of the JSON
mechanism, not a separate one.

### Per-row attribution (no separate samply yet — routed)

A dedicated samply wave on `google_sheets_monolithic` was deferred at
W6.2. **BA.W3 should land sheets-specific samply rows** alongside the
predictive-dispatch refactor — otherwise we cannot tell whether the
65× stress regression is clone-cost (same as JSON) or Pratt-frame-
allocation-cost (Sheets-specific, from 11+ compounds per 7-byte
formula).

---

## III — Why Sheets Compounds Are Positional ("Flat" shape)

JSON has `Object` (key-step navigable: `path!(Json, "user", "name")`)
and `Array` (index-step navigable: `path!(Json, "items", 3)`). Sheets
has neither.

### The structural reason

Sheets's grammar is a Pratt operator-precedence tower over compounds
whose shape is `Seq(head, body+)` — an outer rule with a head
expression followed by zero-or-more `(operator, operand)` repetitions:

```bbnf
add_expr = mul_expr ?w , (add_op ?w , mul_expr ?w) *
mul_expr = exp_expr ?w , (mul_op ?w , exp_expr ?w) *
comparison_expr = concat_expr ?w , (compare_op ?w , concat_expr ?w) *
```

Plus delimited compounds with positional children:

```bbnf
func_call = func_open , (func_args ?) ?w , ")"
func_args = (arg << comma ?) +
let_call = /[lL][eE][tT]\(/ , let_args ?w , ")"
array_literal = "{" , (array_rows) ?w , "}"
range_ref = sheet_prefix ? , range_end , ":" , range_end
```

There are **no key=value records** — `LET(name, value, name, value,
body)` is paired but its pairing is positional (every two items in
`let_args` is `(name_expr, value_expr)`), not field-keyed. There are
no `Field` segments to navigate by name; only `Index` segments to
navigate by position.

The `path_query::walk_path` walker (`document/path_query.rs:35-50`)
codifies this: `(SheetsValue::Compound, PathSegment::Index(idx))` is
the only match arm; `Field` and any leaf+segment combination return
`None`. Comment in `path_query.rs:6-9`: *"Sheets compounds are
positional, so the walker uses `PathSegment::Index` only; a
`PathSegment::Field` step against a Sheets compound returns `None`"*.

### The shape classification

The IR's `shape_dispatch` pass tags Sheets rules from
`{Pratt, Flat, Wrap, Keyword, ArgList}` — none are `Object` or
`Array`. The shape detectors (`is_w3_classified` vs
`is_w4_classified` in `crates/ir/src/passes/recognizers/shape_dispatch/mod.rs:189-216`)
split:

- **W3-classified**: Object, Array, String, Number, Keyword, Scalar
- **W4-classified**: Pratt, Unordered, ArgList, Flat, Wrap, HRegex,
  AltDispatch

Every Sheets compound rule is in the **W4 set**. The lazy bail-out
mechanism (W3-DYNAMIC) was designed for W3-classified shapes only.

### Architectural feature, not artefact

The Flat / Pratt / Wrap shape is the correct projection of an
expression-tower grammar; Sheets cannot be coerced into Object/Array
without violating its semantics. What *is* an artefact: the W3 lazy
mechanism gating Object/Array only. Bindable to BA — see §V.

---

## IV — `SheetsValue` and the Sonic-Class API

### The typed value sum (closed)

`SheetsValue<'p>` (`runtime/google_sheets/value.rs:69-126`) — `Copy`,
nine variants:

```rust
pub enum SheetsValue<'p> {
    Number(f64),                                  // number -> f64
    String(&'p str),                              // string -> input : Span
    Bool(bool),                                   // boolean -> true | false
    Error(u8),                                    // error_literal -> Nu8 (9 branches)
    CellRef(&'p str),                             // cell_ref -> input : Span
    Identifier(&'p str),                          // identifier -> input : Span
    SheetPrefix { tag: u8, text: &'p str },       // sheet_prefix -> Nu8 + span
    Tag(u8),                                      // compare/add/mul/unary_op -> Nu8
    Compound(SheetsCompoundId),                   // every compound rule
}
```

Compound rules (16 distinct kinds: Formula, Expression, ComparisonExpr,
ConcatExpr, AddExpr, MulExpr, ExpExpr, UnaryExpr, PostfixExpr, Primary,
ParenExpr, FuncCall, LetCall, LambdaCall, ArrayLiteral, FuncArgs,
LetArgs, LambdaParams, ArrayRow, ArrayRows, FuncOpen, Arg, LetBinding,
Cell, RangeRef, RangeEnd, CellOrRange, ErrorLiteral, SheetPrefix,
CompareOp, AddOp, MulOp, UnaryPrefix, Wrap) all collapse onto
`SheetsValue::Compound(SheetsCompoundId)`. The arena entry
`SheetsCompound { kind: SheetsCompoundKind, children: Vec<SheetsValue<'p>> }`
preserves the rule-level shape via the `kind` discriminator.

### The path-query trait

`SheetsPathQuery` (`document/path_query.rs:25-29`) is implemented for
five terminal types:

| Type | Variants accepted |
|---|---|
| `f64` | `Number(_)` |
| `bool` | `Bool(_)` |
| `u8` | `Tag(_)`, `Error(_)`, `SheetPrefix { tag, .. }` |
| `&str` | `String`, `CellRef`, `Identifier`, `SheetPrefix.text` |
| `SheetsValue<'_>` | (any — passthrough) |

### `SheetsParser::parse` and `parse_with` exist

- **Eager**: `GoogleSheetsParser::parse(&str) -> Result<SheetsDocument, ParseErr>`
- **Lazy**: `runtime::google_sheets::parse_with::<T>(&str, &TypedPath<Sheets, T>) -> Option<T>`

`parse_with` is the same shape as JSON / CSS L4 / BBNF — the four W3
grammars share an entry-point template. Path-cursor is constructed,
the codegen-emitted `__path_plan::lookup` is wired, the dispatcher is
threaded through `parse_GoogleSheetsParser_formula`, and after
returning the document is finalised + `get::<T>` projected.

### The `path!(Sheets, ...)` macro is wired

`crates/bbnf-path/src/registry.rs:94`: `"Sheets" => Some("google_sheets")`.
Static cache slot at `:131-135`. `supported_markers()` lists Sheets at
`:102`. The proc-macro reads
`crates/core/src/grammar/generated/google_sheets.registry.json` and
validates path segments at compile time. **`path!(Sheets, ...)` works
end-to-end today.**

### What sonic-class for Sheets looks like

```rust
// Today (works on eager path):
let doc = GoogleSheetsParser::parse("=SUM(A1:A100)")?;
let func_name: Option<&str> = doc.get(path!(Sheets, /* positional index path */));

// After BA.W4 (lazy short-circuit):
let func_name: Option<&str> = GoogleSheetsParser::get(formula_text, path!(Sheets, ...));

// Wildcard iteration (BA.W4 + wildcard lane):
for cell_ref in GoogleSheetsParser::get_iter(formula_text, path!(Sheets, ..., "*")) { … }
```

### What is missing for Sheets sonic-class

1. **Variant-name (`VariantName`) navigation against typed compounds.**
   Today `path!(Sheets, "ParenExpr", 0)` cannot disambiguate "the
   inner compound when it is a paren_expr"; `VariantName` lowers to
   `Field` in `parse_with.rs:37`, and `Field` against a Sheets compound
   returns `None`. The `Compound(id)` arm in `walk_path` has no
   variant-aware step. **Closing this requires the `kind` discriminator
   to participate in path resolution** — the typed enum equivalent of
   variant-select that already lands for `CssTypedValue::Color(_)`
   today. BA.W2's direct-projection codegen makes this trivial: the
   per-rule projected struct (e.g. `SheetsParenExpr { inner: Box<SheetsValue<'p>> }`)
   has named fields and the macro can infer them from the registry.

2. **Field navigation on the structurally-named children.** A
   `range_ref` compound has positional children `[sheet_prefix?,
   range_end, range_end]`. The path-query walker treats them as
   `Index(0..3)` only; the user must remember the position. With
   typed projection (BA.W2), `path!(Sheets, "range_ref", "start")`
   becomes meaningful — the registry's field-name metadata is already
   in the `StructLayout` (visible in `google_sheets.registry.json`
   under each Struct-kind layout's `fields[*].name`).

3. **A "common queries" surface** for formulas: extract function
   name, iterate cell references, iterate function calls. These are
   common enough that a per-grammar idiom layer is warranted (a
   `SheetsQuery` module with named accessors over `path!(Sheets, …)`
   common patterns).

---

## V — The Flat-Shape Lazy Mechanism (the named carry)

### What W3-DYNAMIC is

The W3-DYNAMIC mechanism (`backend/rust/emitter/shapes/object.rs:260-272`,
`backend/rust/emitter/shapes/array/mod.rs:230-245`) integrates the
path cursor into Object/Array shape parse loops:

```rust
// Object loop body sketch:
let __decision = cursor.decide(rule_id);              // prime cache
loop {
    let __key = parse_key(input, p, ...)?;
    let __seg_kind = cursor.current_kind();
    let __matched = if __seg_kind == SegmentKind::Field {
        cursor.match_field(__key)
    } else { false };

    if __is_field_seg && !__matched {
        builder.rollback(__key_checkpoint);
        #support_mod::byte_skip_value(input, p)?;     // ← skip value bytes
    } else {
        #value_call;                                   // ← parse normally
    }
    // ... comma / close handling, with early-bail when __matched
}
```

The `byte_skip_value` family (4 helper fns: `byte_skip_value`,
`byte_skip_balanced`, `byte_skip_string`, `byte_skip_scalar` —
emitted into every grammar's `__shape_support_*` module per
`grep -c byte_skip_value`: json 5, ebnf 4, bbnf 4, math 3, sheets 3,
css_l4 3) is generic infrastructure. **Sheets has the helpers
emitted; nothing calls them**. The Flat/Pratt/Wrap/Keyword/ArgList
emitters all carry `let _ = cursor;` (silence the cursor parameter)
or only `cursor.decide(rule_id)` to prime the cache.

### Where Sheets needs the analogous mechanism

The Flat-shape `Seq(head, body+)` body in
`backend/rust/emitter/shapes/flat/struct_direct.rs:425-432`:

```rust
let __body_result: Result<(), DtaError> = (|| {
    // Per-position emission.
    #(#emissions)*
    Ok(())
})();
```

The emissions iterate the rule's flattened positions and call shape
fns / `dispatcher_ident` per position. **The cursor is never consulted
between positions**. There is no "did the path's reach end inside the
just-parsed position; should I bail before parsing the next?" check.

Concretely: on `=42 @@@ malformed`, the formula rule descends:
Formula(`/=?/`, expression) → expression → comparison_expr → …
→ primary → number → 42. After the number is captured, the position
walker advances to the next sibling (which doesn't exist for `formula`
— it's `Seq("=?", expression)` only); the closing bracket-walk fails
on `@@@`, and the eager-style finalise raises a parse error. The lazy
contract requires bailing out *between* the number's deposit and the
next position.

### The mechanism, named

**`Flat-shape early-bail` (BA candidate name)**: at each position
boundary inside a Flat / Pratt / Wrap / Keyword / ArgList body, after
the position emits its compound or leaf, consult
`cursor.has_resolved()` (the cursor maintains its terminal-reach state
already); if true, close the open compound frame and return success
without inspecting subsequent bytes.

The minimum-viable shape:

1. **Cursor surface addition**: `PathCursor::has_resolved() -> bool`
   (returns `true` when `cursor.current_kind() == SegmentKind::Done` —
   i.e. all segments consumed and the terminal payload is in flight).
   Probably already exists; the W3-DYNAMIC sites in object.rs/array.rs
   already check `__matched` for the same condition.

2. **Emission addition** at every Flat / Pratt / Wrap / Keyword /
   ArgList position-boundary in the body:

   ```rust
   // After each position emit:
   if cursor.has_resolved() {
       /* close open frame, return Ok */
       <#builder_ty as StructBuilder>::record_compound_bounds_end(builder, *p as u32);
       <#builder_ty as StructBuilder>::end_compound(builder, #handle_var);
       return Ok(());
   }
   ```

3. **Test redress**: the two ignored Sheets tests
   (`crates/core/tests/parse_with_google_sheets.rs::lazy_error_elision_after_path_reach`
   and `crates/core/src/runtime/google_sheets/parse_with.rs::tests::parse_with_resolves_number_leaf`)
   un-ignore. The `=42 @@@` fixture exercises the mechanism directly.

### Why this lands in BA, not BB

This is not rule discovery; it is the W3-DYNAMIC mechanism extended
to the four W4-shape families that Sheets/CSS L4/BBNF rely on. Per
DEEP-SYNTHESIS §VII, BA's thesis is direct-projection codegen + lazy
canonical-as-eager-degenerate. The Flat-shape early-bail is the lazy
mechanism reaching the W4 shape families. **Bind to BA.W4** alongside
the `Document::get<T>` rerouting through `parse_with` —
Sheets-specific tests provide same-wave consumer evidence.

---

## VI — Sheets `StructRegistry` Audit (per-rule)

37 rules in `google_sheets.registry.json`. Layout-kind distribution:

| Kind | Count | Examples |
|---|---:|---|
| `NewtypeWrapper` | 7 | number, string, sheet_prefix, cell_ref, identifier (single typed-leaf rules) |
| `UntaggedEnum` | 6 | boolean, compare_op, add_op, mul_op, unary_prefix, cell_or_range (alt-of-keywords / alt-of-refs) |
| `Struct` | 23 | error_literal, cell, func_open, range_ref, comparison_expr, func_call, … (positional Seq) |
| `TaggedEnum` | 1 | primary (`HeterogeneousAltJoin[BoxedEnum, F64, Bool, Span]`) |

### Compile gaps observed

1. **`primary` is the only TaggedEnum** — its branch types span
   BoxedEnum (let_call/lambda_call/func_call/cell_or_range/identifier/
   array_literal/paren_expr — the recursive ones), F64 (number),
   Bool (boolean), Span (string). The `HeterogeneousAltJoin` is
   correct, but the emitter today projects this through the standard
   `parse_alt_dispatch` shape (per `ShapeTag::AltDispatch` in
   `shape_dispatch/mod.rs`). The TaggedEnum projection is consumed at
   codegen but the `SheetsValue::Compound` arm is the runtime
   landing — every primary alternative emits a `Compound` with
   `kind=Primary` even for primitive leaves. **BA.W2 direct-
   projection** should project primary's leaf branches directly to the
   typed leaf (`Number(f64)`, `Bool(b)`, `String(span)`) without an
   intermediate Primary compound — the `is_transparent_wrap` check at
   builder time (`arena.rs:138-143`) already forwards single-child
   primary compounds to the child's value, but the compound is still
   *built* (its handle still allocated, its children Vec still
   allocated). BA.W2 emission elides the build entirely when the
   TaggedEnum branch's projected type is a leaf scalar.

2. **`error_literal` is `Struct`** but the emission is two `Span`
   fields named `field_0` and `field_1` — the registry projects the
   `error_literal` keyword + branch_tag pair as positional `Struct`,
   not `UntaggedEnum`. This is correct under the current type
   inference, but it produces a `SheetsCompoundKind::ErrorLiteral`
   compound on the arena rather than landing the discriminant directly
   on `SheetsValue::Error(u8)`. The builder's `push_leaf_error(value)`
   surface (`builder.rs:354-356`) is called by specialised emission;
   the question is whether the *generated codegen* always routes
   through the specialised path or sometimes routes through the
   generic Struct shape. **BA.W2 should normalise this**: error_literal
   is structurally a one-of-9 keyword discriminator; its codegen
   should land `Error(u8)` directly without any compound build.

3. **Operator-tag rules** (`compare_op`, `add_op`, `mul_op`,
   `unary_prefix`) are `UntaggedEnum`. They land
   `SheetsValue::Tag(u8)` via `push_branch_tag` with no compound build.
   This is the cleanest path; no gap.

4. **Recursion: `paren_expr`, `func_call`, `let_call`, `lambda_call`,
   `array_literal`** — all `Struct`. The recursion lands through
   `Compound` handles. The arena's `push_compound(kind, children)` is
   the recursion point; clone-on-checkpoint walks the *entire stack*
   including all open compounds. **The SheetsStructBuilder checkpoint
   is the same Vec-clone hotspot as JSON's**, just exposed worse by
   Pratt depth.

5. **38 unique `SheetsCompoundKind` variants** vs. **23 Struct-kind
   rules** in the registry. The discrepancy is the kinds for rules
   that emit no struct layout (Expression, Primary, RangeEnd,
   SheetPrefix, CompareOp, AddOp, MulOp, UnaryPrefix — the Wrap /
   forwarder rules). These are retained "for AST exhaustiveness" per
   `arena.rs:182-187` — they exist as enum variants but never
   instantiate via `from_layout`, falling through to `Self::Wrap`.
   **This is dead-by-design**; the `from_layout` match in `arena.rs:155-189`
   has 25 actual arms + a `_` fallback. BA.W2 direct-projection
   eliminates this — the per-rule projected struct *is* the kind, no
   external enum needed.

---

## VII — Why Sheets Is a W5.3 Dedup Outlier

`docs/tranches/AZ-IV/audit/W5-arena-builder-dedup.md` codifies the
four outliers (JSON, CSS L4, Sheets, BBNF). Sheets's listed rationale
(`W5-arena-builder-dedup.md:24`):

> **Google Sheets** — `SheetsCompound { kind, children }` shape (no
> `branch_tag`), `push_compound(kind, children)` signature,
> view-returning `compound`. Distinct shape.

The simple-cohort template `SimpleStructBuilder<'p, V, C>` carries an
implicit `branch_tag: Option<u8>` on every compound (for typed-enum
projections that need a discriminator inline with the children).
Sheets does *not* — its discriminator is the `kind` enum itself, set
at `begin_compound` via `from_layout`, and its branch tags land as
leaf `Tag(u8)` values inside the compound's children Vec. The
template would have to widen with another generic parameter to absorb
Sheets, or Sheets would have to thread a synthetic always-None
branch_tag through every push.

**Per `feedback_no-god-modules`** the audit kept Sheets distinct.

### What changes under BA.W2 direct-projection

Once each compound rule emits a typed Rust struct (e.g. `struct
SheetsAddExpr<'p> { head: Box<SheetsValue<'p>>, tail: Vec<(u8, Box<SheetsValue<'p>>)> }`),
the arena/builder template *itself* retires (per DEEP-C §"What this
generalization replaces"). The four outliers stop being outliers
because the template stops existing. **The W5.3 outlier classification
is itself a temporary shape** that closes when BA.W2 lands.

---

## VIII — BA Recommendations for Sheets

Bound to the BA wave shape per DEEP-SYNTHESIS §VII:

### BA.W2 (direct-projection codegen) — Sheets coverage

- **Emit per-rule typed struct** for every Struct-kind layout in
  `google_sheets.registry.json` (23 rules). Field names per layout's
  `fields[*].name`. Replace `SheetsCompoundKind::from_layout` runtime
  registry resolution with an inline `const KIND: SheetsCompoundKind`
  per emitted parse fn.
- **Project `primary` TaggedEnum branches directly** to leaf scalars
  where the branch's `type_desc` is `F64` / `Bool` / `Span` — elide
  the Primary compound build for these branches. `BoxedEnum` branches
  (the recursive cases: let_call, lambda_call, func_call,
  cell_or_range, identifier, array_literal, paren_expr) keep their
  Compound projection.
- **Project `error_literal` directly** to `SheetsValue::Error(u8)` —
  no ErrorLiteral compound on the arena.
- **Eliminate `__layout: StructLayout { rule_type: Span, fields:
  vec![] }`** placeholder construction at every begin_compound site
  (per DEEP-A's nine-emission-site count for the cohort; Sheets's
  share is the 23 Struct-kind rules' begin_compound sites).
- **Eliminate `compound_kind_for_layout` runtime resolution** —
  replace with codegen-time const projection.

### BA.W3 (speculative checkpoint redesign) — Sheets samply

- **Land samply attribution** on `google_sheets_monolithic.parse_stress`
  (the worst regression, 65× BELOW). Confirm or refute that
  `SheetsStructBuilder::checkpoint` shows the same ≥80% inclusive
  hotspot the JSON case shows. **Per the discipline in `feedback_actual_profiling`**:
  do not extrapolate from JSON's profile.
- **Replace `Vec<Frame>::clone`** with `(stack_depth, arena_count)`
  value-typed checkpoint. The Frame's owned `Vec<SheetsValue<'p>>`
  becomes redundant once each compound rule emits its own typed
  body — children are written directly to typed fields, not pooled in
  a generic Vec.
- **Predictive first-byte dispatch in Sheets's Pratt towers** —
  `add_op` is `+ | -`, `mul_op` is `* | /`, `compare_op` is
  `<> <= >= = < >` (six branches), `unary_prefix` is `+ | -`. All
  byte-disjoint. Predictive dispatch eliminates the speculative
  checkpoint at every operator boundary.

### BA.W4 (parse_with as value-API hot path) — Flat-shape early-bail

- **Land the Flat-shape early-bail mechanism** (§V above). The shape
  emitters (Flat, Pratt, Wrap, Keyword, ArgList) consult
  `cursor.has_resolved()` between positions and bail after closing
  the open compound. Test fixtures: the two currently-ignored Sheets
  tests un-ignore.
- **Sonic-class fixtures** for Sheets — at minimum:
  - `path!(Sheets, /* canonical path to a leaf number */)` →
    `Some(f64)` on a happy formula
  - `path!(Sheets, /* canonical path to a cell_ref */)` → `Some(&str)`
  - `GoogleSheetsParser::get(formula, path)` — short-circuit lazy
    surface (the `Document::get<T>` reroute through `parse_with`)
  - **Variant-aware path step**: with BA.W2's typed projection,
    `path!(Sheets, "func_call", "func_open", "identifier")` should
    descend through the typed-struct field names rather than positional
    indices. This is the variant-select test the synthesis names; the
    Sheets case exercises it on a Pratt-tower grammar (vs. CSS's
    flat-property grammar).

### BA.W5 (cursor consult unification) — Sheets parse_with cleanup

- **Retire LegacyPath shim** in `runtime/google_sheets/parse_with.rs:74-78`
  (the `legacy: Vec<LegacySegment<'_>>` allocation per parse). The
  document's `get::<T>` consumes `TypedPath` directly.

### BA.W6 (close measurement)

- **Sheets parse-row floor**: `google_sheets_monolithic.parse_simple`
  AT_OR_ABOVE the 5,271 ns AU floor. Same target on parse_nested
  (11,333 ns) and parse_stress (15,121 ns). The 20-65× regression is
  the canonical measurement of BA.W2 + BA.W3's success on this grammar.
- **Bench-row truth in BA's post-bench JSON**: row-by-row floors
  comparison vs. `post-AZ-IV.json` and `post-AU.json`.

---

## IX — Summary Table

| Question | Answer |
|---|---|
| Is the Flat-shape lazy gap an architectural feature? | **No, an artefact** — the W3-DYNAMIC mechanism gates Object/Array only. The mechanism extends naturally to W4 shapes via `cursor.has_resolved()` consult between positions. |
| Is positional Sheets navigation an artefact? | **No, a feature** — Sheets's grammar is genuinely positional (Pratt tower + delimited compounds with positional children). Variant-aware navigation through *typed* projection (BA.W2) is the orthogonal improvement. |
| Does `path!(Sheets, ...)` work? | **Yes, end-to-end** through the bbnf-path proc-macro + the `__path_plan::lookup` codegen + the `SheetsPathQuery` trait. |
| Is `SheetsValue` the sonic-class typed value? | **Yes** for leaves; **partially** for compounds (the `Compound` arm collapses every compound shape to a single arena handle; per-rule typed projection lands at BA.W2). |
| Is samply attribution available for Sheets parse-* rows? | **No, deferred** at W6.2. BA.W3 should land it. |
| Why is Sheets a W5.3 dedup outlier? | The Sheets compound shape (`{ kind, children: Vec }`, no per-compound `branch_tag`) does not fit the simple-cohort template; widening the template would either eat the dedup (per `feedback_no-god-modules`) or force a synthetic always-None branch_tag through every push. **The outlier classification retires when BA.W2 lands** (the template itself retires). |
| Mechanism name for the Flat-shape lazy carry | **Flat-shape early-bail** — `cursor.has_resolved()` consult between positions in Flat / Pratt / Wrap / Keyword / ArgList shape emitters; close open frame and return success when terminal reached. **Bound to BA.W4.** |
