# AZ-IV.W1-CLOSE — Research Lane

**Lane**: research (read-only)
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-close-research`
**Base**: `f4ab9e90` (post-W1-zero)

This artefact gathers the root-cause evidence that closes the residual
13 W1 failures. Each defect is named with file:line plus the commit
that seeded the regression; the §5 footprint enumerates every surface
the redress lane must touch.

The 13 failures partition into three independent defect classes;
classes A and B share no IR, runtime, or codegen code paths. The Pratt
mining defect (A.2) and the namedColor codegen defect (B.1) live at
opposite ends of the pipeline (IR analysis vs Rust struct-direct
emitter); the dir_pseudo defect (B.3) is a runtime-builder routing
gap; the TS Color defect (C) is a backend-ts type emission gap.

---

## §1 Class A — Sheets `sheets_self_parity` (7 failures)

### A.1  Inventory

| Test | Failing input | First-pass output | Second-pass result |
|---|---|---|---|
| `corpus_simple` | `=CONCATENATE(A1, " ", B1)` | `=CONCATENATE(A1, ,B1)` | mismatch (string `" "` round-trips as bare ` `) |
| `corpus_nested` | `…IFERROR(…, "N/A")` | `…IFERROR(…,N/A)` | mismatch (quotes stripped) |
| `corpus_stress` | `…"No results"…` | `…No results…` | mismatch (quotes stripped) |
| `serialize_roundtrip_array_literal_single_row` | `={1,2,3}` | `={1,,2,,3}` | parse fails at offset 1 |
| `serialize_roundtrip_array_literal_multi_row` | `={1,2;3,4}` | similarly doubled | parse fails |
| `serialize_roundtrip_range_ref_column` | `=A:A` | `=` | parse fails at offset 1 |
| `serialize_roundtrip_string_empty` | `=""` | `=` | parse fails at offset 1 |

The "Syntax { offset: 1, rule: None }" panic from `sheets_self_parity.rs:36`
is the SECOND parse pass — `assert_serialize_fixed_point` parses src
once, reserialises, parses the reserialisation, and compares. The
first parse always succeeds; the round-trip serialiser produces
non-parseable output.

### A.2  Pratt-mined operator alphabet over-shares across rungs

`array_row` and `array_rows` in `grammar/google-sheets/google-sheets.bbnf:159-160`
are not arithmetic Pratt towers — they are list rules with rung-specific
separators (`,` for rows, `;` between rows). `collect_precedence_chain`
at `crates/ir/src/passes/recognizers/dta.rs:965-1031` admits any pair
of rules whose bodies match `Seq(operand, Repeat(Seq(op, operand)))`
and assigns a single `PrecedenceTable` whose `entries` carry both
operators (`,` precedence 1 + `;` precedence 2). `collect_operator_chains`
at `crates/ir/src/passes/recognizers/operator_chain.rs:227-266` then
projects the COMBINED entries onto BOTH rules, so the per-rule
`PRECEDENCE_LUT_array_row` and `PRECEDENCE_LUT_array_rows` each hold
both `,` (byte 44) and `;` (byte 59) — verified in
`crates/core/src/grammar/generated/google_sheets.rs:155-232`.

Pratt emission at `crates/core/src/backend/rust/emitter/shapes/pratt/struct_direct.rs:272-338`
loops through every byte the LUT marks, with NO precedence comparison
in the loop body. So when `array_row` is invoked for `1,2,3`, it
consumes all three commas as Pratt operators, depositing `[expr(1),
Tag(0), expr(2), Tag(0), expr(3)]`. The runtime serializer at
`crates/core/src/runtime/google_sheets/document.rs:281-288` (the
`SheetsCompoundKind::ArrayRow` arm) emits `,` between EVERY pair of
children, including between an expression and the operator Tag —
producing `1,Tag,2,Tag,3` where Tag's `tag_lexeme(ArrayRow, 0)` falls
through to the empty-string default at `document.rs:485-489`. Result:
`1,,2,,3`.

The regression entered at `0ffbd754`'s post-discriminator regen and
became blocking when the W1.2 typed-Nu8 lift restored the IR's
operator-tag projections — `c56bda3f` and downstream regens. The
shape-detector's "operator-chain rung" predicate accepts any Repeat-
of-Alt-of-literal-led tail; list rules satisfy this trivially.

### A.3  String regex captures inner body, not full quoted span

`grammar/google-sheets/google-sheets.bbnf:12` declares
`string = /"([^"]|"")*"/ -> input : Span` — the typed projection
should bind the FULL quoted match to the Span. The codegen at
`crates/core/src/grammar/generated/google_sheets.rs:2369-2456`
(`parse_string_GoogleSheetsParser_string`) uses the JSON-style
`first_quote_or_backslash` scanner from
`__shape_support_GoogleSheetsParser` (rule_id 1) and pushes
`builder.push_leaf_with_str(body)` at line 2398 with `body =
&input[body_start..end]` — the INNER bytes only.

The serializer at `document.rs:171-176` writes `SheetsValue::String(s)`
verbatim (no quotes added back), so every quoted string round-trips
without quotes. `=""` round-trips to `=`; `"N/A"` round-trips to
`N/A`; `" "` round-trips to ` `. The defect shape is identical to
W1.2's sheet_prefix collapse at `0e670141` — the codegen's lift of
JSON's `parse_string` shim into Sheets dropped the source-bytes
contract that `-> input : Span` required.

### A.4  `range_end` regex branches consume bytes without pushing leaves

`grammar/google-sheets/google-sheets.bbnf:75` defines
`range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/`. The codegen
for the regex branches at
`crates/core/src/grammar/generated/google_sheets.rs:3927-4107` uses
`__regex_scan_GoogleSheetsParser` to advance `*p` by the match
length, but emits NO `push_leaf_with_str` call for the matched span.
Only the cell_ref branch (which routes through
`parse_hregex_GoogleSheetsParser_cell_ref`) pushes a leaf.

For `=A:A`, the parser successfully consumes `A`, `:`, `A` (advancing
`*p` to 4), but the `range_ref` compound holds zero children.
Serializer at `document.rs:289-303` (RangeRef arm) iterates an empty
child list and emits nothing. Combined with `SheetsCompoundKind::Wrap`'s
single-child collapse, the formula degenerates to `=`.

This is the same defect class as A.3 — typed/structural projections
that consume bytes without depositing the matched span. The grammar's
`range_end` is implicitly `: Span` but the codegen treats the
non-Ref alts as "byte advance only", losing the source.

### A.5  Class A fix mechanism (research recommendation)

Three independent sub-fixes:

1. **A.2 Pratt-mining narrowing**: gate `collect_precedence_chain`
   admission on rung body shape. Rungs whose operator alphabet is a
   list-separator (single byte, no precedence-binding intent) should
   not collapse multiple rungs into one shared LUT. Mechanism:
   per-rule LUT scoping at
   `crates/ir/src/passes/recognizers/operator_chain.rs:227-266` —
   each chain rung receives ONLY its own operator entries, not the
   union. Or alternatively: drop `array_row`/`array_rows` from the
   `ShapeTag::Pratt` classification by tightening the predicate at
   `crates/ir/src/passes/recognizers/shape_dispatch/pratt.rs:87-128`
   to require ≥ 2 distinct operator bytes per rung (rules with a
   single-byte separator are list-shaped, route to Flat).

2. **A.3 String span capture**: rewrite the codegen at
   `crates/core/src/grammar/generated/google_sheets.rs:2369-2456`
   (or, more precisely, the emitter that produces it — the string-shape
   lift in `crates/core/src/backend/rust/emitter/shapes/string/struct_direct.rs`
   or its Sheets-routing analog) to push the full `[open..end+1]`
   span when the grammar's `-> input : Span` annotation is present.
   Sheets's `""` escape semantics also differ from JSON's `\"` — the
   scanner should pair on `""` not `\\` (a follow-up; the current
   test set doesn't exercise embedded escapes).

3. **A.4 range_end regex span capture**: every regex-led alt branch
   inside a `range_end` (or any rule whose declared `-> input : Span`
   propagates to the regex match) must push the matched span. The
   emitter at
   `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:108-125`
   already does this for AltDispatch shapes; the regression is in the
   Wrap-shape path that ate `range_end`'s wrap collapse. Restore the
   `push_leaf_with_str` emission for regex branches in Wrap context.

For A.2, the serialiser at
`crates/core/src/runtime/google_sheets/document.rs:266-288` ALSO
needs the Pratt-tower-style projection (skip operator Tags from the
separator emission, treat them as the separator themselves) because
some array_row outputs may legitimately carry Tag children when the
underlying expression contains a comparison/concat operator. After
A.2 narrows the Pratt classification, this serialiser path either
becomes dead or stays correct under the simpler list-shape emission.

Per-test impact: A.2 closes `array_literal_single_row`,
`array_literal_multi_row`, plus the `corpus_*` tests' inner
multi-arg-call serialisation. A.3 closes `string_empty`, the
`corpus_*` quoted-string round-trips, and any test consuming a string
literal. A.4 closes `range_ref_column`. The three sub-fixes are
independent.

---

## §2 Class B — CSS L4 (5 failures)

### B.1  Inventory

| Test | Input | Expected | Actual |
|---|---|---|---|
| `every_named_color_materialises_its_u32_payload` | `a { color: <name>; }` (150 colors) | `Some(0xRRGGBBAA)` per name | `None` for 150/150 |
| `white_materialises` | `a { color: white; }` | `Some(0xFFFFFFFF)` | `None` |
| `named_color_aliceblue_fires_inline_u32` | `a { color: aliceblue; }` | `0xF0F8FFFF` payload reaches typed graph | `[]` (no Hex variant materialises) |
| `dir_pseudo_ltr_branch_fires_payload` | `a:dir(ltr) { color: red; }` | selector list contains `:dir(ltr)` | absent |
| `dir_pseudo_rtl_branch_fires_payload` | `a:dir(rtl) { color: red; }` | selector list contains `:dir(rtl)` | absent |

### B.2  namedColor — typed payload lost on prefix-factored branches

`crates/core/src/grammar/generated/css_l4.rs:4667-12136`
(`parse_altdispatch_CssL4Parser_namedColor`) shows 20 outer branches.
Only branches 0 (`violet`, line 4690-4704) and 1 (`khaki`, line 4705-
4719) retain the
`push_leaf_with_u64(<packed_u32>)` + `push_branch_tag(N)` pair —
because they happen to be flat singletons after the IR's
prefix-tree-factor pass.

The remaining 18 branch groups (`a*`, `b*`, …, `ye*`, `yw*`)
are nested Seq+Alt structures: e.g. branch 2 is
`Seq(Literal("a"), Alt(["liceblue", "ntiquewhite", …] -> u32))`. At
the outer dispatch level, only `push_branch_tag(N)` fires (line 287
for branch 2; no `push_leaf_with_u64`). The 148 inner Map-with-IntLit
projections never emit their u32 — verified by grep: only 2 of 20
branch groups carry a `push_leaf_with_u64` emission.

The seam lives at
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:227-282`.
`branch_payload_push` walks the OUTER branch root via `find_map_fn`
(lines 228-243): handles `Map`, `OptionalWhitespace(Map)`, and
`Seq(..., Map_at_last_position)`. It does NOT descend into nested
`Alt` children. After prefix-tree factoring, the outer branch root
is `Seq(Literal("a"), Alt([map(..., Literal), map(...), ...]))` —
`find_map_fn` looks at the LAST Seq child (the inner Alt), returns
`None`, falls through to the `unit_push` default at line 244-246.

The structural emission of the inner Seq+Alt happens at
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:165-196`
through `inline::emit_seq_branch_structural_struct_direct` — that
helper emits inner Alt branches WITHOUT consulting the per-arm
`Map { fn_id }` payload. Each inner arm matches its literal suffix
and breaks out of the outer dispatch with only the outer branch-tag
emitted.

The substrate the W1.5 halt named (`branches.rs:227-298`) is real —
it correctly emits `push_leaf_with_u64` when given a Map root. The
gap is the IR-traversal coverage: prefix-tree factoring rotated the
Map nodes into nested Alts that `find_map_fn` cannot reach.

The regression entered with the prefix-tree-factor rewrite that the
W1.2 typed-Nu8 lift restored (it was previously suppressed when the
Map nodes got dropped at lowering); commit `758e69d6` regenerated
the namedColor codegen against the corrected IR, but the codegen's
walker stayed shallow.

### B.3  dirKeyword tag misrouted as GlobalKeyword

`grammar/css/l4/selectors.bbnf:67-68` declares
`dirKeyword = "ltr" -> 0u8 | "rtl" -> 1u8` and
`dirPseudo = ":dir" , "(" >> dirKeyword << ")"`.

The codegen at
`crates/core/src/grammar/generated/css_l4.rs:25424-25467`
(`parse_keyword_CssL4Parser_dirKeyword`) emits `push_branch_tag(0u32)`
for `ltr` and `push_branch_tag(1u32)` for `rtl`, with no leaf push.
The arglist body at lines 27963-28033 wraps this in a compound for
rule_id 71 (`dirPseudo`).

Rule_id 71 has no entry in `crates/core/src/runtime/css_l4/builder.rs:383-530`
(the `begin_compound` rule_id dispatch); it falls to the catch-all
at line 526-528 → `OpenFrame::Wrap { value: None }`.

When `push_branch_tag(0)` fires inside the dirPseudo Wrap frame,
`push_branch_tag` at `crates/core/src/runtime/css_l4/builder.rs:889-930`
finds no Numeric/ColorFunction/ColorMix on the stack top; falls to
the `_ =>` arm at line 917-928, which interprets the tag as a
`CssGlobalKeyword::from_discriminant(0)` = `Some(RevertLayer)` and
deposits `CssTypedValue::GlobalKeyword(RevertLayer)`.

Same misrouting fires for namedColor: the outer branch tags
(0 = `violet`, 1 = `khaki`, 2..=19 = grouped prefixes) collide with
`CssGlobalKeyword`'s 0..=4 alphabet — `aliceblue`'s tag 2 deposits
as `Initial` (the W1.5 halt's exact observation). For
`white`/`yellow`/etc with tag 19 > 4, the deposit is silently
suppressed (None branch of `if let Some(kw)`), leaving the value as
unit — explaining why `white_materialises` reports `None`.

The dirPseudo arm has the additional gap that no source span is ever
captured — even if `push_branch_tag` were correctly routed, the
selector list never receives a `Selector::PseudoClass(":dir(ltr)")`
text. The Wrap-shape collapse loses the byte-span context entirely.

### B.4  Class B fix mechanism (research recommendation)

Two independent sub-fixes:

1. **B.2 namedColor codegen** at
   `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:227-282`:
   extend `find_map_fn` to descend into `Alt`. Two valid mechanisms:

   a. **Per-inner-arm payload emission** — when the outer branch is
      `Seq(..., Alt([branches]))`, the inner Alt's emission helper
      (`inline::emit_seq_branch_structural_struct_direct`) must call
      `branch_payload_push` per inner arm and emit the matching
      `push_leaf_with_u64` immediately after the inner literal match
      succeeds. Each prefix-factored inner arm carries its own
      Map { fn_id }; the per-arm payload is recoverable.

   b. **Pre-codegen IR un-factoring** — after prefix-tree factoring,
      synthesise an outer Map node carrying the inner Alt's per-arm
      Map projections so `find_map_fn` resolves at the outer level.
      Less surgical; pollutes IR shape.

   Mechanism (a) is the smaller change. Closes 3 tests directly
   (`every_named_color`, `white_materialises`, `aliceblue_fires`).

2. **B.3 dirPseudo / namedColor branch-tag routing** at
   `crates/core/src/runtime/css_l4/builder.rs:889-930` and
   `383-530`. The catch-all at line 917-928 must NOT default-cast
   branch_tag to GlobalKeyword. Two valid mechanisms:

   a. **Per-rule OpenFrame for dirPseudo + namedColor**: extend
      `OpenFrame` with `DirPseudo { kind_tag: Option<u8> }` and
      `NamedColor { tag: Option<u8>, packed: Option<u32> }`; route
      rule_ids 2 (namedColor) and 71 (dirPseudo) at `begin_compound`
      to those frames; deposit through their finalisers.
      `end_compound` for `DirPseudo` deposits a
      `Selector::PseudoClass(":dir(ltr)")` (or rtl) into the
      enclosing SelectorList. `end_compound` for `NamedColor` reads
      the tag and emits the matching packed u32 via a static lookup
      table — but this requires the codegen to ALSO push the tag
      (not the u32) for namedColor; better to combine with B.2's
      payload-fix and let the u32 land via `push_leaf_with_u64`.

   b. **Tighten `push_branch_tag`**: drop the GlobalKeyword
      auto-cast at line 920 entirely. GlobalKeyword's branch-tag
      route should only fire when the topmost frame is the
      `globalKeyword`'s OWN compound; route through a typed frame
      check rather than a discriminant trial-cast. Same for
      MathOperator.

   Mechanism (a) is the architectural fix that closes both
   `dir_pseudo_ltr_branch_fires_payload` and
   `dir_pseudo_rtl_branch_fires_payload`, plus completes B.2 by
   ensuring namedColor's tag-only path doesn't bleed into
   GlobalKeyword. Mechanism (b) is the defensive narrowing that
   should land regardless.

### B.5  Cross-class connection

Classes A and B share NO code paths. Class A defects live in:
- `crates/ir/src/passes/recognizers/operator_chain.rs`
- `crates/core/src/grammar/generated/google_sheets.rs` (Sheets-only)
- `crates/core/src/runtime/google_sheets/document.rs`

Class B defects live in:
- `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`
- `crates/core/src/runtime/css_l4/builder.rs`

The shared theme — typed projections lost between IR and runtime —
is conceptual, not code. The fixes do not cross-contaminate.

---

## §3 Class C — Backend TS Color type (1 failure)

### C.1  Inventory

| Test | Failure |
|---|---|
| `ts_tempdir_typecheck_representative_grammars` | `tsc` rejects emitted `css_l4.ts` with `error TS2304: Cannot find name 'Color'` at line 111 column 30 |

Per `docs/tranches/AZ-IV/audit/W1-nextest-pass.txt:1697` the offending
emission is `| { tag: "colorFn"; value: Color }`. The TS source defines
`Span`, `ParserState`, the union type `stylesheetValue`, and the host
function declarations — but never `type Color = ...`.

### C.2  Root cause

`grammar/css/l4/color.bbnf:228, :255, :303` declare three rules
projecting to `: Color` (`colorFunction`, `colorFn`, `colorMix`).
`Color` is a Named TypeDesc the Rust runtime resolves via host
function `parse_color` to `crate::runtime::css_l4::CssColor`.

The TS backend at
`crates/core/src/backend/ts/projection.rs:11-46`'s `type_desc_to_ts`
maps `TypeDesc::Named(sid)` (line 44) to the raw string the
`StringId` resolves to — i.e., literally `"Color"`. The grammar
emitter at `crates/core/src/backend/ts/emitter/grammar.rs:103-112`
emits the union body verbatim, with `Color` referenced but never
declared.

The host-function declarations at lines 133-142 emit
`declare function parse_color(__input: any): any` (one per called
host fn), but no `type` aliases for the host fns' return types. The
W1-zero commit `bcf68bda` repaired the host-fn name path-strip (so
`parse_color` is correctly emitted instead of
`crate::css_types::parse_color`), but the type-emission gap
predates W1 — the emitter was never asked to produce TS type aliases
for grammar-Named types.

### C.3  Class C fix mechanism (research recommendation)

Extend `crates/core/src/backend/ts/emitter/grammar.rs:88-112`
(`emit_grammar_types`) to walk every `TypeDesc::Named(sid)` referenced
in `ir.types`, deduplicate by name, and emit a `type <Name> = unknown;`
line in the "Runtime types" preamble (or a structurally typed
equivalent if the runtime backend supplies one — e.g.
`type Color = { kind: string; …}` mirroring `CssColor`'s shape). The
W5 binding wave will replace `unknown` with the executable runtime
type; W1's gate (`tsc --noEmit` typecheck pass) is satisfied by the
weakest declaration.

Alternative mechanism: emit the Named type's structural definition by
following the IR's `MapExpr::FnCall` to the host fn's declared return
shape. More invasive; defer to W5 unless there's a specific test
requiring strict typing.

Closes `ts_tempdir_typecheck_representative_grammars`. Independent of
A and B.

---

## §4 Joint vs Independent Scope

The three classes are independent:

- **A** is grammar-derivation (Pratt mining), Sheets-runtime
  (string parser, range_end emitter, ArrayRow serialiser), one
  grammar file (`grammar/google-sheets/google-sheets.bbnf`).
- **B** is Rust-emitter (alt_dispatch payload walker) plus
  CSS-runtime (push_branch_tag routing, OpenFrame dispatch).
- **C** is TS-emitter (type_desc_to_ts plus emit_grammar_types).

No file appears in more than one class's surface list. The redress
lane can split A/B/C across three independent worktrees without
shared-file collisions.

The conceptual theme common to A and B — IR carries the typed
projection but the codegen-runtime seam loses it — is consistent
with `feedback_typed-materialization-invariant`. Both surfaces violate
the invariant; the fixes restore it locally without a shared
mechanism.

---

## §5 Minimal Change Footprint

The redress lane modifies the following files. Generated outputs
(in `crates/core/src/grammar/generated/**`) are mechanical regen
output of the IR/emitter changes; counted but not enumerated.

### Class A (Sheets) — 4 source files + regen

1. `crates/ir/src/passes/recognizers/operator_chain.rs` — narrow
   per-rule scoping at lines 227-266 so list-shaped rungs receive
   only their own LUT entries. (Or §1.5 alternate: tighten predicate
   in `crates/ir/src/passes/recognizers/shape_dispatch/pratt.rs:87-128`.)
2. `crates/core/src/backend/rust/emitter/shapes/string/struct_direct.rs`
   (or its Sheets-routing analog) — restore full `[open..end+1]`
   span capture for `-> input : Span` annotated string rules.
3. `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`
   or the Wrap-shape analog at
   `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs`
   — emit `push_leaf_with_str` for regex-led alt branches inside
   Wrap-shape rules whose declared type is Span.
4. `crates/core/src/runtime/google_sheets/document.rs` — possibly
   adjust the `ArrayRow` serializer at lines 281-288 to skip Tag
   children if the Pratt narrowing in A.2 still leaves operator
   tags in the children list. (May become unnecessary after A.2.)
5. `crates/core/src/grammar/generated/google_sheets.rs` (regen).

Surface count: 3 emitter files + 1 IR file + 1 runtime file +
generated. ~5 named surfaces.

### Class B (CSS L4) — 2 source files + regen

1. `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`
   — extend `find_map_fn` (lines 228-243) and the
   `inline::emit_seq_branch_structural_struct_direct` caller
   (line 165-196) to emit per-inner-arm `push_leaf_with_u64` /
   `push_leaf_with_unit` per the inner Alt's Map { fn_id }.
2. `crates/core/src/runtime/css_l4/builder.rs` — add `OpenFrame::DirPseudo`
   and route rule_id 71 to it (lines 383-530); finalise it to a
   `Selector::PseudoClass(":dir(<kind>)")` in `end_compound`
   (lines 624-628 or new arm); narrow the `push_branch_tag` catch-all
   at lines 917-928 so GlobalKeyword/MathOperator deposit only
   fires from their owning rules' frames.
3. `crates/core/src/grammar/generated/css_l4.rs` (regen — only if
   the emitter change in (1) lands; (2) does not need regen).

Surface count: 1 emitter file + 1 runtime file + generated.
3 named surfaces.

### Class C (Backend TS) — 1 source file

1. `crates/core/src/backend/ts/emitter/grammar.rs` — extend
   `emit_grammar_types` at lines 88-112 to collect every
   `TypeDesc::Named` from `ir.types` and emit `type <Name> = unknown;`
   in the runtime-types preamble.

Surface count: 1 emitter file. 1 named surface.

### Totals

- **Source files**: 7 (3 Class A + 2 Class B + 1 Class C, with one
  emitter file shared between A and B's alt_dispatch change but
  modified in different code regions).
- **Generated files**: 2 (sheets, css_l4) regen.
- **Tests**: 0 new test files required (the existing 13 tests are
  the gate).
- **Brittleness**: every change is local to a single function or
  arm; no architectural pivot.

---

## §6 Plan-Lane Handoff

The plan lane must carve W1-CLOSE into three sequenced sub-units, one
per defect class, dispatched into three sibling worktrees:

- **W1-CLOSE.A (Sheets)**: own
  `crates/ir/src/passes/recognizers/operator_chain.rs` (rung scoping),
  the string and regex-Wrap emitter analogs under
  `crates/core/src/backend/rust/emitter/shapes/`, the Sheets runtime
  serialiser at `crates/core/src/runtime/google_sheets/document.rs`,
  and Sheets regen. Closes 7 tests; HARD CAP 30 min redress.

- **W1-CLOSE.B (CSS L4)**: own
  `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`
  (per-inner-arm payload), `crates/core/src/runtime/css_l4/builder.rs`
  (DirPseudo OpenFrame + push_branch_tag narrowing), and CSS L4
  regen. Closes 5 tests; HARD CAP 30 min redress.

- **W1-CLOSE.C (Backend TS)**: own
  `crates/core/src/backend/ts/emitter/grammar.rs`. Closes 1 test;
  HARD CAP 15 min redress. Trivially scoped — fits under a
  single-pass research-class budget.

The three units share no files, can dispatch in parallel into named
sibling worktrees, and produce three independent commits under the
W1-CLOSE banner. Total worktree-wall budget ≤ 30 min if A/B/C run in
parallel. The W1 hard-gate (zero failing tests) closes the moment
all three commits land and `cargo nextest run --workspace
--cargo-profile ax-iter --no-fail-fast` reports zero failures.

No new tranche, no thesis amendment, no scope-reveal triumvirate
required. The 13 residuals close inside W1's amended file bounds —
specifically, the file bounds at W1.md lines 47-68 already include
every surface enumerated in §5 above (`crates/core/src/runtime/css_l4/**`,
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`,
`crates/core/src/backend/ts/**`, `crates/ir/src/registry/**` —
extending to `crates/ir/src/passes/recognizers/**` is the only file-bound
extension required, and it stays inside crates/ir which W1 already
owns).

---

## Appendix — Evidence Traces

### Sheets array_literal trace

Input `={1,2,3}`. After `parse_array_literal` opens compound at
`*p=1`, calls `parse_pratt_array_rows` at `*p=2`.
`parse_pratt_array_rows` opens its compound, calls
`parse_pratt_array_row`. The inner Pratt loop's LUT at
`crates/core/src/grammar/generated/google_sheets.rs:155-172` shows
both byte 44 (`,`) → `lut=1` and byte 59 (`;`) → `lut=2`, so
`array_row` consumes ALL commas in `1,2,3`, depositing children
`[expr(1), Tag(0), expr(2), Tag(0), expr(3)]`. Round-trip via
`document.rs:281-288` emits `1,Tag,2,Tag,3` where `tag_lexeme(ArrayRow,
0)` falls to the `_ => ""` default at line 489. Result: `1,,2,,3`.
Second parse fails at offset 1 because primary cannot dispatch on
the leading byte after `=` for `,1,,2,,3` (whatever serialiser
produces, the `,,` is structurally invalid).

### namedColor codegen trace

`crates/core/src/grammar/generated/css_l4.rs:4667-12136` shows 20
outer branches in `parse_altdispatch_CssL4Parser_namedColor`. Grep
confirms only 2 `push_leaf_with_u64` emissions (lines ~31, ~46 in
the slice — `violet` and `khaki`). The 18 prefix-grouped branches
emit `push_branch_tag(N)` only; some emit `push_leaf_with_unit` (4
of them, judging from the grep output) but with no u32 payload. So
148 of 150 named colours never deposit their declared u32 — verified
by the test's `failed.push((name, expected, got))` showing every
non-violet-non-khaki name as `got: None`.

### dirPseudo trace

`crates/core/src/grammar/generated/css_l4.rs:25434-25446` —
`parse_keyword_dirKeyword` emits
`push_branch_tag(0u32)` for `ltr` (no leaf push). The arglist body
at 27963-28033 wraps in `dirPseudo` rule_id 71 compound.
`crates/core/src/runtime/css_l4/builder.rs:526-528` falls rule_id 71
to `OpenFrame::Wrap`. `push_branch_tag(0)` at lines 917-928 with
Wrap on top falls to `_ =>`, hits
`CssGlobalKeyword::from_discriminant(0)` = `Some(RevertLayer)`, and
deposits as `GlobalKeyword`. The `:dir(ltr)` text never reaches the
selector list.

### TS Color trace

`docs/tranches/AZ-IV/audit/W1-nextest-pass.txt:1582-1697` shows
`tsc` rejects line 111 column 30 with `Cannot find name 'Color'`,
where line 1697 emits `| { tag: "colorFn"; value: Color }`.
`crates/core/src/backend/ts/projection.rs:44` shows
`TypeDesc::Named(sid) => ir.get_string(*sid).to_string()` returning
the raw `Color` name. `crates/core/src/backend/ts/emitter/grammar.rs:103-112`
emits the union without any preceding `type Color = …` declaration.
