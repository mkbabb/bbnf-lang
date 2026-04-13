# AR Audit — AQ Code State

This audit verifies the AQ tranche claims by reading source, building
production grammars through `compile_paths_request`, expanding the
generated parser code via `cargo expand`, and observing the
instrumented runtime. The verdict for each claim is grounded in code
references and observed behaviour, not in plan documents.

The post-AQ baseline (`docs/benchmarks/post-AQ.json`) describes the
shipped state: PayloadKind deletion, full integer suite, layout
planner wiring, Alt typed enum view codegen, RegexClass deoverfit,
scanner deoverfit, `payload_layouts` field, and per-component
BBNF_CSP_REPORT instrumentation. Each is examined below.

## Direct-to-struct projection

**Verdict: working in code, dormant in production. Zero rules in any
production grammar receive an aggregate payload layout, and the
scalar-payload path falls silent for the JSON `value` Alt after rule
inlining.**

### Layout planner

`crates/ir/src/passes/payload/layout.rs` implements
`compute_payload_layouts(ir)`. The planner walks `ir.types`, accepts
only `TypeDesc::Tuple(fields)` whose every field passes
`is_scalar_payload`, computes naturally-aligned offsets, and rejects
any layout exceeding `MAX_PAYLOAD_BYTES = 16`. The five tests in
`crates/ir/tests/passes/passes_payload.rs` (single F64 packing, U8 +
U32 alignment padding, F64 + U8 packing, non-scalar field rejection,
over-max byte rejection) all pass.

Pipeline integration is correct: `crates/core/src/pipeline/compile.rs`
calls `compute_payload_layouts` after `project_types` for every
target (Vm at line 173, Ts at 179, Wasm at 199), and
`crates/core/src/backend/driver/analysis.rs:141` calls it in the Rust
backend's `analyze_grammar`. The `ir.payload_layouts` field is
declared at `crates/ir/src/types/grammar.rs:217`.

### Production-grammar coverage

Compiling each production grammar through `compile_paths_request`
and counting `ir.payload_layouts.len()` (probe in
`crates/core/examples/probe_payload_layouts.rs`, removed
post-audit) yields:

| Grammar      | Rules | Tuple Types | All-Scalar Tuples | Layouts |
|--------------|-------|-------------|-------------------|---------|
| json         | 10    | 1           | 0                 | 0       |
| css_l4       | 184   | 106         | 0                 | 0       |
| bbnf         | 52    | 24          | 0                 | 0       |
| sheets       | 38    | 20          | 0                 | 0       |
| ebnf         | 14    | 3           | 0                 | 0       |
| css_pretty   | 20    | 8           | 0                 | 0       |

Sample tuple shapes from CSS L4: `colorFn = (Span, BoxedEnum,
BoxedEnum, BoxedEnum, BoxedEnum, Option(BoxedEnum))`, `mathExpr =
(BoxedEnum, Vec(Enum))`, `calcFunction = (Span, BoxedEnum)`. Every
Tuple in every grammar carries at least one `Span`, `BoxedEnum`,
`Option`, or `Vec` field, so none satisfy `is_scalar_payload` for
every field. The aggregate path
(`push_leaf_with_aggregate`) is never emitted: `grep
"push_leaf_with_aggregate"` over `cargo expand` of `bbnf-bootstrap`
and the JSON `json_slab` test returns zero matches.

`crates/bbnf-tape/src/builder.rs::push_leaf_with_aggregate` enforces
the 16-byte cap (`debug_assert!(bytes.len() <= 16, ...)` at line
397) and computes a 1-based `payload_idx` after rounding up to the
next 8-byte slot — slot allocation is correct.

### Scalar payload path on Alt-bodied rules

`crates/core/src/backend/rust/emitter/grammar.rs` reads
`ctx.payload_type` and emits the matching `push_leaf_with_<T>` in
the Alt prelude/epilogue helpers
(`emit_alt_mustape_prelude_epilogue`,
`emit_alt_span_only_prelude_epilogue`). The context is populated in
`crates/core/src/backend/rust/emitter/mod.rs:412-451`: it first
checks `ir.payload_layouts.get(&rule.id)` (always None for
production grammars), then `ir.types[rule.id]` for a direct scalar
match, then walks `IrNode::Alt` branches for `IrNode::Ref(rid)`
where the referenced rule projects as scalar.

The Alt fallback is structurally fragile: `fuse_single_use` (see
the IR pipeline ordering in `crates/core/src/pipeline/compile.rs`
and `crates/ir/CLAUDE.md`) inlines single-use rules including JSON's
`null`, `bool`, `number`, and `string`, so by the time the emitter
walks JSON's `value` rule body, the branches are inlined Map /
Literal / Regex shapes — no longer `IrNode::Ref(rid)`. The Alt
branch loop at `mod.rs:436` is therefore a no-op for JSON, leaving
`ctx.payload_type = None`. The expansion in `/tmp/json_expand.txt`
confirms this: `__value` declares `__has_children` and emits
`scan_number_strict_span`, never `scan_number_strict_f64`, and
never assigns to a `__payload_*` local.

The scalar payload path is therefore wired and provably reachable
(the unit tests prove it for synthetic inputs), but **silently
inactive on the JSON value Alt — the canonical example the design
brief named**. This is a real regression versus the design intent,
not just dormancy.

## Alt typed enum view (AQ.6.C)

**Verdict: implementation present, zero typed enums generated for
any production grammar.**

`crates/core/src/backend/rust/view/alt.rs::emit_alt_accessors` always
emits the `as_<variant>` / `is_<variant>` discriminator surface.
After that surface it calls `emit_typed_enum_value_accessor` to
optionally emit a `<RuleName>Value` enum + `.value()` accessor.

The typed-enum builder (alt.rs:164-291) bails out the moment any
branch fails the `branch_value_shape` predicate — every branch must
be an `IrNode::Ref(rule_id)` whose referenced rule's `TypeDesc`
satisfies either `is_scalar_payload` or has an entry in
`ir.payload_layouts`. JSON's `value` rule branches reference
`object` (Vec) and `array` (Vec), which are non-scalar; the early
return at line 188 fires; no `valueValue` enum is generated.

Across the entire `cargo expand` output for `bbnf-bootstrap`'s
generated parser (~6.7 MB), `grep -c "AQ.6.C\|fn value(&self) ->
::core::option::Option"` returns `1` for the named-child accessor
(an unrelated `value()` returning `valueView`) and `0` for typed
enums. The same grep over the JSON `json_slab` test expansion
matches the same single named-child `value()` and no typed enum.

The codegen is sound for grammars where every Alt branch is itself
a payload-eligible scalar/aggregate rule. No production grammar
satisfies that condition.

## PayloadKind deletion

**Verdict: complete.**

`rg PayloadKind crates/` returns zero matches across the whole
workspace. The replacement field `RustEmitCtx::payload_type:
Option<TypeDesc>` lives at
`crates/core/src/backend/rust/emitter_types.rs:84`, and it is
populated only from `ir.types[rule.id]` (mod.rs:418-450) — never
from a parallel nominal enum. Every consumer reads `TypeDesc`
directly: `grammar.rs:55, 201, 206, 239, 515, 600`,
`leaves.rs:119`, `map_value.rs:73, 144`. Aggregate context lives in
`payload_layout: Option<PayloadLayout>` (emitter_types.rs:94),
populated only from `ir.payload_layouts.get(&rule.id)`
(mod.rs:418).

## RegexClass deoverfit

**Verdict: nominal variants deleted, parameterized variants in
place; the CSS L4 `selectorIdent` pattern still falls through to
`RegexClass::Unknown`.**

`parse-that/rust/regex/src/classify/mod.rs::RegexClass` carries
`Numeric { allows_sign, allows_fraction, allows_exponent,
reject_leading_zero, allow_leading_dot }`, `QuotedString {
quote_char, allows_escapes, allows_u_escapes }`, `Identifier {
allows_leading_dash, allows_double_dash_prefix }`, `HexDigits`,
`WhitespaceWithBlockComment`, `CharClassQuantified(ClassRangeInfo)`,
`PrefixThenClass`, `AccelDriven(u8)`, `Unknown`. Search across
`parse-that/rust/regex/src/` for `JsonString | JsonNumber | CssIdent
| CssQuotedString | WsBlockComment` returns zero matches. The
`canonical_pattern` helper at lines 129-147 covers the `Identifier`
and `QuotedString` variants for the IR-side recognizer-config
consumers.

`try_classify_identifier` at
`parse-that/rust/regex/src/classify/structural.rs:419` handles two
shapes:

1. Top-level alternation with one `-?[a-zA-Z_][\w-]*` branch and
   one `--[\w-]+` branch → `Identifier { allows_leading_dash:
   true, allows_double_dash_prefix: true }`.
2. Single concat with optional leading `-?` followed by an
   `[a-zA-Z_][\w-]*` body.

The CSS L4 `selectorIdent` pattern from
`grammar/css/l4/tokens.bbnf:8` is
`(?:-?[a-zA-Z_]|\\[^\n])(?:[\w-]|\\[^\n])*`, an alternation between
the conventional optional-dash identifier head and an
escape-sequence branch `\\[^\n]`, plus an inner repetition that
also alternates with the escape branch. Hand-traced through
`try_classify_identifier`:

- The outer pattern is `Concat([Group(Alt([head, escape])),
  Repetition(Group(Alt([word, escape])), 0..)])`.
- `unwrap_group(inner)` returns the Concat. The first branch is the
  Group; `is_optional_dash` is false.
- `is_identifier_body(hir)` walks parts, expecting the first part
  to be a letter class (or letter-class repetition). Instead it
  finds an `Alternation`, returns `false`.

A live probe (compiled against `bbnf-regex` and the local
`csp-solver` patch) confirms:

```
selectorIdent classification: Unknown
css ident-with-dash classification: Identifier { allows_leading_dash: true,
    allows_double_dash_prefix: true }
plain ident classification: Identifier { allows_leading_dash: false,
    allows_double_dash_prefix: false }
```

The structural classifier never recognizes any pattern that mixes
identifier shape with `\\[^\n]` escape branches, so AQ.7.1 / AQ.7.2
(CSS L4 `__compoundSelector` → `scan_ident` routing) cannot ship
without classifier extension. The `post-AQ.json deferred_work` block
records exactly this conclusion.

## Scanner deoverfit (AQ.4)

**Verdict: complete.**

`parse-that/rust/parse_that/src/parsers/scan/` exposes nine modules:
`balanced.rs`, `digits.rs`, `ident.rs`, `mod.rs`, `number.rs`,
`number_f64.rs`, `quoted.rs`, `quoted_simd.rs`, `ws_comment.rs`. No
file or public function is JSON / CSS-prefixed. Public surface
(re-exports in `mod.rs`):

- `scan_number_mantissa`, `scan_number_strict_span`,
  `scan_number_strict_fused`, `scan_number_strict_f64`,
  `scan_number_span`, `scan_number_f64`
- `scan_ident(state, &IdentConfig)` with
  `DEFAULT_IDENT_CONFIG`, `CSS_IDENT_CONFIG`
- `scan_string_quoted`, `scan_quoted_string_content`,
  `scan_quoted_string_strict`, `validate_strict_escapes`
- `scan_block_comment`, `scan_ws_block_comments`
- `scan_balanced(bytes, &BalancedScanConfig)`,
  `scan_alnum_mut`, `scan_digits_mut`, `scan_digits_star_mut`,
  `scan_hex_mut`

`rg sp_json_|sp_css_ rust/parse_that/src/` returns zero matches —
the language-prefixed SpanParser constructors are deleted. The
`SpanScanner` enum keeps a `QuotedStringStrictContent` variant that
is currently unused (the compiler emits a `dead_code` warning) but
is structurally named.

Kernel emission in `crates/core/src/backend/kernels/`:

- `quoted_string.rs` — `emit_call` →
  `parse_that::scan_string_quoted`; `emit_call_strict` →
  `parse_that::scan_quoted_string_strict`. Module doc still mentions
  legacy variants `JsonString`/`CssQuotedString` in prose.
- `number.rs` — `emit_call_span` →
  `parse_that::scan_number_strict_span`; `emit_call_fused` →
  `parse_that::scan_number_strict_fused`. Module doc still names
  `JsonNumber`.
- `identifier.rs` — `emit_call` → `parse_that::scan_ident(state,
  &DEFAULT_IDENT_CONFIG)`; `emit_call_css` → ditto with
  `CSS_IDENT_CONFIG`.
- `comment_ws.rs` — `emit_call`.
- `punct_ws_region.rs` — `emit_call(puncts, ws_pattern)` derives the
  punct set per call from the grammar's `AltDispatch` byte
  vocabulary
  (`crates/ir/src/passes/recognizers/punct_ws_region.rs:60` /
  `collect_dispatch_byte_vocabulary`); `STRUCTURAL_PUNCTS` constant
  is gone.
- `balanced_wrap.rs`, `charclass.rs`, `prefix_class.rs` —
  structurally named.

Every kernel emit_call routes to a structurally-named parse-that
function. The remaining drift is in module-doc prose (legacy class
names in docstrings, not in code).

## Instrumentation (AQ.9.3)

**Verdict: per-component instrumentation lands only when a
component's solve actually runs; trivial components silently skip
the report.**

`crates/ir/src/passes/csp_strategy/mod.rs` carries two `eprintln`
sites:

- Line 614: `budget_exceeded` branch, gated on
  `BBNF_CSP_REPORT.is_ok() || cfg!(debug_assertions)`.
- Line 634: per-component success report, gated on
  `BBNF_CSP_REPORT.is_ok()`.

Both live AFTER the `Phase 3: finalize + solve` block (line 596).
The fast-path short-circuit at line 591 — `if constraints_added ==
0 { decode_min_cost_per_variable(...); return; }` — returns BEFORE
either `eprintln`. Components whose only sites are intra-rule (no
cross-rule constraints) therefore skip the report.

Live capture from running `cargo run --example
probe_payload_layouts` with `BBNF_CSP_REPORT=1` (the example
compiles all six production grammars):

```
csp_strategy::solve_component sites=467 constraints=633
    nodes_explored=719 solutions=1 contributing_rules=184
csp_strategy::solve_component sites=94 constraints=296
    nodes_explored=606 solutions=1 contributing_rules=52
csp_strategy::solve_component sites=59 constraints=506
    nodes_explored=1852 solutions=1 contributing_rules=38
csp_strategy::solve_component sites=36 constraints=42
    nodes_explored=66 solutions=1 contributing_rules=20
```

Six grammars compiled; four CSP solve reports surfaced. JSON (10
rules), EBNF (14 rules), and presumably one of the CSS variants
fast-pathed. The post-AQ.json claim "BBNF_CSP_REPORT
instrumentation always emits per component" is inaccurate — it
emits per *non-trivial* component, where "non-trivial" means at
least one cross-variable constraint.

## Span-text fallback (Phase 1.2 regression acceptance)

**Verdict: every directive except `host` retains a span-text
fallback path; the `import_directive` typed view returns None
because the inlined Alt sub-variant collapses sub-variant identity.**

`crates/core/src/grammar/host.rs:212-236` carries the catch-all
fallback dispatcher: when the structural `try_as_*_directive`
helper returns None or when the Alt sub-variant for `directive`
collapses to an empty compound, the code reads the directive's
source span text and dispatches on the leading `@keyword`. The six
fallback functions are:

- `absorb_recover_by_text` (host.rs:244) — `RecoverDirective`'s
  `sync_expr` field is replaced with the directive item itself
  (placeholder, not the actual sync sub-tree).
- `absorb_pretty_by_text` (278) — re-tokenizes hint list from the
  raw substring, including parenthesized arg handling
  (`split_pretty_hint_tokens`, 305).
- `absorb_token_by_text` (346) — splits on whitespace.
- `absorb_debug_by_text` (359) — splits on whitespace.
- `absorb_ws_by_text` (372) — strips `/regex/` markers.
- `absorb_host_by_text` (384) — splits on `:` for the type
  annotation.

Plus `absorb_import_structural` (host.rs:423) for `@import`, which
itself carries an in-function span-text fallback at lines 467-523:
the primary path (descend through `import_path` / `import_items`
rule_kinds) succeeds for the clean structural CST, but the
in-function fallback handles the case where the structural descent
fails because a `import_directive_0` sub-variant has been collapsed
by the codegen's structural-mode dedup.

The structural-extraction failure root cause: BBNF's `directive`
rule resolves to an Alt of sub-variants
(`import_directive_0`...`import_directive_N`) that are themselves
single-use Refs. `fuse_single_use` (the post-Tranche-AB pipeline
pass that inlines single-use rules regardless of body size) inlines
each sub-variant body at its call site. The `import_directive_0`
identity then no longer corresponds to a stamped variant_idx, so
the schema's `try_as_import_directive` accessor's variant_idx check
fails. The host code recovers by walking descendants by `rule_kind`
or by parsing the source span directly.

The post-AQ.json `deferred_work` block records this as "Bootstrap
regen without span-text fallback (Phase 1.2): requires deeper
architectural change to handle sub-variant compounds vs. typed view
accessors." That deferral is honest — fixing it requires either
gating sub-variant rules out of `fuse_single_use` (matching
`@no_collapse`) or stamping per-Alt-arm variant_idx values that
survive structural-mode collapse.

## Top items that claim to be done but are dormant or broken

The ranked list below is by impact on the AQ tranche's stated value
proposition — direct-to-struct projection driving the next round of
parse-throughput wins.

1. **Aggregate payload layout — completely dormant.** The planner,
   builder API, view-layer decoders, and emitter prelude/epilogue
   are wired end-to-end, but every Tuple type in every production
   grammar carries at least one `Span`/`BoxedEnum`/`Option`/`Vec`
   field, so `compute_payload_layouts` returns an empty map
   universally. Zero `push_leaf_with_aggregate` calls land in any
   generated parser. Significant code surface (`bbnf-tape::Builder`
   aggregate methods, layout planner, view aggregate decoders,
   emitter aggregate prelude/epilogue) earns nothing in the
   shipping benchmark grammars. To activate: emit per-Span /
   BoxedEnum field-projection passes that lift Span fields out of
   Tuples into adjacent scalar slots, or change `is_scalar_payload`
   to admit `TypeDesc::Span` (Span is a `(u32, u32)` pair, fits in
   8 bytes, decodable from `payload_bytes`). The latter would
   immediately make CSS L4 `colorFn` and `important` payload-eligible.

2. **Alt typed enum view — generated zero times in production.**
   Same root cause: every production-grammar Alt has at least one
   non-payload-eligible branch (typically a Vec or BoxedEnum
   recursive ref), so the early bailout at
   `view/alt.rs:188` fires and no `<RuleName>Value` enum is
   emitted. The mechanism only fires for grammars where every Alt
   branch references a scalar/aggregate rule — none of the
   shipping grammars satisfy that. To activate: relax the bailout
   to emit a `Value` enum that mixes payload variants with
   cursor-wrapped variants for non-eligible branches (the design
   brief at AQ.6.C.3 anticipates this — "Where branches aren't all
   payload-eligible, mix payload branches with cursor-wrapped
   branches" — but the implementation rejects the mixed case
   instead of handling it).

3. **JSON `value` Alt scalar-payload threading — silently broken.**
   The fallback in
   `crates/core/src/backend/rust/emitter/mod.rs:436` walks Alt
   branches looking for `IrNode::Ref(rid)` where the referenced
   rule is scalar-payload. After `fuse_single_use` (always-on,
   gated only by SCC membership) inlines JSON's `null`, `bool`,
   `number`, and `string` rules, the branches are no longer Refs
   and the loop is a no-op. JSON's `value` rule emits
   `scan_number_strict_span` instead of `scan_number_strict_f64`;
   no `__payload_*` local is declared. The JSON tape never carries
   the parsed F64 — calling `.value()` on a JSON number requires
   re-scanning the span text. This was the canonical example
   AQ.6.A optimized for, and the optimization no-ops for it. To
   fix: either skip `fuse_single_use` for rules with scalar
   payload types, or recursively peel inlined Map / Regex shapes
   in the Alt branch loop.

4. **CSS `selectorIdent` not classified as `Identifier`.** The CSS
   L4 grammar's hot-path identifier pattern
   `(?:-?[a-zA-Z_]|\\[^\n])(?:[\w-]|\\[^\n])*` falls through to
   `RegexClass::Unknown` because `try_classify_identifier` does
   not handle the inner alternation against `\\[^\n]`. The
   structural classifier therefore cannot drive
   `kernels::identifier::emit_call_css`, leaving CSS L4
   `__compoundSelector` (40.2% of normalize, 36.6% of tailwind
   self-time) on the hand-rolled byte-by-byte path. AQ.7 was
   correctly deferred per post-AQ.json. To fix: extend
   `is_identifier_body` to peel an inner Alt whose non-letter
   branch is a CSS escape sequence, OR carry an `allows_escapes:
   bool` flag on `Identifier` and route the kernel to a
   CSS-escape-aware scanner.

5. **BBNF_CSP_REPORT instruments only non-trivial components.**
   Components that fast-path through `decode_min_cost_per_variable`
   (no cross-variable constraints) return at line 593 before the
   eprintln site at line 634. JSON, EBNF, and one other production
   grammar emit zero csp_strategy lines under
   `BBNF_CSP_REPORT=1`. The post-AQ.json claim of universal
   per-component emission is wrong. To fix: hoist the eprintln
   above the constraint-count short-circuit OR add an explicit
   `decode_min_cost_per_variable` report variant.

## Recommended follow-up for tranche AR

The four-layer separation (e-graph / facts / CSP / backend) and
the IR pipeline order are sound. The dormancy / brokenness above
all clusters around the `TypeDesc → payload` projection: every
infrastructure piece exists but the production grammars never
exercise it. AR should close that gap before adding more
infrastructure.

The highest-leverage fix is to admit `TypeDesc::Span` as a
payload-eligible scalar (8 bytes, `(span_lo: u32, span_hi: u32)`).
That single change would make CSS L4's `important = (Span,
BoxedEnum, Span)` payload-eligible at the field level, unlock the
typed enum view for any Alt whose branches are
ident-or-quoted-string, and make the JSON `string` branch of
`value` scalar — eliminating the post-parse span-text fetch.
Pair it with a peel-inlined-shapes fix in the emitter's Alt-branch
fallback so JSON's inlined `number` branch surfaces its F64
payload type. These two changes together would ship the AQ.6
benefit the design intent named without expanding the surface.

The Alt typed enum's "mixed payload + cursor-wrapped" mode is the
design brief's own AQ.6.C.3 case and should land alongside, so
that any Alt of typed branches generates a `Value` enum
discriminating on `variant_idx`. The CSS L4 `selectorIdent`
classifier extension (escape-aware identifier body) is independent
and worth shipping in the same window since it unlocks the
`__compoundSelector` kernel route. Finally, the
`BBNF_CSP_REPORT` instrumentation should hoist above the
short-circuit so the report is genuinely per-component.

The span-text fallback in `host.rs` is symptomatic of the
sub-variant-vs-fuse_single_use coupling identified in post-AQ.json
deferred work. AR can either gate single-use sub-variant rules out
of fusion (matching `@no_collapse`'s reachability semantics) or
plumb per-Alt-arm variant_idx through structural-mode dedup so the
typed `try_as_*_directive` accessors stay correct after collapse.
The latter is the more invasive fix and removes ~400 LOC of
fallback code; the former is a one-line guard in
`fuse_single_use`.
