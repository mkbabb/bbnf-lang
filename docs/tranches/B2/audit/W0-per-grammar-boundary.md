# B2.W0.b — Per-grammar boundary spec

This document specifies which symbols emitted at codegen time live
**per-grammar** (and therefore move to
`crates/core/src/grammar/generated/<ident>.rs`) versus which live as
**shared infrastructure** (and stay in regular `crates/core/src/` /
`crates/tape/src/` source). It is the input W0.c's
`xtask::regen::regen_grammar` reads when it inventories what to emit
into `generated/<ident>.rs`.

The reference snapshot lives alongside this file at
[`W0-bbnf-surface-snapshot.rs`](W0-bbnf-surface-snapshot.rs) — a verbatim
copy of `crates/core/src/grammar/generated.rs` (33 293 lines). Per the
audit below, the *entire* checked-in `generated.rs` is the BBNF
self-host emission; no other grammar's output is interleaved. The
snapshot is therefore byte-identical to the source file modulo path
(`diff -q` confirms zero divergence).

## §1 — Shared infrastructure (stays in `crates/core/src/` and sibling crates)

What lives **outside** `generated.rs` today and continues to live
outside `generated/<ident>.rs` post-W0. None of these are
parameterised on the grammar's `<Grammar>` ident; the per-grammar emit
references them via `::bbnf::runtime::*` or `::bbnf::runtime::tape::*`
imports.

### Trait surface (`crates/core/src/runtime/parsed.rs`)

- `pub trait Root` — `parsed.rs:78`. Generic root marker; per-grammar
  `impl Root for <Grammar>` instantiates the associated `View<'p>`
  type. The trait body is grammar-agnostic.
- `pub trait ValueRoot: Root + Sized` — `parsed.rs:323`. Carries the
  associated `Value<'p>` enum surface.
- `pub trait PathQuery<T>: Root` — `parsed.rs:367`. Generic over the
  query target type (`&'static str`, `f64`, `bool`); per-grammar impls
  satisfy the contract per type.

There is no `pub trait Parser` and no `pub trait ParserNodeView`. The
emitted code uses inherent methods (`<Grammar>::parse`,
`<Grammar>NodeView::*`) rather than dispatching through a trait — see
the §3 discussion of why "Parser" is a documentation marker, not a
real Rust trait.

### Tape substrate (`crates/tape/src/`)

The entire tape crate is the runtime substrate every grammar's emitted
code consumes. None of its symbols carry a grammar ident.

- `pub struct Tape` — `tape/tape.rs:325`.
- `pub struct TapeOffset(pub u32)` — `tape/tape.rs:31`.
- `pub struct TapeRec` — `tape/tape.rs:83`.
- `pub struct TapeIter<'t>` — `tape/tape.rs:759`.
- `pub struct TapeCursor<'tape>` — `tape/cursor.rs:77`.
- `pub struct FusedBuilder` — `tape/builder/mod.rs:205`. The fused
  tape + value builder.
- `pub type TapeBuilder = FusedBuilder` — `tape/builder/mod.rs:1219`.
  Compose-boundary alias; emitted code spells `TapeBuilder::with_capacity`.
- `pub fn FusedBuilder::begin_compound`, `end_compound`,
  `end_compound_post_order`, `finish`, `finish_fused`, `finish_tape_only`
  — `tape/builder/mod.rs:452`, `:494`, `:524`, `:991`, `:1027`, `:1041`.

### Grammar profile + scan policy types (`crates/tape/src/profile.rs`)

The *types* are shared; per-grammar emission only writes literal
instances of these into the per-grammar file.

- `pub struct GrammarProfile` — `tape/profile.rs:56`.
- `pub enum ScanAlphabetClass` — `tape/profile.rs:213`.
- `pub struct ScanActivationFlags(pub u8)` — `tape/profile.rs:241`.
- `pub struct ScanPolicyEntry` — `tape/profile.rs:314`.
- `pub struct StructuralIndex` — `tape/stage1.rs:51`.

### DTA dispatch types (`crates/tape/src/dta.rs`, `tape/driver.rs`)

- `pub enum DtaError` — `tape/driver.rs:42`. Returned from generated
  parse fns; lifted to `ParseErr` in the `<Grammar>::parse` body.
- `pub struct DtaPrecedenceEntry` — `tape/dta.rs:64`. Pratt operator
  table entry; per-grammar emission writes a literal slice of these.

### Parser-state surface (`parse-that` crate)

Used by emitted prettify functions in `impl <Grammar>` blocks.

- `pub struct ParserState<'a>` —
  `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/state.rs:202`.
- `pub struct Span<'a>` —
  `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/state.rs:173`.

### Runtime API + value-builder shim (`crates/core/src/runtime/`)

- `pub struct Parsed<'p, R>` — `runtime/parsed.rs:111`. The fused
  output handle every `<Grammar>::parse` returns.
- `pub enum ParseErr` — `runtime/error.rs:17`. Parse error type.
- `pub use tape::{FusedOutput, FusedBuilder, …}` — `runtime/mod.rs`
  pub-uses tape symbols at canonical paths emitted code references.
- `pub type ValueBuilderOutput<R> = FusedOutput<R>` —
  `runtime/mod.rs:88`. Compose-boundary alias.
- `pub mod value_builder { pub struct _ValueBuilderShim<R>; … }` —
  `runtime/mod.rs:97-141`. The fused-builder back-compat shim.
  Generic over `<R>`; not grammar-parameterised.
- `pub use handle::{CompoundHandle, StringHandle}` — `runtime/mod.rs`.

### Header + workspace plumbing (no grammar ident)

The first 19 lines of `generated.rs` (header + outer-attribute
`#![allow(...)]` + `use` lines) are *fixed* per-grammar but
content-identical across grammars. W0.c's emitter writes these as part
of the `regen_grammar` template — they do not vary per grammar's IR
shape — but they are emitted *once per file* (the header opens each
`generated/<ident>.rs`). The `Regenerate:` comment line should change
from `scripts/bootstrap-bbnf.sh` to `cargo xtask regen --grammar
<ident>` post-W3.

| Lines | Content |
|---|---|
| 1-2 | `//! AUTO-GENERATED ...` doc comments. |
| 4-14 | `#![allow(...)]` outer attribute. |
| 16-18 | `use ::bbnf::runtime::tape::*;` etc. |

### Boundary rule

Anything emitted **same shape** for every grammar — types, traits,
runtime helpers — stays in regular source. Anything emitted
**parameterised on `<Grammar>`** (named with the grammar ident,
referencing per-grammar consts, or whose body computes on this
grammar's IR alone) moves to `generated/<ident>.rs`.

## §2 — Per-grammar items (move to `generated/<ident>.rs`)

Each item below carries its line range in `generated.rs`. The total
emission for the BBNF self-host runs from line 20 (the `pub struct
BbnfBootstrap;` declaration) through line 33 292 (the closing `pub use
__bbnfbootstrap_emit_impl::*;`) — i.e., 33 273 lines of grammar-specific
output. Inside the emit module (`mod __bbnfbootstrap_emit_impl`, lines
22-33 291), every item enumerated below is grammar-specific.

The proc-macro emits these items *in the order shown*. W0.c's
`regen_grammar` should preserve emission order so byte-equivalent
diffing in §4 is meaningful (a re-ordered emit fails the gate even if
semantically identical).

### 2.1 Marker struct + emit-impl module

| Item | Lines | Notes |
|---|---|---|
| `pub struct BbnfBootstrap;` | 20 | The marker. |
| `mod __bbnfbootstrap_emit_impl { use super::*; … }` | 22-33 291 | Wraps every per-grammar item in a private module; the trailing `pub use __bbnfbootstrap_emit_impl::*;` (line 33 292) re-exports them at the parent path. The module name itself encodes the grammar (`__bbnfbootstrap_emit_impl`). |
| `pub use __bbnfbootstrap_emit_impl::*;` | 33 292 | Re-export. |

### 2.2 Grammar source + profile

| Item | Lines | Notes |
|---|---|---|
| `pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize]` | 24-26 | The full grammar source as a string literal (single-element array; multi-import grammars would carry > 1 entry). |
| `static __GRAMMAR_PROFILE_ALPHABET: [u8; 28usize]` | 27-30 | Mined byte set used for `GRAMMAR_PROFILE`. |
| `static __GRAMMAR_PROFILE_DIGRAPHS: [(u8, u8); 17usize]` | 31-53 | Mined digraph set. |
| `pub const GRAMMAR_PROFILE: GrammarProfile` | 54-67 | Constructed from the two static arrays above. |

### 2.3 PHF keyword dispatch tables

One triple per `phf-dispatch` mining site (`__PHF_<Grammar>_<id>_KW`,
`__PHF_<Grammar>_<id>_IDX`, `fn __phf_<Grammar>_dispatch_<id>`).

| Site id | KW slice | IDX slice | Dispatch fn | Lines |
|---|---|---|---|---|
| 8 | 68-71 | 72-81 | 82-93 | 68-93 |
| 9 | 95-98 | 99-108 | 109-120 | 95-120 |
| 11 | 122-132 | 133-142 | 143-154 | 122-154 |
| 21 | 156-170 | 171-180 | 181-192 | 156-192 |
| 29 | 194-205 | 206-215 | 216-227 | 194-227 |
| 30 | 229-232 | 233-242 | 243-254 | 229-254 |
| 33 | 256-259 | 260-269 | 270-281 | 256-281 |
| 50 | 283-294 | 295-304 | 305-317 | 283-317 |

### 2.4 Pratt precedence tables

Each Pratt-shaped rule emits `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>`.

| Rule | LUT | Entries | Lines |
|---|---|---|---|
| `value_mul` | 319-342 | 343-389 | 319-389 |
| `value_add` | 391-414 | 415-461 | 391-461 |
| `value_path` | 463-486 | 487-501 | 463-501 |
| `value_cmp` | 503-527 | 528-582 | 503-582 |
| `value_and` | 584-607 | 608-622 | 584-622 |
| `value_or` | 624-647 | 648-662 | 624-662 |
| `binary_factor` | 664-688 | 689-720 | 664-720 |
| (root composite) | 722-744 | 745-925 | 722-925 |
| `pub const PRECEDENCE_OPERATOR_COUNT: usize = 22usize;` | 926 | — | 926 |

### 2.5 Regex tables + scanner

| Item | Lines |
|---|---|
| `static __DTA_REGEX_<n>: &str = "..."` (10 mined regexes) | 928-949 |
| `pub(crate) const __REGEX_LAST_BYTE_SET_BbnfBootstrap: [(u64, u64); 11]` | 958-972 |
| `fn __regex_scan_BbnfBootstrap(...)` (the bespoke NFA scanner) | 973-2044 |

### 2.6 `__shape_support_<Grammar>` module

| Item | Lines |
|---|---|
| `pub(crate) mod __shape_support_BbnfBootstrap { … }` (preceded by leading doc comments at 2045-2052; the `pub(crate) mod` keyword is line 2054; ScanState struct, `skip_space`, `expect_keyword`, etc.) | 2045-2313 |

### 2.7 Per-rule shape parser fns

Every rule in the grammar emits one `parse_<shape>_<Grammar>_<rule>`
fn. Shape ∈ {`hregex`, `keyword`, `flat`, `pratt`, `arglist`,
`altdispatch`, `wrap`, `scalar`, `array`}.

| Fn | Lines |
|---|---|
| `parse_hregex_BbnfBootstrap_int_lit` | 2322-2364 |
| `parse_hregex_BbnfBootstrap_float_lit` | 2366-2421 |
| `parse_keyword_BbnfBootstrap_bool_lit` | 2423-2509 |
| `parse_flat_BbnfBootstrap_string_lit` | 2511-2615 |
| `parse_hregex_BbnfBootstrap_value_ident` | 2617-2698 |
| `parse_pratt_BbnfBootstrap_value_path` | 2700-2911 |
| `parse_flat_BbnfBootstrap_value_input` | 2913-3066 |
| `parse_arglist_BbnfBootstrap_value_fn_call` | 3068-3335 |
| `parse_altdispatch_BbnfBootstrap_value_atom` | 3337-3973 |
| `parse_keyword_BbnfBootstrap_mul_op` | 3975-4072 |
| `parse_keyword_BbnfBootstrap_add_op` | 4074-4149 |
| `parse_keyword_BbnfBootstrap_cmp_op` | 4151-4306 |
| `parse_altdispatch_BbnfBootstrap_value_unary` | 4308-4639 |
| `parse_pratt_BbnfBootstrap_value_mul` | 4641-4882 |
| `parse_pratt_BbnfBootstrap_value_add` | 4884-5125 |
| `parse_pratt_BbnfBootstrap_value_cmp` | 5127-5368 |
| `parse_pratt_BbnfBootstrap_value_and` | 5370-5611 |
| `parse_pratt_BbnfBootstrap_value_or` | 5613-5824 |
| `parse_flat_BbnfBootstrap_value_closure` | 5826-6014 |
| `parse_wrap_BbnfBootstrap_value_expr` | 6016-6093 |
| `parse_flat_BbnfBootstrap_type_annotation` | 6095-6169 |
| `parse_altdispatch_BbnfBootstrap_type_name` | 6171-6414 |
| `parse_hregex_BbnfBootstrap_identifier` | 6416-6460 |
| `parse_keyword_BbnfBootstrap_literal` | 6462-6752 |
| `parse_flat_BbnfBootstrap_regex` | 6754-6869 |
| `parse_flat_BbnfBootstrap_big_comment` | 6871-6989 |
| `parse_flat_BbnfBootstrap_comment` | 6991-7074 |
| `parse_scalar_BbnfBootstrap_lhs` | 7076-7107 |
| `parse_flat_BbnfBootstrap_call_arg` | 7109-7299 |
| `parse_altdispatch_BbnfBootstrap_term` | 7301-7915 |
| `parse_keyword_BbnfBootstrap_modifier` | 7917-8037 |
| `parse_flat_BbnfBootstrap_factor` | 8039-8274 |
| `parse_flat_BbnfBootstrap_mapped_factor` | 8276-8479 |
| `parse_keyword_BbnfBootstrap_binary_operators` | 8481-8615 |
| `parse_pratt_BbnfBootstrap_binary_factor` | 8617-8828 |
| `parse_flat_BbnfBootstrap_concatenation` | 8830-9021 |
| `parse_flat_BbnfBootstrap_alternation` | 9023-9214 |
| `parse_flat_BbnfBootstrap_closure` | 9216-9406 |
| `parse_wrap_BbnfBootstrap_rhs` | 9408-9480 |
| `parse_flat_BbnfBootstrap_rule` | 9482-9633 |
| `parse_flat_BbnfBootstrap_import_path` | 9635-9750 |
| `parse_flat_BbnfBootstrap_import_items` | 9752-9947 |
| `parse_flat_BbnfBootstrap_import_directive` | 9949-10288 |
| `parse_flat_BbnfBootstrap_recover_directive` | 10290-10500 |
| `parse_flat_BbnfBootstrap_pretty_hint` | 10502-10666 |
| `parse_flat_BbnfBootstrap_pretty_directive` | 10668-11014 |
| `parse_flat_BbnfBootstrap_ws_directive` | 11016-11215 |
| `parse_flat_BbnfBootstrap_token_directive` | 11217-11418 |
| `parse_flat_BbnfBootstrap_debug_directive` | 11420-11690 |
| `parse_flat_BbnfBootstrap_host_directive` | 11692-11964 |
| `parse_keyword_BbnfBootstrap_directive` | 11966-12187 |
| `parse_wrap_BbnfBootstrap_grammar_item` | 12189-12301 |
| `parse_array_BbnfBootstrap_grammar` | 12303-12378 |

53 per-rule shape fns total.

### 2.8 Structural scan policy

| Item | Lines |
|---|---|
| `pub const STRUCTURAL_SCAN_POLICY: &[ScanPolicyEntry]` | 12382-12660 |

### 2.9 Root parse entry + value entry

| Item | Lines |
|---|---|
| `pub fn parse_BbnfBootstrap_grammar(...)` | 12663-12678 |
| `pub fn parse_BbnfBootstrap_grammar__value(...)` | 12680-12692 |

### 2.10 Per-rule view structs

One `<rule>View<'p>` struct per rule. Each carries inherent
`children()`, `child_<n>()`, span accessors, etc. The struct names
themselves are *not* prefixed with the grammar ident, but they live
inside the per-grammar emit-impl module — when multiple grammars
share a rule name (e.g., both `json` and `css` could declare a
`literal` rule), each grammar's `literalView` lives inside its own
`__<grammar>_emit_impl` module and is namespace-isolated.

| View | Lines |
|---|---|
| `pub struct int_litView<'p>` | 12694-12876 |
| `pub struct float_litView<'p>` | 12877-13059 |
| `pub struct bool_litView<'p>` | 13060-13244 |
| `pub struct string_litView<'p>` | 13245-13429 |
| `pub struct value_identView<'p>` | 13430-13614 |
| `pub struct value_pathView<'p>` | 13615-13797 |
| `pub struct value_inputView<'p>` | 13798-13973 |
| `pub struct value_fn_callView<'p>` | 13974-14166 |
| `pub struct value_atomView<'p>` | 14167-14566 |
| `pub enum value_atomValue<'p>` | 14567-14683 |
| `pub struct mul_opView<'p>` | 14684-14868 |
| `pub struct add_opView<'p>` | 14869-15053 |
| `pub struct cmp_opView<'p>` | 15054-15238 |
| `pub struct value_unaryView<'p>` | 15239-15450 |
| `pub struct value_mulView<'p>` | 15451-15633 |
| `pub struct value_addView<'p>` | 15634-15812 |
| `pub struct value_cmpView<'p>` | 15813-15991 |
| `pub struct value_andView<'p>` | 15992-16170 |
| `pub struct value_orView<'p>` | 16171-16349 |
| `pub struct value_closureView<'p>` | 16350-16556 |
| `pub struct value_exprView<'p>` | 16557-16751 |
| `pub struct type_annotationView<'p>` | 16752-16923 |
| `pub struct type_nameView<'p>` | 16924-17108 |
| `pub struct identifierView<'p>` | 17109-17293 |
| `pub struct literalView<'p>` | 17294-17478 |
| `pub struct regexView<'p>` | 17479-17663 |
| `pub struct big_commentView<'p>` | 17664-17848 |
| `pub struct commentView<'p>` | 17849-18033 |
| `pub struct lhsView<'p>` | 18034-18214 |
| `pub struct call_argView<'p>` | 18215-18401 |
| `pub struct termView<'p>` | 18402-18853 |
| `pub enum termValue<'p>` | 18854-18967 |
| `pub struct modifierView<'p>` | 18968-19152 |
| `pub struct factorView<'p>` | 19153-19335 |
| `pub struct mapped_factorView<'p>` | 19336-19514 |
| `pub struct binary_operatorsView<'p>` | 19515-19699 |
| `pub struct binary_factorView<'p>` | 19700-19882 |
| `pub struct concatenationView<'p>` | 19883-20069 |
| `pub struct alternationView<'p>` | 20070-20256 |
| `pub struct closureView<'p>` | 20257-20452 |
| `pub struct rhsView<'p>` | 20453-20645 |
| `pub struct ruleView<'p>` | 20646-20824 |
| `pub struct import_pathView<'p>` | 20825-21009 |
| `pub struct import_itemsView<'p>` | 21010-21178 |
| `pub struct import_directiveView<'p>` | 21179-21347 |
| `pub struct recover_directiveView<'p>` | 21348-21516 |
| `pub struct pretty_hintView<'p>` | 21517-21695 |
| `pub struct pretty_directiveView<'p>` | 21696-21871 |
| `pub struct ws_directiveView<'p>` | 21872-22040 |
| `pub struct token_directiveView<'p>` | 22041-22209 |
| `pub struct debug_directiveView<'p>` | 22210-22378 |
| `pub struct host_directiveView<'p>` | 22379-22554 |
| `pub struct directiveView<'p>` | 22555-22928 |
| `pub enum directiveValue<'p>` | 22929-23031 |
| `pub struct grammar_itemView<'p>` | 23032-23286 |
| `pub enum grammar_itemValue<'p>` | 23287-23355 |
| `pub struct grammarView<'p>` | 23356-23535 |

53 view structs + 3 alt-coercion `<rule>Value` enums.

### 2.11 Generic node view + rule-kind discriminator

| Item | Lines |
|---|---|
| `pub struct BbnfBootstrapNodeView<'p>` (with inherent impl) | 23536-23764 |
| `pub enum BbnfBootstrapRuleKind { … Unknown }` | 23548-23615 (declaration interleaved inside the NodeView impl block) |

### 2.12 Root impls + projection structs

| Item | Lines |
|---|---|
| `impl ::bbnf::runtime::Root for BbnfBootstrap` | 23765-23775 |
| `impl BbnfBootstrap` (`root_rule_name`) | 23776-23782 |
| `pub struct BbnfBootstrap<Rule>Projection` + `impl` (15 of these) | 23797-24390 |
| `pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); 15usize]` | 24392-24412 |
| `pub const PROJECTION_NAMED_BINDINGS: &[&str; 15usize]` | 24413-24435 |
| `pub const PROJECTION_MATERIALIZERS: &[&str; 15usize]` | 24436-24457 |
| `pub const PROJECTION_CONSUMERS: &[&str; 15usize]` | 24458-24482 |
| `pub fn __grammar_projection_<rule>()` (15 of these) | 24483-24643 |

The 15 `BbnfBootstrap<Rule>Projection` structs are: BoolLit, StringLit,
ValueIdent, MulOp, AddOp, CmpOp, TypeName, Identifier, Literal, Regex,
BigComment, Comment, Modifier, BinaryOperators, ImportPath. Each
struct + its `impl` block spans ~40 lines (`23797-23837` for BoolLit's
struct + impl; the others follow at 40-line strides through 24390).

### 2.13 Value enum + ValueRoot impl

| Item | Lines |
|---|---|
| `pub enum BbnfBootstrapValue<'p>` (53 variants + Unknown) | 24649-24707 |
| `impl ::bbnf::runtime::ValueRoot for BbnfBootstrap` | 24708-24734 |

### 2.14 Path-walk fn + PathQuery impls

| Item | Lines |
|---|---|
| `fn __path_walk<'p>(...)` | 24735-24778 |
| `impl PathQuery<&'static str> for BbnfBootstrap` | 24779-24792 |
| `impl PathQuery<f64> for BbnfBootstrap` | 24793-24811 |
| `impl PathQuery<bool> for BbnfBootstrap` | 24812-24834 |

### 2.15 Generic materialiser fns

| Fn | Lines |
|---|---|
| `materialize_object_BbnfBootstrap` | 24839-24853 |
| `materialize_array_BbnfBootstrap` | 24854-24867 |
| `materialize_string_BbnfBootstrap` | 24868-24878 |
| `materialize_number_BbnfBootstrap` | 24879-24888 |
| `materialize_literal_BbnfBootstrap` | 24889-24903 |
| `materialize_value_BbnfBootstrap` | 24904-25132 |
| `project_rule_kind_BbnfBootstrap` | 25133-25197 |
| `project_frame_BbnfBootstrap` | 25198-25655 |
| `project_value_BbnfBootstrap` | 25656-25682 |

### 2.16 Per-rule projection materialiser fns

One `materialize_projection_<rule>_<Grammar>` per rule listed in
`PROJECTION_MATERIALIZERS`. 15 fns total.

| Fn | Lines |
|---|---|
| `materialize_projection_bool_lit_BbnfBootstrap` | 25683-25713 |
| `materialize_projection_string_lit_BbnfBootstrap` | 25714-25744 |
| `materialize_projection_value_ident_BbnfBootstrap` | 25745-25775 |
| `materialize_projection_mul_op_BbnfBootstrap` | 25776-25806 |
| `materialize_projection_add_op_BbnfBootstrap` | 25807-25837 |
| `materialize_projection_cmp_op_BbnfBootstrap` | 25838-25868 |
| `materialize_projection_type_name_BbnfBootstrap` | 25869-25899 |
| `materialize_projection_identifier_BbnfBootstrap` | 25900-25930 |
| `materialize_projection_literal_BbnfBootstrap` | 25931-25961 |
| `materialize_projection_regex_BbnfBootstrap` | 25962-25992 |
| `materialize_projection_big_comment_BbnfBootstrap` | 25993-26023 |
| `materialize_projection_comment_BbnfBootstrap` | 26024-26054 |
| `materialize_projection_modifier_BbnfBootstrap` | 26055-26085 |
| `materialize_projection_binary_operators_BbnfBootstrap` | 26086-26116 |
| `materialize_projection_import_path_BbnfBootstrap` | 26117-26131 |

### 2.17 Prettify impl block

| Item | Lines |
|---|---|
| `impl BbnfBootstrap` (`__<rule>_prettify` fns, plus the `parse(input)` entry near the end) | 26132-32982 |

This is the largest single span (~6 850 lines) in the file. It
contains:
- Per-rule `__<rule>_prettify` fns — one per rule that carries an
  `@pretty` directive. The grammar declares `@pretty grammar block;
  @pretty rule group; @pretty alternation group;` so all 53 rules emit
  prettify fns (the proc-macro defaults non-decorated rules to a
  pass-through prettify).
- `pub fn parse(input: &str) -> Result<Parsed<'_, Self>, ParseErr>` —
  lines 32921-32981. The user-facing entry point.

### 2.18 CST helpers + cst_directives module

| Item | Lines |
|---|---|
| `impl<'p> identifierView<'p>` (`identifier_text`) | 32983-32991 |
| `pub(crate) fn cst_identifier_text<'p>(…)` | 32992-33007 |
| `pub(crate) fn cst_identifier_span<'p>(…)` | 33008-33017 |
| `fn cst_find_identifier_cursor<'p>(…)` | 33018-33044 |
| `pub mod cst_directives { … }` (typed directive structs: ImportDirective, RecoverDirective, PrettyDirective, WsDirective, TokenDirective, DebugDirective, HostDirective, plus extraction helpers) | 33045-33290 |

### Item count summary

- 1 marker struct + 1 emit-impl module wrapper
- 1 grammar source const + 1 grammar profile const
- 8 PHF dispatch sites × 3 items each = 24 items
- 8 Pratt operator sites × 2 items each + 1 root composite = 17 items
- 10 mined regex strings + 1 last-byte-set table + 1 scanner fn = 12 items
- 1 `__shape_support_BbnfBootstrap` module
- 53 per-rule shape parser fns
- 1 `STRUCTURAL_SCAN_POLICY` const
- 2 root parse entry fns (`parse_BbnfBootstrap_grammar`, `parse_BbnfBootstrap_grammar__value`)
- 53 per-rule `<rule>View` structs + 3 alt-coercion `<rule>Value` enums = 56 items
- 1 `BbnfBootstrapNodeView` + 1 `BbnfBootstrapRuleKind` enum = 2 items
- 1 `Root` impl + 1 inherent `impl BbnfBootstrap` (root_rule_name)
- 15 projection structs + 15 projection impls = 30 items
- 4 projection const arrays (`PROJECTION_DIRECT_TO_STRUCT`, `_NAMED_BINDINGS`, `_MATERIALIZERS`, `_CONSUMERS`)
- 15 `__grammar_projection_<rule>()` fns
- 1 `BbnfBootstrapValue` enum + 1 `ValueRoot` impl
- 1 `__path_walk` + 3 `PathQuery` impls = 4 items
- 9 generic materialiser / projection fns
- 15 per-rule `materialize_projection_<rule>_<Grammar>` fns
- 1 large `impl BbnfBootstrap` block (prettify + `parse` entry)
- 4 CST helper items (`identifier_text` impl, `cst_identifier_text`,
  `cst_identifier_span`, `cst_find_identifier_cursor`)
- 1 `pub mod cst_directives` block
- 1 trailing `pub use __bbnfbootstrap_emit_impl::*;`

**Total per-grammar items: 261** (sum of the bullets above). Of these,
~10 are *containers* (the emit-impl module, the prettify impl block,
the cst_directives module, the shape-support module, etc.) carrying
the rest as bodies.

W0.c's emitter therefore ships roughly the same ~33 000-line surface
the proc-macro emits today, modulo prettyplease formatting choices.
The byte-equivalent gate compares the two outputs after `rustfmt`
normalisation.

## §3 — Edge cases / discontinuities

### 3.1 The whole file is one grammar

`crates/core/src/grammar/generated.rs` does **not** interleave
multiple grammars. Every line outside the header (1-19) and the
trailing `pub use` (33 292) is BBNF-specific. The reason: the file is
the output of `scripts/bootstrap-bbnf.sh`, which extracts the *one*
`#[derive(Parser)] pub struct BbnfBootstrap;` site from
`crates/bootstrap/src/lib.rs` via `cargo expand -p bbnf-bootstrap` and
checks in the result. Other grammars (`gorgeous/src/{json, css, ebnf,
bnf, bbnf}.rs`, the ~50 `crates/core/tests/*.rs` derive sites, etc.)
expand at consumer-compile-time only and are *never* checked in.

Consequence for W0.c:
- The post-W0 `generated/bbnf.rs` byte-equivalent gate compares
  against `generated.rs` *as a whole file*, not against a slice of it.
- The reference snapshot at
  `docs/tranches/B2/audit/W0-bbnf-surface-snapshot.rs` is therefore
  byte-identical to `crates/core/src/grammar/generated.rs` (`diff -q`
  zero divergence). When W1+ extends emission to `gorgeous/json.rs` etc.,
  the corresponding `generated/json.rs` file's reference will come
  from a *fresh* `cargo expand -p gorgeous` run (or, more
  ergonomically, from W0.c's own `regen_grammar` output verified
  against a `cargo expand` capture); there is no checked-in reference
  for those grammars today.

### 3.2 No `Parser` trait — only `Root`

The plan text in W0.md and the dispatch prompt describe
"`impl Parser for BbnfBootstrap`" and "`impl<'p> ParserNodeView<...>
for BbnfBootstrapNodeView<'p>`". These traits **do not exist** in the
codebase. `rg -nF 'pub trait Parser' crates/` and
`rg -nF 'pub trait ParserNodeView' crates/` both return zero hits.

What actually exists:

- `pub trait Root` at `crates/core/src/runtime/parsed.rs:78` — the
  root-of-grammar marker; per-grammar `impl Root for <Grammar>`.
- `pub trait ValueRoot: Root + Sized` at `parsed.rs:323` — adds
  `Value<'p>`.
- `pub trait PathQuery<T>: Root` at `parsed.rs:367` — generic over
  query target.
- `<Grammar>NodeView::*` — inherent methods on the grammar's emitted
  node-view struct, not a trait.

W0.c's emitter therefore:
- Emits `impl Root for <Grammar>` (always).
- Emits `impl ValueRoot for <Grammar>` (always).
- Emits `impl PathQuery<&'static str/f64/bool> for <Grammar>` (always — three impls).
- Emits inherent `impl <Grammar>NodeView<'p>` blocks (no trait).
- Emits inherent `impl <Grammar>` blocks (root_rule_name + the parse
  entry + prettify fns).

The `Parser` / `ParserNodeView` names in the dispatch prompt should be
read as documentation pointers to "the root-grammar trait surface and
the per-grammar node-view inherent impl". W0.c follows the actual
shape above.

### 3.3 `BbnfBootstrap` is the *consumer-side* marker, but the file is *core-side*

The marker struct `pub struct BbnfBootstrap;` (line 20) is *also*
declared at `crates/bootstrap/src/lib.rs:16` via `#[derive(Parser)]`.
The two declarations live in different crates (`bbnf` vs
`bbnf-bootstrap`). The proc-macro writes its expansion to
`bbnf-bootstrap`, the bootstrap script captures the expansion + relocates
it to `crates/core/src/grammar/generated.rs` under the `bbnf` crate.

Consequence: today, `bbnf::grammar::generated::BbnfBootstrap` and
`bbnf_bootstrap::BbnfBootstrap` are *two distinct types* with the same
name. The pipeline at `crates/core/src/grammar/mod.rs:50` calls
`generated::BbnfBootstrap::parse(input)` — the *bbnf-internal* one.
The `bbnf-bootstrap` crate's marker is unused except as the proc-macro
expansion target.

W0.c's planned cutover:
- The post-W0.c `crates/bootstrap/src/lib.rs` becomes
  `pub use ::bbnf::grammar::generated::BbnfBootstrap;` (or just
  retires entirely; W0.md's bound list shows `bbnf-bootstrap` as
  modifiable). This unifies the two markers into one.
- W0.c's emitter must therefore *also* be aware that the
  `crates/core/src/grammar/generated/bbnf.rs` it writes carries
  `pub struct BbnfBootstrap;` at the top of the file, replacing the
  current line-20 declaration. The aggregator
  `crates/core/src/grammar/generated/mod.rs` re-exports it.

### 3.4 `<Grammar>Value::Unknown(NodeView)` retire-iff-totality

The `BbnfBootstrapValue::Unknown(BbnfBootstrapNodeView<'p>)` variant at
line 24 706 is the catch-all fallback for records whose `variant_idx`
isn't a known rule discriminator. Per AY-II.md invariant 8 (and the
dispatch prompt's mention of "Unknown variant per AY-II.md invariant
8 — retire iff totality test green for that grammar"), this variant
*should* be removable when `projection_totality.rs` exits 0 for the
grammar.

Today the variant is *always* emitted (the proc-macro doesn't gate on
totality). For W0.c:
- Initial emission: keep the `Unknown(<NodeView>)` variant always —
  byte-equivalence with the proc-macro output requires it.
- Follow-on (post-W0): a totality flag in the grammar manifest can
  retire the variant. Out of W0.b's scope; documented here so W0.c
  knows it's a stable invariant for the *initial* emission.

The `BbnfBootstrapRuleKind::Unknown` variant (line 23 614) is the
parallel discriminator-side fallback. Same retirement story.

### 3.5 Helper fns that look shared but emit per-grammar

Several internal helper fns carry the grammar ident in their name and
therefore emit *per grammar* despite their bodies being grammar-agnostic
in form:

- `__phf_<Grammar>_dispatch_<n>` — the body is keyword-binary-search
  over a per-grammar `KW` slice. Form is grammar-agnostic, but the
  function takes the grammar's `KW` slice as a closed-over reference
  and the `IDX` slice as a closed-over output array, so it emits
  per-grammar.
- `__regex_scan_<Grammar>` (line 973) — body specialises on the
  per-grammar regex IDs in `__DTA_REGEX_<n>`. Per-grammar.
- `materialize_object_<Grammar>`, `materialize_array_<Grammar>`,
  `materialize_string_<Grammar>`, `materialize_number_<Grammar>`,
  `materialize_literal_<Grammar>`, `materialize_value_<Grammar>` —
  these would *look* shareable but each closes over the grammar's
  `<Grammar>Value` enum (which itself is grammar-specific). Per-grammar.
- `project_rule_kind_<Grammar>`, `project_frame_<Grammar>`,
  `project_value_<Grammar>` — close over the grammar's
  `<Grammar>RuleKind` enum + view types. Per-grammar.
- `__path_walk` (line 24 735) — *not* `_<Grammar>`-suffixed, but
  closes over `BbnfBootstrapNodeView<'p>` directly. The function is
  named generically because it's *inside* the per-grammar emit-impl
  module; from outside the module, `BbnfBootstrap::__path_walk` is
  uniquely qualified. Per-grammar.
- `cst_identifier_text` / `cst_identifier_span` /
  `cst_find_identifier_cursor` (lines 32 996 / 33 012 / 33 022) —
  same pattern: unsuffixed names, but reference `BbnfBootstrapNodeView`
  / `TapeCursor` directly. Per-grammar.

W0.c's emitter writes all of the above into `generated/<ident>.rs`.
None move to a shared `crates/core/src/runtime/` location.

### 3.6 `pub mod cst_directives`

The `cst_directives` module (lines 33 045-33 290) carries typed
directive accessors (`ImportDirective`, `PrettyDirective`,
`WsDirective`, etc.). The directive *names* are part of the BBNF DSL
itself — every grammar can in principle have any subset of the seven
directives. The module's *contents* depend on which directives the
grammar declares.

For BBNF self-host, the grammar declares all seven directives, so
`cst_directives` carries all seven typed structs. For a grammar that
declares only `@import` and `@pretty`, the emit will carry only those
two. Per-grammar.

### 3.7 The proc-macro's emission *order* is part of the contract

The byte-equivalent gate in W0.c §6 compares the post-regen
`generated/bbnf.rs` to the pre-B2 `generated.rs` snapshot. A
re-ordered emission fails the gate even when semantically identical.
W0.c's emitter therefore preserves the proc-macro's emission order,
which is the order documented in §2.1-§2.18 above:

1. Grammar source const + profile.
2. PHF dispatch tables (sorted by site id, ascending).
3. Pratt precedence tables (sorted by Pratt-rule declaration order in
   the grammar, with the root composite last).
4. Mined regex strings + last-byte-set + regex scanner.
5. `__shape_support` module.
6. Per-rule shape parser fns (sorted by rule declaration order).
7. `STRUCTURAL_SCAN_POLICY`.
8. Root parse entries.
9. Per-rule view structs (sorted by rule declaration order; alt-coercion `<rule>Value` enums emitted immediately after their parent view).
10. `<Grammar>NodeView` + `<Grammar>RuleKind`.
11. Root impls + inherent root impl.
12. Projection structs + projection consts.
13. `<Grammar>Value` enum + `ValueRoot` impl.
14. Path-walk + PathQuery impls.
15. Generic materialisers + projection fns.
16. Per-rule projection materialiser fns.
17. Prettify impl block (containing `parse`).
18. CST helpers + `cst_directives` module.

Sub-orderings inside each group preserve declaration order from the
grammar file.

## §4 — W0.c emitter task summary

W0.c reads this document as its task spec. The concrete checklist:

### 4.1 Item count

- ~261 per-grammar items per grammar (BBNF count from §2 summary;
  other grammars' counts will differ as their rule counts vary).
- 33 273 lines of grammar-specific output for BBNF (lines 20-33 292
  of `generated.rs`).

### 4.2 Output file path

- Per-grammar: `crates/core/src/grammar/generated/<ident>.rs`.
- W0.c's BBNF emission target: `crates/core/src/grammar/generated/bbnf.rs`.
- Aggregator stub: `crates/core/src/grammar/generated/mod.rs` (begins
  empty; populated to `pub mod bbnf;` by W0.c).
- The legacy single-file `crates/core/src/grammar/generated.rs` either
  retires or becomes the aggregator-only file (per B2 invariant 5);
  W0.c's call is to retire it once the per-grammar file lands and the
  bbnf-bootstrap consumer has been cut over to
  `include!("crates/core/src/grammar/generated/bbnf.rs")`.

### 4.3 Byte-equivalent gate target

- Reference snapshot: `docs/tranches/B2/audit/W0-bbnf-surface-snapshot.rs`
  (byte-identical to pre-B2 `crates/core/src/grammar/generated.rs`,
  33 293 lines).
- Comparison: `diff <(rustfmt --emit=stdout < snapshot) <(rustfmt
  --emit=stdout < generated/bbnf.rs)` returns 0 lines (per W0.md §6),
  OR the diff documents a deterministic macro-hygiene shift.

### 4.4 Cargo command W0.c invokes

- W0.a wires up the CLI: `xtask/src/main.rs` parses `cargo xtask regen
  --grammar <ident>`.
- W0.c implements `xtask::regen::regen_grammar(ident: &str)` per
  W0.md §B2.W0.c.1.
- Validation: `cargo xtask regen --grammar bbnf` exits 0; output file
  `crates/core/src/grammar/generated/bbnf.rs` exists and parses as
  valid Rust.

### 4.5 Validation chain

- `cargo xtask regen --grammar bbnf` produces the file.
- `cargo iter-check-full --profile ax-iter` exits 0 with
  `generated/bbnf.rs` in place AND `bbnf-bootstrap` migrated to
  `include!` + the marker pattern (W0.c's "first cutover" sub-task).
- `cargo nextest run --workspace --profile ax-iter --test
  projection_totality` exits 0 (the runtime-call-count totality
  invariant per AY-II survives the new emission path).
- The byte-equivalent gate at §4.3 holds.

### 4.6 Boundary discipline summary for W0.c

- Per §1: W0.c does **not** emit any of the listed shared-infra
  items into `generated/bbnf.rs`. The runtime types, traits, and tape
  substrate all reference via `::bbnf::runtime::*` /
  `::bbnf::runtime::tape::*` paths (already imported via the
  3-line `use` block at the top of the per-grammar file).
- Per §2: W0.c emits all 261 enumerated items in the order
  documented in §3.7, preserving the proc-macro's emission order so
  the byte-equivalent gate is meaningful.
- Per §3.5: W0.c does **not** factor any of the
  `__regex_scan_<Grammar>` / `materialize_*_<Grammar>` /
  `__phf_<Grammar>_*` helpers into shared code. They look shareable
  but close over per-grammar types and consts.
- Per §3.4: W0.c emits the `<Grammar>Value::Unknown(NodeView)` and
  `<Grammar>RuleKind::Unknown` variants always for byte-equivalence;
  totality-conditional retirement is post-W0 polish.
- Per §3.2: W0.c emits the *actually-existing* trait surface (`Root`,
  `ValueRoot`, `PathQuery<T>`) — not the non-existent `Parser` /
  `ParserNodeView` traits referenced in the dispatch prompt.

End of boundary spec.
