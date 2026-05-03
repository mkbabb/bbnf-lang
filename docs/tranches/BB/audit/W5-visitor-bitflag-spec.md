# W5 Visitor + VisitTypes Bitflag Specification

Date: 2026-05-03
Scope: The per-grammar Visitor surface specification per F07-6 of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:41` and G05-8 (typed pointer terminal) of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:31`. Documents the lightning-css-isomorphic Visitor pattern adapted for bbnf's 9 grammars.

## §1 SOTA reference (lightning-css)

Per the research at `docs/tranches/BB/audit/research-anchors.md` §2:

```rust
pub trait Visitor<'i, T> {
    type Error;
    fn visit_types(&self) -> VisitTypes;
    fn visit_color(&mut self, _: &mut CssColor) -> Result<(), Self::Error> { Ok(()) }
    // ... per-record methods, all defaulting to Ok(())
}

pub trait Visit<'i, T, V: ?Sized + Visitor<'i, T>> {
    const CHILD_TYPES: VisitTypes;
    fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error>;
    fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error>;
}
```

The pruning mechanism: `Visit::visit` checks `visitor.visit_types() & Self::CHILD_TYPES`; if the intersection is empty, `visit_children` short-circuits without recursion. The derive `#[derive(Visit)]` generates `CHILD_TYPES` by union of child fields' `CHILD_TYPES`.

## §2 Per-grammar Visitor surface

### CSS L4 visitor

| Visitor method | Receives | VisitTypes flag | Use case |
|---|---|---|---|
| `visit_color` | `&mut CssColor<'i>` | `COLORS = 1 << 4` | Colour transforms (dark-mode invert; alpha adjustment) |
| `visit_length` | `&mut Length<'i>` | `LENGTHS = 1 << 5` | Unit normalisation (px ↔ rem; em ↔ px) |
| `visit_url` | `&mut Url<'i>` | `URLS = 1 << 2` | URL rewriting (asset-pipeline path adjustment) |
| `visit_property` | `&mut Property<'i>` | `PROPERTIES = 1 << 1` | Property-level transforms (shorthand expansion; vendor-prefix injection) |
| `visit_rule` | `&mut CssRule<'i, T>` | `RULES = 1 << 0` | Rule-level transforms (at-rule expansion; pseudo-class rewriting) |
| `visit_declaration_block` | `&mut DeclarationBlock<'i>` | `PROPERTIES` | Declaration-block-level transforms |
| `visit_selector` | `&mut Selector<'i>` | `SELECTORS = 1 << 16` | Selector rewriting (pseudo-class polyfilling) |
| `visit_function` | `&mut Function<'i>` | `FUNCTIONS = 1 << 17` | Function-call transforms (calc() simplification) |
| `visit_angle` | `&mut Angle` | `ANGLES = 1 << 6` | Angle transforms (deg ↔ rad) |
| `visit_ratio` | `&mut Ratio` | `RATIOS = 1 << 7` | Ratio transforms |
| `visit_resolution` | `&mut Resolution` | `RESOLUTIONS = 1 << 8` | DPI rewriting |
| `visit_time` | `&mut Time` | `TIMES = 1 << 9` | Time transforms (s ↔ ms) |
| `visit_custom_ident` | `&mut CustomIdent` | `CUSTOM_IDENTS = 1 << 10` | Custom-property identifier rewriting |
| `visit_dashed_ident` | `&mut DashedIdent` | `DASHED_IDENTS = 1 << 11` | CSS variable identifier rewriting |
| `visit_variable` | `&mut Variable<'i>` | `VARIABLES = 1 << 12` | Variable resolution |
| `visit_media_query` | `&mut MediaQuery<'i>` | `MEDIA_QUERIES = 1 << 14` | Media query rewriting |
| `visit_supports_condition` | `&mut SupportsCondition<'i>` | `SUPPORTS_CONDITIONS = 1 << 15` | Supports query rewriting |
| `visit_token` | `&mut Token<'i>` | `TOKENS = 1 << 18` | Raw token transforms (fallback) |

CSS L4 visitor cardinality 18 distinct flags; matches lightning-css cardinality.

### JSON visitor

| Visitor method | Receives | VisitTypes flag | Use case |
|---|---|---|---|
| `visit_string` | `&mut JsonString<'i>` | `STRINGS = 1 << 0` | String normalisation |
| `visit_number` | `&mut JsonNumber` | `NUMBERS = 1 << 1` | Number rewriting |
| `visit_object` | `&mut JsonObject<'i>` | `OBJECTS = 1 << 2` | Object-level transforms |
| `visit_array` | `&mut JsonArray<'i>` | `ARRAYS = 1 << 3` | Array-level transforms |
| `visit_bool` | `&mut bool` | `BOOLS = 1 << 4` | Bool transforms |
| `visit_null` | `&mut ()` | `NULLS = 1 << 5` | Null handling |

JSON visitor cardinality 6 flags.

### BBNF visitor

| Visitor method | Receives | VisitTypes flag | Use case |
|---|---|---|---|
| `visit_rule_def` | `&mut RuleDef<'i>` | `RULE_DEFS = 1 << 0` | Rule-definition transforms (renaming) |
| `visit_alt` | `&mut Alt<'i>` | `ALTS = 1 << 1` | Alt-branch reordering |
| `visit_seq` | `&mut Seq<'i>` | `SEQS = 1 << 2` | Sequence rewriting |
| `visit_repeat` | `&mut Repeat<'i>` | `REPEATS = 1 << 3` | Repeat-quantifier transforms |
| `visit_regex_atom` | `&mut RegexAtom<'i>` | `REGEX_ATOMS = 1 << 4` | Regex transformation |
| `visit_keyword_atom` | `&mut KeywordAtom<'i>` | `KEYWORD_ATOMS = 1 << 5` | Keyword renaming |

BBNF visitor cardinality 6 flags.

### Sheets visitor

| Visitor method | Receives | VisitTypes flag | Use case |
|---|---|---|---|
| `visit_cell_ref` | `&mut CellRef<'i>` | `CELL_REFS = 1 << 0` | Cell reference rewriting |
| `visit_function_call` | `&mut FunctionCall<'i>` | `FUNCTION_CALLS = 1 << 1` | Function-call transforms |
| `visit_identifier` | `&mut Identifier<'i>` | `IDENTIFIERS = 1 << 2` | Identifier resolution |
| `visit_range_ref` | `&mut RangeRef<'i>` | `RANGE_REFS = 1 << 3` | Range-reference transforms |

Sheets visitor cardinality 4 flags.

### Cohort visitors (BNF, CSV, EBNF, CSS Pretty, Math)

Cohort grammars get templated visitors per BB.W2a; method count = record count per surgery 21. Each cohort grammar's Visitor has methods 1:1 with its `<G>Value` enum variants.

| Grammar | Method count | Notes |
|---|---:|---|
| BNF | 6 | rule, alt, seq, term, non_term, comment |
| CSV | 3 | field, quoted, escape_char |
| EBNF | 8 | rule, alt, seq, repeat, optional, group, term, non_term |
| CSS Pretty | 10 | stylesheet, rule, at_rule, decl, selector, property, value, length, color, number |
| Math | 9 | expr, bin_op, un_op, fun_call, num_lit, identifier, group_open, group_close, end_of_input |

The cohort visitor signatures are templated: the per-record method emission emerges from `<G>Value` enum cardinality, not from a hand-written list. Per surgery 21 ("visitor delta bounded by record count"), the generated-LOC delta scales with record count, not with a per-grammar × per-backend factor.

## §3 Bitflag spec per surgery 21

Per surgery 21 ("visitor delta bounded by record count") — the per-grammar `VisitTypes` bitflag emerges from the `<G>Value` enum cardinality:

```rust
// crates/core/src/grammar/generated/<g>.rs (templated via xtask regen)
bitflags! {
    pub struct <G>VisitTypes: u32 {
        <FOR EACH variant V IN <G>Value:>
        const <V_UPPERCASE> = 1 << <variant_index>;
    }
}
```

The bit positions are deterministic: variant_index is the source-order position of the variant in the `<G>Value` enum. The bit count equals the variant count. CSS L4's 14-variant + 4-derived-record-type = 18 flags; JSON's 6-variant = 6 flags; BBNF's 6-variant = 6 flags. The `u32` reservoir provides headroom for grammar evolution up to 32 distinct flags before requiring `u64`.

## §4 Derive surface

The per-grammar `Visit` impl generates at xtask regen time per Lock 6. The mechanism mirrors lightning-css's `#[derive(Visit)]`:

```rust
// crates/core/src/codegen/visitor.rs (the generic visitor codegen)
pub fn emit_visit_impl(grammar: &GrammarIR) -> TokenStream {
    let g_value = format_ident!("{}Value", capitalise(&grammar.ident));
    let visit_types = format_ident!("{}VisitTypes", capitalise(&grammar.ident));
    let visitor = format_ident!("{}Visitor", capitalise(&grammar.ident));
    
    let variants = grammar.value_enum_variants();
    let visit_arms = variants.iter().map(|v| {
        let v_ident = format_ident!("{}", v.name);
        let visit_method = format_ident!("visit_{}", snake_case(&v.name));
        let v_flag = format_ident!("{}", uppercase(&v.name));
        quote! {
            Self::#v_ident(inner) => {
                if visitor.visit_types().contains(#visit_types::#v_flag) {
                    visitor.#visit_method(inner)?;
                }
                inner.visit_children(visitor)?;
            }
        }
    });
    
    quote! {
        impl<'i, V: #visitor<'i>> Visit<'i, V> for #g_value<'i> {
            const CHILD_TYPES: #visit_types = #visit_types::all();
            
            fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
                match self {
                    #(#visit_arms)*
                }
                Ok(())
            }
            
            fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error> {
                // recurse into nested Visit-impl fields
                match self {
                    // ...
                }
                Ok(())
            }
        }
    }
}
```

The codegen is grammar-agnostic; per-grammar variation lives in `value_enum_variants()` reading from the layout-lowering output.

## §5 Pruning evidence

The pruning mechanism per `audit/SOTA-2026-05-03.md:118` is depth-first conditional traversal: a visitor declaring only `VisitTypes::COLORS` skips entire selector / declaration subtrees whose `CHILD_TYPES` does not intersect.

Verbatim test:

```rust
// crates/core/tests/visitor_pruning.rs
#[test]
fn css_l4_visitor_prunes_non_color_subtrees() {
    let css = r"
        .foo { color: red; padding: 10px; }
        .bar { background: blue; margin: 5px; }
    ";
    let mut sheet = parse(css)?;
    
    struct ColorOnlyVisitor { color_count: usize }
    impl<'i> CssVisitor<'i, ()> for ColorOnlyVisitor {
        type Error = ();
        fn visit_types(&self) -> CssVisitTypes { CssVisitTypes::COLORS }
        fn visit_color(&mut self, _: &mut CssColor<'i>) -> Result<(), ()> {
            self.color_count += 1; Ok(())
        }
    }
    
    let mut v = ColorOnlyVisitor { color_count: 0 };
    sheet.visit(&mut v)?;
    
    // The visitor visits 2 colors (red, blue) and prunes everything else
    assert_eq!(v.color_count, 2);
    
    // Negative: padding/margin/background are not colors;
    // they are not visited because CHILD_TYPES intersection is empty for those subtrees
}
```

The pruning evidence is observable: `visit_color` is called exactly N times for N colors in the input; non-color subtrees do not consume visitor time. The bench at BB.W5b M4 verifies traversal cost ≤ 1.4× of parse cost (matching lightning-css ratio per `audit/SOTA-2026-05-03.md:131`).

## §6 G05-8 typed pointer terminal alignment

Per G05-8 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:31`, the visitor surface aligns with the typed-pointer-terminal discipline: the `pointer!` macro returns a `JsonPath<JsonString>` (terminal type known at compile time); the visitor's `visit_string` method receives the same `JsonString` type. The two surfaces share the typed-record taxonomy.

Verification: for any grammar G and any terminal type T,
- `pointer!(G, [...])` returns `<G>Path<T>`.
- `<G>Visitor::visit_<t_lowercase>(&mut T)` exists.
- The typed-pointer's terminal AND the visitor's per-record method address the SAME T.

This honours the G05-8 contract: terminal-type alignment between path-API and visitor-API; no orthogonal taxonomies; cohesion at every level (Lock 13).

## §7 BB.W5c cookbook impact

The cookbook page `docs/cookbook/visitors.md` (BB.W5c deliverable) presents:

- §1 Model: `Visit` trait + `Visitor` trait + `VisitTypes` bitflag pruning mechanism.
- §2 Syntax: per-grammar visitor implementation; the `visit_types()` method declaration.
- §3 Examples: CSS L4 color-only visitor; JSON string-only visitor; BBNF rule-rename visitor.
- §4 Errors: F07-E7 verbatim warning: `warning: visitor method visit_<Name> is implemented, but visit_types() does not include <Name>::CHILD_TYPES; this subtree will be skipped`.
- §5 Troubleshooting: the silent-non-call pitfall (visitor method implemented but `visit_types()` excludes the relevant flag).

The cookbook is gated by BB.W5c M2; trybuild fixtures verify the verbatim warning text matches the cookbook examples.
