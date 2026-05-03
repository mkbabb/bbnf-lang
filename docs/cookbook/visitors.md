# Cookbook — Visitors + `VisitTypes` Bitflag Pruning

The `Visitor<'i, T>` trait + `Visit<'i, T, V>` trait + `VisitTypes` bitflag is bbnf-lang's tree-transformation surface. It is lightning-css-isomorphic per `audit/SOTA-2026-05-03.md:104-118`: per-record `visit_<Name>(&mut self, &mut T)` methods; depth-first conditional traversal; bitflag-driven subtree pruning.

This page covers: the mental model (§1), the per-grammar visitor surfaces (§2), examples (§3), the silent-non-call pitfall (§4), and troubleshooting (§5).

## §1 Model

Three traits compose the visitor pattern:

```rust
pub trait Visitor<'i, T> {
    type Error;
    fn visit_types(&self) -> VisitTypes;
    fn visit_color(&mut self, _: &mut CssColor<'i>) -> Result<(), Self::Error> { Ok(()) }
    fn visit_length(&mut self, _: &mut Length<'i>) -> Result<(), Self::Error> { Ok(()) }
    // ... per-record methods, all defaulting to Ok(())
}

pub trait Visit<'i, T, V: ?Sized + Visitor<'i, T>> {
    const CHILD_TYPES: VisitTypes;
    fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error>;
    fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error>;
}
```

The `Visitor` trait declares user logic — what to do at each visited record. The `Visit` trait declares record-level traversal — `CHILD_TYPES` (which types can appear as descendants), `visit` (dispatch to the matching `visit_<Name>` method), `visit_children` (recurse into nested fields).

The `VisitTypes` bitflag is the pruning mechanism. The visitor declares which types it wants via `visit_types()`; the framework compares against each record's `CHILD_TYPES`; if the intersection is empty, `visit_children` short-circuits — the subtree never traverses.

```text
   sheet.visit(&mut visitor)
              |
              v
   for each child of sheet:
       if child.CHILD_TYPES & visitor.visit_types() == empty:
           skip child entirely (PRUNE)
       else:
           recurse into child
```

The pruning is layout-flavoured: the type system computes `CHILD_TYPES` at derive time as the union of the type's child fields' `CHILD_TYPES` plus its own `Self::TYPE`. A `CssRule` whose declarations contain only `Length` and `Number` has `CHILD_TYPES = LENGTHS | RULES`; a visitor with `visit_types() = COLORS` skips the entire `CssRule` subtree.

## §2 Per-grammar visitor surfaces

### CSS L4

| Method | Receives | Flag | Use case |
|---|---|---|---|
| `visit_color` | `&mut CssColor<'i>` | `COLORS` | Colour transforms (dark-mode, alpha) |
| `visit_length` | `&mut Length<'i>` | `LENGTHS` | Unit normalisation (px ↔ rem) |
| `visit_url` | `&mut Url<'i>` | `URLS` | URL rewriting |
| `visit_property` | `&mut Property<'i>` | `PROPERTIES` | Property-level transforms |
| `visit_rule` | `&mut CssRule<'i, T>` | `RULES` | Rule-level transforms |
| `visit_selector` | `&mut Selector<'i>` | `SELECTORS` | Selector rewriting |
| `visit_function` | `&mut Function<'i>` | `FUNCTIONS` | Function-call transforms |
| `visit_angle` | `&mut Angle` | `ANGLES` | Angle transforms |
| `visit_time` | `&mut Time` | `TIMES` | Time transforms (s ↔ ms) |
| `visit_media_query` | `&mut MediaQuery<'i>` | `MEDIA_QUERIES` | Media query rewriting |
| (and 8 others — see `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md` §2) | | | |

### JSON

| Method | Receives | Flag | Use case |
|---|---|---|---|
| `visit_string` | `&mut JsonString<'i>` | `STRINGS` | String normalisation |
| `visit_number` | `&mut JsonNumber` | `NUMBERS` | Number rewriting |
| `visit_object` | `&mut JsonObject<'i>` | `OBJECTS` | Object-level transforms |
| `visit_array` | `&mut JsonArray<'i>` | `ARRAYS` | Array-level transforms |
| `visit_bool` | `&mut bool` | `BOOLS` | Bool transforms |
| `visit_null` | `&mut ()` | `NULLS` | Null handling |

### BBNF

| Method | Receives | Flag |
|---|---|---|
| `visit_rule_def` | `&mut RuleDef<'i>` | `RULE_DEFS` |
| `visit_alt` | `&mut Alt<'i>` | `ALTS` |
| `visit_seq` | `&mut Seq<'i>` | `SEQS` |
| `visit_repeat` | `&mut Repeat<'i>` | `REPEATS` |
| `visit_regex_atom` | `&mut RegexAtom<'i>` | `REGEX_ATOMS` |
| `visit_keyword_atom` | `&mut KeywordAtom<'i>` | `KEYWORD_ATOMS` |

### Sheets

| Method | Receives | Flag |
|---|---|---|
| `visit_cell_ref` | `&mut CellRef<'i>` | `CELL_REFS` |
| `visit_function_call` | `&mut FunctionCall<'i>` | `FUNCTION_CALLS` |
| `visit_identifier` | `&mut Identifier<'i>` | `IDENTIFIERS` |
| `visit_range_ref` | `&mut RangeRef<'i>` | `RANGE_REFS` |

### Cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math)

Cohort visitor methods emerge 1:1 from the `<G>Value` enum variants per BB.W2a templated emission. Each cohort grammar's `<G>Visitor` has methods matching its `<G>Value` cardinality (3-10 methods per cohort).

## §3 Examples

### CSS L4: dark-mode colour transform

```rust
use bbnf::grammar::css_l4::{parse, CssVisitor, CssVisitTypes, CssColor};

let css = r#".foo { color: red; background: blue; }"#;
let mut sheet = parse(css)?;

struct DarkModeVisitor;
impl<'i> CssVisitor<'i, ()> for DarkModeVisitor {
    type Error = ();
    
    fn visit_types(&self) -> CssVisitTypes {
        CssVisitTypes::COLORS  // only colors
    }
    
    fn visit_color(&mut self, color: &mut CssColor<'i>) -> Result<(), ()> {
        // invert: light → dark, dark → light
        *color = invert_color(color.clone());
        Ok(())
    }
}

sheet.visit(&mut DarkModeVisitor)?;
// sheet now carries inverted colors; selectors, lengths, etc. unchanged
```

The visitor declares `visit_types() = COLORS`; `visit_color` mutates each `CssColor`; non-color subtrees are pruned (declarations whose values are only lengths/numbers are skipped entirely).

### JSON: string normalisation

```rust
use bbnf::grammar::json::{parse_owned, JsonVisitor, JsonVisitTypes, JsonString, JsonOwnedValue};

let json = r#"{"name": "  Ada  ", "email": "  ada@analytical.engine  "}"#;
let mut value: JsonOwnedValue = parse_owned(json)?;

struct TrimVisitor;
impl JsonVisitor<JsonOwnedValue> for TrimVisitor {
    type Error = ();
    fn visit_types(&self) -> JsonVisitTypes { JsonVisitTypes::STRINGS }
    fn visit_string(&mut self, s: &mut JsonString) -> Result<(), ()> {
        *s = JsonString::from(s.as_str().trim().to_string());
        Ok(())
    }
}

value.visit(&mut TrimVisitor)?;
// value now has trimmed strings
```

### BBNF: rule renaming

```rust
struct RenameVisitor { from: String, to: String }
impl<'i> BbnfVisitor<'i, ()> for RenameVisitor {
    type Error = ();
    fn visit_types(&self) -> BbnfVisitTypes { BbnfVisitTypes::RULE_DEFS | BbnfVisitTypes::KEYWORD_ATOMS }
    fn visit_rule_def(&mut self, rule: &mut RuleDef<'i>) -> Result<(), ()> {
        if rule.name == self.from { rule.name = self.to.clone().into(); }
        Ok(())
    }
    fn visit_keyword_atom(&mut self, atom: &mut KeywordAtom<'i>) -> Result<(), ()> {
        if atom.text == self.from { atom.text = self.to.clone().into(); }
        Ok(())
    }
}
```

The visitor handles BOTH rule definitions AND atom references; the `visit_types` bitflag must include BOTH for both methods to fire. If only `RULE_DEFS` is set, the keyword atom subtrees are pruned and `visit_keyword_atom` never fires.

## §4 The silent-non-call pitfall (F07-E7)

The most common visitor bug: implementing a `visit_<Name>` method but forgetting to set the corresponding bit in `visit_types()`. The framework prunes the subtree; the method never fires. The compiler does NOT warn (the trait method is well-formed); the bug is silent.

Verbatim warning per F07-E7 of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:30`:

```text
warning: visitor method visit_<Name> is implemented, but visit_types() does not include <Name>::CHILD_TYPES;
         this subtree will be skipped
   --> src/visitor_impl.rs:12:5
    |
  5 |     fn visit_types(&self) -> CssVisitTypes { CssVisitTypes::PROPERTIES }
    |     -- visit_types declares: PROPERTIES
   ..
 12 |     fn visit_color(&mut self, _: &mut CssColor) -> Result<(), ()> {
    |     ^^^^^^^^^^^^^^^^^^^^^^^^^ this method requires `CssVisitTypes::COLORS`
    |
help: add the matching flag to visit_types
    |     fn visit_types(&self) -> CssVisitTypes { CssVisitTypes::PROPERTIES | CssVisitTypes::COLORS }
    |                                                                          ^^^^^^^^^^^^^^^^^^^^^
```

The warning fires at compile time via a `bbnf-derive` lint over the user's `Visitor` impl. The lint walks each implemented `visit_<Name>` and checks against the constant returned by `visit_types()`; missing bits become a warning with a fix-it.

If the user OVERRIDES `visit_types()` with a runtime computation (e.g., reading from a config), the lint cannot statically verify; in that case the warning is silent and runtime testing is the only verification.

## §5 Troubleshooting

### "I implemented `visit_color` but it never fires"

Check: does `visit_types()` include `CssVisitTypes::COLORS`? The lint at §4 catches the static case; if you used a dynamic `visit_types()` body, log the actual value and verify the bit.

### "the visitor recurses into subtrees I don't care about"

Tighten `visit_types()` to only the bits you need. Each excluded bit prunes whole subtrees; the cost is constant per pruned subtree (single bitflag intersection check).

### "my visitor mutates a record but the change is gone after `.visit()`"

You're holding a `&mut <Record>` for the duration of `visit_<Name>`; mutations DO persist. If you observe the change reverting, you might be operating on a clone — check the receiver's actual lifetime model (see `docs/cookbook/lifetime-surfaces.md`).

### "I want to traverse selectively per subtree"

The visitor pattern is uniform across the tree; for selective traversal, use `visit_types()` to gate by record type, then add per-record logic to skip specific instances. Example: skip declarations whose property name starts with `--` (CSS variables):

```rust
fn visit_property(&mut self, p: &mut Property<'i>) -> Result<(), ()> {
    if p.name.starts_with("--") { return Ok(()); }  // skip CSS vars
    // ... transform non-var properties
    Ok(())
}
```

### "I want to halt traversal early"

Return `Err(...)` from any `visit_<Name>` method; the framework propagates the error, halting traversal immediately. The error type is the visitor's `type Error`; choose a sentinel if early-halt is the only signal.

### "I want to visit children before the parent (post-order)"

Override `visit` per-record:

```rust
impl<'i, V: CssVisitor<'i, ()>> Visit<'i, (), V> for CssRule<'i, ()> {
    const CHILD_TYPES: CssVisitTypes = CssVisitTypes::PROPERTIES | CssVisitTypes::SELECTORS;
    fn visit(&mut self, v: &mut V) -> Result<(), V::Error> {
        self.visit_children(v)?;  // children first
        v.visit_rule(self)?;       // then self
        Ok(())
    }
}
```

The default `visit` is pre-order (self before children); overriding swaps the order.

### Performance considerations

- `visit_types()` is called once per record; the bitflag intersection is a single AND.
- Per-record `visit_<Name>` cost is user-defined; default `Ok(())` is zero cost.
- Subtree pruning eliminates traversal cost for excluded types entirely.
- The CSS L4 visitor traversal of bootstrap.css ≤ 5 ms (≤ 1.4× of parse cost) per BB-G9 + BB.W5b M4 bench.
- The JSON visitor traversal of twitter.json ≤ 500 µs per BB.W5b M5 bench.

The visitor is lightning-css-class: ≤ 1.4× of parse cost for full traversal; equal to the SOTA reference at `audit/SOTA-2026-05-03.md:131`.
