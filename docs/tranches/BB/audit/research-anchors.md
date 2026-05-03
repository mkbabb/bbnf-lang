# BB Research Anchors — Primary-Source Findings for the Spec-Depth Re-Draft

Date: 2026-05-03
Scope: Primary-source readings backing every BB-tranche specification gap. Each row names the URL or path:line, the exact API surface extracted, and the BB wave that consumes the finding.

---

## 1. sonic-rs `pointer!` macro + `LazyValue<'a>`

### 1.1 Macro signature (consumed by BB.W5a)

Source: `https://docs.rs/sonic-rs/latest/sonic_rs/macro.pointer.html`

```rust
macro_rules! pointer {
    () => { ... };
    ($($x:expr),+ $(,)?) => { ... };
}
```

Two arms: empty form `pointer![]` and variadic form `pointer![e1, e2, ..., eN]` with optional trailing comma. Heterogeneous segments accepted: `&str`-coercible keys for object indexing; `usize`-coercible expressions for array indexing. Mixed segments permitted within one invocation. Return type is `JsonPointer`. The macro itself performs no validation; invalid type expressions fail at compile time through downstream trait-bound resolution.

### 1.2 `JsonPointer` + `PointerNode` (consumed by BB.W5a)

Source: `https://raw.githubusercontent.com/cloudwego/sonic-rs/main/src/index.rs`

```rust
pub trait Index: Sealed {
    fn value_index_into<'v>(&self, v: &'v Value) -> Option<&'v Value>;
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value>;
    fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value;
    fn as_key(&self) -> Option<&str> { None }
    fn as_index(&self) -> Option<usize> { None }
}

pub enum PointerNode { Index(usize), Key(String) }
```

The `PointerNode` enum is the discriminator carrying both arms. `impl_str_index!` macro implements `Index` for `&str`, `&String`, `&FastStr`. `impl<T: ?Sized + Index> Index for &T` provides reference-transparency. The macro `pointer![...]` delegates to `JsonPointer::from_iter(...)` over `PointerNode`-coercible expressions.

### 1.3 `LazyValue<'a>` (consumed by BB.W5a)

Source: `https://raw.githubusercontent.com/cloudwego/sonic-rs/main/src/lazyvalue/value.rs`

```rust
// lines 77-82
pub struct LazyValue<'a> {
    pub(crate) raw: JsonSlice<'a>,
    pub(crate) inner: Inner,  // AtomicPtr<Arc<...>> for thread-safe lazy unescape
}
```

Trait `JsonValueTrait` (lines 227-280) carries `as_bool`, `as_number`, `as_str`, `get_type`, `get<I>`, `pointer<P>`. The `as_str` method (lines 252-265) strips quotes or lazily parses escaped sequences. `as_raw_str`, `as_raw_cow`, `as_raw_faststr` (lines 298-338) provide zero-copy variants. `into_object_iter`, `into_array_iter` (lines 340-350) consume the value into iterators.

### 1.4 Public API exports (consumed by BB.W5a + BB.W4a)

Source: `https://raw.githubusercontent.com/cloudwego/sonic-rs/main/src/lib.rs`

Lines 25-49 enumerate the public exports. `LazyValue`, `OwnedLazyValue`, `LazyArray`, `LazyObject` exported from `lazyvalue`. `get`, `get_unchecked`, `get_from_str`, `get_from_str_unchecked`, `get_from_bytes`, `get_from_bytes_unchecked`, `get_from_faststr`, `get_from_faststr_unchecked`, `get_from_slice`, `get_from_slice_unchecked`, `get_many`, `get_many_unchecked` exported as standalone fns. Core types: `Error`, `Result`, `JsonInput`, `JsonPointer`, `PointerNode`, `PointerTree`. Traits: `JsonValueTrait`, `JsonValueMutTrait`, `JsonContainerTrait`, `JsonType`. Serialization fns: `from_str`, `from_slice`, `from_reader`, `to_string`, `to_vec`, `to_writer`. The `pointer!` macro itself is exported via `#[macro_export]` from the pointer module (the lib.rs excerpt does not show macro re-exports because macros use a separate path).

---

## 2. lightningcss `Visitor` trait + `VisitTypes` bitflag

### 2.1 Trait surface (consumed by BB.W5b)

Source: `https://raw.githubusercontent.com/parcel-bundler/lightningcss/master/src/visitor.rs`

```rust
// lines ~99-200
pub trait Visitor<'i, T> {
    type Error;
    fn visit_types(&self) -> VisitTypes;
    fn visit_stylesheet(&mut self, _: &mut StyleSheet<'i, '_, T>) -> Result<(), Self::Error> { ... }
    fn visit_rule_list(&mut self, _: &mut Vec<CssRule<'i, T>>) -> Result<(), Self::Error> { ... }
    fn visit_rule(&mut self, _: &mut CssRule<'i, T>) -> Result<(), Self::Error> { ... }
    fn visit_declaration_block(&mut self, _: &mut DeclarationBlock<'i>) -> Result<(), Self::Error> { ... }
    fn visit_property(&mut self, _: &mut Property<'i>) -> Result<(), Self::Error> { ... }
    fn visit_url(&mut self, _: &mut Url<'i>) -> Result<(), Self::Error> { ... }
    fn visit_color(&mut self, _: &mut CssColor) -> Result<(), Self::Error> { ... }
    fn visit_image(&mut self, _: &mut Image<'i>) -> Result<(), Self::Error> { ... }
    fn visit_length(&mut self, _: &mut LengthValue) -> Result<(), Self::Error> { ... }
    fn visit_angle(&mut self, _: &mut Angle) -> Result<(), Self::Error> { ... }
    fn visit_ratio(&mut self, _: &mut Ratio) -> Result<(), Self::Error> { ... }
    fn visit_resolution(&mut self, _: &mut Resolution) -> Result<(), Self::Error> { ... }
    fn visit_time(&mut self, _: &mut Time) -> Result<(), Self::Error> { ... }
    fn visit_custom_ident(&mut self, _: &mut CustomIdent) -> Result<(), Self::Error> { ... }
    fn visit_dashed_ident(&mut self, _: &mut DashedIdent) -> Result<(), Self::Error> { ... }
    fn visit_variable(&mut self, _: &mut Variable<'i>) -> Result<(), Self::Error> { ... }
    fn visit_environment_variable(&mut self, _: &mut EnvironmentVariable<'i>) -> Result<(), Self::Error> { ... }
    fn visit_media_list(&mut self, _: &mut MediaList<'i>) -> Result<(), Self::Error> { ... }
    fn visit_media_query(&mut self, _: &mut MediaQuery<'i>) -> Result<(), Self::Error> { ... }
    fn visit_media_feature(&mut self, _: &mut MediaFeature<'i>) -> Result<(), Self::Error> { ... }
    fn visit_media_feature_value(&mut self, _: &mut MediaFeatureValue<'i>) -> Result<(), Self::Error> { ... }
    fn visit_supports_condition(&mut self, _: &mut SupportsCondition<'i>) -> Result<(), Self::Error> { ... }
    fn visit_selector_list(&mut self, _: &mut SelectorList<'i>) -> Result<(), Self::Error> { ... }
    fn visit_selector(&mut self, _: &mut Selector<'i>) -> Result<(), Self::Error> { ... }
    fn visit_function(&mut self, _: &mut Function<'i>) -> Result<(), Self::Error> { ... }
    fn visit_token_list(&mut self, _: &mut TokenList<'i>) -> Result<(), Self::Error> { ... }
    fn visit_token(&mut self, _: &mut Token<'i>) -> Result<(), Self::Error> { ... }
}
```

Most methods delegate to `visit_children()`. Value-specific methods (color, length, etc.) return `Ok(())` by default. The `Visitor` does NOT walk; the `Visit` trait does.

### 2.2 `VisitTypes` bitflag (consumed by BB.W5b)

Source: same file, lines ~60-80

```rust
bitflags! {
    pub struct VisitTypes: u32 {
        const RULES                  = 1 << 0;
        const PROPERTIES             = 1 << 1;
        const URLS                   = 1 << 2;
        const COLORS                 = 1 << 3;
        const IMAGES                 = 1 << 4;
        const LENGTHS                = 1 << 5;
        const ANGLES                 = 1 << 6;
        const RATIOS                 = 1 << 7;
        const RESOLUTIONS            = 1 << 8;
        const TIMES                  = 1 << 9;
        const CUSTOM_IDENTS          = 1 << 10;
        const DASHED_IDENTS          = 1 << 11;
        const VARIABLES              = 1 << 12;
        const ENVIRONMENT_VARIABLES  = 1 << 13;
        const MEDIA_QUERIES          = 1 << 14;
        const SUPPORTS_CONDITIONS    = 1 << 15;
        const SELECTORS              = 1 << 16;
        const FUNCTIONS              = 1 << 17;
        const TOKENS                 = 1 << 18;
    }
}
```

Cardinality 19 distinct flags; u32 reserved for extension headroom.

### 2.3 `Visit` trait + `CHILD_TYPES` (consumed by BB.W5b)

Source: same file, lines ~212-240

```rust
pub trait Visit<'i, T, V: ?Sized + Visitor<'i, T>> {
    const CHILD_TYPES: VisitTypes;
    fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error>;
    fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error>;
}
```

`CHILD_TYPES` is computed at derive time from nested value analysis. Pruning: the framework compares `CHILD_TYPES` against `visitor.visit_types()`; if their intersection is empty, `visit_children` short-circuits without recursion. Depth-first conditional traversal is layout-flavoured and mechanical.

### 2.4 Procedural derive (consumed by BB.W5b)

Source: `https://raw.githubusercontent.com/parcel-bundler/lightningcss/master/derive/src/lib.rs`

`#[derive(Visit)]` accepts attributes `visit`, `skip_visit`, `skip_type`, `visit_types`. Implementation lives in three internal modules: `parse::`, `to_css::`, `visit::`. The `visit::derive_visit_children()` function generates `Visit` implementations from the AST: it computes `CHILD_TYPES` by union of child-field `CHILD_TYPES`, generates `visit_children` to recurse into each field, and emits `visit` to either dispatch to a named `visit_<Name>` method or delegate to `visit_children` when no per-record method matches.

---

## 3. chumsky `Parser<'src, I, O, E>` typed surface

Source: `https://raw.githubusercontent.com/zesterer/chumsky/master/src/lib.rs`

```rust
pub trait Parser<'src, I: Input<'src>, O, E: ParserExtra<'src, I>> {
    fn parse(&self, input: I) -> ParseResult<O, E::Error>
    where
        I: Input<'src>,
        E::State: Default,
        E::Context: Default;
    
    fn go<M: Mode>(&self, inp: &mut InputRef<'src, '_, I, E>) -> PResult<M, O>;
}
```

Output type `O` is **inferred from usage context**, never user-specified at `parse()`. Constraints come from downstream operations: `.map()`, `.collect()`, type ascription on the result. Combinators transform `O` → `U` via `.map<U, F: Fn(O) -> U>(self, f: F) -> Map<Self, O, F>`. The pattern `pointer!["a","b",1]` analogue: a typed parser whose `O` resolves via call-site context. This is option (ii) of the BB.W5a pointer-syntax decision — implicit inference via type ascription.

---

## 4. lalrpop / pest / chumsky codegen patterns

### 4.1 lalrpop per-rule fn shape (consumed by BB.W2a, BB.W1a–W1c)

The lalrpop codegen emits one function per nonterminal: `fn parse_<NonTerminal><'input>(input: &'input str, ...) -> Result<...>`. The function dispatches on the LALR(1) action table to either shift a token, reduce by a production, or reject. Each rule's typed return signature carries the user-declared action's return type; lalrpop's `<>` placeholder substitutes the matched semantic value.

### 4.2 pest derive output shape (consumed by BB.W2a)

`#[derive(Parser)]` from a `.pest` grammar emits an enum `Rule { foo, bar, ... }` and a parser `impl<'i> Parser<Rule> for FooParser`. The output type is `Pairs<'i, Rule>` (untyped iterator); the user matches on `Rule::*` and walks `Pair::into_inner()`. This is the antithesis of typed AST emission and serves as the negative anchor for BB.W2a's cohort template specification.

### 4.3 chumsky combinator shape (consumed by BB.W2a)

chumsky has no codegen; its parsers are GAT-driven combinators that compile-time-monomorphise. The pattern is anchored as the alternative-to-codegen for grammar-author DSL embedding; BB does not adopt it because Lock 6 requires committed source artefacts.

---

## 5. bumpalo + parse_in idiom

Source: `https://docs.rs/bumpalo/latest/bumpalo/struct.Bump.html`

```rust
pub struct Bump { ... }
impl Bump {
    pub fn new() -> Self;
    pub fn with_capacity(capacity: usize) -> Self;
    pub fn alloc<T>(&self, val: T) -> &mut T;
    pub fn alloc_slice_copy<T: Copy>(&self, src: &[T]) -> &mut [T];
    pub fn alloc_str(&self, src: &str) -> &mut str;
    pub fn reset(&mut self);
}
```

Allocations return `&mut T` exclusive references tied to the `Bump` arena's lifetime. `Drop` impls never run on bump-allocated objects; deallocation occurs only at `reset()` or arena drop. The sonic-rs / serde-bumpalo `parse_in(input, &bump)` pattern: caller provides the `Bump`, parser allocates from it, returned values borrow the arena's lifetime not the input's. This is BB.W4a's bumpalo-opt-in escape hatch.

---

## 6. serde DeserializeOwned escape

Source: `https://docs.rs/serde/latest/serde/de/trait.DeserializeOwned.html`

```rust
pub trait DeserializeOwned: for<'de> Deserialize<'de> { }
impl<T> DeserializeOwned for T where T: for<'de> Deserialize<'de>
```

The `for<'de>` higher-ranked trait bound requires deserialization to work with any input lifetime. `DeserializeOwned` types do not borrow from input; the output remains valid beyond the deserializer's scope. `from_str: T: Deserialize<'a>` (may borrow); `from_reader: T: DeserializeOwned` (must own). This is the precedent for BB.W4a's `parse_owned(input)` surface: returns a `<G>OwnedValue` whose lifetime is `'static`-equivalent.

---

## 7. Pratt parser detection

Standard Pratt heuristic per pest + chumsky `pratt::*` builders: a rule `expr := factor (op factor)*` with a closed-enumerated `op` set and a precedence + associativity declaration. The detection mining at `crates/ir/src/passes/recognizers/operator_chain.rs` walks the IR for rules of this shape: left-recursive Alt with one branch carrying a `factor (op factor)+` Seq pattern. Pratt is selected when (a) `op` enumeration is closed, (b) precedence + associativity are declarable from rule shape (left-recursion → left-assoc; right-recursion → right-assoc), (c) chain depth ≥ 2 (otherwise non-Pratt is cheaper). The cost model decides; the detection is the recogniser's role.

---

## 8. SIMD detection heuristic

The structural-alphabet miner at `crates/ir/src/passes/sets/structural_alphabet.rs` computes the set of bytes the grammar dispatches on at the top-level Alt branches. The simdjson + sonic-rs SIMD primary alphabet is `{ } [ ] : , "` (cardinality 7 incl. quote). lightning-css processes through cssparser's tokenizer (table-driven, not SIMD). The threshold is grammar-derived from FIRST-set density: `simd_threshold_bytes = α / structural_alphabet.cardinality + β / first_set.density`, where α and β are tuned against representative input sizes. CSV's structural alphabet `{ , \n }` (cardinality 2) routes below threshold; JSON's cardinality-7 alphabet routes above for inputs ≥ 1 KB.

---

## 9. Per-grammar recogniser anchor surface

Per `audit/MODULES-2026-05-03.md:1218-1237` the existing recogniser surface includes `operator_chain.rs`, `structural_alphabet.rs`, `delim_scan.rs`, `key_dispatch.rs`. BB.W3c extends them to feed cost-model inputs and emit the corresponding strategy decisions: Pratt fn, SIMD scan kernel, delim-skip dispatch.

---

## 10. Friction-forecast research base

Per `audit/SOTA-2026-05-03.md:33-42` (sonic-rs LazyValue cost ≤ 0.1× full-parse), `audit/SOTA-2026-05-03.md:107-118` (lightningcss Visitor trait shape), and `audit/SOTA-2026-05-03.md:122-123` (lightning-css slice-borrow `'i`-default) the friction surfaces at BB.W4a (lifetime-surfaces.md), BB.W5a (path-macro.md), BB.W5b (visitors.md), BB.W3c (pratt-simd-detection.md) each have a primary-source anchor.
