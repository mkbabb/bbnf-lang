# W5 Pointer Syntax Decision

Date: 2026-05-03
Scope: The `pointer!` macro grammar inference syntax decision per gap E of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:129-144`. Three options enumerate; this document picks one with implementation sketch, error message, friction examples, and rationale.

## §1 Three options

### Option (i) — Mandatory marker

```rust
pointer!(Json, ["a", "b", 1])  // → JsonPath<...> with terminal type known at compile time
```

Every invocation requires the grammar marker. Macro implementation is straightforward: parse the first token as the grammar identifier, look up the grammar's registry, type-resolve subsequent segments.

### Option (ii) — Implicit inference via type ascription

```rust
let p: JsonPath<...> = pointer!["a", "b", 1];  // grammar inferred from context type
```

The macro emits a typed-context-dependent expression that resolves only when the call-site provides a type ascription. If used in untyped context (e.g., `let p = pointer!["a", "b", 1];`), the macro errors with a grammar-context request.

### Option (iii) — Both

```rust
pointer!(Json, ["a", "b", 1])              // explicit grammar marker; always works
let p: JsonPath<...> = pointer!["a", "b", 1];  // typed-context inference; works in typed-context positions
```

The macro accepts both forms; the explicit marker form takes priority; the implicit form requires call-site type context.

## §2 Macro implementation sketch (option iii — recommended)

```rust
// crates/path/src/path_macro.rs (BB.W5a deliverable)
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse::Parse, Expr, Ident, Token, punctuated::Punctuated};

enum PointerInput {
    /// `pointer!(Json, [seg, ...])` — explicit grammar marker
    Explicit { grammar: Ident, segments: Punctuated<Expr, Token![,]> },
    /// `pointer![seg, ...]` — typed-context inference
    Implicit { segments: Punctuated<Expr, Token![,]> },
}

impl Parse for PointerInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) && input.peek2(Token![,]) {
            // Explicit form: pointer!(Grammar, [seg, ...])
            let grammar: Ident = input.parse()?;
            input.parse::<Token![,]>()?;
            // The bracket-expr then becomes a single Expr; parse it as array literal
            let bracket_inner;
            syn::bracketed!(bracket_inner in input);
            let segments = bracket_inner.parse_terminated(Expr::parse, Token![,])?;
            Ok(PointerInput::Explicit { grammar, segments })
        } else {
            // Implicit form: pointer![seg, ...]
            let segments = input.parse_terminated(Expr::parse, Token![,])?;
            Ok(PointerInput::Implicit { segments })
        }
    }
}

#[proc_macro]
pub fn pointer(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as PointerInput);
    
    match parsed {
        PointerInput::Explicit { grammar, segments } => {
            // Compile-time registry lookup against grammar's StructRegistry
            let validated = path_core::validate_explicit(&grammar, &segments)
                .unwrap_or_else(|e| {
                    abort!(e.span, "{}", e.message)  // emit verbatim error
                });
            let registry_ident = format_ident!("{}_REGISTRY", grammar.to_string().to_uppercase());
            quote! {
                ::bbnf::path::TypedPath::<#grammar, _>::from_segments_resolved(
                    &#registry_ident,
                    &[#(#validated),*]
                )
            }.into()
        }
        PointerInput::Implicit { segments } => {
            // Defer grammar resolution to type-ascription context.
            // We emit a TypeInferred wrapper that `From` resolves at use.
            quote! {
                ::bbnf::path::PointerInferred {
                    segments: &[#(#segments),*],
                }
            }.into()
        }
    }
}
```

Companion type:

```rust
// crates/path/src/lib.rs
pub struct PointerInferred<'a> { pub segments: &'a [PointerSegment<'a>] }

impl<'a, G: Grammar, T: Resolves<G>> From<PointerInferred<'a>> for TypedPath<G, T> {
    fn from(p: PointerInferred<'a>) -> Self {
        // run the same resolution logic as the explicit form, with G derived from the destination type
        TypedPath::<G, T>::from_segments_resolved(G::registry(), p.segments)
    }
}
```

The `PointerInferred` wrapper carries the segments unresolved; the `From<PointerInferred<'a>> for TypedPath<G, T>` impl runs resolution at the call site where `G` and `T` are known. This is the chumsky-style type-inference pattern adapted to proc-macros.

## §3 Error messages (verbatim)

All trybuild-tested at `crates/path/tests/error_messages/`.

### Ambiguity error (option ii / iii implicit form, no type context)

```text
error: pointer![] used outside a typed context; cannot infer grammar.
   --> src/main.rs:5:13
    |
  5 |     let p = pointer!["a", "b", 1];
    |             ^^^^^^^^^^^^^^^^^^^^^
    |
help: either annotate the binding with the typed-path destination
    |     let p: JsonPath<JsonValue> = pointer!["a", "b", 1];
    |     |     ^^^^^^^^^^^^^^^^^^^^^
help: or use the explicit marker form
    |     let p = pointer!(Json, ["a", "b", 1]);
    |     |               ^^^^^^
```

### Segment-type mismatch (any form)

```text
error: pointer![] segment 2 of type 'usize' indexes into rule 'object',
       whose values are JsonObject — did you mean a string key?
   --> src/main.rs:8:18
    |
  8 |     pointer!(Json, ["users", 5, "name"])
    |                              ^
    |
note: rule 'users' resolves to JsonObject<{"name": JsonString, "email": JsonString, ...}>
```

### Invalid grammar marker

```text
error: pointer!(<grammar>, ...) — unknown grammar 'JaSon'.
   --> src/main.rs:5:14
    |
  5 |     pointer!(JaSon, ["a", "b"])
    |              ^^^^^
    |
help: known grammars: Json, Bbnf, CssL4, GoogleSheets, Bnf, Csv, Ebnf, CssPretty, Math
```

## §4 Friction examples

### Friction in option (i) — mandatory marker

```rust
// Verbose for hot-path callsites
let names = entries
    .iter()
    .map(|e| pointer!(Json, ["entries", e.idx, "name"]))
    .collect();

// Versus option (iii)
let names: Vec<JsonPath<JsonString>> = entries
    .iter()
    .map(|e| pointer!["entries", e.idx, "name"])  // typed-context infers Json + JsonString
    .collect();
```

Friction: option (i) requires the grammar marker on every callsite; option (iii) infers from the typed `Vec<JsonPath<JsonString>>`.

### Friction in option (ii) — implicit-only

```rust
// Cannot use in expression position without binding
let value = json.get(pointer!["a", "b", 1])?;
//          ^^^ the closure parameter has no type ascription;
//              implicit form fails to infer
```

Friction: option (ii) cannot resolve in function-call argument position when the receiver fn signature does not constrain the path type. Option (iii) accepts the explicit form here:

```rust
let value = json.get(pointer!(Json, ["a", "b", 1]))?;  // works under option (iii)
```

### Friction in option (iii) — combined

```rust
// Cookbook example: all three idioms valid
let p1 = pointer!(Json, ["a", "b", 1]);         // explicit
let p2: JsonPath<i64> = pointer!["a", "b", 1];  // implicit + ascription
fn helper(p: JsonPath<i64>) { ... }
helper(pointer!["a", "b", 1]);                  // implicit + arg-type infers
```

Friction: option (iii) requires teaching grammar authors when to use which form. The cookbook page `docs/cookbook/path-macro.md` mitigates with a decision flowchart (see BB.W5c deliverable).

## §5 Recommended default: option (iii)

Per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:140-144` and the sonic-rs precedent:

1. **sonic-rs precedent**: sonic-rs's `pointer!["a", "b", 1]` uses implicit form because JSON is the only grammar; no marker needed. bbnf-lang has 9 grammars; the marker becomes necessary in some contexts.

2. **chumsky precedent**: chumsky's typed `Parser<I, O, E>` resolves `O` from call-site context (per `audit/SOTA-2026-05-03.md:174-182`). The same pattern works for `pointer!` via the `PointerInferred → TypedPath<G, T>` From impl.

3. **Friction minimization**: option (iii) is a strict superset of (i) and (ii); users with explicit-form preference (e.g., scripting contexts where `let x = pointer![...]` is common) get the marker form; users with typed-context preference (e.g., struct-field initialisers) get the inference form.

4. **Diagnostic clarity**: the verbatim error messages at §3 surface the right alternative for each context; option (iii) does not introduce ambiguity at the macro level, only at the resolution level.

## §6 Implementation sequencing (BB.W5a)

| Milestone | Surface | Action | Gate |
|---|---|---|---|
| W5a M0 | Pre-W5a | Verify `crates/path/src/path_macro/lex.rs` (BA.W2 split) + `crates/path-core/src/lib.rs` (BA.W3) | `test -f crates/path/src/path_macro/lex.rs && test -f crates/path-core/src/lib.rs` |
| W5a M1 | `pointer!` macro extension | Land the explicit-form parsing branch + grammar-marker registry lookup | `cargo nextest run -p path --test pointer_explicit` 100% pass |
| W5a M2 | `pointer!` macro extension | Land the implicit-form parsing branch + `PointerInferred` wrapper + `From` impl | `cargo nextest run -p path --test pointer_implicit` 100% pass |
| W5a M3 | trybuild error fixtures | Land verbatim error messages at `crates/path/tests/error_messages/{ambiguity,segment_mismatch,invalid_grammar}.stderr` | `cargo nextest run -p path --test error_messages` 100% pass with verbatim text |
| W5a M4 | `LazyValue<'a>` runtime evaluator | Each grammar's `<G>Value::pointer(input, &path) -> LazyValue<'_>` lands; `.as_<T>()` materialises | `cargo bench -p bbnf -- lazy_value_twitter` ≤ 50 µs (sonic-rs ratio) |

## §7 BB.W5c cookbook impact

The cookbook page `docs/cookbook/path-macro.md` (BB.W5c deliverable) presents:

- §1 Model: heterogeneous segment types (`&str` keys, `usize` indices); compile-time registry validation; runtime `LazyValue` materialisation.
- §2 Syntax: option (iii) — both forms valid; decision flowchart for choosing.
- §3 Examples: per-grammar callsites for JSON, CSS L4, BBNF, Sheets.
- §4 Errors: the three verbatim error messages from §3 above.
- §5 Troubleshooting: common pitfalls (type-ascription absent; segment-type mismatch; unknown grammar).

The cookbook is gated by BB.W5c M1; trybuild fixtures verify the verbatim error text matches the cookbook examples character-for-character.
