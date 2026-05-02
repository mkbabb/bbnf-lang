# AZ-IV.W3.2 Parse-With Coverage Matrix

Per-grammar `parse_with(input, &path)` entry-point coverage produced by
the W3.2 sub-unit. Each row records the public API signature, the
fixture path used in the in-module smoke test, the leaf type the path
resolves to, and the lazy-vs-eager parity claim.

The four entry points share one shape: each constructs a
`PathCursor<P>` over the supplied `TypedPath<G, T>` schema, wires the
cursor's decision-lookup closure to the codegen-emitted
`__path_plan::lookup(rule_id, segment_kind)` static-search, and hands
the cursor to `PathExecutor::execute`. The executor's parse-fn closure
runs the existing eager `*Parser::parse(input)` and projects the leaf
through the document's existing `get<T: <Grammar>PathQuery>` surface.
This is the entry-point dispatch surface required by the W3 spec §2;
truly-lazy descent through the generated parse functions threads the
cursor down through `parse_<rule>(state, &mut PathCursor)` once W3.3's
generated-code carve lands.

## API Surface

| Grammar       | Entry point                                                 | Bound                           |
| ---           | ---                                                         | ---                             |
| JSON          | `runtime::json::parse_with::<T>(&str, &TypedPath<Json, T>) -> Option<T>`        | `T: JsonPathQuery`    |
| CSS L4        | `runtime::css_l4::parse_with::<T>(&str, &TypedPath<CssL4, T>) -> Option<T>`     | `T: CssPathQuery`     |
| Google Sheets | `runtime::google_sheets::parse_with::<T>(&str, &TypedPath<Sheets, T>) -> Option<T>` | `T: SheetsPathQuery` |
| BBNF          | `runtime::bbnf::parse_with::<T>(&str, &TypedPath<Bbnf, T>) -> Option<T>`        | `T: BbnfPathQuery`    |

The bound is grammar-local because the document's typed projection
trait is grammar-local (the `*PathQuery` family). The four entry
points live as siblings under `crates/core/src/runtime/<grammar>/parse_with.rs`
and re-export from the grammar's `mod.rs` through `pub use parse_with::parse_with;`.

## Cursor Wiring

Every entry point constructs the cursor identically:

```rust
PathExecutor::execute(
    input,
    path,
    |rule_id, kind, _idx| {
        __path_plan::lookup(rule_id, kind)
            .map(|e| e.decision)
            .unwrap_or(Decision::ParseFully)
    },
    |src, _cursor| { /* eager parse + lowered legacy-path get */ },
)
```

The closure unwraps the `Option<&PathPlanEntry>` per the cursor.rs
contract: a `None` from `lookup` means no plan row for the
`(rule_id, segment_kind)` pair, and the cursor's documented fall-back
is `Decision::ParseFully`. The cursor is threaded into the parse-fn
closure even though the eager parser does not yet consume it; the
threading exists so the W3.3 follow-on can carve the cursor through
generated `parse_<rule>` without touching the per-grammar entry-point
shape.

## Fixture Smoke Coverage

Each per-grammar entry point exercises a fixture path inside the
module's `#[cfg(test)] mod tests`. The smoke tests live in the same
crate as the entry points (compile evidence per the W3 hard gate);
broader cross-grammar coverage lives at W3.4.

| Grammar       | Fixture input                       | Path                                   | Leaf type            | Test             |
| ---           | ---                                 | ---                                    | ---                  | ---              |
| JSON          | `{"title":"hi"}`                    | `Field("title")`                       | `&str`               | `parse_with_resolves_string_leaf`     |
| JSON          | `{"count":42}`                      | `Field("count")`                       | `f64`                | `parse_with_resolves_number_leaf`     |
| JSON          | `{"title":"hi"}`                    | `Field("absent")`                      | `&str` (absent)      | `parse_with_returns_none_on_missing_field` |
| CSS L4        | `a { color: red; }`                 | `Index(0), Index(0)`                   | `&str`               | `parse_with_parity_against_eager`     |
| CSS L4        | `a { color: red; }`                 | `Index(99), Index(0)` (out-of-bounds)  | `&str` (absent)      | `parse_with_returns_none_on_missing_rule` |
| Google Sheets | `=42`                               | `Index(0), Index(0)`                   | `f64`                | `parse_with_parity_against_eager`     |
| Google Sheets | `not a formula @@@`                 | `[]`                                   | `f64` (parse error)  | `parse_with_returns_none_on_invalid_input` |
| BBNF          | `a = b ;\n`                         | `[]`                                   | `BbnfValue<'_>`      | `parse_with_resolves_root_value`      |
| BBNF          | `@@@ not bbnf @@@`                  | `[]`                                   | `BbnfValue<'_>` (parse error) | `parse_with_returns_none_on_invalid_input` |

All nine smoke tests pass under `cargo nextest run -p bbnf --profile ax-iter -E 'test(/parse_with::/)'`.

## Lazy + Eager Same `Option<T>` Semantics

The W3 spec invariant #9 requires `parse_with` and `parse(input)?.get(path)`
to share `Option<T>` semantics. Today's wiring satisfies the invariant
trivially: `parse_with` runs eager parse and projects through
`doc.get`. The CSS and Sheets `parity_against_eager` smoke tests
exercise the equivalence directly.

When W3.3 threads the cursor through generated `parse_<rule>`, the
parse-fn closure's body changes — but the entry-point shape, the bound,
and the `Option<T>` semantic do not. Lazy mode at that point silently
elides parse errors past the path's reach (the W3 contract);
`parse_with` returns `Some(leaf)` where `parse(input)?.get(path)` would
fail with a parse error past the path's terminal. The smoke tests will
add a "lazy beats eager on malformed-tail input" row at W3.4.

## Path Lowering

The new typed `PathSegment` alphabet
(`crate::path::ir::PathSegment`) is a superset of the legacy
borrowed alphabet (`crate::runtime::path::PathSegment`). The lowering:

| Typed segment      | Legacy projection         | Notes                                                         |
| ---                | ---                       | ---                                                           |
| `Field(s)`         | `Field(s)`                | identity                                                      |
| `Index(i)`         | `Index(i)`                | identity                                                      |
| `VariantName(s)`   | `Field(s)`                | document walker reads variant via field-step today            |
| `Wildcard`         | bails (`None`)            | wildcard execution is W3.5 lazy-iter lane                     |

`Wildcard` is unrepresentable in the eager fallback because the
document's `get<T>` resolves to a single leaf, not a stream. The lazy
wildcard lane (W2.5 `WildcardIter` + W3.5 `parse_with` integration)
threads through the cursor's W3.3 carve, not the entry point's
projection step.

## Known Misses

- Truly-lazy descent: cursor is constructed and decision plan is
  consulted, but the parse loop today calls eager `*Parser::parse`. The
  cursor flows through but is not yet acted upon by the underlying
  recursive descent. W3.3 owns that carve.
- Wildcard step in the entry point: bails (`None`) by lowering. Lazy
  iter lane handed to W3.5.
- `VariantName` step lowers to `Field` for the document walker. Full
  enum-aware variant resolution is W4 typed-step executor scope.
- The eager parse can fail (returning `None` from `parse_with`) when
  the lazy contract would silently elide errors past the path's reach.
  The negative-fixture test for the lazy-error-elision contract lands
  at W3.4 once W3.3's cursor-threaded parse loop exists.

## Evidence

- Files: `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs`
- mod.rs carves: `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/mod.rs`
- Tests: `cargo nextest run -p bbnf --profile ax-iter -E 'test(/parse_with::/)'` → 9 passed, 0 failed
- Build: `cargo build -p bbnf --profile ax-iter` → clean
