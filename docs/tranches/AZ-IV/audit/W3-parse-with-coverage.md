# AZ-IV.W3 Parse-With Coverage Matrix

Per-grammar `parse_with(input, &path)` entry-point coverage. Each row
records the public API signature, the fixture path used in the
in-module smoke test, the leaf type the path resolves to, and the
lazy-vs-eager parity claim.

After the W3.7 entry rewrite, all four entry points share one shape:
each constructs a `PathCursor<P>` over the supplied `TypedPath<G, T>`
schema, wires the cursor's decision-lookup closure to the
codegen-emitted `__path_plan::lookup(rule_id, segment_kind)` static
search, and threads the cursor through the cursor-aware generated
dispatcher (`parse_<Grammar>_<entry>`). The dispatcher's emitted
control flow consults the cursor at every shape-decision site;
subtrees the path does not visit are byte-skipped and never push
records into the `<Grammar>StructBuilder`. After the dispatcher
returns, the builder is finalised against `input` and the leaf is
projected through the document's `get<T: <Grammar>PathQuery>` surface.

## API Surface

| Grammar       | Entry point                                                 | Bound                           |
| ---           | ---                                                         | ---                             |
| JSON          | `runtime::json::parse_with::<T>(&str, &TypedPath<Json, T>) -> Option<T>`        | `T: JsonPathQuery`    |
| CSS L4        | `runtime::css_l4::parse_with::<T>(&str, &TypedPath<CssL4, T>) -> Option<T>`     | `T: CssPathQuery`     |
| Google Sheets | `runtime::google_sheets::parse_with::<T>(&str, &TypedPath<Sheets, T>) -> Option<T>` | `T: SheetsPathQuery` |
| BBNF          | `runtime::bbnf::parse_with::<T>(&str, &TypedPath<Bbnf, T>) -> Option<T>`        | `T: BbnfPathQuery`    |

The bound is grammar-local because the document's typed projection
trait is grammar-local (the `*PathQuery` family). The four entry
points live as siblings under
`crates/core/src/runtime/<grammar>/parse_with.rs` and re-export from
the grammar's `mod.rs` through `pub use parse_with::parse_with;`.

## Cursor Wiring (Post W3.7)

Every entry point routes through the executor with the same closure
shape; the parse-fn closure body now invokes the cursor-threaded
dispatcher directly:

```rust
PathExecutor::execute(
    input,
    path,
    |rule_id, kind, _idx| {
        __path_plan::lookup(rule_id, kind)
            .map(|e| e.decision)
            .unwrap_or(Decision::ParseFully)
    },
    |src, cursor| {
        let mut state = __shape_support_<Grammar>::ScanState::new();
        let mut builder = <Grammar>StructBuilder::new();
        let mut pos: usize = 0;
        parse_<Grammar>_<entry>(src.as_bytes(), &mut pos, &mut state,
                                 &mut builder, cursor).ok()?;
        let doc = builder.finalise(src);
        // lower typed segments to legacy borrowed alphabet
        let mut legacy = Vec::with_capacity(path.len());
        for owned in path.owned_segments() {
            legacy.push(lower(&owned.as_borrowed())?);
        }
        doc.get::<T>(LegacyPath::new(&legacy))
    },
)
```

The cursor is no longer constructed-and-discarded: the dispatcher
consumes it inside the recursive descent, and at every shape-decision
site (Array / Object / Wrap Alt-dispatch / Flat positional Seq /
cross-shape Ref) the cursor's `decide(rule_id)` consult selects
`ParseFully` / `ParseUntil(idx)` / `Skip` and the emitted control
flow follows. `Skip` invokes the per-shape byte-range scanner emitted
by W3.6; the unvisited subtree's bytes advance `*pos` without any
record landing in the builder.

## Fixture Smoke Coverage

Each per-grammar entry point exercises a fixture path inside the
module's `#[cfg(test)] mod tests`. The smoke tests live in the same
crate as the entry points (compile evidence per the W3 hard gate);
broader cross-grammar coverage lives at `crates/core/tests/parse_with_*.rs`.

| Grammar       | Fixture input                       | Path                                   | Leaf type            | Test                                       |
| ---           | ---                                 | ---                                    | ---                  | ---                                        |
| JSON          | `{"title":"hi"}`                    | `Field("title")`                       | `&str`               | `parse_with_resolves_string_leaf`          |
| JSON          | `{"count":42}`                      | `Field("count")`                       | `f64`                | `parse_with_resolves_number_leaf`          |
| JSON          | `{"title":"hi"}`                    | `Field("absent")`                      | `&str` (absent)      | `parse_with_returns_none_on_missing_field` |
| CSS L4        | `a { color: red; }`                 | `Index(0), Index(0)`                   | `&str`               | `parse_with_parity_against_eager`          |
| CSS L4        | `a { color: red; }`                 | `Index(99), Index(0)` (out-of-bounds)  | `&str` (absent)      | `parse_with_returns_none_on_missing_rule`  |
| Google Sheets | `=42`                               | `Index(0), Index(0)`                   | `f64`                | `parse_with_parity_against_eager`          |
| Google Sheets | `not a formula @@@`                 | `[]`                                   | `f64` (parse error)  | `parse_with_returns_none_on_invalid_input` |
| BBNF          | `a = b ;\n`                         | `[]`                                   | `BbnfValue<'_>`      | `parse_with_resolves_root_value`           |
| BBNF          | `@@@ not bbnf @@@`                  | `[]`                                   | `BbnfValue<'_>` (parse error) | `parse_with_returns_none_on_invalid_input` |

## Cross-Crate Coverage (W3.7 Lazy-Error-Elision Rows)

| File                                          | Row                                       | Contract                                                                                  |
| ---                                           | ---                                       | ---                                                                                       |
| `crates/core/tests/parse_with_json.rs`        | `happy_path_parity_against_eager`         | lazy + eager agree on well-formed input                                                   |
| `crates/core/tests/parse_with_json.rs`        | `lazy_error_elision_after_path_reach`     | malformed bytes past the path's reach: lazy returns `Some("hi")`; eager errors            |
| `crates/core/tests/parse_with_json.rs`        | `out_of_bounds_path_returns_none`         | both lanes return `None` when the path walks past the document                            |
| `crates/core/tests/parse_with_css_l4.rs`      | `happy_path_parity_against_eager`         | lazy + eager agree on `a { color: red; }`                                                 |
| `crates/core/tests/parse_with_css_l4.rs`      | `lazy_error_elision_after_path_reach`     | trailing `@@@`: lazy returns the property; eager errors                                   |
| `crates/core/tests/parse_with_css_l4.rs`      | `out_of_bounds_path_returns_none`         | both lanes return `None` on out-of-bounds index                                           |
| `crates/core/tests/parse_with_google_sheets.rs` | `happy_path_parity_against_eager`       | lazy + eager agree on `=42`                                                                |
| `crates/core/tests/parse_with_google_sheets.rs` | `lazy_error_elision_after_path_reach`   | trailing `@@@`: lazy returns 42.0; eager errors                                           |
| `crates/core/tests/parse_with_google_sheets.rs` | `invalid_input_returns_none`            | input is garbage from byte 0; lazy returns `None`                                          |
| `crates/core/tests/parse_with_bbnf.rs`        | `happy_path_resolves_root_value`          | lazy resolves root identity on well-formed input                                          |
| `crates/core/tests/parse_with_bbnf.rs`        | `lazy_error_elision_after_path_reach`     | trailing garbage: lazy returns root; eager errors                                         |
| `crates/core/tests/parse_with_bbnf.rs`        | `invalid_input_returns_none`              | input is garbage from byte 0; lazy returns `None`                                          |

## Lazy + Eager Same `Option<T>` Semantics

The W3 spec invariant #9 requires `parse_with` and
`parse(input)?.get(path)` to share `Option<T>` semantics on inputs the
eager lane resolves cleanly. The CSS / Sheets / JSON parity rows
exercise the equivalence directly. The lazy-error-elision rows
demonstrate the divergence on malformed-tail inputs: lazy returns
`Some(leaf)` because the cursor's skip-scan never visits the
malformed bytes; eager returns `Err` because it materialises the
entire document.

## Path Lowering

The new typed `PathSegment` alphabet (`crate::path::ir::PathSegment`)
is a superset of the legacy borrowed alphabet
(`crate::runtime::path::PathSegment`). The lowering applied at the
projection step:

| Typed segment      | Legacy projection         | Notes                                                         |
| ---                | ---                       | ---                                                           |
| `Field(s)`         | `Field(s)`                | identity                                                      |
| `Index(i)`         | `Index(i)`                | identity                                                      |
| `VariantName(s)`   | `Field(s)`                | document walker reads variant via field-step today            |
| `Wildcard`         | bails (`None`)            | wildcard execution belongs to a future stream-shaped surface  |

`Wildcard` is unrepresentable in the document's `get` projection
because `get` resolves to a single leaf, not a stream. The lazy
wildcard lane will route through the cursor's W3.6 dispatcher carve
plus a stream-shaped projection, not through the entry point's
`doc.get` step.

## Known Misses

- Wildcard step in the entry point: bails (`None`) by lowering. Lazy
  iter lane handed to a future tranche.
- `VariantName` step lowers to `Field` for the document walker. Full
  enum-aware variant resolution belongs to W4's typed-step executor.
- `Skip` decisions emit byte-range scanners only for the four
  shape families that have an unambiguous open/close delimiter
  pair: Object (brace-balanced), Array (bracket-balanced), String
  (quote-with-escape via `simd_scan::quoted_string_simd_body`),
  scalar (regex-scan adapter). Pratt rules without a delimiter pair
  route through `ParseFully` only — the path-plan walker doesn't
  emit `Skip` for them. Acceptable per the research finding; not a
  blocker.

## Evidence

- Files: `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs`
- mod.rs carves: `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/mod.rs`
- Tests: `cargo nextest run -p bbnf --profile ax-iter -E 'test(/parse_with::/) | test(/parse_with_/)'`
- Build: `cargo build -p bbnf --profile ax-iter` (closes once W3.6 lands)
- Negative-fixture contract: `docs/tranches/AZ-IV/audit/W3-error-elision-contract.txt`
