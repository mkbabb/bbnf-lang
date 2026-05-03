# BB.W5a — `pointer!` Macro + LazyValue

**Thesis** Hereupon the `pointer!(Json, ["a","b",1])` mandatory-marker form and `pointer!["a","b",1]` typed-context inference form land per the syntax decision at `docs/tranches/BB/audit/W5-pointer-syntax-decision.md`; runtime evaluation produces `LazyValue<'a>` borrowed views; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand. **Closer-gate** BB-G7 met; `cargo nextest run -p path --test pointer_macro` 100% pass; `cargo bench -p bbnf -- lazy_value_twitter` ≤ 50 µs (sonic-rs ratio); verbatim error messages committed as trybuild fixtures.

## §1 Deliverable

W5a is the first of three W5 sub-waves. The path-API surface lands here; the visitor surface lands at W5b; the cookbooks + diagnostic gates land at W5c.

The `pointer!` macro per the syntax decision (option iii — both forms):

```rust
// Explicit marker (always works)
let p = pointer!(Json, ["a", "b", 1]);   // → TypedPath<Json, ...>

// Implicit (typed-context inference)
let p: JsonPath<JsonString> = pointer!["a", "b", 1];   // grammar inferred from binding type
```

The macro lands at `crates/path/src/path_macro.rs`; the lex/lower/validate logic lives at `crates/path-core/` (BA→BB.C4 carry); the `LazyValue<'a>` runtime evaluator emits per-grammar at xtask regen time.

The `LazyValue<'a>` per `audit/SOTA-2026-05-03.md:33-42`:

```rust
pub struct LazyValue<'a> {
    slice: &'a [u8],
    type_tag: ValueType,
}

impl<'a> LazyValue<'a> {
    pub fn as_str(&self) -> Result<&'a str, ParseErr> { ... }
    pub fn as_i64(&self) -> Result<i64, ParseErr> { ... }
    pub fn as_<T: FromLazyValue<'a>>(&self) -> Result<T, ParseErr> { ... }
    pub fn owned(self) -> OwnedLazyValue { ... }
}
```

Sonic-rs ratio target: `LazyValue::pointer(input, &path)` ≤ 0.1× full-parse cost. The bench at M2 verifies on twitter.json (≤ 50 µs vs `parse(twitter)` ≤ 400 µs).

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W5a verification | Verify W4b cookbook lands; `crates/path/src/path_macro.rs` exists per BA.W2 split; `crates/path-core/src/lib.rs` exists per BA.W3 | `test -f crates/path/src/path_macro/lex.rs && test -f crates/path-core/src/lib.rs && test -f docs/cookbook/lifetime-surfaces.md` | W5a prerequisites in place. |
| M1 | `pointer!` macro extension (explicit form) | Land the explicit-form parsing branch + grammar-marker registry lookup | `cargo nextest run -p path --test pointer_explicit` 100% pass; `cargo expand -p test_pointer` shows resolved typed path | Explicit form lands. |
| M2 | `pointer!` macro extension (implicit form) | Land the implicit-form parsing branch + `PointerInferred` wrapper + `From` impl | `cargo nextest run -p path --test pointer_implicit` 100% pass | Implicit form lands. |
| M3 | trybuild error fixtures | Land verbatim error messages at `crates/path/tests/error_messages/{ambiguity,segment_mismatch,invalid_grammar}.stderr` per the syntax decision §3 | `cargo nextest run -p path --test error_messages` 100% pass with verbatim text | Verbatim error messages committed. |
| M4 | `LazyValue<'a>` runtime evaluator | Each grammar's `<G>Value::pointer(input, &path) -> LazyValue<'_>` lands; `.as_<T>()` materialises | `cargo bench -p bbnf -- lazy_value_twitter` ≤ 50 µs; `cargo nextest run -p bbnf --test lazy_value_materialisation` 100% pass | LazyValue surface is sonic-class. |
| M5 | Per-grammar coverage artefact | Land `docs/tranches/BB/audit/W5a-pointer-macro-coverage.md` recording per-grammar pointer test coverage | `test -f docs/tranches/BB/audit/W5a-pointer-macro-coverage.md` | Per-grammar coverage artefact lands. |

## §3 Closer gate

```sh
cargo nextest run -p path --test pointer_macro --profile ax-iter                   # 100% pass
cargo nextest run -p path --test error_messages --profile ax-iter                  # 100% pass with verbatim text
cargo nextest run -p bbnf --test lazy_value_materialisation --profile ax-iter      # 100% pass
cargo bench -p bbnf -- lazy_value_twitter --profile ax-iter                        # ≤ 50 µs
test -f crates/path/src/path_macro.rs                                                 # macro lands
test -f docs/tranches/BB/audit/W5a-pointer-macro-coverage.md                         # artefact lands
```

## §4 Invariants

§I1. **Lock 7** — `crates/path/` consolidation; the `pointer!` macro lives at `crates/path/src/path_macro.rs`; lex/lower/validate at `crates/path-core/`.
§I2. **Lock 9** — slice-borrow primary; LazyValue is borrowing wrapper.
§I3. **Lock 8** — surpass sonic-rs; LazyValue ≤ 0.1× full-parse cost (matching sonic-rs ratio).
§I4. **G05-8** of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:31` + surgery 35 — `pointer!` produces typed terminal paths without turbofish on unambiguous paths; wildcard returns typed iterators (deferred); invalid paths include grammar-aware diagnostics.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| `LazyValue` materialisation cost regresses sonic-rs ratio | Medium | LazyValue carries slice + type tag; `.as_<T>()` re-parses only the slice; parse bounded by slice length not document length. |
| Macro `From<PointerInferred> for TypedPath<G, T>` requires complex GAT; doesn't compile | Low | The implicit form's resolution happens in a `From` impl; if GAT complexity is excessive, fall back to a `Path::infer<G, T>(&[seg]) -> TypedPath<G, T>` standalone fn (no GAT needed). |

## §6 Cross-references

- **BB-G gates closing**: BB-G7.
- **Carry-tags consumed**: BA→BB.C4 (path-core); BA→BB.C1 (direct-to-struct codegen).
- **Carry-tags produced**: precursor to BB→BC.C3 (visitor at W5b).
- **Preceding wave**: BB.W4b.
- **Following wave**: BB.W5b.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo check -p bbnf -p path --profile ax-iter` | ≤ 13 s |
| `cargo nextest run -p path --test pointer_macro --profile ax-iter` | ≤ 18 s |
| `cargo nextest run -p bbnf --test lazy_value_materialisation --profile ax-iter` | ≤ 15 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W5a-pointer-macro-coverage.md` | `docs/tranches/BB/audit/` | Per-grammar `pointer!` macro test coverage |
| `W5a-lazyvalue-ratio.md` | same | Per-grammar LazyValue cost vs full-parse cost |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L7 + L8 + L9 honoured |
| Lane 4 | LazyValue ≤ 0.1× full-parse (sonic-rs ratio) |
| Lane 7 | F07-1 verbatim error messages committed |
| Lane 8 | BA→BB.C4 closes |
