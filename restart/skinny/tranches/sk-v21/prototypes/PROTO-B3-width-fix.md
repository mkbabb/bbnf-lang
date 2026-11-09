# PROTO-B3 — the upstream width-preserving lowering fix (P6-v2)

Landed-in-worktree fix, not master. Worktree `wf_bdd2b86b-8a6-6`. All
evidence reproducible from the diffs below plus
`cargo test -p bbnf --test p6_hm_spike`.

## The question and the answer

**Q.** Implement the minimal generic width-preserving lowering fix so the
22 `Nu8` discriminant tags across json + google-sheets land `U8` (not the
`I64` HM default), with zero `Span` defaults and zero grammar-named code.

**A — DONE.** A **12-code-line** helper in a **single file**
(`crates/core/src/lower/expression/wrap.rs`, +45/−15) recovers the numeric
width suffix from the parent-factor source. Re-running the P6 inference
core over both grammars: **all 22 `Nu8` tags land `U8`**, **0 land `I64`**,
**span_default = 0**, **failed = 0**. The fix is a same-wave
substrate+consumer pair and is TW5-schedulable. Confidence 90%.

## Root cause — corrected from P6's citation

P6 cited three loci (`literals.rs:7,17`; `wrap.rs:589-606`; dropped FnCall
args). Tracing the *actual* struct-direct path the corpus exercises
narrows it to **one live locus, and it is not `literals.rs`**:

- `int_lit = /0[xX][0-9a-fA-F]+\w*|[0-9]+\w*/ -> i64`
  (`grammar/bbnf/expressions.bbnf:6`) — the regex `\w*` **does** match the
  `u8` suffix, so `0u8` is one leaf match. The `-> i64` projection parses
  the value and **deposits a typed `BbnfValue::Int(0)` leaf**
  (`lower/value_expr/mod.rs:80`: `Int(v) => MapExpr::IntLit(v)`). The
  suffix is dropped at the value conversion.
- **The typed `Int`/`Float` leaf carries no byte span.**
  `compute_byte_span` returns `None` for every scalar-payload leaf
  (`runtime/bbnf/view.rs:141`, the `_ => None` arm — only `Span` and
  `Compound` get bounds). So `deep_unwrap_value(value_expr).span_text()`
  at the `wrap.rs` recovery site is the **empty string**, not `"0u8"` and
  not P6's reported `"0"`. The existing recovery therefore yielded
  `return_type = None` → `generate.rs` `unwrap_or(Span)`.
- `literals.rs:7,17` (`parse_int_literal`/`parse_float_literal` discarding
  `_suffix`) is a real width-drop but on the **Span-leaf atom fallback**
  path (`atom.rs:163,248`), which these grammars' typed `int_lit` leaves
  never take. It needs no change: `MapExpr::IntLit(i64)` has no width
  field, so width can only ever flow through `return_type` — and the leaf
  the fallback path *does* have a span for is already handled.

So the destroyed datum is not the parsed value — it is the **source span**
of the typed numeric leaf. The suffix survives only in the enclosing
`mapped_factor`'s source (`wrap.rs:260`, `parent_span_for_type =
node.span_text()`), which is exactly what the fix reads.

## The fix (inline)

Single file, `crates/core/src/lower/expression/wrap.rs`. A new helper
mirroring the existing `recover_type_name_from_parent`:

```rust
/// Recover a numeric-literal width suffix (`u8`, `u32`, `f32`, …) from
/// the enclosing `mapped_factor` source when the mapped value is a
/// bare numeric literal.
fn recover_numeric_suffix_from_parent(source: &str) -> Option<&str> {
    let arrow = find_unquoted(source, "->").or_else(|| find_unquoted(source, "=>"))?;
    let after = source[arrow + 2..].trim_start();
    let token = after
        .split(|c: char| c.is_whitespace() || matches!(c, ';' | '|' | ':'))
        .next()?
        .trim();
    if !token
        .as_bytes()
        .first()
        .is_some_and(|b| b.is_ascii_digit() || *b == b'.')
    {
        return None;
    }
    let (_digits, suffix) = split_numeric_suffix(token);
    if suffix.is_empty() { None } else { Some(suffix) }
}
```

Wired into the numeric-suffix recovery block as a fallback after the
leaf-span attempt (which still serves the Span-leaf atom path):

```rust
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let t = leaf.span_text().trim_start();
        let leaf_suffix = matches!(t.as_bytes().first(), Some(b'0'..=b'9') | Some(b'.'))
            .then(|| split_numeric_suffix(t).1)
            .filter(|s| !s.is_empty());
        let suffix = leaf_suffix.or_else(|| recover_numeric_suffix_from_parent(parent_source))?;
        Some(TypeDesc::from_scalar_name(suffix).unwrap_or_else(|| {
            let sid = ctx.strings.intern(suffix);
            TypeDesc::Named(sid)
        }))
    });
```

**Generic, not grammar-named.** No rule name (`error_literal`, `null`,
`compare_op`, …) appears anywhere. Width resolution is `from_scalar_name`,
covering `u8/u16/u32/u64/i8/i16/i32/i64/f32/f64` uniformly; an unknown
suffix interns as `Named`, not `Span`. Both `find_unquoted` and
`split_numeric_suffix` were already in scope — zero new imports.

**Size.** +45 / −15 (net +30) in **1 file**; the new helper is 12 code
lines, the wired block a net simplification of the prior one.

## Results (P6 inference core over both grammars)

All 6 tests pass. Single cargo invocation, `--profile ax-iter`, finished
0.02s; host loadavg ~22 (5-min) at run — immaterial, every measurement is
a deterministic pass/fail type count, not a throughput number.

```
json:           u8=1   i64=0   span_default=0   failed=0   (checked=5  synth=0)
google-sheets:  u8=21  i64=0   span_default=0   failed=0   (checked=28 synth=0)
TOTAL           u8=22  i64=0   span_default=0   failed=0   (checked=33 synth=0)
```

The 22 `Nu8` tags — json `fn[0]` (`null -> 0u8`) and google-sheets
`fn[4..12,16..27]` (error_literal ×9, compare_op ×6, add/mul/unary ×6) —
each land `U8` via the `Some(U8) => check` path: `check(IntLit, U8)`
unifies the integer-literal var with `Con(U8)` and confirms.

The pre-fix picture is **inverted, not merely nudged**. P6 reported
`checked=11 synth=22` — the 22 tags synthesising to the `I64` HM default.
Now **every** `->` map resolves through `check` to a concrete width:
`checked=33 synth=0`, zero `I64`, zero `Span`. Correct-width end-to-end —
the exact outcome P6 flagged as blocked (`PROTO-P6.md:182-183`).

(A `Note: sub-variant type collision …` line prints to stderr during
google-sheets grammar codegen; it is pre-existing, unrelated to this fix,
and does not affect the type counts or the passing gate.)

## TW5-schedulability — a same-wave substrate+consumer pair

CRIT-P6 gap #3 requires "no substrate lands without its consumer in the
same wave." This fix satisfies it **trivially, because the consumer
predates the substrate**:

- **Substrate** — the lowering fix: numeric width suffix →
  `FnDescriptor::Expr.return_type = Some(U8)`.
- **Consumer** — the payload-width sizing that *already reads*
  `return_type` (the `return_type.unwrap_or(Span)` at
  `crates/ir/src/passes/types/generate.rs`). It exists today; the fix
  merely feeds it the correct value instead of `None`. P6's `check`/`synth`
  is the type-layer consumer that *confirms* the width (`Some(ann) =>
  check` → `unify(IntVar, U8)`).

Both the substrate and its consumer land and are gated in TW5. No orphan
substrate; the width fix is **strictly upstream** of an already-live
reader. (The sibling defects P6 flagged — dropped `FnCall` args, the
`@host` param edge — are **out of scope for the width fix** and belong with
the `@host fn` consumer in TW6, per CRIT-P6 gap #3.)

## The corrected TW5 close-gate assertion

CRIT-P6 gap #2: the current gate ("9 grammars type end-to-end with zero
`unwrap_or(Span)`") is width-blind — it passes while the 22 tags land
`I64`. "Zero Span defaults" is necessary, not sufficient. The gate must
also assert correct width:

> **TW5 width-correct close-gate.** For every corpus grammar, each
> `FnDescriptor::Expr` resolves to a non-`Span` type (`span_default == 0`),
> **and** every numeric-literal map carrying a source width suffix
> (`Nu8`/`Nu32`/…) resolves to `return_type == Some(TypeDesc::<width>)` —
> the width the grammar wrote, never the `I64` HM default. Concretely, as
> asserted in `p6_hm_spike::p6_v2_nu8_widths_land_u8`:
>
> ```rust
> assert_eq!(u8_tags_json + u8_tags_gs, 22); // all Nu8 tags land U8
> assert_eq!(i64_tags, 0);                   // none default to I64
> assert_eq!(span_default, 0);               // unwrap_or(Span) stays dead
> assert_eq!(failed, 0);                     // no typecheck failures
> ```

Necessary-and-sufficient: `span_default == 0` kills the old lie;
`i64_tags == 0` ∧ `u8_tags == 22` proves the width actually landed.
