# SK-V16 S-P0 A4 - Generator Vs Hand-Curated

Date: 2026-05-28.
HEAD: `fc16919d4`.
Axis: A4 generator-vs-hand-curated.
Disposition: REVISE / PRUNE REQUIRED.

## Critical And High Findings

### A4-C1 - Skinny CSS L4 generated runtimes do not round-trip

The seven skinny CSS L4 `generated.rs` files fail delete/regenerate
equivalence. A representative executable check fails:

```sh
cargo test --manifest-path skinny/Cargo.toml -p codegen \
  tests::css_l4_generated_runtimes_reproducible_from_request -- --exact
# FAILED: DifferentFile("generated.rs")
```

### A4-H1 - `generated_real_typed.rs` is stale

```sh
(cd skinny && cargo xtask check-real-typed)
# Error: generated real typed DirectBuild module is stale
# Caused by: generated file `generated_real_typed.rs` differs
```

### A4-H2 - CSS string-literal provider risk remains

`skinny/crates/codegen/src/runtime_generator.rs` emits CSS request-facts runtime
from `CSS_GENERATED_RS`, including `emit_fact_stream`, `emit_full_parse`, and
`CssFullParseSummary`. This is not a typed grammar-derived CSS admission
surface.

## Accept Findings

Root Pattern H runtime generation is checkable at current HEAD:

```sh
cargo xtask check-runtime
# PASS

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
# 67

cargo xtask regen --check --grammar css_l4
# regen --check: clean (9 of 9 grammars matched)
```

## Prune Candidates

1. Retire or regenerate the seven dirty skinny CSS L4 `generated.rs` files.
2. Retire, regenerate, or intrinsically block
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs` with manifest-backed
   proof.
3. Block CSS admission through `CSS_GENERATED_RS`, `emit_fact_stream`,
   `CssFullParseSummary`, `parse_full`, or brace/fact summaries.
4. Add explicit source binding for root `css_l4` runtime projection or an
   equivalent gate tying it to `grammar/css/l4/stylesheet.bbnf` and registry
   sidecars.

CH4 split note: A4 generated/codegen work is split-prone. S-P3 must not merge
the seven CSS generated-file drift row with `generated_real_typed.rs`, provider
rewrite, or Pattern H collapse.
