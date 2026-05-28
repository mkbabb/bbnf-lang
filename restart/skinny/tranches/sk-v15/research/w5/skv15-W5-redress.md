# SK-V15 W5 Redress: CSS Typed Visitor Provider

Status: ADMIT-W5.
Wave: W5 CSS typed Value provider.
Implementation commit: `d80702388`.

## Result

W5 adds a generated CSS L4 visitor provider to the root typed runtime without
claiming CSS benchmark admission or retiring the inherited skinny fact-stream
proof.

The accepted implementation differs from PLAN-V1 in one invariant-preserving
detail: `CssVisitor` and `visit_document` are generated inside the existing
`document.rs` module instead of a new `visitor.rs` module. A separate module
would raise Pattern H from 67 to 68 files, violating the cycle close invariant.

## Landed Surface

- `xtask/src/regen_css.rs` emits `CssVisitor<'p>`, `visit_document`, and typed
  recursive walkers from the generated CSS document module.
- `xtask/runtime-projections/css_l4.toml` exports `CssVisitor`,
  `visit_document`, and `CssRule` through the generated CSS L4 module surface.
- `crates/core/src/runtime/css_l4/{document.rs,mod.rs}` and
  `crates/core/src/runtime/mod.rs` expose the provider as
  `runtime::css_l4::visit_document` and `runtime::visit_css_document`.
- `crates/core/tests/typed_accessor_surface.rs` proves visitor traversal reaches
  document-owned typed rules, declarations, color values, and percentage
  dimensions.

## Evidence

Executed on Apple M5 Max / aarch64 with native CPU flags:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- check-runtime
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test typed_accessor_surface
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test css_l4_substrate
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test css_l4_parity
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test projection_totality
git diff --check
cargo fmt --check --package xtask
grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

Observed: all executable checks pass; lock count remains 16; Pattern H remains
67.

## Routed Remainder

W5 consumes the following dependency rows only as replacement-provider supply:

- `DEP-W6-CSS-GENERATED-RS`
- `DEP-W6-CSS-SUMMARY-FACT-STREAM`
- `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`
- `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM`

The old CSS proof remains diagnostic until W6. The following symbols still
exist by design and are not W5 failures:

- `CSS_GENERATED_RS`
- `CssFullParseSummary`
- `Result<String, CssFactError>`
- `emit_fact_stream`
- `LegacyPath`
- `LegacySegment`

W6 owns their retirement plus same-workload retiming. W5 makes no CSS SOTA
claim and no row-admission claim.
