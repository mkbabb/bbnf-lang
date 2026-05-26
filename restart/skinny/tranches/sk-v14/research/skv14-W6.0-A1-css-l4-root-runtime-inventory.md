# SK-V14 W6.0 A1: CSS L4 Root Runtime Inventory

Date: 2026-05-26.
Scope: inventory the root `css_l4` runtime collapse surface at HEAD before W6.0 redress.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. W6.0 is explicitly the CSS L4 root-runtime collapse sub-wave. `SPEC.md:932-984` names `crates/core/src/runtime/css_l4/` as W6.0 scope, requires W6.0 before W6.1-W6.8, and adds the destructive CSS gate `rm -rf crates/core/src/runtime/css_l4 && cargo xtask regen-css && git diff --exit-code -- crates/core/src/runtime/css_l4`.
2. HEAD still has seven CSS L4 root runtime files: `arena.rs`, `builder.rs`, `document.rs`, `mod.rs`, `parse_with.rs`, `value.rs`, and `view.rs`. The W6 inventory count is therefore `css_l4=7`, matching `SPEC.md:954` and the W6.0 row at `SPEC.md:962`.
3. The CSS L4 runtime is not a boilerplate wrapper. `crates/core/src/runtime/css_l4/mod.rs:59-78` exports a rich public API: typed dimensions, colors, selectors, declarations, rule aggregates, arena ids, document/view types, and `parse_with`.
4. The generated parser is bound to that public surface. `Cargo.toml:49` maps `CssL4Parser` to `crate::runtime::css_l4::{CssStructBuilder,CssDocument}`, and `crates/core/src/grammar/generated/css_l4.rs` references those paths in the generated parse entry.
5. `parse_with.rs` still carries the rename shim W6 must remove: it imports `Path as LegacyPath` and `PathSegment as LegacySegment` from `crate::runtime::path` and lowers typed segments into that borrowed alphabet.

## §2 — Recommendations (named falsifiability gates)

- Gate W6.0 on a root runtime file-count delta: CSS L4 handwritten files `7 -> 0`; aggregate Pattern H `67 -> 60` after W6.0.
- Keep the CSS public API intact: focused tests must include `css_l4_substrate`, `parse_with_css_l4`, `runtime_root`, `typed_accessor_surface`, and at least one color/dimension parity test.
- Do not replace CSS L4 with the simple `CompoundSlabArena` runtime; that would violate the rich-AST parity surface asserted by the CSS tests.

## §3 — Risks (REDRESS entries to pre-block)

- Static centralization of the current seven files under a fake generated header would reopen P-6 and violate `SPEC.md:1007-1013`.
- A thin generated wrapper that relocates handwritten CSS semantics outside `runtime/css_l4/` would satisfy the directory-count grep while preserving Pattern H under another name; treat that as REJECT.
- Removing the `LegacyPath` shim without preserving `parse_with_css_l4` lazy parity would break W6.0's same-wave consumer.

## §4 — Sources (every external citation)

- Local repository only; no external sources.
