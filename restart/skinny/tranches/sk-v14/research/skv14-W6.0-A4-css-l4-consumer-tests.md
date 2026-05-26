# SK-V14 W6.0 A4: CSS L4 Consumer Tests

Date: 2026-05-26.
Scope: identify the focused same-wave consumers that must survive CSS L4 root-runtime collapse.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. `parse_with_css_l4` still exercises the legacy borrowed path projection. `crates/core/tests/parse_with_css_l4.rs:5-23` imports `CssL4Parser`, `parse_with`, and `LegacyPath`/`LegacySegment`, then checks lazy/eager equality for the first declaration property.
2. The same test asserts lazy-error elision past the selected path. `crates/core/tests/parse_with_css_l4.rs:26-47` requires `parse_with` to return a leaf even when eager parse fails on trailing malformed bytes.
3. `css_l4_substrate` directly constructs and exercises the CSS substrate. `crates/core/tests/css_l4_substrate.rs:15-23` imports `CssArena`, `CssDocument`, `CssStructBuilder`, typed values, and `StructBuilder`; `crates/core/tests/css_l4_substrate.rs:42-48` expects an empty `CssStructBuilder` to finalise into an empty document.
4. Root API tests assert the generated parser returns a document-owned CSS view. `crates/core/tests/runtime_root.rs:45-73` requires `CssL4Parser::parse` to return a `CssDocument` whose view has `CssDocumentKind::StyleSheet`, declarations, and typed values.
5. Compile-time accessor tests bind exact public types. `crates/core/tests/typed_accessor_surface.rs:560-600` names `bbnf::runtime::css_l4::StyleSheet`, `CssArena`, `CssDocument`, and `CssView`.

## §2 — Recommendations (named falsifiability gates)

- W6.0 redress must run focused root tests: `cargo test -p bbnf --profile ax-iter --test css_l4_substrate`, `--test parse_with_css_l4`, `--test runtime_root`, and `--test typed_accessor_surface`.
- A smaller smoke set is insufficient because it can miss either the direct substrate surface or the generated-parser consumer surface.
- Treat any compile-time accessor regression as W6.0 REJECT even if the destructive file-count gate passes.

## §3 — Risks (REDRESS entries to pre-block)

- A generated root runtime that only satisfies `CssL4Parser::parse` but loses direct `CssStructBuilder` construction breaks substrate consumers.
- A generated root runtime that preserves eager parse but weakens lazy path behavior breaks `parse_with_css_l4`.
- Removing legacy aliases from production while leaving tests on legacy imports requires a simultaneous test migration to the canonical path API.

## §4 — Sources (every external citation)

- Local repository only; no external sources.
