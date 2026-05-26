# SK-V14 W6.0 A6: Path Shim Removal

Date: 2026-05-26.
Scope: determine the concrete `LegacyPath` / `LegacySegment` removal target for CSS L4.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. The canonical borrowed runtime path type is already named `Path` and `PathSegment`. `crates/core/src/runtime/path.rs:19-72` defines `PathSegment::{Field, Index}` and `Path`.
2. CSS L4 `parse_with` imports those same canonical types under legacy aliases. `crates/core/src/runtime/css_l4/parse_with.rs:20-29` imports `Path as LegacyPath` and `PathSegment as LegacySegment`.
3. The actual lowerer is a local typed-to-borrowed projection. `crates/core/src/runtime/css_l4/parse_with.rs:31-42` maps `TypedSegment` into the borrowed `PathSegment` alphabet.
4. The document accessor already consumes canonical `Path`. `crates/core/src/runtime/css_l4/document.rs:136-141` exposes `CssDocument::get<T>(path: Path<'_>)`, so the `Legacy*` names are a local naming shim, not a distinct substrate.

## §2 — Recommendations (named falsifiability gates)

- Remove only the alias naming: import `crate::runtime::path::{Path, PathSegment}` directly and update the local `lower`/vector names.
- Preserve behavior: `parse_with_css_l4` must pass after the rename, and test imports should stop naming `LegacyPath` / `LegacySegment` if production no longer does.
- Apply the same pattern in later W6 sub-waves to the other shim files; do not broaden W6.0 beyond CSS L4.

## §3 — Risks (REDRESS entries to pre-block)

- Replacing the borrowed `Path` projection with a new owned path substrate would create a parallel path API in W6.0.
- Leaving test files on `LegacyPath` names after production cleanup preserves the transitional vocabulary as documentation debt.
- Treating wildcard typed paths as representable in `CssDocument::get` would change the lazy query contract; `Wildcard` remains unrepresentable in this borrowed projection.

## §4 — Sources (every external citation)

- Local repository only; no external sources.
