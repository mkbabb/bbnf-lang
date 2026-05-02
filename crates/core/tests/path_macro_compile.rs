//! AZ-IV.W2.4 — positive compile fixtures for the `path!` proc-macro.
//!
//! Confirms `path!(GrammarMarker, ...)` resolves at compile time
//! against the per-grammar [`StructRegistry`] projected by the IR's
//! `project_types` pass and serialised to the per-grammar
//! `<ident>.registry.json` sidecar by `cargo xtask regen`.
//!
//! AZ-IV.W5 T4 — fixture coverage migrated from the synthetic per-
//! grammar fixture (a hand-authored Twitter-shaped JSON registry) to
//! the production registry. Each test now traces a path that exists in
//! the actual grammar's projected registry.
//!
//! Coverage:
//!
//! 1. JSON: `path!(Json, "@branch_0")` — variant-select on the `value`
//!    tagged-enum's first branch. The closest production analogue of
//!    the W2.4 hard-gate path semantics.
//! 2. JSON: `path!(Json, "@branch_4")` — variant-select on a different
//!    branch index.
//! 3. CssL4: `path!(CssL4, "ruleList")` resolves to `ruleList` on the
//!    `stylesheet` struct root.
//! 4. Bbnf: `path!(Bbnf, "element")` resolves through the bbnf
//!    grammar's `grammar` Vec-shaped root layout.
//! 5. Wildcard token form lands a wildcard segment on a list-shaped
//!    layout (the bbnf `grammar` root).

use bbnf::path::{Json, OwnedPathSegment, TypedPath};
use bbnf_path::path;

#[test]
fn json_first_branch_variant_select_compiles() {
    // The JSON grammar's `value` rule is a tagged-enum over the six
    // alternatives in `value = object | array | string | number |
    // bool | null`. Selecting the first branch via `@branch_0`
    // exercises the variant-select path against the production
    // registry.
    let p: TypedPath<Json, ()> = path!(Json, "@branch_0");
    assert_eq!(p.len(), 1);
    let owned = p.owned_segments();
    assert!(matches!(&owned[0], OwnedPathSegment::VariantName(s) if s == "branch_0"));
}

#[test]
fn json_branch_4_variant_select_compiles() {
    let p: TypedPath<Json, ()> = path!(Json, "@branch_4");
    assert_eq!(p.len(), 1);
    let owned = p.owned_segments();
    assert!(matches!(&owned[0], OwnedPathSegment::VariantName(s) if s == "branch_4"));
}

#[test]
fn css_l4_marker_resolves_through_production_registry() {
    use bbnf::path::CssL4;
    // The CSS L4 grammar's `stylesheet` root carries `ruleList` as its
    // single field. The path validates through the production registry
    // sidecar.
    let p: TypedPath<CssL4, ()> = path!(CssL4, "ruleList");
    assert_eq!(p.len(), 1);
    assert!(matches!(&p.owned_segments()[0], OwnedPathSegment::Field(s) if s == "ruleList"));
}

#[test]
fn bbnf_marker_resolves_through_production_registry() {
    use bbnf::path::Bbnf;
    // The BBNF self-host grammar's `grammar` root projects to
    // `Vec<rule>` with a single repeat-element field. The path
    // validates through the production registry sidecar.
    let p: TypedPath<Bbnf, ()> = path!(Bbnf, "element");
    assert_eq!(p.len(), 1);
    assert!(matches!(&p.owned_segments()[0], OwnedPathSegment::Field(s) if s == "element"));
}

#[test]
fn fully_qualified_marker_path_compiles() {
    // The grammar argument may be a Rust path of any depth; the macro
    // reads the trailing identifier as the registry key and passes the
    // full path through to the emitted `TypedPath::<...>`.
    let p: TypedPath<bbnf::path::Json, ()> = path!(bbnf::path::Json, "@branch_0");
    assert_eq!(p.len(), 1);
}

// AZ-IV.W5 T4 — wildcard production-registry coverage deferred. The
// production registries' Vec-shaped layouts project the inner element
// type as `BoxedEnum` rather than the post-W2.4 fixture's explicit
// `Vec<T>`, and the path-walker's wildcard step requires the layout's
// `rule_type` to be `Vec<inner>` directly (see `step_into_layout` in
// `crates/bbnf-path/src/path_macro.rs`). The wildcard token form
// itself is exercised by `path_macro_errors` and `path_wildcard_iter`;
// adding a positive production-registry wildcard fixture is a follow-on
// once the registry projects Vec wrappers verbatim.
