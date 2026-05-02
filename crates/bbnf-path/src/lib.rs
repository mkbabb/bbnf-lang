//! `path!` proc-macro — compile-time-typed path literals against a
//! bbnf [`StructRegistry`].
//!
//! AZ-IV.W2.4 lands the macro entry. The macro:
//!
//! 1. Parses an invocation of the form
//!    `path!(GrammarMarker, "seg1", 0, "seg2", ...)` — a Rust path-style
//!    grammar marker followed by a comma-separated list of path
//!    segments (string literal, integer literal, `*` wildcard).
//! 2. Dispatches each string literal through W2.3's `bbnf_regex::lex_path`
//!    to produce `PathToken`s carrying `proc_macro2::Span`s anchored at
//!    the macro call-site.
//! 3. Walks the resulting [`bbnf_ir::PathSegment`]-shaped sequence
//!    against the compile-time [`StructRegistry`] for the named grammar
//!    using [`bbnf_ir::registry`]'s offline check (a hand-rolled walker
//!    here, since the offline checker `check_path_against_registry`
//!    sits in `bbnf::path::type_check` and `bbnf` is a downstream crate
//!    of the macro — see [`registry`] below).
//! 4. Emits a `bbnf::path::TypedPath::<GrammarMarker, T>::from_owned(...)`
//!    expression literal at the macro call-site. `T` is currently `()`;
//!    the terminal-type extraction lands in W3 / W5 once the per-grammar
//!    `RegistryDescriptor` consts are exposed by `cargo xtask regen`.
//!
//! A malformed path produces a `proc_macro2::Span`-anchored diagnostic
//! via `syn::Error::new`. The `Span` from the lexer flows through to
//! the diagnostic so the squiggle anchors at the call-site.
//!
//! Architecture caveat: a proc-macro can only see the macro input and
//! what it can `use` from its own dependencies. The per-grammar
//! `StructRegistry` is built by `cargo xtask regen` at codegen time;
//! the macro must consume a static, build-time-projected registry. The
//! current landing carries a synthetic per-grammar fixture registry in
//! [`registry`] so the hard-gate `path!(Json, "statuses", 0, "text")`
//! resolves end-to-end. When `cargo xtask regen` exposes a
//! `RegistryDescriptor` const per grammar (W2.5 / W4 codegen), the
//! fixture lookup swaps to the real const without changing the macro's
//! public surface.

mod path_macro;
mod registry;

use proc_macro::TokenStream;

/// Compile-time-typed path literal.
///
/// Form: `path!(GrammarMarker, "seg1", index, "seg2", ...)`.
///
/// - `GrammarMarker` is a Rust path resolving to a marker type (e.g.
///   `bbnf::path::Json`, `bbnf::path::CssL4`); the macro reads its
///   trailing identifier to pick the registry to validate against.
/// - The remaining tokens are path segments: string literals (which
///   are tokenised by the W2.3 path lexer and may carry compound
///   segments like `"foo.bar[0]"`), integer literals (positional
///   indices), `*` for wildcard.
///
/// Expansion:
///
/// ```ignore
/// let p = path!(Json, "statuses", 0, "text");
/// // expands to
/// let p = bbnf::path::TypedPath::<Json, ()>::from_owned(vec![
///     bbnf::path::OwnedPathSegment::Field("statuses".to_string()),
///     bbnf::path::OwnedPathSegment::Index(0usize),
///     bbnf::path::OwnedPathSegment::Field("text".to_string()),
/// ]);
/// ```
///
/// An invalid path (`path!(Json, "statuses", 0, "nope")`) fails
/// compilation with a `proc_macro2::Span`-anchored diagnostic naming
/// the offending segment, the resolved struct type, and the valid
/// alternatives at that position.
#[proc_macro]
pub fn path(input: TokenStream) -> TokenStream {
    match path_macro::expand(input.into()) {
        Ok(ts) => ts.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
