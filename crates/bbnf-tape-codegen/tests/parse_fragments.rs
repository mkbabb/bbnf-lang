//! Body-fragment validity suite.
//!
//! Each `bbnf-tape-codegen` module exposes a `SOURCE` constant
//! holding the verbatim body text of a `bbnf-tape` helper. The
//! per-shape emitter splices `fragment()`'s [`TokenStream`] inline
//! via `quote!`; a body that does not parse as a [`syn::Block`]
//! would surface as a bootstrap-regen failure deep in the emitter
//! output. This suite catches the divergence at `cargo test -p
//! bbnf-tape-codegen` time instead, and is the W1.1 hard-gate
//! verification per AW-V.md §W1.1.
//!
//! Each test runs two checks in sequence:
//!
//! 1. [`syn::parse_str::<syn::Block>`] succeeds on the `SOURCE`
//!    constant, confirming the body is syntactically valid Rust.
//! 2. The parsed block has at least one statement, confirming the
//!    body was extracted non-trivially (an empty body would indicate
//!    a regression in this crate's extraction, not in the runtime
//!    helper).
//!
//! The `fragment()` accessor is invoked separately to confirm the
//! public API surface is reachable; its own `expect` catches the
//! `syn::parse_str` case redundantly but is the surface the
//! downstream emitter actually uses.

/// Confirm the `advance_or_pop_with` body fragment parses as a
/// [`syn::Block`] and is non-empty.
#[test]
fn advance_or_pop_with_fragment_parses() {
    let block: syn::Block = syn::parse_str(bbnf_tape_codegen::advance::SOURCE)
        .expect("advance_or_pop_with SOURCE must parse as syn::Block");
    assert!(
        !block.stmts.is_empty(),
        "advance_or_pop_with fragment should not be empty",
    );
    // Confirm the public accessor produces a non-empty TokenStream.
    let ts = bbnf_tape_codegen::advance::fragment();
    assert!(
        !ts.is_empty(),
        "advance_or_pop_with fragment() must produce a non-empty TokenStream",
    );
}

/// Confirm the `nearest_variant_frame` body fragment parses as a
/// [`syn::Block`] and is non-empty.
#[test]
fn nearest_variant_frame_fragment_parses() {
    let block: syn::Block = syn::parse_str(bbnf_tape_codegen::frame::SOURCE)
        .expect("nearest_variant_frame SOURCE must parse as syn::Block");
    assert!(
        !block.stmts.is_empty(),
        "nearest_variant_frame fragment should not be empty",
    );
    let ts = bbnf_tape_codegen::frame::fragment();
    assert!(
        !ts.is_empty(),
        "nearest_variant_frame fragment() must produce a non-empty TokenStream",
    );
}

/// Confirm the `write_decoded` body fragment parses as a
/// [`syn::Block`] and is non-empty.
///
/// Note: the runtime helper is `unsafe fn`, but the body itself is
/// a [`syn::Block`]. The splicer wraps the splice in its own
/// `unsafe` block at the call site.
#[test]
fn write_decoded_fragment_parses() {
    let block: syn::Block = syn::parse_str(bbnf_tape_codegen::decoded::SOURCE)
        .expect("write_decoded SOURCE must parse as syn::Block");
    assert!(
        !block.stmts.is_empty(),
        "write_decoded fragment should not be empty",
    );
    let ts = bbnf_tape_codegen::decoded::fragment();
    assert!(
        !ts.is_empty(),
        "write_decoded fragment() must produce a non-empty TokenStream",
    );
}

/// Confirm the `finalise` body fragment parses as a [`syn::Block`]
/// and is non-empty.
#[test]
fn finalise_fragment_parses() {
    let block: syn::Block = syn::parse_str(bbnf_tape_codegen::finalise::SOURCE)
        .expect("finalise SOURCE must parse as syn::Block");
    assert!(
        !block.stmts.is_empty(),
        "finalise fragment should not be empty",
    );
    let ts = bbnf_tape_codegen::finalise::fragment();
    assert!(
        !ts.is_empty(),
        "finalise fragment() must produce a non-empty TokenStream",
    );
}
