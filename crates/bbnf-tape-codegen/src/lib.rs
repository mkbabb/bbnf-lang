//! Body-source fragments for the four residual `bbnf-tape` helpers.
//!
//! # Role
//!
//! AW-IV's `nm` audit identified four helpers in `bbnf-tape` that
//! remained as cross-crate function-call boundaries on the parser's
//! hot path:
//!
//! - [`advance`] — `bbnf_tape::driver::advance_or_pop_with` (the
//!   Seq / Alt / Repeat / ShuntingYard advance dispatcher called
//!   from every walker arm tail).
//! - [`frame`] — `bbnf_tape::driver::FrameStack::nearest_variant_frame`
//!   (the walk-the-stack lookup consulted on every payload-bearing
//!   leaf emission).
//! - [`decoded`] — `bbnf_tape::psi::write_decoded` (the Stage-B
//!   payload-decode kernel consulted by `PayloadStream::fill_*`).
//! - [`finalise`] — `bbnf_tape::finaliser::finalise` (the Stage-C
//!   forward scan patching `sib_skip` / `span_hi` / `child_off`).
//!
//! Inlining these into the per-shape emitter's generated parse
//! functions requires that the emitter own a verbatim copy of each
//! helper's body as a [`proc_macro2::TokenStream`] it can splice via
//! `quote! { #fragment }`. This crate owns that copy: each module
//! exports the helper's body as a Rust source `&str` constant plus a
//! `fragment()` function that parses the source to a `syn::Block` and
//! returns its `TokenStream`.
//!
//! # Why body-source constants, not a `#[proc_macro_attribute]`?
//!
//! Two alternatives exist. (a) A `#[export_body_fragment]` attribute
//! on the helper fn in `bbnf-tape` that produces a paired `pub const
//! __<helper>_BODY: &str` at build time. (b) A `bbnf-tape-codegen`
//! subcrate holding the body as a stringified Rust constant. Approach
//! (b) has fewer moving parts — no attribute macro, no build-script
//! coupling, no paired source-of-truth between helper and its
//! exported body. The constant is checked at `cargo test` time via
//! [`syn::parse_str::<syn::Block>`]; divergence between the runtime
//! helper and the exported body surfaces as a test failure.
//!
//! # AX replay-surface preservation
//!
//! The runtime helpers in `bbnf_tape::driver`, `bbnf_tape::psi`, and
//! `bbnf_tape::finaliser` survive unchanged. `dispatch_one` and the
//! cold-path `dta_run_cold` replay surface continue to call them by
//! name; only the per-shape emitter's new hot-path splices bypass
//! the function-call boundary. One body, two consumers — hot-path
//! inline splice + cold-path fn-by-name — is the generalisation
//! invariant per B2 §10.
//!
//! # API shape per module
//!
//! Every module exposes:
//!
//! - `pub const SOURCE: &str` — the verbatim body text of the helper
//!   (everything between the outer `{` and `}` of the original
//!   `fn`), ready to parse as a [`syn::Block`].
//! - `pub fn fragment() -> proc_macro2::TokenStream` — a one-liner
//!   that parses [`SOURCE`] and re-emits it as a `TokenStream` the
//!   emitter can splice inline.
//!
//! The signature of each helper is the emitter's responsibility at
//! the splice site — the fragment owns the body only. The emitter's
//! per-shape parse function declares parameters (for
//! `advance_or_pop_with`: `_table`, `_input`, `columns`,
//! `frame_depth`, `psi`, `stack`, `pos`, `slot`; for
//! `nearest_variant_frame`: `&self` projecting into `FrameStack`;
//! for `write_decoded`: `job`, `input`, `cells`; for `finalise`:
//! `columns`, `frame_depth`) and captures them in scope before
//! splicing the body.
//!
//! `write_decoded` is an `unsafe fn` in the runtime crate — the
//! fragment captures only the body, and the splicer wraps the
//! splice in an `unsafe` block at its call site.

pub mod advance;
pub mod frame;
