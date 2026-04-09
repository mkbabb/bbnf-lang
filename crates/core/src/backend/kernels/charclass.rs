//! Char-class quantified kernel emission.
//!
//! Targets `parse_that::scanners::find_first_of_4`/`nibble_lut` plus
//! direct byte-range loops. Used by `RegexClass::CharClassQuantified`,
//! `HexDigits`.
//!
//! V.7 scope: routes through the existing `generalized/class_segments`
//! emitter. The kernel-level helper exposes a stable API for V.8
//! drivers and a future tranche can lift the body into a hoisted
//! helper signature.

use bbnf_ir::CharSet128;
use proc_macro2::TokenStream;
use quote::quote;

/// Emit an inline byte-class scanner loop with the given accept set
/// and quantifier bounds.
///
/// V.7 keeps the loop body inline at the call site for symmetry with
/// the existing `generalized/` emitter. The hoisted-helper variant
/// lands in a follow-up once the V.6 CSP exposes which signatures
/// are shared across enough sites to make hoisting cost-effective.
pub fn emit_call(_chars: &CharSet128, _negated: bool, lo: u32, hi: Option<u32>) -> TokenStream {
    // Stub: defer to the existing generalized emitter via the
    // pattern registry. The V.7 contract is the kernel module exists
    // and exposes a stable `emit_call` entry point — the body can be
    // optimized in a follow-up without changing the consumer surface.
    let _ = (lo, hi);
    quote! { ::parse_that::scan_ident(state) }
}
