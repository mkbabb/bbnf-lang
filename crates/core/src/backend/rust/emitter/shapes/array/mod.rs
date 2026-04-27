//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2 / AX.W0a.2.a
//!
//! Emits the per-grammar Array-shape parse function with **walker-
//! identical tape emission**. The Array detector admits two structural
//! shapes:
//!
//! 1. **Shape 1 — wrapped homogeneous repeat** (JSON `array`):
//!
//!    ```text
//!    array = "[" >> ((value << comma?)*)?w << "]"
//!    ```
//!
//!    The body unwraps to `Wrap(open_byte, Repeat, close_byte)` where
//!    `open` and `close` are concrete single-byte literals.
//!    [`wrapped::emit_parse_array_wrapped`] emits the nested
//!    Seq/Seq/Repeat/Seq compound tree with the bracket literals as
//!    Literal leaves.
//!
//! 2. **Shape 2 — entry-rule list** (CSS `stylesheet`, BBNF `grammar`):
//!
//!    ```text
//!    stylesheet = ruleList ?w          // OW(Repeat(...)) after inline
//!    grammar    = ( grammar_item ?w )* // direct Repeat
//!    ```
//!
//!    The body has no bracket wrap — the rule body is either a direct
//!    `Repeat` or an `OptionalWhitespace(Repeat(...))`. No close-
//!    delimiter sentinel exists; iteration terminates when the inner
//!    value's first-byte dispatch rejects (end-of-input or a byte not
//!    in the element's first set). [`list::emit_parse_array_list`]
//!    emits the matching Seq/Rule compound tree — outer Seq when an OW
//!    wrapper is present, otherwise the Repeat's Rule compound
//!    directly.
//!
//! Each structural IR production becomes a `push_compound` record.
//! Downstream view derives (`arrayView`, `valueView`, typed-field
//! projections) and the `tape_parity` golden fixtures navigate that
//! exact record sequence, so the shape emitter must reproduce it byte-
//! for-byte — only the **dispatch** is inlined (no `dispatch_one` /
//! `try_branch` / cross-crate helper chain), not the **records**.
//!
//! # Emitted tape shape — Shape 1 (for `[v1, v2]`)
//!
//! ```text
//! [ 0] Seq     variant=<array_id>  span=0..N   child=1  has_children=true
//! [ 1] Seq     variant=0           span=0..N-1 child=2  has_children=true   <- Next("[", rest)
//! [ 2] Literal variant=0           span=0..1                                 <- "["
//! [ 3] Seq     variant=0           span=1..N-1 child=4  has_children=true   <- OptionalWhitespace
//! [ 4] Rule    variant=0           span=1..N-1 child=5  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- Skip(value, Repeat(,?))
//!         ...value records...
//!         Rule  variant=0          has_children=true                        <- Repeat(,?)
//!           Seq variant=0                                                    <- OptionalWhitespace(",")
//!             Literal variant=0                                              <- ","
//! [ N] Literal variant=0           span=N-1..N                               <- "]"
//! ```
//!
//! # Emitted tape shape — Shape 2 (CSS stylesheet `OW(Repeat(OW(Ref)))`)
//!
//! ```text
//! [ 0] Seq     variant=<rule_id>  span=0..N   child=1  has_children=true   <- OW(Repeat)
//! [ 1] Rule    variant=0          span=L..R   child=2  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- OW(Ref)
//!         ...value records via Ref dispatch...
//! ```
//!
//! # Emitted tape shape — Shape 2 (BBNF grammar, direct Repeat)
//!
//! ```text
//! [ 0] Rule    variant=<rule_id>  span=0..N   child=1  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- OW(Ref)
//!         ...value records via Ref dispatch...
//! ```

use bbnf_ir::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;

mod element;
mod list;
mod visitor;
mod wrapped;

pub use visitor::emit_parse_array_visitor;

/// Emit `pub fn parse_array_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
///
/// Dispatches on rule body structure:
///
/// - **Shape 1** — body unwraps to `Wrap(open, middle, close)` with
///   concrete single-byte open/close literals → [`wrapped::emit_parse_array_wrapped`].
/// - **Shape 2** — body is a `Repeat` (direct) or `OptionalWhitespace(Repeat)`
///   with no delimiter wrap → [`list::emit_parse_array_list`].
///
/// The two variants share the function identity (`parse_array_<grammar>_<rule>`)
/// and the outer signature; only the body differs per shape.
pub fn emit_parse_array(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let body = unwrap_map_ow(&rule.body);
    if let Some((open, _middle, close)) = unwrap_wrap(body) {
        if single_byte_literal(open, ir).is_some()
            && single_byte_literal(close, ir).is_some()
        {
            return wrapped::emit_parse_array_wrapped(grammar_suffix, rule, ir);
        }
    }
    list::emit_parse_array_list(grammar_suffix, rule, ir)
}
