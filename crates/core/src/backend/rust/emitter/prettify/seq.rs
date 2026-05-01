//! Prettify codegen for Seq, Skip, Next, and OptionalWhitespace nodes.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use super::emit_whitespace_segment;
use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_seq_impl(
        &mut self,
        children: Vec<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if children.is_empty() {
            return quote! {};
        }
        if children.len() == 1 {
            return children.into_iter().next().unwrap();
        }
        quote! { { #(#children;)* } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_skip_impl(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Both sides emitted for their effects (parse + format).
        quote! { {
            #left;
            #right;
        } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_next_impl(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Both sides emitted for their effects (parse + format).
        quote! { {
            #left;
            #right;
        } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_optional_ws_impl(
        &mut self,
        inner: TokenStream,
        is_atomic: bool,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Emit ws trim code. `?w` must trim identically to the recognizer:
        // when `@ws` declares a comment-aware pattern, the prettify side
        // must consume the same byte-run so the builder advances through
        // leading/trailing comments. Falling back to ASCII-only trim here
        // is the root cause of the 0-byte leading-comment failure mode
        // documented in `docs/tranches/AX/audit/W1r3-diag.md`: the
        // recognizer traverses `/* ... */` via `Op::TrimWsPattern` while
        // the prettify emitter previously called ASCII-only
        // `trim_leading_whitespace_mut`, leaving the parse state stuck at
        // the opening `/` and producing zero ops.
        let ws_trim = emit_ws_trim_tokens(ir);

        if is_atomic {
            // Deferred pattern: scan leading ws, try inner, then emit ws only
            // after inner succeeds. No checkpoint needed because the inner
            // expression fails atomically (before emitting any ops).
            let ws1 = ctx.fresh("ows");
            let ws2 = ctx.fresh("ows");
            let ws3 = ctx.fresh("ows");
            quote! { {
                let #ws1 = state.offset;
                #ws_trim
                let #ws2 = state.offset;
                #inner;
                __builder.text_inline_ws(&state.src[#ws1..#ws2]);
                let #ws3 = state.offset;
                #ws_trim
                __builder.text_inline_ws(&state.src[#ws3..state.offset]);
            } }
        } else {
            // Non-atomic inner: must checkpoint because the inner expression
            // might emit ops before failing. Leading ws is emitted eagerly.
            let ws_start1 = ctx.fresh("ows");
            let ws_start2 = ctx.fresh("ows");
            let ws_emit1 = emit_whitespace_segment(&ws_start1);
            let ws_emit2 = emit_whitespace_segment(&ws_start2);
            let body = quote! {{
                let #ws_start1 = state.offset;
                #ws_trim
                #ws_emit1
                #inner;
                let #ws_start2 = state.offset;
                #ws_trim
                #ws_emit2
            }};
            let body_try = self.emit_prettify_attempt_impl(body, true, false, ctx);
            quote! { {
                if !#body_try {
                    return false;
                }
            } }
        }
    }
}

/// Emit comment-aware whitespace trim tokens for `?w` / OptionalWhitespace.
///
/// Threads the grammar's `@ws` pattern through the regex emitter when
/// declared so the prettify side performs the same DFA walk the
/// recognizer (`Op::TrimWsPattern`) uses. Falls back to ASCII-only
/// `trim_leading_whitespace_mut` when the grammar has no `@ws` directive
/// — the pre-W1r.3a behaviour, preserved for grammars that don't opt
/// into comment-aware whitespace.
///
/// Why this lives in prettify codegen (not the grammar layer): `?w` is
/// a structural modifier whose semantics are defined grammar-wide by
/// `@ws`. The recognizer's `IrNode::OptionalWhitespace` arm already
/// dispatches on `ir.ws_pattern`; parity demands the prettify emitter
/// do the same. A grammar-level workaround (explicit `ws = /.../`
/// rule + rewriting `?w` to `ws >> X << ws`) would fork every
/// `?w`-using grammar and duplicate the `@ws` regex, violating
/// `feedback_no_workarounds_arch` + `feedback_system_cohesion`.
fn emit_ws_trim_tokens(ir: &GrammarIR) -> TokenStream {
    match ir.ws_pattern {
        Some(sid) => {
            let pattern = ir.get_string(sid);
            let ws_pat = Some(pattern);
            let opts =
                crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                    .with_ir(ir)
                    .with_ws_pattern(ws_pat);
            // `@ws /...*/` matches zero-or-more ws/comment — the regex
            // scan always succeeds. Bind its Option return but discard
            // the value; the side effect of advancing `state.offset`
            // is what we want.
            let code = crate::generate::regex::emit_regex(pattern, &opts);
            quote! {
                let _ = #code;
            }
        }
        None => quote! {
            ::parse_that::trim_leading_whitespace_mut(state);
        },
    }
}
