//! Prettify trait implementation for the Rust emitter.
//!
//! Implements all `emit_prettify_*` methods on [`RustEmitter`] as inherent
//! `_impl` methods, split across cohesive sub-modules. The trait
//! `impl Emitter` in the parent `mod.rs` delegates to these.

use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::prettify::{PrettyPolicy, SeparatorPolicy, WrapperPolicy};

mod alt;
mod attempt;
mod grammar;
mod literal;
mod repeat;
mod seq;

// ─── Helper: prettify function identifier ─────────────────────────────────

pub(super) fn prettify_fn_ident(name: &str) -> syn::Ident {
    syn::Ident::new(
        &format!("__{}_prettify", name),
        proc_macro2::Span::call_site(),
    )
}

// ─── Helper: emit separator ops ───────────────────────────────────────────

pub(super) fn emit_separator_ops(policy: &PrettyPolicy) -> TokenStream {
    match &policy.separator {
        SeparatorPolicy::None => quote! {},
        SeparatorPolicy::Space => quote! { __builder.text(" "); },
        SeparatorPolicy::Softline => quote! { __builder.softline(); },
        SeparatorPolicy::Hardline => quote! { __builder.hardline(); },
        SeparatorPolicy::Blankline => quote! {
            __builder.hardline();
            __builder.hardline();
        },
        SeparatorPolicy::Custom(sep_str) => {
            let sep_lit = proc_macro2::Literal::string(sep_str);
            let brk_lit = proc_macro2::Literal::string(sep_str.trim_end());
            // Flat mode emits the separator; break mode emits its
            // non-blank part, then the newline. An empty break text
            // dropped the `,` of `sep(", ")`, which changed the
            // formula a broken group printed.
            quote! { __builder.sep(#sep_lit, #brk_lit); }
        }
    }
}

// ─── Helper: emit rule wrapper (group/indent/block around body) ───────────

/// The body runs in a closure, so an early `return false` inside it (a
/// failed match) still reaches the wrapper's closes: the op stream stays
/// balanced on every path. pprint's `finish()` refuses an unclosed group
/// (law L8, ABI v2) instead of closing it at the end of the document.
pub(super) fn emit_rule_wrapper(policy: &PrettyPolicy, body: TokenStream) -> TokenStream {
    match policy.wrapper {
        WrapperPolicy::GroupIndent => quote! {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = (|| -> bool { #body })();
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::Group => quote! {
            __builder.group_open();
            let __pretty_ok = (|| -> bool { #body })();
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::BlockIndent => quote! {
            __builder.indent_open();
            __builder.hardline();
            let __pretty_ok = (|| -> bool { #body })();
            __builder.indent_close();
            __builder.hardline();
            __pretty_ok
        },
        WrapperPolicy::Block => quote! { #body },
        WrapperPolicy::Off | WrapperPolicy::None => quote! { #body },
    }
}

// ─── Helper: emit inline-ws text segment ──────────────────────────────────

pub(super) fn emit_whitespace_segment(start_var: &syn::Ident) -> TokenStream {
    quote! {
        __builder.text_inline_ws(&state.src[#start_var..state.offset]);
    }
}

// ─── Helper: emit `@ws` trivia (whitespace with comments) ────────────────

/// A span the grammar's `@ws` pattern matched is trivia, not text (sk-v27
/// LW3.S3c-PP, QX-1 clause 4). Re-emitting the span raw printed each
/// pass's line breaks beside the layout's own, so `fmt . fmt != fmt`.
pub(super) fn emit_trivia_segment(start_var: &syn::Ident) -> TokenStream {
    quote! {
        Self::__prettify_trivia(__builder, &state.src[#start_var..state.offset]);
    }
}

/// The trivia function, emitted once per grammar that declares `@ws`;
/// `ws_code` is that pattern's matcher over `state`. Comments are kept
/// verbatim. A whitespace run is kept inline when it holds no line feed.
/// A run with a line feed is a hard line before a comment (an own-line
/// comment stays on its own line) and one space after one (the layout
/// places the lines around tokens; the renderer trims the space at a line
/// end, and a hard line there would stack on the layout's own line in the
/// next pass), except after a comment that would swallow the next token
/// on its line (a line comment: the `@ws` matcher reads past its end),
/// which keeps its hard line. A run right after a line or separator op is
/// dropped: the layout already placed that line.
pub(super) fn emit_trivia_fn(ws_code: TokenStream) -> TokenStream {
    quote! {
        fn __prettify_trivia<'a>(__builder: &mut ::pprint::FmtBuilder<'a>, __t: &'a str) {
            let __rest = __t.trim_start_matches(char::is_whitespace);
            let __lead = &__t[..__t.len() - __rest.len()];
            let __core = __rest.trim_end_matches(char::is_whitespace);
            let __trail = &__rest[__core.len()..];
            let __after_line = __builder
                .ops()
                .iter()
                .rev()
                .find(|op| {
                    !matches!(
                        op,
                        ::pprint::FmtOp::GroupStart { .. }
                            | ::pprint::FmtOp::GroupEnd
                            | ::pprint::FmtOp::IndentStart
                            | ::pprint::FmtOp::IndentEnd
                    )
                })
                .is_some_and(|op| {
                    matches!(
                        op,
                        ::pprint::FmtOp::Hardline
                            | ::pprint::FmtOp::Softline
                            | ::pprint::FmtOp::BreakLine
                            | ::pprint::FmtOp::CommaSep
                            | ::pprint::FmtOp::Sep { .. }
                            | ::pprint::FmtOp::FillSep { .. }
                    )
                });
            if __core.is_empty() {
                if !__after_line {
                    __builder.text_inline_ws(__lead);
                }
                return;
            }
            if !__after_line {
                if __lead.contains('\n') {
                    __builder.hardline();
                } else {
                    __builder.text_inline_ws(__lead);
                }
            }
            __builder.text(__core);
            if !__trail.contains('\n') {
                __builder.text_inline_ws(__trail);
                return;
            }
            let __probe = ::std::format!("{__core}x");
            let __swallows = {
                let mut __ps = ::parse_that::ParserState::new(&__probe);
                let state = &mut __ps;
                let _ = #ws_code;
                state.offset > __core.len()
            };
            if __swallows {
                __builder.hardline();
            } else {
                __builder.text(" ");
            }
        }
    }
}

// ─── Helper: split compile error ──────────────────────────────────────────

pub(super) fn split_compile_error(rule_name: &str, split: &str) -> TokenStream {
    let msg = format!(
        "`split(\"{split}\")` is not supported by prettify codegen yet on rule `{rule_name}`"
    );
    syn::Error::new(proc_macro2::Span::call_site(), msg).to_compile_error()
}
