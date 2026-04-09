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
            // Sep with empty brk: flat -> emit flat bytes, break -> newline.
            quote! { __builder.sep(#sep_lit, ""); }
        }
    }
}

// ─── Helper: emit rule wrapper (group/indent/block around body) ───────────

pub(super) fn emit_rule_wrapper(policy: &PrettyPolicy, body: TokenStream) -> TokenStream {
    match policy.wrapper {
        WrapperPolicy::GroupIndent => quote! {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = { #body };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::Group => quote! {
            __builder.group_open();
            let __pretty_ok = { #body };
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::BlockIndent => quote! {
            __builder.indent_open();
            __builder.hardline();
            let __pretty_ok = { #body };
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

// ─── Helper: split compile error ──────────────────────────────────────────

pub(super) fn split_compile_error(rule_name: &str, split: &str) -> TokenStream {
    let msg = format!(
        "`split(\"{split}\")` is not supported by prettify codegen yet on rule `{rule_name}`"
    );
    syn::Error::new(proc_macro2::Span::call_site(), msg).to_compile_error()
}
