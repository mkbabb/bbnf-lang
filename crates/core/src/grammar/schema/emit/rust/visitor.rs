//! Visitor trait: `{Enum}Visitor<'a>` — a minimal fold interface over the
//! schema-emitted `walk_children` dispatch.

use proc_macro2::TokenStream;
use quote::quote;

pub(super) fn generate(enum_ident: &syn::Ident, visitor_ident: &syn::Ident) -> TokenStream {
    quote! {
        /// Auto-generated visitor trait for the parser enum.
        ///
        /// Default `visit()` calls `walk()` which dispatches via
        /// `walk_children` (per-variant direct dispatch). Override
        /// `visit()` for short-circuiting; override `combine()` for
        /// non-default fold semantics.
        pub trait #visitor_ident<'a> {
            type Output: Default;

            fn combine(&mut self, outputs: ::std::vec::Vec<Self::Output>) -> Self::Output {
                let _ = outputs;
                Self::Output::default()
            }

            fn visit(&mut self, node: &'a #enum_ident<'a>) -> Self::Output {
                self.walk(node)
            }

            fn walk(&mut self, node: &'a #enum_ident<'a>) -> Self::Output {
                let outputs = #enum_ident::walk_children(node, self);
                if outputs.is_empty() {
                    Self::Output::default()
                } else {
                    self.combine(outputs)
                }
            }
        }
    }
}
