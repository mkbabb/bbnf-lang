//! Allocation codegen: scratch Vec emission and context struct generation.
//!
//! Extracted from `ir_types.rs` — allocation concerns that don't belong in the
//! core type bridge.

use bbnf_ir::TypeDesc;

use proc_macro2::TokenStream;

use super::ir_types::{IrCodegenCtx, type_desc_to_syn};

// ── Scratch Vec emission ─────────────────────────────────────────────────────

impl IrCodegenCtx<'_> {
    /// Emit scratch-based collection init: records scratch depth.
    pub fn emit_scratch_init(&self, elem_desc: &TypeDesc, depth_var: &syn::Ident) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.alloc_helper_ident();
        quote::quote! {
            let #depth_var = #helper(state).#s_fn().len();
        }
    }

    /// Emit scratch push for a value expression.
    pub fn emit_scratch_push(&self, elem_desc: &TypeDesc, value_expr: &TokenStream) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.alloc_helper_ident();
        quote::quote! { #helper(state).#s_fn().push(#value_expr) }
    }

    /// Emit scratch collect: copies scratch[depth..] to slab slice, truncates.
    pub fn emit_scratch_collect(&self, elem_desc: &TypeDesc, depth_var: &syn::Ident) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let c_fn = self.collect_accessor(idx);
        let helper = self.alloc_helper_ident();
        quote::quote! { #helper(state).#c_fn(#depth_var) }
    }

    /// Emit scratch truncate on failure path.
    pub fn emit_scratch_truncate(
        &self,
        elem_desc: &TypeDesc,
        depth_var: &syn::Ident,
    ) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.alloc_helper_ident();
        quote::quote! { #helper(state).#s_fn().truncate(#depth_var); }
    }

    /// Emit scratch len - depth expression (element count).
    pub fn emit_scratch_count(
        &self,
        elem_desc: &TypeDesc,
        depth_var: &syn::Ident,
    ) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.alloc_helper_ident();
        quote::quote! { (#helper(state).#s_fn().len() - #depth_var) }
    }

    /// Emit scratch extend from a slice (for Seq flattening).
    pub fn emit_scratch_extend_slice(
        &self,
        elem_desc: &TypeDesc,
        slice_expr: &TokenStream,
    ) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.alloc_helper_ident();
        quote::quote! { #helper(state).#s_fn().extend_from_slice(#slice_expr) }
    }

    /// Generate the alloc context struct definition + impl + helper function.
    ///
    /// Returns (struct_def, helper_fn) as TokenStreams.
    pub fn generate_alloc_ctx(&self) -> (TokenStream, TokenStream) {
        let ctx_ident = self.alloc_ctx_ident();
        let helper_ident = self.alloc_helper_ident();

        // Generate scratch fields, accessors, and collect methods.
        //
        // Each scratch type gets a staging Vec (for push/truncate during loops)
        // and the shared BumpSlab for permanent allocation (alloc_slice_clone).
        let mut fields = vec![];
        let mut accessors = vec![];
        let mut new_fields = vec![];

        for (i, elem_td) in self.scratch_types.iter().enumerate() {
            let s_ident = self.scratch_accessor(i);
            let c_ident = self.collect_accessor(i);
            let elem_ty = type_desc_to_syn(elem_td, self);

            // Scratch staging Vec.
            fields.push(quote::quote! {
                #s_ident: ::std::cell::UnsafeCell<Vec<#elem_ty>>
            });
            new_fields.push(quote::quote! {
                #s_ident: ::std::cell::UnsafeCell::new(Vec::with_capacity(64))
            });

            accessors.push(quote::quote! {
                #[inline(always)]
                #[allow(non_snake_case)]
                fn #s_ident(&self) -> &mut Vec<#elem_ty> {
                    unsafe { &mut *self.#s_ident.get() }
                }

                #[inline(always)]
                #[allow(non_snake_case)]
                fn #c_ident(&'a self, depth: usize) -> &'a [#elem_ty] {
                    let s = self.#s_ident();
                    let slice = self.__slab.alloc_slice_clone(&s[depth..]);
                    s.truncate(depth);
                    slice
                }
            });
        }

        let struct_def = quote::quote! {
            #[allow(non_camel_case_types)]
            struct #ctx_ident<'a> {
                __slab: ::parse_that::BumpSlab,
                #(#fields),*
            }

            #[allow(non_snake_case)]
            impl<'a> #ctx_ident<'a> {
                fn with_capacity(n: usize) -> Self {
                    Self {
                        __slab: ::parse_that::BumpSlab::with_capacity(n * 32),
                        #(#new_fields),*
                    }
                }

                #[inline(always)]
                fn slab(&self) -> &::parse_that::BumpSlab {
                    &self.__slab
                }

                #(#accessors)*
            }
        };

        let helper_fn = quote::quote! {
            #[allow(non_snake_case)]
            #[inline(always)]
            fn #helper_ident<'a>(
                state: &::parse_that::ParserState<'a>,
            ) -> &'a #ctx_ident<'a> {
                debug_assert!(
                    !state.context_ptr.is_null(),
                    "slab parser requires parse_with_context()"
                );
                unsafe {
                    &*(state.context_ptr as *const #ctx_ident<'a>)
                }
            }
        };

        (struct_def, helper_fn)
    }
}

