//! RustEmitter struct and RustEmitCtx — types for the shared-driver Rust backend.
//!
//! `RustEmitCtx` holds a pointer to `IrCodegenCtx` for access to the full type
//! system, slab allocation helpers, and sub-variant resolution. The pointer is
//! sound because `IrCodegenCtx` is created in `generate_all()` and outlives all
//! emission calls.

use std::collections::HashSet;

use bbnf_ir::RuleId;
use proc_macro2::TokenStream;
use quote::format_ident;

use super::ir_types::IrCodegenCtx;

/// Rust code emitter implementing the [`Emitter`] trait.
pub struct RustEmitter {
    pub enum_ident: syn::Ident,
    pub effective_prettify: bool,
    pub fused_number_rules: HashSet<RuleId>,
    pub operator_chain_rules: HashSet<RuleId>,
    /// Extra methods to inject into the `impl` block (e.g., prettify methods).
    pub extra_impl_methods: TokenStream,
}

impl RustEmitter {
    pub fn new(enum_ident: syn::Ident, effective_prettify: bool) -> Self {
        Self {
            enum_ident,
            effective_prettify,
            fused_number_rules: HashSet::new(),
            operator_chain_rules: HashSet::new(),
            extra_impl_methods: TokenStream::new(),
        }
    }
}

/// Mutable context for Rust emission.
///
/// Holds a pointer to `IrCodegenCtx` for type lookups and slab codegen.
pub struct RustEmitCtx {
    ir_ctx_ptr: *const (),
    pub hoisted: Vec<TokenStream>,
    counter: usize,
    pub current_rule_name: Option<String>,
    pub current_rule_id: Option<RuleId>,
}

impl RustEmitCtx {
    /// Create a new context with access to `IrCodegenCtx`.
    ///
    /// # Safety contract
    /// The caller must ensure `ir_ctx` outlives this `RustEmitCtx` and all
    /// emission calls that use it. This is guaranteed by `generate_all()` which
    /// creates `IrCodegenCtx` on the stack before emission and drops it after.
    pub fn new(ir_ctx: &IrCodegenCtx<'_>) -> Self {
        Self {
            ir_ctx_ptr: ir_ctx as *const IrCodegenCtx<'_> as *const (),
            hoisted: Vec::new(),
            counter: 0,
            current_rule_name: None,
            current_rule_id: None,
        }
    }

    /// Access the `IrCodegenCtx`.
    ///
    /// Returns a reference with an independent lifetime — does NOT borrow `self`.
    /// This allows calling `self.fresh()` while holding an `ir_ctx` reference.
    ///
    /// SAFETY: The pointer is valid for the lifetime of the emission pass.
    /// `IrCodegenCtx` is created in `generate_all()` and outlives this context.
    #[inline]
    pub fn ir_ctx<'b>(&self) -> &'b IrCodegenCtx<'b> {
        unsafe { &*(self.ir_ctx_ptr as *const IrCodegenCtx<'b>) }
    }

    pub fn fresh(&mut self, prefix: &str) -> syn::Ident {
        let id = self.counter;
        self.counter += 1;
        format_ident!("__{}{}", prefix, id)
    }
}

// SAFETY: RustEmitCtx is only used single-threaded within a single compile_grammar call.
unsafe impl Send for RustEmitCtx {}
unsafe impl Sync for RustEmitCtx {}
