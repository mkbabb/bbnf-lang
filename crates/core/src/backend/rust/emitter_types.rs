//! RustEmitter struct and RustEmitCtx — types for the shared-driver Rust backend.
//!
//! `RustEmitCtx` holds a pointer to `IrCodegenCtx` for access to the full type
//! system, slab allocation helpers, and sub-variant resolution. The pointer is
//! sound because `IrCodegenCtx` is created in `generate_all()` and outlives all
//! emission calls.

use std::collections::HashSet;

use bbnf_ir::{RuleId, TypeDesc};
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

/// AM.3 per-branch tape surgery context.
///
/// When active, the Alt emitter wraps each branch arm with its own
/// `push_leaf` or `mark_children` + `push_compound` instead of
/// relying on a shared epilogue. This eliminates the compound record
/// overhead for leaf branches (literals, regex, pure-conversion maps).
#[derive(Clone)]
pub struct TapeSurgeryCtx {
    /// The `TapeKind` to use in push calls (always `TapeKind::Rule`
    /// for rule-level Alt bodies).
    pub tape_kind: TokenStream,
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
    /// When set, the Alt emitter prepends `<ident> = <branch_idx>u8;`
    /// to each branch body. Set by `emit_rule_function_impl` for
    /// Alt-bodied rules so the rule epilogue can use the branch
    /// discriminator instead of the rule's global ID. Cleared after
    /// the Alt body is compiled.
    pub branch_idx_ident: Option<syn::Ident>,
    /// AM.3: when set, the Alt emitter emits per-branch `push_leaf`
    /// or `push_compound` calls instead of relying on a shared
    /// epilogue. Set by `emit_tape_tier_rule` for Alt-bodied
    /// `MustTape` rules; cleared after the body is compiled.
    pub tape_surgery: Option<TapeSurgeryCtx>,
    /// AN.0: stack of saved outer `branch_idx_ident` +
    /// `tape_surgery` so arbitrarily nested Alts inside branch
    /// bodies cannot clobber outer Alt contexts. Pushed by
    /// `save_alt_context`, popped by `restore_alt_context`.
    alt_context_stack: Vec<(Option<syn::Ident>, Option<TapeSurgeryCtx>)>,
    /// AQ.6.A: the rule body's projected scalar payload type, sourced
    /// directly from `ir.types[rule_id]` in `pre_compile_rule_body`.
    /// `Some(td)` iff `td.is_scalar_payload()` — Bool / U8 / I8 / U16 /
    /// I16 / U32 / I32 / U64 / I64 / F64. The rule prelude declares the
    /// matching `__payload_<T>` local; the body sets it via the leaf
    /// emitter; the rule epilogue selects the matching `push_leaf_with_<T>`
    /// (or `push_leaf` when `__has_payload` is false).
    pub payload_type: Option<TypeDesc>,
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
            branch_idx_ident: None,
            tape_surgery: None,
            alt_context_stack: Vec::new(),
            payload_type: None,
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

    /// AN.0: Push `branch_idx_ident` and `tape_surgery` onto the
    /// context stack before the driver compiles Alt branch bodies.
    /// Inner (nested) Alts will see `None` for both fields so they
    /// cannot clobber the outer Alt's context.
    pub fn save_alt_context(&mut self) {
        self.alt_context_stack.push((
            self.branch_idx_ident.take(),
            self.tape_surgery.take(),
        ));
    }

    /// AN.0: Pop `branch_idx_ident` and `tape_surgery` from the
    /// context stack after all branch bodies are compiled, so the
    /// emitter's `emit_alt_*` call sees the correct outer context.
    pub fn restore_alt_context(&mut self) {
        if let Some((saved_idx, saved_surgery)) = self.alt_context_stack.pop() {
            self.branch_idx_ident = saved_idx;
            self.tape_surgery = saved_surgery;
        }
    }
}

// SAFETY: RustEmitCtx is only used single-threaded within a single compile_grammar call.
unsafe impl Send for RustEmitCtx {}
unsafe impl Sync for RustEmitCtx {}
