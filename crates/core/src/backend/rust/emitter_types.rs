//! RustEmitter struct and RustEmitCtx — types for the shared-driver Rust backend.
//!
//! `RustEmitCtx` holds a pointer to `IrCodegenCtx` for access to the full type
//! system, slab allocation helpers, and sub-variant resolution. The pointer is
//! sound because `IrCodegenCtx` is created in `generate_all()` and outlives all
//! emission calls.

use std::collections::HashSet;

use bbnf_ir::passes::PayloadLayout;
use bbnf_ir::{FnDescriptor, IrNode, MapExpr, RuleId, StringId, TypeDesc};
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
    /// AT.1: ordered set of scalar payload types active for the
    /// current rule. For non-Alt rules with a single scalar type,
    /// this contains exactly one element. For Alt-bodied rules,
    /// this contains all distinct scalar types across branches
    /// (e.g., `[F64, Bool, U8]` for JSON's `value` rule).
    ///
    /// The prelude declares a `__payload_<T>` local for each type.
    /// For multi-type Alts, a `__payload_tag: u8` local selects the
    /// matching `push_leaf_with_<T>` in the epilogue (tag = 1-based
    /// index into this vec; 0 = no payload).
    pub payload_types: Vec<TypeDesc>,
    /// AQ.6.B: the rule body's projected aggregate payload layout,
    /// sourced from `ir.payload_layouts` in `pre_compile_rule_body`.
    /// `Some(layout)` iff the rule's `TypeDesc` is a `Tuple` of scalars
    /// whose packed total fits in `MAX_PAYLOAD_BYTES`. When set, the
    /// rule prelude reserves a 16-byte stack buffer; per-field scalar
    /// writes go to the buffer at the layout-recorded offset; the
    /// epilogue commits via `push_leaf_with` + `PayloadData::Aggregate`.
    /// Leaf emitters
    /// consult [`Self::next_aggregate_field`] to advance the
    /// per-field cursor.
    pub payload_layout: Option<PayloadLayout>,
    /// AQ.6.B: cursor into the active `payload_layout`'s field list.
    /// Each scalar capture in the body advances the cursor by one
    /// via `next_aggregate_field`. Reset to 0 when the layout is
    /// installed in `pre_compile_rule_body`.
    pub aggregate_field_cursor: usize,
    /// AU.3.1: precomputed set of regex pattern string ids whose
    /// rules carry `-> decode_json_string_to_arena(input) : String`
    /// (or equivalent string-decode annotations). Populated once per
    /// ctx construction from `ir.rules` + `ir.fns` — the pattern set
    /// is grammar-level structural, not per-rule. Used by
    /// `emit_regex_match_impl` to route into the decode kernel
    /// regardless of whether the rule is being compiled standalone
    /// or inlined into an enclosing rule, which closes the
    /// inlined-rule detection gap (`pre_compile_rule_body` doesn't
    /// run for inlined rules in the driver, so `payload_types`
    /// cannot be relied on as a signal).
    pub string_decode_patterns: HashSet<StringId>,
    /// AU.3.1: map from `StringId` (regex pattern) to the rule's
    /// codegen `variant_idx` — the low 8 bits of `rule.id`. Resolves
    /// the decode-kernel push's `variant_idx` arg without relying on
    /// `pre_compile_rule_body` threading `current_rule_id`.
    pub string_decode_variant_idx: std::collections::HashMap<StringId, u8>,
}

impl RustEmitCtx {
    /// Create a new context with access to `IrCodegenCtx`.
    ///
    /// # Safety contract
    /// The caller must ensure `ir_ctx` outlives this `RustEmitCtx` and all
    /// emission calls that use it. This is guaranteed by `generate_all()` which
    /// creates `IrCodegenCtx` on the stack before emission and drops it after.
    pub fn new(ir_ctx: &IrCodegenCtx<'_>) -> Self {
        let (string_decode_patterns, string_decode_variant_idx) =
            collect_string_decode_patterns(ir_ctx.ir);
        Self {
            ir_ctx_ptr: ir_ctx as *const IrCodegenCtx<'_> as *const (),
            hoisted: Vec::new(),
            counter: 0,
            current_rule_name: None,
            current_rule_id: None,
            branch_idx_ident: None,
            tape_surgery: None,
            alt_context_stack: Vec::new(),
            payload_types: Vec::new(),
            payload_layout: None,
            aggregate_field_cursor: 0,
            string_decode_patterns,
            string_decode_variant_idx,
        }
    }

    /// AU.3.1: check whether a regex pattern (by string id) routes
    /// through the JSON string decode kernel. Called by
    /// `emit_regex_match_impl` — wins over the span-only fallback
    /// when set.
    pub fn is_string_decode_pattern(&self, sid: StringId) -> bool {
        self.string_decode_patterns.contains(&sid)
    }

    /// AU.3.1: look up the variant_idx for a string-decode pattern.
    /// Returns `0` when the pattern is not in the decode set (the
    /// caller should have gated on `is_string_decode_pattern` first).
    pub fn string_decode_variant_idx(&self, sid: StringId) -> u8 {
        self.string_decode_variant_idx
            .get(&sid)
            .copied()
            .unwrap_or(0)
    }

    /// AT.1: check whether a given scalar type should be captured
    /// into a payload variable. True when the type appears in the
    /// current rule's payload_types set.
    pub fn has_payload_type(&self, td: &TypeDesc) -> bool {
        self.payload_types.iter().any(|t| t == td)
    }


    /// AT.1: return the 1-based tag for a payload type in a multi-type
    /// Alt. Returns `None` when payload_types has at most one element
    /// (single-type path doesn't use tags). Returns `Some(tag)` when
    /// multi-type — the tag selects the right `push_leaf_with_<T>` in
    /// the epilogue.
    pub fn payload_tag(&self, td: &TypeDesc) -> Option<u8> {
        if self.payload_types.len() <= 1 {
            return None;
        }
        self.payload_types
            .iter()
            .position(|t| t == td)
            .map(|i| (i + 1) as u8)
    }

    /// AQ.6.B: advance the aggregate-field cursor and return a
    /// reference to the field at the previous position. Returns
    /// `None` when no aggregate layout is active or when the cursor
    /// has already consumed every field. Leaf-payload emitters call
    /// this in payload-write order so each scalar capture lands at
    /// the correct buffer offset.
    pub fn next_aggregate_field(
        &mut self,
    ) -> Option<bbnf_ir::passes::PayloadField> {
        let layout = self.payload_layout.as_ref()?;
        let idx = self.aggregate_field_cursor;
        let field = layout.fields.get(idx).cloned()?;
        self.aggregate_field_cursor = idx + 1;
        Some(field)
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

    pub fn fresh_lifetime(&mut self, prefix: &str) -> syn::Lifetime {
        let id = self.counter;
        self.counter += 1;
        syn::Lifetime::new(
            &format!("'__{}{}", prefix, id),
            proc_macro2::Span::call_site(),
        )
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

    /// AU.3.1: Check whether any outer Alt frame has tape surgery
    /// active. During a branch body compile, `tape_surgery` is moved
    /// from `self` onto the `alt_context_stack` by
    /// `save_alt_context`; the branch body sees `self.tape_surgery
    /// = None` but `alt_context_stack` has the saved state. This
    /// peek lets leaf emitters detect the outer-rule tape-surgery
    /// context without waiting for `restore_alt_context` (which only
    /// runs after every branch has compiled).
    pub fn outer_tape_surgery_active(&self) -> bool {
        if self.tape_surgery.is_some() {
            return true;
        }
        self.alt_context_stack
            .iter()
            .any(|(_, surgery)| surgery.is_some())
    }
}

// SAFETY: RustEmitCtx is only used single-threaded within a single compile_grammar call.
unsafe impl Send for RustEmitCtx {}
unsafe impl Sync for RustEmitCtx {}

/// AU.3.1: walk the grammar IR once, collecting the regex pattern
/// `StringId`s that route through the JSON string-decode kernel.
///
/// A pattern qualifies when it's the inner of a `Map { Regex(sid),
/// FnDescriptor::Expr { expr: FnCall { name: "decode_json_string_to_arena",
/// .. }, .. } }` rule body. Returns `(pattern_set, variant_idx_map)`
/// where `variant_idx_map` records the owning rule's codegen
/// variant_idx (`rule.id & 0xFF`) so the scan site can stamp it
/// into the decode-kernel push without needing a runtime
/// "current rule id" lookup.
///
/// One precompute at ctx construction beats either a thread-local
/// stack or per-trait-method rule threading. The set is small
/// (≤ 1 per grammar in practice) and lookup is O(1) via `HashSet`.
fn collect_string_decode_patterns(
    ir: &bbnf_ir::GrammarIR,
) -> (HashSet<StringId>, std::collections::HashMap<StringId, u8>) {
    let mut patterns = HashSet::new();
    let mut variant_idx_map = std::collections::HashMap::new();
    for rule in &ir.rules {
        let (regex_sid, fn_id) = match &rule.body {
            IrNode::Map { inner, fn_id } => match inner.as_ref() {
                IrNode::Regex(sid) => (*sid, *fn_id),
                _ => continue,
            },
            _ => continue,
        };
        let fn_desc = &ir.fns[fn_id as usize];
        let FnDescriptor::Expr { expr, .. } = fn_desc else {
            continue;
        };
        if !is_decode_json_string_expr(expr, ir) {
            continue;
        }
        let idx = (rule.id & 0xFF) as u8;
        patterns.insert(regex_sid);
        variant_idx_map.insert(regex_sid, idx);
    }
    (patterns, variant_idx_map)
}

/// AU.3.1: predicate on `MapExpr` — true iff the expression is a
/// top-level call to the `decode_json_string_to_arena` kernel.
///
/// The lowerer stores the function arg list as collected via
/// `collect_fn_call_args`, which structurally descends the tape's
/// value_expr subtrees. For `decode_json_string_to_arena(input)`
/// the arg list may lower to `[]` or `[Input]` depending on how
/// the tape rewrite folded the `input` atom — both are equivalent
/// to "the function operates on the parsed input." We accept
/// either shape.
fn is_decode_json_string_expr(expr: &MapExpr, ir: &bbnf_ir::GrammarIR) -> bool {
    let MapExpr::FnCall { name, args } = expr else {
        return false;
    };
    let name_str = ir.get_string(*name);
    if name_str != "decode_json_string_to_arena" {
        return false;
    }
    matches!(args.as_slice(), [] | [MapExpr::Input])
}
