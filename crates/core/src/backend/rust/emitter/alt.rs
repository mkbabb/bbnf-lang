//! Alternation emission for the Rust backend.
//!
//! Tranche AC.2 tape-first. An Alt returns `Option<()>` — the
//! chosen branch is a side-effecting sub-parse; the owning rule's
//! epilogue carries the variant discriminator if the Alt is the
//! rule head. When a nested Alt is inside a bigger compound, the
//! caller composes it like any other sub-parse.
//!
//! All three emission shapes (`dispatch`, `checkpoint`,
//! `all_literal`, `key_dispatch`) follow the same contract: each
//! branch body is `Option<()>` (or composable thereof), and the
//! Alt expression returns `Option<()>` via a labeled block.
//! Heterogeneous type coercion is moot because all branch bodies
//! share the same return shape under tape-first.
//!
//! ## AM.3 per-branch `mark_children`
//!
//! When `ctx.tape_surgery` is active (Alt-bodied `MustTape` rules),
//! compound branches prepend `__children = mark_children(tape);
//! __has_children = true;` before their body. Leaf branches skip
//! `mark_children`. The shared rule epilogue in `grammar.rs` checks
//! `__has_children` to choose `push_compound` vs `push_leaf`.
//!
//! The surgery context is `take()`-ed by the emitter so nested Alts
//! within branch bodies don't re-apply the mark — they compile as
//! normal `Option<()>` sub-expressions.

use bbnf_ir::passes::{PayloadField, PayloadLayout};
use bbnf_ir::{AltBranch, AltDispatch, FnDescriptor, GrammarIR, IrNode, MapExpr, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{AltBranchInfo, KeyClass, KeyDispatchBranch, KeyDispatchConfig, ValuePlacement};

use super::RustEmitter;
use super::RustEmitCtx;

impl RustEmitter {
    /// AM.3: emit `mark_children` + `__has_children = true` for
    /// compound branches. Returns empty tokens for leaf branches.
    fn emit_compound_mark(pushes_children: bool) -> TokenStream {
        if pushes_children {
            quote! {
                __children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
                __has_children = true;
            }
        } else {
            quote! {}
        }
    }

    pub(super) fn emit_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AM.3: take the surgery context so nested Alts within branch
        // bodies don't re-apply the per-branch mark_children.
        //
        // AU.6.5 no-value-discard: dispatch arms now produce natural
        // `Option<()>` / `Option<TapeOffset>` / `Option<Span>` types.
        // Each arm probes its body with `is_some()` and the labeled
        // block yields uniform `Option<()>` via `break #ad_blk
        // Some(())` on success — heterogeneous inner types no longer
        // force a per-call `.map(|_| ())` wrap.
        //
        // AV.0.1 Bug 1: dispatch-path counterpart of the alt-lit
        // composer fix. When an aggregate payload layout is active,
        // every dispatched branch whose IR node is
        // `Map { Literal, constant-MapExpr }` must emit its payload
        // write. The IR Alt is identified by dispatch-table equality
        // plus a full branch-signature match (every branch being a
        // literal-with-constant-map) to guarantee a unique
        // structural fingerprint across the rule set.
        let surgery = ctx.tape_surgery.take();
        let ad_blk = ctx.fresh_lifetime("ad_blk");
        let mut arms = Vec::new();
        let per_branch_writes = dispatch_per_branch_writes(table, branches.len(), ctx);

        for (branch_idx, (info, body)) in branches.iter().enumerate() {
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(byte_val, _)| byte_val as u8)
                .collect();

            if byte_patterns.is_empty() {
                continue;
            }

            // AK.1: when branch_idx_ident is set, prepend the branch
            // index assignment so the rule epilogue can use it as the
            // variant discriminator.
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                let idx = branch_idx as u8;
                quote! { #ident = #idx; }
            } else {
                quote! {}
            };

            // AM.3: compound branches prepend mark_children.
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };

            let patterns: Vec<_> = byte_patterns.iter().map(|b| quote! { #b }).collect();
            let wrapper = per_branch_writes
                .as_ref()
                .and_then(|w| w.get(branch_idx))
                .and_then(|w| w.as_ref());
            let probe = match wrapper {
                Some(buf_setter) => quote! {
                    match ({ #body }) {
                        Some(_) => {
                            #buf_setter;
                            __has_payload = true;
                            Some(())
                        }
                        None => None,
                    }
                },
                None => quote! { #body },
            };
            arms.push(quote! {
                #( #patterns )|* => {
                    #branch_assign
                    #compound_mark
                    if #probe.is_some() {
                        break #ad_blk Some(());
                    }
                }
            });
        }

        let fallback_stmt = if let Some((info, fb_body)) = fallback {
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            quote! {
                {
                    #compound_mark
                    if #fb_body.is_some() {
                        break #ad_blk Some(());
                    }
                }
            }
        } else {
            quote! { {} }
        };

        // `_ => #fallback_stmt` and the EOF branch share the same
        // statement — the fallback runs in either case.
        arms.push(quote! { _ => #fallback_stmt });

        quote! {
            #ad_blk: {
                if state.offset < state.src_bytes.len() {
                    match state.src_bytes[state.offset] {
                        #( #arms ),*
                    }
                } else {
                    #fallback_stmt
                }
                None
            }
        }
    }

    pub(super) fn emit_alt_checkpoint_impl(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AM.3: take the surgery context so nested Alts don't re-apply.
        //
        // AU.6.5 no-value-discard: each branch body now emits a
        // natural `Option<T>` shape. The labeled block yields
        // `Option<()>` via `break ... Some(())` on success (tested
        // with `is_some()`), so heterogeneous inner types do not
        // force a `.map(|_| ())` wrap at each emit site.
        //
        // AV.0.1 Bug 1 (close-out): checkpoint counterpart of the
        // alt-lit and dispatch composer fixes. When the rule has an
        // active aggregate payload layout and at least one branch is a
        // direct `Map { Literal, constant-MapExpr }` shape, hoist the
        // per-branch payload-write onto each arm. Mixed-branch Alts —
        // `error_literal`'s outer Alt mixes direct literal branches
        // with a factored `Seq(Literal("N"), inner-alt-lit)` branch —
        // get per-branch writes only on the direct-literal arms; the
        // factored arms keep their existing inner alt-lit writes from
        // the `emit_alt_all_literal_impl` fix landed earlier.
        let surgery = ctx.tape_surgery.take();
        let alt_blk = ctx.fresh_lifetime("alt_blk");
        let per_branch_writes = checkpoint_per_branch_writes(&branches, ctx);

        if branches.len() == 1 {
            let (ref info, ref body) = branches[0];
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                quote! { #ident = 0u8; }
            } else {
                quote! {}
            };
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            if !branch_assign.is_empty() || !compound_mark.is_empty() {
                return quote! { { #branch_assign #compound_mark #body } };
            }
            return body.clone();
        }

        let mut chain = Vec::new();
        for (i, (info, body)) in branches.iter().enumerate() {
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                let idx = i as u8;
                quote! { #ident = #idx; }
            } else {
                quote! {}
            };
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            let wrapper = per_branch_writes
                .as_ref()
                .and_then(|w| w.get(i))
                .and_then(|w| w.as_ref());
            let probe = match wrapper {
                Some(buf_setter) => quote! {
                    match ({ #body }) {
                        Some(_) => {
                            #buf_setter;
                            __has_payload = true;
                            Some(())
                        }
                        None => None,
                    }
                },
                None => quote! { #body },
            };
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    #branch_assign
                    #compound_mark
                    if #probe.is_some() {
                        break #alt_blk Some(());
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            #alt_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_alt_all_literal_impl(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AU.6.5 no-value-discard: literal-Alt branches probe success
        // via `is_some()` and yield the label's uniform `Option<()>`
        // — avoiding the prior `break __r` which forced the body's
        // natural type onto the label break.
        //
        // AV.0.1 Bug 1: when an aggregate payload layout is active for
        // the enclosing rule, each branch of the all-literal Alt should
        // write its declared return value into the same aggregate
        // buffer slot. The driver compiles each branch sequentially
        // with a shared `aggregate_field_cursor`; the first branch
        // consumes the field and the remaining branches fall through
        // to side-effect-only emission. Repair this in the alt-lit
        // composer: look up each branch's `MapExpr` in the IR, and
        // wrap every body with its own payload-write. Branch 0's
        // existing write becomes idempotent; branches 1+ gain the
        // previously missing write.
        let alt_lit_blk = ctx.fresh_lifetime("alt_lit_blk");
        let per_branch_writes = alt_lit_per_branch_writes(&literals, ctx);
        let mut chain = Vec::new();
        for (idx, (_value, body)) in literals.iter().enumerate() {
            let arm = match per_branch_writes.as_ref().and_then(|w| w.get(idx)) {
                Some(Some(buf_setter)) => quote! {
                    {
                        if match ({ #body }) {
                            Some(_) => {
                                #buf_setter;
                                __has_payload = true;
                                Some(())
                            }
                            None => None,
                        }
                        .is_some()
                        {
                            break #alt_lit_blk Some(());
                        }
                    }
                },
                _ => quote! {
                    {
                        if #body.is_some() { break #alt_lit_blk Some(()); }
                    }
                },
            };
            chain.push(arm);
        }
        quote! {
            #alt_lit_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_key_dispatch_impl(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // `fresh_lifetime` so nested key-dispatch blocks within a
        // single rule body each get a unique label.
        //
        // AU.6.5 no-value-discard: each arm probes its body with
        // `is_some()` and yields the block's uniform `Option<()>`
        // via `break ... Some(())`. The fallback coerces through an
        // `is_some()` probe in the same shape.
        let cp = ctx.fresh("kd_cp");
        let kd_blk = ctx.fresh_lifetime("kd_blk");
        let scanner = match config.key_class {
            KeyClass::Identifier => quote! {
                ::parse_that::scan_ident(state, &::parse_that::DEFAULT_IDENT_CONFIG)
            },
            KeyClass::QuotedString { .. } => quote! { ::parse_that::scan_string_quoted(state) },
        };
        let arm_checks: Vec<TokenStream> = branches
            .into_iter()
            .map(|kd| {
                let comparisons: Vec<TokenStream> = kd
                    .key_bytes
                    .iter()
                    .map(|key| {
                        let byte_lits: Vec<proc_macro2::Literal> = key
                            .iter()
                            .map(|b| proc_macro2::Literal::byte_character(*b))
                            .collect();
                        let len = key.len();
                        quote! { (__kd_len == #len && __kd_bytes == &[#(#byte_lits),*]) }
                    })
                    .collect();
                let body = kd.body;
                quote! {
                    if #(#comparisons)||* {
                        state.offset = #cp;
                        if #body.is_some() {
                            break #kd_blk Some(());
                        }
                        break #kd_blk None;
                    }
                }
            })
            .collect();
        let fallback_expr = if let Some((_info, fb)) = fallback {
            quote! {
                if #fb.is_some() { Some(()) } else { None }
            }
        } else {
            quote! { None }
        };
        quote! {
            #kd_blk: {
                let #cp = state.offset;
                if let Some(ref __kd_s) = #scanner {
                    let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                    let __kd_len = __kd_bytes.len();
                    #(#arm_checks)*
                }
                state.offset = #cp;
                break #kd_blk #fallback_expr;
            }
        }
    }
}

/// AV.0.1: for an all-literal Alt inside a rule with an active
/// aggregate payload layout, compute the per-branch aggregate-buffer
/// write that the alt-lit composer must hoist onto every arm.
///
/// Returns `None` when no layout is active, the enclosing rule body
/// cannot be resolved, or the matching IR Alt cannot be located
/// unambiguously — in those cases the caller falls back to bare
/// emission (no regression versus the pre-AV shape).
///
/// When the Alt is located, returns one entry per `literals` branch.
/// An inner `None` inside the outer `Some` means the branch's
/// `MapExpr` could not be coerced to a constant setter for the
/// target field (non-constant map, type mismatch) — the caller
/// leaves that branch's body unwrapped.
fn alt_lit_per_branch_writes(
    literals: &[(String, TokenStream)],
    ctx: &RustEmitCtx,
) -> Option<Vec<Option<TokenStream>>> {
    let layout = ctx.payload_layout.as_ref()?;
    let ir = ctx.ir_ctx().ir;

    // Search every rule body for the matching Alt. `current_rule_id`
    // is unset on the Rust emitter's ctx, so the identification goes
    // by literal-value signature. `values` is the ordered literal set
    // the alt-lit composer received; an Alt whose branches' literals
    // match in the same order is the unambiguous target. Duplicate
    // signatures across different rules would be ambiguous, but in
    // the AV.0.1 target set (rule-scoped payload layouts, small
    // factored alt-lit groups) the signatures are unique.
    let values: Vec<&str> = literals.iter().map(|(s, _)| s.as_str()).collect();
    let (alt_branches, field) = ir
        .rules
        .iter()
        .find_map(|rule| locate_alt_and_field(&rule.body, &values, ir, layout))?;

    let mut writes = Vec::with_capacity(alt_branches.len());
    for branch in alt_branches {
        writes.push(branch_payload_write(&branch.node, ir, &field));
    }
    Some(writes)
}

/// AV.0.1 (close-out): checkpoint-alt counterpart of
/// [`alt_lit_per_branch_writes`] and [`dispatch_per_branch_writes`].
///
/// The checkpoint composer handles mixed-branch Alts where branches
/// are heterogeneous — Sheets `error_literal`'s outer Alt is the
/// canonical case: direct `Map { Literal(s), IntLit(v) }` branches
/// (`#VALUE!` → 1, `#REF!` → 2, `#DIV/0!` → 3, `#ERROR!` → 7,
/// `#SPILL!` → 8) mixed with a factored `Seq(Literal("N"),
/// inner-alt-lit)` branch that already carries its own per-branch
/// writes via the alt-lit composer fix.
///
/// The structural identifier is relaxed versus the dispatch helper:
/// a branch counts as payload-eligible when its IR node is
/// `Map { Literal, constant-MapExpr }` (direct shape). Factored
/// `Seq` branches — where the MapExprs live on the inner Alt's
/// branches — are identified by falling through to a `None` entry;
/// the checkpoint composer then leaves that arm's body unwrapped
/// and the inner Alt's existing per-branch writes fire normally.
///
/// Uniqueness: the IR scan matches on
///   * exact branch count,
///   * at least one branch being `Map { Literal, constant-MapExpr }`,
///   * every `Map { Literal, constant }` branch's MapExpr target
///     type coercing to the active single-field layout's scalar.
///
/// The type-match constraint pins the search to Alts whose payload
/// shape agrees with the caller's rule — the canonical
/// `error_literal` case has a 1-byte U8 layout and every literal
/// branch's IntLit coerces to U8. Unrelated 7-branch Alts in the
/// grammar with non-U8 targets are filtered out.
fn checkpoint_per_branch_writes(
    branches: &[(AltBranchInfo, TokenStream)],
    ctx: &RustEmitCtx,
) -> Option<Vec<Option<TokenStream>>> {
    let layout = ctx.payload_layout.as_ref()?;
    if layout.fields.len() != 1 {
        return None;
    }
    let field = layout.fields[0].clone();
    let ir = ctx.ir_ctx().ir;
    let branch_count = branches.len();
    let alt_branches = ir.rules.iter().find_map(|rule| {
        locate_checkpoint_alt(&rule.body, branch_count, ir, &field)
    })?;
    let mut writes = Vec::with_capacity(alt_branches.len());
    for branch in alt_branches {
        writes.push(branch_payload_write(&branch.node, ir, &field));
    }
    Some(writes)
}

/// Walk `root` looking for an `IrNode::Alt` matching:
///   * branch count equal to `branch_count`,
///   * every direct-literal branch (`Map { Literal, constant MapExpr }`)
///     coerces to `field.ty` — so non-`U8` Alts with the same branch
///     count are excluded,
///   * at least one branch being the direct-literal shape (versus a
///     factored `Seq`), guaranteeing we have a payload-write to
///     emit somewhere.
///
/// Factored-Seq branches (e.g. `Seq(Literal("N"), inner-alt-lit)`)
/// are admitted as "non-literal" positions — the inner Alt's
/// alt-lit composer already emits per-branch writes, and the outer
/// wrapper intentionally leaves those arms unwrapped.
fn locate_checkpoint_alt<'ir>(
    root: &'ir IrNode,
    branch_count: usize,
    ir: &'ir GrammarIR,
    field: &PayloadField,
) -> Option<&'ir [AltBranch]> {
    if let IrNode::Alt(branches, _) = root {
        if branches.len() == branch_count
            && branches.iter().any(|b| is_literal_with_constant_map(&b.node, ir))
            && branches
                .iter()
                .all(|b| checkpoint_branch_is_compatible(&b.node, ir, field))
        {
            return Some(branches.as_slice());
        }
    }
    for child in node_children(root) {
        if let Some(m) = locate_checkpoint_alt(child, branch_count, ir, field) {
            return Some(m);
        }
    }
    None
}

/// Predicate: a checkpoint branch is compatible with `field` when
/// either
///   * it is a direct `Map { Literal, constant-MapExpr }` whose
///     MapExpr coerces to `field.ty` (verified by the same
///     `aggregate_constant_setter` the per-branch write emitter uses
///     — a `None` return means the MapExpr type does not match the
///     field, so this is NOT the target Alt), or
///   * it is a non-direct-literal shape (factored Seq, nested Alt,
///     etc.) — we leave those arms alone and trust their own
///     composer to emit writes if needed.
fn checkpoint_branch_is_compatible(
    node: &IrNode,
    ir: &GrammarIR,
    field: &PayloadField,
) -> bool {
    if is_literal_with_constant_map(node, ir) {
        branch_payload_write(node, ir, field).is_some()
    } else {
        true
    }
}

/// AV.0.1: dispatch-alt counterpart of [`alt_lit_per_branch_writes`].
/// The dispatch composer receives `(AltBranchInfo, TokenStream)`
/// pairs — no literal signature threads through — so the IR Alt is
/// identified by combining dispatch-table equality, branch count,
/// and a structural signature requiring every branch to be a
/// `Map { Literal, constant-MapExpr }` node. Inlined rules whose
/// bodies are rewritten out of the IR cannot be located this way;
/// the helper returns `None` and the caller falls back to the
/// pre-fix emission for those sites (addressed by a separate
/// inline-aware payload pass outside AV.0.1's scope).
fn dispatch_per_branch_writes(
    table: &AltDispatch,
    branch_count: usize,
    ctx: &RustEmitCtx,
) -> Option<Vec<Option<TokenStream>>> {
    let layout = ctx.payload_layout.as_ref()?;
    if layout.fields.len() != 1 {
        return None;
    }
    let field = layout.fields[0].clone();
    let ir = ctx.ir_ctx().ir;
    let alt_branches = ir
        .rules
        .iter()
        .find_map(|rule| locate_dispatch_alt(&rule.body, table, branch_count, ir))?;
    let mut writes = Vec::with_capacity(alt_branches.len());
    for branch in alt_branches {
        writes.push(branch_payload_write(&branch.node, ir, &field));
    }
    Some(writes)
}

/// Walk `root` looking for an `IrNode::Alt` matching:
///   * branch count equal to `branch_count`,
///   * dispatch table byte-for-byte equal to `table`,
///   * every branch structured as `Map { Literal, constant MapExpr }`
///     (the all-literal-with-constant shape Bug 1 targets).
///
/// The structural constraint rules out coincidental matches against
/// unrelated Alts whose dispatch tables may happen to overlap.
fn locate_dispatch_alt<'ir>(
    root: &'ir IrNode,
    table: &AltDispatch,
    branch_count: usize,
    ir: &'ir GrammarIR,
) -> Option<&'ir [AltBranch]> {
    if let IrNode::Alt(branches, dispatch) = root {
        if branches.len() == branch_count {
            if let Some(t) = dispatch {
                if t == table
                    && branches.iter().all(|b| is_literal_with_constant_map(&b.node, ir))
                {
                    return Some(branches.as_slice());
                }
            }
        }
    }
    for child in node_children(root) {
        if let Some(m) = locate_dispatch_alt(child, table, branch_count, ir) {
            return Some(m);
        }
    }
    None
}

/// Predicate: `node` is `Map { Literal, FnDescriptor::Expr { expr } }`
/// where `expr` is a constant `MapExpr` (`IntLit`, `FloatLit`,
/// `BoolLit`, `StringLit`). These are the exact shapes the driver's
/// all-literal detector recognises and the Bug 1 fix targets.
fn is_literal_with_constant_map(node: &IrNode, ir: &GrammarIR) -> bool {
    let fn_id = match node {
        IrNode::Map { inner, fn_id } if matches!(inner.as_ref(), IrNode::Literal(_)) => *fn_id,
        _ => return false,
    };
    match &ir.fns[fn_id as usize] {
        FnDescriptor::Expr { expr, .. } => expr.is_constant(),
        _ => false,
    }
}

/// Walk `root` looking for an `IrNode::Alt` whose branch literals
/// match `values` in declaration order. Returns the `AltBranch`
/// slice and the `PayloadField` the branches are expected to fill.
///
/// The field is the one consumed by the first-to-compile branch. In
/// the all-literal contract every branch writes a single scalar at
/// the same offset, so any branch's consumption identifies the
/// shared field. The cursor at `emit_alt_all_literal_impl` entry
/// points one past that field, so the target is
/// `layout.fields[cursor - 1]`.
fn locate_alt_and_field<'ir>(
    root: &'ir IrNode,
    values: &[&str],
    ir: &'ir GrammarIR,
    layout: &'ir PayloadLayout,
) -> Option<(&'ir [AltBranch], PayloadField)> {
    fn find<'a>(node: &'a IrNode, values: &[&str], ir: &GrammarIR) -> Option<&'a [AltBranch]> {
        if let IrNode::Alt(branches, _) = node {
            if branches.len() == values.len()
                && branches
                    .iter()
                    .zip(values.iter())
                    .all(|(b, v)| alt_branch_literal(&b.node, ir) == Some(*v))
            {
                return Some(branches.as_slice());
            }
        }
        for child in node_children(node) {
            if let Some(m) = find(child, values, ir) {
                return Some(m);
            }
        }
        None
    }
    let alt_branches = find(root, values, ir)?;
    // Field index: the monotonic cursor is one past the field the
    // first branch consumed. The layout stores field count; guard
    // against underflow by requiring at least one field.
    let field_idx = layout.fields.len().checked_sub(1)?;
    let field = layout.fields.get(field_idx).cloned()?;
    // Accept only layouts with exactly one field — multi-field
    // layouts mean the Alt sits inside a Seq with other cursor
    // consumers and the single-cursor-step assumption doesn't
    // hold. Those rules (none in the AV.0.1 target set) fall
    // through to the pre-fix emission.
    if layout.fields.len() != 1 {
        return None;
    }
    Some((alt_branches, field))
}

/// Iterate direct children of an `IrNode` for structural descent.
fn node_children(node: &IrNode) -> Vec<&IrNode> {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => Vec::new(),
        IrNode::Seq(cs) => cs.iter().collect(),
        IrNode::Alt(bs, _) => bs.iter().map(|b| &b.node).collect(),
        IrNode::Repeat { inner, .. } => vec![inner.as_ref()],
        IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
            vec![l.as_ref(), r.as_ref()]
        }
        IrNode::Negate(inner) => vec![inner.as_ref()],
        IrNode::Map { inner, .. } => vec![inner.as_ref()],
        IrNode::OptionalWhitespace(inner) => vec![inner.as_ref()],
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            let mut children: Vec<&IrNode> = Vec::with_capacity(2 + arms.len());
            children.push(token.as_ref());
            children.push(fallback.as_ref());
            for arm in arms {
                children.push(&arm.continuation);
            }
            children
        }
    }
}

/// Extract the literal string backing an Alt branch's node, when it
/// is a bare literal or a `Map { Literal, .. }` wrapper. Returns
/// `None` for any other shape.
fn alt_branch_literal<'ir>(node: &'ir IrNode, ir: &'ir GrammarIR) -> Option<&'ir str> {
    match node {
        IrNode::Literal(sid) => Some(ir.get_string(*sid)),
        IrNode::Map { inner, .. } => match inner.as_ref() {
            IrNode::Literal(sid) => Some(ir.get_string(*sid)),
            _ => None,
        },
        _ => None,
    }
}

/// Compose the per-branch aggregate-buffer write for `branch_node`
/// writing into `field`. Returns `None` when the branch's `MapExpr`
/// is not a constant compatible with the field's type — the caller
/// leaves that arm unwrapped.
fn branch_payload_write(
    branch_node: &IrNode,
    ir: &GrammarIR,
    field: &PayloadField,
) -> Option<TokenStream> {
    let fn_id = match branch_node {
        IrNode::Map { fn_id, .. } => *fn_id,
        _ => return None,
    };
    let expr = match &ir.fns[fn_id as usize] {
        FnDescriptor::Expr { expr, .. } => expr,
        _ => return None,
    };
    aggregate_constant_setter(&field, expr)
}

/// Mirror of `aggregate_constant_setter` in `map_value.rs`: emit a
/// byte-level write into `__aggregate_buf` at the field's layout
/// offset for a constant `MapExpr`. Kept in-module so the alt-lit
/// composer is self-contained; the two sites stay in lockstep via
/// the `PayloadField` / `MapExpr` surface they share.
fn aggregate_constant_setter(field: &PayloadField, expr: &MapExpr) -> Option<TokenStream> {
    let offset = field.offset as usize;
    let size = field.ty.payload_size_bytes()? as usize;
    let value_expr: TokenStream = match (&field.ty, expr) {
        (TypeDesc::Bool, MapExpr::BoolLit(v)) => {
            let val = *v as u8;
            quote! { [#val] }
        }
        (TypeDesc::I8, MapExpr::IntLit(v)) => {
            let val = *v as i8;
            quote! { (#val as i8).to_le_bytes() }
        }
        (TypeDesc::U8, MapExpr::IntLit(v)) => {
            let val = (*v & 0xFF) as u8;
            quote! { [#val] }
        }
        (TypeDesc::I16, MapExpr::IntLit(v)) => {
            let val = *v as i16;
            quote! { (#val as i16).to_le_bytes() }
        }
        (TypeDesc::U16, MapExpr::IntLit(v)) => {
            let val = *v as u16;
            quote! { (#val as u16).to_le_bytes() }
        }
        (TypeDesc::I32, MapExpr::IntLit(v)) => {
            let val = *v as i32;
            quote! { (#val as i32).to_le_bytes() }
        }
        (TypeDesc::U32, MapExpr::IntLit(v)) => {
            let val = *v as u32;
            quote! { (#val as u32).to_le_bytes() }
        }
        (TypeDesc::I64, MapExpr::IntLit(v)) => {
            let val = *v;
            quote! { (#val as i64).to_le_bytes() }
        }
        (TypeDesc::U64, MapExpr::IntLit(v)) => {
            let val = *v as u64;
            quote! { (#val as u64).to_le_bytes() }
        }
        (TypeDesc::F64, MapExpr::FloatLit(v)) => {
            let val = *v;
            quote! { (#val as f64).to_le_bytes() }
        }
        _ => return None,
    };
    let end = offset + size;
    Some(quote! {
        __aggregate_buf[#offset..#end].copy_from_slice(&#value_expr)
    })
}
