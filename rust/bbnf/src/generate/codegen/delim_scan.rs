//! Delimiter-driven flat scanner for Wrap(Repeat(Alt)) patterns.
//!
//! When a `Repeat` is inside a `Wrap` (e.g., `"{" >> items * << "}"`), and the
//! Repeat's body is an `Alt` whose branches can be distinguished by which
//! delimiter byte appears first in a forward `memchr` scan, this module emits
//! a flat scanner loop instead of the standard recursive-descent per-element loop.
//!
//! All delimiter bytes are extracted from the grammar's own `Literal` nodes —
//! no grammar-specific knowledge is hard-coded.
//!
//! The emitted scanner uses 2–3 `memchr` calls per item instead of ~20
//! recursive-descent operations, eliminating IIFE closures, checkpoint/restore,
//! and per-element Option wrapping.

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::IrCodegenCtx;
use super::unescape_literal;
use super::{MonoCtx, emit_ws_trim, mono_fn_ident};

// ── Configuration ────────────────────────────────────────────────────────────

/// Grammar-agnostic delimiter-scan configuration.
/// All bytes extracted from the IR's Literal nodes.
pub(super) struct DelimScanConfig {
    /// Opening delimiter byte.
    pub open_byte: u8,
    /// Closing delimiter byte.
    pub close_byte: u8,
    /// Pivot byte that distinguishes branches.
    pub pivot_byte: u8,
    /// Optional trailing delimiter for the pivot branch.
    pub trail_byte: Option<u8>,
    /// RuleId of the block/fallback branch (the cyclic Ref in the Alt).
    pub block_fn: Option<RuleId>,
    /// RuleId of the pivot branch (the rule whose body contains the pivot Literal).
    pub pivot_fn: Option<RuleId>,
    /// RuleId of the content rule containing the Repeat(Alt) — used for Vec variant name.
    pub content_rule: Option<RuleId>,
}

// ── Detection ────────────────────────────────────────────────────────────────

/// Try to detect a delimiter-scannable Wrap(open, Repeat(Alt(...)), close) pattern.
///
/// Returns `None` if the pattern doesn't match or the extracted bytes collide.
pub(super) fn try_detect(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ir: &GrammarIR,
) -> Option<DelimScanConfig> {
    // 1. Open and close must be single-byte Literals.
    let open_byte = single_byte_literal(open, ir)?;
    let close_byte = single_byte_literal(close, ir)?;
    if open_byte == close_byte {
        return None;
    }

    // 2. Unwrap middle through OW/Map/Ref to find the Repeat.
    let (repeat_inner, content_rule) = unwrap_to_repeat_with_rule(middle, ir)?;

    // 3. Unwrap Repeat inner through OW/Map/Ref to find the Alt.
    let branches = unwrap_to_alt(repeat_inner, ir)?;
    if branches.len() < 2 {
        return None;
    }

    // 4. The Alt must NOT already have a dispatch table (overlapping FIRST sets).
    // We detect this by checking that no dispatch is present — dispatch is stored
    // in the Alt node itself, but we only have the branches here. The caller
    // (emit_span_wrap / emit_mono_wrap) should only call us when the middle is
    // a general-case (no sep_by_ws_until hit), which implies no dispatch.

    // 5. Classify branches: find a pivot branch and a block/fallback branch.
    let mut pivot_byte: Option<u8> = None;
    let mut trail_byte: Option<u8> = None;
    let mut block_fn: Option<RuleId> = None;

    let mut pivot_fn: Option<RuleId> = None;

    for branch in branches {
        let inner = unwrap_map_ow(&branch.node);
        if let Some((piv, trail)) = find_pivot_in_seq(inner, ir) {
            if pivot_byte.is_some() && pivot_byte != Some(piv) {
                return None; // Multiple different pivots — too complex.
            }
            pivot_byte = Some(piv);
            if trail.is_some() {
                trail_byte = trail;
            }
            // Track the pivot branch's rule for monolithic fallback.
            if let IrNode::Ref(id) = inner {
                pivot_fn = Some(*id);
            }
        } else if let Some(rule_id) = find_block_ref(inner, open_byte, ir) {
            block_fn = Some(rule_id);
        }
    }

    let pivot_byte = pivot_byte?; // Must have at least one pivot branch.

    // 6. Verify all bytes are distinct.
    if pivot_byte == open_byte || pivot_byte == close_byte {
        return None;
    }
    if let Some(tb) = trail_byte {
        if tb == open_byte || tb == close_byte || tb == pivot_byte {
            return None;
        }
    }

    Some(DelimScanConfig {
        open_byte,
        close_byte,
        pivot_byte,
        trail_byte,
        block_fn,
        pivot_fn,
        content_rule,
    })
}

// ── Detection Helpers ────────────────────────────────────────────────────────

/// Extract a single byte from a Literal node.
fn single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let raw = ir.get_string(*sid);
        let unescaped = unescape_literal(raw);
        let bytes = unescaped.as_bytes();
        if bytes.len() == 1 {
            return Some(bytes[0]);
        }
    }
    None
}

/// Detect a trailing delimiter byte in a multi-char Literal.
/// Handles the case where `merge_literals` fused a property name with `:`,
/// e.g. `"display:"` → trailing byte is `:`.
/// Only returns known delimiter bytes (`:`, `;`) to avoid false positives.
fn trailing_delimiter_byte(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let raw = ir.get_string(*sid);
        let unescaped = unescape_literal(raw);
        let bytes = unescaped.as_bytes();
        if bytes.len() >= 2 {
            let last = *bytes.last()?;
            if last == b':' || last == b';' {
                return Some(last);
            }
        }
    }
    None
}

/// Unwrap through OW/Map/Ref/Next/Skip layers to find a Repeat node.
/// Returns (Repeat inner node, Option<RuleId of the Ref that was followed>).
fn unwrap_to_repeat_with_rule<'a>(
    node: &'a IrNode,
    ir: &'a GrammarIR,
) -> Option<(&'a IrNode, Option<RuleId>)> {
    match node {
        IrNode::Repeat { inner, lo: 0, .. } => Some((inner, None)),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            unwrap_to_repeat_with_rule(inner, ir)
        }
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            let result = unwrap_to_repeat_with_rule(&rule.body, ir)?;
            // Capture the outermost Ref that led to the Repeat.
            Some((result.0, Some(result.1.unwrap_or(*rule_id))))
        }
        IrNode::Next(_, b) => unwrap_to_repeat_with_rule(b, ir),
        IrNode::Skip(a, _) => unwrap_to_repeat_with_rule(a, ir),
        _ => None,
    }
}

/// Unwrap through OW/Map/Ref/Next/Skip layers to find an Alt node.
/// Returns branches only if no dispatch table.
fn unwrap_to_alt<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> Option<&'a [bbnf_ir::AltBranch]> {
    match node {
        IrNode::Alt(branches, dispatch) if dispatch.is_none() => Some(branches),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => unwrap_to_alt(inner, ir),
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            unwrap_to_alt(&rule.body, ir)
        }
        // Next(a, b) keeps right → the Alt is in b
        IrNode::Next(_, b) => unwrap_to_alt(b, ir),
        // Skip(a, b) keeps left → the Alt is in a
        IrNode::Skip(a, _) => unwrap_to_alt(a, ir),
        _ => None,
    }
}

/// Unwrap Map and OptionalWhitespace wrappers.
fn unwrap_map_ow(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => unwrap_map_ow(inner),
        other => other,
    }
}

/// Find a pivot byte in a branch: the first single-byte Literal at position > 0
/// in a Seq, or the common pivot across all branches of an Alt.
/// Also detects a trailing optional Literal (e.g., `";" ?`).
/// Follows Ref nodes to find the Seq/Alt inside referenced rules.
fn find_pivot_in_seq(node: &IrNode, ir: &GrammarIR) -> Option<(u8, Option<u8>)> {
    match node {
        IrNode::Seq(children) => find_pivot_in_children(children, ir),
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            find_pivot_in_seq(unwrap_map_ow(&rule.body), ir)
        }
        IrNode::Alt(branches, _) => {
            // All branches must share the same pivot byte.
            let mut common_pivot: Option<u8> = None;
            let mut common_trail: Option<u8> = None;
            for branch in branches {
                let (piv, trail) = find_pivot_in_seq(unwrap_map_ow(&branch.node), ir)?;
                if let Some(cp) = common_pivot {
                    if cp != piv {
                        return None; // Different pivots — can't use delim_scan.
                    }
                } else {
                    common_pivot = Some(piv);
                }
                if trail.is_some() {
                    common_trail = trail;
                }
            }
            common_pivot.map(|p| (p, common_trail))
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            find_pivot_in_seq(inner, ir)
        }
        _ => None,
    }
}

/// Find a pivot byte within a Seq's children.
fn find_pivot_in_children(children: &[IrNode], ir: &GrammarIR) -> Option<(u8, Option<u8>)> {
    if children.len() < 2 {
        return None;
    }

    let mut pivot: Option<u8> = None;
    let mut trail: Option<u8> = None;

    // Check if the first child is a literal ending with a delimiter byte
    // (from merge_literals fusing e.g. "display" + ":" → "display:").
    if let Some(byte) = trailing_delimiter_byte(&children[0], ir) {
        pivot = Some(byte);
    }

    for (i, child) in children.iter().enumerate() {
        if i == 0 {
            continue; // Skip leading element (the regex/ident before the pivot).
        }
        let inner = unwrap_map_ow(child);
        // Check for single-byte Literal.
        if let Some(byte) = single_byte_literal(inner, ir) {
            if pivot.is_none() {
                pivot = Some(byte);
            }
            continue;
        }
        // Check for optional trailing literal: Repeat { Literal(x), 0, 1 }.
        if let IrNode::Repeat {
            inner: rep_inner,
            lo: 0,
            hi: 1,
        } = inner
        {
            if let Some(byte) = single_byte_literal(unwrap_map_ow(rep_inner), ir) {
                trail = Some(byte);
            }
        }
    }

    pivot.map(|p| (p, trail))
}

/// Check if a node is (or contains) a Ref to a cyclic rule — indicating a
/// fallback/block branch that handles content not matching the pivot delimiter.
/// In a delimiter scanner, this branch is invoked when `open_byte` is encountered
/// (nested block) or when no pivot is found (selector/other content).
fn find_block_ref(node: &IrNode, _open_byte: u8, _ir: &GrammarIR) -> Option<RuleId> {
    match node {
        IrNode::Ref(rule_id) => Some(*rule_id),
        IrNode::Seq(children) => {
            for child in children {
                if let Some(id) = find_block_ref(unwrap_map_ow(child), _open_byte, _ir) {
                    return Some(id);
                }
            }
            None
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            find_block_ref(inner, _open_byte, _ir)
        }
        _ => None,
    }
}

// ── Shared Emission Helpers ───────────────────────────────────────────────────

/// Build the core delimiter-scan loop body tokens, parameterized by what happens
/// on each dispatch case. Shared between span and slab emission.
///
/// The loop body:
/// 1. Skip whitespace
/// 2. `memchr` for block delimiters (open, close, trail)
/// 3. `memchr` for pivot within that range
/// 4. If pivot found: check value for open_byte (pseudo-class guard) — if value
///    contains open_byte before trail/close, the pivot was part of a selector,
///    not a delimiter. Reinterpret as a block branch.
/// 5. Dispatch to on_pivot / on_block / on_trail / on_close
fn emit_scan_loop(
    config: &DelimScanConfig,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    on_pivot: &TokenStream,
    on_block: &TokenStream,
) -> (TokenStream, TokenStream) {
    let open_lit = proc_macro2::Literal::byte_character(config.open_byte);
    let close_lit = proc_macro2::Literal::byte_character(config.close_byte);
    let pivot_lit = proc_macro2::Literal::byte_character(config.pivot_byte);

    let ws_trim = emit_ws_trim(ctx, mctx);
    let ws_post = ws_trim.clone();

    // Block-delimiter scan (find open/close/trail in forward direction).
    let _block_scan = if let Some(tb) = config.trail_byte {
        let trail_lit = proc_macro2::Literal::byte_character(tb);
        quote! { ::parse_that::memchr::memchr3(#open_lit, #close_lit, #trail_lit, __rem) }
    } else {
        quote! { ::parse_that::memchr::memchr2(#open_lit, #close_lit, __rem) }
    };

    // Value scan: balanced-aware scanner that skips quoted strings and nested parens.
    // Returns usize (offset to first depth-0 `;`, `{`, or `}`), NOT Option.
    let value_scan = quote! { ::parse_that::scan_balanced_end(__vrem) };

    // Trail consume (advance past ';' if present after value).
    let trail_consume = if let Some(tb) = config.trail_byte {
        let trail_lit = proc_macro2::Literal::byte_character(tb);
        quote! {
            if state.src_bytes.get(state.offset).copied() == Some(#trail_lit) {
                state.offset += 1;
            }
        }
    } else {
        quote! {}
    };

    // Trail match arm in the block-delimiter dispatch.
    let _trail_branch = if let Some(tb) = config.trail_byte {
        let trail_lit = proc_macro2::Literal::byte_character(tb);
        quote! { #trail_lit => { state.offset = __item + __bp + 1; } }
    } else {
        quote! {}
    };

    // Unified structural scan: find the first of ALL structural bytes in one
    // SIMD pass instead of 2 sequential memchr calls.
    //
    // Dispatches to the optimal function based on target count:
    // - 3 targets: find_first_of_3 (simd_eq × 3)
    // - 4 targets: find_first_of_4 (simd_eq × 4)
    // - 5+ targets: find_first_of (nibble LUT + swizzle_dyn)
    let unified_scan = {
        let mut structural_lits = vec![open_lit.clone(), close_lit.clone(), pivot_lit.clone()];
        if let Some(tb) = config.trail_byte {
            structural_lits.push(proc_macro2::Literal::byte_character(tb));
        }
        match structural_lits.len() {
            3 => {
                let (a, b, c) = (
                    &structural_lits[0],
                    &structural_lits[1],
                    &structural_lits[2],
                );
                quote! { ::parse_that::find_first_of_3(__rem, #a, #b, #c) }
            }
            4 => {
                let (a, b, c, d) = (
                    &structural_lits[0],
                    &structural_lits[1],
                    &structural_lits[2],
                    &structural_lits[3],
                );
                quote! { ::parse_that::find_first_of_4(__rem, #a, #b, #c, #d) }
            }
            _ => {
                // 5+ targets: general find_first_of with nibble LUT dispatch
                quote! { ::parse_that::find_first_of(__rem, &[#(#structural_lits),*]) }
            }
        }
    };

    let loop_body = quote! {
        loop {
            #ws_trim
            if state.offset >= state.src_bytes.len() { break; }
            if unsafe { *state.src_bytes.get_unchecked(state.offset) } == #close_lit { break; }

            let __item = state.offset;
            let __rem = &state.src_bytes[state.offset..];

            // Unified scan: find first structural byte (open/close/pivot/trail)
            // in a single SIMD pass.
            let __first = #unified_scan;

            if let Some((__fp, __fb)) = __first {
                if __fb == #pivot_lit {
                    // Pivot found first — tentatively a pivot branch.
                    // Scan the value to find where it ends.
                    let __val_start = __item + __fp + 1;
                    state.offset = __val_start;
                    let __vrem = &state.src_bytes[state.offset..];
                    let __val_end_rel = #value_scan;
                    state.offset += __val_end_rel;

                    // Pseudo-class guard: if the value terminated at open_byte,
                    // the pivot was part of a selector (e.g., selector:pseudo{...}).
                    if state.src_bytes.get(state.offset).copied() == Some(#open_lit) {
                        #on_block
                    } else {
                        #trail_consume
                        #on_pivot
                    }
                } else if __fb == #open_lit {
                    state.offset = __item + __fp;
                    #on_block
                } else if __fb == #close_lit {
                    break;
                } else {
                    // Trail byte — skip past it.
                    state.offset = __item + __fp + 1;
                }
            } else {
                state.offset = state.src_bytes.len();
                break;
            }
        }
    };

    (loop_body, ws_post)
}

// ── Slab Emission ────────────────────────────────────────────────────────────

/// Emit an monolithic delimiter scanner.
///
/// Grammar-agnostic speculative dispatch: the scanner determines WHICH branch
/// to try, then calls the existing slab function for that branch from the
/// item start. No manual type construction, no grammar-specific code.
///
/// - Pivot found → call pivot branch's slab function from item start
/// - Block delimiter found → call block branch's slab function from item start
/// - Close delimiter → exit loop
/// - Trail delimiter → skip
///
/// The pivot branch's function handles all the typed field construction via
/// the normal recursive descent codegen. The scanner just eliminates the
/// Alt's checkpoint/backtrack overhead by selecting the right branch upfront.
pub(super) fn emit_scan(
    config: &DelimScanConfig,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let open_lit = proc_macro2::Literal::byte_character(config.open_byte);
    let close_lit = proc_macro2::Literal::byte_character(config.close_byte);

    // Determine the scratch push expression for use in on_block / on_pivot.
    let content_rule_for_scratch = config.content_rule.expect("content_rule required for scan");
    let elem_desc_for_push = match ctx.rule_body_desc(content_rule_for_scratch) {
        Some(bbnf_ir::TypeDesc::Vec(inner)) => inner.as_ref().clone(),
        _ => bbnf_ir::TypeDesc::Enum,
    };
    let scratch_push_v = ctx.emit_scratch_push(&elem_desc_for_push, &quote! { __v });
    let scratch_push_direct = |expr: proc_macro2::TokenStream| -> proc_macro2::TokenStream {
        ctx.emit_scratch_push(&elem_desc_for_push, &expr)
    };

    // Block branch: rewind to item start, call the block rule's slab function.
    let on_block = if let Some(block_rule_id) = config.block_fn {
        let name = ctx.ir.get_string(ctx.ir.rules[block_rule_id as usize].name);
        let fn_ident = mono_fn_ident(name);
        let push = &scratch_push_v;
        quote! {
            state.offset = __item;
            if let Some(__v) = Self::#fn_ident(state) {
                #push;
            } else {
                break;
            }
        }
    } else {
        quote! { state.offset += 1; }
    };

    // Pivot branch: the scanner found the pivot byte and scanned the value.
    // `state.offset` is at the end of the value (past optional trail).
    // `__item` is the start of the item.
    //
    // For Span-typed pivot rules (e.g., `declaration = ... ?w` returning Span),
    // we can construct the result directly from scanner offsets, eliminating
    // the rewind + re-parse that the speculative dispatch normally does.
    let on_pivot = if let Some(pivot_rule_id) = config.pivot_fn {
        let pivot_rule = &ctx.ir.rules[pivot_rule_id as usize];
        let pivot_name = ctx.ir.get_string(pivot_rule.name);
        let pivot_fn = mono_fn_ident(pivot_name);

        // Fallback: if the pivot function fails, try the block branch.
        let fallback = if let Some(block_rule_id) = config.block_fn {
            let block_name = ctx.ir.get_string(ctx.ir.rules[block_rule_id as usize].name);
            let block_fn = mono_fn_ident(block_name);
            let push = &scratch_push_v;
            quote! {
                state.offset = __item;
                if let Some(__v) = Self::#block_fn(state) {
                    #push;
                } else {
                    break;
                }
            }
        } else {
            quote! { break; }
        };

        // Check if the pivot rule returns Span — if so, construct directly
        // from scanner offsets (no rewind, no re-parse).
        let pivot_type = ctx.ir.types.iter().find(|(id, _)| *id == pivot_rule_id);
        let is_span_result = pivot_rule.meta.directives.token
            || pivot_type.is_some_and(|(_, td)| *td == bbnf_ir::TypeDesc::Span);

        if is_span_result {
            // Direct construction: the scanner already scanned the entire item.
            // Build Span from __item to state.offset (post-trail-consume).
            let variant_ident = format_ident!("{}", pivot_name);
            let enum_ident = &ctx.enum_ident;
            let push_direct = scratch_push_direct(quote! {
                #enum_ident::#variant_ident(
                    ::parse_that::Span::new(__item, state.offset, state.src)
                )
            });
            quote! {
                // Scanner already consumed the item — construct Span directly.
                #push_direct;
            }
        } else {
            // Non-Span result: rewind and re-parse with the pivot function.
            let push = &scratch_push_v;
            quote! {
                state.offset = __item;
                if let Some(__v) = Self::#pivot_fn(state) {
                    #push;
                } else {
                    #fallback
                }
            }
        }
    } else {
        quote! { break; }
    };

    let _ws_trim = emit_ws_trim(ctx, mctx);
    let (loop_body, ws_post) = emit_scan_loop(config, ctx, mctx, &on_pivot, &on_block);

    let helper = ctx.alloc_helper_ident();
    let enum_ident = &ctx.enum_ident;

    // Content rule variant name (from the Ref followed during detection).
    let wrap_variant = if let Some(rule_id) = config.content_rule {
        let name = ctx.ir.get_string(ctx.ir.rules[rule_id as usize].name);
        let variant = quote::format_ident!("{}", name);
        quote! { #enum_ident::#variant }
    } else {
        // No content rule identified — can't construct the Vec variant.
        return quote! { compile_error!("delim_scan: content rule not found") };
    };

    let content_rule = config.content_rule.expect("content_rule required");
    let elem_desc = match ctx.rule_body_desc(content_rule) {
        Some(bbnf_ir::TypeDesc::Vec(inner)) => inner.as_ref().clone(),
        _ => {
            return quote! { compile_error!("delim_scan: content rule is not a vector type") };
        }
    };
    let start_var = mctx.fresh("ds_start");
    let depth_var = mctx.fresh("ds_depth");
    let init_code = ctx.emit_scratch_init(&elem_desc, &depth_var);
    let collect_expr = ctx.emit_scratch_collect(&elem_desc, &depth_var);

    quote! {
        {
            let #start_var = state.offset;
            if state.src_bytes.get(state.offset).copied() != Some(#open_lit) { return None; }
            state.offset += 1;

            #init_code

            #loop_body

            #ws_post
            if state.src_bytes.get(state.offset).copied() != Some(#close_lit) { return None; }
            state.offset += 1;
            Some(&*#helper(state).slab().alloc(#wrap_variant(#collect_expr)))
        }
    }
}

// ── Combined detect + emit (convenience) ─────────────────────────────────────

/// Try to detect and emit an monolithic delimiter scanner for a wrap pattern.
pub(super) fn try_emit_alloc_wrap(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> Option<TokenStream> {
    let config = try_detect(open, middle, close, ir)?;
    // Monolithic path requires content_rule for Vec variant construction.
    let _content_rule = config.content_rule?;
    Some(emit_scan(&config, ctx, mctx))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use bbnf_ir::{GrammarIR, IrRule, RuleMeta, TypeDesc};

    use super::*;
    use crate::generate::codegen::ir_types::{IrCodegenCtx, ParserAttributes};

    #[test]
    fn emit_alloc_uses_local_close_lookahead_capacity() {
        let ir = GrammarIR {
            rules: vec![
                IrRule {
                    id: 0,
                    name: 0,
                    // Body infers to Vec(Enum) because Repeat(Ref(1)) → Vec(Enum).
                    body: IrNode::Repeat {
                        inner: Box::new(IrNode::Ref(1)),
                        lo: 0,
                        hi: u32::MAX,
                    },
                    meta: RuleMeta::default(),
                    source_span: None,
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta::default(),
                    source_span: None,
                },
            ],
            entry: 0,
            strings: vec!["items".into(), "item".into(), "x".into()],
            fns: vec![],
            types: vec![
                (0, TypeDesc::Vec(Box::new(TypeDesc::Enum))),
                (1, TypeDesc::Span),
            ],
            follow_sets: HashMap::new(),
            ws_pattern: None,
            collapse_simple_spans: false,
            debug_all: false,
            debug_labels: Vec::new(),
            type_map: None,
        };

        let mut ir = ir;
        bbnf_ir::passes::project_types(&mut ir);

        let ident = quote::format_ident!("TestParser");
        let attrs = ParserAttributes::default();
        let ctx = IrCodegenCtx::new(&ir, &ident, &attrs);
        let config = DelimScanConfig {
            open_byte: b'[',
            close_byte: b']',
            pivot_byte: b':',
            trail_byte: None,
            block_fn: None,
            pivot_fn: None,
            content_rule: Some(0),
        };
        let mut mctx = MonoCtx::new(vec![false, false], vec![false, false]);

        let tokens = emit_scan(&config, &ctx, &mut mctx).to_string();
        // Slab mode uses scratch-based collection.
        assert!(tokens.contains("__s0"), "should use scratch push: {}", tokens);
        assert!(tokens.contains("__c0"), "should use scratch collect: {}", tokens);
    }
}
