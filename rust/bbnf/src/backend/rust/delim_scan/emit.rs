//! Delimiter-scan emission: codegen for the scan loop and slab-allocated wrappers.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::DelimScanConfig;
use crate::backend::rust::ir_types::IrCodegenCtx;
use crate::backend::rust::{MonoCtx, emit_ws_trim, mono_fn_ident};

// ── Shared Emission Helpers ─────────────────────────────────────────────────

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

// ── Slab Emission ───────────────────────────────────────────────────────────

/// Emit a monolithic delimiter scanner.
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
pub fn emit_scan(
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

// ── Combined detect + emit (convenience) ────────────────────────────────────

/// Try to detect and emit a monolithic delimiter scanner for a wrap pattern.
pub(in crate::backend::rust) fn try_emit_alloc_wrap(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ir: &bbnf_ir::GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> Option<TokenStream> {
    let config = super::detect::try_detect(open, middle, close, ir)?;
    // Monolithic path requires content_rule for Vec variant construction.
    let _content_rule = config.content_rule?;
    Some(emit_scan(&config, ctx, mctx))
}
