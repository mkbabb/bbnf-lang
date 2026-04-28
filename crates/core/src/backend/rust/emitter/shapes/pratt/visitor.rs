//! AW-V.W4.1 — visitor-path Pratt emitter.
//!
//! The visitor-path Pratt emitter invokes the visitor's per-shape
//! operator / operand methods in place of tape record pushes.
//! `V: PrattVisitor + <operand-shape trait bounds>` composes the
//! delegates statically at the call site. The operand dispatch rides
//! the grammar's value-position visitor dispatcher (emitted by
//! `dispatcher::emit_visitor_dispatcher`), so nested expressions,
//! numbers, strings, function calls resolve through the same
//! monomorphic trait surface.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use bbnf_ir::registry::EmitStrategy;
use super::super::dispatcher::{
    emit_ref_call_visitor, visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::super::root_rule_name;
use super::extract_first_ref;

/// Emit `pub fn parse_pratt_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
///
/// # AZ-I.W2-act.recovery — substrate-agnostic body
///
/// The visitor-path body takes `&mut V` where `V: PrattVisitor + …`
/// composes statically at the call site. The visitor methods do not
/// depend on the codegen substrate (tape vs struct), so the same
/// emitted body services every [`EmitStrategy`] arm. The pre-recovery
/// `StructDirect` panic retires here; the visitor path is gated off
/// wholesale at the per-grammar admission level by
/// [`super::super::dispatcher::has_w4_classified`] when the grammar
/// carries any Pratt / Unordered rule, so this body is emitted only
/// when the per-grammar caller has already determined the visitor
/// trait-bound surface admits Pratt — substrate-independence is the
/// invariant.
pub fn emit_parse_pratt_visitor(
    _strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("pratt", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    // AX.W0a.2.l — per-rule visitor-path Pratt LUT (mirrors tape-
    // path emitter above). Each visitor body consults its own
    // rule-scoped LUT + sparse entries slice.
    let rule_lut_ident = format_ident!("PRECEDENCE_LUT_{}", rule_name);
    let rule_entries_ident = format_ident!("PRECEDENCE_ENTRIES_{}", rule_name);

    // Visitor-path dispatcher — wires the RHS operand parse into the
    // grammar's per-shape visitor family. When the grammar has no
    // root dispatcher (Sheets' `formula` root is a Seq not an Alt),
    // the visitor emitter returns an empty stream so the compile
    // gate stays on the walker fallback; the tape-path emitter does
    // the same above.
    let visitor_dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — per-Ref operand calls for visitor path.
    let operand_ref = extract_first_ref(&rule.body);
    let operand_call = operand_ref
        .and_then(|rid| emit_ref_call_visitor(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #visitor_dispatcher_ident(input, p, state, visitor)?;
            }
        });
    let rhs_call = operand_call.clone();

    quote! {
        /// AW-V.W4.1 — visitor-path Pratt-shape parse function.
        ///
        /// Dispatches visitor method calls in place of tape pushes.
        /// Per-grammar `V: PrattVisitor` composes at the call site —
        /// the generic bound statically resolves every method to the
        /// chosen visitor impl.
        ///
        /// # Emitted algorithm (mirrors tape path; see `emit_parse_pratt`)
        ///
        /// 1. `visitor.begin_pratt()` — outer compound marker.
        /// 2. Dispatch leftmost operand through the grammar's
        ///    visitor dispatcher.
        /// 3. `visitor.operand_end()` — per operand boundary.
        /// 4. Loop: peek byte / LUT / reduce / push op / advance / RHS.
        ///    Each `operator()` visitor call fires at the
        ///    reducer-compound stamp; `operand_end` fires after each
        ///    operand parse.
        /// 5. `visitor.end_pratt()` — outer compound close.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_assignments, unused_mut, unused_variables)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::PrattVisitor
                + crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            // Local op stack — same layout as the tape-path emitter's
            // `LocalOpEntry` but carries only the data the visitor
            // reducer thread needs. No `lhs_idx` / `lhs_span_lo` —
            // the visitor receives the operator via its `operator`
            // method and synthesises AST nodes on its own side.
            //
            // AY.W1.4 — initialised via `::core::array::from_fn` for
            // the fixed-size stack array (the bootstrap postprocessor
            // strips inner derive attributes; `from_fn` is the
            // Copy-free idiom).
            struct LocalOpEntry {
                op_discriminant: u8,
                precedence: u8,
                associativity_is_left: bool,
            }

            let _ = #support_mod::skip_space(input, p, state);
            visitor.begin_pratt().map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;

            // Leftmost operand — AW-V.W5.2 per-Ref direct call.
            #operand_call
            visitor.operand_end().map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;

            // AY.W1.4 — fixed-size stack array (mirrors tape-path
            // emitter; 16 entries × 4 bytes = 64 byte stack frame).
            // The pre-AY heap `Vec::with_capacity(4)` allocated per
            // Pratt parse and freed at function exit; the bench delta
            // is measurable on Sheets `parse_stress` whose
            // `parse_pratt_visitor_*` frames recur every formula.
            const OP_STACK_CAP: usize = 16;
            let mut op_stack: [LocalOpEntry; OP_STACK_CAP] =
                ::core::array::from_fn(|_| LocalOpEntry {
                    op_discriminant: 0,
                    precedence: 0,
                    associativity_is_left: false,
                });
            let mut op_stack_len: usize = 0;

            // AX.W0a.2.n — Whitespace-aware operator peek (mirrors
            // tape-path emitter). Dispatch on the current byte
            // directly when it is already an operator; fall back to
            // skip_space + re-peek only when the current byte is
            // not in the rule's LUT alphabet. Preserves whitespace-
            // carrying combinator operators while fixing the
            // trailing-whitespace premature-break.
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = #rule_lut_ident[op_byte as usize];
                if lut_byte == 0 {
                    let _ = #support_mod::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = #rule_lut_ident[op_byte as usize];
                }
                let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                    ::core::option::Option::None
                } else {
                    ::core::option::Option::Some(lut_byte & 0x0Fu8)
                };

                // Reduce: fire `operator` for each op we pop.
                loop {
                    if op_stack_len == 0 {
                        break;
                    }
                    let top_op = &op_stack[op_stack_len - 1];
                    let should_reduce = match new_prec {
                        ::core::option::Option::None => true,
                        ::core::option::Option::Some(p_new) => {
                            top_op.precedence > p_new
                                || (top_op.precedence == p_new
                                    && top_op.associativity_is_left)
                        }
                    };
                    if !should_reduce {
                        break;
                    }
                    let op_disc = top_op.op_discriminant;
                    let op_prec = top_op.precedence;
                    op_stack_len -= 1;
                    visitor
                        .operator(op_disc, op_prec)
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        })?;
                }

                if lut_byte == 0 {
                    break;
                }

                let precedence: u8 = lut_byte & 0x0Fu8;
                let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
                let associativity_is_left: bool = assoc_bit == 0;
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;

                let second_byte: ::core::option::Option<u8> =
                    input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in #rule_entries_ident.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };

                // AX.W0a.2.n — phantom-op guard (mirrors tape-path).
                if !op_matched {
                    break;
                }

                *p = (*p).saturating_add(op_width as usize);

                debug_assert!(
                    op_stack_len < OP_STACK_CAP,
                    "Pratt visitor op_stack overflow at depth {} (cap {})",
                    op_stack_len,
                    OP_STACK_CAP,
                );
                op_stack[op_stack_len] = LocalOpEntry {
                    op_discriminant,
                    precedence,
                    associativity_is_left,
                };
                op_stack_len += 1;

                let _ = #support_mod::skip_space(input, p, state);
                // AW-V.W5.2 — per-Ref RHS operand call.
                #rhs_call
                visitor.operand_end().map_err(|_| crate::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })?;
            }

            visitor.end_pratt().map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
            Ok(())
        }
    }
}
