//! Repetition emission for the Rust backend.
//!
//! Tranche AC.2 tape-first. A Repeat pushes its own compound
//! record — the child run is marked at entry, every successful
//! iteration pushes a child tape record (or runs as a
//! side-effecting sub-parse), and the epilogue emits
//! `push_compound(TapeKind::Repeat, children, lo, hi, 0, 0)` once
//! the lo-bound is satisfied.
//!
//! `Optional` is a special-case Repeat with lo=0,hi=1; it also
//! pushes a compound record so the view layer can read the
//! optional as a 0-or-1 child run.
//!
//! `sep_by` composes `element (sep element)*` under the same
//! compound-record shape.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{SepByConfig, ValuePlacement};

use super::RustEmitCtx;
use super::RustEmitter;

impl RustEmitter {
    pub(super) fn emit_repeat_many_impl(
        &mut self,
        body: TokenStream,
        lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let lo_lit = lo as usize;
        let rpt_blk = ctx.fresh_lifetime("rpt_blk");
        let min_check = if lo == 0 {
            quote! { Some(()) }
        } else {
            quote! {
                if __count >= #lo_lit {
                    Some(())
                } else {
                    break #rpt_blk None;
                }
            }
        };

        // Repeat pushes its own compound record as a side effect —
        // the child run is the per-iteration set the body parsed.
        // The return value is normalized to `Option<()>` so the
        // surrounding Seq / Alt / rule-body composition stays
        // uniform; the compound record is already in the tape when
        // we hand back success.
        quote! {
            #rpt_blk: {
                let __rpt_lo = state.offset as u32;
                let __rpt_children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
                let mut __count: usize = 0;
                loop {
                    let __prev = state.offset;
                    match (#body) {
                        Some(_) => {
                            __count += 1;
                            if state.offset == __prev { break; }
                        }
                        None => {
                            state.offset = __prev;
                            break;
                        }
                    }
                }
                match #min_check {
                    Some(()) => {
                        {
                            let __vi: u8 = 0u8;
                            let __mi: u8 = 0u8;
                            ::bbnf::runtime::tape::TapeBuilder::push_compound(
                                tape,
                                ::bbnf::runtime::tape::TapeKind::Repeat,
                                __rpt_children,
                                __rpt_lo,
                                state.offset as u32,
                                __vi, __mi,
                            );
                        }
                        Some(())
                    }
                    None => None,
                }
            }
        }
    }

    pub(super) fn emit_repeat_optional_impl(
        &mut self,
        body: TokenStream,
        _inner_type: &TypeDesc,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Optional pushes a compound Repeat record with 0 or 1
        // child. On miss, `state.offset` is rolled back to the
        // pre-attempt position; the compound is still pushed with
        // an empty children run. Returns `Option<()>` — the record
        // is already in the tape.
        quote! {
            {
                let __opt_lo = state.offset as u32;
                let __opt_children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
                let __opt_cp = state.offset;
                match (#body) {
                    Some(_) => (),
                    None => {
                        state.offset = __opt_cp;
                    }
                }
                {
                    let __vi: u8 = 0u8;
                    let __mi: u8 = 0u8;
                    ::bbnf::runtime::tape::TapeBuilder::push_compound(
                        tape,
                        ::bbnf::runtime::tape::TapeKind::Repeat,
                        __opt_children,
                        __opt_lo,
                        state.offset as u32,
                        __vi,
                        __mi,
                    );
                }
                Some(())
            }
        }
    }

    pub(super) fn emit_sep_by_impl(
        &mut self,
        element: TokenStream,
        separator: TokenStream,
        config: &SepByConfig,
        _elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let lo_lit = config.lo as usize;
        let rpt_blk = ctx.fresh_lifetime("rpt_blk");

        // Optional terminator-byte short-circuit for delim-wrapped
        // sep_by variants. The break condition takes a peek at the
        // next byte; on hit we exit the loop without consuming it.
        let terminator_break = if let Some(ref tb) = config.terminator_bytes {
            if tb.len() == 1 {
                let byte = tb[0];
                quote! {
                    if state.offset < state.src_bytes.len()
                        && state.src_bytes[state.offset] == #byte
                    {
                        break;
                    }
                }
            } else {
                quote! {}
            }
        } else {
            quote! {}
        };

        quote! {
            #rpt_blk: {
                let __rpt_lo = state.offset as u32;
                let __rpt_children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
                let mut __count: usize = 0;

                match (#element) {
                    Some(_) => {
                        __count += 1;
                    }
                    None => {
                        if __count >= #lo_lit {
                            {
                                let __vi: u8 = 0u8;
                                let __mi: u8 = 0u8;
                                ::bbnf::runtime::tape::TapeBuilder::push_compound(
                                    tape,
                                    ::bbnf::runtime::tape::TapeKind::Repeat,
                                    __rpt_children,
                                    __rpt_lo,
                                    state.offset as u32,
                                    __vi,
                                    __mi,
                                );
                            }
                            break #rpt_blk Some(());
                        } else {
                            break #rpt_blk None;
                        }
                    }
                }

                loop {
                    #terminator_break
                    let __cp = state.offset;
                    match (#separator) {
                        Some(_) => {}
                        None => break,
                    }
                    match (#element) {
                        Some(_) => {
                            __count += 1;
                        }
                        None => {
                            state.offset = __cp;
                            break;
                        }
                    }
                }

                if __count >= #lo_lit {
                    {
                        let __vi: u8 = 0u8;
                        let __mi: u8 = 0u8;
                        ::bbnf::runtime::tape::TapeBuilder::push_compound(
                            tape,
                            ::bbnf::runtime::tape::TapeKind::Repeat,
                            __rpt_children,
                            __rpt_lo,
                            state.offset as u32,
                            __vi, __mi,
                        );
                    }
                    Some(())
                } else {
                    break #rpt_blk None;
                }
            }
        }
    }
}
