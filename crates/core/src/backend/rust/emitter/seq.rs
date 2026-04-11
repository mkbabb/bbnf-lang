//! Sequence emission for the Rust backend.
//!
//! Tranche AC.2 tape-first. A Seq composes its children
//! sequentially; each child is a side-effecting sub-parse
//! (`Option<()>` or `Option<TapeOffset>`). The Seq itself returns
//! `Option<()>` — it does not push its own tape record. The
//! enclosing rule (or the enclosing compound Alt / Repeat) owns
//! the `push_compound` at its epilogue.
//!
//! `FlattenStrategy` and the typed Vec paths used to drive the
//! scratch-Vec allocator for `(T, Vec<T>)` patterns. Under
//! tape-first every sub-parse's child run is already linearized
//! into the tape by the children's own `push_*` calls, so Seq
//! flattening is a no-op here — the shape is carried through the
//! `mark_children` / `push_compound` bracket at the owning rule.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{FlattenStrategy, SeqChildGroup};

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_seq_grouped_impl(
        &mut self,
        groups: Vec<SeqChildGroup<TokenStream>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Each group is either a `Single` sub-parse or a
        // `SpanCompressed` run of side-effecting leaves. Under
        // tape-first both collapse to sequential `match` chaining —
        // the compound's span/children are owned by the parent.
        let mut stmts: Vec<TokenStream> = Vec::new();

        for group in groups {
            match group {
                SeqChildGroup::Single { output, ty: _ } => {
                    stmts.push(quote! {
                        match (#output) {
                            Some(_) => (),
                            None => break 'seq_blk None,
                        }
                    });
                }
                SeqChildGroup::SpanCompressed { outputs } => {
                    for o in outputs {
                        stmts.push(quote! {
                            match (#o) {
                                Some(_) => (),
                                None => break 'seq_blk None,
                            }
                        });
                    }
                }
            }
        }

        quote! {
            'seq_blk: {
                #( #stmts )*
                Some(())
            }
        }
    }
}
