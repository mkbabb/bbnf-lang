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

use std::collections::HashMap;

use bbnf_ir::AltDispatch;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{
    AltBranchInfo, BucketProbe, KeyClass, KeyDispatchBranch, KeyDispatchConfig, KeyIndex,
    LengthBucket, ValuePlacement,
};

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
        let surgery = ctx.tape_surgery.take();
        let mut arms = Vec::new();

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
            arms.push(quote! {
                #( #patterns )|* => { #branch_assign #compound_mark #body }
            });
        }

        let (fallback_arm, eof_expr) = if let Some((info, fb_body)) = fallback {
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            (
                quote! { _ => { #compound_mark #fb_body } },
                quote! { { #compound_mark #fb_body } },
            )
        } else {
            (quote! { _ => None }, quote! { None })
        };
        arms.push(fallback_arm);

        quote! {
            {
                if state.offset < state.src_bytes.len() {
                    match state.src_bytes[state.offset] {
                        #( #arms ),*
                    }
                } else {
                    #eof_expr
                }
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
        let surgery = ctx.tape_surgery.take();

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
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    #branch_assign
                    #compound_mark
                    let __result = #body;
                    if __result.is_some() {
                        break 'alt_blk __result;
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            'alt_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_alt_all_literal_impl(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut chain = Vec::new();
        for (_value, body) in &literals {
            chain.push(quote! {
                {
                    let __r = #body;
                    if __r.is_some() { break 'alt_lit_blk __r; }
                }
            });
        }
        quote! {
            'alt_lit_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_key_dispatch_impl(
        &mut self,
        config: &KeyDispatchConfig,
        index: &KeyIndex,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let cp = ctx.fresh("kd_cp");
        let scanner = match config.key_class {
            KeyClass::Identifier => quote! {
                ::parse_that::scan_ident(state, &::parse_that::DEFAULT_IDENT_CONFIG)
            },
            KeyClass::QuotedString { .. } => quote! { ::parse_that::scan_string_quoted(state) },
        };

        // AQ.7.3: build a NodeId-stable map from `branch_idx` (Alt
        // branch index inside the parent) to the compiled body
        // TokenStream. The KeyIndex's per-bucket entries reference
        // branches by the same index, so we can drop in the right
        // body when we emit each match arm.
        let mut body_by_branch: HashMap<u8, TokenStream> = HashMap::new();
        for kd in branches {
            // The IR's `DetectedBranch.branch_idx` is widened to
            // u8 inside `build_key_index` already. Find the branch
            // by its first key in the bucket — we keyed compilation
            // by `det.branch_idx` (the Alt index), which equals the
            // `branch_idx` field on every KeyEntry the index built
            // for that detected branch. Look it up by hashing
            // `key_bytes` against the bucket entries below.
            //
            // We piggy-back on the `key_bytes` Vec the driver passed
            // in: those are the same byte sequences the KeyIndex
            // entries hold, so we can join the body to the bucket
            // entry by its first key match.
            let first = kd
                .key_bytes
                .first()
                .map(|b| b.as_slice())
                .unwrap_or(&[]);
            if let Some(branch_idx) = lookup_branch_idx(index, first) {
                body_by_branch.insert(branch_idx, kd.body);
            }
        }

        let bucket_arms = emit_bucket_arms(index, &body_by_branch, &cp);

        let fallback_expr = if let Some((_info, fb)) = fallback {
            fb
        } else {
            quote! { None }
        };
        quote! {
            'kd_blk: {
                let #cp = state.offset;
                if let Some(ref __kd_s) = #scanner {
                    let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                    let __kd_len = __kd_bytes.len();
                    match __kd_len {
                        #(#bucket_arms)*
                        _ => {}
                    }
                }
                state.offset = #cp;
                #fallback_expr
            }
        }
    }
}

/// AQ.7.3: locate the branch index for a given key by walking the
/// length bucket. Returns `None` when the key isn't present (which
/// only happens when the driver passes an empty key list — every
/// branch has at least one key in production).
fn lookup_branch_idx(index: &KeyIndex, key: &[u8]) -> Option<u8> {
    if key.is_empty() {
        return None;
    }
    let key_len = key.len() as u8;
    let bucket = index.buckets.iter().find(|b| b.key_len == key_len)?;
    bucket
        .entries
        .iter()
        .find(|e| e.key_bytes.as_slice() == key)
        .map(|e| e.branch_idx)
}

/// AQ.7.3: emit one match arm per length bucket, using the probe
/// shape stored on the bucket.
///
/// The `cp` ident is the saved checkpoint position the dispatch
/// rewinds to before invoking the branch body — every match arm
/// rewinds before breaking with the body.
fn emit_bucket_arms(
    index: &KeyIndex,
    body_by_branch: &HashMap<u8, TokenStream>,
    cp: &proc_macro2::Ident,
) -> Vec<TokenStream> {
    index
        .buckets
        .iter()
        .map(|bucket| emit_one_bucket(bucket, body_by_branch, cp))
        .collect()
}

fn emit_one_bucket(
    bucket: &LengthBucket,
    body_by_branch: &HashMap<u8, TokenStream>,
    cp: &proc_macro2::Ident,
) -> TokenStream {
    let key_len_lit = bucket.key_len as usize;
    let body = match &bucket.probe {
        BucketProbe::Linear => emit_linear_probe(bucket, body_by_branch, cp),
        BucketProbe::FirstByteTable { cells } => {
            emit_first_byte_table_probe(bucket, cells, body_by_branch, cp)
        }
    };
    quote! { #key_len_lit => { #body } }
}

fn emit_linear_probe(
    bucket: &LengthBucket,
    body_by_branch: &HashMap<u8, TokenStream>,
    cp: &proc_macro2::Ident,
) -> TokenStream {
    let arms: Vec<TokenStream> = bucket
        .entries
        .iter()
        .map(|entry| {
            let byte_lits: Vec<proc_macro2::Literal> = entry
                .key_bytes
                .iter()
                .map(|b| proc_macro2::Literal::byte_character(*b))
                .collect();
            let body = body_by_branch
                .get(&entry.branch_idx)
                .cloned()
                .unwrap_or_else(|| quote! { None });
            quote! {
                if __kd_bytes == &[#(#byte_lits),*] {
                    state.offset = #cp;
                    break 'kd_blk #body;
                }
            }
        })
        .collect();
    quote! { #(#arms)* }
}

fn emit_first_byte_table_probe(
    bucket: &LengthBucket,
    cells: &[u8],
    body_by_branch: &HashMap<u8, TokenStream>,
    cp: &proc_macro2::Ident,
) -> TokenStream {
    debug_assert_eq!(cells.len(), 256);
    // Emit the table as a `static` 256-byte array so the lookup
    // turns into one indexed load. Sentinel `255` means "no
    // candidate" — bucket entry counts cap below `u8::MAX` because
    // each entry maps to a single Alt branch.
    let cell_lits: Vec<proc_macro2::Literal> = cells
        .iter()
        .map(|c| proc_macro2::Literal::u8_unsuffixed(*c))
        .collect();
    let table_ident = proc_macro2::Ident::new(
        &format!("__KD_LEN{}_TBL", bucket.key_len),
        proc_macro2::Span::call_site(),
    );
    // The candidate entry's index inside `bucket.entries` selects
    // the body and the equality check.
    let case_arms: Vec<TokenStream> = bucket
        .entries
        .iter()
        .enumerate()
        .map(|(idx, entry)| {
            let idx_lit = idx as u8;
            let byte_lits: Vec<proc_macro2::Literal> = entry
                .key_bytes
                .iter()
                .map(|b| proc_macro2::Literal::byte_character(*b))
                .collect();
            let body = body_by_branch
                .get(&entry.branch_idx)
                .cloned()
                .unwrap_or_else(|| quote! { None });
            quote! {
                #idx_lit => {
                    if __kd_bytes == &[#(#byte_lits),*] {
                        state.offset = #cp;
                        break 'kd_blk #body;
                    }
                }
            }
        })
        .collect();
    quote! {
        static #table_ident: [u8; 256] = [#(#cell_lits),*];
        let __kd_first = unsafe { *__kd_bytes.get_unchecked(0) };
        let __kd_slot = #table_ident[__kd_first as usize];
        match __kd_slot {
            #(#case_arms)*
            _ => {}
        }
    }
}
