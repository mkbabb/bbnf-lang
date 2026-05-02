//! Per-branch tape-path emission: dispatch-arm assembly, branch-body
//! emitters per IR shape (Ref / Literal / Regex / Seq), Seq position
//! flattening, and trivia stripping.

use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::emit_ref_call_shape;
/// AZ-I.W2.RB — emit the dispatch body for an AltDispatch rule under
/// [`bbnf_ir::registry::EmitStrategy::StructDirect`].
///
/// Each branch attempt routes through the per-Ref struct-direct shape
/// fn (or — for non-Ref leaf branches — emits a typed `builder.push_*`
/// for the leaf payload) and records the matched branch tag via
/// `builder.push_branch_tag(idx)` before the per-shape body fires. No
/// `tape.*` calls are emitted under this path.
///
/// JSON does not exercise AltDispatch (its `value` rule is `Wrap`); this
/// path is reachable only when CSS L4 (W3) or Sheets (W2.B) flips a
/// rule with AltDispatch shape into StructDirect. Until then it remains
/// dead code at codegen but the emission contract holds (zero
/// `tape.push` per the W2-EMITTER-REWIRE plan §3 hard gate).
pub(super) fn emit_dispatch_arms_struct_direct(
    branches: &[AltBranch],
    grammar_suffix: &str,
    _rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let mut arms: Vec<TokenStream> = Vec::new();
    for (branch_idx, branch) in branches.iter().enumerate() {
        let idx_u32 = branch_idx as u32;
        let inner = unwrap_trivia(&branch.node);
        // AZ-III.W3c.1 — typed-payload constant-fold pulled from the
        // branch's outermost `Map { fn_id }` chain. Mirrors Wrap's
        // `wrap_branch_payload_push`: `FnDescriptor::Expr { expr, .. }`
        // with `MapExpr::IntLit(n)` produces a `push_leaf_with_u64(n)`
        // call so CSS L4's `namedColor` / hex-color branches deposit
        // the declared `0xRRGGBBAAu32` instead of unit. Non-constant
        // maps (or no Map wrapper) fall back to `push_leaf_with_unit`
        // — the branch still records the discriminator without
        // fabricating typed payload the grammar didn't declare.
        let payload_push = branch_payload_push(&branch.node, ir);
        let body = match inner {
            IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
                Some(call) => quote! {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match #call {
                            Ok(_) => {
                                builder.push_branch_tag(#idx_u32);
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                },
                None => quote! {},
            },
            // AZ-II.cutover.M Phase 3 — Literal-led Alt branches under
            // struct-direct emit the byte-comparison + push_leaf pair
            // mirroring `emit_literal_attempt`'s tape-path body, plus
            // the discriminator-recording `push_branch_tag(idx)` that
            // the AltDispatch frame's TaggedEnum layout consumes when
            // the branch fires. EBNF's `letter`, `digit`, `symbol`,
            // `terminator` and BNF's `terminal` are the canonical
            // Alt-of-literal AltDispatch rules; pre-cutover.M these
            // emitted empty placeholders, dropping every literal
            // candidate on the floor.
            //
            // AZ-III.W3c.1 — typed-payload activation: the previous
            // unconditional `push_leaf_with_unit()` call is replaced
            // by `payload_push`, which constant-folds `Map(Literal)`
            // branches' `-> NN` projections (CSS L4 `namedColor`'s
            // 150 `"name" -> 0xRRGGBBAAu32` mappings) into the typed
            // `push_leaf_with_u64(value)` call the
            // `CssStructBuilder::Color::Hex` reader consumes.
            IrNode::Literal(sid) => {
                let bytes = ir.get_string(*sid).as_bytes();
                let len = bytes.len();
                let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
                quote! {
                    {
                        let at = *p;
                        let end = at + #len;
                        if input.len() >= end && input[at..end] == [#(#byte_lits),*] {
                            *p = end;
                            #payload_push
                            builder.push_branch_tag(#idx_u32);
                            break 'try_branches;
                        }
                    }
                }
            }
            // Regex-led branches dispatch through the per-grammar
            // regex-scan adapter and push the typed leaf on match.
            // Same discriminator-recording shape as the Literal arm.
            //
            // AZ-III.W3c.1 — typed-payload activation: a `Map(Regex)`
            // wrapper's constant-fold (e.g. `/\s*>\s*/ -> 1u8`) flows
            // through `payload_push` so the branch deposits its
            // declared u8 / u32 / bool instead of unit.
            IrNode::Regex(sid) => {
                let pattern = ir.get_string(*sid).to_string();
                let regex_scan_ident = super::super::super::dfa_codegen::regex_scan_adapter_ident(
                    &super::super::sanitise_grammar(grammar_suffix),
                );
                quote! {
                    {
                        if let ::core::option::Option::Some(match_len) =
                            #regex_scan_ident(#pattern, input, *p)
                        {
                            *p += match_len as usize;
                            #payload_push
                            builder.push_branch_tag(#idx_u32);
                            break 'try_branches;
                        }
                    }
                }
            }
            // Seq branches (literal-chain or mixed). Pure literal
            // chains record one unit leaf for the whole match; mixed
            // structural seqs preserve their position-level children
            // inside the surrounding TaggedEnum compound. EBNF
            // `term = "(" S rhs S ")" | "[" ... | "{" ...` is the
            // canonical O2 consumer: the grouped-term branch is not a
            // leaf and must keep the nested `rhs` record while still
            // recording the owning branch tag.
            IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
                if seq_is_pure_literal_chain(inner) {
                    let mut positions: Vec<&IrNode> = Vec::new();
                    flatten(inner, &mut positions);
                    let lit_checks: Vec<TokenStream> = positions
                        .iter()
                        .map(|pos| emit_seq_position(pos, ir))
                        .collect();
                    // AZ-III.W3c.1 — pure-literal-chain Seq branches
                    // also honour the typed payload constant-fold. CSS
                    // L4's `namedColor` after prefix-tree factoring
                    // produces Seq("a", Alt(...)) -> 0xRRGGBBAAu32
                    // shapes that pre-W3c.1 emitted unit instead of
                    // the declared u32 hex.
                    quote! {
                        {
                            let save_p = *p;
                            let attempt: ::core::result::Result<(), ()> = (|| {
                                #(#lit_checks)*
                                Ok(())
                            })();
                            match attempt {
                                Ok(()) => {
                                    #payload_push
                                    builder.push_branch_tag(#idx_u32);
                                    break 'try_branches;
                                }
                                Err(()) => { *p = save_p; }
                            }
                        }
                    }
                } else {
                    let seq_body = super::super::inline::emit_seq_branch_structural_struct_direct(
                        inner,
                        &format_ident!("__shape_support_{}", grammar_suffix),
                        grammar_suffix,
                        ir,
                    );
                    quote! {
                        {
                            let attempt_p = *p;
                            let attempt_builder = builder.checkpoint();
                            let attempt: ::core::result::Result<
                                (),
                                crate::runtime::DtaError,
                            > = (|| {
                                #seq_body
                                ::core::result::Result::Ok(())
                            })();
                            match attempt {
                                ::core::result::Result::Ok(()) => {
                                    builder.push_branch_tag(#idx_u32);
                                    builder.commit(attempt_builder);
                                    break 'try_branches;
                                }
                                ::core::result::Result::Err(_) => {
                                    *p = attempt_p;
                                    builder.rollback(attempt_builder);
                                }
                            }
                        }
                    }
                }
            }
            _ => quote! {},
        };
        arms.push(body);
    }
    quote! {
        'try_branches: loop {
            #(#arms)*
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
    }
}

/// AZ-III.W3c.1 — emit the `builder.push_leaf_with_*` call materialising
/// an AltDispatch-Alt branch's typed `->` projection.
///
/// Symmetric to `wrap::struct_direct::wrap_branch_payload_push`: the
/// alt_dispatch shape is Wrap's strict superset (per
/// `shapes/alt_dispatch/mod.rs` doc), so the same payload-fold rule
/// applies. Walks the branch's outermost `Map { fn_id }` chain
/// (skipping `OptionalWhitespace` trivia), inspects the bound
/// `FnDescriptor`, and constant-folds the leaf projections that CSS
/// L4's `namedColor`'s 150 `"name" -> 0xRRGGBBAAu32` and `combinator`'s
/// `-> Nu8` annotations produce. Non-constant maps (`Input`,
/// `FnCall`, `BinOp`, `UnaryOp`) and unmapped branches fall back to
/// `push_leaf_with_unit()` so the branch still records its
/// discriminator without fabricating typed payload the grammar
/// didn't declare.
///
/// AZ-IV.W1-CLOSE.B — visibility widened from `pub(super)` to the
/// shapes module so the inline structural-branch helper can apply
/// the same fold to each inner Alt arm encountered while emitting a
/// prefix-tree-factored Seq. Without per-inner-arm fold the 148
/// non-singleton namedColor branches lose their declared u32 payload
/// (the outer Seq's tail is the inner Alt; `find_map_fn` cannot
/// recover a single fn_id from N alternative arms).
pub(in crate::backend::rust::emitter::shapes) fn branch_payload_push(
    branch_root: &IrNode,
    ir: &GrammarIR,
) -> TokenStream {
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            // Post-prefix-tree-factor branches look like
            // `Seq(Literal("a"), Map(Literal("liceblue"), fn))`. The
            // typed payload lives on the continuation (the LAST
            // structurally meaningful child); the leading literal
            // bytes are the factored prefix that the Seq's body
            // checks already enforce. Walk the LAST child so we
            // capture the typed projection without picking up an
            // earlier Map that may live on a side rule.
            IrNode::Seq(children) => children.last().and_then(find_map_fn),
            _ => None,
        }
    }
    let unit_push = quote! {
        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(builder);
    };
    let Some(fn_id) = find_map_fn(branch_root) else {
        return unit_push;
    };
    let Some(fn_desc) = ir.fns.get(fn_id as usize) else {
        return unit_push;
    };
    let FnDescriptor::Expr { expr, .. } = fn_desc else {
        return unit_push;
    };
    match expr {
        MapExpr::IntLit(n) => {
            let v: u64 = *n as u64;
            quote! {
                <_ as crate::runtime::StructBuilder>::push_leaf_with_u64(builder, #v);
            }
        }
        MapExpr::FloatLit(f) => {
            let v: f64 = *f;
            quote! {
                <_ as crate::runtime::StructBuilder>::push_leaf_with_f64(builder, #v);
            }
        }
        MapExpr::BoolLit(b) => {
            let v: bool = *b;
            quote! {
                <_ as crate::runtime::StructBuilder>::push_leaf_with_bool(builder, #v);
            }
        }
        // Non-constant or non-scalar projections (`Input`, `FnCall`,
        // `BinOp`, `UnaryOp`, string literal, etc.) cannot be
        // constant-folded at the dispatch arm. Drop to a unit leaf so
        // the branch still records the discriminator without faking a
        // typed payload.
        _ => unit_push,
    }
}

/// Returns `true` when every flattened position in `seq` is a
/// `Literal`, `Alt(of Literals)`, `Regex`, or `Epsilon` — the set
/// [`emit_seq_position`] handles without falling through to
/// `return Err(())`. Refs, Repeats, Negate, Minus, TokenDispatch
/// trip the structural path.
fn seq_is_pure_literal_chain(seq: &IrNode) -> bool {
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten(seq, &mut positions);
    positions.iter().all(|pos| match unwrap_trivia(pos) {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Alt(branches, _) => branches
            .iter()
            .all(|b| matches!(unwrap_trivia(&b.node), IrNode::Literal(_))),
        _ => false,
    })
}

pub(super) fn emit_seq_position(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return Err(());
                }
                *p = end;
            }
        }
        IrNode::Alt(branches, _) => {
            // AZ-IV.W1-CLOSE.B — emit per-inner-arm typed payload pushes
            // for prefix-tree-factored namedColor branches that flatten
            // to a pure-literal chain. Each inner arm carries its own
            // `Map { fn_id }` (the factor pass at
            // `crates/ir/src/passes/prefix.rs:243-250` wraps the
            // continuation in Map when the original branch was mapped).
            // `branch_payload_push` walks the original `branch.node`
            // (not the trivia-stripped form) so the Map wrapper is
            // visible. Without this push the 148 prefix-factored
            // namedColor inner arms drop their declared u32 payload.
            let alt_arms: Vec<TokenStream> = branches
                .iter()
                .filter_map(|b| match unwrap_trivia(&b.node) {
                    IrNode::Literal(sid) => {
                        let bytes = ir.get_string(*sid).as_bytes();
                        let len = bytes.len();
                        let byte_lits: Vec<TokenStream> =
                            bytes.iter().map(|byte| quote! { #byte }).collect();
                        let payload_push = branch_payload_push(&b.node, ir);
                        Some(quote! {
                            if !alt_hit {
                                let at = *p;
                                let end = at + #len;
                                if input.len() >= end
                                    && input[at..end] == [#(#byte_lits),*]
                                {
                                    *p = end;
                                    #payload_push
                                    alt_hit = true;
                                }
                            }
                        })
                    }
                    _ => None,
                })
                .collect();
            quote! {
                {
                    let mut alt_hit = false;
                    #(#alt_arms)*
                    if !alt_hit {
                        return Err(());
                    }
                }
            }
        }
        IrNode::Regex(_) => quote! {
            let at = *p;
            let mut q = at;
            while q < input.len() {
                let b = input[q];
                if b.is_ascii_alphanumeric() || b == b'_' {
                    q += 1;
                } else {
                    break;
                }
            }
            if q == at {
                return Err(());
            }
            *p = q;
        },
        IrNode::Epsilon => quote! {},
        _ => quote! { return Err(()); },
    }
}

pub(super) fn flatten<'a>(node: &'a IrNode, out: &mut Vec<&'a IrNode>) {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            flatten(inner, out);
        }
        IrNode::Seq(children) => {
            for c in children {
                flatten(c, out);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            flatten(lhs, out);
            flatten(rhs, out);
        }
        IrNode::Epsilon => {}
        other => out.push(other),
    }
}

/// Strip Map / OptionalWhitespace trivia.
pub(super) fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}
