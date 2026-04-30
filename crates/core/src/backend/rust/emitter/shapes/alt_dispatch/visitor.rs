//! Visitor-path AltDispatch emitter.
//!
//! Mirrors the tape-path's branch-arm assembly but invokes the
//! visitor surface instead of stamping tape records. The first-byte
//! set computation, trivia stripping, Seq flattening, and per-Seq
//! position emission are reused from the tape-path branches module
//! verbatim.

use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{emit_ref_call_visitor, visitor_shape_fn_ident};
use super::branches::{branch_first_bytes, emit_seq_position, flatten, unwrap_trivia};

/// Emit `pub fn parse_altdispatch_visitor_<grammar>_<rule><V>(input,
/// p, state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_alt_dispatch_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("altdispatch", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let body = unwrap_trivia(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return quote! {};
    };

    let dispatch_arms = emit_dispatch_arms_visitor(branches, grammar_suffix, ir);

    quote! {
        /// AX.W0a.2.b — visitor-path AltDispatch-shape parse function.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments, unreachable_code)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: ::tape::ObjectVisitor
                + ::tape::ArrayVisitor
                + ::tape::StringVisitor
                + ::tape::NumberVisitor
                + ::tape::KeywordVisitor,
        {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })?;
            #dispatch_arms
            Ok(())
        }
    }
}

fn emit_dispatch_arms_visitor(
    branches: &[AltBranch],
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::new();
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        let body = emit_branch_body_visitor(&branch.node, grammar_suffix, ir);
        enumerated.push((first_bytes, body));
    }

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut fallback_arms: Vec<TokenStream> = Vec::new();

    for (first_bytes, body) in &enumerated {
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            fallback_arms.push(body.clone());
        } else {
            for &b in first_bytes {
                per_byte_arms.entry(b).or_default().push(body.clone());
            }
        }
    }

    let byte_arms: Vec<TokenStream> = per_byte_arms
        .into_iter()
        .map(|(byte, bodies)| {
            let byte_lit = byte;
            quote! {
                #byte_lit => {
                    #(#bodies)*
                }
            }
        })
        .collect();

    quote! {
        'try_branches: loop {
            match first {
                #(#byte_arms)*
                _ => {}
            }
            #(#fallback_arms)*
            return Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            });
        }
    }
}

fn emit_branch_body_visitor(node: &IrNode, grammar_suffix: &str, ir: &GrammarIR) -> TokenStream {
    let inner = unwrap_trivia(node);
    match inner {
        IrNode::Ref(rid) => match emit_ref_call_visitor(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                {
                    let attempt_p = *p;
                    match #call {
                        Ok(_) => break 'try_branches,
                        Err(_) => { *p = attempt_p; }
                    }
                }
            },
            None => quote! {},
        },
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
                        break 'try_branches;
                    }
                }
            }
        }
        IrNode::Regex(_) => quote! {
            {
                let at = *p;
                let mut q = at;
                while q < input.len() {
                    let b = input[q];
                    if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
                        || b == b';' || b == b'}' || b == b'!'
                        || b == b',' || b == b'{' || b == b')'
                    {
                        break;
                    }
                    q += 1;
                }
                if q > at {
                    *p = q;
                    break 'try_branches;
                }
            }
        },
        IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
            emit_seq_attempt_visitor(inner, ir)
        }
        _ => quote! {},
    }
}

fn emit_seq_attempt_visitor(seq: &IrNode, ir: &GrammarIR) -> TokenStream {
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten(seq, &mut positions);
    let per_position: Vec<TokenStream> = positions
        .iter()
        .map(|pos| emit_seq_position(pos, ir))
        .collect();
    quote! {
        {
            let save_p = *p;
            let attempt = (|| -> ::core::result::Result<(), ()> {
                #(#per_position)*
                Ok(())
            })();
            match attempt {
                Ok(_) => break 'try_branches,
                Err(_) => { *p = save_p; }
            }
        }
    }
}
