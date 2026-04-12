//! Leaf-op emission for the Rust backend: literals, regex, epsilon.
//!
//! Under Tranche AC.2 tape-first, leaves emit side-effecting token
//! streams of type `Option<()>`. A successful match advances
//! `state.offset`; the tape is untouched — the enclosing rule's
//! prelude / epilogue owns the `push_leaf` / `push_compound` call.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can
//! delegate to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_literal_match_impl(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let bytes = value.as_bytes();

        if guaranteed_byte.is_some() {
            // Dispatch already proved this byte — just advance.
            return quote! {
                {
                    state.offset += 1;
                    Some(())
                }
            };
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            quote! {
                {
                    if state.offset < state.src_bytes.len()
                        && state.src_bytes[state.offset] == #byte
                    {
                        state.offset += 1;
                        Some(())
                    } else {
                        None
                    }
                }
            }
        } else {
            // Byte-array literal equality: load N bytes as `[u8; N]`
            // and compare with `*b"..."`. LLVM lowers this to a
            // single iN load + icmp for N in {2,4,8} and a
            // half-word + tail-byte combo for N in {3,5,6,7},
            // never invoking memcmp.
            let len = bytes.len();
            let lit = proc_macro2::Literal::byte_string(bytes);
            quote! {
                {
                    if state.offset + #len <= state.src_bytes.len()
                        && unsafe {
                            *(state.src_bytes.as_ptr().add(state.offset)
                                as *const [u8; #len])
                        } == *#lit
                    {
                        state.offset += #len;
                        Some(())
                    } else {
                        None
                    }
                }
            }
        }
    }

    pub(super) fn emit_regex_match_impl(
        &mut self,
        pattern: &str,
        _regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AN Phase 0: for number patterns with F64 payload active,
        // emit scan_number_f64 to capture the parsed value in the
        // payload variable. The IR passes strip Map { NumberConvert }
        // down to bare Regex, so we must detect the number pattern
        // here at the regex emission level.
        if ctx.payload_kind == Some(crate::backend::rust::emitter_types::PayloadKind::F64) {
            use parse_that::regex::classify::{RegexClass, classify_regex};
            if matches!(
                classify_regex(pattern),
                RegexClass::Numeric { .. } | RegexClass::JsonNumber
            ) {
                return quote! {
                    match ::parse_that::scan_number_f64(state) {
                        Some(__v) => { __payload_f64 = __v; __has_payload = true; Some(()) }
                        None => None,
                    }
                };
            }
        }

        // Default: span-only scan. The shared regex emitter returns
        // `Option<Span>`; we discard the Span and re-express as
        // `Option<()>` so the tape-first composition pattern holds.
        let opts =
            crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                .with_fuse(!self.effective_prettify)
                .with_ir(ir);
        let regex_expr = crate::generate::regex::emit_regex(pattern, &opts);
        quote! {
            { (#regex_expr).map(|_| ()) }
        }
    }

    pub(super) fn emit_epsilon_impl(&mut self, _ctx: &mut RustEmitCtx) -> TokenStream {
        // Epsilon: matches without advancing, no tape side effect.
        quote! { Some(()) }
    }

    pub(super) fn emit_seq_all_span_impl(
        &mut self,
        child_outputs: Vec<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // All children are side-effecting `Option<()>`. Chain them
        // together under a labeled block for short-circuit failure.
        let child_checks: Vec<TokenStream> = child_outputs
            .into_iter()
            .map(|c| quote! {
                match (#c) {
                    Some(_) => (),
                    None => break 'span_blk None,
                }
            })
            .collect();
        quote! {
            'span_blk: {
                #( #child_checks )*
                Some(())
            }
        }
    }
}
