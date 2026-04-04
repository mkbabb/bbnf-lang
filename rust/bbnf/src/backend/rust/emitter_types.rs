//! RustEmitter struct and RustEmitCtx — types for the shared-driver Rust backend.

use std::collections::HashSet;

use bbnf_ir::RuleId;
use proc_macro2::TokenStream;
use quote::format_ident;

/// Rust code emitter implementing the [`Emitter`] trait.
pub struct RustEmitter {
    pub enum_ident: syn::Ident,
    pub effective_prettify: bool,
    pub fused_number_rules: HashSet<RuleId>,
}

/// Mutable context for Rust emission.
pub struct RustEmitCtx {
    pub hoisted: Vec<TokenStream>,
    pub counter: usize,
}

impl RustEmitCtx {
    pub fn new() -> Self {
        Self { hoisted: Vec::new(), counter: 0 }
    }

    pub fn fresh(&mut self, prefix: &str) -> syn::Ident {
        let id = self.counter;
        self.counter += 1;
        format_ident!("__{}{}", prefix, id)
    }
}
