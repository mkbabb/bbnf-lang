//! Debug trace codegen helpers.
//!
//! Generates `#[cfg(feature = "parser-trace")]` guarded trace calls at
//! rule entry/exit for the monolithic codegen path.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the thread-local depth counter declaration.
///
/// Generated once per impl block. Uses the same pattern as parse_that's `DEBUG_DEPTH`.
pub fn emit_depth_counter() -> TokenStream {
    quote! {
        #[cfg(feature = "parser-trace")]
        std::thread_local! {
            static __TRACE_DEPTH: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
        }
    }
}

/// Emit trace instrumentation at rule function entry.
///
/// Increments depth, prints `→ ruleName at offset N`.
/// Returns a statement block (not an expression).
pub fn emit_trace_entry(rule_name: &str) -> TokenStream {
    quote! {
        #[cfg(feature = "parser-trace")]
        {
            __TRACE_DEPTH.with(|d| d.set(d.get() + 1));
            let __depth = __TRACE_DEPTH.with(|d| d.get());
            let __indent = "  ".repeat(__depth.saturating_sub(1));
            eprintln!("{}→ {} at offset {}", __indent, #rule_name, state.offset);
        }
    }
}

/// Emit trace instrumentation at rule function exit.
///
/// Prints `← ruleName ✓/✗`, decrements depth.
/// `result_expr` is the identifier holding `Option<T>`.
pub fn emit_trace_exit(rule_name: &str, result_expr: &syn::Ident) -> TokenStream {
    quote! {
        #[cfg(feature = "parser-trace")]
        {
            let __depth = __TRACE_DEPTH.with(|d| d.get());
            let __indent = "  ".repeat(__depth.saturating_sub(1));
            let __status = if #result_expr.is_some() { "✓" } else { "✗" };
            eprintln!("{}← {} {}", __indent, #rule_name, __status);
            __TRACE_DEPTH.with(|d| d.set(d.get() - 1));
        }
    }
}
