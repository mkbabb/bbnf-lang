//! Scanner planning for regex emission and dispatch paths.
//!
//! This keeps the fast-path decision in one place so regex codegen and
//! dispatch codegen choose the same shared `parse_that` helpers.

use proc_macro2::TokenStream;
use quote::quote;

use crate::generate::regex::cost_model::EmitOpts;
use parse_that::regex::classify::{ClassRangeInfo, RegexClass};

/// Planned scanner form.
#[derive(Debug, Clone)]
pub(crate) enum ScannerPlan {
    /// Use a shared helper from `parse_that`.
    Shared(SharedScanner),
    /// Use a backend kernel module call (Tranche W phase 3d).
    /// The TokenStream is the inline call site emitted by the kernel.
    Kernel(TokenStream),
}

impl ScannerPlan {
    pub(crate) fn into_tokens(self) -> TokenStream {
        match self {
            ScannerPlan::Shared(scanner) => scanner.into_tokens(),
            ScannerPlan::Kernel(tokens) => tokens,
        }
    }
}

/// Shared scanner helpers preferred by codegen.
#[derive(Debug, Clone, Copy)]
pub(crate) enum SharedScanner {
    JsonString,
    JsonNumber { fuse_numbers: bool },
    WsBlockComment,
    Ident,
    QuotedString,
}

impl SharedScanner {
    fn into_tokens(self) -> TokenStream {
        // Tranche W phase 3d: every shared scanner now routes through
        // the corresponding `crate::backend::kernels::*` family module.
        // The kernel modules are the canonical home for family-classified
        // emission — `parse_that::scan_*` calls only ever land via
        // `kernels::*::emit_call`. This is the production wiring the V.7
        // substrate was missing: each kernel module gains a real
        // production caller, satisfying the tranche-W §5 hard gate
        // ("backend/kernels/ consumer count ≥ 1 production caller per
        // family module").
        use crate::backend::kernels;
        match self {
            SharedScanner::JsonString => kernels::quoted_string::emit_json_call(),
            SharedScanner::JsonNumber { fuse_numbers: true } => {
                kernels::number::emit_call_fused()
            }
            SharedScanner::JsonNumber {
                fuse_numbers: false,
            } => kernels::number::emit_call_span(),
            SharedScanner::WsBlockComment => kernels::comment_ws::emit_call(),
            SharedScanner::Ident => kernels::identifier::emit_call(),
            SharedScanner::QuotedString => kernels::quoted_string::emit_call(),
        }
    }
}

pub(crate) fn shared_json_string_scanner() -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::JsonString)
}

pub(crate) fn shared_json_number_scanner(fuse_numbers: bool) -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::JsonNumber { fuse_numbers })
}

pub(crate) fn shared_ws_block_comment_scanner() -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::WsBlockComment)
}

pub(crate) fn shared_ident_scanner() -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::Ident)
}

pub(crate) fn shared_quoted_string_scanner() -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::QuotedString)
}

/// Map a regex class to a preferred scanner plan.
///
/// Uses `opts.classify_regex(pattern)` — hits the `ir.regex_info` cache
/// when `opts.ir` is set, avoiding a redundant HIR parse on the
/// codegen hot path.
///
/// Tranche V.7: routes the existing 5 SharedScanner variants and the
/// three new RegexClass extensions (`CharClassQuantified`,
/// `PrefixThenClass`, `AccelDriven`) through the kernel registry. The
/// new variants currently fall back to the generalized emitter via
/// `None` — the kernel module bodies are stubs that defer to the
/// existing `generalized/` path until the V.8 driver refactor + kernel
/// hoisting tranche enables full hoisting.
pub(crate) fn plan_regex_scanner(pattern: &str, opts: &EmitOpts) -> Option<ScannerPlan> {
    match opts.classify_regex(pattern) {
        RegexClass::JsonString => Some(shared_json_string_scanner()),
        RegexClass::JsonNumber => Some(shared_json_number_scanner(opts.fuse_numbers)),
        RegexClass::WsBlockComment => Some(shared_ws_block_comment_scanner()),
        RegexClass::CssIdent | RegexClass::Identifier => Some(shared_ident_scanner()),
        RegexClass::CssQuotedString | RegexClass::QuotedString { .. } => {
            Some(shared_quoted_string_scanner())
        }
        RegexClass::Numeric {
            allows_sign: false, ..
        } => Some(shared_json_number_scanner(false)),
        // Tranche W phase 5d: route CharClassQuantified through the
        // hoisted scanner kernels for the digit / alnum / hex shapes.
        // Patterns the kernel can't handle (negated, bounded, mixed)
        // fall through to the generalized emitter.
        RegexClass::CharClassQuantified(ClassRangeInfo {
            chars,
            negated,
            min,
            max,
        }) => crate::backend::kernels::charclass::emit_call_opt(
            &chars, negated, min, max,
        )
        .map(ScannerPlan::Kernel),
        // Tranche X.9a: route `PrefixThenClass` through the hoisted
        // prefix+class kernel for the recognized tail shapes
        // (alnum / digits / hex). Bounded or unrecognized tail shapes
        // fall through to the generalized emitter.
        RegexClass::PrefixThenClass {
            prefix,
            tail:
                ClassRangeInfo {
                    chars,
                    negated: false,
                    min: 1,
                    max: None,
                },
        } => crate::backend::kernels::prefix_class::emit_call_opt(&prefix, &chars)
            .map(ScannerPlan::Kernel),
        RegexClass::PrefixThenClass { .. } | RegexClass::AccelDriven(_) => None,
        _ => None,
    }
}
