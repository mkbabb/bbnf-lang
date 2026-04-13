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
///
/// The `Ident` variant carries the dialect flags lifted from
/// `RegexClass::Identifier` so the kernel call selects the right
/// `IdentConfig` per pattern (bare / vendor-prefixed /
/// custom-property / full CSS fold).
#[derive(Debug, Clone, Copy)]
pub(crate) enum SharedScanner {
    JsonString,
    JsonNumber { fuse_numbers: bool },
    WsBlockComment,
    Ident {
        allow_leading_dash: bool,
        allow_double_dash_prefix: bool,
    },
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
            SharedScanner::JsonString => kernels::quoted_string::emit_call_strict(),
            SharedScanner::JsonNumber { fuse_numbers: true } => {
                kernels::number::emit_call_fused()
            }
            SharedScanner::JsonNumber {
                fuse_numbers: false,
            } => kernels::number::emit_call_span(),
            SharedScanner::WsBlockComment => kernels::comment_ws::emit_call(),
            SharedScanner::Ident {
                allow_leading_dash,
                allow_double_dash_prefix,
            } => kernels::identifier::emit_call_with_flags(
                allow_leading_dash,
                allow_double_dash_prefix,
            ),
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

/// Build an `Ident` scanner plan parameterized by the CSS dialect
/// flags lifted from `RegexClass::Identifier`. The kernel emission
/// picks the appropriate pre-declared `IdentConfig` constant when
/// the flag combination is one of the canonical dialects, and emits
/// an inline `IdentConfig` literal otherwise.
pub(crate) fn shared_ident_scanner(
    allow_leading_dash: bool,
    allow_double_dash_prefix: bool,
) -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::Ident {
        allow_leading_dash,
        allow_double_dash_prefix,
    })
}

pub(crate) fn shared_quoted_string_scanner() -> ScannerPlan {
    ScannerPlan::Shared(SharedScanner::QuotedString)
}

/// Map a regex class to a preferred scanner plan.
///
/// Tranche X.8d: the primary path consults the CSP-decided
/// `RegexEngine` via [`EmitOpts::regex_engine_decision`]. When the
/// CSP has an authoritative decision for this pattern and it's a
/// non-kernel engine (Memchr*, NibbleLut, OnePass, SmallDfa, Dfa),
/// this function returns `None` so the caller routes through the
/// generalized/hir/dfa emitters downstream. For `FamilyHelper`
/// decisions (or no decision), the fall-through classifies the
/// pattern structurally and routes to the matching shared / kernel
/// scanner.
///
/// The `classify_regex` path survives only as the fall-through.
///
/// Uses `opts.classify_regex(pattern)` — hits the `ir.regex_info`
/// cache when `opts.ir` is set, avoiding a redundant HIR parse on
/// the codegen hot path.
pub(crate) fn plan_regex_scanner(pattern: &str, opts: &EmitOpts) -> Option<ScannerPlan> {
    // Tranche X.8d — primary path: honor the CSP's `RegexEngine`
    // decision when one exists. The CSP already picked the
    // lowest-cost feasible engine for this pattern, so we can
    // shortcut the classify re-walk for non-kernel engines. The
    // `FamilyHelper` variant falls through because the concrete
    // family selection is data-driven on `RegexClass`.
    use bbnf_ir::passes::csp_strategy::RegexEngine;
    match opts.regex_engine_decision(pattern) {
        Some(
            RegexEngine::Memchr1
            | RegexEngine::Memchr2
            | RegexEngine::Memchr3
            | RegexEngine::NibbleLut
            | RegexEngine::OnePass
            | RegexEngine::SmallDfa
            | RegexEngine::Dfa,
        ) => {
            return None;
        }
        Some(RegexEngine::FamilyHelper) | None => {
            // Fall through to the classify path below.
        }
    }

    match opts.classify_regex(pattern) {
        // JSON-style strings carry the `\uXXXX` escape vocabulary;
        // the JSON scanner kernel is fastest on those.
        RegexClass::QuotedString {
            allows_u_escapes: true,
            ..
        } => Some(shared_json_string_scanner()),
        // JSON-style numbers fold sign + exponent + leading-zero
        // rejection; route through the fused/span number scanner.
        RegexClass::Numeric {
            reject_leading_zero: true,
            ..
        } => Some(shared_json_number_scanner(opts.fuse_numbers)),
        RegexClass::WhitespaceWithBlockComment => {
            Some(shared_ws_block_comment_scanner())
        }
        RegexClass::Identifier {
            allows_leading_dash,
            allows_double_dash_prefix,
        } => Some(shared_ident_scanner(
            allows_leading_dash,
            allows_double_dash_prefix,
        )),
        RegexClass::QuotedString { .. } => Some(shared_quoted_string_scanner()),
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
