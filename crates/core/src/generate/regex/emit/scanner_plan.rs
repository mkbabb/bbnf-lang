//! Scanner planning for regex emission and dispatch paths.
//!
//! This keeps the fast-path decision in one place so regex codegen and
//! dispatch codegen choose the same shared `parse_that` helpers.

use proc_macro2::TokenStream;
use quote::quote;

use crate::generate::regex::classify::{classify_regex, RegexClass};

/// Planned scanner form.
#[derive(Debug, Clone)]
pub(crate) enum ScannerPlan {
    /// Use a shared helper from `parse_that`.
    Shared(SharedScanner),
}

impl ScannerPlan {
    pub(crate) fn into_tokens(self) -> TokenStream {
        match self {
            ScannerPlan::Shared(scanner) => scanner.into_tokens(),
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
        match self {
            SharedScanner::JsonString => quote! { ::parse_that::scan_json_string(state) },
            SharedScanner::JsonNumber { fuse_numbers: true } => {
                quote! { ::parse_that::scan_number_convert_json(state) }
            }
            SharedScanner::JsonNumber {
                fuse_numbers: false,
            } => {
                quote! { ::parse_that::scan_number_span_json(state) }
            }
            SharedScanner::WsBlockComment => quote! { ::parse_that::scan_ws_block_comments(state) },
            SharedScanner::Ident => quote! { ::parse_that::scan_ident(state) },
            SharedScanner::QuotedString => quote! { ::parse_that::scan_string_quoted(state) },
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
pub(crate) fn plan_regex_scanner(pattern: &str, fuse_numbers: bool) -> Option<ScannerPlan> {
    match classify_regex(pattern) {
        RegexClass::JsonString => Some(shared_json_string_scanner()),
        RegexClass::JsonNumber => Some(shared_json_number_scanner(fuse_numbers)),
        RegexClass::WsBlockComment => Some(shared_ws_block_comment_scanner()),
        RegexClass::CssIdent | RegexClass::Identifier => Some(shared_ident_scanner()),
        RegexClass::CssQuotedString | RegexClass::QuotedString { .. } => {
            Some(shared_quoted_string_scanner())
        }
        RegexClass::Numeric {
            allows_sign: false, ..
        } => Some(shared_json_number_scanner(false)),
        _ => None,
    }
}
