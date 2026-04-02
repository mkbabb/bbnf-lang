//! Inline scanner emitters — emit scanner byte loops directly in generated code,
//! eliminating cross-crate function call overhead.
//!
//! These replace `::parse_that::scan_ident(state)` etc. with the equivalent
//! inline byte operations. LLVM sees the loop in context and can hoist
//! invariants, fuse with surrounding code, and eliminate redundant bounds checks.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit an inline ident scanner: `-?[a-zA-Z_][\w-]* | --[\w-]+`
///
/// Returns `Option<Span<'a>>`, operating on `state: &mut ParserState<'a>`.
/// Replaces `::parse_that::scan_ident(state)`.
pub fn emit_inline_ident_scanner() -> TokenStream {
    quote! {
        (|| {
            let __bytes = state.src_bytes;
            let __start = state.offset;
            let __len = __bytes.len();
            if __start >= __len { return None; }

            let mut __i = __start;
            let __b0 = unsafe { *__bytes.get_unchecked(__i) };

            if __b0 == b'-' {
                __i += 1;
                if __i >= __len { return None; }
                let __b1 = unsafe { *__bytes.get_unchecked(__i) };
                if __b1 == b'-' {
                    // Custom property: --[\w-]+
                    __i += 1;
                    while __i < __len {
                        let __b = unsafe { *__bytes.get_unchecked(__i) };
                        if __b.is_ascii_alphanumeric() || __b == b'_' || __b == b'-' {
                            __i += 1;
                        } else {
                            break;
                        }
                    }
                    if __i == __start + 2 { return None; }
                    state.offset = __i;
                    return Some(::parse_that::Span::new(__start, __i, state.src));
                }
                if !(__b1.is_ascii_alphabetic() || __b1 == b'_') {
                    return None;
                }
                __i += 1;
            } else if __b0.is_ascii_alphabetic() || __b0 == b'_' {
                __i += 1;
            } else {
                return None;
            }

            // Continue: [a-zA-Z0-9_-]*
            while __i < __len {
                let __b = unsafe { *__bytes.get_unchecked(__i) };
                if __b.is_ascii_alphanumeric() || __b == b'_' || __b == b'-' {
                    __i += 1;
                } else {
                    break;
                }
            }

            state.offset = __i;
            Some(::parse_that::Span::new(__start, __i, state.src))
        })()
    }
}

/// Emit an inline whitespace + block comment scanner: `(\s | /\*...\*/)*`
///
/// Always succeeds (returns empty span if no ws/comments found).
/// Replaces `::parse_that::scan_ws_block_comments(state)`.
pub fn emit_inline_ws_comment_scanner() -> TokenStream {
    quote! {
        {
            let __bytes = state.src_bytes;
            let __start = state.offset;
            let __len = __bytes.len();
            let mut __i = __start;

            loop {
                // Skip ASCII whitespace
                while __i < __len {
                    let __b = unsafe { *__bytes.get_unchecked(__i) };
                    if __b == b' ' || __b == b'\t' || __b == b'\n' || __b == b'\r' || __b == b'\x0C' {
                        __i += 1;
                    } else {
                        break;
                    }
                }

                // Check for block comment /*...*/
                if __i + 1 < __len
                    && unsafe { *__bytes.get_unchecked(__i) } == b'/'
                    && unsafe { *__bytes.get_unchecked(__i + 1) } == b'*'
                {
                    __i += 2;
                    loop {
                        match memchr::memchr(b'*', __bytes.get(__i..).unwrap_or(&[])) {
                            None => {
                                __i = __len;
                                break;
                            }
                            Some(__pos) => {
                                __i += __pos + 1;
                                if __i < __len && unsafe { *__bytes.get_unchecked(__i) } == b'/' {
                                    __i += 1;
                                    break;
                                }
                            }
                        }
                    }
                    continue;
                }

                break;
            }

            state.offset = __i;
            Some(::parse_that::Span::new(__start, __i, state.src))
        }
    }
}

/// Emit an inline quoted string scanner: `"..." | '...'` with `\`-escapes.
///
/// Returns span including quote delimiters.
/// Replaces `::parse_that::scan_string_quoted(state)`.
pub fn emit_inline_string_scanner() -> TokenStream {
    quote! {
        (|| {
            let __bytes = state.src_bytes;
            let __start = state.offset;
            if __start >= __bytes.len() { return None; }

            let __quote = unsafe { *__bytes.get_unchecked(__start) };
            if __quote != b'"' && __quote != b'\'' { return None; }

            let mut __i = __start + 1;
            loop {
                match memchr::memchr2(__quote, b'\\', __bytes.get(__i..).unwrap_or(&[])) {
                    None => return None, // unterminated string
                    Some(__pos) => {
                        __i += __pos;
                        if unsafe { *__bytes.get_unchecked(__i) } == __quote {
                            __i += 1;
                            state.offset = __i;
                            return Some(::parse_that::Span::new(__start, __i, state.src));
                        }
                        // backslash: skip next byte
                        __i += 1;
                        if __i >= __bytes.len() { return None; }
                        __i += 1; // skip escaped character
                    }
                }
            }
        })()
    }
}
