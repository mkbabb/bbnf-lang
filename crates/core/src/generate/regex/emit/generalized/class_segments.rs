//! Literal-prefix + char class subsystem — `ClassSegment`, parsing, predicate
//! generation, and prefix+segment emission.

use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::kernels;

/// A parsed char class segment: predicate expression + quantifier.
pub(super) struct ClassSegment {
    pub(super) predicate: TokenStream,
    pub(super) min: usize,
    pub(super) max: usize, // usize::MAX = unbounded
}

/// Parse a sequence of `[class]quantifier?` segments from a pattern suffix.
/// Returns None if the pattern contains anything unparseable.
pub(super) fn parse_class_segments(mut s: &str) -> Option<Vec<ClassSegment>> {
    let mut segments = Vec::new();

    while !s.is_empty() {
        // Must start with [
        if !s.starts_with('[') {
            return None;
        }
        // Find the matching ]
        let close = s.find(']')?;
        let inner = &s[1..close];
        if inner.starts_with('^') {
            return None; // Negated classes handled elsewhere.
        }
        let predicate = char_class_to_predicate(inner)?;

        let after = &s[close + 1..];

        // Parse quantifier.
        let (min, max, rest) = if let Some(r) = after.strip_prefix('+') {
            (1, usize::MAX, r)
        } else if let Some(r) = after.strip_prefix('*') {
            (0, usize::MAX, r)
        } else if let Some(r) = after.strip_prefix('?') {
            (0, 1, r)
        } else if after.starts_with('{') {
            // Bounded quantifier: {n,m} or {n}
            let brace_end = after.find('}')?;
            let quant = &after[1..brace_end];
            let rest = &after[brace_end + 1..];
            if let Some(comma) = quant.find(',') {
                let min: usize = quant[..comma].parse().ok()?;
                let max: usize = quant[comma + 1..].parse().ok()?;
                (min, max, rest)
            } else {
                let exact: usize = quant.parse().ok()?;
                (exact, exact, rest)
            }
        } else {
            // No quantifier — exactly 1.
            (1, 1, after)
        };

        segments.push(ClassSegment {
            predicate,
            min,
            max,
        });
        s = rest;
    }

    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

/// Try to emit a byte-predicate loop for `[charclass]+` or `[charclass]*` patterns.
/// Handles multi-range classes like `[0-9a-fA-F]` and shorthand `\w`.
pub(super) fn emit_char_class_loop(pattern: &str) -> Option<TokenStream> {
    // Strip quantifier: +, *, or {n,m}.
    let (class_str, min_count, max_count) = if let Some(s) = pattern.strip_suffix('+') {
        (s, 1usize, usize::MAX)
    } else if let Some(s) = pattern.strip_suffix('*') {
        (s, 0usize, usize::MAX)
    } else if let Some(brace_start) = pattern.rfind('{') {
        let class_str = &pattern[..brace_start];
        let quant = &pattern[brace_start + 1..pattern.len() - 1]; // strip { }
        if !pattern.ends_with('}') {
            return None;
        }
        if let Some(comma) = quant.find(',') {
            let min: usize = quant[..comma].parse().ok()?;
            let max: usize = quant[comma + 1..].parse().ok()?;
            (class_str, min, max)
        } else {
            let exact: usize = quant.parse().ok()?;
            (class_str, exact, exact)
        }
    } else {
        return None;
    };

    let _is_plus = min_count >= 1 && max_count == usize::MAX;

    // Must be a single char class [...]
    let inner = class_str.strip_prefix('[')?.strip_suffix(']')?;
    if inner.starts_with('^') {
        return None; // Negated classes handled by is_negated_char_class_regex.
    }

    // Tranche X phase 1: route through `kernels::charclass::emit_call_opt`
    // first. The kernel collapses `[0-9]+`, `[0-9]*`, `[a-zA-Z0-9]+`,
    // `[0-9a-fA-F]+` onto the hoisted `parse_that::scan_*_mut` helpers.
    // Falls through to the inline predicate emission for shapes the
    // kernel doesn't recognize. Per §3 rule 16 this is the structural
    // kernel routing gate for the generalized char-class loop path.
    if max_count == usize::MAX && (min_count == 0 || min_count == 1) {
        if let Some((chars, negated)) = kernels::charclass::charset_from_class_body(inner) {
            if let Some(call) =
                kernels::charclass::emit_call_opt(&chars, negated, min_count as u32, None)
            {
                return Some(quote! { { #call } });
            }
        }
    }

    // Build a byte predicate expression from the char class contents.
    let predicate = char_class_to_predicate(inner)?;

    let min_lit = proc_macro2::Literal::usize_unsuffixed(min_count);
    let max_lit = proc_macro2::Literal::usize_unsuffixed(max_count);

    if max_count == usize::MAX {
        // Unbounded repeat: + or *
        if min_count >= 1 {
            Some(quote! {
                {
                    let __start = state.offset;
                    let __end = state.src_bytes.len();
                    let mut __pos = __start;
                    while __pos < __end {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; } else { break; }
                    }
                    if __pos >= __start + #min_lit {
                        state.offset = __pos;
                        Some(::parse_that::Span::new(__start, __pos, state.src))
                    } else {
                        None
                    }
                }
            })
        } else {
            Some(quote! {
                {
                    let __start = state.offset;
                    let __end = state.src_bytes.len();
                    let mut __pos = __start;
                    while __pos < __end {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; } else { break; }
                    }
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                }
            })
        }
    } else {
        // Bounded repeat: {n,m}
        Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                let mut __count: usize = 0;
                while __pos < __end && __count < #max_lit {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if #predicate { __pos += 1; __count += 1; } else { break; }
                }
                if __count >= #min_lit {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        })
    }
}

/// Convert a regex character class body (without [ ]) to a Rust byte predicate.
/// Returns a TokenStream expression that checks if `__b: u8` matches.
pub(super) fn char_class_to_predicate(class: &str) -> Option<TokenStream> {
    let mut conditions: Vec<TokenStream> = Vec::new();
    let mut chars = class.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Escape sequence.
            let esc = chars.next()?;
            match esc {
                'w' => conditions.push(quote! { (__b.is_ascii_alphanumeric() || __b == b'_') }),
                'd' => conditions.push(quote! { __b.is_ascii_digit() }),
                's' => conditions.push(quote! { __b.is_ascii_whitespace() }),
                _ => {
                    let byte = match esc {
                        'n' => b'\n',
                        'r' => b'\r',
                        't' => b'\t',
                        other if other.is_ascii() => other as u8,
                        _ => return None,
                    };
                    let lit = proc_macro2::Literal::byte_character(byte);
                    conditions.push(quote! { __b == #lit });
                }
            }
        } else if c.is_ascii() {
            // Check for range: a-z
            if chars.peek() == Some(&'-') {
                chars.next(); // consume '-'
                let hi = chars.next()?;
                if !hi.is_ascii() {
                    return None;
                }
                let lo_lit = proc_macro2::Literal::byte_character(c as u8);
                let hi_lit = proc_macro2::Literal::byte_character(hi as u8);
                conditions.push(quote! { (__b >= #lo_lit && __b <= #hi_lit) });
            } else {
                let lit = proc_macro2::Literal::byte_character(c as u8);
                conditions.push(quote! { __b == #lit });
            }
        } else {
            return None; // Non-ASCII in char class — bail.
        }
    }

    if conditions.is_empty() {
        return None;
    }

    if conditions.len() == 1 {
        Some(conditions.into_iter().next().unwrap())
    } else {
        Some(quote! { #(#conditions)||* })
    }
}

/// Unescape a regex literal prefix (before the first `[`).
/// Returns the raw bytes, or None if the prefix contains regex metacharacters.
pub(super) fn unescape_regex_prefix(prefix: &str) -> Option<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut chars = prefix.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Escaped character.
            let esc = chars.next()?;
            let b = match esc {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '\\' | '/' | '.' | '-' | '_' | '@' | '#' => esc as u8,
                _ if esc.is_ascii_punctuation() => esc as u8,
                _ => return None,
            };
            bytes.push(b);
        } else if "[](){}|*+?.^$".contains(c) {
            // Unescaped regex metacharacter in the prefix — not a literal prefix.
            return None;
        } else if c.is_ascii() {
            bytes.push(c as u8);
        } else {
            return None;
        }
    }
    Some(bytes)
}

/// Emit the scanning code for a literal prefix followed by class segments.
///
/// Uses a closure IIFE to scope early returns — the emitted code evaluates to
/// `Option<Span>` without leaking `return` into the caller's function.
pub(super) fn emit_prefix_segments(
    prefix_lit: &proc_macro2::Literal,
    prefix_len: usize,
    segments: &[ClassSegment],
) -> Option<TokenStream> {
    let prefix_len_lit = proc_macro2::Literal::usize_unsuffixed(prefix_len);

    // Build the scanning body from segments.
    let mut scan_stmts: Vec<TokenStream> = Vec::new();
    // Track the overall minimum characters required after the prefix.
    let mut total_min: usize = 0;

    // Check if any segment needs early-exit (exact-1 or bounded min > 0).
    // If so, we wrap the prefix-match body in a closure to scope `return`.
    let mut needs_closure = false;

    for seg in segments {
        let predicate = &seg.predicate;
        total_min += seg.min;

        if seg.max == usize::MAX {
            // Unbounded: scan loop (no early exit needed)
            scan_stmts.push(quote! {
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if #predicate { __pos += 1; } else { break; }
                }
            });
        } else if seg.max == 1 && seg.min == 1 {
            // Exactly one character — needs early exit on mismatch.
            needs_closure = true;
            scan_stmts.push(quote! {
                {
                    if __pos < __end {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; } else { return None; }
                    } else {
                        return None;
                    }
                }
            });
        } else if seg.max == 1 && seg.min == 0 {
            // Optional: 0 or 1
            scan_stmts.push(quote! {
                if __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if #predicate { __pos += 1; }
                }
            });
        } else {
            // Bounded: {min,max}
            if seg.min > 0 {
                needs_closure = true;
            }
            let min_lit = proc_macro2::Literal::usize_unsuffixed(seg.min);
            let max_lit = proc_macro2::Literal::usize_unsuffixed(seg.max);
            scan_stmts.push(quote! {
                {
                    let mut __count: usize = 0;
                    while __pos < __end && __count < #max_lit {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; __count += 1; } else { break; }
                    }
                    if __count < #min_lit { return None; }
                }
            });
        }
    }

    let total_min_lit = proc_macro2::Literal::usize_unsuffixed(total_min);

    // For segments with min > 0, we need a minimum-length check after scanning.
    let has_min_requirement = total_min > 0;

    let check = if has_min_requirement {
        quote! {
            if __pos >= __start + #prefix_len_lit + #total_min_lit {
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            } else {
                None
            }
        }
    } else {
        quote! {
            state.offset = __pos;
            Some(::parse_that::Span::new(__start, __pos, state.src))
        }
    };

    // When segments contain early-exit returns, wrap the prefix-match body
    // in a closure IIFE so `return None` is scoped to the closure, not the
    // caller's function.
    let prefix_body = quote! {
        let mut __pos = __start + #prefix_len_lit;
        #(#scan_stmts)*
        #check
    };

    let inner = if needs_closure {
        quote! {
            (|| {
                #prefix_body
            })()
        }
    } else {
        quote! { { #prefix_body } }
    };

    Some(quote! {
        {
            let __start = state.offset;
            let __end = state.src_bytes.len();
            if state.src_bytes.get(__start..__start + #prefix_len_lit) == Some(#prefix_lit as &[u8]) {
                #inner
            } else {
                None
            }
        }
    })
}

/// Emit inline byte scan for literal-prefix + char class tail patterns.
///
/// Detects patterns like:
/// - `--[\w-]+` → check `--` prefix, then scan `[\w-]` bytes
/// - `@[a-zA-Z][\w-]*` → check `@` prefix, match one `[a-zA-Z]`, then scan `[\w-]*`
///
/// The literal prefix is matched exactly, followed by one or more char class segments
/// each with their own quantifier. The overall match requires the prefix plus at least
/// whatever the quantifiers mandate (e.g., `+` requires at least one char class byte
/// after the prefix).
pub(super) fn emit_literal_prefix_class(pattern: &str) -> Option<TokenStream> {
    // Find where the first `[` starts — everything before it is the literal prefix.
    let bracket_pos = pattern.find('[')?;
    if bracket_pos == 0 {
        return None; // No literal prefix — handled by other paths.
    }

    let prefix = &pattern[..bracket_pos];
    // Validate prefix: must be plain ASCII bytes, no regex metacharacters.
    // Allow: letters, digits, `-`, `_`, `@`, `#`, `.` and backslash escapes.
    let prefix_bytes = unescape_regex_prefix(prefix)?;
    if prefix_bytes.is_empty() {
        return None;
    }

    let tail = &pattern[bracket_pos..];

    // Tranche X.9a: kernel routing short-circuit for the common
    // `prefix[class]+` shape. The kernel collapses the emission onto
    // the hoisted `scan_<alnum|digits|hex>_mut` helpers for the
    // recognized tail shapes. Falls through to the inline predicate
    // emission for shapes the kernel doesn't recognize. Per §3 rule 16
    // this is the **structural** kernel routing gate for the literal-
    // prefix+class path.
    if let Some(tail_inner) = tail.strip_prefix('[') {
        // Single-segment `+` tail: `[class]+`
        if let Some(class_body) = tail_inner.strip_suffix('+').and_then(|s| s.strip_suffix(']')) {
            if let Some((chars, false)) =
                kernels::charclass::charset_from_class_body(class_body)
            {
                if let Some(call) =
                    kernels::prefix_class::emit_call_opt(&prefix_bytes, &chars)
                {
                    return Some(call);
                }
            }
        }
    }

    // Parse the tail as a sequence of char class segments.
    // Each segment: `[class]quantifier?`
    // We fuse them into a single scan loop when possible.
    let segments = parse_class_segments(tail)?;
    if segments.is_empty() {
        return None;
    }

    let prefix_len = prefix_bytes.len();
    let prefix_lit = proc_macro2::Literal::byte_string(&prefix_bytes);

    // Simple case: single segment with + or * quantifier.
    // This covers --[\w-]+, @[a-zA-Z][\w-]* (two segments but we handle that too).
    emit_prefix_segments(&prefix_lit, prefix_len, &segments)
}
