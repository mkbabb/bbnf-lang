//! Generalized regex direct emission — strength-reduces regex patterns into
//! inline byte-scanning loops (char ranges, char sets, shorthand classes,
//! literal-prefix + class tail, whitespace-padded literals).

use proc_macro2::TokenStream;
use quote::quote;

use super::negated_class::try_strip_ws_padded_literal;

/// Whether a pattern is a simple character range like `[a-z]` or `[0-9]`.
/// Returns `(lo, hi)` byte range if detected.
fn is_single_char_range_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_prefix('[')?.strip_suffix(']')?;
    // Must be exactly "X-Y" where X and Y are single ASCII characters.
    if inner.len() == 3 && inner.as_bytes()[1] == b'-' {
        let lo = inner.as_bytes()[0];
        let hi = inner.as_bytes()[2];
        if lo.is_ascii() && hi.is_ascii() && lo < hi {
            return Some((lo, hi));
        }
    }
    None
}

/// Whether a pattern is a simple character set like `[abc]` (no ranges, no escapes).
/// Returns the set of bytes if detected (max 8 bytes for practical emission).
fn is_small_char_set_regex(pattern: &str) -> Option<Vec<u8>> {
    let inner = pattern.strip_prefix('[')?.strip_suffix(']')?;
    // Must not contain ranges, escapes, or negation.
    if inner.starts_with('^') || inner.contains('-') || inner.contains('\\') {
        return None;
    }
    let bytes: Vec<u8> = inner.bytes().collect();
    if bytes.len() >= 2 && bytes.len() <= 8 && bytes.iter().all(|b| b.is_ascii()) {
        Some(bytes)
    } else {
        None
    }
}

/// Whether a pattern is a single-char range with `+` quantifier like `[a-z]+`.
/// Returns `(lo, hi)` byte range.
fn is_char_range_plus_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_suffix('+')?;
    is_single_char_range_regex(inner)
}

/// Whether a pattern is a single-char range with `*` quantifier like `[a-z]*`.
/// Returns `(lo, hi)` byte range.
fn is_char_range_star_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_suffix('*')?;
    is_single_char_range_regex(inner)
}

/// Emit a direct call for generalized regex patterns beyond JSON/CSS.
///
/// Covers:
/// - `[a-z]` → single byte range check
/// - `[abc]` → small character set match (2-8 chars)
/// - `[a-z]+` → byte range scan loop
/// - `[a-z]*` → byte range scan loop (zero-or-more)
/// - `\s+`, `\d+`, `\w+` → shorthand class scan loops
/// - `prefix[class]+` → literal prefix + char class tail (e.g., `--[\w-]+`, `@[a-z][\w-]*`)
pub fn emit_generalized_regex_direct(pattern: &str) -> Option<TokenStream> {
    // Whitespace-padded literal: \s*LITERAL\s* — matches a fixed literal
    // with optional surrounding whitespace. Common in CSS for comma separators,
    // combinator operators, etc. Generalizes the \s*,\s* pattern.
    if let Some(inner) = try_strip_ws_padded_literal(pattern) {
        let inner_bytes = inner.as_bytes();
        let inner_len = inner_bytes.len();
        let byte_lits: Vec<proc_macro2::Literal> = inner_bytes
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        let check = if inner_len == 1 {
            quote! {
                __pos < state.src_bytes.len()
                    && unsafe { *state.src_bytes.get_unchecked(__pos) } == #(#byte_lits)*
            }
        } else {
            quote! {
                state.src_bytes.get(__pos..__pos + #inner_len) == Some(&[#(#byte_lits),*] as &[u8])
            }
        };
        return Some(quote! {
            {
                let __start = state.offset;
                let mut __pos = __start;
                while __pos < state.src_bytes.len()
                    && unsafe { *state.src_bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                { __pos += 1; }
                if #check {
                    __pos += #inner_len;
                    while __pos < state.src_bytes.len()
                        && unsafe { *state.src_bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                    { __pos += 1; }
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        });
    }

    // Single character range: [a-z]
    if let Some((lo, hi)) = is_single_char_range_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if __b >= #lo_lit && __b <= #hi_lit {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        });
    }

    // Small character set: [abc]
    if let Some(bytes) = is_small_char_set_regex(pattern) {
        let byte_lits: Vec<_> = bytes
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if matches!(__b, #(#byte_lits)|*) {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        });
    }

    // Character range with + quantifier: [a-z]+
    if let Some((lo, hi)) = is_char_range_plus_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if __b >= #lo_lit && __b <= #hi_lit {
                        __pos += 1;
                    } else {
                        break;
                    }
                }
                if __pos > __start {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        });
    }

    // Character range with * quantifier: [a-z]*
    if let Some((lo, hi)) = is_char_range_star_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if __b >= #lo_lit && __b <= #hi_lit {
                        __pos += 1;
                    } else {
                        break;
                    }
                }
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            }
        });
    }

    // General char class loop: [0-9a-fA-F]+, [\w-]+, etc.
    if let Some(ts) = emit_char_class_loop(pattern) {
        return Some(ts);
    }

    // Shorthand class with quantifier: \s+, \d+, \w+, \s*, \d*, \w*
    if let Some(ts) = emit_shorthand_class_loop(pattern) {
        return Some(ts);
    }

    // Literal prefix followed by char class: --[\w-]+, @[a-zA-Z][\w-]*, etc.
    if let Some(ts) = emit_literal_prefix_class(pattern) {
        return Some(ts);
    }

    None
}

/// Try to emit a byte-predicate loop for `[charclass]+` or `[charclass]*` patterns.
/// Handles multi-range classes like `[0-9a-fA-F]` and shorthand `\w`.
pub(crate) fn emit_char_class_loop(pattern: &str) -> Option<TokenStream> {
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

/// Emit inline byte scan for shorthand class + quantifier: `\s+`, `\d+`, `\w+`, `\s*`, etc.
///
/// These are bare escape sequences (not wrapped in `[...]`) followed by `+` or `*`.
/// Compiles to a tight byte-predicate loop with no regex engine overhead.
pub(crate) fn emit_shorthand_class_loop(pattern: &str) -> Option<TokenStream> {
    let (shorthand, is_plus) = if let Some(s) = pattern.strip_suffix('+') {
        (s, true)
    } else if let Some(s) = pattern.strip_suffix('*') {
        (s, false)
    } else {
        return None;
    };

    // Must be exactly a shorthand: \s, \d, \w
    let predicate = match shorthand {
        r"\s" => quote! { __b.is_ascii_whitespace() },
        r"\d" => quote! { __b.is_ascii_digit() },
        r"\w" => quote! { (__b.is_ascii_alphanumeric() || __b == b'_') },
        _ => return None,
    };

    if is_plus {
        Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if #predicate { __pos += 1; } else { break; }
                }
                if __pos > __start {
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
pub(crate) fn emit_literal_prefix_class(pattern: &str) -> Option<TokenStream> {
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

/// A parsed char class segment: predicate expression + quantifier.
pub(crate) struct ClassSegment {
    predicate: TokenStream,
    min: usize,
    max: usize, // usize::MAX = unbounded
}

/// Parse a sequence of `[class]quantifier?` segments from a pattern suffix.
/// Returns None if the pattern contains anything unparseable.
pub(crate) fn parse_class_segments(mut s: &str) -> Option<Vec<ClassSegment>> {
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

/// Unescape a regex literal prefix (before the first `[`).
/// Returns the raw bytes, or None if the prefix contains regex metacharacters.
pub(crate) fn unescape_regex_prefix(prefix: &str) -> Option<Vec<u8>> {
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
pub(crate) fn emit_prefix_segments(
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

/// Convert a regex character class body (without [ ]) to a Rust byte predicate.
/// Returns a TokenStream expression that checks if `__b: u8` matches.
pub(crate) fn char_class_to_predicate(class: &str) -> Option<TokenStream> {
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
