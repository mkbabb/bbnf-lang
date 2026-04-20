//! Shared bytes-level JSON canonical-form normalizer used by the
//! canonical-form parity harness.
//!
//! bbnf's `serialize_compact` emits the source span verbatim —
//! whitespace between tokens is preserved, numbers carry their
//! original decimal representation. sonic-rs's `to_string` emits
//! whitespace-free output and re-renders numbers through f64 shortest-
//! roundtrip (Ryu). Byte-equality requires the SAME bytes-level
//! transform on BOTH sides; this module provides that single shared
//! pass.
//!
//! The transform is symmetric across two axes:
//!
//!   1. **Whitespace** — ASCII whitespace (`\t`, `\n`, `\r`, ` `) that
//!      occurs OUTSIDE JSON string literals is removed. Whitespace
//!      INSIDE strings is preserved verbatim (including escape
//!      sequences).
//!
//!   2. **Numbers** — every number token is re-parsed as `f64` and
//!      re-rendered via Rust's default `f64` Display. This collapses
//!      all three representations of the same f64 value
//!      (`-65.613616999999977`, `-65.61361699999998`,
//!      `-0.65613617e2`) to a single canonical form. The choice of
//!      Rust's Display (not Ryu, not a custom formatter) is
//!      incidental — both sides feed through the same function, so
//!      the canonicalization is byte-symmetric by construction.
//!
//! Not a bbnf → sonic-rs bridge — neither side sees the other's
//! output. The normalizer is the common denominator: both parsers'
//! outputs fed through the same byte transform, then compared.

/// Canonicalize a JSON string for byte-level parity comparison.
///
/// Applies two symmetric bytes-level transforms:
///
///   1. Strip ASCII whitespace (`\t`, `\n`, `\r`, ` `) OUTSIDE string
///      literals. Whitespace INSIDE strings is preserved verbatim.
///   2. Re-render every number token via `f64` Display, collapsing
///      shortest-roundtrip variations to a single canonical form.
///
/// Implementation is a single pass over the input bytes with a
/// small state machine: out-of-string, in-string (with escape
/// awareness), and in-number. The in-string state tracks backslash
/// escapes so that `"\""` does not prematurely terminate the string.
/// The in-number state consumes a JSON number per RFC 8259 §6 and
/// reformats via `f64`.
pub fn strip_insignificant_ws(src: &str) -> String {
    let bytes = src.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if b == b'"' {
            // String opening quote. Copy the entire string verbatim
            // including escape sequences until the closing quote.
            out.push(b);
            i += 1;
            while i < bytes.len() {
                let c = bytes[i];
                out.push(c);
                i += 1;
                if c == b'\\' {
                    // Copy the next byte verbatim; it's the escape
                    // target (e.g. `\"`, `\\`, `\n`, `\uXXXX`). For
                    // `\uXXXX` the subsequent four hex digits are
                    // handled by the main loop — they're ordinary
                    // string bytes, not a structural escape.
                    if i < bytes.len() {
                        out.push(bytes[i]);
                        i += 1;
                    }
                } else if c == b'"' {
                    break;
                }
            }
        } else if b == b'\t' || b == b'\n' || b == b'\r' || b == b' ' {
            // Insignificant whitespace outside strings — drop.
            i += 1;
        } else if b == b'-' || b.is_ascii_digit() {
            // Number token — consume per RFC 8259 §6 and re-render
            // via f64 to collapse precision variations.
            let start = i;
            if bytes[i] == b'-' {
                i += 1;
            }
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i < bytes.len() && bytes[i] == b'.' {
                i += 1;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
            }
            if i < bytes.len() && (bytes[i] == b'e' || bytes[i] == b'E') {
                i += 1;
                if i < bytes.len() && (bytes[i] == b'+' || bytes[i] == b'-') {
                    i += 1;
                }
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
            }
            let text = std::str::from_utf8(&bytes[start..i])
                .expect("number token ASCII");
            let parsed: f64 = text.parse().expect("valid JSON number");
            // `{}` formatter for f64 emits shortest-roundtrip form
            // (Rust stdlib uses Grisu/Dragon); applied identically
            // to both sides, the output is byte-symmetric. No
            // trailing `.0` for integer-valued floats — `1.0`
            // formats as `1`, matching sonic-rs's `as_f64 → {}`.
            let rendered = format!("{}", parsed);
            out.extend_from_slice(rendered.as_bytes());
        } else {
            out.push(b);
            i += 1;
        }
    }
    // The input is valid UTF-8 (sonic-rs and bbnf both round-trip
    // UTF-8) and the byte edits above preserve UTF-8 boundaries:
    // ASCII whitespace bytes and number tokens (<0x80) are never
    // part of a multibyte sequence.
    String::from_utf8(out).expect("normalized JSON remains valid UTF-8")
}
