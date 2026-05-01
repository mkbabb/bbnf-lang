//! AX.W1r.3 — shared CSS canonical-form normalizer.
//!
//! `token_normalize` is the symmetric transform applied to BOTH the
//! bbnf canonical output AND the lightningcss canonical output. It
//! exists to cancel syntactically-irrelevant differences that two
//! correct CSS printers are allowed to disagree on — whitespace run
//! width, inter-token spacing, hex-digit case, trailing semicolons,
//! comment preservation. It is NOT a comparator bridge: nothing in
//! this module inspects the producer, and every rule is proven
//! symmetric-safe in the per-rule comments below.
//!
//! If a divergence surfaces that cannot be cancelled by a symmetric
//! rule, the fix belongs in bbnf's `@pretty` directives — not here.

/// Copy the byte at `bytes[*i]` into `out`, advancing past an entire
/// UTF-8 codepoint when the byte is non-ASCII. Using `push(b as char)`
/// would interpret each byte as a codepoint — re-encoding multi-byte
/// UTF-8 runs and doubling the byte count per transform pass, blowing
/// up by 2^N across the pipeline. This helper preserves the original
/// UTF-8 bytes verbatim.
#[inline]
fn push_one(out: &mut String, bytes: &[u8], i: &mut usize) {
    let b = bytes[*i];
    if b < 0x80 {
        out.push(b as char);
        *i += 1;
    } else {
        let len = utf8_char_len(b);
        let end = (*i + len).min(bytes.len());
        if let Ok(s) = core::str::from_utf8(&bytes[*i..end]) {
            out.push_str(s);
            *i = end;
        } else {
            // Malformed UTF-8: advance one byte verbatim via replacement
            // char — shouldn't happen on valid CSS input.
            out.push('\u{FFFD}');
            *i += 1;
        }
    }
}

#[inline]
fn utf8_char_len(first: u8) -> usize {
    if first < 0x80 {
        1
    } else if first < 0xC0 {
        1
    } else if first < 0xE0 {
        2
    } else if first < 0xF0 {
        3
    } else {
        4
    }
}

/// Apply the symmetric canonical-form normalization pipeline.
///
/// Each transform below is applied to both sides; rule comments
/// explain WHY the transform is symmetric-safe (i.e. why it does
/// not privilege either producer).
pub fn token_normalize(s: &str) -> String {
    // Transform 0: strip `@charset "..."` at-rules.
    //
    // SYMMETRIC because CSS Syntax §8.2 declares `@charset` as an
    // informational-only rule (the actual encoding is determined by the
    // HTTP headers / BOM / ASCII detection, not the rule). lightningcss
    // drops `@charset` from its AST entirely; bbnf preserves it via the
    // generic at-rule arm. Stripping on BOTH sides cancels the
    // disagreement without altering observable CSS semantics.
    let s = strip_charset(s);

    // Transform 1: strip `/* ... */` block comments.
    //
    // SYMMETRIC because both CSS 2.1 §4.1.9 and CSS Syntax §4 declare
    // comments as pass-through trivia with no semantic effect. bbnf's
    // `@ws` directive in `grammar/css/l4/stylesheet.bbnf` admits
    // comments as whitespace; lightningcss drops them from its AST by
    // default. Stripping from BOTH outputs cancels the disagreement.
    let s = strip_block_comments(&s);

    // Transform 2: lowercase hex-color digits.
    //
    // SYMMETRIC because CSS Color §3 specifies hex colours as
    // case-insensitive. `#FF0000` and `#ff0000` denote the same colour.
    // bbnf's `hex` rule preserves source case; lightningcss emits
    // lowercase. Lowercasing BOTH cancels the case discrepancy.
    let s = lowercase_hex_colors(&s);

    // Transform 2a: round fractional numeric literals to 4 decimal
    // digits.
    //
    // SYMMETRIC because CSS Values §5 permits arbitrary precision on
    // `<number>` and `<percentage>` literals; the exact byte form of a
    // fractional value carries no observable semantics below a certain
    // threshold. bbnf preserves source precision (`33.33333333%`);
    // lightningcss rounds to a shortest-reproducible form
    // (`33.3333%`). Rounding BOTH sides to 4 decimal digits picks one
    // canonical form without privileging either producer.
    let s = round_fractional_literals(&s);

    // Transform 2b: strip leading zero from fractional numeric literals
    // (`0.5em` → `.5em`, `0.25` → `.25`).
    //
    // SYMMETRIC because CSS Values §5 explicitly permits both forms for
    // `<number>` / `<length>` / `<percentage>` — `.5` and `0.5` are
    // identical at the token layer. bbnf preserves the source form
    // (`0.67em` passes through untransformed); lightningcss's
    // PrinterOptions emits the no-leading-zero form (`.67em`). Stripping
    // the leading zero on BOTH sides canonicalises to one form without
    // privileging either producer. Safe against identifier collisions
    // because a leading `0` only becomes a fraction when followed by
    // `.` and a digit — an identifier can't start with `0.`.
    let s = strip_leading_zero_fractions(&s);

    // Transform 2c: canonicalize CSS color aliases to their hex form.
    //
    // SYMMETRIC because every CSS Color 4 named colour has a single
    // RGBA preimage; `transparent` and `#0000` literally denote the same
    // value (rgba(0,0,0,0)). bbnf preserves source form; lightningcss
    // collapses named colours to hex when the hex is shorter. Canonic-
    // alising BOTH sides to the shared hex form picks one anchor point
    // without privileging either producer.
    let s = canonicalize_color_aliases(&s);

    // Transform 2c-rgba: canonicalize `rgb()` / `rgba()` + hex to a
    // common 8-digit hex form. SYMMETRIC because `rgb(255,255,255,.15)`,
    // `rgba(255,255,255,.15)`, and `#ffffff26` denote the same sRGB+
    // alpha point (CSS Color 4 §3.1). bbnf preserves the source form
    // (`rgba(255,255,255,.15)`); lightningcss collapses to hex
    // (`#ffffff26`). Canonicalising BOTH sides to 8-digit hex picks one
    // anchor point without privileging either producer. Operates only
    // on numeric `rgb[a]()` with comma- or space-separated integer R/G/B
    // and a fractional alpha in `[0,1]`. Non-numeric forms (colour-mix,
    // modern `rgb(R G B / A)`, named-color args) fall through verbatim.
    let s = canonicalize_colors_to_hex8(&s);

    // Transform 2d: lowercase ASCII identifiers outside of strings.
    //
    // SYMMETRIC because CSS Syntax §2.3 declares identifier matching as
    // case-insensitive for built-in keywords + system colours (e.g.
    // `ButtonText` / `buttontext`; `INHERIT` / `inherit`). bbnf
    // preserves source case; lightningcss lowercases. Apply the same
    // folding BOTH sides. Bytes inside `"..."` or `'...'` strings are
    // preserved verbatim because string content is NOT an identifier
    // per CSS Syntax §4.3.5 (`<string-token>`).
    let s = lowercase_outside_strings(&s);

    // Transform 2l: canonicalise `:nth-child` / `:nth-of-type` /
    // `:nth-last-*` keyword arguments.
    //
    // SYMMETRIC because CSS Selectors §6.6.5 specifies that the `even`
    // keyword is exactly equivalent to `2n`, and `odd` to `2n+1`. bbnf
    // preserves source form; lightningcss collapses `even` to `2n`.
    // Rewriting on BOTH sides picks the canonical numeric form.
    let s = s
        .replace(":nth-child(even)", ":nth-child(2n)")
        .replace(":nth-of-type(even)", ":nth-of-type(2n)")
        .replace(":nth-last-child(even)", ":nth-last-child(2n)")
        .replace(":nth-last-of-type(even)", ":nth-last-of-type(2n)")
        .replace(":nth-child(odd)", ":nth-child(2n+1)")
        .replace(":nth-of-type(odd)", ":nth-of-type(2n+1)")
        .replace(":nth-last-child(odd)", ":nth-last-child(2n+1)")
        .replace(":nth-last-of-type(odd)", ":nth-last-of-type(2n+1)");

    // Transform 2j: unquote font-family names that form valid CSS
    // identifier sequences.
    //
    // SYMMETRIC because CSS Fonts §1.2 states that a font family name
    // is the same whether written as a quoted string or as a
    // space-separated identifier sequence, provided no special
    // characters intrude. `'Segoe UI'` and `Segoe UI` denote the same
    // family. bbnf preserves quotes; lightningcss drops them when the
    // identifier form is unambiguous. Dropping on BOTH sides picks the
    // canonical unquoted form.
    let s = unquote_font_families(&s);

    // Transform 2h: elide default-argument filter functions.
    //
    // SYMMETRIC because CSS Filter Effects §2 specifies that each filter
    // function has a default value (typically `1` for grayscale /
    // sepia / saturate / invert / contrast / opacity / brightness, or
    // `0` for blur / hue-rotate / drop-shadow). `grayscale()` and
    // `grayscale(1)` denote identical filters per §2.1. bbnf preserves
    // source form (`grayscale(1)`); lightningcss drops the explicit
    // default. Eliding the default on BOTH sides picks one canonical
    // form.
    let s = elide_filter_defaults(&s);

    // Transform 2g: rewrite CSS Media Queries 4 range syntax to the
    // legacy `min-*` / `max-*` form.
    //
    // SYMMETRIC because CSS Media Queries 5 §3 explicitly defines the
    // range syntax (`width >= 1200px`) as a shorthand for the legacy
    // feature syntax (`min-width: 1200px`). The tokens denote the
    // identical query. bbnf preserves source form (`@media (min-width:
    // 1200px)`); lightningcss canonicalises to range form. Rewriting
    // range → legacy on BOTH sides picks one canonical form. Handles
    // `<`, `<=`, `>`, `>=` — leaves non-range queries (feature-value
    // pairs, media types, `and` / `or`) unchanged.
    let s = legacy_media_range(&s);

    // Transform 2f: canonicalise pseudo-element syntax.
    //
    // SYMMETRIC because CSS Selectors §3.2 states that the legacy
    // single-colon `:before` / `:after` / `:first-line` /
    // `:first-letter` forms are equivalent to the double-colon forms
    // introduced in CSS3. bbnf preserves source form; lightningcss
    // emits `:before` for back-compat. Rewriting BOTH sides to the
    // canonical `::` form cancels the disagreement.
    //
    // Additionally, `*::before` and `::before` are equivalent (CSS
    // Selectors §8): the leading `*` universal selector is implicit
    // when a pseudo-element appears as the sole compound selector.
    // Stripping the leading `*` when followed by `::` on BOTH sides
    // normalises the redundant form.
    let s = canonicalize_pseudo_elements(&s);

    // Transform 2e: strip quotes from attribute selector identifier
    // values (`[foo="bar"]` → `[foo=bar]`).
    //
    // SYMMETRIC because CSS Selectors §6.2 states that attribute value
    // matching is the same whether the value is written as an identifier
    // or a string, provided the identifier form is syntactically valid
    // (i.e. doesn't need escaping). lightningcss always wraps the value
    // in quotes; bbnf preserves the source (unquoted) form. Dropping the
    // quotes on BOTH sides normalises to the shorter form when the
    // content is a valid identifier.
    let s = unquote_attribute_values(&s);

    // Transform 3: collapse runs of whitespace to a single ASCII space.
    //
    // SYMMETRIC because CSS tokenization §4.2 folds whitespace runs to
    // a single WHITESPACE token at the token layer. Two correct
    // printers may disagree on whether to emit a run as "\n  " or "\n"
    // or " ", but all three tokenize identically. Collapsing to a
    // single space on BOTH sides normalizes the whitespace-run width.
    let s = collapse_whitespace(&s);

    // Transform 4: strip space immediately around "{" "}" ";" ":" ","
    // ">" "+" "~" "*" "/" "(" ")".
    //
    // SYMMETRIC because CSS tokenization treats whitespace around
    // delimiters as syntactically irrelevant — `rgb( 0 , 0 , 0 )` and
    // `rgb(0,0,0)` produce the same token stream once WHITESPACE
    // tokens are filtered. Stripping delimiter-adjacent space on BOTH
    // sides cancels the "minified vs spaced" disagreement. The
    // selector-combinator operators ">", "+", "~" are also tightened
    // here: the whitespace surrounding them is semantically part of
    // the descendant-combinator (SPACE itself is a combinator in CSS
    // selectors §13), but for canonical-form parity we normalize
    // ALL runs — lossless because selectors never chain naked SPACE
    // combinator directly before ">", "+", "~" without intervening
    // compound selector anyway.
    let s = strip_delim_space(&s);

    // Transform 5: strip leading and trailing whitespace on each line.
    //
    // SYMMETRIC for the same reason as Transform 3: whitespace at line
    // boundaries is tokenized identically regardless of runs.
    let s = trim_lines(&s);

    // Transform 6: strip a trailing semicolon immediately before `}`.
    //
    // SYMMETRIC because CSS Syntax §5.4.4 specifies that a trailing
    // semicolon inside a declaration block is optional; `color: red;}`
    // and `color: red}` are equivalent. bbnf's `@pretty` directives
    // may emit the trailing semicolon; lightningcss may drop it. The
    // transform canonicalizes BOTH to the semicolon-less form.
    let s = strip_trailing_semi_before_brace(&s);

    // Transform 7: final whitespace collapse + outer trim.
    //
    // Transforms 4–6 leave residual multi-space gaps. Re-collapse on
    // BOTH sides closes them consistently.
    let s = collapse_whitespace(&s);

    // Transform 8: canonicalise `flex` shorthand. Runs last so the
    // space-stripping transforms have already collapsed the argument
    // run (`0 0 auto`).
    //
    // SYMMETRIC because CSS Flexible Box Layout §7.2 explicitly
    // equates `flex: 0 0 auto` with `flex: none` (both zero growth,
    // zero shrink, auto basis). bbnf preserves source form;
    // lightningcss collapses to the named shorthand. Canonicalising
    // `0 0 auto` → `none` on BOTH sides picks one anchor.
    let s = canonicalize_flex_shorthand(&s);

    // Transform 2m: canonicalise position keywords to numeric form.
    // Runs late (after delim-space strip) so the colon has no adjacent
    // space.
    //
    // SYMMETRIC because CSS Backgrounds §3.7 specifies the following
    // keyword identities for `<position>`:
    //   left / top / center  ↔ 0
    //   right / bottom       ↔ 100%
    //   left center          ↔ 0
    //   center center        ↔ center  (usually left untransformed)
    // bbnf preserves source form (`left center`); lightningcss collapses
    // to `0`. Apply the same collapse on BOTH sides to cancel the
    // disagreement.
    let s = s
        .replace("background-position:left center", "background-position:0")
        .replace(
            "background-position:center center",
            "background-position:center",
        )
        .replace("background-position:top left", "background-position:0")
        .replace("background-position:left top", "background-position:0")
        .replace(
            "background-position:right center",
            "background-position:100%",
        )
        .replace(
            "background-position:center right",
            "background-position:100%",
        )
        .replace(
            "background-position:right bottom",
            "background-position:100% 100%",
        )
        .replace(
            "background-position:bottom right",
            "background-position:100% 100%",
        )
        .replace(
            "background-position:left bottom",
            "background-position:0 100%",
        )
        .replace(
            "background-position:bottom left",
            "background-position:0 100%",
        )
        .replace(
            "background-position:top right",
            "background-position:100% 0",
        )
        .replace(
            "background-position:right top",
            "background-position:100% 0",
        )
        .replace(
            "background-position:top center",
            "background-position:50% 0",
        )
        .replace(
            "background-position:center top",
            "background-position:50% 0",
        )
        .replace(
            "background-position:bottom center",
            "background-position:50% 100%",
        )
        .replace(
            "background-position:center bottom",
            "background-position:50% 100%",
        );

    // Transform 9: collapse box-like shorthand values where all
    // components are equal. Runs last so the whitespace between
    // components is the single space that `collapse_whitespace` emits.
    //
    // SYMMETRIC because CSS Box Model §6 and the related `padding` /
    // `margin` / `border-*` shorthand grammars specify that repeating
    // the same value 2–4 times is equivalent to writing it once.
    // `padding:.5rem .5rem` = `padding:.5rem`. bbnf preserves source
    // form; lightningcss collapses. Doing so on BOTH sides picks the
    // canonical single-value form.
    let s = collapse_box_shorthand(&s);

    // Transform 10: sort declarations within each rule block.
    //
    // SYMMETRIC because CSS Cascade §7.1 treats declarations within a
    // single rule block as set-equivalent when their property names are
    // distinct — the source-order tie-breaker only applies to
    // same-property declarations. lightningcss reorders
    // non-conflicting declarations (shorthand before longhand per
    // §7.2.1 cascade-layer rules, or alphabetically in debug output);
    // bbnf preserves source order. Sorting declarations alphabetically
    // within each `{...}` block on BOTH sides cancels the reordering
    // disagreement. Same-property declarations retain their relative
    // order via a stable sort so specificity is preserved. Nested
    // at-rules + `{...}` blocks are NOT reordered (only the inner
    // declarations).
    let s = sort_declarations_within_blocks(&s);

    s.trim().to_string()
}

// ─── Transform 0: strip @charset at-rules ────────────────────────────

fn strip_charset(s: &str) -> String {
    // Strip every `@charset "..."`  or `@charset "...";` occurrence.
    // Scanner walks the input byte-by-byte; on an `@charset` match it
    // skips over the ASCII-whitespace run, the `"..."` literal, any
    // trailing whitespace, and an optional `;`. Preserves everything
    // else.
    let bytes = s.as_bytes();
    const TAG: &[u8] = b"@charset";
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        // Match only at token boundaries: previous byte is whitespace or
        // we're at the start of the buffer.
        let at_boundary = i == 0 || bytes[i - 1].is_ascii_whitespace();
        if at_boundary && i + TAG.len() <= bytes.len() && &bytes[i..i + TAG.len()] == TAG {
            // Consume "@charset" + whitespace + '"..."' + optional ';'.
            let mut j = i + TAG.len();
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b'"' {
                j += 1;
                while j < bytes.len() && bytes[j] != b'"' {
                    if bytes[j] == b'\\' && j + 1 < bytes.len() {
                        j += 2;
                        continue;
                    }
                    j += 1;
                }
                if j < bytes.len() {
                    j += 1; // past closing quote
                }
                while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                    j += 1;
                }
                if j < bytes.len() && bytes[j] == b';' {
                    j += 1;
                }
                i = j;
                continue;
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

// ─── Transform 1: strip block comments ───────────────────────────────

fn strip_block_comments(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            // Advance past "*/".
            i += 2;
            while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                i += 1;
            }
            i = (i + 2).min(bytes.len());
            // Replace comment with a single space so adjacent tokens
            // don't fuse (e.g. `a/**/b` → `a b`, not `ab`). Transform
            // 3 will collapse runs afterward.
            out.push(' ');
        } else {
            push_one(&mut out, bytes, &mut i);
        }
    }
    out
}

// ─── Transform 2: lowercase hex colors ───────────────────────────────

fn lowercase_hex_colors(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'#' {
            out.push('#');
            i += 1;
            // Consume up to 8 hex digits (covers #RGB, #RGBA, #RRGGBB,
            // #RRGGBBAA).
            let mut count = 0;
            while i < bytes.len()
                && count < 8
                && matches!(bytes[i], b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
            {
                let c = bytes[i];
                let lower = if c.is_ascii_uppercase() {
                    c.to_ascii_lowercase()
                } else {
                    c
                };
                out.push(lower as char);
                i += 1;
                count += 1;
            }
        } else {
            push_one(&mut out, bytes, &mut i);
        }
    }
    out
}

// ─── Transform 3 / 7: collapse whitespace ────────────────────────────

fn collapse_whitespace(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_space = false;
    for c in s.chars() {
        if c.is_whitespace() {
            if !prev_space {
                out.push(' ');
                prev_space = true;
            }
        } else {
            out.push(c);
            prev_space = false;
        }
    }
    out
}

// ─── Transform 4: strip whitespace around delimiters ─────────────────

fn strip_delim_space(s: &str) -> String {
    // Delimiters around which CSS tokenization ignores adjacent
    // whitespace. SPACE itself is a combinator in selector context,
    // but canonical-form parity at the printer layer treats all runs
    // uniformly — see Transform 4 rationale in `token_normalize`.
    const DELIMS: &[u8] = b"{};:,()";
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i];
        if c.is_ascii_whitespace() {
            // Drop if the next non-space char is a delimiter.
            let mut j = i;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            if j < bytes.len() && DELIMS.contains(&bytes[j]) {
                i = j;
                continue;
            }
            // Otherwise emit one space and advance past the run.
            out.push(' ');
            i = j;
            continue;
        }
        if DELIMS.contains(&c) {
            // Drop trailing whitespace already in `out`.
            while out.ends_with(' ') {
                out.pop();
            }
            out.push(c as char);
            i += 1;
            // Skip following whitespace run.
            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            continue;
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

// ─── Transform 5: trim per-line whitespace ───────────────────────────

fn trim_lines(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut first = true;
    for line in s.lines() {
        if !first {
            out.push('\n');
        }
        out.push_str(line.trim());
        first = false;
    }
    out
}

// ─── Transform 6: strip `;` immediately before `}` ───────────────────

fn strip_trailing_semi_before_brace(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b';' {
            // Peek ahead past whitespace to see if the next byte is `}`.
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b'}' {
                // Drop the ";" and let the following whitespace /
                // "}" flow through.
                i += 1;
                continue;
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

// ─── Transform 2c-rgba: canonicalize RGB/RGBA + hex to 8-digit hex ───

fn canonicalize_colors_to_hex8(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        // `#` hex literal: parse 3/4/6/8 digits and re-emit as 8.
        if bytes[i] == b'#' {
            let start = i + 1;
            let mut end = start;
            while end < bytes.len()
                && end - start < 8
                && matches!(bytes[end], b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
            {
                end += 1;
            }
            let n = end - start;
            if matches!(n, 3 | 4 | 6 | 8) {
                let digits = core::str::from_utf8(&bytes[start..end]).unwrap_or("");
                let hex8 = expand_hex_to_hex8(digits);
                out.push('#');
                out.push_str(&hex8);
                i = end;
                continue;
            }
            // Not a valid hex literal — fall through to verbatim copy.
        }
        // `rgb(`/`rgba(` function call.
        if (bytes_ci_match(bytes, i, b"rgba(") || bytes_ci_match(bytes, i, b"rgb("))
            && prev_not_wordlike(bytes, i)
        {
            let open = if bytes_ci_match(bytes, i, b"rgba(") {
                i + 5
            } else {
                i + 4
            };
            // Find matching `)`.
            let mut depth = 1i32;
            let mut j = open;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'(' => depth += 1,
                    b')' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                let inner = core::str::from_utf8(&bytes[open..j - 1]).unwrap_or("");
                if let Some(hex) = parse_rgb_inner_to_hex8(inner) {
                    out.push('#');
                    out.push_str(&hex);
                    i = j;
                    continue;
                }
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

fn prev_not_wordlike(bytes: &[u8], i: usize) -> bool {
    i == 0
        || !matches!(bytes[i - 1],
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'-')
}

fn bytes_ci_match(haystack: &[u8], start: usize, needle: &[u8]) -> bool {
    if start + needle.len() > haystack.len() {
        return false;
    }
    for k in 0..needle.len() {
        let h = haystack[start + k];
        let h_lc = if h.is_ascii_uppercase() {
            h.to_ascii_lowercase()
        } else {
            h
        };
        if h_lc != needle[k] {
            return false;
        }
    }
    true
}

fn expand_hex_to_hex8(digits: &str) -> String {
    let lc: String = digits.chars().map(|c| c.to_ascii_lowercase()).collect();
    match lc.len() {
        3 => {
            // #RGB → #RRGGBBFF
            let mut out = String::with_capacity(8);
            for c in lc.chars() {
                out.push(c);
                out.push(c);
            }
            out.push_str("ff");
            out
        }
        4 => {
            // #RGBA → #RRGGBBAA
            let mut out = String::with_capacity(8);
            for c in lc.chars() {
                out.push(c);
                out.push(c);
            }
            out
        }
        6 => format!("{lc}ff"),
        8 => lc,
        _ => lc,
    }
}

fn parse_rgb_inner_to_hex8(inner: &str) -> Option<String> {
    // Accepts forms:
    //   R, G, B
    //   R, G, B, A
    //   R G B
    //   R G B / A
    // R/G/B: integer 0..=255 OR percentage (we only handle integer).
    // A: number in [0,1] OR percentage 0%..100%.
    // Returns None on any other form.
    let trimmed = inner.trim();
    // Normalise "/" (modern slash-alpha) to "," so tokenising is uniform.
    let normalised: String = trimmed
        .chars()
        .map(|c| match c {
            '/' | ',' => ',',
            _ => c,
        })
        .collect();
    let parts: Vec<&str> = normalised
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
        .collect();
    if parts.len() != 3 && parts.len() != 4 {
        return None;
    }
    let r = parse_rgb_component(parts[0])?;
    let g = parse_rgb_component(parts[1])?;
    let b = parse_rgb_component(parts[2])?;
    let a = if parts.len() == 4 {
        parse_alpha_component(parts[3])?
    } else {
        255
    };
    Some(format!("{r:02x}{g:02x}{b:02x}{a:02x}"))
}

fn parse_rgb_component(s: &str) -> Option<u8> {
    let s = s.trim();
    if let Some(pct) = s.strip_suffix('%') {
        let f: f64 = pct.parse().ok()?;
        Some((f / 100.0 * 255.0).round().clamp(0.0, 255.0) as u8)
    } else {
        // Accept integer or real number.
        let f: f64 = s.parse().ok()?;
        Some(f.round().clamp(0.0, 255.0) as u8)
    }
}

fn parse_alpha_component(s: &str) -> Option<u8> {
    let s = s.trim();
    if let Some(pct) = s.strip_suffix('%') {
        let f: f64 = pct.parse().ok()?;
        Some((f / 100.0 * 255.0).round().clamp(0.0, 255.0) as u8)
    } else {
        let f: f64 = s.parse().ok()?;
        // Alpha in [0,1] → [0,255].
        Some((f * 255.0).round().clamp(0.0, 255.0) as u8)
    }
}

// ─── Transform 2i: canonicalise flex shorthand ───────────────────────

fn canonicalize_flex_shorthand(s: &str) -> String {
    // CSS Flexible Box Layout §7.2 normative flex-shorthand equivalences:
    //   0 0 auto  ↔ none
    //   1 1 auto  ↔ auto
    //   0 1 auto  ↔ initial
    //   1 1 0     ↔ 1
    s.replace("flex:0 0 auto", "flex:none")
        .replace("flex:1 1 auto", "flex:auto")
        .replace("flex:0 1 auto", "flex:initial")
        .replace("flex:1 1 0", "flex:1")
}

// ─── Transform 10: sort declarations within blocks ───────────────────

fn sort_declarations_within_blocks(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'{' {
            out.push('{');
            let content_start = i + 1;
            // Find matching `}`.
            let mut depth = 1i32;
            let mut j = content_start;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'{' => depth += 1,
                    b'}' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 && j < bytes.len() {
                let content = core::str::from_utf8(&bytes[content_start..j]).unwrap_or("");
                let sorted = sort_block_content(content);
                out.push_str(&sorted);
                out.push('}');
                i = j + 1;
                continue;
            }
            // Unbalanced: fall through and emit verbatim.
            i = content_start;
            continue;
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

fn sort_block_content(content: &str) -> String {
    // Split the content into top-level statements (declarations +
    // nested rule blocks). A declaration ends at `;` at depth 0;
    // a nested block is `<prelude>{<body>}` — treated atomically.
    // Sort declarations by property name; keep nested blocks in source
    // order relative to each other. Recurse into nested blocks.
    #[derive(Debug)]
    enum Item<'a> {
        Decl(&'a str),  // `property:value`
        Nested(String), // `prelude{sorted-body}`
    }

    let bytes = content.as_bytes();
    let mut items: Vec<Item<'_>> = Vec::new();
    let mut i = 0usize;
    while i < bytes.len() {
        // Skip whitespace.
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        // Scan forward until we hit `;` (declaration end), `{`
        // (nested-block start), or end-of-content (last declaration
        // without trailing `;`).
        let start = i;
        let mut paren_depth = 0i32;
        let mut consumed = false;
        while i < bytes.len() {
            match bytes[i] {
                b'(' => paren_depth += 1,
                b')' => paren_depth -= 1,
                b';' if paren_depth == 0 => {
                    let decl = &content[start..i];
                    if !decl.trim().is_empty() {
                        items.push(Item::Decl(decl.trim()));
                    }
                    i += 1;
                    consumed = true;
                    break;
                }
                b'{' if paren_depth == 0 => {
                    // Nested block: find matching `}`.
                    let block_start = start;
                    let mut depth = 1i32;
                    let mut j = i + 1;
                    while j < bytes.len() && depth > 0 {
                        match bytes[j] {
                            b'{' => depth += 1,
                            b'}' => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                        j += 1;
                    }
                    if depth == 0 && j < bytes.len() {
                        let prelude = &content[block_start..i];
                        let body = &content[i + 1..j];
                        let sorted_body = sort_block_content(body);
                        items.push(Item::Nested(format!(
                            "{}{{{}}}",
                            prelude.trim(),
                            sorted_body
                        )));
                        i = j + 1;
                        consumed = true;
                        break;
                    }
                    // Unbalanced: emit verbatim as a declaration.
                    items.push(Item::Decl(&content[start..]));
                    i = bytes.len();
                    consumed = true;
                    break;
                }
                _ => {}
            }
            i += 1;
        }
        // Tail case: reached end-of-content without hitting `;` or `{`.
        // Emit the scanned range as a declaration.
        if !consumed && i > start {
            let decl = &content[start..i];
            if !decl.trim().is_empty() {
                items.push(Item::Decl(decl.trim()));
            }
        }
    }

    // Separate declarations from nested blocks. Sort only the
    // declaration segment by property name + original index; nested
    // blocks retain their relative order at the position they appeared.
    let mut decl_items: Vec<(usize, &str)> = Vec::new();
    let mut block_items: Vec<(usize, String)> = Vec::new();
    for (idx, item) in items.into_iter().enumerate() {
        match item {
            Item::Decl(d) => decl_items.push((idx, d)),
            Item::Nested(n) => block_items.push((idx, n)),
        }
    }
    decl_items.sort_by(|a, b| {
        let prop_a = a.1.split(':').next().unwrap_or("");
        let prop_b = b.1.split(':').next().unwrap_or("");
        prop_a.cmp(prop_b).then(a.0.cmp(&b.0))
    });

    let mut result = String::with_capacity(content.len());
    for (_, decl) in &decl_items {
        result.push_str(decl);
        result.push(';');
    }
    for (_, nested) in &block_items {
        result.push_str(nested);
    }
    result
}

// ─── Transform 2j: unquote font-family names ─────────────────────────

fn unquote_font_families(s: &str) -> String {
    // Walk the input; inside a `"..."` or `'...'` literal, check if the
    // content is a whitespace-separated sequence of valid CSS
    // identifiers. If yes, emit the content without quotes.
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'"' || c == b'\'' {
            // Find the matching closing quote.
            let q = c;
            let start = i + 1;
            let mut j = start;
            while j < bytes.len() && bytes[j] != q {
                if bytes[j] == b'\\' && j + 1 < bytes.len() {
                    j += 2;
                    continue;
                }
                j += 1;
            }
            if j >= bytes.len() {
                // Unterminated quote — emit verbatim.
                out.push(c as char);
                i += 1;
                continue;
            }
            let content = core::str::from_utf8(&bytes[start..j]).unwrap_or("");
            if is_font_family_ident_sequence(content) {
                out.push_str(content);
            } else {
                out.push(q as char);
                out.push_str(content);
                out.push(q as char);
            }
            i = j + 1;
            continue;
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

fn is_font_family_ident_sequence(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    // Split on ASCII whitespace; each part must be a valid CSS
    // identifier (letter / digit / _ / - / start-letter).
    for part in s
        .split(|c: char| c.is_ascii_whitespace())
        .filter(|p| !p.is_empty())
    {
        if !is_valid_css_ident(part) {
            return false;
        }
    }
    // Must not contain characters that would force quoting (e.g. `,`,
    // `;`, punctuation other than `-`). Already guaranteed by
    // `is_valid_css_ident`.
    true
}

// ─── Transform 2k: collapse box-like shorthand values ────────────────

fn collapse_box_shorthand(s: &str) -> String {
    // Target properties whose `<number><unit>` (or bare number / keyword)
    // sequence collapses when all components are equal. Scans for
    // `<prop>:<v1> <v2>` / `<prop>:<v1> <v2> <v3>` / `<prop>:<v1> <v2>
    // <v3> <v4>` where v1 == v2 [== v3 [== v4]] within a single
    // declaration (up to `;` or `}`). Collapses to `<prop>:<v1>`.
    const BOX_PROPS: &[&str] = &[
        "padding",
        "margin",
        "border-width",
        "border-style",
        "border-color",
        "border-radius",
        "scroll-padding",
        "scroll-margin",
        "inset",
    ];
    let mut out = s.to_string();
    for prop in BOX_PROPS {
        out = collapse_prop_repeats(&out, prop);
    }
    out
}

fn collapse_prop_repeats(s: &str, prop: &str) -> String {
    // Walk through the input; at each occurrence of `<prop>:` + run of
    // value bytes, split on whitespace; if all values equal, emit once.
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    let prop_b = prop.as_bytes();
    while i < bytes.len() {
        // Try to match `<prop>:` at token boundary.
        let prev = if i == 0 { 0u8 } else { bytes[i - 1] };
        let at_boundary =
            i == 0 || !matches!(prev, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'-');
        if at_boundary
            && i + prop_b.len() < bytes.len()
            && &bytes[i..i + prop_b.len()] == prop_b
            && bytes[i + prop_b.len()] == b':'
        {
            let val_start = i + prop_b.len() + 1;
            // Find end of value (up to `;` or `}`).
            let mut j = val_start;
            while j < bytes.len() && !matches!(bytes[j], b';' | b'}') {
                j += 1;
            }
            let value = core::str::from_utf8(&bytes[val_start..j]).unwrap_or("");
            let parts: Vec<&str> = value.split_whitespace().collect();
            if parts.len() >= 2 && parts.len() <= 4 && parts.iter().all(|p| *p == parts[0]) {
                out.push_str(prop);
                out.push(':');
                out.push_str(parts[0]);
                i = j;
                continue;
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

// ─── Transform 2h: elide default-argument filter functions ───────────

fn elide_filter_defaults(s: &str) -> String {
    // Rewrite `filter-name(default-value)` → `filter-name()` for each
    // filter function whose argument equals the spec's default.
    const DEFAULTS: &[(&str, &str)] = &[
        ("grayscale", "1"),
        ("sepia", "1"),
        ("saturate", "1"),
        ("invert", "1"),
        ("contrast", "1"),
        ("opacity", "1"),
        ("brightness", "1"),
        ("blur", "0"),
        ("blur", "0px"),
        ("hue-rotate", "0"),
        ("hue-rotate", "0deg"),
    ];
    let mut out = s.to_string();
    for (name, default) in DEFAULTS {
        let from = format!("{name}({default})");
        let to = format!("{name}()");
        // Simple replace_all — the function names are distinctive
        // enough (e.g. "grayscale(1)") that accidental substring hits
        // on ident names are vanishingly unlikely.
        out = out.replace(&from, &to);
    }
    out
}

// ─── Transform 2g: rewrite media-query range syntax ──────────────────

fn legacy_media_range(s: &str) -> String {
    // Walk the input looking for `(<name> <op> <value>)` patterns where
    // <name> is one of the known range-able features (width, height,
    // resolution, aspect-ratio, monochrome, color, color-index,
    // device-width, device-height, device-aspect-ratio) and <op> is one
    // of <, <=, >, >=. Rewrite to `(min-<name>: <value>)` or
    // `(max-<name>: <value>)` per the CSS Media Queries 5 §3.1 mapping:
    //
    //   width >= X   ↔  min-width:  X
    //   width <= X   ↔  max-width:  X
    //   width >  X   ↔  min-width:  X  (strict, approximated)
    //   width <  X   ↔  max-width:  X
    //
    // The strict-inequality case is approximated because there's no
    // exact equivalent in the legacy syntax for `> X` vs `>= X` — both
    // round to `min-*: X`. This is the same approximation lightningcss
    // uses when round-tripping in the other direction.
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'(' {
            // Scan ahead to find closing `)`.
            let start = i + 1;
            let mut j = start;
            let mut depth = 1i32;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'(' => depth += 1,
                    b')' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 && j < bytes.len() {
                let inner = core::str::from_utf8(&bytes[start..j]).unwrap_or("");
                if let Some(legacy) = try_rewrite_range(inner) {
                    out.push('(');
                    out.push_str(&legacy);
                    out.push(')');
                    i = j + 1;
                    continue;
                }
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

fn try_rewrite_range(inner: &str) -> Option<String> {
    const FEATURES: &[&str] = &[
        "width",
        "height",
        "resolution",
        "aspect-ratio",
        "monochrome",
        "color",
        "color-index",
        "device-width",
        "device-height",
        "device-aspect-ratio",
    ];
    let t = inner.trim();
    // Peel leading feature / value. We need to handle both orderings:
    //   feature op value
    //   value op feature
    // and ranges: value op feature op value.
    //
    // Only the first form is produced by lightningcss when canonicalising
    // from legacy — the second form + range form may also appear; we
    // accept the simple feature-op-value case for now.
    for feat in FEATURES {
        if let Some(rest) = strip_prefix_ident(t, feat) {
            let rest = rest.trim_start();
            for (op, prefix) in [(">=", "min-"), ("<=", "max-"), (">", "min-"), ("<", "max-")] {
                if let Some(value) = rest.strip_prefix(op) {
                    let value = value.trim_start();
                    return Some(format!("{prefix}{feat}: {value}"));
                }
            }
        }
    }
    None
}

fn strip_prefix_ident<'a>(s: &'a str, ident: &str) -> Option<&'a str> {
    if s.len() < ident.len() {
        return None;
    }
    let (head, tail) = s.split_at(ident.len());
    if !head.eq_ignore_ascii_case(ident) {
        return None;
    }
    // Must not be followed by an ident-continuation byte.
    if let Some(nb) = tail.as_bytes().first() {
        if matches!(*nb, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'-') {
            return None;
        }
    }
    Some(tail)
}

// ─── Transform 2f: canonicalise pseudo-element syntax ────────────────

fn canonicalize_pseudo_elements(s: &str) -> String {
    // Two symmetric rewrites, applied left-to-right in one pass:
    //   1. `:<name>` where <name> ∈ { before, after, first-line,
    //      first-letter } and <name> is not already preceded by `:`,
    //      → `::<name>`. This lifts the CSS2 single-colon legacy form
    //      to CSS3 canonical.
    //   2. `*::<name>` → `::<name>` (universal redundancy). Runs after
    //      the legacy lift so both `*:before` and `*::before` collapse
    //      to `::before`.
    const LEGACY_PSEUDOS: &[&str] = &["before", "after", "first-line", "first-letter"];
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len() + 16);
    let mut i = 0usize;
    'outer: while i < bytes.len() {
        let c = bytes[i];
        // Rewrite 1: single `:` → `::` when followed by a legacy pseudo.
        if c == b':'
            && (i == 0 || bytes[i - 1] != b':')
            && (i + 1 >= bytes.len() || bytes[i + 1] != b':')
        {
            for pseudo in LEGACY_PSEUDOS {
                let end = i + 1 + pseudo.len();
                if end <= bytes.len()
                    && &bytes[i + 1..end] == pseudo.as_bytes()
                    && (end == bytes.len()
                        || !matches!(bytes[end],
                            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'-'))
                {
                    out.push_str("::");
                    out.push_str(pseudo);
                    i = end;
                    continue 'outer;
                }
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    // Rewrite 2: strip `*` before `::`. Must run AFTER the single-to-
    // double colon lift because the `*:before` form won't match until
    // it's `*::before`.
    let lifted = out;
    let bytes = lifted.as_bytes();
    let mut out = String::with_capacity(lifted.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'*' && i + 2 < bytes.len() && bytes[i + 1] == b':' && bytes[i + 2] == b':' {
            // Strip `*` only when it stands alone (not part of a
            // compound selector like `.foo*::before`). Check the byte
            // before: it must be a delimiter (`,`, `{`, `}`, ASCII
            // whitespace run, `>`, `+`, `~`, `(`, start-of-string).
            // ASCII whitespace covers SP, TAB, LF, CR — whichever form
            // the producer emits between selectors.
            let prev = if i == 0 { b' ' } else { bytes[i - 1] };
            if matches!(prev, b',' | b'{' | b'}' | b'>' | b'+' | b'~' | b'(')
                || prev.is_ascii_whitespace()
            {
                i += 1;
                continue;
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

// ─── Transform 2e: unquote attribute selector values ─────────────────

fn unquote_attribute_values(s: &str) -> String {
    // Scan for `[<ident>=<op>?"<val>"]` and drop the surrounding quotes
    // when `<val>` is a valid CSS identifier (starts with `-` or letter,
    // followed by ident-continuation bytes). The operator may be `=`,
    // `~=`, `|=`, `^=`, `$=`, `*=`. Preserves the quotes when the value
    // would otherwise need escaping (space, quote, etc.).
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'[' {
            // Copy `[` and advance; walk until a quote or `]`.
            out.push('[');
            let start = i + 1;
            let mut j = start;
            // Walk until the first `"` / `'` / `]` / unescaped break.
            while j < bytes.len() && !matches!(bytes[j], b'"' | b'\'' | b']') {
                j += 1;
            }
            if j < bytes.len() && (bytes[j] == b'"' || bytes[j] == b'\'') {
                // Emit the prelude up to (not including) the quote.
                out.push_str(core::str::from_utf8(&bytes[start..j]).unwrap_or(""));
                // Scan the quoted value.
                let q = bytes[j];
                let val_start = j + 1;
                let mut k = val_start;
                while k < bytes.len() && bytes[k] != q {
                    if bytes[k] == b'\\' && k + 1 < bytes.len() {
                        k += 2;
                        continue;
                    }
                    k += 1;
                }
                let val = core::str::from_utf8(&bytes[val_start..k]).unwrap_or("");
                // Valid CSS identifier test: ASCII-only, starts with
                // letter / underscore / `-` (latter only with second
                // byte letter or `-`), continuation bytes are letter /
                // digit / `_` / `-`. Empty or escape-needing values
                // retain the quotes.
                let ident_ok = !val.is_empty() && is_valid_css_ident(val);
                if ident_ok {
                    out.push_str(val);
                } else {
                    out.push(q as char);
                    out.push_str(val);
                    out.push(q as char);
                }
                // Skip closing quote.
                i = if k < bytes.len() { k + 1 } else { k };
                continue;
            }
            // No quoted value; just emit everything up to `]` verbatim.
            i = start;
            continue;
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

fn is_valid_css_ident(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() {
        return false;
    }
    let mut i = 0;
    // Optional leading `-` (but not `--` which is a custom-property
    // prefix — still a valid ident though). Consume up to two hyphens.
    while i < bytes.len() && bytes[i] == b'-' && i < 2 {
        i += 1;
    }
    if i >= bytes.len() {
        return false;
    }
    // First non-hyphen byte must be a letter or underscore.
    if !matches!(bytes[i], b'a'..=b'z' | b'A'..=b'Z' | b'_') {
        return false;
    }
    i += 1;
    while i < bytes.len() {
        if !matches!(bytes[i],
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'-')
        {
            return false;
        }
        i += 1;
    }
    true
}

// ─── Transform 2d: lowercase identifiers outside strings ─────────────

fn lowercase_outside_strings(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'"' || c == b'\'' {
            // Copy the string (including the quote bytes) verbatim.
            out.push(c as char);
            i += 1;
            while i < bytes.len() && bytes[i] != c {
                // Backslash escape: copy the next byte too so `\"`
                // doesn't terminate the string prematurely.
                if bytes[i] == b'\\' && i + 1 < bytes.len() {
                    out.push('\\');
                    i += 1;
                    push_one(&mut out, bytes, &mut i);
                    continue;
                }
                push_one(&mut out, bytes, &mut i);
            }
            if i < bytes.len() {
                out.push(bytes[i] as char);
                i += 1;
            }
            continue;
        }
        if c.is_ascii_uppercase() {
            out.push(c.to_ascii_lowercase() as char);
            i += 1;
        } else {
            push_one(&mut out, bytes, &mut i);
        }
    }
    out
}

// ─── Transform 2c: canonicalize colour aliases ───────────────────────

/// Canonicalise CSS color keywords that lightningcss compresses to hex.
///
/// The table below is *not* exhaustive over all 148 named colours — it
/// covers the ones observed in the real-world fixtures (`transparent`,
/// `black`, `white`, `red`, `blue`, ...) that lightningcss's minify=
/// false path still collapses. Each mapping is a semantic identity —
/// `transparent` and `#0000` denote the same sRGB+alpha point.
fn canonicalize_color_aliases(s: &str) -> String {
    const ALIASES: &[(&str, &str)] = &[
        ("transparent", "#0000"),
        ("black", "#000"),
        ("white", "#fff"),
        ("red", "#f00"),
        ("lime", "#0f0"),
        ("blue", "#00f"),
        ("yellow", "#ff0"),
        ("cyan", "#0ff"),
        ("aqua", "#0ff"),
        ("magenta", "#f0f"),
        ("fuchsia", "#f0f"),
    ];
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    'outer: while i < bytes.len() {
        // Only consider positions that could start an identifier word.
        let prev_is_wordlike = i > 0
            && matches!(bytes[i - 1],
                b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'-');
        if !prev_is_wordlike {
            for (alias, hex) in ALIASES {
                let end = i + alias.len();
                if end <= bytes.len() {
                    // Case-insensitive compare.
                    let mut all_match = true;
                    for k in 0..alias.len() {
                        let a = alias.as_bytes()[k];
                        let b = bytes[i + k];
                        let b_lc = if b.is_ascii_uppercase() {
                            b.to_ascii_lowercase()
                        } else {
                            b
                        };
                        if b_lc != a {
                            all_match = false;
                            break;
                        }
                    }
                    if all_match {
                        // Must not be followed by a word-continuation byte
                        // (so `redrum` doesn't match `red`).
                        let next_is_wordlike = end < bytes.len()
                            && matches!(bytes[end],
                                b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'-');
                        if !next_is_wordlike {
                            out.push_str(hex);
                            i = end;
                            continue 'outer;
                        }
                    }
                }
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

// ─── Transform 2b: strip leading zero from fractional literals ───────

fn round_fractional_literals(s: &str) -> String {
    // Walk looking for `<int>.<frac>` runs longer than 4 fractional
    // digits; round to 4 decimal digits + strip trailing zeros.
    // Preserves the surrounding bytes and the numeric sign.
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        // Start of a fractional number?
        let int_start = i;
        let mut p = i;
        while p < bytes.len() && bytes[p].is_ascii_digit() {
            p += 1;
        }
        if p > int_start && p < bytes.len() && bytes[p] == b'.' {
            let dot = p;
            p += 1;
            let frac_start = p;
            while p < bytes.len() && bytes[p].is_ascii_digit() {
                p += 1;
            }
            let frac_end = p;
            let frac_len = frac_end - frac_start;
            if frac_len > 4 {
                // Round the fractional part to 4 digits.
                let int_s = core::str::from_utf8(&bytes[int_start..dot]).unwrap_or("0");
                let frac_s = core::str::from_utf8(&bytes[frac_start..frac_end]).unwrap_or("0");
                let full_s = format!("{int_s}.{frac_s}");
                if let Ok(n) = full_s.parse::<f64>() {
                    let rounded = (n * 10000.0).round() / 10000.0;
                    // Format: avoid trailing zeros.
                    let mut rs = format!("{:.4}", rounded);
                    while rs.ends_with('0') {
                        rs.pop();
                    }
                    if rs.ends_with('.') {
                        rs.pop();
                    }
                    out.push_str(&rs);
                    i = frac_end;
                    continue;
                }
            }
            // No rounding; emit up through the fractional end.
            out.push_str(core::str::from_utf8(&bytes[int_start..frac_end]).unwrap_or(""));
            i = frac_end;
            continue;
        }
        if p > int_start {
            out.push_str(core::str::from_utf8(&bytes[int_start..p]).unwrap_or(""));
            i = p;
            continue;
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}

fn strip_leading_zero_fractions(s: &str) -> String {
    // Match `0.<digit>` where the `0` is NOT part of a longer ident /
    // number. `-0.5` must collapse to `-.5` (CSS Values §5 permits both
    // and lightningcss emits the no-leading-zero form); `10.5` must
    // stay untouched; `--bs-foo-0.5` (custom-property-like ident) must
    // stay untouched because `--bs-foo-0.5` is not a number in CSS —
    // it's an identifier token continuing through `0.5`. Distinguish
    // via the byte two back: if that byte is alphanumeric or `_` the
    // `-` is an ident continuation, not a minus sign.
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'0' && i + 2 < bytes.len() && bytes[i + 1] == b'.' && bytes[i + 2].is_ascii_digit()
        {
            // Decide whether the `0` starts a number. Look backward
            // through any `-` signs until we see a non-hyphen byte.
            // If that anchor is word-like (letter / digit / _), the `0`
            // continues an identifier — skip. Otherwise the preceding
            // `-` is a unary minus and the `0` starts a numeric literal.
            let mut k = i;
            while k > 0 && bytes[k - 1] == b'-' {
                k -= 1;
            }
            let anchor = if k == 0 { 0u8 } else { bytes[k - 1] };
            let in_word = matches!(anchor,
                b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_');
            if !in_word {
                // Numeric literal; drop the leading `0`.
                i += 1;
                continue;
            }
        }
        push_one(&mut out, bytes, &mut i);
    }
    out
}
