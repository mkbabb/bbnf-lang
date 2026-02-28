//! Conservative first-character extraction from regex patterns.

use super::charset::CharSet;

/// Conservatively extract the set of possible first characters from a regex pattern.
pub fn regex_first_chars(pattern: &str) -> Option<CharSet> {
    let bytes = pattern.as_bytes();
    if bytes.is_empty() {
        return Some(CharSet::new());
    }

    let mut pos = 0;

    // Skip leading anchor.
    if pos < bytes.len() && bytes[pos] == b'^' {
        pos += 1;
    }

    regex_first_chars_at(bytes, &mut pos)
}

/// Parse first chars starting at `pos`, handling alternation at the top level.
fn regex_first_chars_at(bytes: &[u8], pos: &mut usize) -> Option<CharSet> {
    let mut result = CharSet::new();

    loop {
        let alt = regex_first_chars_single(bytes, pos)?;
        result.union(&alt);

        let mut depth = 0u32;
        while *pos < bytes.len() {
            match bytes[*pos] {
                b'(' => { depth += 1; *pos += 1; }
                b')' if depth > 0 => { depth -= 1; *pos += 1; }
                b')' => break,
                b'|' if depth == 0 => break,
                b'\\' => { *pos += 2.min(bytes.len() - *pos); }
                b'[' => {
                    *pos += 1;
                    while *pos < bytes.len() && bytes[*pos] != b']' {
                        if bytes[*pos] == b'\\' { *pos += 1; }
                        *pos += 1;
                    }
                    if *pos < bytes.len() { *pos += 1; }
                }
                _ => { *pos += 1; }
            }
        }

        if *pos < bytes.len() && bytes[*pos] == b'|' {
            *pos += 1;
        } else {
            break;
        }
    }

    Some(result)
}

fn is_nullable_quantifier(bytes: &[u8], pos: usize) -> bool {
    if pos >= bytes.len() {
        return false;
    }
    matches!(bytes[pos], b'?' | b'*')
}

fn regex_first_chars_single(bytes: &[u8], pos: &mut usize) -> Option<CharSet> {
    if *pos >= bytes.len() {
        return Some(CharSet::new());
    }

    match bytes[*pos] {
        b'[' => {
            let cs = regex_parse_char_class(bytes, pos)?;
            let nullable = is_nullable_quantifier(bytes, *pos);
            skip_quantifier(bytes, pos);
            if nullable {
                let mut combined = cs;
                let next = regex_first_chars_single(bytes, pos)?;
                combined.union(&next);
                Some(combined)
            } else {
                Some(cs)
            }
        }

        b'\\' => {
            *pos += 1;
            if *pos >= bytes.len() {
                return None;
            }
            let cs = regex_escape_chars(bytes[*pos])?;
            *pos += 1;
            let nullable = is_nullable_quantifier(bytes, *pos);
            skip_quantifier(bytes, pos);
            if nullable {
                let mut combined = cs;
                let next = regex_first_chars_single(bytes, pos)?;
                combined.union(&next);
                Some(combined)
            } else {
                Some(cs)
            }
        }

        b'(' => {
            *pos += 1;
            if *pos < bytes.len() && bytes[*pos] == b'?' {
                if *pos + 1 < bytes.len() && bytes[*pos + 1] == b':' {
                    *pos += 2;
                } else {
                    return None;
                }
            }
            let inner = regex_first_chars_at(bytes, pos)?;
            if *pos < bytes.len() && bytes[*pos] == b')' {
                *pos += 1;
            }
            let nullable = is_nullable_quantifier(bytes, *pos);
            skip_quantifier(bytes, pos);
            if nullable {
                let mut combined = inner;
                let next = regex_first_chars_single(bytes, pos)?;
                combined.union(&next);
                Some(combined)
            } else {
                Some(inner)
            }
        }

        b'.' => {
            None
        }

        b'|' | b')' => {
            Some(CharSet::new())
        }

        ch => {
            let mut cs = CharSet::new();
            cs.add(ch);
            *pos += 1;
            let nullable = is_nullable_quantifier(bytes, *pos);
            skip_quantifier(bytes, pos);
            if nullable {
                let next = regex_first_chars_single(bytes, pos)?;
                cs.union(&next);
            }
            Some(cs)
        }
    }
}

fn regex_parse_char_class(bytes: &[u8], pos: &mut usize) -> Option<CharSet> {
    *pos += 1; // consume '['
    let mut cs = CharSet::new();

    if *pos < bytes.len() && bytes[*pos] == b'^' {
        return None;
    }

    while *pos < bytes.len() && bytes[*pos] != b']' {
        if bytes[*pos] == b'\\' {
            *pos += 1;
            if *pos >= bytes.len() {
                return None;
            }
            if let Some(esc) = regex_escape_chars(bytes[*pos]) {
                cs.union(&esc);
            } else {
                return None;
            }
            *pos += 1;
        } else if *pos + 2 < bytes.len() && bytes[*pos + 1] == b'-' && bytes[*pos + 2] != b']' {
            let from = bytes[*pos];
            let to = bytes[*pos + 2];
            if from > to {
                return None;
            }
            cs.add_range(from, to);
            *pos += 3;
        } else {
            cs.add(bytes[*pos]);
            *pos += 1;
        }
    }

    if *pos < bytes.len() && bytes[*pos] == b']' {
        *pos += 1;
    }

    Some(cs)
}

fn regex_escape_chars(ch: u8) -> Option<CharSet> {
    let mut cs = CharSet::new();
    match ch {
        b'd' => {
            cs.add_range(b'0', b'9');
        }
        b'D' => {
            return None;
        }
        b'w' => {
            cs.add_range(b'a', b'z');
            cs.add_range(b'A', b'Z');
            cs.add_range(b'0', b'9');
            cs.add(b'_');
        }
        b'W' => {
            return None;
        }
        b's' => {
            cs.add(b' ');
            cs.add(b'\t');
            cs.add(b'\n');
            cs.add(b'\r');
            cs.add(0x0C);
        }
        b'S' => {
            return None;
        }
        b'b' | b'B' => {
            return Some(CharSet::new());
        }
        _ => {
            cs.add(ch);
        }
    }
    Some(cs)
}

fn skip_quantifier(bytes: &[u8], pos: &mut usize) {
    if *pos >= bytes.len() {
        return;
    }
    match bytes[*pos] {
        b'*' | b'+' | b'?' => {
            *pos += 1;
            if *pos < bytes.len() && bytes[*pos] == b'?' {
                *pos += 1;
            }
        }
        b'{' => {
            while *pos < bytes.len() && bytes[*pos] != b'}' {
                *pos += 1;
            }
            if *pos < bytes.len() {
                *pos += 1;
            }
            if *pos < bytes.len() && bytes[*pos] == b'?' {
                *pos += 1;
            }
        }
        _ => {}
    }
}
