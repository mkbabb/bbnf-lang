//! Leading-byte-set derivation for `RegexProgram` leaf patterns.
//!
//! The runtime has no general regex matcher and the `bbnf-regex` HIR is a coarse
//! classifier, so the general descent emitter derives each leaf's first-byte set
//! HERE — a small, pattern-general leading-token parser. It handles the leaf
//! shapes the Sheets grammar uses (`[A-Za-z_]\w*`, `\$?[A-Za-z]{1,3}\$?\d+`,
//! `'(?:[^']|'')*'!|...`, `[lL][eE][tT]\(`, `[tT][rR]...`, the JSON `"..."` and
//! numeric forms) and degrades to an empty set (no dispatch contribution) on
//! anything it cannot read. Generality lives in the algorithm (leading char
//! class / escape / literal / alternation head), not in a per-grammar table.

use std::collections::BTreeSet;

/// Whether `pattern` can match the empty string. A whole-pattern trailing `*` or
/// `?` over a single atom is nullable (`=?`, `\w*`, `[A-Za-z]*`); a leading group
/// whose entire body is optional likewise. Conservative: anything not clearly
/// nullable is treated as non-nullable.
pub(crate) fn is_nullable(pattern: &str) -> bool {
    let trimmed = pattern.trim();
    if trimmed.is_empty() {
        return true;
    }
    // A whole-pattern optional/star quantifier over the single leading atom.
    if let Some(atom) = trimmed
        .strip_suffix('*')
        .or_else(|| trimmed.strip_suffix('?'))
    {
        return is_single_atom(atom);
    }
    false
}

/// Whether `atom` is a single regex atom (one char, one escape, one `[...]`
/// class, or one balanced `(...)` group spanning the WHOLE string) — i.e. the
/// trailing quantifier binds to the whole pattern, making it nullable. A
/// `(a)(b)` string is NOT a single atom (the leading `(` closes before the end),
/// so a pattern like `(...)(...)?` is correctly non-nullable.
fn is_single_atom(atom: &str) -> bool {
    let bytes = atom.as_bytes();
    match bytes {
        [] => false,
        [b'\\', _] => true,
        [b'[', ..] => closing_class_index(bytes) == Some(bytes.len() - 1),
        [b'(', ..] => leading_group_spans_all(bytes),
        [_] => true,
        _ => false,
    }
}

/// Whether the leading `(` of `bytes` closes exactly at the final byte — i.e. the
/// whole string is ONE parenthesised group, not `(a)(b)`.
fn leading_group_spans_all(bytes: &[u8]) -> bool {
    let mut depth = 0i32;
    let mut escaped = false;
    let mut in_class = false;
    for (index, &byte) in bytes.iter().enumerate() {
        if escaped {
            escaped = false;
            continue;
        }
        match byte {
            b'\\' => escaped = true,
            b'[' if !in_class => in_class = true,
            b']' if in_class => in_class = false,
            b'(' if !in_class => depth += 1,
            b')' if !in_class => {
                depth -= 1;
                if depth == 0 {
                    return index == bytes.len() - 1;
                }
            }
            _ => {}
        }
    }
    false
}

/// The leading byte set of a regex `pattern`: the set of bytes that may begin a
/// match. Walks the pattern's leading token, descending into a top-level
/// alternation so each branch's head contributes.
pub(crate) fn first_bytes(pattern: &str) -> BTreeSet<u8> {
    let mut set = BTreeSet::new();
    for branch in split_top_level_alternation(pattern) {
        head_bytes(branch.as_bytes(), &mut set);
    }
    set
}

/// Split a pattern on top-level `|` (not nested in `(...)` or `[...]`).
fn split_top_level_alternation(pattern: &str) -> Vec<String> {
    let bytes = pattern.as_bytes();
    let mut branches = Vec::new();
    let mut start = 0usize;
    let mut depth_paren = 0i32;
    let mut in_class = false;
    let mut escaped = false;
    for (index, &byte) in bytes.iter().enumerate() {
        if escaped {
            escaped = false;
            continue;
        }
        match byte {
            b'\\' => escaped = true,
            b'[' if !in_class => in_class = true,
            b']' if in_class => in_class = false,
            b'(' if !in_class => depth_paren += 1,
            b')' if !in_class => depth_paren -= 1,
            b'|' if !in_class && depth_paren == 0 => {
                branches.push(pattern[start..index].to_string());
                start = index + 1;
            }
            _ => {}
        }
    }
    branches.push(pattern[start..].to_string());
    branches
}

/// Insert the first-byte set of a single (alternation-free) pattern head.
fn head_bytes(bytes: &[u8], set: &mut BTreeSet<u8>) {
    match bytes {
        [] => {}
        // A leading anchor / non-capturing group open: descend.
        [b'(', b'?', b':', rest @ ..] => head_bytes(rest, set),
        [b'(', rest @ ..] => head_bytes(rest, set),
        // A leading character class `[...]`.
        [b'[', ..] => class_bytes(bytes, set),
        // A leading escape.
        [b'\\', escaped, ..] => match escaped {
            b'd' => insert_range(set, b'0', b'9'),
            b'w' => {
                insert_range(set, b'a', b'z');
                insert_range(set, b'A', b'Z');
                insert_range(set, b'0', b'9');
                set.insert(b'_');
            }
            b's' => {
                for byte in [b' ', b'\t', b'\n', b'\r'] {
                    set.insert(byte);
                }
            }
            b't' => {
                set.insert(b'\t');
            }
            b'n' => {
                set.insert(b'\n');
            }
            b'r' => {
                set.insert(b'\r');
            }
            other => {
                set.insert(*other);
            }
        },
        // A leading literal byte (not a quantifier/anchor metachar).
        [byte, ..] => {
            if !matches!(byte, b'|' | b'*' | b'+' | b'?' | b'^' | b'$' | b'.') {
                set.insert(*byte);
            }
        }
    }
}

/// Insert the byte set of a leading `[...]` character class.
fn class_bytes(bytes: &[u8], set: &mut BTreeSet<u8>) {
    // Negated classes contribute an unknown (broad) head; leave empty so the
    // dispatch falls through to the ordered-try path.
    if bytes.get(1).copied() == Some(b'^') {
        return;
    }
    let Some(end) = closing_class_index(bytes) else {
        return;
    };
    let mut index = 1usize;
    while index < end {
        let Some(lo) = class_byte(bytes, &mut index, end) else {
            return;
        };
        if index + 1 < end && bytes[index] == b'-' {
            index += 1;
            let Some(hi) = class_byte(bytes, &mut index, end) else {
                return;
            };
            if lo <= hi {
                insert_range(set, lo, hi);
            }
        } else {
            set.insert(lo);
        }
    }
}

fn closing_class_index(bytes: &[u8]) -> Option<usize> {
    let mut escaped = false;
    for (index, &byte) in bytes.iter().enumerate().skip(1) {
        if escaped {
            escaped = false;
        } else if byte == b'\\' {
            escaped = true;
        } else if byte == b']' {
            return Some(index);
        }
    }
    None
}

fn class_byte(bytes: &[u8], index: &mut usize, end: usize) -> Option<u8> {
    let byte = *bytes.get(*index)?;
    *index += 1;
    if byte != b'\\' {
        return Some(byte);
    }
    if *index >= end {
        return None;
    }
    let escaped = *bytes.get(*index)?;
    *index += 1;
    Some(match escaped {
        b't' => b'\t',
        b'n' => b'\n',
        b'r' => b'\r',
        other => other,
    })
}

fn insert_range(set: &mut BTreeSet<u8>, lo: u8, hi: u8) {
    for byte in lo..=hi {
        set.insert(byte);
    }
}
