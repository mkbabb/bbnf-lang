// Standalone probe: runs the VERBATIM general_descent leaf logic against the
// real bbnf-corpus leaf patterns/tokens to locate the cascade.
// regex_first::{first_bytes,is_nullable} copied verbatim from
// skinny/crates/codegen/src/general_descent/regex_first.rs
// consume_quoted/is_text_byte/match_leaf-head-gate copied verbatim from emit.rs.

use std::collections::BTreeSet;

// ===== VERBATIM regex_first.rs =====
pub fn is_nullable(pattern: &str) -> bool {
    let trimmed = pattern.trim();
    if trimmed.is_empty() { return true; }
    if let Some(atom) = trimmed.strip_suffix('*').or_else(|| trimmed.strip_suffix('?')) {
        return is_single_atom(atom);
    }
    false
}
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
fn leading_group_spans_all(bytes: &[u8]) -> bool {
    let mut depth = 0i32; let mut escaped = false; let mut in_class = false;
    for (index, &byte) in bytes.iter().enumerate() {
        if escaped { escaped = false; continue; }
        match byte {
            b'\\' => escaped = true,
            b'[' if !in_class => in_class = true,
            b']' if in_class => in_class = false,
            b'(' if !in_class => depth += 1,
            b')' if !in_class => { depth -= 1; if depth == 0 { return index == bytes.len() - 1; } }
            _ => {}
        }
    }
    false
}
pub fn first_bytes(pattern: &str) -> BTreeSet<u8> {
    let mut set = BTreeSet::new();
    for branch in split_top_level_alternation(pattern) { head_bytes(branch.as_bytes(), &mut set); }
    set
}
fn split_top_level_alternation(pattern: &str) -> Vec<String> {
    let bytes = pattern.as_bytes();
    let mut branches = Vec::new(); let mut start = 0usize;
    let mut depth_paren = 0i32; let mut in_class = false; let mut escaped = false;
    for (index, &byte) in bytes.iter().enumerate() {
        if escaped { escaped = false; continue; }
        match byte {
            b'\\' => escaped = true,
            b'[' if !in_class => in_class = true,
            b']' if in_class => in_class = false,
            b'(' if !in_class => depth_paren += 1,
            b')' if !in_class => depth_paren -= 1,
            b'|' if !in_class && depth_paren == 0 => { branches.push(pattern[start..index].to_string()); start = index + 1; }
            _ => {}
        }
    }
    branches.push(pattern[start..].to_string());
    branches
}
fn head_bytes(bytes: &[u8], set: &mut BTreeSet<u8>) {
    match bytes {
        [] => {}
        [b'(', b'?', b':', rest @ ..] => head_bytes(rest, set),
        [b'(', rest @ ..] => head_bytes(rest, set),
        [b'[', ..] => class_bytes(bytes, set),
        [b'\\', escaped, ..] => match escaped {
            b'd' => insert_range(set, b'0', b'9'),
            b'w' => { insert_range(set, b'a', b'z'); insert_range(set, b'A', b'Z'); insert_range(set, b'0', b'9'); set.insert(b'_'); }
            b's' => { for byte in [b' ', b'\t', b'\n', b'\r'] { set.insert(byte); } }
            b't' => { set.insert(b'\t'); }
            b'n' => { set.insert(b'\n'); }
            b'r' => { set.insert(b'\r'); }
            other => { set.insert(*other); }
        },
        [byte, ..] => { if !matches!(byte, b'|'|b'*'|b'+'|b'?'|b'^'|b'$'|b'.') { set.insert(*byte); } }
    }
}
fn class_bytes(bytes: &[u8], set: &mut BTreeSet<u8>) {
    if bytes.get(1).copied() == Some(b'^') { return; }
    let Some(end) = closing_class_index(bytes) else { return; };
    let mut index = 1usize;
    while index < end {
        let Some(lo) = class_byte(bytes, &mut index, end) else { return; };
        if index + 1 < end && bytes[index] == b'-' {
            index += 1;
            let Some(hi) = class_byte(bytes, &mut index, end) else { return; };
            if lo <= hi { insert_range(set, lo, hi); }
        } else { set.insert(lo); }
    }
}
fn closing_class_index(bytes: &[u8]) -> Option<usize> {
    let mut escaped = false;
    for (index, &byte) in bytes.iter().enumerate().skip(1) {
        if escaped { escaped = false; } else if byte == b'\\' { escaped = true; } else if byte == b']' { return Some(index); }
    }
    None
}
fn class_byte(bytes: &[u8], index: &mut usize, end: usize) -> Option<u8> {
    let byte = *bytes.get(*index)?; *index += 1;
    if byte != b'\\' { return Some(byte); }
    if *index >= end { return None; }
    let escaped = *bytes.get(*index)?; *index += 1;
    Some(match escaped { b't' => b'\t', b'n' => b'\n', b'r' => b'\r', other => other })
}
fn insert_range(set: &mut BTreeSet<u8>, lo: u8, hi: u8) { for byte in lo..=hi { set.insert(byte); } }

// ===== VERBATIM emit.rs leaf-consumption =====
fn is_text_byte(byte: u8) -> bool { byte.is_ascii_alphanumeric() || matches!(byte, b'_'|b'.'|b'$') }
fn consume_quoted(bytes: &[u8], start: usize) -> usize {
    let quote = bytes[start]; let mut pos = start + 1;
    while pos < bytes.len() {
        if bytes[pos] == quote {
            if bytes.get(pos + 1).copied() == Some(quote) { pos += 2; continue; }
            return pos + 1;
        }
        pos += 1;
    }
    bytes.len()
}

// match_leaf head-gate + body scan, faithful reproduction. Returns Ok(end) or Err(reason).
fn match_leaf(bytes: &[u8], start: usize, first: &BTreeSet<u8>, nullable: bool) -> Result<usize, String> {
    let first: Vec<u8> = first.iter().copied().collect();
    let Some(&head) = bytes.get(start) else {
        return if nullable { Ok(start) } else { Err("EOI, non-nullable".into()) };
    };
    if !first.is_empty() && !first.contains(&head) {
        return if nullable { Ok(start) } else { Err(format!("head 0x{head:02x} '{}' NOT in first set -> REJECT", head as char)) };
    }
    let pos = if matches!(head, b'\'' | b'"') { consume_quoted(bytes, start) }
        else if is_text_byte(head) {
            let mut p = start + 1;
            while let Some(&b) = bytes.get(p) {
                if is_text_byte(b) { p += 1; }
                else if matches!(b, b'\'' | b'"') { p = consume_quoted(bytes, p); }
                else { break; }
            }
            p
        } else {
            let mut p = start + 1;
            while let Some(&b) = bytes.get(p) { if !first.is_empty() && first.contains(&b) { p += 1; } else { break; } }
            p
        };
    Ok(pos)
}

fn show_first(set: &BTreeSet<u8>) -> String {
    if set.is_empty() { return "{} (EMPTY)".into(); }
    if set.len() > 12 { return format!("{{ {} bytes }}", set.len()); }
    let s: Vec<String> = set.iter().map(|&b| if b.is_ascii_graphic() { format!("'{}'", b as char) } else { format!("0x{b:02x}") }).collect();
    format!("{{ {} }}", s.join(", "))
}

fn main() {
    println!("== bbnf structural leaf patterns through general_descent regex_first::first_bytes ==\n");
    let leaves: &[(&str, &str)] = &[
        ("identifier   /.../", "[_a-zA-Z][_a-zA-Z0-9-]*"),
        ("literal \"    /.../", "(\\.|[^\"\\\\])*"),
        ("literal '    /.../", "(\\.|[^'\\\\])*"),
        ("regex body   /.../", "(\\.|[^\\/])+"),
        ("big_comment  /.../", "[^\\*]*"),
        ("comment      /.../", ".*"),
        ("json number  /.../", "-?(0|[1-9]\\d*)(\\.\\d+)?([eE][+-]?\\d+)?"),
        ("sheets number/.../", "(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?"),
    ];
    for (name, pat) in leaves {
        let fb = first_bytes(pat);
        let null = is_nullable(pat);
        println!("  {name}  pattern=/{pat}/\n      first={}  nullable={}", show_first(&fb), null);
    }

    println!("\n== The cascade: parse REAL corpus leaf spans with match_leaf ==\n");

    // Case A: bbnf `regex` rule body applied to bbnf.bbnf line-9 regex literal.
    // After matching opening '/', cursor sits at the regex body. Oracle end vs general_descent.
    let regex_body_first = first_bytes("(\\.|[^\\/])+");
    let regex_null = is_nullable("(\\.|[^\\/])+");
    let inp = b"[_a-zA-Z][_a-zA-Z0-9-]*"; // the body of /[_a-zA-Z][_a-zA-Z0-9-]*/ from bbnf.bbnf:9
    let r = match_leaf(inp, 0, &regex_body_first, regex_null);
    println!("  A) bbnf `regex` body leaf on identifier-regex body {:?}", std::str::from_utf8(inp).unwrap());
    println!("     match_leaf => {:?}   (oracle would consume all {} bytes)", r, inp.len());

    // Case B: bbnf `literal` (double-quote) body on a real bbnf string body `a\"b`.
    // Oracle: /(\.|[^"\\])*/ consumes `a\"b` (4 bytes). general_descent:
    let lit_first = first_bytes("(\\.|[^\"\\\\])*");
    let lit_null = is_nullable("(\\.|[^\"\\\\])*");
    let inp2 = b"a\\\"b"; // a \ " b  (the body between the quotes of "a\"b")
    let r2 = match_leaf(inp2, 0, &lit_first, lit_null);
    println!("\n  B) bbnf `literal` body leaf on string body {:?}", std::str::from_utf8(inp2).unwrap());
    println!("     first={} nullable={}  match_leaf => {:?}  (oracle consumes all {} bytes)", show_first(&lit_first), lit_null, r2, inp2.len());

    // Case C: consume_quoted directly on the whole bbnf source token "a\"b" (backslash escape).
    let tok = b"\"a\\\"b\""; // the 6 source bytes: " a \ " b "
    let ce = consume_quoted(tok, 0);
    println!("\n  C) consume_quoted on bbnf source literal token {:?} (backslash-escaped quote)", std::str::from_utf8(tok).unwrap());
    println!("     consume_quoted => end {}  (oracle backslash-aware end = {})", ce, tok.len());
    println!("     => {}", if ce == tok.len() { "OK" } else { "MIS-TERMINATED: closes at the backslash-escaped quote, drops the tail" });

    // Case D: identifier continuation drops the hyphen (bbnf ident class has '-', is_text_byte does not).
    let id_first = first_bytes("[_a-zA-Z][_a-zA-Z0-9-]*");
    let idtok = b"foo-bar"; // a hyphenated bbnf identifier
    let rd = match_leaf(idtok, 0, &id_first, false);
    println!("\n  D) identifier leaf on {:?} (bbnf ident class includes '-')", std::str::from_utf8(idtok).unwrap());
    println!("     match_leaf => {:?}  (oracle consumes all {} bytes)  is_text_byte('-')={}", rd, idtok.len(), is_text_byte(b'-'));
}
