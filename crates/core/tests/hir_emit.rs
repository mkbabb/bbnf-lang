use bbnf::generate::regex::emit::hir::try_emit_regex_inline;

/// Helper: check that a pattern produces Some (i.e., can be inlined).
fn assert_inlinable(pattern: &str) {
    let result = try_emit_regex_inline(pattern);
    assert!(
        result.is_some(),
        "Expected pattern to be inlinable: {pattern}"
    );
}

/// Helper: check that a pattern produces None (i.e., needs DFA tier).
fn assert_not_inlinable(pattern: &str) {
    let result = try_emit_regex_inline(pattern);
    assert!(
        result.is_none(),
        "Expected pattern to need DFA tier: {pattern}"
    );
}

// ── Literals ────────────────────────────────────────────────────────

#[test]
fn literal_simple() {
    assert_inlinable("from");
}

#[test]
fn literal_single_char() {
    assert_inlinable(":");
}

// ── Character classes ───────────────────────────────────────────────

#[test]
fn char_class_simple_range() {
    assert_inlinable("[a-z]");
}

#[test]
fn char_class_multi_range() {
    assert_inlinable("[a-zA-Z0-9_]");
}

#[test]
fn char_class_digit() {
    assert_inlinable(r"\d");
}

#[test]
fn char_class_word() {
    assert_inlinable(r"\w");
}

#[test]
fn char_class_whitespace() {
    assert_inlinable(r"\s");
}

#[test]
fn char_class_small_set() {
    assert_inlinable("[iIsS]");
}

// ── Quantifiers ─────────────────────────────────────────────────────

#[test]
fn quantifier_plus() {
    assert_inlinable(r"\d+");
}

#[test]
fn quantifier_star() {
    assert_inlinable(r"\s*");
}

#[test]
fn quantifier_optional() {
    assert_inlinable(r"\d?");
}

#[test]
fn quantifier_bounded() {
    assert_inlinable(r"[0-9a-fA-F]{4}");
}

// ── Alternation ─────────────────────────────────────────────────────

#[test]
fn alternation_literals() {
    assert_inlinable("from|to");
}

#[test]
fn alternation_mixed() {
    assert_inlinable(r"from|to|\d+%");
}

// ── Concat ──────────────────────────────────────────────────────────

#[test]
fn concat_literal_class() {
    assert_inlinable(r"0x[0-9a-fA-F]+");
}

// ── CSS patterns ────────────────────────────────────────────────────

#[test]
fn css_combinator_separators() {
    assert_inlinable(r"\s*>\s*|\s*\+\s*|\s*~\s*|\s+");
}

#[test]
fn css_anb_full() {
    assert_inlinable(r"[-+]?\d*n\s*[+-]\s*\d+");
}

#[test]
fn css_anb_short() {
    assert_inlinable(r"[-+]?\d*n");
}

#[test]
fn css_signed_integer() {
    assert_inlinable(r"[-+]?\d+");
}

#[test]
fn css_ident_with_escapes() {
    assert_inlinable(r"(?:-?[a-zA-Z_]|\\[^\n])(?:[\w-]|\\[^\n])*");
}

#[test]
fn css_hash_selector() {
    assert_inlinable(r"#(?:[\w-]|\\[^\n])+");
}

// ── Negated classes ─────────────────────────────────────────────────

#[test]
fn negated_class_plus() {
    assert_inlinable(r"[^\n]+");
}

#[test]
fn negated_class_star() {
    assert_inlinable(r"[^\n]*");
}

// ── Fallback cases ──────────────────────────────────────────────────

#[test]
fn unicode_property_not_inlinable() {
    assert_not_inlinable(r"\p{L}+");
}
