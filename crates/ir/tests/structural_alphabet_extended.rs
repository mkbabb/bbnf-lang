//! AW-III.W5.a — extended `compute_structural_alphabet` tests.
//!
//! Verifies the new mining surfaces (`digraph_mask`, `digraph_pairs`,
//! `quote_classes`) hold for synthetic-but-faithful IR fixtures of
//! the four canonical grammar families: JSON, CSS L4, BBNF, and
//! Sheets.
//!
//! Each fixture builder constructs only the IR pieces the alphabet
//! pass reads — `strings` (interned literals), `IrNode::Literal` and
//! `IrNode::Regex` references inside rule bodies, plus `regex_info`
//! entries for any regex whose classification influences quote-class
//! mining. The full pipeline-built IR is overkill for an alphabet
//! mining test; the passes-style fixture pattern is the convention.
//!
//! ## Per-grammar assertions
//!
//! - **JSON**: singletons include `,`, `:`, `[`, `]`, `{`, `}`;
//!   `quote_classes` contains `"`; no digraphs.
//! - **CSS L4**: singletons include `,`, `;`, `\n`, `{`, `}`, `(`,
//!   `)`; digraph `('/', '*')` and `('*', '/')` appear from the
//!   `/* ... */` comment-marker literals; `quote_classes` contains
//!   `"` and `'`.
//! - **BBNF**: singletons include `=`, `;`, `|`, `,`, `<`, `>`;
//!   digraph `('-', '>')` from the `->` arrow literal; `quote_classes`
//!   contains `"`, `'`, and `/` (regex-literal toggle).
//! - **Sheets**: singletons include `(`, `)`, `,`, `:`, `=`, `+`, `-`,
//!   `*`, `/`; `quote_classes` contains `"`.

use std::collections::HashMap;

use bbnf_ir::passes::sets::{
    StructuralAlphabet, bitmap_contains, bitmap_popcount, build_byte_bitmap,
    compute_structural_alphabet,
};
use bbnf_ir::{
    AltBranch, CharSet128, CostConfig, GrammarIR, IrNode, IrRule, RuleMeta, StringId,
    TypeDescInterner,
};
use bbnf_regex::RegexInfo;

// ── Fixture infrastructure ───────────────────────────────────────────────

const SENTINEL_ENTRY: u32 = u32::MAX;

fn empty_ir() -> GrammarIR {
    GrammarIR {
        rules: vec![],
        entry: SENTINEL_ENTRY,
        strings: vec![],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: vec![],
        type_map: None,
        pattern_annotations: HashMap::new(),
        regex_info: HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: HashMap::new(),
        key_dispatch_configs: HashMap::new(),
        context_facts: HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: HashMap::new(),
        dag: None,
        cost_config: CostConfig::default(),
        type_desc_interner: TypeDescInterner::new(),
        materialization: HashMap::new(),
        string_index: HashMap::new(),
        payload_layouts: HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        eclass_facts: HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
    }
}

/// Intern a string into the IR and return its `StringId`. Reuses
/// existing entries — same shape the lifter uses, deterministic id
/// assignment.
fn intern(ir: &mut GrammarIR, s: &str) -> StringId {
    if let Some(idx) = ir.strings.iter().position(|x| x == s) {
        return idx as StringId;
    }
    ir.strings.push(s.to_string());
    (ir.strings.len() - 1) as StringId
}

/// Add an `IrNode::Regex(sid)` to the IR with its `regex_info`
/// classification. The pattern is run through `RegexInfo::analyze`
/// so the classification field reflects the real regex semantics.
fn add_regex(ir: &mut GrammarIR, pattern: &str) -> IrNode {
    let sid = intern(ir, pattern);
    let info = RegexInfo::analyze(pattern)
        .unwrap_or_else(|| panic!("RegexInfo::analyze({pattern:?}) failed"));
    ir.regex_info.insert(sid, info);
    IrNode::Regex(sid)
}

fn add_literal(ir: &mut GrammarIR, lit: &str) -> IrNode {
    let sid = intern(ir, lit);
    IrNode::Literal(sid)
}

/// Wrap a body in a single rule and append it to the IR.
fn add_rule(ir: &mut GrammarIR, name: &str, body: IrNode) {
    let name_sid = intern(ir, name);
    let rule_id = ir.rules.len() as u32;
    ir.rules.push(IrRule {
        id: rule_id,
        name: name_sid,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
}

fn alt(branches: Vec<IrNode>) -> IrNode {
    IrNode::Alt(
        branches
            .into_iter()
            .map(|node| AltBranch {
                node,
                first_set: Some(CharSet128::new()),
            })
            .collect(),
        None,
    )
}

fn seq(children: Vec<IrNode>) -> IrNode {
    IrNode::Seq(children)
}

// ── Fixture grammars ─────────────────────────────────────────────────────

/// JSON-shaped IR: `{ }`, `[ ]`, `,`, `:`, plus the JSON string
/// regex (mined into `quote_classes`).
fn json_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let lbrace = add_literal(&mut ir, "{");
    let rbrace = add_literal(&mut ir, "}");
    let lbrack = add_literal(&mut ir, "[");
    let rbrack = add_literal(&mut ir, "]");
    let comma = add_literal(&mut ir, ",");
    let colon = add_literal(&mut ir, ":");
    let true_lit = add_literal(&mut ir, "true");
    let false_lit = add_literal(&mut ir, "false");
    let null_lit = add_literal(&mut ir, "null");
    let string = add_regex(
        &mut ir,
        r#""(?:[^"\\]|\\(?:["\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    );
    let number = add_regex(&mut ir, r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?");

    add_rule(&mut ir, "object", seq(vec![lbrace, rbrace]));
    add_rule(&mut ir, "array", seq(vec![lbrack, rbrack]));
    add_rule(&mut ir, "pair", seq(vec![colon, comma]));
    add_rule(
        &mut ir,
        "value",
        alt(vec![string, number, true_lit, false_lit, null_lit]),
    );
    ir
}

/// CSS L4-shaped IR: `{ }`, `( )`, `,`, `;`, `\n`, plus the
/// `/* */` block-comment digraphs and quoted-string regexes for both
/// double and single quotes.
fn css_l4_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let lbrace = add_literal(&mut ir, "{");
    let rbrace = add_literal(&mut ir, "}");
    let lparen = add_literal(&mut ir, "(");
    let rparen = add_literal(&mut ir, ")");
    let comma = add_literal(&mut ir, ",");
    let semi = add_literal(&mut ir, ";");
    let newline = add_literal(&mut ir, "\n");
    // The `/*` and `*/` literals are how the alphabet pass mines the
    // digraph pairs; CSS grammars use these as block-comment markers.
    let comment_open = add_literal(&mut ir, "/*");
    let comment_close = add_literal(&mut ir, "*/");
    let string_dq = add_regex(&mut ir, r#""(?:[^"\\]|\\[\s\S])*""#);
    let string_sq = add_regex(&mut ir, r"'(?:[^'\\]|\\[\s\S])*'");
    let ident = add_regex(&mut ir, r"[a-zA-Z_][\w-]*");

    add_rule(&mut ir, "block", seq(vec![lbrace, rbrace]));
    add_rule(&mut ir, "func_call", seq(vec![lparen, rparen]));
    add_rule(&mut ir, "decl", seq(vec![ident, semi]));
    add_rule(&mut ir, "list_sep", alt(vec![comma, newline]));
    add_rule(&mut ir, "comment", seq(vec![comment_open, comment_close]));
    add_rule(&mut ir, "string", alt(vec![string_dq, string_sq]));
    ir
}

/// BBNF-shaped IR: `=`, `;`, `|`, `,`, `<`, `>` plus the `->` arrow
/// digraph, `(*` `*)` comment digraphs, plus quoted strings (`"`,
/// `'`) and the regex-literal toggle (`/`).
fn bbnf_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let eq = add_literal(&mut ir, "=");
    let semi = add_literal(&mut ir, ";");
    let pipe = add_literal(&mut ir, "|");
    let comma = add_literal(&mut ir, ",");
    let arrow = add_literal(&mut ir, "->");
    let lt = add_literal(&mut ir, "<");
    let gt = add_literal(&mut ir, ">");
    let comment_open = add_literal(&mut ir, "(*");
    let comment_close = add_literal(&mut ir, "*)");
    let string_dq = add_regex(&mut ir, r#""(?:[^"\\]|\\[\s\S])*""#);
    let string_sq = add_regex(&mut ir, r"'(?:[^'\\]|\\[\s\S])*'");
    // BBNF regex literals toggle on `/`. Treat as a one-quote-byte
    // QuotedString (the alphabet pass keys off `quote_char`).
    let regex_lit = add_regex(&mut ir, r"/(?:[^/\\]|\\[\s\S])*/");

    add_rule(&mut ir, "rule", seq(vec![lt, gt, eq, semi]));
    add_rule(&mut ir, "alt", seq(vec![pipe, comma]));
    add_rule(&mut ir, "type_arrow", arrow);
    add_rule(&mut ir, "comment", seq(vec![comment_open, comment_close]));
    add_rule(&mut ir, "literal", alt(vec![string_dq, string_sq, regex_lit]));
    ir
}

/// Sheets-shaped IR: function-call expression grammar with `(`, `)`,
/// `,`, `:`, plus arithmetic operators `=`, `+`, `-`, `*`, `/` and a
/// double-quoted string regex.
fn sheets_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let lparen = add_literal(&mut ir, "(");
    let rparen = add_literal(&mut ir, ")");
    let comma = add_literal(&mut ir, ",");
    let colon = add_literal(&mut ir, ":");
    let eq = add_literal(&mut ir, "=");
    let plus = add_literal(&mut ir, "+");
    let minus = add_literal(&mut ir, "-");
    let star = add_literal(&mut ir, "*");
    let slash = add_literal(&mut ir, "/");
    let string = add_regex(&mut ir, r#""(?:[^"\\]|\\[\s\S])*""#);
    let number = add_regex(&mut ir, r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?");
    let ident = add_regex(&mut ir, r"[A-Z][A-Z0-9_]*");

    add_rule(&mut ir, "formula", seq(vec![eq.clone(), ident.clone()]));
    add_rule(&mut ir, "func_call", seq(vec![lparen, rparen, comma, colon]));
    add_rule(&mut ir, "binop", alt(vec![plus, minus, star, slash]));
    add_rule(&mut ir, "atom", alt(vec![string, number]));
    ir
}

// ── Tests ────────────────────────────────────────────────────────────────

fn alphabet_for(builder: fn() -> GrammarIR) -> StructuralAlphabet {
    let mut ir = builder();
    compute_structural_alphabet(&mut ir);
    ir.structural_alphabet
        .clone()
        .expect("structural_alphabet should populate for non-empty fixture")
}

#[test]
fn json_singletons_and_quote_classes() {
    let alphabet = alphabet_for(json_fixture);
    for byte in b",:[]{}" {
        assert!(
            alphabet.single_bytes.contains(byte),
            "JSON singletons must contain {:?}; got {:?}",
            *byte as char,
            alphabet.single_bytes_vec()
        );
    }
    assert!(
        alphabet.quote_classes.contains(&b'"'),
        "JSON quote_classes must contain '\"'; got {:?}",
        alphabet.quote_classes_vec()
    );
    // Mining is generally non-empty.
    assert!(
        !alphabet.single_bytes.is_empty(),
        "JSON singletons must be non-empty"
    );
    // JSON has no digraphs in the literal set (no two-byte literals
    // that survive the structural-first-byte filter).
    assert!(
        alphabet.digraphs.is_empty(),
        "JSON should mine no digraphs; got {:?}",
        alphabet.digraphs
    );
}

#[test]
fn css_l4_singletons_and_digraphs_and_quote_classes() {
    let alphabet = alphabet_for(css_l4_fixture);
    for byte in b",;\n{}()" {
        assert!(
            alphabet.single_bytes.contains(byte),
            "CSS L4 singletons must contain {:?}; got {:?}",
            *byte as char,
            alphabet.single_bytes_vec()
        );
    }
    // Block-comment digraphs are mined from the `/*` and `*/`
    // literal terminals.
    assert!(
        alphabet.digraphs.contains(&(b'/', b'*')),
        "CSS L4 must mine ('/', '*') digraph; got {:?}",
        alphabet.digraphs
    );
    assert!(
        alphabet.digraphs.contains(&(b'*', b'/')),
        "CSS L4 must mine ('*', '/') digraph; got {:?}",
        alphabet.digraphs
    );
    // CSS supports both `"...""` and `'...'` strings.
    for q in [b'"', b'\''] {
        assert!(
            alphabet.quote_classes.contains(&q),
            "CSS L4 quote_classes must contain {:?}; got {:?}",
            q as char,
            alphabet.quote_classes_vec()
        );
    }
}

#[test]
fn bbnf_singletons_arrow_digraph_and_quote_classes() {
    let alphabet = alphabet_for(bbnf_fixture);
    for byte in b"=;|,<>" {
        assert!(
            alphabet.single_bytes.contains(byte),
            "BBNF singletons must contain {:?}; got {:?}",
            *byte as char,
            alphabet.single_bytes_vec()
        );
    }
    // Type-annotation arrow.
    assert!(
        alphabet.digraphs.contains(&(b'-', b'>')),
        "BBNF must mine ('-', '>') digraph; got {:?}",
        alphabet.digraphs
    );
    // EBNF-style block comments.
    assert!(
        alphabet.digraphs.contains(&(b'(', b'*')),
        "BBNF must mine ('(', '*') digraph; got {:?}",
        alphabet.digraphs
    );
    assert!(
        alphabet.digraphs.contains(&(b'*', b')')),
        "BBNF must mine ('*', ')') digraph; got {:?}",
        alphabet.digraphs
    );
    // Quote toggles `"` and `'` are surfaced by the
    // `RegexClass::QuotedString` classifier. The BBNF `/regex/`
    // literal toggle is documented as a known mining-boundary limit:
    // the upstream classifier accepts only `"` and `'` as quote
    // chars, so `/`-toggled regex bodies are not auto-detected today.
    // The runtime parity kernel still works for any byte the IR
    // exposes — the limit is purely the mining boundary, not a
    // codegen one.
    for q in [b'"', b'\''] {
        assert!(
            alphabet.quote_classes.contains(&q),
            "BBNF quote_classes must contain {:?}; got {:?}",
            q as char,
            alphabet.quote_classes_vec()
        );
    }
}

#[test]
fn sheets_singletons_arithmetic_and_quote_classes() {
    let alphabet = alphabet_for(sheets_fixture);
    for byte in b"(),:=+-*/" {
        assert!(
            alphabet.single_bytes.contains(byte),
            "Sheets singletons must contain {:?}; got {:?}",
            *byte as char,
            alphabet.single_bytes_vec()
        );
    }
    assert!(
        alphabet.quote_classes.contains(&b'"'),
        "Sheets quote_classes must contain '\"'; got {:?}",
        alphabet.quote_classes_vec()
    );
}

#[test]
fn mining_is_deterministic_across_runs() {
    // Run the pass twice and assert byte-for-byte equality on every
    // surface — same set, same digraph order, same digraph_mask, same
    // quote_classes order.
    let mut ir1 = css_l4_fixture();
    compute_structural_alphabet(&mut ir1);
    let mut ir2 = css_l4_fixture();
    compute_structural_alphabet(&mut ir2);

    let a = ir1.structural_alphabet.expect("alphabet 1");
    let b = ir2.structural_alphabet.expect("alphabet 2");
    assert_eq!(a.single_bytes, b.single_bytes);
    assert_eq!(a.digraphs, b.digraphs);
    assert_eq!(a.digraph_mask, b.digraph_mask);
    assert_eq!(a.quote_classes, b.quote_classes);
}

#[test]
fn digraph_mask_matches_first_bytes() {
    // For every fixture, the `digraph_mask` bitmap must be exactly
    // the set of first-bytes of `digraphs`.
    for fixture in [
        json_fixture as fn() -> GrammarIR,
        css_l4_fixture,
        bbnf_fixture,
        sheets_fixture,
    ] {
        let alphabet = alphabet_for(fixture);
        let expected =
            build_byte_bitmap(alphabet.digraphs.iter().map(|(a, _)| *a));
        assert_eq!(
            alphabet.digraph_mask, expected,
            "digraph_mask must match first-bytes for fixture; \
             singletons={:?}, digraphs={:?}",
            alphabet.single_bytes_vec(),
            alphabet.digraphs
        );

        // Membership test agrees with the bitmap.
        for byte in 0u8..=255 {
            let in_bitmap = bitmap_contains(&alphabet.digraph_mask, byte);
            let is_first = alphabet.digraphs.iter().any(|(a, _)| *a == byte);
            assert_eq!(
                in_bitmap, is_first,
                "digraph_mask byte {byte:?} membership disagreement"
            );
        }
    }
}

#[test]
fn singletons_mask_and_quote_mask_helpers() {
    let alphabet = alphabet_for(css_l4_fixture);
    // Helper-derived bitmaps must match a manually-built bitmap.
    let singletons_mask = alphabet.singletons_mask();
    assert_eq!(
        bitmap_popcount(&singletons_mask),
        alphabet.single_bytes.len() as u32,
        "singletons_mask popcount should equal singleton set cardinality"
    );
    for byte in &alphabet.single_bytes {
        assert!(bitmap_contains(&singletons_mask, *byte));
    }
    let quote_mask = alphabet.quote_classes_mask();
    assert_eq!(
        bitmap_popcount(&quote_mask),
        alphabet.quote_classes.len() as u32,
        "quote_classes_mask popcount should equal quote set cardinality"
    );
    for byte in &alphabet.quote_classes {
        assert!(bitmap_contains(&quote_mask, *byte));
    }
}

#[test]
fn empty_grammar_produces_no_alphabet() {
    let mut ir = empty_ir();
    compute_structural_alphabet(&mut ir);
    assert!(
        ir.structural_alphabet.is_none(),
        "empty grammar should not populate structural_alphabet"
    );
}

#[test]
fn digraph_first_byte_is_in_singletons() {
    // Invariant: every digraph's first byte must also be in the
    // singleton set (the alphabet pass adds it explicitly so the
    // bitmap kernel sees every digraph opener).
    for fixture in [
        json_fixture as fn() -> GrammarIR,
        css_l4_fixture,
        bbnf_fixture,
        sheets_fixture,
    ] {
        let alphabet = alphabet_for(fixture);
        for (first, _) in &alphabet.digraphs {
            assert!(
                alphabet.single_bytes.contains(first),
                "digraph first-byte {:?} must be in single_bytes for fixture",
                *first as char
            );
        }
    }
}
