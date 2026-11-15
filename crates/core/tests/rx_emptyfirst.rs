//! sk-v27 LW3.RX-EMPTYFIRST (RULINGS 2026-11-14i GSEQ-F (b)): a regex that
//! can match the empty string is never refused by the first-byte pre-check.
//!
//! Root's regex scan adapter (AY.W4.3, `regex_scan_adapter.rs`) consults a
//! per-byte LUT before running a pattern's DFA and returns `None` when the
//! byte at `pos` is not in the pattern's FIRST set. The FIRST set came from
//! `regex_first_chars`, which holds the bytes a non-empty match can begin
//! with, so a nullable pattern such as `/=?/` was refused at every byte
//! outside `{=}`, though it matches the empty string there. google_sheets
//! (`formula = /=?/ , expression ;`) refused `1+2` for that reason.
//!
//! The first test reproduces the defect on a minimal grammar of five
//! adapter patterns (the LUT is emitted at four or more), reading the
//! emitted adapter: the nullable pattern's branch must admit every byte.
//! The second pins google_sheets reading its text as written.
//!
//! LW3.RX-EMPTYFIRST-B (RULINGS 2026-11-14j RXEF-A (a), QX-1 clause 7): the
//! cure above unmasked a second defect. The ArgList emitter
//! (`shapes/arglist.rs`) parsed every position it had no direct call for (a
//! regex head, an inline alternation, a rule with no shape of its own) with
//! the root rule's value dispatcher, so an argument was parsed as a whole
//! entry-rule match. The third test reads every emitted ArgList body of a
//! minimal grammar (not google_sheets) for a call into the root dispatcher.

use bbnf::ParserAttributes;
use bbnf::generate::generate_all;
use bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

const MINIMAL: &str = r#"
value = number | text | name | quoted ;
number = /[0-9]+(\.[0-9]+)?/ ;
text = /"([^"]|"")*"/ ;
name = /[A-Za-z_][A-Za-z0-9_]*/ ;
quoted = /'[^']*'/ ;
cell = /=?/ , value ;
"#;

fn emitted(grammar: &str) -> String {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust {
            requested_prettify: false,
        },
    };
    let prepared = match compile_grammar_request(grammar, &request).expect("grammar compiles") {
        CompileOutput::Rust(prepared) => *prepared,
        _ => panic!("expected Rust output"),
    };
    let attrs = ParserAttributes {
        builder_path: Some("crate::runtime::bbnf::BbnfStructBuilder".to_string()),
        document_path: Some("crate::runtime::bbnf::BbnfDocument".to_string()),
        ..ParserAttributes::default()
    };
    generate_all(&prepared, &attrs, &quote::format_ident!("EmptyFirstParser")).to_string()
}

/// The `[u32; 256]` LUT literal named `ident`, if emitted.
fn lut(source: &str, ident: &str) -> Option<Vec<u32>> {
    let at = source.find(&format!("const {ident} : [u32 ; 256] = ["))?;
    let body = &source[at..];
    let open = body.find("= [")? + 3;
    let close = body[open..].find(']')? + open;
    Some(
        body[open..close]
            .split(',')
            .map(|cell| cell.trim().trim_end_matches("u32").parse().expect("LUT cell is a u32"))
            .collect(),
    )
}

/// The body of the adapter branch for the static whose pattern text is `pattern`.
fn branch<'a>(source: &'a str, pattern: &str) -> &'a str {
    let decl = format!(": & str = {pattern:?} ;");
    let at = source.find(&decl).unwrap_or_else(|| panic!("no static for {pattern:?}"));
    let name_start = source[..at].rfind("static ").expect("static keyword") + "static ".len();
    let name = source[name_start..at].trim();
    let test = format!("pattern == {name} {{");
    let start = source.find(&test).unwrap_or_else(|| panic!("no adapter branch for {name}"));
    let rest = &source[start + test.len()..];
    let end = rest.find("pattern == __DTA_REGEX_").unwrap_or(rest.len());
    &rest[..end]
}

#[test]
fn nullable_regex_is_not_refused_by_first_byte_check() {
    let source = emitted(MINIMAL);
    let lut_ident = "__REGEX_FIRST_BYTE_LUT_EmptyFirstParser";
    let body = branch(&source, "=?");
    let probe = format!("{lut_ident} [__byte as usize] >> ");
    let Some(at) = body.find(&probe) else {
        // No first-byte check in the nullable pattern's branch: nothing refuses it.
        return;
    };
    let shift: u32 = body[at + probe.len()..]
        .split(|c: char| !c.is_ascii_digit())
        .next()
        .and_then(|digits| digits.parse().ok())
        .expect("LUT shift is a literal");
    let table = lut(&source, lut_ident).expect("the branch reads a LUT the module emits");
    let refused: Vec<usize> = (0..256).filter(|&b| (table[b] >> shift) & 1 == 0).collect();
    assert!(
        refused.is_empty(),
        "the nullable pattern `=?` (LUT bit {shift}) is refused before its DFA at {} of 256 bytes, \
         e.g. {:?}; it matches the empty string at every one of them",
        refused.len(),
        refused.iter().take(8).map(|&b| b as u8 as char).collect::<Vec<_>>()
    );
}

/// A grammar whose root (the last rule, as root reads it) has a nullable
/// prefix before its value, and two ArgList rules: one with a regex head that
/// takes its `(` inline, one with a literal head whose argument is an inline
/// alternation. The arguments are not themselves calls: a cycle through the
/// value rule sends root's driver (`backend/driver/alt.rs`
/// `branch_pushes_children`) into unbounded recursion at generation time, a
/// separate defect recorded in the LEDGER, not this test's subject.
const ARGLIST: &str = r#"
number = /[0-9]+/ ;
name = /[a-z]+/ ;
args = number , ("," >> number) * ;
call = /[cC][aA][lL][lL]\(/ , args ?w , ")" ;
wrap = "wrap" , "(" >> (number | name) << ")" ;
value = call | wrap | number | name ;
cell = /=?/ , value ;
"#;

#[test]
fn arglist_argument_is_parsed_by_its_own_rule_not_the_root_dispatcher() {
    let source = emitted(ARGLIST);
    let root_dispatcher = "parse_EmptyFirstParser_cell__value (";
    let marker = "pub fn parse_arglist_EmptyFirstParser_";
    let mut bodies = Vec::new();
    let mut rest = source.as_str();
    while let Some(at) = rest.find(marker) {
        let after = &rest[at + marker.len()..];
        let name: String = after.chars().take_while(|c| c.is_alphanumeric() || *c == '_').collect();
        let end = after.find("pub fn ").unwrap_or(after.len());
        bodies.push((name, &after[..end]));
        rest = &after[end..];
    }
    let names: Vec<&str> = bodies.iter().map(|(n, _)| n.as_str()).collect();
    assert!(
        names.contains(&"call") && names.contains(&"wrap"),
        "the fixture's two ArgList rules are emitted as ArgList shapes, got {names:?}"
    );
    let offending: Vec<&str> = bodies
        .iter()
        .filter(|(_, body)| body.contains(root_dispatcher))
        .map(|(n, _)| n.as_str())
        .collect();
    assert!(
        offending.is_empty(),
        "ArgList bodies {offending:?} parse a position with the root rule's dispatcher \
         (`{root_dispatcher}`), not the rule or terminal the grammar names there"
    );
}

#[test]
fn google_sheets_reads_a_formula_with_or_without_its_optional_leading_eq() {
    // grammar/google-sheets/google-sheets.bbnf: `formula = /=?/ , expression ;`.
    let refused: Vec<&str> = ["1+2", "A1", "42", "=1+2"]
        .into_iter()
        .filter(|src| GoogleSheetsParser::parse(src).is_err())
        .collect();
    assert!(refused.is_empty(), "google_sheets refused {refused:?}");
}

/// RULINGS 2026-11-15a RXEFB-D (a): an ArgList head is recorded (its matched
/// text deposited on the call's compound) only when the call's `(` is its own
/// next position. A head written as a regex or a literal and followed by a
/// separate `"("` is the callee's name; a head whose match takes the `(` itself
/// is the call's opening token, which the rule already names, and deposits
/// nothing.
const HEADS: &str = r#"
number = /[0-9]+/ ;
args = number , ("," >> number) * ;
opened = /[oO][pP][eE][nN]\(/ , args ?w , ")" ;
named = /[a-z]+/ , "(" , args ?w , ")" ;
value = opened | named | number ;
cell = /=?/ , value ;
"#;

#[test]
fn arglist_head_is_recorded_only_when_its_paren_is_its_own_next_position() {
    let source = emitted(HEADS);
    let marker = "pub fn parse_arglist_EmptyFirstParser_";
    let body = |rule: &str| -> String {
        let at = source
            .find(&format!("{marker}{rule} <"))
            .unwrap_or_else(|| panic!("`{rule}` is not emitted as an ArgList shape"));
        let after = &source[at + marker.len()..];
        let end = after.find("pub fn ").unwrap_or(after.len());
        after[..end].to_string()
    };
    let deposit = "__head_text";
    assert!(
        body("named").contains(deposit),
        "`named = /[a-z]+/ , \"(\" , …`: the head is followed by its own `(` position, so its \
         text is the call's name and must be deposited on the call's compound"
    );
    assert!(
        !body("opened").contains(deposit),
        "`opened = /[oO][pP][eE][nN]\\(/ , …`: the head takes the `(` itself; it is the call's \
         opening token and must deposit nothing"
    );
}
