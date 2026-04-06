//! Verify the regex grammar's recursive rules survive the IR optimizer.

use bbnf::pipeline::{PipelineOptions, compile_grammar};

/// Regex grammar for self-hosting parse-that's regex parser.
/// Entry point (`regex`) must be the LAST rule — compile_grammar uses
/// last-rule-in-source-order as entry, and prune_unreachable DFS from entry.
const REGEX_GRAMMAR: &str = r#"
literal = /[^\\()\[\]{}|*+?.^$]/ ;
class_escape
    = "\\" >> /[dDwWsS]/
    | "\\" >> "u" >> "{" >> /[0-9a-fA-F]+/ << "}"
    | "\\" >> "u" >> /[0-9a-fA-F]{4}/
    | "\\" >> "x" >> /[0-9a-fA-F]{2}/
    | "\\" >> /[^\n]/
    ;
class_atom = class_escape | /[^\]\\]/ ;
class_item
    = class_atom , "-" >> class_atom
    | class_escape
    | /[^\]\\]/
    ;
char_class = "[" >> "^" ? , class_item + << "]" ;
escape
    = "\\" >> /[dDwWsS]/
    | "\\" >> /[pP]/ >> "{" >> /[A-Za-z_]+/ << "}"
    | "\\" >> "u" >> "{" >> /[0-9a-fA-F]+/ << "}"
    | "\\" >> "u" >> /[0-9a-fA-F]{4}/
    | "\\" >> "x" >> /[0-9a-fA-F]{2}/
    | "\\" >> /[^\n]/
    ;
quantifier
    = "*" , "?" ?
    | "+" , "?" ?
    | "?" , "?" ?
    | "{" >> /\d+/ , ( "," >> /\d*/ ) ? << "}" , "?" ?
    ;
group
    = "(?:" >> alternation << ")"
    | "(?" >> /[simux]+/ , ":" >> alternation << ")"
    | "(?" >> /[simux]+/ << ")"
    | "(" >> alternation << ")"
    ;
atom
    = group
    | char_class
    | "."
    | "^"
    | "$"
    | escape
    | literal
    ;
quantified = atom , quantifier ? ;
concat = quantified + ;
alternation = concat , ( "|" >> concat ) * ;
regex = alternation ;
"#;

#[test]
fn regex_cyclic_rules_survive_optimizer() {
    let ir = compile_grammar(REGEX_GRAMMAR, &PipelineOptions::default()).unwrap();

    let rule_names: Vec<&str> = ir.rules.iter().map(|r| ir.get_string(r.name)).collect();
    let cyclic_count = ir.rules.iter().filter(|r| r.meta.is_cyclic).count();

    // The recursive spine (alternation, concat, quantified, atom, group)
    // must survive as cyclic rules.
    assert!(
        cyclic_count >= 5,
        "expected ≥5 cyclic rules (alternation→concat→quantified→atom→group), got {}: {:?}",
        cyclic_count, rule_names
    );
    assert!(
        ir.rules.len() >= 6,
        "expected ≥6 surviving rules, got {}: {:?}",
        ir.rules.len(), rule_names
    );
}
