//! Tests for `@recover` directive parsing and codegen integration.

use bbnf::grammar::BBNFGrammar;
use bbnf::types::Expression;

#[test]
fn parse_recover_directive() {
    let input = r#"
@recover stmt /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;
program = stmt * ;
"#;
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(input);
    let pg = result.expect("should parse grammar with @recover");

    assert_eq!(pg.recovers.len(), 1);
    assert_eq!(pg.recovers[0].rule_name.as_ref(), "stmt");
    assert!(matches!(pg.recovers[0].sync_expr, Expression::Regex(_)));
    assert_eq!(pg.rules.len(), 2);
}

#[test]
fn parse_multiple_recover_directives() {
    let input = r#"
@recover decl /[^;]*;/ ;
@recover rule /[^}]*}/ ;

decl = /[a-z]+/ , ":" , /[^;]+/ , ";" ;
rule = /[a-z]+/ , "{" , decl * , "}" ;
"#;
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(input);
    let pg = result.expect("should parse grammar with multiple @recover");

    assert_eq!(pg.recovers.len(), 2);
    assert_eq!(pg.recovers[0].rule_name.as_ref(), "decl");
    assert_eq!(pg.recovers[1].rule_name.as_ref(), "rule");
}

#[test]
fn parse_recover_mixed_with_imports() {
    let input = r#"
@import "some-file.bbnf" ;

@recover stmt /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;
program = stmt * ;
"#;
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(input);
    let pg = result.expect("should parse grammar with @import and @recover");

    assert_eq!(pg.imports.len(), 1);
    assert_eq!(pg.recovers.len(), 1);
    assert_eq!(pg.rules.len(), 2);
}

#[test]
fn parse_recover_nonexistent_target() {
    let input = r#"
@recover nonexistent /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;
"#;
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(input);
    let pg = result.expect("should parse even with nonexistent target");

    assert_eq!(pg.recovers.len(), 1);
    assert_eq!(pg.recovers[0].rule_name.as_ref(), "nonexistent");
}

#[test]
fn parse_recover_with_alternation_sync_expr() {
    let input = r#"
@recover atRule /[^;{}]*[;]/ | /[^}]*}/ ;

atRule = /@[a-z]+/ , /[^;]+/ , ";" ;
"#;
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(input);
    let pg = result.expect("should parse recover with alternation sync");

    assert_eq!(pg.recovers.len(), 1);
    assert!(matches!(pg.recovers[0].sync_expr, Expression::Alternation(_)));
}

#[test]
fn parse_recover_without_terminator() {
    // @recover should work without a trailing ; (like @import)
    let input = r#"
@recover stmt /[^;]*;/

stmt = /[a-z]+/ , ";" ;
"#;
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(input);
    let pg = result.expect("should parse recover without terminator");

    assert_eq!(pg.recovers.len(), 1);
}
