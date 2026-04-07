#[test]
fn test_consecutive_comments() {
    let source = "// comment 1\n// comment 2\nrule = \"x\" ;";
    let pg = bbnf::grammar::parse(source).unwrap();
    assert_eq!(pg.rules.len(), 1, "should parse rule after consecutive comments");
}

#[test]
fn test_comment_between_rules() {
    let source = "a = \"x\" ;\n// comment\nb = \"y\" ;";
    let pg = bbnf::grammar::parse(source).unwrap();
    assert_eq!(pg.rules.len(), 2, "both rules should parse");
}
