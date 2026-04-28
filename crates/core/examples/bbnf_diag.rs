fn main() {
    use bbnf::grammar::generated::BbnfBootstrap;
    let cases = [
        ("simple-rule", "x = \"a\" ;\n"),
        ("alt-rule", "x = \"a\" | \"b\" ;\n"),
        ("regex-rule", "x = /[a-z]+/ ;\n"),
        ("typed-leaf", "x = /[a-z]+/ -> str ;\n"),
        ("ident-rule", "x = y , z ;\ny = \"a\" ;\nz = \"b\" ;\n"),
    ];
    for (name, input) in cases {
        match BbnfBootstrap::parse(input) {
            Ok(_) => println!("PASS: {}", name),
            Err(e) => println!("FAIL: {} — {:?}", name, e),
        }
    }
}
