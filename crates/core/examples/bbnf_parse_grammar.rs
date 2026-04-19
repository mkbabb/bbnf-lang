//! AX.W0a.2.k — diagnose json.bbnf / bnf.bbnf parse failures.

use bbnf::grammar::generated::BbnfBootstrap;

fn probe(name: &str, path: &str) {
    let input = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{name}: READ ERR {:?}", e);
            return;
        }
    };
    println!("--- {name} (len={}) ---", input.len());
    match BbnfBootstrap::parse(&input) {
        Ok(_) => println!("{name}: parse OK"),
        Err(e) => {
            println!("{name}: PARSE ERR {:?}", e);
        }
    }
}

fn main() {
    probe("json.bbnf", "grammar/json/json.bbnf");
    probe("bnf.bbnf", "grammar/bnf/bnf.bbnf");
    probe("ebnf.bbnf", "grammar/ebnf/ebnf.bbnf");
    probe("bbnf.bbnf", "grammar/bbnf/bbnf.bbnf");
}
