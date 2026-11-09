// LOCK-7 PROTO-1 probe: run the builder pipeline stages over the bbnf corpus
// and report exactly where/how each stage fails. Read-only diagnostic.

fn main() {
    let path = std::env::args().nth(1).expect("usage: bbnf_probe <inlined.bbnf>");
    let source = std::fs::read_to_string(&path).expect("read source");

    println!("=== STAGE 1: grammar::parse_grammar (hand seed / builder frontend) ===");
    match grammar::parse_grammar("bbnf", &source) {
        Ok(g) => {
            println!("OK  rules={} exprs={}", g.rules.len(), g.exprs.len());
            println!("=== STAGE 2: passes::compile ===");
            match passes::compile(&g) {
                Ok(out) => {
                    println!("OK  backend rules={}", out.backend_ir.rules.len());
                    println!("=== STAGE 3: codegen::emit_from_source (full builder emit) ===");
                    match codegen::emit_from_source("bbnf", &source) {
                        Ok(e) => println!("OK  files={:?}", e.files().map(|(n, _)| n).collect::<Vec<_>>()),
                        Err(e) => println!("ERR emit_from_source: {e:?}"),
                    }
                }
                Err(e) => println!("ERR passes::compile: {e:?}"),
            }
        }
        Err(e) => println!("ERR parse_grammar: {e:?}"),
    }

    println!("=== STAGE 0: grammar::parse_runtime_source_facts (recognizer facts + unsupported gate) ===");
    let src = grammar::RuntimeSource::new("bbnf.bbnf", &source);
    match grammar::parse_runtime_source_facts(&[src]) {
        Ok(facts) => {
            match facts.first_unsupported() {
                Some(u) => println!("first_unsupported: code={} offset={}", u.code, u.offset),
                None => println!("no unsupported constructs"),
            }
        }
        Err(e) => println!("ERR facts: {e:?}"),
    }
}
