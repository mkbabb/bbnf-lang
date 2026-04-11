//! Debug harness: drive `bbnf::grammar::parse` and
//! `BbnfBootstrap::parse` directly on a grammar file and report
//! the outcome. Lives in `bbnf-bootstrap` so it reaches `bbnf`
//! without dragging in the `bbnf` crate's dev-deps (which
//! currently fail to compile during the tape-first transition).

fn main() {
    let path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "grammar/bbnf/bbnf.bbnf".to_string());
    let source = std::fs::read_to_string(&path).expect("read grammar file");

    println!("Parsing {} ({} bytes)", path, source.len());

    // Step 1: drive the tape-first bootstrap parser directly.
    match bbnf::grammar::generated::BbnfBootstrap::parse(&source) {
        Ok(parsed) => {
            println!(
                "BbnfBootstrap::parse OK — tape has {} records, root_offset={:?}",
                parsed.tape().len(),
                parsed.root_offset(),
            );
        }
        Err(e) => {
            println!("BbnfBootstrap::parse Err: {:?}", e);
            std::process::exit(1);
        }
    }

    // Step 2: dump the tape root's direct children, peeling
    // grammar_item / directive wrappers to show what each top-
    // level item's concrete `rule_kind` resolves to.
    {
        let parsed = bbnf::grammar::generated::BbnfBootstrap::parse(&source)
            .expect("second parse");
        let root = parsed.view();
        use bbnf::runtime::tape::TapeKind;
        println!("--- root children dump ---");
        for (i, item) in root.children().enumerate() {
            if item.kind() == TapeKind::Repeat {
                for (j, gc) in item.children().enumerate() {
                    let peeled = peel(gc);
                    println!(
                        "  top[{i}][{j}]: kind={:?} variant_idx={} rule_kind={:?}",
                        peeled.kind(),
                        peeled.variant_idx(),
                        peeled.rule_kind(),
                    );
                }
            } else {
                let peeled = peel(item);
                println!(
                    "  top[{i}]: kind={:?} variant_idx={} rule_kind={:?}",
                    peeled.kind(),
                    peeled.variant_idx(),
                    peeled.rule_kind(),
                );
            }
        }
    }

    // Step 3: drive bbnf::grammar::parse (parse + extract_grammar).
    match bbnf::grammar::parse(&source) {
        Some(parsed_grammar) => {
            println!(
                "bbnf::grammar::parse OK — {} rules, {} recovers, {} imports",
                parsed_grammar.rules.len(),
                parsed_grammar.recovers.len(),
                parsed_grammar.imports.len(),
            );
            for (name, _) in parsed_grammar.rules.iter().take(15) {
                println!("  rule: {}", name);
            }
        }
        None => {
            println!("bbnf::grammar::parse returned None (extract_grammar failed)");
            std::process::exit(1);
        }
    }
}

fn peel<'a>(
    node: bbnf::grammar::generated::BbnfBootstrapNodeView<'a>,
) -> bbnf::grammar::generated::BbnfBootstrapNodeView<'a> {
    use bbnf::grammar::generated::BbnfBootstrapRuleKind as K;
    match node.rule_kind() {
        K::grammar_item | K::directive => {
            if let Some(c) = node.child(0) {
                peel(c)
            } else {
                node
            }
        }
        _ => node,
    }
}
