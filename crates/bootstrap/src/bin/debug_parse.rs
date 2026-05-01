//! Debug harness: drive `bbnf::grammar::parse` and
//! `BbnfBootstrap::parse` directly on a grammar file and report
//! the outcome.
//!
//! AZ-II.cutover.D — `BbnfBootstrap::parse` now returns
//! `bbnf::runtime::bbnf::BbnfDocument<'_>` per the StructDirect
//! resolver-arm flip. Tape-cursor accessors retired; we report
//! the document via `RuntimeView` + `BbnfCompoundKind`.

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};

fn main() {
    let path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "grammar/bbnf/bbnf.bbnf".to_string());
    let source = std::fs::read_to_string(&path).expect("read grammar file");

    println!("Parsing {} ({} bytes)", path, source.len());

    // Step 1: drive the struct-direct bootstrap parser directly.
    match bbnf::grammar::generated::BbnfBootstrap::parse(&source) {
        Ok(document) => {
            let root = document.view();
            println!(
                "BbnfBootstrap::parse OK — root kind={:?} compound_kind={:?} \
                 root_children={}",
                root.kind(),
                root.compound_kind(),
                root.num_children(),
            );
        }
        Err(e) => {
            println!("BbnfBootstrap::parse Err: {:?}", e);
            std::process::exit(1);
        }
    }

    // Step 2: dump the root's direct children, peeling grammar_item /
    // directive wrappers to show what each top-level item's concrete
    // `compound_kind` resolves to.
    {
        let document =
            bbnf::grammar::generated::BbnfBootstrap::parse(&source).expect("second parse");
        let root = document.view();
        println!("--- root children dump ---");
        for (i, item) in root.children().enumerate() {
            let peeled = peel(item);
            println!(
                "  top[{i}]: kind={:?} compound_kind={:?} branch_tag={:?} text={:?}",
                peeled.kind(),
                peeled.compound_kind(),
                peeled.branch_tag(),
                peeled.span_text(),
            );
        }
    }

    // Step 3: drive bbnf::grammar::parse (parse + extract_grammar).
    match bbnf::grammar::parse(&source) {
        Some(parsed_grammar) => {
            println!(
                "bbnf::grammar::parse OK — {} rules, {} recovers, {} imports, \
                 {} pretties, {} tokens, {} debugs, {} hosts, ws={}",
                parsed_grammar.rules.len(),
                parsed_grammar.recovers.len(),
                parsed_grammar.imports.len(),
                parsed_grammar.pretties.len(),
                parsed_grammar.token_rules.len(),
                parsed_grammar.debug_rules.len(),
                parsed_grammar.host_fns.len(),
                parsed_grammar.ws_pattern.is_some(),
            );
            for (name, _) in parsed_grammar.rules.iter().take(15) {
                println!("  rule: {}", name);
            }
            for imp in &parsed_grammar.imports {
                println!(
                    "  import: path={:?}, items={:?}",
                    imp.path.as_ref(),
                    imp.items.as_ref().map(|v| v
                        .iter()
                        .map(|n| n.name.as_ref().to_string())
                        .collect::<Vec<_>>())
                );
            }
            for p in &parsed_grammar.pretties {
                println!(
                    "  pretty: rule={:?}, hints={:?}",
                    p.rule_name.as_ref(),
                    p.hints
                        .iter()
                        .map(|h| h.as_ref().to_string())
                        .collect::<Vec<_>>()
                );
            }
            for d in &parsed_grammar.debug_rules {
                println!("  debug: {}", d);
            }
            for h in &parsed_grammar.host_fns {
                println!("  host: {} -> {:?}", h.name, h.return_type);
            }
            if let Some(ws) = &parsed_grammar.ws_pattern {
                println!("  ws: {}", ws);
            }
        }
        None => {
            println!("bbnf::grammar::parse returned None (extract_grammar failed)");
            std::process::exit(1);
        }
    }
}

fn peel<'a, 'p>(node: BbnfView<'a, 'p>) -> BbnfView<'a, 'p> {
    match node.compound_kind() {
        Some(BbnfCompoundKind::GrammarItem) | Some(BbnfCompoundKind::Directive) => {
            if let Some(c) = node.child(0) {
                peel(c)
            } else {
                node
            }
        }
        _ => node,
    }
}
