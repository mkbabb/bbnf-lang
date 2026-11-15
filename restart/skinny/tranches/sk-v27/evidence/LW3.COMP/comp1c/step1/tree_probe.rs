//! LW3.COMP-1c step 1 probe (evidence form; copied into crates/core/tests by probe.sh and removed
//! after): dumps the tree `BbnfBootstrap::parse` builds for a source that writes every form
//! `grammar/bbnf/bbnf.bbnf` names, so the Syntax builder's mapping is read from the tree, not guessed.
use bbnf::grammar::generated::bbnf::BbnfBootstrap;
use bbnf::runtime::bbnf::{BbnfDocument, BbnfValue};

const SOURCE: &str = r#"@import { a, b } from "lib" ;
@import "other" ;
@ws /[ \t]+/ ;
@token ident ;
@debug * ;
@host Env : Map ;
@recover stmt /[^;]*/ ;
@pretty rule group indent sep(", ") ;
// a line comment
/* a block comment */
ident = /[a-z]+/ -> Span ;
num = /[0-9]+/ -> parse(input) : i64 ;
tagged = "x" -> 3u8 ;
pair = ident << ":" >> num ;
diff = ident - "x" ;
juxt = ident num ;
opt = [ ident ] , { num } , @{ ident , num } , ( ident | num ) ;
mods = ident ?w , num ? , ident * , num + ;
wrap = |x, y| "(" , x , y , ")" ;
use = wrap(ident, num | ident) ;
eps = ε | epsilon ;
done = ident .
"#;

fn dump(doc: &BbnfDocument<'_>, v: &BbnfValue<'_>, depth: usize, out: &mut String) {
    let pad = "  ".repeat(depth);
    match v {
        BbnfValue::Compound(id) => {
            let c = doc.compound(*id);
            let text = c.bounds.map(|(s, e)| doc.input()[s as usize..e as usize].replace('\n', "\\n"));
            out.push_str(&format!("{pad}{:?} tag={:?} bounds={:?} {:?}\n", c.kind, c.branch_tag, c.bounds, text));
            for child in &c.children {
                dump(doc, child, depth + 1, out);
            }
        }
        other => out.push_str(&format!("{pad}{other:?}\n")),
    }
}

#[test]
fn dump_bootstrap_tree() {
    let doc = BbnfBootstrap::parse(SOURCE).expect("the probe source parses");
    let mut out = String::new();
    dump(&doc, doc.root(), 0, &mut out);
    println!("{out}");
}
