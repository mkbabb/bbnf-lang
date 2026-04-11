use bbnf::runtime::ParseErr;
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover)]
struct CssPrettyParser;

/// Probe the CSS prettify grammar against a few representative
/// inputs. Tranche AC.2 collapsed the per-rule entry points into
/// a single `Grammar::parse(input)` surface, so the old probe that
/// called `ruleBlock()` / `qualifiedRule()` / `ruleItem()` against
/// substrings no longer maps onto anything the generated parser
/// exposes. The equivalent demonstration now parses each snippet
/// at the stylesheet level and reports whether the parse succeeded
/// and, on failure, the byte offset the tape-first parser halted at.
fn probe(label: &str, input: &str) {
    match CssPrettyParser::parse(input) {
        Ok(parsed) => {
            let view = parsed.view();
            eprintln!(
                "{:20} ok len={} root_kind={:?}",
                label,
                input.len(),
                view.cursor().kind()
            );
        }
        Err(ParseErr::Syntax { offset, rule }) => {
            eprintln!(
                "{:20} err offset={}/{} rule={:?}",
                label,
                offset,
                input.len(),
                rule
            );
        }
        Err(ParseErr::Tape(e)) => {
            eprintln!("{:20} tape error: {:?}", label, e);
        }
    }
}

fn main() {
    probe("rule_block", "{ color: red; }");
    probe("qualified_rule", "body { color: red; }");
    probe("multi_decl", "body { color: red; font-size: 14px; }");
    probe("nested", "body { .btn { color: red; } }");
    probe("stylesheet", "body { color: red; } p { margin: 0; }");
}
