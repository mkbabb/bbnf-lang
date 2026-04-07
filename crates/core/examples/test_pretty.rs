use bbnf_derive::Parser;
use parse_that::BumpSlab;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover, slab)]
struct CssPrettyParser;

fn main() {
    let input = "body { color: red; }";
    let slab = BumpSlab::with_capacity(64 * std::mem::size_of::<CssPrettyParserEnum>());

    let (r, s) =
        CssPrettyParser::ruleBlock().parse_return_state_with_context("{ color: red; }", &slab);
    eprintln!("ruleBlock: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::qualifiedRule().parse_return_state_with_context(input, &slab);
    eprintln!("qualifiedRule: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::ruleItem().parse_return_state_with_context(input, &slab);
    eprintln!("ruleItem: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::ruleList().parse_return_state_with_context(input, &slab);
    eprintln!("ruleList: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::stylesheet().parse_return_state_with_context(input, &slab);
    eprintln!(
        "stylesheet: ok={} offset={}/{} furthest={}",
        r.is_some(),
        s.offset,
        input.len(),
        s.furthest_offset
    );
}
