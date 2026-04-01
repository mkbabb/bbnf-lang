use bbnf_derive::Parser;
use parse_that::BumpArena;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover, arena)]
struct CssPrettyParser;

fn main() {
    let input = "body { color: red; }";
    let arena = BumpArena::<CssPrettyParserArenaEnum<'_>>::with_capacity(64);

    let (r, s) =
        CssPrettyParser::selectorSpan_arena().parse_return_state_with_context(input, &arena);
    eprintln!("selectorSpan: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::ruleBlock_arena()
        .parse_return_state_with_context("{ color: red; }", &arena);
    eprintln!("ruleBlock: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) =
        CssPrettyParser::qualifiedRule_arena().parse_return_state_with_context(input, &arena);
    eprintln!("qualifiedRule: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::ruleItem_arena().parse_return_state_with_context(input, &arena);
    eprintln!("ruleItem: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::ruleList_arena().parse_return_state_with_context(input, &arena);
    eprintln!("ruleList: ok={} offset={}", r.is_some(), s.offset);

    let (r, s) = CssPrettyParser::stylesheet_arena().parse_return_state_with_context(input, &arena);
    eprintln!(
        "stylesheet: ok={} offset={}/{} furthest={}",
        r.is_some(),
        s.offset,
        input.len(),
        s.furthest_offset
    );
}
