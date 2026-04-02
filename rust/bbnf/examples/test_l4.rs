use bbnf_derive::Parser;
use parse_that::BumpSlab;

#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_s: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover, arena)]
struct CssL4Parser;

fn test_input(label: &str, input: &str) {
    let slab = BumpSlab::with_capacity(64 * std::mem::size_of::<CssL4ParserEnum>());
    let (result, state) =
        CssL4Parser::stylesheet().parse_return_state_with_context(input, &slab);
    let pct = if input.is_empty() {
        100
    } else {
        state.offset * 100 / input.len()
    };
    eprintln!(
        "{:40} offset={}/{} ({}%) success={}",
        label,
        state.offset,
        input.len(),
        pct,
        result.is_some()
    );
    if state.offset < input.len() {
        let end = std::cmp::min(state.offset + 40, input.len());
        eprintln!("  stuck at: '{}'", &input[state.offset..end]);
    }
}

fn main() {
    test_input("basic", "body { font-family: Arial; color: red; }");
    test_input(
        "custom prop empty value",
        "body { --bs-btn-font-family: ; }",
    );
    test_input("custom prop with value", "body { --bs-btn-color: red; }");
    test_input(
        "bootstrap mixed",
        ".btn { --bs-btn-font-family: ; --bs-btn-color: red; font-size: 1rem; }",
    );
}
