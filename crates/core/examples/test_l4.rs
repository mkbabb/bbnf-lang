use bbnf::runtime::{ParseErr, RuntimeView};
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_s: &str) -> u32 {
        0
    }
}

use ::bbnf::grammar::generated::css_l4::*;


/// Probe the CSS L4 grammar against representative inputs. Under
/// tape-first (Tranche AC.2) the parser exposes a single
/// `CssL4Parser::parse` entry point; the old probe that reached in
/// through `CssL4Parser::stylesheet()` to inspect the combinator
/// state's `offset` / `furthest_offset` is replaced by
/// `ParseErr::Syntax { offset, rule }`, which carries the same
/// halt-offset information through the public surface.
fn test_input(label: &str, input: &str) {
    match CssL4Parser::parse(input) {
        Ok(parsed) => {
            let view = parsed.view();
            eprintln!(
                "{:40} ok len={} root_kind={:?}",
                label,
                input.len(),
                view.kind()
            );
        }
        Err(ParseErr::Syntax { offset, rule }) => {
            let pct = if input.is_empty() {
                100
            } else {
                offset as usize * 100 / input.len()
            };
            eprintln!(
                "{:40} err offset={}/{} ({}%) rule={:?}",
                label,
                offset,
                input.len(),
                pct,
                rule
            );
            let offset = offset as usize;
            if offset < input.len() {
                let end = std::cmp::min(offset + 40, input.len());
                eprintln!("  stuck at: '{}'", &input[offset..end]);
            }
        }
        Err(ParseErr::Tape(e)) => {
            eprintln!("{:40} tape error: {:?}", label, e);
        }
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
