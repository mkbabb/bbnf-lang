#![feature(cold_path)]
use bbnf_derive::Parser;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(Parser)]
#[parser(path = "benches/grammars/json.bbnf")]
struct BbnfJsonParser;

fn main() {
    let files = ["data.json", "twitter.json", "citm_catalog.json", "canada.json"];
    for name in &files {
        let path = format!("../../data/json/{}", name);
        let input = std::fs::read_to_string(&path).unwrap();
        let parser = BbnfJsonParser::value();
        let (result, state) = parser.parse_return_state(&input);
        let pct = (state.offset as f64 / input.len() as f64) * 100.0;
        println!("{}: len={}, consumed={}, pct={:.1}%, ok={}",
            name, input.len(), state.offset, pct, result.is_some());
    }
}
