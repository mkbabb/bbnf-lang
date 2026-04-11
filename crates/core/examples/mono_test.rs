use bbnf_derive::Parser;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct P;

fn main() {
    for name in [
        "data.json",
        "twitter.json",
        "citm_catalog.json",
        "canada.json",
        "data_xl.json",
    ] {
        let path = format!("../../data/json/{}", name);
        let input = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };
        let len = input.len();
        let n = if len > 1_000_000 { 5 } else { 20 };

        // Cold — fresh tape + parser state per iteration. The
        // tape-first `P::parse` entry point owns its allocation
        // internally; there is no longer a user-visible slab to
        // pre-size.
        let start = std::time::Instant::now();
        for _ in 0..n {
            let parsed = P::parse(std::hint::black_box(&input)).expect("parse failed");
            std::hint::black_box(&parsed);
        }
        let cold = start.elapsed() / n as u32;

        eprintln!(
            "{:25} {:>8}B  cold:{:>6.0} MB/s",
            name,
            len,
            len as f64 / cold.as_secs_f64() / 1e6
        );
    }
}
