#![feature(cold_path)]
use bbnf_derive::Parser;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;
#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", arena)]
struct P;
fn main() {
    for name in ["data.json", "twitter.json", "citm_catalog.json", "canada.json", "data_xl.json"] {
        let path = format!("../../data/json/{}", name);
        let input = match std::fs::read_to_string(&path) { Ok(s) => s, Err(_) => continue };
        let len = input.len();
        let n = if len > 1_000_000 { 5 } else { 20 };

        // Span cold — single parse, fresh parser construction
        let start = std::time::Instant::now();
        let span_p = P::value();
        let _ = std::hint::black_box(span_p.parse(std::hint::black_box(&input)));
        let span_cold = start.elapsed();

        // Arena — fresh BumpArena + parser per iteration
        let start = std::time::Instant::now();
        for _ in 0..n {
            let a = parse_that::BumpArena::<PArenaEnum<'_>>::with_capacity(input.len() / 32);
            let p = P::value_arena();
            let r = p.parse_with_context(std::hint::black_box(&input), &a).unwrap();
            std::hint::black_box(r as *const _);
        }
        let arena = start.elapsed() / n as u32;

        eprintln!("{:25} {:>8}B  span_cold:{:>6.0}  arena:{:>6.0} MB/s",
            name, len,
            len as f64 / span_cold.as_secs_f64() / 1e6,
            len as f64 / arena.as_secs_f64() / 1e6);
    }
}
