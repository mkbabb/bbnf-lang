use bbnf_derive::Parser;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", slab)]
struct JsonParser;

fn bench_file(name: &str) {
    let candidates = [
        format!("../../data/json/{}", name),
        format!("../data/json/{}", name),
        format!("data/json/{}", name),
    ];
    let path = match candidates.iter().find(|p| std::path::Path::new(p).exists()) {
        Some(p) => p.clone(),
        None => {
            eprintln!("skip {} (not found)", name);
            return;
        }
    };
    let input = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(_) => {
            eprintln!("skip {}", name);
            return;
        }
    };
    let len = input.len();

    // Cold span — single parse, fresh parser construction
    let start = std::time::Instant::now();
    let span_p = JsonParser::value();
    let _ = std::hint::black_box(span_p.parse(std::hint::black_box(&input)));
    let span_cold = start.elapsed();

    // Slab — fresh BumpSlab + parser per iteration
    let n = if len > 1_000_000 { 5 } else { 20 };
    let start = std::time::Instant::now();
    for _ in 0..n {
        let ctx = __JsonParserEnumCtx::with_capacity(input.len() / 32);
        let slab_parser = JsonParser::value();
        let ast = slab_parser
            .parse_with_context(std::hint::black_box(&input), &ctx)
            .unwrap();
        let _ = std::hint::black_box(ast as *const _);
    }
    let slab = start.elapsed() / n as u32;

    let mb = |d: std::time::Duration| len as f64 / d.as_secs_f64() / 1e6;
    println!(
        "{:25} {:>8}B  span_cold:{:>6.0}  slab:{:>6.0} MB/s",
        name,
        len,
        mb(span_cold),
        mb(slab)
    );
}

fn main() {
    for name in [
        "data.json",
        "twitter.json",
        "citm_catalog.json",
        "canada.json",
        "data_xl.json",
    ] {
        bench_file(name);
    }
}
