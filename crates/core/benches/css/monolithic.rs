
//! BBNF CSS monolithic benchmark — cold per-parse (pretty.bbnf, tape-first).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover)]
struct CssPrettyParser;

fn load(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            {
                let parsed = CssPrettyParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                black_box(&parsed);
            }
            b.iter(|| {
                let parsed = CssPrettyParser::parse(black_box(&input)).unwrap();
                black_box(parsed);
            });
        }
    };
}

bench!(normalize, "normalize.css");
bench!(bootstrap, "bootstrap.css");
bench!(tailwind, "tailwind.css");

benchmark_group!(benches, normalize, bootstrap, tailwind);
benchmark_main!(benches);
