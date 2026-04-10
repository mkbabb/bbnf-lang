
//! BBNF CSS monolithic slab benchmark — cold per-parse (pretty.bbnf).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover, slab)]
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
                let slab = __CssPrettyParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssPrettyParser::stylesheet();
                let (result, state) = parser.parse_return_state_with_context(&input, &slab);
                assert!(result.is_some(), concat!($file, ": parse failed"));
                assert!(
                    state.offset >= input.trim_end().len(),
                    concat!($file, ": incomplete parse ({}/{})"),
                    state.offset,
                    input.len(),
                );
            }
            b.iter(|| {
                let slab = __CssPrettyParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssPrettyParser::stylesheet();
                let ast = parser.parse_with_context(black_box(&input), &slab).unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench!(normalize, "normalize.css");
bench!(bootstrap, "bootstrap.css");
bench!(tailwind, "tailwind.css");

benchmark_group!(benches, normalize, bootstrap, tailwind);
benchmark_main!(benches);
