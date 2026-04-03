#![feature(cold_path)]

//! Google Sheets formula AOT benchmark — monolithic codegen.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", prettify)]
struct GoogleSheetsParser;

const PATHOLOGICAL: &str = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))"#;

fn generate_large_formula(n_bindings: usize) -> String {
    let mut parts = Vec::with_capacity(n_bindings * 2 + 1);
    for i in 0..n_bindings {
        parts.push(format!("v{}", i));
        parts.push(format!(
            "IF(v{}>0, FILTER(A1:Z100, INDEX(A1:Z100,,{})>0), SUM(A1:A{}))",
            i,
            i + 1,
            i + 10
        ));
    }
    parts.push(format!("v{}", n_bindings - 1));
    format!("=LET({})", parts.join(", "))
}

fn parse_pathological(b: &mut Bencher) {
    b.bytes = PATHOLOGICAL.len() as u64;
    {
        let slab = __GoogleSheetsParserEnumCtx::with_capacity(PATHOLOGICAL.len() / 16);
        let parser = GoogleSheetsParser::formula();
        let (result, state) = parser.parse_return_state_with_context(PATHOLOGICAL, &slab);
        assert!(result.is_some(), "pathological: parse failed");
        assert_eq!(
            state.offset,
            PATHOLOGICAL.len(),
            "pathological: incomplete parse ({}/{})",
            state.offset,
            PATHOLOGICAL.len(),
        );
    }
    b.iter(|| {
        let slab = __GoogleSheetsParserEnumCtx::with_capacity(PATHOLOGICAL.len() / 16);
        let parser = GoogleSheetsParser::formula();
        let ast = parser
            .parse_with_context(black_box(PATHOLOGICAL), &slab)
            .unwrap();
        black_box(&ast as *const _ as usize)
    });
}

fn parse_1kb(b: &mut Bencher) {
    let input = generate_large_formula(10);
    b.bytes = input.len() as u64;
    {
        let slab = __GoogleSheetsParserEnumCtx::with_capacity(input.len() / 16);
        let parser = GoogleSheetsParser::formula();
        let (result, state) = parser.parse_return_state_with_context(&input, &slab);
        assert!(result.is_some(), "parse_1kb: parse failed");
        assert_eq!(
            state.offset,
            input.len(),
            "parse_1kb: incomplete parse ({}/{})",
            state.offset,
            input.len(),
        );
    }
    b.iter(|| {
        let slab = __GoogleSheetsParserEnumCtx::with_capacity(input.len() / 16);
        let parser = GoogleSheetsParser::formula();
        let ast = parser.parse_with_context(black_box(&input), &slab).unwrap();
        black_box(&ast as *const _ as usize)
    });
}

fn parse_10kb(b: &mut Bencher) {
    let input = generate_large_formula(100);
    b.bytes = input.len() as u64;
    {
        let slab = __GoogleSheetsParserEnumCtx::with_capacity(input.len() / 16);
        let parser = GoogleSheetsParser::formula();
        let (result, state) = parser.parse_return_state_with_context(&input, &slab);
        assert!(result.is_some(), "parse_10kb: parse failed");
        assert_eq!(
            state.offset,
            input.len(),
            "parse_10kb: incomplete parse ({}/{})",
            state.offset,
            input.len(),
        );
    }
    b.iter(|| {
        let slab = __GoogleSheetsParserEnumCtx::with_capacity(input.len() / 16);
        let parser = GoogleSheetsParser::formula();
        let ast = parser.parse_with_context(black_box(&input), &slab).unwrap();
        black_box(&ast as *const _ as usize)
    });
}

fn format_pathological(b: &mut Bencher) {
    let config = pprint::Printer::new(80, 2, false);
    b.bytes = PATHOLOGICAL.len() as u64;
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(PATHOLOGICAL);
        assert!(result.is_some(), "format_pathological: parse failed");
        assert_eq!(
            state.offset,
            PATHOLOGICAL.len(),
            "format_pathological: incomplete parse ({}/{})",
            state.offset,
            PATHOLOGICAL.len(),
        );
    }
    b.iter(|| {
        let parser = GoogleSheetsParser::formula_prettify();
        let ops = parser.parse(black_box(PATHOLOGICAL)).unwrap();
        pprint::render(&ops, config)
    });
}

fn format_1kb(b: &mut Bencher) {
    let input = generate_large_formula(10);
    let config = pprint::Printer::new(80, 2, false);
    b.bytes = input.len() as u64;
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(&input);
        assert!(result.is_some(), "format_1kb: parse failed");
        assert_eq!(
            state.offset,
            input.len(),
            "format_1kb: incomplete parse ({}/{})",
            state.offset,
            input.len(),
        );
    }
    b.iter(|| {
        let parser = GoogleSheetsParser::formula_prettify();
        let ops = parser.parse(black_box(&input)).unwrap();
        pprint::render(&ops, config)
    });
}

fn format_10kb(b: &mut Bencher) {
    let input = generate_large_formula(100);
    let config = pprint::Printer::new(80, 2, false);
    b.bytes = input.len() as u64;
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(&input);
        assert!(result.is_some(), "format_10kb: parse failed");
        assert_eq!(
            state.offset,
            input.len(),
            "format_10kb: incomplete parse ({}/{})",
            state.offset,
            input.len(),
        );
    }
    b.iter(|| {
        let parser = GoogleSheetsParser::formula_prettify();
        let ops = parser.parse(black_box(&input)).unwrap();
        pprint::render(&ops, config)
    });
}

benchmark_group!(
    benches,
    parse_pathological,
    parse_1kb,
    parse_10kb,
    format_pathological,
    format_1kb,
    format_10kb,
);
benchmark_main!(benches);
