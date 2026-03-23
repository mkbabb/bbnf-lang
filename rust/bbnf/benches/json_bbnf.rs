#![feature(cold_path)]

//! BBNF JSON parsing benchmarks — cold per-parse, three tiers + VM.
//!
//! Fresh BumpArena + Parser per iteration. No warm-cache benchmarks.
//!
//! - **span**: opaque AST spans, structural validation only
//! - **borrow**: borrowed JsonValue, numbers parsed, strings stripped, no escape decode
//! - **copy**: owned JsonValue, full escape decode, Cow strings
//! - **vm**: bytecode interpreter

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::borrow::Cow;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_derive::Parser;
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use parse_that::{BumpArena, Span};

#[derive(Parser)]
#[parser(path = "benches/grammars/json.bbnf", arena)]
struct BbnfJsonParser;

// Compile-time enum size audit: ensure the generated enum stays compact.
// Smaller enums → faster Vec operations (memcpy, reallocation).
const _: () = assert!(std::mem::size_of::<BbnfJsonParserEnum>() <= 48);

fn load_json(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

fn compiled_json_vm() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar =
        std::fs::read_to_string("../../grammar/lang/json.bbnf").expect("failed to read json.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

// ── Borrowed JSON value type ────────────────────────────────────────────────
// Numbers parsed to f64. Strings borrowed as &str (quotes stripped, no escape
// decode). Comparable to serde_json_borrow on inputs without escape sequences.

#[derive(Debug)]
#[allow(dead_code)]
enum BorrowedJsonValue<'a> {
    Null,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Array(Vec<BorrowedJsonValue<'a>>),
    Object(Vec<(&'a str, BorrowedJsonValue<'a>)>),
}

fn to_borrowed_arena<'a>(node: &'a BbnfJsonParserArenaEnum<'a>) -> BorrowedJsonValue<'a> {
    match node {
        BbnfJsonParserArenaEnum::null(_) => BorrowedJsonValue::Null,
        BbnfJsonParserArenaEnum::r#bool(s) => BorrowedJsonValue::Bool(s.as_str() == "true"),
        BbnfJsonParserArenaEnum::number(s) => {
            BorrowedJsonValue::Number(fast_float2::parse(s.as_str()).unwrap())
        }
        BbnfJsonParserArenaEnum::string(s) => {
            let raw = s.as_str();
            BorrowedJsonValue::String(&raw[1..raw.len() - 1])
        }
        BbnfJsonParserArenaEnum::array(items) => {
            BorrowedJsonValue::Array(items.iter().map(to_borrowed_arena).collect())
        }
        BbnfJsonParserArenaEnum::object(pairs) => BorrowedJsonValue::Object(
            pairs
                .iter()
                .map(|p| {
                    let BbnfJsonParserArenaEnum::pair((key_span, val_ref)) = p else {
                        unreachable!()
                    };
                    let raw = key_span.as_str();
                    (&raw[1..raw.len() - 1], to_borrowed_arena(val_ref))
                })
                .collect(),
        ),
        _ => unreachable!(),
    }
}

// ── Owned JSON value type ───────────────────────────────────────────────────
// Full escape decode including Unicode surrogates. Cow<str> borrows when no
// escapes present, allocates only on the slow path.

#[derive(Debug)]
#[allow(dead_code)]
enum OwnedJsonValue<'a> {
    Null,
    Bool(bool),
    Number(f64),
    String(Cow<'a, str>),
    Array(Vec<OwnedJsonValue<'a>>),
    Object(Vec<(Cow<'a, str>, OwnedJsonValue<'a>)>),
}

fn to_owned_value_arena<'a>(node: &'a BbnfJsonParserArenaEnum<'a>) -> OwnedJsonValue<'a> {
    match node {
        BbnfJsonParserArenaEnum::null(_) => OwnedJsonValue::Null,
        BbnfJsonParserArenaEnum::r#bool(s) => OwnedJsonValue::Bool(s.as_str() == "true"),
        BbnfJsonParserArenaEnum::number(s) => {
            OwnedJsonValue::Number(fast_float2::parse(s.as_str()).unwrap())
        }
        BbnfJsonParserArenaEnum::string(s) => OwnedJsonValue::String(decode_string(*s)),
        BbnfJsonParserArenaEnum::array(items) => {
            OwnedJsonValue::Array(items.iter().map(to_owned_value_arena).collect())
        }
        BbnfJsonParserArenaEnum::object(pairs) => OwnedJsonValue::Object(
            pairs
                .iter()
                .map(|p| {
                    let BbnfJsonParserArenaEnum::pair((key_span, val_ref)) = p else {
                        unreachable!()
                    };
                    (decode_string(*key_span), to_owned_value_arena(val_ref))
                })
                .collect(),
        ),
        _ => unreachable!(),
    }
}

#[inline]
fn decode_string<'a>(s: Span<'a>) -> Cow<'a, str> {
    let raw = s.as_str();
    let inner = &raw[1..raw.len() - 1];
    if !inner.contains('\\') {
        return Cow::Borrowed(inner);
    }
    let mut out = String::with_capacity(inner.len());
    let bytes = inner.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            i += 1;
            match bytes[i] {
                b'"' => out.push('"'),
                b'\\' => out.push('\\'),
                b'/' => out.push('/'),
                b'b' => out.push('\u{08}'),
                b'f' => out.push('\u{0C}'),
                b'n' => out.push('\n'),
                b'r' => out.push('\r'),
                b't' => out.push('\t'),
                b'u' => {
                    let hex = &inner[i + 1..i + 5];
                    let cp = u16::from_str_radix(hex, 16).unwrap();
                    i += 4;
                    if (0xD800..=0xDBFF).contains(&cp) {
                        i += 1; // skip backslash
                        i += 1; // skip 'u'
                        let hex2 = &inner[i..i + 4];
                        let lo = u16::from_str_radix(hex2, 16).unwrap();
                        i += 4;
                        let full = 0x10000 + ((cp as u32 - 0xD800) << 10) + (lo as u32 - 0xDC00);
                        out.push(char::from_u32(full).unwrap());
                        i += 1;
                        continue;
                    }
                    out.push(char::from_u32(cp as u32).unwrap());
                }
                _ => {
                    out.push('\\');
                    out.push(bytes[i] as char);
                }
            }
        } else {
            out.push(bytes[i] as char);
        }
        i += 1;
    }
    Cow::Owned(out)
}

// ── Span tier (cold per-parse) ──────────────────────────────────────

macro_rules! bench_span {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            b.bytes = input.len() as u64;
            {
                let arena = BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    concat!($file, ": arena parse failed")
                );
            }
            b.iter(|| {
                let arena = BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_span!(span_data, "data.json");
bench_span!(span_twitter, "twitter.json");
bench_span!(span_citm, "citm_catalog.json");
bench_span!(span_canada, "canada.json");
bench_span!(span_data_xl, "data_xl.json");

// ── Borrow tier (cold per-parse) ────────────────────────────────────

macro_rules! bench_borrow {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            b.bytes = input.len() as u64;
            {
                let arena = BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    concat!($file, ": arena parse failed")
                );
            }
            b.iter(|| {
                let arena = BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(to_borrowed_arena(ast));
            });
        }
    };
}

bench_borrow!(borrow_data, "data.json");
bench_borrow!(borrow_twitter, "twitter.json");
bench_borrow!(borrow_citm, "citm_catalog.json");
bench_borrow!(borrow_canada, "canada.json");
bench_borrow!(borrow_data_xl, "data_xl.json");

// ── Copy tier (cold per-parse) ─────────────────────────────────────

macro_rules! bench_copy {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            b.bytes = input.len() as u64;
            {
                let arena = BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    concat!($file, ": arena parse failed")
                );
            }
            b.iter(|| {
                let arena = BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(to_owned_value_arena(ast));
            });
        }
    };
}

bench_copy!(copy_data, "data.json");
bench_copy!(copy_twitter, "twitter.json");
bench_copy!(copy_citm, "citm_catalog.json");
bench_copy!(copy_canada, "canada.json");
bench_copy!(copy_data_xl, "data_xl.json");

// ── VM tier (bytecode interpreter) ──────────────────────────────────────────

macro_rules! bench_vm {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            let (_ir, program) = compiled_json_vm();
            b.bytes = input.len() as u64;
            {
                let mut interp = Interpreter::new(&program, &input);
                let r = interp.run();
                assert!(r.success, concat!($file, ": VM parse failed"));
            }
            b.iter(|| {
                let mut interp = Interpreter::new(&program, black_box(&input));
                let r = interp.run();
                assert!(r.success);
            });
        }
    };
}

bench_vm!(vm_data, "data.json");
bench_vm!(vm_twitter, "twitter.json");
bench_vm!(vm_citm, "citm_catalog.json");
bench_vm!(vm_canada, "canada.json");
bench_vm!(vm_data_xl, "data_xl.json");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    span,
    span_data,
    span_twitter,
    span_citm,
    span_canada,
    span_data_xl
);
benchmark_group!(
    borrow,
    borrow_data,
    borrow_twitter,
    borrow_citm,
    borrow_canada,
    borrow_data_xl
);
benchmark_group!(
    copy,
    copy_data,
    copy_twitter,
    copy_citm,
    copy_canada,
    copy_data_xl
);
benchmark_group!(vm, vm_data, vm_twitter, vm_citm, vm_canada, vm_data_xl);
benchmark_main!(span, borrow, copy, vm);

