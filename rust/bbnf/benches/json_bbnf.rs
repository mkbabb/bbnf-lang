#![feature(cold_path)]

//! BBNF JSON parsing benchmarks — four tiers.
//!
//! - **span**: Raw BBNF parse (opaque AST spans)
//! - **borrow**: Borrowed JsonValue (numbers parsed, strings stripped — no escape decode)
//! - **owned**: Owned JsonValue (full escape decode, Cow strings)
//! - **vm**: Bytecode interpreter

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::borrow::Cow;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_derive::Parser;
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use parse_that::Span;

#[derive(Parser)]
#[parser(path = "benches/grammars/json.bbnf")]
struct BbnfJsonParser;

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

fn to_borrowed<'a>(node: BbnfJsonParserEnum<'a>) -> BorrowedJsonValue<'a> {
    match node {
        BbnfJsonParserEnum::null(_) => BorrowedJsonValue::Null,
        BbnfJsonParserEnum::r#bool(s) => BorrowedJsonValue::Bool(s.as_str() == "true"),
        BbnfJsonParserEnum::number(s) => {
            BorrowedJsonValue::Number(fast_float2::parse(s.as_str()).unwrap())
        }
        BbnfJsonParserEnum::string(s) => {
            let raw = s.as_str();
            BorrowedJsonValue::String(&raw[1..raw.len() - 1])
        }
        BbnfJsonParserEnum::array(items) => {
            BorrowedJsonValue::Array(items.into_iter().map(to_borrowed).collect())
        }
        BbnfJsonParserEnum::object(pairs) => BorrowedJsonValue::Object(
            pairs
                .into_iter()
                .map(|p| {
                    let BbnfJsonParserEnum::pair((key_span, val_box)) = p else {
                        unreachable!()
                    };
                    let raw = key_span.as_str();
                    (&raw[1..raw.len() - 1], to_borrowed(*val_box))
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

fn to_owned_value<'a>(node: BbnfJsonParserEnum<'a>) -> OwnedJsonValue<'a> {
    match node {
        BbnfJsonParserEnum::null(_) => OwnedJsonValue::Null,
        BbnfJsonParserEnum::r#bool(s) => OwnedJsonValue::Bool(s.as_str() == "true"),
        BbnfJsonParserEnum::number(s) => {
            OwnedJsonValue::Number(fast_float2::parse(s.as_str()).unwrap())
        }
        BbnfJsonParserEnum::string(s) => OwnedJsonValue::String(decode_string(s)),
        BbnfJsonParserEnum::array(items) => {
            OwnedJsonValue::Array(items.into_iter().map(to_owned_value).collect())
        }
        BbnfJsonParserEnum::object(pairs) => OwnedJsonValue::Object(
            pairs
                .into_iter()
                .map(|p| {
                    let BbnfJsonParserEnum::pair((key_span, val_box)) = p else {
                        unreachable!()
                    };
                    (decode_string(key_span), to_owned_value(*val_box))
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
                        let full =
                            0x10000 + ((cp as u32 - 0xD800) << 10) + (lo as u32 - 0xDC00);
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

// ── Span tier (raw BBNF parse) ─────────────────────────────────────────────

macro_rules! bench_span {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            let parser = BbnfJsonParser::value();
            b.bytes = input.len() as u64;
            assert!(parser.parse(&input).is_some(), concat!($file, ": parse failed"));
            b.iter(|| parser.parse(black_box(&input)).unwrap());
        }
    };
}

bench_span!(span_data, "data.json");
bench_span!(span_twitter, "twitter.json");
bench_span!(span_citm, "citm_catalog.json");
bench_span!(span_canada, "canada.json");

// ── Borrow tier (borrowed JsonValue) ────────────────────────────────────────

macro_rules! bench_borrow {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            let parser = BbnfJsonParser::value();
            b.bytes = input.len() as u64;
            assert!(parser.parse(&input).is_some(), concat!($file, ": parse failed"));
            b.iter(|| {
                let ast = parser.parse(black_box(&input)).unwrap();
                to_borrowed(*ast)
            });
        }
    };
}

bench_borrow!(borrow_data, "data.json");
bench_borrow!(borrow_twitter, "twitter.json");
bench_borrow!(borrow_citm, "citm_catalog.json");
bench_borrow!(borrow_canada, "canada.json");

// ── Owned tier (full escape decode) ─────────────────────────────────────────

macro_rules! bench_owned {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            let parser = BbnfJsonParser::value();
            b.bytes = input.len() as u64;
            assert!(parser.parse(&input).is_some(), concat!($file, ": parse failed"));
            b.iter(|| {
                let ast = parser.parse(black_box(&input)).unwrap();
                to_owned_value(*ast)
            });
        }
    };
}

bench_owned!(owned_data, "data.json");
bench_owned!(owned_twitter, "twitter.json");
bench_owned!(owned_citm, "citm_catalog.json");
bench_owned!(owned_canada, "canada.json");

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

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(span, span_data, span_twitter, span_citm, span_canada);
benchmark_group!(borrow, borrow_data, borrow_twitter, borrow_citm, borrow_canada);
benchmark_group!(owned, owned_data, owned_twitter, owned_citm, owned_canada);
benchmark_group!(vm, vm_data, vm_twitter, vm_citm, vm_canada);
benchmark_main!(span, borrow, owned, vm);
