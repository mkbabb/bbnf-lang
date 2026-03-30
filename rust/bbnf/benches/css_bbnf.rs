#![feature(cold_path)]

//! CSS parsing benchmarks — cold per-parse, four BBNF tiers.
//!
//! All benches construct a fresh BumpArena + Parser per iteration.
//!
//! - **arena**: css-fast.bbnf — L0 typed enum tree, opaque spans for values/selectors
//! - **span**: css-fast.bbnf — L0 zero-alloc byte ranges, validation only
//! - **semantic**: css-semantic.bbnf — L1 typed values (numbers → f64 via -> mapping)
//! - **l4**: css-stylesheet.bbnf — full CSS L4 spec via @import composition

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[cfg(not(feature = "dhat-heap"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};
use parse_that::BumpArena;

/// Semantic CSS value types for direct-to-struct parsing.
/// These are constructed DURING parsing via `->` grammar map expressions.
#[allow(dead_code)]
mod css_types {
    /// Parse a hex color string (#RGB, #RRGGBB, #RGBA, #RRGGBBAA) to packed RGBA u32.
    pub fn parse_hex_color(s: &str) -> u32 {
        let hex = s.as_bytes();
        let len = hex.len();
        match len {
            3 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
            }
            4 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                let a = hex_digit(hex[3]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | (a << 4 | a)
            }
            6 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                (r << 24) | (g << 16) | (b << 8) | 0xFF
            }
            8 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                let a = hex_byte(hex[6], hex[7]);
                (r << 24) | (g << 16) | (b << 8) | a
            }
            _ => 0,
        }
    }

    #[inline(always)]
    fn hex_digit(b: u8) -> u32 {
        match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => 0,
        }
    }

    #[inline(always)]
    fn hex_byte(hi: u8, lo: u8) -> u32 {
        (hex_digit(hi) << 4) | hex_digit(lo)
    }

    /// CSS length unit discriminant (matches lightningcss LengthValue variants).
    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum LengthUnit {
        Px = 0,
        Em = 1,
        Rem = 2,
        Vh = 3,
        Vw = 4,
        Vmin = 5,
        Vmax = 6,
        Ch = 7,
        Ex = 8,
        Cm = 9,
        Mm = 10,
        In = 11,
        Pt = 12,
        Pc = 13,
        Lh = 14,
        Rlh = 15,
        Svw = 16,
        Svh = 17,
        Dvw = 18,
        Dvh = 19,
        Lvw = 20,
        Lvh = 21,
        Cqw = 22,
        Cqh = 23,
        Cqi = 24,
        Cqb = 25,
    }

    /// CSS angle unit discriminant.
    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum AngleUnit {
        Deg = 0,
        Rad = 1,
        Grad = 2,
        Turn = 3,
    }

    /// CSS time unit discriminant.
    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum TimeUnit {
        Ms = 0,
        S = 1,
    }

    /// Packed RGBA color (same layout as lightningcss RGBA).
    #[derive(Debug, Clone, Copy)]
    pub struct CssColor {
        pub r: u8,
        pub g: u8,
        pub b: u8,
        pub a: u8,
    }

    /// Parse CSS rgb/rgba function arguments to CssColor.
    pub fn parse_rgb_color(s: &str) -> CssColor {
        let inner = s.find('(').map(|i| &s[i + 1..]).unwrap_or(s);
        let inner = inner.trim_end_matches(')').trim();
        let mut nums = inner.split(',').map(|n| {
            let n = n.trim();
            if n.ends_with('%') {
                let pct: f64 = n.trim_end_matches('%').trim().parse().unwrap_or(0.0);
                (pct * 2.55) as u8
            } else {
                n.parse::<f64>().unwrap_or(0.0) as u8
            }
        });
        CssColor {
            r: nums.next().unwrap_or(0),
            g: nums.next().unwrap_or(0),
            b: nums.next().unwrap_or(0),
            a: nums.next().unwrap_or(255),
        }
    }
}

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "benches/grammars/css-fast.bbnf", arena)]
struct CssFastParser;

#[derive(Parser)]
#[parser(path = "benches/grammars/css-fast.bbnf", span)]
struct CssFastSpanParser;

#[derive(Parser)]
#[parser(path = "benches/grammars/css-semantic.bbnf", arena)]
struct CssSemanticParser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/css-stylesheet.bbnf", skip_recover, arena)]
struct CssImportParser;

fn load_css(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

// ── Arena (cold per-parse) ───────────────────────────────────────────

macro_rules! bench_arena {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (bytes, consumed_pct) = {
                let arena = BumpArena::<CssFastParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet_arena();
                let (_result, state) = parser.parse_return_state_with_context(&input, &arena);
                (state.offset as u64, state.offset * 100 / input.len().max(1))
            };
            assert!(
                consumed_pct >= 95,
                concat!($file, ": fast arena parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = bytes;
            b.iter(|| {
                let arena = BumpArena::<CssFastParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_arena!(arena_normalize, "normalize.css");
bench_arena!(arena_bootstrap, "bootstrap.css");
bench_arena!(arena_tailwind, "tailwind.css");

// ── Span (cold per-parse, zero allocations) ──────────────────────────

macro_rules! bench_span {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (bytes, consumed_pct) = {
                let parser = CssFastSpanParser::stylesheet_span();
                let (_result, state) = parser.parse_return_state(&input);
                (state.offset as u64, state.offset * 100 / input.len().max(1))
            };
            assert!(
                consumed_pct >= 95,
                concat!($file, ": span-only parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = bytes;
            b.iter(|| {
                let parser = CssFastSpanParser::stylesheet_span();
                let result = parser.parse(black_box(&input)).unwrap();
                black_box(&result as *const _);
            });
        }
    };
}

bench_span!(span_normalize, "normalize.css");
bench_span!(span_bootstrap, "bootstrap.css");
bench_span!(span_tailwind, "tailwind.css");

// ── Semantic (cold per-parse, numbers → f64 during parsing) ──────────────

macro_rules! bench_semantic {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (bytes, consumed_pct) = {
                let arena = BumpArena::<CssSemanticParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssSemanticParser::stylesheet_arena();
                let (_result, state) = parser.parse_return_state_with_context(&input, &arena);
                (state.offset as u64, state.offset * 100 / input.len().max(1))
            };
            assert!(
                consumed_pct >= 95,
                concat!($file, ": semantic arena parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = bytes;
            b.iter(|| {
                let arena = BumpArena::<CssSemanticParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssSemanticParser::stylesheet_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_semantic!(semantic_normalize, "normalize.css");
bench_semantic!(semantic_bootstrap, "bootstrap.css");
bench_semantic!(semantic_tailwind, "tailwind.css");

// ── L4 (cold per-parse, full CSS L4 spec via @import composition) ──

macro_rules! bench_l4 {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (bytes, _consumed_pct) = {
                let arena = BumpArena::<CssImportParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssImportParser::stylesheet_arena();
                let (_result, state) = parser.parse_return_state_with_context(&input, &arena);
                let pct = state.offset * 100 / input.len().max(1);
                if pct < 95 {
                    let f = state.furthest_offset;
                    let around = &input[f.saturating_sub(10)..(f+30).min(input.len())];
                    panic!(
                        "{}: l4 consumed {}% (offset {}, furthest {} = '{}')",
                        $file, pct, state.offset, f, around
                    );
                }
                (state.offset as u64, pct)
            };
            b.bytes = bytes;
            b.iter(|| {
                let arena = BumpArena::<CssImportParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssImportParser::stylesheet_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_l4!(l4_normalize, "normalize.css");
bench_l4!(l4_bootstrap, "bootstrap.css");
bench_l4!(l4_tailwind, "tailwind.css");

// ── VM tier (bytecode interpreter) ─────────────────────────────────────────

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;

fn compiled_css_vm() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar = std::fs::read_to_string("benches/grammars/css-fast.bbnf")
        .expect("failed to read css-fast.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

macro_rules! bench_vm {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (_ir, program) = compiled_css_vm();
            {
                let mut interp = Interpreter::new(&program, &input);
                let r = interp.run();
                let consumed_pct = r.offset as usize * 100 / input.len().max(1);
                assert!(
                    r.success && consumed_pct >= 95,
                    concat!($file, ": VM consumed only {}% (offset {})"),
                    consumed_pct, r.offset
                );
            }
            b.bytes = input.len() as u64;
            b.iter(|| {
                let mut interp = Interpreter::new(&program, black_box(&input));
                let r = interp.run();
                black_box(r.offset);
            });
        }
    };
}

bench_vm!(vm_normalize, "normalize.css");
bench_vm!(vm_bootstrap, "bootstrap.css");
bench_vm!(vm_tailwind, "tailwind.css");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(arena, arena_normalize, arena_bootstrap, arena_tailwind);
benchmark_group!(span, span_normalize, span_bootstrap, span_tailwind);
benchmark_group!(semantic, semantic_normalize, semantic_bootstrap, semantic_tailwind);
benchmark_group!(l4, l4_normalize, l4_bootstrap, l4_tailwind);
benchmark_group!(vm, vm_normalize, vm_bootstrap, vm_tailwind);

benchmark_main!(arena, span, semantic, l4, vm);
