
//! BBNF CSS L4 typed slab benchmark — cold per-parse (l4/stylesheet.bbnf).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;

/// Semantic CSS value types for TypeDesc-driven value materialization.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(s: &str) -> u32 {
        let hex = s.as_bytes();
        match hex.len() {
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

    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum AngleUnit {
        Deg = 0,
        Rad = 1,
        Grad = 2,
        Turn = 3,
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum TimeUnit {
        Ms = 0,
        S = 1,
    }

    #[derive(Debug, Clone, Copy)]
    pub struct CssColor {
        pub r: u8,
        pub g: u8,
        pub b: u8,
        pub a: u8,
    }

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

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

fn load(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let parsed = CssL4Parser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                divan::black_box(&parsed);
            }
            bench_with_timeout(
                b,
                limits::CSS_TAILWIND_PARSE,
                |input: String| {
                    let parsed = CssL4Parser::parse(divan::black_box(&input)).unwrap();
                    divan::black_box(parsed);
                },
                &input,
            );
        }
    };
}

bench!(normalize, "normalize.css");
bench!(bootstrap, "bootstrap.css");
bench!(tailwind, "tailwind.css");

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
