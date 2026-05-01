//! Integration tests for CSS L4 typed parsing through the tape-first
//! `the proc-macro derive (retired B2)` codegen path using the l4/stylesheet.bbnf grammar.

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

use ::bbnf::grammar::generated::css_l4::*;

fn load_css(name: &str) -> String {
    let candidates = [
        format!("../../data/css/{}", name),
        format!("../data/css/{}", name),
    ];
    for path in &candidates {
        if let Ok(contents) = std::fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find data file '{}'; tried: {:?}",
        name, candidates
    );
}

fn parse_full(input: &str) -> bool {
    // The tape-first parser enforces full input consumption inside
    // `parse()`, so a successful `Ok` implies the old "offset reached
    // end" invariant.
    CssL4Parser::parse(input).is_ok()
}

// ---------------------------------------------------------------------------
// Unit value tests
// ---------------------------------------------------------------------------

#[test]
fn unit_px() {
    assert!(parse_full("a { x: 100px; }"), "100px should parse");
}

#[test]
fn unit_percent() {
    assert!(parse_full("a { x: 100%; }"), "100% should parse");
}

#[test]
fn unit_deg() {
    assert!(parse_full("a { x: 100deg; }"), "100deg should parse");
}

#[test]
fn unit_ms() {
    assert!(parse_full("a { x: 100ms; }"), "100ms should parse");
}

#[test]
fn unit_em() {
    assert!(parse_full("a { x: 100em; }"), "100em should parse");
}

#[test]
fn unit_rem() {
    assert!(parse_full("a { x: 100rem; }"), "100rem should parse");
}

#[test]
fn unit_fr() {
    assert!(parse_full("a { x: 100fr; }"), "100fr should parse");
}

// ---------------------------------------------------------------------------
// var() / calc() tests
// ---------------------------------------------------------------------------

#[test]
fn calc_expression() {
    assert!(
        parse_full("body { width: calc(100% - 20px); }"),
        "calc() should parse"
    );
}

#[test]
fn var_simple() {
    assert!(
        parse_full("body { color: var(--x); }"),
        "var(--x) should parse"
    );
}

#[test]
fn var_with_fallback() {
    assert!(
        parse_full("body { color: var(--x, red); }"),
        "var(--x, red) should parse"
    );
}

// ---------------------------------------------------------------------------
// @media test
// ---------------------------------------------------------------------------

#[test]
fn media_query() {
    assert!(
        parse_full("@media (min-width: 768px) { body { margin: 0; } }"),
        "@media query should parse"
    );
}

// ---------------------------------------------------------------------------
// Vendor-prefix and multi-rule isolation
// ---------------------------------------------------------------------------

#[test]
fn vendor_prefix_property() {
    assert!(
        parse_full("html { -webkit-text-size-adjust: 100%; }"),
        "-webkit vendor prefix property should parse"
    );
}

#[test]
fn two_rules_sequential() {
    assert!(
        parse_full("html { margin: 0; }\nbody { margin: 0; }"),
        "two sequential rules should parse"
    );
}

// ---------------------------------------------------------------------------
// Full file tests
// ---------------------------------------------------------------------------

#[test]
fn parse_normalize_css() {
    let input = load_css("normalize.css");
    assert!(
        parse_full(&input),
        "normalize.css: parse failed or incomplete"
    );
}

#[test]
fn parse_bootstrap_css() {
    // bootstrap.css requires a large stack due to deep recursion in funcBody.
    let input = load_css("bootstrap.css");
    let child = std::thread::Builder::new()
        .name("bootstrap-parse".into())
        .stack_size(67_108_864) // 64 MiB
        .spawn(move || {
            assert!(
                parse_full(&input),
                "bootstrap.css: parse failed or incomplete"
            );
        })
        .expect("failed to spawn thread");
    child.join().expect("bootstrap parse thread panicked");
}

// ---------------------------------------------------------------------------
// AU.2.4 — Typed hex colour round-trip (W2 agent B)
//
// Asserts that the aggregate u32 payload produced by the tape-first
// emitter round-trips through the parsed tape: the matched span is
// the hex digits and the payload bytes decode back to the
// lightningcss-equivalent 0xRRGGBBAA u32.
// ---------------------------------------------------------------------------

/// AZ-I.W2-act.close B3 — walk the parsed [`CssDocument`] for a
/// hex-colour typed leaf, returning the decoded u32 payload when it
/// matches `target`. Returns `None` when the parse fails or no
/// matching `CssColor::Hex` (the variant `parse_hex_color`'s u32
/// projection lands on per the struct-direct builder) appears in the
/// document's typed value graph.
///
/// The post-W2-act.B3 builder routes `hex = "#" , /…/ -> u32`
/// through [`bbnf::runtime::css_l4::CssTypedValue::Color`] /
/// [`bbnf::runtime::css_l4::CssColor::Hex`]; the typed walk is more
/// direct than the pre-W2-act `payload_bytes(rec, 4)` tape inspection
/// because the value IS the parse output.
fn find_hex_payload_u32(css_input: &str, target: u32) -> Option<u32> {
    use ::bbnf::runtime::css_l4::{CssColor, CssTypedValue};

    let doc = CssL4Parser::parse(css_input).ok()?;
    for (_property, value) in doc.walk_values() {
        if let CssTypedValue::Color(CssColor::Hex(packed)) = value {
            if *packed == target {
                return Some(*packed);
            }
        }
    }
    None
}

#[test]
fn hex_color_roundtrip_6digit() {
    let target: u32 = 0xFF00_FFFF;
    let u32_val = find_hex_payload_u32("a { color: #FF00FF; }", target)
        .expect("hex #FF00FF must materialise as a 4-byte KvPair payload");
    assert_eq!(u32_val, target);
}

#[test]
fn hex_color_roundtrip_3digit() {
    // #abc -> r=0xaa, g=0xbb, b=0xcc, a=0xff = 0xaabbccff
    let target: u32 = 0xAABB_CCFF;
    let u32_val = find_hex_payload_u32("a { color: #abc; }", target)
        .expect("hex #abc must expand to 0xAABBCCFF in a 4-byte KvPair payload");
    assert_eq!(u32_val, target);
}

#[test]
fn hex_color_roundtrip_8digit() {
    let target: u32 = 0x1234_5678;
    let u32_val = find_hex_payload_u32("a { color: #12345678; }", target)
        .expect("hex #12345678 must materialise as a 4-byte KvPair payload");
    assert_eq!(u32_val, target);
}
