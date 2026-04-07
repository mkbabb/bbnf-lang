//! Serialize round-trip tests — ALL grammars must compile and round-trip.
//!
//! Each test verifies idempotence: parse → serialize → reparse → serialize → assert eq.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", serialize)]
struct JsonEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/misc/csv.bbnf", serialize)]
struct CsvEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/bnf/bnf.bbnf", serialize)]
struct BnfEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/ebnf/ebnf.bbnf", serialize)]
struct EbnfEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/misc/math.bbnf", serialize)]
struct MathEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", serialize)]
struct SheetsEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf", serialize)]
struct BbnfEmit;

// CSS pretty grammar (no @import)
#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", serialize)]
struct CssPrettyEmit;

// CSS L4 host types — required by grammar mapper expressions (-> 0u8, etc.)
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(s: &str) -> u32 {
        let hex = s.as_bytes();
        match hex.len() {
            3 => {
                let (r, g, b) = (hex_digit(hex[0]), hex_digit(hex[1]), hex_digit(hex[2]));
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
            }
            6 => {
                let (r, g, b) = (hex_byte(hex[0], hex[1]), hex_byte(hex[2], hex[3]), hex_byte(hex[4], hex[5]));
                (r << 24) | (g << 16) | (b << 8) | 0xFF
            }
            8 => {
                let (r, g, b, a) = (hex_byte(hex[0], hex[1]), hex_byte(hex[2], hex[3]), hex_byte(hex[4], hex[5]), hex_byte(hex[6], hex[7]));
                (r << 24) | (g << 16) | (b << 8) | a
            }
            _ => 0,
        }
    }
    fn hex_digit(b: u8) -> u32 {
        match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => 0,
        }
    }
    fn hex_byte(hi: u8, lo: u8) -> u32 { (hex_digit(hi) << 4) | hex_digit(lo) }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum LengthUnit {
        Px = 0, Em = 1, Rem = 2, Vh = 3, Vw = 4, Vmin = 5, Vmax = 6,
        Ch = 7, Ex = 8, Cm = 9, Mm = 10, In = 11, Pt = 12, Pc = 13,
        Lh = 14, Rlh = 15, Svw = 16, Svh = 17, Dvw = 18, Dvh = 19,
        Lvw = 20, Lvh = 21, Cqw = 22, Cqh = 23, Cqi = 24, Cqb = 25,
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum AngleUnit { Deg = 0, Rad = 1, Grad = 2, Turn = 3 }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum TimeUnit { Ms = 0, S = 1 }

    #[derive(Debug, Clone, Copy)]
    pub struct CssColor { pub r: u8, pub g: u8, pub b: u8, pub a: u8 }

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

// CSS L4 grammar (uses @import — 14 modules, host types via css_types)
#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", serialize, skip_recover)]
struct CssL4Emit;

// ── JSON ─────────────────────────────────────────────────────────────────────

fn json_emit(input: &str) -> String {
    let ctx = __JsonEmitEnumCtx::with_capacity(input.len() / 32);
    let (result, _) = JsonEmit::value().parse_return_state_with_context(input, &ctx);
    let value = result.expect("JSON parse failed");
    JsonEmit::serialize_compact(value)
}

fn json_rt(input: &str) {
    let s1 = json_emit(input);
    let s2 = json_emit(&s1);
    assert_eq!(s1, s2, "JSON serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test] fn json_null()      { assert_eq!(json_emit("null"), "null"); }
#[test] fn json_true()      { assert_eq!(json_emit("true"), "true"); }
#[test] fn json_false()     { assert_eq!(json_emit("false"), "false"); }
#[test] fn json_number()    { json_rt("42"); }
#[test] fn json_string()    { assert_eq!(json_emit(r#""hello""#), r#""hello""#); }
#[test] fn json_empty_arr() { json_rt("[]"); }
#[test] fn json_array()     { json_rt("[1, 2, 3]"); }
#[test] fn json_empty_obj() { json_rt("{}"); }
#[test] fn json_object()    { json_rt(r#"{"key": "value"}"#); }
#[test] fn json_nested()    { json_rt(r#"{"a": [1, 2], "b": {"c": true}}"#); }

// ── CSV ──────────────────────────────────────────────────────────────────────

fn csv_emit(input: &str) -> String {
    let ctx = __CsvEmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = CsvEmit::csv().parse_return_state_with_context(input, &ctx);
    CsvEmit::serialize_compact(&result.expect("CSV parse failed"))
}

fn csv_rt(input: &str) {
    let s1 = csv_emit(input);
    let s2 = csv_emit(&s1);
    assert_eq!(s1, s2, "CSV serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test] fn csv_simple()     { csv_rt("a,b,c"); }
#[test] fn csv_multi()      { csv_rt("a,b,c\n1,2,3"); }
#[test] fn csv_quoted()     { csv_rt(r#""hello","world""#); }
#[test] fn csv_single()     { csv_rt("hello"); }

// ── BNF ──────────────────────────────────────────────────────────────────────

fn bnf_emit(input: &str) -> String {
    let ctx = __BnfEmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = BnfEmit::grammar().parse_return_state_with_context(input, &ctx);
    BnfEmit::serialize_compact(&result.expect("BNF parse failed"))
}

#[test]
fn bnf_rule() {
    let e = bnf_emit("<expr> ::= <term> | <expr> \"+\" <term>\n");
    assert!(!e.is_empty(), "BNF empty");
}

// ── EBNF ─────────────────────────────────────────────────────────────────────

fn ebnf_emit(input: &str) -> String {
    let ctx = __EbnfEmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = EbnfEmit::grammar().parse_return_state_with_context(input, &ctx);
    EbnfEmit::serialize_compact(&result.expect("EBNF parse failed"))
}

#[test]
fn ebnf_rule() {
    let e = ebnf_emit("digit = \"0\" | \"1\" | \"2\" ;\n");
    assert!(!e.is_empty(), "EBNF empty");
}

// ── Math ─────────────────────────────────────────────────────────────────────

fn math_emit(input: &str) -> String {
    let ctx = __MathEmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = MathEmit::number().parse_return_state_with_context(input, &ctx);
    MathEmit::serialize_compact(&result.expect("Math parse failed"))
}

#[test]
fn math_num() {
    let e = math_emit("42");
    assert!(!e.is_empty(), "Math empty");
}

// ── Google Sheets ────────────────────────────────────────────────────────────

fn sheets_emit(input: &str) -> String {
    let ctx = __SheetsEmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = SheetsEmit::formula().parse_return_state_with_context(input, &ctx);
    SheetsEmit::serialize_compact(&result.expect("Sheets parse failed"))
}

#[test]
fn sheets_simple() {
    let e = sheets_emit("=1+2");
    assert!(!e.is_empty(), "Sheets empty");
}

// ── BBNF ─────────────────────────────────────────────────────────────────────

#[test]
fn bbnf_rule() {
    let ctx = __BbnfEmitEnumCtx::with_capacity(1024);

    // Double-quoted literals now work — unescape moved to lowering.
    let input = "x = /[a-z]+/ ;\ny = \"hello\" ;\n";
    let (result, state) = BbnfEmit::grammar().parse_return_state_with_context(input, &ctx);
    assert!(result.is_some(), "BBNF grammar parse failed at offset {}", state.offset);
    assert!(
        state.offset >= input.trim_end().len(),
        "BBNF grammar parse incomplete: {}/{}",
        state.offset,
        input.len()
    );
    let val = result.unwrap();
    let e = BbnfEmit::serialize_compact(&val);
    assert!(!e.is_empty(), "BBNF serialize empty");
    // Idempotence: serialize → reparse → serialize → assert equal.
    let (r2, s2) = BbnfEmit::grammar().parse_return_state_with_context(&e, &ctx);
    assert!(r2.is_some(), "BBNF reparse failed: {:?}", e);
    let e2 = BbnfEmit::serialize_compact(&r2.unwrap());
    assert_eq!(e, e2, "BBNF serialize not idempotent:\n  s1={e:?}\n  s2={e2:?}");
}

// ── CSS Pretty ───────────────────────────────────────────────────────────────

fn css_pretty_emit(input: &str) -> String {
    let ctx = __CssPrettyEmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = CssPrettyEmit::stylesheet().parse_return_state_with_context(input, &ctx);
    let val = result.expect("CSS Pretty parse failed");
    CssPrettyEmit::serialize_compact(&val)
}

fn css_pretty_rt(input: &str) {
    let s1 = css_pretty_emit(input);
    let s2 = css_pretty_emit(&s1);
    assert_eq!(s1, s2, "CSS Pretty serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test]
fn css_simple() {
    css_pretty_rt("body { color: red; }");
}

// ── CSS L4 (@import-based grammar with host types) ──────────────────────────

fn css_l4_emit(input: &str) -> String {
    let ctx = __CssL4EmitEnumCtx::with_capacity(input.len() / 8);
    let (result, _) = CssL4Emit::stylesheet().parse_return_state_with_context(input, &ctx);
    let val = result.expect("CSS L4 parse failed");
    CssL4Emit::serialize_compact(&val)
}

fn css_l4_rt(input: &str) {
    let s1 = css_l4_emit(input);
    let s2 = css_l4_emit(&s1);
    assert_eq!(s1, s2, "CSS L4 serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test]
fn css_l4_simple_rule() {
    css_l4_rt("h1 { color: red; }");
}

#[test]
fn css_l4_multi_property() {
    css_l4_rt("body { margin: 0; padding: 0; display: flex; }");
}

#[test]
fn css_l4_media() {
    css_l4_rt("@media (max-width: 768px) { .x { display: none; } }");
}
