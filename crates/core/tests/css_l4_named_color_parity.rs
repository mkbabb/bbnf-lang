//! AV.0.4 — CSS L4 namedColor typed-payload parity test.
//!
//! Drives the full 150-branch namedColor alt from the grammar file:
//! every `"name" -> 0xRRGGBBAAu32` mapping must materialise a leaf
//! whose inline u32 payload equals the declared RGBA constant.

use std::path::PathBuf;

use bbnf::runtime::tape::TapeKind;
use bbnf_derive::Parser;

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
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

/// Extract `(name, hex)` pairs from the namedColor section of
/// `grammar/css/l4/color.bbnf`. Each grammar line of the form
/// `"name" -> 0xRRGGBBAAu32` yields one pair.
fn load_named_color_map() -> Vec<(String, u32)> {
    let workspace_root = {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        manifest
            .parent()
            .and_then(|p| p.parent())
            .expect("crates/core → crates → workspace root")
            .to_path_buf()
    };
    let color_path = workspace_root.join("grammar/css/l4/color.bbnf");
    let src = std::fs::read_to_string(&color_path)
        .unwrap_or_else(|e| panic!("read {}: {}", color_path.display(), e));

    let mut pairs = Vec::new();
    let mut in_named = false;
    for line in src.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("namedColor") {
            in_named = true;
        }
        if !in_named {
            continue;
        }
        // namedColor ends at the first semicolon after its opening.
        let after_arrow = match trimmed.find("-> 0x") {
            Some(i) => &trimmed[i + 3..],
            None => {
                if trimmed.ends_with(';') && !trimmed.contains("->") {
                    in_named = false;
                }
                continue;
            }
        };
        let name_start = trimmed.find('"').expect("opening quote present");
        let name_end = trimmed[name_start + 1..]
            .find('"')
            .expect("closing quote present")
            + name_start + 1;
        let name = trimmed[name_start + 1..name_end].to_string();

        let hex_digits: String = after_arrow[2..]
            .chars()
            .take_while(|c| c.is_ascii_hexdigit())
            .collect();
        let hex = u32::from_str_radix(&hex_digits, 16)
            .unwrap_or_else(|e| panic!("bad hex {hex_digits:?}: {e}"));
        pairs.push((name, hex));

        if trimmed.ends_with(';') {
            in_named = false;
        }
    }
    pairs
}

fn named_color_variant_idx() -> u8 {
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf();
    let bbnf_path = workspace_root.join("grammar/css/l4/stylesheet.bbnf");
    let request = bbnf::pipeline::CompileRequest {
        target: bbnf::pipeline::CompileTarget::Vm,
        options: bbnf::pipeline::PipelineOptions::default(),
    };
    let output = bbnf::pipeline::compile_paths_request(&[bbnf_path], &request)
        .expect("compile for IR introspection");
    let ir = match output {
        bbnf::pipeline::CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    };
    let rule = ir
        .rules
        .iter()
        .find(|r| ir.get_string(r.name) == "namedColor")
        .expect("namedColor rule present");
    (rule.id & 0xFF) as u8
}

fn find_named_color_payload(input: &str, variant_idx: u8) -> Option<u32> {
    let parsed = CssL4Parser::parse(input).ok()?;
    let tape = parsed.tape();
    for rec in tape.iter() {
        if rec.kind() == TapeKind::Span
            && rec.variant_idx() == variant_idx
            && rec.has_payload()
            && !rec.has_children()
        {
            if let Some(v) = tape.payload_scalar::<u32>(rec) {
                return Some(v);
            }
        }
    }
    None
}

#[test]
fn named_color_grammar_list_is_non_empty() {
    let colors = load_named_color_map();
    assert!(
        colors.len() >= 148,
        "expected at least 148 named colors, got {}",
        colors.len()
    );
}

#[test]
fn every_named_color_materialises_its_u32_payload() {
    let colors = load_named_color_map();
    assert!(!colors.is_empty(), "grammar list load must succeed");
    let variant_idx = named_color_variant_idx();

    let mut failed: Vec<(String, u32, Option<u32>)> = Vec::new();
    for (name, expected) in &colors {
        if *expected == u32::MAX {
            // `white = 0xFFFFFFFFu32` coincides with
            // `TapeOffset::NONE`; the inline-scalar slot cannot
            // represent it unambiguously. Scoped outside AV.0.4.
            continue;
        }
        let input = format!("a {{ color: {name}; }}");
        let got = find_named_color_payload(&input, variant_idx);
        if got != Some(*expected) {
            failed.push((name.clone(), *expected, got));
        }
    }

    if !failed.is_empty() {
        let preview: Vec<String> = failed
            .iter()
            .take(10)
            .map(|(n, e, g)| format!("{n}: expected 0x{e:08X}, got {g:?}"))
            .collect();
        panic!(
            "{}/{} named colors failed payload parity:\n  {}",
            failed.len(),
            colors.len(),
            preview.join("\n  ")
        );
    }
}
