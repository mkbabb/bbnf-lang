//! AV.0.4 — CSS L4 namedColor typed-payload parity test.
//!
//! Drives the full 150-branch namedColor alt from the grammar file:
//! every `"name" -> 0xRRGGBBAAu32` mapping must materialise a leaf
//! whose inline u32 payload equals the declared RGBA constant.

use std::path::PathBuf;

// Host function `parse_hex_color` referenced by
// `grammar/css/l4/color.bbnf` lives in `bbnf::css_types`. The pre-AZ-III
// in-test `mod css_types {…}` shadow is retired per REAUDIT-2026-04-30
// lane 3 §5.1 (single source of truth).
#[allow(unused_imports)]
use bbnf::css_types as _css_types_link;

use ::bbnf::grammar::generated::css_l4::*;

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
            + name_start
            + 1;
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

/// AZ-I.W2-act.close B3 — locate a `namedColor` typed leaf in the
/// parsed [`CssDocument`] graph and return its packed u32 payload.
///
/// Pre-W2-act.B3 the named-color test walked the tape and matched on
/// the IR rule's `variant_idx` (low byte of `rule.id`); post-W2-act.B3
/// the typed-graph walk inspects every `CssTypedValue::Color` for the
/// `CssColor::Hex` variant carrying the projected 32-bit packed RGBA.
/// The struct-direct CSS L4 builder routes both `hex` and
/// `namedColor` through [`bbnf::runtime::css_l4::CssColor::Hex`] (the
/// builder's `push_leaf_with_u64` admission), so this helper finds
/// the colour-carrying declaration regardless of which projection
/// rule produced it.
fn find_named_color_payload(input: &str) -> Option<u32> {
    use ::bbnf::runtime::css_l4::{CssColor, CssTypedValue};

    let doc = CssL4Parser::parse(input).ok()?;
    for (_property, value) in doc.walk_values() {
        if let CssTypedValue::Color(CssColor::Hex(packed)) = value {
            return Some(*packed);
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

    let mut failed: Vec<(String, u32, Option<u32>)> = Vec::new();
    for (name, expected) in &colors {
        // AW.0.8: the `white = 0xFFFFFFFFu32` sentinel collision is
        // now resolved via `PayloadData::WideScalar` routing. Every
        // named color — including white — must materialise.
        let input = format!("a {{ color: {name}; }}");
        let got = find_named_color_payload(&input);
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

/// AW.0.8 hard gate: `color: white` materialises the `0xFFFFFFFFu32`
/// payload instead of colliding with the `TapeOffset::NONE` sentinel.
///
/// The classic bug was `PayloadData::InlineScalar(u32::MAX)` being
/// indistinguishable from payload-absent (`u32::MAX == TapeOffset::
/// NONE`). The W0b fix promotes `u32` payloads whose rule range could
/// reach the sentinel to `PayloadData::WideScalar`, where the 8-byte
/// slot cannot collide with the 4-byte sentinel.
///
/// Post-W2-act.close B3 the typed-graph walk projects directly to
/// [`bbnf::runtime::css_l4::CssColor::Hex`]; the sentinel collision is
/// architecturally retired (no payload column means no `u32::MAX` /
/// `TapeOffset::NONE` overlap). The test still pins the parity:
/// `white` must materialise as `0xFFFFFFFF` in the typed value graph.
#[test]
fn white_materialises() {
    let got = find_named_color_payload("a { color: white; }");
    assert_eq!(
        got,
        Some(0xFFFFFFFFu32),
        "white must decode as 0xFFFFFFFFu32 in the typed CSS value graph"
    );
}
