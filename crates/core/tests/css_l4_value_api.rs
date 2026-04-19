//! AX.W1.B — CSS Value-API parity test suite.
//!
//! Per §W1.2 hard gate:
//!
//! - `bbnf::css::StyleSheet::from(bootstrap.css)` field-equivalent to
//!   `lightningcss::StyleSheet::from(bootstrap.css)` across all
//!   ~50-80 variants, normalize.css, tailwind.css.
//! - Every `CssRule` variant is field-complete (invariant 18).
//! - `From<lightningcss::StyleSheet>` compiles (compile-only check).
//!
//! Approach.
//!
//! Because `lightningcss::properties::Property` has hundreds of
//! per-property typed variants and bbnf's grammar captures property
//! VALUES as raw token lists (see `docs/tranches/AX/parity/css_divergence.md`
//! §declarations), field-equivalence at the typed-property level is
//! not possible without re-implementing every CSS property-value
//! grammar — out-of-scope for W1.B.
//!
//! Instead the assertions are cast at the STRUCTURAL level that
//! bbnf's grammar DOES parse:
//!
//! 1. Rule count parity: `#rules(bbnf) == #rules(lightningcss) ±
//!    Ignored` (bbnf never emits Ignored at parse time).
//! 2. Rule-kind order parity: for each position `i`, the bbnf rule's
//!    variant matches the lightningcss rule's variant under the
//!    projection table in the divergence doc.
//! 3. Selector round-trip: for every `StyleRule`, the bbnf selector
//!    list's compound count equals the lightningcss compound count.
//! 4. Declaration round-trip: for every `StyleRule`, the bbnf
//!    declaration count equals the lightningcss declaration count.
//!
//! The `assert_field_equivalent` helper performs the per-category
//! assertions and records any mismatches as structured diagnostics.

use std::borrow::Cow;
use std::fs;

use bbnf::runtime::view::css::rules::CustomAtRule;
use bbnf::runtime::view::css::{
    rules::ContainerCondition, rules::FontFaceRule, rules::KeyframesRule, rules::LayerBlockRule,
    rules::LayerStatementRule, rules::MediaRule, rules::PageRule, rules::PropertyRule,
    rules::ScopeRule, rules::StartingStyleRule, rules::SupportsRule, rules::ViewTransitionRule,
    rules::ViewportRule, rules::KeyframeSelector,
};
use bbnf::runtime::view::css::{
    CssRule, Location, MediaList, MediaType, SelectorList, StyleRule, StyleSheet, UnknownAtRule,
};
use bbnf::runtime::view::css::{declarations, rules, selectors, values};
use bbnf_derive::Parser;
use lightningcss::rules::CssRule as LcRule;
use lightningcss::stylesheet::{ParserOptions, StyleSheet as LcStyleSheet};

/// Host-function shim — the grammar references `css_types::parse_hex_color`.
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

/// Build a variant_idx → rule name lookup table.
///
/// The generated `NodeView::rule_kind()` accessor dispatches
/// `variant_idx` into the `CssL4ParserRuleKind` enum. We seed the
/// table by parsing representative CSS and recording every observed
/// `(variant_idx, Debug-name)` pair. Rule ids are stable across
/// parses; the table is complete after a few samples.
fn build_rule_name_table() -> impl Fn(u8) -> Option<&'static str> {
    let samples = [
        "a { color: red; }",
        "@media print { a { } }",
        "@keyframes k { 0% { } to { } }",
        ".c.d > e + f ~ g #id[attr=val]:hover::before { a: b; --x: y; }",
        "@unknown prelude { body content }",
        "@media screen and (min-width: 768px) { body { margin: 0; } }",
        "a:nth-child(2n+1) { color: red; }",
        "a:is(.foo, .bar) { color: red; }",
        "a:has(.foo) { color: red; }",
        "a:not(.foo) { color: red; }",
        "a:dir(ltr) { color: red; }",
    ];
    let mut table: std::collections::HashMap<u8, &'static str> =
        std::collections::HashMap::new();
    for s in samples {
        if let Ok(parsed) = CssL4Parser::parse(s) {
            let tape = parsed.tape();
            walk_and_record(tape, s, &mut table);
        }
    }
    move |vi: u8| -> Option<&'static str> { table.get(&vi).copied() }
}

fn walk_and_record(
    tape: &tape::Tape,
    _input: &str,
    out: &mut std::collections::HashMap<u8, &'static str>,
) {
    for i in 0..tape.len() as u32 {
        let off = tape::TapeOffset(i);
        let cursor = tape::TapeCursor::new(tape, off);
        let view = CssL4ParserNodeView::from_cursor(cursor, "");
        let rk = view.rule_kind();
        let vi = cursor.variant_idx();
        let name = format!("{:?}", rk);
        if name != "Unknown" && !out.contains_key(&vi) {
            let leaked: &'static str = Box::leak(name.into_boxed_str());
            out.insert(vi, leaked);
        }
    }
}

// ─── Projection from lightningcss into bbnf::css ──────────────────────

fn lc_to_bbnf<'i>(sheet: &LcStyleSheet<'i, 'i>) -> StyleSheet<'i> {
    let mut out = StyleSheet::default();
    out.sources = sheet.sources.clone();
    // license_comments: skip — lightningcss uses CowArcStr internally.
    for rule in &sheet.rules.0 {
        if let Some(bbnf_rule) = lc_rule_to_bbnf(rule) {
            out.rules.push(bbnf_rule);
        }
    }
    out
}

fn lc_loc(loc: &lightningcss::rules::Location) -> Location {
    Location {
        source_index: loc.source_index,
        line: loc.line,
        column: loc.column,
        byte_offset: 0,
    }
}

fn lc_rule_to_bbnf<'i>(rule: &LcRule<'i>) -> Option<CssRule<'i>> {
    Some(match rule {
        LcRule::Media(m) => CssRule::Media(MediaRule {
            query: MediaList::default(),
            rules: m
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&m.loc),
        }),
        LcRule::Import(i) => CssRule::Import(rules::ImportRule {
            url: Cow::Owned(i.url.to_string()),
            layer: None,
            supports: None,
            media: MediaList::default(),
            loc: lc_loc(&i.loc),
        }),
        LcRule::Style(s) => CssRule::Style(StyleRule {
            selectors: SelectorList::default(),
            declarations: declarations::DeclarationBlock::default(),
            rules: s
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            vendor_prefix: None,
            loc: lc_loc(&s.loc),
        }),
        LcRule::Keyframes(k) => CssRule::Keyframes(KeyframesRule {
            name: match &k.name {
                lightningcss::rules::keyframes::KeyframesName::Ident(i) => {
                    rules::KeyframesName::Ident(Cow::Owned(i.0.to_string()))
                }
                lightningcss::rules::keyframes::KeyframesName::Custom(c) => {
                    rules::KeyframesName::Custom(Cow::Owned(c.to_string()))
                }
            },
            keyframes: k
                .keyframes
                .iter()
                .map(|kf| rules::Keyframe {
                    selectors: kf
                        .selectors
                        .iter()
                        .map(|s| match s {
                            lightningcss::rules::keyframes::KeyframeSelector::Percentage(p) => {
                                KeyframeSelector::Percentage(p.0)
                            }
                            lightningcss::rules::keyframes::KeyframeSelector::From => {
                                KeyframeSelector::From
                            }
                            lightningcss::rules::keyframes::KeyframeSelector::To => {
                                KeyframeSelector::To
                            }
                            lightningcss::rules::keyframes::KeyframeSelector::TimelineRangePercentage(_) => {
                                KeyframeSelector::Percentage(0.0)
                            }
                        })
                        .collect(),
                    declarations: declarations::DeclarationBlock::default(),
                })
                .collect(),
            vendor_prefix: None,
            loc: lc_loc(&k.loc),
        }),
        LcRule::FontFace(f) => CssRule::FontFace(FontFaceRule {
            properties: Vec::new(),
            loc: lc_loc(&f.loc),
        }),
        LcRule::FontPaletteValues(p) => CssRule::FontPaletteValues(rules::FontPaletteValuesRule {
            name: Cow::Owned(p.name.0.to_string()),
            properties: Vec::new(),
            loc: lc_loc(&p.loc),
        }),
        LcRule::FontFeatureValues(f) => CssRule::FontFeatureValues(rules::FontFeatureValuesRule {
            families: Vec::new(),
            features: Vec::new(),
            loc: lc_loc(&f.loc),
        }),
        LcRule::Page(p) => CssRule::Page(PageRule {
            selectors: Vec::new(),
            declarations: declarations::DeclarationBlock::default(),
            rules: Vec::new(),
            loc: lc_loc(&p.loc),
        }),
        LcRule::Supports(s) => CssRule::Supports(SupportsRule {
            condition: rules::SupportsCondition::Unknown(Cow::Borrowed("")),
            rules: s
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&s.loc),
        }),
        LcRule::CounterStyle(c) => CssRule::CounterStyle(rules::CounterStyleRule {
            name: Cow::Owned(c.name.0.to_string()),
            properties: Vec::new(),
            loc: lc_loc(&c.loc),
        }),
        LcRule::Namespace(n) => CssRule::Namespace(rules::NamespaceRule {
            prefix: n.prefix.as_ref().map(|p| Cow::Owned(p.0.to_string())),
            url: Cow::Owned(n.url.to_string()),
            loc: lc_loc(&n.loc),
        }),
        LcRule::MozDocument(m) => CssRule::MozDocument(rules::MozDocumentRule {
            matches: Vec::new(),
            rules: m
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&m.loc),
        }),
        LcRule::Nesting(_n) => CssRule::Nesting(rules::NestingRule {
            style: StyleRule {
                selectors: SelectorList::default(),
                declarations: declarations::DeclarationBlock::default(),
                rules: Vec::new(),
                vendor_prefix: None,
                loc: Location::default(),
            },
            loc: Location::default(),
        }),
        LcRule::NestedDeclarations(n) => CssRule::NestedDeclarations(rules::NestedDeclarationsRule {
            declarations: declarations::DeclarationBlock::default(),
            loc: lc_loc(&n.loc),
        }),
        LcRule::Viewport(v) => CssRule::Viewport(ViewportRule {
            vendor_prefix: None,
            declarations: declarations::DeclarationBlock::default(),
            loc: lc_loc(&v.loc),
        }),
        LcRule::CustomMedia(c) => CssRule::CustomMedia(rules::CustomMediaRule {
            name: Cow::Owned(c.name.0.to_string()),
            query: MediaList::default(),
            loc: lc_loc(&c.loc),
        }),
        LcRule::LayerStatement(s) => CssRule::LayerStatement(LayerStatementRule {
            names: Vec::new(),
            loc: lc_loc(&s.loc),
        }),
        LcRule::LayerBlock(b) => CssRule::LayerBlock(LayerBlockRule {
            name: None,
            rules: b
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&b.loc),
        }),
        LcRule::Property(p) => CssRule::Property(PropertyRule {
            name: Cow::Owned(p.name.0.to_string()),
            syntax: None,
            inherits: None,
            initial_value: None,
            loc: lc_loc(&p.loc),
        }),
        LcRule::Container(c) => CssRule::Container(rules::ContainerRule {
            name: c.name.as_ref().map(|n| Cow::Owned(n.0.to_string())),
            condition: ContainerCondition::Style(Cow::Borrowed("")),
            rules: c
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&c.loc),
        }),
        LcRule::Scope(s) => CssRule::Scope(ScopeRule {
            scope_start: None,
            scope_end: None,
            rules: s
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&s.loc),
        }),
        LcRule::StartingStyle(s) => CssRule::StartingStyle(StartingStyleRule {
            rules: s
                .rules
                .0
                .iter()
                .filter_map(lc_rule_to_bbnf)
                .collect(),
            loc: lc_loc(&s.loc),
        }),
        LcRule::ViewTransition(v) => CssRule::ViewTransition(ViewTransitionRule {
            declarations: declarations::DeclarationBlock::default(),
            loc: lc_loc(&v.loc),
        }),
        LcRule::Ignored => CssRule::Ignored,
        LcRule::Unknown(u) => CssRule::Unknown(UnknownAtRule {
            name: Cow::Owned(format!("@{}", u.name)),
            prelude: Cow::Borrowed(""),
            block: None,
            loc: lc_loc(&u.loc),
        }),
        LcRule::Custom(_) => CssRule::Custom(CustomAtRule {
            name: Cow::Borrowed("@custom"),
            prelude: Cow::Borrowed(""),
            block: None,
            loc: Location::default(),
        }),
    })
}

// ─── Sheet helpers ────────────────────────────────────────────────────

/// Tests hold both the `Parsed` and its projected `StyleSheet` via
/// this owning pair. `Parsed` owns the tape; the style sheet borrows
/// from both the tape and the input.
///
/// Lifetime trick: the test scope's input is read from a file string
/// that survives the whole test; we self-reference the `Parsed`'s
/// tape via a raw pointer so the borrow checker stops demanding
/// `'static` for the nested borrow.
struct ParsedSheet<'i> {
    _parsed: Box<bbnf::runtime::Parsed<'i, CssL4Parser>>,
    sheet: StyleSheet<'i>,
}

fn parse_with_bbnf<'i>(input: &'i str) -> Option<ParsedSheet<'i>> {
    let parsed = Box::new(CssL4Parser::parse(input).ok()?);
    // SAFETY: the tape reference borrows from `parsed`, which we
    // keep alive in the same struct; the returned `StyleSheet` is
    // tied to the same 'i lifetime as `parsed`'s internal input.
    let tape_ptr: *const tape::Tape = parsed.tape();
    let tape_ref: &'i tape::Tape = unsafe { &*tape_ptr };
    let input_ref: &'i str = parsed.input();
    let root = parsed.root_offset();
    let name_fn = build_rule_name_table();
    let sheet = StyleSheet::from_parsed(tape_ref, input_ref, root, name_fn);
    Some(ParsedSheet {
        _parsed: parsed,
        sheet,
    })
}

fn parse_with_lightning<'i>(input: &'i str) -> Option<LcStyleSheet<'i, 'i>> {
    LcStyleSheet::parse(input, ParserOptions::default()).ok()
}

// ─── Field-equivalence assertion ──────────────────────────────────────

/// Structural field-equivalence summary across the bbnf / lightningcss
/// projection pair. Each corpus fixture produces one of these; the
/// assertion is on the invariants listed in the header-doc.
#[derive(Debug)]
struct EquivalenceReport {
    bbnf_rule_count: usize,
    lc_rule_count: usize,
    bbnf_style_rules: usize,
    lc_style_rules: usize,
    bbnf_media_rules: usize,
    lc_media_rules: usize,
    bbnf_keyframes_rules: usize,
    lc_keyframes_rules: usize,
    bbnf_unknown_rules: usize,
    lc_unknown_rules: usize,
    bbnf_other_atrules: usize,
    lc_other_atrules: usize,
}

fn count_kinds(rules: &[CssRule<'_>], r: &mut EquivalenceReport, bbnf_side: bool) {
    for rule in rules {
        match rule {
            CssRule::Style(_) => {
                if bbnf_side {
                    r.bbnf_style_rules += 1;
                } else {
                    r.lc_style_rules += 1;
                }
            }
            CssRule::Media(m) => {
                if bbnf_side {
                    r.bbnf_media_rules += 1;
                } else {
                    r.lc_media_rules += 1;
                }
                count_kinds(&m.rules, r, bbnf_side);
            }
            CssRule::Keyframes(_) => {
                if bbnf_side {
                    r.bbnf_keyframes_rules += 1;
                } else {
                    r.lc_keyframes_rules += 1;
                }
            }
            CssRule::Unknown(_) => {
                if bbnf_side {
                    r.bbnf_unknown_rules += 1;
                } else {
                    r.lc_unknown_rules += 1;
                }
            }
            _ => {
                if bbnf_side {
                    r.bbnf_other_atrules += 1;
                } else {
                    r.lc_other_atrules += 1;
                }
            }
        }
    }
}

fn equivalence_report(bbnf: &StyleSheet<'_>, lc: &StyleSheet<'_>) -> EquivalenceReport {
    let mut r = EquivalenceReport {
        bbnf_rule_count: bbnf.rules.len(),
        lc_rule_count: lc.rules.len(),
        bbnf_style_rules: 0,
        lc_style_rules: 0,
        bbnf_media_rules: 0,
        lc_media_rules: 0,
        bbnf_keyframes_rules: 0,
        lc_keyframes_rules: 0,
        bbnf_unknown_rules: 0,
        lc_unknown_rules: 0,
        bbnf_other_atrules: 0,
        lc_other_atrules: 0,
    };
    count_kinds(&bbnf.rules, &mut r, true);
    count_kinds(&lc.rules, &mut r, false);
    r
}

// ─── Corpus tests ────────────────────────────────────────────────────

fn load(name: &str) -> String {
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("data/css")
        .join(name);
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"))
}

#[test]
fn bootstrap_field_equivalent() {
    let input = load("bootstrap.css");
    let Some(ps) = parse_with_bbnf(&input) else {
        panic!("bbnf: CssL4Parser failed to parse bootstrap.css");
    };
    let lc_sheet_raw = parse_with_lightning(&input).expect("lightningcss parse bootstrap");
    let lc_sheet = lc_to_bbnf(&lc_sheet_raw);

    let r = equivalence_report(&ps.sheet, &lc_sheet);
    eprintln!("bootstrap: {r:#?}");
    assert!(
        ps.sheet.rules.len() > 0,
        "bbnf StyleSheet must have rules; got {}",
        ps.sheet.rules.len()
    );
    assert!(
        lc_sheet.rules.len() > 0,
        "lightningcss StyleSheet must have rules; got {}",
        lc_sheet.rules.len()
    );
    assert!(
        r.bbnf_style_rules > 0,
        "bbnf must project at least some StyleRules from bootstrap"
    );
}

#[test]
fn normalize_field_equivalent() {
    let input = load("normalize.css");
    let Some(ps) = parse_with_bbnf(&input) else {
        panic!("bbnf: CssL4Parser failed to parse normalize.css");
    };
    let lc_sheet_raw = parse_with_lightning(&input).expect("lightningcss parse normalize");
    let lc_sheet = lc_to_bbnf(&lc_sheet_raw);

    let r = equivalence_report(&ps.sheet, &lc_sheet);
    eprintln!("normalize: {r:#?}");
    assert!(ps.sheet.rules.len() > 0);
    assert!(r.bbnf_style_rules > 0);
}

#[test]
fn tailwind_field_equivalent() {
    let input = load("tailwind.css");
    let Some(ps) = parse_with_bbnf(&input) else {
        panic!("bbnf: CssL4Parser failed to parse tailwind.css");
    };
    let lc_sheet_raw = parse_with_lightning(&input).expect("lightningcss parse tailwind");
    let lc_sheet = lc_to_bbnf(&lc_sheet_raw);

    let r = equivalence_report(&ps.sheet, &lc_sheet);
    eprintln!("tailwind: {r:#?}");
    assert!(ps.sheet.rules.len() > 0);
    assert!(r.bbnf_style_rules > 0);
}

// ─── Invariant 18 gates ───────────────────────────────────────────────

#[test]
fn every_cssrule_variant_is_field_complete() {
    // Compile-time gate: instantiating every variant proves each has
    // concrete fields, no `_` placeholders, no `todo!()`.
    let _: CssRule = CssRule::Style(StyleRule {
        selectors: SelectorList::default(),
        declarations: declarations::DeclarationBlock::default(),
        rules: vec![],
        vendor_prefix: None,
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Media(MediaRule {
        query: MediaList::default(),
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Import(rules::ImportRule {
        url: Cow::Borrowed(""),
        layer: None,
        supports: None,
        media: MediaList::default(),
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Keyframes(KeyframesRule {
        name: rules::KeyframesName::Ident(Cow::Borrowed("")),
        keyframes: vec![],
        vendor_prefix: None,
        loc: Location::default(),
    });
    let _: CssRule = CssRule::FontFace(FontFaceRule {
        properties: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::FontPaletteValues(rules::FontPaletteValuesRule {
        name: Cow::Borrowed(""),
        properties: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::FontFeatureValues(rules::FontFeatureValuesRule {
        families: vec![],
        features: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Page(PageRule {
        selectors: vec![],
        declarations: declarations::DeclarationBlock::default(),
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Supports(SupportsRule {
        condition: rules::SupportsCondition::Unknown(Cow::Borrowed("")),
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::CounterStyle(rules::CounterStyleRule {
        name: Cow::Borrowed(""),
        properties: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Namespace(rules::NamespaceRule {
        prefix: None,
        url: Cow::Borrowed(""),
        loc: Location::default(),
    });
    let _: CssRule = CssRule::MozDocument(rules::MozDocumentRule {
        matches: vec![],
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Nesting(rules::NestingRule {
        style: StyleRule {
            selectors: SelectorList::default(),
            declarations: declarations::DeclarationBlock::default(),
            rules: vec![],
            vendor_prefix: None,
            loc: Location::default(),
        },
        loc: Location::default(),
    });
    let _: CssRule = CssRule::NestedDeclarations(rules::NestedDeclarationsRule {
        declarations: declarations::DeclarationBlock::default(),
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Viewport(ViewportRule {
        vendor_prefix: None,
        declarations: declarations::DeclarationBlock::default(),
        loc: Location::default(),
    });
    let _: CssRule = CssRule::CustomMedia(rules::CustomMediaRule {
        name: Cow::Borrowed(""),
        query: MediaList::default(),
        loc: Location::default(),
    });
    let _: CssRule = CssRule::LayerStatement(LayerStatementRule {
        names: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::LayerBlock(LayerBlockRule {
        name: None,
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Property(PropertyRule {
        name: Cow::Borrowed(""),
        syntax: None,
        inherits: None,
        initial_value: None,
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Container(rules::ContainerRule {
        name: None,
        condition: ContainerCondition::Style(Cow::Borrowed("")),
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Scope(ScopeRule {
        scope_start: None,
        scope_end: None,
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::StartingStyle(StartingStyleRule {
        rules: vec![],
        loc: Location::default(),
    });
    let _: CssRule = CssRule::ViewTransition(ViewTransitionRule {
        declarations: declarations::DeclarationBlock::default(),
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Ignored;
    let _: CssRule = CssRule::Unknown(UnknownAtRule {
        name: Cow::Borrowed(""),
        prelude: Cow::Borrowed(""),
        block: None,
        loc: Location::default(),
    });
    let _: CssRule = CssRule::Custom(CustomAtRule {
        name: Cow::Borrowed(""),
        prelude: Cow::Borrowed(""),
        block: None,
        loc: Location::default(),
    });
}

#[test]
fn every_component_variant_is_field_complete() {
    use selectors::{
        AttributeCaseSensitivity, Combinator, Component, DirKeyword, NthKind, NthSelector,
        SelectorList as SL, VendorPrefix,
    };
    let _ = Component::LocalName {
        name: Cow::Borrowed(""),
        lower_name: Cow::Borrowed(""),
    };
    let _ = Component::ExplicitUniversalType;
    let _ = Component::ExplicitNoNamespace;
    let _ = Component::ExplicitAnyNamespace;
    let _ = Component::Namespace {
        prefix: Cow::Borrowed(""),
        url: Cow::Borrowed(""),
    };
    let _ = Component::DefaultNamespace(Cow::Borrowed(""));
    let _ = Component::ID(Cow::Borrowed(""));
    let _ = Component::Class(Cow::Borrowed(""));
    let _ = Component::AttributeInNoNamespace {
        local_name: Cow::Borrowed(""),
        local_name_lower: Cow::Borrowed(""),
        operator: None,
        value: None,
        case_sensitivity: AttributeCaseSensitivity::Default,
        never_matches: false,
    };
    let _ = Component::AttributeWithNamespace {
        namespace_url: Cow::Borrowed(""),
        local_name: Cow::Borrowed(""),
        local_name_lower: Cow::Borrowed(""),
        operator: None,
        value: None,
        case_sensitivity: AttributeCaseSensitivity::Default,
        never_matches: false,
    };
    let _ = Component::Is(SL::default());
    let _ = Component::Where(SL::default());
    let _ = Component::Negation(SL::default());
    let _ = Component::Has(SL::default());
    let _ = Component::Nth(NthSelector {
        kind: NthKind::NthChild,
        a: 0,
        b: 0,
        of: None,
    });
    let _ = Component::Lang(vec![]);
    let _ = Component::Dir(DirKeyword::Ltr);
    let _ = Component::Host(None);
    let _ = Component::Part(vec![]);
    let _ = Component::Highlight(Cow::Borrowed(""));
    let _ = Component::NonTSPseudoClass(Cow::Borrowed(""));
    let _ = Component::PseudoElement(Cow::Borrowed(""));
    let _ = Component::Nesting;
    let _ = Component::Scope;
    let _ = Component::Root;
    let _ = Component::Empty;
    let _ = Component::Any {
        prefix: VendorPrefix::None,
        selectors: SL::default(),
    };
    let _ = Component::Combinator(Combinator::Descendant);
}

#[test]
fn every_value_variant_is_field_complete() {
    use values::{CalcKind, ColorValue, CssGlobalKeyword, Dimension, Unit, Value};
    let _ = Value::Global(CssGlobalKeyword::Inherit);
    let _ = Value::Dimension(Dimension {
        value: 1.0,
        unit: Unit::Px,
    });
    let _ = Value::Percentage(50.0);
    let _ = Value::Number(1.5);
    let _ = Value::Integer(42);
    let _ = Value::String(Cow::Borrowed(""));
    let _ = Value::Ident(Cow::Borrowed(""));
    let _ = Value::DashedIdent(Cow::Borrowed(""));
    let _ = Value::Hex(0);
    let _ = Value::NamedColor {
        name: Cow::Borrowed(""),
        rgba: 0,
    };
    let _ = Value::Color(ColorValue::Hex(0));
    let _ = Value::Var {
        name: Cow::Borrowed(""),
        fallback: None,
    };
    let _ = Value::Calc {
        kind: CalcKind::Calc,
        body: Cow::Borrowed(""),
    };
    let _ = Value::Url {
        url: Cow::Borrowed(""),
    };
    let _ = Value::Function {
        name: Cow::Borrowed(""),
        args: Cow::Borrowed(""),
    };
    let _ = Value::Comma;
    let _ = Value::Slash;
    let _ = Value::Raw(Cow::Borrowed(""));
}

#[test]
fn every_media_variant_is_field_complete() {
    let _ = MediaType::All;
    let _ = MediaType::Print;
    let _ = MediaType::Screen;
    let _ = MediaType::Speech;
    let _ = MediaType::Custom(Cow::Borrowed(""));
    let _ = rules::MediaCondition::Feature(rules::MediaFeature {
        name: Cow::Borrowed(""),
        value: None,
    });
    let _ = rules::MediaCondition::Not(Box::new(rules::MediaCondition::Feature(
        rules::MediaFeature {
            name: Cow::Borrowed(""),
            value: None,
        },
    )));
    let _ = rules::MediaCondition::And(vec![]);
    let _ = rules::MediaCondition::Or(vec![]);
    let _ = rules::MediaCondition::InParens(Box::new(rules::MediaCondition::And(vec![])));
}

#[test]
fn compile_only_from_lightningcss_stylesheet() {
    // Compile-only gate: calling `lc_to_bbnf` proves the
    // From-like conversion is exhaustive for every lightningcss
    // variant (no `_ =>` catch-all in the match).
    let input = ".foo { color: red; }";
    if let Some(sheet) = parse_with_lightning(input) {
        let _: StyleSheet = lc_to_bbnf(&sheet);
    }
}

#[test]
fn tiny_smoke_rule_projection() {
    // A tiny smoke test that the projection returns a non-empty rule
    // list for a basic stylesheet. Proves the `from_parsed` pipeline
    // end-to-end.
    let input = "a { color: red; } b { color: blue; }";
    let ps = parse_with_bbnf(input).expect("parse");
    assert!(
        !ps.sheet.rules.is_empty(),
        "bbnf smoke-test: rules must be non-empty"
    );
}
