use std::fs;
use std::path::Path;
use std::process::Command;

use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{
    AltBranch, GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc, interpreter::parse_with_ir,
};

#[path = "../../crates/core/src/runtime/css_l4/host.rs"]
mod css_host;

fn compile_and_run(name: &str, source: &str) {
    let temp = tempfile::tempdir().expect("temporary rustc fixture");
    let source_path = temp.path().join(format!("{name}.rs"));
    let binary_path = temp.path().join(name);
    fs::write(&source_path, source).expect("write rustc fixture");
    let output = Command::new("rustc")
        .args(["--edition=2024", "-o"])
        .arg(&binary_path)
        .arg(&source_path)
        .output()
        .expect("run ARM-native rustc");
    assert!(
        output.status.success(),
        "rustc failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let status = Command::new(&binary_path)
        .status()
        .expect("run exact-integer fixture");
    assert!(status.success(), "exact-integer fixture failed");
}

fn compile_sheets_canonical_with_add_byte(name: &str, add_byte: u8, expected_add: &str) {
    let canonical = xtask::regen_simple_runtime::render_source("google_sheets", "canonical.rs")
        .expect("Sheets serializer");
    let support = r#"
mod grammar {
    pub mod generated {
        pub mod google_sheets {
            #[derive(Clone, Copy)]
            pub struct PrattEntry {
                pub byte: u8,
                pub second_byte: Option<u8>,
                pub op_discriminant: u8,
            }

            pub const PRECEDENCE_ENTRIES_add_expr: &[PrattEntry] = &[
                PrattEntry {
                    byte: __ADD_BYTE__,
                    second_byte: None,
                    op_discriminant: 0,
                },
                PrattEntry {
                    byte: b'-',
                    second_byte: None,
                    op_discriminant: 1,
                },
            ];
            pub const PRECEDENCE_ENTRIES_array_row: &[PrattEntry] = &[PrattEntry {
                byte: b',',
                second_byte: None,
                op_discriminant: 0,
            }];
            pub const PRECEDENCE_ENTRIES_array_rows: &[PrattEntry] = &[PrattEntry {
                byte: b';',
                second_byte: None,
                op_discriminant: 0,
            }];
            pub const PRECEDENCE_ENTRIES_comparison_expr: &[PrattEntry] = &[PrattEntry {
                byte: b'<',
                second_byte: Some(b'>'),
                op_discriminant: 0,
            }];
            pub const PRECEDENCE_ENTRIES_concat_expr: &[PrattEntry] = &[PrattEntry {
                byte: b'&',
                second_byte: None,
                op_discriminant: 0,
            }];
            pub const PRECEDENCE_ENTRIES_exp_expr: &[PrattEntry] = &[PrattEntry {
                byte: b'^',
                second_byte: None,
                op_discriminant: 0,
            }];
            pub const PRECEDENCE_ENTRIES_mul_expr: &[PrattEntry] = &[PrattEntry {
                byte: b'*',
                second_byte: None,
                op_discriminant: 0,
            }];
        }
    }
}

mod runtime {
    pub mod google_sheets {
        pub mod arena {
            use super::value::SheetsValue;

            #[derive(Clone, Copy)]
            pub struct SheetsCompoundId(pub usize);

            #[derive(Clone, Copy, PartialEq, Eq)]
            pub enum SheetsCompoundKind {
                Unknown,
                Formula,
                Expression,
                ComparisonExpr,
                ConcatExpr,
                AddExpr,
                MulExpr,
                ExpExpr,
                UnaryExpr,
                PostfixExpr,
                Primary,
                ParenExpr,
                FuncOpen,
                Arg,
                FuncArgs,
                FuncCall,
                LetBinding,
                LetArgs,
                LetCall,
                LambdaParams,
                LambdaCall,
                ArrayRow,
                ArrayRows,
                ArrayLiteral,
                Cell,
                RangeRef,
                RangeEnd,
                CellOrRange,
                ErrorLiteral,
                SheetPrefix,
                CompareOp,
                AddOp,
                MulOp,
                UnaryPrefix,
                Wrap,
            }

            pub struct SheetsCompoundView<'a, 'p> {
                pub kind: SheetsCompoundKind,
                pub children: &'a [SheetsValue<'p>],
            }
        }

        pub mod value {
            use super::arena::SheetsCompoundId;

            #[derive(Clone, Copy)]
            pub enum SheetsValue<'p> {
                Number(f64),
                Integer(i64),
                Unsigned(u64),
                String(&'p str),
                Bool(bool),
                Error(u8),
                CellRef(&'p str),
                Identifier(&'p str),
                SheetPrefix { tag: u8, text: &'p str },
                Tag(u8),
                Compound(SheetsCompoundId),
            }
        }
    }
}
"#
    .replace("__ADD_BYTE__", &add_byte.to_string());

    let mut source = support;
    source.push_str(
        r#"
mod projected {
    use crate::runtime::google_sheets::arena::{
        SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView,
    };
    use crate::runtime::google_sheets::value::SheetsValue;

    pub struct SheetsCompound<'p> {
        kind: SheetsCompoundKind,
        children: Vec<SheetsValue<'p>>,
    }

    pub struct SheetsDocument<'p> {
        pub input: &'p [u8],
        pub root: SheetsValue<'p>,
        compounds: Vec<SheetsCompound<'p>>,
    }

    impl<'p> SheetsDocument<'p> {
        pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
            let entry = &self.compounds[id.0];
            SheetsCompoundView {
                kind: entry.kind,
                children: &entry.children,
            }
        }
    }

    mod canonical {
"#,
    );
    source.push_str(&canonical);
    source.push_str(
        r#"
    }

    pub fn serialize_binary(kind: SheetsCompoundKind) -> String {
        let doc = SheetsDocument {
            input: b"",
            root: SheetsValue::Compound(SheetsCompoundId(0)),
            compounds: vec![SheetsCompound {
                kind,
                children: vec![
                    SheetsValue::Integer(1),
                    SheetsValue::Tag(0),
                    SheetsValue::Integer(2),
                ],
            }],
        };
        canonical::serialize_compact(&doc)
    }

    pub fn serialize_prefix(kind: SheetsCompoundKind, tag: u8) -> String {
        let doc = SheetsDocument {
            input: b"",
            root: SheetsValue::Compound(SheetsCompoundId(0)),
            compounds: vec![SheetsCompound {
                kind,
                children: vec![SheetsValue::Tag(tag), SheetsValue::Integer(2)],
            }],
        };
        canonical::serialize_compact(&doc)
    }
}

fn main() {
    use runtime::google_sheets::arena::SheetsCompoundKind;

    std::panic::set_hook(Box::new(|_| {}));
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::AddExpr),
        "__EXPECTED_ADD__"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::AddOp),
        "__EXPECTED_ADD__"
    );
    assert_eq!(
        projected::serialize_prefix(SheetsCompoundKind::UnaryExpr, 0),
        "__EXPECTED_PREFIX__"
    );
    assert_eq!(
        projected::serialize_prefix(SheetsCompoundKind::UnaryPrefix, 0),
        "__EXPECTED_PREFIX__"
    );
    assert!(
        std::panic::catch_unwind(|| {
            projected::serialize_prefix(SheetsCompoundKind::UnaryExpr, 2)
        })
        .is_err()
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::MulExpr),
        "=1*2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::MulOp),
        "=1*2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::ConcatExpr),
        "=1&2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::ExpExpr),
        "=1^2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::ComparisonExpr),
        "=1<>2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::CompareOp),
        "=1<>2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::ArrayRow),
        "=1,2"
    );
    assert_eq!(
        projected::serialize_binary(SheetsCompoundKind::ArrayRows),
        "=1;2"
    );
}
"#,
    );
    source = source
        .replace("__EXPECTED_ADD__", expected_add)
        .replace("__EXPECTED_PREFIX__", &format!("={}2", &expected_add[2..3]));
    compile_and_run(name, &source);
}

#[test]
fn generated_bbnf_pratt_projection_is_semantic_tag_only() {
    let builder =
        xtask::regen_simple_runtime::render_source("bbnf", "builder.rs").expect("BBNF builder");
    let view = xtask::regen_simple_runtime::render_source("bbnf", "view.rs").expect("BBNF view");
    let bbnf_serialize = xtask::regen_simple_runtime::render_source("bbnf", "serialize.rs")
        .expect("BBNF serializer");
    let sheets_serialize =
        xtask::regen_simple_runtime::render_source("google_sheets", "canonical.rs")
            .expect("Sheets serializer");
    let lower = fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../crates/core/src/lower/expression/pratt.rs"),
    )
    .expect("Pratt lowerer");

    assert!(builder.contains("self.deposit(BbnfValue::Tag(value));"));
    assert!(view.contains("pub fn tag_value(&self) -> Option<u8>"));
    assert!(lower.contains("PRECEDENCE_ENTRIES_binary_factor"));
    assert!(lower.contains("while let Some(tag) = children.next()"));
    assert!(!lower.contains("branch_tag()"));
    assert!(!lower.contains("recover_binary_op"));
    assert!(bbnf_serialize.contains("PRECEDENCE_ENTRIES_binary_factor"));
    assert!(bbnf_serialize.contains("write_pratt_operator"));
    assert!(!bbnf_serialize.contains("out.push_str(\" - \")"));
    assert!(sheets_serialize.contains("PRECEDENCE_ENTRIES_comparison_expr"));
    assert!(sheets_serialize.contains("write_pratt_operator"));
    assert!(!sheets_serialize.contains("(__P__CompoundKind::ComparisonExpr, 3)"));

    let json =
        xtask::regen_simple_runtime::render_source("json", "builder.rs").expect("JSON builder");
    assert!(json.contains("only null owns this projection"));
    assert!(json.contains("value, 0,"));
}

#[test]
fn generated_sheets_serializer_has_one_operator_spelling_authority() {
    let canonical = xtask::regen_simple_runtime::render_source("google_sheets", "canonical.rs")
        .expect("Sheets serializer");

    for table in [
        "PRECEDENCE_ENTRIES_add_expr",
        "PRECEDENCE_ENTRIES_array_row",
        "PRECEDENCE_ENTRIES_array_rows",
        "PRECEDENCE_ENTRIES_comparison_expr",
        "PRECEDENCE_ENTRIES_concat_expr",
        "PRECEDENCE_ENTRIES_exp_expr",
        "PRECEDENCE_ENTRIES_mul_expr",
    ] {
        assert!(
            canonical.contains(table),
            "canonical serializer omitted {table}"
        );
    }
    assert!(canonical.contains("fn operator_entries("));
    assert!(canonical.contains("Some(PRECEDENCE_ENTRIES_array_row)"));
    assert!(canonical.contains("Some(PRECEDENCE_ENTRIES_array_rows)"));
    assert!(!canonical.contains("fn tag_lexeme("));
    assert!(!canonical.contains("(SheetsCompoundKind::AddExpr, 0)"));
    assert!(!canonical.contains("out.push(';')"));

    compile_sheets_canonical_with_add_byte("sheets_operator_metadata_baseline", b'+', "=1+2");
    compile_sheets_canonical_with_add_byte("sheets_operator_metadata_mutated", b'~', "=1~2");
}

#[test]
fn generated_css_roles_and_checkpoint_are_unambiguous_and_transactional() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let builder =
        fs::read_to_string(root.join("xtask/src/regen_css.rs")).expect("CSS builder generator");
    let projection = fs::read_to_string(root.join("xtask/runtime-projections/css_l4.toml"))
        .expect("CSS projection");
    let value = xtask::regen_css::render_value_source().expect("CSS value projection");

    assert!(builder.contains("ColorFunction {{ kind_tag: Option<u8>"));
    assert!(builder.contains("ColorPredefined {{ space_tag: Option<u8>"));
    assert!(builder.contains("\"HexColor\" => \"OpenFrame::HexColor { packed: None }\""));
    assert!(builder.contains("\"NamedColor\" => \"OpenFrame::NamedColor { packed: None }\""));
    assert!(builder.contains("\"CurrentColor\" => \"OpenFrame::CurrentColor\""));
    assert!(builder.contains("OpenFrame::HexColor {{ packed }}"));
    assert!(builder.contains("CssColor::Named(packed)"));
    assert!(builder.contains("CssColor::CurrentColor"));
    assert!(builder.contains("u32::try_from(value)"));
    assert!(builder.contains("HexColor closed without an exact u32 payload"));
    assert!(builder.contains("_ => self.deposit_value(CssTypedValue::Unsigned(value))"));
    assert!(builder.contains("Numeric closed without a magnitude"));
    assert!(builder.contains("Numeric received more than one unit tag"));
    assert!(builder.contains("functional color closed without a valid grammar-owned kind tag"));
    assert!(builder.contains("predefined color closed without a valid grammar-owned space tag"));
    assert!(
        builder.contains(
            "CssTypedValue::Dimension(CssDimension::Percentage(CssPercentage {{ value }}))"
        )
    );
    assert!(!builder.contains("ColorMix"));
    assert!(!builder.contains("CssHueMethod"));
    assert!(!builder.contains("unwrap_or(CssColorType::Rgb)"));
    assert!(!builder.contains("unwrap_or(CssColorSpace::Srgb)"));
    assert!(builder.contains("CssColor::Function(CssColorFunction"));
    assert!(builder.contains("CssColor::Predefined(CssColorPredefined"));
    assert!(builder.contains("span_starts: self.span_starts.clone()"));
    assert!(builder.contains("self.span_starts = checkpoint.span_starts"));
    assert!(!builder.contains("span_starts_len"));
    assert!(!builder.contains("self.span_starts.truncate"));
    assert!(!builder.contains("O(1) checkpoint"));
    assert!(value.contains("Unsigned(u64)"));
    assert!(value.contains("U8(u8)"));
    assert!(value.contains("Named(u32)"));
    assert!(value.contains("CurrentColor"));
    assert!(!value.contains("CssColorMix"));
    assert!(projection.contains(
        "rule = \"colorFunction\"\nframe = \"ColorFunction\"\n\n[[builder.color_routes]]\nrule = \"colorFn\"\nframe = \"ColorPredefined\""
    ));
    assert!(projection.contains("{ name = \"Ric\", value = 48 }"));
    let color_type = projection
        .split_once("name = \"CssColorType\"")
        .expect("CssColorType projection")
        .1
        .split_once("[[repr_enums]]")
        .expect("CssColorType projection boundary")
        .0;
    for functional in ["Hsl", "Hsla", "Hwb", "Lab", "Lch", "Oklab", "Oklch"] {
        assert!(
            color_type.contains(&format!("name = \"{functional}\"")),
            "CssColorType lost functional Color 4 family {functional}"
        );
    }
    let color_space = projection
        .split_once("name = \"CssColorSpace\"")
        .expect("CssColorSpace projection")
        .1
        .split_once("[[repr_enums]]")
        .expect("CssColorSpace projection boundary")
        .0;
    for functional in ["Hsl", "Hsla", "Hwb", "Lab", "Lch", "Oklab", "Oklch"] {
        assert!(
            !color_space.contains(&format!("name = \"{functional}\"")),
            "functional family {functional} is not a predefined color space"
        );
    }

    let grammar =
        fs::read_to_string(root.join("grammar/css/l4/color.bbnf")).expect("CSS color grammar");
    let host = fs::read_to_string(root.join("crates/core/src/runtime/css_l4/host.rs"))
        .expect("CSS color host");
    assert!(host.contains("pub(crate) fn parse_hex_color"));
    assert!(host.contains("pub fn try_parse_hex_color"));
    assert!(grammar.contains("currentColor = \"currentcolor\" ;"));
    for legal in [8, 6, 4, 3] {
        assert!(grammar.contains(&format!(
            "/[0-9a-fA-F]{{{legal}}}/ - ( /[0-9a-fA-F]{{{legal}}}/ , hexHashContinuation )"
        )));
    }
    assert!(grammar.contains("hexHashContinuation ="));
    assert!(grammar.contains("/[0-9A-Za-z_-]/"));
    assert!(grammar.contains("\\u{00B7}"));
    assert!(grammar.contains("\\u{10000}-\\u{10FFFF}"));
    assert!(grammar.contains(r#"/\\[^\n\r\f]/"#));
    assert!(grammar.contains(r#"( "\\" - ( "\\" , /(?s)./ ) )"#));
    assert!(!grammar.contains("{3,8}"));
    assert!(!grammar.contains("colorMix"));
    for function in [
        "\"oklch\" -> 8u8",
        "\"oklab\" -> 7u8",
        "\"hsla\" -> 3u8",
        "\"hsl\" -> 2u8",
        "\"hwb\" -> 4u8",
        "\"lab\" -> 5u8",
        "\"lch\" -> 6u8",
    ] {
        assert!(
            grammar.contains(function),
            "functional Color 4 production missing {function}"
        );
    }
}

#[test]
fn simple_runtime_rejects_unrepresentable_scalar_registries() {
    let mut registry = StructRegistry::new();
    registry.insert(StructLayout {
        rule_id: 7,
        rule_name: "lossy".into(),
        kind: LayoutKind::NewtypeWrapper,
        rule_type: TypeDesc::F64,
        fields: vec![StructField {
            name: "value".into(),
            type_desc: TypeDesc::F64,
            source: FieldSource::TypedLeaf,
        }],
    });
    let error = xtask::regen_simple_runtime::validate_simple_scalar_registry(&registry)
        .expect_err("f64 cannot be represented by SimpleValue");
    assert!(error.to_string().contains("f64"));

    let mut supported = StructRegistry::new();
    supported.insert(StructLayout {
        rule_id: 8,
        rule_name: "semantic_byte".into(),
        kind: LayoutKind::NewtypeWrapper,
        rule_type: TypeDesc::U8,
        fields: vec![StructField {
            name: "value".into(),
            type_desc: TypeDesc::U8,
            source: FieldSource::TypedLeaf,
        }],
    });
    xtask::regen_simple_runtime::validate_simple_scalar_registry(&supported)
        .expect("semantic u8 is representable");

    for grammar in ["bnf", "ebnf", "csv", "css_pretty", "math"] {
        xtask::regen_simple_runtime::render_source(grammar, "value.rs")
            .unwrap_or_else(|error| panic!("{grammar} registry rejected: {error:#}"));
    }
}

#[test]
fn css_hex_conversion_is_exact_and_fallible() {
    for (hex, packed) in [
        ("abc", 0xAABBCCFF),
        ("abcd", 0xAABBCCDD),
        ("112233", 0x112233FF),
        ("11223344", 0x11223344),
    ] {
        assert_eq!(css_host::try_parse_hex_color(hex), Some(packed));
        assert_eq!(
            css_host::try_parse_hex_color(&format!("#{hex}")),
            Some(packed)
        );
    }
    for invalid in [
        "",
        "#",
        "12",
        "#12",
        "12345",
        "#12345",
        "1234567",
        "#1234567",
        "123456789",
        "#123456789",
        "123456789a",
        "#123456789a",
        "ggg",
        "#ggg",
        "12g",
        "#12g",
        "abc\\",
        "#abc\\",
    ] {
        assert_eq!(css_host::try_parse_hex_color(invalid), None, "{invalid:?}");
    }
}

fn exact_hex_ir(parent_suffix: Option<&str>) -> GrammarIR {
    const HASH: u32 = 1;
    const HEX8: u32 = 2;
    const HEX6: u32 = 3;
    const HEX4: u32 = 4;
    const HEX3: u32 = 5;
    const HASH_CONTINUATION: u32 = 6;
    const BACKSLASH: u32 = 7;
    const ANY_CODEPOINT: u32 = 8;
    const PARENT_SUFFIX: u32 = 9;

    let exact_width = |pattern| {
        IrNode::Minus(
            Box::new(IrNode::Regex(pattern)),
            Box::new(IrNode::Seq(vec![
                IrNode::Regex(pattern),
                IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Regex(HASH_CONTINUATION),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Minus(
                                Box::new(IrNode::Literal(BACKSLASH)),
                                Box::new(IrNode::Seq(vec![
                                    IrNode::Literal(BACKSLASH),
                                    IrNode::Regex(ANY_CODEPOINT),
                                ])),
                            ),
                            first_set: None,
                        },
                    ],
                    None,
                ),
            ])),
        )
    };
    let widths = [HEX8, HEX6, HEX4, HEX3]
        .into_iter()
        .map(|pattern| AltBranch {
            node: exact_width(pattern),
            first_set: None,
        })
        .collect();
    let mut body = IrNode::Seq(vec![IrNode::Literal(HASH), IrNode::Alt(widths, None)]);
    if parent_suffix.is_some() {
        body = IrNode::Seq(vec![body, IrNode::Literal(PARENT_SUFFIX)]);
    }

    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings: vec![
            "start".into(),
            "#".into(),
            "[0-9a-fA-F]{8}".into(),
            "[0-9a-fA-F]{6}".into(),
            "[0-9a-fA-F]{4}".into(),
            "[0-9a-fA-F]{3}".into(),
            concat!(
                "(?:[0-9A-Za-z_-]|",
                "[\\u{00B7}\\u{00C0}-\\u{00D6}\\u{00D8}-\\u{00F6}",
                "\\u{00F8}-\\u{037D}\\u{037F}-\\u{1FFF}\\u{200C}\\u{200D}",
                "\\u{203F}\\u{2040}\\u{2070}-\\u{218F}\\u{2C00}-\\u{2FEF}",
                "\\u{3001}-\\u{D7FF}\\u{F900}-\\u{FDCF}\\u{FDF0}-\\u{FFFD}",
                "\\u{10000}-\\u{10FFFF}]|\\\\[^\\n\\r\\f])"
            )
            .into(),
            "\\".into(),
            "(?s).".into(),
            parent_suffix.unwrap_or_default().into(),
        ],
        ..GrammarIR::default()
    }
}

#[test]
fn ir_minus_enforces_complete_css_hash_token_boundaries() {
    let direct = exact_hex_ir(None);
    for (valid, offset) in [
        ("#abc", 4),
        ("#abcd", 5),
        ("#112233", 7),
        ("#11223344", 9),
        ("#abc;", 4),
        ("#abc ", 4),
        ("#abc)", 4),
        ("#abc\u{00B8}", 4),
        ("#abc\\\n", 4),
    ] {
        let parsed = parse_with_ir(&direct, valid);
        assert!(parsed.success, "legal hash boundary rejected {valid:?}");
        assert_eq!(parsed.offset, offset, "wrong boundary for {valid:?}");
    }

    for invalid in [
        "abc",
        "#ab",
        "#12345",
        "#1234567",
        "#123456789",
        "#abg",
        "#abcg",
        "#abc_",
        "#abc-",
        "#abc\u{00B7}",
        "#abc\\g",
        "#abc\\",
    ] {
        assert!(
            !parse_with_ir(&direct, invalid).success,
            "hash-token prefix accepted {invalid:?}"
        );
    }

    let delimited_parent = exact_hex_ir(Some(";"));
    let delimited = parse_with_ir(&delimited_parent, "#abc;");
    assert!(delimited.success);
    assert_eq!(delimited.offset, 5);

    for (suffix, input) in [
        ("g", "#abcg"),
        ("_", "#abc_"),
        ("-", "#abc-"),
        ("\u{00B7}", "#abc\u{00B7}"),
        ("\\g", "#abc\\g"),
        ("\\", "#abc\\"),
    ] {
        assert!(
            !parse_with_ir(&exact_hex_ir(Some(suffix)), input).success,
            "parent consumed a continuation that belongs to the hash token: {input:?}"
        );
    }

    let escaped_newline_parent = parse_with_ir(&exact_hex_ir(Some("\\\n")), "#abc\\\n");
    assert!(
        escaped_newline_parent.success,
        "backslash-newline is not a valid escape and must remain available to the parent"
    );
    assert_eq!(escaped_newline_parent.offset, 6);
}

#[test]
fn ir_minus_probes_the_exclusion_from_the_primary_start() {
    let ir = GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Seq(vec![
                IrNode::Minus(Box::new(IrNode::Regex(1)), Box::new(IrNode::Regex(2))),
                IrNode::Literal(3),
            ]),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings: vec![
            "start".into(),
            "[0-9a-fA-F]{3}".into(),
            "[0-9a-fA-F]{4}".into(),
            "d".into(),
        ],
        ..GrammarIR::default()
    };

    let prefix_in_parent = parse_with_ir(&ir, "abcd");
    assert!(
        !prefix_in_parent.success,
        "Minus must test the four-digit exclusion at the same start before the parent consumes d"
    );
}

#[test]
fn generated_integer_projection_round_trips_precision_boundaries() {
    let sheets =
        xtask::regen_simple_runtime::render_source("google_sheets", "value.rs").expect("Sheets");
    let bbnf = xtask::regen_simple_runtime::render_source("bbnf", "value.rs").expect("BBNF");
    let css = xtask::regen_css::render_value_source().expect("CSS");

    let source = format!(
        r#"
mod runtime {{
    pub mod google_sheets {{
        pub mod arena {{
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct SheetsCompoundId;
        }}
        pub mod value {{ {sheets} }}
    }}
    pub mod bbnf {{
        pub mod arena {{
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct BbnfCompoundId;
        }}
        pub mod value {{ {bbnf} }}
    }}
    pub mod css_l4 {{
        pub mod arena {{
            macro_rules! handle {{
                ($($name:ident),+ $(,)?) => {{
                    $(
                        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                        pub struct $name;
                    )+
                }};
            }}
            handle!(
                CssDeclListId,
                CssRuleListId,
                CssSelectorListId,
                CssValueListId,
                CssKeyframeListId,
            );
        }}
        pub mod value {{ {css} }}
    }}
}}

use runtime::bbnf::value::BbnfValue;
use runtime::css_l4::value::CssTypedValue;
use runtime::google_sheets::value::SheetsValue;

fn main() {{
    let signed = [-(1_i64 << 53) - 1, (1_i64 << 53) - 1, i64::MAX];
    for value in signed {{
        assert_eq!(SheetsValue::Integer(value).as_i64(), Some(value));
        assert!(matches!(BbnfValue::Int(value), BbnfValue::Int(found) if found == value));
        assert!(matches!(CssTypedValue::Integer(value), CssTypedValue::Integer(found) if found == value));
    }}

    let unsigned = [
        (1_u64 << 53) - 1,
        (1_u64 << 53) + 1,
        i64::MAX as u64,
        i64::MAX as u64 + 1,
        u64::MAX,
    ];
    for value in unsigned {{
        assert_eq!(SheetsValue::Unsigned(value).as_u64(), Some(value));
        assert!(matches!(BbnfValue::UInt(value), BbnfValue::UInt(found) if found == value));
        assert!(matches!(CssTypedValue::Unsigned(value), CssTypedValue::Unsigned(found) if found == value));
    }}
}}
"#
    );

    compile_and_run("projection_integer_boundaries", &source);
}
