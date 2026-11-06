use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, MapBinOp, MapExpr, MapUnaryOp, TypeDesc};

use crate::graph::{calculate_ast_deps, tarjan_scc};
use crate::lower::{DirectiveSet, lower_to_ir};

fn lower_grammar(source: &str) -> GrammarIR {
    let ast = crate::grammar::parse(source)
        .expect("source-isomorphic lowerer fixture must parse")
        .rules;
    let deps = calculate_ast_deps(&ast);
    let scc_result = tarjan_scc(&deps);
    lower_to_ir(&ast, &scc_result, &DirectiveSet::empty(), &[])
}

fn rule_body<'a>(ir: &'a GrammarIR, name: &str) -> &'a IrNode {
    &ir.find_rule(name).expect("fixture rule must exist").body
}

fn assert_literal(ir: &GrammarIR, node: &IrNode, expected: &str) {
    let IrNode::Literal(string_id) = node else {
        panic!("expected literal {expected:?}, got {node:#?}");
    };
    assert_eq!(ir.get_string(*string_id), expected);
}

fn assert_regex(ir: &GrammarIR, node: &IrNode, expected: &str) {
    let IrNode::Regex(string_id) = node else {
        panic!("expected regex {expected:?}, got {node:#?}");
    };
    assert_eq!(ir.get_string(*string_id), expected);
}

fn assert_int_map<'a>(
    ir: &GrammarIR,
    node: &'a IrNode,
    value: i64,
    return_type: Option<TypeDesc>,
) -> &'a IrNode {
    let IrNode::Map { inner, fn_id } = node else {
        panic!("expected typed Map, got {node:#?}");
    };
    assert_eq!(
        ir.fns.get(*fn_id as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::IntLit(value),
            return_type,
        }),
        "FnId {fn_id} must retain the exact typed integer expression",
    );
    inner
}

fn assert_uint_map<'a>(
    ir: &GrammarIR,
    node: &'a IrNode,
    value: u64,
    return_type: TypeDesc,
) -> &'a IrNode {
    let IrNode::Map { inner, fn_id } = node else {
        panic!("expected typed Map, got {node:#?}");
    };
    assert_eq!(
        ir.fns.get(*fn_id as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::UIntLit(value),
            return_type: Some(return_type),
        }),
        "FnId {fn_id} must retain the exact typed unsigned expression",
    );
    inner
}

fn count_maps(node: &IrNode) -> usize {
    let mut count = usize::from(matches!(node, IrNode::Map { .. }));
    node.for_each_child(&mut |child| count += count_maps(child));
    count
}

fn mapped_after_prefix<'a>(
    ir: &'a GrammarIR,
    rule: &str,
    prefix: &str,
    inner: &str,
) -> &'a FnDescriptor {
    let body = rule_body(ir, rule);
    let IrNode::Seq(items) = body else {
        panic!("{rule} fixture must remain a sequence");
    };
    let [
        prefix_node,
        IrNode::Map {
            inner: mapped,
            fn_id,
        },
    ] = items.as_slice()
    else {
        panic!("{rule} fixture must contain one prefix and one map");
    };
    assert_literal(ir, prefix_node, prefix);
    assert_literal(ir, mapped, inner);
    assert_eq!(count_maps(body), 1);
    &ir.fns[*fn_id as usize]
}

#[test]
fn one_parenthesized_inner_map_lowers_only_the_inner_mapped_factor() {
    let ir = lower_grammar(r#"subject = ("x" -> 0u8) ;"#);
    let inner = assert_uint_map(&ir, rule_body(&ir, "subject"), 0, TypeDesc::U8);
    assert_literal(&ir, inner, "x");
    assert_eq!(count_maps(rule_body(&ir, "subject")), 1);
    assert_eq!(ir.fns.len(), 1);
}

#[test]
fn multiple_parenthesized_levels_preserve_one_inner_map() {
    let ir = lower_grammar(r#"subject = ((("x" -> 0u8))) ;"#);
    let inner = assert_uint_map(&ir, rule_body(&ir, "subject"), 0, TypeDesc::U8);
    assert_literal(&ir, inner, "x");
    assert_eq!(count_maps(rule_body(&ir, "subject")), 1);
    assert_eq!(ir.fns.len(), 1);
}

#[test]
fn outer_map_around_mapped_group_lowers_to_exact_nested_maps() {
    let ir = lower_grammar(r#"subject = ("x" -> 0u8) -> 1u16 ;"#);
    let inner_map = assert_uint_map(&ir, rule_body(&ir, "subject"), 1, TypeDesc::U16);
    let literal = assert_uint_map(&ir, inner_map, 0, TypeDesc::U8);
    assert_literal(&ir, literal, "x");
    assert_eq!(count_maps(rule_body(&ir, "subject")), 2);
    assert_eq!(ir.fns.len(), 2);
}

#[test]
fn literal_and_regex_arrows_are_not_direct_maps() {
    let ir = lower_grammar(
        r#"
double = "left->right" ;
single = 'left=>right' ;
backtick = `left->right` ;
regex = /left=>right|next->item/ ;
"#,
    );

    assert_literal(&ir, rule_body(&ir, "double"), "left->right");
    assert_literal(&ir, rule_body(&ir, "single"), "left=>right");
    assert_literal(&ir, rule_body(&ir, "backtick"), "left->right");
    assert_regex(&ir, rule_body(&ir, "regex"), "left=>right|next->item");
    assert!(ir.rules.iter().all(|rule| count_maps(&rule.body) == 0));
    assert!(ir.fns.is_empty());
}

#[test]
fn direct_typed_map_stays_on_its_source_factor_with_exact_descriptor() {
    let ir = lower_grammar(r#"subject = "before", "mapped" -> 7u8, "after" ;"#);
    let IrNode::Seq(items) = rule_body(&ir, "subject") else {
        panic!("direct-map fixture must remain a sequence");
    };
    let [before, mapped, after] = items.as_slice() else {
        panic!("direct-map fixture must contain exactly three sequence items");
    };

    assert_literal(&ir, before, "before");
    let inner = assert_uint_map(&ir, mapped, 7, TypeDesc::U8);
    assert_literal(&ir, inner, "mapped");
    assert_literal(&ir, after, "after");
    assert_eq!(count_maps(rule_body(&ir, "subject")), 1);
    assert_eq!(ir.fns.len(), 1);
}

#[test]
fn explicit_annotations_and_qualified_call_keep_exact_utf8_bounds() {
    let ir = lower_grammar(
        r#"
scalar = "λ", /* before / */ "scalar" /* after / */ -> input : u8 ;
named = "前", /* before / */ "named" /* after / */
      -> crate::codec::decode(input) : Payload ;
"#,
    );

    assert_eq!(
        mapped_after_prefix(&ir, "scalar", "λ", "scalar"),
        &FnDescriptor::Expr {
            expr: MapExpr::Input,
            return_type: Some(TypeDesc::U8),
        },
    );

    let FnDescriptor::Expr {
        expr: MapExpr::FnCall { name, args },
        return_type: Some(TypeDesc::Named(type_name)),
    } = mapped_after_prefix(&ir, "named", "前", "named")
    else {
        panic!("named fixture must retain an exact typed function descriptor");
    };
    assert_eq!(ir.get_string(*name), "crate::codec::decode");
    assert_eq!(args.as_slice(), &[MapExpr::Input]);
    assert_eq!(ir.get_string(*type_name), "Payload");

    assert_eq!(ir.fns.len(), 2);
}

#[test]
fn block_comment_slash_arrow_is_not_a_map() {
    let ir = lower_grammar(r#"subject = /*a/b -> c*/ "x" ;"#);
    let body = rule_body(&ir, "subject");

    assert_literal(&ir, body, "x");
    assert_eq!(count_maps(body), 0);
    assert!(ir.fns.is_empty());
}

#[test]
fn division_inside_group_does_not_hide_outer_typed_map() {
    let ir = lower_grammar(r#"subject = ("x" -> input / 2 : i32) -> 1u8 ;"#);
    let body = rule_body(&ir, "subject");
    let IrNode::Map {
        inner: inner_map,
        fn_id: outer_fn,
    } = body
    else {
        panic!("expected outer typed Map, got {body:#?}");
    };
    assert_eq!(
        ir.fns.get(*outer_fn as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::UIntLit(1),
            return_type: Some(TypeDesc::U8),
        }),
    );

    let IrNode::Map {
        inner: literal,
        fn_id: inner_fn,
    } = inner_map.as_ref()
    else {
        panic!("expected inner division Map, got {inner_map:#?}");
    };
    assert_eq!(
        ir.fns.get(*inner_fn as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::BinOp {
                op: MapBinOp::Div,
                operand_domain: Some(bbnf_ir::NumericDomain::I32),
                lhs: Box::new(MapExpr::Input),
                rhs: Box::new(MapExpr::IntLit(2)),
            },
            return_type: Some(TypeDesc::I32),
        }),
    );
    assert_literal(&ir, literal, "x");
    assert_eq!(count_maps(body), 2);
    assert_eq!(ir.fns.len(), 2);
}

#[test]
fn numeric_suffix_recovery_is_confined_to_one_structural_literal() {
    let ir = lower_grammar(
        r#"
single = "x" -> 1u8 ;
compound = "y" -> 1 + input ;
"#,
    );

    let single_inner = assert_uint_map(&ir, rule_body(&ir, "single"), 1, TypeDesc::U8);
    assert_literal(&ir, single_inner, "x");

    let compound = rule_body(&ir, "compound");
    let IrNode::Map { inner, fn_id } = compound else {
        panic!("expected compound expression Map, got {compound:#?}");
    };
    assert_literal(&ir, inner, "y");
    assert_eq!(
        ir.fns.get(*fn_id as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::BinOp {
                op: MapBinOp::Add,
                operand_domain: Some(bbnf_ir::NumericDomain::I64),
                lhs: Box::new(MapExpr::IntLit(1)),
                rhs: Box::new(MapExpr::Input),
            },
            return_type: None,
        }),
    );
    assert!(ir.fns.iter().all(|descriptor| !matches!(
        descriptor,
        FnDescriptor::Expr {
            return_type: Some(TypeDesc::Named(_)),
            ..
        }
    )));
    assert_eq!(ir.fns.len(), 2);
}

#[test]
fn integer_spelling_preserves_signedness_and_full_u64_domain() {
    let ir = lower_grammar(
        r#"
plain = "p" -> 42 ;
signed = "s" -> 42i64 ;
small = "u" -> 42u8 ;
wide = "w" -> 18446744073709551615u64 ;
hex = "h" -> 0xffffffffffffffffu64 ;
"#,
    );

    let plain = assert_int_map(&ir, rule_body(&ir, "plain"), 42, None);
    assert_literal(&ir, plain, "p");
    let signed = assert_int_map(&ir, rule_body(&ir, "signed"), 42, Some(TypeDesc::I64));
    assert_literal(&ir, signed, "s");
    let small = assert_uint_map(&ir, rule_body(&ir, "small"), 42, TypeDesc::U8);
    assert_literal(&ir, small, "u");
    let wide = assert_uint_map(&ir, rule_body(&ir, "wide"), u64::MAX, TypeDesc::U64);
    assert_literal(&ir, wide, "w");
    let hex = assert_uint_map(&ir, rule_body(&ir, "hex"), u64::MAX, TypeDesc::U64);
    assert_literal(&ir, hex, "h");
}

#[test]
fn signed_minima_lower_as_one_literal_operation_with_their_suffixes() {
    let ir = lower_grammar(
        r#"
i8_min = "a" -> -128i8 ;
i16_min = "b" -> -32768i16 ;
i32_min = "c" -> -2147483648i32 ;
i64_min = "d" -> -9223372036854775808i64 ;
plain_min = "e" -> -9223372036854775808 ;
hex_i8_min = "f" -> -0x80i8 ;
"#,
    );

    for (rule, expected, return_type, literal) in [
        ("i8_min", i8::MIN as i64, Some(TypeDesc::I8), "a"),
        ("i16_min", i16::MIN as i64, Some(TypeDesc::I16), "b"),
        ("i32_min", i32::MIN as i64, Some(TypeDesc::I32), "c"),
        ("i64_min", i64::MIN, Some(TypeDesc::I64), "d"),
        ("plain_min", i64::MIN, None, "e"),
        ("hex_i8_min", i8::MIN as i64, Some(TypeDesc::I8), "f"),
    ] {
        let inner = assert_int_map(&ir, rule_body(&ir, rule), expected, return_type);
        assert_literal(&ir, inner, literal);
    }
}

#[test]
fn numeric_domain_survives_parentheses_and_same_domain_composition() {
    let ir = lower_grammar(
        r#"
direct = "a" -> 7u8 ;
paren = "b" -> (7u8) ;
composite = "c" -> 40u8 + 2u8 ;
nested = "d" -> (20u16 + 1u16) * 2u16 ;
signed = "e" -> 100i8 - 58i8 ;
wide = "f" -> 9223372036854775806i64 + 1i64 ;
float = "g" -> (1.25f64 + 2.5f64) ;
annotated_i8 = "h" -> 40 + 2 : i8 ;
annotated_i16 = "i" -> (300 + 20) : i16 ;
"#,
    );

    for (rule, value, ty, literal) in [
        ("direct", 7, TypeDesc::U8, "a"),
        ("paren", 7, TypeDesc::U8, "b"),
        ("composite", 42, TypeDesc::U8, "c"),
        ("nested", 42, TypeDesc::U16, "d"),
    ] {
        let inner = assert_uint_map(&ir, rule_body(&ir, rule), value, ty);
        assert_literal(&ir, inner, literal);
    }

    for (rule, value, ty, literal) in [
        ("signed", 42, TypeDesc::I8, "e"),
        ("wide", i64::MAX, TypeDesc::I64, "f"),
        ("annotated_i8", 42, TypeDesc::I8, "h"),
        ("annotated_i16", 320, TypeDesc::I16, "i"),
    ] {
        let inner = assert_int_map(&ir, rule_body(&ir, rule), value, Some(ty));
        assert_literal(&ir, inner, literal);
    }

    let IrNode::Map { inner, fn_id } = rule_body(&ir, "float") else {
        panic!("float must lower to a Map");
    };
    assert_eq!(
        ir.fns.get(*fn_id as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::FloatLit(3.75),
            return_type: Some(TypeDesc::F64),
        })
    );
    assert_literal(&ir, inner, "g");
}

#[test]
fn mixed_domains_and_intermediate_overflow_fail_before_ir_deposition() {
    for source in [
        r#"subject = "x" -> 1u8 + 2u16 ;"#,
        r#"subject = "x" -> 1i8 + 2i16 ;"#,
        r#"subject = "x" -> 1u8 + 2i8 ;"#,
        r#"subject = "x" -> 1.0f64 + 2i64 ;"#,
        r#"subject = "x" -> 127i8 + 1i8 ;"#,
        r#"subject = "x" -> (100i8 + 100i8) - 100i8 ;"#,
        r#"subject = "x" -> 0u8 - 1u8 ;"#,
        r#"subject = "x" -> 255u8 + 1u8 ;"#,
        r#"subject = "x" -> 1u8 / 0u8 ;"#,
        r#"subject = "x" -> -(-128i8) ;"#,
        r#"subject = "x" -> input + 1 ;"#,
        r#"subject = "x" -> input < 1 ;"#,
    ] {
        assert!(
            std::panic::catch_unwind(|| lower_grammar(source)).is_err(),
            "invalid numeric expression must fail before entering the IR: {source}"
        );
    }
}

#[test]
fn each_signed_boundary_rejects_both_adjacent_overflow_spellings() {
    for source in [
        r#"subject = "x" -> 128i8 ;"#,
        r#"subject = "x" -> -129i8 ;"#,
        r#"subject = "x" -> 32768i16 ;"#,
        r#"subject = "x" -> -32769i16 ;"#,
        r#"subject = "x" -> 2147483648i32 ;"#,
        r#"subject = "x" -> -2147483649i32 ;"#,
        r#"subject = "x" -> 9223372036854775808i64 ;"#,
        r#"subject = "x" -> -9223372036854775809i64 ;"#,
        r#"subject = "x" -> 9223372036854775808 ;"#,
        r#"subject = "x" -> -9223372036854775809 ;"#,
    ] {
        assert!(
            std::panic::catch_unwind(|| lower_grammar(source)).is_err(),
            "adjacent signed overflow must fail before entering the IR: {source}"
        );
    }
}

#[test]
fn unary_noninteger_atoms_do_not_enter_the_integer_literal_parser() {
    let ir = lower_grammar(
        r#"
input_neg = "a" -> -input ;
ident_neg = "b" -> -foo ;
paren_neg = "c" -> -(input) ;
call_neg = "d" -> -decode(input) ;
float_neg = "e" -> -1.5f64 ;
bool_not = "f" -> !false ;
"#,
    );

    for (rule, op) in [
        ("input_neg", MapUnaryOp::Neg),
        ("ident_neg", MapUnaryOp::Neg),
        ("paren_neg", MapUnaryOp::Neg),
        ("call_neg", MapUnaryOp::Neg),
        ("bool_not", MapUnaryOp::Not),
    ] {
        let IrNode::Map { fn_id, .. } = rule_body(&ir, rule) else {
            panic!("{rule} must retain its map");
        };
        let Some(FnDescriptor::Expr {
            expr: MapExpr::UnaryOp { op: actual, .. },
            return_type,
        }) = ir.fns.get(*fn_id as usize)
        else {
            panic!("{rule} must lower as a unary expression");
        };
        assert_eq!(*actual, op);
        assert_ne!(return_type, &Some(TypeDesc::F64));
    }

    let IrNode::Map { fn_id, .. } = rule_body(&ir, "float_neg") else {
        panic!("float_neg must retain its map");
    };
    assert_eq!(
        ir.fns.get(*fn_id as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::FloatLit(-1.5),
            return_type: Some(TypeDesc::F64),
        }),
        "constant unary f64 must use the shared checked fold",
    );
}

#[test]
fn invalid_overflowing_and_negated_unsigned_literals_fail_closed() {
    for source in [
        r#"subject = "x" -> 18446744073709551616u64 ;"#,
        r#"subject = "x" -> 256u8 ;"#,
        r#"subject = "x" -> 1wat ;"#,
        r#"subject = "x" -> -1u64 ;"#,
        r#"subject = "x" -> 1.0e999 ;"#,
        r#"subject = "x" -> 3.5e38f32 ;"#,
    ] {
        assert!(
            std::panic::catch_unwind(|| lower_grammar(source)).is_err(),
            "invalid literal must fail closed: {source}"
        );
    }
}

#[test]
fn malformed_float_suffixes_and_exponents_are_rejected_by_the_grammar() {
    for source in [
        r#"subject = "x" -> 1.0wat ;"#,
        r#"subject = "x" -> 1.0f16 ;"#,
        r#"subject = "x" -> 1.0f64wat ;"#,
        r#"subject = "x" -> 1.0e ;"#,
        r#"subject = "x" -> 1.0e+ ;"#,
        r#"subject = "x" -> 1.0e-f64 ;"#,
    ] {
        assert!(
            crate::grammar::parse(source).is_err(),
            "the grammar must reject a malformed float token: {source}"
        );
    }
}

#[test]
fn float_spellings_use_f64_transport_and_f32_source_rounding() {
    let ir = lower_grammar(
        r#"
plain = "p" -> 1.5 ;
single = "s" -> 1.5f32 ;
double = "d" -> 1.5f64 ;
rounded = "r" -> 16777217.0f32 ;
"#,
    );
    for (rule, value, return_type) in [
        ("plain", 1.5, None),
        ("single", 1.5, Some(TypeDesc::F64)),
        ("double", 1.5, Some(TypeDesc::F64)),
        // MapExpr has one floating carrier. The f32 spelling rounds first,
        // then promotes that exact rounded value into the f64 carrier.
        ("rounded", 16_777_216.0, Some(TypeDesc::F64)),
    ] {
        let IrNode::Map { inner, fn_id } = rule_body(&ir, rule) else {
            panic!("{rule} must lower to a Map");
        };
        assert_eq!(
            ir.fns.get(*fn_id as usize),
            Some(&FnDescriptor::Expr {
                expr: MapExpr::FloatLit(value),
                return_type,
            })
        );
        assert_literal(&ir, inner, &rule[..1]);
    }
}
