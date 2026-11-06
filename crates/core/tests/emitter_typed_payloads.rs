//! R7 emitter contracts for exact scalar payloads and mapped continuations.

use bbnf::backend::rust::emitter::shapes::{
    PayloadPlanner, alt_dispatch, flat, hregex, keyword, scalar, wrap,
};
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::registry::{
    EmitStrategy, FieldSource, LayoutKind, StructField, StructLayout, SubstrateBinding,
};
use bbnf_ir::{
    AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, MapBinOp, MapExpr, RuleMeta,
    TokenDispatchArm, TypeDesc,
};

fn strategy() -> EmitStrategy {
    EmitStrategy::StructDirect {
        rust: SubstrateBinding {
            builder_path: "crate::runtime::bbnf::BbnfStructBuilder",
            document_path: "crate::runtime::bbnf::BbnfDocument",
        },
        ts: None,
        wasm: None,
    }
}

fn formatted(tokens: proc_macro2::TokenStream) -> String {
    let file: syn::File = syn::parse2(tokens).expect("emitter output must be valid Rust");
    prettyplease::unparse(&file)
}

fn discarded_builder_region(emitted: &str) -> (usize, usize) {
    let start = emitted
        .find("let __discarded_builder_checkpoint = builder.checkpoint()")
        .expect("discarded side must begin a builder-only transaction");
    let end = emitted[start..]
        .find("builder.rollback(__discarded_builder_checkpoint)")
        .map(|offset| start + offset)
        .expect("discarded side must roll its builder effects back");
    (start, end)
}

fn scalar_fn(value: i64, ty: TypeDesc) -> FnDescriptor {
    FnDescriptor::Expr {
        expr: MapExpr::IntLit(value),
        return_type: Some(ty),
    }
}

fn branch(node: IrNode) -> AltBranch {
    AltBranch {
        node,
        first_set: None,
    }
}

fn rule(id: u32, name: u32, body: IrNode) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

fn hregex_ir(return_type: TypeDesc) -> GrammarIR {
    let mut ir = GrammarIR::default();
    ir.strings = vec![
        "integer".into(),
        r"-?(?:0[xX][0-9a-fA-F]+|[0-9]+)\w*".into(),
    ];
    ir.fns.push(FnDescriptor::Expr {
        expr: MapExpr::Input,
        return_type: Some(return_type),
    });
    ir.rules.push(rule(
        0,
        0,
        IrNode::Map {
            inner: Box::new(IrNode::Regex(1)),
            fn_id: 0,
        },
    ));
    ir
}

#[test]
fn hregex_integer_inputs_are_suffix_aware_fallible_and_exact_width() {
    let u8_ir = hregex_ir(TypeDesc::U8);
    let u8_tokens = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &u8_ir.rules[0],
        &u8_ir,
    ));
    assert!(u8_tokens.contains("strip_suffix(\"u8\")"));
    assert!(u8_tokens.contains("parse::<u8>()"));
    assert!(u8_tokens.contains("push_leaf_with_u8(builder, __decoded)"));
    assert!(u8_tokens.contains("*p = span_lo as usize"));
    assert!(!u8_tokens.contains("unwrap_or(0)"));

    let u64_ir = hregex_ir(TypeDesc::U64);
    let u64_tokens = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &u64_ir.rules[0],
        &u64_ir,
    ));
    assert!(u64_tokens.contains("strip_suffix(\"u64\")"));
    assert!(u64_tokens.contains("parse::<u64>()"));
    assert!(u64_tokens.contains("push_leaf_with_u64(builder, __decoded as u64)"));
    assert!(!u64_tokens.contains("unwrap_or(0)"));
}

#[test]
fn signed_hex_decoder_admits_minimum_magnitude_and_rejects_overflow() {
    let ir = hregex_ir(TypeDesc::I8);
    let emitted = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));

    // `-0x80i8` must parse its magnitude outside i8 before negation;
    // conversion back to i8 is the exact min/max and overflow gate.
    assert!(emitted.contains("u128::from_str_radix(hex, 16)"));
    assert!(emitted.contains("i128::try_from(magnitude)"));
    assert!(emitted.contains("i8::try_from(value)"));
    assert!(!emitted.contains("i8::from_str_radix(hex, 16)"));
    assert!(emitted.contains("Result::Err(())"));
    assert!(emitted.contains("*p = span_lo as usize"));
}

#[test]
fn hregex_constants_preserve_u8_max_wide_u64_and_negative_i64() {
    let cases = [
        (255, TypeDesc::U8, "push_leaf_with_u8(builder, 255u8)"),
        (
            i64::from(u32::MAX) + 1,
            TypeDesc::U64,
            "push_leaf_with_u64(builder, 4294967296u64)",
        ),
        (-17, TypeDesc::I64, "push_leaf_with_i64(builder, -17i64)"),
    ];

    for (value, ty, expected) in cases {
        let mut ir = hregex_ir(ty.clone());
        ir.fns[0] = scalar_fn(value, ty);
        let emitted = formatted(hregex::emit_parse_hregex(
            &strategy(),
            "Fixture",
            &ir.rules[0],
            &ir,
        ));
        assert!(emitted.contains(expected), "missing {expected}:\n{emitted}");
    }

    let mut exact_u64 = hregex_ir(TypeDesc::U64);
    exact_u64.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::UIntLit(u64::MAX),
        return_type: Some(TypeDesc::U64),
    };
    let emitted = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &exact_u64.rules[0],
        &exact_u64,
    ));
    assert!(
        emitted.contains("push_leaf_with_u64(builder, 18446744073709551615u64)"),
        "full-domain u64 literal was not emitted exactly:\n{emitted}"
    );
}

#[test]
fn hregex_routes_float_and_bool_constants_and_rejects_type_contradictions() {
    let cases = [
        (
            FnDescriptor::Expr {
                expr: MapExpr::FloatLit(1.5),
                return_type: Some(TypeDesc::F64),
            },
            "push_leaf_with_f64(builder, 1.5f64)",
        ),
        (
            FnDescriptor::Expr {
                expr: MapExpr::BoolLit(true),
                return_type: Some(TypeDesc::Bool),
            },
            "push_leaf_with_bool(builder, true)",
        ),
    ];
    for (descriptor, expected) in cases {
        let mut ir = hregex_ir(TypeDesc::U8);
        ir.fns[0] = descriptor;
        let emitted = formatted(hregex::emit_parse_hregex(
            &strategy(),
            "Fixture",
            &ir.rules[0],
            &ir,
        ));
        assert!(emitted.contains(expected), "missing {expected}:\n{emitted}");
    }

    let mut contradictory = hregex_ir(TypeDesc::U8);
    contradictory.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::BoolLit(true),
        return_type: Some(TypeDesc::F64),
    };
    let emitted = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &contradictory.rules[0],
        &contradictory,
    ));
    assert!(emitted.contains("boolean map constant has a non-boolean return type"));

    contradictory.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::UIntLit(u64::MAX),
        return_type: Some(TypeDesc::I64),
    };
    let emitted = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &contradictory.rules[0],
        &contradictory,
    ));
    assert!(emitted.contains("numeric literal is out of range"));

    contradictory.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::UIntLit(255),
        return_type: Some(TypeDesc::I8),
    };
    let emitted = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &contradictory.rules[0],
        &contradictory,
    ));
    assert!(emitted.contains("numeric literal is out of range"));
}

fn mapped_branch_ir(branches: Vec<AltBranch>, fns: Vec<FnDescriptor>) -> GrammarIR {
    let mut ir = GrammarIR::default();
    ir.strings = vec!["subject".into(), "target".into(), "x".into(), "y".into()];
    ir.fns = fns;
    ir.rules.push(rule(0, 0, IrNode::Alt(branches, None)));
    ir.rules.push(rule(1, 1, IrNode::Literal(2)));
    ir.shape_assignments.assign(1, ShapeTag::Keyword);
    ir
}

#[test]
fn alt_ref_maps_run_once_after_child_success() {
    let ir = mapped_branch_ir(
        vec![
            branch(IrNode::Map {
                inner: Box::new(IrNode::Ref(1)),
                fn_id: 0,
            }),
            branch(IrNode::Map {
                inner: Box::new(IrNode::Ref(1)),
                fn_id: 1,
            }),
        ],
        vec![
            scalar_fn(255, TypeDesc::U8),
            scalar_fn(4_294_967_296, TypeDesc::U64),
        ],
    );
    let emitted = formatted(alt_dispatch::emit_parse_alt_dispatch(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));

    assert_eq!(
        emitted.matches("push_leaf_with_u8(builder, 255u8)").count(),
        1
    );
    assert_eq!(
        emitted
            .matches("push_leaf_with_u64(builder, 4294967296u64)")
            .count(),
        1
    );
    let child = emitted.find("parse_keyword_Fixture_target").unwrap();
    let payload = emitted.find("push_leaf_with_u8(builder, 255u8)").unwrap();
    let commit = emitted.find("builder.commit(attempt_builder)").unwrap();
    assert!(child < payload && payload < commit);
    assert!(emitted.contains("builder.rollback(attempt_builder)"));
}

#[test]
fn mapped_mixed_sequence_continuation_is_inside_the_transaction() {
    let mixed = IrNode::Map {
        inner: Box::new(IrNode::Seq(vec![IrNode::Literal(3), IrNode::Ref(1)])),
        fn_id: 0,
    };
    let ir = mapped_branch_ir(vec![branch(mixed)], vec![scalar_fn(-9, TypeDesc::I64)]);

    let alt = formatted(alt_dispatch::emit_parse_alt_dispatch(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    assert_eq!(alt.matches("push_leaf_with_i64(builder, -9i64)").count(), 1);
    let alt_replacement = alt
        .find("builder.rollback(__map_builder_checkpoint)")
        .expect("Alt scalar Map must replace its inner builder effects");
    assert!(alt_replacement < alt.find("push_leaf_with_i64(builder, -9i64)").unwrap());
    assert!(alt.contains("builder.rollback(attempt_builder)"));
    assert!(
        alt.find("push_leaf_with_i64(builder, -9i64)").unwrap()
            < alt.find("builder.commit(attempt_builder)").unwrap()
    );

    let wrapped = formatted(wrap::emit_parse_wrap(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    assert_eq!(
        wrapped
            .matches("push_leaf_with_i64(builder, -9i64)")
            .count(),
        1
    );
    let replacement = wrapped
        .find("builder.rollback(__map_builder_checkpoint)")
        .expect("Wrap scalar Map must replace its inner builder effects");
    assert!(replacement < wrapped.find("push_leaf_with_i64(builder, -9i64)").unwrap());
    assert!(wrapped.contains("builder.rollback(attempt_builder)"));
    assert!(
        wrapped.find("push_leaf_with_i64(builder, -9i64)").unwrap()
            < wrapped.find("builder.commit(attempt_builder)").unwrap()
    );
}

#[test]
fn bare_sequence_child_map_owns_one_payload_without_outer_duplication() {
    let bare = IrNode::Seq(vec![
        IrNode::Literal(3),
        IrNode::Map {
            inner: Box::new(IrNode::Ref(1)),
            fn_id: 0,
        },
    ]);
    let ir = mapped_branch_ir(vec![branch(bare)], vec![scalar_fn(7, TypeDesc::U8)]);
    let alt = formatted(alt_dispatch::emit_parse_alt_dispatch(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));

    assert_eq!(
        alt.matches("push_leaf_with_u8(builder, 7u8)").count(),
        1,
        "the inline child Map owns exactly one payload"
    );
    assert!(alt.contains("builder.rollback(attempt_builder)"));

    let wrapped = formatted(wrap::emit_parse_wrap(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    assert_eq!(
        wrapped.matches("push_leaf_with_u8(builder, 7u8)").count(),
        1,
        "Wrap must consume the same inline child-map ownership"
    );
    assert!(wrapped.contains("builder.rollback(attempt_builder)"));
}

#[test]
fn keyword_constant_uses_signed_builder_surface() {
    let mut ir = GrammarIR::default();
    ir.strings = vec!["signed".into(), "x".into()];
    ir.fns.push(scalar_fn(-1, TypeDesc::I64));
    ir.rules.push(rule(
        0,
        0,
        IrNode::Map {
            inner: Box::new(IrNode::Literal(1)),
            fn_id: 0,
        },
    ));

    let emitted = formatted(keyword::emit_parse_keyword(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    assert!(emitted.contains("push_leaf_with_i64(-1i64)"));
    assert!(!emitted.contains("18446744073709551615"));
}

#[test]
fn folded_and_untyped_int_literals_stay_on_the_signed_rust_surface() {
    let mut ir = hregex_ir(TypeDesc::I8);
    ir.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::BinOp {
            op: MapBinOp::Add,
            operand_domain: None,
            lhs: Box::new(MapExpr::IntLit(40)),
            rhs: Box::new(MapExpr::IntLit(2)),
        },
        return_type: Some(TypeDesc::I8),
    };
    let folded = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(folded.contains("push_leaf_with_i64(builder, 42i64)"));

    ir.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::IntLit(42),
        return_type: None,
    };
    let untyped = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(untyped.contains("push_leaf_with_i64(builder, 42i64)"));
    assert!(!untyped.contains("push_leaf_with_u64(builder, 42u64)"));

    ir.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::BinOp {
            op: MapBinOp::Add,
            operand_domain: None,
            lhs: Box::new(MapExpr::IntLit(i64::MAX)),
            rhs: Box::new(MapExpr::IntLit(1)),
        },
        return_type: Some(TypeDesc::I64),
    };
    let overflow = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(overflow.contains("numeric expression overflows its domain"));

    ir.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::BinOp {
            op: MapBinOp::Add,
            operand_domain: None,
            lhs: Box::new(MapExpr::FloatLit(f64::MAX)),
            rhs: Box::new(MapExpr::FloatLit(f64::MAX)),
        },
        return_type: Some(TypeDesc::F64),
    };
    let float_overflow = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(float_overflow.contains("floating-point expression is not finite"));

    ir.fns[0] = FnDescriptor::Expr {
        expr: MapExpr::FloatLit(f64::INFINITY),
        return_type: Some(TypeDesc::F64),
    };
    let non_finite = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(non_finite.contains("floating-point expression is not finite"));
}

#[test]
fn nested_hregex_maps_use_only_the_proven_outer_replacement() {
    let mut ir = hregex_ir(TypeDesc::I64);
    ir.fns[0] = scalar_fn(7, TypeDesc::I64);
    ir.fns.push(scalar_fn(9, TypeDesc::I64));
    ir.rules[0].body = IrNode::Map {
        inner: Box::new(IrNode::Map {
            inner: Box::new(IrNode::Regex(1)),
            fn_id: 0,
        }),
        fn_id: 1,
    };

    let emitted = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert_eq!(
        emitted.matches("push_leaf_with_i64(builder, 9i64)").count(),
        1
    );
    assert!(
        !emitted.contains("push_leaf_with_i64(builder, 7i64)"),
        "the outer Map replaces, rather than supplements, the nested payload"
    );

    ir.fns[1] = FnDescriptor::Expr {
        expr: MapExpr::Input,
        return_type: Some(TypeDesc::I64),
    };
    let dynamic = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(
        dynamic.contains("nested HRegex Map is not a proven constant replacement"),
        "a nested dynamic replacement must fail closed without a rollback authority"
    );

    ir.rules[0].body = IrNode::Map {
        inner: Box::new(IrNode::OptionalWhitespace(Box::new(IrNode::Regex(1)))),
        fn_id: 1,
    };
    let direct_with_whitespace = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        &ir,
    ));
    assert!(
        !direct_with_whitespace.contains("nested HRegex Map is not a proven constant replacement"),
        "OptionalWhitespace around the Regex does not create a nested Map"
    );
    assert!(direct_with_whitespace.contains("parse::<i64>()"));
    assert_eq!(
        direct_with_whitespace
            .matches("skip_space(input, p, state)")
            .count(),
        2,
        "OptionalWhitespace must preserve both structural bookends"
    );
}

fn mapped_regex(fn_id: u32) -> IrNode {
    IrNode::Map {
        inner: Box::new(IrNode::Regex(4)),
        fn_id,
    }
}

fn flat_ir(body: IrNode, fns: Vec<FnDescriptor>, targets: Vec<IrNode>) -> GrammarIR {
    let mut ir = GrammarIR::default();
    ir.strings = vec![
        "subject".into(),
        "target".into(),
        "x".into(),
        "y".into(),
        "x".into(),
        "semantic".into(),
    ];
    ir.fns = fns;
    ir.rules.push(rule(0, 0, body));
    for (index, target) in targets.into_iter().enumerate() {
        let id = index as u32 + 1;
        ir.rules.push(rule(id, 1, target));
        ir.shape_assignments.assign(id, ShapeTag::Keyword);
    }
    ir
}

fn emit_flat(ir: &GrammarIR) -> String {
    formatted(flat::emit_parse_flat(
        &strategy(),
        "Fixture",
        &ir.rules[0],
        ir,
        &PayloadPlanner::new(ir),
    ))
}

#[test]
fn scalar_map_replaces_nested_inner_payload_before_its_one_deposit() {
    let nested = IrNode::Seq(vec![
        IrNode::Literal(2),
        IrNode::Map {
            inner: Box::new(IrNode::Map {
                inner: Box::new(IrNode::Ref(1)),
                fn_id: 0,
            }),
            fn_id: 1,
        },
    ]);
    let ir = mapped_branch_ir(
        vec![branch(nested)],
        vec![scalar_fn(7, TypeDesc::I64), scalar_fn(9, TypeDesc::I64)],
    );
    let emitted = formatted(alt_dispatch::emit_parse_alt_dispatch(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));

    let inner_push = emitted
        .find("push_leaf_with_i64(builder, 7i64)")
        .expect("inner payload tokens");
    let outer_rollback = emitted
        .rfind("builder.rollback(__map_builder_checkpoint)")
        .expect("outer map rollback");
    let outer_push = emitted
        .find("push_leaf_with_i64(builder, 9i64)")
        .expect("outer payload");
    assert!(inner_push < outer_rollback && outer_rollback < outer_push);
    assert_eq!(
        emitted
            .matches("builder.rollback(__map_builder_checkpoint)")
            .count(),
        2,
        "each real nested Map must replace its own inner effects"
    );
    assert_eq!(
        emitted.matches("push_leaf_with_i64(builder, 9i64)").count(),
        1
    );
}

#[test]
fn flat_preserves_map_ref_seq_string_and_dynamic_passthrough_boundaries() {
    let target = mapped_regex(0);
    let mut mapped_ref = flat_ir(
        IrNode::Map {
            inner: Box::new(IrNode::Ref(1)),
            fn_id: 1,
        },
        vec![scalar_fn(7, TypeDesc::I64), scalar_fn(9, TypeDesc::I64)],
        vec![target.clone()],
    );
    mapped_ref.shape_assignments.assign(1, ShapeTag::HRegex);
    let mapped_ref = emit_flat(&mapped_ref);
    let child = mapped_ref.find("parse_hregex_Fixture_target").unwrap();
    let rollback = mapped_ref
        .find("builder.rollback(__map_builder_checkpoint)")
        .unwrap();
    let payload = mapped_ref
        .find("push_leaf_with_i64(builder, 9i64)")
        .unwrap();
    assert!(child < rollback && rollback < payload);
    assert_eq!(
        mapped_ref
            .matches("push_leaf_with_i64(builder, 9i64)")
            .count(),
        1
    );

    let mapped_seq = flat_ir(
        IrNode::Map {
            inner: Box::new(IrNode::Seq(vec![mapped_regex(0), IrNode::Literal(2)])),
            fn_id: 1,
        },
        vec![scalar_fn(7, TypeDesc::I64), scalar_fn(9, TypeDesc::I64)],
        vec![],
    );
    let mapped_seq = emit_flat(&mapped_seq);
    let inner = mapped_seq
        .find("push_leaf_with_i64(builder, 7i64)")
        .unwrap();
    let rollback = mapped_seq
        .rfind("builder.rollback(__map_builder_checkpoint)")
        .unwrap();
    let outer = mapped_seq
        .find("push_leaf_with_i64(builder, 9i64)")
        .unwrap();
    assert!(inner < rollback && rollback < outer);

    let mut mapped_string = flat_ir(
        IrNode::Map {
            inner: Box::new(IrNode::Ref(1)),
            fn_id: 1,
        },
        vec![
            scalar_fn(7, TypeDesc::I64),
            FnDescriptor::Expr {
                expr: MapExpr::StringLit(5),
                return_type: None,
            },
        ],
        vec![target.clone()],
    );
    mapped_string.shape_assignments.assign(1, ShapeTag::HRegex);
    let mapped_string = emit_flat(&mapped_string);
    assert_eq!(
        mapped_string
            .matches("push_leaf_with_str(builder, \"semantic\")")
            .count(),
        1
    );
    assert!(
        mapped_string
            .find("builder.rollback(__map_builder_checkpoint)")
            .unwrap()
            < mapped_string
                .find("push_leaf_with_str(builder, \"semantic\")")
                .unwrap()
    );

    let mut unsupported_dynamic = flat_ir(
        IrNode::Map {
            inner: Box::new(IrNode::Ref(1)),
            fn_id: 1,
        },
        vec![
            scalar_fn(7, TypeDesc::I64),
            FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(TypeDesc::I64),
            },
        ],
        vec![target],
    );
    unsupported_dynamic
        .shape_assignments
        .assign(1, ShapeTag::HRegex);
    let unsupported_dynamic = emit_flat(&unsupported_dynamic);
    assert!(
        unsupported_dynamic.contains("Flat emitted payload arity does not exactly agree"),
        "a dynamic Map without an owned replacement emitter must fail closed"
    );
}

#[test]
fn flat_skip_and_next_keep_only_their_semantic_side() {
    let scalar = scalar_fn(7, TypeDesc::I64);

    let skip_kept = emit_flat(&flat_ir(
        IrNode::Skip(Box::new(mapped_regex(0)), Box::new(IrNode::Regex(4))),
        vec![scalar.clone()],
        vec![],
    ));
    let (discard_start, _) = discarded_builder_region(&skip_kept);
    assert!(skip_kept.find("push_leaf_with_i64(builder, 7i64)").unwrap() < discard_start);
    assert!(!skip_kept.contains("emitted payload arity does not exactly agree"));

    let skip_discarded = emit_flat(&flat_ir(
        IrNode::Skip(Box::new(IrNode::Regex(4)), Box::new(mapped_regex(0))),
        vec![scalar.clone()],
        vec![],
    ));
    let (discard_start, discard_end) = discarded_builder_region(&skip_discarded);
    let discarded_payload = skip_discarded
        .find("push_leaf_with_i64(builder, 7i64)")
        .unwrap();
    assert!(discard_start < discarded_payload && discarded_payload < discard_end);
    assert!(skip_discarded.contains("__span_slice"));
    assert!(!skip_discarded.contains("emitted payload arity does not exactly agree"));

    let next_discarded = emit_flat(&flat_ir(
        IrNode::Next(Box::new(mapped_regex(0)), Box::new(IrNode::Regex(4))),
        vec![scalar.clone()],
        vec![],
    ));
    let (discard_start, discard_end) = discarded_builder_region(&next_discarded);
    let discarded_payload = next_discarded
        .find("push_leaf_with_i64(builder, 7i64)")
        .unwrap();
    assert!(discard_start < discarded_payload && discarded_payload < discard_end);
    assert!(next_discarded.contains("__span_slice"));
    assert!(!next_discarded.contains("emitted payload arity does not exactly agree"));

    let next_kept = emit_flat(&flat_ir(
        IrNode::Next(Box::new(IrNode::Regex(4)), Box::new(mapped_regex(0))),
        vec![scalar],
        vec![],
    ));
    let (_, discard_end) = discarded_builder_region(&next_kept);
    assert!(discard_end < next_kept.find("push_leaf_with_i64(builder, 7i64)").unwrap());
    assert!(!next_kept.contains("emitted payload arity does not exactly agree"));
}

#[test]
fn flat_skip_and_next_apply_the_same_boundary_to_nested_refs() {
    fn emit(body: IrNode) -> String {
        let mut ir = flat_ir(
            body,
            vec![scalar_fn(7, TypeDesc::I64)],
            vec![mapped_regex(0)],
        );
        ir.shape_assignments.assign(1, ShapeTag::HRegex);
        emit_flat(&ir)
    }

    let skip_kept = emit(IrNode::Skip(
        Box::new(IrNode::Ref(1)),
        Box::new(IrNode::Literal(2)),
    ));
    let call = skip_kept.find("parse_hregex_Fixture_target").unwrap();
    let (discard_start, _) = discarded_builder_region(&skip_kept);
    assert!(call < discard_start);
    assert!(!skip_kept.contains("__span_slice"));

    let skip_discarded = emit(IrNode::Skip(
        Box::new(IrNode::Literal(2)),
        Box::new(IrNode::Ref(1)),
    ));
    let call = skip_discarded.find("parse_hregex_Fixture_target").unwrap();
    let (discard_start, discard_end) = discarded_builder_region(&skip_discarded);
    assert!(discard_start < call && call < discard_end);
    assert!(skip_discarded.contains("__span_slice"));

    let next_discarded = emit(IrNode::Next(
        Box::new(IrNode::Ref(1)),
        Box::new(IrNode::Literal(2)),
    ));
    let call = next_discarded.find("parse_hregex_Fixture_target").unwrap();
    let (discard_start, discard_end) = discarded_builder_region(&next_discarded);
    assert!(discard_start < call && call < discard_end);
    assert!(next_discarded.contains("__span_slice"));

    let next_kept = emit(IrNode::Next(
        Box::new(IrNode::Literal(2)),
        Box::new(IrNode::Ref(1)),
    ));
    let call = next_kept.find("parse_hregex_Fixture_target").unwrap();
    let (_, discard_end) = discarded_builder_region(&next_kept);
    assert!(discard_end < call);
    assert!(!next_kept.contains("__span_slice"));
}

#[test]
fn inline_skip_and_next_rollback_discarded_direct_maps_and_refs() {
    fn mapped_literal(sid: u32, fn_id: u32) -> IrNode {
        IrNode::Map {
            inner: Box::new(IrNode::Literal(sid)),
            fn_id,
        }
    }

    fn emit_wrap_branch(node: IrNode, fns: Vec<FnDescriptor>) -> String {
        let ir = mapped_branch_ir(vec![branch(node)], fns);
        formatted(wrap::emit_parse_wrap(
            "Fixture",
            &ir.rules[0],
            &ir,
            &strategy(),
            &PayloadPlanner::new(&ir),
        ))
    }

    let skip = emit_wrap_branch(
        IrNode::Skip(
            Box::new(mapped_literal(2, 0)),
            Box::new(mapped_literal(3, 1)),
        ),
        vec![scalar_fn(7, TypeDesc::I64), scalar_fn(9, TypeDesc::I64)],
    );
    let kept = skip.find("push_leaf_with_i64(builder, 7i64)").unwrap();
    let discarded = skip.find("push_leaf_with_i64(builder, 9i64)").unwrap();
    let (discard_start, discard_end) = discarded_builder_region(&skip);
    assert!(kept < discard_start && discard_start < discarded && discarded < discard_end);

    let next = emit_wrap_branch(
        IrNode::Next(
            Box::new(mapped_literal(2, 0)),
            Box::new(mapped_literal(3, 1)),
        ),
        vec![scalar_fn(7, TypeDesc::I64), scalar_fn(9, TypeDesc::I64)],
    );
    let discarded = next.find("push_leaf_with_i64(builder, 7i64)").unwrap();
    let kept = next.find("push_leaf_with_i64(builder, 9i64)").unwrap();
    let (discard_start, discard_end) = discarded_builder_region(&next);
    assert!(discard_start < discarded && discarded < discard_end && discard_end < kept);

    for node in [
        IrNode::Skip(Box::new(IrNode::Literal(3)), Box::new(IrNode::Ref(1))),
        IrNode::Next(Box::new(IrNode::Ref(1)), Box::new(IrNode::Literal(3))),
    ] {
        let mut ir = mapped_branch_ir(vec![branch(node)], vec![scalar_fn(7, TypeDesc::I64)]);
        ir.rules[1].body = mapped_literal(2, 0);
        let emitted = formatted(wrap::emit_parse_wrap(
            "Fixture",
            &ir.rules[0],
            &ir,
            &strategy(),
            &PayloadPlanner::new(&ir),
        ));
        let call = emitted.find("parse_keyword_Fixture_target").unwrap();
        let (discard_start, discard_end) = discarded_builder_region(&emitted);
        assert!(discard_start < call && call < discard_end);
    }
}

#[test]
fn string_literal_is_a_real_semantic_payload_not_a_span_suppression_guess() {
    let descriptor = FnDescriptor::Expr {
        expr: MapExpr::StringLit(5),
        return_type: None,
    };
    let ir = flat_ir(mapped_regex(0), vec![descriptor], vec![]);
    let flat = emit_flat(&ir);
    assert_eq!(
        flat.matches("push_leaf_with_str(builder, \"semantic\")")
            .count(),
        1
    );
    assert!(
        !flat.contains("__span_slice"),
        "a real StringLit payload must replace synthetic source-span emission"
    );

    let mut wrap_ir = mapped_branch_ir(
        vec![branch(IrNode::Map {
            inner: Box::new(IrNode::Literal(2)),
            fn_id: 0,
        })],
        vec![FnDescriptor::Expr {
            expr: MapExpr::StringLit(3),
            return_type: None,
        }],
    );
    wrap_ir.strings[3] = "semantic".into();
    let wrapped = formatted(wrap::emit_parse_wrap(
        "Fixture",
        &wrap_ir.rules[0],
        &wrap_ir,
        &strategy(),
        &PayloadPlanner::new(&wrap_ir),
    ));
    assert_eq!(
        wrapped
            .matches("push_leaf_with_str(builder, \"semantic\")")
            .count(),
        1
    );
}

#[test]
fn zero_and_path_dependent_payload_paths_never_suppress_or_duplicate_fallback() {
    let scalar = scalar_fn(1, TypeDesc::I8);

    let zero_repeat = flat_ir(
        IrNode::Repeat {
            inner: Box::new(mapped_regex(0)),
            lo: 0,
            hi: 1,
        },
        vec![scalar.clone()],
        vec![],
    );
    let zero_repeat = emit_flat(&zero_repeat);
    assert!(
        zero_repeat.contains("emitted payload arity does not exactly agree"),
        "zero-or-one Repeat must fail closed without a runtime/layout authority"
    );

    let exact_zero = flat_ir(
        IrNode::Repeat {
            inner: Box::new(mapped_regex(0)),
            lo: 0,
            hi: 0,
        },
        vec![scalar.clone()],
        vec![],
    );
    let exact_zero = emit_flat(&exact_zero);
    assert!(exact_zero.contains("if __iter_count >= 0u32"));
    assert!(exact_zero.contains("__span_slice"));
    assert!(!exact_zero.contains("emitted payload arity does not exactly agree"));

    let negate = flat_ir(
        IrNode::Negate(Box::new(mapped_regex(0))),
        vec![scalar.clone()],
        vec![],
    );
    let negate = emit_flat(&negate);
    assert!(negate.contains("__span_slice"));
    assert!(!negate.contains("emitted payload arity does not exactly agree"));

    let minus = flat_ir(
        IrNode::Minus(Box::new(IrNode::Regex(4)), Box::new(mapped_regex(0))),
        vec![scalar.clone()],
        vec![],
    );
    let minus = emit_flat(&minus);
    assert!(minus.contains("__span_slice"));
    assert!(!minus.contains("emitted payload arity does not exactly agree"));

    let partial_alt = flat_ir(
        IrNode::Alt(
            vec![branch(mapped_regex(0)), branch(IrNode::Regex(4))],
            None,
        ),
        vec![scalar.clone()],
        vec![],
    );
    assert!(emit_flat(&partial_alt).contains("emitted payload arity does not exactly agree"));

    let mut ref_epsilon = flat_ir(IrNode::Ref(1), vec![], vec![IrNode::Epsilon]);
    ref_epsilon.shape_assignments.assign(1, ShapeTag::None);
    let ref_epsilon = emit_flat(&ref_epsilon);
    assert!(ref_epsilon.contains("Flat Ref target has no emitted per-shape driver"));
    assert!(ref_epsilon.contains("emitted payload arity does not exactly agree"));

    let mut ref_partial = flat_ir(
        IrNode::Ref(1),
        vec![scalar],
        vec![IrNode::Alt(
            vec![branch(mapped_regex(0)), branch(IrNode::Regex(4))],
            None,
        )],
    );
    ref_partial.shape_assignments.assign(1, ShapeTag::None);
    assert!(emit_flat(&ref_partial).contains("emitted payload arity does not exactly agree"));

    let mut recursive_unknown = flat_ir(IrNode::Ref(1), vec![], vec![IrNode::Ref(1)]);
    recursive_unknown
        .shape_assignments
        .assign(1, ShapeTag::None);
    assert!(
        emit_flat(&recursive_unknown).contains("emitted payload arity does not exactly agree"),
        "an unproductive recursive Ref must remain conservative"
    );
}

#[test]
fn classified_refs_are_one_caller_visible_value_boundary() {
    let mut hregex = flat_ir(IrNode::Ref(1), vec![], vec![IrNode::Regex(4)]);
    hregex.shape_assignments.assign(1, ShapeTag::HRegex);
    let hregex = emit_flat(&hregex);
    assert!(!hregex.contains("__span_slice"));
    assert!(!hregex.contains("emitted payload arity does not exactly agree"));

    let mut flat_compound = flat_ir(
        IrNode::Ref(1),
        vec![],
        vec![IrNode::Seq(vec![IrNode::Regex(4)])],
    );
    flat_compound.shape_assignments.assign(1, ShapeTag::Flat);
    let flat_compound = emit_flat(&flat_compound);
    assert!(!flat_compound.contains("__span_slice"));
    assert!(!flat_compound.contains("emitted payload arity does not exactly agree"));

    let mut scalar_separator = flat_ir(IrNode::Ref(1), vec![], vec![IrNode::Literal(2)]);
    scalar_separator
        .shape_assignments
        .assign(1, ShapeTag::Scalar);
    let scalar_separator = emit_flat(&scalar_separator);
    assert!(
        scalar_separator.contains("__span_slice"),
        "generic Scalar separators are the classified zero-deposit exception"
    );

    let mut scalar_bool = flat_ir(IrNode::Ref(1), vec![], vec![IrNode::Literal(2)]);
    scalar_bool.strings[2] = "true".into();
    scalar_bool.shape_assignments.assign(1, ShapeTag::Scalar);
    let scalar_bool = emit_flat(&scalar_bool);
    assert!(!scalar_bool.contains("__span_slice"));
    assert!(!scalar_bool.contains("emitted payload arity does not exactly agree"));
}

#[test]
fn many_payloads_require_a_matching_solved_aggregate_layout() {
    let two_maps = IrNode::Seq(vec![mapped_regex(0), mapped_regex(1)]);
    let fns = vec![scalar_fn(1, TypeDesc::I8), scalar_fn(2, TypeDesc::I8)];

    let no_layout = flat_ir(two_maps.clone(), fns.clone(), vec![]);
    assert!(emit_flat(&no_layout).contains("emitted payload arity does not exactly agree"));

    let repeated = flat_ir(
        IrNode::Repeat {
            inner: Box::new(mapped_regex(0)),
            lo: 2,
            hi: 2,
        },
        vec![fns[0].clone()],
        vec![],
    );
    assert!(emit_flat(&repeated).contains("emitted payload arity does not exactly agree"));

    let ref_many = flat_ir(IrNode::Ref(1), fns.clone(), vec![two_maps.clone()]);
    assert!(emit_flat(&ref_many).contains("emitted payload arity does not exactly agree"));

    let mut aggregate = flat_ir(two_maps, fns, vec![]);
    aggregate.struct_registry.insert(StructLayout {
        rule_id: 0,
        rule_name: "subject".into(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::I8, TypeDesc::I8]),
        fields: vec![
            StructField {
                name: "left".into(),
                type_desc: TypeDesc::I8,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "right".into(),
                type_desc: TypeDesc::I8,
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });
    assert!(
        !emit_flat(&aggregate).contains("emitted payload arity does not exactly agree"),
        "a solved two-field Struct layout admits legal aggregate multiplicity"
    );
}

fn install_struct_layout(ir: &mut GrammarIR, field_count: usize, rule_type: TypeDesc) {
    let fields = (0..field_count)
        .map(|position| StructField {
            name: format!("field_{position}"),
            type_desc: TypeDesc::I8,
            source: FieldSource::SeqPosition {
                position: position as u32,
            },
        })
        .collect();
    ir.struct_registry.insert(StructLayout {
        rule_id: 0,
        rule_name: "subject".into(),
        kind: LayoutKind::Struct,
        rule_type,
        fields,
    });
}

fn fixed_map_sequence(count: usize) -> (IrNode, Vec<FnDescriptor>) {
    let fns = (0..count)
        .map(|value| scalar_fn(value as i64, TypeDesc::I8))
        .collect();
    let nodes: Vec<_> = (0..count).map(|fn_id| mapped_regex(fn_id as u32)).collect();
    let body = match nodes.as_slice() {
        [] => IrNode::Regex(4),
        [only] => only.clone(),
        _ => IrNode::Seq(nodes),
    };
    (body, fns)
}

#[test]
fn exact_zero_one_two_three_arities_match_only_exact_fixed_layouts() {
    for arity in 0..=3 {
        let (body, fns) = fixed_map_sequence(arity);
        let mut ir = flat_ir(body, fns, vec![]);
        install_struct_layout(&mut ir, arity, TypeDesc::Tuple(vec![TypeDesc::I8; arity]));
        let emitted = emit_flat(&ir);
        assert!(
            !emitted.contains("emitted payload arity does not exactly agree"),
            "exact arity {arity} must match an exact {arity}-field layout:\n{emitted}"
        );
        if arity == 0 {
            assert!(
                !emitted.contains("__span_slice"),
                "a solved zero-field layout must not be rewritten to arity one"
            );
        }
    }

    for (body_arity, layout_arity) in [(0, 2), (1, 0), (2, 3), (3, 2)] {
        let (body, fns) = fixed_map_sequence(body_arity);
        let mut ir = flat_ir(body, fns, vec![]);
        install_struct_layout(
            &mut ir,
            layout_arity,
            TypeDesc::Tuple(vec![TypeDesc::I8; layout_arity]),
        );
        assert!(
            emit_flat(&ir).contains("emitted payload arity does not exactly agree"),
            "body arity {body_arity} must not be admitted by layout arity {layout_arity}"
        );
    }
}

#[test]
fn repeat_and_alt_ranges_are_exact_for_fixed_layouts_and_valid_for_vec() {
    let repeated = IrNode::Repeat {
        inner: Box::new(mapped_regex(0)),
        lo: 3,
        hi: 3,
    };
    let mut exact_three = flat_ir(repeated, vec![scalar_fn(7, TypeDesc::I8)], vec![]);
    install_struct_layout(&mut exact_three, 3, TypeDesc::Tuple(vec![TypeDesc::I8; 3]));
    assert!(!emit_flat(&exact_three).contains("emitted payload arity does not exactly agree"));

    let variable_repeat = IrNode::Repeat {
        inner: Box::new(mapped_regex(0)),
        lo: 2,
        hi: 3,
    };
    let mut fixed_two = flat_ir(
        variable_repeat.clone(),
        vec![scalar_fn(7, TypeDesc::I8)],
        vec![],
    );
    install_struct_layout(&mut fixed_two, 2, TypeDesc::Tuple(vec![TypeDesc::I8; 2]));
    assert!(emit_flat(&fixed_two).contains("emitted payload arity does not exactly agree"));

    let mut variable_vec = flat_ir(variable_repeat, vec![scalar_fn(7, TypeDesc::I8)], vec![]);
    variable_vec.struct_registry.insert(StructLayout {
        rule_id: 0,
        rule_name: "subject".into(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Vec(Box::new(TypeDesc::I8)),
        fields: vec![StructField {
            name: "element".into(),
            type_desc: TypeDesc::I8,
            source: FieldSource::RepeatElement,
        }],
    });
    let variable_vec = emit_flat(&variable_vec);
    assert!(
        !variable_vec.contains("emitted payload arity does not exactly agree"),
        "a proven variable repeat is the solved Vec path, not a fixed-arity mismatch"
    );
    assert!(!variable_vec.contains("compile_error"));

    let (two, fns) = fixed_map_sequence(2);
    let (three, _) = fixed_map_sequence(3);
    let mut uniform_alt = flat_ir(
        IrNode::Alt(vec![branch(two.clone()), branch(two)], None),
        fns.clone(),
        vec![],
    );
    install_struct_layout(&mut uniform_alt, 2, TypeDesc::Tuple(vec![TypeDesc::I8; 2]));
    assert!(!emit_flat(&uniform_alt).contains("emitted payload arity does not exactly agree"));

    let mut varying_alt = flat_ir(
        IrNode::Alt(vec![branch(fixed_map_sequence(2).0), branch(three)], None),
        vec![
            scalar_fn(1, TypeDesc::I8),
            scalar_fn(2, TypeDesc::I8),
            scalar_fn(3, TypeDesc::I8),
        ],
        vec![],
    );
    install_struct_layout(&mut varying_alt, 2, TypeDesc::Tuple(vec![TypeDesc::I8; 2]));
    assert!(emit_flat(&varying_alt).contains("emitted payload arity does not exactly agree"));
}

#[test]
fn structural_sequences_preserve_real_regex_patterns_and_optional_whitespace() {
    let patterns = [r"[,:;]", r"\s+", r#""(?:\\.|[^"])*""#, r"[A-Za-z]{1,3}"];
    let mut ir = GrammarIR::default();
    ir.strings = vec![
        "subject".into(),
        "(".into(),
        ")".into(),
        patterns[0].into(),
        patterns[1].into(),
        patterns[2].into(),
        patterns[3].into(),
    ];
    ir.rules.push(rule(
        0,
        0,
        IrNode::Alt(
            vec![branch(IrNode::Seq(vec![
                IrNode::Literal(1),
                IrNode::Regex(3),
                IrNode::OptionalWhitespace(Box::new(IrNode::Regex(4))),
                IrNode::Regex(5),
                IrNode::Regex(6),
                IrNode::Literal(2),
            ]))],
            None,
        ),
    ));

    let emitted = formatted(alt_dispatch::emit_parse_alt_dispatch(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    for pattern in patterns {
        assert!(
            emitted.contains(&format!("{pattern:?}")),
            "canonical regex adapter call lost pattern {pattern:?}:\n{emitted}"
        );
    }
    assert_eq!(emitted.matches("__regex_scan_Fixture").count(), 4);
    assert!(
        emitted.matches("skip_space(input, p, state)").count() >= 2,
        "OptionalWhitespace must emit both bookends"
    );
    assert!(
        !emitted.contains("is_ascii_alphanumeric"),
        "the deleted ASCII-word scanner must not survive in structural Seq emission"
    );
}

#[test]
fn scalar_optional_whitespace_and_map_replacement_share_the_structural_path() {
    let mut ir = GrammarIR::default();
    ir.strings = vec!["subject".into(), ",".into()];
    ir.fns.push(scalar_fn(9, TypeDesc::I8));
    ir.rules.push(rule(
        0,
        0,
        IrNode::Map {
            inner: Box::new(IrNode::OptionalWhitespace(Box::new(IrNode::Literal(1)))),
            fn_id: 0,
        },
    ));
    let emitted = formatted(scalar::emit_parse_scalar(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
    ));
    assert_eq!(emitted.matches("skip_space(input, p, state)").count(), 2);
    assert!(emitted.contains("builder.rollback(__map_builder_checkpoint)"));
    assert_eq!(
        emitted.matches("push_leaf_with_i64(builder, 9i64)").count(),
        1
    );
}

fn transparent_wrap_scc(productive: bool) -> GrammarIR {
    let mut ir = GrammarIR::default();
    ir.strings = vec![
        "subject".into(),
        "left".into(),
        "right".into(),
        "leaf".into(),
        "x".into(),
    ];
    ir.rules.push(rule(0, 0, IrNode::Ref(1)));
    let mut left_branches = vec![branch(IrNode::Ref(2))];
    let mut right_branches = vec![branch(IrNode::Ref(1))];
    if productive {
        left_branches.push(branch(IrNode::Ref(3)));
        right_branches.push(branch(IrNode::Ref(3)));
    }
    let mut left = rule(1, 1, IrNode::Alt(left_branches, None));
    left.meta.is_transparent = true;
    let mut right = rule(2, 2, IrNode::Alt(right_branches, None));
    right.meta.is_transparent = true;
    ir.rules.push(left);
    ir.rules.push(right);
    if productive {
        ir.rules.push(rule(3, 3, IrNode::Literal(4)));
        ir.shape_assignments.assign(3, ShapeTag::Keyword);
    }
    ir.shape_assignments.assign(1, ShapeTag::Wrap);
    ir.shape_assignments.assign(2, ShapeTag::Wrap);
    ir
}

#[test]
fn recursive_transparent_sccs_require_a_productive_emitted_base() {
    let productive = emit_flat(&transparent_wrap_scc(true));
    assert!(productive.contains("parse_wrap_Fixture_left"));
    assert!(
        !productive.contains("emitted payload arity does not exactly agree"),
        "the productive transparent SCC has an exact one-payload fixed point"
    );

    let unproductive = emit_flat(&transparent_wrap_scc(false));
    assert!(
        unproductive.contains("emitted payload arity does not exactly agree"),
        "an unproductive transparent SCC must terminate at fail-closed"
    );
}

#[test]
fn a_productive_base_does_not_hide_an_arity_growing_recursive_branch() {
    let mut ir = GrammarIR::default();
    ir.strings = vec!["subject".into(), "recursive".into(), "x".into()];
    ir.fns.push(scalar_fn(1, TypeDesc::I8));
    ir.rules.push(rule(0, 0, IrNode::Ref(1)));
    ir.rules.push(rule(
        1,
        1,
        IrNode::Alt(
            vec![
                branch(IrNode::Map {
                    inner: Box::new(IrNode::Regex(2)),
                    fn_id: 0,
                }),
                branch(IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Map {
                        inner: Box::new(IrNode::Regex(2)),
                        fn_id: 0,
                    },
                ])),
            ],
            None,
        ),
    ));
    ir.shape_assignments.assign(1, ShapeTag::Flat);
    ir.struct_registry.insert(StructLayout {
        rule_id: 1,
        rule_name: "recursive".into(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::I8]),
        fields: vec![StructField {
            name: "value".into(),
            type_desc: TypeDesc::I8,
            source: FieldSource::SeqPosition { position: 0 },
        }],
    });

    assert!(
        emit_flat(&ir).contains("emitted payload arity does not exactly agree"),
        "the base arity-one path must not hide the recursive arity-two path"
    );
}

#[test]
fn every_owned_ref_path_rejects_a_classified_alias_the_driver_does_not_emit() {
    let mut ir = GrammarIR::default();
    ir.strings = vec!["subject".into(), "alias".into(), "x".into()];
    ir.rules.push(rule(0, 0, IrNode::Ref(1)));
    let mut alias = rule(1, 1, IrNode::Regex(2));
    alias.meta.is_transparent = true;
    ir.rules.push(alias);
    ir.shape_assignments.assign(1, ShapeTag::HRegex);

    let flat = emit_flat(&ir);
    assert!(flat.contains("Flat Ref target has no emitted per-shape driver"));
    assert!(!flat.contains("parse_hregex_Fixture_alias"));

    let scalar = formatted(scalar::emit_parse_scalar(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
    ));
    assert!(scalar.contains("Scalar Ref target has no emitted per-shape driver"));
    assert!(!scalar.contains("parse_hregex_Fixture_alias"));

    ir.rules[0].body = IrNode::Alt(vec![branch(IrNode::Ref(1))], None);
    let wrap = formatted(wrap::emit_parse_wrap(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    assert!(wrap.contains("Wrap Ref target has no emitted per-shape driver"));
    assert!(!wrap.contains("parse_hregex_Fixture_alias"));

    ir.rules[0].body = IrNode::Alt(vec![branch(IrNode::Seq(vec![IrNode::Ref(1)]))], None);
    let inline = formatted(alt_dispatch::emit_parse_alt_dispatch(
        "Fixture",
        &ir.rules[0],
        &ir,
        &strategy(),
        &PayloadPlanner::new(&ir),
    ));
    assert!(inline.contains("structural Ref target has no emitted per-shape driver"));
    assert!(!inline.contains("parse_hregex_Fixture_alias"));
}

#[test]
fn token_dispatch_keyword_and_pratt_without_owned_proofs_fail_closed() {
    let token_dispatch = IrNode::TokenDispatch {
        token: Box::new(IrNode::Regex(4)),
        arms: vec![TokenDispatchArm {
            patterns: vec![2],
            guard_byte: Some(b'('),
            continuation: IrNode::Epsilon,
            map_fn: None,
        }],
        fallback: Box::new(IrNode::Epsilon),
    };
    let direct = emit_flat(&flat_ir(token_dispatch.clone(), vec![], vec![]));
    assert!(direct.contains("TokenDispatch has no exact StructDirect emission path"));
    assert!(direct.contains("emitted payload arity does not exactly agree"));

    let mut keyword_ir = flat_ir(IrNode::Ref(1), vec![], vec![token_dispatch]);
    keyword_ir.shape_assignments.assign(1, ShapeTag::Keyword);
    assert!(
        emit_flat(&keyword_ir).contains("emitted payload arity does not exactly agree"),
        "unsupported Keyword bodies must not be advertised as exact-one"
    );

    let mut pratt_ir = flat_ir(
        IrNode::Ref(1),
        vec![],
        vec![IrNode::Ref(2), IrNode::Literal(2)],
    );
    pratt_ir.shape_assignments.assign(1, ShapeTag::Pratt);
    pratt_ir.shape_assignments.assign(2, ShapeTag::Keyword);
    assert!(
        emit_flat(&pratt_ir).contains("emitted payload arity does not exactly agree"),
        "Pratt's dynamic operator/RHS layout remains explicitly unsupported"
    );
}
