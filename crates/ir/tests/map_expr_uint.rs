use bbnf_ir::{
    FnDescriptor, GrammarIR, MapBinOp, MapExpr, MapUnaryOp, NumericDomain, NumericExprError,
    TypeDesc,
};

fn binary(op: MapBinOp, lhs: MapExpr, rhs: MapExpr) -> MapExpr {
    MapExpr::BinOp {
        op,
        operand_domain: None,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[test]
fn uint_literal_is_constant_and_round_trips_without_narrowing() {
    let mut ir = GrammarIR::default();
    ir.fns.push(FnDescriptor::Expr {
        expr: MapExpr::UIntLit(u64::MAX),
        return_type: Some(TypeDesc::U64),
    });
    let carried_comparison = MapExpr::BinOp {
        op: MapBinOp::Lt,
        operand_domain: Some(NumericDomain::U64),
        lhs: Box::new(MapExpr::Input),
        rhs: Box::new(MapExpr::UIntLit(2)),
    };
    ir.fns.push(FnDescriptor::Expr {
        expr: carried_comparison.clone(),
        return_type: Some(TypeDesc::Bool),
    });

    let json = ir.to_json().expect("UIntLit IR must serialize to JSON");
    let from_json = GrammarIR::from_json(&json).expect("UIntLit IR must deserialize from JSON");
    let msgpack = ir
        .to_msgpack()
        .expect("UIntLit IR must serialize to MessagePack");
    let from_msgpack =
        GrammarIR::from_msgpack(&msgpack).expect("UIntLit IR must deserialize from MessagePack");

    for decoded in [&from_json, &from_msgpack] {
        assert_eq!(
            decoded.fns.first(),
            Some(&FnDescriptor::Expr {
                expr: MapExpr::UIntLit(u64::MAX),
                return_type: Some(TypeDesc::U64),
            })
        );
        assert_eq!(
            decoded.fns.get(1),
            Some(&FnDescriptor::Expr {
                expr: carried_comparison.clone(),
                return_type: Some(TypeDesc::Bool),
            })
        );
    }
    assert!(MapExpr::UIntLit(u64::MAX).is_constant());
}

#[test]
fn uint_checked_fold_reports_overflow_and_underflow() {
    let mut sum = binary(MapBinOp::Add, MapExpr::UIntLit(40), MapExpr::UIntLit(2));
    assert_eq!(sum.checked_fold(Some(NumericDomain::U64)), Ok(()));
    assert_eq!(sum, MapExpr::UIntLit(42));

    let mut overflow = binary(
        MapBinOp::Add,
        MapExpr::UIntLit(u64::MAX),
        MapExpr::UIntLit(1),
    );
    let original_overflow = overflow.clone();
    assert_eq!(
        overflow.checked_fold(Some(NumericDomain::U64)),
        Err(NumericExprError::Overflow)
    );
    assert_eq!(overflow, original_overflow);

    let mut underflow = binary(MapBinOp::Sub, MapExpr::UIntLit(0), MapExpr::UIntLit(1));
    let original_underflow = underflow.clone();
    assert_eq!(
        underflow.checked_fold(Some(NumericDomain::U64)),
        Err(NumericExprError::Overflow)
    );
    assert_eq!(underflow, original_underflow);
}

#[test]
fn signed_checked_fold_reports_overflow() {
    let mut overflow = binary(MapBinOp::Add, MapExpr::IntLit(i64::MAX), MapExpr::IntLit(1));
    let original = overflow.clone();
    assert_eq!(
        overflow.checked_fold(Some(NumericDomain::I64)),
        Err(NumericExprError::Overflow)
    );
    assert_eq!(overflow, original);
}

#[test]
fn float_constant_fold_commits_only_finite_results() {
    let mut finite = binary(
        MapBinOp::Add,
        MapExpr::FloatLit(1.25),
        MapExpr::FloatLit(2.5),
    );
    assert_eq!(finite.checked_fold(Some(NumericDomain::F64)), Ok(()));
    assert_eq!(finite, MapExpr::FloatLit(3.75));

    let mut overflow = binary(
        MapBinOp::Add,
        MapExpr::FloatLit(f64::MAX),
        MapExpr::FloatLit(f64::MAX),
    );
    let original_overflow = overflow.clone();
    assert_eq!(
        overflow.checked_fold(Some(NumericDomain::F64)),
        Err(NumericExprError::NonFinite)
    );
    assert_eq!(overflow, original_overflow);

    let mut division_by_zero = binary(
        MapBinOp::Div,
        MapExpr::FloatLit(1.0),
        MapExpr::FloatLit(0.0),
    );
    let original_division = division_by_zero.clone();
    assert_eq!(
        division_by_zero.checked_fold(Some(NumericDomain::F64)),
        Err(NumericExprError::DivisionByZero)
    );
    assert_eq!(division_by_zero, original_division);

    let mut modulo_by_zero = binary(
        MapBinOp::Mod,
        MapExpr::FloatLit(1.0),
        MapExpr::FloatLit(0.0),
    );
    let original_modulo = modulo_by_zero.clone();
    assert_eq!(
        modulo_by_zero.checked_fold(Some(NumericDomain::F64)),
        Err(NumericExprError::DivisionByZero)
    );
    assert_eq!(modulo_by_zero, original_modulo);

    let mut nan_modulo = binary(
        MapBinOp::Mod,
        MapExpr::FloatLit(f64::INFINITY),
        MapExpr::FloatLit(2.0),
    );
    let original_nan = nan_modulo.clone();
    assert_eq!(
        nan_modulo.checked_fold(Some(NumericDomain::F64)),
        Err(NumericExprError::NonFinite)
    );
    assert_eq!(nan_modulo, original_nan);

    let mut finite_modulo = binary(
        MapBinOp::Mod,
        MapExpr::FloatLit(7.5),
        MapExpr::FloatLit(2.0),
    );
    assert_eq!(finite_modulo.checked_fold(Some(NumericDomain::F64)), Ok(()));
    assert_eq!(finite_modulo, MapExpr::FloatLit(1.5));
}

#[test]
fn exact_domain_controls_direct_and_composite_width() {
    let mut direct = MapExpr::IntLit(i8::MAX.into());
    assert_eq!(direct.checked_fold(Some(NumericDomain::I8)), Ok(()));

    let mut out_of_range = MapExpr::IntLit(i8::MAX as i64 + 1);
    let original = out_of_range.clone();
    assert_eq!(
        out_of_range.checked_fold(Some(NumericDomain::I8)),
        Err(NumericExprError::OutOfRange)
    );
    assert_eq!(out_of_range, original);

    let mut signed = binary(MapBinOp::Add, MapExpr::IntLit(40), MapExpr::IntLit(2));
    assert_eq!(signed.checked_fold(Some(NumericDomain::I8)), Ok(()));
    assert_eq!(signed, MapExpr::IntLit(42));

    let mut unsigned = binary(MapBinOp::Add, MapExpr::UIntLit(40), MapExpr::UIntLit(2));
    assert_eq!(unsigned.checked_fold(Some(NumericDomain::U8)), Ok(()));
    assert_eq!(unsigned, MapExpr::UIntLit(42));
}

#[test]
fn mixed_domains_and_unsupported_operator_surface_fail_typed() {
    let mut mixed = binary(MapBinOp::Add, MapExpr::IntLit(1), MapExpr::UIntLit(2));
    let original_mixed = mixed.clone();
    assert_eq!(
        mixed.checked_fold(None),
        Err(NumericExprError::DomainMismatch)
    );
    assert_eq!(mixed, original_mixed);

    for op in [
        MapBinOp::BitAnd,
        MapBinOp::BitOr,
        MapBinOp::Shl,
        MapBinOp::Shr,
    ] {
        for domain in [None, Some(NumericDomain::I32)] {
            let mut expr = binary(op, MapExpr::Input, MapExpr::IntLit(1));
            let original = expr.clone();
            assert_eq!(
                expr.checked_fold(domain),
                Err(NumericExprError::InvalidOperator)
            );
            assert_eq!(expr, original);
        }
    }

    for op in [
        MapBinOp::Add,
        MapBinOp::Lt,
        MapBinOp::Gt,
        MapBinOp::Le,
        MapBinOp::Ge,
    ] {
        let mut expr = binary(op, MapExpr::Input, MapExpr::IntLit(1));
        let original = expr.clone();
        assert_eq!(
            expr.checked_fold(None),
            Err(NumericExprError::DomainMismatch)
        );
        assert_eq!(expr, original);
    }

    let mut unsigned_neg = MapExpr::UnaryOp {
        op: MapUnaryOp::Neg,
        operand_domain: None,
        inner: Box::new(MapExpr::Input),
    };
    let original_neg = unsigned_neg.clone();
    assert_eq!(
        unsigned_neg.checked_fold(Some(NumericDomain::U32)),
        Err(NumericExprError::InvalidNegation)
    );
    assert_eq!(unsigned_neg, original_neg);

    let mut strings = binary(MapBinOp::Eq, MapExpr::StringLit(7), MapExpr::StringLit(7));
    assert_eq!(strings.checked_fold(None), Ok(()));
    assert_eq!(strings, MapExpr::BoolLit(true));

    let mut dynamic_bool = binary(MapBinOp::And, MapExpr::Input, MapExpr::Input);
    let original_bool = dynamic_bool.clone();
    assert_eq!(dynamic_bool.checked_fold(None), Ok(()));
    assert_eq!(dynamic_bool, original_bool);

    for op in [MapBinOp::Eq, MapBinOp::Ne] {
        for rhs in [MapExpr::BoolLit(true), MapExpr::StringLit(7)] {
            let mut expr = binary(op, MapExpr::Input, rhs);
            let original = expr.clone();
            assert_eq!(expr.checked_fold(None), Ok(()));
            assert_eq!(expr, original);
        }
    }
}

#[test]
fn failed_parent_fold_is_transactional() {
    let child = binary(MapBinOp::Add, MapExpr::IntLit(1), MapExpr::IntLit(2));
    let mut expr = binary(MapBinOp::Add, child, MapExpr::IntLit(i64::MAX));
    let original = expr.clone();
    assert_eq!(
        expr.checked_fold(Some(NumericDomain::I64)),
        Err(NumericExprError::Overflow)
    );
    assert_eq!(
        expr, original,
        "successful child folds must not leak on Err"
    );
}
