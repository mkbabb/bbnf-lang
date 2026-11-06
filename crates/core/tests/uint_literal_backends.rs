use std::panic::{AssertUnwindSafe, catch_unwind};

use bbnf::backend::ts::projection::{
    compile_map_expr_to_js, translate_rust_constant_to_js, type_desc_to_ts,
};
use bbnf::backend::wasm::{WasmEmitCtx, WasmEmitter};
use bbnf::backend::{Emitter, ValuePlacement};
use bbnf_ir::{GrammarIR, MapBinOp, MapExpr, TypeDesc};

#[test]
fn typescript_projects_full_u64_as_exact_bigint() {
    let ir = GrammarIR::default();

    assert_eq!(type_desc_to_ts(&TypeDesc::U64, "Unused", &ir), "bigint");
    assert_eq!(type_desc_to_ts(&TypeDesc::I64, "Unused", &ir), "bigint");
    assert_eq!(
        compile_map_expr_to_js(&MapExpr::UIntLit(u64::MAX), Some(&TypeDesc::U64), &ir),
        "18446744073709551615n"
    );
    assert_eq!(
        compile_map_expr_to_js(&MapExpr::UIntLit(42), Some(&TypeDesc::U8), &ir),
        "42"
    );
    assert_eq!(
        compile_map_expr_to_js(&MapExpr::IntLit(i64::MIN), Some(&TypeDesc::I64), &ir),
        "-9223372036854775808n"
    );
    assert_eq!(
        compile_map_expr_to_js(&MapExpr::IntLit(i64::MAX), None, &ir),
        "9223372036854775807n"
    );
    assert_eq!(
        translate_rust_constant_to_js("18446744073709551615u64"),
        "18446744073709551615n"
    );
    assert_eq!(
        translate_rust_constant_to_js("0xffffffffffffffffu64"),
        "0xffffffffffffffffn"
    );
    assert_eq!(
        translate_rust_constant_to_js("-9223372036854775808i64"),
        "-9223372036854775808n"
    );

    let unresolved = MapExpr::BinOp {
        op: MapBinOp::Add,
        operand_domain: None,
        lhs: Box::new(MapExpr::UIntLit(u64::MAX)),
        rhs: Box::new(MapExpr::Input),
    };
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            compile_map_expr_to_js(&unresolved, None, &ir)
        }))
        .is_err(),
        "untyped wide integer plus dynamic input must reject instead of emitting mixed JS"
    );
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            compile_map_expr_to_js(&MapExpr::IntLit(-1), Some(&TypeDesc::U8), &ir)
        }))
        .is_err(),
        "negative IntLit must not project to U8"
    );
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            compile_map_expr_to_js(&MapExpr::UIntLit(255), Some(&TypeDesc::I8), &ir)
        }))
        .is_err(),
        "UIntLit outside I8 must reject"
    );

    let folded = MapExpr::BinOp {
        op: MapBinOp::Add,
        operand_domain: None,
        lhs: Box::new(MapExpr::IntLit(40)),
        rhs: Box::new(MapExpr::IntLit(2)),
    };
    assert_eq!(
        compile_map_expr_to_js(&folded, Some(&TypeDesc::I8), &ir),
        "42",
        "TypeScript must fold before choosing and validating its scalar projection"
    );
    let overflow = MapExpr::BinOp {
        op: MapBinOp::Add,
        operand_domain: None,
        lhs: Box::new(MapExpr::IntLit(i64::MAX)),
        rhs: Box::new(MapExpr::IntLit(1)),
    };
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            compile_map_expr_to_js(&overflow, Some(&TypeDesc::I64), &ir)
        }))
        .is_err(),
        "a checked-fold overflow must not escape as unbounded TypeScript arithmetic"
    );

    let float_overflow = MapExpr::BinOp {
        op: MapBinOp::Add,
        operand_domain: None,
        lhs: Box::new(MapExpr::FloatLit(f64::MAX)),
        rhs: Box::new(MapExpr::FloatLit(f64::MAX)),
    };
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            compile_map_expr_to_js(&float_overflow, Some(&TypeDesc::F64), &ir)
        }))
        .is_err(),
        "a non-finite float fold must not escape into TypeScript source"
    );
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            compile_map_expr_to_js(&MapExpr::FloatLit(f64::INFINITY), Some(&TypeDesc::F64), &ir)
        }))
        .is_err(),
        "a pre-existing non-finite float literal must fail at the TS boundary"
    );
}

#[test]
fn wasm_emits_representable_uint_and_rejects_unrepresentable_widths() {
    let mut emitter = WasmEmitter {
        module_name: "uint-literal-test".to_string(),
        ws_regex_id: None,
    };
    let mut ctx = WasmEmitCtx::default();
    let ir = GrammarIR::default();

    let exact = emitter.emit_map_expr(
        "(i32.const 0)".to_string(),
        &MapExpr::UIntLit(255),
        Some(&TypeDesc::U8),
        ValuePlacement::Inline,
        &ir,
        &mut ctx,
    );
    assert!(exact.contains("(then (i32.const 255))"));

    let signed = emitter.emit_map_expr(
        "(i32.const 0)".to_string(),
        &MapExpr::IntLit(i32::MAX.into()),
        Some(&TypeDesc::I32),
        ValuePlacement::Inline,
        &ir,
        &mut ctx,
    );
    assert!(signed.contains("(then (i32.const 2147483647))"));

    let folded = emitter.emit_map_expr(
        "(i32.const 0)".to_string(),
        &MapExpr::BinOp {
            op: MapBinOp::Add,
            operand_domain: None,
            lhs: Box::new(MapExpr::IntLit(40)),
            rhs: Box::new(MapExpr::IntLit(2)),
        },
        Some(&TypeDesc::I8),
        ValuePlacement::Inline,
        &ir,
        &mut ctx,
    );
    assert!(
        folded.contains("(then (i32.const 42))"),
        "Wasm must fold a constant expression before scalar decoding"
    );

    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            emitter.emit_map_expr(
                "(i32.const 0)".to_string(),
                &MapExpr::BinOp {
                    op: MapBinOp::Add,
                    operand_domain: None,
                    lhs: Box::new(MapExpr::Input),
                    rhs: Box::new(MapExpr::IntLit(1)),
                },
                Some(&TypeDesc::I32),
                ValuePlacement::Inline,
                &ir,
                &mut ctx,
            )
        }))
        .is_err(),
        "Wasm must reject a dynamic expression instead of passing input through"
    );
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            emitter.emit_map_expr(
                "(i32.const 0)".to_string(),
                &MapExpr::BinOp {
                    op: MapBinOp::Add,
                    operand_domain: None,
                    lhs: Box::new(MapExpr::IntLit(i64::MAX)),
                    rhs: Box::new(MapExpr::IntLit(1)),
                },
                Some(&TypeDesc::I64),
                ValuePlacement::Inline,
                &ir,
                &mut ctx,
            )
        }))
        .is_err(),
        "Wasm must reject a constant expression whose checked fold overflowed"
    );

    let signed_failure = catch_unwind(AssertUnwindSafe(|| {
        emitter.emit_map_expr(
            "(i32.const 0)".to_string(),
            &MapExpr::IntLit(i64::MAX),
            Some(&TypeDesc::I64),
            ValuePlacement::Inline,
            &ir,
            &mut ctx,
        )
    }))
    .expect_err("WASM must reject I64 under the current i32 result ABI");
    let signed_message = signed_failure
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| signed_failure.downcast_ref::<&str>().copied())
        .unwrap_or("");
    assert!(signed_message.contains("cannot emit exact i64"));

    let failure = catch_unwind(AssertUnwindSafe(|| {
        emitter.emit_map_expr(
            "(i32.const 0)".to_string(),
            &MapExpr::UIntLit(u64::MAX),
            Some(&TypeDesc::U64),
            ValuePlacement::Inline,
            &ir,
            &mut ctx,
        )
    }))
    .expect_err("WASM must reject a u64 literal instead of narrowing or passing it through");

    let message = failure
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| failure.downcast_ref::<&str>().copied())
        .unwrap_or("");
    assert!(message.contains("current map-result ABI is i32"));
}
