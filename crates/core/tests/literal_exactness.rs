//! End-to-end literal exactness from BBNF source through IR and each backend.

use std::panic::{AssertUnwindSafe, catch_unwind};
use std::process::Command;

use bbnf::backend::rust::emitter::shapes::hregex;
use bbnf::backend::ts::projection::{compile_map_expr_to_js, ts_escape};
use bbnf::backend::wasm::{WasmEmitCtx, WasmEmitter};
use bbnf::backend::{Emitter, ValuePlacement};
use bbnf::pipeline::{PipelineOptions, compile_grammar};
use bbnf_ir::registry::{EmitStrategy, SubstrateBinding};
use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, MapBinOp, MapExpr, NumericDomain, TypeDesc};

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

fn lower_map(source_value: &str) -> (GrammarIR, u32) {
    let source = format!(r#"subject = /x/ -> {source_value} ;"#);
    let ir = compile_grammar(&source, &PipelineOptions::default())
        .expect("literal fixture must compile through the grammar pipeline");
    let rule = ir.find_rule("subject").expect("subject rule");
    let IrNode::Map { fn_id, .. } = &rule.body else {
        panic!("literal fixture must retain its Map: {:#?}", rule.body);
    };
    (ir, *fn_id)
}

#[test]
fn every_signed_minimum_reaches_rust_typescript_and_wasm_exactly() {
    for (spelling, value, ty, js) in [
        ("-128i8", i8::MIN as i64, TypeDesc::I8, "-128"),
        ("-32768i16", i16::MIN as i64, TypeDesc::I16, "-32768"),
        (
            "-2147483648i32",
            i32::MIN as i64,
            TypeDesc::I32,
            "-2147483648",
        ),
        (
            "-9223372036854775808i64",
            i64::MIN,
            TypeDesc::I64,
            "-9223372036854775808n",
        ),
    ] {
        let (ir, fn_id) = lower_map(spelling);
        let descriptor = &ir.fns[fn_id as usize];
        assert_eq!(
            descriptor,
            &FnDescriptor::Expr {
                expr: MapExpr::IntLit(value),
                return_type: Some(ty.clone()),
            },
            "{spelling} must be one signed IR literal"
        );

        let rule = ir.find_rule("subject").unwrap();
        let rust = formatted(hregex::emit_parse_hregex(
            &strategy(),
            "LiteralExactness",
            rule,
            &ir,
        ));
        assert_eq!(
            rust.matches(&format!("push_leaf_with_i64(builder, {value}i64)"))
                .count(),
            1,
            "Rust must emit {spelling} exactly once:\n{rust}"
        );

        let FnDescriptor::Expr { expr, return_type } = descriptor else {
            unreachable!()
        };
        assert_eq!(compile_map_expr_to_js(expr, return_type.as_ref(), &ir), js);

        let mut wasm = WasmEmitter {
            module_name: "literal-exactness".into(),
            ws_regex_id: None,
        };
        let mut ctx = WasmEmitCtx::default();
        if matches!(ty, TypeDesc::I64) {
            assert!(
                catch_unwind(AssertUnwindSafe(|| {
                    wasm.emit_map_expr(
                        "(i32.const 0)".into(),
                        expr,
                        return_type.as_ref(),
                        ValuePlacement::Inline,
                        &ir,
                        &mut ctx,
                    )
                }))
                .is_err(),
                "Wasm must reject an exact i64 it cannot represent"
            );
        } else {
            let wat = wasm.emit_map_expr(
                "(i32.const 0)".into(),
                expr,
                return_type.as_ref(),
                ValuePlacement::Inline,
                &ir,
                &mut ctx,
            );
            assert!(wat.contains(&format!("(then (i32.const {value}))")));
        }
    }
}

#[test]
fn adjacent_signed_overflow_is_rejected_before_backend_emission() {
    for spelling in [
        "128i8",
        "-129i8",
        "32768i16",
        "-32769i16",
        "2147483648i32",
        "-2147483649i32",
        "9223372036854775808i64",
        "-9223372036854775809i64",
    ] {
        let result = catch_unwind(AssertUnwindSafe(|| lower_map(spelling)));
        assert!(result.is_err(), "overflowing {spelling} must fail closed");
    }
}

#[test]
fn string_literal_emits_its_semantic_value_and_wasm_rejects_it() {
    let (ir, fn_id) = lower_map(r#""semantic""#);
    let descriptor = &ir.fns[fn_id as usize];
    let FnDescriptor::Expr { expr, return_type } = descriptor else {
        panic!("string fixture must lower to an expression descriptor");
    };
    let MapExpr::StringLit(sid) = expr else {
        panic!("string fixture must retain StringLit, got {expr:#?}");
    };
    assert_eq!(ir.get_string(*sid), "semantic");

    let rust = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "LiteralExactness",
        ir.find_rule("subject").unwrap(),
        &ir,
    ));
    assert_eq!(
        rust.matches("push_leaf_with_str(builder, \"semantic\")")
            .count(),
        1,
        "Rust must emit the semantic string exactly once:\n{rust}"
    );
    assert_eq!(
        compile_map_expr_to_js(expr, return_type.as_ref(), &ir),
        "\"semantic\""
    );

    let mut wasm = WasmEmitter {
        module_name: "literal-exactness".into(),
        ws_regex_id: None,
    };
    let mut ctx = WasmEmitCtx::default();
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            wasm.emit_map_expr(
                "(i32.const 0)".into(),
                expr,
                return_type.as_ref(),
                ValuePlacement::Inline,
                &ir,
                &mut ctx,
            )
        }))
        .is_err(),
        "Wasm must reject StringLit rather than pass input through"
    );

    let (escaped_ir, escaped_fn) = lower_map(r#""line\nnext""#);
    let FnDescriptor::Expr {
        expr: escaped_expr,
        return_type: escaped_type,
    } = &escaped_ir.fns[escaped_fn as usize]
    else {
        unreachable!()
    };
    let MapExpr::StringLit(escaped_sid) = escaped_expr else {
        panic!("escaped string must remain a StringLit");
    };
    assert_eq!(escaped_ir.get_string(*escaped_sid), "line\nnext");
    assert_eq!(
        compile_map_expr_to_js(escaped_expr, escaped_type.as_ref(), &escaped_ir),
        "\"line\\nnext\""
    );
}

#[test]
fn typescript_string_escape_is_strict_safe_for_nul_digits_controls_and_unicode() {
    let (ir, fn_id) = lower_map(r#""\01""#);
    let FnDescriptor::Expr { expr, return_type } = &ir.fns[fn_id as usize] else {
        unreachable!()
    };
    let js = compile_map_expr_to_js(expr, return_type.as_ref(), &ir);
    assert_eq!(js, "\"\\u00001\"");
    assert!(
        !js.contains("\\01"),
        "legacy octal escape must never be emitted"
    );

    let script = format!(
        r#""use strict";
const value = {js};
if (value.length !== 2 || value.charCodeAt(0) !== 0 || value.charCodeAt(1) !== 49) {{
  throw new Error("NUL-plus-digit changed value");
}}"#
    );
    let output = Command::new("node")
        .args(["--input-type=module", "--eval", &script])
        .output()
        .expect("Node.js is required for the TypeScript lexical-safety gate");
    assert!(
        output.status.success(),
        "Node rejected or misexecuted emitted JS: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    assert_eq!(
        ts_escape("\0\u{1}\u{8}\u{c}\u{7f}\u{2028}\u{2029}é😀"),
        "\\u0000\\u0001\\u0008\\u000C\\u007F\\u2028\\u2029é\\uD83D\\uDE00"
    );
}

#[test]
fn source_composites_reach_rust_typescript_and_wasm_with_one_domain() {
    let (ir, fn_id) = lower_map("(40u8 + 2u8)");
    let descriptor = &ir.fns[fn_id as usize];
    assert_eq!(
        descriptor,
        &FnDescriptor::Expr {
            expr: MapExpr::UIntLit(42),
            return_type: Some(TypeDesc::U8),
        }
    );

    let rust = formatted(hregex::emit_parse_hregex(
        &strategy(),
        "LiteralExactness",
        ir.find_rule("subject").unwrap(),
        &ir,
    ));
    assert!(rust.contains("push_leaf_with_u8(builder, 42u8)"));

    let FnDescriptor::Expr { expr, return_type } = descriptor else {
        unreachable!()
    };
    assert_eq!(
        compile_map_expr_to_js(expr, return_type.as_ref(), &ir),
        "42"
    );

    let mut wasm = WasmEmitter {
        module_name: "literal-exactness".into(),
        ws_regex_id: None,
    };
    let mut ctx = WasmEmitCtx::default();
    let wat = wasm.emit_map_expr(
        "(i32.const 0)".into(),
        expr,
        return_type.as_ref(),
        ValuePlacement::Inline,
        &ir,
        &mut ctx,
    );
    assert!(wat.contains("(then (i32.const 42))"));
}

#[test]
fn dynamic_comparison_nodes_retain_local_domains_for_ts_and_wasm() {
    let (wide_ir, wide_fn) = lower_map("input < 2u64");
    let FnDescriptor::Expr {
        expr: wide_expr,
        return_type: wide_type,
    } = &wide_ir.fns[wide_fn as usize]
    else {
        unreachable!()
    };
    assert_eq!(wide_type, &Some(TypeDesc::Bool));
    assert!(matches!(
        wide_expr,
        MapExpr::BinOp {
            op: MapBinOp::Lt,
            operand_domain: Some(NumericDomain::U64),
            ..
        }
    ));
    assert_eq!(
        compile_map_expr_to_js(wide_expr, wide_type.as_ref(), &wide_ir),
        "(__input < 2n)"
    );

    let (nested_ir, nested_fn) = lower_map("input < 2u32 && input >= 3i64");
    let FnDescriptor::Expr {
        expr: nested_expr,
        return_type: nested_type,
    } = &nested_ir.fns[nested_fn as usize]
    else {
        unreachable!()
    };
    let MapExpr::BinOp {
        op: MapBinOp::And,
        operand_domain: None,
        lhs,
        rhs,
    } = nested_expr
    else {
        panic!("nested comparisons must retain their boolean join");
    };
    assert!(matches!(
        lhs.as_ref(),
        MapExpr::BinOp {
            operand_domain: Some(NumericDomain::U32),
            ..
        }
    ));
    assert!(matches!(
        rhs.as_ref(),
        MapExpr::BinOp {
            operand_domain: Some(NumericDomain::I64),
            ..
        }
    ));
    assert_eq!(nested_type, &Some(TypeDesc::Bool));
    assert_eq!(
        compile_map_expr_to_js(nested_expr, nested_type.as_ref(), &nested_ir),
        "((__input < 2) && (__input >= 3n))"
    );

    let mut wasm = WasmEmitter {
        module_name: "literal-exactness".into(),
        ws_regex_id: None,
    };
    let mut ctx = WasmEmitCtx::default();
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            wasm.emit_map_expr(
                "(i32.const 0)".into(),
                nested_expr,
                nested_type.as_ref(),
                ValuePlacement::Inline,
                &nested_ir,
                &mut ctx,
            )
        }))
        .is_err(),
        "Wasm must fail closed on a dynamic typed comparison tree"
    );
}
