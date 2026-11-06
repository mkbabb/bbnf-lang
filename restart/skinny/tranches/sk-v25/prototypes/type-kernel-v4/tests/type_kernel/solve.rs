use bbnf_type_kernel_v4::*;

use super::super::operations;

pub mod row;
pub mod unify;
pub mod zonk;

#[test]
fn semantic_digest_excludes_process_scope_and_failed_solves_publish_nothing() {
    let first = super::super::primitive_solution(Primitive::I64, "value");
    let _unrelated = super::super::primitive_solution(Primitive::Bool, "other");
    let second = super::super::primitive_solution(Primitive::I64, "value");
    assert_eq!(first.semantic_digest(), second.semantic_digest());

    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let left = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("left");
    let right = builder
        .primitive(Primitive::Bool, &mut operations)
        .expect("right");
    builder
        .equal(
            left,
            right,
            Origin::new("contradiction", "load-bearing"),
            &mut operations,
        )
        .expect("constraint");
    let tautology = builder
        .primitive(Primitive::U8, &mut operations)
        .expect("tautology");
    builder
        .equal(
            tautology,
            tautology,
            Origin::new("unrelated", "tautology"),
            &mut operations,
        )
        .expect("tautology constraint");
    let (problem, _) = builder
        .close([(ExportKey::new("root"), left)], &mut operations)
        .expect("close");
    let error = solve(problem, &mut operations).expect_err("solve must fail atomically");
    let origins = error.proof().origins();
    assert!(
        origins
            .iter()
            .any(|origin| origin.site() == "contradiction")
    );
    assert!(!origins.iter().any(|origin| origin.site() == "unrelated"));
}

#[test]
fn zero_budget_is_typed_and_attributed() {
    let mut setup = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder.primitive(Primitive::I64, &mut setup).expect("type");
    let (problem, _) = builder
        .close([(ExportKey::new("root"), ty)], &mut setup)
        .expect("close");
    let error = solve(problem, &mut OperationContext::new(0)).expect_err("exhaustion");
    assert!(matches!(error.kind(), TypeErrorKind::Exhausted { .. }));
    assert!(!error.proof().nodes().is_empty());
}
