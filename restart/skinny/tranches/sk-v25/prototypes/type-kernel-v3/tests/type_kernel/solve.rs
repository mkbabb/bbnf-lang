use std::thread;

use bbnf_type_kernel_v3::type_kernel::solve::{Constraint, solve};
use bbnf_type_kernel_v3::type_kernel::store::{Primitive, StoreBuilder};
use bbnf_type_kernel_v3::type_kernel::{OperationContext, Origin, TypeErrorKind};

#[test]
fn zero_budget_fails_during_preprocessing_with_provenance() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let store = builder.finish([i64_ty]).unwrap();
    let constraint = Constraint::equal(i64_ty, i64_ty, Origin::new("zero", "tautology"));
    let mut operations = OperationContext::new(0);
    let error = solve(&store, &[constraint], [i64_ty], &mut operations).unwrap_err();
    assert_eq!(error.kind(), &TypeErrorKind::Exhausted { limit: 0 });
    assert!(!error.proof().nodes().is_empty());
    assert!(!error.proof().origins().is_empty());
}

#[test]
fn contradictory_bindings_have_a_minimal_causal_slice() {
    let mut builder = StoreBuilder::new();
    let (_, unknown) = builder.fresh_meta(0).unwrap();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let bool_ty = builder.primitive(Primitive::Bool).unwrap();
    let store = builder.finish([unknown, i64_ty, bool_ty]).unwrap();
    let constraints = [
        Constraint::equal(unknown, i64_ty, Origin::new("integer", "bind")),
        Constraint::equal(unknown, bool_ty, Origin::new("boolean", "conflict")),
        Constraint::equal(i64_ty, i64_ty, Origin::new("unrelated", "tautology")),
    ];
    let mut operations = OperationContext::unlimited();
    let error = solve(&store, &constraints, [unknown], &mut operations).unwrap_err();
    let sites = error
        .proof()
        .origins()
        .into_iter()
        .map(|origin| origin.site().to_owned())
        .collect::<Vec<_>>();
    assert!(sites.iter().any(|site| site == "integer"));
    assert!(sites.iter().any(|site| site == "boolean"));
    assert!(!sites.iter().any(|site| site == "unrelated"));
}

#[test]
fn twenty_thousand_deep_program_returns_typed_exhaustion_on_small_stack() {
    let result = thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut builder = StoreBuilder::new();
            let (_, mut left) = builder.fresh_meta(0).unwrap();
            let mut right = builder.primitive(Primitive::I64).unwrap();
            for _ in 0..20_000 {
                left = builder.option(left).unwrap();
                right = builder.option(right).unwrap();
            }
            let store = builder.finish([left, right]).unwrap();
            let constraints = [Constraint::equal(
                left,
                right,
                Origin::new("deep", "twenty-thousand options"),
            )];
            let mut operations = OperationContext::new(256);
            solve(&store, &constraints, [left], &mut operations).unwrap_err()
        })
        .unwrap()
        .join()
        .expect("no process or thread stack overflow");
    assert_eq!(result.kind(), &TypeErrorKind::Exhausted { limit: 256 });
    assert!(!result.proof().origins().is_empty());
}
