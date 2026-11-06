use bbnf_type_kernel_v4::*;

use super::super::operations;

#[test]
fn qualified_rows_instantiate_fresh_and_expansive_rigids_are_reused() {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let tail = builder
        .row_meta(RowKind::Record, 2, &mut operations)
        .expect("tail");
    builder
        .lacks(
            tail,
            "reserved",
            Origin::boundary("qualified-lacks"),
            &mut operations,
        )
        .expect("lacks");
    let record = builder.record(tail, &mut operations).expect("record");
    let (problem, _) = builder
        .close([(ExportKey::new("root"), record)], &mut operations)
        .expect("close");
    let solution = solve(problem, &mut operations).expect("solve");
    let root = solution.export_root(&ExportKey::new("root")).expect("root");
    let scheme = generalize(
        &solution,
        root,
        0,
        BindingForm::SyntacticValue,
        &mut operations,
    )
    .expect("generalize");
    assert_eq!(scheme.row_quantifiers(), 1);

    let mut target = ProblemBuilder::new();
    let mut rigids = RigidContext::new();
    let first = instantiate(
        &scheme,
        &mut target,
        &mut rigids,
        1,
        Origin::boundary("first"),
        &mut operations,
    )
    .expect("first instance")
    .root()
    .expect("first root");
    let second = instantiate(
        &scheme,
        &mut target,
        &mut rigids,
        1,
        Origin::boundary("second"),
        &mut operations,
    )
    .expect("second instance")
    .root()
    .expect("second root");
    let i64_type = target
        .primitive(Primitive::I64, &mut operations)
        .expect("i64");
    let bool_type = target
        .primitive(Primitive::Bool, &mut operations)
        .expect("bool");
    let first_row = target
        .row(
            RowKind::Record,
            [("left".to_owned(), i64_type)],
            None,
            Origin::boundary("first row"),
            &mut operations,
        )
        .expect("first row");
    let second_row = target
        .row(
            RowKind::Record,
            [("right".to_owned(), bool_type)],
            None,
            Origin::boundary("second row"),
            &mut operations,
        )
        .expect("second row");
    let first_closed = target
        .record(first_row, &mut operations)
        .expect("first closed");
    let second_closed = target
        .record(second_row, &mut operations)
        .expect("second closed");
    target
        .equal(
            first,
            first_closed,
            Origin::boundary("first equality"),
            &mut operations,
        )
        .expect("first equality");
    target
        .equal(
            second,
            second_closed,
            Origin::boundary("second equality"),
            &mut operations,
        )
        .expect("second equality");
    let (problem, _) = target
        .close(
            [
                (ExportKey::new("first"), first),
                (ExportKey::new("second"), second),
            ],
            &mut operations,
        )
        .expect("close instances");
    solve(problem, &mut operations).expect("fresh row instances solve independently");

    let expansive = generalize(&solution, root, 0, BindingForm::Expansive, &mut operations)
        .expect("expansive scheme");
    assert_eq!(expansive.row_quantifiers(), 0);
}

#[test]
fn deep_scheme_traversal_is_iterative_and_budgeted() {
    let mut setup = OperationContext::new(20_000_000);
    let mut builder = ProblemBuilder::new();
    let mut ty = builder.meta(2, &mut setup).expect("meta");
    for _ in 0..20_000 {
        ty = builder.option(ty, &mut setup).expect("option");
    }
    let (problem, _) = builder
        .close([(ExportKey::new("deep"), ty)], &mut setup)
        .expect("close");
    let solution = solve(problem, &mut setup).expect("solve");
    let root = solution.export_root(&ExportKey::new("deep")).expect("root");
    let error = generalize(
        &solution,
        root,
        0,
        BindingForm::SyntacticValue,
        &mut OperationContext::new(256),
    )
    .expect_err("small budget");
    assert!(matches!(error.kind(), TypeErrorKind::Exhausted { .. }));

    let thread = std::thread::Builder::new()
        .stack_size(512 * 1_024)
        .spawn(move || {
            let mut operations = OperationContext::new(20_000_000);
            let scheme = generalize(
                &solution,
                root,
                0,
                BindingForm::SyntacticValue,
                &mut operations,
            )
            .expect("deep generalize");
            let mut builder = ProblemBuilder::new();
            instantiate(
                &scheme,
                &mut builder,
                &mut RigidContext::new(),
                1,
                Origin::boundary("deep instantiate"),
                &mut operations,
            )
            .expect("deep instantiate");
            operations.used()
        })
        .expect("spawn");
    assert!(thread.join().expect("join") > 20_000);
}
