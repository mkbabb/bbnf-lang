use bbnf_type_kernel_v3::type_kernel::solve::{Constraint, SolvedType, solve};
use bbnf_type_kernel_v3::type_kernel::store::{Primitive, RowSpec, StoreBuilder};
use bbnf_type_kernel_v3::type_kernel::{OperationContext, Origin};

#[test]
fn sibling_rows_are_both_fully_zonked_and_repeated_runs_are_idempotent() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let tail = builder.fresh_row(0).unwrap();
    let first = builder
        .record(RowSpec::open([("a", i64_ty)], tail).unwrap())
        .unwrap();
    let second = builder
        .record(RowSpec::open([("b", i64_ty)], tail).unwrap())
        .unwrap();
    let closed = builder
        .record(RowSpec::closed([("a", i64_ty), ("c", i64_ty)]).unwrap())
        .unwrap();
    let store = builder.finish([first, second, closed]).unwrap();
    let constraints = [Constraint::equal(
        first,
        closed,
        Origin::new("zonk", "bind shared tail"),
    )];
    let mut first_ops = OperationContext::unlimited();
    let solution = solve(&store, &constraints, [first, second], &mut first_ops).unwrap();
    let SolvedType::Record(second_row) = solution.type_of(second).unwrap() else {
        panic!("second sibling must remain a record")
    };
    assert!(second_row.fields().contains_key("b"));
    assert!(second_row.fields().contains_key("c"));
    assert!(second_row.tail().is_none());

    let mut second_ops = OperationContext::unlimited();
    let repeated = solve(&store, &constraints, [first, second], &mut second_ops).unwrap();
    assert_eq!(solution.identity(), repeated.identity());
    assert_eq!(solution.type_of(second), repeated.type_of(second));
}
