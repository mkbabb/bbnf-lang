use std::panic::{catch_unwind, AssertUnwindSafe};

use bbnf_type_kernel_v2::{
    solve, Budget, Constraint, Origin, Primitive, RowSpec, SolveError, StoreBuilder,
};

#[test]
fn public_error_corpus_never_panics() {
    for index in 0..256 {
        let result = catch_unwind(AssertUnwindSafe(|| {
            let mut first = StoreBuilder::new();
            let foreign = first.primitive(Primitive::I64).unwrap();
            let foreign_row = first.fresh_row().unwrap();
            let foreign_store = first.finish();

            let mut second = StoreBuilder::new();
            let local = second.primitive(Primitive::Bool).unwrap();
            assert!(second.list(foreign).is_err());
            assert!(RowSpec::closed([("dup", local), ("dup", local)]).is_err());
            let local_store = second.finish();
            let constraint = Constraint::equal(
                foreign,
                local,
                Origin::new(format!("case-{index}"), "cross revision"),
            );
            assert_eq!(
                solve(&foreign_store, &[constraint], Budget::default()),
                Err(SolveError::CrossStore)
            );
            let lacks = Constraint::lacks(
                foreign_row,
                "x",
                Origin::new(format!("case-{index}"), "cross revision row"),
            );
            assert_eq!(
                solve(&local_store, &[lacks], Budget::default()),
                Err(SolveError::CrossStore)
            );
        }));
        assert!(
            result.is_ok(),
            "public API panicked for corpus case {index}"
        );
    }
}
