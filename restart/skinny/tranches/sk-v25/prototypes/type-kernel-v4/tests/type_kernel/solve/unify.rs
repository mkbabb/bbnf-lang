use bbnf_type_kernel_v4::*;

use super::super::super::operations;

#[test]
fn type_occurs_walks_rows_and_prior_substitutions() {
    for reverse in [false, true] {
        let mut operations = operations();
        let mut builder = ProblemBuilder::new();
        let meta = builder.meta(1, &mut operations).expect("meta");
        let row = builder
            .row(
                RowKind::Record,
                [("value".to_owned(), meta)],
                None,
                Origin::boundary("row"),
                &mut operations,
            )
            .expect("row");
        let record = builder.record(row, &mut operations).expect("record");
        let (left, right) = if reverse {
            (record, meta)
        } else {
            (meta, record)
        };
        builder
            .equal(left, right, Origin::boundary("occurs"), &mut operations)
            .expect("constraint");
        let (problem, _) = builder
            .close([(ExportKey::new("root"), meta)], &mut operations)
            .expect("close");
        let error = solve(problem, &mut operations).expect_err("occurs must fail");
        assert!(matches!(error.kind(), TypeErrorKind::TypeOccurs { .. }));
    }
}
