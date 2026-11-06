use bbnf_type_kernel_v4::*;

use super::super::super::operations;

#[test]
fn sibling_rows_and_repeated_clean_builds_have_identical_flat_bytes() {
    let first = build();
    let second = build();
    assert_eq!(first.semantic_digest(), second.semantic_digest());
    let left = first
        .export_root(&ExportKey::new("left"))
        .expect("left root");
    let right = first
        .export_root(&ExportKey::new("right"))
        .expect("right root");
    assert_eq!(first.root_bytes(left), first.root_bytes(right));
}

fn build() -> TypeSolution {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let item = builder.meta(1, &mut operations).expect("item");
    let tail = builder
        .row_meta(RowKind::Record, 1, &mut operations)
        .expect("tail");
    let left_row = builder
        .row(
            RowKind::Record,
            [("x".to_owned(), item)],
            Some(tail),
            Origin::boundary("left"),
            &mut operations,
        )
        .expect("left row");
    let right_row = builder
        .row(
            RowKind::Record,
            [("x".to_owned(), item)],
            Some(tail),
            Origin::boundary("right"),
            &mut operations,
        )
        .expect("right row");
    let left = builder.record(left_row, &mut operations).expect("left");
    let right = builder.record(right_row, &mut operations).expect("right");
    let i64_type = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("i64");
    builder
        .equal(item, i64_type, Origin::boundary("item"), &mut operations)
        .expect("item constraint");
    let empty = builder
        .row(
            RowKind::Record,
            [],
            None,
            Origin::boundary("empty"),
            &mut operations,
        )
        .expect("empty row");
    let empty_record = builder
        .record(empty, &mut operations)
        .expect("empty record");
    let tail_record = builder.record(tail, &mut operations).expect("tail record");
    builder
        .equal(
            tail_record,
            empty_record,
            Origin::boundary("close tail"),
            &mut operations,
        )
        .expect("tail constraint");
    let (problem, _) = builder
        .close(
            [
                (ExportKey::new("left"), left),
                (ExportKey::new("right"), right),
            ],
            &mut operations,
        )
        .expect("close");
    solve(problem, &mut operations).expect("solve")
}
