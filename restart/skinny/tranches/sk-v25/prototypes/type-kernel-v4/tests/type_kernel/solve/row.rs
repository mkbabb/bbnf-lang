use std::collections::BTreeSet;

use bbnf_type_kernel_v4::*;

use super::super::super::operations;

#[test]
fn record_and_variant_matrix_covers_closed_open_lacks_occurs_and_lowering() {
    for kind in [RowKind::Record, RowKind::Variant] {
        closed_permutation(kind);
        open_closed_symmetry(kind);
        open_open_disjoint(kind);
        closed_difference(kind);
        lacks_conflict(kind);
        direct_and_field_row_occurs(kind);
        level_capture(kind);
        extension_laws(kind);
    }
}

#[test]
fn duplicate_labels_fail_at_construction_for_both_row_kinds() {
    for kind in [RowKind::Record, RowKind::Variant] {
        let mut operations = operations();
        let mut builder = ProblemBuilder::new();
        let ty = builder
            .primitive(Primitive::I64, &mut operations)
            .expect("type");
        let error = builder
            .row(
                kind,
                [("x".to_owned(), ty), ("x".to_owned(), ty)],
                None,
                Origin::boundary("duplicate"),
                &mut operations,
            )
            .expect_err("duplicate label");
        assert_eq!(
            error.kind(),
            &TypeErrorKind::DuplicateRowLabel {
                label: "x".to_owned()
            }
        );
    }
}

#[test]
fn at_least_1024_genuinely_distinct_constraint_orders_hold_per_row_kind() {
    for kind in [RowKind::Record, RowKind::Variant] {
        let mut permutation = vec![0, 1, 2, 3, 4, 5, 6];
        let mut seen = BTreeSet::new();
        let mut expected = None;
        for _ in 0..1_024 {
            assert!(seen.insert(permutation.clone()));
            let digest = permuted_system(kind, &permutation);
            if let Some(expected) = &expected {
                assert_eq!(expected, &digest);
            } else {
                expected = Some(digest);
            }
            assert!(next_permutation(&mut permutation));
        }
        assert_eq!(seen.len(), 1_024);
    }
}

fn closed_permutation(kind: RowKind) {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let i64_type = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("i64");
    let bool_type = builder
        .primitive(Primitive::Bool, &mut operations)
        .expect("bool");
    let left_row = builder
        .row(
            kind,
            [("a".to_owned(), i64_type), ("b".to_owned(), bool_type)],
            None,
            Origin::boundary("left"),
            &mut operations,
        )
        .expect("left row");
    let right_row = builder
        .row(
            kind,
            [("b".to_owned(), bool_type), ("a".to_owned(), i64_type)],
            None,
            Origin::boundary("right"),
            &mut operations,
        )
        .expect("right row");
    let left = row_type(&mut builder, kind, left_row, &mut operations);
    let right = row_type(&mut builder, kind, right_row, &mut operations);
    builder
        .equal(
            left,
            right,
            Origin::boundary("permutation"),
            &mut operations,
        )
        .expect("constraint");
    let solution = close_solve(&mut operations, builder, [("left", left), ("right", right)]);
    let left_root = solution
        .export_root(&ExportKey::new("left"))
        .expect("left root");
    let right_root = solution
        .export_root(&ExportKey::new("right"))
        .expect("right root");
    assert_eq!(
        solution.root_bytes(left_root),
        solution.root_bytes(right_root)
    );
}

fn open_closed_symmetry(kind: RowKind) {
    let forward = extension_solution(kind, false);
    let reverse = extension_solution(kind, true);
    assert_eq!(forward.semantic_digest(), reverse.semantic_digest());
}

fn extension_solution(kind: RowKind, reverse: bool) -> TypeSolution {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let i64_type = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("i64");
    let bool_type = builder
        .primitive(Primitive::Bool, &mut operations)
        .expect("bool");
    let tail = builder.row_meta(kind, 1, &mut operations).expect("tail");
    let open_row = builder
        .row(
            kind,
            [("a".to_owned(), i64_type)],
            Some(tail),
            Origin::boundary("open"),
            &mut operations,
        )
        .expect("open row");
    let closed_row = builder
        .row(
            kind,
            [("a".to_owned(), i64_type), ("b".to_owned(), bool_type)],
            None,
            Origin::boundary("closed"),
            &mut operations,
        )
        .expect("closed row");
    let open = row_type(&mut builder, kind, open_row, &mut operations);
    let closed = row_type(&mut builder, kind, closed_row, &mut operations);
    let (left, right) = if reverse {
        (closed, open)
    } else {
        (open, closed)
    };
    builder
        .equal(left, right, Origin::boundary("extension"), &mut operations)
        .expect("constraint");
    close_solve(&mut operations, builder, [("root", open)])
}

fn open_open_disjoint(kind: RowKind) {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let i64_type = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("i64");
    let left_tail = builder
        .row_meta(kind, 2, &mut operations)
        .expect("left tail");
    let right_tail = builder
        .row_meta(kind, 1, &mut operations)
        .expect("right tail");
    let left_row = builder
        .row(
            kind,
            [("a".to_owned(), i64_type)],
            Some(left_tail),
            Origin::boundary("left"),
            &mut operations,
        )
        .expect("left row");
    let right_row = builder
        .row(
            kind,
            [("b".to_owned(), i64_type)],
            Some(right_tail),
            Origin::boundary("right"),
            &mut operations,
        )
        .expect("right row");
    let left = row_type(&mut builder, kind, left_row, &mut operations);
    let right = row_type(&mut builder, kind, right_row, &mut operations);
    builder
        .equal(left, right, Origin::boundary("join"), &mut operations)
        .expect("constraint");
    close_solve(&mut operations, builder, [("left", left), ("right", right)]);
}

fn closed_difference(kind: RowKind) {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("type");
    let left_row = builder
        .row(
            kind,
            [("a".to_owned(), ty)],
            None,
            Origin::boundary("left"),
            &mut operations,
        )
        .expect("left");
    let right_row = builder
        .row(
            kind,
            [("b".to_owned(), ty)],
            None,
            Origin::boundary("right"),
            &mut operations,
        )
        .expect("right");
    let left = row_type(&mut builder, kind, left_row, &mut operations);
    let right = row_type(&mut builder, kind, right_row, &mut operations);
    builder
        .equal(left, right, Origin::boundary("difference"), &mut operations)
        .expect("constraint");
    let (problem, _) = builder
        .close([(ExportKey::new("root"), left)], &mut operations)
        .expect("close");
    let error = solve(problem, &mut operations).expect_err("closed difference");
    assert_eq!(
        error.kind(),
        &TypeErrorKind::ClosedRow {
            missing: vec!["b".to_owned()],
            extra: vec!["a".to_owned()]
        }
    );
}

fn lacks_conflict(kind: RowKind) {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("type");
    let tail = builder.row_meta(kind, 1, &mut operations).expect("tail");
    builder
        .lacks(
            tail,
            "x",
            Origin::new("predicate", "x absent"),
            &mut operations,
        )
        .expect("lacks");
    let open = row_type(&mut builder, kind, tail, &mut operations);
    let field_row = builder
        .row(
            kind,
            [("x".to_owned(), ty)],
            None,
            Origin::new("field", "x present"),
            &mut operations,
        )
        .expect("field row");
    let field = row_type(&mut builder, kind, field_row, &mut operations);
    builder
        .equal(open, field, Origin::boundary("equation"), &mut operations)
        .expect("constraint");
    let (problem, _) = builder
        .close([(ExportKey::new("root"), open)], &mut operations)
        .expect("close");
    let error = solve(problem, &mut operations).expect_err("lacks conflict");
    assert!(matches!(error.kind(), TypeErrorKind::RowLacks { .. }));
    let origins = error.proof().origins();
    assert!(origins.iter().any(|origin| origin.site() == "predicate"));
    assert!(origins.iter().any(|origin| origin.site() == "equation"));
}

fn direct_and_field_row_occurs(kind: RowKind) {
    for through_field in [false, true] {
        let mut operations = operations();
        let mut builder = ProblemBuilder::new();
        let tail = builder.row_meta(kind, 1, &mut operations).expect("tail");
        let element = if through_field {
            row_type(&mut builder, kind, tail, &mut operations)
        } else {
            builder
                .primitive(Primitive::I64, &mut operations)
                .expect("element")
        };
        let recursive_row = builder
            .row(
                kind,
                [("x".to_owned(), element)],
                Some(tail),
                Origin::boundary("recursive"),
                &mut operations,
            )
            .expect("recursive row");
        let left = row_type(&mut builder, kind, tail, &mut operations);
        let right = row_type(&mut builder, kind, recursive_row, &mut operations);
        builder
            .equal(left, right, Origin::boundary("occurs"), &mut operations)
            .expect("constraint");
        let (problem, _) = builder
            .close([(ExportKey::new("root"), left)], &mut operations)
            .expect("close");
        let error = solve(problem, &mut operations).expect_err("row occurs");
        assert!(matches!(error.kind(), TypeErrorKind::RowOccurs { .. }));
    }
}

fn level_capture(kind: RowKind) {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let old = builder.row_meta(kind, 0, &mut operations).expect("old row");
    let young = builder.meta(2, &mut operations).expect("young type");
    let young_row = builder
        .row(
            kind,
            [("x".to_owned(), young)],
            None,
            Origin::boundary("capture"),
            &mut operations,
        )
        .expect("young row");
    let old_type = row_type(&mut builder, kind, old, &mut operations);
    let young_type = row_type(&mut builder, kind, young_row, &mut operations);
    builder
        .equal(
            old_type,
            young_type,
            Origin::boundary("capture"),
            &mut operations,
        )
        .expect("constraint");
    let solution = close_solve(&mut operations, builder, [("root", old_type)]);
    let root = solution.export_root(&ExportKey::new("root")).expect("root");
    let scheme = generalize(
        &solution,
        root,
        0,
        BindingForm::SyntacticValue,
        &mut operations,
    )
    .expect("generalize");
    assert_eq!(scheme.type_quantifiers(), 0);
}

fn extension_laws(kind: RowKind) {
    let first = extension_order(kind, false);
    let second = extension_order(kind, true);
    assert_eq!(first.semantic_digest(), second.semantic_digest());

    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("type");
    let tail = builder.row_meta(kind, 1, &mut operations).expect("tail");
    let one = builder
        .row(
            kind,
            [("x".to_owned(), ty)],
            Some(tail),
            Origin::boundary("one"),
            &mut operations,
        )
        .expect("one");
    let error = builder
        .row(
            kind,
            [("x".to_owned(), ty)],
            Some(one),
            Origin::boundary("duplicate"),
            &mut operations,
        )
        .expect_err("duplicate extension rejects at construction");
    assert!(matches!(
        error.kind(),
        TypeErrorKind::DuplicateRowLabel { .. }
    ));
}

fn extension_order(kind: RowKind, reverse: bool) -> TypeSolution {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("type");
    let tail = builder.row_meta(kind, 1, &mut operations).expect("tail");
    let labels = if reverse { ["b", "a"] } else { ["a", "b"] };
    let first = builder
        .row(
            kind,
            [(labels[0].to_owned(), ty)],
            Some(tail),
            Origin::boundary("first"),
            &mut operations,
        )
        .expect("first");
    let second = builder
        .row(
            kind,
            [(labels[1].to_owned(), ty)],
            Some(first),
            Origin::boundary("second"),
            &mut operations,
        )
        .expect("second");
    let root = row_type(&mut builder, kind, second, &mut operations);
    close_solve(&mut operations, builder, [("root", root)])
}

fn permuted_system(kind: RowKind, order: &[usize]) -> String {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let i64_type = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("i64");
    let bool_type = builder
        .primitive(Primitive::Bool, &mut operations)
        .expect("bool");
    let a = builder.meta(2, &mut operations).expect("a");
    let b = builder.meta(2, &mut operations).expect("b");
    let c = builder.meta(2, &mut operations).expect("c");
    let tail = builder.row_meta(kind, 1, &mut operations).expect("tail");
    let open_row = builder
        .row(
            kind,
            [("a".to_owned(), a)],
            Some(tail),
            Origin::boundary("open"),
            &mut operations,
        )
        .expect("open row");
    let closed_row = builder
        .row(
            kind,
            [("a".to_owned(), i64_type), ("b".to_owned(), bool_type)],
            None,
            Origin::boundary("closed"),
            &mut operations,
        )
        .expect("closed row");
    let open = row_type(&mut builder, kind, open_row, &mut operations);
    let closed = row_type(&mut builder, kind, closed_row, &mut operations);
    let tuple_left = builder.tuple([a, b], &mut operations).expect("left tuple");
    let tuple_right = builder
        .tuple([i64_type, bool_type], &mut operations)
        .expect("right tuple");
    for operation in order {
        match operation {
            0 => builder.equal(a, i64_type, Origin::boundary("a"), &mut operations),
            1 => builder.equal(b, bool_type, Origin::boundary("b"), &mut operations),
            2 => builder.equal(c, a, Origin::boundary("c"), &mut operations),
            3 => builder.equal(open, closed, Origin::boundary("row"), &mut operations),
            4 => builder.lacks(tail, "a", Origin::boundary("lacks"), &mut operations),
            5 => builder.equal(a, c, Origin::boundary("transitive"), &mut operations),
            6 => builder.equal(
                tuple_left,
                tuple_right,
                Origin::boundary("tuple"),
                &mut operations,
            ),
            _ => unreachable!(),
        }
        .expect("compatible operation");
    }
    close_solve(&mut operations, builder, [("root", open)])
        .semantic_digest()
        .as_str()
        .to_owned()
}

fn row_type(
    builder: &mut ProblemBuilder,
    kind: RowKind,
    row: DraftRowId,
    operations: &mut OperationContext,
) -> DraftTypeId {
    match kind {
        RowKind::Record => builder.record(row, operations).expect("record"),
        RowKind::Variant => builder.variant(row, operations).expect("variant"),
    }
}

fn close_solve<const N: usize>(
    operations: &mut OperationContext,
    builder: ProblemBuilder,
    exports: [(&str, DraftTypeId); N],
) -> TypeSolution {
    let (problem, _) = builder
        .close(
            exports
                .into_iter()
                .map(|(key, ty)| (ExportKey::new(key), ty)),
            operations,
        )
        .expect("close");
    solve(problem, operations).expect("solve")
}

fn next_permutation(values: &mut [usize]) -> bool {
    let Some(pivot) = (0..values.len() - 1)
        .rev()
        .find(|index| values[*index] < values[*index + 1])
    else {
        return false;
    };
    let successor = (pivot + 1..values.len())
        .rev()
        .find(|index| values[*index] > values[pivot])
        .expect("successor");
    values.swap(pivot, successor);
    values[pivot + 1..].reverse();
    true
}
