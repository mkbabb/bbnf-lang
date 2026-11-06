use bbnf_type_kernel_v3::type_kernel::solve::{Constraint, solve};
use bbnf_type_kernel_v3::type_kernel::store::{Primitive, StoreBuilder};
use bbnf_type_kernel_v3::type_kernel::{OperationContext, Origin, TypeErrorKind};

#[test]
fn ordinary_type_occurs_is_typed_and_attributed() {
    let mut builder = StoreBuilder::new();
    let (meta, unknown) = builder.fresh_meta(0).unwrap();
    let recursive = builder.option(unknown).unwrap();
    let store = builder.finish([unknown, recursive]).unwrap();
    let constraints = [Constraint::equal(
        unknown,
        recursive,
        Origin::new("occurs", "a = Option<a>"),
    )];
    let mut operations = OperationContext::unlimited();
    let error = solve(&store, &constraints, [unknown], &mut operations).unwrap_err();
    assert_eq!(
        error.kind(),
        &TypeErrorKind::TypeOccurs {
            meta: meta.ordinal()
        }
    );
    assert_eq!(error.proof().origins().len(), 1);
}

#[test]
fn constraint_permutations_have_one_canonical_solution() {
    let mut builder = StoreBuilder::new();
    let (_, a) = builder.fresh_meta(0).unwrap();
    let (_, b) = builder.fresh_meta(0).unwrap();
    let (_, c) = builder.fresh_meta(0).unwrap();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let store = builder.finish([a, b, c, i64_ty]).unwrap();
    let base = [
        Constraint::equal(a, b, Origin::new("ab", "equal")),
        Constraint::equal(b, c, Origin::new("bc", "equal")),
        Constraint::equal(c, i64_ty, Origin::new("ground", "i64")),
    ];
    let orders = [
        [0, 1, 2],
        [0, 2, 1],
        [1, 0, 2],
        [1, 2, 0],
        [2, 0, 1],
        [2, 1, 0],
    ];
    let mut identities = Vec::new();
    for order in orders {
        let constraints = order
            .iter()
            .map(|index| base[*index].clone())
            .collect::<Vec<_>>();
        let mut operations = OperationContext::unlimited();
        identities.push(
            solve(&store, &constraints, [a, b, c], &mut operations)
                .unwrap()
                .identity()
                .to_owned(),
        );
    }
    assert!(identities.windows(2).all(|window| window[0] == window[1]));
}
