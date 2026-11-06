use bbnf_type_kernel_v3::type_kernel::scheme::{BindingForm, RigidContext, Scheme};
use bbnf_type_kernel_v3::type_kernel::solve::{Constraint, solve};
use bbnf_type_kernel_v3::type_kernel::store::{Primitive, RowSpec, StoreBuilder};
use bbnf_type_kernel_v3::type_kernel::{OperationContext, Origin};

#[test]
fn qualified_lacks_survives_generalize_and_instantiate() {
    let mut builder = StoreBuilder::new();
    let (_, field) = builder.fresh_meta(1).unwrap();
    let tail = builder.fresh_row(1).unwrap();
    let root = builder
        .record(RowSpec::open([("value", field)], tail).unwrap())
        .unwrap();
    let store = builder.finish([root]).unwrap();
    let constraints = [Constraint::lacks(
        tail,
        "forbidden",
        Origin::new("scheme", "qualified lacks"),
    )];
    let mut operations = OperationContext::unlimited();
    let solution = solve(&store, &constraints, [root], &mut operations).unwrap();
    let scheme = Scheme::generalize(&solution, root, 0, BindingForm::SyntacticValue).unwrap();
    assert_eq!(scheme.quantified_type_count(), 1);
    assert_eq!(scheme.quantified_row_count(), 1);
    assert_eq!(
        scheme.predicate_count(),
        2,
        "explicit field lacks plus qualified lacks"
    );

    let mut target = StoreBuilder::new();
    let mut rigids = RigidContext::new();
    let first = scheme
        .instantiate(
            &mut target,
            &mut rigids,
            1,
            Origin::new("instantiation-1", "fresh"),
        )
        .unwrap();
    let second = scheme
        .instantiate(
            &mut target,
            &mut rigids,
            1,
            Origin::new("instantiation-2", "fresh"),
        )
        .unwrap();
    assert_ne!(
        first.root(),
        second.root(),
        "quantified instances are fresh"
    );
    assert_eq!(first.constraints().len(), scheme.predicate_count());
    assert_eq!(second.constraints().len(), scheme.predicate_count());
}

#[test]
fn value_restriction_keeps_expansive_environment_variables_rigid_and_shared() {
    let mut builder = StoreBuilder::new();
    let (_, unknown) = builder.fresh_meta(2).unwrap();
    let store = builder.finish([unknown]).unwrap();
    let mut operations = OperationContext::unlimited();
    let solution = solve(&store, &[], [unknown], &mut operations).unwrap();
    let scheme = Scheme::generalize(&solution, unknown, 0, BindingForm::Expansive).unwrap();
    assert_eq!(scheme.quantified_type_count(), 0);

    let mut target = StoreBuilder::new();
    let mut rigids = RigidContext::new();
    let first = scheme
        .instantiate(
            &mut target,
            &mut rigids,
            2,
            Origin::new("first", "expansive"),
        )
        .unwrap();
    let second = scheme
        .instantiate(
            &mut target,
            &mut rigids,
            2,
            Origin::new("second", "expansive"),
        )
        .unwrap();
    assert_eq!(
        first.root(),
        second.root(),
        "owned rigid context shares the environment variable"
    );
}

#[test]
fn variables_at_the_environment_level_do_not_generalize() {
    let mut builder = StoreBuilder::new();
    let (_, unknown) = builder.fresh_meta(0).unwrap();
    let store = builder.finish([unknown]).unwrap();
    let mut operations = OperationContext::unlimited();
    let solution = solve(&store, &[], [unknown], &mut operations).unwrap();
    let scheme = Scheme::generalize(&solution, unknown, 0, BindingForm::SyntacticValue).unwrap();
    assert_eq!(scheme.quantified_type_count(), 0);
}

#[test]
fn nested_recursive_identities_survive_scheme_round_trip() {
    let mut builder = StoreBuilder::new();
    let outer = builder.fresh_rec_var().unwrap();
    let outer_ref = builder.rec_var(outer).unwrap();
    let inner = builder.fresh_rec_var().unwrap();
    let inner_ref = builder.rec_var(inner).unwrap();
    let body = builder.tuple([outer_ref, inner_ref]).unwrap();
    let inner_mu = builder.mu(inner, body).unwrap();
    let root = builder.mu(outer, inner_mu).unwrap();
    let store = builder.finish([root]).unwrap();
    let mut operations = OperationContext::unlimited();
    let solution = solve(&store, &[], [root], &mut operations).unwrap();
    let scheme = Scheme::generalize(&solution, root, 0, BindingForm::SyntacticValue).unwrap();

    let mut target = StoreBuilder::new();
    let mut rigids = RigidContext::new();
    let instantiated = scheme
        .instantiate(
            &mut target,
            &mut rigids,
            0,
            Origin::new("recursion", "round trip"),
        )
        .unwrap();
    let target_store = target.finish([instantiated.root()]).unwrap();
    let mut source_ops = OperationContext::unlimited();
    let mut target_ops = OperationContext::unlimited();
    assert_eq!(
        store.fingerprint(root, &mut source_ops).unwrap(),
        target_store
            .fingerprint(instantiated.root(), &mut target_ops)
            .unwrap()
    );
}

#[test]
fn a_qualified_instance_rejects_reintroduced_labels() {
    let mut builder = StoreBuilder::new();
    let tail = builder.fresh_row(1).unwrap();
    let root = builder
        .record(RowSpec::open(Vec::<(String, _)>::new(), tail).unwrap())
        .unwrap();
    let store = builder.finish([root]).unwrap();
    let mut operations = OperationContext::unlimited();
    let solution = solve(
        &store,
        &[Constraint::lacks(
            tail,
            "x",
            Origin::new("scheme", "r lacks x"),
        )],
        [root],
        &mut operations,
    )
    .unwrap();
    let scheme = Scheme::generalize(&solution, root, 0, BindingForm::SyntacticValue).unwrap();

    let mut target = StoreBuilder::new();
    let mut rigids = RigidContext::new();
    let instance = scheme
        .instantiate(
            &mut target,
            &mut rigids,
            1,
            Origin::new("instance", "qualified"),
        )
        .unwrap();
    let i64_ty = target.primitive(Primitive::I64).unwrap();
    let closed = target
        .record(RowSpec::closed([("x", i64_ty)]).unwrap())
        .unwrap();
    let target_store = target.finish([instance.root(), closed]).unwrap();
    let mut constraints = instance.constraints().to_vec();
    constraints.push(Constraint::equal(
        instance.root(),
        closed,
        Origin::new("reintroduce", "x field"),
    ));
    let mut target_operations = OperationContext::unlimited();
    let error = solve(
        &target_store,
        &constraints,
        [instance.root()],
        &mut target_operations,
    )
    .unwrap_err();
    assert!(matches!(
        error.kind(),
        bbnf_type_kernel_v3::type_kernel::TypeErrorKind::RowLacks { label, .. }
            if label == "x"
    ));
}
