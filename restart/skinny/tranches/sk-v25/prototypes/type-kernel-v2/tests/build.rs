use bbnf_type_kernel_v2::{BuildError, Primitive, RowSpec, StoreBuilder};

#[test]
fn terms_are_interned_and_store_revision_is_immutable() {
    let mut builder = StoreBuilder::new();
    let integer_a = builder.primitive(Primitive::I64).unwrap();
    let integer_b = builder.primitive(Primitive::I64).unwrap();
    let list_a = builder.list(integer_a).unwrap();
    let list_b = builder.list(integer_b).unwrap();
    assert_eq!(integer_a, integer_b);
    assert_eq!(list_a, list_b);

    let store = builder.finish();
    assert_eq!(store.fingerprint(list_a).unwrap(), "List<I64>");
    assert_eq!(store.len(), 2);
    assert_ne!(store.revision(), 0);
}

#[test]
fn cross_store_references_and_duplicate_labels_are_rejected() {
    let mut first = StoreBuilder::new();
    let foreign = first.primitive(Primitive::I64).unwrap();
    let foreign_row = first.fresh_row().unwrap();

    let mut second = StoreBuilder::new();
    assert_eq!(second.list(foreign), Err(BuildError::CrossStore));
    let local = second.primitive(Primitive::Bool).unwrap();
    let row = RowSpec::open([("ok", local)], foreign_row).unwrap();
    assert_eq!(second.record(row), Err(BuildError::CrossStore));
    assert_eq!(
        RowSpec::closed([("x", local), ("x", local)]),
        Err(BuildError::DuplicateLabel("x".into()))
    );
}

#[test]
fn binders_are_unforgeable_scoped_and_alpha_stable() {
    fn recursive_option() -> (bbnf_type_kernel_v2::TypeStore, bbnf_type_kernel_v2::TypeId) {
        let mut builder = StoreBuilder::new();
        let root = builder
            .mu(|builder, binder| {
                let recur = builder.bound(binder)?;
                builder.option(recur)
            })
            .unwrap();
        (builder.finish(), root)
    }

    let (first, first_root) = recursive_option();
    let (second, second_root) = recursive_option();
    assert_eq!(first.fingerprint(first_root).unwrap(), "mu.Option<^0>");
    assert_eq!(
        first.fingerprint(first_root).unwrap(),
        second.fingerprint(second_root).unwrap()
    );
}

#[test]
fn escaped_and_vacuous_recursive_binders_are_rejected() {
    let mut builder = StoreBuilder::new();
    let mut escaped = None;
    let recursive = builder
        .mu(|builder, binder| {
            let bound = builder.bound(binder)?;
            escaped = Some(bound);
            builder.option(bound)
        })
        .unwrap();
    assert!(builder.option(recursive).is_ok());
    assert_eq!(
        builder.option(escaped.unwrap()),
        Err(BuildError::BinderEscapes)
    );

    let integer = builder.primitive(Primitive::I64).unwrap();
    assert_eq!(
        builder.mu(|_, _| Ok(integer)),
        Err(BuildError::BinderEscapes)
    );
}
