use std::panic::{AssertUnwindSafe, catch_unwind};

use bbnf_type_kernel_v3::type_kernel::OperationContext;
use bbnf_type_kernel_v3::type_kernel::store::{BuildErrorKind, Primitive, StoreBuilder};

#[test]
fn explicit_rec_vars_close_nested_outer_use_compositionally() {
    let mut builder = StoreBuilder::new();
    let outer = builder.fresh_rec_var().expect("outer identity");
    let outer_ref = builder.rec_var(outer).expect("prebuilt outer reference");
    let inner = builder.fresh_rec_var().expect("inner identity");
    let inner_ref = builder.rec_var(inner).expect("inner reference");
    let pair = builder
        .tuple([outer_ref, inner_ref])
        .expect("prebuilt terms compose under the inner binder");
    let inner_mu = builder.mu(inner, pair).expect("inner occurs exactly");
    let root = builder.mu(outer, inner_mu).expect("outer occurs exactly");
    let store = builder.finish([root]).expect("closed recursive root");

    let mut operations = OperationContext::unlimited();
    assert_eq!(
        store.fingerprint(root, &mut operations).unwrap(),
        "mu0.mu1.(^0,^1)"
    );
}

#[test]
fn nested_vacuity_is_rejected_at_the_named_binder() {
    let mut builder = StoreBuilder::new();
    let outer = builder.fresh_rec_var().unwrap();
    let outer_ref = builder.rec_var(outer).unwrap();
    let inner = builder.fresh_rec_var().unwrap();
    let error = builder.mu(inner, outer_ref).unwrap_err();
    assert_eq!(error.kind(), &BuildErrorKind::VacuousRecursion(inner));
    assert_eq!(error.site(), "store.mu");
}

#[test]
fn panic_cannot_leave_binder_state_behind() {
    let mut builder = StoreBuilder::new();
    let _ = catch_unwind(AssertUnwindSafe(|| {
        let rec = builder.fresh_rec_var().unwrap();
        let _ = builder.rec_var(rec).unwrap();
        panic!("construction callback aborted")
    }));
    let root = builder.primitive(Primitive::I64).unwrap();
    builder
        .finish([root])
        .expect("there is no active-binder stack to poison");
}

#[test]
fn alpha_equivalent_recursion_has_the_same_fingerprint() {
    fn recursive_option() -> (
        bbnf_type_kernel_v3::type_kernel::store::TypeStore,
        bbnf_type_kernel_v3::type_kernel::store::TypeId,
    ) {
        let mut builder = StoreBuilder::new();
        let binder = builder.fresh_rec_var().unwrap();
        let recursive = builder.rec_var(binder).unwrap();
        let body = builder.option(recursive).unwrap();
        let root = builder.mu(binder, body).unwrap();
        (builder.finish([root]).unwrap(), root)
    }
    let (left, left_root) = recursive_option();
    let (right, right_root) = recursive_option();
    let mut left_ops = OperationContext::unlimited();
    let mut right_ops = OperationContext::unlimited();
    assert_eq!(
        left.fingerprint(left_root, &mut left_ops).unwrap(),
        right.fingerprint(right_root, &mut right_ops).unwrap()
    );
}
