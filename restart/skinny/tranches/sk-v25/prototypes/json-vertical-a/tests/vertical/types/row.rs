use bbnf_json_stages_prototype::vertical::types::{Type, row::ClosedRow};
use std::collections::BTreeMap;

#[test]
fn closed_rows_are_permutation_stable_and_conflicts_fail() {
    let left = ClosedRow(BTreeMap::from([
        ("key".into(), Type::String),
        ("value".into(), Type::Json),
    ]));
    let right = ClosedRow(BTreeMap::from([
        ("value".into(), Type::Json),
        ("key".into(), Type::String),
    ]));
    for _ in 0..1_024 {
        assert_eq!(left.unify(&right).expect("same row"), left);
    }
    let conflict = ClosedRow(BTreeMap::from([
        ("key".into(), Type::Bool),
        ("value".into(), Type::Json),
    ]));
    assert!(left.unify(&conflict).is_err());
}
