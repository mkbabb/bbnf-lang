//! Tests for the connected-components substrate (Tranche Y.5).

use bbnf_ir::passes::csp_strategy::components::UnionFind;

#[test]
fn singleton_components() {
    let mut uf = UnionFind::new(5);
    assert_eq!(uf.component_count(), 5);
    let comps = uf.components();
    assert_eq!(comps.len(), 5);
    for (_root, members) in comps {
        assert_eq!(members.len(), 1);
    }
}

#[test]
fn linear_chain() {
    let mut uf = UnionFind::new(5);
    uf.union(0, 1);
    uf.union(1, 2);
    uf.union(3, 4);
    assert_eq!(uf.component_count(), 2);
    let comps = uf.components();
    let mut sizes: Vec<usize> = comps.values().map(|v| v.len()).collect();
    sizes.sort();
    assert_eq!(sizes, vec![2, 3]);
}

#[test]
fn union_idempotent() {
    let mut uf = UnionFind::new(3);
    assert!(uf.union(0, 1));
    // Second union of the same pair reports "already joined".
    assert!(!uf.union(0, 1));
    assert!(!uf.union(1, 0));
}

#[test]
fn path_compression_works() {
    let mut uf = UnionFind::new(6);
    // Build a chain: 0 - 1 - 2 - 3 - 4 - 5
    for i in 0..5 {
        uf.union(i, i + 1);
    }
    // All six should share the same root after find().
    let root = uf.find(0);
    for i in 1..6 {
        assert_eq!(uf.find(i), root);
    }
}

#[test]
fn bidirectional_union_is_stable() {
    let mut uf = UnionFind::new(4);
    uf.union(0, 2);
    uf.union(1, 3);
    assert_eq!(uf.component_count(), 2);
    // Merging the two components gives one.
    uf.union(2, 3);
    assert_eq!(uf.component_count(), 1);
    let root = uf.find(0);
    for i in 1..4 {
        assert_eq!(uf.find(i), root);
    }
}
