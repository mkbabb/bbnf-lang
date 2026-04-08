//! `#[derive(Language)]` behavioral tests.

use egraph::{Id, Language};
use egraph_derive::Language;

#[derive(Clone, Eq, PartialEq, Hash, Debug, Language)]
enum Expr {
    // Leaf: no children, no tag.
    Num(i64),
    // Single child.
    Neg(#[language(child)] Id),
    // Variadic children.
    Sum(#[language(children)] Box<[Id]>),
    // Named fields with a single child + scalar leaves.
    Pow {
        #[language(child)]
        base: Id,
        exp: u32,
    },
    // Unit variant.
    Empty,
}

#[test]
fn leaf_has_no_children() {
    let n = Expr::Num(42);
    assert_eq!(n.children(), &[]);
}

#[test]
fn unit_has_no_children() {
    let e = Expr::Empty;
    assert_eq!(e.children(), &[]);
}

#[test]
fn single_child_visible() {
    let n = Expr::Neg(Id(7));
    assert_eq!(n.children(), &[Id(7)]);
}

#[test]
fn variadic_children_visible() {
    let s = Expr::Sum(Box::new([Id(1), Id(2), Id(3)]));
    assert_eq!(s.children(), &[Id(1), Id(2), Id(3)]);
}

#[test]
fn named_single_child_visible() {
    let p = Expr::Pow {
        base: Id(5),
        exp: 2,
    };
    assert_eq!(p.children(), &[Id(5)]);
}

#[test]
fn children_mut_replaces_ids() {
    let mut n = Expr::Neg(Id(0));
    for c in n.children_mut() {
        *c = Id(99);
    }
    assert_eq!(n.children(), &[Id(99)]);

    let mut s = Expr::Sum(Box::new([Id(0), Id(1), Id(2)]));
    for c in s.children_mut() {
        c.0 += 10;
    }
    assert_eq!(s.children(), &[Id(10), Id(11), Id(12)]);
}
