use bbnf_ir::CharSet128;

#[test]
fn basic_operations() {
    let mut set = CharSet128::new();
    assert!(set.is_empty());

    set.add(b'a');
    set.add(b'z');
    assert!(set.has(b'a'));
    assert!(set.has(b'z'));
    assert!(!set.has(b'b'));
    assert_eq!(set.len(), 2);
}

#[test]
fn union_and_disjoint() {
    let mut a = CharSet128::new();
    a.add(b'a');
    a.add(b'b');

    let mut b = CharSet128::new();
    b.add(b'c');
    b.add(b'd');

    assert!(a.is_disjoint(&b));

    a.union(&b);
    assert!(a.has(b'a'));
    assert!(a.has(b'd'));
    assert_eq!(a.len(), 4);
}

#[test]
fn roundtrip_u32x4() {
    let mut set = CharSet128::new();
    set.add(b'A');
    set.add(b'Z');
    set.add(b'0');
    set.add(b'9');

    let u32x4 = set.to_u32x4();
    let roundtripped = CharSet128::from_u32x4(&u32x4);
    assert_eq!(set, roundtripped);
}

#[test]
fn iter() {
    let mut set = CharSet128::new();
    set.add(b'x');
    set.add(b'y');
    set.add(b'z');

    let collected: Vec<u8> = set.iter().collect();
    assert_eq!(collected, vec![b'x', b'y', b'z']);
}
