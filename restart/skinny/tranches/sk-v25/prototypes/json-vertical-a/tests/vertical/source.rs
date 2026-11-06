#[test]
fn source_is_exactly_locked() {
    let (source, ..) = super::stages();
    assert_eq!(source.bytes().len(), 537);
    assert_eq!(source.digest().hex(), super::SOURCE_SHA256);
}
