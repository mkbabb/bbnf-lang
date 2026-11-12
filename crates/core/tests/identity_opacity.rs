#[test]
fn local_ordinals_and_capability_construction_stay_private() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/identity/*.rs");
}
