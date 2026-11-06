#[cfg(not(feature = "architecture-test-seams"))]
#[test]
fn architecture_test_seams_are_explicitly_enabled() {
    panic!("architecture tests require the `xtask/architecture-test-seams` feature");
}

#[cfg(feature = "architecture-test-seams")]
#[test]
fn architecture_test_seams_are_explicitly_enabled() {}
