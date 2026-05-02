//! AZ-IV.W2.4 — negative compile fixtures for the `path!` proc-macro.
//!
//! Each fixture under `tests/path_macro_errors/*.rs` is a compile-fail
//! input the macro must reject with a `proc_macro2::Span`-anchored
//! diagnostic naming the offending segment, the resolved struct type,
//! and the valid alternatives at the failure position.
//!
//! Hard-gate W2.4 sub-clause: `path!(Json, "statuses", 0, "nope")`
//! fails compilation with a diagnostic naming `nope`, the resolved
//! struct type for `statuses[0]`, and valid alternatives.

#[test]
fn compile_fail_fixtures() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/path_macro_errors/unknown_field.rs");
    t.compile_fail("tests/path_macro_errors/unknown_marker.rs");
    t.compile_fail("tests/path_macro_errors/index_into_struct.rs");
    t.compile_fail("tests/path_macro_errors/empty_path.rs");
}
