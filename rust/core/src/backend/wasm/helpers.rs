//! WASM codegen helper functions.

pub fn unescape_literal(s: &str) -> String {
    crate::backend::unescape_literal(s)
}
