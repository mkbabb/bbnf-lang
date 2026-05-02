//! `path!` rejects an `Index` step targeted at a non-list field.
//!
//! AZ-IV.W5 T4 — fixture migrated from the synthetic Twitter-shape
//! fixture to the production registry. The CSS L4 `stylesheet` root's
//! `ruleList` field projects to `BoxedEnum`; indexing into a
//! BoxedEnum is invalid because BoxedEnum is not a list.

use bbnf::path::{CssL4, TypedPath};
use bbnf_path::path;

fn main() {
    // `stylesheet.ruleList` is a BoxedEnum scalar, not a list.
    let _: TypedPath<CssL4, ()> = path!(CssL4, "ruleList", 0);
}
