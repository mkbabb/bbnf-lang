//! `path!` rejects an `Index` step targeted at a non-list layout.
//!
//! AZ-IV.W5 T4 — fixture on the production registry. LW2.TOPO-6b:
//! a `RuleReference` field now descends into the referenced rule's
//! layout (CSS L4's `stylesheet.ruleList` is that list, so indexing it
//! is valid). The JSON root `json = ws >> value << ws` is a struct with
//! one field, not a list, so indexing it is invalid.

use bbnf::path::{Json, TypedPath};
use bbnf_path::path;

fn main() {
    // The `json` root struct is not a list.
    let _: TypedPath<Json, ()> = path!(Json, 0);
}
