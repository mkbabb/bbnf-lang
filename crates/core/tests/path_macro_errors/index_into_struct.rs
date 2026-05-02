//! `path!` rejects an `Index` step targeted at a non-list field.

use bbnf::path::{Json, TypedPath};
use bbnf_path::path;

fn main() {
    // `Document.id` is an `F64` scalar — indexing into it is invalid.
    let _: TypedPath<Json, ()> = path!(Json, "id", 0);
}
