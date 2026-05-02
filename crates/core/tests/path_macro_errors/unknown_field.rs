//! Hard-gate W2.4 sub-clause: an invalid path fails compilation with a
//! diagnostic naming the offending segment (`nope`), the resolved
//! struct type (`Status`), and the valid alternatives.

use bbnf::path::{Json, TypedPath};
use bbnf_path::path;

fn main() {
    let _: TypedPath<Json, ()> = path!(Json, "statuses", 0, "nope");
}
