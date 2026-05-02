//! `path!` requires at least one segment after the grammar marker.

use bbnf_path::path;

fn main() {
    let _ = path!(bbnf::path::Json);
}
