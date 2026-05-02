//! `path!` rejects an unknown grammar marker with a diagnostic listing
//! the supported markers.

use bbnf_path::path;

fn main() {
    let _ = path!(NotARealGrammar, "foo");
}
