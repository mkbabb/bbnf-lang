//! AZ-IV.W2.4 hard-gate sub-clause: an invalid path fails compilation
//! with a diagnostic naming the offending segment, the resolved struct
//! type, and the valid alternatives.
//!
//! AZ-IV.W5 T4 — fixture migrated from the synthetic per-grammar
//! fixture (`Status` Twitter shape) to the production registry. The
//! JSON grammar's `value` rule is a TaggedEnum; a `Field` segment on
//! a tagged-enum is a type error — the macro must surface valid
//! alternatives drawn from the production registry.

use bbnf::path::{Json, TypedPath};
use bbnf_path::path;

fn main() {
    // `value` is a tagged enum; `nope` is not a Field-style descent.
    let _: TypedPath<Json, ()> = path!(Json, "nope");
}
