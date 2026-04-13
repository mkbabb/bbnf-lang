//! Bootstrap crate for the self-hosted BBNF grammar parser.
//!
//! `cargo expand -p bbnf-bootstrap` produces the generated parser code.
//! Post-process the output to extract the `impl BbnfBootstrap { ... }` block
//! and embed it in `crates/core/src/grammar/generated.rs`.
//!
//! Usage:
//! ```bash
//! scripts/bootstrap-bbnf.sh
//! ```

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf")]
pub struct BbnfBootstrap;
