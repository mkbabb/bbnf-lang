//! xtask-emitted grammar generated code.
//!
//! Each `pub mod <ident>;` corresponds to a grammar listed in
//! `[workspace.metadata.bbnf.grammars]`. `cargo xtask regen --grammar
//! <ident>` refreshes the per-grammar source on disk; consumers
//! `use bbnf::grammar::generated::<ident>::*` to reach each grammar's
//! marker struct (e.g., `BbnfBootstrap`, `JsonParser`, `CssL4Parser`)
//! and the per-grammar emitted items.
//!
//! Pre-B2 the workspace shipped a single monolithic `generated.rs`
//! produced by `scripts/bootstrap-bbnf.sh`. B2.W0.c retired that path:
//! the IR pipeline + emission move into `cargo xtask regen`, and the
//! per-grammar source files live under this directory.
//!
//! The BBNF self-host re-exports `BbnfBootstrap` directly at this
//! aggregator path for back-compat with consumers (most notably
//! `bbnf-bootstrap` crate's `pub use ::bbnf::grammar::generated::
//! BbnfBootstrap`). Other grammars require namespaced access via
//! their per-grammar module to avoid marker-struct collisions across
//! grammars (each grammar has its own `__<lowered>_emit_impl` module
//! whose `pub use ...::*;` re-exports a large set of grammar-specific
//! items; re-exporting all eight at the aggregator level would
//! collide).

pub mod bbnf;
pub mod bnf;
pub mod css_l4;
pub mod css_pretty;
pub mod csv;
pub mod ebnf;
pub mod google_sheets;
pub mod json;
pub mod math;

pub use bbnf::*;
