//! xtask-emitted grammar generated code.
//!
//! Each `pub mod <ident>;` corresponds to a grammar listed in
//! `[workspace.metadata.bbnf.grammars]`. `cargo xtask regen --grammar
//! <ident>` refreshes the per-grammar source on disk; consumers
//! `use bbnf::grammar::generated::*` to reach `BbnfBootstrap` and the
//! per-grammar emitted items.
//!
//! Pre-B2 the workspace shipped a single monolithic `generated.rs`
//! produced by `scripts/bootstrap-bbnf.sh`. B2.W0.c retired that path:
//! the IR pipeline + emission move into `cargo xtask regen`, and the
//! per-grammar source files live under this directory.

pub mod bbnf;

pub use bbnf::*;
