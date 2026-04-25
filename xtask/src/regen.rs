//! IR-pipeline + emission entrypoint.
//!
//! W0.a lays the function signatures (this file's current shape).
//! W0.c lands the implementation that mirrors `crates/derive/src/lib.rs`
//! lines 281-361 — the proc-macro entry that calls
//! `bbnf::pipeline::compile_paths_request` + `bbnf::generate::generate_all`,
//! then formats the resulting `TokenStream` via
//! `prettyplease::unparse(&syn::parse2(stream)?)`, then writes the
//! formatted output to `crates/core/src/grammar/generated/<ident>.rs`.
//!
//! The grammar manifest lives at `[workspace.metadata.bbnf.grammars]`
//! in the workspace `Cargo.toml`; see `xtask/Cargo.toml` for the
//! `cargo_metadata` dep that reads it.

use anyhow::Result;

/// Top-level entry. `grammar = None` regenerates every grammar in the
/// workspace manifest; `Some(ident)` regenerates that grammar only.
/// `check = true` regenerates to a tempdir + diffs against the
/// checked-in tree, exiting non-zero on drift.
pub fn run(grammar: Option<&str>, check: bool) -> Result<()> {
    // W0.c: read [workspace.metadata.bbnf.grammars] from workspace
    // Cargo.toml via cargo_metadata; dispatch to regen_grammar /
    // regen_all / regen_check.
    let _ = (grammar, check);
    anyhow::bail!("xtask::regen::run — implementation lands in B2.W0.c")
}

/// Regenerate a single grammar. Reads the grammar source, runs the
/// 17-pass IR pipeline, runs `generate_all`, writes the formatted
/// output to `crates/core/src/grammar/generated/<ident>.rs`.
#[allow(dead_code)]
pub(crate) fn regen_grammar(_ident: &str) -> Result<()> {
    anyhow::bail!("regen_grammar — implementation lands in B2.W0.c")
}

/// Regenerate every grammar enumerated in the workspace manifest.
#[allow(dead_code)]
pub(crate) fn regen_all() -> Result<()> {
    anyhow::bail!("regen_all — implementation lands in B2.W0.c")
}

/// Regenerate to a tempdir; diff against the checked-in tree; exit
/// non-zero on drift. Used by CI + pre-commit hook.
#[allow(dead_code)]
pub(crate) fn regen_check() -> Result<()> {
    anyhow::bail!("regen_check — implementation lands in B2.W0.c")
}
