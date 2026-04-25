//! cargo xtask — workspace build-time codegen entrypoint.
//!
//! `xtask::regen` runs the IR pipeline + emission once per grammar at
//! build time, replacing the pre-B2 `bbnf_derive` proc-macro contract
//! that ran the same pipeline at every consumer's `cargo expand` time.
//!
//! See `docs/tranches/B2/B2.md` and `docs/audit/2026-04-25-deep-audit/
//! AUDIT-D-architectural-transposition.md` for the architectural
//! rationale (T3: xtask + checked-in generation).

use clap::{Parser, Subcommand};

mod regen;

#[derive(Parser)]
#[command(name = "xtask", about = "bbnf-lang workspace build-time codegen")]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Regenerate per-grammar source files under
    /// `crates/core/src/grammar/generated/`.
    Regen {
        /// Regenerate only the named grammar (default: every grammar
        /// listed in `[workspace.metadata.bbnf.grammars]`).
        #[arg(long)]
        grammar: Option<String>,
        /// Regenerate to a tempdir + diff against the checked-in tree;
        /// exit non-zero on drift. Used by CI + pre-commit hook.
        #[arg(long)]
        check: bool,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.cmd {
        Cmd::Regen { grammar, check } => regen::run(grammar.as_deref(), check),
    }
}
