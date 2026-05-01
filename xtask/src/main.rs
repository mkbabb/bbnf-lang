//! cargo xtask — workspace build-time codegen entrypoint.
//!
//! `xtask::regen` runs the IR pipeline + emission once per grammar at
//! build time, replacing the pre-B2 `bbnf_derive` proc-macro contract
//! that ran the same pipeline at every consumer's `cargo expand` time.
//!
//! See `docs/tranches/B2/B2.md` and `docs/audit/2026-04-25-deep-audit/
//! AUDIT-D-architectural-transposition.md` for the architectural
//! rationale (T3: xtask + checked-in generation).

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use xtask::regen;

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
        /// Restrict the `--check` regen loop to grammars whose source
        /// path or generated output path is in the `git diff --cached
        /// --name-only` set. Combined with `--check` this gives the
        /// pre-commit hook an O(staged-grammars) fast path: when no
        /// staged file matches `grammar/`, `crates/core/src/grammar/
        /// generated/`, or `xtask/src/regen`, the command returns 0
        /// immediately without compiling the IR pipeline against any
        /// grammar. Has no effect without `--check`.
        #[arg(long)]
        staged: bool,
        /// Override the output directory. When set, generated files
        /// land at `<output>/<ident>.rs` instead of the default
        /// `crates/core/src/grammar/generated/<ident>.rs`. Used by the
        /// `bbnf_bootstrap_reproducibility` CI gate to capture two
        /// successive regen outputs into a tempdir without polluting
        /// the workspace.
        #[arg(long)]
        output: Option<PathBuf>,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.cmd {
        Cmd::Regen {
            grammar,
            check,
            staged,
            output,
        } => regen::run(grammar.as_deref(), check, staged, output.as_deref()),
    }
}
