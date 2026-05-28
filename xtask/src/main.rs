//! cargo xtask — workspace build-time codegen entrypoint.
//!
//! `xtask::regen` runs the IR pipeline + emission once per grammar at
//! build time, replacing the pre-B2 `bbnf_derive` proc-macro contract
//! that ran the same pipeline at every consumer's `cargo expand` time.
//!
//! See `docs/tranches/B2/B2.md` and `docs/audit/2026-04-25-deep-audit/
//! AUDIT-D-architectural-transposition.md` for the architectural
//! rationale (T3: xtask + checked-in generation).

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
#[cfg(not(feature = "grammar-regen"))]
use std::process::Command;

use anyhow::{Context, bail};
use clap::{Parser, Subcommand};
#[cfg(feature = "grammar-regen")]
use xtask::regen;
use xtask::regen_css;
use xtask::regen_simple_runtime;

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
    /// Regenerate the root CSS L4 runtime projection under
    /// `crates/core/src/runtime/css_l4/`.
    RegenCss,
    /// Regenerate the root math runtime projection under
    /// `crates/core/src/runtime/math/`.
    RegenMath,
    /// Regenerate the root CSV runtime projection under
    /// `crates/core/src/runtime/csv/`.
    RegenCsv,
    /// Regenerate the root BBNF runtime projection under
    /// `crates/core/src/runtime/bbnf/`.
    RegenBbnf,
    /// Regenerate the root BNF runtime projection under
    /// `crates/core/src/runtime/bnf/`.
    RegenBnf,
    /// Regenerate the root EBNF runtime projection under
    /// `crates/core/src/runtime/ebnf/`.
    RegenEbnf,
    /// Regenerate the root CSS Pretty runtime projection under
    /// `crates/core/src/runtime/css_pretty/`.
    RegenCssPretty,
    /// Regenerate the root Google Sheets runtime projection under
    /// `crates/core/src/runtime/google_sheets/`.
    RegenGoogleSheets,
    /// Regenerate the root JSON runtime projection under
    /// `crates/core/src/runtime/json/`.
    RegenJson,
    /// Check root runtime projections without writing generated files.
    CheckRuntime {
        /// Check only one runtime projection. The full no-arg command is the
        /// W4 close gate; this flag is diagnostic.
        #[arg(long)]
        grammar: Option<String>,
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
        } => run_regen(grammar.as_deref(), check, staged, output.as_deref()),
        Cmd::RegenCss => regen_css::run(),
        Cmd::RegenMath => regen_simple_runtime::run("math"),
        Cmd::RegenCsv => regen_simple_runtime::run("csv"),
        Cmd::RegenBbnf => regen_simple_runtime::run("bbnf"),
        Cmd::RegenBnf => regen_simple_runtime::run("bnf"),
        Cmd::RegenEbnf => regen_simple_runtime::run("ebnf"),
        Cmd::RegenCssPretty => regen_simple_runtime::run("css_pretty"),
        Cmd::RegenGoogleSheets => regen_simple_runtime::run("google_sheets"),
        Cmd::RegenJson => regen_simple_runtime::run("json"),
        Cmd::CheckRuntime { grammar } => run_check_runtime(grammar.as_deref()),
    }
}

const ROOT_RUNTIME_GRAMMARS: &[&str] = &[
    "css_l4",
    "math",
    "csv",
    "bbnf",
    "bnf",
    "ebnf",
    "css_pretty",
    "google_sheets",
    "json",
];

fn run_check_runtime(grammar: Option<&str>) -> anyhow::Result<()> {
    if let Some(grammar) = grammar {
        check_runtime_grammar(grammar)?;
        return Ok(());
    }

    let workspace_root = workspace_root()?;
    validate_runtime_projection_set(&workspace_root)?;

    let mut expected = BTreeSet::new();
    for grammar in ROOT_RUNTIME_GRAMMARS {
        for path in check_runtime_grammar(grammar)? {
            if !expected.insert(path) {
                bail!("duplicate root runtime generated path for `{grammar}`");
            }
        }
    }

    let actual = collect_pattern_h_paths(&workspace_root)?;
    if expected != actual {
        let missing = expected.difference(&actual).collect::<Vec<_>>();
        let extra = actual.difference(&expected).collect::<Vec<_>>();
        bail!("root runtime generated path set mismatch; missing={missing:?}; extra={extra:?}");
    }
    Ok(())
}

fn check_runtime_grammar(grammar: &str) -> anyhow::Result<Vec<PathBuf>> {
    match grammar {
        "css_l4" => regen_css::check(),
        "math" | "csv" | "bbnf" | "bnf" | "ebnf" | "css_pretty" | "google_sheets" | "json" => {
            regen_simple_runtime::check(grammar)
        }
        other => bail!("unknown root runtime projection `{other}`"),
    }
}

fn workspace_root() -> anyhow::Result<PathBuf> {
    let metadata = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .context("cargo_metadata: failed to read workspace manifest")?;
    Ok(metadata.workspace_root.into_std_path_buf())
}

fn validate_runtime_projection_set(workspace_root: &Path) -> anyhow::Result<()> {
    let expected = runtime_grammar_set();
    let projection_dir = workspace_root.join("xtask/runtime-projections");
    let mut actual = BTreeSet::new();
    for entry in std::fs::read_dir(&projection_dir)
        .with_context(|| format!("read `{}`", projection_dir.display()))?
    {
        let path = entry?.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("toml") {
            let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) else {
                bail!(
                    "runtime projection has non-utf8 filename `{}`",
                    path.display()
                );
            };
            actual.insert(stem.to_string());
        }
    }
    if actual != expected {
        let missing = expected.difference(&actual).collect::<Vec<_>>();
        let extra = actual.difference(&expected).collect::<Vec<_>>();
        bail!("runtime projection set mismatch; missing={missing:?}; extra={extra:?}");
    }
    Ok(())
}

fn runtime_grammar_set() -> BTreeSet<String> {
    ROOT_RUNTIME_GRAMMARS
        .iter()
        .map(|grammar| (*grammar).to_string())
        .collect()
}

fn collect_pattern_h_paths(workspace_root: &Path) -> anyhow::Result<BTreeSet<PathBuf>> {
    let runtime_root = workspace_root.join("crates/core/src/runtime");
    let mut paths = BTreeSet::new();
    for grammar in ROOT_RUNTIME_GRAMMARS {
        collect_rs_files(&runtime_root.join(grammar), &mut paths)?;
    }
    Ok(paths)
}

fn collect_rs_files(dir: &Path, paths: &mut BTreeSet<PathBuf>) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir).with_context(|| format!("read `{}`", dir.display()))? {
        let entry = entry?;
        let path = entry.path();
        if entry.file_type()?.is_dir() {
            collect_rs_files(&path, paths)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            paths.insert(path);
        }
    }
    Ok(())
}

#[cfg(feature = "grammar-regen")]
fn run_regen(
    grammar: Option<&str>,
    check: bool,
    staged: bool,
    output: Option<&std::path::Path>,
) -> anyhow::Result<()> {
    regen::run(grammar, check, staged, output)
}

#[cfg(not(feature = "grammar-regen"))]
fn run_regen(
    grammar: Option<&str>,
    check: bool,
    staged: bool,
    output: Option<&std::path::Path>,
) -> anyhow::Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.args([
        "run",
        "-p",
        "xtask",
        "--profile",
        "ax-iter",
        "--features",
        "grammar-regen",
        "--",
        "regen",
    ]);
    if let Some(grammar) = grammar {
        cmd.args(["--grammar", grammar]);
    }
    if check {
        cmd.arg("--check");
    }
    if staged {
        cmd.arg("--staged");
    }
    if let Some(output) = output {
        cmd.arg("--output").arg(output);
    }
    let status = cmd
        .status()
        .context("failed to launch grammar-regeneration xtask")?;
    if !status.success() {
        bail!("grammar-regeneration xtask exited with {status}");
    }
    Ok(())
}
