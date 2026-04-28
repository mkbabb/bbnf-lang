//! IR-pipeline + emission entrypoint.
//!
//! Replaces the pre-B2 `bbnf_derive` proc-macro contract that ran the
//! 17-pass IR pipeline + emission at every consumer's `cargo expand`
//! time. The pipeline now runs once per `cargo xtask regen` invocation,
//! producing per-grammar source files at
//! `crates/core/src/grammar/generated/<ident>.rs`. Consumers
//! `include!` the on-disk product instead of `the proc-macro derive (retired B2)`.
//!
//! The grammar manifest lives at `[workspace.metadata.bbnf.grammars]`
//! in the workspace `Cargo.toml`; this module reads it via
//! `cargo_metadata`.
//!
//! Mirrors `crates/derive/src/lib.rs` lines 281-361 — the proc-macro
//! entry that calls `bbnf::pipeline::compile_paths_request` +
//! `bbnf::generate::generate_all`, formats via
//! `prettyplease::unparse(&syn::parse2(stream)?)`, and writes the
//! result to disk.

use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use bbnf::ParserAttributes;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::rewrites::RuleSet;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// One row of the workspace grammar manifest at
/// `[workspace.metadata.bbnf.grammars]`.
#[derive(serde::Deserialize, Clone, Debug)]
struct GrammarEntry {
    ident: String,
    path: String,
    #[serde(default)]
    features: Vec<String>,
}

impl GrammarEntry {
    /// Resolve the manifest's relative `path` against the workspace root.
    fn grammar_source(&self, workspace_root: &Path) -> PathBuf {
        workspace_root.join(&self.path)
    }

    /// Tranche BB.scaffold.C — per-grammar rewrites directory.
    ///
    /// Returns the canonical path `grammar/<ident>/rewrites/` under
    /// the workspace root. The directory is optional; absence is
    /// the no-rule baseline (handled by
    /// [`RuleSet::load_from_dir`]). The path is derived from the
    /// grammar source's parent — every grammar manifest entry
    /// points at `grammar/<ident>/<file>.bbnf`, so the parent of
    /// the source path is the grammar's own directory.
    fn rewrites_dir(&self, workspace_root: &Path) -> PathBuf {
        let source = self.grammar_source(workspace_root);
        source
            .parent()
            .map(|p| p.join("rewrites"))
            .unwrap_or_else(|| workspace_root.join("grammar").join(&self.ident).join("rewrites"))
    }

    /// Marker-struct ident emitted into the per-grammar file. Mirrors
    /// the proc-macro's `the proc-macro derive (retired B2)` consumer-side struct name
    /// — for the BBNF self-host this is `BbnfBootstrap`, for other
    /// grammars the per-grammar consumer cutover (B2.W1) declares the
    /// canonical ident; today the manifest carries it implicitly via
    /// the W0.b boundary spec. For BBNF (W0.c's only target),
    /// `BbnfBootstrap`. For the rest the ident derives from
    /// `entry.ident` PascalCased: `json` → `JsonParser`, etc. — but
    /// only BBNF emits at W0.c so the rest of the table is
    /// declarative-only here.
    fn marker_ident(&self) -> syn::Ident {
        match self.ident.as_str() {
            "bbnf" => format_ident!("BbnfBootstrap"),
            other => {
                // Default: PascalCase + "Parser" suffix. W1 reviews
                // each grammar's actual marker name as it migrates;
                // the table is declarative so collisions surface as
                // compile failures.
                let camel = pascal_case(other);
                format_ident!("{}Parser", camel)
            }
        }
    }

    /// `ParserAttributes` reconstructed from the manifest's `features`
    /// list. Mirrors the proc-macro's `#[parser(...)]` attribute
    /// parsing (`crates/derive/src/lib.rs:226-278`).
    ///
    /// Populates both `paths` (absolute, for the IR pipeline to read
    /// grammar source bytes at codegen time) and `grammar_rel_paths`
    /// (workspace-root-relative POSIX, for the emitter to embed in
    /// `include_str!()` so the generated file is portable across
    /// worktrees + checkouts). Both are pushed in lock-step.
    fn parser_attributes(&self, grammar_path: PathBuf) -> ParserAttributes {
        let mut attrs = ParserAttributes::default();
        attrs.paths.push(grammar_path);
        // The manifest's raw `path` is already workspace-relative
        // (e.g. `grammar/bbnf/bbnf.bbnf`). Normalise to forward
        // slashes for cross-platform stability of the embedded literal.
        attrs.grammar_rel_paths.push(self.path.replace('\\', "/"));
        for feat in &self.features {
            match feat.as_str() {
                "structural" => attrs.structural = true,
                "prettify" => attrs.prettify = true,
                "skip_recover" => attrs.skip_recover = true,
                "serialize" => attrs.serialize = true,
                "remove_left_recursion" => attrs.remove_left_recursion = true,
                "debug" => attrs.debug = true,
                other => {
                    // Unknown feature: warn but don't fail; the
                    // manifest is the source of truth and unknown
                    // entries indicate a forthcoming feature this
                    // xtask doesn't yet recognise.
                    eprintln!("xtask::regen: warning — unknown feature `{other}` on grammar `{}`", self.ident);
                }
            }
        }
        attrs
    }
}

fn pascal_case(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut upper_next = true;
    for ch in input.chars() {
        if ch == '_' || ch == '-' {
            upper_next = true;
        } else if upper_next {
            out.extend(ch.to_uppercase());
            upper_next = false;
        } else {
            out.push(ch);
        }
    }
    out
}

/// Top-level entry. `grammar = None` regenerates every grammar in the
/// workspace manifest; `Some(ident)` regenerates that grammar only.
/// `check = true` regenerates to a tempdir + diffs against the
/// checked-in tree, exiting non-zero on drift. `output_override`
/// redirects every per-grammar emission to `<output_override>/<ident>.rs`
/// — used by the AZ-II.cutover.B reproducibility CI gate to capture
/// successive regen outputs into a tempdir without disturbing the
/// checked-in `crates/core/src/grammar/generated/` tree.
pub fn run(
    grammar: Option<&str>,
    check: bool,
    output_override: Option<&Path>,
) -> Result<()> {
    let (workspace_root, grammars) = load_manifest()?;

    if check {
        regen_check(&workspace_root, &grammars)
    } else if let Some(ident) = grammar {
        let entry = grammars
            .iter()
            .find(|g| g.ident == ident)
            .ok_or_else(|| anyhow!("grammar `{ident}` not found in [workspace.metadata.bbnf.grammars]"))?;
        let target = match output_override {
            Some(dir) => dir.join(format!("{}.rs", entry.ident)),
            None => output_path(&workspace_root, &entry.ident),
        };
        regen_grammar(&workspace_root, entry, &target)?;
        Ok(())
    } else if let Some(dir) = output_override {
        for entry in &grammars {
            let target = dir.join(format!("{}.rs", entry.ident));
            regen_grammar(&workspace_root, entry, &target)?;
        }
        Ok(())
    } else {
        regen_all(&workspace_root, &grammars)
    }
}

/// Read `[workspace.metadata.bbnf.grammars]` from the workspace
/// `Cargo.toml` via `cargo_metadata`. Returns the workspace root +
/// the parsed grammar list.
fn load_manifest() -> Result<(PathBuf, Vec<GrammarEntry>)> {
    let metadata = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .context("cargo_metadata: failed to read workspace manifest")?;

    let workspace_root = metadata.workspace_root.clone().into_std_path_buf();

    let bbnf_meta = metadata
        .workspace_metadata
        .get("bbnf")
        .ok_or_else(|| anyhow!("workspace `Cargo.toml` has no [workspace.metadata.bbnf] table"))?;

    let grammars_value = bbnf_meta
        .get("grammars")
        .cloned()
        .ok_or_else(|| anyhow!("[workspace.metadata.bbnf] has no `grammars` array"))?;

    let grammars: Vec<GrammarEntry> = serde_json::from_value(grammars_value)
        .context("failed to deserialize [workspace.metadata.bbnf.grammars]")?;

    Ok((workspace_root, grammars))
}

/// Output path for a grammar's per-grammar emission.
fn output_path(workspace_root: &Path, ident: &str) -> PathBuf {
    workspace_root
        .join("crates")
        .join("core")
        .join("src")
        .join("grammar")
        .join("generated")
        .join(format!("{ident}.rs"))
}

/// Regenerate a single grammar. Reads the grammar source, runs the
/// 17-pass IR pipeline, runs `generate_all`, writes the formatted
/// output to `target_path`.
fn regen_grammar(
    workspace_root: &Path,
    entry: &GrammarEntry,
    target_path: &Path,
) -> Result<usize> {
    let grammar_path = entry.grammar_source(workspace_root);
    if !grammar_path.exists() {
        bail!(
            "grammar `{}` source file not found: {}",
            entry.ident,
            grammar_path.display()
        );
    }

    let parser_attrs = entry.parser_attributes(grammar_path.clone());
    let marker_ident = entry.marker_ident();

    // Tranche BB.scaffold.C — load the per-grammar rewrite-rule
    // directory `grammar/<ident>/rewrites/*.ron` into a `RuleSet`.
    // Absent directory → empty ruleset (treated identically to
    // `None` downstream). Read errors propagate up so a corrupted
    // .ron payload fails regen rather than silently producing a
    // half-loaded ruleset.
    let rewrites_dir = entry.rewrites_dir(workspace_root);
    let rewrites = RuleSet::load_from_dir(&rewrites_dir).with_context(|| {
        format!(
            "load rewrites from `{}` for grammar `{}`",
            rewrites_dir.display(),
            entry.ident
        )
    })?;
    let rewrites_for_pipeline = if rewrites.is_empty() {
        None
    } else {
        Some(rewrites.clone())
    };

    let request = CompileRequest {
        options: PipelineOptions {
            remove_left_recursion: parser_attrs.remove_left_recursion,
            entry_rule: None,
            structural: parser_attrs.structural,
            rewrites: rewrites_for_pipeline,
        },
        target: CompileTarget::Rust {
            requested_prettify: parser_attrs.prettify,
        },
    };

    eprintln!(
        "[xtask::regen] {}: compile_paths_request started ({} paths, structural={}, prettify={}, rewrites={})",
        entry.ident,
        parser_attrs.paths.len(),
        parser_attrs.structural,
        parser_attrs.prettify,
        rewrites.len(),
    );
    let t0 = std::time::Instant::now();

    let prepared = match compile_paths_request(&parser_attrs.paths, &request) {
        Ok(CompileOutput::Rust(prepared)) => prepared,
        Ok(_) => bail!("Rust target produced non-Rust pipeline output"),
        Err(err) => bail!("compile_paths_request for `{}`: {err}", entry.ident),
    };

    eprintln!(
        "[xtask::regen] {}: compile_paths_request done in {:?}",
        entry.ident,
        t0.elapsed()
    );

    // Run `generate_all` to produce the inner `TokenStream` — same
    // call the proc-macro makes at `crates/derive/src/lib.rs:324`.
    let t1 = std::time::Instant::now();
    let inner: TokenStream = bbnf::generate::generate_all(&prepared, &parser_attrs, &marker_ident);
    eprintln!(
        "[xtask::regen] {}: generate_all done in {:?}",
        entry.ident,
        t1.elapsed()
    );

    // Wrap the inner stream in the per-grammar emit-impl module —
    // mirrors the proc-macro's wrapping at
    // `crates/derive/src/lib.rs:335-353`. The module name is
    // `__<lowered_ident>_emit_impl`; the `pub use ...::*;` re-export
    // lifts every emitted item to the parent path. The inner-attribute
    // `#![allow(...)]` swallows the lint surface the IR codegen
    // generates.
    let mod_name = format_ident!("__{}_emit_impl", marker_ident.to_string().to_lowercase());
    let body: TokenStream = quote! {
        pub struct #marker_ident;

        mod #mod_name {
            #![allow(
                dead_code,
                unused_variables,
                unused_mut,
                unused_parens,
                unused_assignments,
                non_camel_case_types,
                non_snake_case,
                non_upper_case_globals,
                clippy::all,
            )]
            use super::*;
            #inner
        }
        pub use #mod_name::*;
    };

    // Format via prettyplease. The IR codegen emits well-formed
    // Rust; `syn::parse2` succeeds on any valid `TokenStream`.
    let t2 = std::time::Instant::now();
    let parsed: syn::File = syn::parse2(body)
        .with_context(|| format!("syn::parse2 of generated TokenStream for `{}`", entry.ident))?;
    let formatted_body = prettyplease::unparse(&parsed);
    eprintln!(
        "[xtask::regen] {}: prettyplease done in {:?}",
        entry.ident,
        t2.elapsed()
    );

    // Compose the final file: header (doc + crate-level allow + use
    // imports) + the formatted body. The header mirrors the
    // bootstrap script's pre-B2 header so byte-equivalent diffing
    // against `generated.rs` is meaningful.
    let header = file_header(&entry.ident);
    let output = format!("{header}{formatted_body}");

    // Ensure the parent directory exists.
    if let Some(parent) = target_path.parent() {
        std::fs::create_dir_all(parent).with_context(|| {
            format!("create parent dir for `{}`", target_path.display())
        })?;
    }

    let bytes = output.len();

    // Tranche B6.W0 — content-equality skip.
    //
    // `cargo xtask regen` runs the IR pipeline against the on-disk
    // grammar and writes the formatted Rust source. The dominant
    // cold-wall cost on the post-B5 substrate is not the IR pipeline
    // (~3 ms) nor `BbnfBootstrap::parse` itself but cargo's release-
    // mode rebuild of `bbnf` core, which `include!()`s this file.
    //
    // Pre-B6 the write was unconditional: every successful regen
    // advanced the file's mtime regardless of byte equality. Cargo's
    // fingerprint check observed the mtime delta and recompiled
    // `bbnf` (1.6 MB generated source, ~85 s release build) on every
    // subsequent `cargo xtask regen`, even when the regen output was
    // structurally identical to the prior one — a self-invalidation
    // cycle where the act of regen guaranteed the next regen pays the
    // full rebuild cost.
    //
    // Reading the existing file once and comparing bytes before
    // writing breaks the cycle: regen invocations whose IR-pipeline
    // output is byte-identical to the on-disk file leave mtime
    // unchanged, and the next `cargo xtask` invocation reuses the
    // cached `bbnf` rmeta. Regen invocations that produce different
    // output write as before; cargo's rebuild of `bbnf` against the
    // genuinely-new generated source is unavoidable and correct.
    //
    // The check is content-equality on the full output buffer; mtime
    // is preserved by skipping the write entirely. `std::fs::read`
    // returns the on-disk bytes; `Vec<u8> == &[u8]` is element-wise
    // comparison short-circuiting at first mismatch.
    let on_disk = std::fs::read(target_path).ok();
    let unchanged = matches!(&on_disk, Some(existing) if existing.as_slice() == output.as_bytes());
    if !unchanged {
        std::fs::write(target_path, &output)
            .with_context(|| format!("write `{}`", target_path.display()))?;
    }

    Ok(bytes)
}

/// File header emitted before the per-grammar body. Carries the
/// canonical doc comment + crate-level `#![allow(...)]` block + the
/// runtime imports the per-grammar emission relies on. The
/// `Regenerate:` line points at `cargo xtask regen --grammar <ident>`,
/// the canonical entrypoint post-B2.W3 (the pre-B2
/// `scripts/bootstrap-bbnf.sh` retired with the proc-macro contract).
fn file_header(ident: &str) -> String {
    format!(
        "//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.\n\
         //! Regenerate: cargo xtask regen --grammar {ident}\n\
         \n\
         #![allow(\n    \
             dead_code,\n    \
             unused_variables,\n    \
             unused_mut,\n    \
             unused_parens,\n    \
             unused_assignments,\n    \
             non_camel_case_types,\n    \
             non_snake_case,\n    \
             non_upper_case_globals,\n    \
             clippy::all\n\
         )]\n\
         \n\
         use crate::runtime::tape::*;\n\
         use crate::runtime::{{Parsed, ParseErr, Root}};\n\
         use ::parse_that::*;\n\
         \n",
    )
}

/// Regenerate every grammar enumerated in the workspace manifest.
fn regen_all(workspace_root: &Path, grammars: &[GrammarEntry]) -> Result<()> {
    for entry in grammars {
        let target = output_path(workspace_root, &entry.ident);
        let bytes = regen_grammar(workspace_root, entry, &target)?;
        println!(
            "regen {}: wrote {} bytes to {}",
            entry.ident,
            bytes,
            target
                .strip_prefix(workspace_root)
                .unwrap_or(&target)
                .display()
        );
    }
    Ok(())
}

/// Regenerate to a tempdir; diff against the checked-in tree; exit
/// non-zero on drift. Used by CI + pre-commit hook.
fn regen_check(workspace_root: &Path, grammars: &[GrammarEntry]) -> Result<()> {
    let tmpdir = tempfile::tempdir().context("create tempdir for regen --check")?;
    let mut drift = Vec::new();

    for entry in grammars {
        let tmp_target = tmpdir.path().join(format!("{}.rs", entry.ident));
        regen_grammar(workspace_root, entry, &tmp_target)?;

        let checked_in = output_path(workspace_root, &entry.ident);
        if !checked_in.exists() {
            drift.push(format!(
                "missing checked-in `{}` — regenerate with `cargo xtask regen --grammar {}`",
                checked_in.display(),
                entry.ident
            ));
            continue;
        }

        let regenerated_bytes = std::fs::read(&tmp_target)
            .with_context(|| format!("read regenerated `{}`", tmp_target.display()))?;
        let checked_in_bytes = std::fs::read(&checked_in)
            .with_context(|| format!("read checked-in `{}`", checked_in.display()))?;

        if regenerated_bytes != checked_in_bytes {
            drift.push(format!(
                "drift: `{}` differs from `cargo xtask regen --grammar {}` output",
                checked_in.display(),
                entry.ident
            ));
        }
    }

    if drift.is_empty() {
        println!(
            "regen --check: clean ({} grammars matched)",
            grammars.len()
        );
        Ok(())
    } else {
        for msg in &drift {
            eprintln!("{msg}");
        }
        bail!(
            "regen --check: {} of {} grammars drifted",
            drift.len(),
            grammars.len()
        );
    }
}
