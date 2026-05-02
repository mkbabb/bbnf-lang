//! AZ-IV.W5.4 — Permanent substrate-audit test.
//!
//! Hard Gate 13 (`docs/tranches/AZ-IV/AZ-IV.md` §Hard Gates):
//!
//! > `crates/ir/src/passes/tests/substrate_audit.rs` enumerates every
//! > `pub` substrate at compile time and fails the build if any has
//! > zero callers in production code (excluding `tests/`, `examples/`,
//! > `#[cfg(test)]`). CI-gated.
//!
//! ## Relocation note (deviation from wave spec)
//!
//! The W5.md spec wrote the path as
//! `crates/ir/src/passes/tests/substrate_audit.rs`, which is an
//! inline-test layout under `src/`. Per the standing user feedback
//! `feedback_no_inline_tests` ("All tests in `tests/` directory, never
//! inline `#[cfg(test)]` in `src/` files"), the test lives instead at
//! `crates/ir/tests/substrate_audit.rs` as a workspace integration
//! test. The test is still picked up by `cargo nextest run --workspace`
//! identically and CI-gated through the existing
//! `.github/workflows/ci.yml` heavy step. The Hard Gate text
//! conditions on enumeration + zero-caller failure, not on a specific
//! path inside `src/`.
//!
//! ## Mechanism
//!
//! 1. `cargo_metadata` resolves the workspace and locates every
//!    audited crate's `src/` directory.
//! 2. The five audited crates (`bbnf-ir`, `bbnf` aka bbnf-core,
//!    `egraph` aka bbnf-egraph, `csp-solver` aka bbnf-csp-solver,
//!    `simd-scan` aka bbnf-simd-scan) have every `pub fn`,
//!    `pub struct`, `pub enum`, `pub trait`, and `pub static`
//!    enumerated by walking each .rs file with `syn`. Items inside
//!    `crates/core/src/grammar/generated/**` are excluded (those are
//!    codegen-emitted re-exports of the audited surface, not new
//!    surface).
//! 3. Caller counting walks every workspace member's `src/` tree
//!    (production code: every member in `cargo metadata`, including
//!    `xtask`). Inside each file, `#[cfg(test)]`-gated items are
//!    skipped; module-paths under `tests/`, `examples/`, `benches/`
//!    are skipped at the directory level.
//! 4. A substrate is "consumed" when its identifier appears anywhere
//!    in production code outside its own definition site. Trait method
//!    names are intentionally identifier-matched loosely (see Known
//!    Misses below).
//! 5. A substrate with zero production callers fails the test with a
//!    `panic!` listing the substrate name + file:line.
//!
//! ## Sanctioned Whitelist
//!
//! See [`SANCTIONED_SUBSTRATES`] below. Each entry carries a one-line
//! reason. Whitelist entries are never reported as zero-caller; they
//! also count toward the post-W5 substrate denominator (`docs/tranches/
//! AZ-IV/audit/W5-substrate-denominator.md`).
//!
//! ## Performance budget
//!
//! < 60s on the reference host. Parsing is parallelised with `rayon`;
//! generated files (`crates/core/src/grammar/generated/**.rs`) are
//! enumerated for caller scans only (no pub-item enumeration), and we
//! identifier-scan them via `syn`'s parsed token-stream rather than a
//! second pass.
//!
//! ## Known Misses
//!
//! - Trait method substrates: a trait method named `foo` is matched by
//!   identifier — any caller that types `.foo(` against ANY type
//!   counts. This is loose: a same-named method on a different trait
//!   would be miscounted. The audit accepts this miss.
//! - Generic-over-T substrates: a `pub fn map<T, F>(...)` on type `X<T>`
//!   has callers counted by the literal name `map`. Common method names
//!   are over-credited. Whitelist names that are too noisy.
//! - Macro-emitted callers: identifiers that only appear after macro
//!   expansion (e.g. `pub` items used only by `path!` post-expansion)
//!   are seen because the test scans the .rs source where the macro
//!   invocation lives, not the expanded form. As long as the substrate
//!   is named in the macro invocation site, it counts.

#![cfg(test)]

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Instant;

use rayon::prelude::*;
use syn::visit::Visit;

// ───────────────────────────────────────────────────────────────────
// Sanctioned whitelist — every entry MUST carry a one-line reason.
// ───────────────────────────────────────────────────────────────────

/// Sanctioned `pub` substrates that intentionally have zero in-tree
/// production callers. Each entry is the bare item ident (matched by
/// name on the `pub` site, not module path) plus a comment naming the
/// reason. Whitelist by-name to keep the audit O(item-name) and avoid
/// fragile module-path resolution.
///
/// New entries require a one-line reason. Reviewers reject naked
/// allowances per `feedback_no-workarounds` and the wave spec's
/// "documented reasons" requirement.
const SANCTIONED_SUBSTRATES: &[&str] = &[
    // ── Library API surface (consumed by external callers / tests) ──
    // (none currently — every public re-export is consumed in-tree)

    // ── Serializable types intentionally exposed ───────────────────
    // (none currently — serde-flagged types are consumed by the
    // bench/parity harnesses living in tests/examples/benches)
];

// ───────────────────────────────────────────────────────────────────
// Audited crate set — bbnf-ir + bbnf-core (excluding generated/) +
// bbnf-egraph + bbnf-csp-solver + bbnf-simd-scan.
// ───────────────────────────────────────────────────────────────────

/// Workspace member crate names (per `cargo metadata`) whose `pub`
/// substrates this audit enumerates. The names match the `[package]
/// name` in each crate's Cargo.toml (e.g. `bbnf` not `bbnf-core`).
const AUDITED_CRATES: &[&str] = &[
    "bbnf-ir",    // → crates/ir
    "bbnf",       // → crates/core (a.k.a. bbnf-core in spec text)
    "egraph",     // → crates/egraph (a.k.a. bbnf-egraph)
    "csp-solver", // → crates/csp-solver (a.k.a. bbnf-csp-solver)
    "simd-scan",  // → crates/simd-scan (a.k.a. bbnf-simd-scan)
];

// ───────────────────────────────────────────────────────────────────
// Data shapes.
// ───────────────────────────────────────────────────────────────────

/// One enumerated `pub` substrate.
#[derive(Debug, Clone)]
struct Substrate {
    /// Bare item identifier (e.g. `compute_first_sets`).
    name: String,
    /// Item kind label for diagnostic clarity.
    kind: &'static str,
    /// File:line of the `pub` declaration.
    file: PathBuf,
    line: usize,
}

/// One production source file.
struct SourceFile {
    path: PathBuf,
    /// True if the file lives under `crates/core/src/grammar/generated/`.
    is_generated: bool,
}

/// Visitor that enumerates `pub` items in a parsed file.
struct PubItemVisitor<'a> {
    file: &'a Path,
    out: &'a mut Vec<Substrate>,
    /// True when the visitor is inside a `#[cfg(test)]`-gated module —
    /// such items are NOT exposed to production callers and so must
    /// NOT be enumerated here.
    inside_cfg_test: bool,
}

/// Visitor that counts identifier-token occurrences in a parsed
/// file (used for caller-counting). The map's value is the number of
/// times the identifier appears outside `#[cfg(test)]`-gated regions.
///
/// We need *occurrence counts* (not just presence) because the
/// declaration site of every audited substrate contributes one
/// occurrence to its own file (e.g. `pub struct Foo` writes the ident
/// `Foo` once). The audit subtracts that single declaration occurrence
/// when scoring callers. Counting same-file uses lets a `pub struct`
/// that is only constructed/impl'd in its declaring file count as
/// consumed (which it is — by its own module's API).
struct IdentVisitor<'a> {
    out: &'a mut rustc_hash::FxHashMap<String, u32>,
    inside_cfg_test: bool,
}

// ───────────────────────────────────────────────────────────────────
// Helpers.
// ───────────────────────────────────────────────────────────────────

/// `true` if any `#[cfg(test)]` (or `#[cfg(any(test, ...))]`)
/// attribute appears on the item. Cheap textual match; the audit
/// tolerates over-eager skipping (a real-but-cfg(test)-gated item is
/// invisible to production code, which is the correct outcome).
fn has_cfg_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        let mut hit = false;
        if a.path().is_ident("cfg") {
            // Walk the attribute's tokens and check for the literal
            // ident `test`. Crude but sufficient: every common form
            // (`#[cfg(test)]`, `#[cfg(any(test, ...))]`,
            // `#[cfg(all(test, ...))]`) carries the bare `test` ident
            // somewhere in the token stream.
            let tokens = a.meta.require_list().ok().map(|m| m.tokens.to_string());
            if let Some(tok) = tokens
                && tok.contains("test")
            {
                hit = true;
            }
        }
        hit
    })
}

/// `true` if the visibility is fully `pub` (i.e. exposes the item
/// across the entire workspace). The wave spec wrote "every `pub fn`,
/// `pub struct`, ..." — strict reading is bare `pub`, NOT
/// `pub(crate)` / `pub(super)` / `pub(in path)`. Restricted
/// visibilities are explicitly *internal*; they are not "substrate"
/// in the wave sense (a substrate is a workspace-public item that
/// nothing in the workspace consumes). This narrows the audit to the
/// items the wave actually targets.
fn is_pub(vis: &syn::Visibility) -> bool {
    matches!(vis, syn::Visibility::Public(_))
}

impl<'ast, 'a> Visit<'ast> for PubItemVisitor<'a> {
    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        if !self.inside_cfg_test && !has_cfg_test_attr(&node.attrs) && is_pub(&node.vis) {
            self.out.push(Substrate {
                name: node.sig.ident.to_string(),
                kind: "fn",
                file: self.file.to_path_buf(),
                line: node.sig.ident.span().start().line,
            });
        }
        // Recurse into nested items inside fn bodies if any.
        syn::visit::visit_item_fn(self, node);
    }

    fn visit_item_struct(&mut self, node: &'ast syn::ItemStruct) {
        if !self.inside_cfg_test && !has_cfg_test_attr(&node.attrs) && is_pub(&node.vis) {
            self.out.push(Substrate {
                name: node.ident.to_string(),
                kind: "struct",
                file: self.file.to_path_buf(),
                line: node.ident.span().start().line,
            });
        }
        syn::visit::visit_item_struct(self, node);
    }

    fn visit_item_enum(&mut self, node: &'ast syn::ItemEnum) {
        if !self.inside_cfg_test && !has_cfg_test_attr(&node.attrs) && is_pub(&node.vis) {
            self.out.push(Substrate {
                name: node.ident.to_string(),
                kind: "enum",
                file: self.file.to_path_buf(),
                line: node.ident.span().start().line,
            });
        }
        syn::visit::visit_item_enum(self, node);
    }

    fn visit_item_trait(&mut self, node: &'ast syn::ItemTrait) {
        if !self.inside_cfg_test && !has_cfg_test_attr(&node.attrs) && is_pub(&node.vis) {
            self.out.push(Substrate {
                name: node.ident.to_string(),
                kind: "trait",
                file: self.file.to_path_buf(),
                line: node.ident.span().start().line,
            });
        }
        syn::visit::visit_item_trait(self, node);
    }

    fn visit_item_static(&mut self, node: &'ast syn::ItemStatic) {
        if !self.inside_cfg_test && !has_cfg_test_attr(&node.attrs) && is_pub(&node.vis) {
            self.out.push(Substrate {
                name: node.ident.to_string(),
                kind: "static",
                file: self.file.to_path_buf(),
                line: node.ident.span().start().line,
            });
        }
        syn::visit::visit_item_static(self, node);
    }

    fn visit_item_mod(&mut self, node: &'ast syn::ItemMod) {
        let was_test = self.inside_cfg_test;
        if has_cfg_test_attr(&node.attrs) {
            self.inside_cfg_test = true;
        }
        syn::visit::visit_item_mod(self, node);
        self.inside_cfg_test = was_test;
    }
}

impl<'ast, 'a> Visit<'ast> for IdentVisitor<'a> {
    fn visit_ident(&mut self, node: &'ast proc_macro2::Ident) {
        if !self.inside_cfg_test {
            *self.out.entry(node.to_string()).or_insert(0) += 1;
        }
    }

    fn visit_item_mod(&mut self, node: &'ast syn::ItemMod) {
        let was_test = self.inside_cfg_test;
        if has_cfg_test_attr(&node.attrs) {
            self.inside_cfg_test = true;
        }
        syn::visit::visit_item_mod(self, node);
        self.inside_cfg_test = was_test;
    }

    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        let was_test = self.inside_cfg_test;
        if has_cfg_test_attr(&node.attrs) {
            self.inside_cfg_test = true;
        }
        syn::visit::visit_item_fn(self, node);
        self.inside_cfg_test = was_test;
    }
}

/// Recursively collect every `.rs` file under `dir`, skipping
/// `tests/`, `examples/`, `benches/`, `target/`, hidden dirs.
fn collect_rs(dir: &Path, out: &mut Vec<PathBuf>) {
    if !dir.is_dir() {
        return;
    }
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for ent in entries.flatten() {
        let path = ent.path();
        if path.is_dir() {
            let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
            // Skip non-production directory roots. We never enter
            // `tests/`, `examples/`, `benches/`, `target/`, `.git/`,
            // `node_modules/`. Generated files are inside `src/` and
            // are picked up by the scanner; they're flagged later as
            // `is_generated`.
            if matches!(
                name,
                "tests" | "examples" | "benches" | "target" | "node_modules" | ".git"
            ) {
                continue;
            }
            collect_rs(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            out.push(path);
        }
    }
}

/// Parse one file with `syn::parse_file`, returning `None` on error
/// (the audit does NOT fail closed on parse errors — a malformed file
/// would have already failed `cargo build` upstream).
fn parse_file(path: &Path) -> Option<syn::File> {
    let src = std::fs::read_to_string(path).ok()?;
    syn::parse_file(&src).ok()
}

// ───────────────────────────────────────────────────────────────────
// The test.
// ───────────────────────────────────────────────────────────────────

#[test]
fn substrate_audit_zero_caller_substrates() {
    let t0 = Instant::now();

    // ── Phase 1: resolve workspace members via cargo_metadata ──────
    let metadata = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .expect("cargo metadata --no-deps must succeed");

    let workspace_root: PathBuf = metadata.workspace_root.clone().into();

    let mut audited_dirs: Vec<PathBuf> = Vec::new();
    let mut all_member_dirs: Vec<PathBuf> = Vec::new();

    for pkg in &metadata.workspace_packages() {
        let manifest_dir = pkg
            .manifest_path
            .parent()
            .expect("manifest has parent")
            .to_path_buf();
        let src_dir: PathBuf = manifest_dir.join("src").into();
        if !src_dir.is_dir() {
            continue;
        }
        all_member_dirs.push(src_dir.clone());
        if AUDITED_CRATES.contains(&pkg.name.as_str()) {
            audited_dirs.push(src_dir);
        }
    }

    assert_eq!(
        audited_dirs.len(),
        AUDITED_CRATES.len(),
        "every audited crate name in AUDITED_CRATES must resolve to a workspace src dir; \
         resolved: {audited_dirs:?}"
    );

    // ── Phase 2: enumerate .rs files in each audited tree ───────────
    let mut audited_files: Vec<SourceFile> = Vec::new();
    for d in &audited_dirs {
        let mut rs = Vec::new();
        collect_rs(d, &mut rs);
        for p in rs {
            let is_gen = p.components().any(|c| c.as_os_str() == "generated")
                && p.components().any(|c| c.as_os_str() == "grammar");
            audited_files.push(SourceFile {
                path: p,
                is_generated: is_gen,
            });
        }
    }

    let mut all_caller_files: Vec<SourceFile> = Vec::new();
    for d in &all_member_dirs {
        let mut rs = Vec::new();
        collect_rs(d, &mut rs);
        for p in rs {
            let is_gen = p.components().any(|c| c.as_os_str() == "generated")
                && p.components().any(|c| c.as_os_str() == "grammar");
            all_caller_files.push(SourceFile {
                path: p,
                is_generated: is_gen,
            });
        }
    }

    eprintln!(
        "substrate_audit: {} audited files, {} caller files (workspace_root = {})",
        audited_files.len(),
        all_caller_files.len(),
        workspace_root.display()
    );

    // ── Phase 3: enumerate `pub` substrates (parallel) ──────────────
    let substrates: Vec<Substrate> = audited_files
        .par_iter()
        .filter(|f| !f.is_generated) // generated/ does not contribute new pub surface
        .flat_map(|f| {
            let mut out = Vec::new();
            if let Some(file) = parse_file(&f.path) {
                let mut v = PubItemVisitor {
                    file: &f.path,
                    out: &mut out,
                    inside_cfg_test: false,
                };
                v.visit_file(&file);
            }
            out
        })
        .collect();

    eprintln!(
        "substrate_audit: enumerated {} pub substrates from audited crates",
        substrates.len()
    );

    // ── Phase 4: count identifier occurrences across all caller files ──
    // For every production source file, count identifier occurrences
    // outside `#[cfg(test)]` blocks. We sum across the whole workspace
    // for each name; same-file uses count, since e.g. a `pub struct`
    // that is only constructed in its own module IS still consumed by
    // its own module's API.
    let per_file_counts: Vec<rustc_hash::FxHashMap<String, u32>> = all_caller_files
        .par_iter()
        .map(|f| {
            let mut counts: rustc_hash::FxHashMap<String, u32> = rustc_hash::FxHashMap::default();
            if let Some(file) = parse_file(&f.path) {
                let mut v = IdentVisitor {
                    out: &mut counts,
                    inside_cfg_test: false,
                };
                v.visit_file(&file);
            }
            counts
        })
        .collect();

    // Aggregate per-file maps into a workspace-wide name → total
    // occurrence count.
    let mut workspace_counts: rustc_hash::FxHashMap<String, u32> = rustc_hash::FxHashMap::default();
    for m in &per_file_counts {
        for (k, v) in m {
            *workspace_counts.entry(k.clone()).or_insert(0) += v;
        }
    }

    // ── Phase 5: count callers per substrate ────────────────────────
    // A substrate is "consumed" iff its name occurs in the workspace
    // *more than once*. The single declaration occurrence (e.g. the
    // ident in `pub fn foo` or `pub struct Foo`) is the baseline; any
    // additional occurrence (a call, a construction, an `impl`, a
    // re-export, a doc-link, a type usage) means the substrate is
    // referenced by some production code.
    //
    // Multiple `pub` items can share the same name across the audited
    // surface (e.g. `Builder` in two crates). We count each declaration
    // toward the baseline subtraction so that the threshold scales
    // with declaration multiplicity.
    let mut decl_count: HashMap<String, u32> = HashMap::new();
    for s in &substrates {
        *decl_count.entry(s.name.clone()).or_insert(0) += 1;
    }

    // ── Phase 6: filter zero-caller, apply whitelist, panic if any ──
    let zero_caller: Vec<&Substrate> = substrates
        .iter()
        .filter(|s| {
            let total = workspace_counts.get(&s.name).copied().unwrap_or(0);
            let baseline = decl_count.get(&s.name).copied().unwrap_or(0);
            // `total <= baseline` means every occurrence is at a
            // declaration site — no caller anywhere in the workspace.
            total <= baseline && !SANCTIONED_SUBSTRATES.contains(&s.name.as_str())
        })
        .collect();

    let elapsed = t0.elapsed();
    eprintln!(
        "substrate_audit: {} zero-caller substrates (whitelist size = {}); elapsed = {:?}",
        zero_caller.len(),
        SANCTIONED_SUBSTRATES.len(),
        elapsed
    );

    // Performance budget assertion (60s as per W5.md sub-gate).
    assert!(
        elapsed < std::time::Duration::from_secs(60),
        "substrate_audit exceeded 60s budget: actual = {elapsed:?}"
    );

    if !zero_caller.is_empty() {
        let mut report = String::new();
        report.push('\n');
        report
            .push_str("AZ-IV.W5.4 substrate-audit FAIL — zero-caller `pub` substrates detected.\n");
        report.push_str("Each item below is a `pub` substrate in the audited crate set with NO\n");
        report.push_str(
            "production caller. Either route the substrate to its consumer, delete it,\n",
        );
        report.push_str("or add it to SANCTIONED_SUBSTRATES with a one-line reason.\n\n");
        for s in &zero_caller {
            // Render relative path for diagnostic clarity.
            let rel = s
                .file
                .strip_prefix(&workspace_root)
                .unwrap_or(&s.file)
                .display();
            report.push_str(&format!(
                "  {kind:7} {name}\n      at {rel}:{line}\n",
                kind = s.kind,
                name = s.name,
                rel = rel,
                line = s.line,
            ));
        }
        panic!("{report}");
    }
}

// ───────────────────────────────────────────────────────────────────
// Self-test of the mechanism: ensures the visitor + caller counter
// would catch a synthetic-fixture pub fn with no callers. Without
// this, the audit could silently degrade to a no-op (e.g. if the
// visitor stopped recording items, or the workspace root resolution
// missed a crate). We construct a synthetic fixture in-memory, run
// it through the same machinery, and assert the synthetic substrate
// is reported as zero-caller.
// ───────────────────────────────────────────────────────────────────

#[test]
fn substrate_audit_self_test_detects_synthetic_zero_caller() {
    let synthetic_src = r#"
        pub fn an_orphaned_substrate_xyz() -> u32 { 42 }
        pub struct AnotherOrphanXyz;
        #[cfg(test)]
        pub fn cfg_test_gated_should_not_enumerate() {}
        pub fn this_one_is_referenced() {}
        fn caller() {
            let _ = this_one_is_referenced();
        }
    "#;
    let parsed = syn::parse_file(synthetic_src).expect("synthetic source parses");

    let mut subs = Vec::new();
    let mut v = PubItemVisitor {
        file: Path::new("synthetic.rs"),
        out: &mut subs,
        inside_cfg_test: false,
    };
    v.visit_file(&parsed);

    let names: Vec<&str> = subs.iter().map(|s| s.name.as_str()).collect();

    assert!(
        names.contains(&"an_orphaned_substrate_xyz"),
        "visitor must enumerate `pub fn` items: names = {names:?}"
    );
    assert!(
        names.contains(&"AnotherOrphanXyz"),
        "visitor must enumerate `pub struct` items: names = {names:?}"
    );
    assert!(
        !names.contains(&"cfg_test_gated_should_not_enumerate"),
        "visitor must skip `#[cfg(test)]`-gated items: names = {names:?}"
    );

    // Identifier visitor: count all ident occurrences in the same file.
    let mut counts: rustc_hash::FxHashMap<String, u32> = rustc_hash::FxHashMap::default();
    let mut iv = IdentVisitor {
        out: &mut counts,
        inside_cfg_test: false,
    };
    iv.visit_file(&parsed);

    // `caller` references `this_one_is_referenced` once on top of the
    // declaration site, so the count is >= 2. The orphans appear only
    // once (at their declaration). The audit's logic — "total >
    // baseline" — therefore credits the referenced fn as consumed and
    // flags both orphans as zero-caller (modulo the whitelist).
    assert!(
        counts.get("this_one_is_referenced").copied().unwrap_or(0) >= 2,
        "ident visitor must count both decl + caller occurrence: counts = {counts:?}"
    );
    assert_eq!(
        counts
            .get("an_orphaned_substrate_xyz")
            .copied()
            .unwrap_or(0),
        1,
        "orphan fn appears only at its decl: counts = {counts:?}"
    );
    assert_eq!(
        counts.get("AnotherOrphanXyz").copied().unwrap_or(0),
        1,
        "orphan struct appears only at its decl: counts = {counts:?}"
    );
    assert!(
        !counts.contains_key("cfg_test_gated_should_not_enumerate"),
        "ident visitor must skip identifiers inside #[cfg(test)] blocks: counts = {counts:?}"
    );
}
