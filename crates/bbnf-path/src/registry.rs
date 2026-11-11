//! AZ-IV.W5 T4 — production [`StructRegistry`] resolver for the
//! `path!` proc-macro.
//!
//! Per AUDIT-F mid-tranche §R1: the proc-macro previously consumed a
//! hand-authored synthetic fixture (`GrammarFixture`) that mirrored the
//! production registry by discipline. T4 swaps the source-of-truth: the
//! macro now reads the per-grammar [`StructRegistry`] projected by the
//! IR's `project_types` pass, serialised to JSON by `cargo xtask regen`
//! at `crates/core/src/grammar/generated/<ident>.registry.json`, and
//! copied by the same regen into this crate at
//! `registry/<ident>.registry.json` (sk-v27 LW2.TOPO-4c).
//!
//! ## Why a JSON sidecar
//!
//! The cleanest in-tree consumer would be the codegen-emitted const at
//! `crates/core/src/grammar/generated/<ident>.rs::REGISTRY` —
//! [`StructRegistry`] is a `LazyLock<StructRegistry>` static the runtime
//! reads through directly. A proc-macro cannot use that path: the
//! `bbnf` core crate depends on `bbnf-path` (for the `path!` symbol),
//! so a `bbnf-path → bbnf` import is a dependency cycle. The sidecar
//! JSON breaks the cycle: the proc-macro embeds the in-crate copy with
//! `include_str!`, so the bytes ship inside the published package and
//! a changed copy rebuilds the macro, deserialises through `serde_json` against
//! `bbnf-ir`'s `StructRegistry` (which already derives serde), and
//! caches per-marker via `OnceLock` to avoid re-parsing on every macro
//! invocation.
//!
//! The sidecar shape mirrors the codegen-emitted `REGISTRY` /
//! `REGISTRY_ENTRY_RULE` pair byte-for-byte (the xtask sidecar writer
//! and the `registry_emit` codegen path resolve `entry_rule` through
//! the same `ir.entry`-keyed lookup), so the macro's compile-time
//! validation reads the same registry the runtime audit pass +
//! emitter consume from `GrammarIR::struct_registry`.
//!
//! ## Marker → grammar ident mapping
//!
//! The `path!` proc-macro receives a Rust path resolving to a marker
//! struct (`Json`, `CssL4`, `Sheets`, `Bbnf`). Each marker maps 1:1 to
//! a grammar ident under `[workspace.metadata.bbnf.grammars]`:
//!
//! | Marker  | Grammar ident      |
//! |---------|--------------------|
//! | `Json`  | `json`             |
//! | `CssL4` | `css_l4`           |
//! | `Sheets`| `google_sheets`    |
//! | `Bbnf`  | `bbnf`             |
//!
//! Adding a new marker (post-W5) is a pure additive change: register
//! the marker ZST under `crates/core/src/path/markers.rs`, add the
//! marker → grammar-ident mapping to [`marker_to_grammar_ident`], embed
//! its copy in [`sidecar_for_ident`], and list the ident under
//! `[package.metadata.bbnf-path] grammars` in this crate's `Cargo.toml`
//! (the list `cargo xtask regen` copies and `regen --check` holds as a
//! set).

use std::sync::OnceLock;

use bbnf_ir::registry::StructRegistry;

/// One grammar's resolved registry: the entry-rule name plus the
/// populated [`StructRegistry`] deserialised from the per-grammar
/// `<ident>.registry.json` sidecar.
///
/// Owned (not `&'static`) because the proc-macro builds the registry
/// lazily on first reference; the cached registry lives in a module-
/// level `OnceLock` (one per marker), so the lookup is amortised across
/// every macro invocation in the same compiler process.
pub(crate) struct GrammarRegistry {
    /// Entry-rule name resolved from the IR's `ir.entry`-keyed lookup.
    /// Path resolution starts at this rule's layout.
    pub(crate) entry_rule: String,
    /// Production registry projected by `project_types` and serialised
    /// to disk by `cargo xtask regen`.
    pub(crate) registry: StructRegistry,
}

/// Sidecar JSON shape — must mirror
/// `xtask::regen::GrammarRegistrySidecar` byte-for-byte.
#[derive(serde::Deserialize)]
struct Sidecar {
    entry_rule: String,
    registry: StructRegistry,
}

/// Resolve a marker's trailing identifier (`Json`, `CssL4`, `Sheets`,
/// `Bbnf`) to its grammar ident under
/// `[workspace.metadata.bbnf.grammars]`. Returns `None` if the marker
/// is unknown — the macro surfaces a `syn::Error` naming the supported
/// markers.
///
/// The mapping is intentionally explicit. Each marker corresponds to
/// exactly one grammar ident; the inverse is also 1:1 today (no two
/// markers ever resolve to the same grammar). Post-W5 marker additions
/// extend this match.
fn marker_to_grammar_ident(marker: &str) -> Option<&'static str> {
    match marker {
        "Json" => Some("json"),
        "CssL4" => Some("css_l4"),
        "Sheets" => Some("google_sheets"),
        "Bbnf" => Some("bbnf"),
        _ => None,
    }
}

/// Supported marker names, sorted, for diagnostic alternatives.
pub(crate) fn supported_markers() -> &'static [&'static str] {
    &["Bbnf", "CssL4", "Json", "Sheets"]
}

/// Resolve a marker's grammar registry through the per-grammar sidecar.
///
/// First call per marker reads the sidecar from disk and deserialises
/// it; subsequent calls return the cached `&'static GrammarRegistry`
/// from a marker-keyed `OnceLock`. Returns `None` when the marker is
/// unknown; surfaces an `Err(String)` when the sidecar is missing or
/// malformed (the proc-macro converts this to a `syn::Error` anchored
/// at the marker token).
pub(crate) fn registry_for_marker(
    marker: &str,
) -> Option<Result<&'static GrammarRegistry, String>> {
    let ident = marker_to_grammar_ident(marker)?;
    Some(load_registry_for_ident(ident, marker))
}

/// Per-marker `OnceLock` cache. Each marker has its own slot; the slot
/// stores the loaded registry for the lifetime of the proc-macro's
/// process. Concurrent macro invocations across crates in the same
/// `cargo build` share the same proc-macro instance, so the cache is
/// correct + non-redundant.
fn cache_slot_for_ident(ident: &str) -> Option<&'static OnceLock<GrammarRegistry>> {
    static JSON: OnceLock<GrammarRegistry> = OnceLock::new();
    static CSS_L4: OnceLock<GrammarRegistry> = OnceLock::new();
    static GOOGLE_SHEETS: OnceLock<GrammarRegistry> = OnceLock::new();
    static BBNF: OnceLock<GrammarRegistry> = OnceLock::new();

    match ident {
        "json" => Some(&JSON),
        "css_l4" => Some(&CSS_L4),
        "google_sheets" => Some(&GOOGLE_SHEETS),
        "bbnf" => Some(&BBNF),
        _ => None,
    }
}

/// Read the sidecar JSON for `ident`, deserialise, and cache. Returns
/// the cached `&GrammarRegistry` on hit; on miss, performs the read +
/// parse and stores the result in the marker's `OnceLock` slot.
fn load_registry_for_ident(
    ident: &'static str,
    marker: &str,
) -> Result<&'static GrammarRegistry, String> {
    let slot = cache_slot_for_ident(ident).ok_or_else(|| {
        format!(
            "internal: marker `{marker}` resolved to grammar ident `{ident}` but no cache slot \
             registered (extend `cache_slot_for_ident`)",
        )
    })?;

    if let Some(cached) = slot.get() {
        return Ok(cached);
    }

    let bytes = sidecar_for_ident(ident).ok_or_else(|| {
        format!(
            "internal: marker `{marker}` resolved to grammar ident `{ident}` but no registry \
             sidecar is embedded (extend `sidecar_for_ident`)",
        )
    })?;

    let parsed: Sidecar = serde_json::from_str(bytes).map_err(|err| {
        format!(
            "failed to parse registry sidecar `registry/{ident}.registry.json` for marker \
             `{marker}`: {err} — the sidecar is likely stale; regenerate via \
             `cargo xtask regen --grammar {ident}`",
        )
    })?;

    let resolved = GrammarRegistry {
        entry_rule: parsed.entry_rule,
        registry: parsed.registry,
    };

    // Race-free init: the loser's `resolved` is dropped; both threads
    // observe the same `&'static GrammarRegistry`.
    Ok(slot.get_or_init(|| resolved))
}

/// The embedded `<ident>.registry.json` sidecar: the in-crate copy at
/// `registry/<ident>.registry.json`, which `cargo xtask regen` writes
/// from `crates/core/src/grammar/generated/` and `regen --check` keeps
/// equal to it. Embedded rather than read at expansion time, so the
/// macro needs no file outside its own package (sk-v27 LW2.TOPO-4c).
fn sidecar_for_ident(ident: &str) -> Option<&'static str> {
    macro_rules! sidecar {
        ($name:literal) => {
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/registry/", $name, ".registry.json"))
        };
    }
    match ident {
        "json" => Some(sidecar!("json")),
        "css_l4" => Some(sidecar!("css_l4")),
        "google_sheets" => Some(sidecar!("google_sheets")),
        "bbnf" => Some(sidecar!("bbnf")),
        _ => None,
    }
}
