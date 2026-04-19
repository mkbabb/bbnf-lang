//! Top-level `StyleSheet` AST — `bbnf::css::StyleSheet`.
//!
//! AX.W1.B: the entry point for the CSS Value API.
//! Isomorphic to `lightningcss::stylesheet::StyleSheet` at the
//! structural level: a `rules: Vec<CssRule>` list, plus auxiliary
//! `sources` / `license_comments` bookkeeping for file-level
//! round-trip.
//!
//! See `docs/tranches/AX/parity/css_divergence.md` for the per-variant
//! isomorphism ledger.

use std::borrow::Cow;

use super::rules::CssRule;

/// A parsed CSS style sheet.
///
/// Structurally isomorphic to `lightningcss::stylesheet::StyleSheet`:
/// every public field has a lightningcss counterpart. Projection from
/// bbnf's CSS L4 tape is `StyleSheet::from_tape`, and conversion from
/// lightningcss via `impl From<lightningcss::stylesheet::StyleSheet>
/// for StyleSheet`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct StyleSheet<'i> {
    /// Top-level rule list.
    pub rules: Vec<CssRule<'i>>,
    /// Source file names (index space matching `CssRule::Location::source_index`).
    pub sources: Vec<String>,
    /// Leading license comments (`/*! ... */`).
    pub license_comments: Vec<Cow<'i, str>>,
}

impl<'i> StyleSheet<'i> {
    /// Construct an empty style sheet.
    pub fn new() -> Self {
        Self::default()
    }

    /// Number of top-level rules.
    pub fn len(&self) -> usize {
        self.rules.len()
    }

    /// Is the style sheet empty?
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }
}
