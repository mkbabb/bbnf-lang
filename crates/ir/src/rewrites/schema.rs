//! RON schema for per-grammar rule files.
//!
//! Rule files live at `grammar/<g>/rewrites/*.ron` and feed the
//! per-grammar [`crate::rewrites::RuleSet`] consumed by the BB.close
//! cost-config integration.
//!
//! # Versioning
//!
//! [`SCHEMA_VERSION`] is the v1 format. A future schema bump increments
//! this constant and adds a from-v1 migration path inside
//! [`RuleFile::deserialize`]. Loaders that see a version they don't
//! understand return [`SchemaError::VersionMismatch`].
//!
//! # Round-trip discipline
//!
//! The only way to construct a [`RuleFile`] from RON is
//! [`crate::rewrites::RuleSet::load_from_ron`]; the only way to write
//! one is [`crate::rewrites::RuleSet::save_to_ron`]. Both delegate to
//! this module's [`RuleSerialized`] adapter which mirrors [`crate::rewrites::Rule`]
//! field-for-field — the [`Rule`] type carries [`crate::rewrites::RewriteRuleId`]
//! and [`crate::rewrites::tiering::RuleClass`] which both serialize
//! transparently, but we don't `#[derive(Serialize, Deserialize)]` on
//! [`Rule`] itself because the `class` field is reconstructed at load
//! time (via [`crate::rewrites::tiering::classify`]) rather than
//! trusted from disk — keeps the file format minimal and the
//! classifier the single source of truth.

use serde::{Deserialize, Serialize};

use super::base::{Pattern, Witness};
use super::tiering::{RuleClass, classify};
use super::{RewriteRuleId, Rule};

/// Schema-version constant. Bump on incompatible format changes.
pub const SCHEMA_VERSION: u32 = 1;

/// Top-level RON document for a per-grammar rule file.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuleFile {
    /// Format-version handle. Reject unknown versions at load.
    pub schema_version: u32,
    /// Grammar identifier (e.g. `"json"`, `"css_l4"`). Cross-checked
    /// against the consumer-side `GrammarIR::name` at integration
    /// time.
    pub grammar: String,
    /// Rule list — the wire form of [`crate::rewrites::Rule`].
    pub rules: Vec<RuleSerialized>,
}

impl RuleFile {
    /// Construct a fresh empty [`RuleFile`] keyed against `grammar`.
    pub fn empty(grammar: impl Into<String>) -> Self {
        Self {
            schema_version: SCHEMA_VERSION,
            grammar: grammar.into(),
            rules: Vec::new(),
        }
    }
}

/// On-disk representation of one rule. Mirrors [`crate::rewrites::Rule`]
/// but omits the `class` field — the classifier reconstructs it from
/// `lhs` / `rhs` at load time.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuleSerialized {
    /// LHS pattern.
    pub lhs: Pattern,
    /// RHS pattern.
    pub rhs: Pattern,
    /// Witness provenance.
    pub witness: Witness,
    /// Cost delta (RHS cost minus LHS cost).
    pub cost_delta: i64,
    /// Observed firing frequency (per 1k bytes; defaults to 0 for
    /// authored rules).
    #[serde(default)]
    pub frequency: u32,
}

impl RuleSerialized {
    /// Project a [`crate::rewrites::Rule`] to its on-disk form.
    pub fn from_rule(r: &Rule) -> Self {
        Self {
            lhs: r.lhs.clone(),
            rhs: r.rhs.clone(),
            witness: r.witness.clone(),
            cost_delta: r.cost_delta,
            frequency: r.frequency,
        }
    }

    /// Inflate the on-disk form into a [`crate::rewrites::Rule`] with
    /// the classifier-assigned class.
    pub fn into_rule(self, id: RewriteRuleId) -> Rule {
        let class: RuleClass = classify(&self.lhs, &self.rhs);
        Rule {
            id,
            class,
            lhs: self.lhs,
            rhs: self.rhs,
            witness: self.witness,
            cost_delta: self.cost_delta,
            frequency: self.frequency,
        }
    }
}

/// Errors raised by RON load / save.
#[derive(Debug)]
pub enum SchemaError {
    /// I/O failure (file missing, permission denied, &c).
    Io(std::io::Error),
    /// RON parse / encode failure (malformed input, bad UTF-8).
    Decode(String),
    /// RON encode failure during save.
    Encode(String),
    /// `schema_version` field on disk doesn't match
    /// [`SCHEMA_VERSION`].
    VersionMismatch {
        /// What this build expects.
        expected: u32,
        /// What the file says.
        got: u32,
    },
}

impl std::fmt::Display for SchemaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SchemaError::Io(e) => write!(f, "io: {e}"),
            SchemaError::Decode(s) => write!(f, "decode: {s}"),
            SchemaError::Encode(s) => write!(f, "encode: {s}"),
            SchemaError::VersionMismatch { expected, got } => {
                write!(f, "schema version mismatch: file says {got}, build expects {expected}")
            }
        }
    }
}

impl std::error::Error for SchemaError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SchemaError::Io(e) => Some(e),
            _ => None,
        }
    }
}

