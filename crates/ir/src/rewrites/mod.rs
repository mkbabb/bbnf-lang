//! Rewrite-rule storage substrate.
//!
//! The `rewrites/` module is the per-grammar rule store consumed by
//! BB.close (Wave 4) and downstream cost-config integration. It
//! provides:
//!
//! - [`Rule`] / [`RuleSet`] — owned rule data + ordered storage.
//! - [`RuleClass`] — Class-1 / Class-2 / Class-3 tier (defined in
//!   [`tiering`]).
//! - [`RuleId`] — a u32 newtype distinct from
//!   [`crate::types::RuleId`] (grammar-rule index): a rewrite-rule id.
//! - RON load/save via [`RuleSet::load_from_ron`] /
//!   [`RuleSet::save_to_ron`] (delegates to the [`schema`] layer).
//!
//! BB.scaffold.B authors this substrate ahead of the e-graph `ruler`
//! consumer; module loading is independent. BB.close wires the two
//! together via the per-grammar adapter in
//! `crates/ir/src/passes/cost_integration.rs` (Wave 4 file).

pub mod base;
pub mod rank;
pub mod schema;
pub mod tiering;

pub use base::{Alphabet, Atom, Pattern, PatternRef, Witness};
pub use rank::{RankConfig, rank, select_top_k};
pub use schema::{RuleFile, RuleSerialized, SchemaError, SCHEMA_VERSION};
pub use tiering::{RuleClass, classify};

use std::path::Path;

/// A rewrite-rule identifier within a [`RuleSet`].
///
/// Distinct from [`crate::types::RuleId`] (which indexes grammar
/// rules); this is an index into the rewrite-rule storage.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RewriteRuleId(pub u32);

impl serde::Serialize for RewriteRuleId {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(s)
    }
}

impl<'de> serde::Deserialize<'de> for RewriteRuleId {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        u32::deserialize(d).map(RewriteRuleId)
    }
}

/// One rewrite rule: `lhs ⇒ rhs` plus enough metadata for the ranker
/// + classifier + cost integration to decide whether to apply it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rule {
    /// Stable id within the [`RuleSet`].
    pub id: RewriteRuleId,
    /// Tier (Class-1 / Class-2 / Class-3); set by [`classify`] at
    /// insertion or load.
    pub class: RuleClass,
    /// Pattern matched against the e-graph.
    pub lhs: Pattern,
    /// Pattern installed as the equivalent.
    pub rhs: Pattern,
    /// How the rule was discovered + how strongly the oracle accepted
    /// it.
    pub witness: Witness,
    /// `cost(rhs) - cost(lhs)`. Negative ⇒ the rule wins on the
    /// extraction cost model. Computed by the cost-integration pass
    /// at insertion / load.
    pub cost_delta: i64,
    /// Number of times the rule fired in a representative parse-corpus
    /// sample. Populated by Wave 4's profiler integration; defaults to 0
    /// for fresh-loaded rules.
    pub frequency: u32,
}

impl Rule {
    /// Construct a rule with default `frequency = 0`. The class is
    /// computed from `lhs` / `rhs` shape; the caller fills `cost_delta`.
    pub fn new(
        id: RewriteRuleId,
        lhs: Pattern,
        rhs: Pattern,
        witness: Witness,
        cost_delta: i64,
    ) -> Self {
        let class = classify(&lhs, &rhs);
        Self { id, class, lhs, rhs, witness, cost_delta, frequency: 0 }
    }
}

/// An ordered, owned bag of rewrite rules for one grammar.
///
/// Storage is a `Vec<Rule>`; the ordering is the *current* ranking
/// (top-k extraction reads the prefix). [`rank`] re-orders in place.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuleSet {
    /// Grammar identifier this rule set is keyed against (e.g.
    /// `"json"`, `"css_l4"`). Surfaces in RON files as the
    /// `grammar:` field.
    pub grammar: String,
    /// Schema version of the most recent load (or [`SCHEMA_VERSION`]
    /// for fresh sets).
    pub schema_version: u32,
    /// The rules themselves, in current ranking order.
    pub rules: Vec<Rule>,
}

impl RuleSet {
    /// Construct an empty set keyed against `grammar`.
    pub fn new(grammar: impl Into<String>) -> Self {
        Self {
            grammar: grammar.into(),
            schema_version: SCHEMA_VERSION,
            rules: Vec::new(),
        }
    }

    /// Add a rule, assigning a fresh id (one past the current max).
    pub fn push(&mut self, mut rule: Rule) -> RewriteRuleId {
        let id = RewriteRuleId(self.rules.len() as u32);
        rule.id = id;
        self.rules.push(rule);
        id
    }

    /// Number of rules.
    pub fn len(&self) -> usize {
        self.rules.len()
    }

    /// Whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    /// Iterate rules in current ranking order.
    pub fn iter(&self) -> std::slice::Iter<'_, Rule> {
        self.rules.iter()
    }

    /// Filter by [`RuleClass`].
    pub fn by_class(&self, class: RuleClass) -> impl Iterator<Item = &Rule> {
        self.rules.iter().filter(move |r| r.class == class)
    }

    /// Load a rule set from a RON file. Returns [`SchemaError`] on
    /// version mismatch or malformed input.
    pub fn load_from_ron(path: &Path) -> Result<Self, SchemaError> {
        let bytes = std::fs::read(path).map_err(SchemaError::Io)?;
        let s = std::str::from_utf8(&bytes).map_err(|e| SchemaError::Decode(e.to_string()))?;
        let file: RuleFile = ron::from_str(s).map_err(|e| SchemaError::Decode(e.to_string()))?;
        Self::from_file(file)
    }

    /// Save this rule set to a RON file at `path`.
    pub fn save_to_ron(&self, path: &Path) -> Result<(), SchemaError> {
        let file = self.to_file();
        let s = ron::ser::to_string_pretty(&file, ron::ser::PrettyConfig::default())
            .map_err(|e| SchemaError::Encode(e.to_string()))?;
        std::fs::write(path, s).map_err(SchemaError::Io)
    }

    /// Convert to the schema-layer [`RuleFile`] form.
    pub fn to_file(&self) -> RuleFile {
        RuleFile {
            schema_version: self.schema_version,
            grammar: self.grammar.clone(),
            rules: self.rules.iter().map(RuleSerialized::from_rule).collect(),
        }
    }

    /// Reconstruct a [`RuleSet`] from a schema-layer [`RuleFile`].
    /// Validates `schema_version` equals [`SCHEMA_VERSION`].
    pub fn from_file(file: RuleFile) -> Result<Self, SchemaError> {
        if file.schema_version != SCHEMA_VERSION {
            return Err(SchemaError::VersionMismatch {
                expected: SCHEMA_VERSION,
                got: file.schema_version,
            });
        }
        let rules = file
            .rules
            .into_iter()
            .enumerate()
            .map(|(i, r)| r.into_rule(RewriteRuleId(i as u32)))
            .collect();
        Ok(Self {
            grammar: file.grammar,
            schema_version: file.schema_version,
            rules,
        })
    }
}

