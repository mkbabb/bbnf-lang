//! `EClassFacts` — monotone per-class lattice data consumed by
//! downstream phases of Tranche AA.
//!
//! Every field is a monotone lattice:
//!
//! - `first_set` joins by union (bit-or over `CharSet128`).
//! - `nullable` joins by `||`.
//! - `width.min` joins by `min` (the tightest lower bound, widened
//!   toward zero when siblings disagree).
//! - `width.max` joins by `max` (the loosest upper bound, widened
//!   toward infinity / `None` when any sibling has unbounded max).
//! - `literal_sid`, `regex_sid` join by "same value → keep; otherwise
//!   drop to None". This is monotonically lossy (information can only
//!   disappear, never appear), which is the correct direction for
//!   these fast-path facts.

use bbnf_regex::sets::charset::CharSet128;

use crate::StringId;

/// Bounded length lattice. `min` is saturated on addition;
/// `max == None` represents unbounded (e.g. `T+`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WidthBound {
    pub min: u32,
    pub max: Option<u32>,
}

impl Default for WidthBound {
    fn default() -> Self {
        Self { min: 0, max: Some(0) }
    }
}

impl WidthBound {
    /// Unit-width bound: exactly `n` bytes.
    pub fn exact(n: u32) -> Self {
        Self { min: n, max: Some(n) }
    }

    /// Unknown upper bound: `≥ min`, unbounded above.
    pub fn unbounded_from(min: u32) -> Self {
        Self { min, max: None }
    }

    /// Monotone lattice join — widen both endpoints.
    /// Returns whether the result changed.
    pub fn join(&mut self, other: &Self) -> bool {
        let mut changed = false;
        let new_min = self.min.min(other.min);
        if new_min != self.min {
            self.min = new_min;
            changed = true;
        }
        let new_max = match (self.max, other.max) {
            (Some(a), Some(b)) => Some(a.max(b)),
            _ => None, // any unbounded widens to unbounded
        };
        if new_max != self.max {
            self.max = new_max;
            changed = true;
        }
        changed
    }
}

/// Per-e-class lattice facts. One instance per e-class, joined
/// monotonically when classes merge.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EClassFacts {
    /// ASCII first-byte set. Empty on construction; union over all
    /// e-nodes in the class.
    pub first_set: CharSet128,
    /// True iff the class matches the empty string.
    pub nullable: bool,
    /// Byte-width bounds.
    pub width: WidthBound,
    /// Fast-path: the StringId of a `Literal` leaf when all e-nodes
    /// in the class are the same literal. `None` once siblings disagree.
    pub literal_sid: Option<StringId>,
    /// Fast-path: the StringId of a `Regex` leaf under the same rule.
    pub regex_sid: Option<StringId>,
}

impl Default for EClassFacts {
    fn default() -> Self {
        Self {
            first_set: CharSet128::new(),
            nullable: false,
            width: WidthBound::default(),
            literal_sid: None,
            regex_sid: None,
        }
    }
}

impl EClassFacts {
    /// Facts for an epsilon node: matches empty, width=0.
    pub fn epsilon() -> Self {
        Self {
            first_set: CharSet128::new(),
            nullable: true,
            width: WidthBound { min: 0, max: Some(0) },
            literal_sid: None,
            regex_sid: None,
        }
    }

    /// Facts for a literal leaf. First set is the single first byte
    /// of the literal (approximated as empty until the pool resolves).
    /// The actual first-byte set is filled in by `GrammarAnalysis::make`
    /// from the shared string pool when it's available; the bare fact
    /// here holds the literal sid as a fast-path handle.
    pub fn literal(sid: StringId) -> Self {
        Self {
            first_set: CharSet128::new(),
            nullable: false,
            width: WidthBound::unbounded_from(1),
            literal_sid: Some(sid),
            regex_sid: None,
        }
    }

    /// Facts for a regex leaf. First set / nullable / width are
    /// computed by downstream `RegexInfo` analysis; the bare fact
    /// here holds the regex sid.
    pub fn regex(sid: StringId) -> Self {
        Self {
            first_set: CharSet128::new(),
            nullable: false,
            width: WidthBound::unbounded_from(0),
            literal_sid: None,
            regex_sid: Some(sid),
        }
    }

    /// Facts for an unresolved `Ref` leaf. Before the analysis reaches
    /// a fixed point the Ref's target may not exist yet; start
    /// pessimistically (empty first_set, not nullable) and let the
    /// e-class merge widen as the target's class joins this one.
    pub fn unknown_ref() -> Self {
        Self::default()
    }

    /// Monotone lattice join. Returns whether `self` changed.
    pub fn merge(&mut self, other: &Self) -> bool {
        let mut changed = false;

        if !other.first_set.is_subset(&self.first_set) {
            self.first_set.union(&other.first_set);
            changed = true;
        }

        if !self.nullable && other.nullable {
            self.nullable = true;
            changed = true;
        }

        if self.width.join(&other.width) {
            changed = true;
        }

        // Monotonically lossy: disagreement erases the fast-path.
        match (self.literal_sid, other.literal_sid) {
            (None, _) => {}
            (Some(_), None) => {
                self.literal_sid = None;
                changed = true;
            }
            (Some(a), Some(b)) if a != b => {
                self.literal_sid = None;
                changed = true;
            }
            _ => {}
        }
        match (self.regex_sid, other.regex_sid) {
            (None, _) => {}
            (Some(_), None) => {
                self.regex_sid = None;
                changed = true;
            }
            (Some(a), Some(b)) if a != b => {
                self.regex_sid = None;
                changed = true;
            }
            _ => {}
        }

        changed
    }
}
