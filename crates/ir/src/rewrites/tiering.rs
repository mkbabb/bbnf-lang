//! Class-1 / Class-2 / Class-3 rewrite-rule classifier.
//!
//! Given the LHS / RHS shape pair, [`classify`] decides which tier the
//! rule belongs to. The classifier is *purely structural* — it takes
//! no context, makes no semantic decisions, and never consults an
//! oracle. (The oracle stamps [`crate::rewrites::base::Witness`]
//! upstream of this call; the witness is orthogonal to the tier.)
//!
//! ## Tier definitions
//!
//! | Tier      | Shape relation                      | Cost discipline      | Examples                          |
//! |-----------|-------------------------------------|----------------------|-----------------------------------|
//! | Class-1   | RHS is a strict structural sub-tree of LHS | Free win — always apply | `Seq(x) ⇒ x`, `Alt(x) ⇒ x`        |
//! | Class-2   | LHS contains a constant-foldable sub-pattern | Conditional — apply if cost-delta ≤ 0 | `Seq(Repeat{ε, 0, 0}, x) ⇒ x`     |
//! | Class-3   | Different shape / variable shuffle  | Adversarial — accept only with VM oracle | `a >> b ⇒ a << b` (for free b)    |
//!
//! ## Algorithmic skeleton
//!
//! 1. **Class-1 check** — does the RHS appear as a structural sub-tree
//!    of the LHS? (Walk the LHS, return true if any sub-tree
//!    structurally equals the RHS.)
//! 2. **Class-2 check** — does the LHS contain a sub-pattern that
//!    constant-folds (an `Atom::Epsilon`, a `Repeat { lo: 0, hi: 0 }`,
//!    a `Negate(Negate(_))`)?
//! 3. Otherwise — Class-3.
//!
//! Steps 1 and 2 can both apply to one rule; the lower (more
//! permissive) tier wins.

use serde::{Deserialize, Serialize};

use super::base::{Atom, Pattern};

/// Rewrite-rule tier. Lower tiers are safer / cheaper.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RuleClass {
    /// Pure structural elimination — RHS is a sub-tree of LHS.
    Class1,
    /// Constant-fold / partial-eval — LHS has a foldable sub-pattern.
    Class2,
    /// Semantic rewrite — shape change requiring oracle acceptance.
    Class3,
}

/// Classify a rule from its LHS / RHS pair. No context required.
pub fn classify(lhs: &Pattern, rhs: &Pattern) -> RuleClass {
    if is_class1(lhs, rhs) {
        RuleClass::Class1
    } else if is_class2(lhs) {
        RuleClass::Class2
    } else {
        RuleClass::Class3
    }
}

/// Class-1: RHS appears as a strict sub-tree of LHS *and* RHS is
/// strictly smaller than LHS (forbids the trivial LHS = RHS case).
fn is_class1(lhs: &Pattern, rhs: &Pattern) -> bool {
    if rhs.ast_size() >= lhs.ast_size() {
        return false;
    }
    contains_subpattern(lhs, rhs)
}

/// Walk `haystack`, checking whether any sub-tree structurally
/// equals `needle`.
fn contains_subpattern(haystack: &Pattern, needle: &Pattern) -> bool {
    if haystack == needle {
        return true;
    }
    match haystack {
        Pattern::Atom(_) => false,
        Pattern::Seq(xs) | Pattern::Alt(xs) => {
            xs.iter().any(|x| contains_subpattern(x, needle))
        }
        Pattern::Repeat { inner, .. }
        | Pattern::Negate(inner)
        | Pattern::Map { inner, .. } => contains_subpattern(inner, needle),
        Pattern::Skip(a, b) | Pattern::Next(a, b) | Pattern::Minus(a, b) => {
            contains_subpattern(a, needle) || contains_subpattern(b, needle)
        }
    }
}

/// Class-2: LHS contains a constant-foldable sub-pattern. Currently
/// recognised:
///
/// - `Atom::Epsilon` adjacent to a `Seq` neighbour → could be fused.
/// - `Repeat { lo: 0, hi: 0 }` → vacuous.
/// - `Negate(Negate(x))` → double-negation cancellation.
/// - Singleton `Seq([x])` / singleton `Alt([x])` → identity wrapper.
///
/// The set is deliberately conservative; expanding this set is a
/// BB.close (Wave 4) integration concern. Each rule the cost-config
/// integration adds here must come with a test fixture pinning the
/// new triggering shape to Class-2.
fn is_class2(lhs: &Pattern) -> bool {
    match lhs {
        Pattern::Atom(Atom::Epsilon) => true,
        Pattern::Repeat { lo: 0, hi: 0, .. } => true,
        Pattern::Negate(inner) if matches!(inner.as_ref(), Pattern::Negate(_)) => true,
        Pattern::Seq(xs) if xs.len() == 1 => true,
        Pattern::Alt(xs) if xs.len() == 1 => true,
        Pattern::Seq(xs) | Pattern::Alt(xs) => xs.iter().any(is_class2),
        Pattern::Repeat { inner, .. }
        | Pattern::Negate(inner)
        | Pattern::Map { inner, .. } => is_class2(inner),
        Pattern::Skip(a, b) | Pattern::Next(a, b) | Pattern::Minus(a, b) => {
            is_class2(a) || is_class2(b)
        }
        _ => false,
    }
}

