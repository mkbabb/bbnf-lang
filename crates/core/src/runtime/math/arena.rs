//! AZ-II.cutover.E (Phase 2) — Math parse arena.
//!
//! Mirror of `CsvArena` / `BbnfArena` / `JsonArena`. Owns every
//! compound child slice; resolves handles via [`MathArena::compound`].

use bbnf_ir::RuleId;

use crate::runtime::math::value::MathValue;

/// Discriminator — the structural shape of a [`MathValue::Compound`].
///
/// One arm per compound rule in `grammar/misc/math.bbnf`. The deep
/// alias chain (`p` → `pp` → `ppp` → `pppp` → `ppppp` → `pppppp`)
/// produces six distinct rule names that all wrap a literal `(`; the
/// classifier admits each as its own kind so consumers can identify
/// which alias level the parser threaded through.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathCompoundKind {
    /// `expr = term, { ("+" | "-"), term }`.
    Expr,
    /// `term = factor, { ("*" | "/"), factor }`.
    Term,
    /// `factor = number | wrapped` — Alt.
    Factor,
    /// `wrapped = pppppp, expr, ")"`.
    Wrapped,
    /// `p = "("`.
    P,
    /// `pp = p` — alias chain link.
    Pp,
    /// `ppp = pp`.
    Ppp,
    /// `pppp = ppp`.
    Pppp,
    /// `ppppp = pppp`.
    Ppppp,
    /// `pppppp = ppppp`.
    Pppppp,
    /// Catch-all for compound rules not recognised by the alphabet.
    Other,
}

impl MathCompoundKind {
    /// Resolve a rule id to a kind.
    ///
    /// `grammar/misc/math.bbnf` parses purely through Pratt operator
    /// emission: no struct layouts are emitted into
    /// `crates/core/src/grammar/generated/math.rs`, so the rule-id
    /// space is empty in production. Variants are retained for AST
    /// exhaustiveness against future structural emission; every rule
    /// id collapses to [`Self::Other`].
    pub fn from_rule_id(_rule_id: RuleId) -> Self {
        Self::Other
    }
}

/// A compound entry in the arena.
#[derive(Debug, Clone)]
pub struct MathCompound<'p> {
    pub kind: MathCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<MathValue<'p>>,
}

impl<'p> Default for MathCompound<'p> {
    fn default() -> Self {
        Self {
            kind: MathCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}

/// Opaque handle for a compound entry. `0` = `EMPTY`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MathCompoundId(u32);

impl MathCompoundId {
    pub const EMPTY: Self = Self(0);

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Owning slab for compound entries.
#[derive(Debug, Default)]
pub struct MathArena<'p> {
    compounds: Vec<MathCompound<'p>>,
    empty: MathCompound<'p>,
}

impl<'p> MathArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: MathCompound::default(),
        }
    }

    #[inline]
    pub fn push_compound(&mut self, compound: MathCompound<'p>) -> MathCompoundId {
        self.compounds.push(compound);
        let idx = self.compounds.len() as u32;
        MathCompoundId(idx)
    }

    #[inline]
    pub fn compound(&self, id: MathCompoundId) -> &MathCompound<'p> {
        match id.slab_index() {
            None => &self.empty,
            Some(i) => &self.compounds[i],
        }
    }

    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }

    /// Roll back the arena to a prior compound-count snapshot.
    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}
