//! Prenex schemes `∀ᾱ:κ̄. τ`: generalise and instantiate.
//!
//! Every binding in the language is top-level (LAYOUT-HM-V2 §1.3 (V)), so the
//! environment holds closed schemes and the current component's monotypes;
//! generalisation closes over the variables not free in that environment.
//! There are no levels. A quantified variable keeps its kind.

use std::collections::{BTreeMap, BTreeSet};

use super::subst::Subst;
use super::ty::{Ty, Var, Vars};

/// `∀ quantified. body`; each quantified variable's kind is the one the supply
/// recorded for it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Scheme {
    pub(crate) quantified: Vec<Var>,
    pub(crate) body: Ty,
}

impl Scheme {
    /// A scheme quantifying nothing.
    pub(crate) fn mono(t: Ty) -> Scheme {
        Scheme { quantified: Vec::new(), body: t }
    }

    /// The variables free in the scheme.
    pub(crate) fn free_vars(&self) -> Vec<Var> {
        self.body.free_vars().into_iter().filter(|v| !self.quantified.contains(v)).collect()
    }
}

/// Close `t` over every variable not free in the environment, in order of
/// first occurrence, each with its kind kept.
pub(crate) fn generalise(env_free: &BTreeSet<Var>, t: &Ty) -> Scheme {
    let quantified = t.free_vars().into_iter().filter(|v| !env_free.contains(v)).collect();
    Scheme { quantified, body: t.clone() }
}

/// A fresh instance: each quantified variable is replaced by a new variable of
/// the same kind; free variables are left as they are.
pub(crate) fn instantiate(vars: &mut Vars, s: &Scheme) -> Ty {
    let mut map = BTreeMap::new();
    for &q in &s.quantified {
        let k = vars.kind(q);
        map.insert(q, Ty::Var(vars.fresh(k)));
    }
    Subst::from_map(map).apply(&s.body)
}
