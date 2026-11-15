//! Substitutions and kinded first-order unification with the occurs check.
//!
//! The substitution is kept idempotent at every step (no variable of its
//! domain occurs in its range): a new binding is applied to the existing range
//! before it is added. So `apply` is one pass, and the most general unifier is
//! returned in the textbook triangular-free form.

use std::collections::BTreeMap;

use super::ty::{Kind, Ty, Var, Vars};

/// A finite map from variables to types, idempotent by construction.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct Subst {
    map: BTreeMap<Var, Ty>,
}

/// Why two types do not unify.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum UnifyError {
    /// Different constructors, or one constructor at different arities.
    Clash(Ty, Ty),
    /// The variable occurs in the type it would be bound to.
    Occurs(Var, Ty),
    /// The type is not in the variable's kind.
    Kind(Var, Kind, Ty),
}

impl Subst {
    /// A substitution from an explicit map. The caller guarantees idempotence
    /// (no variable of the domain occurs in the range).
    pub(crate) fn from_map(map: BTreeMap<Var, Ty>) -> Subst {
        Subst { map }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub(crate) fn get(&self, v: Var) -> Option<&Ty> {
        self.map.get(&v)
    }

    pub(crate) fn bindings(&self) -> impl Iterator<Item = (Var, &Ty)> {
        self.map.iter().map(|(v, t)| (*v, t))
    }

    pub(crate) fn apply(&self, t: &Ty) -> Ty {
        match t {
            Ty::Var(v) => self.map.get(v).cloned().unwrap_or_else(|| t.clone()),
            Ty::Con(h, args) => Ty::Con(h.clone(), args.iter().map(|a| self.apply(a)).collect()),
        }
    }

    /// Add `v := t`, where `t` has already been applied and passed the
    /// occurs and kind checks. The binding is composed into the range first,
    /// which keeps the substitution idempotent.
    fn bind(&mut self, v: Var, t: Ty) {
        let single = Subst { map: BTreeMap::from([(v, t.clone())]) };
        for r in self.map.values_mut() {
            *r = single.apply(r);
        }
        self.map.insert(v, t);
    }

    /// Extend this substitution to a unifier of `a` and `b`, or refuse.
    pub(crate) fn unify(&mut self, vars: &Vars, a: &Ty, b: &Ty) -> Result<(), UnifyError> {
        let (a, b) = (self.apply(a), self.apply(b));
        match (&a, &b) {
            (Ty::Var(x), Ty::Var(y)) if x == y => Ok(()),
            (Ty::Var(x), Ty::Var(y)) => {
                // Bind the variable of the larger kind to the one of the
                // smaller: the result has the meet of the two kinds.
                if vars.kind(*x) <= vars.kind(*y) {
                    self.bind(*y, a.clone());
                } else {
                    self.bind(*x, b.clone());
                }
                Ok(())
            }
            (Ty::Var(x), t) | (t, Ty::Var(x)) => self.bind_checked(vars, *x, t.clone()),
            (Ty::Con(h1, a1), Ty::Con(h2, a2)) => {
                if h1 != h2 || a1.len() != a2.len() {
                    return Err(UnifyError::Clash(a.clone(), b.clone()));
                }
                a1.iter().zip(a2).try_for_each(|(x, y)| self.unify(vars, x, y))
            }
        }
    }

    fn bind_checked(&mut self, vars: &Vars, x: Var, t: Ty) -> Result<(), UnifyError> {
        if t.mentions(x) {
            return Err(UnifyError::Occurs(x, t));
        }
        if vars.kind_of(&t) > vars.kind(x) {
            return Err(UnifyError::Kind(x, vars.kind(x), t));
        }
        self.bind(x, t);
        Ok(())
    }
}

/// The most general unifier of `a` and `b`.
pub(crate) fn mgu(vars: &Vars, a: &Ty, b: &Ty) -> Result<Subst, UnifyError> {
    let mut s = Subst::default();
    s.unify(vars, a, b)?;
    Ok(s)
}
