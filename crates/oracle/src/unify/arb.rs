//! Deterministic generators for the unifier's laws (the crate's SplitMix64, so
//! a failing case is reproduced by its seed).

use crate::sample::Rng;

use super::ty::{Base, Head, Kind, Ty, Var, Vars};

pub(super) const KINDS: [Kind; 3] = [Kind::Num, Kind::Eq, Kind::Any];

pub(super) fn kind(rng: &mut Rng) -> Kind {
    *rng.pick(&KINDS)
}

/// A kind holding a type whose least kind is `least`.
pub(super) fn kind_at_least(rng: &mut Rng, least: Kind) -> Kind {
    let ks: Vec<Kind> = KINDS.into_iter().filter(|k| *k >= least).collect();
    *rng.pick(&ks)
}

/// `n` variables of random kinds.
pub(super) fn pool(rng: &mut Rng, vars: &mut Vars, n: usize) -> Vec<Var> {
    (0..n).map(|_| vars.fresh(kind(rng))).collect()
}

fn leaf(rng: &mut Rng) -> Ty {
    match rng.below(6) {
        0 => Ty::Con(Head::Host((*rng.pick(&["Color", "String"])).into()), Vec::new()),
        1 => Ty::Con(Head::Named((*rng.pick(&["R0", "R1"])).into()), Vec::new()),
        _ => Ty::base(*rng.pick(&Base::ALL)),
    }
}

/// A constructor with `n` argument slots, chosen at random, and its arity.
fn constructor(rng: &mut Rng) -> (Head, usize) {
    match rng.below(6) {
        0 => (Head::Prod, 2 + rng.below(2) as usize),
        1 => (Head::Sum, 2 + rng.below(2) as usize),
        2 => (Head::Option, 1),
        3 => (Head::List, 1),
        _ => (Head::Fn, 1 + rng.below(3) as usize),
    }
}

/// A random type of depth at most `depth`, its variables drawn from `pool`.
pub(super) fn ty(rng: &mut Rng, pool: &[Var], depth: u32) -> Ty {
    if depth == 0 || rng.chance(1, 3) {
        return if !pool.is_empty() && rng.chance(1, 2) { Ty::Var(*rng.pick(pool)) } else { leaf(rng) };
    }
    let (h, n) = constructor(rng);
    Ty::Con(h, (0..n).map(|_| ty(rng, pool, depth - 1)).collect())
}

/// A constructor term in which `v` occurs strictly inside, at a random place.
pub(super) fn containing(rng: &mut Rng, pool: &[Var], v: Var, depth: u32) -> Ty {
    let (h, n) = constructor(rng);
    let at = rng.below(n as u64) as usize;
    let args = (0..n)
        .map(|i| match i == at {
            true if depth > 1 && rng.chance(1, 2) => containing(rng, pool, v, depth - 1),
            true => Ty::Var(v),
            false => ty(rng, pool, depth.saturating_sub(1)),
        })
        .collect();
    Ty::Con(h, args)
}

/// Replace random subterms of `t` by fresh variables of a kind that holds the
/// subterm, recording the replaced subterm in `theta`. So `theta` applied to
/// the result is `t`, and `theta` respects kinds.
pub(super) fn abstracted(rng: &mut Rng, vars: &mut Vars, t: &Ty, theta: &mut Vec<(Var, Ty)>) -> Ty {
    if rng.chance(1, 4) {
        let k = kind_at_least(rng, vars.kind_of(t));
        let v = vars.fresh(k);
        theta.push((v, t.clone()));
        return Ty::Var(v);
    }
    match t {
        Ty::Var(_) => t.clone(),
        Ty::Con(h, args) => Ty::Con(h.clone(), args.iter().map(|a| abstracted(rng, vars, a, theta)).collect()),
    }
}

/// `t` with each variable renamed to a fresh one of the same kind.
pub(super) fn renamed(vars: &mut Vars, t: &Ty) -> Ty {
    let map: std::collections::BTreeMap<Var, Ty> = t.free_vars().into_iter().map(|v| (v, Ty::Var(vars.fresh(vars.kind(v))))).collect();
    super::subst::Subst::from_map(map).apply(t)
}
