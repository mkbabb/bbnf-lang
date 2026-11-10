//! The inline CSP's algebraic contract (sk-v27 LW1.COMP-3, born-RED).
//!
//! Two properties the inline plan relies on and does not have today:
//!
//! 1. `InlineDomain` is a join-semilattice. `LatticeDomain::join` is
//!    the least upper bound, so it must be commutative, associative,
//!    idempotent and extensive, have `bottom()` as identity, and report
//!    `true` exactly when the value changed. The law oracle below is
//!    generic over `LatticeDomain` and is run against
//!    `BitsetLatticeDomain` as the control, so a RED on `InlineDomain`
//!    is a defect of that domain and not of the oracle.
//!
//! 2. Two forced decisions that disagree are a contradiction. Forcing
//!    one rule to both `InlineBody` and `DirectCall` must make
//!    `Csp::propagate` return `Err(Unsatisfiable)`; today the second
//!    `ForceCallMode::revise` reports `Unchanged` and the first writer
//!    wins.
//!
//! Born RED in LW1.COMP-3 and cured by LW2.COMP-2: `InlineDomain` has
//! a ⊤ for a Decided/Decided conflict, and the inline plan routes
//! `Unsatisfiable` to a `CallModeConflict` naming both forces.

use csp_solver::Csp;
use csp_solver::domain::{BitsetDomain, BitsetLatticeDomain, LatticeDomain};

use super::constraints::{ForceCallMode, InlineDecision, InlineDomain};
use super::plan::{CallMode, CallModeForce};

/// One law the sample set breaks, with the operands that break it.
// Read only through `Debug`, in the assertion message.
#[derive(Debug)]
#[allow(dead_code)]
struct LawViolation<D> {
    law: &'static str,
    operands: Vec<D>,
}

fn joined<D: LatticeDomain>(a: &D, b: &D) -> (D, bool) {
    let mut out = a.clone();
    let changed = out.join(b);
    (out, changed)
}

/// Every join-semilattice law, checked over all pairs and triples of
/// `samples`.
fn lattice_law_violations<D: LatticeDomain>(samples: &[D]) -> Vec<LawViolation<D>> {
    let mut out = Vec::new();
    let mut fail = |law, operands: &[&D]| {
        out.push(LawViolation {
            law,
            operands: operands.iter().map(|d| (*d).clone()).collect(),
        })
    };
    let bottom = D::bottom();

    for a in samples {
        let (aa, changed) = joined(a, a);
        if aa != *a || changed {
            fail("idempotent: a ⊔ a = a, unchanged", &[a]);
        }
        if joined(&bottom, a).0 != *a || joined(a, &bottom).0 != *a {
            fail("identity: ⊥ ⊔ a = a = a ⊔ ⊥", &[a]);
        }
        for b in samples {
            let (ab, changed) = joined(a, b);
            if ab != joined(b, a).0 {
                fail("commutative: a ⊔ b = b ⊔ a", &[a, b]);
            }
            if changed != (ab != *a) {
                fail(
                    "change flag: join reports true iff the value changed",
                    &[a, b],
                );
            }
            if joined(a, &ab).0 != ab || joined(b, &ab).0 != ab {
                fail("extensive: a ⊑ a ⊔ b and b ⊑ a ⊔ b", &[a, b]);
            }
            for c in samples {
                let left = joined(&joined(a, b).0, c).0;
                let right = joined(a, &joined(b, c).0).0;
                if left != right {
                    fail("associative: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)", &[a, b, c]);
                }
            }
        }
    }
    out
}

fn decided(mode: CallMode) -> InlineDomain {
    InlineDomain {
        decision: InlineDecision::Decided(mode),
    }
}

/// The oracle's control: set union over bitsets is a lattice, so the
/// oracle must find nothing. Standing GREEN.
#[test]
fn bitset_lattice_domain_satisfies_the_lattice_laws() {
    let samples: Vec<BitsetLatticeDomain> = [vec![], vec![0], vec![1], vec![0, 1], vec![2, 7]]
        .into_iter()
        .map(|bits| BitsetLatticeDomain::new(BitsetDomain::new(bits)))
        .collect();
    let violations = lattice_law_violations(&samples);
    assert!(violations.is_empty(), "{violations:#?}");
}

#[test]
fn inline_domain_satisfies_the_lattice_laws() {
    let samples = [
        InlineDomain::undecided(),
        decided(CallMode::InlineBody),
        decided(CallMode::DirectCall),
    ];
    let violations = lattice_law_violations(&samples);
    assert!(violations.is_empty(), "{violations:#?}");
}

#[test]
fn conflicting_forced_call_modes_are_unsatisfiable() {
    for (first, second) in [
        (
            CallModeForce::TokenDirective,
            CallModeForce::PrettyDirective,
        ),
        (
            CallModeForce::RecoverDirective,
            CallModeForce::SingleSiteInline,
        ),
    ] {
        let mut csp: Csp<InlineDomain> = Csp::new();
        let var = csp.add_variable(InlineDomain::undecided());
        csp.add_constraint(ForceCallMode::new(var, first));
        csp.add_constraint(ForceCallMode::new(var, second));
        let result = csp.propagate();
        assert!(
            result.is_err(),
            "forcing {first:?} then {second:?} on one rule propagated as {result:?}; \
             the domain holds {:?}",
            csp.variables[var as usize].domain.decision,
        );
    }
}

/// A small deterministic generator (xorshift64*), so the property cases
/// are reproducible without a property-testing dependency.
struct Cases(u64);

impl Cases {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    fn mode(&mut self) -> CallMode {
        if self.next() & 1 == 0 {
            CallMode::InlineBody
        } else {
            CallMode::DirectCall
        }
    }

    fn forces(&mut self) -> Vec<CallMode> {
        let len = self.below(6);
        (0..len).map(|_| self.mode()).collect()
    }

    fn shuffle<T>(&mut self, items: &mut [T]) {
        for i in (1..items.len()).rev() {
            let j = self.below(i + 1);
            items.swap(i, j);
        }
    }
}

/// The value the forces reach when joined into ⊥ in the given order.
fn fold_forces(forces: &[CallMode]) -> InlineDomain {
    let mut value = InlineDomain::bottom();
    for &mode in forces {
        value.join(&decided(mode));
    }
    value
}

/// `InlineDomain::join` is a lattice join, read over 10⁴ generated
/// cases (LW2.COMP-2's law falsifier). Each case draws a multiset of
/// forced call modes and checks that
///
/// - every order of joining them reaches the same value (the fold is
///   confluent, which is what lets propagation order not matter);
/// - the value is decided exactly when the forces agree, and is the
///   conflict element (neither undecided nor decided) exactly when two
///   forces disagree;
/// - the join laws (idempotent, identity, commutative, associative,
///   extensive, change flag, monotone) hold over a triple of values
///   reached by three such multisets.
#[test]
fn inline_domain_join_is_a_lattice_join_over_generated_cases() {
    const CASES: usize = 10_000;
    let mut cases = Cases(0x9e37_79b9_7f4a_7c15);
    let mut checked = 0usize;
    let mut failures: Vec<String> = Vec::new();

    for case in 0..CASES {
        let forces = cases.forces();
        let reference = fold_forces(&forces);
        for _ in 0..4 {
            let mut order = forces.clone();
            cases.shuffle(&mut order);
            let other = fold_forces(&order);
            if other != reference {
                failures.push(format!(
                    "case {case}: {forces:?} reaches {reference:?}, its reordering {order:?} \
                     reaches {other:?}"
                ));
            }
        }

        let agree = forces.windows(2).all(|w| w[0] == w[1]);
        let is_decided_or_bottom = matches!(
            reference.decision,
            InlineDecision::Undecided | InlineDecision::Decided(_)
        );
        if agree != is_decided_or_bottom {
            failures.push(format!(
                "case {case}: forces {forces:?} (agree: {agree}) reach {reference:?}"
            ));
        }

        let triple = [
            reference,
            fold_forces(&cases.forces()),
            fold_forces(&cases.forces()),
        ];
        for violation in lattice_law_violations(&triple) {
            failures.push(format!("case {case}: {violation:?}"));
        }
        let [a, b, c] = &triple;
        if joined(a, b).0 == *b && joined(&joined(a, c).0, &joined(b, c).0).0 != joined(b, c).0 {
            failures.push(format!(
                "case {case}: monotone: a ⊑ b must give a ⊔ c ⊑ b ⊔ c for {triple:?}"
            ));
        }
        checked += 1;
    }

    assert!(checked >= CASES, "only {checked} cases ran");
    failures.dedup();
    assert!(
        failures.is_empty(),
        "{} of {CASES} cases break a law; first: {:#?}",
        failures.len(),
        &failures[..failures.len().min(5)]
    );
}
