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
//! The born-RED tests are `#[ignore]`d so the suite stays green until
//! LW2.COMP-2 cures them (`InlineDomain` gains a ⊤ for a
//! Decided/Decided conflict, and the six `let _ = csp.propagate()`
//! sites route `Unsatisfiable` to a diagnostic). Run them with
//! `cargo test -p bbnf --lib csp_contract -- --ignored`.

use csp_solver::Csp;
use csp_solver::domain::{BitsetDomain, BitsetLatticeDomain, LatticeDomain};

use super::constraints::{ForceCallMode, InlineDecision, InlineDomain};
use super::plan::CallMode;

/// One law the sample set breaks, with the operands that break it.
#[derive(Debug)]
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
                fail("change flag: join reports true iff the value changed", &[a, b]);
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
#[ignore = "born-RED sk-v27 LW1.COMP-3: InlineDomain::join is first-writer-wins; cured by LW2.COMP-2"]
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
#[ignore = "born-RED sk-v27 LW1.COMP-3: a Decided/Decided conflict propagates as Ok; cured by LW2.COMP-2"]
fn conflicting_forced_call_modes_are_unsatisfiable() {
    for (first, second) in [
        (CallMode::InlineBody, CallMode::DirectCall),
        (CallMode::DirectCall, CallMode::InlineBody),
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
