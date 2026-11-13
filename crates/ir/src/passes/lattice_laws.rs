//! The join-semilattice law oracle every `LatticeDomain` of the IR is
//! read against (sk-v27 LW3.COMP-6; first written for `InlineDomain` by
//! LW1.COMP-3).
//!
//! `LatticeDomain::join` must be the least upper bound: commutative,
//! associative, idempotent, extensive and monotone, with `bottom()` as
//! identity, and it must report `true` exactly when the value changed.
//! The oracle is generic, so a violation is a defect of the domain, and
//! `BitsetLatticeDomain` (a set union) is run as its control.

use csp_solver::domain::LatticeDomain;

/// One law a sample set breaks, with the operands that break it.
#[derive(Debug)]
pub(crate) struct LawViolation<D> {
    pub(crate) law: &'static str,
    pub(crate) operands: Vec<D>,
}

pub(crate) fn joined<D: LatticeDomain>(a: &D, b: &D) -> (D, bool) {
    let mut out = a.clone();
    let changed = out.join(b);
    (out, changed)
}

/// Every join-semilattice law, checked over all pairs and triples of
/// `samples`.
pub(crate) fn law_violations<D: LatticeDomain>(samples: &[D]) -> Vec<LawViolation<D>> {
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
                let left = joined(&ab, c).0;
                let right = joined(a, &joined(b, c).0).0;
                if left != right {
                    fail("associative: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)", &[a, b, c]);
                }
                if ab == *b && joined(&joined(a, c).0, &joined(b, c).0).0 != joined(b, c).0 {
                    fail("monotone: a ⊑ b gives a ⊔ c ⊑ b ⊔ c", &[a, b, c]);
                }
            }
        }
    }
    out
}

/// A small deterministic generator (xorshift64*), so the cases are
/// reproducible without a property-testing dependency.
pub(crate) struct Rng(pub(crate) u64);

impl Rng {
    pub(crate) fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    pub(crate) fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

/// The number of generated cases COMP-6 reads each domain at.
pub(crate) const CASES: usize = 100_000;

/// Read the laws over `CASES` generated triples. Each case draws three
/// values from `draw` and also joins pairs of them, so the triple holds
/// values the join itself reaches (⊤ included). Returns the distinct
/// laws broken, each with the first operands that broke it.
pub(crate) fn generated_violations<D: LatticeDomain>(
    seed: u64,
    mut draw: impl FnMut(&mut Rng) -> D,
) -> Vec<LawViolation<D>> {
    let mut rng = Rng(seed);
    let mut first: Vec<LawViolation<D>> = Vec::new();
    for _ in 0..CASES {
        let a = draw(&mut rng);
        let b = draw(&mut rng);
        let c = if rng.next() & 3 == 0 { joined(&a, &b).0 } else { draw(&mut rng) };
        for v in law_violations(&[a, b, c]) {
            if !first.iter().any(|f| f.law == v.law) {
                first.push(v);
            }
        }
    }
    first
}

/// The oracle's control: set union over bitsets is a lattice, so the
/// oracle must find nothing.
#[test]
fn bitset_lattice_domain_satisfies_the_lattice_laws() {
    use csp_solver::domain::{BitsetDomain, BitsetLatticeDomain};
    let violations = generated_violations(0x5eed_0001, |rng| {
        let bits: Vec<u32> = (0..rng.below(5)).map(|_| rng.below(12) as u32).collect();
        BitsetLatticeDomain::new(BitsetDomain::new(bits))
    });
    assert!(violations.is_empty(), "{violations:#?}");
}
