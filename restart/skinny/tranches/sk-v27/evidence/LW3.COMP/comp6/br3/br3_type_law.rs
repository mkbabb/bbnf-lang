//! sk-v27 LW3.COMP-6-BR3 (RULINGS 2026-11-13b COMP6-T): `TypeDomain::join` is a lattice join.
//! A named born-RED gate kept out of the merged suite: `br3.sh` places this file at
//! crates/ir/tests/br3_type_law.rs for one run and removes it. RED today (the join overwrites
//! a decided type, so it is not commutative and the order it induces is not antisymmetric).
//! Cure unit LW5.COMP-1 "TypeDomain on its real refinement order"; the record is
//! type-cure-held.diff and type-cure-panic-entries.txt (the flat-lattice cure changes 21 of 70
//! MANIFEST entries) and refinement-order.tsv beside this file.
use bbnf_ir::TypeDesc;
use bbnf_ir::passes::types::constraint::TypeDomain;
use csp_solver::domain::LatticeDomain;

struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

fn ty(rng: &mut Rng, depth: usize) -> TypeDesc {
    let leaf = [TypeDesc::Span, TypeDesc::F64, TypeDesc::Bool, TypeDesc::U32, TypeDesc::BoxedEnum, TypeDesc::Enum];
    if depth == 0 {
        return leaf[rng.below(leaf.len())].clone();
    }
    match rng.below(5) {
        0 => TypeDesc::Option(Box::new(ty(rng, depth - 1))),
        1 => TypeDesc::Vec(Box::new(ty(rng, depth - 1))),
        2 => TypeDesc::Tuple(vec![ty(rng, depth - 1), ty(rng, depth - 1)]),
        _ => leaf[rng.below(leaf.len())].clone(),
    }
}

fn join(a: &TypeDomain, b: &TypeDomain) -> TypeDomain {
    let mut out = a.clone();
    out.join(b);
    out
}

#[test]
fn br3_type_domain_join_is_commutative_and_antisymmetric() {
    let mut rng = Rng(0x5eed_7e9e);
    let draw = |rng: &mut Rng| if rng.below(4) == 0 { TypeDomain::unsolved() } else { TypeDomain::ground(ty(rng, 2)) };
    let (mut commutative, mut antisymmetric) = (0usize, 0usize);
    for _ in 0..100_000 {
        let (a, b) = (draw(&mut rng), draw(&mut rng));
        let (ab, ba) = (join(&a, &b), join(&b, &a));
        if ab != ba {
            commutative += 1;
        }
        // a ⊑ b and b ⊑ a must give a = b.
        if ab == b && ba == a && a != b {
            antisymmetric += 1;
        }
    }
    assert!(
        commutative == 0 && antisymmetric == 0,
        "of 100000 cases: commutative broken {commutative}, antisymmetric broken {antisymmetric}"
    );
}
