//! Pre-built constraint dependency graph with flat arena storage.

use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;

#[derive(Debug, Clone)]
struct ArenaIndex {
    entries: Vec<(u32, u32)>,
}

impl ArenaIndex {
    fn new(count: usize) -> Self {
        Self {
            entries: vec![(0, 0); count],
        }
    }

    fn get<'a>(&self, idx: usize, pool: &'a [u32]) -> &'a [u32] {
        let (offset, len) = self.entries[idx];
        &pool[offset as usize..(offset + len) as usize]
    }
}

#[derive(Debug, Clone)]
pub struct Adjacency {
    pool: Vec<u32>,
    var_constraints: ArenaIndex,
    constraint_neighbors: ArenaIndex,
    var_neighbors: ArenaIndex,
}

impl Adjacency {
    pub fn build<D: Domain>(num_vars: usize, constraints: &[ConstraintEnum<D>]) -> Self
    where
        D::Value: PartialEq,
    {
        let num_constraints = constraints.len();

        let mut vc_lists: Vec<Vec<u32>> = vec![Vec::new(); num_vars];
        for (ci, c) in constraints.iter().enumerate() {
            for &v in c.scope() {
                vc_lists[v as usize].push(ci as u32);
            }
        }

        let mut cn_lists: Vec<Vec<u32>> = vec![Vec::new(); num_constraints];
        for vc in &vc_lists {
            for &ci in vc {
                for &cj in vc {
                    if ci != cj {
                        cn_lists[ci as usize].push(cj);
                    }
                }
            }
        }
        for neighbors in &mut cn_lists {
            neighbors.sort_unstable();
            neighbors.dedup();
        }

        let mut vn_lists: Vec<Vec<u32>> = vec![Vec::new(); num_vars];
        for c in constraints {
            let scope = c.scope();
            for &vi in scope {
                for &vj in scope {
                    if vi != vj {
                        vn_lists[vi as usize].push(vj);
                    }
                }
            }
        }
        for neighbors in &mut vn_lists {
            neighbors.sort_unstable();
            neighbors.dedup();
        }

        let total_len: usize = vc_lists.iter().map(|v| v.len()).sum::<usize>()
            + cn_lists.iter().map(|v| v.len()).sum::<usize>()
            + vn_lists.iter().map(|v| v.len()).sum::<usize>();
        let mut pool = Vec::with_capacity(total_len);

        let mut var_constraints = ArenaIndex::new(num_vars);
        for (i, list) in vc_lists.iter().enumerate() {
            let offset = pool.len() as u32;
            pool.extend_from_slice(list);
            var_constraints.entries[i] = (offset, list.len() as u32);
        }

        let mut constraint_neighbors = ArenaIndex::new(num_constraints);
        for (i, list) in cn_lists.iter().enumerate() {
            let offset = pool.len() as u32;
            pool.extend_from_slice(list);
            constraint_neighbors.entries[i] = (offset, list.len() as u32);
        }

        let mut var_neighbors = ArenaIndex::new(num_vars);
        for (i, list) in vn_lists.iter().enumerate() {
            let offset = pool.len() as u32;
            pool.extend_from_slice(list);
            var_neighbors.entries[i] = (offset, list.len() as u32);
        }

        Self {
            pool,
            var_constraints,
            constraint_neighbors,
            var_neighbors,
        }
    }

    pub fn constraints_for(&self, var: VarId) -> &[u32] {
        self.var_constraints.get(var as usize, &self.pool)
    }

    pub fn neighbors_of_constraint(&self, ci: usize) -> &[u32] {
        self.constraint_neighbors.get(ci, &self.pool)
    }

    pub fn neighbors_of_var(&self, var: VarId) -> &[u32] {
        self.var_neighbors.get(var as usize, &self.pool)
    }
}
