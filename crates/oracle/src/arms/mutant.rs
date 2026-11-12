//! The harness's own falsifier: an arm that returns a constant. The
//! self-check must refuse it (LW3.COMP-3a's mutant). Built only with the
//! `mutants` feature, so no production `g1` can select it.

use crate::arm::{Arm, Case, Outcome};
use crate::obs::Obs;
use crate::registry::Grammar;

pub struct Constant;

impl Arm for Constant {
    fn name(&self) -> &'static str {
        "mutant-const"
    }

    fn supports(&self, _g: &Grammar) -> bool {
        true
    }

    fn run(&mut self, _g: &Grammar, cases: &[Case]) -> anyhow::Result<Vec<Outcome>> {
        Ok(cases
            .iter()
            .map(|_| Outcome::Accept {
                obs: Obs::leaf(Some("const"), None, "c"),
                value: None,
            })
            .collect())
    }
}
