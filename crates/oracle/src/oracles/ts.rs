//! The TS 0.3.0 package as an oracle (compiler design C1-7: "the TS 0.3.0
//! package, for accept/reject and tree").
//!
//! - `ts-package`: the package runs the grammar itself; another arm must give
//!   the same verdict on every case. Trees are not compared across a root arm,
//!   whose labels are typed kinds rather than rule names: G1 compares trees
//!   between rule-labelled arms (the spine, the interpreter, this package)
//!   under each rewrite's class.
//! - `ts-frontend`: for the `bbnf` grammar, whose inputs are grammar texts,
//!   the package's own front end (`BBNFToASTWithImports`) judges each input.

use std::cell::RefCell;

use super::{Judgement, Oracle, verdict_judgement};
use crate::arm::{Arm, Case, Outcome};
use crate::arms::ts_package::TsPackage;
use crate::registry::Grammar;

#[derive(Default)]
pub struct TsPackageOracle {
    outcomes: RefCell<Vec<Outcome>>,
}

impl Oracle for TsPackageOracle {
    fn name(&self) -> &'static str {
        "ts-package"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id != "bbnf"
    }

    fn prepare(&mut self, g: &Grammar, cases: &[Case]) -> anyhow::Result<bool> {
        let outs = TsPackage::default().run(g, cases)?;
        let refused = outs.iter().all(|o| matches!(o, Outcome::GrammarRefused(_)));
        *self.outcomes.borrow_mut() = outs;
        Ok(!refused && !cases.is_empty())
    }

    fn expected_verdict(&self, _g: &Grammar, i: usize, _case: &Case) -> Option<bool> {
        self.outcomes.borrow().get(i).and_then(Outcome::accepted)
    }

    fn judge(&self, _g: &Grammar, i: usize, _case: &Case, out: &Outcome) -> Judgement {
        match self.outcomes.borrow().get(i).and_then(Outcome::accepted) {
            Some(expect) => verdict_judgement(expect, out),
            None => Judgement::NotApplicable,
        }
    }
}

#[derive(Default)]
pub struct TsFrontend {
    verdicts: Vec<Option<bool>>,
}

impl Oracle for TsFrontend {
    fn name(&self) -> &'static str {
        "ts-frontend"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id == "bbnf"
    }

    fn prepare(&mut self, g: &Grammar, cases: &[Case]) -> anyhow::Result<bool> {
        let lines = TsPackage::default().request("frontend", g, cases)?;
        self.verdicts = lines
            .iter()
            .map(|l| match l["verdict"].as_str() {
                Some("accept") => Some(true),
                Some("reject") | Some("unreadable") => Some(false),
                _ => None,
            })
            .collect();
        Ok(!cases.is_empty())
    }

    fn expected_verdict(&self, _g: &Grammar, i: usize, _case: &Case) -> Option<bool> {
        self.verdicts.get(i).copied().flatten()
    }

    fn judge(&self, _g: &Grammar, i: usize, _case: &Case, out: &Outcome) -> Judgement {
        match self.verdicts.get(i).copied().flatten() {
            Some(expect) => verdict_judgement(expect, out),
            None => Judgement::NotApplicable,
        }
    }
}
