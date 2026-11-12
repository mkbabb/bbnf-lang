//! Independent oracles (compiler design C1-7; LW3-ORDER COMP-3a, COMP-5b).
//!
//! An oracle judges an arm's outcome for one case: it agrees, it diverges
//! (with the reason), or it has nothing to say about that case. An oracle is
//! independent when it shares no front end with the spine: the TS 0.3.0
//! package, JSONTestSuite's verdicts, serde_json, the value.js leaves' V8/JSC
//! answers, toml-test, graphql-js, lexpr, and peggy reading a hand
//! transcription of the EBNF, BNF and Math grammars of record. Root `backend/rust` is an arm,
//! never counted here.

pub mod graphql;
pub mod json;
pub mod leaves;
pub mod peg;
pub mod r7rs;
pub mod toml;
pub mod ts;

use crate::arm::{Case, Outcome};
use crate::registry::Grammar;

#[derive(Clone, Debug, PartialEq)]
pub enum Judgement {
    Agree,
    Diverge(String),
    /// The oracle has no authority over this case (an `i_` file, a form
    /// outside the oracle's reader, an arm with no typed value).
    NotApplicable,
}

pub trait Oracle {
    fn name(&self) -> &'static str;
    /// Whether this oracle has authority over `grammar` at all.
    fn applies(&self, grammar: &Grammar) -> bool;
    /// Called once per grammar before judging (a JS oracle runs its batch here).
    /// Returns false when the oracle turns out to have nothing for this grammar
    /// (the TS package refuses the grammar), so it is not counted.
    fn prepare(&mut self, _grammar: &Grammar, _cases: &[Case]) -> anyhow::Result<bool> {
        Ok(true)
    }
    fn judge(&self, grammar: &Grammar, index: usize, case: &Case, out: &Outcome) -> Judgement;
    /// The verdict this oracle expects for a case, when it has one (used by
    /// the harness's self-check).
    fn expected_verdict(&self, _grammar: &Grammar, _index: usize, _case: &Case) -> Option<bool> {
        None
    }
}

/// The verdict half every verdict oracle shares.
pub fn verdict_judgement(expect_accept: bool, out: &Outcome) -> Judgement {
    match (expect_accept, out.accepted()) {
        (_, None) => Judgement::Diverge(format!("arm reported {}", out.verdict())),
        (true, Some(true)) | (false, Some(false)) => Judgement::Agree,
        (true, Some(false)) => Judgement::Diverge("expected accept, arm rejected".into()),
        (false, Some(true)) => Judgement::Diverge("expected reject, arm accepted".into()),
    }
}

/// Every oracle the harness knows, fresh.
pub fn all() -> Vec<Box<dyn Oracle>> {
    vec![
        Box::new(json::JsonTestSuite),
        Box::new(json::SerdeJson),
        Box::new(ts::TsPackageOracle::default()),
        Box::new(ts::TsFrontend::default()),
        Box::new(leaves::ValueJsLeavesMarker),
        Box::new(toml::TomlTest),
        Box::new(graphql::GraphqlJs::default()),
        Box::new(r7rs::Lexpr),
        Box::new(peg::Peggy::default()),
    ]
}
