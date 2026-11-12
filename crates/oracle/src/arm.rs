//! Arms: implementations under test or used as oracles, each reporting an
//! [`Outcome`] per input (sk-v27 LW3.COMP-3a).

use std::path::PathBuf;

use crate::obs::{FailureContract, Obs};

/// One input of a grammar's corpus.
#[derive(Clone, Debug)]
pub struct Case {
    /// Stable id: the corpus-relative path (or `in-tree:<repo path>`).
    pub id: String,
    pub path: PathBuf,
}

impl Case {
    pub fn bytes(&self) -> Vec<u8> {
        std::fs::read(&self.path).unwrap_or_else(|e| {
            panic!(
                "case {} unreadable at {}: {e}",
                self.id,
                self.path.display()
            )
        })
    }

    /// The case's file name (for JSONTestSuite / toml-test prefixes).
    pub fn file_name(&self) -> &str {
        self.id.rsplit('/').next().unwrap_or(&self.id)
    }
}

/// What an arm reports for one input.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "verdict", content = "data", rename_all = "kebab-case")]
pub enum Outcome {
    /// Accepted: the observation tree and, when the arm has one, its typed
    /// projection into the JSON data model (root's JSON document).
    Accept {
        obs: Obs,
        value: Option<serde_json::Value>,
    },
    /// Rejected, with the failure contract. Input that is not UTF-8 is
    /// rejected at the arm's boundary at the first invalid byte.
    Reject(FailureContract),
    /// The arm cannot read the grammar at all.
    GrammarRefused(String),
    /// The arm panicked, aborted or exceeded its resource bound on this
    /// input (a defect, never a verdict).
    Crash(String),
}

impl Outcome {
    pub fn accepted(&self) -> Option<bool> {
        match self {
            Outcome::Accept { .. } => Some(true),
            Outcome::Reject(_) => Some(false),
            Outcome::GrammarRefused(_) | Outcome::Crash(_) => None,
        }
    }

    pub fn verdict(&self) -> &'static str {
        match self {
            Outcome::Accept { .. } => "accept",
            Outcome::Reject(_) => "reject",
            Outcome::GrammarRefused(_) => "grammar-refused",
            Outcome::Crash(_) => "crash",
        }
    }

    /// The rejection a non-UTF-8 input gets at an arm's `&str` boundary.
    pub fn not_utf8(bytes: &[u8]) -> Outcome {
        let at = std::str::from_utf8(bytes)
            .err()
            .map(|e| e.valid_up_to())
            .unwrap_or(0);
        Outcome::Reject(
            FailureContract::new(at as u32, vec!["UTF-8".to_string()])
                .expect("one label is sorted"),
        )
    }
}

/// An arm runs a grammar over a batch of cases.
pub trait Arm {
    fn name(&self) -> &'static str;
    /// Whether this arm has an implementation of `grammar` at all.
    fn supports(&self, grammar: &crate::registry::Grammar) -> bool;
    /// One outcome per case, in order.
    fn run(
        &mut self,
        grammar: &crate::registry::Grammar,
        cases: &[Case],
    ) -> anyhow::Result<Vec<Outcome>>;
}
