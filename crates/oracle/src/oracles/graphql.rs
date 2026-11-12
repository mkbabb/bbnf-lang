//! GraphQL's oracle: graphql-js 16.11.0 (see ts/pin.json for why this release), pinned by
//! sha256 in `ts/pin.json`, run by `heldout/graphql-js.mjs`.
//!
//! The verdicts must be equal; where both accept, the arm's scalar events
//! (`obs(ScalarEvents, tree)`: its non-trivia leaf texts in order) must equal
//! graphql-js's token stream, each token as the source text it spans.

use super::{Judgement, Oracle};
use crate::arm::{Case, Outcome};
use crate::arms::ts_package::run_node;
use crate::obs::{Obs, ObsClass, obs};
use crate::registry::{Grammar, repo_root};

#[derive(Default)]
pub struct GraphqlJs {
    answers: Vec<serde_json::Value>,
}

pub fn run(cases: &[Case]) -> anyhow::Result<Vec<serde_json::Value>> {
    let pkg = crate::npm::ensure("graphql-js")?;
    let req = serde_json::json!({
        "package": pkg,
        "inputs": cases.iter().map(|c| c.path.clone()).collect::<Vec<_>>(),
    });
    let lines = run_node(
        &repo_root().join("crates/oracle/heldout/graphql-js.mjs"),
        &req,
    )?;
    anyhow::ensure!(
        lines.len() == cases.len(),
        "graphql-js returned {} lines for {} cases",
        lines.len(),
        cases.len()
    );
    Ok(lines)
}

impl Oracle for GraphqlJs {
    fn name(&self) -> &'static str {
        "graphql-js"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id == "graphql"
    }

    fn prepare(&mut self, _g: &Grammar, cases: &[Case]) -> anyhow::Result<bool> {
        self.answers = run(cases)?;
        Ok(!cases.is_empty())
    }

    fn expected_verdict(&self, _g: &Grammar, i: usize, _case: &Case) -> Option<bool> {
        self.answers.get(i).map(|a| a["verdict"] == "accept")
    }

    fn judge(&self, _g: &Grammar, i: usize, _case: &Case, out: &Outcome) -> Judgement {
        let a = &self.answers[i];
        let accept = a["verdict"] == "accept";
        match (accept, out) {
            (_, Outcome::GrammarRefused(m)) => {
                Judgement::Diverge(format!("arm refused the grammar: {m}"))
            }
            (false, Outcome::Reject(_)) => Judgement::Agree,
            (false, _) => Judgement::Diverge(format!(
                "graphql-js rejects ({}), arm accepted",
                a["message"]
            )),
            (true, Outcome::Reject(_)) | (true, Outcome::Crash(_)) => {
                Judgement::Diverge("graphql-js accepts, arm rejected".into())
            }
            (true, Outcome::Accept { obs: tree, .. }) => {
                let events: Vec<String> = match obs(ObsClass::ScalarEvents, tree) {
                    Obs::Node { children, .. } => children
                        .into_iter()
                        .map(|c| match c {
                            Obs::Leaf { text, .. } => text,
                            Obs::Node { .. } => String::new(),
                        })
                        .collect(),
                    Obs::Leaf { text, .. } => vec![text],
                };
                let want: Vec<String> = a["tokens"]
                    .as_array()
                    .map(|v| {
                        v.iter()
                            .filter_map(|t| t.as_str().map(str::to_owned))
                            .collect()
                    })
                    .unwrap_or_default();
                if events == want {
                    Judgement::Agree
                } else {
                    let at = events
                        .iter()
                        .zip(&want)
                        .position(|(x, y)| x != y)
                        .unwrap_or(events.len().min(want.len()));
                    Judgement::Diverge(format!(
                        "scalar events differ at {at}: arm {:?} graphql-js {:?}",
                        events.get(at),
                        want.get(at)
                    ))
                }
            }
        }
    }
}
