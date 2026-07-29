use serde::Serialize;

use super::event::ScalarEventPlan;
use super::lower::LoweringPlan;

const LOWERING_REPLAY_SCHEMA: &str = "bbnf.lowering-plan.v1";

#[derive(Serialize)]
struct ReplayEnvelope<'a, T> {
    schema: &'a str,
    revision: String,
    value: &'a T,
}

impl LoweringPlan {
    /// Serialize this sealed lowering product with an explicit schema and
    /// revision. Field and collection order are deterministic.
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serde_json::to_vec(&ReplayEnvelope {
            schema: LOWERING_REPLAY_SCHEMA,
            revision: self.revision().hex(),
            value: self,
        })
    }
}

impl ScalarEventPlan {
    /// Serialize this sealed event product with its event schema and revision.
    /// The embedded lowering plan makes the replay record self-contained.
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serde_json::to_vec(&ReplayEnvelope {
            schema: self.schema(),
            revision: self.revision().hex(),
            value: self,
        })
    }
}
