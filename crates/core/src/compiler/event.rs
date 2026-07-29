use serde::{Deserialize, Serialize};

use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::lower::LoweringPlan;
use super::source::Revision;
use super::syntax::{PlanNodeId, Program, RuleId};

pub const EVENT_SCHEMA: &str = "bbnf.scalar-events.v1";

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum EventKind {
    EnterRule,
    LeaveRule,
    Literal,
    Regex,
    ChoiceBranch,
    RepeatCount,
    EndInput,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct EventSite {
    kind: EventKind,
    node: Option<PlanNodeId>,
    rule: Option<RuleId>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ScalarEventPlan {
    revision: Revision,
    schema: String,
    lowering: LoweringPlan,
    sites: Vec<EventSite>,
}

impl ScalarEventPlan {
    pub(crate) fn seal(program: &Program, lowering: &LoweringPlan) -> StageResult<Self> {
        if program.revision() != lowering.revision() {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::RevisionMismatch,
                0,
                0,
                "event plan inputs carry different revisions",
            ));
        }

        let mut sites = Vec::new();
        for rule in lowering.rules() {
            sites.push(EventSite {
                kind: EventKind::EnterRule,
                node: None,
                rule: Some(rule.id()),
            });
            sites.push(EventSite {
                kind: EventKind::LeaveRule,
                node: None,
                rule: Some(rule.id()),
            });
        }
        for node in lowering.nodes() {
            let kind = match node.op() {
                super::lower::PlanOp::Literal(_) => Some(EventKind::Literal),
                super::lower::PlanOp::Regex(_) => Some(EventKind::Regex),
                super::lower::PlanOp::Choice(_) => Some(EventKind::ChoiceBranch),
                super::lower::PlanOp::Repeat { .. } => Some(EventKind::RepeatCount),
                _ => None,
            };
            if let Some(kind) = kind {
                sites.push(EventSite {
                    kind,
                    node: Some(node.id()),
                    rule: None,
                });
            }
        }
        sites.push(EventSite {
            kind: EventKind::EndInput,
            node: None,
            rule: None,
        });

        StageResult::accepted(Self {
            revision: program.revision(),
            schema: EVENT_SCHEMA.to_owned(),
            lowering: lowering.clone(),
            sites,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn schema(&self) -> &str {
        &self.schema
    }

    pub const fn lowering(&self) -> &LoweringPlan {
        &self.lowering
    }

    pub fn sites(
        &self,
    ) -> impl ExactSizeIterator<Item = (EventKind, Option<PlanNodeId>, Option<RuleId>)> + '_ {
        self.sites
            .iter()
            .map(|site| (site.kind, site.node, site.rule))
    }
}
