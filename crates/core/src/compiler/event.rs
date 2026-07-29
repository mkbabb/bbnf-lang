use serde::Serialize;

use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::lower::LoweringPlan;
use super::source::Revision;
use super::syntax::{FieldId, PlanNodeId, Program, RuleId, TransformId};

pub const EVENT_SCHEMA: &str = "bbnf.scalar-events.v5";

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum EventKind {
    EnterRule,
    LeaveRule,
    EnterField,
    LeaveField,
    Literal,
    Regex,
    ChoiceBranch,
    RepeatCount,
    Transform,
    EndInput,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct EventSite {
    kind: EventKind,
    node: Option<PlanNodeId>,
    rule: Option<RuleId>,
    field: Option<FieldId>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct TransformSite {
    id: TransformId,
    node: PlanNodeId,
    name: String,
}

impl TransformSite {
    pub const fn id(&self) -> TransformId {
        self.id
    }

    pub const fn node(&self) -> PlanNodeId {
        self.node
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ScalarEventPlan {
    revision: Revision,
    schema: String,
    sites: Vec<EventSite>,
    transforms: Vec<TransformSite>,
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
        let mut transforms = Vec::new();
        for rule in lowering.rules() {
            sites.push(EventSite {
                kind: EventKind::EnterRule,
                node: None,
                rule: Some(rule.id()),
                field: None,
            });
            sites.push(EventSite {
                kind: EventKind::LeaveRule,
                node: None,
                rule: Some(rule.id()),
                field: None,
            });
        }
        for node in lowering.nodes() {
            let kind = match node.op() {
                super::lower::PlanOp::Literal(_) => Some(EventKind::Literal),
                super::lower::PlanOp::Regex(_) => Some(EventKind::Regex),
                super::lower::PlanOp::Choice(_) => Some(EventKind::ChoiceBranch),
                super::lower::PlanOp::Repeat { .. } => Some(EventKind::RepeatCount),
                super::lower::PlanOp::Transform {
                    transform, name, ..
                } => {
                    if transform.index() != transforms.len() {
                        return StageResult::rejected(Diagnostic::new(
                            DiagnosticCode::InternalInvariant,
                            0,
                            0,
                            "lowering transform identities are not contiguous lexical ordinals",
                        ));
                    }
                    transforms.push(TransformSite {
                        id: *transform,
                        node: node.id(),
                        name: name.clone(),
                    });
                    Some(EventKind::Transform)
                }
                super::lower::PlanOp::Record { owner, slots } => {
                    for slot in slots {
                        if let super::lower::PlanRecordSlot::Field { field, child } = slot {
                            sites.push(EventSite {
                                kind: EventKind::EnterField,
                                node: Some(*child),
                                rule: Some(*owner),
                                field: Some(*field),
                            });
                            sites.push(EventSite {
                                kind: EventKind::LeaveField,
                                node: Some(*child),
                                rule: Some(*owner),
                                field: Some(*field),
                            });
                        }
                    }
                    None
                }
                _ => None,
            };
            if let Some(kind) = kind {
                sites.push(EventSite {
                    kind,
                    node: Some(node.id()),
                    rule: None,
                    field: None,
                });
            }
        }
        sites.push(EventSite {
            kind: EventKind::EndInput,
            node: None,
            rule: None,
            field: None,
        });

        StageResult::accepted(Self {
            revision: program.revision(),
            schema: EVENT_SCHEMA.to_owned(),
            sites,
            transforms,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn schema(&self) -> &str {
        &self.schema
    }

    pub fn sites(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (
            EventKind,
            Option<PlanNodeId>,
            Option<RuleId>,
            Option<FieldId>,
        ),
    > + '_ {
        self.sites
            .iter()
            .map(|site| (site.kind, site.node, site.rule, site.field))
    }

    pub fn transforms(&self) -> &[TransformSite] {
        &self.transforms
    }
}
