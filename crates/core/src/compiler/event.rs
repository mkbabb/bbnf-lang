use std::ops::Range;

use serde::Serialize;

use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::lower::{LoweringPlan, PlanOpData, PlanRecordSlotData};
use super::source::Revision;
use super::syntax::{
    FieldId, FieldIx, PlanNodeId, PlanNodeIx, Program, RuleId, RuleIx, TransformId, TransformIx,
};

pub const EVENT_SCHEMA: &str = "bbnf.scalar-events.v4";

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
pub(crate) struct EventSiteData {
    kind: EventKind,
    node: Option<PlanNodeIx>,
    rule: Option<RuleIx>,
    field: Option<FieldIx>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct TransformSiteData {
    id: TransformIx,
    node: PlanNodeIx,
    name: String,
}

#[derive(Clone, Copy, Debug)]
pub struct TransformSite<'a> {
    revision: &'a Revision,
    data: &'a TransformSiteData,
}

impl<'a> TransformSite<'a> {
    pub const fn id(self) -> TransformId {
        TransformId::bind(*self.revision, self.data.id)
    }

    pub const fn node(self) -> PlanNodeId {
        PlanNodeId::bind(*self.revision, self.data.node)
    }

    pub fn name(self) -> &'a str {
        &self.data.name
    }
}

#[derive(Clone, Debug)]
pub struct TransformSites<'a> {
    revision: &'a Revision,
    rows: &'a [TransformSiteData],
    remaining: Range<usize>,
}

impl<'a> TransformSites<'a> {
    fn new(revision: &'a Revision, rows: &'a [TransformSiteData]) -> Self {
        Self {
            revision,
            rows,
            remaining: 0..rows.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<TransformSite<'a>> {
        self.rows.get(index).map(|data| TransformSite {
            revision: self.revision,
            data,
        })
    }

    pub fn iter(&self) -> Self {
        Self::new(self.revision, self.rows)
    }

    pub fn is_empty(&self) -> bool {
        self.remaining.start >= self.remaining.end
    }
}

impl<'a> Iterator for TransformSites<'a> {
    type Item = TransformSite<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.remaining.next()?;
        self.get(index)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for TransformSites<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let index = self.remaining.next_back()?;
        self.get(index)
    }
}

impl ExactSizeIterator for TransformSites<'_> {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScalarEventPlan {
    revision: Revision,
    schema: String,
    sites: Vec<EventSiteData>,
    transforms: Vec<TransformSiteData>,
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
        for rule in lowering.rule_data_rows() {
            sites.push(EventSiteData {
                kind: EventKind::EnterRule,
                node: None,
                rule: Some(rule.id()),
                field: None,
            });
            sites.push(EventSiteData {
                kind: EventKind::LeaveRule,
                node: None,
                rule: Some(rule.id()),
                field: None,
            });
        }
        for node in lowering.node_data_rows() {
            let kind = match node.op() {
                PlanOpData::Literal(_) => Some(EventKind::Literal),
                PlanOpData::Regex(_) => Some(EventKind::Regex),
                PlanOpData::Choice(_) => Some(EventKind::ChoiceBranch),
                PlanOpData::Repeat { .. } => Some(EventKind::RepeatCount),
                PlanOpData::Transform {
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
                    transforms.push(TransformSiteData {
                        id: *transform,
                        node: node.id(),
                        name: name.clone(),
                    });
                    Some(EventKind::Transform)
                }
                PlanOpData::Record { owner, slots } => {
                    for slot in slots {
                        if let PlanRecordSlotData::Field { field, child } = slot {
                            sites.push(EventSiteData {
                                kind: EventKind::EnterField,
                                node: Some(*child),
                                rule: Some(*owner),
                                field: Some(*field),
                            });
                            sites.push(EventSiteData {
                                kind: EventKind::LeaveField,
                                node: Some(*child),
                                rule: Some(*owner),
                                field: Some(*field),
                            });
                        }
                    }
                    None
                }
                PlanOpData::Epsilon | PlanOpData::RuleRef(_) | PlanOpData::Sequence(_) => None,
            };
            if let Some(kind) = kind {
                sites.push(EventSiteData {
                    kind,
                    node: Some(node.id()),
                    rule: None,
                    field: None,
                });
            }
        }
        sites.push(EventSiteData {
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
        self.sites.iter().map(|site| {
            (
                site.kind,
                site.node
                    .map(|local| PlanNodeId::bind(self.revision, local)),
                site.rule.map(|local| RuleId::bind(self.revision, local)),
                site.field.map(|local| FieldId::bind(self.revision, local)),
            )
        })
    }

    pub fn transforms(&self) -> TransformSites<'_> {
        TransformSites::new(&self.revision, &self.transforms)
    }

    pub fn transform(&self, id: TransformId) -> Option<TransformSite<'_>> {
        (id.revision() == self.revision)
            .then(|| {
                self.transforms
                    .get(id.local().index())
                    .filter(|site| site.id == id.local())
                    .map(|data| TransformSite {
                        revision: &self.revision,
                        data,
                    })
            })
            .flatten()
    }

    pub(super) fn replay_value(&self) -> ScalarEventPlanReplay<'_> {
        ScalarEventPlanReplay {
            revision: self.revision,
            schema: &self.schema,
            sites: &self.sites,
            transforms: &self.transforms,
        }
    }
}

#[derive(Serialize)]
pub(super) struct ScalarEventPlanReplay<'a> {
    revision: Revision,
    schema: &'a str,
    sites: &'a [EventSiteData],
    transforms: &'a [TransformSiteData],
}

#[cfg(test)]
mod representation_laws {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn event_rows_use_only_local_ordinals() {
        assert!(size_of::<EventSiteData>() <= 32);
        assert!(size_of::<TransformSiteData>() <= 32);
        assert_eq!(size_of::<Option<PlanNodeIx>>(), 8);
    }
}
