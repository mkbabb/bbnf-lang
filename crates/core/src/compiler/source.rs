use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};
use std::fmt::Write as _;

use super::syntax::{NodeIx, ProjectionKeep, RuleIx, TransformIx};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Revision([u8; 32]);

impl Revision {
    pub(super) const fn from_bytes(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    pub const fn bytes(self) -> [u8; 32] {
        self.0
    }

    pub fn hex(self) -> String {
        let mut output = String::with_capacity(64);
        for byte in self.0 {
            write!(output, "{byte:02x}").expect("writing to String is infallible");
        }
        output
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct SourceSpan {
    start: u32,
    end: u32,
}

impl SourceSpan {
    pub(crate) fn checked(start: usize, end: usize) -> Option<Self> {
        Some(Self {
            start: u32::try_from(start).ok()?,
            end: u32::try_from(end).ok()?,
        })
    }

    pub const fn start(self) -> u32 {
        self.start
    }

    pub const fn end(self) -> u32 {
        self.end
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum SourceRecordSlotData {
    Field {
        name: String,
        child: NodeIx,
        name_span: SourceSpan,
        span: SourceSpan,
    },
    Discard {
        child: NodeIx,
        span: SourceSpan,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum SourceNodeKindData {
    Epsilon,
    Literal(Vec<u8>),
    Regex(String),
    Reference(String),
    Sequence(Vec<NodeIx>),
    Choice(Vec<NodeIx>),
    Repeat {
        child: NodeIx,
        minimum: u32,
        maximum: Option<u32>,
    },
    Transform {
        child: NodeIx,
        transform: TransformIx,
        name: String,
    },
    Projection {
        left: NodeIx,
        right: NodeIx,
        keep: ProjectionKeep,
    },
    Difference {
        include: NodeIx,
        exclude: NodeIx,
    },
    Trim {
        child: NodeIx,
    },
    Record(Vec<SourceRecordSlotData>),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct SourceNodeData {
    pub(crate) kind: SourceNodeKindData,
    pub(crate) span: SourceSpan,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct SourceRuleData {
    pub(crate) id: RuleIx,
    pub(crate) name: String,
    pub(crate) root: NodeIx,
    pub(crate) span: SourceSpan,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub(crate) enum SourcePresentationHintData {
    Group,
    Block,
    Indent,
    Dedent,
    Softline,
    Hardline,
    Blankline,
    Compact,
    Sep { literal: Vec<u8> },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct SourcePresentationData {
    pub(crate) target: String,
    pub(crate) hints: Vec<SourcePresentationHintData>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceModule {
    revision: Revision,
    pub(crate) nodes: Vec<SourceNodeData>,
    pub(crate) rules: Vec<SourceRuleData>,
}

impl SourceModule {
    pub(crate) fn new(
        revision: Revision,
        nodes: Vec<SourceNodeData>,
        rules: Vec<SourceRuleData>,
    ) -> Self {
        Self {
            revision,
            nodes,
            rules,
        }
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn transform_count(&self) -> usize {
        self.nodes
            .iter()
            .filter(|node| matches!(node.kind, SourceNodeKindData::Transform { .. }))
            .count()
    }

    pub(crate) fn uses_trivia(&self) -> bool {
        self.nodes
            .iter()
            .any(|node| matches!(node.kind, SourceNodeKindData::Trim { .. }))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceMap {
    revision: Revision,
    modules: Vec<SourceModule>,
    presentation: Vec<Vec<SourcePresentationData>>,
}

impl SourceMap {
    pub(crate) fn one(module: SourceModule, presentation: Vec<SourcePresentationData>) -> Self {
        Self {
            revision: module.revision(),
            modules: vec![module],
            presentation: vec![presentation],
        }
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn modules(&self) -> &[SourceModule] {
        &self.modules
    }

    pub(super) fn replay_value(&self) -> SourceMapReplay<'_> {
        SourceMapReplay {
            revision: self.revision,
            modules: SourceModulesReplay {
                modules: &self.modules,
                presentation: &self.presentation,
            },
        }
    }
}

#[derive(Serialize)]
pub(super) struct SourceMapReplay<'a> {
    revision: Revision,
    modules: SourceModulesReplay<'a>,
}

struct SourceModulesReplay<'a> {
    modules: &'a [SourceModule],
    presentation: &'a [Vec<SourcePresentationData>],
}

impl Serialize for SourceModulesReplay<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Module<'a> {
            revision: Revision,
            nodes: &'a [SourceNodeData],
            rules: &'a [SourceRuleData],
            presentation: &'a [SourcePresentationData],
        }

        debug_assert_eq!(self.modules.len(), self.presentation.len());
        let mut sequence = serializer.serialize_seq(Some(self.modules.len()))?;
        for (module, presentation) in self.modules.iter().zip(self.presentation) {
            sequence.serialize_element(&Module {
                revision: module.revision,
                nodes: &module.nodes,
                rules: &module.rules,
                presentation,
            })?;
        }
        sequence.end()
    }
}

#[cfg(test)]
mod representation_laws {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn source_rows_keep_local_ordinals_and_pre_branding_widths() {
        assert!(size_of::<SourceNodeData>() <= 48);
        assert!(size_of::<SourceRuleData>() <= 40);
        assert!(size_of::<SourceRecordSlotData>() <= 48);
    }
}
