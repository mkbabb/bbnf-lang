use blake3::Hasher;
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};
use std::fmt::Write as _;

use super::syntax::{NodeIx, RuleIx, TransformIx};

const REVISION_SCHEMA: &[u8] = b"bbnf.w2.source.v4\0";

#[derive(Clone, Copy)]
pub struct ModuleInput<'a> {
    logical_path: &'a str,
    bytes: &'a [u8],
}

impl<'a> ModuleInput<'a> {
    pub const fn new(logical_path: &'a str, bytes: &'a [u8]) -> Self {
        Self {
            logical_path,
            bytes,
        }
    }

    pub const fn logical_path(self) -> &'a str {
        self.logical_path
    }

    pub const fn bytes(self) -> &'a [u8] {
        self.bytes
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Revision([u8; 32]);

impl Revision {
    pub(crate) fn for_module(logical_path: &str, bytes: &[u8]) -> Self {
        let mut hasher = Hasher::new();
        hasher.update(REVISION_SCHEMA);
        hasher.update(&(logical_path.len() as u64).to_le_bytes());
        hasher.update(logical_path.as_bytes());
        hasher.update(&(bytes.len() as u64).to_le_bytes());
        hasher.update(bytes);
        Self(*hasher.finalize().as_bytes())
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceModule {
    revision: Revision,
    logical_path: String,
    bytes: Vec<u8>,
    pub(crate) nodes: Vec<SourceNodeData>,
    pub(crate) rules: Vec<SourceRuleData>,
}

impl SourceModule {
    pub(crate) fn new(
        revision: Revision,
        logical_path: String,
        bytes: Vec<u8>,
        nodes: Vec<SourceNodeData>,
        rules: Vec<SourceRuleData>,
    ) -> Self {
        debug_assert_eq!(revision, Revision::for_module(&logical_path, &bytes));
        Self {
            revision,
            logical_path,
            bytes,
            nodes,
            rules,
        }
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn logical_path(&self) -> &str {
        &self.logical_path
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
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
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceMap {
    revision: Revision,
    modules: Vec<SourceModule>,
}

impl SourceMap {
    pub(crate) fn one(module: SourceModule) -> Self {
        Self {
            revision: module.revision(),
            modules: vec![module],
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
            modules: SourceModulesReplay(&self.modules),
        }
    }
}

#[derive(Serialize)]
pub(super) struct SourceMapReplay<'a> {
    revision: Revision,
    modules: SourceModulesReplay<'a>,
}

struct SourceModulesReplay<'a>(&'a [SourceModule]);

impl Serialize for SourceModulesReplay<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Module<'a> {
            revision: Revision,
            logical_path: &'a str,
            bytes: &'a [u8],
            nodes: &'a [SourceNodeData],
            rules: &'a [SourceRuleData],
        }

        let mut sequence = serializer.serialize_seq(Some(self.0.len()))?;
        for module in self.0 {
            sequence.serialize_element(&Module {
                revision: module.revision,
                logical_path: &module.logical_path,
                bytes: &module.bytes,
                nodes: &module.nodes,
                rules: &module.rules,
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
