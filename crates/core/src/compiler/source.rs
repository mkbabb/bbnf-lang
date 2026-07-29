use blake3::Hasher;
use serde::{Deserialize, Serialize};
use std::fmt::Write as _;

use super::syntax::{NodeId, RuleId};

const REVISION_SCHEMA: &[u8] = b"bbnf.w2.source.v1\0";

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

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
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

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) enum SourceNodeKind {
    Epsilon,
    Literal(Vec<u8>),
    Regex(String),
    Reference(String),
    Sequence(Vec<NodeId>),
    Choice(Vec<NodeId>),
    Repeat {
        child: NodeId,
        minimum: u32,
        maximum: Option<u32>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) struct SourceNode {
    pub(crate) kind: SourceNodeKind,
    pub(crate) span: SourceSpan,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) struct SourceRule {
    pub(crate) id: RuleId,
    pub(crate) name: String,
    pub(crate) root: NodeId,
    pub(crate) span: SourceSpan,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceModule {
    revision: Revision,
    logical_path: String,
    bytes: Vec<u8>,
    pub(crate) nodes: Vec<SourceNode>,
    pub(crate) rules: Vec<SourceRule>,
}

impl SourceModule {
    pub(crate) fn new(
        logical_path: String,
        bytes: Vec<u8>,
        nodes: Vec<SourceNode>,
        rules: Vec<SourceRule>,
    ) -> Self {
        let revision = Revision::for_module(&logical_path, &bytes);
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
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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
}
