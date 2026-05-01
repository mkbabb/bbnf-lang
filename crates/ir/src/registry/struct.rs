//! `StructLayout` + `StructRegistry` — typed struct-shape projections for
//! every `Named` rule in a grammar.
//!
//! The registry maps `RuleId → StructLayout`. Each layout carries:
//!
//! - The originating rule's name + id (so emitter consumers can resolve
//!   field types whose `TypeDesc::Named` references another registered
//!   rule by name).
//! - A discriminator [`LayoutKind`] derived from the rule body's `IrNode`
//!   shape (`Alt` → tagged enum; `Seq` / scalar / Map → struct; pure `Map`
//!   wrapping a single typed leaf → newtype wrapper; type-level `Vec`
//!   inner → list field).
//! - A list of [`StructField`]s; each field carries the projected
//!   `TypeDesc`, the rule-local field name, and a [`FieldSource`] tag
//!   describing which IR construct produced the field.
//!
//! The discriminator is data, not a switch: consumers query
//! [`StructLayout::is_tagged_enum`] / [`StructLayout::is_newtype`] /
//! [`StructLayout::field_count`] etc. rather than re-pattern-matching on
//! `LayoutKind`. Every field carries enough provenance to bridge
//! emission against the existing tape pipeline during the W1 wave (W2 / W3
//! sever the tape path; W1 only populates).
//!
//! Iteration order is `BTreeMap`-stable for byte-identical audit JSON
//! output across runs: the audit gate's recorded artefact is a structural
//! diff target, so non-determinism in registry iteration would break
//! reproducibility.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::types::{RuleId, TypeDesc};

/// Discriminator for a registered layout's emitted shape.
///
/// Projected from the originating rule's body shape during
/// `project_types` closure. Each variant projects from a specific
/// `IrNode` family:
///
/// - [`LayoutKind::Struct`] — projects from `Seq`, scalar leaves, or
///   `Map` wrapping a non-singleton body. Fields carry per-position
///   projected types.
/// - [`LayoutKind::TaggedEnum`] — projects from `Alt`. Each `StructField`
///   represents one alternation branch; the field's `name` is the
///   sub-variant name (per `RuleMeta::sub_variants` if present, else
///   `branch_<idx>`); the field's `type_desc` is the branch's projected
///   type.
/// - [`LayoutKind::UntaggedEnum`] — `Alt` whose every branch projects to
///   the same `TypeDesc`. The discriminator collapses to the unitary
///   branch type at emission time; sub-variants are still listed for
///   audit completeness.
/// - [`LayoutKind::NewtypeWrapper`] — `Map` of a single leaf
///   (`Regex` / `Literal`) whose typed `->` annotation projects a
///   primitive (`F64`, `U32`, `Bool`, `Span`, …). The single field is
///   the wrapped scalar.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LayoutKind {
    /// Composite struct — fields from a `Seq` body or a structurally
    /// composite `Map` rule. Field count >= 0; field-per-position.
    Struct,
    /// Sum type with discriminator. Fields enumerate alternation
    /// branches. Field count = branch count.
    TaggedEnum,
    /// Sum type whose every branch projects to the same payload type.
    /// Emitted as a single typed payload at the consumer; sub-variant
    /// metadata is retained for audit symmetry.
    UntaggedEnum,
    /// Newtype wrapper around a single typed leaf — `Map(Regex,
    /// -> Primitive)` is the canonical case. Always exactly one field.
    NewtypeWrapper,
}

/// Provenance tag for a `StructField`.
///
/// The audit pass and W2/W3 emitters consult this to resolve which IR
/// construct produced a field — sub-variant arms route through `BranchTag`,
/// `Seq` positions through `SeqPosition`, typed-leaf single-field rules
/// through `TypedLeaf`, and `Repeat` inner types through `RepeatElement`.
/// Per `feedback_pluggable-components` the field's source is data, not a
/// match-arm in the consumer: emitters query
/// [`StructField::is_branch_tag`] / [`StructField::is_seq_position`] etc.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FieldSource {
    /// Field came from a typed-leaf marker (`-> F64`, `-> U32`,
    /// `-> Span`, etc.) on a `Map` of a single regex / literal body.
    /// The single source for the [`LayoutKind::NewtypeWrapper`] case.
    TypedLeaf,
    /// Field came from one branch of an `Alt`. The companion `name` on
    /// the `StructField` is the sub-variant name.
    BranchTag {
        /// Index of the branch within the originating `Alt` body.
        branch_index: u32,
    },
    /// Field came from one position of a `Seq` body. The companion
    /// `name` is `field_<index>` unless a more specific name is
    /// available from upstream metadata.
    SeqPosition {
        /// Position within the originating `Seq` body.
        position: u32,
    },
    /// Field came from the element type of a `Repeat` rule body. The
    /// rule projects to `Vec<T>` and the field carries `T`'s type
    /// descriptor with a list discriminator on the parent layout.
    RepeatElement,
    /// Field came from a `Ref` to another rule. The projection types
    /// the field as `TypeDesc::Named` keyed by the referenced rule.
    /// Recursive references and forward references both project here.
    RuleReference {
        /// The referenced rule's id; the consumer indexes back into
        /// the registry to resolve its layout.
        target_rule: RuleId,
    },
}

/// One field of a registered [`StructLayout`].
///
/// Carries the projected `TypeDesc`, a rule-local name, and a
/// [`FieldSource`] tag identifying the IR construct that produced the
/// field.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructField {
    /// Field name, rule-local. For `BranchTag` fields this is the
    /// sub-variant name; for `SeqPosition` fields this is
    /// `field_<position>`; for `TypedLeaf` newtypes this is the leaf
    /// name (`value`); for `RuleReference` fields this is the
    /// referenced rule's name; for `RepeatElement` this is `element`.
    pub name: String,
    /// Projected scalar / compound type produced by the field-emitting
    /// IR construct. `Named` variants reference another registered
    /// rule by string id.
    pub type_desc: TypeDesc,
    /// Provenance tag — which IR construct produced the field.
    pub source: FieldSource,
}

impl StructField {
    /// True when this field came from a typed-leaf `->` marker on a
    /// `Map` of a single leaf body (the [`LayoutKind::NewtypeWrapper`]
    /// case).
    pub fn is_typed_leaf(&self) -> bool {
        matches!(self.source, FieldSource::TypedLeaf)
    }

    /// True when this field came from one branch of an `Alt`.
    pub fn is_branch_tag(&self) -> bool {
        matches!(self.source, FieldSource::BranchTag { .. })
    }

    /// True when this field came from one position of a `Seq` body.
    pub fn is_seq_position(&self) -> bool {
        matches!(self.source, FieldSource::SeqPosition { .. })
    }

    /// True when this field came from the element type of a `Repeat`
    /// body — the parent layout's outermost `TypeDesc` is `Vec<T>`.
    pub fn is_repeat_element(&self) -> bool {
        matches!(self.source, FieldSource::RepeatElement)
    }

    /// True when this field is a reference to another registered rule.
    /// Recursive references and forward references both project here.
    pub fn is_rule_reference(&self) -> bool {
        matches!(self.source, FieldSource::RuleReference { .. })
    }

    /// Returns the referenced rule id when this field is a
    /// rule-reference; `None` otherwise.
    pub fn target_rule(&self) -> Option<RuleId> {
        match self.source {
            FieldSource::RuleReference { target_rule } => Some(target_rule),
            _ => None,
        }
    }

    /// Returns the alternation branch index when this field is a
    /// branch tag; `None` otherwise.
    pub fn branch_index(&self) -> Option<u32> {
        match self.source {
            FieldSource::BranchTag { branch_index } => Some(branch_index),
            _ => None,
        }
    }

    /// Returns the Seq position when this field is a Seq-position
    /// field; `None` otherwise.
    pub fn seq_position(&self) -> Option<u32> {
        match self.source {
            FieldSource::SeqPosition { position } => Some(position),
            _ => None,
        }
    }
}

/// A registered struct layout for a single `Named` rule.
///
/// Drive emission via the accessor methods rather than re-matching on
/// [`LayoutKind`]: `is_tagged_enum`, `is_struct`, `is_newtype`,
/// `is_untagged_enum`, `field_count`, `fields()`, `field(name)`,
/// `branches()`. The discriminator is data; the consumer queries it.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructLayout {
    /// Originating rule id. Stable within the GrammarIR.
    pub rule_id: RuleId,
    /// Originating rule name (resolved from `ir.strings` at population
    /// time). Owned `String` so layouts survive the `ir.strings`
    /// table's lifetime in caller-owned tests and serialised audit
    /// snapshots.
    pub rule_name: String,
    /// Layout kind discriminator — projected from the rule's body
    /// shape. Drives emitter dispatch but consumers should query via
    /// `is_struct` / `is_tagged_enum` etc. accessors.
    pub kind: LayoutKind,
    /// The rule's outermost projected type descriptor — typically a
    /// `Tuple` for structs, `BoxedEnum` / `Enum` for tagged enums, or
    /// the wrapped scalar for newtypes. Consumers needing the actual
    /// emission shape use this in conjunction with `kind`.
    pub rule_type: TypeDesc,
    /// Fields in declaration order. For tagged enums the order matches
    /// branch order in the originating `Alt`; for structs the order
    /// matches `Seq` position; for newtypes there is exactly one
    /// field.
    pub fields: Vec<StructField>,
}

impl StructLayout {
    /// True iff [`LayoutKind::Struct`].
    pub fn is_struct(&self) -> bool {
        matches!(self.kind, LayoutKind::Struct)
    }

    /// True iff [`LayoutKind::TaggedEnum`].
    pub fn is_tagged_enum(&self) -> bool {
        matches!(self.kind, LayoutKind::TaggedEnum)
    }

    /// True iff [`LayoutKind::UntaggedEnum`].
    pub fn is_untagged_enum(&self) -> bool {
        matches!(self.kind, LayoutKind::UntaggedEnum)
    }

    /// True iff [`LayoutKind::NewtypeWrapper`].
    pub fn is_newtype(&self) -> bool {
        matches!(self.kind, LayoutKind::NewtypeWrapper)
    }

    /// Number of fields registered. For a tagged enum this equals the
    /// branch count; for a struct the position count; for a newtype
    /// always 1.
    pub fn field_count(&self) -> usize {
        self.fields.len()
    }

    /// True iff every registered field carries the same `TypeDesc`.
    /// Used to project an `Alt` whose branches all carry one shared
    /// payload type to [`LayoutKind::UntaggedEnum`] at emission time.
    pub fn all_fields_share_type(&self) -> bool {
        match self.fields.split_first() {
            None => true,
            Some((first, rest)) => rest.iter().all(|f| f.type_desc == first.type_desc),
        }
    }

    /// Returns the field whose name matches `name`, if any. Linear
    /// scan; field counts are typically small (< 30).
    pub fn field(&self, name: &str) -> Option<&StructField> {
        self.fields.iter().find(|f| f.name == name)
    }

    /// Iterator over registered fields in declaration order.
    pub fn fields(&self) -> impl Iterator<Item = &StructField> {
        self.fields.iter()
    }

    /// Iterator over branch-tag fields only — returns the
    /// `(branch_index, &StructField)` pair for each
    /// [`FieldSource::BranchTag`] field. Empty for non-Alt-derived
    /// layouts.
    pub fn branches(&self) -> impl Iterator<Item = (u32, &StructField)> {
        self.fields
            .iter()
            .filter_map(|f| f.branch_index().map(|idx| (idx, f)))
    }

    /// True if any field on this layout admits the given type
    /// descriptor as its projected leaf. Used by the audit pass to
    /// classify a typed marker as `Mapped` once W1 closure runs.
    pub fn admits_type(&self, ty: &TypeDesc) -> bool {
        self.fields.iter().any(|f| f.type_desc == *ty)
    }

    /// True if any field on this layout admits a scalar payload at any
    /// nesting depth (the `Mapped` admission criterion under the audit
    /// pass's default scalar-cover policy).
    ///
    /// Recurses into `Tuple`, `Option`, and `Vec` wrappers via
    /// [`TypeDesc::has_scalar_payload`]. The lowering pipeline wraps
    /// keyword-discriminator branch payloads as `Tuple([Span,
    /// scalar])`; the coverage policy admits these as Mapped because
    /// the typed leaf the marker declared still reaches the layout's
    /// projected type — only the wrapper nesting differs.
    pub fn admits_scalar_payload(&self) -> bool {
        self.fields.iter().any(|f| f.type_desc.has_scalar_payload())
    }
}

/// Registry of [`StructLayout`] entries, one per `Named` rule.
///
/// `BTreeMap`-keyed for byte-stable iteration order across compile
/// runs — the audit pass's recorded JSON output is a structural diff
/// target, so iteration determinism is load-bearing.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct StructRegistry {
    layouts: BTreeMap<RuleId, StructLayout>,
}

impl StructRegistry {
    /// Construct an empty registry. Populate via [`Self::insert`] from
    /// the registry-population phase of `project_types`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a layout. Overwrites any existing entry for the same
    /// rule id; idempotent in the closure's fixed-point loop.
    pub fn insert(&mut self, layout: StructLayout) {
        self.layouts.insert(layout.rule_id, layout);
    }

    /// Lookup the layout for `rule_id`, if registered.
    pub fn layout(&self, rule_id: RuleId) -> Option<&StructLayout> {
        self.layouts.get(&rule_id)
    }

    /// Lookup a registered layout by rule name. Linear scan over
    /// registered entries.
    pub fn layout_by_name(&self, name: &str) -> Option<&StructLayout> {
        self.layouts.values().find(|l| l.rule_name == name)
    }

    /// Iterate over registered `(rule_id, &StructLayout)` pairs in
    /// `RuleId` order.
    pub fn layouts(&self) -> impl Iterator<Item = (&RuleId, &StructLayout)> {
        self.layouts.iter()
    }

    /// Iterate over registered layouts only, in `RuleId` order.
    pub fn iter(&self) -> impl Iterator<Item = &StructLayout> {
        self.layouts.values()
    }

    /// Number of registered layouts.
    pub fn len(&self) -> usize {
        self.layouts.len()
    }

    /// True iff the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.layouts.is_empty()
    }

    /// True iff the rule has a registered layout.
    pub fn contains(&self, rule_id: RuleId) -> bool {
        self.layouts.contains_key(&rule_id)
    }

    /// Drop every entry. Used by the closure's fixed-point loop when
    /// the upstream type projection invalidates the registry.
    pub fn clear(&mut self) {
        self.layouts.clear();
    }
}
