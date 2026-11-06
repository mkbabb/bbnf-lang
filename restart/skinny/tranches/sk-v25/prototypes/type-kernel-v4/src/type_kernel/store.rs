//! Draft semantic/type/row arenas and consumed closure certification.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::atomic::{AtomicU64, Ordering};

use super::{InputDigest, OperationContext, Origin, TypeError, TypeErrorKind, charge_text};

static NEXT_SCOPE: AtomicU64 = AtomicU64::new(1);

macro_rules! draft_id {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name {
            pub(crate) scope: u64,
            pub(crate) slot: u32,
        }
    };
}

draft_id!(DraftTypeId);
draft_id!(DraftRowId);
draft_id!(DraftRecVar);

/// Semantic primitives. These are not storage layouts.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Primitive {
    Bool,
    Bytes,
    Diagnostic,
    Enum,
    BoxedEnum,
    F64,
    I8,
    I16,
    I32,
    I64,
    ParseResult,
    Span,
    Str,
    U8,
    U16,
    U32,
    U64,
    Unit,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RowKind {
    Record,
    Variant,
}

#[derive(Clone, Debug)]
pub(crate) enum DraftNode {
    Meta {
        level: u32,
        ordinal: u32,
    },
    Rec(DraftRecVar),
    Primitive(Primitive),
    Named(String),
    Tuple(Vec<DraftTypeId>),
    List(DraftTypeId),
    Option(DraftTypeId),
    Function {
        parameters: Vec<DraftTypeId>,
        result: DraftTypeId,
    },
    Record(DraftRowId),
    Variant(DraftRowId),
    Mu {
        binder: DraftRecVar,
        body: DraftTypeId,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum DraftRowNode {
    Meta {
        kind: RowKind,
        level: u32,
    },
    Extend {
        kind: RowKind,
        fields: BTreeMap<String, DraftTypeId>,
        tail: Option<DraftRowId>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum Constraint {
    Equal {
        left: DraftTypeId,
        right: DraftTypeId,
        origin: Origin,
    },
    Lacks {
        row: DraftRowId,
        label: String,
        origin: Origin,
    },
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ExportKey(String);

impl ExportKey {
    #[must_use]
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ClosedExport {
    pub(crate) key: ExportKey,
    pub(crate) ty: DraftTypeId,
}

/// The only post-draft problem value. Its fields and validation scope are private.
#[derive(Clone, Debug)]
pub struct ClosedProblem {
    pub(crate) validation_revision: u64,
    pub(crate) nodes: Vec<DraftNode>,
    pub(crate) rows: Vec<DraftRowNode>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) exports: Vec<ClosedExport>,
    pub(crate) input_digest: Option<InputDigest>,
}

/// Closed export lookup. Values become solve roots only after successful solve.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExportMap {
    keys: Vec<ExportKey>,
}

impl ExportMap {
    #[must_use]
    pub fn keys(&self) -> &[ExportKey] {
        &self.keys
    }
}

/// Private-arena builder. Every handle it returns is draft-only.
#[derive(Debug)]
pub struct ProblemBuilder {
    scope: u64,
    nodes: Vec<DraftNode>,
    rows: Vec<DraftRowNode>,
    rec_vars: u32,
    metas: u32,
    constraints: Vec<Constraint>,
    input_digest: Option<InputDigest>,
}

impl Default for ProblemBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ProblemBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scope: NEXT_SCOPE.fetch_add(1, Ordering::Relaxed),
            nodes: Vec::new(),
            rows: Vec::new(),
            rec_vars: 0,
            metas: 0,
            constraints: Vec::new(),
            input_digest: None,
        }
    }

    pub(crate) fn with_input_digest(digest: InputDigest) -> Self {
        let mut builder = Self::new();
        builder.input_digest = Some(digest);
        builder
    }

    pub fn primitive(
        &mut self,
        primitive: Primitive,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.push_type(
            DraftNode::Primitive(primitive),
            "store.primitive",
            operations,
        )
    }

    pub fn named(
        &mut self,
        name: impl Into<String>,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        let name = name.into();
        charge_text(
            operations,
            "store.named.bytes",
            &name,
            &[],
            Origin::boundary("store.named"),
        )?;
        if name.is_empty() {
            return Err(operations.fail(
                TypeErrorKind::InvalidInput {
                    detail: "semantic names cannot be empty".to_owned(),
                },
                &[],
                "store.named",
            ));
        }
        self.push_type(DraftNode::Named(name), "store.named", operations)
    }

    pub fn meta(
        &mut self,
        level: u32,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        let ordinal = self.metas;
        self.metas = self
            .metas
            .checked_add(1)
            .ok_or_else(|| operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "store.meta"))?;
        self.push_type(DraftNode::Meta { level, ordinal }, "store.meta", operations)
    }

    pub fn tuple(
        &mut self,
        items: impl IntoIterator<Item = DraftTypeId>,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        let items = items.into_iter().collect::<Vec<_>>();
        self.validate_types(&items, operations, "store.tuple")?;
        operations.charge(
            items.len().max(1),
            "store.tuple.edges",
            &[],
            [Origin::boundary("store.tuple")],
        )?;
        self.push_type(DraftNode::Tuple(items), "store.tuple", operations)
    }

    pub fn list(
        &mut self,
        item: DraftTypeId,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.validate_type(item, operations, "store.list")?;
        self.push_type(DraftNode::List(item), "store.list", operations)
    }

    pub fn option(
        &mut self,
        item: DraftTypeId,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.validate_type(item, operations, "store.option")?;
        self.push_type(DraftNode::Option(item), "store.option", operations)
    }

    pub fn function(
        &mut self,
        parameters: impl IntoIterator<Item = DraftTypeId>,
        result: DraftTypeId,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        let parameters = parameters.into_iter().collect::<Vec<_>>();
        self.validate_types(&parameters, operations, "store.function")?;
        self.validate_type(result, operations, "store.function")?;
        operations.charge(
            parameters.len().saturating_add(1),
            "store.function.edges",
            &[],
            [Origin::boundary("store.function")],
        )?;
        self.push_type(
            DraftNode::Function { parameters, result },
            "store.function",
            operations,
        )
    }

    pub fn fresh_rec_var(
        &mut self,
        operations: &mut OperationContext,
    ) -> Result<DraftRecVar, TypeError> {
        operations.charge(1, "store.rec.fresh", &[], [Origin::boundary("store.rec")])?;
        let slot = self.rec_vars;
        self.rec_vars = self
            .rec_vars
            .checked_add(1)
            .ok_or_else(|| operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "store.rec"))?;
        Ok(DraftRecVar {
            scope: self.scope,
            slot,
        })
    }

    pub fn rec_var(
        &mut self,
        binder: DraftRecVar,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.validate_rec(binder, operations, "store.rec_var")?;
        self.push_type(DraftNode::Rec(binder), "store.rec_var", operations)
    }

    pub fn mu(
        &mut self,
        binder: DraftRecVar,
        body: DraftTypeId,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.validate_rec(binder, operations, "store.mu")?;
        self.validate_type(body, operations, "store.mu")?;
        if !self.contains_rec(body, binder, operations)? {
            return Err(operations.fail(TypeErrorKind::VacuousRecursion, &[], "store.mu"));
        }
        self.push_type(DraftNode::Mu { binder, body }, "store.mu", operations)
    }

    pub fn row_meta(
        &mut self,
        kind: RowKind,
        level: u32,
        operations: &mut OperationContext,
    ) -> Result<DraftRowId, TypeError> {
        self.push_row(
            DraftRowNode::Meta { kind, level },
            "store.row_meta",
            operations,
        )
    }

    pub fn row(
        &mut self,
        kind: RowKind,
        fields: impl IntoIterator<Item = (String, DraftTypeId)>,
        tail: Option<DraftRowId>,
        origin: Origin,
        operations: &mut OperationContext,
    ) -> Result<DraftRowId, TypeError> {
        let mut canonical = BTreeMap::new();
        for (label, ty) in fields {
            charge_text(operations, "store.row.label", &label, &[], origin.clone())?;
            self.validate_type(ty, operations, "store.row")?;
            if canonical.insert(label.clone(), ty).is_some() {
                return Err(operations.fail(
                    TypeErrorKind::DuplicateRowLabel { label },
                    &[],
                    origin.site(),
                ));
            }
        }
        if let Some(tail) = tail {
            self.validate_row(tail, operations, "store.row")?;
            if self.row_kind(tail) != Some(kind) {
                return Err(operations.fail(TypeErrorKind::RowKindMismatch, &[], "store.row"));
            }
            let mut pending = Some(tail);
            while let Some(row) = pending {
                operations.charge(1, "store.row.tail_labels", &[], [origin.clone()])?;
                match &self.rows[row.slot as usize] {
                    DraftRowNode::Meta { .. } => break,
                    DraftRowNode::Extend {
                        fields, tail: next, ..
                    } => {
                        if let Some(label) =
                            fields.keys().find(|label| canonical.contains_key(*label))
                        {
                            return Err(operations.fail(
                                TypeErrorKind::DuplicateRowLabel {
                                    label: label.clone(),
                                },
                                &[],
                                origin.site(),
                            ));
                        }
                        pending = *next;
                    }
                }
            }
        }
        operations.charge(canonical.len().max(1), "store.row.fields", &[], [origin])?;
        self.push_row(
            DraftRowNode::Extend {
                kind,
                fields: canonical,
                tail,
            },
            "store.row",
            operations,
        )
    }

    pub fn record(
        &mut self,
        row: DraftRowId,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.validate_row(row, operations, "store.record")?;
        if self.row_kind(row) != Some(RowKind::Record) {
            return Err(operations.fail(TypeErrorKind::RowKindMismatch, &[], "store.record"));
        }
        self.push_type(DraftNode::Record(row), "store.record", operations)
    }

    pub fn variant(
        &mut self,
        row: DraftRowId,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        self.validate_row(row, operations, "store.variant")?;
        if self.row_kind(row) != Some(RowKind::Variant) {
            return Err(operations.fail(TypeErrorKind::RowKindMismatch, &[], "store.variant"));
        }
        self.push_type(DraftNode::Variant(row), "store.variant", operations)
    }

    pub fn equal(
        &mut self,
        left: DraftTypeId,
        right: DraftTypeId,
        origin: Origin,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        self.validate_type(left, operations, "store.equal")?;
        self.validate_type(right, operations, "store.equal")?;
        operations.charge(2, "store.equal.endpoints", &[], [origin.clone()])?;
        self.constraints.push(Constraint::Equal {
            left,
            right,
            origin,
        });
        Ok(())
    }

    pub fn lacks(
        &mut self,
        row: DraftRowId,
        label: impl Into<String>,
        origin: Origin,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        self.validate_row(row, operations, "store.lacks")?;
        let label = label.into();
        charge_text(operations, "store.lacks.label", &label, &[], origin.clone())?;
        self.constraints
            .push(Constraint::Lacks { row, label, origin });
        Ok(())
    }

    pub fn close(
        self,
        exports: impl IntoIterator<Item = (ExportKey, DraftTypeId)>,
        operations: &mut OperationContext,
    ) -> Result<(ClosedProblem, ExportMap), TypeError> {
        let mut exports = exports.into_iter().collect::<Vec<_>>();
        exports.sort_by(|left, right| left.0.cmp(&right.0));
        for (key, _) in &exports {
            charge_text(
                operations,
                "store.close.export_key",
                key.as_str(),
                &[],
                Origin::boundary("store.close"),
            )?;
        }
        for window in exports.windows(2) {
            if window[0].0 == window[1].0 {
                return Err(operations.fail(
                    TypeErrorKind::InvalidInput {
                        detail: format!("duplicate export {}", window[0].0.as_str()),
                    },
                    &[],
                    "store.close",
                ));
            }
        }
        let mut closure_roots = exports.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
        for constraint in &self.constraints {
            match constraint {
                Constraint::Equal { left, right, .. } => {
                    closure_roots.push(*left);
                    closure_roots.push(*right);
                }
                Constraint::Lacks { .. } => {}
            }
        }
        for root in closure_roots {
            self.validate_type(root, operations, "store.close")?;
            self.validate_closed_root(root, operations)?;
        }
        for constraint in &self.constraints {
            if let Constraint::Lacks { row, .. } = constraint {
                self.validate_row(*row, operations, "store.close")?;
            }
        }
        operations.charge(
            exports.len().max(1),
            "store.close.publish",
            &[],
            [Origin::boundary("store.close")],
        )?;
        let keys = exports
            .iter()
            .map(|(key, _)| key.clone())
            .collect::<Vec<_>>();
        let closed_exports = exports
            .into_iter()
            .map(|(key, ty)| ClosedExport { key, ty })
            .collect();
        Ok((
            ClosedProblem {
                validation_revision: self.scope,
                nodes: self.nodes,
                rows: self.rows,
                constraints: self.constraints,
                exports: closed_exports,
                input_digest: self.input_digest,
            },
            ExportMap { keys },
        ))
    }

    fn push_type(
        &mut self,
        node: DraftNode,
        site: &str,
        operations: &mut OperationContext,
    ) -> Result<DraftTypeId, TypeError> {
        operations.charge(1, site, &[], [Origin::boundary(site)])?;
        let slot = u32::try_from(self.nodes.len())
            .map_err(|_| operations.fail(TypeErrorKind::ArithmeticOverflow, &[], site))?;
        self.nodes.push(node);
        Ok(DraftTypeId {
            scope: self.scope,
            slot,
        })
    }

    fn push_row(
        &mut self,
        row: DraftRowNode,
        site: &str,
        operations: &mut OperationContext,
    ) -> Result<DraftRowId, TypeError> {
        operations.charge(1, site, &[], [Origin::boundary(site)])?;
        let slot = u32::try_from(self.rows.len())
            .map_err(|_| operations.fail(TypeErrorKind::ArithmeticOverflow, &[], site))?;
        self.rows.push(row);
        Ok(DraftRowId {
            scope: self.scope,
            slot,
        })
    }

    fn validate_types(
        &self,
        ids: &[DraftTypeId],
        operations: &OperationContext,
        site: &str,
    ) -> Result<(), TypeError> {
        for id in ids {
            self.validate_type(*id, operations, site)?;
        }
        Ok(())
    }

    fn validate_type(
        &self,
        id: DraftTypeId,
        operations: &OperationContext,
        site: &str,
    ) -> Result<(), TypeError> {
        if id.scope != self.scope {
            return Err(operations.fail(TypeErrorKind::CrossStore, &[], site));
        }
        if id.slot as usize >= self.nodes.len() {
            return Err(operations.fail(TypeErrorKind::DanglingType, &[], site));
        }
        Ok(())
    }

    fn validate_row(
        &self,
        id: DraftRowId,
        operations: &OperationContext,
        site: &str,
    ) -> Result<(), TypeError> {
        if id.scope != self.scope {
            return Err(operations.fail(TypeErrorKind::CrossStore, &[], site));
        }
        if id.slot as usize >= self.rows.len() {
            return Err(operations.fail(TypeErrorKind::DanglingRow, &[], site));
        }
        Ok(())
    }

    fn validate_rec(
        &self,
        id: DraftRecVar,
        operations: &OperationContext,
        site: &str,
    ) -> Result<(), TypeError> {
        if id.scope != self.scope {
            return Err(operations.fail(TypeErrorKind::CrossStore, &[], site));
        }
        if id.slot >= self.rec_vars {
            return Err(operations.fail(TypeErrorKind::DanglingRecursion, &[], site));
        }
        Ok(())
    }

    fn row_kind(&self, row: DraftRowId) -> Option<RowKind> {
        self.rows.get(row.slot as usize).map(|row| match row {
            DraftRowNode::Meta { kind, .. } | DraftRowNode::Extend { kind, .. } => *kind,
        })
    }

    fn contains_rec(
        &self,
        root: DraftTypeId,
        target: DraftRecVar,
        operations: &mut OperationContext,
    ) -> Result<bool, TypeError> {
        let mut types = vec![root];
        let mut rows = Vec::new();
        let mut seen_types = BTreeSet::new();
        let mut seen_rows = BTreeSet::new();
        while let Some(ty) = types.pop() {
            operations.charge(1, "store.mu.occurs", &[], [Origin::boundary("store.mu")])?;
            if !seen_types.insert(ty) {
                continue;
            }
            match &self.nodes[ty.slot as usize] {
                DraftNode::Rec(binder) if *binder == target => return Ok(true),
                DraftNode::Tuple(items) => types.extend(items.iter().copied()),
                DraftNode::List(item) | DraftNode::Option(item) => types.push(*item),
                DraftNode::Function { parameters, result } => {
                    types.extend(parameters.iter().copied());
                    types.push(*result);
                }
                DraftNode::Record(row) | DraftNode::Variant(row) => rows.push(*row),
                DraftNode::Mu { body, .. } => types.push(*body),
                DraftNode::Meta { .. }
                | DraftNode::Rec(_)
                | DraftNode::Primitive(_)
                | DraftNode::Named(_) => {}
            }
            while let Some(row) = rows.pop() {
                if !seen_rows.insert(row) {
                    continue;
                }
                if let DraftRowNode::Extend { fields, tail, .. } = &self.rows[row.slot as usize] {
                    types.extend(fields.values().copied());
                    rows.extend(*tail);
                }
            }
        }
        Ok(false)
    }

    fn validate_closed_root(
        &self,
        root: DraftTypeId,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        enum Frame {
            Enter(DraftTypeId),
            Leave(DraftRecVar),
        }
        let mut pending = vec![Frame::Enter(root)];
        let mut active = BTreeMap::<DraftRecVar, usize>::new();
        while let Some(frame) = pending.pop() {
            operations.charge(
                1,
                "store.close.closure",
                &[],
                [Origin::boundary("store.close")],
            )?;
            match frame {
                Frame::Leave(binder) => {
                    if let Some(count) = active.get_mut(&binder) {
                        *count -= 1;
                        if *count == 0 {
                            active.remove(&binder);
                        }
                    }
                }
                Frame::Enter(ty) => match &self.nodes[ty.slot as usize] {
                    DraftNode::Rec(binder) if !active.contains_key(binder) => {
                        return Err(operations.fail(
                            TypeErrorKind::EscapingRecursion {
                                site: format!("type:{}", ty.slot),
                            },
                            &[],
                            "store.close.escaping_recursion",
                        ));
                    }
                    DraftNode::Tuple(items) => {
                        pending.extend(items.iter().rev().copied().map(Frame::Enter));
                    }
                    DraftNode::List(item) | DraftNode::Option(item) => {
                        pending.push(Frame::Enter(*item));
                    }
                    DraftNode::Function { parameters, result } => {
                        pending.push(Frame::Enter(*result));
                        pending.extend(parameters.iter().rev().copied().map(Frame::Enter));
                    }
                    DraftNode::Record(row) | DraftNode::Variant(row) => {
                        let mut rows = vec![*row];
                        let mut seen = BTreeSet::new();
                        while let Some(row) = rows.pop() {
                            operations.charge(
                                1,
                                "store.close.row",
                                &[],
                                [Origin::boundary("store.close")],
                            )?;
                            if !seen.insert(row) {
                                continue;
                            }
                            if let DraftRowNode::Extend { fields, tail, .. } =
                                &self.rows[row.slot as usize]
                            {
                                pending.extend(fields.values().rev().copied().map(Frame::Enter));
                                rows.extend(*tail);
                            }
                        }
                    }
                    DraftNode::Mu { binder, body } => {
                        *active.entry(*binder).or_default() += 1;
                        pending.push(Frame::Leave(*binder));
                        pending.push(Frame::Enter(*body));
                    }
                    DraftNode::Meta { .. }
                    | DraftNode::Rec(_)
                    | DraftNode::Primitive(_)
                    | DraftNode::Named(_) => {}
                },
            }
        }
        Ok(())
    }
}
