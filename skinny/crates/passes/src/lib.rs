use ir::{
    all_backend_shapes, BackendExpr, BackendIr, BackendRule, BackendShape, CapacityPolicy,
    CostFacts, DirectBuildField, DirectBuildSource, EvidenceSource, ExprId, ExprKind, GrammarIr,
    Measurement, PriorityStep, Recognizer, RejectedAlternative, RejectionReason, ShapeFacts,
    ShapeRationale, SimdMode, SimdSite, SourceSpan, SpanKind, SpanMarkKind, StructuralAlphabet,
    TapeKind, ValidationError,
};
use std::collections::{HashMap, HashSet};
use thiserror::Error;

pub mod diagnostics;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum PassError {
    #[error(transparent)]
    Validation(#[from] ValidationError),
    #[error("{0}")]
    Type(String),
    #[error("missing entry rule `{0}`")]
    MissingEntry(String),
}

pub fn normalize(grammar: &GrammarIr) -> Result<GrammarIr, PassError> {
    grammar.validate()?;
    Ok(grammar.clone())
}

pub fn compile(grammar: &GrammarIr) -> Result<PipelineOutput, PassError> {
    let normalized = normalize(grammar)?;
    let entry_rule = derive_entry_rule(&normalized)?;
    let type_facts = layout::types::infer(&normalized)?;
    let mut layout_facts = layout::run(&normalized, type_facts, entry_rule);
    let materialization = extract::derive_materialization_plan(&normalized);
    let shape_facts = shapes::derive_shape_facts(&normalized, &materialization);
    let recognizers = recognizers::derive_recognizers(&normalized);
    let backend_ir = extract::single_plan(
        &normalized,
        &layout_facts,
        entry_rule,
        shape_facts,
        recognizers,
        &materialization,
    )?;
    let shape_plan = recognizers::derive_backend_shape_with_diagnostics(
        &normalized,
        &backend_ir,
        &layout_facts,
        recognizers::TargetFeatures::host(),
    );
    layout_facts.backend_shape = shape_plan.backend_shape;
    layout_facts.cost_facts = shape_plan.cost_facts;
    debug_assert!(layout_facts
        .cost_facts
        .iter()
        .all(|(rule, facts)| layout_facts.backend_shape.get(rule) == Some(&facts.chosen)));
    Ok(PipelineOutput {
        grammar: normalized,
        layout_facts,
        backend_ir,
        diagnostics: shape_plan.diagnostics,
    })
}

fn derive_entry_rule(grammar: &GrammarIr) -> Result<ir::RuleId, PassError> {
    if let Some(rule) = grammar
        .rules
        .iter()
        .find(|rule| rule.name == grammar.name)
        .or_else(|| grammar.rules.last())
    {
        return Ok(rule.id);
    }
    Err(PassError::MissingEntry(grammar.name.clone()))
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PipelineOutput {
    pub grammar: GrammarIr,
    pub layout_facts: LayoutFacts,
    pub backend_ir: BackendIr,
    pub diagnostics: Vec<diagnostics::PassDiagnostic>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LayoutFacts {
    pub rule_types: HashMap<ir::RuleId, Type>,
    pub node_types: HashMap<ExprId, Type>,
    pub layout_policies: HashMap<String, String>,
    pub hot_call_graph: HashMap<ir::RuleId, recognizers::hot_path::HotPathFact>,
    pub backend_shape: HashMap<ir::RuleId, BackendShape>,
    pub cost_facts: HashMap<ir::RuleId, CostFacts>,
}

pub mod layout {
    use super::*;

    pub fn run(
        grammar: &GrammarIr,
        type_facts: types::TypeFacts,
        entry_rule: ir::RuleId,
    ) -> LayoutFacts {
        LayoutFacts {
            rule_types: type_facts.rule_types,
            node_types: type_facts.node_types,
            layout_policies: HashMap::new(),
            hot_call_graph: recognizers::hot_path::derive_hot_path(grammar, Some(entry_rule), None),
            backend_shape: HashMap::new(),
            cost_facts: HashMap::new(),
        }
    }

    pub mod types {
        use super::*;

        #[derive(Clone, Debug, Default, PartialEq, Eq)]
        pub struct TypeFacts {
            pub rule_types: HashMap<ir::RuleId, Type>,
            pub node_types: HashMap<ExprId, Type>,
            pub subst: Substitution,
            pub obligations: Vec<TypeObligation>,
        }

        #[derive(Clone, Debug, Default, PartialEq, Eq)]
        pub struct Substitution {
            pub vars: HashMap<TypeVarId, Type>,
        }

        #[derive(Clone, Debug, PartialEq, Eq)]
        pub struct TypeObligation {
            pub span: SourceSpan,
            pub expected_from: String,
            pub actual_from: String,
            pub solver_stage: &'static str,
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub struct TypeVarId(pub usize);

        pub fn infer(grammar: &GrammarIr) -> Result<TypeFacts, PassError> {
            let mut infer = Infer::new(grammar);
            infer.seed_rule_types();
            for rule in &grammar.rules {
                let ty = infer.infer_expr(rule.body)?;
                infer.facts.rule_types.insert(rule.id, ty);
            }
            Ok(infer.facts)
        }

        struct Infer<'a> {
            grammar: &'a GrammarIr,
            facts: TypeFacts,
        }

        impl<'a> Infer<'a> {
            fn new(grammar: &'a GrammarIr) -> Self {
                Self {
                    grammar,
                    facts: TypeFacts::default(),
                }
            }

            fn seed_rule_types(&mut self) {
                for rule in &self.grammar.rules {
                    self.facts.rule_types.insert(rule.id, Type::Rule(rule.id));
                }
            }

            fn infer_expr(&mut self, expr_id: ExprId) -> Result<Type, PassError> {
                if let Some(ty) = self.facts.node_types.get(&expr_id) {
                    return Ok(ty.clone());
                }

                let expr = self.grammar.expr(expr_id);
                let ty = match &expr.kind {
                    ExprKind::Seq(children) => {
                        Type::Seq(self.infer_many(children.iter().copied())?)
                    }
                    ExprKind::Alt { branches, .. } => {
                        Type::Alt(self.infer_many(branches.iter().copied())?)
                    }
                    ExprKind::Repeat { body, .. } => Type::List(Box::new(self.infer_expr(*body)?)),
                    ExprKind::Optional(body) => Type::Option(Box::new(self.infer_expr(*body)?)),
                    ExprKind::Literal { .. } => Type::Builtin(BuiltinTy::Bytes),
                    ExprKind::Regex { pattern } => Type::Builtin(regex_type(pattern)),
                    ExprKind::Ref { target, name } => {
                        let Some(rule_id) = *target else {
                            return Err(PassError::Type(format!(
                                "unresolved rule reference `{name}` during inference"
                            )));
                        };
                        Type::Rule(rule_id)
                    }
                    ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit),
                };

                self.facts.node_types.insert(expr_id, ty.clone());
                Ok(ty)
            }

            fn infer_many(
                &mut self,
                exprs: impl Iterator<Item = ExprId>,
            ) -> Result<Vec<Type>, PassError> {
                exprs.map(|expr| self.infer_expr(expr)).collect()
            }
        }

        fn regex_type(pattern: &str) -> BuiltinTy {
            if pattern == r"[ \t\n\r]*" {
                BuiltinTy::Unit
            } else if pattern.starts_with('"') {
                BuiltinTy::Span
            } else {
                BuiltinTy::Span
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(layout::types::TypeVarId),
    Builtin(BuiltinTy),
    Seq(Vec<Type>),
    Alt(Vec<Type>),
    List(Box<Type>),
    Option(Box<Type>),
    Rule(ir::RuleId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BuiltinTy {
    Unit,
    Bytes,
    Span,
    Str,
    F64,
    U8,
    Bool,
}

pub mod shapes {
    use super::*;

    pub fn derive_shape_facts(
        grammar: &GrammarIr,
        materialization: &extract::MaterializationPlan,
    ) -> ShapeFacts {
        let mut facts = ShapeFacts::new();
        let prefix = pascal_case(&grammar.name);
        let root_shape = format!("{prefix}Root");
        let value_shape = format!("{prefix}Value");
        let value_ty = format!("{value_shape}<'i>");
        facts.add_struct(root_shape, &[("value", value_ty.as_str())]);

        let shape_for = |kind: TapeKind| {
            materialization
                .descriptors
                .values()
                .find(|descriptor| descriptor.kind == kind)
                .map(|descriptor| descriptor.shape.as_str())
        };
        let object_shape = shape_for(TapeKind::Container).unwrap_or("Object");
        let array_shape = shape_for(TapeKind::Sequence).unwrap_or("Array");
        let pair_shape = shape_for(TapeKind::KeyValuePair).unwrap_or("Pair");
        let string_shape = shape_for(TapeKind::StringValue).unwrap_or("String");
        let number_shape = shape_for(TapeKind::NumberValue).unwrap_or("Number");
        let bool_shape = shape_for(TapeKind::BoolValue).unwrap_or("Bool");
        let null_shape = shape_for(TapeKind::NullValue).unwrap_or("Null");

        let variants = [
            format!("Object({object_shape}<'i>)"),
            format!("Array({array_shape}<'i>)"),
            format!("String({string_shape}<'i>)"),
            format!("Number({number_shape}<'i>)"),
            "Bool(bool)".to_string(),
            "Null".to_string(),
        ];
        let variant_refs = variants.iter().map(String::as_str).collect::<Vec<_>>();
        facts.add_enum(&value_shape, &variant_refs);

        let pair_ty = format!("TapeSlice<'i, {pair_shape}<'i>>");
        let value_slice_ty = format!("TapeSlice<'i, {value_shape}<'i>>");
        let string_ty = format!("{string_shape}<'i>");
        let value_ty = format!("{value_shape}<'i>");
        facts.add_struct(object_shape, &[("members", pair_ty.as_str())]);
        facts.add_struct(array_shape, &[("elements", value_slice_ty.as_str())]);
        facts.add_struct(
            pair_shape,
            &[("key", string_ty.as_str()), ("value", value_ty.as_str())],
        );
        facts.add_struct(
            string_shape,
            &[("span", "Span<'i>"), ("needs_unescape", "bool")],
        );
        facts.add_struct(number_shape, &[("span", "Span<'i>")]);
        facts.add_struct(bool_shape, &[("value", "bool")]);
        facts.add_struct(null_shape, &[]);
        facts
    }

    fn pascal_case(input: &str) -> String {
        let mut out = String::new();
        let mut upper = true;
        for ch in input.chars() {
            if ch == '_' || ch == '-' {
                upper = true;
                continue;
            }
            if upper {
                out.extend(ch.to_uppercase());
                upper = false;
            } else {
                out.push(ch);
            }
        }
        out
    }
}

pub mod recognizers {
    use super::*;

    pub fn derive_recognizers(grammar: &GrammarIr) -> Vec<Recognizer> {
        let mut present = HashSet::new();
        for expr in &grammar.exprs {
            match &expr.kind {
                ExprKind::Literal { bytes, .. } if bytes.len() == 1 => {
                    let byte = bytes[0];
                    if matches!(byte, b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"') {
                        present.insert(byte);
                    }
                }
                ExprKind::Regex { pattern } if pattern.starts_with('"') => {
                    present.insert(b'"');
                }
                _ => {}
            }
        }
        let mut bytes = present.into_iter().collect::<Vec<_>>();
        bytes.sort_unstable();
        if bytes.is_empty() {
            return Vec::new();
        }
        vec![Recognizer::SimdScan {
            mode: SimdMode::Exact,
            alphabet: StructuralAlphabet { bytes },
            site: SimdSite::PreEntry,
        }]
    }

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct TargetFeatures {
        pub avx512bw: bool,
        pub collapsed_stage_author_declared: bool,
        pub direct_only_output: bool,
        pub retained_api_consumer: bool,
    }

    impl TargetFeatures {
        pub fn host() -> Self {
            Self {
                avx512bw: cfg!(all(target_arch = "x86_64", target_feature = "avx512bw")),
                collapsed_stage_author_declared: false,
                direct_only_output: false,
                retained_api_consumer: true,
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct BackendShapePlan {
        pub backend_shape: HashMap<ir::RuleId, BackendShape>,
        pub cost_facts: HashMap<ir::RuleId, CostFacts>,
        pub diagnostics: Vec<diagnostics::PassDiagnostic>,
    }

    pub fn derive_backend_shape(
        grammar: &GrammarIr,
        backend: &BackendIr,
        layout: &LayoutFacts,
        target: TargetFeatures,
    ) -> HashMap<ir::RuleId, BackendShape> {
        derive_backend_shape_with_diagnostics(grammar, backend, layout, target).backend_shape
    }

    pub fn derive_backend_shape_with_diagnostics(
        grammar: &GrammarIr,
        backend: &BackendIr,
        layout: &LayoutFacts,
        target: TargetFeatures,
    ) -> BackendShapePlan {
        let mut backend_shape = HashMap::with_capacity(grammar.rules.len());
        let mut cost_facts = HashMap::with_capacity(grammar.rules.len());
        let mut diagnostics = Vec::new();

        for rule in &grammar.rules {
            let backend_rule = backend.rules.get(rule.id.0);
            let (chosen, rationale, priority_fired) = choose_backend_shape(
                grammar,
                rule,
                backend_rule,
                layout,
                target,
                &mut diagnostics,
            );
            let rejected = rejected_alternatives(backend_rule, chosen, target);
            let capacity_policy = capacity_policy(backend_rule, chosen);
            let facts = CostFacts {
                rule_id: rule.id,
                chosen,
                rationale,
                rejected,
                priority_fired,
                capacity_policy,
            };
            if facts.rejected.len() < 4 || !has_measurement_evidence(&facts) {
                diagnostics.push(diagnostics::PassDiagnostic::cost_facts_missing_evidence(
                    rule.id,
                    "cost facts entry has no measurement-backed evidence for at least one audited decision",
                ));
            }
            if has_previously_regressed_evidence(&facts) {
                diagnostics.push(diagnostics::PassDiagnostic::dominated_alternative(
                    rule.id,
                    "previously regressed alternative retained as a rejected cost fact",
                ));
            }
            backend_shape.insert(rule.id, facts.chosen);
            cost_facts.insert(rule.id, facts);
        }

        BackendShapePlan {
            backend_shape,
            cost_facts,
            diagnostics,
        }
    }

    const PRIORITY_TABLE: [PriorityStep; 8] = PriorityStep::ALL;

    pub fn priority_steps() -> &'static [PriorityStep] {
        &PRIORITY_TABLE
    }

    fn choose_backend_shape(
        grammar: &GrammarIr,
        rule: &ir::Rule,
        backend_rule: Option<&BackendRule>,
        layout: &LayoutFacts,
        target: TargetFeatures,
        diagnostics: &mut Vec<diagnostics::PassDiagnostic>,
    ) -> (BackendShape, ShapeRationale, PriorityStep) {
        match backend_rule {
            Some(_) if requires_eager_tape(grammar, rule.body, layout) => (
                BackendShape::EagerTape,
                eager_rationale(grammar, rule.body, layout),
                PriorityStep::P1EagerForced,
            ),
            Some(rule_ir) if admits_sink_only(rule_ir, target) => (
                BackendShape::SinkOnly,
                ShapeRationale::DirectBuildNoConsumer,
                PriorityStep::P2SinkOnlyConsumer,
            ),
            Some(rule_ir) if admits_collapsed_stage(rule_ir, target) => {
                if target.collapsed_stage_author_declared {
                    (
                        BackendShape::CollapsedStage,
                        ShapeRationale::CollapsedStageAdmissible,
                        PriorityStep::P3CollapsedStage,
                    )
                } else {
                    diagnostics.push(diagnostics::PassDiagnostic::collapsed_stage_not_viable(
                        rule.id,
                        "missing per-grammar collapsed-stage assembly wrapper",
                    ));
                    (
                        BackendShape::OffsetTape,
                        ShapeRationale::DefaultOffsetTape,
                        PriorityStep::P7OffsetTapeDefault,
                    )
                }
            }
            Some(rule_ir) if prefers_event_tape(rule_ir) => (
                BackendShape::EventTape,
                ShapeRationale::EventTapeAltDensity,
                PriorityStep::P4EventTapeAltDensity,
            ),
            Some(_) => (
                BackendShape::OffsetTape,
                ShapeRationale::DefaultOffsetTape,
                PriorityStep::P7OffsetTapeDefault,
            ),
            None => {
                diagnostics.push(diagnostics::PassDiagnostic::backend_shape_inconsistent(
                    rule.id,
                    "grammar rule has no matching backend rule",
                ));
                (
                    BackendShape::EagerTape,
                    ShapeRationale::DefaultOffsetTape,
                    PriorityStep::P8EagerFallback,
                )
            }
        }
    }

    fn eager_rationale(
        grammar: &GrammarIr,
        expr_id: ExprId,
        layout: &LayoutFacts,
    ) -> ShapeRationale {
        if has_recovery_annotation(grammar, expr_id) {
            ShapeRationale::ErrorRecoveryRequired
        } else if has_parse_time_host_decode(grammar, expr_id) {
            ShapeRationale::HostFnParseTime
        } else if has_layout_policy(layout) {
            ShapeRationale::LayoutScopeWide
        } else {
            ShapeRationale::FirstSetOverlap
        }
    }

    fn rejected_alternatives(
        backend_rule: Option<&BackendRule>,
        chosen: BackendShape,
        target: TargetFeatures,
    ) -> Vec<RejectedAlternative> {
        all_backend_shapes()
            .iter()
            .copied()
            .filter(|shape| *shape != chosen)
            .map(|shape| {
                let evidence = redress_72_evidence(backend_rule, shape);
                let reason = if !evidence.is_empty() {
                    RejectionReason::PreviouslyRegressed
                } else {
                    rejection_reason(backend_rule, shape, target)
                };
                RejectedAlternative {
                    shape,
                    reason,
                    evidence,
                }
            })
            .collect()
    }

    fn rejection_reason(
        backend_rule: Option<&BackendRule>,
        shape: BackendShape,
        target: TargetFeatures,
    ) -> RejectionReason {
        match (backend_rule, shape) {
            (None, _) => RejectionReason::PreconditionUnmet,
            (Some(rule_ir), BackendShape::SinkOnly) if !admits_sink_only(rule_ir, target) => {
                RejectionReason::ConsumerMismatch
            }
            (Some(_), BackendShape::CollapsedStage) if !target.collapsed_stage_author_declared => {
                RejectionReason::AuthorWaiverAbsent
            }
            _ => RejectionReason::PreconditionUnmet,
        }
    }

    fn capacity_policy(
        backend_rule: Option<&BackendRule>,
        chosen: BackendShape,
    ) -> Option<CapacityPolicy> {
        let rule_ir = backend_rule?;
        if chosen == BackendShape::OffsetTape
            && contains_tape_kind(&rule_ir.expr, TapeKind::StringValue)
        {
            return Some(CapacityPolicy {
                tiny_string_cap: Some(16),
                container_initial_capacity: None,
            });
        }
        None
    }

    fn redress_72_evidence(
        backend_rule: Option<&BackendRule>,
        rejected_shape: BackendShape,
    ) -> Vec<Measurement> {
        let Some(rule_ir) = backend_rule else {
            return Vec::new();
        };
        if rejected_shape != BackendShape::SinkOnly
            || !contains_tape_kind(&rule_ir.expr, TapeKind::StringValue)
        {
            return Vec::new();
        }
        ["direct", "track2"]
            .into_iter()
            .map(|workload| Measurement {
                workload: workload.to_string(),
                throughput_mbps_x1000: None,
                cycles_per_byte_x1000: None,
                hot_leaf_count: None,
                source: EvidenceSource::RedressBackfill,
                source_ref: "REDRESS-72".to_string(),
            })
            .collect()
    }

    fn has_measurement_evidence(facts: &CostFacts) -> bool {
        facts
            .rejected
            .iter()
            .any(|alternative| !alternative.evidence.is_empty())
            || facts.capacity_policy.is_some()
    }

    fn has_previously_regressed_evidence(facts: &CostFacts) -> bool {
        facts.rejected.iter().any(|alternative| {
            alternative.reason == RejectionReason::PreviouslyRegressed
                && alternative
                    .evidence
                    .iter()
                    .any(|measurement| measurement.source == EvidenceSource::RedressBackfill)
        })
    }

    fn requires_eager_tape(grammar: &GrammarIr, expr_id: ExprId, layout: &LayoutFacts) -> bool {
        has_recovery_annotation(grammar, expr_id)
            || has_parse_time_host_decode(grammar, expr_id)
            || has_layout_policy(layout)
            || has_dispatch_overlap(grammar, expr_id)
    }

    fn has_recovery_annotation(grammar: &GrammarIr, expr_id: ExprId) -> bool {
        match &grammar.expr(expr_id).kind {
            ExprKind::Annotation { name, value } => {
                name.contains("recover")
                    || value
                        .as_deref()
                        .is_some_and(|value| value.contains("recover"))
            }
            ExprKind::Seq(children)
            | ExprKind::Alt {
                branches: children, ..
            } => children
                .iter()
                .copied()
                .any(|child| has_recovery_annotation(grammar, child)),
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                has_recovery_annotation(grammar, *body)
            }
            ExprKind::Literal { .. } | ExprKind::Regex { .. } | ExprKind::Ref { .. } => false,
        }
    }

    fn has_parse_time_host_decode(grammar: &GrammarIr, expr_id: ExprId) -> bool {
        match &grammar.expr(expr_id).kind {
            ExprKind::Annotation { name, value } => {
                let value = value.as_deref().unwrap_or_default();
                name.contains("host") && (value.contains("decode") || value.contains("parse"))
            }
            ExprKind::Seq(children)
            | ExprKind::Alt {
                branches: children, ..
            } => children
                .iter()
                .copied()
                .any(|child| has_parse_time_host_decode(grammar, child)),
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                has_parse_time_host_decode(grammar, *body)
            }
            ExprKind::Literal { .. } | ExprKind::Regex { .. } | ExprKind::Ref { .. } => false,
        }
    }

    fn has_layout_policy(layout: &LayoutFacts) -> bool {
        !layout.layout_policies.is_empty()
    }

    fn has_dispatch_overlap(grammar: &GrammarIr, expr_id: ExprId) -> bool {
        match &grammar.expr(expr_id).kind {
            ExprKind::Alt { branches, .. } => branches_overlap(grammar, branches),
            ExprKind::Seq(children) => children
                .iter()
                .copied()
                .any(|child| has_dispatch_overlap(grammar, child)),
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                has_dispatch_overlap(grammar, *body)
            }
            ExprKind::Literal { .. }
            | ExprKind::Regex { .. }
            | ExprKind::Ref { .. }
            | ExprKind::Annotation { .. } => false,
        }
    }

    fn branches_overlap(grammar: &GrammarIr, branches: &[ExprId]) -> bool {
        let mut seen = HashSet::new();
        for branch in branches {
            let Some(first) = first_bytes(grammar, *branch, 0) else {
                continue;
            };
            for byte in first.bytes {
                if !seen.insert(byte) {
                    return true;
                }
            }
        }
        false
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    struct FirstBytes {
        bytes: HashSet<u8>,
        nullable: bool,
    }

    fn first_bytes(grammar: &GrammarIr, expr_id: ExprId, depth: usize) -> Option<FirstBytes> {
        if depth > grammar.rules.len() + 1 {
            return None;
        }
        match &grammar.expr(expr_id).kind {
            ExprKind::Seq(children) => {
                let mut bytes = HashSet::new();
                let mut nullable = true;
                for child in children {
                    let child_first = first_bytes(grammar, *child, depth + 1)?;
                    bytes.extend(child_first.bytes);
                    if !child_first.nullable {
                        nullable = false;
                        break;
                    }
                }
                Some(FirstBytes { bytes, nullable })
            }
            ExprKind::Alt { branches, .. } => {
                let mut bytes = HashSet::new();
                let mut nullable = false;
                for branch in branches {
                    let branch_first = first_bytes(grammar, *branch, depth + 1)?;
                    bytes.extend(branch_first.bytes);
                    nullable |= branch_first.nullable;
                }
                Some(FirstBytes { bytes, nullable })
            }
            ExprKind::Repeat { body, min, .. } => {
                let mut first = first_bytes(grammar, *body, depth + 1)?;
                first.nullable |= *min == 0;
                Some(first)
            }
            ExprKind::Optional(body) => {
                let mut first = first_bytes(grammar, *body, depth + 1)?;
                first.nullable = true;
                Some(first)
            }
            ExprKind::Literal { bytes, .. } => {
                let mut set = HashSet::new();
                if let Some(byte) = bytes.first().copied() {
                    set.insert(byte);
                }
                Some(FirstBytes {
                    bytes: set,
                    nullable: bytes.is_empty(),
                })
            }
            ExprKind::Regex { pattern } => regex_first_bytes(pattern),
            ExprKind::Ref {
                target: Some(target),
                ..
            } => grammar
                .rule(*target)
                .and_then(|rule| first_bytes(grammar, rule.body, depth + 1)),
            ExprKind::Ref { target: None, .. } => None,
            ExprKind::Annotation { .. } => Some(FirstBytes {
                bytes: HashSet::new(),
                nullable: true,
            }),
        }
    }

    fn regex_first_bytes(pattern: &str) -> Option<FirstBytes> {
        let mut bytes = HashSet::new();
        let nullable = pattern == r"[ \t\n\r]*";
        match pattern {
            r"[ \t\n\r]*" => {
                bytes.extend([b' ', b'\t', b'\n', b'\r']);
            }
            r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+\-]?[0-9]+)?" => {
                bytes.insert(b'-');
                bytes.extend(b'0'..=b'9');
            }
            pattern if pattern.starts_with('"') => {
                bytes.insert(b'"');
            }
            _ => return None,
        }
        Some(FirstBytes { bytes, nullable })
    }

    fn admits_sink_only(rule_ir: &BackendRule, target: TargetFeatures) -> bool {
        target.direct_only_output
            && !target.retained_api_consumer
            && contains_direct_build(&rule_ir.expr)
    }

    fn admits_collapsed_stage(rule_ir: &BackendRule, target: TargetFeatures) -> bool {
        target.avx512bw && matches!(rule_ir.expr, BackendExpr::Entry(_))
    }

    fn prefers_event_tape(rule_ir: &BackendRule) -> bool {
        alt_branch_count(&rule_ir.expr) >= 8
    }

    fn contains_direct_build(expr: &BackendExpr) -> bool {
        match expr {
            BackendExpr::DirectBuild { .. } => true,
            BackendExpr::Entry(inner)
            | BackendExpr::OptionalBranch(inner)
            | BackendExpr::RepeatLoop { body: inner, .. } => contains_direct_build(inner),
            BackendExpr::Seq(children)
            | BackendExpr::Alt {
                branches: children, ..
            } => children.iter().any(contains_direct_build),
            BackendExpr::ByteLiteral(_)
            | BackendExpr::RegexProgram { .. }
            | BackendExpr::CallRule { .. }
            | BackendExpr::SpanMark { .. }
            | BackendExpr::TapeEmit { .. }
            | BackendExpr::ValueProject { .. }
            | BackendExpr::Return => false,
        }
    }

    fn contains_tape_kind(expr: &BackendExpr, needle: TapeKind) -> bool {
        match expr {
            BackendExpr::TapeEmit { kind } => *kind == needle,
            BackendExpr::Entry(inner)
            | BackendExpr::OptionalBranch(inner)
            | BackendExpr::RepeatLoop { body: inner, .. } => contains_tape_kind(inner, needle),
            BackendExpr::Seq(children)
            | BackendExpr::Alt {
                branches: children, ..
            } => children
                .iter()
                .any(|child| contains_tape_kind(child, needle)),
            BackendExpr::ByteLiteral(_)
            | BackendExpr::RegexProgram { .. }
            | BackendExpr::CallRule { .. }
            | BackendExpr::SpanMark { .. }
            | BackendExpr::DirectBuild { .. }
            | BackendExpr::ValueProject { .. }
            | BackendExpr::Return => false,
        }
    }

    fn alt_branch_count(expr: &BackendExpr) -> usize {
        match expr {
            BackendExpr::Alt { branches, .. } => branches.len(),
            BackendExpr::Entry(inner)
            | BackendExpr::OptionalBranch(inner)
            | BackendExpr::RepeatLoop { body: inner, .. } => alt_branch_count(inner),
            BackendExpr::Seq(children) => children.iter().map(alt_branch_count).max().unwrap_or(0),
            BackendExpr::ByteLiteral(_)
            | BackendExpr::RegexProgram { .. }
            | BackendExpr::CallRule { .. }
            | BackendExpr::SpanMark { .. }
            | BackendExpr::TapeEmit { .. }
            | BackendExpr::DirectBuild { .. }
            | BackendExpr::ValueProject { .. }
            | BackendExpr::Return => 0,
        }
    }

    pub mod hot_path {
        use super::*;

        #[derive(Clone, Debug, PartialEq, Eq)]
        pub struct HotPathFact {
            pub force_inline: bool,
            pub max_inline_size_hint: usize,
        }

        #[derive(Clone, Debug, Default, PartialEq, Eq)]
        pub struct PriorBenchProfile {
            pub hot_rule_names: Vec<String>,
        }

        pub fn derive_hot_path(
            grammar_ir: &GrammarIr,
            entry_rule: Option<ir::RuleId>,
            profile_hints: Option<&PriorBenchProfile>,
        ) -> HashMap<ir::RuleId, HotPathFact> {
            let mut hot = HashMap::new();
            if let Some(rule_id) = entry_rule {
                mark_transitive(grammar_ir, rule_id, 0, &mut hot);
            } else if let Some(rule) = grammar_ir.rules.last() {
                mark_transitive(grammar_ir, rule.id, 0, &mut hot);
            }
            if let Some(profile) = profile_hints {
                for name in &profile.hot_rule_names {
                    if let Some(rule) = grammar_ir.rule_by_name(name) {
                        hot.insert(rule.id, hot_fact());
                    }
                }
            }
            hot
        }

        fn mark_transitive(
            grammar_ir: &GrammarIr,
            rule_id: ir::RuleId,
            depth: usize,
            hot: &mut HashMap<ir::RuleId, HotPathFact>,
        ) {
            if depth > 5 || hot.insert(rule_id, hot_fact()).is_some() {
                return;
            }
            let Some(rule) = grammar_ir.rule(rule_id) else {
                return;
            };
            mark_expr(grammar_ir, rule.body, depth, hot);
        }

        fn mark_expr(
            grammar_ir: &GrammarIr,
            expr_id: ExprId,
            depth: usize,
            hot: &mut HashMap<ir::RuleId, HotPathFact>,
        ) {
            match &grammar_ir.expr(expr_id).kind {
                ExprKind::Seq(children) => {
                    for child in children {
                        mark_expr(grammar_ir, *child, depth, hot);
                    }
                }
                ExprKind::Alt { branches, .. } => {
                    for branch in branches {
                        mark_expr(grammar_ir, *branch, depth, hot);
                    }
                }
                ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                    mark_expr(grammar_ir, *body, depth, hot);
                }
                ExprKind::Ref {
                    target: Some(target),
                    ..
                } => mark_transitive(grammar_ir, *target, depth + 1, hot),
                ExprKind::Literal { .. }
                | ExprKind::Regex { .. }
                | ExprKind::Ref { target: None, .. }
                | ExprKind::Annotation { .. } => {}
            }
        }

        fn hot_fact() -> HotPathFact {
            HotPathFact {
                force_inline: true,
                max_inline_size_hint: 20 * 1024,
            }
        }
    }
}

pub mod extract {
    use super::*;

    #[derive(Clone, Debug, Default, PartialEq, Eq)]
    pub struct MaterializationPlan {
        pub descriptors: HashMap<ir::RuleId, MaterializationDescriptor>,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct MaterializationDescriptor {
        pub(crate) kind: TapeKind,
        pub(crate) label: String,
        pub(crate) shape: String,
        pub(crate) fields: Vec<DirectBuildField>,
    }

    pub fn derive_materialization_plan(grammar: &GrammarIr) -> MaterializationPlan {
        let mut descriptors = HashMap::new();
        let prefix = pascal_case(&grammar.name);
        let roles = derive_materialization_roles(grammar);

        if let Some(rule) = roles.container {
            if let Some(pair_rule) = roles.pair.and_then(|id| grammar.rule(id)) {
                descriptors.insert(
                    rule,
                    MaterializationDescriptor {
                        kind: TapeKind::Container,
                        label: "object".to_string(),
                        shape: format!("{prefix}Object"),
                        fields: vec![DirectBuildField {
                            name: "members".to_string(),
                            source: DirectBuildSource::RepeatedRule {
                                rule: pair_rule.name.clone(),
                            },
                            target: None,
                        }],
                    },
                );
            }
        }

        if let Some(rule) = roles.sequence {
            if let Some(value_rule) = roles.value.and_then(|id| grammar.rule(id)) {
                descriptors.insert(
                    rule,
                    MaterializationDescriptor {
                        kind: TapeKind::Sequence,
                        label: "array".to_string(),
                        shape: format!("{prefix}Array"),
                        fields: vec![DirectBuildField {
                            name: "elements".to_string(),
                            source: DirectBuildSource::RepeatedRule {
                                rule: value_rule.name.clone(),
                            },
                            target: None,
                        }],
                    },
                );
            }
        }

        if let Some(rule) = roles.pair {
            if let (Some(string_rule), Some(value_rule)) = (
                roles.string.and_then(|id| grammar.rule(id)),
                roles.value.and_then(|id| grammar.rule(id)),
            ) {
                descriptors.insert(
                    rule,
                    MaterializationDescriptor {
                        kind: TapeKind::KeyValuePair,
                        label: "pair".to_string(),
                        shape: format!("{prefix}Pair"),
                        fields: vec![
                            DirectBuildField {
                                name: "key".to_string(),
                                source: DirectBuildSource::ChildRule {
                                    rule: string_rule.name.clone(),
                                },
                                target: None,
                            },
                            DirectBuildField {
                                name: "value".to_string(),
                                source: DirectBuildSource::ChildRule {
                                    rule: value_rule.name.clone(),
                                },
                                target: None,
                            },
                        ],
                    },
                );
            }
        }

        if let Some(rule) = roles.string {
            descriptors.insert(
                rule,
                MaterializationDescriptor {
                    kind: TapeKind::StringValue,
                    label: "string".to_string(),
                    shape: format!("{prefix}String"),
                    fields: vec![DirectBuildField {
                        name: "span".to_string(),
                        source: DirectBuildSource::Span {
                            label: "string".to_string(),
                        },
                        target: None,
                    }],
                },
            );
        }

        if let Some(rule) = roles.number {
            descriptors.insert(
                rule,
                MaterializationDescriptor {
                    kind: TapeKind::NumberValue,
                    label: "number".to_string(),
                    shape: format!("{prefix}Number"),
                    fields: vec![DirectBuildField {
                        name: "span".to_string(),
                        source: DirectBuildSource::Span {
                            label: "number".to_string(),
                        },
                        target: None,
                    }],
                },
            );
        }

        if let Some(rule) = roles.bool_value {
            descriptors.insert(
                rule,
                MaterializationDescriptor {
                    kind: TapeKind::BoolValue,
                    label: "bool".to_string(),
                    shape: format!("{prefix}Bool"),
                    fields: vec![DirectBuildField {
                        name: "value".to_string(),
                        source: DirectBuildSource::Literal { bytes: Vec::new() },
                        target: None,
                    }],
                },
            );
        }

        if let Some(rule) = roles.null_value {
            descriptors.insert(
                rule,
                MaterializationDescriptor {
                    kind: TapeKind::NullValue,
                    label: "null".to_string(),
                    shape: format!("{prefix}Null"),
                    fields: Vec::new(),
                },
            );
        }

        MaterializationPlan { descriptors }
    }

    pub fn single_plan(
        grammar: &GrammarIr,
        _layout_facts: &LayoutFacts,
        entry_rule: ir::RuleId,
        shape_facts: ShapeFacts,
        recognizers: Vec<Recognizer>,
        materialization: &MaterializationPlan,
    ) -> Result<BackendIr, PassError> {
        let mut rules = Vec::with_capacity(grammar.rules.len());
        for rule in &grammar.rules {
            let expr = lower_expr(grammar, rule.body);
            let expr = materialize_rule(rule.id, expr, materialization);
            let expr = if rule.id == entry_rule {
                BackendExpr::Entry(Box::new(expr))
            } else {
                expr
            };
            rules.push(BackendRule {
                name: rule.name.clone(),
                expr,
            });
        }

        Ok(BackendIr {
            grammar_name: grammar.name.clone(),
            entry_rule: grammar
                .rule(entry_rule)
                .map(|rule| rule.name.clone())
                .ok_or_else(|| PassError::MissingEntry(grammar.name.clone()))?,
            recognizers,
            rules,
            shape_facts,
        })
    }

    fn lower_expr(grammar: &GrammarIr, expr_id: ExprId) -> BackendExpr {
        match &grammar.expr(expr_id).kind {
            ExprKind::Seq(children) => BackendExpr::Seq(
                children
                    .iter()
                    .map(|child| lower_expr(grammar, *child))
                    .collect(),
            ),
            ExprKind::Alt { branches, mode } => BackendExpr::Alt {
                mode: *mode,
                branches: branches
                    .iter()
                    .map(|branch| lower_expr(grammar, *branch))
                    .collect(),
            },
            ExprKind::Repeat { body, min, .. } => BackendExpr::RepeatLoop {
                body: Box::new(lower_expr(grammar, *body)),
                min: *min,
            },
            ExprKind::Optional(body) => {
                BackendExpr::OptionalBranch(Box::new(lower_expr(grammar, *body)))
            }
            ExprKind::Literal { bytes, .. } => BackendExpr::ByteLiteral(bytes.clone()),
            ExprKind::Regex { pattern } => BackendExpr::RegexProgram {
                pattern: pattern.clone(),
                span_kind: span_kind(pattern),
            },
            ExprKind::Ref { name, .. } => BackendExpr::CallRule {
                callee: name.clone(),
            },
            ExprKind::Annotation { .. } => BackendExpr::Seq(Vec::new()),
        }
    }

    fn materialize_rule(
        rule_id: ir::RuleId,
        body: BackendExpr,
        materialization: &MaterializationPlan,
    ) -> BackendExpr {
        let Some(descriptor) = materialization.descriptors.get(&rule_id) else {
            return body;
        };
        BackendExpr::Seq(vec![
            BackendExpr::SpanMark {
                kind: SpanMarkKind::Start,
                label: descriptor.label.clone(),
            },
            body,
            BackendExpr::SpanMark {
                kind: SpanMarkKind::End,
                label: descriptor.label.clone(),
            },
            BackendExpr::TapeEmit {
                kind: descriptor.kind,
            },
            BackendExpr::DirectBuild {
                shape: descriptor.shape.clone(),
                fields: descriptor.fields.clone(),
            },
            BackendExpr::Return,
        ])
    }

    #[derive(Clone, Copy, Debug, Default)]
    struct MaterializationRoles {
        container: Option<ir::RuleId>,
        sequence: Option<ir::RuleId>,
        pair: Option<ir::RuleId>,
        string: Option<ir::RuleId>,
        number: Option<ir::RuleId>,
        bool_value: Option<ir::RuleId>,
        null_value: Option<ir::RuleId>,
        value: Option<ir::RuleId>,
    }

    fn derive_materialization_roles(grammar: &GrammarIr) -> MaterializationRoles {
        let string = grammar
            .rules
            .iter()
            .find(|rule| rule_has_regex_kind(grammar, rule, SpanKind::String))
            .map(|rule| rule.id);
        let number = grammar
            .rules
            .iter()
            .find(|rule| rule_has_regex_kind(grammar, rule, SpanKind::Number))
            .map(|rule| rule.id);
        let bool_value = grammar
            .rules
            .iter()
            .find(|rule| {
                let literals = direct_literals(grammar, rule.body);
                literals.iter().any(|literal| literal.as_slice() == b"true")
                    && literals
                        .iter()
                        .any(|literal| literal.as_slice() == b"false")
            })
            .map(|rule| rule.id);
        let null_value = grammar
            .rules
            .iter()
            .find(|rule| {
                let literals = direct_literals(grammar, rule.body);
                literals.len() == 1 && literals[0].as_slice() == b"null"
            })
            .map(|rule| rule.id);
        let container = grammar
            .rules
            .iter()
            .find(|rule| {
                rule_has_direct_literal(grammar, rule, b"{")
                    && rule_has_direct_literal(grammar, rule, b"}")
            })
            .map(|rule| rule.id);
        let sequence = grammar
            .rules
            .iter()
            .find(|rule| {
                rule_has_direct_literal(grammar, rule, b"[")
                    && rule_has_direct_literal(grammar, rule, b"]")
            })
            .map(|rule| rule.id);

        let value_targets = [container, sequence, string, number, bool_value, null_value]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        let value = grammar
            .rules
            .iter()
            .find(|rule| {
                let refs = direct_rule_refs(grammar, rule.body);
                value_targets
                    .iter()
                    .filter(|target| refs.contains(target))
                    .count()
                    >= 4
            })
            .map(|rule| rule.id);

        let pair = match (string, value) {
            (Some(string_rule), Some(value_rule)) => grammar
                .rules
                .iter()
                .find(|rule| {
                    let refs = direct_rule_refs(grammar, rule.body);
                    refs.contains(&string_rule)
                        && refs.contains(&value_rule)
                        && rule_has_literal(grammar, rule, b":")
                })
                .map(|rule| rule.id),
            _ => None,
        };

        MaterializationRoles {
            container,
            sequence,
            pair,
            string,
            number,
            bool_value,
            null_value,
            value,
        }
    }

    fn rule_has_regex_kind(grammar: &GrammarIr, rule: &ir::Rule, kind: SpanKind) -> bool {
        expr_has_regex_kind(grammar, rule.body, kind)
    }

    fn expr_has_regex_kind(grammar: &GrammarIr, expr_id: ExprId, kind: SpanKind) -> bool {
        match &grammar.expr(expr_id).kind {
            ExprKind::Seq(children) => children
                .iter()
                .any(|child| expr_has_regex_kind(grammar, *child, kind)),
            ExprKind::Alt { branches, .. } => branches
                .iter()
                .any(|branch| expr_has_regex_kind(grammar, *branch, kind)),
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                expr_has_regex_kind(grammar, *body, kind)
            }
            ExprKind::Regex { pattern } => span_kind(pattern) == kind,
            ExprKind::Literal { .. } | ExprKind::Ref { .. } | ExprKind::Annotation { .. } => false,
        }
    }

    fn rule_has_direct_literal(grammar: &GrammarIr, rule: &ir::Rule, literal: &[u8]) -> bool {
        direct_literals(grammar, rule.body)
            .iter()
            .any(|candidate| candidate.as_slice() == literal)
    }

    fn rule_has_literal(grammar: &GrammarIr, rule: &ir::Rule, literal: &[u8]) -> bool {
        let mut visited_rules = HashSet::new();
        expr_has_literal(grammar, rule.body, literal, &mut visited_rules)
    }

    fn expr_has_literal(
        grammar: &GrammarIr,
        expr_id: ExprId,
        literal: &[u8],
        visited_rules: &mut HashSet<ir::RuleId>,
    ) -> bool {
        match &grammar.expr(expr_id).kind {
            ExprKind::Seq(children) => children
                .iter()
                .any(|child| expr_has_literal(grammar, *child, literal, visited_rules)),
            ExprKind::Alt { branches, .. } => branches
                .iter()
                .any(|branch| expr_has_literal(grammar, *branch, literal, visited_rules)),
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                expr_has_literal(grammar, *body, literal, visited_rules)
            }
            ExprKind::Literal { bytes, .. } => bytes == literal,
            ExprKind::Ref {
                target: Some(rule), ..
            } => {
                if !visited_rules.insert(*rule) {
                    return false;
                }
                grammar.rule(*rule).is_some_and(|rule| {
                    expr_has_literal(grammar, rule.body, literal, visited_rules)
                })
            }
            ExprKind::Ref { target: None, .. }
            | ExprKind::Regex { .. }
            | ExprKind::Annotation { .. } => false,
        }
    }

    fn direct_rule_refs(grammar: &GrammarIr, expr_id: ExprId) -> HashSet<ir::RuleId> {
        let mut refs = HashSet::new();
        collect_direct_rule_refs(grammar, expr_id, &mut refs);
        refs
    }

    fn collect_direct_rule_refs(
        grammar: &GrammarIr,
        expr_id: ExprId,
        refs: &mut HashSet<ir::RuleId>,
    ) {
        match &grammar.expr(expr_id).kind {
            ExprKind::Seq(children) => {
                for child in children {
                    collect_direct_rule_refs(grammar, *child, refs);
                }
            }
            ExprKind::Alt { branches, .. } => {
                for branch in branches {
                    collect_direct_rule_refs(grammar, *branch, refs);
                }
            }
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                collect_direct_rule_refs(grammar, *body, refs);
            }
            ExprKind::Ref {
                target: Some(rule), ..
            } => {
                refs.insert(*rule);
            }
            ExprKind::Ref { target: None, .. }
            | ExprKind::Literal { .. }
            | ExprKind::Regex { .. }
            | ExprKind::Annotation { .. } => {}
        }
    }

    fn direct_literals(grammar: &GrammarIr, expr_id: ExprId) -> Vec<Vec<u8>> {
        let mut literals = Vec::new();
        collect_direct_literals(grammar, expr_id, &mut literals);
        literals
    }

    fn collect_direct_literals(grammar: &GrammarIr, expr_id: ExprId, literals: &mut Vec<Vec<u8>>) {
        match &grammar.expr(expr_id).kind {
            ExprKind::Seq(children) => {
                for child in children {
                    collect_direct_literals(grammar, *child, literals);
                }
            }
            ExprKind::Alt { branches, .. } => {
                for branch in branches {
                    collect_direct_literals(grammar, *branch, literals);
                }
            }
            ExprKind::Repeat { body, .. } | ExprKind::Optional(body) => {
                collect_direct_literals(grammar, *body, literals);
            }
            ExprKind::Literal { bytes, .. } => literals.push(bytes.clone()),
            ExprKind::Ref { .. } | ExprKind::Regex { .. } | ExprKind::Annotation { .. } => {}
        }
    }

    fn pascal_case(input: &str) -> String {
        let mut out = String::new();
        let mut upper = true;
        for ch in input.chars() {
            if ch == '_' || ch == '-' {
                upper = true;
                continue;
            }
            if upper {
                out.extend(ch.to_uppercase());
                upper = false;
            } else {
                out.push(ch);
            }
        }
        out
    }

    fn span_kind(pattern: &str) -> SpanKind {
        if pattern == r"[ \t\n\r]*" {
            SpanKind::Whitespace
        } else if pattern.starts_with('"') {
            SpanKind::String
        } else {
            SpanKind::Number
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const JSON_GRAMMAR: &str = include_str!("../../../grammars/json.bbnf");

    #[test]
    fn compiles_json_to_single_plan_bir() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let output = compile(&grammar).unwrap();

        assert_eq!(output.backend_ir.entry_rule, "json");
        assert_eq!(output.backend_ir.recognizers.len(), 1);
        assert_eq!(output.backend_ir.rules.len(), 15);
        assert!(output.layout_facts.layout_policies.is_empty());
        assert!(output
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code() == "BBNF-COSTFACTS-MISSING-EVIDENCE"));
        assert_eq!(output.layout_facts.backend_shape.len(), 15);
        assert_eq!(output.layout_facts.cost_facts.len(), 15);
        assert!(output
            .layout_facts
            .backend_shape
            .values()
            .all(|shape| *shape == BackendShape::OffsetTape));
        let object = output
            .backend_ir
            .rules
            .iter()
            .find(|rule| rule.name == "object")
            .unwrap();
        assert!(contains_tape_emit_and_direct_build(&object.expr));
    }

    #[test]
    fn cost_facts_populate_direct_build_rules_with_redress_evidence() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let output = compile(&grammar).unwrap();
        let direct_build_rule_ids = output
            .backend_ir
            .rules
            .iter()
            .enumerate()
            .filter_map(|(index, rule)| {
                contains_tape_emit_and_direct_build(&rule.expr).then_some(ir::RuleId(index))
            })
            .collect::<Vec<_>>();

        assert_eq!(direct_build_rule_ids.len(), 7);
        for rule_id in &direct_build_rule_ids {
            let facts = output.layout_facts.cost_facts.get(rule_id).unwrap();
            assert_eq!(
                output.layout_facts.backend_shape.get(rule_id),
                Some(&facts.chosen)
            );
            assert!(facts.rejected.len() >= 4);
        }
        assert!(output.layout_facts.cost_facts.values().any(|facts| {
            facts
                .capacity_policy
                .as_ref()
                .is_some_and(|policy| policy.tiny_string_cap == Some(16))
        }));
        assert!(output.layout_facts.cost_facts.values().any(|facts| {
            facts.rejected.iter().any(|alternative| {
                alternative.reason == ir::RejectionReason::PreviouslyRegressed
                    && alternative.evidence.iter().any(|measurement| {
                        measurement.source == ir::EvidenceSource::RedressBackfill
                    })
            })
        }));
        assert!(output
            .diagnostics
            .iter()
            .any(|diagnostic| { diagnostic.code() == "BBNF-DOMINATED-ALTERNATIVE" }));
        assert!(output
            .diagnostics
            .iter()
            .any(|diagnostic| { diagnostic.code() == "BBNF-COSTFACTS-MISSING-EVIDENCE" }));
    }

    #[test]
    fn priority_table_matches_costfacts_priority_enum() {
        assert_eq!(
            recognizers::priority_steps(),
            ir::PriorityStep::ALL.as_slice()
        );
    }

    #[test]
    fn infers_value_as_alt_of_named_rules() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let facts = layout::types::infer(&grammar).unwrap();
        let value = grammar.rule_by_name("value").unwrap();
        let Type::Seq(items) = facts.rule_types.get(&value.id).unwrap() else {
            panic!("value is a sequence with explicit ws around alt");
        };
        assert_eq!(items.len(), 3);
        assert!(matches!(items[1], Type::Alt(_)));
    }

    #[test]
    fn json_shapes_are_curated() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let materialization = extract::derive_materialization_plan(&grammar);
        let shapes = shapes::derive_shape_facts(&grammar, &materialization);
        assert_eq!(shapes.shapes.len(), 9);
    }

    #[test]
    fn materializes_json_value_rules_with_neutral_tape_kinds() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let output = compile(&grammar).unwrap();

        let expected = [
            (
                "object",
                TapeKind::Container,
                "JsonObject",
                &["members"][..],
            ),
            ("array", TapeKind::Sequence, "JsonArray", &["elements"][..]),
            (
                "pair",
                TapeKind::KeyValuePair,
                "JsonPair",
                &["key", "value"][..],
            ),
            ("string", TapeKind::StringValue, "JsonString", &["span"][..]),
            ("number", TapeKind::NumberValue, "JsonNumber", &["span"][..]),
            ("bool", TapeKind::BoolValue, "JsonBool", &["value"][..]),
            ("null", TapeKind::NullValue, "JsonNull", &[][..]),
        ];

        for (rule_name, expected_kind, expected_shape, expected_fields) in expected {
            let rule = output
                .backend_ir
                .rules
                .iter()
                .find(|rule| rule.name == rule_name)
                .unwrap_or_else(|| panic!("missing rule {rule_name}"));
            let (kind, shape, fields) = materialization_of(&rule.expr)
                .unwrap_or_else(|| panic!("missing materialization for rule {rule_name}"));
            assert_eq!(kind, expected_kind, "{rule_name} tape kind");
            assert_eq!(shape, expected_shape, "{rule_name} shape");
            assert_eq!(fields, expected_fields, "{rule_name} fields");
        }
    }

    #[test]
    fn materialization_derives_from_structure_not_rule_names() {
        let source = r#"
nothing   = "null" ;
truth     = "true" | "false" ;
digits    = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;
quoted    = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
space     = /[ \t\n\r]*/ ;
delimiter = "," space ;
split     = ":" space ;
item      = space (map | list | quoted | digits | truth | nothing) space ;
entry     = quoted space split item ;
entries   = entry (delimiter entry)* ;
many      = entries? ;
members   = (item (delimiter item)*)? ;
list      = "[" space members "]" ;
map       = "{" space many "}" ;
sample_json = space item space ;
"#;
        let grammar = grammar::parse_grammar("sample_json", source).unwrap();
        let output = compile(&grammar).unwrap();

        let expected = [
            ("map", TapeKind::Container, "SampleJsonObject"),
            ("list", TapeKind::Sequence, "SampleJsonArray"),
            ("entry", TapeKind::KeyValuePair, "SampleJsonPair"),
            ("quoted", TapeKind::StringValue, "SampleJsonString"),
            ("digits", TapeKind::NumberValue, "SampleJsonNumber"),
            ("truth", TapeKind::BoolValue, "SampleJsonBool"),
            ("nothing", TapeKind::NullValue, "SampleJsonNull"),
        ];

        for (rule_name, expected_kind, expected_shape) in expected {
            let rule = output
                .backend_ir
                .rules
                .iter()
                .find(|rule| rule.name == rule_name)
                .unwrap_or_else(|| panic!("missing rule {rule_name}"));
            let (kind, shape, _) = materialization_of(&rule.expr)
                .unwrap_or_else(|| panic!("missing materialization for rule {rule_name}"));
            assert_eq!(kind, expected_kind, "{rule_name} tape kind");
            assert_eq!(shape, expected_shape, "{rule_name} shape");
        }
    }

    #[test]
    fn collapsed_stage_without_author_falls_back_with_diagnostic() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let output = compile(&grammar).unwrap();
        let plan = recognizers::derive_backend_shape_with_diagnostics(
            &output.grammar,
            &output.backend_ir,
            &output.layout_facts,
            recognizers::TargetFeatures {
                avx512bw: true,
                collapsed_stage_author_declared: false,
                direct_only_output: false,
                retained_api_consumer: true,
            },
        );

        let entry = derive_entry_rule(&output.grammar).unwrap();
        assert_eq!(
            plan.backend_shape.get(&entry).copied(),
            Some(BackendShape::OffsetTape)
        );
        assert!(plan.diagnostics.iter().any(|diagnostic| {
            diagnostic.code() == "BBNF-COLLAPSEDSTAGE-NOT-VIABLE" && diagnostic.rule == Some(entry)
        }));
    }

    fn contains_tape_emit_and_direct_build(expr: &BackendExpr) -> bool {
        match expr {
            BackendExpr::Seq(children) => {
                children
                    .iter()
                    .any(|child| matches!(child, BackendExpr::TapeEmit { .. }))
                    && children
                        .iter()
                        .any(|child| matches!(child, BackendExpr::DirectBuild { .. }))
            }
            BackendExpr::Entry(inner)
            | BackendExpr::OptionalBranch(inner)
            | BackendExpr::RepeatLoop { body: inner, .. } => {
                contains_tape_emit_and_direct_build(inner)
            }
            BackendExpr::Alt { branches, .. } => {
                branches.iter().any(contains_tape_emit_and_direct_build)
            }
            _ => false,
        }
    }

    fn materialization_of(expr: &BackendExpr) -> Option<(TapeKind, &str, Vec<&str>)> {
        match expr {
            BackendExpr::Seq(children) => {
                let kind = children.iter().find_map(|child| match child {
                    BackendExpr::TapeEmit { kind } => Some(*kind),
                    _ => None,
                })?;
                let (shape, fields) = children.iter().find_map(|child| match child {
                    BackendExpr::DirectBuild { shape, fields } => Some((
                        shape.as_str(),
                        fields
                            .iter()
                            .map(|field| field.name.as_str())
                            .collect::<Vec<_>>(),
                    )),
                    _ => None,
                })?;
                Some((kind, shape, fields))
            }
            BackendExpr::Entry(inner)
            | BackendExpr::OptionalBranch(inner)
            | BackendExpr::RepeatLoop { body: inner, .. } => materialization_of(inner),
            BackendExpr::Alt { branches, .. } => branches.iter().find_map(materialization_of),
            _ => None,
        }
    }
}
