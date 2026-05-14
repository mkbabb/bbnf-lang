use ir::{
    BackendExpr, BackendIr, BackendRule, BackendShape, DirectBuildField, DirectBuildSource, ExprId,
    ExprKind, GrammarIr, Recognizer, ShapeFacts, SimdMode, SimdSite, SourceSpan, SpanKind,
    SpanMarkKind, StructuralAlphabet, TapeKind, ValidationError,
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
    let type_facts = layout::types::infer(&normalized)?;
    let mut layout_facts = layout::run(&normalized, type_facts);
    let shape_facts = shapes::shapes_for_json();
    let recognizers = recognizers::nominate_json(&normalized);
    let backend_ir = extract::single_plan(&normalized, &layout_facts, shape_facts, recognizers)?;
    let shape_plan = recognizers::derive_backend_shape_with_diagnostics(
        &normalized,
        &backend_ir,
        &layout_facts,
        recognizers::TargetFeatures::host(),
    );
    layout_facts.backend_shape = shape_plan.backend_shape;
    Ok(PipelineOutput {
        grammar: normalized,
        layout_facts,
        backend_ir,
        diagnostics: shape_plan.diagnostics,
    })
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
}

pub mod layout {
    use super::*;

    pub fn run(grammar: &GrammarIr, type_facts: types::TypeFacts) -> LayoutFacts {
        LayoutFacts {
            rule_types: type_facts.rule_types,
            node_types: type_facts.node_types,
            layout_policies: HashMap::new(),
            hot_call_graph: recognizers::hot_path::derive_hot_path(grammar, None),
            backend_shape: HashMap::new(),
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

    pub fn shapes_for_json() -> ShapeFacts {
        let mut facts = ShapeFacts::new();
        facts.add_struct("JsonRoot", &[("value", "JsonValue<'i>")]);
        facts.add_enum(
            "JsonValue",
            &[
                "Object(JsonObject<'i>)",
                "Array(JsonArray<'i>)",
                "String(JsonString<'i>)",
                "Number(JsonNumber<'i>)",
                "Bool(bool)",
                "Null",
            ],
        );
        facts.add_struct("JsonObject", &[("members", "TapeSlice<'i, JsonPair<'i>>")]);
        facts.add_struct("JsonArray", &[("elements", "TapeSlice<'i, JsonValue<'i>>")]);
        facts.add_struct(
            "JsonPair",
            &[("key", "JsonString<'i>"), ("value", "JsonValue<'i>")],
        );
        facts.add_struct(
            "JsonString",
            &[("span", "Span<'i>"), ("needs_unescape", "bool")],
        );
        facts.add_struct("JsonNumber", &[("span", "Span<'i>")]);
        facts.add_struct("JsonBool", &[("value", "bool")]);
        facts.add_struct("JsonNull", &[]);
        facts
    }
}

pub mod recognizers {
    use super::*;

    pub fn nominate_json(_grammar: &GrammarIr) -> Vec<Recognizer> {
        vec![Recognizer::SimdScan {
            mode: SimdMode::Exact,
            alphabet: StructuralAlphabet::json(),
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
        let mut diagnostics = Vec::new();

        for rule in &grammar.rules {
            let backend_rule = backend.rules.get(rule.id.0);
            let shape = match backend_rule {
                Some(_) if requires_eager_tape(grammar, rule.body, layout) => {
                    BackendShape::EagerTape
                }
                Some(rule_ir) if admits_sink_only(rule_ir, target) => BackendShape::SinkOnly,
                Some(rule_ir) if admits_collapsed_stage(rule_ir, target) => {
                    if target.collapsed_stage_author_declared {
                        BackendShape::CollapsedStage
                    } else {
                        diagnostics.push(diagnostics::PassDiagnostic::collapsed_stage_not_viable(
                            rule.id,
                            "missing per-grammar collapsed-stage assembly wrapper",
                        ));
                        BackendShape::OffsetTape
                    }
                }
                Some(rule_ir) if prefers_event_tape(rule_ir) => BackendShape::EventTape,
                Some(_) => BackendShape::OffsetTape,
                None => {
                    diagnostics.push(diagnostics::PassDiagnostic::backend_shape_inconsistent(
                        rule.id,
                        "grammar rule has no matching backend rule",
                    ));
                    BackendShape::EagerTape
                }
            };
            backend_shape.insert(rule.id, shape);
        }

        BackendShapePlan {
            backend_shape,
            diagnostics,
        }
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
            profile_hints: Option<&PriorBenchProfile>,
        ) -> HashMap<ir::RuleId, HotPathFact> {
            let entry = grammar_ir
                .rule_by_name("json")
                .or_else(|| grammar_ir.rule_by_name("parse_value"))
                .or_else(|| grammar_ir.rules.first());
            let mut hot = HashMap::new();
            if let Some(rule) = entry {
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

    pub fn single_plan(
        grammar: &GrammarIr,
        _layout_facts: &LayoutFacts,
        shape_facts: ShapeFacts,
        recognizers: Vec<Recognizer>,
    ) -> Result<BackendIr, PassError> {
        let entry = grammar
            .rule_by_name("json")
            .ok_or_else(|| PassError::MissingEntry("json".to_string()))?;
        let mut rules = Vec::with_capacity(grammar.rules.len());
        for rule in &grammar.rules {
            let expr = lower_expr(grammar, rule.body);
            let expr = materialize_rule(&rule.name, expr);
            let expr = if rule.id == entry.id {
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
            entry_rule: entry.name.clone(),
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

    fn materialize_rule(name: &str, body: BackendExpr) -> BackendExpr {
        let Some((kind, shape)) = materialization_for_rule(name) else {
            return body;
        };
        BackendExpr::Seq(vec![
            BackendExpr::SpanMark {
                kind: SpanMarkKind::Start,
                label: name.to_string(),
            },
            body,
            BackendExpr::SpanMark {
                kind: SpanMarkKind::End,
                label: name.to_string(),
            },
            BackendExpr::TapeEmit { kind },
            BackendExpr::DirectBuild {
                shape: shape.to_string(),
                fields: direct_fields_for_rule(name),
            },
            BackendExpr::Return,
        ])
    }

    fn materialization_for_rule(name: &str) -> Option<(TapeKind, &'static str)> {
        match name {
            "object" => Some((TapeKind::Object, "JsonObject")),
            "array" => Some((TapeKind::Array, "JsonArray")),
            "pair" => Some((TapeKind::Pair, "JsonPair")),
            "string" => Some((TapeKind::String, "JsonString")),
            "number" => Some((TapeKind::Number, "JsonNumber")),
            "bool" => Some((TapeKind::Bool, "JsonBool")),
            "null" => Some((TapeKind::Null, "JsonNull")),
            _ => None,
        }
    }

    fn direct_fields_for_rule(name: &str) -> Vec<DirectBuildField> {
        match name {
            "object" => vec![DirectBuildField {
                name: "members".to_string(),
                source: DirectBuildSource::RepeatedRule {
                    rule: "pair".to_string(),
                },
            }],
            "array" => vec![DirectBuildField {
                name: "elements".to_string(),
                source: DirectBuildSource::RepeatedRule {
                    rule: "value".to_string(),
                },
            }],
            "pair" => vec![
                DirectBuildField {
                    name: "key".to_string(),
                    source: DirectBuildSource::ChildRule {
                        rule: "string".to_string(),
                    },
                },
                DirectBuildField {
                    name: "value".to_string(),
                    source: DirectBuildSource::ChildRule {
                        rule: "value".to_string(),
                    },
                },
            ],
            "string" => vec![DirectBuildField {
                name: "span".to_string(),
                source: DirectBuildSource::Span {
                    label: "string".to_string(),
                },
            }],
            "number" => vec![DirectBuildField {
                name: "span".to_string(),
                source: DirectBuildSource::Span {
                    label: "number".to_string(),
                },
            }],
            "bool" => vec![DirectBuildField {
                name: "value".to_string(),
                source: DirectBuildSource::Literal { bytes: Vec::new() },
            }],
            "null" => Vec::new(),
            _ => Vec::new(),
        }
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
        assert!(output.diagnostics.is_empty());
        assert_eq!(output.layout_facts.backend_shape.len(), 15);
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
        let shapes = shapes::shapes_for_json();
        assert_eq!(shapes.shapes.len(), 9);
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

        let json = output.grammar.rule_by_name("json").unwrap();
        assert_eq!(
            plan.backend_shape.get(&json.id).copied(),
            Some(BackendShape::OffsetTape)
        );
        assert!(plan.diagnostics.iter().any(|diagnostic| {
            diagnostic.code() == "BBNF-COLLAPSEDSTAGE-NOT-VIABLE"
                && diagnostic.rule == Some(json.id)
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
}
