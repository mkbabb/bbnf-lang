use ir::{
    BackendExpr, BackendIr, BackendRule, ExprId, ExprKind, GrammarIr, Recognizer, ShapeFacts,
    SimdMode, SimdSite, SourceSpan, SpanKind, SpanMarkKind, StructuralAlphabet, TapeKind,
    ValidationError,
};
use std::collections::HashMap;
use thiserror::Error;

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
    let layout_facts = layout::run(&normalized, type_facts);
    let shape_facts = shapes::shapes_for_json();
    let recognizers = recognizers::nominate_json(&normalized);
    let backend_ir = extract::single_plan(&normalized, &layout_facts, shape_facts, recognizers)?;
    Ok(PipelineOutput {
        grammar: normalized,
        layout_facts,
        backend_ir,
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PipelineOutput {
    pub grammar: GrammarIr,
    pub layout_facts: LayoutFacts,
    pub backend_ir: BackendIr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LayoutFacts {
    pub rule_types: HashMap<ir::RuleId, Type>,
    pub node_types: HashMap<ExprId, Type>,
    pub layout_policies: HashMap<String, String>,
}

pub mod layout {
    use super::*;

    pub fn run(_grammar: &GrammarIr, type_facts: types::TypeFacts) -> LayoutFacts {
        LayoutFacts {
            rule_types: type_facts.rule_types,
            node_types: type_facts.node_types,
            layout_policies: HashMap::new(),
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
