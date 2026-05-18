pub mod direct_schema;
mod json_provider;
pub(crate) mod lower;
mod sink_direct;
mod typed_direct;

use direct_schema::DirectSchemaSet;
use ir::{BackendIr, BackendShape, CostFacts, PriorityStep, RuleId, ShapeRationale};
use std::collections::BTreeMap;
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    Grammar(#[from] grammar::GrammarError),
    #[error(transparent)]
    Pass(#[from] passes::PassError),
    #[error("missing generated file `{0}`")]
    MissingFile(String),
    #[error("generated file `{0}` differs")]
    DifferentFile(String),
    #[error("lowering failed: {0}")]
    Lowering(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmittedSource {
    files: BTreeMap<String, String>,
}

impl EmittedSource {
    pub fn files(&self) -> impl Iterator<Item = (&str, &str)> {
        self.files
            .iter()
            .map(|(path, source)| (path.as_str(), source.as_str()))
    }

    pub fn get(&self, path: &str) -> Option<&str> {
        self.files.get(path).map(String::as_str)
    }

    pub fn write_to_dir(&self, output_dir: impl AsRef<Path>) -> Result<(), CodegenError> {
        let output_dir = output_dir.as_ref();
        std::fs::create_dir_all(output_dir)?;
        for (path, source) in &self.files {
            let file = output_dir.join(path);
            std::fs::write(file, source)?;
        }
        Ok(())
    }

    pub fn check_dir(&self, output_dir: impl AsRef<Path>) -> Result<(), CodegenError> {
        let output_dir = output_dir.as_ref();
        for (path, source) in &self.files {
            let file = output_dir.join(path);
            let actual = std::fs::read_to_string(&file)
                .map_err(|_| CodegenError::MissingFile(path.clone()))?;
            if actual != *source {
                return Err(CodegenError::DifferentFile(path.clone()));
            }
        }
        Ok(())
    }
}

pub fn emit_from_source(grammar_name: &str, source: &str) -> Result<EmittedSource, CodegenError> {
    let grammar = grammar::parse_grammar(grammar_name, source)?;
    let output = passes::compile(&grammar)?;
    emit_with_layout(
        &output.backend_ir,
        &output.layout_facts.backend_shape,
        &output.layout_facts.cost_facts,
        &output.diagnostics,
    )
}

pub fn emit(backend: &BackendIr) -> Result<EmittedSource, CodegenError> {
    let backend_shape = default_backend_shape(backend);
    let cost_facts = default_cost_facts(&backend_shape);
    emit_with_layout(backend, &backend_shape, &cost_facts, &[])
}

pub fn emit_typed_from_source(
    grammar_name: &str,
    source: &str,
    schema: &DirectSchemaSet,
) -> Result<EmittedSource, CodegenError> {
    let grammar = grammar::parse_grammar(grammar_name, source)?;
    let output = passes::compile(&grammar)?;
    emit_typed_with_layout(
        &output.backend_ir,
        &output.layout_facts.backend_shape,
        &output.layout_facts.cost_facts,
        &output.diagnostics,
        schema,
    )
}

fn emit_with_layout(
    backend: &BackendIr,
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
    cost_facts: &std::collections::HashMap<RuleId, CostFacts>,
    diagnostics: &[passes::diagnostics::PassDiagnostic],
) -> Result<EmittedSource, CodegenError> {
    json_provider::ensure_runtime_profile(backend)?;
    let lowered = lower::lower_to_rust(
        backend,
        &lower::LowerCtx {
            backend_shape,
            cost_facts,
            diagnostics,
        },
    );
    let mut files = BTreeMap::new();
    let mut generated = json_provider::generated_rs();
    let sink_only = lowered.sink_only_program.as_ref().ok_or_else(|| {
        CodegenError::Lowering(
            "BackendIr did not contain DirectBuild sink-only program".to_string(),
        )
    })?;
    generated.push('\n');
    generated.push_str(&sink_direct::render(sink_only).map_err(CodegenError::Lowering)?);

    files.insert("generated.rs".to_string(), generated);
    files.insert("host.rs".to_string(), json_provider::host_rs());
    files.insert("mod.rs".to_string(), json_provider::mod_rs());
    files.insert("parser.rs".to_string(), json_provider::parser_rs());
    files.insert("scan.rs".to_string(), json_provider::scan_rs());
    files.insert("sink.rs".to_string(), json_provider::sink_rs());
    files.insert("value.rs".to_string(), json_provider::value_rs());
    files.insert("view.rs".to_string(), json_provider::view_rs());
    files.insert("visitor.rs".to_string(), json_provider::visitor_rs());
    Ok(EmittedSource { files })
}

fn emit_typed_with_layout(
    backend: &BackendIr,
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
    cost_facts: &std::collections::HashMap<RuleId, CostFacts>,
    diagnostics: &[passes::diagnostics::PassDiagnostic],
    schema: &DirectSchemaSet,
) -> Result<EmittedSource, CodegenError> {
    json_provider::ensure_runtime_profile(backend)?;
    let lowered = lower::lower_to_rust(
        backend,
        &lower::LowerCtx {
            backend_shape,
            cost_facts,
            diagnostics,
        },
    );
    let sink_only = lowered.sink_only_program.as_ref().ok_or_else(|| {
        CodegenError::Lowering(
            "BackendIr did not contain DirectBuild sink-only program".to_string(),
        )
    })?;
    let typed =
        lower::schema_direct::lower_program(sink_only, schema).map_err(CodegenError::Lowering)?;
    let mut files = BTreeMap::new();
    files.insert(
        format!("{}.rs", schema.module_name),
        typed_direct::render(&typed).map_err(CodegenError::Lowering)?,
    );
    Ok(EmittedSource { files })
}

fn default_backend_shape(backend: &BackendIr) -> std::collections::HashMap<RuleId, BackendShape> {
    backend
        .rules
        .iter()
        .enumerate()
        .map(|(index, _)| (RuleId(index), BackendShape::OffsetTape))
        .collect()
}

fn default_cost_facts(
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
) -> std::collections::HashMap<RuleId, CostFacts> {
    backend_shape
        .iter()
        .map(|(rule_id, shape)| {
            (
                *rule_id,
                CostFacts::projection(
                    *rule_id,
                    *shape,
                    ShapeRationale::DefaultOffsetTape,
                    PriorityStep::P7OffsetTapeDefault,
                ),
            )
        })
        .collect()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CostFactsSnapshot {
    pub grammar: String,
    pub cost_facts: std::collections::BTreeMap<String, CostFacts>,
    pub diagnostics: Vec<DiagnosticSnapshot>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiagnosticSnapshot {
    pub code: String,
    pub rule: Option<RuleId>,
    pub message: String,
}

pub fn cost_facts_from_source(
    grammar_name: &str,
    source: &str,
) -> Result<CostFactsSnapshot, CodegenError> {
    let grammar = grammar::parse_grammar(grammar_name, source)?;
    let output = passes::compile(&grammar)?;
    let cost_facts = output
        .layout_facts
        .cost_facts
        .iter()
        .map(|(rule_id, facts)| (rule_id.0.to_string(), facts.clone()))
        .collect();
    let diagnostics = output
        .diagnostics
        .iter()
        .map(|diagnostic| DiagnosticSnapshot {
            code: diagnostic.code().to_string(),
            rule: diagnostic.rule,
            message: diagnostic.message.clone(),
        })
        .collect();
    Ok(CostFactsSnapshot {
        grammar: output.grammar.name,
        cost_facts,
        diagnostics,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use direct_schema::{
        DirectFieldSchema, DirectRootSchema, DirectScalar, DirectTypeKind, DirectTypeRef,
        DirectTypeSchema, DuplicatePolicy, PresencePolicy, UnknownFieldPolicy,
    };
    use ir::BackendExpr;

    const JSON_GRAMMAR: &str = include_str!("../../../grammars/json.bbnf");

    #[test]
    fn emits_expected_file_set_in_order() {
        let emitted = emit_from_source("json", JSON_GRAMMAR).unwrap();
        let names: Vec<_> = emitted.files().map(|(path, _)| path).collect();

        assert_eq!(
            names,
            [
                "generated.rs",
                "host.rs",
                "mod.rs",
                "parser.rs",
                "scan.rs",
                "sink.rs",
                "value.rs",
                "view.rs",
                "visitor.rs"
            ]
        );
    }

    #[test]
    fn emission_is_deterministic() {
        let first = emit_from_source("json", JSON_GRAMMAR).unwrap();
        let second = emit_from_source("json", JSON_GRAMMAR).unwrap();

        assert_eq!(first, second);
        assert!(first
            .get("generated.rs")
            .unwrap()
            .contains("STRUCTURAL_ALPHABET_JSON"));
    }

    #[test]
    fn direct_parser_is_authored_from_sink_only_lowering() {
        let grammar = grammar::parse_grammar("json", JSON_GRAMMAR).unwrap();
        let output = passes::compile(&grammar).unwrap();
        let lowered = lower::lower_to_rust(
            &output.backend_ir,
            &lower::LowerCtx {
                backend_shape: &output.layout_facts.backend_shape,
                cost_facts: &output.layout_facts.cost_facts,
                diagnostics: &output.diagnostics,
            },
        );
        let program = lowered.sink_only_program.unwrap();

        assert_eq!(program.entry_rule, "json");
        assert!(program.has_shape("JsonObject"));
        assert!(program.has_shape("JsonNull"));
        assert!(program.has_literal(b"true"));
        assert!(program.has_literal(b"false"));
        assert!(program.has_literal(b"null"));

        let emitted = emit_from_source("json", JSON_GRAMMAR).unwrap();
        let generated = emitted.get("generated.rs").unwrap();
        assert!(generated.contains("// sink-only lowered from BackendIr: entry=json"));
        assert!(generated.contains("pub fn parse_direct"));
    }

    #[test]
    fn refuses_direct_parser_without_direct_builds() {
        let grammar = grammar::parse_grammar("json", JSON_GRAMMAR).unwrap();
        let mut output = passes::compile(&grammar).unwrap();
        for rule in &mut output.backend_ir.rules {
            strip_direct_builds(&mut rule.expr);
        }

        let err = emit_with_layout(
            &output.backend_ir,
            &output.layout_facts.backend_shape,
            &output.layout_facts.cost_facts,
            &output.diagnostics,
        )
        .unwrap_err();

        assert!(matches!(err, CodegenError::Lowering(_)));
    }

    #[test]
    fn emits_typed_direct_consumer_module() {
        let schema = tiny_schema();
        let emitted = emit_typed_from_source("json", JSON_GRAMMAR, &schema).unwrap();
        let generated = emitted.get("tiny_typed.rs").unwrap();

        assert!(generated.contains("pub fn parse_tiny"));
        assert!(generated.contains("fn parse_type_tiny_root"));
        assert!(generated.contains("DirectBuildError"));
        assert!(!generated.contains("JsonSink"));
        assert!(!generated.contains("serde_json::Value"));
    }

    #[test]
    fn refuses_typed_emission_without_direct_builds() {
        let schema = tiny_schema();
        let grammar = grammar::parse_grammar("json", JSON_GRAMMAR).unwrap();
        let mut output = passes::compile(&grammar).unwrap();
        for rule in &mut output.backend_ir.rules {
            strip_direct_builds(&mut rule.expr);
        }

        let err = emit_typed_with_layout(
            &output.backend_ir,
            &output.layout_facts.backend_shape,
            &output.layout_facts.cost_facts,
            &output.diagnostics,
            &schema,
        )
        .unwrap_err();

        assert!(matches!(err, CodegenError::Lowering(_)));
    }

    fn tiny_schema() -> DirectSchemaSet {
        DirectSchemaSet {
            module_name: "tiny_typed".to_string(),
            schema_hash: "test-schema".to_string(),
            roots: vec![DirectRootSchema {
                function_name: "parse_tiny".to_string(),
                rust_type: "crate::TinyRoot<'i>".to_string(),
                type_id: "TinyRoot".to_string(),
            }],
            types: vec![DirectTypeSchema {
                type_id: "TinyRoot".to_string(),
                rust_type: "crate::TinyRoot<'i>".to_string(),
                kind: DirectTypeKind::Struct {
                    unknown_fields: UnknownFieldPolicy::Skip,
                    ignored_fields: Vec::new(),
                    fields: vec![
                        DirectFieldSchema {
                            key_literal: "name".to_string(),
                            rust_field: "name".to_string(),
                            ty: DirectTypeRef::Scalar(DirectScalar::String),
                            presence: PresencePolicy::Required,
                            duplicate: DuplicatePolicy::Reject,
                        },
                        DirectFieldSchema {
                            key_literal: "items".to_string(),
                            rust_field: "items".to_string(),
                            ty: DirectTypeRef::Vec {
                                inner: Box::new(DirectTypeRef::Scalar(DirectScalar::U64)),
                                capacity_hint: None,
                            },
                            presence: PresencePolicy::Default,
                            duplicate: DuplicatePolicy::LastWins,
                        },
                    ],
                },
            }],
        }
    }

    fn strip_direct_builds(expr: &mut BackendExpr) {
        match expr {
            BackendExpr::Entry(inner)
            | BackendExpr::OptionalBranch(inner)
            | BackendExpr::RepeatLoop { body: inner, .. } => strip_direct_builds(inner),
            BackendExpr::Seq(children)
            | BackendExpr::Alt {
                branches: children, ..
            } => {
                children.retain(|child| !matches!(child, BackendExpr::DirectBuild { .. }));
                for child in children {
                    strip_direct_builds(child);
                }
            }
            BackendExpr::ByteLiteral(_)
            | BackendExpr::RegexProgram { .. }
            | BackendExpr::CallRule { .. }
            | BackendExpr::SpanMark { .. }
            | BackendExpr::TapeEmit { .. }
            | BackendExpr::DirectBuild { .. }
            | BackendExpr::ValueProject { .. }
            | BackendExpr::Return => {}
        }
    }
}
