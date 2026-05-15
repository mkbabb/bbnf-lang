pub mod direct_schema;
mod json_sink_direct;
mod json_typed_direct;
pub(crate) mod lower;

use direct_schema::DirectSchemaSet;
use ir::{BackendIr, BackendShape, RuleId};
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

pub fn emit_json_from_source(source: &str) -> Result<EmittedSource, CodegenError> {
    let grammar = grammar::parse_json_grammar(source)?;
    let output = passes::compile(&grammar)?;
    emit_json_with_layout(
        &output.backend_ir,
        &output.layout_facts.backend_shape,
        &output.diagnostics,
    )
}

pub fn emit_json(backend: &BackendIr) -> Result<EmittedSource, CodegenError> {
    let backend_shape = default_backend_shape(backend);
    emit_json_with_layout(backend, &backend_shape, &[])
}

pub fn emit_json_typed_from_source(
    source: &str,
    schema: &DirectSchemaSet,
) -> Result<EmittedSource, CodegenError> {
    let grammar = grammar::parse_json_grammar(source)?;
    let output = passes::compile(&grammar)?;
    emit_json_typed_with_layout(
        &output.backend_ir,
        &output.layout_facts.backend_shape,
        &output.diagnostics,
        schema,
    )
}

fn emit_json_with_layout(
    backend: &BackendIr,
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
    diagnostics: &[passes::diagnostics::PassDiagnostic],
) -> Result<EmittedSource, CodegenError> {
    let lowered = lower::lower_to_rust(
        backend,
        &lower::LowerCtx {
            backend_shape,
            diagnostics,
        },
    );
    let mut files = BTreeMap::new();
    let mut generated = generated_rs();
    let sink_only = lowered.sink_only_program.as_ref().ok_or_else(|| {
        CodegenError::Lowering(
            "BackendIr did not contain DirectBuild sink-only program".to_string(),
        )
    })?;
    generated.push('\n');
    generated.push_str(&json_sink_direct::render(sink_only).map_err(CodegenError::Lowering)?);

    files.insert("generated.rs".to_string(), generated);
    files.insert("host.rs".to_string(), host_rs());
    files.insert("mod.rs".to_string(), mod_rs());
    files.insert("parser.rs".to_string(), parser_rs());
    files.insert("scan.rs".to_string(), scan_rs());
    files.insert("sink.rs".to_string(), sink_rs());
    files.insert("value.rs".to_string(), value_rs());
    files.insert("view.rs".to_string(), view_rs());
    files.insert("visitor.rs".to_string(), visitor_rs());
    Ok(EmittedSource { files })
}

fn emit_json_typed_with_layout(
    backend: &BackendIr,
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
    diagnostics: &[passes::diagnostics::PassDiagnostic],
    schema: &DirectSchemaSet,
) -> Result<EmittedSource, CodegenError> {
    let lowered = lower::lower_to_rust(
        backend,
        &lower::LowerCtx {
            backend_shape,
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
        json_typed_direct::render(&typed).map_err(CodegenError::Lowering)?,
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

fn mod_rs() -> String {
    normalize(
        r#"
        pub mod generated;
        pub mod host;
        pub mod parser;
        pub mod scan;
        pub mod sink;
        pub mod value;
        pub mod view;
        pub mod visitor;

        pub use parser::{parse, parse_bytes, RECOGNIZER_COUNT};
        pub use generated::parse_direct;
        pub use sink::JsonSink;
        pub use value::{JsonNodeKind, JsonToken, JsonValue, ParseError, ParseErrorKind};
        pub use view::{
            JsonArray, JsonBool, JsonDocument, JsonNull, JsonNumber, JsonObject, JsonPair,
            JsonRoot, JsonString,
        };
        pub use visitor::JsonVisitor;
        "#,
    )
}

fn host_rs() -> String {
    normalize(
        r#"
        // JSON is host-fn-free in the skinny compiler slice.
        "#,
    )
}

fn generated_rs() -> String {
    include_str!("json_templates/generated.rs").to_string()
}

fn parser_rs() -> String {
    include_str!("json_templates/parser.rs").to_string()
}

fn scan_rs() -> String {
    include_str!("../../runtime/src/grammars/json/scan.rs").to_string()
}

fn sink_rs() -> String {
    include_str!("../../runtime/src/grammars/json/sink.rs").to_string()
}

fn view_rs() -> String {
    include_str!("json_templates/view.rs").to_string()
}

fn value_rs() -> String {
    include_str!("json_templates/value.rs").to_string()
}

fn visitor_rs() -> String {
    normalize(include_str!("json_templates/visitor.rs"))
}

fn normalize(source: &str) -> String {
    let mut out = String::new();
    out.push_str("// @generated by skinny bbnf-codegen; do not edit by hand.\n");
    let trimmed = source.trim_matches('\n');
    let indent = trimmed
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.chars().take_while(|ch| *ch == ' ').count())
        .min()
        .unwrap_or(0);
    for raw_line in trimmed.lines() {
        let line = raw_line.get(indent..).unwrap_or(raw_line).trim_end();
        if line.is_empty() {
            out.push('\n');
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    out
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
        let emitted = emit_json_from_source(JSON_GRAMMAR).unwrap();
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
        let first = emit_json_from_source(JSON_GRAMMAR).unwrap();
        let second = emit_json_from_source(JSON_GRAMMAR).unwrap();

        assert_eq!(first, second);
        assert!(first
            .get("generated.rs")
            .unwrap()
            .contains("STRUCTURAL_ALPHABET_JSON"));
    }

    #[test]
    fn direct_parser_is_authored_from_sink_only_lowering() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let output = passes::compile(&grammar).unwrap();
        let lowered = lower::lower_to_rust(
            &output.backend_ir,
            &lower::LowerCtx {
                backend_shape: &output.layout_facts.backend_shape,
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

        let emitted = emit_json_from_source(JSON_GRAMMAR).unwrap();
        let generated = emitted.get("generated.rs").unwrap();
        assert!(generated.contains("// sink-only lowered from BackendIr: entry=json"));
        assert!(generated.contains("pub fn parse_direct"));
    }

    #[test]
    fn refuses_direct_parser_without_direct_builds() {
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let mut output = passes::compile(&grammar).unwrap();
        for rule in &mut output.backend_ir.rules {
            strip_direct_builds(&mut rule.expr);
        }

        let err = emit_json_with_layout(
            &output.backend_ir,
            &output.layout_facts.backend_shape,
            &output.diagnostics,
        )
        .unwrap_err();

        assert!(matches!(err, CodegenError::Lowering(_)));
    }

    #[test]
    fn emits_typed_direct_consumer_module() {
        let schema = tiny_schema();
        let emitted = emit_json_typed_from_source(JSON_GRAMMAR, &schema).unwrap();
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
        let grammar = grammar::parse_json_grammar(JSON_GRAMMAR).unwrap();
        let mut output = passes::compile(&grammar).unwrap();
        for rule in &mut output.backend_ir.rules {
            strip_direct_builds(&mut rule.expr);
        }

        let err = emit_json_typed_with_layout(
            &output.backend_ir,
            &output.layout_facts.backend_shape,
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
                            json_key: "name".to_string(),
                            rust_field: "name".to_string(),
                            ty: DirectTypeRef::Scalar(DirectScalar::String),
                            presence: PresencePolicy::Required,
                            duplicate: DuplicatePolicy::Reject,
                        },
                        DirectFieldSchema {
                            json_key: "items".to_string(),
                            rust_field: "items".to_string(),
                            ty: DirectTypeRef::Vec(Box::new(DirectTypeRef::Scalar(
                                DirectScalar::U64,
                            ))),
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
