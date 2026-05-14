mod lower;

use ir::{BackendIr, BackendShape, RuleId};
use std::collections::BTreeMap;
use std::path::Path;
use thiserror::Error;

const JSON_SINK_SHAPES: [&str; 7] = [
    "JsonObject",
    "JsonArray",
    "JsonPair",
    "JsonString",
    "JsonNumber",
    "JsonBool",
    "JsonNull",
];

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

fn emit_json_with_layout(
    backend: &BackendIr,
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
    diagnostics: &[passes::diagnostics::PassDiagnostic],
) -> Result<EmittedSource, CodegenError> {
    let _lowered = lower::lower_to_rust(
        backend,
        &lower::LowerCtx {
            backend_shape,
            diagnostics,
        },
    );
    let mut files = BTreeMap::new();
    let mut generated = generated_rs();
    if lower::sink_only::direct_builds_all(backend, &JSON_SINK_SHAPES) {
        generated.push('\n');
        generated.push_str(&sink_direct_rs());
    }

    files.insert("generated.rs".to_string(), generated);
    files.insert("host.rs".to_string(), host_rs());
    files.insert("mod.rs".to_string(), mod_rs());
    files.insert("parser.rs".to_string(), parser_rs());
    files.insert("scan.rs".to_string(), scan_rs());
    files.insert("value.rs".to_string(), value_rs());
    files.insert("view.rs".to_string(), view_rs());
    files.insert("visitor.rs".to_string(), visitor_rs());
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

fn sink_direct_rs() -> String {
    include_str!("json_templates/sink_direct.rs").to_string()
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
}
