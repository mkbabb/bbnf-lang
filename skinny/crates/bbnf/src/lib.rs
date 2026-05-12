use std::borrow::Cow;
use thiserror::Error;

pub use codegen::EmittedSource;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: Cow<'static, str>,
    pub message: String,
    pub offset: Option<usize>,
}

impl Diagnostic {
    pub fn new(code: impl Into<Cow<'static, str>>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            offset: None,
        }
    }

    pub fn at(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0:?}")]
    Diagnostic(Diagnostic),
    #[error(transparent)]
    Grammar(#[from] grammar::GrammarError),
    #[error(transparent)]
    Codegen(#[from] codegen::CodegenError),
    #[error("JSON parse error: {0}")]
    JsonParse(String),
}

pub trait Grammar {
    type Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> Result<Self::Output<'i>, Error>;
}

#[derive(Clone, Copy, Debug, Default)]
pub struct JsonGrammar;

impl Grammar for JsonGrammar {
    type Output<'i> = runtime::grammars::json::JsonRoot<'i>;

    fn parse<'i>(&self, input: &'i str) -> Result<Self::Output<'i>, Error> {
        json::parse(input)
    }
}

pub fn compile_json_source(source: &str) -> Result<EmittedSource, Error> {
    Ok(codegen::emit_json_from_source(source)?)
}

pub fn compile_json_file(path: impl AsRef<std::path::Path>) -> Result<EmittedSource, Error> {
    let source = std::fs::read_to_string(path.as_ref())
        .map_err(|err| Error::Diagnostic(Diagnostic::new("BBNF-SOURCE-LOAD", err.to_string())))?;
    compile_json_source(&source)
}

pub mod json {
    use super::Error;

    pub use runtime::grammars::json::{
        JsonArray, JsonBool, JsonNodeKind, JsonNull, JsonNumber, JsonObject, JsonPair, JsonRoot,
        JsonString, JsonToken, JsonValue,
    };

    pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, Error> {
        runtime::generated_json::parse(input).map_err(|err| Error::JsonParse(err.to_string()))
    }

    pub fn parse_in<'i>(input: &'i str) -> Result<JsonRoot<'i>, Error> {
        parse(input)
    }

    pub fn parse_owned(input: String) -> Result<serde_json::Value, Error> {
        parse(&input).map(|doc| {
            serde_json::from_str(&doc.to_canonical_string())
                .expect("runtime canonical JSON should parse through serde_json")
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const JSON_GRAMMAR: &str = include_str!("../../../grammars/json.bbnf");

    #[test]
    fn facade_compiles_json_source() {
        let emitted = compile_json_source(JSON_GRAMMAR).unwrap();
        assert!(emitted.get("parser.rs").unwrap().contains("pub fn parse"));
    }

    #[test]
    fn facade_parses_json_with_runtime_tape() {
        let doc = JsonGrammar.parse(r#"{"ok":true}"#).unwrap();
        let json::JsonValue::Object(object) = doc.value() else {
            panic!("expected object");
        };
        assert_eq!(object.get("ok").unwrap().to_canonical_string(), "true");
    }
}
