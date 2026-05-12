use std::fmt;

use crate::tape::{NodeKindId, TokenFlags, ValueRef};

use super::view::{JsonArray, JsonBool, JsonNull, JsonNumber, JsonObject, JsonString};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(u16)]
pub enum JsonNodeKind {
    Root = 0,
    ObjectOpen = 1,
    ObjectClose = 2,
    ArrayOpen = 3,
    ArrayClose = 4,
    Pair = 5,
    String = 6,
    Number = 7,
    True = 8,
    False = 9,
    Null = 10,
}

impl JsonNodeKind {
    pub fn from_id(id: NodeKindId) -> Option<Self> {
        match id.0 {
            0 => Some(Self::Root),
            1 => Some(Self::ObjectOpen),
            2 => Some(Self::ObjectClose),
            3 => Some(Self::ArrayOpen),
            4 => Some(Self::ArrayClose),
            5 => Some(Self::Pair),
            6 => Some(Self::String),
            7 => Some(Self::Number),
            8 => Some(Self::True),
            9 => Some(Self::False),
            10 => Some(Self::Null),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Root => "Root",
            Self::ObjectOpen => "ObjectOpen",
            Self::ObjectClose => "ObjectClose",
            Self::ArrayOpen => "ArrayOpen",
            Self::ArrayClose => "ArrayClose",
            Self::Pair => "Pair",
            Self::String => "String",
            Self::Number => "Number",
            Self::True => "True",
            Self::False => "False",
            Self::Null => "Null",
        }
    }
}

impl From<JsonNodeKind> for NodeKindId {
    fn from(value: JsonNodeKind) -> Self {
        Self(value as u16)
    }
}

#[derive(Clone)]
pub enum JsonValue<'doc, 'input: 'doc> {
    Object(JsonObject<'doc, 'input>),
    Array(JsonArray<'doc, 'input>),
    String(JsonString<'doc, 'input>),
    Number(JsonNumber<'doc, 'input>),
    Bool(JsonBool<'doc, 'input>),
    Null(JsonNull<'doc, 'input>),
}

impl<'doc, 'input: 'doc> JsonValue<'doc, 'input> {
    pub fn span(&self) -> std::ops::Range<usize> {
        match self {
            Self::Object(value) => value.inner.token().span(),
            Self::Array(value) => value.inner.token().span(),
            Self::String(value) => value.inner.token().span(),
            Self::Number(value) => value.inner.token().span(),
            Self::Bool(value) => value.inner.token().span(),
            Self::Null(value) => value.inner.token().span(),
        }
    }

    pub fn to_canonical_string(&self) -> String {
        match self {
            Self::Object(object) => object.to_canonical_string(),
            Self::Array(array) => array.to_canonical_string(),
            Self::String(string) => serde_json::to_string(string.as_str().as_ref())
                .expect("JSON string serialization cannot fail"),
            Self::Number(number) => number.raw().to_owned(),
            Self::Bool(value) if value.value() => "true".to_owned(),
            Self::Bool(_) => "false".to_owned(),
            Self::Null(_) => "null".to_owned(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct JsonToken {
    pub kind: JsonNodeKind,
    pub flags: TokenFlags,
    pub start: u32,
    pub end: u32,
    pub payload_or_skip: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError<'input> {
    pub input: &'input str,
    pub offset: usize,
    pub kind: ParseErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    ExpectedValue,
    ExpectedObjectKeyOrEnd,
    ExpectedArrayValueOrEnd,
    ExpectedColon,
    ExpectedCommaOrObjectEnd,
    ExpectedCommaOrArrayEnd,
    TrailingCharacters,
    InvalidNumber,
    InvalidString,
    InvalidLiteral(&'static str),
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at byte {}", self.kind, self.offset)
    }
}

impl std::error::Error for ParseError<'_> {}

pub(crate) fn value_from_ref<'doc, 'input: 'doc>(
    inner: ValueRef<'doc, 'input>,
) -> JsonValue<'doc, 'input> {
    match JsonNodeKind::from_id(inner.token().kind).expect("parser emits only JSON node kinds") {
        JsonNodeKind::ObjectOpen => JsonValue::Object(JsonObject {
            inner: ValueRef::new(inner.tape(), inner.index()),
        }),
        JsonNodeKind::ArrayOpen => JsonValue::Array(JsonArray {
            inner: ValueRef::new(inner.tape(), inner.index()),
        }),
        JsonNodeKind::String => JsonValue::String(JsonString {
            inner: ValueRef::new(inner.tape(), inner.index()),
        }),
        JsonNodeKind::Number => JsonValue::Number(JsonNumber {
            inner: ValueRef::new(inner.tape(), inner.index()),
        }),
        JsonNodeKind::True => JsonValue::Bool(JsonBool {
            inner: ValueRef::new(inner.tape(), inner.index()),
            value: true,
        }),
        JsonNodeKind::False => JsonValue::Bool(JsonBool {
            inner: ValueRef::new(inner.tape(), inner.index()),
            value: false,
        }),
        JsonNodeKind::Null => JsonValue::Null(JsonNull {
            inner: ValueRef::new(inner.tape(), inner.index()),
        }),
        other => panic!("node kind {other:?} is not a JSON value"),
    }
}
