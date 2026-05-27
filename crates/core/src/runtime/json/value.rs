use crate::runtime::json::arena::{JsonArrayId, JsonObjectId};
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JsonValue<'p> {
    Null,
    Bool(bool),
    Number(JsonNumber),
    String(&'p str),
    Array(JsonArrayId),
    Object(JsonObjectId),
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JsonNumber {
    Int(i64),
    UInt(u64),
    Float(f64),
}
impl JsonNumber {
    #[inline]
    pub fn as_f64(&self) -> f64 {
        match *self {
            JsonNumber::Int(v) => v as f64,
            JsonNumber::UInt(v) => v as f64,
            JsonNumber::Float(v) => v,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct JsonPair<'p> {
    pub key: &'p str,
    pub value: JsonValue<'p>,
}
#[derive(Debug, Clone, Copy)]
pub struct JsonArray<'p> {
    pub items: &'p [JsonValue<'p>],
}
#[derive(Debug, Clone, Copy)]
pub struct JsonObject<'p> {
    pub pairs: &'p [JsonPair<'p>],
}
