//! AZ-I.W2.A — JSON typed value sum.
//!
//! The parse output is rooted at a [`JsonDocument<'p>`] holding a
//! [`JsonArena<'p>`] (the owner of every compound's child slice) and a
//! root [`JsonValue<'p>`]. The `'p` lifetime ties every borrowed
//! string slice and every arena-allocated child slice to the parse
//! call site.
//!
//! Shape derivation comes directly from `grammar/json/json.bbnf`:
//!
//! ```text
//! null   = "null"                          -> 0u8                              ;
//! bool   = "true" -> true | "false" -> false                                   ;
//! number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/  -> f64                     ;
//! string = /"…"/                            -> decode_json_string_to_arena(...): String ;
//! array  = "[" >> ((value << comma?)*)?w << "]"                                 ;
//! pair   = string, colon >> value                                              ;
//! object = "{" >> ((pair << comma?)*)?w << "}"                                 ;
//! value  = object | array | string | number | bool | null                      ;
//! ```
//!
//! [`JsonValue`] is the closure of `value`'s alternation; the
//! `Object` / `Array` arms hold `JsonObjectId` / `JsonArrayId`
//! arena handles that resolve to slices via [`JsonDocument::object`]
//! / [`JsonDocument::array`].

use crate::runtime::json::arena::{JsonArena, JsonArrayId, JsonObjectId};

/// A JSON value — sum of every alternation branch in the `value`
/// rule. Arrays and objects carry arena-backed handles; strings carry
/// a borrowed slice (either zero-copy from the input or arena-decoded
/// when escapes are present).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JsonValue<'p> {
    /// `null = "null" -> 0u8`.
    Null,
    /// `bool = "true" -> true | "false" -> false`.
    Bool(bool),
    /// `number = /…/ -> f64` — emitted as the `Float` witness today;
    /// `Int` / `UInt` slots are reserved for grammar evolutions that
    /// project integral parses without floating widening.
    Number(JsonNumber),
    /// `string = /…/` — borrowed from input bytes for unescaped
    /// strings, from the arena for escaped strings.
    String(&'p str),
    /// `array = "[" >> (value …)* << "]"` — pair handle resolves via
    /// [`JsonDocument::array`] to a `&[JsonValue]` slice.
    Array(JsonArrayId),
    /// `object = "{" >> (pair …)* << "}"` — pair handle resolves via
    /// [`JsonDocument::object`] to a `&[JsonPair]` slice.
    Object(JsonObjectId),
}

/// Numeric witness for [`JsonValue::Number`].
///
/// `Int(i64)` / `UInt(u64)` slots are the post-AZ-II projection
/// targets — the JSON grammar today always projects through the f64
/// codec produced by the `-> f64` annotation, which decodes signed
/// integers losslessly through the Eisel-Lemire fast path. The
/// integral variants exist so a future grammar refinement
/// (`number_int = /-?\d+/ -> i64`) closes its typed projection
/// without re-shaping the runtime.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JsonNumber {
    /// Signed integer projection — reserved for future grammar
    /// refinements that distinguish integral parses.
    Int(i64),
    /// Unsigned integer projection — same.
    UInt(u64),
    /// Floating-point projection — the canonical witness today, fed
    /// by the `-> f64` typed annotation on the `number` rule.
    Float(f64),
}

impl JsonNumber {
    /// Coerce to `f64`. Lossless for `Float` and any `Int` / `UInt`
    /// in the f64 lossless range; consumers that need exact integral
    /// width should match on the variant directly.
    #[inline]
    pub fn as_f64(&self) -> f64 {
        match *self {
            JsonNumber::Int(v) => v as f64,
            JsonNumber::UInt(v) => v as f64,
            JsonNumber::Float(v) => v,
        }
    }
}

/// A `(key, value)` pair from an object body.
///
/// `key` is borrowed (input-side or arena-decoded) per the same
/// discipline as [`JsonValue::String`]; `value` recurses into the
/// full sum.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct JsonPair<'p> {
    /// The pair's key — produced by the `string` rule.
    pub key: &'p str,
    /// The pair's value — any [`JsonValue`] arm.
    pub value: JsonValue<'p>,
}

/// Newtype around `&[JsonValue]` for ergonomic indexing.
#[derive(Debug, Clone, Copy)]
pub struct JsonArray<'p> {
    /// The element slice, lent from the arena.
    pub items: &'p [JsonValue<'p>],
}

/// Newtype around `&[JsonPair]` for ergonomic indexing.
#[derive(Debug, Clone, Copy)]
pub struct JsonObject<'p> {
    /// The pair slice, lent from the arena.
    pub pairs: &'p [JsonPair<'p>],
}

/// The root document returned by `bbnf::grammar::generated::json::JsonParser::parse`.
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
///
/// Resolving a [`JsonValue::Array`] / [`JsonValue::Object`] handle
/// goes through [`Self::array`] / [`Self::object`].
#[derive(Debug)]
pub struct JsonDocument<'p> {
    /// The compound child arena — owns every `[JsonValue]` and
    /// `[JsonPair]` slice the document references via handles.
    pub arena: JsonArena<'p>,
    /// The root value of the document.
    pub root: JsonValue<'p>,
}

impl<'p> JsonDocument<'p> {
    /// Construct a document from a populated arena and a root value.
    /// The typical caller is the generated parse function; consumers
    /// outside the emitter rarely build a `JsonDocument` directly.
    #[inline]
    pub fn new(arena: JsonArena<'p>, root: JsonValue<'p>) -> Self {
        Self { arena, root }
    }

    /// Resolve a [`JsonArrayId`] handle to the underlying element
    /// slice. Returns an empty slice for `JsonArrayId::EMPTY`.
    #[inline]
    pub fn array(&self, id: JsonArrayId) -> &[JsonValue<'p>] {
        self.arena.array(id)
    }

    /// Resolve a [`JsonObjectId`] handle to the underlying pair slice.
    /// Returns an empty slice for `JsonObjectId::EMPTY`.
    #[inline]
    pub fn object(&self, id: JsonObjectId) -> &[JsonPair<'p>] {
        self.arena.object(id)
    }
}
