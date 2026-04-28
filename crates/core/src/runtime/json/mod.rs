//! AZ-I.W2.A — JSON struct-direct runtime.
//!
//! `crates/core/src/runtime/json/` is the typed-struct runtime for the
//! JSON grammar. The generated parser writes directly into a
//! [`JsonDocument`] graph via the [`JsonStructBuilder`] consumer of
//! the [`crate::runtime::builder::StructBuilder`] trait — the tape
//! substrate is severed on the JSON parse path; no `TapeBuilder`
//! / `TapeRec` / `TapeCursor` symbol appears in this module's
//! transitive code.
//!
//! # Module layout
//!
//! - [`value`]   — typed [`JsonValue`] / [`JsonNumber`] / [`JsonPair`]
//!   / [`JsonObject`] / [`JsonArray`] sum-types and their handles.
//! - [`arena`]   — the [`JsonArena`] owning slab for compound child
//!   slices ([`JsonValue`] in arrays, [`JsonPair`] in objects).
//! - [`builder`] — the [`JsonStructBuilder`] concrete `StructBuilder`
//!   impl that the generated parse function targets.
//!
//! # Wire contract
//!
//! `bbnf::grammar::generated::json::JsonParser::parse(src)` returns a
//! [`JsonDocument<'_>`] borrowing from `src`'s lifetime. The
//! grammar's typed `->` annotations close as follows:
//!
//! - `null = "null" -> 0u8` → [`JsonValue::Null`]
//! - `bool = "true" -> true | "false" -> false` → [`JsonValue::Bool`]
//! - `number = /…/ -> f64` → [`JsonValue::Number`] holding
//!   [`JsonNumber::Float`]
//! - `string = /…/ -> decode_json_string_to_arena(input) : String` →
//!   [`JsonValue::String`] borrowing the decoded bytes from the arena
//! - `array = "[" >> … << "]"` → [`JsonValue::Array`] with the child
//!   slice arena-allocated
//! - `pair = string, colon >> value` → [`JsonPair`] in the object's
//!   pair slice
//! - `object = "{" >> … << "}"` → [`JsonValue::Object`] with the
//!   pair slice arena-allocated

pub mod arena;
pub mod builder;
pub mod value;

pub use arena::{JsonArena, JsonArrayId, JsonObjectId};
pub use builder::JsonStructBuilder;
pub use value::{JsonArray, JsonDocument, JsonNumber, JsonObject, JsonPair, JsonValue};
