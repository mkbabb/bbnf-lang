//! AZ-I.W2.A — `JsonStructBuilder` — the concrete `StructBuilder`
//! impl that the generated JSON parse function targets.
//!
//! The builder maintains an in-flight typed stack of partially-built
//! compounds. Each frame on the stack is one of:
//!
//! - [`OpenFrame::Array`] — collecting `JsonValue` elements for an
//!   array body.
//! - [`OpenFrame::Object`] — collecting `JsonPair` entries for an
//!   object body, with a half-built pair (key fixed, value pending).
//! - [`OpenFrame::Pair`] — a pair shape's two-position struct frame
//!   between the key push and the value push.
//! - [`OpenFrame::Wrap`] — a transparent wrapper (the JSON `value`
//!   rule, post-classifier `Wrap` shape) that simply forwards its
//!   single child to the enclosing frame on close.
//!
//! Scalar pushes (`push_leaf_with_*`) land on the top frame's
//! pending slot — for `Array` the element slot, for `Object` the
//! current-pair value slot, for `Pair` the matching key/value slot,
//! for `Wrap` the forwarded slot.
//!
//! `begin_compound` consults the [`StructLayout`]'s
//! [`bbnf_ir::registry::LayoutKind`] to pick the matching frame.
//! `end_compound` finalises the frame into a [`JsonValue`] and lands
//! it on the parent (or, for the root frame, into
//! [`JsonStructBuilder::finalise`]'s return).
//!
//! # Wire contract
//!
//! Every typed `->` annotation in `grammar/json/json.bbnf` reaches a
//! [`StructBuilder`] method:
//!
//! - `null  -> 0u8`               → `push_leaf_with_unit()`
//! - `bool  -> true | false`      → `push_leaf_with_bool(value)`
//! - `number -> f64`              → `push_leaf_with_f64(value)`
//! - `string -> decode_…: String` → `push_leaf_with_str(slice)`
//! - `array`                      → `begin_compound(layout: Array)` /
//!                                   `end_compound`
//! - `object`                     → `begin_compound(layout: Object)` /
//!                                   `end_compound`
//! - `pair`                       → `begin_compound(layout: Pair)` /
//!                                   `end_compound`
//! - `value` (Alt)                → `begin_compound(layout: Wrap)` /
//!                                   `push_branch_tag(idx)` /
//!                                   `end_compound`

use bbnf_ir::registry::{LayoutKind, StructLayout};

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
use crate::runtime::json::arena::JsonArena;
use crate::runtime::json::value::{JsonDocument, JsonNumber, JsonPair, JsonValue};

/// One open compound frame on the builder's stack.
///
/// Each frame collects per-shape state (elements, pairs, key, slots)
/// and is finalised by [`JsonStructBuilder::end_compound`] into a
/// [`JsonValue`] that lands on the parent frame's pending slot.
#[derive(Debug)]
enum OpenFrame<'p> {
    /// `array` shape — collecting element values.
    Array { items: Vec<JsonValue<'p>> },
    /// `object` shape — collecting key/value pairs. `pending_key` is
    /// `Some` between the key push and the value push of an in-flight
    /// pair; `None` between completed pairs.
    Object {
        pairs: Vec<JsonPair<'p>>,
        pending_key: Option<&'p str>,
    },
    /// `pair` shape — typed two-position struct (key + value). The
    /// `key` slot fills first via `push_leaf_with_str`; the `value`
    /// slot fills via the next scalar push or via the enclosed
    /// compound's close. On `end_compound` the pair lands directly on
    /// the enclosing `Object` frame.
    Pair {
        key: Option<&'p str>,
        value: Option<JsonValue<'p>>,
    },
    /// `value` Alt-of-Refs shape — forwards its single classified
    /// child to the enclosing frame's pending slot. The branch tag
    /// (recorded via `push_branch_tag`) is retained for audit
    /// completeness but not stored on the value (JSON's untagged
    /// projection collapses every branch into the unitary
    /// `JsonValue`).
    Wrap { value: Option<JsonValue<'p>> },
}

/// Concrete `StructBuilder` for the JSON grammar.
///
/// Owns a [`JsonArena`] and a stack of open frames. The generated
/// parse function constructs a builder, threads it through every
/// per-shape parse fn, and calls [`JsonStructBuilder::finalise`] at
/// EOF to recover the [`JsonDocument`].
#[derive(Debug)]
pub struct JsonStructBuilder<'p> {
    /// Owning arena — every closed compound's child slice lands
    /// here.
    arena: JsonArena<'p>,
    /// In-flight open compound stack.
    stack: Vec<OpenFrame<'p>>,
    /// The root value, set when the outermost compound (or scalar
    /// against an empty stack) finalises. The generated parse
    /// function reads this via [`Self::finalise`].
    root: Option<JsonValue<'p>>,
    /// Monotonic compound handle counter. The handle is opaque to
    /// the emitter — it pairs `begin_compound` with the matching
    /// `end_compound` only.
    next_handle: u64,
}

impl<'p> Default for JsonStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> JsonStructBuilder<'p> {
    /// Construct a fresh builder with an empty arena and no open
    /// frames.
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: JsonArena::new(),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    /// Construct a builder with arena slab capacity hints. The
    /// emitter folds capacity estimates from
    /// [`bbnf_ir::registry::StructRegistry`] / `GrammarProfile` into
    /// this constructor at codegen time.
    #[inline]
    pub fn with_capacity(arrays: usize, objects: usize) -> Self {
        Self {
            arena: JsonArena::with_capacity(arrays, objects),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    /// Finalise the builder into a [`JsonDocument`]. Panics if no
    /// root value was emitted (the generated parse fn guarantees one
    /// scalar / compound emission against an empty stack), or if
    /// any open frame remains.
    #[inline]
    pub fn finalise(mut self) -> JsonDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "JsonStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("JsonStructBuilder::finalise called before any value emission");
        JsonDocument::new(self.arena, root)
    }

    /// Land a finalised [`JsonValue`] on the topmost open frame, or
    /// store it as the root if the stack is empty. Used by both
    /// scalar pushes and compound finalisations.
    #[inline]
    fn deposit(&mut self, value: JsonValue<'p>) {
        match self.stack.last_mut() {
            None => {
                // Root scalar / compound — generated parse fn invokes
                // exactly one such emission per parse.
                self.root = Some(value);
            }
            Some(OpenFrame::Array { items }) => {
                items.push(value);
            }
            Some(OpenFrame::Object {
                pairs,
                pending_key,
            }) => {
                // Object's value slot fills here; the matching key
                // was set by the most recent `push_leaf_with_str`
                // (or by a typed Pair frame's finalisation). When
                // pending_key is None the value lands as a
                // string-pair without a paired key — the emitter
                // generation order guarantees a key push precedes
                // every value push on the Object frame, so this
                // branch should never fire in well-formed
                // generation.
                debug_assert!(
                    pending_key.is_some(),
                    "Object value pushed without pending key"
                );
                if let Some(key) = pending_key.take() {
                    pairs.push(JsonPair { key, value });
                }
            }
            Some(OpenFrame::Pair { key, value: slot }) => {
                // Pair frame: the first scalar push lands the key as
                // a string; the second push (or compound finalisation)
                // lands the value. The emitter's classifier sees
                // `pair = string, colon >> value` so the key always
                // arrives first as a `JsonValue::String`.
                if key.is_none() {
                    if let JsonValue::String(s) = value {
                        *key = Some(s);
                    } else {
                        debug_assert!(false, "Pair key slot received non-string {:?}", value);
                    }
                } else {
                    *slot = Some(value);
                }
            }
            Some(OpenFrame::Wrap { value: slot }) => {
                *slot = Some(value);
            }
        }
    }
}

impl<'p> StructBuilder for JsonStructBuilder<'p> {
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let frame = match (layout.kind, layout.rule_name.as_str()) {
            // The `array` rule: collect `JsonValue` elements.
            (LayoutKind::Struct, "array")
            | (LayoutKind::TaggedEnum, "array")
            | (LayoutKind::UntaggedEnum, "array") => OpenFrame::Array {
                items: Vec::new(),
            },
            // The `object` rule: collect `JsonPair` entries.
            (LayoutKind::Struct, "object")
            | (LayoutKind::TaggedEnum, "object")
            | (LayoutKind::UntaggedEnum, "object") => OpenFrame::Object {
                pairs: Vec::new(),
                pending_key: None,
            },
            // The `pair` rule: typed two-position struct.
            (LayoutKind::Struct, "pair") => OpenFrame::Pair {
                key: None,
                value: None,
            },
            // The `value` rule (Alt-of-Refs) and any other transparent
            // wrap shape: forward the single child.
            _ => OpenFrame::Wrap { value: None },
        };
        self.stack.push(frame);
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("JsonStructBuilder::end_compound on empty stack");
        let value = match frame {
            OpenFrame::Array { items } => {
                let id = self.arena.push_array(items);
                JsonValue::Array(id)
            }
            OpenFrame::Object { pairs, pending_key } => {
                debug_assert!(
                    pending_key.is_none(),
                    "Object closed with dangling pending_key"
                );
                let id = self.arena.push_object(pairs);
                JsonValue::Object(id)
            }
            OpenFrame::Pair { key, value } => {
                // The Pair finalisation lands on the enclosing
                // Object frame as a `JsonPair`. Skip the
                // `JsonValue::deposit` path — the parent expects a
                // pair, not a value.
                let key = key.expect("Pair closed without key");
                let value = value.expect("Pair closed without value");
                if let Some(OpenFrame::Object { pairs, .. }) = self.stack.last_mut() {
                    pairs.push(JsonPair { key, value });
                    return;
                }
                debug_assert!(false, "Pair closed outside Object context");
                // Fall through to deposit a string-as-value as a
                // recovery: defensive only, never fires in
                // well-formed generation.
                JsonValue::String(key)
            }
            OpenFrame::Wrap { value } => {
                value.expect("Wrap closed without forwarded value")
            }
        };
        self.deposit(value);
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        self.deposit(JsonValue::Number(JsonNumber::Float(value)));
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        self.deposit(JsonValue::Number(JsonNumber::Int(value)));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        self.deposit(JsonValue::Number(JsonNumber::UInt(value)));
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.deposit(JsonValue::Bool(value));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // The emitter passes a slice borrowed from the input or the
        // arena; both carry the `'p` lifetime via the parse
        // function's signature.
        // SAFETY: the slice's lifetime is bound to the parse call
        // site by the generated function's signature; the trait
        // surface elides this so concrete builders can specialise.
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(JsonValue::String(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(JsonValue::Null);
    }

    #[inline]
    fn push_branch_tag(&mut self, _branch_index: u32) {
        // JSON's `value` Alt collapses every branch into the unitary
        // `JsonValue`; the tag is retained at the trait surface for
        // audit symmetry but does not contribute to the projected
        // shape. Sheets / CSS tagged-enum projections store the
        // index in a per-builder slot — the trait surface stays
        // identical.
    }
}
