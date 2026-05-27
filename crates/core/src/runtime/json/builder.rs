use bbnf_ir::registry::{LayoutKind, StructLayout};
use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
use crate::runtime::json::arena::JsonArena;
use crate::runtime::json::document::JsonDocument;
use crate::runtime::json::value::{JsonNumber, JsonPair, JsonValue};
#[derive(Debug, Clone)]
enum OpenFrame<'p> {
    Array { items: Vec<JsonValue<'p>> },
    Object { pairs: Vec<JsonPair<'p>>, pending_key: Option<&'p str> },
    Pair { key: Option<&'p str>, value: Option<JsonValue<'p>> },
    Wrap { value: Option<JsonValue<'p>> },
}
#[derive(Debug)]
pub struct JsonStructBuilder<'p> {
    arena: JsonArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<JsonValue<'p>>,
    next_handle: u64,
}
#[derive(Debug, Clone)]
pub struct JsonStructCheckpoint<'p> {
    arrays: usize,
    objects: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<JsonValue<'p>>,
    next_handle: u64,
}
impl<'p> Default for JsonStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'p> JsonStructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: JsonArena::new(),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }
    #[inline]
    pub fn with_capacity(arrays: usize, objects: usize) -> Self {
        Self {
            arena: JsonArena::with_capacity(arrays, objects),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }
    #[inline]
    pub fn finalise(mut self, input: &'p str) -> JsonDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "JsonStructBuilder::finalise called with {} open frame(s)", self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("JsonStructBuilder::finalise called before any value emission");
        JsonDocument::new(self.arena, root, input)
    }
    #[inline]
    fn deposit(&mut self, value: JsonValue<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(OpenFrame::Array { items }) => items.push(value),
            Some(OpenFrame::Object { pairs, pending_key }) => {
                if pending_key.is_none() {
                    if let JsonValue::String(s) = value {
                        *pending_key = Some(s);
                        return;
                    }
                }
                debug_assert!(
                    pending_key.is_some(), "Object value pushed without pending key"
                );
                if let Some(key) = pending_key.take() {
                    pairs.push(JsonPair { key, value });
                }
            }
            Some(OpenFrame::Pair { key, value: slot }) => {
                if key.is_none() {
                    if let JsonValue::String(s) = value {
                        *key = Some(s);
                    } else {
                        debug_assert!(
                            false, "Pair key slot received non-string {:?}", value
                        );
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
    type Checkpoint = JsonStructCheckpoint<'p>;
    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        JsonStructCheckpoint {
            arrays: self.arena.array_count(),
            objects: self.arena.object_count(),
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
        }
    }
    #[inline]
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.arena.truncate(checkpoint.arrays, checkpoint.objects);
        self.stack = checkpoint.stack;
        self.root = checkpoint.root;
        self.next_handle = checkpoint.next_handle;
    }
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let frame = match (layout.kind, layout.rule_id) {
            (LayoutKind::Struct, 5)
            | (LayoutKind::TaggedEnum, 5)
            | (LayoutKind::UntaggedEnum, 5) => {
                OpenFrame::Array {
                    items: Vec::new(),
                }
            }
            (LayoutKind::Struct, 4)
            | (LayoutKind::TaggedEnum, 4)
            | (LayoutKind::UntaggedEnum, 4) => {
                OpenFrame::Object {
                    pairs: Vec::new(),
                    pending_key: None,
                }
            }
            (LayoutKind::Struct, 6) => {
                OpenFrame::Pair {
                    key: None,
                    value: None,
                }
            }
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
                    pending_key.is_none(), "Object closed with dangling pending_key"
                );
                let id = self.arena.push_object(pairs);
                JsonValue::Object(id)
            }
            OpenFrame::Pair { key, value } => {
                let key = key.expect("Pair closed without key");
                let value = value.expect("Pair closed without value");
                if let Some(OpenFrame::Object { pairs, .. }) = self.stack.last_mut() {
                    pairs.push(JsonPair { key, value });
                    return;
                }
                debug_assert!(false, "Pair closed outside Object context");
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
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(JsonValue::String(lifetime_extended));
    }
    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(JsonValue::Null);
    }
    #[inline]
    fn push_branch_tag(&mut self, _branch_index: u32) {}
}
