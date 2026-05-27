use crate::runtime::json::arena::{JsonArena, JsonArrayId, JsonObjectId};
use crate::runtime::json::value::{JsonPair, JsonValue};
use crate::runtime::path::{Path, PathSegment};
#[derive(Debug)]
pub struct JsonDocument<'p> {
    pub arena: JsonArena<'p>,
    pub root: JsonValue<'p>,
    pub input: &'p str,
}
impl<'p> JsonDocument<'p> {
    #[inline]
    pub fn new(arena: JsonArena<'p>, root: JsonValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }
    #[inline]
    pub fn root(&self) -> &JsonValue<'p> {
        &self.root
    }
    #[inline]
    pub fn arena(&self) -> &JsonArena<'p> {
        &self.arena
    }
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }
    #[inline]
    pub fn array(&self, id: JsonArrayId) -> &[JsonValue<'p>] {
        self.arena.array(id)
    }
    #[inline]
    pub fn object(&self, id: JsonObjectId) -> &[JsonPair<'p>] {
        self.arena.object(id)
    }
    #[inline]
    pub fn view<'a>(&'a self) -> JsonView<'a, 'p> {
        JsonView {
            doc: self,
            focus: self.root,
        }
    }
    #[inline]
    pub fn to_value(&self) -> &JsonValue<'p> {
        &self.root
    }
    #[inline]
    pub fn get<T: JsonPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}
#[derive(Debug, Clone, Copy)]
pub struct JsonView<'a, 'p: 'a> {
    pub(crate) doc: &'a JsonDocument<'p>,
    pub(crate) focus: JsonValue<'p>,
}
impl<'a, 'p: 'a> JsonView<'a, 'p> {
    #[inline]
    pub fn focused(doc: &'a JsonDocument<'p>, focus: JsonValue<'p>) -> Self {
        Self { doc, focus }
    }
    #[inline]
    pub fn document(&self) -> &'a JsonDocument<'p> {
        self.doc
    }
    #[inline]
    pub fn focus(&self) -> JsonValue<'p> {
        self.focus
    }
    #[inline]
    pub fn root(&self) -> &'a JsonValue<'p> {
        &self.doc.root
    }
    #[inline]
    pub fn arena(&self) -> &'a JsonArena<'p> {
        &self.doc.arena
    }
    #[inline]
    pub fn array(&self, id: JsonArrayId) -> &'a [JsonValue<'p>] {
        self.doc.array(id)
    }
    #[inline]
    pub fn object(&self, id: JsonObjectId) -> &'a [JsonPair<'p>] {
        self.doc.object(id)
    }
    #[inline]
    pub fn kind(&self) -> JsonKind {
        match &self.focus {
            JsonValue::Null => JsonKind::Null,
            JsonValue::Bool(_) => JsonKind::Bool,
            JsonValue::Number(_) => JsonKind::Number,
            JsonValue::String(_) => JsonKind::String,
            JsonValue::Array(_) => JsonKind::Array,
            JsonValue::Object(_) => JsonKind::Object,
        }
    }
    #[inline]
    pub fn is_object(&self) -> bool {
        matches!(self.focus, JsonValue::Object(_))
    }
    #[inline]
    pub fn is_array(&self) -> bool {
        matches!(self.focus, JsonValue::Array(_))
    }
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self.focus, JsonValue::String(_))
    }
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, JsonValue::Number(_))
    }
    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.focus, JsonValue::Bool(_))
    }
    #[inline]
    pub fn is_null(&self) -> bool {
        matches!(self.focus, JsonValue::Null)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JsonKind {
    Null,
    Bool,
    Number,
    String,
    Array,
    Object,
}
pub trait JsonPathQuery: Sized {
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self>;
}
#[inline]
fn walk_path<'a, 'p>(
    doc: &'a JsonDocument<'p>,
    path: Path<'_>,
) -> Option<&'a JsonValue<'p>> {
    let mut current: &'a JsonValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (JsonValue::Object(id), PathSegment::Field(name)) => {
                let pairs = doc.object(*id);
                let pair = pairs.iter().find(|p| p.key == *name)?;
                &pair.value
            }
            (JsonValue::Array(id), PathSegment::Index(idx)) => {
                let items = doc.array(*id);
                items.get(*idx)?
            }
            _ => return None,
        };
    }
    Some(current)
}
impl JsonPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        match value {
            JsonValue::String(s) => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}
impl JsonPathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            JsonValue::Number(n) => Some(n.as_f64()),
            _ => None,
        }
    }
}
impl JsonPathQuery for bool {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            JsonValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}
impl JsonPathQuery for JsonValue<'_> {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: JsonValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<JsonValue<'p>, JsonValue<'_>>(copied) })
    }
}
