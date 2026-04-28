//! AZ-I.W2-act.close A.fix — [`crate::runtime::RuntimeView`] impl
//! for [`super::JsonView`].
//!
//! The JSON struct-direct runtime focuses on a [`JsonValue`] within
//! a [`JsonDocument`]; the trait's `Kind` associated type is the
//! existing [`JsonKind`] discriminator. `kind()` reports the focused
//! value's typed shape; `span()` returns the borrowed source slice
//! for [`JsonValue::String`] focuses (the only JSON shape that
//! carries a single contiguous source slice); `input()` returns the
//! full input the parse consumed; `children()` walks the focused
//! compound's structural children — array elements for
//! [`JsonValue::Array`], pair values for [`JsonValue::Object`],
//! empty for any leaf shape.

use crate::runtime::RuntimeView;
use crate::runtime::json::document::{JsonKind, JsonView};
use crate::runtime::json::value::JsonValue;

impl<'a, 'p: 'a> RuntimeView<'p> for JsonView<'a, 'p> {
    type Kind = JsonKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            JsonValue::Null => JsonKind::Null,
            JsonValue::Bool(_) => JsonKind::Bool,
            JsonValue::Number(_) => JsonKind::Number,
            JsonValue::String(_) => JsonKind::String,
            JsonValue::Array(_) => JsonKind::Array,
            JsonValue::Object(_) => JsonKind::Object,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        // String leaves carry a borrowed input slice (zero-copy or
        // arena-decoded); other shapes do not project to a single
        // contiguous source slice. Numbers, bools, null, and the
        // arena-handle compounds all decline a `span()` projection.
        match self.focus {
            JsonValue::String(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    fn input(&self) -> &'p str {
        self.doc.input
    }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        JsonChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

/// AZ-I.W2-act.close A.fix — child iterator for JSON views.
///
/// Walks the focused value's structural children in source order:
/// [`JsonValue::Array`] yields one sub-view per element;
/// [`JsonValue::Object`] yields one sub-view per pair value (keys
/// are not structural children — the trait surface treats key /
/// value as a single navigation step keyed on order). Leaf shapes
/// (`Null`, `Bool`, `Number`, `String`) yield nothing.
pub struct JsonChildrenIter<'a, 'p: 'a> {
    doc: &'a crate::runtime::json::JsonDocument<'p>,
    focus: JsonValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for JsonChildrenIter<'a, 'p> {
    type Item = JsonView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            JsonValue::Array(id) => {
                let items = self.doc.array(id);
                let item = items.get(self.index)?;
                self.index += 1;
                Some(JsonView::focused(self.doc, *item))
            }
            JsonValue::Object(id) => {
                let pairs = self.doc.object(id);
                let pair = pairs.get(self.index)?;
                self.index += 1;
                Some(JsonView::focused(self.doc, pair.value))
            }
            _ => None,
        }
    }
}
