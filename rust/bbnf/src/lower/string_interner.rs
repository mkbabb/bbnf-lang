//! String interning table used during lowering.

use std::collections::HashMap;

use bbnf_ir::StringId;

/// String interning table used during lowering.
pub(crate) struct StringInterner {
    pub(crate) strings: Vec<String>,
    map: HashMap<String, StringId>,
}

impl StringInterner {
    pub(crate) fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub(crate) fn intern(&mut self, s: &str) -> StringId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = self.strings.len() as StringId;
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }
}
