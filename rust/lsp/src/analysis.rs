use tower_lsp_server::ls_types::*;

use crate::state::{DocumentInfo, RuleInfo};

// ---------------------------------------------------------------------------
// LineIndex — O(1) build, O(log n) lookup
// ---------------------------------------------------------------------------

/// Precomputed line-start offsets for O(log n) position conversion.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset of the start of each line. `line_starts[0]` is always 0.
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Build a line index from document text. O(n) in text length.
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, byte) in text.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Convert a byte offset to an LSP Position. O(log n) via binary search.
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let line = self.line_starts.partition_point(|&s| s <= offset).saturating_sub(1);
        let col = offset.saturating_sub(self.line_starts[line]);
        Position::new(line as u32, col as u32)
    }

    /// Convert an LSP Position to a byte offset. O(1).
    #[allow(dead_code)]
    pub fn position_to_offset(&self, pos: Position) -> usize {
        let line = pos.line as usize;
        if line < self.line_starts.len() {
            self.line_starts[line] + pos.character as usize
        } else {
            *self.line_starts.last().unwrap_or(&0)
        }
    }

    /// Convert byte offset span to an LSP Range.
    pub fn span_to_range(&self, start: usize, end: usize) -> Range {
        Range::new(self.offset_to_position(start), self.offset_to_position(end))
    }
}

// ---------------------------------------------------------------------------
// Legacy free functions — delegate to linear scan for callers that don't
// have a LineIndex (e.g., formatting which re-parses).
// ---------------------------------------------------------------------------

/// Convert a byte offset to an LSP Position (0-based line, 0-based character).
pub fn offset_to_position(text: &str, offset: usize) -> Position {
    let offset = offset.min(text.len());
    let mut line: u32 = 0;
    let mut col: u32 = 0;
    for (i, byte) in text.bytes().enumerate() {
        if i == offset {
            break;
        }
        if byte == b'\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position::new(line, col)
}

/// Convert an LSP Position to a byte offset.
pub fn position_to_offset(text: &str, position: Position) -> usize {
    let mut current_line: u32 = 0;
    let mut current_col: u32 = 0;
    for (i, byte) in text.bytes().enumerate() {
        if current_line == position.line && current_col == position.character {
            return i;
        }
        if byte == b'\n' {
            if current_line == position.line {
                // Position is past end of line — clamp.
                return i;
            }
            current_line += 1;
            current_col = 0;
        } else {
            current_col += 1;
        }
    }
    text.len()
}

/// Convert byte offset span to an LSP Range.
pub fn span_to_range(text: &str, start: usize, end: usize) -> Range {
    Range::new(offset_to_position(text, start), offset_to_position(text, end))
}

// ---------------------------------------------------------------------------
// Symbol lookup
// ---------------------------------------------------------------------------

/// What lives at a given byte offset?
#[derive(Debug)]
pub enum SymbolAtOffset<'a> {
    /// Cursor is on the LHS definition of a rule.
    RuleDefinition(&'a RuleInfo),
    /// Cursor is on a nonterminal reference in some rule's RHS.
    RuleReference {
        /// Name of the referenced nonterminal.
        name: String,
        /// The rule that contains this reference.
        containing_rule: &'a RuleInfo,
    },
}

/// Resolve what symbol is at the given byte offset.
pub fn symbol_at_offset<'a>(info: &'a DocumentInfo, offset: usize) -> Option<SymbolAtOffset<'a>> {
    // Check rule definitions first.
    for rule in &info.rules {
        if offset >= rule.name_span.0 && offset <= rule.name_span.1 {
            return Some(SymbolAtOffset::RuleDefinition(rule));
        }
    }
    // Check references in all rules.
    for rule in &info.rules {
        for refinfo in &rule.references {
            if offset >= refinfo.span.0 && offset <= refinfo.span.1 {
                return Some(SymbolAtOffset::RuleReference {
                    name: refinfo.name.clone(),
                    containing_rule: rule,
                });
            }
        }
    }
    None
}
