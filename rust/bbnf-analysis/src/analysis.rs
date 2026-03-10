use ls_types::*;

use crate::state::{DocumentInfo, RuleInfo};

// ---------------------------------------------------------------------------
// LineIndex — O(1) build, O(log n) lookup
// ---------------------------------------------------------------------------

/// Precomputed line-start offsets for O(log n) position conversion.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset of the start of each line. `line_starts[0]` is always 0.
    line_starts: Vec<usize>,
    /// Total text length in bytes.
    text_len: usize,
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
        Self {
            line_starts,
            text_len: text.len(),
        }
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
            let line_start = self.line_starts[line];
            let line_end = if line + 1 < self.line_starts.len() {
                // Clamp to the '\n' byte for non-last lines.
                self.line_starts[line + 1].saturating_sub(1)
            } else {
                // Last line can clamp to EOF.
                self.text_len
            };
            let requested = line_start + pos.character as usize;
            requested.min(line_end)
        } else {
            panic!(
                "position_to_offset received out-of-range line {} (line_count={})",
                line,
                self.line_starts.len()
            );
        }
    }

    /// Convert byte offset span to an LSP Range.
    pub fn span_to_range(&self, start: usize, end: usize) -> Range {
        Range::new(self.offset_to_position(start), self.offset_to_position(end))
    }
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
        /// Byte span of this reference token.
        span: (usize, usize),
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
                    span: refinfo.span,
                });
            }
        }
    }
    // Check @recover directive rule names.
    for rec in &info.recovers {
        if offset >= rec.rule_name_span.0 && offset <= rec.rule_name_span.1 {
            return Some(SymbolAtOffset::RuleReference {
                name: rec.rule_name.clone(),
                span: rec.rule_name_span,
            });
        }
    }
    // Check @no_collapse directive rule names.
    for nc in &info.no_collapses {
        if offset >= nc.rule_name_span.0 && offset <= nc.rule_name_span.1 {
            return Some(SymbolAtOffset::RuleReference {
                name: nc.rule_name.clone(),
                span: nc.rule_name_span,
            });
        }
    }
    // Check @pretty directive rule names.
    for pretty in &info.pretties {
        if offset >= pretty.rule_name_span.0 && offset <= pretty.rule_name_span.1 {
            return Some(SymbolAtOffset::RuleReference {
                name: pretty.rule_name.clone(),
                span: pretty.rule_name_span,
            });
        }
    }
    None
}
