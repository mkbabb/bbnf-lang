//! Line/offset mapping and breakpoint resolution.
//!
//! Bridges DAP line numbers (1-based) to grammar byte offsets and `RuleId`s.

use bbnf_ir::{GrammarIR, RuleId};

/// Pre-computed line start offsets for a source text.
pub struct LineIndex {
    /// Byte offset of the start of each line (0-indexed).
    line_starts: Vec<u32>,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut starts = vec![0u32];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                starts.push((i + 1) as u32);
            }
        }
        Self {
            line_starts: starts,
        }
    }

    /// Convert a 1-based line number to a byte offset (start of that line).
    pub fn line_to_offset(&self, line_1based: u32) -> u32 {
        let idx = (line_1based.saturating_sub(1)) as usize;
        self.line_starts.get(idx).copied().unwrap_or(0)
    }

    /// Convert a byte offset to a 1-based line number.
    pub fn offset_to_line(&self, offset: u32) -> u32 {
        match self.line_starts.binary_search(&offset) {
            Ok(i) => (i + 1) as u32,
            Err(i) => i as u32, // offset is within line i (0-indexed) → line i (1-based).
        }
    }
}

/// Find the `RuleId` whose `source_span` contains the given byte offset.
pub fn rule_at_offset(ir: &GrammarIR, offset: u32) -> Option<RuleId> {
    ir.rules
        .iter()
        .find(|r| {
            r.source_span
                .as_ref()
                .is_some_and(|s| s.start <= offset && offset < s.end)
        })
        .map(|r| r.id)
}

/// Resolve a DAP breakpoint line to a `RuleId` and the verified line.
///
/// Returns `(rule_id, snapped_line)` — the breakpoint is snapped to the
/// rule definition line closest to the requested line.
pub fn resolve_breakpoint(
    ir: &GrammarIR,
    line_index: &LineIndex,
    line_1based: u32,
) -> Option<(RuleId, u32)> {
    let offset = line_index.line_to_offset(line_1based);
    // Exact match: a rule starts at or contains this offset.
    if let Some(rule_id) = rule_at_offset(ir, offset) {
        let rule = &ir.rules[rule_id as usize];
        let snapped_line = rule
            .source_span
            .as_ref()
            .map(|s| line_index.offset_to_line(s.start))
            .unwrap_or(line_1based);
        return Some((rule_id, snapped_line));
    }
    // Nearest: find the closest rule after this line.
    ir.rules
        .iter()
        .filter_map(|r| {
            r.source_span.as_ref().and_then(|s| {
                if s.start >= offset {
                    Some((r.id, line_index.offset_to_line(s.start)))
                } else {
                    None
                }
            })
        })
        .min_by_key(|(_, line)| *line)
}

/// Get the rule name for a `RuleId`.
pub fn rule_name(ir: &GrammarIR, rule_id: RuleId) -> &str {
    let rule = &ir.rules[rule_id as usize];
    ir.get_string(rule.name)
}
