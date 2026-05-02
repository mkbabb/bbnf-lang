use std::collections::{HashMap, HashSet};

use ls_types::*;

use crate::analysis::LineIndex;

use super::super::types::{DocumentInfo, ImportInfo, ParseDiagnostics, RecoverInfo};

/// Build a stub `DocumentInfo` for the parser-panic early-return path.
pub(super) fn empty_doc_info_for_panic(msg: &str) -> DocumentInfo {
    let pos = Position::new(0, 0);
    let diag = Diagnostic {
        range: Range::new(pos, pos),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(crate::DIAGNOSTIC_SOURCE.into()),
        message: format!("Parse error: {}", msg),
        ..Default::default()
    };
    DocumentInfo {
        rules: Vec::new(),
        diagnostics: vec![diag],
        rule_index: HashMap::new(),
        semantic_tokens: Vec::new(),
        first_set_labels: HashMap::new(),
        nullable_rules: HashSet::new(),
        cyclic_rule_paths: HashMap::new(),
        imports: Vec::new(),
        recovers: Vec::new(),
        pretties: Vec::new(),
        debugs: Vec::new(),
        tokens: Vec::new(),
        ws_pattern: None,
        ir_meta: HashMap::new(),
    }
}

/// Build a stub `DocumentInfo` for the parse-failure early-return path (no cached AST).
pub(super) fn empty_doc_info_for_failure(
    line_index: &LineIndex,
    parse_diag: &ParseDiagnostics,
) -> DocumentInfo {
    let offset = parse_diag.furthest_offset.max(parse_diag.offset);
    let pos = line_index.offset_to_position(offset);
    let diag = Diagnostic {
        range: Range::new(pos, pos),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(crate::DIAGNOSTIC_SOURCE.into()),
        message: format!(
            "Parse error at offset {} (line {}, col {})",
            offset,
            pos.line + 1,
            pos.character + 1
        ),
        ..Default::default()
    };
    DocumentInfo {
        rules: Vec::new(),
        diagnostics: vec![diag],
        rule_index: HashMap::new(),
        semantic_tokens: Vec::new(),
        first_set_labels: HashMap::new(),
        nullable_rules: HashSet::new(),
        cyclic_rule_paths: HashMap::new(),
        imports: Vec::new(),
        recovers: Vec::new(),
        pretties: Vec::new(),
        debugs: Vec::new(),
        tokens: Vec::new(),
        ws_pattern: None,
        ir_meta: HashMap::new(),
    }
}

/// Build a stub `DocumentInfo` for the empty-AST-on-non-empty-input early-return path.
pub(super) fn empty_doc_info_for_empty_ast(
    text: &str,
    line_index: &LineIndex,
    parse_diag: &ParseDiagnostics,
    import_infos: Vec<ImportInfo>,
    recover_infos: Vec<RecoverInfo>,
) -> DocumentInfo {
    let furthest = parse_diag.furthest_offset.max(parse_diag.offset);
    let pos = line_index.offset_to_position(furthest.min(text.len()));
    let diag = Diagnostic {
        range: Range::new(Position::new(0, 0), pos),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(crate::DIAGNOSTIC_SOURCE.into()),
        message: "Failed to parse any rules. Check syntax (each rule needs: name = expression ;)"
            .into(),
        ..Default::default()
    };
    DocumentInfo {
        rules: Vec::new(),
        diagnostics: vec![diag],
        rule_index: HashMap::new(),
        semantic_tokens: Vec::new(),
        first_set_labels: HashMap::new(),
        nullable_rules: HashSet::new(),
        cyclic_rule_paths: HashMap::new(),
        imports: import_infos,
        recovers: recover_infos,
        pretties: Vec::new(),
        debugs: Vec::new(),
        tokens: Vec::new(),
        ws_pattern: None,
        ir_meta: HashMap::new(),
    }
}
