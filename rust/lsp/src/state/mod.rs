mod ast_utils;
mod diagnostics;
mod parsing;
pub mod types;

pub use ast_utils::compute_expression_end_pub;
pub use types::*;

#[cfg(test)]
pub use diagnostics::analyze;

use bbnf::types::AST;

use crate::analysis::LineIndex;

use diagnostics::analyze_from_cache;
use parsing::{OwnedAst, parse_once};

/// Stored per-document: raw text + analyzed info + precomputed line index + cached AST.
pub struct DocumentState {
    pub text: String,
    pub info: DocumentInfo,
    /// Precomputed line-start offsets for O(log n) position conversion.
    pub line_index: LineIndex,
    /// Cached parsed AST (self-referential: borrows from its own String).
    ast_cell: OwnedAst,
}

impl DocumentState {
    pub fn new(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        // Parse once: the OwnedAst captures the AST, and we extract diagnostics
        // via a Cell to avoid double-parsing.
        let parse_diag = std::cell::Cell::new(None::<ParseDiagnostics>);
        let ast_cell = OwnedAst::new(text.clone(), |src| {
            let (cached, diag) = parse_once(src);
            parse_diag.set(Some(diag));
            cached
        });
        let diag = parse_diag.into_inner().unwrap();

        let info = analyze_from_cache(
            &text,
            &line_index,
            ast_cell.borrow_dependent().as_ref(),
            &diag,
        );
        Self {
            text,
            info,
            line_index,
            ast_cell,
        }
    }

    pub fn update(&mut self, text: String) {
        // Fast path: if text unchanged from the last parsed source, skip everything.
        // Compare against the ast_cell's owned string (the source the AST was built from),
        // not self.text, because the server's did_change handler may have already mutated
        // self.text via apply_incremental_changes before calling update().
        if *self.ast_cell.borrow_owner() == text {
            return;
        }

        let line_index = LineIndex::new(&text);
        let parse_diag = std::cell::Cell::new(None::<ParseDiagnostics>);
        let ast_cell = OwnedAst::new(text.clone(), |src| {
            let (cached, diag) = parse_once(src);
            parse_diag.set(Some(diag));
            cached
        });
        let diag = parse_diag.into_inner().unwrap();

        self.info = analyze_from_cache(
            &text,
            &line_index,
            ast_cell.borrow_dependent().as_ref(),
            &diag,
        );

        self.line_index = line_index;
        self.ast_cell = ast_cell;
        self.text = text;
    }

    /// Access the cached AST (borrows from internally-owned text).
    pub fn ast(&self) -> Option<&AST<'_>> {
        self.ast_cell.borrow_dependent().as_ref().map(|c| &c.ast)
    }
}
