//! Literal inspection helpers.
//!
//! [`single_byte_literal`] tests whether a node is a [`IrNode::Literal`]
//! whose interned string is exactly one byte, returning that byte. The
//! recognizer miners use this to identify single-byte delimiters (open,
//! close, separator, pivot bytes, etc.).

use crate::{GrammarIR, IrNode};

/// Return the single byte of a one-byte [`IrNode::Literal`], or `None`
/// when the node is not a literal or its string is not exactly one
/// byte long.
///
/// String content is read directly from the interner — no allocation.
pub fn single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let s = ir.get_string(*sid);
        let bytes = s.as_bytes();
        if bytes.len() == 1 {
            return Some(bytes[0]);
        }
    }
    None
}
