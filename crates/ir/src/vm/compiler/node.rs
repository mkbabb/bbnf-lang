//! Per-`IrNode` dispatch — the small entry that fans out to the compound
//! compilers in [`super::compound`].

use crate::vm::bytecode::Op;
use crate::{GrammarIR, IrNode};

use super::Compiler;

impl Compiler {
    pub(super) fn compile_node(&mut self, node: &IrNode, ir: &GrammarIR) {
        match node {
            IrNode::Literal(sid) => {
                self.emit(Op::MatchString(*sid));
            }
            IrNode::Regex(sid) => {
                self.emit(Op::MatchRegex(*sid));
            }
            IrNode::Epsilon => {
                self.emit(Op::Epsilon);
            }
            IrNode::Ref(rule_id) => {
                self.emit(Op::Call(*rule_id));
            }
            IrNode::Seq(children) => self.compile_seq(children, ir),
            IrNode::Alt(branches, dispatch) => self.compile_alt(branches, dispatch.as_ref(), ir),
            IrNode::Repeat { inner, lo, hi } => self.compile_repeat(inner, *lo, *hi, ir),
            IrNode::Skip(left, right) => self.compile_binary_backtrack(left, right, true, ir),
            IrNode::Next(left, right) => self.compile_binary_backtrack(left, right, false, ir),
            IrNode::Minus(left, right) => self.compile_minus(left, right, ir),
            IrNode::Negate(inner) => self.compile_negate(inner, ir),
            IrNode::Map { inner, .. } => self.compile_node(inner, ir),
            IrNode::OptionalWhitespace(inner) => {
                // Use custom @ws pattern if available; otherwise basic ASCII whitespace.
                if let Some(ws_sid) = ir.ws_pattern {
                    self.emit(Op::TrimWsPattern(ws_sid));
                } else {
                    self.emit(Op::TrimWs);
                }
                self.compile_node(inner, ir);
                if let Some(ws_sid) = ir.ws_pattern {
                    self.emit(Op::TrimWsPattern(ws_sid));
                } else {
                    self.emit(Op::TrimWs);
                }
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                self.compile_token_dispatch(token, arms, fallback, ir);
            }
        }
    }
}
