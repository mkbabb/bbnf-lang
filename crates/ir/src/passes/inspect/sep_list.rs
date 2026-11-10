//! The strict separated list `x (sep x)*`.
//!
//! BBNF reads every expression literally (ruling S1-B, R-b: spec text
//! first). `( x << sep ? ) *` is a repetition of `x` each optionally
//! followed by `sep`, so it accepts `x x` and a trailing `sep`. A
//! grammar that means a strict separated list writes `x (sep x)*`
//! (one or more) or `(x (sep x)*)?` (zero or more). This module
//! recognizes exactly those two spellings, which every backend may
//! then lower to a separated-list loop: the loop accepts the same
//! language as the literal reading.
//!
//! In the IR the spellings are
//!
//! - `Seq([x, Repeat { inner: Next(sep, x), lo: 0, hi: MAX }])` → `lo = 1`
//! - `Repeat { inner: <the Seq above>, lo: 0, hi: 1 }` → `lo = 0`
//!
//! where the two `x` are structurally equal. Nothing else matches.

use crate::passes::inspect::{single_byte_literal, unwrap_map_ow};
use crate::{GrammarIR, IrNode};

/// A recognized strict separated list.
#[derive(Debug, Clone, Copy)]
pub struct SepList<'a> {
    /// The list element `x`.
    pub element: &'a IrNode,
    /// The separator `sep`.
    pub separator: &'a IrNode,
    /// Minimum element count: `0` for `(x (sep x)*)?`, `1` for `x (sep x)*`.
    pub lo: u32,
}

impl SepList<'_> {
    /// The separator's byte when it is a single-byte literal, looked
    /// at through `?w` / `Map` wrappers and one `Ref` hop
    /// (`comma = "," ?w`).
    pub fn separator_byte(&self, ir: &GrammarIR) -> Option<u8> {
        match unwrap_map_ow(self.separator) {
            IrNode::Ref(rid) => ir
                .rules
                .get(*rid as usize)
                .and_then(|rule| single_byte_literal(unwrap_map_ow(&rule.body), ir)),
            other => single_byte_literal(other, ir),
        }
    }
}

/// Recognize `x (sep x)*` (`lo = 1`) or `(x (sep x)*)?` (`lo = 0`).
pub fn sep_list(node: &IrNode) -> Option<SepList<'_>> {
    match node {
        IrNode::Repeat {
            inner,
            lo: 0,
            hi: 1,
        } => sep_list_many1(inner).map(|list| SepList { lo: 0, ..list }),
        _ => sep_list_many1(node),
    }
}

fn sep_list_many1(node: &IrNode) -> Option<SepList<'_>> {
    let IrNode::Seq(children) = node else {
        return None;
    };
    let [
        head,
        IrNode::Repeat {
            inner,
            lo: 0,
            hi: u32::MAX,
        },
    ] = children.as_slice()
    else {
        return None;
    };
    let IrNode::Next(separator, tail) = inner.as_ref() else {
        return None;
    };
    (head == tail.as_ref()).then_some(SepList {
        element: head,
        separator,
        lo: 1,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn many(inner: IrNode) -> IrNode {
        IrNode::Repeat {
            inner: Box::new(inner),
            lo: 0,
            hi: u32::MAX,
        }
    }
    fn opt(inner: IrNode) -> IrNode {
        IrNode::Repeat {
            inner: Box::new(inner),
            lo: 0,
            hi: 1,
        }
    }
    fn next(a: IrNode, b: IrNode) -> IrNode {
        IrNode::Next(Box::new(a), Box::new(b))
    }

    #[test]
    fn strict_spellings_match() {
        let x = IrNode::Ref(1);
        let sep = IrNode::Literal(2);
        let list = IrNode::Seq(vec![x.clone(), many(next(sep.clone(), x.clone()))]);
        assert_eq!(sep_list(&list).map(|l| l.lo), Some(1));
        assert_eq!(sep_list(&opt(list)).map(|l| l.lo), Some(0));
    }

    #[test]
    fn literal_optional_separator_form_does_not_match() {
        // `( x << sep ? ) *` reads literally; it is not a separated list.
        let x = IrNode::Ref(1);
        let sep = IrNode::Literal(2);
        let literal = many(IrNode::Skip(Box::new(x), Box::new(opt(sep))));
        assert!(sep_list(&literal).is_none());
    }

    #[test]
    fn differing_elements_do_not_match() {
        let list = IrNode::Seq(vec![
            IrNode::Ref(1),
            many(next(IrNode::Literal(2), IrNode::Ref(3))),
        ]);
        assert!(sep_list(&list).is_none());
    }
}
