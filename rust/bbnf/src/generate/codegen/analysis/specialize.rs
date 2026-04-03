use bbnf_ir::IrNode;

#[derive(Clone, Copy, Debug, Default)]
pub struct InlineShapeStats {
    pub refs: usize,
    pub wrappers: usize,
    pub repeats: usize,
    pub alts: usize,
    pub token_dispatches: usize,
}

impl InlineShapeStats {
    pub const fn control_nodes(self) -> usize {
        self.repeats + self.alts + self.token_dispatches
    }

    pub const fn is_wrapper_heavy(self) -> bool {
        self.wrappers >= 3
    }
}

pub fn gather_inline_shape_stats(node: &IrNode) -> InlineShapeStats {
    let mut stats = InlineShapeStats::default();
    gather(node, &mut stats);
    stats
}

fn gather(node: &IrNode, stats: &mut InlineShapeStats) {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
        IrNode::Ref(_) => stats.refs += 1,
        IrNode::Seq(children) => {
            for child in children {
                gather(child, stats);
            }
        }
        IrNode::Alt(branches, _) => {
            stats.alts += 1;
            for branch in branches {
                gather(&branch.node, stats);
            }
        }
        IrNode::Repeat { inner, .. } => {
            stats.repeats += 1;
            gather(inner, stats);
        }
        IrNode::Skip(left, right) | IrNode::Next(left, right) | IrNode::Minus(left, right) => {
            stats.wrappers += 1;
            gather(left, stats);
            gather(right, stats);
        }
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            stats.wrappers += 1;
            gather(inner, stats);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            stats.token_dispatches += 1;
            gather(token, stats);
            for arm in arms {
                gather(&arm.continuation, stats);
            }
            gather(fallback, stats);
        }
    }
}

