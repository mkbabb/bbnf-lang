use bbnf::backend::rust::analysis::inline::should_force_direct_call;
use bbnf::backend::rust::analysis::specialize::gather_inline_shape_stats;
use bbnf_ir::{AltBranch, IrNode};

#[test]
fn wrapper_heavy_multi_ref_rules_prefer_direct_calls() {
    let node = IrNode::Repeat {
        inner: Box::new(IrNode::Seq(vec![
            IrNode::OptionalWhitespace(Box::new(IrNode::Ref(1))),
            IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::OptionalWhitespace(Box::new(IrNode::Ref(2))),
                        first_set: None,
                    },
                    AltBranch {
                        node: IrNode::OptionalWhitespace(Box::new(IrNode::Ref(3))),
                        first_set: None,
                    },
                ],
                None,
            ),
        ])),
        lo: 0,
        hi: u32::MAX,
    };

    let shape = gather_inline_shape_stats(&node);
    assert!(should_force_direct_call(shape, 56, 1600));
}

#[test]
fn tiny_leaf_rules_stay_inline_eligible() {
    let shape = gather_inline_shape_stats(&IrNode::Literal(0));
    assert!(!should_force_direct_call(shape, 2, 2));
}
