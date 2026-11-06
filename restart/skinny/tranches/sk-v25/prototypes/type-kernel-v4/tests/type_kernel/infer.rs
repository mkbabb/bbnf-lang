use bbnf_type_kernel_v4::*;

use super::super::operations;

#[test]
fn deep_flat_host_descriptor_exhausts_and_succeeds_on_a_small_stack() {
    let spec = deep_spec(20_000);
    let error = KernelInput::snapshot(spec.clone(), &mut OperationContext::new(256))
        .expect_err("small budget must exhaust");
    assert!(matches!(error.kind(), TypeErrorKind::Exhausted { .. }));
    assert!(!error.proof().nodes().is_empty());

    let thread = std::thread::Builder::new()
        .stack_size(512 * 1_024)
        .spawn(move || {
            let mut operations = OperationContext::new(20_000_000);
            let input = KernelInput::snapshot(spec, &mut operations).expect("snapshot");
            let problem = infer(&input, &mut operations).expect("infer");
            let solution = solve(problem, &mut operations).expect("solve");
            (
                solution.semantic_digest().as_str().to_owned(),
                operations.used(),
            )
        })
        .expect("spawn bounded-stack thread");
    let (digest, used) = thread.join().expect("deep thread must not abort");
    assert!(digest.starts_with("fnv1a64:"));
    assert!(used > 20_000);
}

#[test]
fn whole_grammar_vocabulary_is_total_and_snapshot_order_is_metamorphic() {
    let spec = whole_grammar();
    let mut reversed = spec.clone();
    reversed.rules.reverse();
    reversed.nodes.reverse();
    let mut first_operations = operations();
    let mut second_operations = operations();
    let first = KernelInput::snapshot(spec, &mut first_operations).expect("first snapshot");
    let second = KernelInput::snapshot(reversed, &mut second_operations).expect("second snapshot");
    assert_eq!(first.digest(), second.digest());
    assert_eq!(first.node_count(), 27);
    let problem = infer(&first, &mut first_operations).expect("whole-grammar inference");
    let solution = solve(problem, &mut first_operations).expect("whole-grammar solve");
    for id in 0..27 {
        assert!(
            solution
                .export_root(&ExportKey::new(format!("node:{id}")))
                .is_some()
        );
    }
    for name in ["scalar", "whole"] {
        assert!(
            solution
                .export_root(&ExportKey::new(format!("rule:{name}")))
                .is_some()
        );
    }
}

#[test]
fn missing_host_signature_is_a_named_clean_break() {
    let grammar = GrammarSpec {
        rules: vec![RuleSpec {
            id: RuleId(0),
            name: "root".to_owned(),
            root: NodeId(1),
            declared_type: None,
            transparent: false,
        }],
        nodes: vec![
            (NodeId(0), GrammarNode::Epsilon),
            (
                NodeId(1),
                GrammarNode::Map {
                    child: NodeId(0),
                    function: FnDescriptor::Expr {
                        function: 17,
                        return_type: None,
                        effect: HostEffect::Fallible,
                    },
                },
            ),
        ],
        host_types: Vec::new(),
        source_name: "host-signature".to_owned(),
    };
    let mut operations = operations();
    let input = KernelInput::snapshot(grammar, &mut operations).expect("snapshot");
    let error = infer(&input, &mut operations).expect_err("missing signature");
    assert_eq!(
        error.kind(),
        &TypeErrorKind::MissingHostSignature { function: 17 }
    );
}

#[test]
fn recursive_json_is_one_assay_not_a_hardcoded_schema() {
    let grammar = GrammarSpec {
        rules: vec![
            RuleSpec {
                id: RuleId(0),
                name: "Json".to_owned(),
                root: NodeId(5),
                declared_type: None,
                transparent: false,
            },
            RuleSpec {
                id: RuleId(1),
                name: "Array".to_owned(),
                root: NodeId(3),
                declared_type: None,
                transparent: false,
            },
        ],
        nodes: vec![
            (NodeId(0), GrammarNode::Literal("null".to_owned())),
            (
                NodeId(1),
                GrammarNode::Ref {
                    rule: RuleId(0),
                    mode: RefMode::Scalar,
                },
            ),
            (
                NodeId(2),
                GrammarNode::Repeat {
                    child: NodeId(1),
                    one_or_more: false,
                    collapse_span: false,
                },
            ),
            (
                NodeId(3),
                GrammarNode::Seq {
                    items: vec![NodeId(2)],
                    policy: SeqPolicy::default(),
                },
            ),
            (
                NodeId(4),
                GrammarNode::Ref {
                    rule: RuleId(1),
                    mode: RefMode::Scalar,
                },
            ),
            (
                NodeId(5),
                GrammarNode::Alt {
                    branches: vec![NodeId(0), NodeId(4)],
                    homogeneous: false,
                },
            ),
        ],
        host_types: Vec::new(),
        source_name: "recursive-json".to_owned(),
    };
    let mut operations = operations();
    let input = KernelInput::snapshot(grammar, &mut operations).expect("snapshot");
    let solution = solve(
        infer(&input, &mut operations).expect("infer"),
        &mut operations,
    )
    .expect("solve");
    let root = solution
        .export_root(&ExportKey::new("rule:Json"))
        .and_then(|root| solution.root_type(root))
        .expect("json root");
    assert!(matches!(
        solution.view(root),
        Some(SolvedTypeView::Variant(_))
    ));
}

#[test]
fn css_l4_shapes_are_generically_representable_without_css_kernel_logic() {
    let grammar = css_shape_grammar();
    let mut operations = operations();
    let input = KernelInput::snapshot(grammar, &mut operations).expect("css shape snapshot");
    let solution = solve(
        infer(&input, &mut operations).expect("css shape inference"),
        &mut operations,
    )
    .expect("css shape solve");
    let root = solution
        .export_root(&ExportKey::new("rule:CssValue"))
        .and_then(|root| solution.root_type(root))
        .expect("css root");
    assert!(matches!(
        solution.view(root),
        Some(SolvedTypeView::Variant(_))
    ));
}

fn deep_spec(depth: usize) -> GrammarSpec {
    let mut host_types = vec![HostTypeNode::Primitive(Primitive::U8)];
    for index in 0..depth {
        host_types.push(HostTypeNode::Option(HostTypeId(index as u32)));
    }
    GrammarSpec {
        rules: vec![RuleSpec {
            id: RuleId(0),
            name: "deep".to_owned(),
            root: NodeId(1),
            declared_type: None,
            transparent: false,
        }],
        nodes: vec![
            (NodeId(0), GrammarNode::Epsilon),
            (
                NodeId(1),
                GrammarNode::Map {
                    child: NodeId(0),
                    function: FnDescriptor::Expr {
                        function: 1,
                        return_type: Some(HostTypeId(depth as u32)),
                        effect: HostEffect::Pure,
                    },
                },
            ),
        ],
        host_types,
        source_name: "deep-host-type".to_owned(),
    }
}

fn whole_grammar() -> GrammarSpec {
    let host_types = vec![
        HostTypeNode::Primitive(Primitive::Span),
        HostTypeNode::Primitive(Primitive::U8),
        HostTypeNode::Named("HostSymbol".to_owned()),
        HostTypeNode::Tuple(vec![HostTypeId(1), HostTypeId(2)]),
        HostTypeNode::List(HostTypeId(3)),
        HostTypeNode::Option(HostTypeId(4)),
        HostTypeNode::Function {
            parameters: vec![HostTypeId(1)],
            result: HostTypeId(5),
        },
        HostTypeNode::Record(vec![
            ("item".to_owned(), HostTypeId(1)),
            ("maybe".to_owned(), HostTypeId(5)),
        ]),
        HostTypeNode::Variant(vec![
            ("scalar".to_owned(), HostTypeId(1)),
            ("record".to_owned(), HostTypeId(7)),
        ]),
    ];
    let nodes = vec![
        (NodeId(0), GrammarNode::Literal("x".to_owned())),
        (NodeId(1), GrammarNode::Regex("[a-z]+".to_owned())),
        (NodeId(2), GrammarNode::Epsilon),
        (
            NodeId(3),
            GrammarNode::Seq {
                items: Vec::new(),
                policy: SeqPolicy::default(),
            },
        ),
        (
            NodeId(4),
            GrammarNode::Seq {
                items: vec![NodeId(0)],
                policy: SeqPolicy::default(),
            },
        ),
        (
            NodeId(5),
            GrammarNode::Seq {
                items: vec![NodeId(0), NodeId(1)],
                policy: SeqPolicy::default(),
            },
        ),
        (
            NodeId(6),
            GrammarNode::Alt {
                branches: vec![NodeId(0), NodeId(1)],
                homogeneous: true,
            },
        ),
        (
            NodeId(7),
            GrammarNode::Alt {
                branches: vec![NodeId(0), NodeId(2)],
                homogeneous: false,
            },
        ),
        (
            NodeId(8),
            GrammarNode::Optional {
                child: NodeId(0),
                collapse_span: true,
            },
        ),
        (
            NodeId(9),
            GrammarNode::Optional {
                child: NodeId(7),
                collapse_span: false,
            },
        ),
        (
            NodeId(10),
            GrammarNode::Repeat {
                child: NodeId(0),
                one_or_more: false,
                collapse_span: true,
            },
        ),
        (
            NodeId(11),
            GrammarNode::Repeat {
                child: NodeId(7),
                one_or_more: true,
                collapse_span: false,
            },
        ),
        (
            NodeId(12),
            GrammarNode::Ref {
                rule: RuleId(0),
                mode: RefMode::Scalar,
            },
        ),
        (
            NodeId(13),
            GrammarNode::Ref {
                rule: RuleId(0),
                mode: RefMode::Compound,
            },
        ),
        (
            NodeId(14),
            GrammarNode::Ref {
                rule: RuleId(0),
                mode: RefMode::Vector,
            },
        ),
        map(15, FnDescriptor::BoxWrap),
        map(16, FnDescriptor::NumberConvert),
        map(17, FnDescriptor::HexConvert),
        map(18, FnDescriptor::SpanCapture),
        map(
            19,
            FnDescriptor::EnumWrap {
                variant: "value".to_owned(),
            },
        ),
        map(
            20,
            FnDescriptor::Expr {
                function: 20,
                return_type: Some(HostTypeId(8)),
                effect: HostEffect::Stateful,
            },
        ),
        (
            NodeId(21),
            GrammarNode::Skip {
                kept: NodeId(20),
                discarded: NodeId(0),
            },
        ),
        (
            NodeId(22),
            GrammarNode::Next {
                discarded: NodeId(0),
                kept: NodeId(20),
            },
        ),
        (
            NodeId(23),
            GrammarNode::Minus {
                kept: NodeId(20),
                predicate: NodeId(1),
            },
        ),
        (NodeId(24), GrammarNode::Negate(NodeId(0))),
        (NodeId(25), GrammarNode::OptionalWhitespace(NodeId(20))),
        (
            NodeId(26),
            GrammarNode::TokenDispatch {
                arms: vec![("x".to_owned(), NodeId(20))],
                fallback: NodeId(7),
            },
        ),
    ];
    GrammarSpec {
        rules: vec![
            RuleSpec {
                id: RuleId(0),
                name: "scalar".to_owned(),
                root: NodeId(0),
                declared_type: Some(HostTypeId(0)),
                transparent: true,
            },
            RuleSpec {
                id: RuleId(1),
                name: "whole".to_owned(),
                root: NodeId(26),
                declared_type: None,
                transparent: false,
            },
        ],
        nodes,
        host_types,
        source_name: "whole-vocabulary".to_owned(),
    }
}

fn map(id: u32, function: FnDescriptor) -> (NodeId, GrammarNode) {
    (
        NodeId(id),
        GrammarNode::Map {
            child: NodeId(0),
            function,
        },
    )
}

fn css_shape_grammar() -> GrammarSpec {
    let host_types = vec![
        HostTypeNode::Primitive(Primitive::F64),
        HostTypeNode::Named("CssUnit".to_owned()),
        HostTypeNode::Record(vec![
            ("value".to_owned(), HostTypeId(0)),
            ("unit".to_owned(), HostTypeId(1)),
        ]),
        HostTypeNode::List(HostTypeId(2)),
        HostTypeNode::Function {
            parameters: vec![HostTypeId(3)],
            result: HostTypeId(2),
        },
        HostTypeNode::Variant(vec![
            ("dimension".to_owned(), HostTypeId(2)),
            ("function".to_owned(), HostTypeId(4)),
            ("token".to_owned(), HostTypeId(1)),
        ]),
    ];
    GrammarSpec {
        rules: vec![RuleSpec {
            id: RuleId(0),
            name: "CssValue".to_owned(),
            root: NodeId(1),
            declared_type: None,
            transparent: false,
        }],
        nodes: vec![
            (NodeId(0), GrammarNode::Regex("css-token".to_owned())),
            (
                NodeId(1),
                GrammarNode::Map {
                    child: NodeId(0),
                    function: FnDescriptor::Expr {
                        function: 99,
                        return_type: Some(HostTypeId(5)),
                        effect: HostEffect::Fallible,
                    },
                },
            ),
        ],
        host_types,
        source_name: "css-l4-shape-representability".to_owned(),
    }
}
