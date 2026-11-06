use std::collections::BTreeSet;

use bbnf_ir::{AltBranch, FnDescriptor, IrNode, MapExpr, TokenDispatchArm, TypeDesc};
use bbnf_type_kernel_v3::type_kernel::infer::{HostEnvironment, Operator, ValueDisposition, infer};
use bbnf_type_kernel_v3::type_kernel::solve::{SolvedType, solve};
use bbnf_type_kernel_v3::type_kernel::store::Primitive;
use bbnf_type_kernel_v3::type_kernel::{OperationContext, TypeErrorKind};

fn host_descriptors() -> (Vec<String>, Vec<FnDescriptor>) {
    let strings = vec![
        "Variant".to_owned(),
        "named_host_type".to_owned(),
        "hex".to_owned(),
    ];
    let fns = vec![
        FnDescriptor::EnumWrap { variant: 0 },
        FnDescriptor::BoxWrap,
        FnDescriptor::NumberConvert {
            allow_leading_dot: true,
        },
        FnDescriptor::HexConvert { fn_path: 2 },
        FnDescriptor::SpanCapture,
        FnDescriptor::Expr {
            expr: MapExpr::Input,
            return_type: Some(TypeDesc::Bool),
        },
    ];
    (strings, fns)
}

#[test]
fn every_live_ir_node_variant_generates_real_constraints_and_solutions() {
    let (strings, fns) = host_descriptors();
    let host = HostEnvironment::new(&fns, &strings).with_rule(0, TypeDesc::U16);
    let nodes = vec![
        IrNode::Literal(0),
        IrNode::Regex(0),
        IrNode::Epsilon,
        IrNode::Seq(vec![IrNode::Literal(0), IrNode::Epsilon]),
        IrNode::Alt(
            vec![AltBranch {
                node: IrNode::Literal(0),
                first_set: None,
            }],
            None,
        ),
        IrNode::Repeat {
            inner: Box::new(IrNode::Literal(0)),
            lo: 0,
            hi: 1,
        },
        IrNode::Ref(0),
        IrNode::Skip(Box::new(IrNode::Literal(0)), Box::new(IrNode::Regex(0))),
        IrNode::Next(Box::new(IrNode::Literal(0)), Box::new(IrNode::Regex(0))),
        IrNode::Minus(Box::new(IrNode::Literal(0)), Box::new(IrNode::Regex(0))),
        IrNode::Negate(Box::new(IrNode::Literal(0))),
        IrNode::Map {
            inner: Box::new(IrNode::Literal(0)),
            fn_id: 5,
        },
        IrNode::OptionalWhitespace(Box::new(IrNode::Literal(0))),
        IrNode::TokenDispatch {
            token: Box::new(IrNode::Regex(0)),
            arms: vec![TokenDispatchArm {
                patterns: vec![0],
                guard_byte: Some(b'('),
                continuation: IrNode::Literal(0),
                map_fn: Some(0),
            }],
            fallback: Box::new(IrNode::Literal(0)),
        },
    ];
    let mut covered = BTreeSet::new();
    for node in &nodes {
        let mut operations = OperationContext::unlimited();
        let inferred = infer(node, &host, &mut operations).unwrap();
        assert!(!inferred.constraints().is_empty());
        assert_eq!(inferred.constraints().len(), inferred.nodes().len());
        covered.insert(
            inferred
                .nodes()
                .iter()
                .find(|fact| fact.path() == "root")
                .unwrap()
                .operator() as u8,
        );
        let roots = inferred.roots().collect::<Vec<_>>();
        let solution = solve(
            inferred.store(),
            inferred.constraints(),
            roots,
            &mut operations,
        )
        .unwrap();
        assert!(solution.type_of(inferred.root()).is_some());
    }
    assert_eq!(covered.len(), 14, "all actual enum variants are covered");
}

#[test]
fn live_operator_projection_matches_the_adjudicated_semantic_fixtures() {
    let (strings, fns) = host_descriptors();
    let host = HostEnvironment::new(&fns, &strings).with_rule(0, TypeDesc::U16);
    let solve_root = |node: &IrNode| {
        let mut operations = OperationContext::unlimited();
        let inferred = infer(node, &host, &mut operations).unwrap();
        solve(
            inferred.store(),
            inferred.constraints(),
            inferred.roots(),
            &mut operations,
        )
        .unwrap()
        .type_of(inferred.root())
        .unwrap()
        .clone()
    };
    for leaf in [IrNode::Literal(0), IrNode::Regex(0), IrNode::Epsilon] {
        assert_eq!(solve_root(&leaf), SolvedType::Primitive(Primitive::Span));
    }
    assert_eq!(
        solve_root(&IrNode::Negate(Box::new(IrNode::Literal(0)))),
        SolvedType::Tuple(Vec::new())
    );
    assert_eq!(
        solve_root(&IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Literal(0),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Regex(0),
                    first_set: None,
                },
            ],
            None,
        )),
        SolvedType::Primitive(Primitive::Span),
        "homogeneous alternatives normalize to their common semantic type"
    );
    assert_eq!(
        solve_root(&IrNode::Map {
            inner: Box::new(IrNode::Literal(0)),
            fn_id: 2,
        }),
        SolvedType::Primitive(Primitive::F64)
    );
    assert_eq!(
        solve_root(&IrNode::Map {
            inner: Box::new(IrNode::Literal(0)),
            fn_id: 3,
        }),
        SolvedType::Primitive(Primitive::U32)
    );
    assert_eq!(
        solve_root(&IrNode::Map {
            inner: Box::new(IrNode::Literal(0)),
            fn_id: 4,
        }),
        SolvedType::Primitive(Primitive::Span)
    );
    assert_eq!(
        solve_root(&IrNode::Map {
            inner: Box::new(IrNode::Literal(0)),
            fn_id: 5,
        }),
        SolvedType::Primitive(Primitive::Bool)
    );
    assert_eq!(
        solve_root(&IrNode::Ref(0)),
        SolvedType::Primitive(Primitive::U16)
    );
    assert_eq!(
        solve_root(&IrNode::TokenDispatch {
            token: Box::new(IrNode::Regex(0)),
            arms: vec![TokenDispatchArm {
                patterns: vec![0],
                guard_byte: None,
                continuation: IrNode::Literal(0),
                map_fn: None,
            }],
            fallback: Box::new(IrNode::Regex(0)),
        }),
        SolvedType::Primitive(Primitive::Span)
    );
}

#[test]
fn token_dispatch_classifies_the_token_instead_of_silently_skipping_it() {
    let (strings, fns) = host_descriptors();
    let host = HostEnvironment::new(&fns, &strings);
    let node = IrNode::TokenDispatch {
        token: Box::new(IrNode::Regex(0)),
        arms: Vec::new(),
        fallback: Box::new(IrNode::Literal(0)),
    };
    let mut operations = OperationContext::unlimited();
    let inferred = infer(&node, &host, &mut operations).unwrap();
    let token = inferred
        .nodes()
        .iter()
        .find(|fact| fact.path().ends_with("dispatch.token"))
        .unwrap();
    assert_eq!(token.operator(), Operator::Regex);
    assert_eq!(token.disposition(), ValueDisposition::Discriminator);
}

#[test]
fn every_real_host_descriptor_and_type_desc_variant_is_bridged() {
    let (strings, mut fns) = host_descriptors();
    let descriptors = vec![
        TypeDesc::Span,
        TypeDesc::F64,
        TypeDesc::Bool,
        TypeDesc::I8,
        TypeDesc::U8,
        TypeDesc::I16,
        TypeDesc::U16,
        TypeDesc::I32,
        TypeDesc::U32,
        TypeDesc::I64,
        TypeDesc::U64,
        TypeDesc::Option(Box::new(TypeDesc::U8)),
        TypeDesc::Vec(Box::new(TypeDesc::U16)),
        TypeDesc::Tuple(vec![TypeDesc::Bool]),
        TypeDesc::BoxedEnum,
        TypeDesc::Enum,
        TypeDesc::Named(1),
        TypeDesc::HeterogeneousAltJoin(vec![TypeDesc::U8, TypeDesc::Bool]),
    ];
    for descriptor in descriptors {
        fns.push(FnDescriptor::Expr {
            expr: MapExpr::Input,
            return_type: Some(descriptor),
        });
    }
    let host = HostEnvironment::new(&fns, &strings);
    for function in 0..fns.len() {
        let node = IrNode::Map {
            inner: Box::new(IrNode::Literal(0)),
            fn_id: function as u32,
        };
        let mut operations = OperationContext::unlimited();
        let inferred = infer(&node, &host, &mut operations).unwrap();
        let solution = solve(
            inferred.store(),
            inferred.constraints(),
            inferred.roots(),
            &mut operations,
        )
        .unwrap();
        assert!(solution.type_of(inferred.root()).is_some());
    }
}

#[test]
fn unannotated_host_expr_is_a_diagnostic_not_span() {
    let strings = Vec::new();
    let fns = vec![FnDescriptor::Expr {
        expr: MapExpr::Input,
        return_type: None,
    }];
    let host = HostEnvironment::new(&fns, &strings);
    let node = IrNode::Map {
        inner: Box::new(IrNode::Literal(0)),
        fn_id: 0,
    };
    let mut operations = OperationContext::unlimited();
    let error = infer(&node, &host, &mut operations).unwrap_err();
    assert_eq!(
        error.kind(),
        &TypeErrorKind::MissingHostSignature { function: 0 }
    );
    assert!(!error.proof().origins().is_empty());
}
