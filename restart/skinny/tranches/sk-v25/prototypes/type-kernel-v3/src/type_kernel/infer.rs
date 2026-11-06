//! Exhaustive constraint generation over the real `bbnf-ir` boundaries.

use std::collections::BTreeMap;

use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};

use super::solve::Constraint;
use super::store::{Primitive, RowSpec, StoreBuilder, TypeId, TypeStore};
use super::{OperationContext, Origin, TypeError, TypeErrorKind};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operator {
    Literal,
    Regex,
    Epsilon,
    Seq,
    Alt,
    Repeat,
    Ref,
    Skip,
    Next,
    Minus,
    Negate,
    Map,
    OptionalWhitespace,
    TokenDispatch,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ValueDisposition {
    Produced,
    Consumed,
    Discarded,
    Predicate,
    Discriminator,
}

#[derive(Clone, Debug)]
pub struct NodeTyping {
    path: String,
    operator: Operator,
    disposition: ValueDisposition,
    ty: TypeId,
}

impl NodeTyping {
    #[must_use]
    pub fn path(&self) -> &str {
        &self.path
    }

    #[must_use]
    pub fn operator(&self) -> Operator {
        self.operator
    }

    #[must_use]
    pub fn disposition(&self) -> ValueDisposition {
        self.disposition
    }

    #[must_use]
    pub fn ty(&self) -> TypeId {
        self.ty
    }
}

pub struct HostEnvironment<'a> {
    fns: &'a [FnDescriptor],
    strings: &'a [String],
    rules: BTreeMap<u32, TypeDesc>,
}

impl<'a> HostEnvironment<'a> {
    #[must_use]
    pub fn new(fns: &'a [FnDescriptor], strings: &'a [String]) -> Self {
        Self {
            fns,
            strings,
            rules: BTreeMap::new(),
        }
    }

    #[must_use]
    pub fn with_rule(mut self, rule: u32, ty: TypeDesc) -> Self {
        self.rules.insert(rule, ty);
        self
    }
}

#[derive(Clone, Debug)]
pub struct Inference {
    store: TypeStore,
    constraints: Vec<Constraint>,
    nodes: Vec<NodeTyping>,
    root: TypeId,
}

impl Inference {
    #[must_use]
    pub fn store(&self) -> &TypeStore {
        &self.store
    }

    #[must_use]
    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    #[must_use]
    pub fn nodes(&self) -> &[NodeTyping] {
        &self.nodes
    }

    #[must_use]
    pub fn root(&self) -> TypeId {
        self.root
    }

    pub fn roots(&self) -> impl Iterator<Item = TypeId> + '_ {
        self.nodes.iter().map(NodeTyping::ty)
    }
}

enum Work<'node> {
    Enter {
        node: &'node IrNode,
        path: String,
        disposition: ValueDisposition,
    },
    Exit {
        node: &'node IrNode,
        path: String,
    },
}

/// Generate a non-empty constraint for every actual IR node.  The exhaustive
/// matches intentionally omit wildcards: adding an `IrNode`, `FnDescriptor`, or
/// `TypeDesc` variant breaks this module at compile time.
pub fn infer(
    root: &IrNode,
    host: &HostEnvironment<'_>,
    operations: &mut OperationContext,
) -> Result<Inference, TypeError> {
    let mut builder = StoreBuilder::new();
    let mut constraints = Vec::new();
    let mut facts = Vec::new();
    let mut types = BTreeMap::new();
    let mut work = vec![Work::Enter {
        node: root,
        path: "root".to_owned(),
        disposition: ValueDisposition::Produced,
    }];

    while let Some(item) = work.pop() {
        match item {
            Work::Enter {
                node,
                path,
                disposition,
            } => {
                let proof = operations.step(
                    "infer.enter",
                    &[],
                    [Origin::new(path.clone(), format!("{:?}", operator(node)))],
                )?;
                let (_, ty) = builder.fresh_meta(0).map_err(|error| {
                    operations.fail(
                        TypeErrorKind::Construction {
                            detail: error.to_string(),
                        },
                        &[proof],
                        "infer.allocate",
                    )
                })?;
                types.insert(path.clone(), ty);
                facts.push(NodeTyping {
                    path: path.clone(),
                    operator: operator(node),
                    disposition,
                    ty,
                });
                work.push(Work::Exit {
                    node,
                    path: path.clone(),
                });
                push_children(node, &path, &mut work);
            }
            Work::Exit { node, path } => {
                let proof = operations.step(
                    "infer.emit",
                    &[],
                    [Origin::new(path.clone(), format!("{:?}", operator(node)))],
                )?;
                let target =
                    infer_target(node, &path, &types, &mut builder, host, operations, proof)?;
                let ty = types[&path];
                constraints.push(Constraint::equal(
                    ty,
                    target,
                    Origin::new(path, format!("{:?}", operator(node))),
                ));
            }
        }
    }

    facts.sort_by(|left, right| left.path.cmp(&right.path));
    let root = types["root"];
    let roots = facts.iter().map(NodeTyping::ty).collect::<Vec<_>>();
    let store = builder.finish(roots).map_err(|error| {
        operations.fail(
            TypeErrorKind::Construction {
                detail: error.to_string(),
            },
            &[],
            "infer.finish",
        )
    })?;
    Ok(Inference {
        store,
        constraints,
        nodes: facts,
        root,
    })
}

fn operator(node: &IrNode) -> Operator {
    match node {
        IrNode::Literal(_) => Operator::Literal,
        IrNode::Regex(_) => Operator::Regex,
        IrNode::Epsilon => Operator::Epsilon,
        IrNode::Seq(_) => Operator::Seq,
        IrNode::Alt(_, _) => Operator::Alt,
        IrNode::Repeat { .. } => Operator::Repeat,
        IrNode::Ref(_) => Operator::Ref,
        IrNode::Skip(_, _) => Operator::Skip,
        IrNode::Next(_, _) => Operator::Next,
        IrNode::Minus(_, _) => Operator::Minus,
        IrNode::Negate(_) => Operator::Negate,
        IrNode::Map { .. } => Operator::Map,
        IrNode::OptionalWhitespace(_) => Operator::OptionalWhitespace,
        IrNode::TokenDispatch { .. } => Operator::TokenDispatch,
    }
}

fn push_children<'node>(node: &'node IrNode, path: &str, work: &mut Vec<Work<'node>>) {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        IrNode::Seq(children) => {
            for (index, child) in children.iter().enumerate().rev() {
                work.push(Work::Enter {
                    node: child,
                    path: format!("{path}.seq[{index}]"),
                    disposition: ValueDisposition::Produced,
                });
            }
        }
        IrNode::Alt(branches, _) => {
            for (index, branch) in branches.iter().enumerate().rev() {
                work.push(Work::Enter {
                    node: &branch.node,
                    path: format!("{path}.alt[{index}]"),
                    disposition: ValueDisposition::Produced,
                });
            }
        }
        IrNode::Repeat { inner, .. } => work.push(Work::Enter {
            node: inner,
            path: format!("{path}.repeat"),
            disposition: ValueDisposition::Produced,
        }),
        IrNode::Skip(left, right) => {
            work.push(Work::Enter {
                node: right,
                path: format!("{path}.skip.right"),
                disposition: ValueDisposition::Discarded,
            });
            work.push(Work::Enter {
                node: left,
                path: format!("{path}.skip.left"),
                disposition: ValueDisposition::Produced,
            });
        }
        IrNode::Next(left, right) => {
            work.push(Work::Enter {
                node: right,
                path: format!("{path}.next.right"),
                disposition: ValueDisposition::Produced,
            });
            work.push(Work::Enter {
                node: left,
                path: format!("{path}.next.left"),
                disposition: ValueDisposition::Discarded,
            });
        }
        IrNode::Minus(left, right) => {
            work.push(Work::Enter {
                node: right,
                path: format!("{path}.minus.right"),
                disposition: ValueDisposition::Predicate,
            });
            work.push(Work::Enter {
                node: left,
                path: format!("{path}.minus.left"),
                disposition: ValueDisposition::Produced,
            });
        }
        IrNode::Negate(inner) => work.push(Work::Enter {
            node: inner,
            path: format!("{path}.negate"),
            disposition: ValueDisposition::Predicate,
        }),
        IrNode::Map { inner, .. } => work.push(Work::Enter {
            node: inner,
            path: format!("{path}.map"),
            disposition: ValueDisposition::Consumed,
        }),
        IrNode::OptionalWhitespace(inner) => work.push(Work::Enter {
            node: inner,
            path: format!("{path}.whitespace"),
            disposition: ValueDisposition::Produced,
        }),
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            work.push(Work::Enter {
                node: fallback,
                path: format!("{path}.dispatch.fallback"),
                disposition: ValueDisposition::Produced,
            });
            for (index, arm) in arms.iter().enumerate().rev() {
                work.push(Work::Enter {
                    node: &arm.continuation,
                    path: format!("{path}.dispatch.arm[{index}]"),
                    disposition: ValueDisposition::Produced,
                });
            }
            work.push(Work::Enter {
                node: token,
                path: format!("{path}.dispatch.token"),
                disposition: ValueDisposition::Discriminator,
            });
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn infer_target(
    node: &IrNode,
    path: &str,
    types: &BTreeMap<String, TypeId>,
    builder: &mut StoreBuilder,
    host: &HostEnvironment<'_>,
    operations: &OperationContext,
    proof: usize,
) -> Result<TypeId, TypeError> {
    let build = |result: Result<TypeId, super::store::BuildError>| {
        result.map_err(|error| {
            operations.fail(
                TypeErrorKind::Construction {
                    detail: error.to_string(),
                },
                &[proof],
                "infer.construct",
            )
        })
    };
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {
            build(builder.primitive(Primitive::Span))
        }
        IrNode::Seq(children) => build(
            builder.tuple((0..children.len()).map(|index| types[&format!("{path}.seq[{index}]")])),
        ),
        IrNode::Alt(branches, _) => build(
            builder.join((0..branches.len()).map(|index| types[&format!("{path}.alt[{index}]")])),
        ),
        IrNode::Repeat { lo, hi, .. } => {
            let inner = types[&format!("{path}.repeat")];
            if *lo == 0 && *hi == 1 {
                build(builder.option(inner))
            } else {
                build(builder.list(inner))
            }
        }
        IrNode::Ref(rule) => {
            let descriptor = host.rules.get(rule).ok_or_else(|| {
                operations.fail(
                    TypeErrorKind::MissingRule { rule: *rule },
                    &[proof],
                    "infer.ref",
                )
            })?;
            type_desc(descriptor, builder, host, operations, proof)
        }
        IrNode::Skip(_, _) => Ok(types[&format!("{path}.skip.left")]),
        IrNode::Next(_, _) => Ok(types[&format!("{path}.next.right")]),
        IrNode::Minus(_, _) => Ok(types[&format!("{path}.minus.left")]),
        IrNode::Negate(_) => build(builder.tuple(std::iter::empty())),
        IrNode::Map { fn_id, .. } => host_function(*fn_id, builder, host, operations, proof),
        IrNode::OptionalWhitespace(_) => Ok(types[&format!("{path}.whitespace")]),
        IrNode::TokenDispatch { arms, .. } => {
            let mut alternatives = Vec::with_capacity(arms.len() + 1);
            for (index, arm) in arms.iter().enumerate() {
                let ty = if let Some(function) = arm.map_fn {
                    host_function(function, builder, host, operations, proof)?
                } else {
                    types[&format!("{path}.dispatch.arm[{index}]")]
                };
                alternatives.push(ty);
            }
            alternatives.push(types[&format!("{path}.dispatch.fallback")]);
            build(builder.join(alternatives))
        }
    }
}

fn host_function(
    function: u32,
    builder: &mut StoreBuilder,
    host: &HostEnvironment<'_>,
    operations: &OperationContext,
    proof: usize,
) -> Result<TypeId, TypeError> {
    let descriptor = host.fns.get(function as usize).ok_or_else(|| {
        operations.fail(
            TypeErrorKind::MissingHostFunction { function },
            &[proof],
            "infer.host",
        )
    })?;
    match descriptor {
        FnDescriptor::EnumWrap { variant } => {
            let variant = string(*variant, host, operations, proof)?;
            builder
                .named(format!("bbnf::Enum::{variant}"))
                .map_err(|error| construction(error, operations, proof))
        }
        FnDescriptor::BoxWrap => builder
            .named("bbnf::BoxedEnum")
            .map_err(|error| construction(error, operations, proof)),
        FnDescriptor::NumberConvert { .. } => builder
            .primitive(Primitive::F64)
            .map_err(|error| construction(error, operations, proof)),
        FnDescriptor::HexConvert { .. } => builder
            .primitive(Primitive::U32)
            .map_err(|error| construction(error, operations, proof)),
        FnDescriptor::SpanCapture => builder
            .primitive(Primitive::Span)
            .map_err(|error| construction(error, operations, proof)),
        FnDescriptor::Expr {
            return_type: Some(return_type),
            ..
        } => type_desc(return_type, builder, host, operations, proof),
        FnDescriptor::Expr {
            return_type: None, ..
        } => Err(operations.fail(
            TypeErrorKind::MissingHostSignature { function },
            &[proof],
            "infer.host_signature",
        )),
    }
}

fn type_desc(
    descriptor: &TypeDesc,
    builder: &mut StoreBuilder,
    host: &HostEnvironment<'_>,
    operations: &OperationContext,
    proof: usize,
) -> Result<TypeId, TypeError> {
    let result = match descriptor {
        TypeDesc::Span => builder.primitive(Primitive::Span),
        TypeDesc::F64 => builder.primitive(Primitive::F64),
        TypeDesc::Bool => builder.primitive(Primitive::Bool),
        TypeDesc::I8 => builder.primitive(Primitive::I8),
        TypeDesc::U8 => builder.primitive(Primitive::U8),
        TypeDesc::I16 => builder.primitive(Primitive::I16),
        TypeDesc::U16 => builder.primitive(Primitive::U16),
        TypeDesc::I32 => builder.primitive(Primitive::I32),
        TypeDesc::U32 => builder.primitive(Primitive::U32),
        TypeDesc::I64 => builder.primitive(Primitive::I64),
        TypeDesc::U64 => builder.primitive(Primitive::U64),
        TypeDesc::Option(item) => {
            let item = type_desc(item, builder, host, operations, proof)?;
            builder.option(item)
        }
        TypeDesc::Vec(item) => {
            let item = type_desc(item, builder, host, operations, proof)?;
            builder.list(item)
        }
        TypeDesc::Tuple(items) => {
            let items = items
                .iter()
                .map(|item| type_desc(item, builder, host, operations, proof))
                .collect::<Result<Vec<_>, _>>()?;
            builder.tuple(items)
        }
        TypeDesc::BoxedEnum => builder.named("bbnf::BoxedEnum"),
        TypeDesc::Enum => builder.named("bbnf::Enum"),
        TypeDesc::Named(name) => {
            let name = string(*name, host, operations, proof)?;
            builder.named(name)
        }
        TypeDesc::HeterogeneousAltJoin(items) => {
            let fields = items
                .iter()
                .enumerate()
                .map(|(index, item)| {
                    type_desc(item, builder, host, operations, proof)
                        .map(|ty| (format!("branch_{index:04}"), ty))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let row =
                RowSpec::closed(fields).map_err(|error| construction(error, operations, proof))?;
            builder.variant(row)
        }
    };
    result.map_err(|error| construction(error, operations, proof))
}

fn string<'host>(
    id: u32,
    host: &'host HostEnvironment<'_>,
    operations: &OperationContext,
    proof: usize,
) -> Result<&'host str, TypeError> {
    host.strings
        .get(id as usize)
        .map(String::as_str)
        .ok_or_else(|| {
            operations.fail(
                TypeErrorKind::MissingString { string: id },
                &[proof],
                "infer.string",
            )
        })
}

fn construction(
    error: super::store::BuildError,
    operations: &OperationContext,
    proof: usize,
) -> TypeError {
    operations.fail(
        TypeErrorKind::Construction {
            detail: error.to_string(),
        },
        &[proof],
        "infer.construct",
    )
}
