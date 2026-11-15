//! Grammar functions (`grammar/bbnf/bbnf.bbnf`: `rule = |p, q| body ;` and
//! `f(a, b)`), given their meaning before resolve (sk-v27 LW3.COMP-1c, step 1
//! carry (a); RULINGS 2026-11-14i COMP1C-1 (b)).
//!
//! A grammar function is a parameterised rule body; a call is that body with
//! each parameter replaced by the argument written at its position. Root's
//! pipeline reads them the same way (`pipeline/compile/pipeline.rs`:
//! "Closures are first-class grammar functions expanded inline during
//! lowering"). Here the expansion is a source-to-source pass: every call is
//! replaced by a fresh copy of the function's body with its arguments
//! substituted, and the function rules leave the module, so resolve, types
//! and facts read only forms they already give a meaning. Every rule body
//! stays a tree: each use of a parameter is its own copy of the argument.
//!
//! Refused, each with `DiagnosticCode::GrammarFunction` naming the misuse: a
//! call whose callee is not a grammar function; a call with the wrong number
//! of arguments; a function that reaches itself through calls (its expansion
//! would not end); a function's name used as a plain reference; a `| … |`
//! definition anywhere but a rule's whole right side.

use std::borrow::Cow;
use std::collections::BTreeMap;

use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::{
    SourceModule, SourceNodeData, SourceNodeKindData, SourceRecordSlotData, SourceRuleData,
};
use super::syntax::{NodeIx, RuleIx};

/// The module with every grammar-function call expanded and the function
/// rules removed. A module that writes no call and no definition is returned
/// as it is.
pub(crate) fn expand(module: &SourceModule) -> StageResult<Cow<'_, SourceModule>> {
    let uses_functions = module.nodes.iter().any(|node| {
        matches!(
            node.kind,
            SourceNodeKindData::Call { .. } | SourceNodeKindData::Closure { .. }
        )
    });
    if !uses_functions {
        return StageResult::accepted(Cow::Borrowed(module));
    }

    let mut functions = BTreeMap::new();
    for rule in &module.rules {
        if let SourceNodeKindData::Closure { params, body } = &module.nodes[rule.root.index()].kind
        {
            functions.insert(rule.name.as_str(), (params.as_slice(), *body));
        }
    }

    let mut expander = Expander {
        module,
        functions,
        nodes: Vec::with_capacity(module.nodes.len()),
        diagnostics: Vec::new(),
        calling: Vec::new(),
    };
    let mut rules = Vec::with_capacity(module.rules.len());
    for rule in &module.rules {
        if expander.functions.contains_key(rule.name.as_str()) {
            continue;
        }
        let Some(root) = expander.copy(rule.root, &Env::default()) else {
            continue;
        };
        let Some(id) = RuleIx::checked(rules.len()) else {
            expander.refuse(rule.root, "more rules than the rule budget");
            continue;
        };
        rules.push(SourceRuleData {
            id,
            name: rule.name.clone(),
            root,
            span: rule.span,
        });
    }
    if !expander.diagnostics.is_empty() {
        return StageResult::rejected_set(DiagnosticSet::from_vec(expander.diagnostics));
    }
    StageResult::accepted(Cow::Owned(SourceModule::new(
        module.revision(),
        expander.nodes,
        rules,
    )))
}

/// Parameter name → the argument's root in the output arena (already copied
/// in the caller's scope); each use copies it again.
#[derive(Default)]
struct Env {
    bound: BTreeMap<String, NodeIx>,
}

struct Expander<'m> {
    module: &'m SourceModule,
    functions: BTreeMap<&'m str, (&'m [String], NodeIx)>,
    nodes: Vec<SourceNodeData>,
    diagnostics: Vec<Diagnostic>,
    /// The functions whose bodies are being expanded, outermost first.
    calling: Vec<&'m str>,
}

impl<'m> Expander<'m> {
    fn refuse(&mut self, at: NodeIx, message: impl Into<String>) {
        let span = self.module.nodes[at.index()].span;
        self.diagnostics.push(Diagnostic::new(
            DiagnosticCode::GrammarFunction,
            span.start() as usize,
            span.end() as usize,
            message,
        ));
    }

    fn push(&mut self, node: SourceNodeData) -> Option<NodeIx> {
        let id = NodeIx::checked(self.nodes.len())?;
        self.nodes.push(node);
        Some(id)
    }

    /// Copy the source subtree at `at` into the output arena, expanding calls
    /// and substituting the parameters `env` binds.
    fn copy(&mut self, at: NodeIx, env: &Env) -> Option<NodeIx> {
        let module = self.module;
        let source = &module.nodes[at.index()];
        let span = source.span;
        let kind = match &source.kind {
            SourceNodeKindData::Reference(name) => {
                if let Some(&argument) = env.bound.get(name) {
                    return self.duplicate(argument);
                }
                if self.functions.contains_key(name.as_str()) {
                    self.refuse(
                        at,
                        format!("grammar function `{name}` is used as a rule; call it with its arguments"),
                    );
                    return None;
                }
                SourceNodeKindData::Reference(name.clone())
            }
            SourceNodeKindData::Call { callee, args } => return self.call(at, callee, args, env),
            SourceNodeKindData::Closure { .. } => {
                self.refuse(
                    at,
                    "a grammar-function definition `| … |` must be a rule's whole right side",
                );
                return None;
            }
            SourceNodeKindData::Epsilon => SourceNodeKindData::Epsilon,
            SourceNodeKindData::Literal(bytes) => SourceNodeKindData::Literal(bytes.clone()),
            SourceNodeKindData::Regex(pattern) => SourceNodeKindData::Regex(pattern.clone()),
            SourceNodeKindData::Sequence(children) => {
                SourceNodeKindData::Sequence(self.copy_all(children, env)?)
            }
            SourceNodeKindData::Choice(children) => {
                SourceNodeKindData::Choice(self.copy_all(children, env)?)
            }
            SourceNodeKindData::Repeat {
                child,
                minimum,
                maximum,
            } => SourceNodeKindData::Repeat {
                child: self.copy(*child, env)?,
                minimum: *minimum,
                maximum: *maximum,
            },
            SourceNodeKindData::Transform {
                child,
                transform,
                name,
            } => SourceNodeKindData::Transform {
                child: self.copy(*child, env)?,
                transform: *transform,
                name: name.clone(),
            },
            SourceNodeKindData::Projection { left, right, keep } => {
                SourceNodeKindData::Projection {
                    left: self.copy(*left, env)?,
                    right: self.copy(*right, env)?,
                    keep: *keep,
                }
            }
            SourceNodeKindData::Difference { include, exclude } => SourceNodeKindData::Difference {
                include: self.copy(*include, env)?,
                exclude: self.copy(*exclude, env)?,
            },
            SourceNodeKindData::Trim { child } => SourceNodeKindData::Trim {
                child: self.copy(*child, env)?,
            },
            SourceNodeKindData::Record(slots) => {
                let mut copied = Vec::with_capacity(slots.len());
                for slot in slots {
                    copied.push(match slot {
                        SourceRecordSlotData::Field {
                            name,
                            child,
                            name_span,
                            span,
                        } => SourceRecordSlotData::Field {
                            name: name.clone(),
                            child: self.copy(*child, env)?,
                            name_span: *name_span,
                            span: *span,
                        },
                        SourceRecordSlotData::Discard { child, span } => {
                            SourceRecordSlotData::Discard {
                                child: self.copy(*child, env)?,
                                span: *span,
                            }
                        }
                    });
                }
                SourceNodeKindData::Record(copied)
            }
            SourceNodeKindData::Mapping { child, value, ty } => SourceNodeKindData::Mapping {
                child: self.copy(*child, env)?,
                value: value.clone(),
                ty: ty.clone(),
            },
            SourceNodeKindData::SpanCapture { child } => SourceNodeKindData::SpanCapture {
                child: self.copy(*child, env)?,
            },
        };
        self.push(SourceNodeData { kind, span })
    }

    fn copy_all(&mut self, children: &[NodeIx], env: &Env) -> Option<Vec<NodeIx>> {
        let mut out = Vec::with_capacity(children.len());
        for &child in children {
            out.push(self.copy(child, env)?);
        }
        Some(out)
    }

    fn call(&mut self, at: NodeIx, callee: &str, args: &[NodeIx], env: &Env) -> Option<NodeIx> {
        let Some(&(params, body)) = self.functions.get(callee) else {
            self.refuse(
                at,
                format!("`{callee}` is called but is not a grammar function"),
            );
            return None;
        };
        if params.len() != args.len() {
            self.refuse(
                at,
                format!(
                    "grammar function `{callee}` takes {} argument(s), called with {}",
                    params.len(),
                    args.len()
                ),
            );
            return None;
        }
        if let Some(position) = self.calling.iter().position(|&name| name == callee) {
            let mut path: Vec<&str> = self.calling[position..].to_vec();
            path.push(callee);
            self.refuse(
                at,
                format!(
                    "grammar function `{callee}` reaches itself through calls ({}); its expansion would not end",
                    path.join(" -> ")
                ),
            );
            return None;
        }
        // The arguments are read in the caller's scope.
        let mut bound = BTreeMap::new();
        for (param, &arg) in params.iter().zip(args) {
            bound.insert(param.clone(), self.copy(arg, env)?);
        }
        let callee = self
            .functions
            .get_key_value(callee)
            .map(|(name, _)| *name)?;
        self.calling.push(callee);
        let expanded = self.copy(body, &Env { bound });
        self.calling.pop();
        expanded
    }

    /// A fresh copy of a subtree already in the output arena (a parameter
    /// used more than once must not share nodes: rule bodies are trees).
    fn duplicate(&mut self, at: NodeIx) -> Option<NodeIx> {
        let node = self.nodes[at.index()].clone();
        let kind = match node.kind {
            SourceNodeKindData::Sequence(children) => {
                SourceNodeKindData::Sequence(self.duplicate_all(&children)?)
            }
            SourceNodeKindData::Choice(children) => {
                SourceNodeKindData::Choice(self.duplicate_all(&children)?)
            }
            SourceNodeKindData::Repeat {
                child,
                minimum,
                maximum,
            } => SourceNodeKindData::Repeat {
                child: self.duplicate(child)?,
                minimum,
                maximum,
            },
            SourceNodeKindData::Transform {
                child,
                transform,
                name,
            } => SourceNodeKindData::Transform {
                child: self.duplicate(child)?,
                transform,
                name,
            },
            SourceNodeKindData::Projection { left, right, keep } => {
                SourceNodeKindData::Projection {
                    left: self.duplicate(left)?,
                    right: self.duplicate(right)?,
                    keep,
                }
            }
            SourceNodeKindData::Difference { include, exclude } => SourceNodeKindData::Difference {
                include: self.duplicate(include)?,
                exclude: self.duplicate(exclude)?,
            },
            SourceNodeKindData::Trim { child } => SourceNodeKindData::Trim {
                child: self.duplicate(child)?,
            },
            SourceNodeKindData::Record(slots) => {
                let mut copied = Vec::with_capacity(slots.len());
                for slot in slots {
                    copied.push(match slot {
                        SourceRecordSlotData::Field {
                            name,
                            child,
                            name_span,
                            span,
                        } => SourceRecordSlotData::Field {
                            name,
                            child: self.duplicate(child)?,
                            name_span,
                            span,
                        },
                        SourceRecordSlotData::Discard { child, span } => {
                            SourceRecordSlotData::Discard {
                                child: self.duplicate(child)?,
                                span,
                            }
                        }
                    });
                }
                SourceNodeKindData::Record(copied)
            }
            SourceNodeKindData::Mapping { child, value, ty } => SourceNodeKindData::Mapping {
                child: self.duplicate(child)?,
                value,
                ty,
            },
            SourceNodeKindData::SpanCapture { child } => SourceNodeKindData::SpanCapture {
                child: self.duplicate(child)?,
            },
            // The output arena holds no call or definition, and the leaves
            // carry no children.
            leaf @ (SourceNodeKindData::Epsilon
            | SourceNodeKindData::Literal(_)
            | SourceNodeKindData::Regex(_)
            | SourceNodeKindData::Reference(_)
            | SourceNodeKindData::Call { .. }
            | SourceNodeKindData::Closure { .. }) => leaf,
        };
        self.push(SourceNodeData {
            kind,
            span: node.span,
        })
    }

    fn duplicate_all(&mut self, children: &[NodeIx]) -> Option<Vec<NodeIx>> {
        let mut out = Vec::with_capacity(children.len());
        for &child in children {
            out.push(self.duplicate(child)?);
        }
        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::bootstrap::read_source;
    use crate::compiler::budget::CompileOptions;
    use crate::compiler::source::Revision;

    fn module(text: &str) -> SourceModule {
        match read_source(
            text.as_bytes(),
            Revision::from_bytes([0; 32]),
            CompileOptions::default(),
        ) {
            StageResult::Accepted { value, .. } => value.module,
            StageResult::Rejected { diagnostics } => panic!("front end refused: {diagnostics:?}"),
        }
    }

    fn expanded(text: &str) -> SourceModule {
        match expand(&module(text)) {
            StageResult::Accepted { value, .. } => value.into_owned(),
            StageResult::Rejected { diagnostics } => panic!("expansion refused: {diagnostics:?}"),
        }
    }

    fn refused(text: &str) -> Vec<Diagnostic> {
        match expand(&module(text)) {
            StageResult::Accepted { .. } => panic!("expansion accepted {text:?}"),
            StageResult::Rejected { diagnostics } => diagnostics.iter().cloned().collect(),
        }
    }

    /// The tree at `at`, printed with references by name.
    fn show(module: &SourceModule, at: NodeIx) -> String {
        let all = |c: &[NodeIx], sep: &str| {
            c.iter()
                .map(|&n| show(module, n))
                .collect::<Vec<_>>()
                .join(sep)
        };
        match &module.nodes[at.index()].kind {
            SourceNodeKindData::Literal(bytes) => format!("{:?}", String::from_utf8_lossy(bytes)),
            SourceNodeKindData::Regex(p) => format!("/{p}/"),
            SourceNodeKindData::Reference(name) => name.clone(),
            SourceNodeKindData::Sequence(c) => format!("({})", all(c, " , ")),
            SourceNodeKindData::Choice(c) => format!("({})", all(c, " | ")),
            SourceNodeKindData::Repeat {
                child,
                minimum: 0,
                maximum: None,
            } => format!("{} *", show(module, *child)),
            other => format!("{other:?}"),
        }
    }

    fn rule<'m>(module: &'m SourceModule, name: &str) -> Option<&'m SourceRuleData> {
        module.rules.iter().find(|r| r.name == name)
    }

    #[test]
    fn a_call_is_the_function_body_with_its_arguments_substituted() {
        let m = expanded(
            r#"ident = /[a-z]+/ ;
num = /[0-9]+/ ;
wrap = |x, y| "(" , x , y , ")" ;
use = wrap(ident, num | ident) ;
"#,
        );
        assert!(
            rule(&m, "wrap").is_none(),
            "the function rule leaves the module"
        );
        let names: Vec<&str> = m.rules.iter().map(|r| r.name.as_str()).collect();
        assert_eq!(names, ["ident", "num", "use"]);
        for (index, r) in m.rules.iter().enumerate() {
            assert_eq!(r.id.index(), index, "rule ids stay dense in source order");
        }
        let use_rule = rule(&m, "use").unwrap();
        assert_eq!(
            show(&m, use_rule.root),
            r#"("(" , ident , (num | ident) , ")")"#
        );
        assert!(
            m.nodes.iter().all(|n| !matches!(
                n.kind,
                SourceNodeKindData::Call { .. } | SourceNodeKindData::Closure { .. }
            )),
            "no call or definition is left"
        );
    }

    #[test]
    fn a_parameter_used_twice_is_two_copies_and_nested_calls_expand() {
        let m = expanded(
            r#"a = "a" ;
twice = |x| x , x ;
sep = |item, s| item , ( s , item ) * ;
list = sep(twice(a), ",") ;
"#,
        );
        let list = rule(&m, "list").unwrap();
        let mut owners = vec![0usize; m.nodes.len()];
        fn mark(m: &SourceModule, at: NodeIx, owners: &mut [usize]) {
            owners[at.index()] += 1;
            let children: Vec<NodeIx> = match &m.nodes[at.index()].kind {
                SourceNodeKindData::Sequence(c) | SourceNodeKindData::Choice(c) => c.clone(),
                SourceNodeKindData::Repeat { child, .. } => vec![*child],
                _ => vec![],
            };
            for c in children {
                mark(m, c, owners);
            }
        }
        for r in &m.rules {
            mark(&m, r.root, &mut owners);
        }
        assert!(
            owners.iter().all(|&n| n <= 1),
            "every node has one parent: the bodies are trees"
        );
        let text = show(&m, list.root);
        assert_eq!(text.matches("(a , a)").count(), 2, "{text}");
        assert!(text.contains(r#"",""#), "{text}");
    }

    #[test]
    fn misuse_of_a_grammar_function_is_refused_by_name() {
        let cases = [
            (
                "a = \"a\" ;\nf = |x| x ;\nb = f(a, a) ;\n",
                "takes 1 argument(s), called with 2",
            ),
            ("a = \"a\" ;\nb = a(a) ;\n", "is not a grammar function"),
            ("a = \"a\" ;\nf = |x| x ;\nb = f ;\n", "is used as a rule"),
            (
                "f = |x| x , f(x) ;\nb = f(\"a\") ;\n",
                "reaches itself through calls (f -> f)",
            ),
            (
                "f = |x| g(x) ;\ng = |y| f(y) ;\nb = f(\"a\") ;\n",
                "reaches itself through calls (f -> g -> f)",
            ),
        ];
        for (text, needle) in cases {
            let diagnostics = refused(text);
            assert!(
                diagnostics
                    .iter()
                    .any(|d| d.code() == DiagnosticCode::GrammarFunction
                        && d.message().contains(needle)),
                "{text:?}: {diagnostics:?}"
            );
        }
    }

    #[test]
    fn a_module_without_functions_is_returned_as_it_is() {
        let m = module("a = \"a\" ;\nb = a , a ;\n");
        match expand(&m) {
            StageResult::Accepted { value, .. } => {
                assert!(matches!(value, Cow::Borrowed(_)), "no copy is made");
                assert_eq!(*value, m);
            }
            StageResult::Rejected { diagnostics } => panic!("{diagnostics:?}"),
        }
    }
}
