//! sk-v27 LW3.COMP-1c step 1 (RULINGS 2026-11-14i COMP1C-1 (b)): the spine's
//! source module read from the grammar of record.
//!
//! The text is parsed by the parser generated from `grammar/bbnf/bbnf.bbnf`
//! (`BbnfBootstrap`), and its tree is read to the spine's typed source nodes:
//! every form `bbnf.bbnf` names becomes a node (or a directive row), none is
//! skipped. The bootstrap tree is lossy in four places, and there the reader
//! takes the lexeme from the source text inside the bounds the tree does keep:
//! a binary operator (`<<`, `>>`, `-`) between two mapped factors; the
//! separator (`|` or juxtaposition) between the items of a call argument; the
//! text of a `->` value expression and of its `: Type`; a `@host` type and a
//! `@pretty` hint's argument.
//!
//! Step 1 builds the reader beside the spine's hand-written recognizer
//! (`frontend.rs`); step 2 routes `compile_validated_instance` through it and
//! deletes the recognizer in the same commit (LEDGER "LW3.COMP-1c, the eleventh
//! seat's hand-off"). Until then only this module's tests call it.
#![cfg_attr(not(test), allow(dead_code))]

use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::regex_table::{RegexTablePlan, TablePlanCompileError};
use super::source::{
    Revision, SourceDirectives, SourceHost, SourceImport, SourceModule, SourceNodeData,
    SourceNodeKindData, SourcePresentationData, SourcePresentationHintData, SourceRecover,
    SourceRuleData, SourceSpan, SourceValueData, SourceValueHead,
};
use super::syntax::{NodeIx, ProjectionKeep, RuleIx};
use crate::grammar::generated::bbnf::BbnfBootstrap;
use crate::runtime::bbnf::{BbnfCompound, BbnfCompoundKind as K, BbnfDocument, BbnfValue};

/// What the bootstrap front end reads from one grammar text.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct BootstrapRead {
    pub(crate) module: SourceModule,
    pub(crate) presentation: Vec<SourcePresentationData>,
    pub(crate) directives: SourceDirectives,
}

pub(crate) fn read_source(
    bytes: &[u8],
    revision: Revision,
    options: CompileOptions,
) -> StageResult<BootstrapRead> {
    let Ok(text) = std::str::from_utf8(bytes) else {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::Syntax,
            0,
            bytes.len(),
            "grammar source is not UTF-8",
        ));
    };
    let Ok(doc) = BbnfBootstrap::parse(text) else {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::Syntax,
            0,
            bytes.len(),
            "expected a complete BBNF grammar",
        ));
    };
    let mut reader = Reader {
        doc: &doc,
        text,
        options,
        nodes: Vec::new(),
        rules: Vec::new(),
        presentation: Vec::new(),
        directives: SourceDirectives::default(),
        diagnostics: Vec::new(),
        read_to: 0,
    };
    reader.grammar(doc.root());
    if reader.diagnostics.is_empty() && !text[reader.read_to..].trim().is_empty() {
        let at = reader.read_to
            + (text[reader.read_to..].len() - text[reader.read_to..].trim_start().len());
        reader.diagnostics.push(Diagnostic::new(
            DiagnosticCode::Syntax,
            at,
            (at + 1).min(text.len()),
            "expected a complete BBNF rule, directive or comment",
        ));
    }
    if !reader.diagnostics.is_empty() {
        return StageResult::rejected_set(super::diagnostic::DiagnosticSet::from_vec(
            reader.diagnostics,
        ));
    }
    StageResult::accepted(BootstrapRead {
        module: SourceModule::new(revision, reader.nodes, reader.rules),
        presentation: reader.presentation,
        directives: reader.directives,
    })
}

struct Reader<'d, 'p> {
    doc: &'d BbnfDocument<'p>,
    text: &'p str,
    options: CompileOptions,
    nodes: Vec<SourceNodeData>,
    rules: Vec<SourceRuleData>,
    presentation: Vec<SourcePresentationData>,
    directives: SourceDirectives,
    diagnostics: Vec<Diagnostic>,
    /// The furthest byte any read item ends at.
    read_to: usize,
}

type Bounds = (usize, usize);

impl<'d, 'p> Reader<'d, 'p> {
    fn compound(&self, value: &BbnfValue<'p>) -> Option<&'d BbnfCompound<'p>> {
        match value {
            BbnfValue::Compound(id) => Some(self.doc.compound(*id)),
            _ => None,
        }
    }

    /// The bounds of a value: its own, else the union of its children's.
    fn bounds(&self, value: &BbnfValue<'p>) -> Option<Bounds> {
        let compound = self.compound(value)?;
        if let Some((start, end)) = compound.bounds {
            return Some((start as usize, end as usize));
        }
        let mut union: Option<Bounds> = None;
        for child in &compound.children {
            if let Some((start, end)) = self.bounds(child) {
                union = Some(match union {
                    Some((s, e)) => (s.min(start), e.max(end)),
                    None => (start, end),
                });
            }
        }
        union
    }

    fn span(&self, bounds: Bounds) -> SourceSpan {
        SourceSpan::checked(bounds.0, bounds.1).expect("source size is checked before the read")
    }

    fn invariant(&mut self, bounds: Bounds, message: &str) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            bounds.0,
            bounds.1,
            format!("bootstrap tree: {message}"),
        ));
    }

    fn push(&mut self, kind: SourceNodeKindData, bounds: Bounds) -> NodeIx {
        let index = NodeIx::checked(self.nodes.len()).expect("node budget bounds node identity");
        self.nodes.push(SourceNodeData {
            kind,
            span: self.span(bounds),
        });
        index
    }

    fn note_end(&mut self, bounds: Option<Bounds>) {
        if let Some((_, end)) = bounds {
            self.read_to = self.read_to.max(end);
        }
    }

    fn grammar(&mut self, root: &BbnfValue<'p>) {
        let Some(grammar) = self.compound(root) else {
            return;
        };
        for item in &grammar.children {
            let Some(item) = self.compound(item) else {
                continue;
            };
            let Some(inner) = item.children.first() else {
                continue;
            };
            let bounds = self.bounds(inner);
            self.note_end(bounds);
            let Some(bounds) = bounds else {
                self.invariant((0, 0), "a grammar item without bounds");
                continue;
            };
            let Some(node) = self.compound(inner) else {
                self.invariant(bounds, "a grammar item that is not a compound");
                continue;
            };
            match node.kind {
                K::Rule => self.rule(node, bounds),
                K::ImportDirective => self.import(node, bounds),
                K::WsDirective => self.ws(node, bounds),
                K::TokenDirective => {
                    let name = self.first_span(node).unwrap_or_default();
                    self.directives.tokens.push((name, self.span(bounds)));
                }
                K::DebugDirective => {
                    let name = self.first_span(node).unwrap_or_default();
                    let target = (name != "*").then_some(name);
                    self.directives.debug.push((target, self.span(bounds)));
                }
                K::HostDirective => self.host(node, bounds),
                K::RecoverDirective => self.recover(node, bounds),
                K::PrettyDirective => self.pretty(node, bounds),
                // `comment` and `big_comment` (grammar_item tags 0 and 1).
                _ if matches!(item.branch_tag, Some(0 | 1)) => {
                    self.directives.comments.push(self.span(bounds));
                }
                other => self.invariant(bounds, &format!("unread grammar item {other:?}")),
            }
        }
    }

    fn first_span(&self, node: &BbnfCompound<'p>) -> Option<String> {
        node.children.iter().find_map(|child| match child {
            BbnfValue::Span(text) => Some((*text).to_string()),
            _ => None,
        })
    }

    fn rule(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) {
        let Some(name) = self.first_span(node) else {
            self.invariant(bounds, "a rule without a name");
            return;
        };
        let Some(rhs) = node.children.iter().find(|c| self.compound(c).is_some()) else {
            self.invariant(bounds, "a rule without a right side");
            return;
        };
        let Some(root) = self.rhs(rhs, bounds) else {
            return;
        };
        let id = RuleIx::checked(self.rules.len()).expect("rule budget bounds rule identity");
        self.rules.push(SourceRuleData {
            id,
            name,
            root,
            span: self.span(bounds),
        });
    }

    /// `rhs = closure | alternation`.
    fn rhs(&mut self, value: &BbnfValue<'p>, outer: Bounds) -> Option<NodeIx> {
        let node = self.compound(value)?;
        let bounds = self.bounds(value).unwrap_or(outer);
        match node.kind {
            K::Closure => {
                let params = node
                    .children
                    .iter()
                    .filter_map(|c| match c {
                        BbnfValue::Span(text) => Some((*text).to_string()),
                        _ => None,
                    })
                    .collect();
                let body = node.children.iter().find(|c| self.compound(c).is_some())?;
                let body = self.rhs(body, bounds)?;
                Some(self.push(SourceNodeKindData::Closure { params, body }, bounds))
            }
            K::Alternation => {
                let mut arms = Vec::new();
                for child in &node.children {
                    arms.push(self.concatenation(child, bounds)?);
                }
                Some(match arms.as_slice() {
                    [one] => *one,
                    _ => self.push(SourceNodeKindData::Choice(arms), bounds),
                })
            }
            other => {
                self.invariant(bounds, &format!("unread right side {other:?}"));
                None
            }
        }
    }

    fn concatenation(&mut self, value: &BbnfValue<'p>, outer: Bounds) -> Option<NodeIx> {
        let node = self.compound(value)?;
        let bounds = self.bounds(value).unwrap_or(outer);
        let mut items = Vec::new();
        for child in &node.children {
            items.push(self.binary_factor(child, bounds)?);
        }
        Some(match items.as_slice() {
            [one] => *one,
            _ => self.push(SourceNodeKindData::Sequence(items), bounds),
        })
    }

    /// `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *`,
    /// folded to the left; each operator is read from the source between two
    /// operands (the tree keeps one branch tag for the whole chain).
    fn binary_factor(&mut self, value: &BbnfValue<'p>, outer: Bounds) -> Option<NodeIx> {
        let node = self.compound(value)?;
        let mut operands = node.children.iter();
        let first = operands.next()?;
        let mut left_bounds = self.bounds(first).unwrap_or(outer);
        let mut left = self.mapped_factor(first, left_bounds)?;
        for right_value in operands {
            let right_bounds = self.bounds(right_value).unwrap_or(outer);
            let right = self.mapped_factor(right_value, right_bounds)?;
            let between = self.text[left_bounds.1.min(right_bounds.0)..right_bounds.0].trim();
            let kind = match between {
                "<<" => SourceNodeKindData::Projection {
                    left,
                    right,
                    keep: ProjectionKeep::Left,
                },
                ">>" => SourceNodeKindData::Projection {
                    left,
                    right,
                    keep: ProjectionKeep::Right,
                },
                "-" => SourceNodeKindData::Difference {
                    include: left,
                    exclude: right,
                },
                other => {
                    self.invariant(
                        (left_bounds.1, right_bounds.0),
                        &format!("binary operator `{other}` is none of `<<`, `>>`, `-`"),
                    );
                    return None;
                }
            };
            let bounds = (left_bounds.0, right_bounds.1);
            left = self.push(kind, bounds);
            left_bounds = bounds;
        }
        Some(left)
    }

    /// `mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ?`.
    fn mapped_factor(&mut self, value: &BbnfValue<'p>, outer: Bounds) -> Option<NodeIx> {
        let node = self.compound(value)?;
        let bounds = self.bounds(value).unwrap_or(outer);
        let factor_value = node.children.first()?;
        let factor_bounds = self.bounds(factor_value).unwrap_or(bounds);
        let child = self.factor(factor_value, factor_bounds)?;
        if node.children.len() == 1 {
            return Some(child);
        }
        let annotation = node.children.iter().skip(1).find(|c| {
            self.compound(c)
                .is_some_and(|c| c.kind == K::TypeAnnotation)
        });
        let annotation_bounds = annotation.and_then(|a| self.bounds(a));
        let value_end = annotation_bounds.map_or(bounds.1, |(start, _)| start);
        let raw = &self.text[factor_bounds.1.min(value_end)..value_end];
        let Some(value_text) = raw.trim().strip_prefix("->") else {
            self.invariant(bounds, "a mapping without `->`");
            return None;
        };
        let value_text = value_text.trim().to_string();
        let head = node
            .children
            .get(1)
            .map_or(SourceValueHead::Expression, |v| self.value_head(v));
        let ty = annotation_bounds.map(|(start, end)| {
            self.text[start..end]
                .trim()
                .trim_start_matches(':')
                .trim()
                .to_string()
        });
        Some(self.push(
            SourceNodeKindData::Mapping {
                child,
                value: SourceValueData {
                    head,
                    text: value_text,
                },
                ty,
            },
            bounds,
        ))
    }

    /// The typed head of a value expression: descend through single-child
    /// precedence levels to the first level that is not a pass-through.
    fn value_head(&self, value: &BbnfValue<'p>) -> SourceValueHead {
        let Some(node) = self.compound(value) else {
            return match value {
                BbnfValue::Int(_) => SourceValueHead::Int,
                BbnfValue::Float(_) => SourceValueHead::Float,
                BbnfValue::Bool(_) => SourceValueHead::Bool,
                BbnfValue::Span(text) if text.starts_with('"') => SourceValueHead::String,
                BbnfValue::Span(text) => SourceValueHead::Path(vec![(*text).to_string()]),
                _ => SourceValueHead::Expression,
            };
        };
        let spans = |node: &BbnfCompound<'p>| -> Vec<String> {
            node.children
                .iter()
                .filter_map(|c| match c {
                    BbnfValue::Span(text) => Some((*text).to_string()),
                    _ => None,
                })
                .collect()
        };
        match node.kind {
            K::ValueOr | K::ValueAnd | K::ValueCmp | K::ValueAdd | K::ValueMul
                if node.children.len() == 1 =>
            {
                self.value_head(&node.children[0])
            }
            // value_unary tag 1: a bare atom.
            K::ValueUnary if node.branch_tag == Some(1) && node.children.len() == 1 => {
                self.value_head(&node.children[0])
            }
            K::ValueAtom => match node.children.as_slice() {
                [only] => self.value_head(only),
                _ => SourceValueHead::Expression,
            },
            K::ValuePath => SourceValueHead::Path(spans(node)),
            K::ValueInput => SourceValueHead::Input(spans(node)),
            K::ValueFnCall => SourceValueHead::Call(
                node.children
                    .first()
                    .and_then(|p| self.compound(p))
                    .map(spans)
                    .unwrap_or_default(),
            ),
            K::ValueClosure => SourceValueHead::Closure(spans(node)),
            _ => SourceValueHead::Expression,
        }
    }

    /// `factor = ( big_comment | comment ) * , term ?w , modifier ? , ( big_comment | comment ) *`.
    fn factor(&mut self, value: &BbnfValue<'p>, outer: Bounds) -> Option<NodeIx> {
        let node = self.compound(value)?;
        let bounds = self.bounds(value).unwrap_or(outer);
        let mut term = None;
        let mut modifier = None;
        for child in &node.children {
            match (child, self.compound(child)) {
                (_, Some(c)) if c.kind == K::Term => {
                    term = Some(self.term(c, self.bounds(child).unwrap_or(bounds))?);
                }
                // A comment inside a factor: a compound holding the comment's span.
                (_, Some(_)) => {
                    if let Some(b) = self.bounds(child) {
                        self.directives.comments.push(self.span(b));
                    }
                }
                (BbnfValue::Span(text), None) => modifier = Some(*text),
                _ => {}
            }
        }
        let Some(term) = term else {
            self.invariant(bounds, "a factor without a term");
            return None;
        };
        Some(match modifier {
            None => term,
            Some("?w") => self.push(SourceNodeKindData::Trim { child: term }, bounds),
            Some("?") => self.push(
                SourceNodeKindData::Repeat {
                    child: term,
                    minimum: 0,
                    maximum: Some(1),
                },
                bounds,
            ),
            Some("*") => self.push(
                SourceNodeKindData::Repeat {
                    child: term,
                    minimum: 0,
                    maximum: None,
                },
                bounds,
            ),
            Some("+") => self.push(
                SourceNodeKindData::Repeat {
                    child: term,
                    minimum: 1,
                    maximum: None,
                },
                bounds,
            ),
            Some(other) => {
                self.invariant(bounds, &format!("modifier `{other}` is none of `?w ? * +`"));
                return None;
            }
        })
    }

    /// `term`, by its branch: 0 `ε`, 1 `epsilon`, 2 identifier (with call
    /// arguments), 3 literal, 4 regex, 5 `@{ }`, 6 `( )`, 7 `[ ]`, 8 `{ }`.
    fn term(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) -> Option<NodeIx> {
        let inner_rhs = |reader: &mut Self| -> Option<NodeIx> {
            let rhs = node
                .children
                .iter()
                .find(|c| reader.compound(c).is_some())?;
            reader.rhs(rhs, bounds)
        };
        match node.branch_tag {
            Some(0 | 1) => Some(self.push(SourceNodeKindData::Epsilon, bounds)),
            Some(2) => {
                let name = self.first_span(node)?;
                let args: Vec<&BbnfValue<'p>> = node
                    .children
                    .iter()
                    .filter(|c| self.compound(c).is_some_and(|c| c.kind == K::CallArg))
                    .collect();
                if args.is_empty() {
                    return Some(self.push(SourceNodeKindData::Reference(name), bounds));
                }
                let mut read = Vec::with_capacity(args.len());
                for arg in args {
                    read.push(self.call_arg(arg, bounds)?);
                }
                Some(self.push(
                    SourceNodeKindData::Call {
                        callee: name,
                        args: read,
                    },
                    bounds,
                ))
            }
            Some(3) => {
                let text = self.first_span(node)?;
                Some(self.push(SourceNodeKindData::Literal(unescape_literal(&text)), bounds))
            }
            Some(4) => {
                let regex = node.children.iter().find_map(|c| self.compound(c))?;
                let text = self.first_span(regex)?;
                let pattern = text
                    .strip_prefix('/')
                    .and_then(|t| t.strip_suffix('/'))
                    .unwrap_or(&text)
                    .to_string();
                let regex_bounds = self.bounds(&node.children[0]).unwrap_or(bounds);
                if let Err(error) =
                    RegexTablePlan::compile_with(&pattern, self.options.regex.table_options())
                {
                    self.diagnostics
                        .push(regex_diagnostic(regex_bounds.0, regex_bounds.1, error));
                    return None;
                }
                Some(self.push(SourceNodeKindData::Regex(pattern), regex_bounds))
            }
            Some(5) => {
                let child = inner_rhs(self)?;
                Some(self.push(SourceNodeKindData::SpanCapture { child }, bounds))
            }
            Some(6) => inner_rhs(self),
            Some(7) => {
                let child = inner_rhs(self)?;
                Some(self.push(
                    SourceNodeKindData::Repeat {
                        child,
                        minimum: 0,
                        maximum: Some(1),
                    },
                    bounds,
                ))
            }
            Some(8) => {
                let child = inner_rhs(self)?;
                Some(self.push(
                    SourceNodeKindData::Repeat {
                        child,
                        minimum: 0,
                        maximum: None,
                    },
                    bounds,
                ))
            }
            other => {
                self.invariant(bounds, &format!("unread term branch {other:?}"));
                None
            }
        }
    }

    /// `call_arg = ( binary_factor ?w , "|" ? ) +`: items separated by `|`
    /// are alternatives, juxtaposed items a sequence (read from the source).
    fn call_arg(&mut self, value: &BbnfValue<'p>, outer: Bounds) -> Option<NodeIx> {
        let node = self.compound(value)?;
        let bounds = self.bounds(value).unwrap_or(outer);
        let mut groups: Vec<Vec<NodeIx>> = vec![Vec::new()];
        let mut previous_end: Option<usize> = None;
        for child in &node.children {
            let child_bounds = self.bounds(child).unwrap_or(bounds);
            if let Some(end) = previous_end
                && self.text[end.min(child_bounds.0)..child_bounds.0].trim() == "|"
            {
                groups.push(Vec::new());
            }
            let item = self.binary_factor(child, child_bounds)?;
            groups.last_mut().expect("one group at least").push(item);
            previous_end = Some(child_bounds.1);
        }
        let mut arms = Vec::with_capacity(groups.len());
        for group in groups {
            arms.push(match group.as_slice() {
                [one] => *one,
                _ => self.push(SourceNodeKindData::Sequence(group), bounds),
            });
        }
        Some(match arms.as_slice() {
            [one] => *one,
            _ => self.push(SourceNodeKindData::Choice(arms), bounds),
        })
    }

    fn import(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) {
        let mut items = None;
        let mut path = None;
        for child in &node.children {
            let Some(c) = self.compound(child) else {
                continue;
            };
            match c.kind {
                K::ImportItems => {
                    items = Some(
                        c.children
                            .iter()
                            .filter_map(|v| match v {
                                BbnfValue::Span(text) => Some((*text).to_string()),
                                _ => None,
                            })
                            .collect(),
                    );
                }
                K::ImportPath => {
                    path = self
                        .first_span(c)
                        .map(|p| String::from_utf8_lossy(&unescape_literal(&p)).into_owned());
                }
                _ => {}
            }
        }
        let Some(path) = path else {
            self.invariant(bounds, "an import without a path");
            return;
        };
        self.directives.imports.push(SourceImport {
            items,
            path,
            span: self.span(bounds),
        });
    }

    fn ws(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) {
        let pattern = node
            .children
            .iter()
            .find_map(|c| self.compound(c))
            .and_then(|regex| self.first_span(regex))
            .map(|text| {
                text.strip_prefix('/')
                    .and_then(|t| t.strip_suffix('/'))
                    .unwrap_or(&text)
                    .to_string()
            });
        let Some(pattern) = pattern else {
            self.invariant(bounds, "a `@ws` without its regex");
            return;
        };
        if let Err(error) =
            RegexTablePlan::compile_with(&pattern, self.options.regex.table_options())
        {
            self.diagnostics
                .push(regex_diagnostic(bounds.0, bounds.1, error));
            return;
        }
        self.directives.ws.push((pattern, self.span(bounds)));
    }

    /// `@host name ( : type_name ) ?` — the type is read from the source (the
    /// tree keeps the type as a unit value without bounds).
    fn host(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) {
        let Some(name) = self.first_span(node) else {
            self.invariant(bounds, "a `@host` without a name");
            return;
        };
        let text = self.text[bounds.0..bounds.1]
            .trim_end()
            .trim_end_matches([';', '.'])
            .trim_end();
        let ty = text
            .split_once(':')
            .map(|(_, ty)| ty.trim().to_string())
            .filter(|ty| !ty.is_empty());
        self.directives.hosts.push(SourceHost {
            name,
            ty,
            span: self.span(bounds),
        });
    }

    fn recover(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) {
        let Some(rule) = self.first_span(node) else {
            self.invariant(bounds, "a `@recover` without its rule");
            return;
        };
        let Some(rhs) = node.children.iter().find(|c| self.compound(c).is_some()) else {
            self.invariant(bounds, "a `@recover` without its sync expression");
            return;
        };
        let Some(sync) = self.rhs(rhs, bounds) else {
            return;
        };
        self.directives.recovers.push(SourceRecover {
            rule,
            sync,
            span: self.span(bounds),
        });
    }

    fn pretty(&mut self, node: &'d BbnfCompound<'p>, bounds: Bounds) {
        let Some(target) = self.first_span(node) else {
            self.invariant(bounds, "a `@pretty` without its target");
            return;
        };
        let mut hints = Vec::new();
        for child in &node.children {
            let Some(c) = self.compound(child) else {
                continue;
            };
            if c.kind != K::PrettyHint {
                continue;
            }
            let hint_bounds = self.bounds(child).unwrap_or(bounds);
            let Some(name) = self.first_span(c) else {
                continue;
            };
            let argument = {
                let text = &self.text[hint_bounds.0..hint_bounds.1];
                text.split_once('(')
                    .and_then(|(_, rest)| rest.rsplit_once(')'))
                    .map(|(arg, _)| arg.trim().to_string())
            };
            let hint = match (name.as_str(), argument) {
                ("group", None) => SourcePresentationHintData::Group,
                ("block", None) => SourcePresentationHintData::Block,
                ("indent", None) => SourcePresentationHintData::Indent,
                ("dedent", None) => SourcePresentationHintData::Dedent,
                ("softline", None) => SourcePresentationHintData::Softline,
                ("hardline", None) => SourcePresentationHintData::Hardline,
                ("blankline", None) => SourcePresentationHintData::Blankline,
                ("compact", None) => SourcePresentationHintData::Compact,
                ("hardbreak", None) => SourcePresentationHintData::Hardbreak,
                ("softbreak", None) => SourcePresentationHintData::Softbreak,
                ("nobreak", None) => SourcePresentationHintData::Nobreak,
                ("fast", None) => SourcePresentationHintData::Fast,
                ("off", None) => SourcePresentationHintData::Off,
                ("sep", Some(arg)) => SourcePresentationHintData::Sep {
                    literal: unescape_literal(&arg),
                },
                (other, _) => {
                    self.diagnostics.push(Diagnostic::new(
                        DiagnosticCode::FormWithoutMeaning,
                        hint_bounds.0,
                        hint_bounds.1,
                        format!(
                            "`@pretty` hint `{other}` is not one `docs/bbnf/pretty-directives.md` names"
                        ),
                    ));
                    continue;
                }
            };
            hints.push(hint);
        }
        self.presentation
            .push(SourcePresentationData { target, hints });
    }
}

/// A quoted literal's bytes: the delimiters removed and `\n`, `\r`, `\t`,
/// `\0` and `\<byte>` decoded, as the spine's lexical reading decodes them.
fn unescape_literal(quoted: &str) -> Vec<u8> {
    let bytes = quoted.as_bytes();
    let inner = match bytes {
        [open, inner @ .., close] if open == close && matches!(open, b'"' | b'\'' | b'`') => inner,
        _ => bytes,
    };
    let mut out = Vec::with_capacity(inner.len());
    let mut iter = inner.iter();
    while let Some(&byte) = iter.next() {
        if byte == b'\\' {
            if let Some(&escaped) = iter.next() {
                out.push(match escaped {
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'0' => b'\0',
                    other => other,
                });
            }
        } else {
            out.push(byte);
        }
    }
    out
}

fn regex_diagnostic(start: usize, end: usize, error: TablePlanCompileError) -> Diagnostic {
    let code = if matches!(
        error,
        TablePlanCompileError::NfaStateLimitExceeded { .. }
            | TablePlanCompileError::StateLimitExceeded { .. }
    ) {
        DiagnosticCode::ResourceLimit
    } else {
        DiagnosticCode::Regex
    };
    Diagnostic::new(code, start, end, error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::diagnostic::DiagnosticCode;

    /// Every form `grammar/bbnf/bbnf.bbnf` names, once.
    const EVERY_FORM: &str = r#"@import { a, b } from "lib" ;
@import "other" ;
@ws /[ \t]+/ ;
@token ident ;
@debug * ;
@host Env : Map ;
@recover stmt /[^;]*/ ;
@pretty pair group indent sep(", ") ;
// a line comment
/* a block comment */
ident = /[a-z]+/ -> Span ;
num = /[0-9]+/ -> parse(input) : i64 ;
tagged = "x\n" -> 3u8 ;
pair = ident << ":" >> num ;
diff = ident - "x" ;
juxt = ident num ;
opt = [ ident ] , { num } , @{ ident , num } , ( ident | num ) ;
mods = ident ?w , num ? , ident * , num + ;
wrap = |x, y| "(" , x , y , ")" ;
use = wrap(ident, num | ident) ;
eps = ε | epsilon ;
flags = /(?i)abc/ ;
done = ident .
"#;

    fn read(text: &str) -> StageResult<BootstrapRead> {
        read_source(
            text.as_bytes(),
            Revision::from_bytes([0; 32]),
            CompileOptions::default(),
        )
    }

    fn accepted(text: &str) -> BootstrapRead {
        match read(text) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { diagnostics } => panic!("refused: {diagnostics:?}"),
        }
    }

    fn root<'r>(read: &'r BootstrapRead, rule: &str) -> &'r SourceNodeKindData {
        let rule = read
            .module
            .rules
            .iter()
            .find(|r| r.name == rule)
            .unwrap_or_else(|| panic!("rule {rule}"));
        &read.module.nodes[rule.root.index()].kind
    }

    fn kind(read: &BootstrapRead, node: NodeIx) -> &SourceNodeKindData {
        &read.module.nodes[node.index()].kind
    }

    #[test]
    fn every_rule_form_reads_to_a_typed_node() {
        let read = accepted(EVERY_FORM);
        let names: Vec<&str> = read.module.rules.iter().map(|r| r.name.as_str()).collect();
        assert_eq!(
            names,
            [
                "ident", "num", "tagged", "pair", "diff", "juxt", "opt", "mods", "wrap", "use",
                "eps", "flags", "done"
            ]
        );
        match root(&read, "ident") {
            SourceNodeKindData::Mapping { child, value, ty } => {
                assert!(
                    matches!(kind(&read, *child), SourceNodeKindData::Regex(p) if p == "[a-z]+")
                );
                assert_eq!(value.head, SourceValueHead::Path(vec!["Span".into()]));
                assert_eq!(value.text, "Span");
                assert_eq!(ty, &None);
            }
            other => panic!("ident: {other:?}"),
        }
        match root(&read, "num") {
            SourceNodeKindData::Mapping { value, ty, .. } => {
                assert_eq!(value.head, SourceValueHead::Call(vec!["parse".into()]));
                assert_eq!(value.text, "parse(input)");
                assert_eq!(ty.as_deref(), Some("i64"));
            }
            other => panic!("num: {other:?}"),
        }
        match root(&read, "tagged") {
            SourceNodeKindData::Mapping { child, value, .. } => {
                assert!(
                    matches!(kind(&read, *child), SourceNodeKindData::Literal(b) if b == b"x\n")
                );
                assert_eq!(value.head, SourceValueHead::Int);
                assert_eq!(
                    value.text, "3u8",
                    "the integer suffix is read from the source"
                );
            }
            other => panic!("tagged: {other:?}"),
        }
        match root(&read, "pair") {
            SourceNodeKindData::Projection {
                left,
                keep: ProjectionKeep::Right,
                ..
            } => {
                assert!(matches!(
                    kind(&read, *left),
                    SourceNodeKindData::Projection {
                        keep: ProjectionKeep::Left,
                        ..
                    }
                ));
            }
            other => panic!("pair: {other:?}"),
        }
        assert!(matches!(
            root(&read, "diff"),
            SourceNodeKindData::Difference { .. }
        ));
        assert!(
            matches!(root(&read, "juxt"), SourceNodeKindData::Sequence(items) if items.len() == 2)
        );
        match root(&read, "opt") {
            SourceNodeKindData::Sequence(items) => {
                assert!(matches!(
                    kind(&read, items[0]),
                    SourceNodeKindData::Repeat {
                        minimum: 0,
                        maximum: Some(1),
                        ..
                    }
                ));
                assert!(matches!(
                    kind(&read, items[1]),
                    SourceNodeKindData::Repeat {
                        minimum: 0,
                        maximum: None,
                        ..
                    }
                ));
                assert!(matches!(
                    kind(&read, items[2]),
                    SourceNodeKindData::SpanCapture { .. }
                ));
                assert!(
                    matches!(kind(&read, items[3]), SourceNodeKindData::Choice(arms) if arms.len() == 2)
                );
            }
            other => panic!("opt: {other:?}"),
        }
        match root(&read, "mods") {
            SourceNodeKindData::Sequence(items) => {
                assert!(matches!(
                    kind(&read, items[0]),
                    SourceNodeKindData::Trim { .. }
                ));
                assert!(matches!(
                    kind(&read, items[1]),
                    SourceNodeKindData::Repeat {
                        minimum: 0,
                        maximum: Some(1),
                        ..
                    }
                ));
                assert!(matches!(
                    kind(&read, items[2]),
                    SourceNodeKindData::Repeat {
                        minimum: 0,
                        maximum: None,
                        ..
                    }
                ));
                assert!(matches!(
                    kind(&read, items[3]),
                    SourceNodeKindData::Repeat {
                        minimum: 1,
                        maximum: None,
                        ..
                    }
                ));
            }
            other => panic!("mods: {other:?}"),
        }
        match root(&read, "wrap") {
            SourceNodeKindData::Closure { params, body } => {
                assert_eq!(params, &["x".to_string(), "y".to_string()]);
                assert!(
                    matches!(kind(&read, *body), SourceNodeKindData::Sequence(items) if items.len() == 4)
                );
            }
            other => panic!("wrap: {other:?}"),
        }
        match root(&read, "use") {
            SourceNodeKindData::Call { callee, args } => {
                assert_eq!(callee, "wrap");
                assert_eq!(args.len(), 2);
                assert!(
                    matches!(kind(&read, args[0]), SourceNodeKindData::Reference(n) if n == "ident")
                );
                assert!(
                    matches!(kind(&read, args[1]), SourceNodeKindData::Choice(arms) if arms.len() == 2)
                );
            }
            other => panic!("use: {other:?}"),
        }
        match root(&read, "eps") {
            SourceNodeKindData::Choice(arms) => {
                assert!(
                    arms.iter()
                        .all(|a| matches!(kind(&read, *a), SourceNodeKindData::Epsilon))
                );
            }
            other => panic!("eps: {other:?}"),
        }
        assert!(
            matches!(root(&read, "flags"), SourceNodeKindData::Regex(p) if p == "(?i)abc"),
            "an inline flag group is read as part of its pattern"
        );
        assert!(matches!(root(&read, "done"), SourceNodeKindData::Reference(n) if n == "ident"));
        // Postorder: every child precedes its parent.
        for (index, node) in read.module.nodes.iter().enumerate() {
            let children: Vec<NodeIx> = match &node.kind {
                SourceNodeKindData::Sequence(c) | SourceNodeKindData::Choice(c) => c.clone(),
                SourceNodeKindData::Repeat { child, .. }
                | SourceNodeKindData::Trim { child }
                | SourceNodeKindData::SpanCapture { child }
                | SourceNodeKindData::Mapping { child, .. } => vec![*child],
                SourceNodeKindData::Projection { left, right, .. } => vec![*left, *right],
                SourceNodeKindData::Difference { include, exclude } => vec![*include, *exclude],
                SourceNodeKindData::Call { args, .. } => args.clone(),
                SourceNodeKindData::Closure { body, .. } => vec![*body],
                _ => Vec::new(),
            };
            assert!(
                children.iter().all(|c| c.index() < index),
                "node {index} is not postorder"
            );
        }
    }

    #[test]
    fn every_directive_and_comment_is_read() {
        let read = accepted(EVERY_FORM);
        let d = &read.directives;
        assert_eq!(d.imports.len(), 2);
        assert_eq!(
            d.imports[0].items.as_deref(),
            Some(&["a".to_string(), "b".to_string()][..])
        );
        assert_eq!(d.imports[0].path, "lib");
        assert_eq!(d.imports[1].items, None);
        assert_eq!(d.imports[1].path, "other");
        assert_eq!(
            d.ws.iter().map(|(p, _)| p.as_str()).collect::<Vec<_>>(),
            ["[ \\t]+"]
        );
        assert_eq!(
            d.tokens.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
            ["ident"]
        );
        assert_eq!(
            d.debug.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(),
            [None]
        );
        assert_eq!(d.hosts.len(), 1);
        assert_eq!(d.hosts[0].name, "Env");
        assert_eq!(d.hosts[0].ty.as_deref(), Some("Map"));
        assert_eq!(d.recovers.len(), 1);
        assert_eq!(d.recovers[0].rule, "stmt");
        assert!(matches!(
            &read.module.nodes[d.recovers[0].sync.index()].kind,
            SourceNodeKindData::Regex(p) if p == "[^;]*"
        ));
        assert_eq!(d.comments.len(), 2);
        assert_eq!(read.presentation.len(), 1);
        assert_eq!(read.presentation[0].target, "pair");
        assert_eq!(
            read.presentation[0].hints,
            [
                SourcePresentationHintData::Group,
                SourcePresentationHintData::Indent,
                SourcePresentationHintData::Sep {
                    literal: b", ".to_vec()
                },
            ]
        );
    }

    #[test]
    fn text_that_is_not_a_grammar_is_refused_as_syntax() {
        for text in ["rule = ;", "a = b", "= x ;", "a = \"x\" ; ???"] {
            match read(text) {
                StageResult::Rejected { diagnostics } => assert!(
                    diagnostics
                        .iter()
                        .any(|d| d.code() == DiagnosticCode::Syntax),
                    "{text:?}: {diagnostics:?}"
                ),
                StageResult::Accepted { .. } => panic!("{text:?} was accepted"),
            }
        }
    }

    #[test]
    fn an_invalid_regex_is_refused_as_regex() {
        match read("a = /(/ ;") {
            StageResult::Rejected { diagnostics } => {
                assert!(
                    diagnostics
                        .iter()
                        .any(|d| d.code() == DiagnosticCode::Regex),
                    "{diagnostics:?}"
                );
            }
            StageResult::Accepted { .. } => panic!("an unbalanced regex was accepted"),
        }
    }
}
