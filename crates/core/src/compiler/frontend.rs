use bbnf_regex::{RegexTablePlan, TablePlanCompileError};
use parse_that::{
    EventSink, Fault, Input, RegexPlan, RegexTerminal, ResourceKind, Run, Step, choice, eof,
    literal, regex, repeat, sequence,
};

use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::{
    ModuleInput, SourceModule, SourceNode, SourceNodeKind, SourceRecordSlot, SourceRule, SourceSpan,
};
use super::syntax::{NodeId, RuleId, TransformId};

const IDENTIFIER_PATTERN: &str = "[A-Za-z_][A-Za-z0-9_-]*";
const WHITESPACE_PATTERN: &str = "[ \t\r\n]*";

macro_rules! matched {
    ($step:expr) => {
        match $step {
            Step::Matched(value) => value,
            Step::Mismatch(failure) => return Step::Mismatch(failure),
            Step::Fault(fault) => return Step::Fault(fault),
        }
    };
}

#[derive(Clone, Debug)]
enum DraftNodeKind {
    Epsilon,
    Literal(Vec<u8>),
    Regex(Vec<u8>),
    Reference(String),
    Sequence(Vec<usize>),
    Choice(Vec<usize>),
    Repeat {
        child: usize,
        minimum: u32,
        maximum: Option<u32>,
    },
    Transform {
        child: usize,
        transform: usize,
        name: String,
    },
    Record(Vec<DraftRecordSlot>),
}

#[derive(Clone, Debug)]
enum DraftRecordSlot {
    Field {
        name: String,
        child: usize,
        name_start: usize,
        name_end: usize,
        start: usize,
        end: usize,
    },
    Discard {
        child: usize,
        start: usize,
        end: usize,
    },
}

#[derive(Clone, Debug)]
enum SyntaxEvent {
    Node {
        index: usize,
        kind: DraftNodeKind,
        start: usize,
        end: usize,
    },
    Rule {
        index: usize,
        name: String,
        root: usize,
        start: usize,
        end: usize,
    },
}

#[derive(Default)]
struct SyntaxSink {
    events: Vec<SyntaxEvent>,
    nodes: usize,
    rules: usize,
    transforms: usize,
}

impl EventSink for SyntaxSink {
    type Event = SyntaxEvent;

    fn len(&self) -> usize {
        self.events.len()
    }

    fn truncate(&mut self, len: usize) {
        while self.events.len() > len {
            match self
                .events
                .pop()
                .expect("event length exceeds rollback mark")
            {
                SyntaxEvent::Node { kind, .. } => {
                    self.nodes -= 1;
                    if matches!(kind, DraftNodeKind::Transform { .. }) {
                        self.transforms -= 1;
                    }
                }
                SyntaxEvent::Rule { .. } => self.rules -= 1,
            }
        }
    }

    fn push(&mut self, event: Self::Event) {
        match event {
            SyntaxEvent::Node { ref kind, .. } => {
                self.nodes += 1;
                if matches!(kind, DraftNodeKind::Transform { .. }) {
                    self.transforms += 1;
                }
            }
            SyntaxEvent::Rule { .. } => self.rules += 1,
        }
        self.events.push(event);
    }
}

struct LexicalPlans {
    identifier: RegexTablePlan,
    whitespace: RegexTablePlan,
}

impl LexicalPlans {
    fn compile(options: CompileOptions) -> Result<Self, Diagnostic> {
        let table_options = options.regex.table_options();
        let identifier = RegexTablePlan::compile_with(IDENTIFIER_PATTERN, table_options)
            .map_err(|error| lexical_plan_diagnostic("identifier", error))?;
        let whitespace = RegexTablePlan::compile_with(WHITESPACE_PATTERN, table_options)
            .map_err(|error| lexical_plan_diagnostic("whitespace", error))?;
        Ok(Self {
            identifier,
            whitespace,
        })
    }
}

struct LexicalTerminals<'plans> {
    identifier: RegexTerminal<'plans>,
    whitespace: RegexTerminal<'plans>,
}

impl<'plans> LexicalTerminals<'plans> {
    fn checked(plans: &'plans LexicalPlans) -> Result<Self, Diagnostic> {
        Ok(Self {
            identifier: RegexTerminal::new(runtime_plan(&plans.identifier)?),
            whitespace: RegexTerminal::new(runtime_plan(&plans.whitespace)?),
        })
    }
}

#[derive(Clone, Copy)]
struct ParsedNode {
    index: usize,
    start: usize,
    end: usize,
}

struct Parser<'bytes, 'plans> {
    bytes: &'bytes [u8],
    lexical: LexicalTerminals<'plans>,
    options: CompileOptions,
}

impl<'bytes, 'plans> Parser<'bytes, 'plans> {
    fn module<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        matched!(self.whitespace(run));
        matched!(repeat(run, 1, None, |run| self.rule(run)));
        matched!(self.whitespace(run));
        matched!(eof(run));
        Step::Matched(())
    }

    fn rule<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        sequence(run, |run| {
            matched!(self.whitespace(run));
            let name_span = matched!(regex(run, &self.lexical.identifier));
            if run.sink().rules >= self.options.rules {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Work,
                    limit: self.options.rules,
                });
            }
            let name = String::from_utf8(name_span.as_bytes().to_vec())
                .expect("the identifier DFA accepts ASCII only");
            matched!(self.whitespace(run));
            matched!(literal(run, b"="));
            matched!(self.whitespace(run));
            let root = matched!(self.expression(run, 0));
            matched!(self.whitespace(run));
            matched!(literal(run, b";"));
            let end = run.cursor().offset();
            let index = run.sink().rules;
            run.emit(SyntaxEvent::Rule {
                index,
                name,
                root: root.index,
                start: name_span.start(),
                end,
            });
            Step::Matched(())
        })
    }

    fn expression<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        let first = matched!(self.sequence_expression(run, depth));
        let mut children = vec![first.index];
        let mut end = first.end;
        matched!(repeat(run, 0, None, |run| {
            sequence(run, |run| {
                matched!(self.whitespace(run));
                matched!(literal(run, b"|"));
                matched!(self.whitespace(run));
                let child = matched!(self.sequence_expression(run, depth));
                children.push(child.index);
                end = child.end;
                Step::Matched(())
            })
        }));
        if children.len() == 1 {
            Step::Matched(first)
        } else {
            self.emit_node(run, DraftNodeKind::Choice(children), first.start, end)
        }
    }

    fn sequence_expression<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        depth: usize,
    ) -> Step<'i, ParsedNode> {
        let first = matched!(self.postfix(run, depth));
        let mut children = vec![first.index];
        let mut end = first.end;
        matched!(repeat(run, 0, None, |run| {
            sequence(run, |run| {
                matched!(self.whitespace(run));
                matched!(literal(run, b","));
                matched!(self.whitespace(run));
                let child = matched!(self.postfix(run, depth));
                children.push(child.index);
                end = child.end;
                Step::Matched(())
            })
        }));
        if children.len() == 1 {
            Step::Matched(first)
        } else {
            self.emit_node(run, DraftNodeKind::Sequence(children), first.start, end)
        }
    }

    fn postfix<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        let child = matched!(self.primary(run, depth));
        let quantifier = matched!(choice(
            run,
            |run| literal(run, b"?").map_matched(|_| (0, Some(1))),
            |run| {
                choice(
                    run,
                    |run| literal(run, b"*").map_matched(|_| (0, None)),
                    |run| {
                        choice(
                            run,
                            |run| literal(run, b"+").map_matched(|_| (1, None)),
                            |_| Step::Matched((u32::MAX, Some(u32::MAX))),
                        )
                    },
                )
            },
        ));
        if quantifier.0 == u32::MAX {
            Step::Matched(child)
        } else {
            self.emit_node(
                run,
                DraftNodeKind::Repeat {
                    child: child.index,
                    minimum: quantifier.0,
                    maximum: quantifier.1,
                },
                child.start,
                run.cursor().offset(),
            )
        }
    }

    fn primary<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        choice(
            run,
            |run| self.transform(run, depth),
            |run| self.non_transform_primary(run, depth),
        )
    }

    fn non_transform_primary<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        depth: usize,
    ) -> Step<'i, ParsedNode> {
        choice(
            run,
            |run| self.record(run, depth),
            |run| {
                choice(
                    run,
                    |run| self.group(run, depth),
                    |run| {
                        choice(
                            run,
                            |run| self.string_terminal(run),
                            |run| {
                                choice(
                                    run,
                                    |run| self.regex_terminal(run),
                                    |run| {
                                        choice(
                                            run,
                                            |run| self.unicode_epsilon(run),
                                            |run| self.reference_or_epsilon(run),
                                        )
                                    },
                                )
                            },
                        )
                    },
                )
            },
        )
    }

    /// Parse an action-free nominal record: `%{ field: postfix, ~postfix }`.
    ///
    /// The frontend preserves field spelling and slot spans but deliberately
    /// assigns neither ownership nor `FieldId`s. Resolution admits a record
    /// only when this node is the complete root of one rule, then assigns
    /// retained fields in lexical rule/slot order.
    fn record<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            matched!(literal(run, b"%{"));
            if depth >= self.options.syntax_nesting {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Nesting,
                    limit: self.options.syntax_nesting,
                });
            }
            matched!(self.whitespace(run));

            let slots = matched!(choice(
                run,
                |run| { literal(run, b"}").map_matched(|_| Vec::<DraftRecordSlot>::new()) },
                |run| {
                    let mut slots = vec![matched!(self.record_slot(run, depth + 1))];
                    matched!(repeat(run, 0, None, |run| {
                        sequence(run, |run| {
                            matched!(self.whitespace(run));
                            matched!(literal(run, b","));
                            matched!(self.whitespace(run));
                            slots.push(matched!(self.record_slot(run, depth + 1)));
                            Step::Matched(())
                        })
                    }));
                    matched!(self.whitespace(run));
                    matched!(literal(run, b"}"));
                    Step::Matched(slots)
                },
            ));

            self.emit_node(
                run,
                DraftNodeKind::Record(slots),
                start,
                run.cursor().offset(),
            )
        })
    }

    fn record_slot<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        depth: usize,
    ) -> Step<'i, DraftRecordSlot> {
        choice(
            run,
            |run| {
                sequence(run, |run| {
                    let start = run.cursor().offset();
                    let name_span = matched!(regex(run, &self.lexical.identifier));
                    let name = String::from_utf8(name_span.as_bytes().to_vec())
                        .expect("the identifier DFA accepts ASCII only");
                    matched!(self.whitespace(run));
                    matched!(literal(run, b":"));
                    matched!(self.whitespace(run));
                    let child = matched!(self.postfix(run, depth));
                    Step::Matched(DraftRecordSlot::Field {
                        name,
                        child: child.index,
                        name_start: name_span.start(),
                        name_end: name_span.end(),
                        start,
                        end: child.end,
                    })
                })
            },
            |run| {
                sequence(run, |run| {
                    let start = run.cursor().offset();
                    matched!(literal(run, b"~"));
                    matched!(self.whitespace(run));
                    let child = matched!(self.postfix(run, depth));
                    Step::Matched(DraftRecordSlot::Discard {
                        child: child.index,
                        start,
                        end: child.end,
                    })
                })
            },
        )
    }

    /// Parse the sole W2 transform form: `@identifier(expression)`.
    ///
    /// The identifier is inert declarative metadata. It is never interpreted
    /// as host code, a callback, a dependency, or a build feature.
    fn transform<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            matched!(literal(run, b"@"));
            let name_span = matched!(regex(run, &self.lexical.identifier));
            let name = String::from_utf8(name_span.as_bytes().to_vec())
                .expect("the identifier DFA accepts ASCII only");
            matched!(self.whitespace(run));
            matched!(literal(run, b"("));
            if depth >= self.options.syntax_nesting {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Nesting,
                    limit: self.options.syntax_nesting,
                });
            }
            matched!(self.whitespace(run));
            let child = matched!(self.expression(run, depth + 1));
            matched!(self.whitespace(run));
            matched!(literal(run, b")"));
            let transform = run.sink().transforms;
            self.emit_node(
                run,
                DraftNodeKind::Transform {
                    child: child.index,
                    transform,
                    name,
                },
                start,
                run.cursor().offset(),
            )
        })
    }

    fn group<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            matched!(literal(run, b"("));
            if depth >= self.options.syntax_nesting {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Nesting,
                    limit: self.options.syntax_nesting,
                });
            }
            matched!(self.whitespace(run));
            let mut inner = matched!(self.expression(run, depth + 1));
            matched!(self.whitespace(run));
            matched!(literal(run, b")"));
            inner.start = start;
            inner.end = run.cursor().offset();
            Step::Matched(inner)
        })
    }

    fn string_terminal<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            let quote = matched!(choice(
                run,
                |run| literal(run, b"\""),
                |run| literal(run, b"'"),
            ))
            .as_bytes()[0];
            let mut value = Vec::new();
            loop {
                let offset = run.cursor().offset();
                let Some(&byte) = self.bytes.get(offset) else {
                    let expected = [quote];
                    matched!(literal(run, &expected));
                    unreachable!("an EOF cannot match a closing quote");
                };
                if byte == quote {
                    let expected = [quote];
                    matched!(literal(run, &expected));
                    break;
                }
                if byte == b'\n' || byte == b'\r' {
                    let expected = [quote];
                    matched!(literal(run, &expected));
                    unreachable!("a line break cannot match a closing quote");
                }
                if byte == b'\\' {
                    matched!(literal(run, b"\\"));
                    let escaped_at = run.cursor().offset();
                    let Some(&escaped) = self.bytes.get(escaped_at) else {
                        let expected = [quote];
                        matched!(literal(run, &expected));
                        unreachable!("an EOF cannot match a closing quote");
                    };
                    matched!(literal(run, &self.bytes[escaped_at..escaped_at + 1]));
                    value.push(match escaped {
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'0' => b'\0',
                        other => other,
                    });
                } else {
                    matched!(literal(run, &self.bytes[offset..offset + 1]));
                    value.push(byte);
                }
            }
            self.emit_node(
                run,
                DraftNodeKind::Literal(value),
                start,
                run.cursor().offset(),
            )
        })
    }

    fn regex_terminal<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            matched!(literal(run, b"/"));
            let content_start = run.cursor().offset();
            let content_end = loop {
                let offset = run.cursor().offset();
                let Some(&byte) = self.bytes.get(offset) else {
                    matched!(literal(run, b"/"));
                    unreachable!("an EOF cannot match a regex delimiter");
                };
                if byte == b'/' {
                    break offset;
                }
                if byte == b'\\' {
                    matched!(literal(run, b"\\"));
                    let escaped_at = run.cursor().offset();
                    if self.bytes.get(escaped_at).is_none() {
                        matched!(literal(run, b"/"));
                        unreachable!("an EOF cannot match a regex delimiter");
                    }
                    matched!(literal(run, &self.bytes[escaped_at..escaped_at + 1]));
                } else {
                    matched!(literal(run, &self.bytes[offset..offset + 1]));
                }
            };
            matched!(literal(run, b"/"));
            self.emit_node(
                run,
                DraftNodeKind::Regex(self.bytes[content_start..content_end].to_vec()),
                start,
                run.cursor().offset(),
            )
        })
    }

    fn unicode_epsilon<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ParsedNode> {
        let start = run.cursor().offset();
        matched!(literal(run, "ε".as_bytes()));
        self.emit_node(run, DraftNodeKind::Epsilon, start, run.cursor().offset())
    }

    fn reference_or_epsilon<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ParsedNode> {
        let span = matched!(regex(run, &self.lexical.identifier));
        let name = String::from_utf8(span.as_bytes().to_vec())
            .expect("the identifier DFA accepts ASCII only");
        let kind = if name == "epsilon" {
            DraftNodeKind::Epsilon
        } else {
            DraftNodeKind::Reference(name)
        };
        self.emit_node(run, kind, span.start(), span.end())
    }

    fn whitespace<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, parse_that::Span<'i>> {
        regex(run, &self.lexical.whitespace)
    }

    fn emit_node<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        kind: DraftNodeKind,
        start: usize,
        end: usize,
    ) -> Step<'i, ParsedNode> {
        if run.sink().nodes >= self.options.nodes {
            return Step::Fault(Fault::ResourceLimit {
                at: run.cursor(),
                kind: ResourceKind::Events,
                limit: self.options.nodes,
            });
        }
        let index = run.sink().nodes;
        run.emit(SyntaxEvent::Node {
            index,
            kind,
            start,
            end,
        });
        Step::Matched(ParsedNode { index, start, end })
    }
}

trait StepMap<'i, T> {
    fn map_matched<U>(self, map: impl FnOnce(T) -> U) -> Step<'i, U>;
}

impl<'i, T> StepMap<'i, T> for Step<'i, T> {
    fn map_matched<U>(self, map: impl FnOnce(T) -> U) -> Step<'i, U> {
        match self {
            Step::Matched(value) => Step::Matched(map(value)),
            Step::Mismatch(failure) => Step::Mismatch(failure),
            Step::Fault(fault) => Step::Fault(fault),
        }
    }
}

pub(crate) fn parse_source(
    input: ModuleInput<'_>,
    options: CompileOptions,
) -> StageResult<SourceModule> {
    let bytes = input.bytes();
    if bytes.len() > options.source_bytes || u32::try_from(bytes.len()).is_err() {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::SourceTooLarge,
            0,
            bytes.len(),
            format!(
                "source has {} bytes; configured limit is {}",
                bytes.len(),
                options.source_bytes
            ),
        ));
    }

    let plans = match LexicalPlans::compile(options) {
        Ok(plans) => plans,
        Err(diagnostic) => return StageResult::rejected(diagnostic),
    };
    let lexical = match LexicalTerminals::checked(&plans) {
        Ok(lexical) => lexical,
        Err(diagnostic) => return StageResult::rejected(diagnostic),
    };
    let parser = Parser {
        bytes,
        lexical,
        options,
    };
    let result = Run::with_sink(Input::new(bytes), SyntaxSink::default())
        .parse_module(|run| parser.module(run));
    let (_, outcome, sink) = result.into_parts();
    match outcome {
        Step::Matched(_) => build_module(input, sink, options),
        Step::Mismatch(failure) => {
            let at = failure.at().offset();
            StageResult::rejected(Diagnostic::new(
                DiagnosticCode::Syntax,
                at,
                at.saturating_add(1).min(bytes.len()),
                "expected a complete BBNF rule or expression",
            ))
        }
        Step::Fault(Fault::ResourceLimit { at, kind, limit }) => {
            StageResult::rejected(Diagnostic::new(
                DiagnosticCode::ResourceLimit,
                at.offset(),
                at.offset(),
                format!("{kind:?} resource limit {limit} exhausted"),
            ))
        }
        Step::Fault(fault) => StageResult::rejected(Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            fault.at().offset(),
            fault.at().offset(),
            format!("parse-that transaction fault: {fault:?}"),
        )),
    }
}

fn build_module(
    input: ModuleInput<'_>,
    sink: SyntaxSink,
    options: CompileOptions,
) -> StageResult<SourceModule> {
    for event in &sink.events {
        let SyntaxEvent::Node {
            kind: DraftNodeKind::Regex(pattern),
            start,
            end,
            ..
        } = event
        else {
            continue;
        };
        let pattern = match std::str::from_utf8(pattern) {
            Ok(pattern) => pattern,
            Err(error) => {
                return StageResult::rejected(Diagnostic::new(
                    DiagnosticCode::Regex,
                    *start,
                    *end,
                    format!("regex terminal is not UTF-8: {error}"),
                ));
            }
        };
        if let Err(error) = RegexTablePlan::compile_with(pattern, options.regex.table_options()) {
            return StageResult::rejected(regex_diagnostic(*start, *end, error));
        }
    }

    let mut nodes = Vec::with_capacity(sink.nodes);
    let mut rules = Vec::with_capacity(sink.rules);
    let expected_transforms = sink.transforms;
    let mut transforms = 0;
    for event in sink.events {
        match event {
            SyntaxEvent::Node {
                index,
                kind,
                start,
                end,
            } => {
                if index != nodes.len() {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "syntax events are not in stable postorder",
                    ));
                }
                let Some(span) = SourceSpan::checked(start, end) else {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "node span exceeds the source coordinate domain",
                    ));
                };
                let kind = match materialize_node_kind(kind, index) {
                    Some(kind) => kind,
                    None => {
                        return StageResult::rejected(invariant_diagnostic(
                            start,
                            end,
                            "node event refers to a non-postorder child",
                        ));
                    }
                };
                if let SourceNodeKind::Transform { transform, .. } = &kind {
                    if transform.index() != transforms {
                        return StageResult::rejected(invariant_diagnostic(
                            start,
                            end,
                            "transform identities are not contiguous lexical ordinals",
                        ));
                    }
                    transforms += 1;
                }
                nodes.push(SourceNode { kind, span });
            }
            SyntaxEvent::Rule {
                index,
                name,
                root,
                start,
                end,
            } => {
                if index != rules.len() || root >= nodes.len() {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "rule events are not lexical or refer to an unknown root",
                    ));
                }
                let Some(id) = RuleId::checked(index) else {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "rule identity exceeds the compiler coordinate domain",
                    ));
                };
                let Some(root) = NodeId::checked(root) else {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "root identity exceeds the compiler coordinate domain",
                    ));
                };
                let Some(span) = SourceSpan::checked(start, end) else {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "rule span exceeds the source coordinate domain",
                    ));
                };
                rules.push(SourceRule {
                    id,
                    name,
                    root,
                    span,
                });
            }
        }
    }
    if transforms != expected_transforms {
        return StageResult::rejected(invariant_diagnostic(
            0,
            input.bytes().len(),
            "transform event count disagrees with the transactional syntax sink",
        ));
    }

    StageResult::accepted(SourceModule::new(
        input.logical_path().to_owned(),
        input.bytes().to_vec(),
        nodes,
        rules,
    ))
}

fn materialize_node_kind(kind: DraftNodeKind, parent: usize) -> Option<SourceNodeKind> {
    Some(match kind {
        DraftNodeKind::Epsilon => SourceNodeKind::Epsilon,
        DraftNodeKind::Literal(bytes) => SourceNodeKind::Literal(bytes),
        DraftNodeKind::Regex(pattern) => SourceNodeKind::Regex(String::from_utf8(pattern).ok()?),
        DraftNodeKind::Reference(name) => SourceNodeKind::Reference(name),
        DraftNodeKind::Sequence(children) => SourceNodeKind::Sequence(
            children
                .into_iter()
                .map(|child| (child < parent).then_some(child).and_then(NodeId::checked))
                .collect::<Option<Vec<_>>>()?,
        ),
        DraftNodeKind::Choice(children) => SourceNodeKind::Choice(
            children
                .into_iter()
                .map(|child| (child < parent).then_some(child).and_then(NodeId::checked))
                .collect::<Option<Vec<_>>>()?,
        ),
        DraftNodeKind::Repeat {
            child,
            minimum,
            maximum,
        } => SourceNodeKind::Repeat {
            child: (child < parent)
                .then_some(child)
                .and_then(NodeId::checked)?,
            minimum,
            maximum,
        },
        DraftNodeKind::Transform {
            child,
            transform,
            name,
        } => SourceNodeKind::Transform {
            child: (child < parent)
                .then_some(child)
                .and_then(NodeId::checked)?,
            transform: TransformId::checked(transform)?,
            name,
        },
        DraftNodeKind::Record(slots) => SourceNodeKind::Record(
            slots
                .into_iter()
                .map(|slot| match slot {
                    DraftRecordSlot::Field {
                        name,
                        child,
                        name_start,
                        name_end,
                        start,
                        end,
                    } => Some(SourceRecordSlot::Field {
                        name,
                        child: (child < parent)
                            .then_some(child)
                            .and_then(NodeId::checked)?,
                        name_span: SourceSpan::checked(name_start, name_end)?,
                        span: SourceSpan::checked(start, end)?,
                    }),
                    DraftRecordSlot::Discard { child, start, end } => {
                        Some(SourceRecordSlot::Discard {
                            child: (child < parent)
                                .then_some(child)
                                .and_then(NodeId::checked)?,
                            span: SourceSpan::checked(start, end)?,
                        })
                    }
                })
                .collect::<Option<Vec<_>>>()?,
        ),
    })
}

fn runtime_plan(table: &RegexTablePlan) -> Result<RegexPlan<'_>, Diagnostic> {
    RegexPlan::new(
        table.byte_classes(),
        table.state_count() as usize,
        table.class_count() as usize,
        table.start_state() as usize,
        table.transitions(),
        table.accepting(),
    )
    .map_err(|error| {
        Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            0,
            0,
            format!("bbnf-regex emitted a plan rejected by parse-that: {error:?}"),
        )
    })
}

fn lexical_plan_diagnostic(name: &str, error: TablePlanCompileError) -> Diagnostic {
    let code = if is_regex_resource_error(&error) {
        DiagnosticCode::ResourceLimit
    } else {
        DiagnosticCode::InternalInvariant
    };
    Diagnostic::new(
        code,
        0,
        0,
        format!("cannot build the {name} lexical terminal: {error}"),
    )
}

fn regex_diagnostic(start: usize, end: usize, error: TablePlanCompileError) -> Diagnostic {
    let code = if is_regex_resource_error(&error) {
        DiagnosticCode::ResourceLimit
    } else {
        DiagnosticCode::Regex
    };
    Diagnostic::new(code, start, end, error.to_string())
}

fn is_regex_resource_error(error: &TablePlanCompileError) -> bool {
    matches!(
        error,
        TablePlanCompileError::NfaStateLimitExceeded { .. }
            | TablePlanCompileError::StateLimitExceeded { .. }
    )
}

fn invariant_diagnostic(start: usize, end: usize, message: &str) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::InternalInvariant, start, end, message)
}
