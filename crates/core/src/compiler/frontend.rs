use bbnf_regex::{RegexTablePlan, TablePlanCompileError};
use parse_that::{
    EventSink, Fault, Input, RegexPlan, RegexTerminal, ResourceKind, Run, Step, choice, eof,
    literal, regex, repeat, sequence,
};

use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::{
    Revision, SourceModule, SourceNodeData, SourceNodeKindData, SourcePresentationData,
    SourcePresentationHintData, SourceRecordSlotData, SourceRuleData, SourceSpan,
};
use super::syntax::{NodeIx, ProjectionKeep, RuleIx, TransformIx};

const IDENTIFIER_PATTERN: &str = "[A-Za-z_][A-Za-z0-9_-]*";
const WHITESPACE_PATTERN: &str = "[ \t\r\n]+";

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
    Projection {
        left: usize,
        right: usize,
        keep: ProjectionKeep,
    },
    Difference {
        include: usize,
        exclude: usize,
    },
    Trim {
        child: usize,
    },
    Record(Vec<DraftRecordSlot>),
}

#[derive(Clone, Copy, Debug)]
enum PostfixOperator {
    Trim,
    Optional,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Clone, Copy, Debug)]
enum BinaryOperator {
    ProjectLeft,
    ProjectRight,
    Difference,
}

#[derive(Clone, Debug)]
enum DraftPresentationHint {
    Group,
    Block,
    Indent,
    Dedent,
    Softline,
    Hardline,
    Blankline,
    Compact,
    Sep(Vec<u8>),
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
    Presentation {
        target: String,
        hints: Vec<DraftPresentationHint>,
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
    presentation_hints: usize,
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
                SyntaxEvent::Presentation { hints, .. } => {
                    self.presentation_hints -= hints.len();
                }
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
            SyntaxEvent::Presentation { ref hints, .. } => {
                self.presentation_hints += hints.len();
            }
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
        matched!(self.trivia(run));
        matched!(repeat(run, 1, None, |run| {
            choice(run, |run| self.presentation(run), |run| self.rule(run))
        }));
        matched!(self.trivia(run));
        matched!(eof(run));
        Step::Matched(())
    }

    fn presentation<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        sequence(run, |run| {
            matched!(self.trivia(run));
            let start = run.cursor().offset();
            matched!(literal(run, b"@pretty"));
            matched!(self.trivia1(run));
            let target_span = matched!(regex(run, &self.lexical.identifier));
            let target = String::from_utf8(target_span.as_bytes().to_vec())
                .expect("the identifier DFA accepts ASCII only");
            matched!(self.trivia1(run));

            let initial = matched!(self.presentation_hint(run));
            if run.sink().presentation_hints >= self.options.presentation_hints {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Events,
                    limit: self.options.presentation_hints,
                });
            }
            let mut hints = vec![initial];
            matched!(repeat(run, 0, None, |run| {
                sequence(run, |run| {
                    matched!(self.trivia1(run));
                    let hint = matched!(self.presentation_hint(run));
                    if run.sink().presentation_hints + hints.len()
                        >= self.options.presentation_hints
                    {
                        return Step::Fault(Fault::ResourceLimit {
                            at: run.cursor(),
                            kind: ResourceKind::Events,
                            limit: self.options.presentation_hints,
                        });
                    }
                    hints.push(hint);
                    Step::Matched(())
                })
            }));
            matched!(self.trivia(run));
            matched!(literal(run, b";"));
            run.emit(SyntaxEvent::Presentation {
                target,
                hints,
                start,
                end: run.cursor().offset(),
            });
            Step::Matched(())
        })
    }

    fn presentation_hint<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
    ) -> Step<'i, DraftPresentationHint> {
        choice(
            run,
            |run| literal(run, b"group").map_matched(|_| DraftPresentationHint::Group),
            |run| {
                choice(
                    run,
                    |run| literal(run, b"block").map_matched(|_| DraftPresentationHint::Block),
                    |run| {
                        choice(
                            run,
                            |run| {
                                literal(run, b"indent")
                                    .map_matched(|_| DraftPresentationHint::Indent)
                            },
                            |run| {
                                choice(
                                    run,
                                    |run| {
                                        literal(run, b"dedent")
                                            .map_matched(|_| DraftPresentationHint::Dedent)
                                    },
                                    |run| {
                                        choice(
                                            run,
                                            |run| {
                                                literal(run, b"softline").map_matched(|_| {
                                                    DraftPresentationHint::Softline
                                                })
                                            },
                                            |run| {
                                                choice(
                                                    run,
                                                    |run| {
                                                        literal(run, b"hardline").map_matched(
                                                            |_| DraftPresentationHint::Hardline,
                                                        )
                                                    },
                                                    |run| {
                                                        choice(
                                                            run,
                                                            |run| {
                                                                literal(run, b"blankline")
                                                                    .map_matched(|_| {
                                                                        DraftPresentationHint::Blankline
                                                                    })
                                                            },
                                                            |run| {
                                                                choice(
                                                                    run,
                                                                    |run| {
                                                                        literal(run, b"compact")
                                                                            .map_matched(|_| {
                                                                                DraftPresentationHint::Compact
                                                                            })
                                                                    },
                                                                    |run| {
                                                                        self.presentation_sep(run)
                                                                    },
                                                                )
                                                            },
                                                        )
                                                    },
                                                )
                                            },
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

    fn presentation_sep<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
    ) -> Step<'i, DraftPresentationHint> {
        sequence(run, |run| {
            matched!(literal(run, b"sep"));
            matched!(self.trivia(run));
            matched!(literal(run, b"("));
            matched!(self.trivia(run));
            let separator = matched!(self.string_value(run));
            matched!(self.trivia(run));
            matched!(literal(run, b")"));
            Step::Matched(DraftPresentationHint::Sep(separator))
        })
    }

    fn rule<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        sequence(run, |run| {
            matched!(self.trivia(run));
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
            matched!(self.trivia(run));
            matched!(literal(run, b"="));
            matched!(self.trivia(run));
            let root = matched!(self.expression(run, 0));
            matched!(self.trivia(run));
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
        let initial = matched!(self.sequence_expression(run, depth));
        let mut children = vec![initial.index];
        let mut end = initial.end;
        matched!(repeat(run, 0, None, |run| {
            sequence(run, |run| {
                matched!(self.trivia(run));
                matched!(literal(run, b"|"));
                matched!(self.trivia(run));
                let child = matched!(self.sequence_expression(run, depth));
                children.push(child.index);
                end = child.end;
                Step::Matched(())
            })
        }));
        if children.len() == 1 {
            Step::Matched(initial)
        } else {
            self.emit_node(run, DraftNodeKind::Choice(children), initial.start, end)
        }
    }

    fn sequence_expression<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        depth: usize,
    ) -> Step<'i, ParsedNode> {
        let initial = matched!(self.binary_expression(run, depth));
        let mut children = vec![initial.index];
        let mut end = initial.end;
        matched!(repeat(run, 0, None, |run| {
            sequence(run, |run| {
                matched!(self.trivia(run));
                matched!(literal(run, b","));
                matched!(self.trivia(run));
                let child = matched!(self.binary_expression(run, depth));
                children.push(child.index);
                end = child.end;
                Step::Matched(())
            })
        }));
        if children.len() == 1 {
            Step::Matched(initial)
        } else {
            self.emit_node(run, DraftNodeKind::Sequence(children), initial.start, end)
        }
    }

    fn binary_expression<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        depth: usize,
    ) -> Step<'i, ParsedNode> {
        let mut left = matched!(self.postfix(run, depth));
        matched!(repeat(run, 0, None, |run| {
            sequence(run, |run| {
                matched!(self.trivia(run));
                let operator = matched!(choice(
                    run,
                    |run| literal(run, b"<<").map_matched(|_| BinaryOperator::ProjectLeft),
                    |run| {
                        choice(
                            run,
                            |run| literal(run, b">>").map_matched(|_| BinaryOperator::ProjectRight),
                            |run| literal(run, b"-").map_matched(|_| BinaryOperator::Difference),
                        )
                    },
                ));
                matched!(self.trivia(run));
                let right = matched!(self.postfix(run, depth));
                let kind = match operator {
                    BinaryOperator::ProjectLeft => DraftNodeKind::Projection {
                        left: left.index,
                        right: right.index,
                        keep: ProjectionKeep::Left,
                    },
                    BinaryOperator::ProjectRight => DraftNodeKind::Projection {
                        left: left.index,
                        right: right.index,
                        keep: ProjectionKeep::Right,
                    },
                    BinaryOperator::Difference => DraftNodeKind::Difference {
                        include: left.index,
                        exclude: right.index,
                    },
                };
                left = matched!(self.emit_node(run, kind, left.start, right.end));
                Step::Matched(())
            })
        }));
        Step::Matched(left)
    }

    fn postfix<'i>(&self, run: &mut Run<'i, SyntaxSink>, depth: usize) -> Step<'i, ParsedNode> {
        let child = matched!(self.primary(run, depth));
        let operator = matched!(choice(
            run,
            |run| {
                sequence(run, |run| {
                    matched!(self.trivia(run));
                    choice(
                        run,
                        |run| literal(run, b"?w").map_matched(|_| PostfixOperator::Trim),
                        |run| {
                            choice(
                                run,
                                |run| literal(run, b"?").map_matched(|_| PostfixOperator::Optional),
                                |run| {
                                    choice(
                                        run,
                                        |run| {
                                            literal(run, b"*")
                                                .map_matched(|_| PostfixOperator::ZeroOrMore)
                                        },
                                        |run| {
                                            literal(run, b"+")
                                                .map_matched(|_| PostfixOperator::OneOrMore)
                                        },
                                    )
                                },
                            )
                        },
                    )
                    .map_matched(Some)
                })
            },
            |_| Step::Matched(None),
        ));
        match operator {
            None => Step::Matched(child),
            Some(PostfixOperator::Trim) => self.emit_node(
                run,
                DraftNodeKind::Trim { child: child.index },
                child.start,
                run.cursor().offset(),
            ),
            Some(operator) => {
                let (minimum, maximum) = match operator {
                    PostfixOperator::Optional => (0, Some(1)),
                    PostfixOperator::ZeroOrMore => (0, None),
                    PostfixOperator::OneOrMore => (1, None),
                    PostfixOperator::Trim => unreachable!("Trim is materialized separately"),
                };
                self.emit_node(
                    run,
                    DraftNodeKind::Repeat {
                        child: child.index,
                        minimum,
                        maximum,
                    },
                    child.start,
                    run.cursor().offset(),
                )
            }
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
                            |run| self.delimited_repeat(run, depth, b"{", b"}", 0, None),
                            |run| {
                                choice(
                                    run,
                                    |run| self.delimited_repeat(run, depth, b"[", b"]", 0, Some(1)),
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
            matched!(self.trivia(run));

            let slots = matched!(choice(
                run,
                |run| { literal(run, b"}").map_matched(|_| Vec::<DraftRecordSlot>::new()) },
                |run| {
                    let mut slots = vec![matched!(self.record_slot(run, depth + 1))];
                    matched!(repeat(run, 0, None, |run| {
                        sequence(run, |run| {
                            matched!(self.trivia(run));
                            matched!(literal(run, b","));
                            matched!(self.trivia(run));
                            slots.push(matched!(self.record_slot(run, depth + 1)));
                            Step::Matched(())
                        })
                    }));
                    matched!(self.trivia(run));
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
                    matched!(self.trivia(run));
                    matched!(literal(run, b":"));
                    matched!(self.trivia(run));
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
                    matched!(self.trivia(run));
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
            matched!(self.trivia(run));
            matched!(literal(run, b"("));
            if depth >= self.options.syntax_nesting {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Nesting,
                    limit: self.options.syntax_nesting,
                });
            }
            matched!(self.trivia(run));
            let child = matched!(self.expression(run, depth + 1));
            matched!(self.trivia(run));
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
            matched!(self.trivia(run));
            let mut inner = matched!(self.expression(run, depth + 1));
            matched!(self.trivia(run));
            matched!(literal(run, b")"));
            inner.start = start;
            inner.end = run.cursor().offset();
            Step::Matched(inner)
        })
    }

    fn delimited_repeat<'i>(
        &self,
        run: &mut Run<'i, SyntaxSink>,
        depth: usize,
        open: &'static [u8],
        close: &'static [u8],
        minimum: u32,
        maximum: Option<u32>,
    ) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            matched!(literal(run, open));
            if depth >= self.options.syntax_nesting {
                return Step::Fault(Fault::ResourceLimit {
                    at: run.cursor(),
                    kind: ResourceKind::Nesting,
                    limit: self.options.syntax_nesting,
                });
            }
            matched!(self.trivia(run));
            let child = matched!(self.expression(run, depth + 1));
            matched!(self.trivia(run));
            matched!(literal(run, close));
            self.emit_node(
                run,
                DraftNodeKind::Repeat {
                    child: child.index,
                    minimum,
                    maximum,
                },
                start,
                run.cursor().offset(),
            )
        })
    }

    fn string_terminal<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ParsedNode> {
        sequence(run, |run| {
            let start = run.cursor().offset();
            let value = matched!(self.string_value(run));
            self.emit_node(
                run,
                DraftNodeKind::Literal(value),
                start,
                run.cursor().offset(),
            )
        })
    }

    fn string_value<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, Vec<u8>> {
        sequence(run, |run| {
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
            Step::Matched(value)
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

    fn trivia<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        matched!(repeat(run, 0, None, |run| { self.trivia_piece(run) }));
        Step::Matched(())
    }

    fn trivia1<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        matched!(repeat(run, 1, None, |run| self.trivia_piece(run)));
        Step::Matched(())
    }

    fn trivia_piece<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        choice(
            run,
            |run| regex(run, &self.lexical.whitespace).map_matched(|_| ()),
            |run| {
                choice(
                    run,
                    |run| self.line_comment(run),
                    |run| self.block_comment(run),
                )
            },
        )
    }

    fn line_comment<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        sequence(run, |run| {
            matched!(literal(run, b"//"));
            loop {
                let offset = run.cursor().offset();
                let Some(&byte) = self.bytes.get(offset) else {
                    break;
                };
                if byte == b'\n' || byte == b'\r' {
                    break;
                }
                matched!(literal(run, &self.bytes[offset..offset + 1]));
            }
            Step::Matched(())
        })
    }

    fn block_comment<'i>(&self, run: &mut Run<'i, SyntaxSink>) -> Step<'i, ()> {
        sequence(run, |run| {
            matched!(literal(run, b"/*"));
            loop {
                let offset = run.cursor().offset();
                if self
                    .bytes
                    .get(offset..offset.saturating_add(2))
                    .is_some_and(|bytes| bytes == b"*/")
                {
                    matched!(literal(run, b"*/"));
                    return Step::Matched(());
                }
                if self.bytes.get(offset).is_none() {
                    matched!(literal(run, b"*/"));
                    unreachable!("an EOF cannot match a block-comment delimiter");
                }
                matched!(literal(run, &self.bytes[offset..offset + 1]));
            }
        })
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
    bytes: &[u8],
    revision: Revision,
    options: CompileOptions,
) -> StageResult<(SourceModule, Vec<SourcePresentationData>)> {
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
        Step::Matched(_) => build_module(bytes, revision, sink, options),
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
    bytes: &[u8],
    revision: Revision,
    sink: SyntaxSink,
    options: CompileOptions,
) -> StageResult<(SourceModule, Vec<SourcePresentationData>)> {
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
    let mut presentation = Vec::new();
    let mut presentation_spans = Vec::new();
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
                if let SourceNodeKindData::Transform { transform, .. } = &kind {
                    if transform.index() != transforms {
                        return StageResult::rejected(invariant_diagnostic(
                            start,
                            end,
                            "transform identities are not contiguous lexical ordinals",
                        ));
                    }
                    transforms += 1;
                }
                nodes.push(SourceNodeData { kind, span });
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
                let Some(id) = RuleIx::checked(index) else {
                    return StageResult::rejected(invariant_diagnostic(
                        start,
                        end,
                        "rule identity exceeds the compiler coordinate domain",
                    ));
                };
                let Some(root) = NodeIx::checked(root) else {
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
                rules.push(SourceRuleData {
                    id,
                    name,
                    root,
                    span,
                });
            }
            SyntaxEvent::Presentation {
                target,
                hints,
                start,
                end,
            } => {
                presentation.push(SourcePresentationData {
                    target,
                    hints: hints
                        .into_iter()
                        .map(materialize_presentation_hint)
                        .collect(),
                });
                presentation_spans.push((start, end));
            }
        }
    }
    if transforms != expected_transforms {
        return StageResult::rejected(invariant_diagnostic(
            0,
            bytes.len(),
            "transform event count disagrees with the transactional syntax sink",
        ));
    }
    for (row, (start, end)) in presentation.iter().zip(presentation_spans) {
        if !rules.iter().any(|rule| rule.name == row.target) {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::Syntax,
                start,
                end,
                format!(
                    "presentation target `{}` does not name a source rule",
                    row.target
                ),
            ));
        }
    }

    StageResult::accepted((SourceModule::new(revision, nodes, rules), presentation))
}

fn materialize_presentation_hint(hint: DraftPresentationHint) -> SourcePresentationHintData {
    match hint {
        DraftPresentationHint::Group => SourcePresentationHintData::Group,
        DraftPresentationHint::Block => SourcePresentationHintData::Block,
        DraftPresentationHint::Indent => SourcePresentationHintData::Indent,
        DraftPresentationHint::Dedent => SourcePresentationHintData::Dedent,
        DraftPresentationHint::Softline => SourcePresentationHintData::Softline,
        DraftPresentationHint::Hardline => SourcePresentationHintData::Hardline,
        DraftPresentationHint::Blankline => SourcePresentationHintData::Blankline,
        DraftPresentationHint::Compact => SourcePresentationHintData::Compact,
        DraftPresentationHint::Sep(literal) => SourcePresentationHintData::Sep { literal },
    }
}

fn materialize_node_kind(kind: DraftNodeKind, parent: usize) -> Option<SourceNodeKindData> {
    Some(match kind {
        DraftNodeKind::Epsilon => SourceNodeKindData::Epsilon,
        DraftNodeKind::Literal(bytes) => SourceNodeKindData::Literal(bytes),
        DraftNodeKind::Regex(pattern) => {
            SourceNodeKindData::Regex(String::from_utf8(pattern).ok()?)
        }
        DraftNodeKind::Reference(name) => SourceNodeKindData::Reference(name),
        DraftNodeKind::Sequence(children) => SourceNodeKindData::Sequence(
            children
                .into_iter()
                .map(|child| (child < parent).then_some(child).and_then(NodeIx::checked))
                .collect::<Option<Vec<_>>>()?,
        ),
        DraftNodeKind::Choice(children) => SourceNodeKindData::Choice(
            children
                .into_iter()
                .map(|child| (child < parent).then_some(child).and_then(NodeIx::checked))
                .collect::<Option<Vec<_>>>()?,
        ),
        DraftNodeKind::Repeat {
            child,
            minimum,
            maximum,
        } => SourceNodeKindData::Repeat {
            child: (child < parent)
                .then_some(child)
                .and_then(NodeIx::checked)?,
            minimum,
            maximum,
        },
        DraftNodeKind::Transform {
            child,
            transform,
            name,
        } => SourceNodeKindData::Transform {
            child: (child < parent)
                .then_some(child)
                .and_then(NodeIx::checked)?,
            transform: TransformIx::checked(transform)?,
            name,
        },
        DraftNodeKind::Projection { left, right, keep } => SourceNodeKindData::Projection {
            left: (left < parent).then_some(left).and_then(NodeIx::checked)?,
            right: (right < parent)
                .then_some(right)
                .and_then(NodeIx::checked)?,
            keep,
        },
        DraftNodeKind::Difference { include, exclude } => SourceNodeKindData::Difference {
            include: (include < parent)
                .then_some(include)
                .and_then(NodeIx::checked)?,
            exclude: (exclude < parent)
                .then_some(exclude)
                .and_then(NodeIx::checked)?,
        },
        DraftNodeKind::Trim { child } => SourceNodeKindData::Trim {
            child: (child < parent)
                .then_some(child)
                .and_then(NodeIx::checked)?,
        },
        DraftNodeKind::Record(slots) => SourceNodeKindData::Record(
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
                    } => Some(SourceRecordSlotData::Field {
                        name,
                        child: (child < parent)
                            .then_some(child)
                            .and_then(NodeIx::checked)?,
                        name_span: SourceSpan::checked(name_start, name_end)?,
                        span: SourceSpan::checked(start, end)?,
                    }),
                    DraftRecordSlot::Discard { child, start, end } => {
                        Some(SourceRecordSlotData::Discard {
                            child: (child < parent)
                                .then_some(child)
                                .and_then(NodeIx::checked)?,
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
