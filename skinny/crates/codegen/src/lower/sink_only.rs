use super::{LowerCtx, ShapeLowering};
use ir::{
    AltMode, BackendExpr, BackendIr, BackendRule, BackendShape, CostFacts, DirectBuildField,
    SpanKind,
};
use std::collections::BTreeSet;

pub static LOWERING: Lowering = Lowering;

pub struct Lowering;

impl ShapeLowering for Lowering {
    fn lower_rule(&self, _ctx: &LowerCtx<'_>, rule: &BackendRule, cost: &CostFacts) -> String {
        debug_assert_eq!(cost.chosen, BackendShape::SinkOnly);
        lower_rule(rule)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SinkOnlyProgram {
    pub entry_rule: String,
    pub rules: Vec<SinkOnlyRule>,
    pub direct_shapes: BTreeSet<String>,
    pub span_kinds: BTreeSet<SinkOnlySpanKind>,
    pub literals: BTreeSet<Vec<u8>>,
    pub dispatch_alt_count: usize,
}

impl SinkOnlyProgram {
    pub fn has_rule(&self, name: &str) -> bool {
        self.rules.iter().any(|rule| rule.name == name)
    }

    #[cfg(test)]
    pub fn has_shape(&self, shape: &str) -> bool {
        self.direct_shapes.contains(shape)
    }

    #[cfg(test)]
    pub fn has_literal(&self, literal: &[u8]) -> bool {
        self.literals.contains(literal)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SinkOnlyRule {
    pub name: String,
    pub expr: SinkOnlyExpr,
    pub direct_shape: Option<DirectShape>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectShape {
    pub shape: String,
    pub fields: Vec<DirectBuildField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SinkOnlyExpr {
    Entry(Box<SinkOnlyExpr>),
    Seq(Vec<SinkOnlyExpr>),
    Alt {
        mode: AltMode,
        branches: Vec<SinkOnlyExpr>,
    },
    RepeatLoop {
        body: Box<SinkOnlyExpr>,
        min: u32,
    },
    OptionalBranch(Box<SinkOnlyExpr>),
    ByteLiteral(Vec<u8>),
    RegexProgram {
        span_kind: SinkOnlySpanKind,
        pattern: String,
    },
    CallRule {
        callee: String,
    },
    SpanMark {
        label: String,
    },
    TapeEmit,
    DirectBuild(DirectShape),
    ValueProject,
    Return,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SinkOnlySpanKind {
    String,
    Number,
    Whitespace,
}

pub fn lower_rule(rule: &BackendRule) -> String {
    let mut facts = SinkOnlyFacts::default();
    let expr = lower_expr(&rule.expr, &mut facts);
    let direct_shape = direct_shape(&expr);
    let shape = direct_shape
        .as_ref()
        .map(|shape| shape.shape.as_str())
        .unwrap_or("none");
    format!(
        "rule {} -> sink_only(shape={shape}, spans={}, literals={}, dispatch_alts={})",
        rule.name,
        facts.span_kinds.len(),
        facts.literals.len(),
        facts.dispatch_alt_count
    )
}

pub fn lower_program(backend: &BackendIr) -> Option<SinkOnlyProgram> {
    let mut program_facts = SinkOnlyFacts::default();
    let mut rules = Vec::with_capacity(backend.rules.len());

    for rule in &backend.rules {
        let mut rule_facts = SinkOnlyFacts::default();
        let expr = lower_expr(&rule.expr, &mut rule_facts);
        let direct_shape = direct_shape(&expr);
        program_facts.merge(rule_facts);
        rules.push(SinkOnlyRule {
            name: rule.name.clone(),
            expr,
            direct_shape,
        });
    }

    if program_facts.direct_shapes.is_empty() {
        return None;
    }

    Some(SinkOnlyProgram {
        entry_rule: backend.entry_rule.clone(),
        rules,
        direct_shapes: program_facts.direct_shapes,
        span_kinds: program_facts.span_kinds,
        literals: program_facts.literals,
        dispatch_alt_count: program_facts.dispatch_alt_count,
    })
}

fn lower_expr(expr: &BackendExpr, facts: &mut SinkOnlyFacts) -> SinkOnlyExpr {
    match expr {
        BackendExpr::Entry(inner) => SinkOnlyExpr::Entry(Box::new(lower_expr(inner, facts))),
        BackendExpr::Seq(children) => SinkOnlyExpr::Seq(
            children
                .iter()
                .map(|child| lower_expr(child, facts))
                .collect(),
        ),
        BackendExpr::Alt { mode, branches } => {
            if *mode == AltMode::Dispatch {
                facts.dispatch_alt_count += branches.len();
            }
            SinkOnlyExpr::Alt {
                mode: *mode,
                branches: branches
                    .iter()
                    .map(|branch| lower_expr(branch, facts))
                    .collect(),
            }
        }
        BackendExpr::RepeatLoop { body, min } => SinkOnlyExpr::RepeatLoop {
            body: Box::new(lower_expr(body, facts)),
            min: *min,
        },
        BackendExpr::OptionalBranch(inner) => {
            SinkOnlyExpr::OptionalBranch(Box::new(lower_expr(inner, facts)))
        }
        BackendExpr::ByteLiteral(bytes) => {
            facts.literals.insert(bytes.clone());
            SinkOnlyExpr::ByteLiteral(bytes.clone())
        }
        BackendExpr::RegexProgram { pattern, span_kind } => {
            let span_kind = sink_span_kind(*span_kind);
            facts.span_kinds.insert(span_kind);
            SinkOnlyExpr::RegexProgram {
                span_kind,
                pattern: pattern.clone(),
            }
        }
        BackendExpr::CallRule { callee } => SinkOnlyExpr::CallRule {
            callee: callee.clone(),
        },
        BackendExpr::SpanMark { label, .. } => SinkOnlyExpr::SpanMark {
            label: label.clone(),
        },
        BackendExpr::TapeEmit { .. } => SinkOnlyExpr::TapeEmit,
        BackendExpr::DirectBuild { shape, fields } => {
            facts.direct_shapes.insert(shape.clone());
            SinkOnlyExpr::DirectBuild(DirectShape {
                shape: shape.clone(),
                fields: fields.clone(),
            })
        }
        BackendExpr::ValueProject { .. } => SinkOnlyExpr::ValueProject,
        BackendExpr::Return => SinkOnlyExpr::Return,
    }
}

fn direct_shape(expr: &SinkOnlyExpr) -> Option<DirectShape> {
    match expr {
        SinkOnlyExpr::DirectBuild(shape) => Some(shape.clone()),
        SinkOnlyExpr::Entry(inner)
        | SinkOnlyExpr::OptionalBranch(inner)
        | SinkOnlyExpr::RepeatLoop { body: inner, .. } => direct_shape(inner),
        SinkOnlyExpr::Seq(children) => children.iter().find_map(direct_shape),
        SinkOnlyExpr::Alt { branches, .. } => branches.iter().find_map(direct_shape),
        SinkOnlyExpr::ByteLiteral(_)
        | SinkOnlyExpr::RegexProgram { .. }
        | SinkOnlyExpr::CallRule { .. }
        | SinkOnlyExpr::SpanMark { .. }
        | SinkOnlyExpr::TapeEmit
        | SinkOnlyExpr::ValueProject
        | SinkOnlyExpr::Return => None,
    }
}

fn sink_span_kind(kind: SpanKind) -> SinkOnlySpanKind {
    match kind {
        SpanKind::String => SinkOnlySpanKind::String,
        SpanKind::Number => SinkOnlySpanKind::Number,
        SpanKind::Whitespace => SinkOnlySpanKind::Whitespace,
    }
}

#[derive(Default)]
struct SinkOnlyFacts {
    direct_shapes: BTreeSet<String>,
    span_kinds: BTreeSet<SinkOnlySpanKind>,
    literals: BTreeSet<Vec<u8>>,
    dispatch_alt_count: usize,
}

impl SinkOnlyFacts {
    fn merge(&mut self, other: Self) {
        self.direct_shapes.extend(other.direct_shapes);
        self.span_kinds.extend(other.span_kinds);
        self.literals.extend(other.literals);
        self.dispatch_alt_count += other.dispatch_alt_count;
    }
}
