use bbnf_regex::analyze;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

pub mod cost;
pub use cost::{
    all_backend_shapes, ActiveCostFacts, CapacityPolicy, CostFacts, DecisionCspFacts,
    EvidenceSource, Measurement, PriorityStep, RejectedAlternative, RejectionReason,
    ShapeRationale,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RuleId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExprId(pub usize);

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GrammarIr {
    pub name: String,
    pub source_hash: String,
    pub rules: Vec<Rule>,
    pub exprs: Vec<Expr>,
}

impl GrammarIr {
    pub fn new(name: impl Into<String>, source_hash: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            source_hash: source_hash.into(),
            rules: Vec::new(),
            exprs: Vec::new(),
        }
    }

    pub fn add_expr(&mut self, kind: ExprKind, span: SourceSpan) -> ExprId {
        let id = ExprId(self.exprs.len());
        self.exprs.push(Expr { id, kind, span });
        id
    }

    pub fn add_rule(&mut self, name: impl Into<String>, body: ExprId, span: SourceSpan) -> RuleId {
        let id = RuleId(self.rules.len());
        self.rules.push(Rule {
            id,
            name: name.into(),
            body,
            span,
        });
        id
    }

    pub fn rule_by_name(&self, name: &str) -> Option<&Rule> {
        self.rules.iter().find(|rule| rule.name == name)
    }

    pub fn rule(&self, id: RuleId) -> Option<&Rule> {
        self.rules.get(id.0)
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id.0]
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut Expr {
        &mut self.exprs[id.0]
    }

    pub fn resolve_refs(&mut self) -> Result<(), ValidationError> {
        let mut names = HashMap::new();
        for rule in &self.rules {
            if names.insert(rule.name.clone(), rule.id).is_some() {
                return Err(ValidationError::DuplicateRule {
                    name: rule.name.clone(),
                    span: rule.span,
                });
            }
        }

        for expr in &mut self.exprs {
            if let ExprKind::Ref { name, target } = &mut expr.kind {
                let Some(rule_id) = names.get(name).copied() else {
                    return Err(ValidationError::UnresolvedRef {
                        name: name.clone(),
                        span: expr.span,
                    });
                };
                *target = Some(rule_id);
            }
        }

        Ok(())
    }

    pub fn validate(&self) -> Result<ValidationReport, ValidationError> {
        validate_resolved_refs(self)?;
        validate_repeat_bodies(self)?;
        Ok(ValidationReport {
            rule_count: self.rules.len(),
            expr_count: self.exprs.len(),
        })
    }

    pub fn pretty(&self) -> String {
        let mut out = String::new();
        for rule in &self.rules {
            out.push_str(&rule.name);
            out.push_str(" = ");
            self.pretty_expr(rule.body, &mut out);
            out.push_str(" ;\n");
        }
        out
    }

    fn pretty_expr(&self, expr: ExprId, out: &mut String) {
        match &self.expr(expr).kind {
            ExprKind::Seq(children) => {
                for (index, child) in children.iter().enumerate() {
                    if index > 0 {
                        out.push(' ');
                    }
                    self.pretty_expr_wrapped(*child, out);
                }
            }
            ExprKind::Alt { branches, .. } => {
                for (index, branch) in branches.iter().enumerate() {
                    if index > 0 {
                        out.push_str(" | ");
                    }
                    self.pretty_expr_wrapped(*branch, out);
                }
            }
            ExprKind::Repeat { body, min, max } => {
                self.pretty_expr_wrapped(*body, out);
                match (*min, *max) {
                    (0, None) => out.push('*'),
                    (1, None) => out.push('+'),
                    (0, Some(1)) => out.push('?'),
                    _ => out.push_str("{n}"),
                }
            }
            ExprKind::Optional(body) => {
                self.pretty_expr_wrapped(*body, out);
                out.push('?');
            }
            ExprKind::Literal { bytes, .. } => {
                out.push('"');
                out.push_str(&String::from_utf8_lossy(bytes));
                out.push('"');
            }
            ExprKind::Regex { pattern } => {
                out.push('/');
                out.push_str(pattern);
                out.push('/');
            }
            ExprKind::Ref { name, .. } => out.push_str(name),
            ExprKind::Annotation { name, value } => {
                out.push('@');
                out.push_str(name);
                if let Some(value) = value {
                    out.push(' ');
                    out.push_str(value);
                }
            }
        }
    }

    fn pretty_expr_wrapped(&self, expr: ExprId, out: &mut String) {
        match self.expr(expr).kind {
            ExprKind::Alt { .. } => {
                out.push('(');
                self.pretty_expr(expr, out);
                out.push(')');
            }
            _ => self.pretty_expr(expr, out),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Rule {
    pub id: RuleId,
    pub name: String,
    pub body: ExprId,
    pub span: SourceSpan,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub span: SourceSpan,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExprKind {
    Seq(Vec<ExprId>),
    Alt {
        branches: Vec<ExprId>,
        mode: AltMode,
    },
    Repeat {
        body: ExprId,
        min: u32,
        max: Option<u32>,
    },
    Optional(ExprId),
    Literal {
        bytes: Vec<u8>,
        case: CaseSensitivity,
    },
    Regex {
        pattern: String,
    },
    Ref {
        name: String,
        target: Option<RuleId>,
    },
    Annotation {
        name: String,
        value: Option<String>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum AltMode {
    Dispatch,
    Speculative,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CaseSensitivity {
    Sensitive,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidationReport {
    pub rule_count: usize,
    pub expr_count: usize,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ValidationError {
    #[error("duplicate rule `{name}`")]
    DuplicateRule { name: String, span: SourceSpan },
    #[error("unresolved rule reference `{name}`")]
    UnresolvedRef { name: String, span: SourceSpan },
    #[error("expression {expr:?} references missing rule id {rule:?}")]
    MissingRuleId { expr: ExprId, rule: RuleId },
    #[error("repeat body at {span:?} is nullable")]
    NullableRepeatBody { span: SourceSpan },
}

fn validate_resolved_refs(grammar: &GrammarIr) -> Result<(), ValidationError> {
    for expr in &grammar.exprs {
        if let ExprKind::Ref { name, target } = &expr.kind {
            let Some(rule_id) = *target else {
                return Err(ValidationError::UnresolvedRef {
                    name: name.clone(),
                    span: expr.span,
                });
            };
            if grammar.rule(rule_id).is_none() {
                return Err(ValidationError::MissingRuleId {
                    expr: expr.id,
                    rule: rule_id,
                });
            }
        }
    }
    Ok(())
}

fn validate_repeat_bodies(grammar: &GrammarIr) -> Result<(), ValidationError> {
    let nullable = nullability(grammar);
    for expr in &grammar.exprs {
        if let ExprKind::Repeat { body, .. } = expr.kind {
            if nullable.get(body.0).copied().unwrap_or(false) {
                return Err(ValidationError::NullableRepeatBody { span: expr.span });
            }
        }
    }
    Ok(())
}

pub fn nullability(grammar: &GrammarIr) -> Vec<bool> {
    let mut nullable = vec![false; grammar.exprs.len()];
    loop {
        let mut changed = false;
        for expr in &grammar.exprs {
            let next = match &expr.kind {
                ExprKind::Seq(children) => children.iter().all(|child| nullable[child.0]),
                ExprKind::Alt { branches, .. } => branches.iter().any(|branch| nullable[branch.0]),
                ExprKind::Repeat { min, .. } => *min == 0,
                ExprKind::Optional(_) => true,
                ExprKind::Literal { bytes, .. } => bytes.is_empty(),
                ExprKind::Regex { pattern } => analyze(pattern).nullable,
                ExprKind::Ref { target, .. } => target
                    .and_then(|rule_id| grammar.rule(rule_id))
                    .map(|rule| nullable[rule.body.0])
                    .unwrap_or(false),
                ExprKind::Annotation { .. } => true,
            };
            if next != nullable[expr.id.0] {
                nullable[expr.id.0] = next;
                changed = true;
            }
        }
        if !changed {
            return nullable;
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BackendIr {
    pub grammar_name: String,
    pub entry_rule: String,
    pub recognizers: Vec<Recognizer>,
    pub rules: Vec<BackendRule>,
    pub shape_facts: ShapeFacts,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BackendShape {
    EagerTape,
    OffsetTape,
    EventTape,
    SinkOnly,
    CollapsedStage,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BackendRule {
    pub name: String,
    pub expr: BackendExpr,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BackendExpr {
    Entry(Box<BackendExpr>),
    Seq(Vec<BackendExpr>),
    Alt {
        mode: AltMode,
        branches: Vec<BackendExpr>,
    },
    RepeatLoop {
        body: Box<BackendExpr>,
        min: u32,
    },
    OptionalBranch(Box<BackendExpr>),
    ByteLiteral(Vec<u8>),
    RegexProgram {
        pattern: String,
        span_kind: SpanKind,
    },
    CallRule {
        callee: String,
    },
    SpanMark {
        kind: SpanMarkKind,
        label: String,
    },
    TapeEmit {
        kind: TapeKind,
    },
    DirectBuild {
        shape: String,
        fields: Vec<DirectBuildField>,
    },
    ValueProject {
        projection: String,
    },
    Return,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Recognizer {
    SimdScan {
        mode: SimdMode,
        alphabet: StructuralAlphabet,
        site: SimdSite,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum SimdMode {
    Exact,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum SimdSite {
    PreEntry,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructuralAlphabet {
    pub bytes: Vec<u8>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum SpanKind {
    String,
    Number,
    Whitespace,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum SpanMarkKind {
    Start,
    End,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TapeKind {
    Container,
    Sequence,
    KeyValuePair,
    StringValue,
    NumberValue,
    BoolValue,
    NullValue,
    Member,
    Element,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DirectBuildField {
    pub name: String,
    pub source: DirectBuildSource,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target: Option<DirectBuildTarget>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildSource {
    Span { label: String },
    ChildRule { rule: String },
    RepeatedRule { rule: String },
    Literal { bytes: Vec<u8> },
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DirectBuildTarget {
    pub rust_field: String,
    pub type_ref: DirectBuildTypeRef,
    pub presence: DirectBuildPresence,
    pub cardinality: DirectBuildCardinality,
    pub representation: DirectBuildRepresentation,
    pub decode: DirectBuildDecode,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildTypeRef {
    Named { type_id: String },
    Scalar { kind: DirectBuildScalar },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildScalar {
    String,
    Bool,
    I64,
    U64,
    F64,
    Null,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildPresence {
    Required,
    Optional,
    Default,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildCardinality {
    One,
    Vec,
    Map,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildRepresentation {
    Borrowed,
    Owned,
    BorrowedOrOwned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectBuildDecode {
    Raw,
    EscapedString,
    NumberScalar,
    Literal,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeFacts {
    pub shapes: Vec<Shape>,
}

impl ShapeFacts {
    pub fn new() -> Self {
        Self { shapes: Vec::new() }
    }

    pub fn add_struct(&mut self, name: impl Into<String>, fields: &[(&str, &str)]) {
        self.shapes.push(Shape::Struct {
            name: name.into(),
            fields: fields
                .iter()
                .map(|(name, ty)| ShapeField {
                    name: (*name).to_string(),
                    ty: (*ty).to_string(),
                })
                .collect(),
        });
    }

    pub fn add_enum(&mut self, name: impl Into<String>, variants: &[&str]) {
        self.shapes.push(Shape::Enum {
            name: name.into(),
            variants: variants
                .iter()
                .map(|variant| (*variant).to_string())
                .collect(),
        });
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Shape {
    Struct {
        name: String,
        fields: Vec<ShapeField>,
    },
    Enum {
        name: String,
        variants: Vec<String>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeField {
    pub name: String,
    pub ty: String,
}

impl fmt::Display for BackendIr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "backend {} entry {}", self.grammar_name, self.entry_rule)?;
        for recognizer in &self.recognizers {
            writeln!(f, "recognizer {recognizer:?}")?;
        }
        for rule in &self.rules {
            writeln!(f, "rule {} {:?}", rule.name, rule.expr)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_nullable_repeat_body() {
        let mut grammar = GrammarIr::new("test", "hash");
        let literal = grammar.add_expr(
            ExprKind::Literal {
                bytes: Vec::new(),
                case: CaseSensitivity::Sensitive,
            },
            SourceSpan::new(4, 6),
        );
        let repeat = grammar.add_expr(
            ExprKind::Repeat {
                body: literal,
                min: 0,
                max: None,
            },
            SourceSpan::new(4, 7),
        );
        grammar.add_rule("bad", repeat, SourceSpan::new(0, 8));

        assert!(matches!(
            grammar.validate(),
            Err(ValidationError::NullableRepeatBody { .. })
        ));
    }

    #[test]
    fn pretty_prints_rules_deterministically() {
        let mut grammar = GrammarIr::new("test", "hash");
        let lit = grammar.add_expr(
            ExprKind::Literal {
                bytes: b"null".to_vec(),
                case: CaseSensitivity::Sensitive,
            },
            SourceSpan::new(7, 13),
        );
        grammar.add_rule("null", lit, SourceSpan::new(0, 15));

        assert_eq!(grammar.pretty(), "null = \"null\" ;\n");
    }

    #[test]
    fn regex_nullability_uses_quantified_atom_shape() {
        let mut grammar = GrammarIr::new("test", "hash");
        let ws = grammar.add_expr(
            ExprKind::Regex {
                pattern: r"[ \t\n\r]*".to_string(),
            },
            SourceSpan::new(0, 11),
        );
        let string = grammar.add_expr(
            ExprKind::Regex {
                pattern: r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#.to_string(),
            },
            SourceSpan::new(12, 64),
        );
        let number = grammar.add_expr(
            ExprKind::Regex {
                pattern: r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?".to_string(),
            },
            SourceSpan::new(65, 112),
        );

        let nullable = nullability(&grammar);
        assert!(nullable[ws.0]);
        assert!(!nullable[string.0]);
        assert!(!nullable[number.0]);
    }
}
