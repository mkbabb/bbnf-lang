use std::collections::VecDeque;

use bbnf_regex::{DEAD_STATE, RegexTablePlan};
use serde::{Deserialize, Serialize};

use super::budget::CompileOptions;
use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::Revision;
use super::syntax::{NodeId, NodeKind, Program, RuleId};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct NodeFacts {
    revision: Revision,
    node: NodeId,
    nullable: bool,
    minimum_consumption: Option<u64>,
}

impl NodeFacts {
    pub const fn revision(self) -> Revision {
        self.revision
    }

    pub const fn node(self) -> NodeId {
        self.node
    }

    pub const fn nullable(self) -> bool {
        self.nullable
    }

    /// Least number of bytes consumed by a successful match. `None` denotes
    /// an empty language rather than an unknown result.
    pub const fn minimum_consumption(self) -> Option<u64> {
        self.minimum_consumption
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct RuleFacts {
    revision: Revision,
    rule: RuleId,
    nullable: bool,
    minimum_consumption: Option<u64>,
}

impl RuleFacts {
    pub const fn revision(self) -> Revision {
        self.revision
    }

    pub const fn rule(self) -> RuleId {
        self.rule
    }

    pub const fn nullable(self) -> bool {
        self.nullable
    }

    pub const fn minimum_consumption(self) -> Option<u64> {
        self.minimum_consumption
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AnalysisFacts {
    revision: Revision,
    iterations: usize,
    nodes: Vec<NodeFacts>,
    rules: Vec<RuleFacts>,
}

impl AnalysisFacts {
    pub(crate) fn derive(
        program: &Program,
        dependency: &DependencySolution,
        options: CompileOptions,
    ) -> StageResult<Self> {
        if program.revision() != dependency.revision() {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::RevisionMismatch,
                0,
                0,
                format!(
                    "analysis cannot mix program revision {} with dependency revision {}",
                    program.revision().hex(),
                    dependency.revision().hex()
                ),
            ));
        }

        let revision = program.revision();
        let mut work = vec![WorkFacts::default(); program.nodes().len()];
        for node in program.nodes() {
            match node.kind() {
                NodeKind::Epsilon => {
                    work[node.id().index()] = WorkFacts::some(0);
                }
                NodeKind::Literal(bytes) => {
                    work[node.id().index()] = WorkFacts::some(bytes.len() as u64);
                }
                NodeKind::Regex(pattern) => {
                    let plan = match RegexTablePlan::compile_with(
                        pattern,
                        options.regex.table_options(),
                    ) {
                        Ok(plan) => plan,
                        Err(error) => {
                            let span = node.span();
                            return StageResult::rejected(Diagnostic::new(
                                DiagnosticCode::Regex,
                                span.start() as usize,
                                span.end() as usize,
                                format!("regex analysis failed: {error}"),
                            ));
                        }
                    };
                    work[node.id().index()] = regex_facts(&plan);
                }
                NodeKind::RuleRef(_)
                | NodeKind::Sequence(_)
                | NodeKind::Choice(_)
                | NodeKind::Repeat { .. } => {}
            }
        }
        let mut rule_work = vec![WorkFacts::default(); program.rules().len()];

        let mut iterations = 0;
        let converged = loop {
            if iterations >= options.analysis_iterations {
                break false;
            }
            iterations += 1;
            let mut changed = false;

            for node in program.nodes() {
                let derived = match node.kind() {
                    NodeKind::Epsilon | NodeKind::Literal(_) | NodeKind::Regex(_) => {
                        work[node.id().index()]
                    }
                    NodeKind::RuleRef(rule) => rule_work[rule.index()],
                    NodeKind::Sequence(children) => sequence_facts(children, &work),
                    NodeKind::Choice(children) => choice_facts(children, &work),
                    NodeKind::Repeat { child, minimum, .. } => {
                        repeat_facts(work[child.index()], *minimum)
                    }
                };
                changed |= update_monotone(&mut work[node.id().index()], derived);
            }

            for rule in program.rules() {
                let derived = work[rule.root().index()];
                changed |= update_monotone(&mut rule_work[rule.id().index()], derived);
            }

            if !changed {
                break true;
            }
        };

        if !converged {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::ResourceLimit,
                0,
                0,
                format!(
                    "analysis fixed point exceeded the deterministic limit of {} iterations",
                    options.analysis_iterations
                ),
            ));
        }

        let mut nullable_repeat_diagnostics = Vec::new();
        for node in program.nodes() {
            let NodeKind::Repeat { child, maximum, .. } = node.kind() else {
                continue;
            };
            if work[child.index()].nullable && *maximum != Some(0) {
                let span = node.span();
                nullable_repeat_diagnostics.push(Diagnostic::new(
                    DiagnosticCode::NullableRepeat,
                    span.start() as usize,
                    span.end() as usize,
                    "repetition of a nullable expression is rejected because it can succeed without advancing input",
                ));
            }
        }
        if !nullable_repeat_diagnostics.is_empty() {
            return StageResult::rejected_set(DiagnosticSet::from_vec(nullable_repeat_diagnostics));
        }

        let nodes = program
            .nodes()
            .iter()
            .map(|node| NodeFacts {
                revision,
                node: node.id(),
                nullable: work[node.id().index()].nullable,
                minimum_consumption: work[node.id().index()].minimum_consumption,
            })
            .collect();
        let rules = program
            .rules()
            .iter()
            .map(|rule| RuleFacts {
                revision,
                rule: rule.id(),
                nullable: rule_work[rule.id().index()].nullable,
                minimum_consumption: rule_work[rule.id().index()].minimum_consumption,
            })
            .collect();

        StageResult::accepted(Self {
            revision,
            iterations,
            nodes,
            rules,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub const fn iterations(&self) -> usize {
        self.iterations
    }

    pub fn nodes(&self) -> &[NodeFacts] {
        &self.nodes
    }

    pub fn rules(&self) -> &[RuleFacts] {
        &self.rules
    }

    pub fn node(&self, id: NodeId) -> NodeFacts {
        self.nodes[id.index()]
    }

    pub fn rule(&self, id: RuleId) -> RuleFacts {
        self.rules[id.index()]
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct WorkFacts {
    nullable: bool,
    minimum_consumption: Option<u64>,
}

impl WorkFacts {
    const fn some(minimum_consumption: u64) -> Self {
        Self {
            nullable: minimum_consumption == 0,
            minimum_consumption: Some(minimum_consumption),
        }
    }
}

fn regex_facts(plan: &RegexTablePlan) -> WorkFacts {
    let start = plan.start_state() as usize;
    if plan.accepting()[start] {
        return WorkFacts::some(0);
    }

    let classes = plan.class_count() as usize;
    let mut distance = vec![None; plan.state_count() as usize];
    distance[start] = Some(0_u64);
    let mut pending = VecDeque::from([start]);
    while let Some(state) = pending.pop_front() {
        let next_distance = distance[state]
            .expect("queued regex states have a distance")
            .saturating_add(1);
        for &target in &plan.transitions()[state * classes..(state + 1) * classes] {
            if target == DEAD_STATE {
                continue;
            }
            let target = target as usize;
            if distance[target].is_some() {
                continue;
            }
            if plan.accepting()[target] {
                return WorkFacts::some(next_distance);
            }
            distance[target] = Some(next_distance);
            pending.push_back(target);
        }
    }

    WorkFacts::default()
}

fn sequence_facts(children: &[NodeId], facts: &[WorkFacts]) -> WorkFacts {
    let nullable = children.iter().all(|child| facts[child.index()].nullable);
    let minimum_consumption = children.iter().try_fold(0_u64, |total, child| {
        total.checked_add(facts[child.index()].minimum_consumption?)
    });
    WorkFacts {
        nullable,
        minimum_consumption,
    }
}

fn choice_facts(children: &[NodeId], facts: &[WorkFacts]) -> WorkFacts {
    WorkFacts {
        nullable: children.iter().any(|child| facts[child.index()].nullable),
        minimum_consumption: children
            .iter()
            .filter_map(|child| facts[child.index()].minimum_consumption)
            .min(),
    }
}

fn repeat_facts(child: WorkFacts, minimum: u32) -> WorkFacts {
    if minimum == 0 {
        return WorkFacts::some(0);
    }
    WorkFacts {
        nullable: child.nullable,
        minimum_consumption: child
            .minimum_consumption
            .and_then(|minimum_child| minimum_child.checked_mul(u64::from(minimum))),
    }
}

fn update_monotone(current: &mut WorkFacts, derived: WorkFacts) -> bool {
    let next = WorkFacts {
        nullable: current.nullable || derived.nullable,
        minimum_consumption: match (current.minimum_consumption, derived.minimum_consumption) {
            (Some(current), Some(derived)) => Some(current.min(derived)),
            (current @ Some(_), None) => current,
            (None, derived) => derived,
        },
    };
    if *current == next {
        false
    } else {
        *current = next;
        true
    }
}
